use std::{future::Future, ops::ControlFlow, pin::Pin, sync::Arc};

pub struct Handler<'a, Input, Output, Cont>(
    Arc<dyn Fn(Input, Cont) -> HandlerOutput<'a, Input, Output> + Send + Sync + 'a>,
);

impl<'a, I, O, C> Clone for Handler<'a, I, O, C> {
    fn clone(&self) -> Self {
        Handler(self.0.clone())
    }
}

pub type HandlerOutput<'fut, Input, Output> =
    Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'fut>>;

impl<'a, Input, Output> Handler<'a, Input, Output, ()>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub fn pipe_to(self, child: Handler<'a, Input, Output, ()>) -> Handler<'a, Input, Output, ()> {
        from_fn(move |event, _| {
            let this = self.clone();
            let child = child.clone();
            Box::pin(async move {
                match (this.0)(event, ()).await {
                    ControlFlow::Continue(c) => match (child.0)(c, ()).await {
                        ControlFlow::Continue(c) => ControlFlow::Continue(c),
                        ControlFlow::Break(b) => ControlFlow::Break(b.into()),
                    },
                    ControlFlow::Break(b) => ControlFlow::Break(b),
                }
            })
        })
    }
}

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Cont: 'a,
{
    pub fn pipe_to(
        self,
        child: Handler<'a, Input, Output, Cont>,
    ) -> Handler<'a, Input, Output, Cont> {
        from_fn(move |event, cont| {
            let this = self.clone();
            let child = child.clone();
            Box::pin((this.0)(event, (child, cont)))
        })
    }
}

impl<'a, Input, Output> Handler<'a, Input, Output, (Handler<'a, Input, Output, ()>, ())>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub fn endpoint<F, Fut>(self, endp: F) -> Handler<'a, Input, Output, ()>
    where
        F: Fn(Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = Output> + Send + Sync,
    {
        self.pipe_to(endpoint(endp))
    }
}

impl<'a, Input, Output> Handler<'a, Input, Output, ()>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub async fn execute(&self, event: Input) -> ControlFlow<Output, Input>
    where
        Input: Send + Sync + 'a,
    {
        (self.0)(event, ()).await
    }
}

pub fn from_fn<'a, F, Fut, Input, Output, Cont>(f: F) -> Handler<'a, Input, Output, Cont>
where
    F: Fn(Input, Cont) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
{
    Handler(Arc::new(move |event, cont| Box::pin(f(event, cont))))
}

pub fn dummy<'a, Input, Output>() -> Handler<'a, Input, Output, ()>
where
    Input: Send + Sync + 'a,
{
    from_fn(|event, _cont| async move { ControlFlow::Continue(event) })
}

pub fn filter<'a, Pred, Fut, Input, Output>(
    pred: Pred,
) -> Handler<'a, Input, Output, (Handler<'a, Input, Output, ()>, ())>
where
    Pred: Fn(&Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = bool> + Send + Sync,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    let pred = Arc::new(pred);

    from_fn(move |event, (next, _): (Handler<'a, Input, Output, ()>, ())| {
        let pred = Arc::clone(&pred);

        async move {
            if pred(&event).await {
                next.execute(event).await
            } else {
                ControlFlow::Continue(event)
            }
        }
    })
}

pub fn endpoint<'a, F, Fut, Input, Output>(f: F) -> Handler<'a, Input, Output, ()>
where
    F: Fn(Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = Output> + Send + Sync,
    Input: Send + Sync + 'a,
{
    let f = Arc::new(f);

    from_fn(move |event, _| {
        let f = Arc::clone(&f);
        async move { ControlFlow::Break(f(event).await) }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::ops::ControlFlow;

    #[tokio::test]
    async fn test_dummy() {
        type Input = i32;
        type Output = String;

        let input = 123;

        let result = dummy::<Input, Output>().execute(input).await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, cont| async move {
            assert_eq!(event, input);
            assert_eq!(cont, ());
            ControlFlow::Break(output)
        })
        .execute(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_from_fn_continue() {
        type Output = &'static str;

        let input = 123;

        let result = from_fn(|event, cont| async move {
            assert_eq!(event, input);
            assert_eq!(cont, ());
            ControlFlow::<Output, _>::Continue(event)
        })
        .execute(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_from_fn_call_k() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, (cont, next_cont): (Handler<_, _, _>, ())| async move {
            assert!(event == input);
            (cont.0)(event, next_cont).await
        })
        .endpoint(|event| async move {
            assert!(event == input);
            output
        })
        .execute(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_endpoint() {
        let input = 123;
        let output = 7;

        let result = endpoint(|event| async move {
            assert!(event == input);
            output
        })
        .execute(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_filter() {
        let input = 123;
        let output = 7;

        let result = filter(|&event| async move {
            assert!(event == input);
            true
        })
        .endpoint(|event| async move {
            assert!(event == input);
            output
        })
        .execute(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }
}
