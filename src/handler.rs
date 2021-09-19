use std::{convert::Infallible, future::Future, ops::ControlFlow, pin::Pin, sync::Arc};

pub struct Handler<'a, Input, Output, Cont>(
    Box<dyn Fn(Input, Arc<Cont>) -> HandlerOutput<'a, Input, Output> + Send + Sync + 'a>,
);

pub type HandlerOutput<'a, Input, Output> =
    Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a>>;

pub type TerminalCont = Infallible;

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>>
where
    Input: Send + Sync + 'a,
    Output: 'a,
    Cont: 'a + Send + Sync,
{
    pub fn pipe_to(self, child: Arc<Handler<'a, Input, Output, Cont>>) -> Self {
        let child = Arc::new(child);

        from_fn(move |event, k| {
            let child = Arc::clone(&child);

            (self.0)(
                event,
                Arc::new(from_fn(move |event, _k| {
                    let k = Arc::clone(&k);
                    (child.0)(event, k)
                })),
            )
        })
    }
}

impl<'a, Input, Output> Handler<'a, Input, Output, Handler<'a, Input, Output, TerminalCont>>
where
    Input: Send + Sync + 'a,
    Output: 'a,
{
    pub fn endpoint<F, Fut>(
        self,
        endp: F,
    ) -> Handler<'a, Input, Output, Handler<'a, Input, Output, TerminalCont>>
    where
        F: Fn(Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = Output> + Send + Sync,
    {
        self.pipe_to(Arc::new(endpoint(endp)))
    }

    pub async fn execute(&self, event: Input) -> ControlFlow<Output, Input>
    where
        Input: Send + Sync + 'a,
    {
        (self.0)(event, Arc::new(dummy())).await
    }
}

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>>
where
    Input: Send + Sync + 'a,
    Output: 'a,
{
    pub async fn handle(&self, event: Input) -> ControlFlow<Output, Input>
    where
        Input: Send + Sync + 'a,
    {
        (self.0)(event, Arc::new(dummy())).await
    }
}

pub fn from_fn<'a, F, Fut, Input, Output, Cont>(f: F) -> Handler<'a, Input, Output, Cont>
where
    F: Fn(Input, Arc<Cont>) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
{
    Handler(Box::new(move |event, k| Box::pin(f(event, k))))
}

pub fn dummy<'a, Input, Output, Cont>() -> Handler<'a, Input, Output, Cont>
where
    Input: Send + Sync + 'a,
{
    from_fn(|event, _k| async move { ControlFlow::Continue(event) })
}

pub fn filter<'a, Pred, Fut, Input, Output, Cont>(pred: Pred) -> Filter<'a, Input, Output, Cont>
where
    Pred: Fn(&Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = bool> + Send + Sync,
    Input: Send + Sync + 'a,
    Output: 'a,
    Cont: 'a,
{
    let pred = Arc::new(pred);

    from_fn(move |event, k: Arc<Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>>>| {
        let pred = Arc::clone(&pred);

        async move {
            if pred(&event).await {
                k.handle(event).await
            } else {
                ControlFlow::Continue(event)
            }
        }
    })
}

pub type Filter<'a, Input, Output, Cont> =
    Handler<'a, Input, Output, Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>>>;

pub fn endpoint<'a, F, Fut, Input, Output>(
    f: F,
) -> Handler<'a, Input, Output, Handler<'a, Input, Output, TerminalCont>>
where
    F: Fn(Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = Output> + Send + Sync,
    Input: Send + Sync + 'a,
{
    let f = Arc::new(f);

    from_fn(move |event, _k| {
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

        let result = dummy::<Input, Output, _>().execute(input).await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, _k| async move {
            assert!(event == input);
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

        let result = from_fn(|event, _k| async move {
            assert!(event == input);
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

        let result = from_fn(|event, k| async move {
            assert!(event == input);
            k.handle(event).await
        })
        .endpoint(|event| async move {
            assert!(event == input);
            output
        })
        .execute(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }
    //
    //    #[tokio::test]
    //    async fn test_endpoint() {
    //        let input = 123;
    //        let output = 7;
    //
    //        let result = endpoint(|event| async move {
    //            assert!(event == input);
    //            output
    //        })
    //        .handle(input)
    //        .await;
    //
    //        assert!(result == ControlFlow::Break(output));
    //    }
    //
    //    #[tokio::test]
    //    async fn test_empty_filter() {
    //        let input = 123;
    //
    //        type Output = i32;
    //
    //        let result = filter::<_, _, _, Output>(|&event| async move {
    //            assert!(event == input);
    //            false
    //        })
    //        .handle(input)
    //        .await;
    //
    //        assert!(result == ControlFlow::Continue(input));
    //    }
    //
    //    #[tokio::test]
    //    async fn test_filter() {
    //        let input = 123;
    //        let output = 7;
    //
    //        let result = filter(|&event| async move {
    //            assert!(event == input);
    //            true
    //        })
    //        .endpoint(|event| async move {
    //            assert!(event == input);
    //            output
    //        })
    //        .handle(input)
    //        .await;
    //
    //        assert!(result == ControlFlow::Break(output));
    //    }
}
