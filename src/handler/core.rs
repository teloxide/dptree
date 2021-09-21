use std::{future::Future, ops::ControlFlow, pin::Pin, sync::Arc};

pub struct Handler<'a, Input, Output, Cont = TerminalCont>(
    Arc<dyn Fn(Input, Cont) -> HandlerOutput<'a, Input, Output> + Send + Sync + 'a>,
);

// `#[derive(Clone)]` obligates all type parameters to satisfy `Clone` as well,
// but we do not need it here because of `Arc`.
impl<'a, Input, Output, Cont> Clone for Handler<'a, Input, Output, Cont> {
    fn clone(&self) -> Self {
        Handler(self.0.clone())
    }
}

pub type TerminalCont = ();

pub static TERMINATE: TerminalCont = ();

pub type HandlerOutput<'fut, Input, Output> =
    Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'fut>>;

impl<'a, Input, Output>
    Handler<'a, Input, Output, (Handler<'a, Input, Output, TerminalCont>, TerminalCont)>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub fn pipe_to(
        self,
        child: Handler<'a, Input, Output, TerminalCont>,
    ) -> Handler<'a, Input, Output, TerminalCont> {
        from_fn(move |event, _cont| {
            let this = self.clone();
            let child = child.clone();

            async move { this.execute(event, (child, TERMINATE)).await }
        })
    }
}

impl<'a, Input, Output, Cont>
    Handler<
        'a,
        Input,
        Output,
        (
            Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)>,
            (Handler<'a, Input, Output, Cont>, Cont),
        ),
    >
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Cont: Send + Sync + 'a,
{
    pub fn pipe_to(
        self,                                                                        // H<H<H<N>>>
        child: Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)>, // H<H<N>>
    ) -> Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)> // H<H<N>>
    {
        from_fn(move |event, cont| {
            let this = self.clone();
            let child = child.clone();

            async move { this.execute(event, (child, cont)).await }
        })
    }
}

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Cont>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub async fn execute(&self, event: Input, cont: Cont) -> ControlFlow<Output, Input>
    where
        Input: Send + Sync + 'a,
    {
        (self.0)(event, cont).await
    }
}

impl<'a, Input, Output> Handler<'a, Input, Output, TerminalCont>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub async fn handle(&self, event: Input) -> ControlFlow<Output, Input>
    where
        Input: Send + Sync + 'a,
    {
        self.execute(event, TERMINATE).await
    }
}

pub fn from_fn<'a, F, Fut, Input, Output, Cont>(f: F) -> Handler<'a, Input, Output, Cont>
where
    F: Fn(Input, Cont) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
{
    Handler(Arc::new(move |event, cont| Box::pin(f(event, cont))))
}

pub fn handler<'a, Input, Output, Cont>(
) -> Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Cont: Send + Sync + 'a,
{
    from_fn(move |event, (h, cont): (Handler<'a, Input, Output, Cont>, Cont)| async move {
        h.execute(event, cont).await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, cont| async move {
            assert_eq!(event, input);
            assert_eq!(cont, TERMINATE);
            ControlFlow::Break(output)
        })
        .handle(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_from_fn_continue() {
        type Output = &'static str;

        let input = 123;

        let result = from_fn(|event, cont| async move {
            assert_eq!(event, input);
            assert_eq!(cont, TERMINATE);
            ControlFlow::<Output, _>::Continue(event)
        })
        .handle(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }
}
