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

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct TerminalCont;

pub type HandlerOutput<'fut, Input, Output> =
    Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'fut>>;

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>> {
    pub fn pipe_to<NextCont>(
        self,
        next: Handler<'a, Input, Output, NextCont>,
    ) -> Handler<'a, Input, Output, Handler<'a, Input, Output, NextCont>>
    where
        Self: Handleable<'a, Input, Output>,
        Handler<'a, Input, Output, NextCont>: Handleable<'a, Input, Output>,
        Input: Send + Sync + 'a,
        Output: Send + Sync + 'a,
        Cont: Send + Sync + 'a,
        NextCont: Clone + Send + Sync + 'a,
    {
        from_fn(move |event, cont: Handler<'a, Input, Output, NextCont>| {
            let this = self.clone();
            let next = next.clone();
            let cont = cont.clone();

            this.handle(
                event,
                Arc::new(move |event| {
                    let next = next.clone();
                    let cont = cont.clone();

                    next.handle(event, Arc::new(move |event| cont.clone().dispatch(event)))
                }),
            )
        })
    }
}

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>> {
    pub fn dispatch_to<NextCont>(
        self,
        next: Handler<'a, Input, Output, NextCont>,
    ) -> Handler<'a, Input, Output, Handler<'a, Input, Output, NextCont>>
    where
        Self: Handleable<'a, Input, Output>,
        Handler<'a, Input, Output, NextCont>: Handleable<'a, Input, Output>,
        Input: Send + Sync + 'a,
        Output: Send + Sync + 'a,
        Cont: Send + Sync + 'a,
        NextCont: Clone + Send + Sync + 'a,
    {
        from_fn(move |event, cont: Handler<'a, Input, Output, NextCont>| {
            let this = self.clone();
            let next = next.clone();
            let cont = cont.clone();

            this.handle(
                event,
                Arc::new(move |event| {
                    let next = next.clone();
                    let cont = cont.clone();

                    async move {
                        match next.dispatch(event).await {
                            ControlFlow::Continue(event) => cont.clone().dispatch(event).await,
                            done => done,
                        }
                    }
                }),
            )
        })
    }
}

pub type HandleResult<'a, Input, Output> =
    Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a>>;

pub trait Handleable<'a, Input, Output> {
    fn handle<F, Fut>(self, event: Input, fallback: Arc<F>) -> HandleResult<'a, Input, Output>
    where
        F: Fn(Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a;
}

impl<'a, Input, Output> Handleable<'a, Input, Output> for TerminalCont
where
    Input: Send + Sync + 'a,
{
    fn handle<F, Fut>(self, event: Input, fallback: Arc<F>) -> HandleResult<'a, Input, Output>
    where
        F: Fn(Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
    {
        Box::pin(fallback(event))
    }
}

impl<'a, Input, Output> Handleable<'a, Input, Output> for Handler<'a, Input, Output>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    fn handle<F, Fut>(self, event: Input, fallback: Arc<F>) -> HandleResult<'a, Input, Output>
    where
        F: Fn(Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
    {
        Box::pin(async move {
            match self.execute(event, TerminalCont).await {
                ControlFlow::Continue(event) => fallback(event).await,
                done => done,
            }
        })
    }
}

impl<'a, Input, Output, Cont> Handleable<'a, Input, Output>
    for Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>>
where
    Cont: Handleable<'a, Input, Output>,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Cont: Send + Sync + 'a,
{
    fn handle<F, Fut>(self, event: Input, fallback: Arc<F>) -> HandleResult<'a, Input, Output>
    where
        F: Fn(Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
    {
        Box::pin(
            self.execute(
                event,
                from_fn(move |event, cont: Cont| cont.handle(event, fallback.clone())),
            ),
        )
    }
}

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Cont>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub async fn execute(self, event: Input, cont: Cont) -> ControlFlow<Output, Input>
    where
        Input: Send + Sync + 'a,
    {
        (self.0)(event, cont).await
    }

    pub async fn dispatch(self, event: Input) -> ControlFlow<Output, Input>
    where
        Self: Handleable<'a, Input, Output>,
        Input: Send + Sync + 'a,
    {
        self.handle(event, Arc::new(|event| async move { ControlFlow::Continue(event) })).await
    }
}

pub fn from_fn<'a, F, Fut, Input, Output, Cont>(f: F) -> Handler<'a, Input, Output, Cont>
where
    F: Fn(Input, Cont) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
{
    Handler(Arc::new(move |event, cont| Box::pin(f(event, cont))))
}

pub fn entry<'a, Input, Output, Cont>(
) -> Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>>
where
    Handler<'a, Input, Output, Cont>: Handleable<'a, Input, Output>,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Cont: 'a,
{
    from_fn(
        |event, cont: Handler<'a, Input, Output, Cont>| async move { cont.dispatch(event).await },
    )
}

#[cfg(test)]
mod tests {
    use crate::handler::{endpoint::endpoint, filter};

    use super::*;

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event: i32, _: TerminalCont| async move {
            assert_eq!(event, input);
            ControlFlow::Break(output)
        })
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_from_fn_continue() {
        type Output = &'static str;

        let input = 123;

        let result = from_fn(|event: i32, _: TerminalCont| async move {
            assert_eq!(event, input);
            ControlFlow::<Output, _>::Continue(event)
        })
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_entry() {
        type Output = &'static str;

        let input = 123;

        let result = entry::<_, Output, TerminalCont>().dispatch(input).await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_dispatch_to() {
        #[derive(Debug, PartialEq)]
        enum Output {
            Five,
            One,
            GT,
        }

        let dispatcher = entry::<_, _, TerminalCont>()
            .dispatch_to(
                filter(|&num| async move { num == 5 }).endpoint(|_| async move { Output::Five }),
            )
            .dispatch_to(
                filter(|&num| async move { num == 1 }).endpoint(|_| async move { Output::One }),
            )
            .dispatch_to(
                filter(|&num| async move { num > 2 }).endpoint(|_| async move { Output::GT }),
            );

        assert_eq!(dispatcher.clone().dispatch(5).await, ControlFlow::Break(Output::Five));
        assert_eq!(dispatcher.clone().dispatch(1).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.clone().dispatch(3).await, ControlFlow::Break(Output::GT));
        assert_eq!(dispatcher.clone().dispatch(0).await, ControlFlow::Continue(0));
    }

    #[tokio::test]
    async fn test_tree() {
        #[derive(Debug, PartialEq)]
        enum Output {
            LT,
            MinusOne,
            Zero,
            One,
            GT,
        }

        let negative_handler = filter::<_, _, _, _, TerminalCont>(|&num| async move { num < 0 })
            .dispatch_to(
                filter(|&num| async move { num == -1 })
                    .endpoint(|_| async move { Output::MinusOne }),
            )
            .dispatch_to(endpoint(|_| async move { Output::LT }));

        let zero_handler =
            filter(|&num| async move { num == 0 }).endpoint(|_| async move { Output::Zero });

        let positive_handler = filter::<_, _, _, _, TerminalCont>(|&num| async move { num > 0 })
            .dispatch_to(
                filter(|&num| async move { num == 1 }).endpoint(|_| async move { Output::One }),
            )
            .dispatch_to(endpoint(|_| async move { Output::GT }));

        let dispatcher = entry::<_, _, TerminalCont>()
            .dispatch_to(negative_handler)
            .dispatch_to(zero_handler)
            .dispatch_to(positive_handler);

        assert_eq!(dispatcher.clone().dispatch(2).await, ControlFlow::Break(Output::GT));
        assert_eq!(dispatcher.clone().dispatch(1).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.clone().dispatch(0).await, ControlFlow::Break(Output::Zero));
        assert_eq!(dispatcher.clone().dispatch(-1).await, ControlFlow::Break(Output::MinusOne));
        assert_eq!(dispatcher.clone().dispatch(-2).await, ControlFlow::Break(Output::LT));
    }
}
