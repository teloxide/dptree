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

pub trait PipeTo<'a, Input, Output, NextCont> {
    fn pipe_to(self, next: NextCont) -> Handler<'a, Input, Output, NextCont>;
}

impl<'a, Input, Output, NextCont> PipeTo<'a, Input, Output, NextCont> for TerminalCont
where
    NextCont: Handleable<'a, Input, Output> + Send + Sync + Clone + 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    fn pipe_to(self, next: NextCont) -> Handler<'a, Input, Output, NextCont> {
        from_fn(move |event, cont: NextCont| {
            let next = next.clone();

            async move {
                match next.handle(event).await {
                    ControlFlow::Continue(event) => cont.handle(event).await,
                    done => done,
                }
            }
        })
    }
}

impl<'a, Input, Output, NextCont> PipeTo<'a, Input, Output, NextCont> for Handler<'a, Input, Output>
where
    Handler<'a, Input, Output, NextCont>: Handleable<'a, Input, Output>,
    NextCont: PipeTo<'a, Input, Output, NextCont> + Clone + Send + Sync + 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    fn pipe_to(self, next: NextCont) -> Handler<'a, Input, Output, NextCont> {
        from_fn(move |event, cont: NextCont| {
            let this = self.clone();
            let next = next.clone();

            async move {
                match this.handle(event).await {
                    ControlFlow::Continue(event) => next.pipe_to(cont).handle(event).await,
                    done => done,
                }
            }
        })
    }
}

impl<'a, Input, Output, NextCont> PipeTo<'a, Input, Output, NextCont>
    for Handler<'a, Input, Output, Handler<'a, Input, Output, NextCont>>
where
    NextCont: PipeTo<'a, Input, Output, NextCont> + Send + Sync + Clone + 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    fn pipe_to(self, next: NextCont) -> Handler<'a, Input, Output, NextCont> {
        from_fn(move |event, cont: NextCont| {
            let this = self.clone();
            let next = next.clone();

            this.execute(event, next.pipe_to(cont))
        })
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
}

pub trait Handleable<'a, Input, Output> {
    fn handle(
        self,
        event: Input,
    ) -> Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a>>;
}

impl<'a, Input, Output> Handleable<'a, Input, Output> for TerminalCont
where
    Input: Send + Sync + 'a,
{
    fn handle(
        self,
        event: Input,
    ) -> Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a>> {
        Box::pin(async move { ControlFlow::Continue(event) })
    }
}

impl<'a, Input, Output> Handleable<'a, Input, Output> for Handler<'a, Input, Output>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    fn handle(
        self,
        event: Input,
    ) -> Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a>> {
        Box::pin(self.execute(event, TerminalCont))
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
    fn handle(
        self,
        event: Input,
    ) -> Pin<Box<dyn Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a>> {
        Box::pin(self.execute(event, from_fn(|event, cont: Cont| cont.handle(event))))
    }
}

pub fn from_fn<'a, F, Fut, Input, Output, Cont>(f: F) -> Handler<'a, Input, Output, Cont>
where
    F: Fn(Input, Cont) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + Sync + 'a,
{
    Handler(Arc::new(move |event, cont| Box::pin(f(event, cont))))
}

pub fn entry<'a, Input, Output, Cont>() -> Handler<'a, Input, Output, Cont>
where
    Cont: Handleable<'a, Input, Output>,
    Input: 'a,
    Output: 'a,
{
    from_fn(|event, cont: Cont| cont.handle(event))
}

#[cfg(test)]
mod tests {
    use crate::handler::{endpoint, filter};

    use super::*;

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event: i32, cont: TerminalCont| async move {
            assert_eq!(event, input);
            assert_eq!(cont, TerminalCont);
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

        let result = from_fn(|event: i32, cont: TerminalCont| async move {
            assert_eq!(event, input);
            assert_eq!(cont, TerminalCont);
            ControlFlow::<Output, _>::Continue(event)
        })
        .handle(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_linear_pipe_to() {
        #[derive(Debug, PartialEq)]
        enum Output {
            Five,
            One,
            BT2,
        }

        let dispatcher = entry()
            .pipe_to(
                filter(|&num| async move { num == 5 }).endpoint(|_| async move { Output::Five }),
            )
            .pipe_to(
                filter(|&num| async move { num == 1 }).endpoint(|_| async move { Output::One }),
            )
            .pipe_to::<TerminalCont>(
                filter(|&num| async move { num > 2 }).endpoint(|_| async move { Output::BT2 }),
            );

        assert_eq!(dispatcher.clone().handle(5).await, ControlFlow::Break(Output::Five));
        assert_eq!(dispatcher.clone().handle(1).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.clone().handle(3).await, ControlFlow::Break(Output::BT2));
        assert_eq!(dispatcher.clone().handle(0).await, ControlFlow::Continue(0));
    }

    // #[tokio::test]
    // async fn test_hierarchical_pipe_to() {
    //     #[derive(Debug, PartialEq)]
    //     enum Output {
    //         LT,
    //         MinusOne,
    //         Zero,
    //         One,
    //         GT,
    //     }

    //     let negative_handler = filter(|&num| async move { num < 0 })
    //         .pipe_to(
    //             filter(|&num| async move { num == -1 })
    //                 .endpoint(|_| async move { Output::MinusOne }),
    //         )
    //         .pipe_to::<TerminalCont>(endpoint(|_| async move { Output::LT
    // }));

    //     dbg!(negative_handler.clone().handle(1).await);

    //     let zero_handler =
    //         filter(|&num| async move { num == 0 }).endpoint(|_| async move {
    // Output::Zero });

    //     let positive_handler = filter(|&num| async move { num > 0 })
    //         .pipe_to(
    //             filter(|&num| async move { num == 1 }).endpoint(|_| async
    // move { Output::One }),         )
    //         .pipe_to::<TerminalCont>(endpoint(|_| async move { Output::GT
    // }));

    //     let dispatcher = entry()
    //         .pipe_to(negative_handler)
    //         .pipe_to(zero_handler)
    //         .pipe_to::<TerminalCont>(positive_handler);

    //     // assert_eq!(dispatcher.clone().handle(2).await,
    //     // ControlFlow::Break(Output::GT));
    //     assert_eq!(dispatcher.clone().handle(1).await,
    // ControlFlow::Break(Output::One));     //  assert_eq!(dispatcher.
    // clone().handle(0).await,     // ControlFlow::Break(Output::Zero));
    //     assert_eq!(dispatcher.clone().handle(-1).await,
    // ControlFlow::Break(Output::MinusOne));     assert_eq!(dispatcher.
    // clone().handle(-2).await, ControlFlow::Break(Output::LT)); }
}
