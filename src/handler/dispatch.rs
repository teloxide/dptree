use crate::{
    handler::core::{from_fn, Handler},
    TerminalCont,
};
use std::ops::ControlFlow;

impl<'a, Input, Output>
    Handler<'a, Input, Output, (Handler<'a, Input, Output, TerminalCont>, TerminalCont)>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub fn end_dispatch(
        self,
        child: Handler<'a, Input, Output, TerminalCont>,
    ) -> Handler<'a, Input, Output, TerminalCont> {
        self.pipe_to(child)
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
    pub fn dispatch(
        self,                                            // H<H<H<N>>>
        child: Handler<'a, Input, Output, TerminalCont>, // H<()>
    ) -> Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)> // H<H<N>>
    {
        let h =
            from_fn(move |event, (cont, next_cont): (Handler<'a, Input, Output, Cont>, Cont)| {
                let child = child.clone();
                async move {
                    match child.handle(event).await {
                        ControlFlow::Continue(c) => cont.execute(c, next_cont).await,
                        b => b,
                    }
                }
            });
        self.pipe_to(h)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_dispatch() {
        #[derive(Debug, PartialEq)]
        enum Output {
            Five,
            One,
            BT2,
        }

        let dispatcher = crate::handler()
            .dispatch(
                crate::filter(|&num| async move { num == 5 })
                    .endpoint(|_| async move { Output::Five }),
            )
            .dispatch(
                crate::filter(|&num| async move { num == 1 })
                    .endpoint(|_| async move { Output::One }),
            )
            .end_dispatch(
                crate::filter(|&num| async move { num > 2 })
                    .endpoint(|_| async move { Output::BT2 }),
            );

        assert_eq!(dispatcher.handle(5).await, ControlFlow::Break(Output::Five));
        assert_eq!(dispatcher.handle(1).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.handle(3).await, ControlFlow::Break(Output::BT2));
        assert_eq!(dispatcher.handle(0).await, ControlFlow::Continue(0));
    }

    #[tokio::test]
    async fn test_hierarchical_dispatch() {
        #[derive(Debug, PartialEq)]
        enum Output {
            One,
            BT0,
            Zero,
            LT0,
            MinusOne,
        }

        let dispatcher = crate::handler()
            .dispatch(
                crate::filter(|&num| async move { num > 0 })
                    .dispatch(
                        crate::filter(|&num| async move { num == 1 })
                            .endpoint(|_| async move { Output::One }),
                    )
                    .end_dispatch(crate::endpoint(|_| async move { Output::BT0 })),
            )
            .dispatch(
                crate::filter(|&num| async move { num == 0 })
                    .endpoint(|_| async move { Output::Zero }),
            )
            .end_dispatch(
                crate::filter(|&num| async move { num < 0 })
                    .dispatch(
                        crate::filter(|&num| async move { num == -1 })
                            .endpoint(|_| async move { Output::MinusOne }),
                    )
                    .end_dispatch(crate::endpoint(|_| async move { Output::LT0 })),
            );

        assert_eq!(dispatcher.handle(2).await, ControlFlow::Break(Output::BT0));
        assert_eq!(dispatcher.handle(1).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.handle(0).await, ControlFlow::Break(Output::Zero));
        assert_eq!(dispatcher.handle(-1).await, ControlFlow::Break(Output::MinusOne));
        assert_eq!(dispatcher.handle(-2).await, ControlFlow::Break(Output::LT0));
    }
}
