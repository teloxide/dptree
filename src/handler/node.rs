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
    pub fn dispatch<F>(self, f: F) -> Handler<'a, Input, Output, TerminalCont>
    where
        F: FnOnce(
            Handler<'a, Input, Output, TerminalCont>,
        ) -> Handler<'a, Input, Output, TerminalCont>,
    {
        let handler = f(dispatch());
        self.pipe_to(handler)
    }
}

pub fn dispatch<'a, Input, Output>() -> Handler<'a, Input, Output, ()>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    from_fn(move |event, _| async move { ControlFlow::Continue(event) })
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

        let dispatcher = dispatch()
            .pipe_to(
                crate::filter(|&num| async move { num == 5 })
                    .endpoint(|_| async move { Output::Five }),
            )
            .pipe_to(
                crate::filter(|&num| async move { num == 1 })
                    .endpoint(|_| async move { Output::One }),
            )
            .pipe_to(
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

        let dispatcher = dispatch()
            .pipe_to(crate::filter(|&num| async move { num > 0 }).dispatch(|c| {
                c.pipe_to(
                    crate::filter(|&num| async move { num == 1 })
                        .endpoint(|_| async move { Output::One }),
                )
                .pipe_to(crate::endpoint(|_| async move { Output::BT0 }))
            }))
            .pipe_to(
                crate::filter(|&num| async move { num == 0 })
                    .endpoint(|_| async move { Output::Zero }),
            )
            .pipe_to(crate::filter(|&num| async move { num < 0 }).dispatch(|c| {
                c.pipe_to(
                    crate::filter(|&num| async move { num == -1 })
                        .endpoint(|_| async move { Output::MinusOne }),
                )
                .pipe_to(crate::endpoint(|_| async move { Output::LT0 }))
            }));

        //assert_eq!(dispatcher.handle(2).await, ControlFlow::Break(Output::BT0));
        //assert_eq!(dispatcher.handle(1).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.handle(0).await, ControlFlow::Break(Output::Zero));
        assert_eq!(dispatcher.handle(-1).await, ControlFlow::Break(Output::MinusOne));
        assert_eq!(dispatcher.handle(-2).await, ControlFlow::Break(Output::LT0));
    }
}
