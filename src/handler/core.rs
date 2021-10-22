use std::{future::Future, ops::ControlFlow, sync::Arc};

use futures::future::BoxFuture;

pub struct Handler<'a, Input, Output, Intermediate>(
    Arc<
        dyn Fn(Input, Cont<'a, Intermediate, Output>) -> HandlerResult<'a, Input, Output>
            + Send
            + Sync
            + 'a,
    >,
);

pub type Cont<'a, Intermediate, Output> =
    Box<dyn Fn(Intermediate) -> HandlerResult<'a, Intermediate, Output> + Send + Sync + 'a>;

pub type HandlerResult<'a, Input, Output> = BoxFuture<'a, ControlFlow<Output, Input>>;

// `#[derive(Clone)]` obligates all type parameters to satisfy `Clone` as well,
// but we do not need it here because of `Arc`.
impl<'a, Input, Output, Cont> Clone for Handler<'a, Input, Output, Cont> {
    fn clone(&self) -> Self {
        Handler(self.0.clone())
    }
}

impl<'a, Input, Output, Intermediate> Handler<'a, Input, Output, Intermediate>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Intermediate: Send + Sync + 'a,
{
    pub fn chain<NextCont, Intermediate2>(
        self,
        next: Handler<'a, Intermediate, Output, Intermediate2>,
    ) -> Handler<'a, Input, Output, Intermediate2>
    where
        NextCont: Clone,
        Intermediate2: Send + Sync + 'a,
    {
        from_fn(move |event, cont| {
            let this = self.clone();
            let next = next.clone();

            this.execute(event, move |event| {
                let next = next.clone();

                next.execute(event, move |event| cont(event))
            })
        })
    }

    pub fn branch<NextCont, Intermediate2>(
        self,
        next: Handler<'a, Intermediate, Output, Intermediate2>,
    ) -> Handler<'a, Input, Output, Intermediate>
    where
        NextCont: Clone,
        Intermediate2: Send + Sync + 'a,
    {
        from_fn(move |event, cont| {
            let this = self.clone();
            let next = next.clone();

            this.execute(event, move |event| {
                let next = next.clone();

                async move {
                    match next.dispatch(event).await {
                        ControlFlow::Continue(event) => cont(event).await,
                        done => done,
                    }
                }
            })
        })
    }

    pub async fn execute<Cont, ContFut>(
        self,
        event: Input,
        cont: Cont,
    ) -> ControlFlow<Output, Input>
    where
        Input: Send + Sync + 'a,
        Cont: Fn(Intermediate) -> ContFut + Send + Sync + 'a,
        ContFut: Future<Output = ControlFlow<Output, Intermediate>> + Send + 'a,
    {
        (self.0)(event, Box::new(move |event| Box::pin(cont(event)))).await
    }

    pub async fn dispatch(self, event: Input) -> ControlFlow<Output, Input> {
        self.execute(event, |event| async move { ControlFlow::Continue(event) }).await
    }
}

pub fn from_fn<'a, F, Fut, Input, Output, Intermediate>(
    f: F,
) -> Handler<'a, Input, Output, Intermediate>
where
    F: Fn(Input, Cont<Intermediate, Output>) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + 'a,
{
    Handler(Arc::new(move |event, cont| Box::pin(f(event, cont))))
}

pub fn entry<'a, Input, Output>() -> Handler<'a, Input, Output, Input>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    from_fn(|event, cont: Cont<'a, Input, Output>| cont(event))
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
        let input = 123;
        type Output = &'static str;

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
        let input = 123;
        type Output = &'static str;

        let result = entry::<_, Output, TerminalCont>().dispatch(input).await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_execute() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, cont: Handler<_, _>| {
            assert!(event == input);
            cont.dispatch(event)
        })
        .execute(
            input,
            endpoint(|event| {
                assert!(event == input);
                async move { output }
            }),
        )
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_handle_base_case_0_break() {
        let input = 123;
        let output = "ABC";

        let result = TerminalCont
            .handle(
                input,
                Arc::new(|event| async move {
                    assert!(event == input);
                    ControlFlow::Break(output)
                }),
            )
            .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_handle_base_case_0_continue() {
        let input = 123;
        type Output = &'static str;

        let result = TerminalCont
            .handle(
                input,
                Arc::new(|event| async move {
                    assert!(event == input);
                    ControlFlow::<Output, _>::Continue(event)
                }),
            )
            .await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_handle_base_case_1_break() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, _: TerminalCont| async move {
            assert!(event == input);
            ControlFlow::Continue(event)
        })
        .handle(
            input,
            Arc::new(|event| async move {
                assert!(event == input);
                ControlFlow::Break(output)
            }),
        )
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_handle_base_case_1_continue() {
        let input = 123;
        type Output = &'static str;

        let result = from_fn(|event, _: TerminalCont| async move {
            assert!(event == input);
            ControlFlow::Continue(event)
        })
        .handle(
            input,
            Arc::new(|event| async move {
                assert!(event == input);
                ControlFlow::<Output, _>::Continue(event)
            }),
        )
        .await;

        assert!(result == ControlFlow::Continue(input));
    }

    // #[tokio::test]
    // async fn test_handle_induction_case_break() {
    //     let input = 123;
    //     let output = "ABC";

    //     let result = from_fn::<_, _, i32, _, _>(|event, cont: Handler<_, _>|
    // {         assert!(event == input);
    //         cont.dispatch(event)
    //     })
    //     .chain(from_fn(|event, _: TerminalCont| async move {
    //         assert!(event == input);
    //         ControlFlow::Continue(event)
    //     }))
    //     .handle(
    //         input,
    //         Arc::new(|event| async move {
    //             assert!(event == input);
    //             ControlFlow::Break(output)
    //         }),
    //     )
    //     .await;

    //     assert!(result == ControlFlow::Break(output));
    // }

    // #[tokio::test]
    // async fn test_handle_induction_case_continue() {
    //     let input = 123;
    //     type Output = &'static str;

    //     let result = from_fn(|event, cont: Handler<_, _>| {
    //         assert!(event == input);
    //         cont.dispatch(event)
    //     })
    //     .chain(from_fn(|event, _: TerminalCont| async move {
    //         assert!(event == input);
    //         ControlFlow::Continue(event)
    //     }))
    //     .handle(
    //         input,
    //         Arc::new(|event| async move {
    //             assert!(event == input);
    //             ControlFlow::<Output, _>::Continue(input)
    //         }),
    //     )
    //     .await;

    //     assert!(result == ControlFlow::Continue(input));
    // }

    // #[tokio::test]
    // async fn test_tree() {
    //     #[derive(Debug, PartialEq)]
    //     enum Output {
    //         Five,
    //         One,
    //         GT,
    //     }

    //     let dispatcher = entry::<_, _, TerminalCont>()
    //         .branch(
    //             filter(|&num| async move { num == 5 }).endpoint(|_| async
    // move { Output::Five }),         )
    //         .branch(filter(|&num| async move { num == 1 }).endpoint(|_| async
    // move { Output::One }))         .branch(filter(|&num| async move { num
    // > 2 }).endpoint(|_| async move { Output::GT }));

    //     assert_eq!(dispatcher.clone().dispatch(5).await,
    // ControlFlow::Break(Output::Five));     assert_eq!(dispatcher.clone().
    // dispatch(1).await, ControlFlow::Break(Output::One));     assert_eq!
    // (dispatcher.clone().dispatch(3).await, ControlFlow::Break(Output::GT));
    //     assert_eq!(dispatcher.clone().dispatch(0).await,
    // ControlFlow::Continue(0)); }

    // #[tokio::test]
    // async fn test_deeply_nested_tree() {
    //     #[derive(Debug, PartialEq)]
    //     enum Output {
    //         LT,
    //         MinusOne,
    //         Zero,
    //         One,
    //         GT,
    //     }

    //     let negative_handler = filter::<_, _, _, _, TerminalCont>(|&num|
    // async move { num < 0 })         .branch(
    //             filter(|&num| async move { num == -1 })
    //                 .endpoint(|_| async move { Output::MinusOne }),
    //         )
    //         .branch(endpoint(|_| async move { Output::LT }));

    //     let zero_handler =
    //         filter(|&num| async move { num == 0 }).endpoint(|_| async move {
    // Output::Zero });

    //     let positive_handler = filter::<_, _, _, _, TerminalCont>(|&num|
    // async move { num > 0 })         .branch(filter(|&num| async move {
    // num == 1 }).endpoint(|_| async move { Output::One }))
    //         .branch(endpoint(|_| async move { Output::GT }));

    //     let dispatcher = entry::<_, _, TerminalCont>()
    //         .branch(negative_handler)
    //         .branch(zero_handler)
    //         .branch(positive_handler);

    //     assert_eq!(dispatcher.clone().dispatch(2).await,
    // ControlFlow::Break(Output::GT));     assert_eq!(dispatcher.clone().
    // dispatch(1).await, ControlFlow::Break(Output::One));     assert_eq!
    // (dispatcher.clone().dispatch(0).await, ControlFlow::Break(Output::Zero));
    //     assert_eq!(dispatcher.clone().dispatch(-1).await,
    // ControlFlow::Break(Output::MinusOne));     assert_eq!(dispatcher.
    // clone().dispatch(-2).await, ControlFlow::Break(Output::LT)); }
}
