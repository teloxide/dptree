use std::{future::Future, ops::ControlFlow, sync::Arc};

use futures::future::BoxFuture;

/// Struct that handles input values and returns some output. You can think of
/// it as a function that do some calculations.
///
/// You must not create it directly by yourself. Instead, you can use predefined
/// functions from `dptree::handler` mod.
pub struct Handler<'a, Input, Output, Intermediate = Input>(
    #[allow(clippy::type_complexity)]
    Arc<
        dyn Fn(Input, Cont<'a, Intermediate, Output>) -> HandlerResult<'a, Input, Output>
            + Send
            + Sync
            + 'a,
    >,
);

/// Continuation of the handler, which will be called if handler want it.
pub type Cont<'a, Intermediate, Output> =
    Box<dyn Fn(Intermediate) -> HandlerResult<'a, Intermediate, Output> + Send + Sync + 'a>;

/// Output of the `Handler`
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
    /// Chain two handlers to form a [Chain of responsibility].
    ///
    /// When executed, first will be executed `self` handler, and then, if
    /// `self` has been executed successfully, `next` handler will be
    /// executed.
    ///
    /// Example:
    /// ```
    /// # #[tokio::main]
    /// # async fn main() {
    /// use dptree::{container::Value, prelude::*};
    /// use std::ops::ControlFlow;
    ///
    /// let handler = dptree::filter(|x: Arc<i32>| async move { *x > 0 })
    ///     .chain(dptree::endpoint(|| async { "done" }));
    ///
    /// assert_eq!(handler.dispatch(Value::new(10)).await, ControlFlow::Break("done"));
    /// assert_eq!(handler.dispatch(Value::new(-10)).await, ControlFlow::Continue(Value::new(-10)));
    ///
    /// # }
    /// ```
    ///
    /// [Chain of responsibility]: https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern
    pub fn chain<Intermediate2>(
        self,
        next: Handler<'a, Intermediate, Output, Intermediate2>,
    ) -> Handler<'a, Input, Output, Intermediate2>
    where
        Intermediate2: Send + Sync + 'a,
    {
        from_fn(move |event, cont| {
            let this = self.clone();
            let next = next.clone();
            let cont = Arc::new(cont);

            this.execute(event, move |event| {
                let next = next.clone();
                let cont = cont.clone();

                #[allow(clippy::redundant_closure)] // Clippy is a fucking donkey.
                next.execute(event, move |event| cont(event))
            })
        })
    }

    /// Chain two handlers to make a tree.
    ///
    /// When executed, first will be executed `self` handler, and then, if
    /// `self` has been executed successfully, `next` handler will be
    /// executed. If `next` handler returns `ControlFlow::Break`, execution will
    /// stop, otherwise next handler added by `Handler::branch` will be
    /// executed.
    ///
    /// Example:
    /// ```
    /// use dptree::{container::Value, prelude::*};
    /// use std::ops::ControlFlow;
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    ///
    /// #[derive(Debug, PartialEq)]
    /// enum Output {
    ///     Five,
    ///     One,
    ///     GT,
    /// }
    ///
    /// let dispatcher = dptree::entry()
    ///     .branch(
    ///         dptree::filter(|num: Arc<i32>| async move { *num == 5 })
    ///             .endpoint(|| async move { Output::Five }),
    ///     )
    ///     .branch(
    ///         dptree::filter(|num: Arc<i32>| async move { *num == 1 })
    ///             .endpoint(|| async move { Output::One }),
    ///     )
    ///     .branch(
    ///         dptree::filter(|num: Arc<i32>| async move { *num > 2 })
    ///             .endpoint(|| async move { Output::GT }),
    ///     );
    ///
    /// assert_eq!(dispatcher.dispatch(Value::new(5)).await, ControlFlow::Break(Output::Five));
    /// assert_eq!(dispatcher.dispatch(Value::new(1)).await, ControlFlow::Break(Output::One));
    /// assert_eq!(dispatcher.dispatch(Value::new(3)).await, ControlFlow::Break(Output::GT));
    /// assert_eq!(dispatcher.dispatch(Value::new(0)).await, ControlFlow::Continue(Value::new(0)));
    /// # }
    /// ```
    pub fn branch<Intermediate2>(
        self,
        next: Handler<'a, Intermediate, Output, Intermediate2>,
    ) -> Handler<'a, Input, Output, Intermediate>
    where
        Intermediate2: Send + Sync + 'a,
    {
        from_fn(move |event, cont| {
            let this = self.clone();
            let next = next.clone();
            let cont = Arc::new(cont);

            this.execute(event, move |event| {
                let next = next.clone();
                let cont = cont.clone();

                async move {
                    match next.dispatch(event).await {
                        ControlFlow::Continue(event) => cont(event).await,
                        done => done,
                    }
                }
            })
        })
    }

    /// Execute handler with specified continuation.
    ///
    /// Usually, you do not want to call this method by yourself, if you do not
    /// write your own handler. If you wish to execute handler without
    /// continuation, take a look at the `Handler::dispatch` method.
    ///
    /// Example:
    /// ```
    /// # #[tokio::main]
    /// # async fn main() {
    /// use dptree::{container::Value, prelude::*};
    /// use std::ops::ControlFlow;
    ///
    /// let handler = dptree::filter(|x: Arc<i32>| async move { *x > 0 });
    ///
    /// let output = handler.execute(Value::new(10), |_| async { ControlFlow::Break("done") }).await;
    /// assert_eq!(output, ControlFlow::Break("done"));
    ///
    /// # }
    /// ```
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

    /// Execute handler with specified container.
    ///
    /// Returns `ControlFlow::Break` when executed successfully,
    /// `ControlFlow::Continue` otherwise.
    pub async fn dispatch(&self, container: Input) -> ControlFlow<Output, Input> {
        self.clone().execute(container, |event| async move { ControlFlow::Continue(event) }).await
    }
}

/// Constructor for the `Handler` struct.
///
/// Usually you do not want to use this func. Take a look at the functions
/// available in the root of the crate.
pub fn from_fn<'a, F, Fut, Input, Output, Intermediate>(
    f: F,
) -> Handler<'a, Input, Output, Intermediate>
where
    F: Fn(Input, Cont<'a, Intermediate, Output>) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + 'a,
{
    Handler(Arc::new(move |event, cont| Box::pin(f(event, cont))))
}

/// Create empty handler.
///
/// Convenient way to start building handler, because it provide all functions
/// to build chained handler: `filter`, `parser`, `endpoint`, etc.
pub fn entry<'a, Input, Output>() -> Handler<'a, Input, Output, Input>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    from_fn(|event, cont| cont(event))
}

#[cfg(test)]
mod tests {
    use crate::handler::{endpoint, filter};

    use super::*;
    use crate::container::Value;

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, _cont: Cont<i32, &'static str>| async move {
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

        let result = from_fn(|event: i32, _cont: Cont<i32, &'static str>| async move {
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

        let result = entry::<_, Output>().dispatch(input).await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_execute() {
        let input = 123;
        let output = "ABC";

        let result = from_fn(|event, cont| {
            assert!(event == input);
            cont(event)
        })
        .execute(input, |event| async move {
            assert!(event == input);
            ControlFlow::Break(output)
        })
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_deeply_nested_tree() {
        #[derive(Debug, PartialEq)]
        enum Output {
            LT,
            MinusOne,
            Zero,
            One,
            GT,
        }

        let negative_handler = filter(|num: Arc<i32>| async move { *num < 0 })
            .branch(
                filter(|num: Arc<i32>| async move { *num == -1 })
                    .endpoint(|| async move { Output::MinusOne }),
            )
            .branch(endpoint(|| async move { Output::LT }));

        let zero_handler = filter(|num: Arc<i32>| async move { *num == 0 })
            .endpoint(|| async move { Output::Zero });

        let positive_handler = filter(|num: Arc<i32>| async move { *num > 0 })
            .branch(
                filter(|num: Arc<i32>| async move { *num == 1 })
                    .endpoint(|| async move { Output::One }),
            )
            .branch(endpoint(|| async move { Output::GT }));

        let dispatcher =
            entry().branch(negative_handler).branch(zero_handler).branch(positive_handler);

        assert_eq!(dispatcher.dispatch(Value::new(2)).await, ControlFlow::Break(Output::GT));
        assert_eq!(dispatcher.dispatch(Value::new(1)).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.dispatch(Value::new(0)).await, ControlFlow::Break(Output::Zero));
        assert_eq!(dispatcher.dispatch(Value::new(-1)).await, ControlFlow::Break(Output::MinusOne));
        assert_eq!(dispatcher.dispatch(Value::new(-2)).await, ControlFlow::Break(Output::LT));
    }
}
