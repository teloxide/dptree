use std::{future::Future, ops::ControlFlow, sync::Arc};

use futures::future::BoxFuture;

use crate::{description, HandlerDescription};

/// An instance that receives an input and decides whether to break a chain or
/// pass the value further.
///
/// In order to create this structure, you can use the predefined functions from
/// [`crate`].
pub struct Handler<'a, Input, Output, Descr = description::Unspecified> {
    data: Arc<HandlerData<Descr, DynF<'a, Input, Output>>>,
}

struct HandlerData<Descr, F: ?Sized> {
    description: Descr,
    f: F,
}

type DynF<'a, Input, Output> =
    dyn Fn(Input, Cont<'a, Input, Output>) -> HandlerResult<'a, Input, Output> + Send + Sync + 'a;

/// A continuation representing the rest of a handler chain.
pub type Cont<'a, Input, Output> =
    Box<dyn Fn(Input) -> HandlerResult<'a, Input, Output> + Send + Sync + 'a>;

/// An output type produced by a handler.
pub type HandlerResult<'a, Input, Output> = BoxFuture<'a, ControlFlow<Output, Input>>;

// `#[derive(Clone)]` obligates all type parameters to satisfy `Clone` as well,
// but we do not need it here because of `Arc`.
impl<'a, Input, Output, Descr> Clone for Handler<'a, Input, Output, Descr> {
    fn clone(&self) -> Self {
        Handler { data: Arc::clone(&self.data) }
    }
}

impl<'a, Input, Output, Descr> Handler<'a, Input, Output, Descr>
where
    Input: Send + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    /// Chain two handlers to form a [chain of responsibility].
    ///
    /// First, `self` will be executed, and then, if `self` decides to continue
    /// execution, `next` will be executed.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[tokio::main]
    /// # async fn main() {
    /// use dptree::prelude::*;
    ///
    /// let handler: Handler<_, _> =
    ///     dptree::filter(|x: i32| x > 0).chain(dptree::endpoint(|| async { "done" }));
    ///
    /// assert_eq!(handler.dispatch(dptree::deps![10]).await, ControlFlow::Break("done"));
    /// assert_eq!(
    ///     handler.dispatch(dptree::deps![-10]).await,
    ///     ControlFlow::Continue(dptree::deps![-10])
    /// );
    ///
    /// # }
    /// ```
    ///
    /// [chain of responsibility]: https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern
    #[must_use]
    #[track_caller]
    pub fn chain(self, next: Self) -> Self {
        let required_update_kinds_set = self.description().merge_chain(next.description());

        from_fn_with_description(required_update_kinds_set, move |event, cont| {
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

    /// Chain two handlers to make a tree of responsibility.
    ///
    /// This function is the same as [`Handler::chain`] but instead of expanding
    /// a chain, it adds a new branch, thereby forming a tree.
    ///
    /// # Examples
    ///
    /// ```
    /// use dptree::prelude::*;
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
    /// let dispatcher: Handler<_, _> = dptree::entry()
    ///     .branch(dptree::filter(|num: i32| num == 5).endpoint(|| async move { Output::Five }))
    ///     .branch(dptree::filter(|num: i32| num == 1).endpoint(|| async move { Output::One }))
    ///     .branch(dptree::filter(|num: i32| num > 2).endpoint(|| async move { Output::GT }));
    ///
    /// assert_eq!(dispatcher.dispatch(dptree::deps![5]).await, ControlFlow::Break(Output::Five));
    /// assert_eq!(dispatcher.dispatch(dptree::deps![1]).await, ControlFlow::Break(Output::One));
    /// assert_eq!(dispatcher.dispatch(dptree::deps![3]).await, ControlFlow::Break(Output::GT));
    /// assert_eq!(
    ///     dispatcher.dispatch(dptree::deps![0]).await,
    ///     ControlFlow::Continue(dptree::deps![0])
    /// );
    /// # }
    /// ```
    #[must_use]
    #[track_caller]
    pub fn branch(self, next: Self) -> Self
    where
        Output: Send,
    {
        let required_update_kinds_set = self.description().merge_branch(next.description());

        from_fn_with_description(required_update_kinds_set, move |event, cont| {
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

    /// Executes this handler with a continuation.
    ///
    /// Usually, you do not want to call this method by yourself, if you do not
    /// write your own handler implementation. If you wish to execute handler
    /// without a continuation, take a look at the [`Handler::dispatch`] method.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[tokio::main]
    /// # async fn main() {
    /// use dptree::prelude::*;
    ///
    /// let handler: Handler<_, _> = dptree::filter(|x: i32| x > 0);
    ///
    /// let output = handler.execute(dptree::deps![10], |_| async { ControlFlow::Break("done") }).await;
    /// assert_eq!(output, ControlFlow::Break("done"));
    ///
    /// # }
    /// ```
    pub async fn execute<Cont, ContFut>(
        self,
        container: Input,
        cont: Cont,
    ) -> ControlFlow<Output, Input>
    where
        Cont: Fn(Input) -> ContFut,
        Cont: Send + Sync + 'a,
        ContFut: Future<Output = ControlFlow<Output, Input>> + Send + 'a,
    {
        (self.data.f)(container, Box::new(move |event| Box::pin(cont(event)))).await
    }

    /// Executes this handler.
    ///
    /// Returns [`ControlFlow::Break`] when executed successfully,
    /// [`ControlFlow::Continue`] otherwise.
    pub async fn dispatch(&self, container: Input) -> ControlFlow<Output, Input> {
        self.clone().execute(container, |event| async move { ControlFlow::Continue(event) }).await
    }

    /// Returns the set of updates that can be processed by this handler.
    pub fn description(&self) -> &Descr {
        &self.data.description
    }
}

/// Constructs a handler from a function.
///
/// Most of the time, you do not want to use this function. Take a look at more
/// specialised functions: [`crate::endpoint`], [`crate::filter`],
/// [`crate::filter_map`], etc.
#[must_use]
pub fn from_fn<'a, F, Fut, Input, Output, Descr>(f: F) -> Handler<'a, Input, Output, Descr>
where
    F: Fn(Input, Cont<'a, Input, Output>) -> Fut,
    F: Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + 'a,
    Descr: HandlerDescription,
{
    from_fn_with_description(Descr::user_defined(), f)
}

/// [`from_fn`] with a custom description.
#[must_use]
pub fn from_fn_with_description<'a, F, Fut, Input, Output, Descr>(
    description: Descr,
    f: F,
) -> Handler<'a, Input, Output, Descr>
where
    F: Fn(Input, Cont<'a, Input, Output>) -> Fut,
    F: Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + 'a,
{
    Handler {
        data: Arc::new(HandlerData {
            f: move |event, cont| Box::pin(f(event, cont)) as HandlerResult<_, _>,
            description,
        }),
    }
}

/// Constructs an entry point handler.
///
/// This function is only used to specify other handlers upon it (see the root
/// examples).
#[must_use]
#[track_caller]
pub fn entry<'a, Input, Output, Descr>() -> Handler<'a, Input, Output, Descr>
where
    Input: Send + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    from_fn_with_description(Descr::entry(), |event, cont| cont(event))
}

#[cfg(test)]
pub(crate) fn help_inference<I, O>(h: Handler<I, O>) -> Handler<I, O> {
    h
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use maplit::hashset;

    use crate::{
        deps, description, filter_map, filter_map_with_description,
        handler::{endpoint, filter, filter_async},
        prelude::DependencyMap,
    };

    use super::*;

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let result = help_inference(from_fn(|event, _cont: Cont<i32, &'static str>| async move {
            assert_eq!(event, input);
            ControlFlow::Break(output)
        }))
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_from_fn_continue() {
        let input = 123;
        type Output = &'static str;

        let result =
            help_inference(from_fn(|event: i32, _cont: Cont<i32, &'static str>| async move {
                assert_eq!(event, input);
                ControlFlow::<Output, _>::Continue(event)
            }))
            .dispatch(input)
            .await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_entry() {
        let input = 123;
        type Output = &'static str;

        let result = help_inference(entry::<_, Output, _>()).dispatch(input).await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_execute() {
        let input = 123;
        let output = "ABC";

        let result = help_inference(from_fn(|event, cont| {
            assert!(event == input);
            cont(event)
        }))
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

        let negative_handler = filter(|num: i32| num < 0)
            .branch(
                filter_async(|num: i32| async move { num == -1 })
                    .endpoint(|| async move { Output::MinusOne }),
            )
            .branch(endpoint(|| async move { Output::LT }));

        let zero_handler = filter_async(|num: i32| async move { num == 0 })
            .endpoint(|| async move { Output::Zero });

        let positive_handler = filter_async(|num: i32| async move { num > 0 })
            .branch(
                filter_async(|num: i32| async move { num == 1 })
                    .endpoint(|| async move { Output::One }),
            )
            .branch(endpoint(|| async move { Output::GT }));

        let dispatcher = help_inference(entry())
            .branch(negative_handler)
            .branch(zero_handler)
            .branch(positive_handler);

        assert_eq!(dispatcher.dispatch(deps![2]).await, ControlFlow::Break(Output::GT));
        assert_eq!(dispatcher.dispatch(deps![1]).await, ControlFlow::Break(Output::One));
        assert_eq!(dispatcher.dispatch(deps![0]).await, ControlFlow::Break(Output::Zero));
        assert_eq!(dispatcher.dispatch(deps![-1]).await, ControlFlow::Break(Output::MinusOne));
        assert_eq!(dispatcher.dispatch(deps![-2]).await, ControlFlow::Break(Output::LT));
    }

    #[tokio::test]
    async fn allowed_updates() {
        use crate::description::{EventKind, InterestSet};
        use UpdateKind::*;

        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        enum UpdateKind {
            A,
            B,
            C,
        }

        impl EventKind for UpdateKind {
            fn full_set() -> HashSet<Self> {
                hashset! { A, B, C }
            }

            fn empty_set() -> HashSet<Self> {
                hashset! {}
            }
        }

        #[derive(Clone)]
        #[allow(dead_code)]
        enum Update {
            A(i32),
            B(u8),
            C(u64),
        }

        fn filter_a<Out>() -> Handler<'static, DependencyMap, Out, InterestSet<UpdateKind>>
        where
            Out: Send + Sync + 'static,
        {
            filter_map_with_description(
                InterestSet::new_filter(hashset! { A }),
                |update: Update| match update {
                    Update::A(x) => Some(x),
                    _ => None,
                },
            )
        }

        fn filter_b<Out>() -> Handler<'static, DependencyMap, Out, InterestSet<UpdateKind>>
        where
            Out: Send + Sync + 'static,
        {
            filter_map_with_description(
                InterestSet::new_filter(hashset! { B }),
                |update: Update| match update {
                    Update::B(x) => Some(x),
                    _ => None,
                },
            )
        }

        fn filter_c<Out>() -> Handler<'static, DependencyMap, Out, InterestSet<UpdateKind>>
        where
            Out: Send + Sync + 'static,
        {
            filter_map_with_description(
                InterestSet::new_filter(hashset! { C }),
                |update: Update| match update {
                    Update::B(x) => Some(x),
                    _ => None,
                },
            )
        }

        // User-defined filter that doesn't provide allowed updates
        fn user_defined_filter<Out>(
        ) -> Handler<'static, DependencyMap, Out, InterestSet<UpdateKind>>
        where
            Out: Send + Sync + 'static,
        {
            filter_map(|update: Update| match update {
                Update::B(x) => Some(x),
                _ => None,
            })
        }

        #[track_caller]
        fn assert(
            handler: Handler<'static, DependencyMap, (), description::InterestSet<UpdateKind>>,
            allowed: HashSet<UpdateKind>,
        ) {
            assert_eq!(handler.description().observed, allowed);
        }

        // Filters don't observe anything on their own
        assert(filter_a(), hashset! {});
        assert(entry().chain(filter_b()), hashset! {});
        assert(filter_a().chain(filter_b()), hashset! {});
        assert(filter_a().branch(filter_b()), hashset! {});
        assert(filter_a().branch(filter_b()).branch(filter_c().chain(filter_c())), hashset! {});

        // Anything user-defined observes everything that it can
        assert(filter_a().chain(filter(|| true)), hashset! { A });
        assert(user_defined_filter().chain(filter_a()), hashset! { A, B, C });
        assert(filter_a().chain(user_defined_filter()), hashset! { A });
        assert(
            entry().branch(filter_a()).branch(filter_b()).chain(user_defined_filter()),
            hashset! { A, B, C },
        );
        assert(
            entry()
                .branch(filter_a().endpoint(|| async {}))
                .branch(filter_b().endpoint(|| async {})),
            hashset! { A, B },
        );
        assert(user_defined_filter(), hashset! { A, B, C });
        assert(user_defined_filter().branch(filter_a()), hashset! { A, B, C });

        // Entry is invisible
        assert(entry(), hashset! {});
        assert(entry().chain(filter_a().endpoint(|| async {})), hashset! { A });
        assert(entry().branch(filter_a()), hashset! {});

        // Chained non-overlapping filters do not allow anything
        assert(filter_a().chain(filter_b()).endpoint(|| async {}), hashset! {});
    }
}
