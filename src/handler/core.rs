use std::{
    any::TypeId, collections::HashSet, fmt::Write, future::Future, ops::ControlFlow, sync::Arc,
};

use futures::future::BoxFuture;

use crate::{description, prelude::DependencyMap, HandlerDescription};

/// An instance that receives an input and decides whether to break a chain or
/// pass the value further.
///
/// In order to create this structure, you can use the predefined functions from
/// [`crate`].
///
/// ## The difference between chaining and branching
///
/// Handlers can be chained via [`Handler::chain`] and branched via
/// [`Handler::branch`]. To understand the difference between the two, consider
/// the following examples: `a.chain(b).c` and `a.branch(b).c`.
///
/// In `a.chain(b).c`, the handler `a` is given the rest of the handler chain,
/// `b` and `c`; if `a` decides to pass the value further, it invokes `b`. Then,
/// if `b` decides to pass the value further, it invokes `c`. Thus, the handler
/// chain is _linear_.
///
/// In `a.branch(b).c`, if `a` decides to pass the value further, it invokes
/// `b`. But since `b` is "branched", it receives an empty chain, so it cannot
/// invoke `c`. Instead, if `b` decides to continue execution
/// ([`ControlFlow::Continue`]), `a` invokes `c`; otherwise
/// ([`ControlFlow::Break`]), the process is terminated. The handler chain is
/// _nested_.
///
/// To sum up, chaining looks like this:
///
/// ```txt
/// a -> b -> c
/// ```
///
/// And branching looks like this:
///
/// ```txt
/// a -> b
///   -> c
/// ```
///
/// This is very crucial when `b` is a filter: if it is chained, it decides
/// whether or not to call `c`, but when it is branched, whether `c` is called
/// depends solely on `a`.
pub struct Handler<'a, Input, Output, Descr = description::Unspecified> {
    data: Arc<HandlerData<Descr, DynF<'a, Input, Output>>>,
}

struct HandlerData<Descr, F: ?Sized> {
    description: Descr,
    sig: HandlerSignature,
    f: F,
}

/// A run-time handler signature. Used only for run-time type inference and
/// checking of handler chains.
///
/// See [`crate::type_check`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HandlerSignature {
    /// The signature corresponding to [`crate::entry`].
    Entry,
    /// The handler signature with exact input and output types.
    Other {
        /// The set of types that the handler accepts. In the case of a DI
        /// container, this is the set of types that this handler retrieves from
        /// the container.
        input_types: HashSet<Type>,
        /// The set of types that this handler passes down the chain.
        output_types: HashSet<Type>,
    },
}

/// A run-time representation of a type. Used only for run-time type inference
/// and checking of handler chains.
///
/// See [`crate::type_check`].
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Type {
    /// The unique type identifier.
    pub id: TypeId,

    /// The type name used for printing.
    pub name: &'static str,
}

type DynF<'a, Input, Output> =
    dyn Fn(Input, Cont<'a, Input, Output>) -> HandlerResult<'a, Input, Output> + Send + Sync + 'a;

/// A continuation representing the rest of a handler chain.
pub type Cont<'a, Input, Output> =
    Box<dyn FnOnce(Input) -> HandlerResult<'a, Input, Output> + Send + Sync + 'a>;

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
    /// Chaining is different from branching. See ["The difference between
    /// chaining and
    /// branching"](#the-difference-between-chaining-and-branching).
    ///
    /// # Type inference algorithm
    ///
    /// This method makes use of [`HandlerSignature`] (stored in every handler)
    /// to perform run-time type inference of the resulting handler
    /// signature; this information is used by [`crate::type_check`] to make
    /// sure that all required types are provided _before_ execution.
    ///
    /// The algorithm is as follows:
    ///  1. If the second handler's signature is [`HandlerSignature::Entry`],
    /// then **panic**.
    ///  2. If the first handler's signature is [`HandlerSignature::Entry`] and
    /// that of the second one is [`HandlerSignature::Other`], then the
    /// signature of the resulting handler is taken from the second one.
    ///  3. If the first handler's signature is [`HandlerSignature::Other`] and
    /// that the second one is also [`HandlerSignature::Other`], then the
    /// signature of the resulting handler is `HandlerSignature::Other {
    /// input_types: self_input_types UNION (next_input_types DIFFERENCE
    /// self_output_types), output_types: self_output_types UNION
    /// next_output_types }`.
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

        let new_sig = match (&self.data.sig, &next.data.sig) {
            (_, HandlerSignature::Entry) => {
                panic!("Ill-typed handler chain: the second handler cannot be an entry")
            }
            (HandlerSignature::Entry, HandlerSignature::Other { .. }) => next.data.sig.clone(),
            (
                HandlerSignature::Other {
                    input_types: self_input_types,
                    output_types: self_output_types,
                },
                HandlerSignature::Other {
                    input_types: next_input_types,
                    output_types: next_output_types,
                },
            ) => HandlerSignature::Other {
                /// Since the first handler can call the second one, take the
                /// union of (the first handler's input types) and (the
                /// difference between the second handler's input types and the
                /// first handler's output types). The difference is needed
                /// because the first handler can pass values to the second one.
                input_types: self_input_types
                    .union(&next_input_types.difference(self_output_types).cloned().collect())
                    .cloned()
                    .collect(),

                /// Since the first handler can call the second one, take the
                /// union of their output types.
                output_types: self_output_types.union(next_output_types).cloned().collect(),
            },
        };

        from_fn_with_description(
            required_update_kinds_set,
            move |event, cont| {
                let this = self.clone();
                let next = next.clone();

                this.execute(event, |event| next.execute(event, cont))
            },
            new_sig,
        )
    }

    /// Chain two handlers to make a tree of responsibility.
    ///
    /// Chaining is different from branching. See ["The difference between
    /// chaining and
    /// branching"](#the-difference-between-chaining-and-branching).
    ///
    /// # Type inference algorithm
    ///
    /// This method makes use of [`HandlerSignature`] (stored in every handler)
    /// to perform run-time type inference of the resulting handler
    /// signature; this information is used by [`crate::type_check`] to make
    /// sure that all required types are provided _before_ execution.
    ///
    /// The algorithm is as follows:
    ///  1. If the second handler's signature is [`HandlerSignature::Entry`],
    /// then **panic**.
    ///  2. If the first handler's signature is [`HandlerSignature::Entry`] and
    /// that of the second one is [`HandlerSignature::Other`], then the
    /// signature of the resulting handler is taken from the second one.
    ///  3. If the first handler's signature is [`HandlerSignature::Other`] and
    /// that the second one is also [`HandlerSignature::Other`], then the
    /// signature of the resulting handler is `HandlerSignature::Other {
    /// input_types: self_input_types UNION next_input_types, output_types:
    /// next_output_types INTERSECTION self_output_types }`.
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

        let new_sig = match (&self.data.sig, &next.data.sig) {
            (_, HandlerSignature::Entry) => {
                panic!("Ill-typed handler branch: the second handler cannot be an entry")
            }
            (HandlerSignature::Entry, HandlerSignature::Other { .. }) => next.data.sig.clone(),
            (
                HandlerSignature::Other {
                    input_types: self_input_types,
                    output_types: self_output_types,
                },
                HandlerSignature::Other {
                    input_types: next_input_types,
                    output_types: next_output_types,
                },
            ) => {
                HandlerSignature::Other {
                    // Since any of the two handlers can end up being executed, take the union of
                    // the input types of both handlers.
                    input_types: self_input_types.union(next_input_types).cloned().collect(),

                    // Since any of the two handlers can end up being executed, take the
                    // intersection of the output types of both handlers.
                    output_types: next_output_types
                        .intersection(self_output_types)
                        .cloned()
                        .collect(),
                }
            }
        };

        from_fn_with_description(
            required_update_kinds_set,
            move |event, cont| {
                let this = self.clone();
                let next = next.clone();

                this.execute(event, |event| async move {
                    match next.dispatch(event).await {
                        ControlFlow::Continue(event) => cont(event).await,
                        done => done,
                    }
                })
            },
            new_sig,
        )
    }

    /// Executes this handler with a continuation.
    ///
    /// Usually, you do not want to call this method by yourself, if you do not
    /// write your own handler implementation. If you wish to execute handler
    /// without a continuation, take a look at the [`Handler::dispatch`] method.
    ///
    /// **NOTE**: If you are using [`DependencyMap`], you should first call
    /// [`crate::type_check`] to type-check your handler against a container;
    /// otherwise, type checking will be delayed until the actual execution,
    /// which can make things harder to debug.
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
        input: Input,
        cont: Cont,
    ) -> ControlFlow<Output, Input>
    where
        Cont: FnOnce(Input) -> ContFut,
        Cont: Send + Sync + 'a,
        ContFut: Future<Output = ControlFlow<Output, Input>> + Send + 'a,
    {
        (self.data.f)(input, Box::new(|event| Box::pin(cont(event)))).await
    }

    /// Executes this handler.
    ///
    /// Returns [`ControlFlow::Break`] when executed successfully,
    /// [`ControlFlow::Continue`] otherwise.
    ///
    /// **NOTE**: If you are using [`DependencyMap`], you should first call
    /// [`crate::type_check`] to type-check your handler against a container;
    /// otherwise, type checking will be delayed until the actual execution,
    /// which can make things harder to debug.
    pub async fn dispatch(&self, input: Input) -> ControlFlow<Output, Input> {
        self.clone().execute(input, |event| async move { ControlFlow::Continue(event) }).await
    }

    /// Returns the set of updates that can be processed by this handler.
    pub fn description(&self) -> &Descr {
        &self.data.description
    }

    /// Returns this handler's run-time type signature.
    pub fn sig(&self) -> &HandlerSignature {
        &self.data.sig
    }
}

fn print_rt_types<'a>(types: impl IntoIterator<Item = &'a Type>) -> String {
    let mut res = String::new();
    let mut types = types.into_iter();

    if let Some(ty) = types.next() {
        write!(res, "{}", ty.name).unwrap();
    }
    for ty in types {
        write!(res, ", {}", ty.name).unwrap();
    }

    res
}

impl Type {
    /// Constructs [`Type`] from a static type.
    #[must_use]
    pub fn of<T>() -> Self
    where
        T: 'static,
    {
        Self { id: TypeId::of::<T>(), name: std::any::type_name::<T>() }
    }
}

/// Constructs a handler from a function.
///
/// `sig` is the run-time handler signature used for type inference and
/// checking.
///
/// Most of the time, you do not want to use this function. Take a look at more
/// specialised functions: [`crate::endpoint`], [`crate::filter`],
/// [`crate::filter_map`], etc.
#[must_use]
pub fn from_fn<'a, F, Fut, Input, Output, Descr>(
    f: F,
    sig: HandlerSignature,
) -> Handler<'a, Input, Output, Descr>
where
    F: Fn(Input, Cont<'a, Input, Output>) -> Fut,
    F: Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, Input>> + Send + 'a,
    Descr: HandlerDescription,
{
    from_fn_with_description(Descr::user_defined(), f, sig)
}

/// [`from_fn`] with a custom description.
#[must_use]
pub fn from_fn_with_description<'a, F, Fut, Input, Output, Descr>(
    description: Descr,
    f: F,
    sig: HandlerSignature,
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
            sig,
        }),
    }
}

/// Constructs an entry point handler.
///
/// This function is only used to specify other handlers upon it (see the root
/// examples).
///
/// # Signature
///
/// The run-time type signature of this handler is `HandlerSignature::Entry`.
#[must_use]
#[track_caller]
pub fn entry<'a, Input, Output, Descr>() -> Handler<'a, Input, Output, Descr>
where
    Input: Send + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    from_fn_with_description(Descr::entry(), |event, cont| cont(event), HandlerSignature::Entry)
}

/// Performs run-time type checking.
///
/// If you are using [`DependencyMap`], you should call this function _before_
/// executing the handler via [`Handler::execute`] or [`Handler::dispatch`]. If
/// you do _not_ call this function before execution, type checking will be
/// delayed until the actual execution, which can make things harder to debug.
///
/// `sig` can be obtained from [`Handler::sig`].
///
/// # Panics
///
/// If `container` does not contain all the types that the handler accepts.
///
/// # Examples
///
/// ```should_panic
/// use dptree::prelude::*;
///
/// #[derive(Clone)]
/// struct A;
/// #[derive(Clone)]
/// struct B;
///
/// // Requires both `A` and `B` to be present in the container.
/// let handler: Handler<DependencyMap, ()> =
///     dptree::entry().filter(|_: A| true).endpoint(|_: B| async { () });
///
/// // Ok.
/// dptree::type_check(handler.sig(), &dptree::deps![A, B]);
///
/// // Panics with a proper message.
/// dptree::type_check(handler.sig(), &dptree::deps![A /* Missing `B` */]);
/// ```
pub fn type_check(sig: &HandlerSignature, container: &DependencyMap) {
    match sig {
        HandlerSignature::Entry => {}
        HandlerSignature::Other { input_types, output_types: _ } => {
            let container_types = container
                .map
                .iter()
                .map(|(type_id, dep)| Type { id: *type_id, name: dep.type_name })
                .collect::<HashSet<_>>();

            if !input_types.is_subset(&container_types) {
                panic!(
                    "This handler accepts the following types:\n    {}\n, but only the following \
                     types are provided:\n    {}\nThe missing types are:\n    {}",
                    print_rt_types(input_types),
                    print_rt_types(&container_types),
                    print_rt_types(input_types.difference(&container_types))
                );
            }
        }
    }
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

        let result = help_inference(from_fn(
            |event, _cont: Cont<i32, &'static str>| async move {
                assert_eq!(event, input);
                ControlFlow::Break(output)
            },
            HandlerSignature::Other {
                input_types: HashSet::from([Type::of::<i32>()]),
                output_types: HashSet::from([]),
            },
        ))
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_from_fn_continue() {
        let input = 123;
        type Output = &'static str;

        let result = help_inference(from_fn(
            |event: i32, _cont: Cont<i32, &'static str>| async move {
                assert_eq!(event, input);
                ControlFlow::<Output, _>::Continue(event)
            },
            HandlerSignature::Other {
                input_types: HashSet::from([Type::of::<i32>()]),
                output_types: HashSet::from([Type::of::<i32>()]),
            },
        ))
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

        let result = help_inference(from_fn(
            |event, cont| {
                assert!(event == input);
                cont(event)
            },
            HandlerSignature::Other {
                input_types: HashSet::from([Type::of::<i32>()]),
                output_types: HashSet::from([Type::of::<i32>()]),
            },
        ))
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

        // Filters do not observe anything on their own.
        assert(filter_a(), hashset! {});
        assert(entry().chain(filter_b()), hashset! {});
        assert(filter_a().chain(filter_b()), hashset! {});
        assert(filter_a().branch(filter_b()), hashset! {});
        assert(filter_a().branch(filter_b()).branch(filter_c().chain(filter_c())), hashset! {});

        // Anything user-defined observes everything that it can observe.
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

        // An entry is "invisible".
        assert(entry(), hashset! {});
        assert(entry().chain(filter_a().endpoint(|| async {})), hashset! { A });
        assert(entry().branch(filter_a()), hashset! {});

        // Chained non-overlapping filters do not allow anything.
        assert(filter_a().chain(filter_b()).endpoint(|| async {}), hashset! {});
    }

    #[tokio::test]
    async fn test_type_check() {
        #[derive(Clone)]
        struct A;
        #[derive(Clone)]
        struct B;
        #[derive(Clone)]
        struct C;

        let deps = deps![A, B, C];

        // Type checking an entry must always succeed.
        type_check(&HandlerSignature::Entry, &deps);

        // The case for the same handler's input types and those in the container.
        type_check(
            &HandlerSignature::Other {
                input_types: HashSet::from([Type::of::<A>(), Type::of::<B>(), Type::of::<C>()]),
                output_types: HashSet::from([]),
            },
            &deps,
        );

        // The case for handler's input types and a superset thereof in the container.
        type_check(
            &HandlerSignature::Other {
                input_types: HashSet::from([Type::of::<A>(), Type::of::<B>()]),
                output_types: HashSet::from([]),
            },
            &deps,
        );
    }

    #[test]
    // Cannot specify the exact panic message because `HashSet` is iterated in a random order.
    #[should_panic]
    fn type_check_panic() {
        #[derive(Clone)]
        struct A;
        #[derive(Clone)]
        struct B;
        #[derive(Clone)]
        struct C;

        type_check(
            &HandlerSignature::Other {
                input_types: HashSet::from([Type::of::<A>(), Type::of::<B>(), Type::of::<C>()]),
                output_types: HashSet::from([]),
            },
            // `C` is required but not provided.
            &deps![A, B],
        );
    }

    #[tokio::test]
    async fn type_infer_check_chained_combinators() {
        #[derive(Clone)]
        struct A;
        #[derive(Clone)]
        struct B;
        #[derive(Clone)]
        struct C;
        #[derive(Clone)]
        struct D;
        #[derive(Clone)]
        struct E;
        #[derive(Clone)]
        struct F;
        #[derive(Clone)]
        struct G;
        #[derive(Clone, Debug, Eq, PartialEq)]
        struct H;

        let h: Handler<DependencyMap, H> = entry()
            .map(|/* In the final input types. */ _: A| B)
            .inspect(
                |/* This type must be removed from the final input types because it is
                  * provided by the `.map` above. */
                 _: B,
                 /* This type must "propagate" to the final input types. */
                 _: E| (),
            )
            .filter_map(|/* In the final input types. */ _: C| Some(D))
            .filter(
                |/* This type is provided by the `.filter_map` above. */ _: D,
                 /* Must propagate. */ _: F| true,
            )
            .endpoint(
                /* `B` and `D` are provided at this point. */
                |_: B, _: D, /* Must propagate. */ _: G| async { H },
            );

        assert_eq!(
            h.sig(),
            &HandlerSignature::Other {
                input_types: HashSet::from([
                    Type::of::<A>(),
                    Type::of::<C>(),
                    Type::of::<E>(),
                    Type::of::<F>(),
                    Type::of::<G>()
                ]),
                output_types: HashSet::from([Type::of::<B>(), Type::of::<D>()])
            }
        );

        let deps = deps![A, C, E, F, G];

        type_check(h.sig(), &deps);

        // Must not panic during execution.
        assert_eq!(h.dispatch(deps).await, ControlFlow::Break(H));
    }

    #[tokio::test]
    async fn type_infer_check_branched_combinators() {
        #[derive(Clone)]
        struct A;
        #[derive(Clone)]
        struct B;
        #[derive(Clone)]
        struct C;
        #[derive(Clone)]
        struct D;
        #[derive(Clone)]
        struct E;
        #[derive(Clone, Debug, Eq, PartialEq)]
        struct F;

        let h: Handler<DependencyMap, F> = entry()
            .branch(
                crate::inspect(|_: A| ()).map(|| B).map(|| C).map(|| D).endpoint(|| async { F }),
            )
            .branch(crate::inspect(|_: E| ()).map(|| B).map(|| D).endpoint(|| async { F }));

        assert_eq!(
            h.sig(),
            &HandlerSignature::Other {
                // The union of the input types of both branches.
                input_types: HashSet::from([Type::of::<A>(), Type::of::<E>(),]),
                // The intersection of the output types of both branches.
                output_types: HashSet::from([Type::of::<B>(), Type::of::<D>()])
            }
        );

        let deps = deps![A, E];

        type_check(h.sig(), &deps);

        // Must not panic during execution.
        assert_eq!(h.dispatch(deps).await, ControlFlow::Break(F));
    }

    #[test]
    #[should_panic(expected = "Ill-typed handler chain: the second handler cannot be an entry")]
    fn chain_entry() {
        let _: Handler<(), ()> = entry().chain(entry());
    }

    #[test]
    #[should_panic(expected = "Ill-typed handler branch: the second handler cannot be an entry")]
    fn branch_entry() {
        let _: Handler<(), ()> = entry().branch(entry());
    }
}
