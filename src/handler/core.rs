// In order not to break the unit tests if this file is edited, we place this
// constant right at the beginning.
#[cfg(test)]
const FIXED_LOCATION: &'static Location = Location::caller();

use crate::{description, prelude::DependencyMap, HandlerDescription};

use std::{
    any::TypeId,
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    fmt::Write,
    future::Future,
    hash::{Hash, Hasher},
    ops::ControlFlow,
    panic::Location,
    sync::Arc,
};

use futures::future::BoxFuture;

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
pub struct Handler<'a, Output, Descr = description::Unspecified> {
    data: Arc<HandlerData<Descr, DynFn<'a, Output>>>,
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
        /// The map from types that this handler accepts to locations where they
        /// are required in source code.
        obligations: BTreeMap<Type, &'static Location<'static>>,

        /// The set of types that this handler passes down the chain.
        outcomes: BTreeSet<Type>,
    },
}

/// A run-time representation of a type. Used only for run-time type inference
/// and checking of handler chains.
///
/// See [`crate::type_check`].
#[derive(Clone, Copy, Debug)]
pub struct Type {
    /// The unique type identifier.
    pub id: TypeId,

    /// The type name used for printing.
    pub name: &'static str,
}

impl Hash for Type {
    /// Hashing is done by type identifiers (type names are ignored).
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Type {
    /// Equality is done by type identifiers (type names are ignored).
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Type {}

impl PartialOrd for Type {
    /// The partial order is done by type names for better diagnostics.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Type {
    /// The total order is done by type names for better diagnostics.
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(other.name)
    }
}

type DynFn<'a, Output> =
    dyn Fn(DependencyMap, Cont<'a, Output>) -> HandlerResult<'a, Output> + Send + Sync + 'a;

/// A continuation representing the rest of a handler chain.
pub type Cont<'a, Output> =
    Box<dyn FnOnce(DependencyMap) -> HandlerResult<'a, Output> + Send + Sync + 'a>;

/// An output type produced by a handler.
pub type HandlerResult<'a, Output> = BoxFuture<'a, ControlFlow<Output, DependencyMap>>;

// `#[derive(Clone)]` obligates all type parameters to satisfy `Clone` as well,
// but we do not need it here because of `Arc`.
impl<Output, Descr> Clone for Handler<'_, Output, Descr> {
    fn clone(&self) -> Self {
        Handler { data: Arc::clone(&self.data) }
    }
}

impl<'a, Output, Descr> Handler<'a, Output, Descr>
where
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
    ///     then **panic**.
    ///  2. If the first handler's signature is [`HandlerSignature::Entry`] and
    ///     that of the second one is [`HandlerSignature::Other`], then the
    ///     signature of the resulting handler is taken from the second one.
    ///  3. If the first handler's signature is [`HandlerSignature::Other`] and
    ///     that the second one is also [`HandlerSignature::Other`], then the
    ///     signature of the resulting handler is `HandlerSignature::Other {
    ///     obligations: self_obligations UNION (next_obligations DIFFERENCE
    ///     self_outcomes), outcomes: self_outcomes UNION next_outcomes }`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[tokio::main]
    /// # async fn main() {
    /// use dptree::prelude::*;
    ///
    /// let handler: Handler<_> =
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
            (HandlerSignature::Entry, other_sig @ HandlerSignature::Other { .. }) => {
                other_sig.clone()
            }
            (
                HandlerSignature::Other { obligations: self_obligations, outcomes: self_outcomes },
                HandlerSignature::Other { obligations: next_obligations, outcomes: next_outcomes },
            ) => {
                HandlerSignature::Other {
                    // Take the union of (the first handler's input types) and (the difference
                    // between the second handler's input types and the first handler's output
                    // types). The difference is needed because the first handler can pass values to
                    // the second one by calling it. The first handler's obligations take priority
                    // over those of the second one.
                    obligations: next_obligations
                        .clone()
                        .into_iter()
                        .filter(|(ty, _location)| !self_outcomes.contains(ty))
                        .chain(self_obligations.clone())
                        .collect(),

                    // Since the first handler can call the second one, take the union of their
                    // output types. If the second handler is not called, the dependency map will
                    // not be touched, so there will be no type errors.
                    outcomes: self_outcomes.union(next_outcomes).cloned().collect(),
                }
            }
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
    ///     then **panic**.
    ///  2. If the first handler's signature is [`HandlerSignature::Entry`] and
    ///     that of the second one is [`HandlerSignature::Other`], then the
    ///     signature of the resulting handler is taken from the second one.
    ///  3. If the first handler's signature is [`HandlerSignature::Other`] and
    ///     that the second one is also [`HandlerSignature::Other`], then the
    ///     signature of the resulting handler is `HandlerSignature::Other {
    ///     obligations: self_obligations UNION next_obligations, outcomes:
    ///     next_outcomes INTERSECTION self_outcomes }`.
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
    /// let dispatcher: Handler<_> = dptree::entry()
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
            (HandlerSignature::Entry, other_sig @ HandlerSignature::Other { .. }) => {
                other_sig.clone()
            }
            (
                HandlerSignature::Other { obligations: self_obligations, outcomes: self_outcomes },
                HandlerSignature::Other { obligations: next_obligations, outcomes: next_outcomes },
            ) => {
                HandlerSignature::Other {
                    // Take the union of the input types of both handlers, because either of them
                    // (or both) can end up being executed. The first handler's obligations take
                    // priority over those of the second one.
                    obligations: next_obligations
                        .clone()
                        .into_iter()
                        .chain(self_obligations.clone())
                        .collect(),

                    // Since any of the two handlers can end up being executed, take the
                    // intersection of the output types of both handlers.
                    outcomes: next_outcomes.intersection(self_outcomes).cloned().collect(),
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
    /// let handler: Handler<_> = dptree::filter(|x: i32| x > 0);
    ///
    /// let output = handler.execute(dptree::deps![10], |_| async { ControlFlow::Break("done") }).await;
    /// assert_eq!(output, ControlFlow::Break("done"));
    ///
    /// # }
    /// ```
    pub async fn execute<Cont, ContFut>(
        self,
        input: DependencyMap,
        cont: Cont,
    ) -> ControlFlow<Output, DependencyMap>
    where
        Cont: FnOnce(DependencyMap) -> ContFut,
        Cont: Send + Sync + 'a,
        ContFut: Future<Output = ControlFlow<Output, DependencyMap>> + Send + 'a,
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
    pub async fn dispatch(&self, input: DependencyMap) -> ControlFlow<Output, DependencyMap> {
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

fn print_types<'a>(
    types: impl IntoIterator<Item = &'a Type>,
    f: impl Fn(&Type) -> String,
) -> String {
    let mut res = String::new();
    let mut types = types.into_iter();

    if let Some(ty) = types.next() {
        write!(res, "{}", f(ty)).unwrap();
    }
    for ty in types {
        write!(res, "\n    {}", f(ty)).unwrap();
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
pub fn from_fn<'a, F, Fut, Output, Descr>(f: F, sig: HandlerSignature) -> Handler<'a, Output, Descr>
where
    F: Fn(DependencyMap, Cont<'a, Output>) -> Fut,
    F: Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, DependencyMap>> + Send + 'a,
    Descr: HandlerDescription,
{
    from_fn_with_description(Descr::user_defined(), f, sig)
}

/// [`from_fn`] with a custom description.
#[must_use]
pub fn from_fn_with_description<'a, F, Fut, Output, Descr>(
    description: Descr,
    f: F,
    sig: HandlerSignature,
) -> Handler<'a, Output, Descr>
where
    F: Fn(DependencyMap, Cont<'a, Output>) -> Fut,
    F: Send + Sync + 'a,
    Fut: Future<Output = ControlFlow<Output, DependencyMap>> + Send + 'a,
{
    Handler {
        data: Arc::new(HandlerData {
            f: move |event, cont| Box::pin(f(event, cont)) as HandlerResult<_>,
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
pub fn entry<'a, Output, Descr>() -> Handler<'a, Output, Descr>
where
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
/// If `container` does not contain all the types that the handler accepts; in
/// this case, helpful diagnostic information about missing types and code
/// locations is displayed.
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
/// let handler: Handler<()> = dptree::entry().filter(|_: A| true).endpoint(|_: B| async { () });
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
        HandlerSignature::Other { obligations, outcomes: _ } => {
            let container_types = container
                .map
                .iter()
                .map(|(type_id, dep)| Type { id: *type_id, name: dep.type_name })
                .collect::<BTreeSet<_>>();

            if obligations.iter().any(|(ty, _location)| !container_types.contains(ty)) {
                panic!(
                    "This handler accepts the following types:\n    {}\n, but only the following \
                     types are provided:\n    {}\nThe missing types are:\n    {}",
                    print_types(obligations.keys(), |ty| format!("`{}`", ty.name)),
                    print_types(&container_types, |ty| format!("`{}`", ty.name)),
                    print_types(
                        obligations.iter().filter_map(|(ty, _location)| {
                            if !container_types.contains(ty) {
                                Some(ty)
                            } else {
                                None
                            }
                        }),
                        |ty| { format!("`{}` from {}", ty.name, obligations[ty]) },
                    )
                );
            }
        }
    }
}

#[cfg(test)]
pub(crate) fn help_inference<Output>(h: Handler<Output>) -> Handler<Output> {
    h
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        deps, description, filter_map, filter_map_with_description,
        handler::{endpoint, filter, filter_async},
    };

    use std::{collections::HashSet, iter::FromIterator};

    use maplit::{btreemap, btreeset, hashset};

    #[tokio::test]
    async fn test_from_fn_break() {
        let input = 123;
        let output = "ABC";

        let input_types = vec![Type::of::<i32>()];
        let location = Location::caller();

        let result = help_inference(from_fn(
            |event, _cont: Cont<&'static str>| async move {
                assert_eq!(event, deps![input]);
                ControlFlow::Break(output)
            },
            HandlerSignature::Other {
                obligations: BTreeMap::from_iter(
                    input_types.iter().cloned().map(|ty| (ty, location)),
                ),
                outcomes: btreeset! {},
            },
        ))
        .dispatch(deps![input])
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_from_fn_continue() {
        let input = 123;
        type Output = &'static str;

        let input_types = vec![Type::of::<i32>()];
        let location = Location::caller();

        let result = help_inference(from_fn(
            |event, _cont: Cont<&'static str>| async move {
                assert_eq!(event, deps![input]);
                ControlFlow::<Output, _>::Continue(event)
            },
            HandlerSignature::Other {
                obligations: BTreeMap::from_iter(
                    input_types.iter().cloned().map(|ty| (ty, location)),
                ),
                outcomes: btreeset! {Type::of::<i32>()},
            },
        ))
        .dispatch(deps![input])
        .await;

        assert!(result == ControlFlow::Continue(deps![input]));
    }

    #[tokio::test]
    async fn test_entry() {
        let input = 123;
        type Output = &'static str;

        let result = help_inference(entry::<Output, _>()).dispatch(deps![input]).await;

        assert!(result == ControlFlow::Continue(deps![input]));
    }

    #[tokio::test]
    async fn test_execute() {
        let input = 123;
        let output = "ABC";

        let input_types = vec![Type::of::<i32>()];
        let location = Location::caller();

        let result = help_inference(from_fn(
            |event, cont| {
                assert!(event == deps![input]);
                cont(event)
            },
            HandlerSignature::Other {
                obligations: BTreeMap::from_iter(
                    input_types.iter().cloned().map(|ty| (ty, location)),
                ),
                outcomes: btreeset! {Type::of::<i32>()},
            },
        ))
        .execute(deps![input], |event| async move {
            assert!(event == deps![input]);
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

        fn filter_a<Out>() -> Handler<'static, Out, InterestSet<UpdateKind>>
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

        fn filter_b<Out>() -> Handler<'static, Out, InterestSet<UpdateKind>>
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

        fn filter_c<Out>() -> Handler<'static, Out, InterestSet<UpdateKind>>
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
        fn user_defined_filter<Out>() -> Handler<'static, Out, InterestSet<UpdateKind>>
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
            handler: Handler<'static, (), description::InterestSet<UpdateKind>>,
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
    async fn type_check_success() {
        #[derive(Clone)]
        struct A;
        #[derive(Clone)]
        struct B;
        #[derive(Clone)]
        struct C;

        macro_rules! test {
            ($($key:ty),*) => {
                type_check(
                    &HandlerSignature::Other {
                        obligations: btreemap! {
                            $(Type::of::<$key>() => Location::caller(),)*
                        },
                        outcomes: btreeset! {},
                    },
                    &deps![A, B, C],
                );
            };
        }

        // Type-checking an entry must succeed.
        type_check(&HandlerSignature::Entry, &deps![]);

        // Type-checking subsets of provided types must succeed.
        test!(A, B, C);
        test!(A, B);
        test!(A, C);
        test!(B, C);
        test!(A);
        test!(B);
        test!(C);
    }

    #[test]
    #[should_panic(expected = "This handler accepts the following types:
    `dptree::handler::core::tests::type_check_panic::A`
    `dptree::handler::core::tests::type_check_panic::B`
    `dptree::handler::core::tests::type_check_panic::C`
, but only the following types are provided:
    `dptree::handler::core::tests::type_check_panic::A`
    `dptree::handler::core::tests::type_check_panic::B`
The missing types are:
    `dptree::handler::core::tests::type_check_panic::C` from src/handler/core.rs:4:43")]
    fn type_check_panic() {
        #[derive(Clone)]
        struct A;
        #[derive(Clone)]
        struct B;
        #[derive(Clone)]
        struct C;

        type_check(
            &HandlerSignature::Other {
                obligations: btreemap! {
                    Type::of::<A>() => Location::caller(),
                    Type::of::<B>() => Location::caller(),
                    Type::of::<C>() => FIXED_LOCATION,
                },
                outcomes: btreeset! {},
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

        let h: Handler<H> = entry()
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

        let input_types = vec![
            Type::of::<A>(),
            Type::of::<C>(),
            Type::of::<E>(),
            Type::of::<F>(),
            Type::of::<G>(),
        ];
        let outcomes = btreeset! {Type::of::<B>(), Type::of::<D>()};

        if let HandlerSignature::Other {
            obligations: actual_obligations,
            outcomes: actual_outcomes,
        } = h.sig()
        {
            assert_eq!(
                actual_obligations.keys().collect::<Vec<_>>(),
                input_types.iter().collect::<Vec<_>>()
            );
            assert_eq!(actual_outcomes, &outcomes);
        } else {
            panic!("Expected `HandlerSignature::Other`");
        }

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

        let h: Handler<F> = entry()
            .branch(
                crate::inspect(|_: A| ()).map(|| B).map(|| C).map(|| D).endpoint(|| async { F }),
            )
            .branch(crate::inspect(|_: E| ()).map(|| B).map(|| D).endpoint(|| async { F }));

        // The union of the input types of both branches.
        let input_types = btreeset! {Type::of::<A>(), Type::of::<E>()};
        // The intersection of the output types of both branches.
        let output_types = btreeset! {Type::of::<B>(), Type::of::<D>()};

        if let HandlerSignature::Other {
            obligations: actual_obligations,
            outcomes: actual_outcomes,
        } = h.sig()
        {
            assert_eq!(
                actual_obligations.keys().collect::<Vec<_>>(),
                input_types.iter().collect::<Vec<_>>()
            );
            assert_eq!(actual_outcomes, &output_types);
        } else {
            panic!("Expected `HandlerSignature::Other`");
        }

        let deps = deps![A, E];

        type_check(h.sig(), &deps);

        // Must not panic during execution.
        assert_eq!(h.dispatch(deps).await, ControlFlow::Break(F));
    }

    #[tokio::test]
    async fn obligations_priority() {
        #[derive(Clone)]
        struct A;
        #[derive(Clone)]
        struct B;
        #[derive(Clone, Debug, Eq, PartialEq)]
        struct C;

        fn test<T: 'static>(h: Handler<C>, column: u32) {
            if let HandlerSignature::Other { obligations, outcomes: _ } = h.sig() {
                let (_ty, &location) = obligations
                    .iter()
                    .find(|(ty, _location)| ty.id == TypeId::of::<T>())
                    .expect("Missing obligation");
                assert_eq!(location.column(), column);
            } else {
                panic!("Expected `HandlerSignature::Other`");
            }
        }

        #[rustfmt::skip]
        let h: Handler<C> = entry().map(|_: A| ()).endpoint(|_: A, _: B| async { C });
        test::<A>(h, 37);

        #[rustfmt::skip]
        let h: Handler<C> =
            entry().branch(endpoint(|_: A| async { C })).branch(endpoint(|_: A, _: B| async { C }));
        test::<A>(h, 28);
    }

    #[test]
    #[should_panic(expected = "Ill-typed handler chain: the second handler cannot be an entry")]
    fn chain_entry() {
        let _: Handler<()> = entry().chain(entry());
    }

    #[test]
    #[should_panic(expected = "Ill-typed handler branch: the second handler cannot be an entry")]
    fn branch_entry() {
        let _: Handler<()> = entry().branch(entry());
    }
}
