use crate::{
    di::{Asyncify, Injectable},
    from_fn_with_description,
    send::{MaybeSend, MaybeSync},
    Fallible, Handler, HandlerDescription, HandlerSignature, Type,
};

use std::{collections::BTreeSet, iter::FromIterator, ops::ControlFlow, sync::Arc};

/// Constructs a fallible handler that passes a value of a new type further.
///
/// Like [`map`](crate::map), the result of invoking `proj` is added to the
/// container and passed further down the chain, but `proj` returns
/// [`Result<NewType, E>`]:
///
///  - `Ok(v)`: `v` is inserted into the container and the continuation
///    is called.
///  - `Err(e)`: the handler short-circuits and returns
///    [`ControlFlow::Break`] with an error value built from `e`, without
///    calling the continuation.
///
/// The handler `Output` must be fallible (i.e. implement [`Fallible`]); in
/// practice this means it is a `Result<T, E>`, and `E` is the error type that
/// `proj` returns on failure.
///
/// See also: [`try_filter_map`](crate::try_filter_map).
#[must_use]
#[track_caller]
pub fn try_map<'a, Projection, Output, NewType, Args, Descr>(
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Projection>:
        Injectable<Result<NewType, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
    Descr: HandlerDescription,
    NewType: Send + Sync + 'static,
{
    try_map_with_description(Descr::try_map(), proj)
}

/// The asynchronous version of [`try_map`].
#[must_use]
#[track_caller]
pub fn try_map_async<'a, Projection, Output, NewType, Args, Descr>(
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Projection: Injectable<Result<NewType, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
    Descr: HandlerDescription,
    NewType: Send + Sync + 'static,
{
    try_map_async_with_description(Descr::try_map_async(), proj)
}

/// [`try_map`] with a custom description.
#[must_use]
#[track_caller]
pub fn try_map_with_description<'a, Projection, Output, NewType, Args, Descr>(
    description: Descr,
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Projection>:
        Injectable<Result<NewType, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
    NewType: Send + Sync + 'static,
{
    try_map_async_with_description(description, Asyncify(proj))
}

/// [`try_map_async`] with a custom description.
#[must_use]
#[track_caller]
pub fn try_map_async_with_description<'a, Projection, Output, NewType, Args, Descr>(
    description: Descr,
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Projection: Injectable<Result<NewType, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
    NewType: Send + Sync + 'static,
{
    let proj = Arc::new(proj);

    from_fn_with_description(
        description,
        move |container, cont| {
            let proj = Arc::clone(&proj);

            async move {
                let proj = proj.inject(&container);
                let res = proj().await;
                drop(proj);

                match res {
                    Ok(new_type) => {
                        let mut intermediate = container.clone();
                        intermediate.insert(new_type);
                        match cont(intermediate).await {
                            ControlFlow::Continue(_) => ControlFlow::Continue(container),
                            ControlFlow::Break(result) => ControlFlow::Break(result),
                        }
                    }
                    Err(err) => ControlFlow::Break(Output::from_error(err)),
                }
            }
        },
        HandlerSignature::Other {
            obligations: Projection::obligations(),
            guaranteed_outcomes: BTreeSet::from_iter(vec![Type::of::<NewType>()]),
            conditional_outcomes: BTreeSet::new(),
            continues: true,
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{deps, help_inference};

    type Out = Result<i32, &'static str>;

    crate::cross_test! {
        async fn ok_inserts_and_continues() {
            let result = help_inference::<Out>(try_map(move || Ok(123)))
                .endpoint(move |event: i32| async move {
                    assert_eq!(event, 123);
                    Ok(event)
                })
                .dispatch(deps![])
                .await;

            assert_eq!(result, ControlFlow::Break(Ok(123)));
        }

        async fn err_short_circuits_past_endpoint() {
            let result = help_inference::<Out>(try_map(|| Err::<i32, _>("nope")))
                .endpoint(|| async move { unreachable!() })
                .dispatch(deps![])
                .await;

            assert_eq!(result, ControlFlow::Break(Err("nope")));
        }

        async fn async_variant() {
            let result = help_inference::<Out>(try_map_async(move || async move { Ok(123) }))
                .endpoint(move |event: i32| async move { Ok(event) })
                .dispatch(deps![])
                .await;

            assert_eq!(result, ControlFlow::Break(Ok(123)));
        }
    }
}
