use crate::{
    di::{Asyncify, Injectable},
    from_fn_with_description,
    send::{MaybeSend, MaybeSync},
    Fallible, Handler, HandlerDescription, HandlerSignature,
};

use std::{collections::BTreeSet, ops::ControlFlow, sync::Arc};

/// Constructs a fallible handler that filters input with the predicate `pred`.
///
/// Like [`filter`](crate::filter), `pred` has access to all values in the input
/// container, but it returns [`Result<bool, E>`] instead of `bool`:
///
///  - `Ok(true)`: the continuation is called (execution continues).
///  - `Ok(false)`: the handler returns [`ControlFlow::Continue`] (it
///    tries the next branch), just like `filter`.
///  - `Err(e)`: the handler short-circuits and returns
///    [`ControlFlow::Break`] with an error value built from `e`.
///
/// The `Err` case is symmetric to how [`endpoint`](crate::endpoint) breaks the
/// chain with a value, and unlike `Ok(false)` it does **not** fall through to
/// sibling branches.
///
/// The handler `Output` must be fallible (i.e. implement [`Fallible`]); in
/// practice this means it is a `Result<T, E>`, and `E` is the error type that
/// `pred` returns on failure.
#[must_use]
#[track_caller]
pub fn try_filter<'a, Pred, Output, FnArgs, Descr>(pred: Pred) -> Handler<'a, Output, Descr>
where
    Asyncify<Pred>: Injectable<Result<bool, Output::Error>, FnArgs> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
    Descr: HandlerDescription,
{
    try_filter_with_description(Descr::try_filter(), pred)
}

/// The asynchronous version of [`try_filter`].
#[must_use]
#[track_caller]
pub fn try_filter_async<'a, Pred, Output, FnArgs, Descr>(pred: Pred) -> Handler<'a, Output, Descr>
where
    Pred: Injectable<Result<bool, Output::Error>, FnArgs> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
    Descr: HandlerDescription,
{
    try_filter_async_with_description(Descr::try_filter_async(), pred)
}

/// [`try_filter`] with a custom description.
#[must_use]
#[track_caller]
pub fn try_filter_with_description<'a, Pred, Output, FnArgs, Descr>(
    description: Descr,
    pred: Pred,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Pred>: Injectable<Result<bool, Output::Error>, FnArgs> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
{
    try_filter_async_with_description(description, Asyncify(pred))
}

/// [`try_filter_async`] with a custom description.
#[must_use]
#[track_caller]
pub fn try_filter_async_with_description<'a, Pred, Output, FnArgs, Descr>(
    description: Descr,
    pred: Pred,
) -> Handler<'a, Output, Descr>
where
    Pred: Injectable<Result<bool, Output::Error>, FnArgs> + MaybeSend + MaybeSync + 'a,
    Output: Fallible + 'a,
    Output::Error: MaybeSend,
{
    let pred = Arc::new(pred);

    from_fn_with_description(
        description,
        move |event, cont| {
            let pred = Arc::clone(&pred);

            async move {
                let pred = pred.inject(&event);
                let res = pred().await;
                drop(pred);

                match res {
                    Ok(true) => cont(event).await,
                    Ok(false) => ControlFlow::Continue(event),
                    Err(err) => ControlFlow::Break(Output::from_error(err)),
                }
            }
        },
        HandlerSignature::Other {
            obligations: Pred::obligations(),
            guaranteed_outcomes: BTreeSet::default(),
            conditional_outcomes: BTreeSet::default(),
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
        async fn ok_true_continues_to_endpoint() {
            let result = help_inference::<Out>(try_filter(move |event: i32| {
                assert_eq!(event, 5);
                Ok(true)
            }))
            .endpoint(|| async move { Ok(7) })
            .dispatch(deps![5])
            .await;

            assert_eq!(result, ControlFlow::Break(Ok(7)));
        }

        async fn ok_false_continues_to_next_branch() {
            let result = help_inference::<Out>(try_filter(|| Ok(false)))
                .endpoint(|| async move { unreachable!() })
                .dispatch(deps![])
                .await;

            assert!(matches!(result, ControlFlow::Continue(_)));
        }

        async fn err_short_circuits_past_endpoint() {
            let result = help_inference::<Out>(try_filter(|| Err::<bool, _>("nope")))
                .endpoint(|| async move { unreachable!() })
                .dispatch(deps![])
                .await;

            assert_eq!(result, ControlFlow::Break(Err("nope")));
        }

        async fn async_variant() {
            let result = help_inference::<Out>(try_filter_async(move |event: i32| async move {
                assert_eq!(event, 5);
                Ok(event > 0)
            }))
            .endpoint(|| async move { Ok(7) })
            .dispatch(deps![5])
            .await;

            assert_eq!(result, ControlFlow::Break(Ok(7)));
        }
    }
}
