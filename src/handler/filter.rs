use crate::{
    di::{Asyncify, Injectable},
    from_fn_with_description,
    handler::core::Handler,
    HandlerDescription, HandlerSignature,
};

use std::{ops::ControlFlow, sync::Arc};

use rustc_hash::FxHashSet;

/// Constructs a handler that filters input with the predicate `pred`.
///
/// `pred` has an access to all values that are stored in the input container.
/// If it returns `true`, a continuation of the handler will be called,
/// otherwise the handler returns [`ControlFlow::Continue`].
///
/// # Signature
///
/// The run-time type signature of this handler is `HandlerSignature::Other {
/// input_types: Pred::input_types(), output_types: FxHashSet::default() }`.
#[must_use]
#[track_caller]
pub fn filter<'a, Pred, Output, FnArgs, Descr>(pred: Pred) -> Handler<'a, Output, Descr>
where
    Asyncify<Pred>: Injectable<bool, FnArgs> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    filter_with_description(Descr::filter(), pred)
}

/// The asynchronous version of [`filter`].
#[must_use]
#[track_caller]
pub fn filter_async<'a, Pred, Output, FnArgs, Descr>(pred: Pred) -> Handler<'a, Output, Descr>
where
    Pred: Injectable<bool, FnArgs> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    filter_async_with_description(Descr::filter_async(), pred)
}

/// [`filter`] with a custom description.
#[must_use]
#[track_caller]
pub fn filter_with_description<'a, Pred, Output, FnArgs, Descr>(
    description: Descr,
    pred: Pred,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Pred>: Injectable<bool, FnArgs> + Send + Sync + 'a,
    Output: 'a,
{
    filter_async_with_description(description, Asyncify(pred))
}

/// [`filter_async`] with a custom description.
#[must_use]
#[track_caller]
pub fn filter_async_with_description<'a, Pred, Output, FnArgs, Descr>(
    description: Descr,
    pred: Pred,
) -> Handler<'a, Output, Descr>
where
    Pred: Injectable<bool, FnArgs> + Send + Sync + 'a,
    Output: 'a,
{
    let pred = Arc::new(pred);

    from_fn_with_description(
        description,
        move |event, cont| {
            let pred = Arc::clone(&pred);

            async move {
                let pred = pred.inject(&event);
                let cond = pred().await;
                drop(pred);

                if cond {
                    cont(event).await
                } else {
                    ControlFlow::Continue(event)
                }
            }
        },
        HandlerSignature::Other {
            input_types: Pred::input_types(),
            output_types: FxHashSet::default(),
            obligations: Pred::obligations(),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{deps, help_inference};

    #[tokio::test]
    async fn test_filter() {
        let input_value = 123;
        let input = deps![input_value];
        let output = 7;

        let result = help_inference(filter_async(move |event: i32| async move {
            assert_eq!(event, input_value);
            true
        }))
        .endpoint(move |event: i32| async move {
            assert_eq!(event, input_value);
            output
        })
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_and_then_filter() {
        let input = 123;
        let output = 7;

        let result = help_inference(filter(move |event: i32| {
            assert_eq!(event, input);
            true
        }))
        .chain(
            filter_async(move |event: i32| async move {
                assert_eq!(event, input);
                true
            })
            .endpoint(move |event: i32| async move {
                assert_eq!(event, input);
                output
            }),
        )
        .dispatch(deps![input])
        .await;

        assert!(result == ControlFlow::Break(output));
    }
}
