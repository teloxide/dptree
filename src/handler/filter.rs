use crate::{
    di::{Asyncify, Injectable},
    handler::core::{from_fn, Handler},
    UpdateSet,
};
use std::{ops::ControlFlow, sync::Arc};

/// Constructs a handler that filters input with the predicate `pred`.
///
/// `pred` has an access to all values that are stored in the input container.
/// If it returns `true`, a continuation of the handler will be called,
/// otherwise the handler returns [`ControlFlow::Continue`].
#[must_use]
pub fn filter<'a, Pred, Input, Output, FnArgs, UpdSet>(
    pred: Pred,
) -> Handler<'a, Input, Output, UpdSet>
where
    Asyncify<Pred>: Injectable<Input, bool, FnArgs> + Send + Sync + 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    UpdSet: UpdateSet,
{
    filter_async(Asyncify(pred))
}

/// The asynchronous version of [`filter`].
#[must_use]
pub fn filter_async<'a, Pred, Input, Output, FnArgs, UpdSet>(
    pred: Pred,
) -> Handler<'a, Input, Output, UpdSet>
where
    Pred: Injectable<Input, bool, FnArgs> + Send + Sync + 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    UpdSet: UpdateSet,
{
    let pred = Arc::new(pred);

    from_fn(move |event, cont| {
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
    })
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
