use crate::{
    di_fn::IntoDiFn,
    handler::core::{from_fn, Handler},
};
use std::{ops::ControlFlow, sync::Arc};

/// Create a handler that filter input by some condition.
///
/// Condition can access to all values that are stored in the input container,
/// and must return `bool` value. If it returns `true`, continuation of the
/// handler will be called, otherwise handler returns `ControlFlow::Continue`.
///
/// Example:
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::{container::Value, prelude::*};
/// use std::ops::ControlFlow;
///
/// let handler = dptree::filter(|x: Arc<i32>| async move { *x > 0 }).endpoint(|| async { "done" });
///
/// assert_eq!(handler.dispatch(Value::new(10)).await, ControlFlow::Break("done"));
/// assert_eq!(handler.dispatch(Value::new(-10)).await, ControlFlow::Continue(Value::new(-10)));
///
/// # }
/// ```
pub fn filter<'a, Pred, Input, Output, FnArgs>(pred: Pred) -> Handler<'a, Input, Output>
where
    Pred: IntoDiFn<Input, bool, FnArgs>,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    let pred = pred.into();

    from_fn(move |event, cont| {
        let pred = Arc::clone(&pred);

        async move {
            if pred(&event).await {
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
    use crate::container::Value;

    #[tokio::test]
    async fn test_filter() {
        let input_value = 123;
        let input = Value::new(input_value);
        let output = 7;

        let result = filter(move |event: Arc<i32>| async move {
            assert_eq!(*event, input_value);
            true
        })
        .endpoint(move |event: Arc<i32>| async move {
            assert_eq!(*event, input_value);
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

        let result = filter(move |event: Arc<i32>| async move {
            assert_eq!(*event, input);
            true
        })
        .chain(
            filter(move |event: Arc<i32>| async move {
                assert_eq!(*event, input);
                true
            })
            .endpoint(move |event: Arc<i32>| async move {
                assert_eq!(*event, input);
                output
            }),
        )
        .dispatch(Value::new(input))
        .await;

        assert!(result == ControlFlow::Break(output));
    }
}
