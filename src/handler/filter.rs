use crate::{
    handler::core::{from_fn, Handler},
    IntoDiFunc,
};
use std::{ops::ControlFlow, sync::Arc};

pub fn filter<'a, Pred, Input, Output, Args>(pred: Pred) -> Handler<'a, Input, Output>
where
    Pred: IntoDiFunc<Input, bool, Args>,
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
    use crate::di::Value;

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
