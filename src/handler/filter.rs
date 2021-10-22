use crate::handler::core::{from_fn, Handler};
use std::{future::Future, ops::ControlFlow, sync::Arc};

pub fn filter<'a, Pred, Fut, Input, Output>(pred: Pred) -> Handler<'a, Input, Output>
where
    Pred: Fn(&Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = bool> + Send + Sync,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    let pred = Arc::new(pred);

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

    #[tokio::test]
    async fn test_filter() {
        let input = 123;
        let output = 7;

        let result = filter(|&event| async move {
            assert_eq!(event, input);
            true
        })
        .endpoint(|event| async move {
            assert_eq!(event, input);
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

        let result = filter(|&event: &i32| async move {
            assert_eq!(event, input);
            true
        })
        .chain(
            filter(|&event| async move {
                assert_eq!(event, input);
                true
            })
            .endpoint(|event| async move {
                assert_eq!(event, input);
                output
            }),
        )
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }
}
