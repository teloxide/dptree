use std::{ops::ControlFlow, sync::Arc};

use futures::Future;

use crate::{from_fn, Handler};

pub fn parser<'a, Projection, Fut, Input, Output, Intermediate, Cont>(
    proj: Projection,
) -> Handler<'a, Input, Output, Cont>
where
    Projection: Fn(&Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = Option<Intermediate>> + Send + Sync,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Intermediate: Send + Sync + 'a,
{
    let proj = Arc::new(proj);

    from_fn(move |event, cont: Handler<'a, Intermediate, Output>| {
        let proj = Arc::clone(&proj);

        async move {
            match proj(&event).await {
                Some(intermediate) => match cont.dispatch(intermediate).await {
                    ControlFlow::Continue(_) => ControlFlow::Continue(event),
                    ControlFlow::Break(result) => ControlFlow::Break(result),
                },
                None => ControlFlow::Continue(event),
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_some() {
        let input = 123;
        let output = 7;

        let result = parser(|&event| async move {
            assert_eq!(event, input);
            Some(event)
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
    async fn test_none() {
        let input = 123;
        let output = 7;

        let result = parser(|&event| async move {
            assert_eq!(event, input);
            None
        })
        .endpoint(|event| async move {
            assert_eq!(event, input);
            output
        })
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_and_then_parser() {
        let input = 123;
        let output = 7;

        let result = parser(|&event| async move {
            assert_eq!(event, input);
            Some(event)
        })
        .chain(
            parser(|&event| async move {
                assert_eq!(event, input);
                None
            })
            .endpoint(|event| async move {
                assert_eq!(event, input);
                output
            }),
        )
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }
}
