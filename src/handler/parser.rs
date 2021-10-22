use std::{ops::ControlFlow, sync::Arc};

use futures::Future;

use crate::{from_fn, Handler};

pub fn parser<'a, Projection, Fut, Input, Output, Intermediate>(
    proj: Projection,
) -> Handler<'a, Input, Output, Intermediate>
where
    Projection: Fn(&Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = Option<Intermediate>> + Send + Sync,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Intermediate: Send + Sync + 'a,
{
    let proj = Arc::new(proj);

    from_fn(move |event, cont| {
        let proj = Arc::clone(&proj);

        async move {
            match proj(&event).await {
                Some(intermediate) => match cont(intermediate).await {
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
        let output = "abc";

        let result = parser(|&event| async move {
            assert_eq!(event, input);
            Some(output)
        })
        .endpoint(|event| async move {
            assert_eq!(event, output);
            output
        })
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_none() {
        let input = 123;

        let result = parser(|&event| async move {
            assert_eq!(event, input);
            None::<String>
        })
        .endpoint(|_event| async move { unreachable!() })
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }

    #[tokio::test]
    async fn test_chain_some() {
        let input = 123;
        let output = "abc";

        let result = parser(|&event| async move {
            assert_eq!(event, input);
            Some(event)
        })
        .chain(
            parser(|&event| async move {
                assert_eq!(event, input);
                Some(output)
            })
            .endpoint(|event| async move {
                assert_eq!(event, output);
                output
            }),
        )
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_chain_none() {
        let input = 123;

        let result = parser(|&event| async move {
            assert_eq!(event, input);
            Some(event)
        })
        .chain(
            parser(|&event| async move {
                assert_eq!(event, input);
                None::<&'static str>
            })
            .endpoint(|_event| async move { unreachable!() }),
        )
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Continue(input));
    }
}
