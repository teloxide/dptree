use std::{ops::ControlFlow, sync::Arc};

use crate::{container::DiContainer, from_fn, Handler};

pub fn parser<'a, Projection, Input, IT, OT, Output, Intermediate>(
    proj: Projection,
) -> Handler<'a, Input, Output, Intermediate>
where
    Input: Replace<IT, OT, Out = Intermediate> + DiContainer<IT>,
    Intermediate: Replace<OT, IT, Out = Input>,
    Projection: Fn(&IT) -> Option<OT> + Send + Sync + 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    OT: Send + Sync + 'static,
    IT: Send + Sync + 'static,
    Intermediate: Send + Sync + 'a,
{
    let proj = Arc::new(proj);

    from_fn(move |container: Input, cont| {
        let proj = Arc::clone(&proj);

        async move {
            match proj(&container.get()) {
                Some(ot) => {
                    let (intermediate, inp) = container.replace(Arc::new(ot));
                    match cont(intermediate).await {
                        ControlFlow::Continue(container) => {
                            ControlFlow::Continue(container.replace(inp).0)
                        }
                        ControlFlow::Break(result) => ControlFlow::Break(result),
                    }
                }
                None => ControlFlow::Continue(container),
            }
        }
    })
}

pub trait Replace<From, To> {
    type Out;
    fn replace(self, to: Arc<To>) -> (Self::Out, Arc<From>);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::container::Value;

    #[tokio::test]
    async fn test_some() {
        let input = 123;
        let output = "abc";

        let result = parser(move |&event| {
            assert_eq!(event, input);
            Some(output)
        })
        .endpoint(move |event: Arc<&'static str>| async move {
            assert_eq!(*event, output);
            output
        })
        .dispatch(Value::new(input))
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_none() {
        let input = 123;

        let result = parser(|&event| {
            assert_eq!(event, input);
            None::<String>
        })
        .endpoint(|| async move { unreachable!() })
        .dispatch(Value::new(input))
        .await;

        assert!(result == ControlFlow::Continue(Value::new(input)));
    }

    #[tokio::test]
    async fn test_chain_some() {
        let input = 123;
        let output = "abc";

        let result = parser(|&event| {
            assert_eq!(event, input);
            Some(event)
        })
        .chain(
            parser(|&event| {
                assert_eq!(event, input);
                Some(output)
            })
            .endpoint(move |event: Arc<&'static str>| async move {
                assert_eq!(*event, output);
                output
            }),
        )
        .dispatch(Value::new(input))
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_chain_none() {
        let input = 123;

        let result = parser(|&event| {
            assert_eq!(event, input);
            Some(event)
        })
        .chain(
            parser(|&event| {
                assert_eq!(event, input);
                None::<&'static str>
            })
            .endpoint(|| async move { unreachable!() }),
        )
        .dispatch(Value::new(input))
        .await;

        assert!(result == ControlFlow::Continue(Value::new(input)));
    }
}
