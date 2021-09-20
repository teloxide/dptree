use crate::handler::core::{from_fn, Handler};
use std::{future::Future, ops::ControlFlow, sync::Arc};

impl<'a, Input, Output, Cont>
    Handler<
        'a,
        Input,
        Output,
        (
            Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)>,
            (Handler<'a, Input, Output, Cont>, Cont),
        ),
    >
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Cont: Send + Sync + 'a,
{
    pub fn filter<F, Fut>(
        self,
        f: F,
    ) -> Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)>
    where
        F: Fn(&Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = bool> + Send + Sync,
    {
        self.pipe_to(filter::<_, _, Input, Output, Cont>(f))
    }
}

pub fn filter<'a, Pred, Fut, Input, Output, Cont>(
    pred: Pred,
) -> Handler<'a, Input, Output, (Handler<'a, Input, Output, Cont>, Cont)>
where
    Pred: Fn(&Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = bool> + Send + Sync,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Cont: Send + Sync + 'a,
{
    let pred = Arc::new(pred);

    from_fn(move |event, (next, cont): (Handler<'a, Input, Output, Cont>, Cont)| {
        let pred = Arc::clone(&pred);

        async move {
            if pred(&event).await {
                next.execute(event, cont).await
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
        .handle(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }

    #[tokio::test]
    async fn test_and_then_filter() {
        let input = 123;
        let output = 7;

        let result = filter(|&event| async move {
            assert_eq!(event, input);
            true
        })
        .filter(|&event| async move {
            assert_eq!(event, input);
            true
        })
        .endpoint(|event| async move {
            assert_eq!(event, input);
            output
        })
        .handle(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }
}
