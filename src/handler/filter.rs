use crate::{
    handler::core::{from_fn, Handler},
    Handleable,
};
use std::{future::Future, ops::ControlFlow, sync::Arc};

pub fn filter<'a, Pred, Fut, Input, Output, Cont>(pred: Pred) -> Filter<'a, Input, Output, Cont>
where
    Pred: Fn(&Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = bool> + Send + Sync,
    Handler<'a, Input, Output, Cont>: Handleable<'a, Input, Output>,
    Cont: 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    let pred = Arc::new(pred);

    from_fn(move |event, cont: Handler<'a, Input, Output, Cont>| {
        let pred = Arc::clone(&pred);

        async move {
            if pred(&event).await {
                cont.dispatch(event).await
            } else {
                ControlFlow::Continue(event)
            }
        }
    })
}

pub type Filter<'a, Input, Output, Cont> =
    Handler<'a, Input, Output, Handler<'a, Input, Output, Cont>>;

#[cfg(test)]
mod tests {
    use super::*;

    use crate::TerminalCont;

    #[tokio::test]
    async fn test_filter() {
        let input = 123;
        let output = 7;

        let result = filter::<_, _, _, _, TerminalCont>(|&event| async move {
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

        let result = filter::<_, _, _, _, TerminalCont>(|&event| async move {
            assert_eq!(event, input);
            true
        })
        .chain(
            filter::<_, _, _, _, TerminalCont>(|&event| async move {
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
