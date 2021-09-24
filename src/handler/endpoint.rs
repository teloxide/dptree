use crate::{
    handler::core::{from_fn, Handler},
    PipeTo,
};
use std::{future::Future, ops::ControlFlow, sync::Arc};

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Cont>
where
    Self: PipeTo<'a, Input, Output, Endpoint<'a, Input, Output>>,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub fn endpoint<F, Fut>(
        self,
        endp: F,
    ) -> Handler<'a, Input, Output, Endpoint<'a, Input, Output>>
    where
        F: Fn(Input) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = Output> + Send + Sync,
    {
        self.pipe_to(endpoint(endp))
    }
}

pub fn endpoint<'a, F, Fut, Input, Output>(f: F) -> Endpoint<'a, Input, Output>
where
    F: Fn(Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = Output> + Send + Sync,
    Input: Send + Sync + 'a,
{
    let f = Arc::new(f);

    from_fn(move |event, _cont| {
        let f = Arc::clone(&f);
        async move { ControlFlow::Break(f(event).await) }
    })
}

pub type Endpoint<'a, Input, Output> = Handler<'a, Input, Output>;

#[cfg(test)]
mod tests {
    use super::*;

    use crate::handler::core::Handleable;

    // #[tokio::test]
    // async fn test_endpoint() {
    //     let input = 123;
    //     let output = 7;

    //     let result = endpoint(|event| async move {
    //         assert_eq!(event, input);
    //         output
    //     })
    //     .handle(input)
    //     .await;

    //     assert!(result == ControlFlow::Break(output));
    // }
}
