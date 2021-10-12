use crate::{entry, handler::core::Handler};

use std::{future::Future, ops::ControlFlow};

impl<'a, Input, Output, Cont> Handler<'a, Input, Output, Cont>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    pub fn endpoint<Intermediate, ContFut, F, Fut>(self, endp: F) -> Self
    where
        F: Fn(Intermediate) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = Output> + Send + Sync,
        Intermediate: Send + Sync + 'a,
        Cont: Fn(Intermediate) -> ContFut,
        ContFut: Future<Output = ControlFlow<Output, Intermediate>>,
    {
        self.chain(endpoint(endp))
    }
}

pub fn endpoint<'a, F, Fut, Input, Output, Intermediate, Cont, ContFut>(
    f: F,
) -> Handler<'a, Input, Output, Cont>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    F: Fn(Input) -> Fut + Send + Sync + 'a,
    Fut: Future<Output = Output> + Send + Sync,
    Input: Send + Sync + 'a,
    Cont: Fn(Intermediate) -> ContFut,
    ContFut: Future<Output = ControlFlow<Output, Intermediate>>,
{
    entry().chain(f)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_endpoint() {
        let input = 123;
        let output = 7;

        let result = endpoint(|event| async move {
            assert_eq!(event, input);
            output
        })
        .dispatch(input)
        .await;

        assert!(result == ControlFlow::Break(output));
    }
}
