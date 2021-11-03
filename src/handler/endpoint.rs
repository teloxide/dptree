use crate::{di_fn::IntoDiFn, from_fn, Handler};
use futures::FutureExt;
use std::{convert::Infallible, ops::ControlFlow};

impl<'a, Input, Output, Intermediate> Handler<'a, Input, Output, Intermediate>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Intermediate: Send + Sync + 'a,
{
    /// Chain self handler with `endpoint` handler.
    pub fn endpoint<F, FnArgs>(self, endp: F) -> Endpoint<'a, Input, Output>
    where
        F: IntoDiFn<Intermediate, Output, FnArgs>,
    {
        self.chain(endpoint(endp))
    }
}

/// Create endpoint handler.
///
/// Endpoint is a handler that _always_ break execution after its completion.
pub fn endpoint<'a, F, Input, Output, FnArgs>(f: F) -> Endpoint<'a, Input, Output>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    F: IntoDiFn<Input, Output, FnArgs>,
{
    let func = f.into();
    from_fn(move |x, _cont| {
        let func = func.clone();
        async move {
            let x = x;
            let func2 = func;
            let res = func2(&x).map(ControlFlow::Break).await;
            res
        }
    })
}

pub type Endpoint<'a, Input, Output> = Handler<'a, Input, Output, Infallible>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::container::TypeMapDi;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_endpoint() {
        let input = 123;
        let output = 7;

        let mut store = TypeMapDi::new();
        store.insert(input);

        let result = endpoint(move |num: Arc<i32>| async move {
            assert_eq!(*num, input);
            output
        })
        .dispatch(store)
        .await;

        let result = match result {
            ControlFlow::Break(b) => b,
            _ => panic!("Unexpected: handler return ControlFlow::Break"),
        };
        assert_eq!(result, output);
    }
}
