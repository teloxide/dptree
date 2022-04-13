use crate::{di::Injectable, from_fn, Handler};
use futures::FutureExt;
use std::{ops::ControlFlow, sync::Arc};

impl<'a, Input, Output> Handler<'a, Input, Output>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
{
    /// Chain this handler with the endpoint handler `f`.
    #[must_use]
    pub fn endpoint<F, FnArgs>(self, f: F) -> Endpoint<'a, Input, Output>
    where
        F: Injectable<Input, Output, FnArgs> + Send + Sync + 'a,
    {
        self.chain(endpoint(f))
    }
}

/// Constructs a handler that has no further handlers in a chain.
///
/// An endpoint is a handler that _always_ breaks handler execution after its
/// completion. So, you can use it when your chain of responsibility must end
/// up, and handle an incoming event.
#[must_use]
pub fn endpoint<'a, F, Input, Output, FnArgs>(f: F) -> Endpoint<'a, Input, Output>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    F: Injectable<Input, Output, FnArgs> + Send + Sync + 'a,
{
    let f = Arc::new(f);

    from_fn(move |x, _cont| {
        let f = Arc::clone(&f);
        async move {
            let f = f.inject(&x);
            f().map(ControlFlow::Break).await
        }
    })
}

/// A handler with no further handlers in a chain.
pub type Endpoint<'a, Input, Output> = Handler<'a, Input, Output>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::deps;

    #[tokio::test]
    async fn test_endpoint() {
        let input = 123;
        let output = 7;

        let result = endpoint(move |num: i32| async move {
            assert_eq!(num, input);
            output
        })
        .dispatch(deps![input])
        .await;

        let result = match result {
            ControlFlow::Break(b) => b,
            _ => panic!("Unexpected: handler return ControlFlow::Break"),
        };
        assert_eq!(result, output);
    }
}
