use crate::{di::Injectable, from_fn, Handler};
use futures::FutureExt;
use std::{convert::Infallible, ops::ControlFlow, sync::Arc};

impl<'a, Input, Output, Intermediate> Handler<'a, Input, Output, Intermediate>
where
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Intermediate: Send + Sync + 'a,
{
    /// Chain this handler with the endpoint handler `endp`.
    #[must_use]
    pub fn endpoint<F, FnArgs>(self, endp: F) -> Endpoint<'a, Input, Output>
    where
        F: Injectable<Intermediate, Output, FnArgs> + Send + Sync + 'a,
    {
        self.chain(endpoint(endp))
    }
}

/// Constructs a handler that has no further handlers in a chain.
///
/// An endpoint is a handler that _always_ breaks handler execution after its
/// completion. So, you can use it when your chain of responsibility must end
/// up, and handle an incoming event.
///
/// # Examples
///
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::prelude::*;
///
/// let multiply = dptree::endpoint(|x: i32| async move { x * 10 });
/// assert_eq!(multiply.dispatch(dptree::deps![5]).await, ControlFlow::Break(50));
///
/// # }
/// ```
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
pub type Endpoint<'a, Input, Output> = Handler<'a, Input, Output, Infallible>;

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
