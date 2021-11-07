use crate::{di::IntoDiFn, from_fn, Handler};
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
/// Endpoint is a handler that _always_ break handler execution after its
/// completion. So, you can use it when your chain of responsibility must end
/// up, and handle incoming event.
///
/// Examples:
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::{container::Value, prelude::*};
/// use std::ops::ControlFlow;
///
/// let hello_world = dptree::endpoint(|| async { "Hello, World!" });
/// assert_eq!(hello_world.dispatch(Value::new(0)).await, ControlFlow::Break("Hello, World!"));
///
/// let multiply = dptree::endpoint(|x: Arc<i32>| async move { *x * 10 });
/// assert_eq!(multiply.dispatch(Value::new(5)).await, ControlFlow::Break(50));
///
/// # }
/// ```
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

/// Endpoint handler type.
///
/// Infallible in position of intermediate type means that continuation never
/// will be called.
pub type Endpoint<'a, Input, Output> = Handler<'a, Input, Output, Infallible>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::container::DependencyMap;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_endpoint() {
        let input = 123;
        let output = 7;

        let mut store = DependencyMap::new();
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
