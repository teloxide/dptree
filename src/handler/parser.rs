use std::{ops::ControlFlow, sync::Arc};

use crate::{di::DependencySupplier, from_fn, Handler};

#[rustfmt::skip] // rustfmt too bad in formatting lists
/// Create handler that try to parse one input value from container into
/// another.
///
/// How it works:
///
/// 1. Handler get value of type `A` specified in the parser function from input container.
/// 2. Handler call parser function, and pass input value `A` into a function.
/// 3. If the function returns `None`, then handler returns `ControlFlow::Continue`.
/// 4. Otherwise if the function returns `Some(B)`, handler replace `A` type with `B` type in the container.
/// 5. Handler call continuation with new container with `B` type.
/// 6. If next handler returns `ControlFlow::Continue`, handler replace value `B` with `A`.
///
/// Example:
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::{container::Value, prelude::*};
/// use std::ops::ControlFlow;
///
/// #[derive(Debug, PartialEq)]
/// enum StringOrInt {
///     String(String),
///     Int(i32),
/// }
///
/// let handler = dptree::parser(|x: &StringOrInt| match x {
///     StringOrInt::String(s) => s.parse().ok(),
///     StringOrInt::Int(int) => Some(*int),
/// })
/// .endpoint(|value: Arc<i32>| async move { *value });
///
/// assert_eq!(handler.dispatch(Value::new(StringOrInt::Int(10))).await, ControlFlow::Break(10));
/// assert_eq!(
///     handler.dispatch(Value::new(StringOrInt::String("10".into()))).await,
///     ControlFlow::Break(10)
/// );
/// assert!(matches!(
///     handler.dispatch(Value::new(StringOrInt::String("NaN".into()))).await,
///     ControlFlow::Continue(_)
/// ));
///
/// # }
/// ```
pub fn parser<'a, Projection, Input, IT, OT, Output, Intermediate>(
    proj: Projection,
) -> Handler<'a, Input, Output, Intermediate>
where
    Input: Replace<IT, OT, Out = Intermediate> + DependencySupplier<IT>,
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

/// The trait is used to replace value of one type to a value with another type.
///
/// Used only in `dptree::parser` method when parsing one type to another. You
/// can implement it for your DI container to allow users use your container in
/// `dptree::parser`.
pub trait Replace<From, To> {
    /// An output when type `From` is replaced by type `To`.
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
