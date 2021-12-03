use std::{ops::ControlFlow, sync::Arc};

use crate::{from_fn, Handler};

/// Create handler that has direct access to the container.
///
/// If a function returns `Some(new_container)` then next handler will be called
/// with `new_container`.
///
/// If a function returns `None` then handler will return
/// `ControlFlow::Continue(old_container)`.
///
/// Example:
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::{
///     di::{DependencyMap, Value},
///     prelude::*,
/// };
/// use std::ops::ControlFlow;
///
/// #[derive(Debug, PartialEq)]
/// enum StringOrInt {
///     String(String),
///     Int(i32),
/// }
///
/// let handler = dptree::middleware(|val: &Value<StringOrInt>| {
///     let value = match val.0.as_ref() {
///         StringOrInt::String(s) => s.parse().ok()?,
///         StringOrInt::Int(int) => *int,
///     };
///     Some(Value::new(value))
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
pub fn middleware<'a, Projection, Input, Output, Intermediate>(
    proj: Projection,
) -> Handler<'a, Input, Output, Intermediate>
where
    Projection: Fn(&Input) -> Option<Intermediate> + Send + Sync + 'a,
    Input: Send + Sync + 'a,
    Output: Send + Sync + 'a,
    Intermediate: Send + Sync + 'a,
{
    let proj = Arc::new(proj);

    from_fn(move |container: Input, cont| {
        let proj = Arc::clone(&proj);

        async move {
            match proj(&container) {
                Some(intermediate) => match cont(intermediate).await {
                    ControlFlow::Continue(_) => ControlFlow::Continue(container),
                    ControlFlow::Break(result) => ControlFlow::Break(result),
                },
                None => ControlFlow::Continue(container),
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::di::Value;

    #[tokio::test]
    async fn test_some() {
        let input = 123;
        let output = "abc";

        let result = middleware(move |value: &Value<i32>| {
            assert_eq!(*value.0, input);
            Some(Value::new(output))
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

        let result = middleware(|event: &Value<i32>| {
            assert_eq!(*event.0, input);
            None::<Value<i32>>
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

        let result = middleware(|event: &Value<i32>| {
            assert_eq!(*event.0, input);
            Some(event.clone())
        })
        .chain(
            middleware(|event: &Value<i32>| {
                assert_eq!(*event.0, input);
                Some(Value::new(output))
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

        let result = middleware(|event: &Value<i32>| {
            assert_eq!(*event.0, input);
            Some(event.clone())
        })
        .chain(
            middleware(|event: &Value<i32>| {
                assert_eq!(*event.0, input);
                None::<Value<&'static str>>
            })
            .endpoint(|| async move { unreachable!() }),
        )
        .dispatch(Value::new(input))
        .await;

        assert!(result == ControlFlow::Continue(Value::new(input)));
    }
}
