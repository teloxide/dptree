use crate::{
    di::{Injectable, Insert},
    from_fn, Handler,
};
use std::{ops::ControlFlow, sync::Arc};

/// Constructs a handler that optionally passes a value of a new type further.
///
/// If the `proj` function returns `Some(v)` then `v` will be added to the
/// container and passed further in a handler chain. If the function returns
/// `None`, then the handler will return [`ControlFlow::Continue`] with the old
/// container.
///
/// # Examples
///
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::prelude::*;
///
/// #[derive(Debug, Clone, PartialEq)]
/// enum StringOrInt {
///     String(String),
///     Int(i32),
/// }
///
/// let handler = dptree::filter_map(|val: StringOrInt| async move {
///     match val {
///         StringOrInt::String(s) => s.parse().ok(),
///         StringOrInt::Int(int) => Some(int),
///     }
/// })
/// .endpoint(|value: i32| async move { value });
///
/// assert_eq!(handler.dispatch(dptree::deps!(StringOrInt::Int(10))).await, ControlFlow::Break(10));
/// assert_eq!(
///     handler.dispatch(dptree::deps!(StringOrInt::String("10".into()))).await,
///     ControlFlow::Break(10)
/// );
/// assert!(matches!(
///     handler.dispatch(dptree::deps!(StringOrInt::String("NaN".into()))).await,
///     ControlFlow::Continue(_)
/// ));
///
/// # }
/// ```
pub fn filter_map<'a, Projection, Input, Output, NewType, Args>(
    proj: Projection,
) -> Handler<'a, Input, Output, Input>
where
    Input: Clone,
    Projection: Injectable<Input, Option<NewType>, Args> + Send + Sync + 'a,
    Input: Insert<NewType> + Send + Sync + 'a,
    Output: Send + Sync + 'a,
    NewType: Send,
{
    let proj = Arc::new(proj);

    from_fn(move |container: Input, cont| {
        let proj = Arc::clone(&proj);

        async move {
            let proj = proj.inject(&container);
            let res = proj().await;
            std::mem::drop(proj);

            match res {
                Some(new_type) => {
                    let mut intermediate = container.clone();
                    intermediate.insert(new_type);
                    match cont(intermediate).await {
                        ControlFlow::Continue(_) => ControlFlow::Continue(container),
                        ControlFlow::Break(result) => ControlFlow::Break(result),
                    }
                }
                None => ControlFlow::Continue(container),
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::DependencyMap;

    #[tokio::test]
    async fn test_some() {
        let value = 123;
        let deps = DependencyMap::new();

        let result = filter_map(move || async move { Some(value) })
            .endpoint(move |event: i32| async move {
                assert_eq!(event, value);
                value
            })
            .dispatch(deps)
            .await;

        assert!(result == ControlFlow::Break(value));
    }

    #[tokio::test]
    async fn test_none() {
        let result = filter_map(|| async move { None::<i32> })
            .endpoint(|| async move { unreachable!() })
            .dispatch(DependencyMap::new())
            .await;

        assert!(result == ControlFlow::Continue(DependencyMap::new()));
    }
}
