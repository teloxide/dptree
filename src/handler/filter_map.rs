use crate::{
    di::{DependencyMap, Injector},
    from_fn, Handler,
};
use std::{ops::ControlFlow, sync::Arc};

/// Insert some value to a container.
pub trait Insert<Value> {
    /// Inserts `value` into itself, returning the previous value, if exists.
    fn insert(&mut self, value: Value) -> Option<Arc<Value>>;
}

impl<T: Send + Sync + 'static> Insert<T> for DependencyMap {
    fn insert(&mut self, value: T) -> Option<Arc<T>> {
        DependencyMap::insert(self, value)
    }
}

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
/// use dptree::{
///     deps,
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
/// let handler = dptree::filter_map(|val: Arc<StringOrInt>| async move {
///     match val.as_ref() {
///         StringOrInt::String(s) => s.parse().ok(),
///         StringOrInt::Int(int) => Some(*int),
///     }
/// })
/// .endpoint(|value: Arc<i32>| async move { *value });
///
/// assert_eq!(handler.dispatch(deps! { StringOrInt::Int(10) }).await, ControlFlow::Break(10));
/// assert_eq!(
///     handler.dispatch(deps! { StringOrInt::String("10".into()) }).await,
///     ControlFlow::Break(10)
/// );
/// assert!(matches!(
///     handler.dispatch(deps! { StringOrInt::String("NaN".into()) }).await,
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
    Projection: Injector<Input, Option<NewType>, Args> + Send + Sync + 'a,
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

    #[tokio::test]
    async fn test_some() {
        let value = 123;
        let deps = DependencyMap::new();

        let result = filter_map(move || async move { Some(value) })
            .endpoint(move |event: Arc<i32>| async move {
                assert_eq!(*event, value);
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
