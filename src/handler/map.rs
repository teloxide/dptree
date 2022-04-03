use crate::{
    di::{Asyncify, Injectable, Insert},
    from_fn, Handler,
};
use std::{ops::ControlFlow, sync::Arc};

/// Constructs a handler that passes a value of a new type further.
///
/// The result of invoking `proj` will be added to the container and passed
/// further in a handler chain.
///
/// See also: [`crate::filter_map`].
#[must_use]
pub fn map<'a, Projection, Input, Output, NewType, Args>(
    proj: Projection,
) -> Handler<'a, Input, Output, Input>
where
    Input: Clone,
    Asyncify<Projection>: Injectable<Input, NewType, Args> + Send + Sync + 'a,
    Input: Insert<NewType> + Send + Sync + 'a,
    Output: Send + Sync + 'a,
    NewType: Send,
{
    map_async(Asyncify(proj))
}

/// The asynchronous version of [`map`].
#[must_use]
pub fn map_async<'a, Projection, Input, Output, NewType, Args>(
    proj: Projection,
) -> Handler<'a, Input, Output, Input>
where
    Input: Clone,
    Projection: Injectable<Input, NewType, Args> + Send + Sync + 'a,
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

            let mut intermediate = container.clone();
            intermediate.insert(res);
            match cont(intermediate).await {
                ControlFlow::Continue(_) => ControlFlow::Continue(container),
                ControlFlow::Break(result) => ControlFlow::Break(result),
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::deps;

    #[tokio::test]
    async fn test_map() {
        let value = 123;

        let result = map(move || value)
            .endpoint(move |event: i32| async move {
                assert_eq!(event, value);
                value
            })
            .dispatch(deps![])
            .await;

        assert!(result == ControlFlow::Break(value));
    }
}
