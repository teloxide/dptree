use crate::{
    di::{Asyncify, Injectable},
    from_fn_with_description, Handler, HandlerDescription, HandlerSignature, Type,
};
use std::{collections::HashSet, ops::ControlFlow, sync::Arc};

/// Constructs a handler that optionally passes a value of a new type further.
///
/// If the `proj` function returns `Some(v)` then `v` will be added to the
/// container and passed further in a handler chain. If the function returns
/// `None`, then the handler will return [`ControlFlow::Continue`] with the old
/// container.
///
/// # Signature
///
/// The run-time type signature of this handler is `HandlerSignature::Other {
/// input_types: Projection::input_types(), output_types:
/// HashSet::from([RtType::of::<NewType>()]) }`.
#[must_use]
#[track_caller]
pub fn filter_map<'a, Projection, Output, NewType, Args, Descr>(
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Projection>: Injectable<Option<NewType>, Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
    NewType: Send + Sync + 'static,
{
    filter_map_with_description(Descr::filter_map(), proj)
}

/// The asynchronous version of [`filter_map`].
#[must_use]
#[track_caller]
pub fn filter_map_async<'a, Projection, Output, NewType, Args, Descr>(
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Projection: Injectable<Option<NewType>, Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
    NewType: Send + Sync + 'static,
{
    filter_map_async_with_description(Descr::filter_map_async(), proj)
}

/// [`filter_map`] with a custom description.
#[must_use]
#[track_caller]
pub fn filter_map_with_description<'a, Projection, Output, NewType, Args, Descr>(
    description: Descr,
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Projection>: Injectable<Option<NewType>, Args> + Send + Sync + 'a,
    Output: 'a,
    NewType: Send + Sync + 'static,
{
    filter_map_async_with_description(description, Asyncify(proj))
}

/// [`filter_map_async`] with a custom description.
#[must_use]
#[track_caller]
pub fn filter_map_async_with_description<'a, Projection, Output, NewType, Args, Descr>(
    description: Descr,
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Projection: Injectable<Option<NewType>, Args> + Send + Sync + 'a,
    Output: 'a,
    NewType: Send + Sync + 'static,
{
    let proj = Arc::new(proj);

    from_fn_with_description(
        description,
        move |container, cont| {
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
        },
        HandlerSignature::Other {
            input_types: Projection::input_types(),
            output_types: HashSet::from([Type::of::<NewType>()]),
            obligations: Projection::obligations(),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{deps, help_inference};

    #[tokio::test]
    async fn test_some() {
        let value = 123;

        let result = help_inference(filter_map(move || Some(value)))
            .endpoint(move |event: i32| async move {
                assert_eq!(event, value);
                value
            })
            .dispatch(deps![])
            .await;

        assert!(result == ControlFlow::Break(value));
    }

    #[tokio::test]
    async fn test_none() {
        let result = help_inference(filter_map(|| None::<i32>))
            .endpoint(|| async move { unreachable!() })
            .dispatch(deps![])
            .await;

        assert!(result == ControlFlow::Continue(crate::deps![]));
    }
}
