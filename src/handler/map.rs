use crate::{
    di::{Asyncify, Injectable},
    from_fn_with_description, Handler, HandlerDescription, HandlerSignature, Type,
};

use std::{collections::BTreeSet, iter::FromIterator, ops::ControlFlow, sync::Arc};

/// Constructs a handler that passes a value of a new type further.
///
/// The result of invoking `proj` will be added to the container and passed
/// further in a handler chain.
///
/// See also: [`crate::filter_map`].
///
/// # Run-time signature
///
/// - Obligations: `Projection::obligations()`
/// - Outcomes: `BTreeSet::from_iter(vec![Type::of::<NewType>()])`
#[must_use]
#[track_caller]
pub fn map<'a, Projection, Output, NewType, Args, Descr>(
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Projection>: Injectable<NewType, Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
    NewType: Send + Sync + 'static,
{
    map_with_description(Descr::map(), proj)
}

/// The asynchronous version of [`map`].
#[must_use]
#[track_caller]
pub fn map_async<'a, Projection, Output, NewType, Args, Descr>(
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Projection: Injectable<NewType, Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
    NewType: Send + Sync + 'static,
{
    map_async_with_description(Descr::map_async(), proj)
}

/// [`map`] with a custom description.
#[must_use]
#[track_caller]
pub fn map_with_description<'a, Projection, Output, NewType, Args, Descr>(
    description: Descr,
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Asyncify<Projection>: Injectable<NewType, Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
    NewType: Send + Sync + 'static,
{
    map_async_with_description(description, Asyncify(proj))
}

/// [`map_async`] with a custom description.
#[must_use]
#[track_caller]
pub fn map_async_with_description<'a, Projection, Output, NewType, Args, Descr>(
    description: Descr,
    proj: Projection,
) -> Handler<'a, Output, Descr>
where
    Projection: Injectable<NewType, Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
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

                let mut intermediate = container.clone();
                intermediate.insert(res);
                match cont(intermediate).await {
                    ControlFlow::Continue(_) => ControlFlow::Continue(container),
                    ControlFlow::Break(result) => ControlFlow::Break(result),
                }
            }
        },
        HandlerSignature::Other {
            obligations: Projection::obligations(),
            guaranteed_outcomes: BTreeSet::from_iter(vec![Type::of::<NewType>()]),
            conditional_outcomes: BTreeSet::new(),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{deps, help_inference};

    #[tokio::test]
    async fn test_map() {
        let value = 123;

        let result = help_inference(map(move || value))
            .endpoint(move |event: i32| async move {
                assert_eq!(event, value);
                value
            })
            .dispatch(deps![])
            .await;

        assert!(result == ControlFlow::Break(value));
    }
}
