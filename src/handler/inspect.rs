use crate::{
    di::{Asyncify, Injectable},
    from_fn_with_description, Handler, HandlerDescription, HandlerSignature,
};

use std::{collections::HashSet, sync::Arc};

/// Constructs a handler that inspects current state. Like [`map`] but does not
/// add return value of `f` to the container.
///
/// # Signature
///
/// The run-time type signature of this handler is `HandlerSignature::Other {
/// input_types: F::input_types(), output_types: HashSet::new() }`.
///
/// [`map`]: crate::map
#[must_use]
#[track_caller]
pub fn inspect<'a, F, Output, Args, Descr>(f: F) -> Handler<'a, Output, Descr>
where
    Asyncify<F>: Injectable<(), Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    inspect_with_description(Descr::inspect(), f)
}

/// The asynchronous version of [`inspect`].
#[must_use]
#[track_caller]
pub fn inspect_async<'a, F, Output, Args, Descr>(f: F) -> Handler<'a, Output, Descr>
where
    F: Injectable<(), Args> + Send + Sync + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    inspect_async_with_description(Descr::inspect_async(), f)
}

/// [`inspect`] with a custom description.
#[must_use]
#[track_caller]
pub fn inspect_with_description<'a, F, Output, Args, Descr>(
    description: Descr,
    f: F,
) -> Handler<'a, Output, Descr>
where
    Asyncify<F>: Injectable<(), Args> + Send + Sync + 'a,
    Output: 'a,
{
    inspect_async_with_description(description, Asyncify(f))
}

/// [`inspect_async`] with a custom description.
#[must_use]
#[track_caller]
pub fn inspect_async_with_description<'a, F, Output, Args, Descr>(
    description: Descr,
    f: F,
) -> Handler<'a, Output, Descr>
where
    F: Injectable<(), Args> + Send + Sync + 'a,
    Output: 'a,
{
    let f = Arc::new(f);

    from_fn_with_description(
        description,
        move |x, cont| {
            let f = Arc::clone(&f);
            async move {
                {
                    let f = f.inject(&x);
                    f().await;
                }

                cont(x).await
            }
        },
        HandlerSignature::Other {
            input_types: F::input_types(),
            output_types: HashSet::new(),
            obligations: F::obligations(),
        },
    )
}

#[cfg(test)]
mod tests {
    use std::{
        ops::ControlFlow,
        sync::atomic::{AtomicBool, Ordering},
    };

    use super::*;
    use crate::{deps, help_inference};

    #[tokio::test]
    async fn test_inspect() {
        let value = 123;
        let inspect_passed = Arc::new(AtomicBool::new(false));
        let inspect_passed_cloned = Arc::clone(&inspect_passed);

        let result: ControlFlow<(), _> = help_inference(inspect(move |x: i32| {
            assert_eq!(x, value);
            inspect_passed_cloned.swap(true, Ordering::Relaxed);
        }))
        .dispatch(deps![value])
        .await;

        assert!(matches!(result, ControlFlow::Continue(_)));
        assert!(inspect_passed.load(Ordering::Relaxed));
    }
}
