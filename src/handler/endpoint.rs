use crate::{
    description, di::Injectable, from_fn_with_description, Handler, HandlerDescription,
    HandlerSignature,
};

use std::{collections::BTreeSet, ops::ControlFlow, sync::Arc};

use futures::FutureExt;

/// Constructs a handler that has no further handlers in a chain.
///
/// An endpoint is a handler that _always_ breaks handler execution after its
/// completion. So, you can use it when your chain of responsibility must end
/// up, and handle an incoming event.
///
/// # Run-time signature
///
/// - Obligations: `F::obligations()`
/// - Outcomes: `BTreeSet::default()`
#[must_use]
#[track_caller]
pub fn endpoint<'a, F, Output, FnArgs, Descr>(f: F) -> Endpoint<'a, Output, Descr>
where
    F: Injectable<Output, FnArgs> + Send + Sync + 'a,
    Output: 'static,
    Descr: HandlerDescription,
{
    let f = Arc::new(f);

    from_fn_with_description(
        Descr::endpoint(),
        move |x, _cont| {
            let f = Arc::clone(&f);
            async move {
                let f = f.inject(&x);
                f().map(ControlFlow::Break).await
            }
        },
        HandlerSignature::Other { obligations: F::obligations(), outcomes: BTreeSet::default() },
    )
}

/// A handler with no further handlers in a chain.
pub type Endpoint<'a, Output, Descr = description::Unspecified> = Handler<'a, Output, Descr>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{deps, help_inference};

    #[tokio::test]
    async fn test_endpoint() {
        let input = 123;
        let output = 7;

        let result = help_inference(endpoint(move |num: i32| async move {
            assert_eq!(num, input);
            output
        }))
        .dispatch(deps![input])
        .await;

        let result = match result {
            ControlFlow::Break(b) => b,
            _ => panic!("Unexpected: handler return ControlFlow::Break"),
        };
        assert_eq!(result, output);
    }
}
