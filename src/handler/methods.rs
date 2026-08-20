use crate::{
    di::{Asyncify, Injectable},
    send::{MaybeSend, MaybeSync},
    Fallible, Handler, HandlerDescription,
};

impl<'a, Output, Descr> Handler<'a, Output, Descr>
where
    Output: 'a,
    Descr: HandlerDescription,
{
    /// Chain this handler with the filter predicate `pred`.
    #[must_use]
    #[track_caller]
    pub fn filter<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Output, Descr>
    where
        Asyncify<Pred>: Injectable<bool, FnArgs> + MaybeSend + MaybeSync + 'a,
    {
        self.chain(crate::filter(pred))
    }

    /// Chain this handler with the async filter predicate `pred`.
    #[must_use]
    #[track_caller]
    pub fn filter_async<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Output, Descr>
    where
        Pred: Injectable<bool, FnArgs> + MaybeSend + MaybeSync + 'a,
    {
        self.chain(crate::filter_async(pred))
    }

    /// Chain this handler with the filter projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn filter_map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Asyncify<Proj>: Injectable<Option<NewType>, Args> + MaybeSend + MaybeSync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::filter_map(proj))
    }

    /// Chain this handler with the async filter projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn filter_map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Proj: Injectable<Option<NewType>, Args> + MaybeSend + MaybeSync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::filter_map_async(proj))
    }

    /// Chain this handler with the map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Asyncify<Proj>: Injectable<NewType, Args> + MaybeSend + MaybeSync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::map(proj))
    }

    /// Chain this handler with the async map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Proj: Injectable<NewType, Args> + MaybeSend + MaybeSync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::map_async(proj))
    }

    /// Chain this handler with the inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect<F, Args>(self, f: F) -> Handler<'a, Output, Descr>
    where
        Asyncify<F>: Injectable<(), Args> + MaybeSend + MaybeSync + 'a,
    {
        self.chain(crate::inspect(f))
    }

    /// Chain this handler with the async inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect_async<F, Args>(self, f: F) -> Handler<'a, Output, Descr>
    where
        F: Injectable<(), Args> + MaybeSend + MaybeSync + 'a,
    {
        self.chain(crate::inspect_async(f))
    }

    /// Chain this handler with the endpoint handler `f`.
    #[must_use]
    #[track_caller]
    pub fn endpoint<F, FnArgs>(self, f: F) -> Handler<'a, Output, Descr>
    where
        F: Injectable<Output, FnArgs> + MaybeSend + MaybeSync + 'a,
        Output: 'static,
    {
        self.chain(crate::endpoint(f))
    }

    /// Chain this handler with the fallible filter predicate `pred`.
    ///
    /// `pred` returns [`Result<bool, E>`] (where `E` is the error type of this
    /// handler's `Output`). On `Ok(true)` execution continues; on `Ok(false)`
    /// the handler returns [`ControlFlow::Continue`](std::ops::ControlFlow::Continue)
    /// (try the next branch); on `Err(e)` the handler short-circuits with
    /// [`ControlFlow::Break`](std::ops::ControlFlow::Break) carrying
    /// the error, without falling through to sibling branches.
    #[must_use]
    #[track_caller]
    pub fn try_filter<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Output, Descr>
    where
        Asyncify<Pred>:
            Injectable<Result<bool, Output::Error>, FnArgs> + MaybeSend + MaybeSync + 'a,
        Output: Fallible,
        Output::Error: MaybeSend,
    {
        self.chain(crate::try_filter(pred))
    }

    /// Chain this handler with the async fallible filter predicate `pred`.
    ///
    /// See [`try_filter`](Handler::try_filter).
    #[must_use]
    #[track_caller]
    pub fn try_filter_async<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Output, Descr>
    where
        Pred: Injectable<Result<bool, Output::Error>, FnArgs> + MaybeSend + MaybeSync + 'a,
        Output: Fallible,
        Output::Error: MaybeSend,
    {
        self.chain(crate::try_filter_async(pred))
    }

    /// Chain this handler with the fallible filter projection `proj`.
    ///
    /// `proj` returns [`Result<Option<NewType>, E>`]. On `Ok(Some(v))` `v` is
    /// inserted into the container and execution continues; on `Ok(None)` the
    /// handler returns [`ControlFlow::Continue`](std::ops::ControlFlow::Continue)
    /// (try the next branch); on `Err(e)` the handler short-circuits with
    /// [`ControlFlow::Break`](std::ops::ControlFlow::Break) carrying
    /// the error.
    #[must_use]
    #[track_caller]
    pub fn try_filter_map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Asyncify<Proj>:
            Injectable<Result<Option<NewType>, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
        Output: Fallible,
        Output::Error: MaybeSend,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::try_filter_map(proj))
    }

    /// Chain this handler with the async fallible filter projection `proj`.
    ///
    /// See [`try_filter_map`](Handler::try_filter_map).
    #[must_use]
    #[track_caller]
    pub fn try_filter_map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Proj: Injectable<Result<Option<NewType>, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
        Output: Fallible,
        Output::Error: MaybeSend,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::try_filter_map_async(proj))
    }

    /// Chain this handler with the fallible map projection `proj`.
    ///
    /// `proj` returns [`Result<NewType, E>`]. On `Ok(v)` `v` is inserted into
    /// the container and execution continues; on `Err(e)` the handler
    /// short-circuits with [`ControlFlow::Break`](std::ops::ControlFlow::Break)
    /// carrying the error, without calling the continuation.
    #[must_use]
    #[track_caller]
    pub fn try_map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Asyncify<Proj>:
            Injectable<Result<NewType, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
        Output: Fallible,
        Output::Error: MaybeSend,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::try_map(proj))
    }

    /// Chain this handler with the async fallible map projection `proj`.
    ///
    /// See [`try_map`](Handler::try_map).
    #[must_use]
    #[track_caller]
    pub fn try_map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Proj: Injectable<Result<NewType, Output::Error>, Args> + MaybeSend + MaybeSync + 'a,
        Output: Fallible,
        Output::Error: MaybeSend,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::try_map_async(proj))
    }
}

#[cfg(test)]
mod tests {
    use std::ops::ControlFlow;

    use crate::{deps, help_inference};

    crate::cross_test! {
        // Test that these methods just do compile.
        async fn test_methods() {
            let value = 42;

            let _: ControlFlow<(), _> =
                help_inference(crate::entry()).filter(|| true).dispatch(deps![value]).await;

            let _: ControlFlow<(), _> = help_inference(crate::entry())
                .filter_async(|| async { true })
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<(), _> =
                help_inference(crate::entry()).filter_map(|| Some("abc")).dispatch(deps![value]).await;

            let _: ControlFlow<(), _> = help_inference(crate::entry())
                .filter_map_async(|| async { Some("abc") })
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<(), _> =
                help_inference(crate::entry()).map(|| "abc").dispatch(deps![value]).await;

            let _: ControlFlow<(), _> = help_inference(crate::entry())
                .map_async(|| async { "abc" })
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<(), _> =
                help_inference(crate::entry()).inspect(|| {}).dispatch(deps![value]).await;

            let _: ControlFlow<(), _> =
                help_inference(crate::entry()).inspect_async(|| async {}).dispatch(deps![value]).await;

            let _: ControlFlow<(), _> =
                help_inference(crate::entry()).endpoint(|| async {}).dispatch(deps![value]).await;

            // Fallible handlers require a fallible (`Result` in here) output.
            let _: ControlFlow<Result<(), &str>, _> = help_inference(crate::entry())
                .try_filter(|| Ok::<bool, &str>(true))
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<Result<(), &str>, _> = help_inference(crate::entry())
                .try_filter_async(|| async { Ok::<bool, &str>(true) })
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<Result<(), &str>, _> = help_inference(crate::entry())
                .try_filter_map(|| Ok::<Option<()>, &str>(Some(())))
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<Result<(), &str>, _> = help_inference(crate::entry())
                .try_filter_map_async(|| async { Ok::<Option<()>, &str>(Some(())) })
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<Result<(), &str>, _> = help_inference(crate::entry())
                .try_map(|| Ok::<(), &str>(()))
                .dispatch(deps![value])
                .await;

            let _: ControlFlow<Result<(), &str>, _> = help_inference(crate::entry())
                .try_map_async(|| async { Ok::<(), &str>(()) })
                .dispatch(deps![value])
                .await;
        }
    }
}
