use crate::{
    di::{Asyncify, Injectable},
    Handler, HandlerDescription,
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
        Asyncify<Pred>: Injectable<bool, FnArgs> + Send + Sync + 'a,
    {
        self.chain(crate::filter(pred))
    }

    /// Chain this handler with the async filter predicate `pred`.
    #[must_use]
    #[track_caller]
    pub fn filter_async<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Output, Descr>
    where
        Pred: Injectable<bool, FnArgs> + Send + Sync + 'a,
    {
        self.chain(crate::filter_async(pred))
    }

    /// Chain this handler with the filter projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn filter_map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Asyncify<Proj>: Injectable<Option<NewType>, Args> + Send + Sync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::filter_map(proj))
    }

    /// Chain this handler with the async filter projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn filter_map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Proj: Injectable<Option<NewType>, Args> + Send + Sync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::filter_map_async(proj))
    }

    /// Chain this handler with the map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Asyncify<Proj>: Injectable<NewType, Args> + Send + Sync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::map(proj))
    }

    /// Chain this handler with the async map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Output, Descr>
    where
        Proj: Injectable<NewType, Args> + Send + Sync + 'a,
        NewType: Send + Sync + 'static,
    {
        self.chain(crate::map_async(proj))
    }

    /// Chain this handler with the inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect<F, Args>(self, f: F) -> Handler<'a, Output, Descr>
    where
        Asyncify<F>: Injectable<(), Args> + Send + Sync + 'a,
    {
        self.chain(crate::inspect(f))
    }

    /// Chain this handler with the async inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect_async<F, Args>(self, f: F) -> Handler<'a, Output, Descr>
    where
        F: Injectable<(), Args> + Send + Sync + 'a,
    {
        self.chain(crate::inspect_async(f))
    }

    /// Chain this handler with the endpoint handler `f`.
    #[must_use]
    #[track_caller]
    pub fn endpoint<F, FnArgs>(self, f: F) -> Handler<'a, Output, Descr>
    where
        F: Injectable<Output, FnArgs> + Send + Sync + 'a,
        Output: 'static,
    {
        self.chain(crate::endpoint(f))
    }
}

#[cfg(test)]
mod tests {
    use std::ops::ControlFlow;

    use crate::{deps, help_inference};

    // Test that these methods just do compile.
    #[tokio::test]
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
    }
}
