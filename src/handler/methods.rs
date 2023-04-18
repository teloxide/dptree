use crate::{
    di::{Asyncify, Injectable, Insert},
    Handler, HandlerDescription,
};

impl<'a, Input, Output, Descr> Handler<'a, Input, Output, Descr>
where
    Input: Send + 'a,
    Output: 'a,
    Descr: HandlerDescription,
{
    /// Chain this handler with the filter predicate `pred`.
    #[must_use]
    #[track_caller]
    pub fn filter<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Input, Output, Descr>
    where
        Asyncify<Pred>: Injectable<Input, bool, FnArgs> + Send + Sync + 'a,
    {
        self.chain(crate::filter(pred))
    }

    /// Chain this handler with the async filter predicate `pred`.
    #[must_use]
    #[track_caller]
    pub fn filter_async<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Input, Output, Descr>
    where
        Pred: Injectable<Input, bool, FnArgs> + Send + Sync + 'a,
    {
        self.chain(crate::filter_async(pred))
    }

    /// Chain this handler with the filter projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn filter_map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Input, Output, Descr>
    where
        Input: Insert<NewType> + Clone,
        Asyncify<Proj>: Injectable<Input, Option<NewType>, Args> + Send + Sync + 'a,
        NewType: Send + 'static,
    {
        self.chain(crate::filter_map(proj))
    }

    /// Chain this handler with the async filter projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn filter_map_async<Proj, NewType, Args>(
        self,
        proj: Proj,
    ) -> Handler<'a, Input, Output, Descr>
    where
        Input: Insert<NewType> + Clone,
        Proj: Injectable<Input, Option<NewType>, Args> + Send + Sync + 'a,
        NewType: Send + 'static,
    {
        self.chain(crate::filter_map_async(proj))
    }

    /// Chain this handler with the map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Input, Output, Descr>
    where
        Input: Insert<NewType> + Clone,
        Asyncify<Proj>: Injectable<Input, NewType, Args> + Send + Sync + 'a,
        NewType: Send + 'static,
    {
        self.chain(crate::map(proj))
    }

    /// Chain this handler with the async map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Input, Output, Descr>
    where
        Input: Insert<NewType> + Clone,
        Proj: Injectable<Input, NewType, Args> + Send + Sync + 'a,
        NewType: Send + 'static,
    {
        self.chain(crate::map_async(proj))
    }

    /// Chain this handler with the inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect<F, Args>(self, f: F) -> Handler<'a, Input, Output, Descr>
    where
        Asyncify<F>: Injectable<Input, (), Args> + Send + Sync + 'a,
    {
        self.chain(crate::inspect(f))
    }

    /// Chain this handler with the async inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect_async<F, Args>(self, f: F) -> Handler<'a, Input, Output, Descr>
    where
        F: Injectable<Input, (), Args> + Send + Sync + 'a,
    {
        self.chain(crate::inspect_async(f))
    }

    /// Chain this handler with the endpoint handler `f`.
    #[must_use]
    #[track_caller]
    pub fn endpoint<F, FnArgs>(self, f: F) -> Handler<'a, Input, Output, Descr>
    where
        F: Injectable<Input, Output, FnArgs> + Send + Sync + 'a,
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
