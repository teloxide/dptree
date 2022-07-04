use crate::{
    di::{Asyncify, Injectable, Insert},
    endpoint, filter, filter_async, filter_map, filter_map_async, inspect, inspect_async, map,
    map_async, Handler, HandlerDescription,
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
        self.chain(filter(pred))
    }

    /// Chain this handler with the async filter predicate `pred`.
    #[must_use]
    #[track_caller]
    pub fn filter_async<Pred, FnArgs>(self, pred: Pred) -> Handler<'a, Input, Output, Descr>
    where
        Pred: Injectable<Input, bool, FnArgs> + Send + Sync + 'a,
    {
        self.chain(filter_async(pred))
    }

    /// Chain this handler with the filter projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn filter_map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Input, Output, Descr>
    where
        Input: Insert<NewType> + Clone,
        Asyncify<Proj>: Injectable<Input, Option<NewType>, Args> + Send + Sync + 'a,
        NewType: Send,
    {
        self.chain(filter_map(proj))
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
        NewType: Send,
    {
        self.chain(filter_map_async(proj))
    }

    /// Chain this handler with the map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Input, Output, Descr>
    where
        Input: Insert<NewType> + Clone,
        Asyncify<Proj>: Injectable<Input, NewType, Args> + Send + Sync + 'a,
        NewType: Send,
    {
        self.chain(map(proj))
    }

    /// Chain this handler with the async map projection `proj`.
    #[must_use]
    #[track_caller]
    pub fn map_async<Proj, NewType, Args>(self, proj: Proj) -> Handler<'a, Input, Output, Descr>
    where
        Input: Insert<NewType> + Clone,
        Proj: Injectable<Input, NewType, Args> + Send + Sync + 'a,
        NewType: Send,
    {
        self.chain(map_async(proj))
    }

    /// Chain this handler with the inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect<F, Args>(self, f: F) -> Handler<'a, Input, Output, Descr>
    where
        Asyncify<F>: Injectable<Input, (), Args> + Send + Sync + 'a,
    {
        self.chain(inspect(f))
    }

    /// Chain this handler with the async inspection function `f`.
    #[must_use]
    #[track_caller]
    pub fn inspect_async<F, Args>(self, f: F) -> Handler<'a, Input, Output, Descr>
    where
        F: Injectable<Input, (), Args> + Send + Sync + 'a,
    {
        self.chain(inspect_async(f))
    }

    /// Chain this handler with the endpoint handler `f`.
    #[must_use]
    #[track_caller]
    pub fn endpoint<F, FnArgs>(self, f: F) -> Handler<'a, Input, Output, Descr>
    where
        F: Injectable<Input, Output, FnArgs> + Send + Sync + 'a,
    {
        self.chain(endpoint(f))
    }
}
