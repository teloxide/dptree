use crate::handler::endpoint::by_event::{EndpointByEvent, EndpointByEventEnter};
use crate::handler::endpoint::by_store::{EndpointByStore, EndpointByStoreEnter};
use crate::handler::endpoint::Endpoint;
use crate::handler::filter::FilterBuilder;
use crate::Handler;
use std::marker::PhantomData;

pub trait HandlerBuilder<InEvent, H>: Sized {
    type OutEvent;
    type ResultAndThen: Handler<InEvent>;
    fn and_then(self, handler: H) -> Self::ResultAndThen;
    fn and_filter<F>(
        self,
        filter: F,
    ) -> TwoBuilders<Self, FilterBuilder<F, Self::OutEvent>, Self::OutEvent>
    where
        F: Fn(&Self::OutEvent) -> bool + Send + Sync,
    {
        TwoBuilders::new(self, FilterBuilder::new(filter))
    }
}

pub struct EmptyBuilder;

impl<Event, H> HandlerBuilder<Event, H> for EmptyBuilder
where
    H: Handler<Event>,
{
    type OutEvent = Event;
    type ResultAndThen = H;

    fn and_then(self, handler: H) -> Self::ResultAndThen {
        handler
    }
}

impl EmptyBuilder {
    /// Shortcut for `builder.and_then(Endpoint::by_event(func))`.
    pub fn endpoint<Func, Event, Need>(self, func: Func) -> EndpointByEvent<Func, Need>
    where
        Endpoint<Event>: EndpointByEventEnter<Func, Event, Need>,
    {
        Endpoint::by_event(func)
    }

    /// Shortcut for `builder.and_then(Endpoint::enter_store(func))`.
    pub fn endpoint_by_store<Func, Args, Store>(self, func: Func) -> EndpointByStore<Func, Args>
    where
        Endpoint<Store>: EndpointByStoreEnter<Func, Args>,
    {
        Endpoint::by_store(func)
    }
}

pub struct TwoBuilders<A, B, BEvent>(A, B, PhantomData<BEvent>);

impl<A, B, BEvent> TwoBuilders<A, B, BEvent> {
    pub fn new(a: A, b: B) -> Self {
        TwoBuilders(a, b, PhantomData)
    }

    pub fn into_inner(self) -> (A, B) {
        (self.0, self.1)
    }
}

impl<A, B, InEvent, OutEvent, H> HandlerBuilder<InEvent, H> for TwoBuilders<A, B, OutEvent>
where
    H: Handler<OutEvent>,
    A: HandlerBuilder<InEvent, B::ResultAndThen, OutEvent = OutEvent>,
    B: HandlerBuilder<OutEvent, H>,
{
    type OutEvent = OutEvent;
    type ResultAndThen = <A as HandlerBuilder<InEvent, B::ResultAndThen>>::ResultAndThen;

    fn and_then(self, handler: H) -> Self::ResultAndThen {
        let (a, b) = self.into_inner();
        let b = b.and_then(handler);
        let a = a.and_then(b);
        a
    }
}
