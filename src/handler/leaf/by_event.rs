use crate::handler::leaf::Leaf;
use crate::handler::HandlerFuture;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;

pub struct LeafByEvent<H, Need> {
    handler: H,
    _phantom: PhantomData<Need>,
}

pub trait LeafEventEnter<F, Event, Need> {
    fn enter_event(func: F) -> LeafByEvent<F, Need>;
}

impl<F, Event, Need> LeafEventEnter<F, Event, Need> for Leaf
where
    LeafByEvent<F, Need>: Handler<Event>,
{
    fn enter_event(func: F) -> LeafByEvent<F, Need> {
        LeafByEvent {
            handler: func,
            _phantom: PhantomData,
        }
    }
}

impl<H, E, Res, Fut> Handler<E> for LeafByEvent<H, EventNeed>
where
    H: Fn(E) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    E: 'static,
    Res: 'static,
{
    type Res = Res;

    fn handle(&self, event: E) -> HandlerFuture<Res, E> {
        Box::pin((self.handler)(event).map(Ok))
    }
}

impl<H, E, Res, Fut> Handler<E> for LeafByEvent<H, EventNotNeed>
where
    H: Fn() -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    E: 'static,
    Res: 'static,
{
    type Res = Res;

    fn handle(&self, _: E) -> HandlerFuture<Res, E> {
        Box::pin((self.handler)().map(Ok))
    }
}

pub struct EventNeed;
pub struct EventNotNeed;
