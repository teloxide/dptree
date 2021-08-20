use crate::handler::leaf::Leaf;
use crate::handler::HandlerFuture;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;

pub struct LeafByEvent<H> {
    handler: H,
}

pub trait LeafEventEnter<F, Event> {
    fn enter_event(func: F) -> LeafByEvent<F>;
}

impl<F, Event> LeafEventEnter<F, Event> for Leaf
where
    LeafByEvent<F>: Handler<Event>,
{
    fn enter_event(func: F) -> LeafByEvent<F> {
        LeafByEvent { handler: func }
    }
}

impl<H, E, Res, Fut> Handler<E> for LeafByEvent<H>
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
