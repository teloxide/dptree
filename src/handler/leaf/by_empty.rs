use crate::handler::leaf::Leaf;
use crate::handler::HandlerFuture;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;

pub struct LeafByEmpty<H> {
    handler: H,
}

pub trait LeafEmptyEnter<F> {
    fn enter_empty(func: F) -> LeafByEmpty<F>;
}

impl<F, Fut> LeafEmptyEnter<F> for Leaf
where
    F: Fn() -> Fut,
    Fut: Future + Send + 'static,
    Fut::Output: 'static,
{
    fn enter_empty(func: F) -> LeafByEmpty<F> {
        LeafByEmpty { handler: func }
    }
}

impl<H, E, Res, Fut> Handler<E> for LeafByEmpty<H>
where
    H: Fn() -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    Res: 'static,
    E: 'static,
{
    type Res = Res;

    fn handle(&self, _: E) -> HandlerFuture<Res, E> {
        Box::pin((self.handler)().map(Ok))
    }
}
