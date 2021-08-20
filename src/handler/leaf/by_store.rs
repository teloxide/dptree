use crate::handler::leaf::Leaf;
use crate::handler::HandlerFuture;
use crate::store::Store;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;

pub struct LeafByStore<H, Args> {
    handler: H,
    _phantom: PhantomData<Args>,
}

pub trait LeafStoreEnter<F, Args> {
    fn enter_store(func: F) -> LeafByStore<F, Args>;
}

impl<F, Fut, A1> LeafStoreEnter<F, (A1,)> for Leaf
where
    F: Fn(A1) -> Fut,
    Fut: Future + Send + 'static,
    Fut::Output: 'static,
{
    fn enter_store(func: F) -> LeafByStore<F, (A1,)> {
        LeafByStore {
            handler: func,
            _phantom: PhantomData,
        }
    }
}
impl<F, Fut, A1, A2> LeafStoreEnter<F, (A1, A2)> for Leaf
where
    F: Fn(A1, A2) -> Fut,
    Fut: Future + Send + 'static,
    Fut::Output: 'static,
{
    fn enter_store(func: F) -> LeafByStore<F, (A1, A2)> {
        LeafByStore {
            handler: func,
            _phantom: PhantomData,
        }
    }
}
impl<H, S, Res, Fut, A1> Handler<S> for LeafByStore<H, (A1,)>
where
    H: Fn(A1) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    Res: 'static,
    S: Store<A1> + 'static,
{
    type Res = Res;

    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get()).map(Ok))
    }
}

impl<H, S, Res, Fut, A1, A2> Handler<S> for LeafByStore<H, (A1, A2)>
where
    H: Fn(A1, A2) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    Res: 'static,
    S: Store<A1> + Store<A2> + 'static,
{
    type Res = Res;

    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get(), store.get()).map(Ok))
    }
}
