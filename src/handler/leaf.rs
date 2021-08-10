use crate::handler::{Handler, HandlerFuture};
use crate::store::Store;
use futures::{Future, FutureExt};
use std::marker::PhantomData;

/// The struct for simulate leaves in the tree. Implements `Handler` trait.
///
/// `Leaf` exists due to nonexistence of the specialization in the stable Rust. It have multiply
/// `From` implementation for generics `F` where `F` is a functions with different number of the
/// arguments. Each argument must be should be received from the incoming `Data` in the `Handler::handler`
/// method, where `Data` must implement `Store<Argument>`.
pub struct Leaf<H, Args, Type, Fut> {
    handler: H,
    _phantom: PhantomData<(Args, Type, Fut)>,
}

pub struct EventOwned<E>(pub E);
pub struct StoreData<T>(pub T);

// These types are pub but not exported anywhere. This is due to compiler restrictions of the
// usage of the private types.
pub struct EventDataType;
pub struct NothingDataType;
pub struct StoreDataType;

impl<E, F, Fut> From<F> for Leaf<F, E, EventDataType, Fut>
where
    F: Fn(EventOwned<E>) -> Fut,
    Fut: Future + Send + 'static,
{
    fn from(f: F) -> Self {
        Leaf {
            handler: f,
            _phantom: PhantomData,
        }
    }
}

impl<F, Fut> From<F> for Leaf<F, (), NothingDataType, Fut>
where
    F: Fn() -> Fut,
    Fut: Future + Send + 'static,
{
    fn from(f: F) -> Self {
        Leaf {
            handler: f,
            _phantom: PhantomData,
        }
    }
}

impl<F, A1, Fut> From<F> for Leaf<F, (A1,), StoreDataType, Fut>
where
    F: Fn(StoreData<A1>) -> Fut,
    Fut: Future + Send + 'static,
{
    fn from(f: F) -> Self {
        Leaf {
            handler: f,
            _phantom: PhantomData,
        }
    }
}

impl<F, A1, A2, Fut> From<F> for Leaf<F, (A1, A2), StoreDataType, Fut>
where
    F: Fn(StoreData<A1>, StoreData<A2>) -> Fut,
    Fut: Future + Send + 'static,
{
    fn from(f: F) -> Self {
        Leaf {
            handler: f,
            _phantom: PhantomData,
        }
    }
}

impl<H, E, Res, Fut> Handler<E> for Leaf<H, E, EventDataType, Fut>
where
    H: Fn(EventOwned<E>) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    E: 'static,
    Res: 'static,
{
    type Res = Res;
    fn handle(&self, event: E) -> HandlerFuture<Res, E> {
        Box::pin((self.handler)(EventOwned(event)).map(Ok))
    }
}

impl<H, S, Res, Fut> Handler<S> for Leaf<H, (), NothingDataType, Fut>
where
    H: Fn() -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    S: 'static,
    Res: 'static,
{
    type Res = Res;
    fn handle(&self, _: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)().map(Ok))
    }
}

impl<H, S, Res, Fut, A1> Handler<S> for Leaf<H, (A1,), StoreDataType, Fut>
where
    H: Fn(StoreData<A1>) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    S: Store<A1> + 'static,
    Res: 'static,
{
    type Res = Res;
    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(StoreData(store.get())).map(Ok))
    }
}

impl<H, S, Res, Fut, A1, A2> Handler<S> for Leaf<H, (A1, A2), StoreDataType, Fut>
where
    H: Fn(StoreData<A1>, StoreData<A2>) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    S: Store<A1> + Store<A2> + 'static,
    Res: 'static,
{
    type Res = Res;
    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(StoreData(store.get()), StoreData(store.get())).map(Ok))
    }
}
