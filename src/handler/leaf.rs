use std::marker::PhantomData;
use crate::handler::{Handler, HandlerFuture};
use futures::{Future, FutureExt};
use crate::store::Store;

/// The struct for simulate leaves in the tree. Implements `Handler` trait.
///
/// `Leaf` exists due to nonexistence of the specialization in the stable Rust. It have multiply
/// `From` implementation for generics `F` where `F` is a functions with different number of the
/// arguments. Each argument must be should be received from the incoming `Data` in the `Handler::handler`
/// method, where `Data` must implement `Store<Argument>`.
pub struct Leaf<H, Args, Fut> {
    handler: H,
    _phantom: PhantomData<(Args, Fut)>
}

impl<F, Fut> From<F> for Leaf<F, (), Fut>
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

impl<F, A1, Fut> From<F> for Leaf<F, (A1,), Fut>
where
    F: Fn(A1) -> Fut,
    Fut: Future + Send + 'static,
{
    fn from(f: F) -> Self {
        Leaf {
            handler: f,
            _phantom: PhantomData,
        }
    }
}

impl<F, A1, A2, Fut> From<F> for Leaf<F, (A1, A2), Fut>
where
    F: Fn(A1, A2) -> Fut,
    Fut: Future + Send + 'static,
{
    fn from(f: F) -> Self {
        Leaf {
            handler: f,
            _phantom: PhantomData,
        }
    }
}

impl<H, S, Res, Fut> Handler<S, Res> for Leaf<H, (), Fut>
where
    H: Fn() -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    S: 'static,
    Res: 'static,
{
    fn handle(&self, _: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)().map(Ok))
    }
}

impl<H, S, Res, Fut, A1> Handler<S, Res> for Leaf<H, (A1,), Fut>
where
    H: Fn(A1) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    S: Store<A1> + 'static,
    Res: 'static,
{
    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get()).map(Ok))
    }
}

impl<H, S, Res, Fut, A1, A2> Handler<S, Res> for Leaf<H, (A1, A2), Fut>
where
    H: Fn(A1, A2) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    S: Store<A1> + Store<A2> + 'static,
    Res: 'static,
{
    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get(), store.get()).map(Ok))
    }
}
