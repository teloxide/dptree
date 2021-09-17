use crate::handler::endpoint::Endpoint;
use crate::handler::HandlerFuture;
use crate::store::Store;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;
use std::sync::Arc;

/// Endpoint that accepts a function with many args that are stored in an incoming event.
///
/// This `Endpoint` implements the DI (Dependency Injection) pattern. It accepts input event
/// that must store all handler function args, extracts them and then call function with extracted
/// parameters.
///
/// `EndpointByStore` implements extracting from generic storage, so you can use any implementation of
/// DI storage. Only thing you need to do is implement `Storage` trait for this storage.
///
/// Can be constructed only via `Endpoint::by_store`.
///
/// Basic usage:
/// ```
/// # use std::sync::Arc;
/// use dptree::Handler;
/// use dptree::handler::{Endpoint, endpoint::by_store::EndpointByStoreEnter};
///
/// # #[tokio::main]
/// # async fn main() {
///
/// // Creating store.
/// let mut store = dptree::store::TypeMapPanickableStore::new();
///
/// // Creating endpoint.
/// let extractor = Endpoint::by_store(
///     |num: Arc<u32>, string: Arc<String>| async move {
///         (num, string)
///     });
///
/// // Inserting data to store.
/// store.insert(10u32);
/// store.insert("Hello".to_string());
///
/// let (num, string) = extractor.handle(store).await.unwrap();
/// assert_eq!(*num, 10);
/// assert_eq!(&*string, "Hello");
/// # }
/// ```
pub struct EndpointByStore<H, Args> {
    handler: H,
    _phantom: PhantomData<Args>,
}

/// Provides a way to construct `EndpointByStore`.
pub trait EndpointByStoreEnter<F, Args> {
    fn by_store(func: F) -> EndpointByStore<F, Args>;
}

impl<F, Fut, A1> EndpointByStoreEnter<F, (A1,)> for Endpoint<()>
where
    F: Fn(Arc<A1>) -> Fut,
    Fut: Future + Send + 'static,
    Fut::Output: 'static,
{
    fn by_store(func: F) -> EndpointByStore<F, (A1,)> {
        EndpointByStore {
            handler: func,
            _phantom: PhantomData,
        }
    }
}
impl<F, Fut, A1, A2> EndpointByStoreEnter<F, (A1, A2)> for Endpoint<()>
where
    F: Fn(Arc<A1>, Arc<A2>) -> Fut,
    Fut: Future + Send + 'static,
    Fut::Output: 'static,
{
    fn by_store(func: F) -> EndpointByStore<F, (A1, A2)> {
        EndpointByStore {
            handler: func,
            _phantom: PhantomData,
        }
    }
}
impl<H, S, Res, Fut, A1> Handler<S> for EndpointByStore<H, (A1,)>
where
    H: Fn(Arc<A1>) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    Res: 'static,
    S: Store<A1> + 'static,
{
    type Output = Res;

    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get()).map(Ok))
    }
}

impl<H, S, Res, Fut, A1, A2> Handler<S> for EndpointByStore<H, (A1, A2)>
where
    H: Fn(Arc<A1>, Arc<A2>) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    Res: 'static,
    S: Store<A1> + Store<A2> + 'static,
{
    type Output = Res;

    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get(), store.get()).map(Ok))
    }
}

pub fn endpoint_by_store<F, Event, Args>(func: F) -> EndpointByStore<F, Args>
where
    Endpoint<Event>: EndpointByStoreEnter<F, Args>,
{
    Endpoint::by_store(func)
}
