use crate::handler::end_point::EndPoint;
use crate::handler::HandlerFuture;
use crate::store::Store;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;
use std::sync::Arc;

/// EndPoint that accepts a function with many args that are stored in an incoming event.
///
/// This `EndPoint` implements the DI (Dependency Injection) pattern. It accepts input event
/// that must store all handler function args, extracts them and then call function with extracted
/// parameters.
///
/// `EndPointByStore` implements extracting from generic storage, so you can use any implementation of
/// DI storage. Only thing you need to do is implement `Storage` trait for this storage.
///
/// Can be constructed only via `EndPoint::by_store`.
///
/// Basic usage:
/// ```
/// # use std::sync::Arc;
/// use dptree::Handler;
/// use dptree::handler::{EndPoint, end_point::by_store::EndPointByStoreEnter};
///
/// # #[tokio::main]
/// # async fn main() {
///
/// // Creating store.
/// let mut store = dptree::store::TypeMapPanickableStore::new();
///
/// // Creating endpoint.
/// let extractor = EndPoint::by_store(
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
pub struct EndPointByStore<H, Args> {
    handler: H,
    _phantom: PhantomData<Args>,
}

/// Provides a way to construct `EndPointByStore`.
pub trait EndPointByStoreEnter<F, Args> {
    fn by_store(func: F) -> EndPointByStore<F, Args>;
}

impl<F, Fut, A1> EndPointByStoreEnter<F, (A1,)> for EndPoint<()>
where
    F: Fn(Arc<A1>) -> Fut,
    Fut: Future + Send + 'static,
    Fut::Output: 'static,
{
    fn by_store(func: F) -> EndPointByStore<F, (A1,)> {
        EndPointByStore {
            handler: func,
            _phantom: PhantomData,
        }
    }
}
impl<F, Fut, A1, A2> EndPointByStoreEnter<F, (A1, A2)> for EndPoint<()>
where
    F: Fn(Arc<A1>, Arc<A2>) -> Fut,
    Fut: Future + Send + 'static,
    Fut::Output: 'static,
{
    fn by_store(func: F) -> EndPointByStore<F, (A1, A2)> {
        EndPointByStore {
            handler: func,
            _phantom: PhantomData,
        }
    }
}
impl<H, S, Res, Fut, A1> Handler<S> for EndPointByStore<H, (A1,)>
where
    H: Fn(Arc<A1>) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    Res: 'static,
    S: Store<A1> + 'static,
{
    type Res = Res;

    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get()).map(Ok))
    }
}

impl<H, S, Res, Fut, A1, A2> Handler<S> for EndPointByStore<H, (A1, A2)>
where
    H: Fn(Arc<A1>, Arc<A2>) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    Res: 'static,
    S: Store<A1> + Store<A2> + 'static,
{
    type Res = Res;

    fn handle(&self, store: S) -> HandlerFuture<Res, S> {
        Box::pin((self.handler)(store.get(), store.get()).map(Ok))
    }
}

pub fn end_point_by_store<F, Event, Args>(func: F) -> EndPointByStore<F, Args>
where
    EndPoint<Event>: EndPointByStoreEnter<F, Args>,
{
    EndPoint::by_store(func)
}
