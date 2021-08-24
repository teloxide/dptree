use crate::handler::leaf::Leaf;
use crate::handler::HandlerFuture;
use crate::store::Store;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;

/// Leaf that accepts a function with many args that are stored in an incoming event.
///
/// This `Leaf` implements the DI (Dependency Injection) pattern. It accepts input event
/// that must store all handler function args, extracts them and then call function with extracted
/// parameters.
///
/// `LeafByStore` implements extracting from generic storage, so you can use any implementation of
/// DI storage. Only thing you need to do is implement `Store` trait for this storage.
///
/// Can be constructed only via `Leaf::enter_store`.
///
/// Basic usage:
/// ```
/// # use std::sync::Arc;
/// use dispatch_tree::Handler;
/// use dispatch_tree::handler::{Leaf, leaf::by_store::LeafStoreEnter};
///
/// # #[tokio::main]
/// # async fn main() {
///
/// // Creating store.
/// let mut store = dispatch_tree::store::TypeMapPanickableStore::new();
///
/// // Creating leaf.
/// let extractor = Leaf::enter_store(
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
pub struct LeafByStore<H, Args> {
    handler: H,
    _phantom: PhantomData<Args>,
}

/// Provides a way to construct `LeafByStore`.
pub trait LeafStoreEnter<F, Args> {
    fn enter_store(func: F) -> LeafByStore<F, Args>;
}

impl<F, Fut, A1> LeafStoreEnter<F, (A1,)> for Leaf<()>
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
impl<F, Fut, A1, A2> LeafStoreEnter<F, (A1, A2)> for Leaf<()>
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
