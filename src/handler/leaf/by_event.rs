//! `LeafByEvent` handler and traits.
//!
//!

use crate::handler::leaf::Leaf;
use crate::handler::HandlerFuture;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;

/// Leaf that accepts a function with input event as first and one arg.
///
/// Can be constructed only via `Leaf::enter_event`.
///
/// Basic usage:
/// ```
/// use dispatch_tree::Handler;
/// use dispatch_tree::handler::{Leaf, leaf::by_event::LeafEventEnter};
///
/// # #[tokio::main]
/// # async fn main() {
/// // Creating handler that multiply input number.
/// let multiply = Leaf::enter_event(|num: u32| async move { num * 2 });
/// let four = multiply.handle(2).await.unwrap();
/// assert_eq!(four, 4);
///
/// // Creating handler that just return some value.
/// let get_value = Leaf::<()>::enter_event(|| async move { 10u32 });
/// let ten = get_value.handle(()).await.unwrap();
/// assert_eq!(ten, 10);
/// # }
/// ```
pub struct LeafByEvent<H, Need> {
    handler: H,
    _phantom: PhantomData<Need>,
}

/// Provides a way to construct `LeafByEvent`.
pub trait LeafEventEnter<F, Event, Need> {
    fn enter_event(func: F) -> LeafByEvent<F, Need>;
}

impl<F, Event, Need> LeafEventEnter<F, Event, Need> for Leaf<Event>
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
