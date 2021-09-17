//! `EndpointByEvent` handler and traits.
//!
//!

use crate::handler::endpoint::Endpoint;
use crate::handler::HandlerFuture;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;

/// Endpoint that accepts a function with input event as first and one arg.
///
/// Can be constructed only via `Endpoint::enter_event`.
///
/// Basic usage:
/// ```
/// use dptree::Handler;
/// use dptree::handler::{Endpoint, endpoint::by_event::EndpointByEventEnter};
///
/// # #[tokio::main]
/// # async fn main() {
/// // Creating handler that multiply input number.
/// let multiply = Endpoint::by_event(|num: u32| async move { num * 2 });
/// let four = multiply.handle(2).await.unwrap();
/// assert_eq!(four, 4);
///
/// // Creating handler that just return some value.
/// let get_value = Endpoint::<()>::by_event(|| async move { 10u32 });
/// let ten = get_value.handle(()).await.unwrap();
/// assert_eq!(ten, 10);
/// # }
/// ```
pub struct EndpointByEvent<H, Need> {
    handler: H,
    _phantom: PhantomData<Need>,
}

/// Provides a way to construct `EndpointByEvent`.
pub trait EndpointByEventEnter<F, Event, Need> {
    fn by_event(func: F) -> EndpointByEvent<F, Need>;
}

impl<F, Event, Need> EndpointByEventEnter<F, Event, Need> for Endpoint<Event>
where
    EndpointByEvent<F, Need>: Handler<Event>,
{
    fn by_event(func: F) -> EndpointByEvent<F, Need> {
        EndpointByEvent {
            handler: func,
            _phantom: PhantomData,
        }
    }
}

impl<H, E, Res, Fut> Handler<E> for EndpointByEvent<H, EventNeed>
where
    H: Fn(E) -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    E: 'static,
    Res: 'static,
{
    type Output = Res;

    fn handle(&self, event: E) -> HandlerFuture<Res, E> {
        Box::pin((self.handler)(event).map(Ok))
    }
}

impl<H, E, Res, Fut> Handler<E> for EndpointByEvent<H, EventNotNeed>
where
    H: Fn() -> Fut,
    Fut: Future<Output = Res> + Send + 'static,
    E: 'static,
    Res: 'static,
{
    type Output = Res;

    fn handle(&self, _: E) -> HandlerFuture<Res, E> {
        Box::pin((self.handler)().map(Ok))
    }
}

pub struct EventNeed;
pub struct EventNotNeed;

pub fn endpoint<F, Event, Need>(func: F) -> EndpointByEvent<F, Need>
where
    Endpoint<Event>: EndpointByEventEnter<F, Event, Need>,
{
    Endpoint::by_event(func)
}
