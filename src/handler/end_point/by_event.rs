//! `EndPointByEvent` handler and traits.
//!
//!

use crate::handler::end_point::EndPoint;
use crate::handler::HandlerFuture;
use crate::Handler;
use futures::FutureExt;
use std::future::Future;
use std::marker::PhantomData;

/// EndPoint that accepts a function with input event as first and one arg.
///
/// Can be constructed only via `EndPoint::enter_event`.
///
/// Basic usage:
/// ```
/// use dptree::Handler;
/// use dptree::handler::{EndPoint, end_point::by_event::EndPointByEventEnter};
///
/// # #[tokio::main]
/// # async fn main() {
/// // Creating handler that multiply input number.
/// let multiply = EndPoint::by_event(|num: u32| async move { num * 2 });
/// let four = multiply.handle(2).await.unwrap();
/// assert_eq!(four, 4);
///
/// // Creating handler that just return some value.
/// let get_value = EndPoint::<()>::by_event(|| async move { 10u32 });
/// let ten = get_value.handle(()).await.unwrap();
/// assert_eq!(ten, 10);
/// # }
/// ```
pub struct EndPointByEvent<H, Need> {
    handler: H,
    _phantom: PhantomData<Need>,
}

/// Provides a way to construct `EndPointByEvent`.
pub trait EndPointByEventEnter<F, Event, Need> {
    fn by_event(func: F) -> EndPointByEvent<F, Need>;
}

impl<F, Event, Need> EndPointByEventEnter<F, Event, Need> for EndPoint<Event>
where
    EndPointByEvent<F, Need>: Handler<Event>,
{
    fn by_event(func: F) -> EndPointByEvent<F, Need> {
        EndPointByEvent {
            handler: func,
            _phantom: PhantomData,
        }
    }
}

impl<H, E, Res, Fut> Handler<E> for EndPointByEvent<H, EventNeed>
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

impl<H, E, Res, Fut> Handler<E> for EndPointByEvent<H, EventNotNeed>
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

pub fn end_point<F, Event, Need>(func: F) -> EndPointByEvent<F, Need>
where
    EndPoint<Event>: EndPointByEventEnter<F, Event, Need>,
{
    EndPoint::by_event(func)
}
