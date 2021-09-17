//! Filter handler and builder.
//!
//! Filter is a handler that filters input event by some condition and call next handler if the
//! condition was satisfied. Returns error otherwise.
//!
//! Basic usage of filter:
//! ```
//! # #[tokio::main]
//! # async fn main() {
//! use dptree::Handler;
//!
//! let filter = dptree::filter(|&num: &u32| num == 10)
//!     .endpoint(|num| async move { num * 2 });
//!
//! let result_with_10 = filter.handle(10u32).await;
//! assert_eq!(result_with_10, Ok(20));
//!
//! let result_with_2 = filter.handle(2u32).await;
//! assert_eq!(result_with_2, Err(2));
//! # }
//! ```

use crate::builder::HandlerBuilder;
use crate::handler::endpoint::by_event::{EndpointByEvent, EndpointByEventEnter};
use crate::handler::{Endpoint, Handler, HandlerFuture};
use std::marker::PhantomData;

/// Struct that filtering event by a condition.
///
/// If the event satisfy the condition then handler will be called. Otherwise event will be sent up.
///
/// Basic usage:
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::handler::filter::Filter;
/// use dptree::Handler;
///
/// let filter = Filter::new(
///     |&data: &u32| data == 10, // filter event by condition
///     |data: u32| async move { Ok(()) }, // just return Ok(()) if event satisfy the condition
/// );
///
/// let result_with_10 = filter.handle(10u32).await;
/// assert_eq!(result_with_10, Ok(()));
///
/// let result_with_2 = filter.handle(2u32).await;
/// assert_eq!(result_with_2, Err(2));
/// # }
/// ```
pub struct Filter<F, H> {
    condition: F,
    handler: H,
}

impl<F, H> Filter<F, H> {
    /// Condition must be function `Fn(&Data) -> bool`, handler must implements `Handler<Data>`
    pub fn new(condition: F, handler: H) -> Self {
        Filter { condition, handler }
    }

    pub fn into_inner(self) -> (F, H) {
        (self.condition, self.handler)
    }
}

impl<F, H, Data, Res> Handler<Data> for Filter<F, H>
where
    F: Fn(&Data) -> bool + Send + Sync,
    H: Handler<Data, Output = Res> + Send + Sync,
    Data: Send + Sync + 'static,
    Res: Send + 'static,
{
    type Output = Res;
    fn handle(&self, data: Data) -> HandlerFuture<Res, Data> {
        match (self.condition)(&data) {
            true => Box::pin(self.handler.handle(data)),
            false => Box::pin(futures::future::err(data)),
        }
    }
}

/// Builder for the `Filter` struct
///
/// Basic usage:
/// ```
/// use dptree::HandlerBuilder;
/// use dptree::handler::filter::FilterBuilder;
///
/// let filter1 = FilterBuilder::new(|&data: &u32| data == 0)
///     .and_then(|data: u32| async move { Ok(()) });
///
/// let filter2 = FilterBuilder::new(|&data: &u32| data == 0)
///     .endpoint(|data: u32| async move { data * 2 });
/// ```
pub struct FilterBuilder<F, Data> {
    condition: F,
    _phantom: PhantomData<Data>,
}

impl<F, Data> FilterBuilder<F, Data>
where
    F: Fn(&Data) -> bool + Send + Sync,
{
    /// Creates `FilterBuilder`. Requires a condition for construction.
    pub fn new(condition: F) -> Self {
        FilterBuilder {
            condition,
            _phantom: PhantomData,
        }
    }

    /// Shortcut for `builder.and_then(Endpoint::by_event(func))`.
    pub fn endpoint<Func, Need>(self, func: Func) -> Filter<F, EndpointByEvent<Func, Need>>
    where
        Func: Send + Sync,
        Need: Send + Sync,
        Data: Send + Sync + 'static,
        EndpointByEvent<Func, Need>: Handler<Data>,
        <EndpointByEvent<Func, Need> as Handler<Data>>::Output: Send + Sync + 'static,
    {
        self.and_then(Endpoint::by_event(func))
    }
}

impl<F, Event, H, Res> HandlerBuilder<Event, H> for FilterBuilder<F, Event>
where
    F: Fn(&Event) -> bool + Send + Sync,
    H: Handler<Event, Output = Res> + Send + Sync,
    Event: Send + Sync + 'static,
    Res: Send + 'static,
{
    type OutEvent = Event;
    type ResultAndThen = Filter<F, H>;

    fn and_then(self, handler: H) -> Self::ResultAndThen {
        Filter {
            condition: self.condition,
            handler,
        }
    }
}

/// Shortcut for `FilterBuilder::new`.
pub fn filter<F, Data>(condition: F) -> FilterBuilder<F, Data>
where
    F: Fn(&Data) -> bool + Send + Sync,
{
    FilterBuilder::new(condition)
}
