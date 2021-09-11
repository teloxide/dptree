//! EndPoint handlers and builders.
//!
//! `EndPoint` in dispatching tree means node that always handle the incoming event regardless of its
//! characteristics. Usually `EndPoint`s follows the `Filter`s where `Filter`s decides whether
//! an event is suitable for a handler or not and `EndPoint` only handle the event.
//!
//! There are two kind of `EndPoint`s in the library: `EndPointByEvent` and `EndPointByStore`, but
//! you can also make your own endpoints if you need.
//!
//! Only way to construct `EndPoint`s is via `EndPoint` struct methods.

pub mod by_event;
pub mod by_store;

/// Namespace for constructing `EndPoint`s handlers.
pub struct EndPoint<Event>(std::marker::PhantomData<Event>);
