//! Endpoint handlers and builders.
//!
//! `Endpoint` in dispatching tree means node that always handle the incoming event regardless of its
//! characteristics. Usually `Endpoint`s follows the `Filter`s where `Filter`s decides whether
//! an event is suitable for a handler or not and `Endpoint` only handle the event.
//!
//! There are two kind of `Endpoint`s in the library: `EndpointByEvent` and `EndpointByStore`, but
//! you can also make your own endpoints if you need.
//!
//! Only way to construct `Endpoint`s is via `Endpoint` struct methods.

pub mod by_event;
pub mod by_store;

/// Namespace for constructing `Endpoint`s handlers.
pub struct Endpoint<Event>(std::marker::PhantomData<Event>);
