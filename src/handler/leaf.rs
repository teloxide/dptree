//! Leaf handlers and builders.
//!
//! `Leaf` in dispatching tree means node that always handle the incoming event regardless of its
//! characteristics. Usually `Leaf`s follows the `Filter`s where `Filter`s decides decides whether
//! an event is suitable for a handler or not and `Leaf` only handle the event.
//!
//! There are two kind of `Leaf`s in the library: `LeafByEvent` and `LeafByStore`, but you can also
//! make your own leaf if you need.
//!
//! Only way to construct `Leaf`s is via `Leaf` struct methods.

pub mod by_event;
pub mod by_store;

/// Namespace for constructing `Leaf`s handlers.
pub struct Leaf<Event>(std::marker::PhantomData<Event>);
