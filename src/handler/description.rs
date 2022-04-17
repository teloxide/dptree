//! Built-in handler description types.

use core::mem;
use std::{
    collections::{hash_map::RandomState, HashSet},
    hash::{BuildHasher, Hash},
};

/// Handler description.
///
/// This trait allows information to flow "back up" the tree, allowing to check
/// its structure.
///
/// ## Examples
///
/// Count how many branches are in the tree:
///
/// ```
/// use dptree::{prelude::DependencyMap, Handler, HandlerDescription};
///
/// struct CountBranches(u32);
///
/// impl HandlerDescription for CountBranches {
///     fn entry() -> Self {
///         Self(0)
///     }
///
///     fn user_defined() -> Self {
///         Self(0)
///     }
///
///     fn merge_chain(&self, other: &Self) -> Self {
///         Self(self.0 + other.0)
///     }
///
///     fn merge_branch(&self, other: &Self) -> Self {
///         Self(self.0 + other.0 + 1)
///     }
/// }
///
/// #[track_caller]
/// fn assert_count(count: u32, handler: Handler<DependencyMap, (), CountBranches>) {
///     assert_eq!(handler.description().0, count);
/// }
///
/// assert_count(0, dptree::entry());
/// assert_count(1, dptree::entry().branch(dptree::entry()));
/// assert_count(
///     5,
///     dptree::entry()
///         .branch(
///             dptree::entry()
///                 .branch(dptree::entry().branch(dptree::filter(|| true)))
///                 .branch(dptree::entry().chain(dptree::filter(|| false))),
///         )
///         .branch(dptree::entry()),
/// );
/// ```
pub trait HandlerDescription: Sized + Send + Sync + 'static {
    /// Description for [`entry`](crate::entry).
    fn entry() -> Self;

    /// Description for a user-defined handler that can do practically
    /// everything.
    fn user_defined() -> Self;

    /// Merge descriptions to get a description for a chain handler.
    fn merge_chain(&self, other: &Self) -> Self;

    /// Merge descriptions to get a description for a branch handler.
    fn merge_branch(&self, other: &Self) -> Self;

    /// Description for [`map`](crate::map).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn map() -> Self {
        Self::user_defined()
    }

    /// Description for [`map_async`](crate::map_async).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn map_async() -> Self {
        Self::user_defined()
    }

    /// Description for [`filter`](crate::filter).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn filter() -> Self {
        Self::user_defined()
    }

    /// Description for [`filter_async`](crate::filter_async).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn filter_async() -> Self {
        Self::user_defined()
    }

    /// Description for [`filter_map`](crate::filter_map).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn filter_map() -> Self {
        Self::user_defined()
    }

    /// Description for [`filter_map_async`](crate::filter_map_async).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn filter_map_async() -> Self {
        Self::user_defined()
    }

    /// Description for [`endpoint`](crate::endpoint).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn endpoint() -> Self {
        Self::user_defined()
    }
}

/// Uninformative handler description.
#[derive(Debug, PartialEq, Eq)]
pub struct Unspecified(());

/// Description for a handler that describes what event kinds are interesting to
/// the handler.
#[derive(Debug, Clone)]
pub enum EventKind<K, S = RandomState> {
    /// Only event kinds in the set are "interesting".
    InterestList(HashSet<K, S>),
    /// Any event kind may be "interesting".
    UserDefined,
    /// No event kinds are "interesting", this handler doesn't do anything.
    Entry,
}

impl<T, S> HandlerDescription for EventKind<T, S>
where
    T: Eq + Hash + Clone,
    S: BuildHasher + Clone,
    T: Send + Sync + 'static,
    S: Send + Sync + 'static,
{
    fn entry() -> Self {
        EventKind::Entry
    }

    fn user_defined() -> Self {
        EventKind::UserDefined
    }

    fn merge_chain(&self, other: &Self) -> Self {
        use EventKind::*;

        match (self, other) {
            // If we chain anything with entry, then we are only interested in events that are
            // interesting from POV of the non-entry handler (this is because `entry` doesn't
            // observe anything).
            (Entry, other) | (other, Entry) => other.clone(),
            // If we chain two filters together, we are only interested in events that can
            // pass either of them.
            (InterestList(l), InterestList(r)) => {
                let hasher = l.hasher().clone();
                let mut res = HashSet::with_hasher(hasher);

                res.extend(l.intersection(r).cloned());

                InterestList(res)
            }
            // If we chain a filter with something user-defined (but not the other way around), then
            // we are interested only in things that could pass the filter.
            (InterestList(known), UserDefined) => InterestList(known.clone()),
            // If we chain something user-defined with anything than anything could be interesting,
            // since we don't know user intentions.
            (UserDefined, _) => UserDefined,
        }
    }

    fn merge_branch(&self, other: &Self) -> Self {
        use EventKind::*;

        match (self, other) {
            // If we branch anything with entry, then we are only interested in events that are
            // interesting from POV of the non-entry handler (this is because `entry` doesn't
            // observe anything).
            (Entry, other) | (other, Entry) => other.clone(),
            // If we branch two filters together, we are interested in all events that are
            // interesting to either handler (they both can be executed).
            (InterestList(l), InterestList(r)) => {
                let hasher = l.hasher().clone();
                let mut res = HashSet::with_hasher(hasher);
                res.extend(l.union(r).cloned());

                InterestList(res)
            }
            // If either of the operands is user-defined, than we may be interested in anything.
            (UserDefined, _) | (_, UserDefined) => UserDefined,
        }
    }
}

impl<K: Hash + Eq, S: BuildHasher> Eq for EventKind<K, S> {}

impl<K: Hash + Eq, S: BuildHasher> PartialEq for EventKind<K, S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InterestList(l), Self::InterestList(r)) => l == r,
            _ => mem::discriminant(self) == mem::discriminant(other),
        }
    }
}

impl HandlerDescription for Unspecified {
    fn entry() -> Self {
        Self(())
    }

    fn user_defined() -> Self {
        Self(())
    }

    fn merge_chain(&self, _other: &Self) -> Self {
        Self(())
    }

    fn merge_branch(&self, _other: &Self) -> Self {
        Self(())
    }
}
