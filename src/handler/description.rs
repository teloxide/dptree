use core::mem;
use std::{
    collections::{hash_map::RandomState, HashSet},
    hash::{BuildHasher, Hash},
};

/// Handler description.
///
/// This trait allows information to flow "back up" the tree, allowing to check
/// its structure.
pub trait HandlerDescription: Send + Sync + 'static {
    /// Description for [`entry`](crate::entry).
    fn entry() -> Self;

    /// Description for a user-defined handler that can do practically
    /// everything.
    fn user_defined() -> Self;

    /// Merge descriptions to get a description for a chain handler.
    fn merge_chain(&self, other: &Self) -> Self;

    /// Merge descriptions to get a description for a branch handler.
    fn merge_branch(&self, other: &Self) -> Self;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unspecified(());

/// Description for a handler that describes what event kinds are interesting to
/// the handler.
#[derive(Debug, Clone)]
pub enum EventKindDescription<K, S = RandomState> {
    /// Only event) kinds in the set are "interesting".
    InterestingEventKinds(HashSet<K, S>),
    /// Any event kind may be "interesting".
    UserDefined,
    /// No event kinds are "interesting", this handler doesn't do anything.
    Entry,
}

impl<T, S> HandlerDescription for EventKindDescription<T, S>
where
    T: Eq + Hash + Clone,
    S: BuildHasher + Clone,
    T: Send + Sync + 'static,
    S: Send + Sync + 'static,
{
    fn entry() -> Self {
        EventKindDescription::Entry
    }

    fn user_defined() -> Self {
        EventKindDescription::UserDefined
    }

    fn merge_chain(&self, other: &Self) -> Self {
        use EventKindDescription::*;

        match (self, other) {
            // If we chain anything with entry, then we are only interested in events that are
            // interesting from POV of the non-entry handler (this is because `entry` doesn't
            // observe anything).
            (Entry, other) | (other, Entry) => other.clone(),
            // If we chain two filters together, we are only interested in events that can
            // pass either of them.
            (InterestingEventKinds(l), InterestingEventKinds(r)) => {
                let hasher = l.hasher().clone();
                let mut res = HashSet::with_hasher(hasher);

                res.extend(l.intersection(r).cloned());

                InterestingEventKinds(res)
            }
            // If we chain a filter with something user-defined (but not the other way around), then
            // we are interested only in things that could pass the filter.
            (InterestingEventKinds(known), UserDefined) => InterestingEventKinds(known.clone()),
            // If we chain something user-defined with anything than anything could be interesting,
            // since we don't know user intentions.
            (UserDefined, _) => UserDefined,
        }
    }

    fn merge_branch(&self, other: &Self) -> Self {
        use EventKindDescription::*;

        match (self, other) {
            // If we branch anything with entry, then we are only interested in events that are
            // interesting from POV of the non-entry handler (this is because `entry` doesn't
            // observe anything).
            (Entry, other) | (other, Entry) => other.clone(),
            // If we branch two filters together, we are interested in all events that are
            // interesting to either handler (they both can be executed).
            (InterestingEventKinds(l), InterestingEventKinds(r)) => {
                let hasher = l.hasher().clone();
                let mut res = HashSet::with_hasher(hasher);
                res.extend(l.union(r).cloned());

                InterestingEventKinds(res)
            }
            // If either of the operands is user-defined, than we may be interested in anything.
            (UserDefined, _) | (_, UserDefined) => UserDefined,
        }
    }
}

impl<K: Hash + Eq, S: BuildHasher> Eq for EventKindDescription<K, S> {}

impl<K: Hash + Eq, S: BuildHasher> PartialEq for EventKindDescription<K, S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InterestingEventKinds(l), Self::InterestingEventKinds(r)) => l == r,
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
