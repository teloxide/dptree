use std::{
    collections::HashSet,
    hash::{BuildHasher, Hash},
};

pub trait UpdateSet: Send + Sync + 'static {
    /// This should behave as a "set of all possible update kinds"
    fn unknown() -> Self;

    fn union(&self, other: &Self) -> Self;

    fn intersection(&self, other: &Self) -> Self;
}

// named option
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum MaybeUnknown<T> {
    Known(T),
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unspecified(());

impl<T, S> UpdateSet for MaybeUnknown<HashSet<T, S>>
where
    T: Eq + Hash + Clone,
    S: BuildHasher + Clone,
    T: Send + Sync + 'static,
    S: Send + Sync + 'static,
{
    fn unknown() -> Self {
        MaybeUnknown::Unknown
    }

    fn union(&self, other: &Self) -> Self {
        use MaybeUnknown::*;

        match (self, other) {
            (Known(l), Known(r)) => {
                let hasher = l.hasher().clone();
                let mut res = HashSet::with_hasher(hasher);
                res.extend(l.union(r).cloned());

                Known(res)
            }
            (Unknown, _) | (_, Unknown) => Unknown,
        }
    }

    fn intersection(&self, other: &Self) -> Self {
        use MaybeUnknown::*;

        match (self, other) {
            (Known(l), Known(r)) => {
                let hasher = l.hasher().clone();
                let mut res = HashSet::with_hasher(hasher);
                res.extend(l.intersection(r).cloned());

                Known(res)
            }
            (Known(known), Unknown) | (Unknown, Known(known)) => Known(known.clone()),
            (Unknown, Unknown) => Unknown,
        }
    }
}

impl UpdateSet for Unspecified {
    fn unknown() -> Self {
        Self(())
    }

    fn union(&self, _other: &Self) -> Self {
        Self(())
    }

    fn intersection(&self, _other: &Self) -> Self {
        Self(())
    }
}
