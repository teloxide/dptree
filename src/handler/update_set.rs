use std::{
    collections::HashSet,
    hash::{BuildHasher, Hash},
};

pub trait UpdateSet: Send + Sync + 'static {
    /// This should behave as a "set of all possible update kinds"
    fn unknown() -> Self;

    /// Returns a value that doesn't affect anything.
    fn invisible() -> Self;

    fn union(&self, other: &Self) -> Self;

    fn intersection(&self, other: &Self) -> Self;
}

// named option
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum MaybeSpecial<T> {
    Known(T),
    Unknown,
    Invisible,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unspecified(());

impl<T, S> UpdateSet for MaybeSpecial<HashSet<T, S>>
where
    T: Eq + Hash + Clone,
    S: BuildHasher + Clone,
    T: Send + Sync + 'static,
    S: Send + Sync + 'static,
{
    fn unknown() -> Self {
        MaybeSpecial::Unknown
    }

    fn invisible() -> Self {
        MaybeSpecial::Invisible
    }

    fn union(&self, other: &Self) -> Self {
        use MaybeSpecial::*;

        match (self, other) {
            (Invisible, other) | (other, Invisible) => other.clone(),
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
        use MaybeSpecial::*;

        match (self, other) {
            (Invisible, other) | (other, Invisible) => other.clone(),
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

    fn invisible() -> Self {
        Self(())
    }

    fn union(&self, _other: &Self) -> Self {
        Self(())
    }

    fn intersection(&self, _other: &Self) -> Self {
        Self(())
    }
}
