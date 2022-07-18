//! Built-in handler description types.

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

    /// Description for [`inspect`](crate::inspect).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn inspect() -> Self {
        Self::user_defined()
    }

    /// Description for [`inspect_async`](crate::inspect_async).
    ///
    /// ## Default implementation
    ///
    /// By default this returns the value from
    /// [`user_defined`](HandlerDescription::user_defined).
    #[track_caller]
    fn inspect_async() -> Self {
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
pub struct InterestList<K, S = RandomState> {
    /// Event kinds that are of interested for a given handler.
    pub observed: HashSet<K, S>,
    /// Event kinds that can be observed by handlers chained to this one.
    pub filtered: HashSet<K, S>,
}

/// An event kind that can be used with [`InterestList`].
pub trait EventKind<S = RandomState>: Sized {
    /// Set of all event kinds.
    fn full_set() -> HashSet<Self, S>;

    /// An empty set.
    fn empty_set() -> HashSet<Self, S>;
}

impl<K: EventKind<S>, S> InterestList<K, S> {
    pub fn new_filter(filtered: HashSet<K, S>) -> Self {
        Self { observed: K::empty_set(), filtered }
    }
}

impl<T, S> HandlerDescription for InterestList<T, S>
where
    T: EventKind<S> + Eq + Hash + Clone,
    S: BuildHasher + Clone,
    T: Send + Sync + 'static,
    S: Send + Sync + 'static,
{
    fn entry() -> Self {
        InterestList { observed: T::empty_set(), filtered: T::full_set() }
    }

    fn user_defined() -> Self {
        InterestList { observed: T::full_set(), filtered: T::full_set() }
    }

    fn merge_chain(&self, other: &Self) -> Self {
        let InterestList { observed: l_obs, filtered: l_flt } = self;
        let InterestList { observed: r_obs, filtered: r_flt } = other;

        // If we chain two filters together, we are only interested in events that can
        // pass both of them.
        let filtered = {
            let hasher = l_flt.hasher().clone();
            let mut tmp = HashSet::with_hasher(hasher);

            tmp.extend(l_flt.intersection(r_flt).cloned());
            tmp
        };

        // Second handler can only observe things that were output by the first one.
        let observed = {
            let hasher = l_obs.hasher().clone();
            let mut tmp = HashSet::with_hasher(hasher);

            tmp.extend(l_obs.iter().cloned());
            tmp.extend(l_flt.intersection(r_obs).cloned());
            tmp
        };

        InterestList { filtered, observed }
    }

    fn merge_branch(&self, other: &Self) -> Self {
        let InterestList { observed: l_obs, filtered: l_flt } = self;
        let InterestList { observed: r_obs, filtered: _ } = other;

        // Second handler can only observe things that were output by the first one.
        let observed = {
            let hasher = l_obs.hasher().clone();
            let mut tmp = HashSet::with_hasher(hasher);

            tmp.extend(l_obs.iter().cloned());
            tmp.extend(l_flt.intersection(r_obs).cloned());
            tmp
        };

        // Even if second filter did not pass, the execution continues
        let filtered = l_flt.clone();

        InterestList { observed, filtered }
    }

    fn endpoint() -> Self {
        InterestList { observed: T::full_set(), filtered: T::empty_set() }
    }
}

impl<K: Hash + Eq, S: BuildHasher> Eq for InterestList<K, S> {}

impl<K: Hash + Eq, S: BuildHasher> PartialEq for InterestList<K, S> {
    fn eq(&self, other: &Self) -> bool {
        let InterestList { observed: l_obs, filtered: l_flt } = self;
        let InterestList { observed: r_obs, filtered: r_flt } = other;

        l_obs == r_obs && l_flt == r_flt
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
