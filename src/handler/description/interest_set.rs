use crate::HandlerDescription;

use std::{
    collections::HashSet,
    hash::{BuildHasher, Hash, RandomState},
};

/// Description for a handler that describes what event kinds are interesting to
/// the handler.
///
/// This can be useful if you can filter events before they are passed to
/// `dptree`. In this case you should keep updates that are in the `observed`
/// set.
#[derive(Debug, Clone)]
pub struct InterestSet<K, S = RandomState> {
    /// Event kinds that are of interested for a given handler.
    ///
    /// I.e. the ones that can cause meaningful side-effects.
    pub observed: HashSet<K, S>,

    /// Event kinds that can be observed by handlers chained to this one.
    pub filtered: HashSet<K, S>,
}

/// An event kind that can be used with [`InterestSet`].
///
/// Usually this would be implemented by a field-less enumeration of all update
/// kinds.
pub trait EventKind<S = RandomState>: Sized {
    /// Set of all event kinds.
    fn full_set() -> HashSet<Self, S>;

    /// An empty set.
    fn empty_set() -> HashSet<Self, S>;
}

impl<K: EventKind<S>, S> InterestSet<K, S> {
    /// Constructs an [`InterestSet`] for a filter that allows to pass through
    /// it only updates with kinds in the `filtered` set.
    ///
    /// Note that the filter should not have observable side-effects, for
    /// example:
    /// ```
    /// use dptree::{description::{InterestSet, EventKind}, filter_with_description};
    /// use std::collections::HashSet;
    ///
    /// # enum K {} impl EventKind for K { fn full_set() -> HashSet<Self> { HashSet::default() } fn empty_set() -> HashSet<Self> { HashSet::default() } }
    /// # let _: dptree::Handler<(), InterestSet<K>> =
    /// filter_with_description(InterestSet::new_filter(HashSet::new()), || {
    ///     println!("Filter called!"); // <-- bad
    ///
    ///     false
    /// });
    ///
    /// # #[derive(Clone)] struct Db; impl Db { fn fetch_enabled(&self) -> bool { false } }
    /// # let _: dptree::Handler<(), InterestSet<K>> =
    /// filter_with_description(InterestSet::new_filter(HashSet::new()), |db: Db| {
    ///     let pass = db.fetch_enabled(); // <-- fine
    ///
    ///     pass
    /// });
    /// ```
    pub fn new_filter(filtered: HashSet<K, S>) -> Self {
        // We assume that well behaved filters don't observe anything (filters should
        // only filter!).
        Self { observed: K::empty_set(), filtered }
    }
}

impl<T, S> HandlerDescription for InterestSet<T, S>
where
    T: EventKind<S> + Eq + Hash + Clone,
    S: BuildHasher + Clone,
    T: Send + Sync + 'static,
    S: Send + Sync + 'static,
{
    fn entry() -> Self {
        // Entry does not observe anything and allows everything.
        Self { observed: T::empty_set(), filtered: T::full_set() }
    }

    fn user_defined() -> Self {
        // We don't know what user defined code does, so we assume the most forgiving
        // case: it observes everything and allows everything.
        //
        // Were we to choose anything else, there would be user code that would be
        // broken.
        Self { observed: T::full_set(), filtered: T::full_set() }
    }

    fn endpoint() -> Self {
        // Endpoint observes everything (again, as per the same reasoning as
        // `user_defined`), but does not allow anything to pass through (it's
        // the last handler).
        Self { observed: T::full_set(), filtered: T::empty_set() }
    }

    fn merge_chain(&self, other: &Self) -> Self {
        let Self { observed: l_obs, filtered: l_flt } = self;
        let Self { observed: r_obs, filtered: r_flt } = other;

        // Second handler can only observe things that were passed through by the first
        // one.
        let observed = {
            let hasher = l_obs.hasher().clone();
            let mut tmp = HashSet::with_hasher(hasher);

            tmp.extend(l_obs.iter().cloned());
            tmp.extend(l_flt.intersection(r_obs).cloned());
            tmp
        };

        // If we chain two filters together, we are only passing through events that can
        // pass both of them.
        let filtered = {
            let hasher = l_flt.hasher().clone();
            let mut tmp = HashSet::with_hasher(hasher);

            tmp.extend(l_flt.intersection(r_flt).cloned());
            tmp
        };

        Self { filtered, observed }
    }

    fn merge_branch(&self, other: &Self) -> Self {
        let Self { observed: l_obs, filtered: l_flt } = self;
        let Self { observed: r_obs, filtered: _ } = other;

        // Second handler can only observe things that were passed through by the first
        // one.
        let observed = {
            let hasher = l_obs.hasher().clone();
            let mut tmp = HashSet::with_hasher(hasher);

            tmp.extend(l_obs.iter().cloned());
            tmp.extend(l_flt.intersection(r_obs).cloned());
            tmp
        };

        // Even if second filter did not pass something through, the execution still
        // continues.
        let filtered = l_flt.clone();

        Self { observed, filtered }
    }
}

impl<K: Hash + Eq, S: BuildHasher> Eq for InterestSet<K, S> {}

impl<K: Hash + Eq, S: BuildHasher> PartialEq for InterestSet<K, S> {
    fn eq(&self, other: &Self) -> bool {
        let Self { observed: l_obs, filtered: l_flt } = self;
        let Self { observed: r_obs, filtered: r_flt } = other;

        l_obs == r_obs && l_flt == r_flt
    }
}
