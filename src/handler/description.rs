//! Built-in handler description types.

mod interest_set;
mod unspecified;

pub use interest_set::{EventKind, InterestSet};
pub use unspecified::Unspecified;

/// Handler description.
///
/// This trait allows information to flow "back up" the tree, allowing a user to
/// check its structure.
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
/// assert_count(1, dptree::entry().branch(dptree::inspect(|| ())));
/// assert_count(
///     5,
///     dptree::entry()
///         .branch(
///             dptree::entry()
///                 .branch(dptree::entry().branch(dptree::filter(|| true)))
///                 .branch(dptree::entry().chain(dptree::filter(|| false))),
///         )
///         .branch(dptree::inspect(|| ())),
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
