//! An implementation of [dependency injection].
//!
//! If you do not know what is dependency injection (DI), please read [this
//! discussion on StackOverflow], then come back. The only difference is that in
//! `dptree`, we inject objects into function-handlers, not into objects.
//!
//! Currently, the only container is [`DependencyMap`]. It implements the DI
//! pattern completely, but be careful: it can panic when you do not provide
//! necessary types. Most the times, it is profitable to use
//! [`crate::type_check`] to make sure that all types are provided _before_
//! executing a particular handler.
//!
//! [dependency injection]: https://en.wikipedia.org/wiki/Dependency_injection
//! [this discussion on StackOverflow]: https://stackoverflow.com/questions/130794/what-is-dependency-injection

use futures::future::{ready, BoxFuture};

use std::{
    any::{Any, TypeId},
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Formatter, Write},
    future::Future,
    iter::FromIterator,
    panic::Location,
    sync::Arc,
};

use crate::Type;

/// A DI container with multiple dependencies.
///
/// This DI container stores types by their corresponding type identifiers. It
/// cannot prove at compile-time that a type of a requested value exists within
/// the container, so if you do not provide necessary types but they were
/// requested, it will panic. However, you can use [`crate::type_check`] to make
/// sure that all types are provided _before_ executing a particular handler.
///
/// # Examples
///
/// ```
/// use dptree::di::DependencyMap;
/// use std::sync::Arc;
///
/// let mut container = DependencyMap::new();
/// container.insert(5_i32);
/// container.insert("abc");
///
/// assert_eq!(container.get(), Arc::new(5_i32));
/// assert_eq!(container.get(), Arc::new("abc"));
///
/// // If a type of a value already exists within the container, it will be replaced.
/// let old_value = container.insert(10_i32).unwrap();
///
/// assert_eq!(old_value, Arc::new(5_i32));
/// assert_eq!(container.get(), Arc::new(10_i32));
/// ```
///
/// When a value is not found within the container, it will panic:
///
/// ```should_panic
/// use dptree::di::DependencyMap;
/// use std::sync::Arc;
///
/// let mut container = DependencyMap::new();
/// container.insert(10i32);
/// container.insert(true);
/// container.insert("static str");
///
/// // `alloc::string::String` was requested, but not provided. Available types:
/// //     `i32`
/// //     `&str`
/// //     `bool`
/// let string: Arc<String> = container.get();
/// ```
#[derive(Default, Clone, Debug)]
pub struct DependencyMap {
    pub(crate) map: BTreeMap<TypeId, Dependency>,
}

#[derive(Clone)]
pub(crate) struct Dependency {
    pub(crate) type_name: &'static str,
    inner: Arc<dyn Any + Send + Sync>,
}

impl Debug for Dependency {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Dependency").field("type_name", &self.type_name).finish_non_exhaustive()
    }
}

impl PartialEq for DependencyMap {
    fn eq(&self, other: &Self) -> bool {
        let keys1 = self.map.keys();
        let keys2 = other.map.keys();
        keys1.zip(keys2).map(|(k1, k2)| k1 == k2).all(|x| x)
    }
}

impl DependencyMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a value into the container.
    ///
    /// If the container do not has this type present, `None` is returned.
    /// Otherwise, the value is updated, and the old value is returned.
    pub fn insert<T: Send + Sync + 'static>(&mut self, item: T) -> Option<Arc<T>> {
        self.map
            .insert(
                TypeId::of::<T>(),
                Dependency { type_name: std::any::type_name::<T>(), inner: Arc::new(item) },
            )
            .map(|dep| dep.inner.downcast().expect("Values are stored by `TypeId`"))
    }

    /// Inserts all dependencies from another container into itself.
    pub fn insert_container(&mut self, container: Self) {
        self.map.extend(container.map);
    }

    /// Removes a value from the container.
    ///
    /// If the container do not has this type present, `None` is returned.
    /// Otherwise, the value is removed and returned.
    pub fn remove<T: Send + Sync + 'static>(&mut self) -> Option<Arc<T>> {
        self.map
            .remove(&TypeId::of::<T>())
            .map(|dep| dep.inner.downcast().expect("Values are stored by `TypeId`"))
    }

    /// Retrieves the value of type `V` from this container.
    ///
    /// ## Panics
    ///
    /// If the container has no value of type `V`.
    pub fn get<V>(&self) -> Arc<V>
    where
        V: Send + Sync + 'static,
    {
        self.map
            .get(&TypeId::of::<V>())
            .unwrap_or_else(|| {
                panic!(
                    "`{}` was requested, but not provided. Available types:\n{}",
                    std::any::type_name::<V>(),
                    self.available_types()
                )
            })
            .clone()
            .inner
            .downcast::<V>()
            .expect("Checked by `unwrap_or_else`")
    }

    /// Tries to get a value from the container; does not panic.
    pub fn try_get<V: Send + Sync + 'static>(&self) -> Option<Arc<V>> {
        let key = TypeId::of::<V>();
        self.map
            .get(&key)
            .cloned()
            .map(|v| v.inner.downcast().expect("Values are stored by `TypeId`"))
    }

    fn available_types(&self) -> String {
        let mut list = String::new();

        for dep in self.map.values() {
            writeln!(list, "    `{}`", dep.type_name).unwrap();
        }

        list
    }
}

/// Converts functions into [`CompiledFn`].
///
/// For a function to be convertible into [`CompiledFn`], it must be of 0-12
/// arguments and return [`Future`].
pub trait Injectable<Output, FnArgs>
where
    Output: 'static,
{
    fn inject<'a>(&'a self, container: &'a DependencyMap) -> CompiledFn<'a, Output>;

    /// Returns the set of types that this function depends on. Used only for
    /// run-time "type checking".
    fn input_types() -> BTreeSet<Type>;

    /// Returns the map of obligations of this function.
    ///
    /// The map contains all types from [`Self::input_types`] as keys, each one
    /// corresponding to the current [`Location::caller`].
    ///
    /// [`Location::caller`]: std::panic::Location::caller
    #[track_caller]
    fn obligations() -> BTreeMap<Type, &'static Location<'static>> {
        let location = Location::caller();
        Self::input_types().into_iter().map(|ty| (ty, location)).collect()
    }
}

/// A function with all dependencies satisfied.
pub type CompiledFn<'a, Output> = Arc<dyn Fn() -> BoxFuture<'a, Output> + Send + Sync + 'a>;

/// Turns a synchronous function into a type that implements [`Injectable`].
pub struct Asyncify<F>(pub F);

macro_rules! impl_into_di {
    ($($generic:ident),*) => {
        impl<Func, Output, Fut, $($generic),*> Injectable<Output, ($($generic,)*)> for Func
        where
            Func: Fn($($generic),*) -> Fut + Send + Sync + 'static,
            Fut: Future<Output = Output> + Send + 'static,
            Output: 'static,
            $($generic: Clone + Send + Sync + 'static),*
        {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn inject<'a>(&'a self, container: &'a DependencyMap) -> CompiledFn<'a, Output> {
                Arc::new(move || {
                    $(let $generic = std::borrow::Borrow::<$generic>::borrow(&container.get()).clone();)*
                    let fut = self( $( $generic ),* );
                    Box::pin(fut)
                })
            }

            fn input_types() -> BTreeSet<Type> {
                BTreeSet::from_iter(vec![
                    $(Type::of::<$generic>()),*
                ])
            }
        }

        impl<Func, Output, $($generic),*> Injectable<Output, ($($generic,)*)> for Asyncify<Func>
        where
            Func: Fn($($generic),*) -> Output + Send + Sync + 'static,
            Output: Send + 'static,
            $($generic: Clone + Send + Sync + 'static),*
        {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn inject<'a>(&'a self, container: &'a DependencyMap) -> CompiledFn<'a, Output> {
                let Asyncify(this) = self;
                Arc::new(move || {
                    $(let $generic = std::borrow::Borrow::<$generic>::borrow(&container.get()).clone();)*
                    let out = this( $( $generic ),* );
                    Box::pin(ready(out))
                })
            }

            fn input_types() -> BTreeSet<Type> {
                BTreeSet::from_iter(vec![
                    $(Type::of::<$generic>()),*
                ])
            }
        }
    };
}

impl_into_di!();
impl_into_di!(T1);
impl_into_di!(T1, T2);
impl_into_di!(T1, T2, T3);
impl_into_di!(T1, T2, T3, T4);
impl_into_di!(T1, T2, T3, T4, T5);
impl_into_di!(T1, T2, T3, T4, T5, T6);
impl_into_di!(T1, T2, T3, T4, T5, T6, T7);
impl_into_di!(T1, T2, T3, T4, T5, T6, T7, T8);
impl_into_di!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_into_di!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_into_di!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_into_di!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

/// Constructs [`DependencyMap`] with a list of dependencies.
///
/// # Examples
///
/// ```
/// use dptree::di::DependencyMap;
///
/// let map = dptree::deps![123, "abc", true];
///
/// let i: i32 = *map.get();
/// let str: &str = *map.get();
/// let b: bool = *map.get();
///
/// assert!(i == 123);
/// assert!(str == "abc");
/// assert!(b == true);
/// ```
#[macro_export]
macro_rules! deps {
    ($($dep:expr),*) => {
        {
            // In the case if this macro receives zero arguments.
            #[allow(unused_mut)]
            let mut map = $crate::di::DependencyMap::new();
            $(map.insert($dep);)*
            map
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get() {
        let mut map = DependencyMap::new();
        map.insert(42i32);
        map.insert("hello world");
        map.insert_container(deps![true]);

        assert_eq!(map.get(), Arc::new(42i32));
        assert_eq!(map.get(), Arc::new("hello world"));
        assert_eq!(map.get(), Arc::new(true));
    }

    #[test]
    fn try_get() {
        let mut map = DependencyMap::new();
        assert_eq!(map.try_get::<i32>(), None);
        map.insert(42i32);
        assert_eq!(map.try_get(), Some(Arc::new(42i32)));
        assert_eq!(map.try_get::<f32>(), None);
    }
}
