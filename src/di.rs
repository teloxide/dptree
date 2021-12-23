//! An implementation of [dependency injection].
//!
//! If you do not know what is dependency injection (DI), please read [this
//! discussion on StackOverflow], then come back. The only difference is that in
//! `dptree`, we inject objects into function-handlers, not into objects.
//!
//! The most important trait here is [`DependencySupplier`]. It must be
//! implemented for all DI containers. It specifies types that can be obtained
//! from a DI container.
//!
//! There is one implementation in `dptree` of this trait, [`DependencyMap`]. It
//! implements the DI pattern completely, but be careful: it can panic when you
//! do not provide necessary types. See more in its documentation.
//!
//! [dependency injection]: https://en.wikipedia.org/wiki/Dependency_injection
//! [this discussion on StackOverflow]: https://stackoverflow.com/questions/130794/what-is-dependency-injection
use futures::future::BoxFuture;

use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::{Debug, Formatter},
    future::Future,
    ops::Deref,
    sync::Arc,
};

/// A DI container from which we can extract a value of a given type.
///
/// There are two possible ways to handle the situation when your container
/// cannot return a value of specified type:
///
/// 1. Do not implement [`DependencySupplier`] for the type. It often requires
/// some type-level manipulations.
/// 2. Runtime panic. Be careful in this case: check whether you add your type
/// to the container.
///
/// A concrete solution is left to a particular implementation.
pub trait DependencySupplier<Value> {
    /// Get the value.
    ///
    /// We assume that all values are stored in `Arc<_>`.
    fn get(&self) -> Arc<Value>;
}

/// A DI container with multiple dependencies.
///
/// This DI container stores types by their corresponding type identifiers. It
/// cannot prove at compile-time that a type of a requested value exists within
/// the container, so if you do not provide necessary types but they were
/// requested, it will panic.
///
/// # Examples
///
/// ```
/// # use std::sync::Arc;
/// use dptree::di::{DependencyMap, DependencySupplier};
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
/// # use std::sync::Arc;
/// use dptree::di::{DependencyMap, DependencySupplier};
/// let mut container = DependencyMap::new();
/// container.insert(10i32);
///
/// let string: Arc<String> = container.get();
/// ```
#[derive(Default, Clone)]
pub struct DependencyMap {
    map: HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
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
            .insert(TypeId::of::<T>(), Arc::new(item))
            .map(|arc| arc.downcast().expect("Values are stored by TypeId"))
    }

    /// Removes a value from the container.
    ///
    /// If the container do not has this type present, `None` is returned.
    /// Otherwise, the value is removed and returned.
    pub fn remove<T: Send + Sync + 'static>(&mut self) -> Option<Arc<T>> {
        self.map
            .remove(&TypeId::of::<T>())
            .map(|arc| arc.downcast().expect("Values are stored by TypeId"))
    }
}

impl Debug for DependencyMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("DependencyMap").finish()
    }
}

impl<V: Send + Sync + 'static> DependencySupplier<V> for DependencyMap {
    fn get(&self) -> Arc<V> {
        self.map
            .get(&TypeId::of::<V>())
            .unwrap_or_else(|| {
                panic!("{} was requested, but not provided.", std::any::type_name::<V>())
            })
            .clone()
            .downcast::<V>()
            .expect("we already checks that line before")
    }
}

impl<V, S> DependencySupplier<V> for Arc<S>
where
    S: DependencySupplier<V>,
{
    fn get(&self) -> Arc<V> {
        self.deref().get()
    }
}

/// Converts functions into [`CompiledFn`].
///
/// The function must follow some rules, to be usable with DI:
///
/// 1. All input values must be wrapped into `Arc`. It is a requirement of the
/// [`DependencySupplier`] trait.
/// 2. The function must be of 0-9 arguments.
/// 3. The function must return [`Future`].
pub trait Injector<Input, Output, FnArgs> {
    fn inject<'a>(&'a self, container: &'a Input) -> CompiledFn<'a, Output>;
}

/// A function with all dependencies satisfied.
pub type CompiledFn<'a, Output> = Arc<dyn Fn() -> BoxFuture<'a, Output> + Send + Sync + 'a>;

macro_rules! impl_into_di {
    ($($generic:ident),*) => {
        impl<Func, Input, Output, Fut, $($generic),*>  Injector<Input, Output, (Fut, $($generic),*)> for Func
        where
            Input: $(DependencySupplier<$generic> +)*,
            Input: Send + Sync,
            Func: Fn($($generic),*) -> Fut + Send + Sync + 'static,
            Fut: Future<Output = Output> + Send + 'static,
            $($generic: Clone + Send + Sync),*
        {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn inject<'a>(&'a self, container: &'a Input) -> CompiledFn<'a, Output> {
                Arc::new(move || {
                    $(let $generic = std::borrow::Borrow::<$generic>::borrow(&container.get()).clone();)*
                    let fut = self( $( $generic ),* );
                    Box::pin(fut)
                })
            }
        }
    };
}

impl_into_di!();
impl_into_di!(A);
impl_into_di!(A, B);
impl_into_di!(A, B, C);
impl_into_di!(A, B, C, D);
impl_into_di!(A, B, C, D, E);
impl_into_di!(A, B, C, D, E, F);
impl_into_di!(A, B, C, D, E, F, G);
impl_into_di!(A, B, C, D, E, F, G, H);
impl_into_di!(A, B, C, D, E, F, G, H, I);

/// Constructs [`DependencyMap`] with a list of dependencies.
///
/// # Examples
///
/// ```
/// use dptree::di::{DependencyMap, DependencySupplier};
///
/// let map = dptree::deps!(123, "abc", true);
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
            let mut map = DependencyMap::new();
            $(map.insert($dep);)*
            map
        }
    }
}
