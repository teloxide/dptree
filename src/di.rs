//! Traits and implementations of [Dependency Injection pattern].
//!
//! If you do not know what is DI (dependency injection), please, read [this
//! discussion on StackOverflow], then come back. Only difference that in
//! `dptree` we inject objects into functions-handlers, not in objects.
//!
//! Most important trait here is `DiContainer`. It must be implemented for all
//! DI containers. It specify types that can be obtained from DI container.
//!
//! There are two implementations in `dptree` of this trait:
//!
//! 1. `Value`. It always contain only one value. Use it where you want to pass
//! only one value to the handlers.
//! 2. `DependencyMap`. It implements DI pattern fully, but be careful: it can
//! panic when you do not provide necessary types. See more in its
//! documentation.
//!
//! We strongly not recommend to use these implementations in production code,
//! because of its inefficient. You can use them for testing or prototyping, but
//! we recommend to switch on implementations of foreign `DI` libraries.
//!
//! [Dependency Injection pattern]: https://en.wikipedia.org/wiki/Dependency_injection
//! [this discussion on StackOverflow]: https://stackoverflow.com/questions/130794/what-is-dependency-injection
use futures::future::BoxFuture;

use crate::Replace;
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::{Debug, Formatter},
    future::Future,
    ops::Deref,
    sync::Arc,
};

/// The trait is used to specify container that can return value of specified
/// type.
///
/// There are two possible ways to handle situation when container cannot return
/// value of specified type:
///
/// 1. Container may not implement `DiContainer` for the type.
/// It often requires some type-level manipulations.
/// 2. Container can panic in the runtime. Be careful in this case,
/// and check whether you add you type to container.
///
/// Concrete solution is chosen by implementation.
pub trait DependencySupplier<Value> {
    /// Get value.
    ///
    /// We assume that all values are stored in `Arc<_>`.
    fn get(&self) -> Arc<Value>;
}

/// Container that store only one value.
///
/// Primarily used in tests, but can be also used in the handlers which require
/// only one input value.
///
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::{di::Value, prelude::*};
/// use std::ops::ControlFlow;
///
/// let handler = dptree::endpoint(|x: Arc<i32>| async move { *x });
///
/// assert_eq!(handler.dispatch(Value::new(10)).await, ControlFlow::Break(10));
///
/// # }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Value<T>(pub Arc<T>);

impl<T> Value<T> {
    pub fn new(value: T) -> Self {
        Value(Arc::new(value))
    }
}

impl<T> DependencySupplier<T> for Value<T> {
    fn get(&self) -> Arc<T> {
        self.0.clone()
    }
}

impl<From, To> Replace<From, To> for Value<From> {
    type Out = Value<To>;

    fn replace(self, to: Arc<To>) -> (Value<To>, Arc<From>) {
        (Value(to), self.0)
    }
}

/// DI container using DependencyMap pattern.
///
/// This DI container stores types by its `TypeId`. It cannot prove in
/// compile-time what types are contained inside, so if you do not provide
/// necessary types but they were requested, panic will cause.
///
/// Example of right usage:
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
/// // if we add a type that already stored, it will be replaced
/// let old_value = container.insert(10_i32).unwrap();
///
/// assert_eq!(old_value, Arc::new(5_i32));
/// assert_eq!(container.get(), Arc::new(10_i32));
/// ```
///
/// When type is not provided, panic will cause:
/// ```should_panic
/// # use std::sync::Arc;
/// use dptree::di::{DependencyMap, DependencySupplier};
/// let mut container = DependencyMap::new();
/// container.insert(10i32);
///
/// let string: Arc<String> = container.get();
/// ```
#[derive(Default)]
pub struct DependencyMap {
    map: HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
}

impl DependencyMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a value into container.
    ///
    /// If the container did not have this type present, `None` is returned.
    ///
    /// If the container did have this type present, the value is updated, and
    /// the old value is returned.
    ///
    /// For examples see `DependencyMap` docs.
    pub fn insert<T: Send + Sync + 'static>(&mut self, item: T) -> Option<Arc<T>> {
        self.map
            .insert(TypeId::of::<T>(), Arc::new(item))
            .map(|arc| arc.downcast().expect("Values are stored by TypeId"))
    }

    /// Remove value from container.
    ///
    /// If the container did not have this type present, `None` is returned.
    ///
    /// If the map did have this type present, the value is removed and
    /// returned.
    pub fn remove<T: Send + Sync + 'static>(&mut self) -> Option<Arc<T>> {
        self.map
            .remove(&TypeId::of::<T>())
            .map(|arc| arc.downcast().expect("Values are stored by TypeId"))
    }
}

impl Debug for DependencyMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("DependencyMapPanickableStore").finish()
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

impl<From, To> Replace<From, To> for DependencyMap
where
    From: Send + Sync + 'static,
    To: Send + Sync + 'static,
{
    type Out = DependencyMap;

    fn replace(mut self, to: Arc<To>) -> (DependencyMap, Arc<From>) {
        let from = self.remove::<From>().unwrap_or_else(|| {
            panic!("Requested type {} does not provided.", std::any::type_name::<From>())
        });
        self.map.insert(TypeId::of::<To>(), to);
        (self, from)
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

#[rustfmt::skip] // rustfmt too bad in formatting lists
/// The trait is used to convert functions into `DiFn`.
///
/// The function must follow some rules, to be usable with DI:
///
/// 1. All input values must be wrapped around `Arc`. It is requirement of the
/// `DiContainer` trait.
/// 2. Function must have 0-9 arguments.
/// 3. Function must return `Future`.
pub trait Injector<Input, Output, FnArgs> {
    fn inject<'a>(&'a self, container: &'a Input) -> CompiledFn<'a, Output>;
}

/// The function to which the reference to the container is passed.
pub type CompiledFn<'a, Output> = Arc<dyn Fn() -> BoxFuture<'a, Output> + Send + Sync + 'a>;

macro_rules! impl_into_di {
    ($($generic:ident),*) => {
        impl<Func, Input, Output, Fut, $($generic),*>  Injector<Input, Output, (Fut, $($generic),*)> for Func
        where
            Input: $(DependencySupplier<$generic> +)*,
            Input: Send + Sync,
            Func: Fn($(Arc<$generic>),*) -> Fut + Send + Sync + 'static,
            Fut: Future<Output = Output> + Send + 'static,
            $($generic: Send + Sync),*
        {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn inject<'a>(&'a self, container: &'a Input) -> CompiledFn<'a, Output> {
                Arc::new(move || {
                    $(let $generic = container.get();)*
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
