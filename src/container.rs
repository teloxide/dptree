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
//! 2. `TypeMapDi`. It implements DI pattern fully, but be careful: it can panic
//! when you do not provide necessary types. See more in its documentation.
//!
//! We strongly not recommend to use these implementations in production code,
//! because of its inefficient. You can use them for testing or prototyping, but
//! we recommend to switch on implementations of foreign `DI` libraries.
//!
//! [Dependency Injection pattern]: https://en.wikipedia.org/wiki/Dependency_injection
//! [this discussion on StackOverflow]: https://stackoverflow.com/questions/130794/what-is-dependency-injection
use crate::Replace;
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::{Debug, Formatter},
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
pub trait DiContainer<Value> {
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
/// use dptree::{container::Value, prelude::*};
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

impl<T> DiContainer<T> for Value<T> {
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

/// DI container using TypeMap pattern.
///
/// This DI container stores types by its `TypeId`. It cannot prove in compile-time what
/// types are contained inside, so if you do not provide necessary types but they were requested,
/// panic will cause.
///
/// Example of right usage:
/// ```
/// # use std::sync::Arc;
/// use dptree::container::{TypeMapDi, DiContainer};
///
/// let mut container = TypeMapDi::new();
/// container.insert(5_i32);
/// container.insert("abc");
///
/// assert_eq!(container.get(), Arc::new(5_i32));
/// assert_eq!(container.get(), Arc::new("abc"));
///
/// // if we add a type that already stored, it will be replaced
/// container.insert(5i32);
/// assert_ne!(container.get(), Arc::new(10i32));
/// assert_eq!(container.get(), Arc::new(5i32));
///
/// ```
///
/// When type is not provided, panic will cause:
/// ```should_panic
/// # use std::sync::Arc;
/// use dptree::container::{TypeMapDi, DiContainer};
/// let mut container = TypeMapDi::new();
/// container.insert(10i32);
///
/// let string: Arc<String> = container.get();
/// ```
pub struct TypeMapDi {
    map: HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
}

impl TypeMapDi {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn insert<T: Send + Sync + 'static>(&mut self, item: T) {
        self.map.insert(TypeId::of::<T>(), Arc::new(item));
    }

    pub fn remove<T: Send + Sync + 'static>(&mut self) -> Option<Arc<T>> {
        self.map
            .remove(&TypeId::of::<T>())
            .map(|arc| arc.downcast().expect("Values are stored by TypeId"))
    }
}

impl Debug for TypeMapDi {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("TypeMapPanickableStore").finish()
    }
}

impl<V: Send + Sync + 'static> DiContainer<V> for TypeMapDi {
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

impl<From, To> Replace<From, To> for TypeMapDi
where
    From: Send + Sync + 'static,
    To: Send + Sync + 'static,
{
    type Out = TypeMapDi;

    fn replace(mut self, to: Arc<To>) -> (TypeMapDi, Arc<From>) {
        let from = self.remove::<From>().unwrap_or_else(|| {
            panic!("Requested type {} does not provided.", std::any::type_name::<From>())
        });
        self.map.insert(TypeId::of::<To>(), to);
        (self, from)
    }
}

impl<V, S> DiContainer<V> for Arc<S>
where
    S: DiContainer<V>,
{
    fn get(&self) -> Arc<V> {
        self.deref().get()
    }
}
