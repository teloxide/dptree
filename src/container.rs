use crate::Replace;
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::{Debug, Formatter},
    ops::Deref,
    sync::Arc,
};

/// The trait is used to specify data type as storing some value `Value`. Means
/// that `Value` can be obtained by-value.
pub trait DiContainer<Value> {
    fn get(&self) -> Arc<Value>;
}

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

/// Panickable realisation for the `Store` trait.
pub struct TypeMapDi {
    map: HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
}

impl TypeMapDi {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn data<T: Send + Sync + 'static>(mut self, value: T) -> Self {
        self.insert(value);
        self
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
