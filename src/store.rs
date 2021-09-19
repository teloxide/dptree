//! The module contains traits and structs that are used for storing some values
//! in abstract stores.

use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::{Debug, Formatter},
    ops::Deref,
    sync::Arc,
};

pub trait Storage<Value>: Store<Value> + Insert<Value> + Remove<Value> {}
impl<S, V> Storage<V> for S where S: Store<V> + Insert<V> + Remove<V> {}

/// The trait is used to specify data type as storing some value `Value`. Means
/// that `Value` can be obtained by-value.
pub trait Store<Value> {
    fn get(&self) -> Arc<Value>;
}

pub trait Insert<Value>: Sized {
    fn insert_arc(self, value: Arc<Value>) -> Self;
    fn insert(self, value: Value) -> Self {
        self.insert_arc(Arc::new(value))
    }
}

pub trait Remove<Value> {
    fn remove(self) -> (Arc<Value>, Self);
}

/// Panickable realisation for the `Store` trait.
pub struct TypeMapPanickableStore {
    map: HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
}

impl TypeMapPanickableStore {
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

impl Debug for TypeMapPanickableStore {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("TypeMapPanickableStore").finish()
    }
}

impl<V: Send + Sync + 'static> Store<V> for TypeMapPanickableStore {
    fn get(&self) -> Arc<V> {
        self.map
            .get(&TypeId::of::<V>())
            .unwrap_or_else(|| {
                panic!("{} was requested, but not provided.", std::any::type_name::<V>())
            })
            .clone()
            .downcast::<V>()
            .expect("we already checks that line before")
            .clone()
    }
}

impl<Value: Send + Sync + 'static> Insert<Value> for TypeMapPanickableStore {
    fn insert_arc(mut self, value: Arc<Value>) -> Self {
        self.map.insert(TypeId::of::<Value>(), value);
        self
    }
}

impl<Value: Send + Sync + 'static> Remove<Value> for TypeMapPanickableStore {
    fn remove(mut self) -> (Arc<Value>, Self) {
        let value = self
            .map
            .remove(&TypeId::of::<Value>())
            .unwrap_or_else(|| {
                panic!(
                    "Trying to remove {} which is not inserted before.",
                    std::any::type_name::<Value>()
                );
            })
            .downcast()
            .expect("Values are stored by TypeId");

        (value, self)
    }
}

impl<V, S, ST> Store<V> for S
where
    S: Deref<Target = ST>,
    ST: Store<V>,
{
    fn get(&self) -> Arc<V> {
        self.deref().get()
    }
}
