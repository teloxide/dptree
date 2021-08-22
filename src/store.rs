//! The module contains traits and structs that are used for storing some values in
//! abstract stores.

use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

/// The trait is used to specify data type as storing some value `Value`. Means that
/// `Value` can be obtained by-value.
pub trait Store<Value> {
    fn get(&self) -> Value;
}

/// Panickable realisation for the `Store` trait.
pub struct TypeMapPanickableStore {
    map: HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
}

impl TypeMapPanickableStore {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert<T: Send + Sync + 'static>(&mut self, item: T) {
        self.map.insert(TypeId::of::<T>(), Arc::new(item));
    }
}

impl<V: Send + Sync + 'static> Store<Arc<V>> for TypeMapPanickableStore {
    fn get(&self) -> Arc<V> {
        self.map
            .get(&TypeId::of::<V>())
            .unwrap_or_else(|| {
                panic!(
                    "{} was requested, but not provided.",
                    std::any::type_name::<V>()
                )
            })
            .clone()
            .downcast::<V>()
            .expect("we already checks that line before")
            .clone()
    }
}

impl<V, S, ST> Store<V> for S
where
    S: Deref<Target = ST>,
    ST: Store<V>,
{
    fn get(&self) -> V {
        self.deref().get()
    }
}
