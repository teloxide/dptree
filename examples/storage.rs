use dptree::handler::endpoint::by_store::EndpointByStoreEnter;
use dptree::handler::Endpoint;
use dptree::store::TypeMapPanickableStore;
use dptree::Handler;
use std::net::Ipv4Addr;
use std::sync::Arc;

// `TypeMapPanickableStore` implements `TypeMap` store with panics on resolves types.
type Store = Arc<TypeMapPanickableStore>;

#[tokio::main]
async fn main() {
    fn assert_num_string_handler(
        expected_num: u32,
        expected_string: &'static str,
    ) -> impl Handler<Store, Output = ()> {
        // The handler requires `u32` and `String` types from the input storage.
        Endpoint::by_store(move |num: Arc<u32>, string: Arc<String>| async move {
            assert_eq!(*num, expected_num);
            assert_eq!(&*string, expected_string);
        })
    }

    // Init storage with string and num
    let store = init_store();

    let str_num_handler = assert_num_string_handler(10u32, "Hello");

    str_num_handler.handle(store.clone()).await.unwrap();

    // This will cause a panic because we do not store `Ipv4Addr` in out store.
    let handle = tokio::spawn(async move {
        let ip_handler = Endpoint::by_store(|ip: Arc<Ipv4Addr>| async move {
            assert_eq!(*ip, Ipv4Addr::new(0, 0, 0, 0));
        });
        ip_handler.handle(store.clone()).await.unwrap();
    });
    let result = handle.await;
    assert!(result.is_err())
}

fn init_store() -> Store {
    let mut store = TypeMapPanickableStore::new();

    store.insert(10u32);
    store.insert("Hello".to_string());

    let store = Arc::new(store);

    store
}
