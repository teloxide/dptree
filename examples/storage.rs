use dptree::{container::TypeMapDi, prelude::*};
use std::{net::Ipv4Addr, sync::Arc};

type Store = Arc<TypeMapDi>;

#[tokio::main]
async fn main() {
    fn assert_num_string_handler(
        expected_num: u32,
        expected_string: &'static str,
    ) -> Endpoint<'static, Store, ()> {
        // The handler requires `u32` and `String` types from the input storage.
        dptree::endpoint(move |num: Arc<u32>, string: Arc<String>| async move {
            assert_eq!(*num, expected_num);
            assert_eq!(&*string, expected_string);
        })
    }

    // Init storage with string and num
    let store = init_store();

    let str_num_handler = assert_num_string_handler(10u32, "Hello");

    str_num_handler.dispatch(store.clone()).await;

    // This will cause a panic because we do not store `Ipv4Addr` in out store.
    let handle = tokio::spawn(async move {
        let ip_handler = dptree::endpoint(|ip: Arc<Ipv4Addr>| async move {
            assert_eq!(*ip, Ipv4Addr::new(0, 0, 0, 0));
        });
        ip_handler.dispatch(store.clone()).await;
    });
    let result = handle.await;
    assert!(result.is_err())
}

fn init_store() -> Store {
    let mut store = TypeMapDi::new();

    store.insert(10u32);
    store.insert("Hello".to_string());

    let store = Arc::new(store);

    store
}
