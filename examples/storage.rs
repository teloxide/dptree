extern crate dispatch_tree as dptree;

use dptree::store::TypeMapPanickableStore;
use dptree::Handler;
use dptree::handler::Leaf;
use dptree::handler::leaf::by_store::LeafStoreEnter;
use std::net::Ipv4Addr;
use std::sync::Arc;

type Store = Arc<TypeMapPanickableStore>;

#[tokio::main]
async fn main() {
    fn assert_num_string_handler(
        expected_num: u32,
        expected_string: &'static str
    ) -> impl Handler<Store, Res = ()> {
        Leaf::enter_store(move |num: u32, string: String| async move {
            assert_eq!(num, expected_num);
            assert_eq!(&string, expected_string);
        })
    }

    #[allow(unused)]
    fn assert_ip_handler() -> impl Handler<Store, Res = ()> {
        Leaf::enter_store(|ip: Ipv4Addr| async move { assert_eq!(ip, Ipv4Addr::new(0, 0, 0, 0)) })
    }

    let store = init_store();
    let str_num_handler = assert_num_string_handler(10u32, "Hello");

    str_num_handler.handle(store.clone()).await.unwrap_or_else(|_| unreachable!());

    // This will cause a panic because we do not store `Ipv4Addr` in out store.
    // let ip_handler = assert_ip_handler();
    // ip_handler.handle(store.clone()).await.unwrap_or_else(|_| unreachable!());
}

fn init_store() -> Store {
    let mut store = TypeMapPanickableStore::new();

    store.insert(10u32);
    store.insert("Hello".to_string());

    let store = Arc::new(store);

    store
}
