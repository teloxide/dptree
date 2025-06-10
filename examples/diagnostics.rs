use dptree::prelude::*;

#[derive(Clone)]
struct A;
#[derive(Clone)]
struct B;
#[derive(Clone)]
struct C;
#[derive(Clone)]
struct D;

#[tokio::main]
async fn main() {
    let h: Handler<D> = dptree::entry().map(|_: A| B).inspect(|_: C| ()).endpoint(|| async { D });

    // Missing `A` and `C` in the dependency map (see the panic message below).
    dptree::type_check(h.sig(), &dptree::deps![B, D], &[]);
}

// thread 'main' panicked at src/handler/core.rs:551:17:
// Your handler accepts the following types:
//     `diagnostics::A`
//     `diagnostics::C`
// But only the following values were given to it:
//     `diagnostics::B`
//     `diagnostics::D`
// The missing values are:
//     `diagnostics::A` from examples/diagnostics.rs:14:41
//     `diagnostics::C` from examples/diagnostics.rs:14:55
//
// Make sure all the required values are provided to the handler. For more information, visit <https://docs.rs/dptree/latest/dptree>.
//
