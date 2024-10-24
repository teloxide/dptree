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
    dptree::type_check(h.sig(), &dptree::deps![B, D]);
}

// thread 'main' panicked at src/handler/core.rs:551:17:
// This handler accepts the following types:
//     `diagnostics::A`
//     `diagnostics::C`
// , but only the following types are provided:
//     `diagnostics::B`
//     `diagnostics::D`
// The missing types are:
//     `diagnostics::A` from examples/diagnostics.rs:14:41
//     `diagnostics::C` from examples/diagnostics.rs:14:55
