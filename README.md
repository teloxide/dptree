# dptree

An implementation of the [chain (tree) of responsibility] pattern.

[[`examples/web_server.rs`](https://github.com/p0lunin/dptree/blob/master/examples/web_server.rs)]
```rust
use dptree::prelude::*;

type WebHandler = Endpoint<'static, DependencyMap, String>;

#[rustfmt::skip]
#[tokio::main]
async fn main() {
    let web_server = dptree::entry()
        .branch(smiles_handler())
        .branch(sqrt_handler())
        .branch(not_found_handler());

    assert_eq!(
        web_server.dispatch(dptree::deps!("/smile")).await,
        ControlFlow::Break("🙃".to_owned())
    );
    assert_eq!(
        web_server.dispatch(dptree::deps!("/sqrt 16")).await,
        ControlFlow::Break("4".to_owned())
    );
    assert_eq!(
        web_server.dispatch(dptree::deps!("/lol")).await,
        ControlFlow::Break("404 Not Found".to_owned())
    );
}

fn smiles_handler() -> WebHandler {
    dptree::filter(|req: &'static str| async move { req.starts_with("/smile") })
        .endpoint(|| async { "🙃".to_owned() })
}

fn sqrt_handler() -> WebHandler {
    dptree::filter_map(|req: &'static str| async move {
        if req.starts_with("/sqrt") {
            let (_, n) = req.split_once(" ")?;
            n.parse::<f64>().ok()
        } else {
            None
        }
    })
    .endpoint(|n: f64| async move { format!("{}", n.sqrt()) })
}

fn not_found_handler() -> WebHandler {
    dptree::endpoint(|| async { "404 Not Found".to_owned() })
}
```

[chain (tree) of responsibility]: https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern

## Features

 - ✔️ Declarative handlers: `dptree::{endpoint, filter, filter_map, ...}`.
 - ✔️ A lightweight functional design using a form of [continuation-passing style (CPS)] internally.
 - ✔️ [Dependency injection (DI)] out-of-the-box.
 - ✔️ Supports both handler _chaining_ and _branching_ operations.
 - ✔️ Battle-tested: dptree is used in [teloxide] as a framework for Telegram update dispatching.

[continuation-passing style (CPS)]: https://en.wikipedia.org/wiki/Continuation-passing_style
[Dependency injection (DI)]: https://en.wikipedia.org/wiki/Dependency_injection
[teloxide]: https://github.com/teloxide/teloxide
