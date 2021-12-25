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

The above code is a simple web server dispatching tree. In pseudocode, it would look like this:

 - `dptree::entry()`: dispatch an update to the following branch handlers:
   - `.branch(smiles_handler())`: if the update satisfies the condition (`dptree::filter`), return a smile (`.endpoint`). Otherwise, pass the update forwards.
   - `.branch(sqrt_handler())`: if the update is a number (`dptree::filter_map`), return the square of it. Otherwise, pass the update forwards.
   - `.branch(not_found_handler())`: return `404 Not Found` immediately.

**Control flow:** as you can see, we have just described a dispatching scheme consisting of three branches. First, dptree enters the first handler `smiles_handler`, then, if it fails to process an update, it passes the update to `sqrt_handler` and so on. If nobody have succeeded in handling an update, the control flow enters `not_found_handler` that returns the error. In other words, the result of the whole `.dispatch` call would be the result of the first handler that succeeded to handle an incoming update.

**Dependency injection:** instead of passing straightforward values to `.dispatch`, we use the `dptree::deps!` macro. It accepts a sequence of values and constructs `DependencyMap` out of them. The handlers request values of certain types in their signatures (such as `|req: &'static str|`), and dptree automatically _injects_ the values from `dptree::deps!` into these functions. If it fails to obtain values of requested types, it will [panic at run-time](#di), so be careful and always test your code before pushing to production.

Using dptree, you can specify arbitrary complex dispatching schemes using the same recurring patterns you have seen above.

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

## Design choices

### Functional

We decided to use a [continuation-passing style (CPS)] internally and expose neat handler patterns to library users. This is contrary to what you might have seen in typical object-oriented models of the chain of responsibility pattern. In fact, we have first tried to make a typical OO design, but then resorted to FP because of its simplicity. With this design, each handler accepts a continuation representing the rest of the handlers in the chain; the handler can either call this continuation or not. Using this simple model, we can express pretty much any handler pattern like `filter` and `filter_map`, using only functions and nothing else. You do not need complex programming machinery such as abstract factories, builders, etc.

### DI

In Rust, it is possible to have a type-safe DI container instead of `DependencyMap` that panics at run-time. However, this would require complex type-level manipulations (like those in the [frunk] library). [@p0lunin] and I ([@Hirrolot]) decided not to trade comprehensible error messages for compile-time safety, since we had a plenty of experience that the uninitiated users simply cannot understand what is wrong with their code, owing to monstrous diagnostic messages from rustc.

[frunk]: https://github.com/lloydmeta/frunk
[@p0lunin]: https://github.com/p0lunin
[@Hirrolot]: https://github.com/Hirrolot
