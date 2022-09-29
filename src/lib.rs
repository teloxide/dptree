//! An implementation of the [chain (tree) of responsibility] pattern.
//!
//! [[`examples/web_server.rs`](https://github.com/teloxide/dptree/blob/master/examples/web_server.rs)]
//! ```
//! use dptree::prelude::*;
//!
//! type WebHandler = Endpoint<'static, DependencyMap, String>;
//!
//! #[rustfmt::skip]
//! #[tokio::main]
//! async fn main() {
//!     let web_server = dptree::entry()
//!         .branch(smiles_handler())
//!         .branch(sqrt_handler())
//!         .branch(not_found_handler());
//!     
//!     assert_eq!(
//!         web_server.dispatch(dptree::deps!["/smile"]).await,
//!         ControlFlow::Break("🙃".to_owned())
//!     );
//!     assert_eq!(
//!         web_server.dispatch(dptree::deps!["/sqrt 16"]).await,
//!         ControlFlow::Break("4".to_owned())
//!     );
//!     assert_eq!(
//!         web_server.dispatch(dptree::deps!["/lol"]).await,
//!         ControlFlow::Break("404 Not Found".to_owned())
//!     );
//! }
//!
//! fn smiles_handler() -> WebHandler {
//!     dptree::filter(|req: &'static str| req.starts_with("/smile"))
//!         .endpoint(|| async { "🙃".to_owned() })
//! }
//!
//! fn sqrt_handler() -> WebHandler {
//!     dptree::filter_map(|req: &'static str| {
//!         if req.starts_with("/sqrt") {
//!             let (_, n) = req.split_once(' ')?;
//!             n.parse::<f64>().ok()
//!         } else {
//!             None
//!         }
//!     })
//!     .endpoint(|n: f64| async move { format!("{}", n.sqrt()) })
//! }
//!
//! fn not_found_handler() -> WebHandler {
//!     dptree::endpoint(|| async { "404 Not Found".to_owned() })
//! }
//! ```
//!
//! For a high-level overview, please see [`README.md`](https://github.com/p0lunin/dptree).
//!
//! [chain (tree) of responsibility]: https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern

mod handler;

pub mod di;
pub mod prelude;

pub use handler::*;

/// Filters an enumeration, passing its payload forwards.
///
/// This macro expands to a [`crate::Handler`] that acts on your enumeration
/// type: if the enumeration is of a certain variant, the execution continues;
/// otherwise, `dptree` will try the next branch. This is very useful for
/// dialogue FSM transitions and incoming command filtering; for a real-world
/// example, please see teloxide's [`examples/purchase.rs`].
///
/// Variants can take the following forms:
///
///  - `Enum::MyVariant` for empty variants;
///  - `Enum::MyVariant(param1, ..., paramN)` for tuple-like variants;
///  - `Enum::MyVariant { param1, ..., paramN }` for `struct`-like variants.
///
/// In the first case, this macro results in a simple [`crate::filter`]; in the
/// second and third cases, this macro results in [`crate::filter_map`] that
/// passes the payload of `MyVariant` to the next handler if the match occurs.
/// (This next handler can be an endpoint or a more complex one.) The payload
/// format depend on the form of `MyVariant`:
///
///  - For `Enum::MyVariant(param)` and `Enum::MyVariant { param }`, the payload
///    is `param`.
///  - For `Enum::MyVariant(param,)` and `Enum::MyVariant { param, }`, the
///    payload is `(param,)`.
///  - For `Enum::MyVariant(param1, ..., paramN)` and `Enum::MyVariant { param1,
///    ..., paramN }`, the payload is `(param1, ..., paramN)` (where `N`>1).
///
/// ## Dependency requirements
///
///  - Your enumeration `Enum`.
///
/// ## Examples
///
/// ```
/// use dptree::prelude::*;
///
/// # #[tokio::main]
/// # async fn main() {
/// #[derive(Clone)]
/// enum Command {
///     Meow,
///     Add(i32, i32),
/// }
///
/// let h: crate::Handler<_, _> = dptree::entry()
///     .branch(dptree::case![Command::Meow].endpoint(|| async move { format!("Meow!") }))
///     .branch(
///         dptree::case![Command::Add(x, y)]
///             .endpoint(|(x, y): (i32, i32)| async move { format!("{}", x + y) }),
///     );
///
/// assert_eq!(
///     h.dispatch(dptree::deps![Command::Meow]).await,
///     ControlFlow::Break("Meow!".to_owned())
/// );
/// assert_eq!(
///     h.dispatch(dptree::deps![Command::Add(1, 2)]).await,
///     ControlFlow::Break("3".to_owned())
/// );
/// # }
/// ```
///
/// [`examples/purchase.rs`]: https://github.com/teloxide/teloxide/blob/master/examples/purchase.rs
#[macro_export]
macro_rules! case {
    ($($variant:ident)::+) => {
        $crate::filter(|x| matches!(x, $($variant)::+))
    };
    ($($variant:ident)::+ ($param:ident)) => {
        $crate::filter_map(|x| match x {
            $($variant)::+($param) => Some($param),
            _ => None,
        })
    };
    ($($variant:ident)::+ ($($param:ident),+ $(,)?)) => {
        $crate::filter_map(|x| match x {
            $($variant)::+($($param),+) => Some(($($param),+ ,)),
            _ => None,
        })
    };
    ($($variant:ident)::+ {$param:ident}) => {
        $crate::filter_map(|x| match x {
            $($variant)::+{$param} => Some($param),
            _ => None,
        })
    };
    ($($variant:ident)::+ {$($param:ident),+ $(,)?}) => {
        $crate::filter_map(|x| match x {
            $($variant)::+ { $($param),+ } => Some(($($param),+ ,)),
            _ => None,
        })
    };
}

#[cfg(test)]
mod tests {
    use std::ops::ControlFlow;

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    enum State {
        A,
        B(i32),
        C(i32, &'static str),
        D { foo: i32 },
        E { foo: i32, bar: &'static str },
        Other,
    }

    #[tokio::test]
    async fn handler_empty_variant() {
        let input = State::A;
        let h: crate::Handler<_, _> = case![State::A].endpoint(|| async move { 123 });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_single_fn_variant() {
        let input = State::B(42);
        let h: crate::Handler<_, _> = case![State::B(x)].endpoint(|x: i32| async move {
            assert_eq!(x, 42);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_single_fn_variant_trailing_comma() {
        let input = State::B(42);
        let h: crate::Handler<_, _> = case![State::B(x,)].endpoint(|(x,): (i32,)| async move {
            assert_eq!(x, 42);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_fn_variant() {
        let input = State::C(42, "abc");
        let h: crate::Handler<_, _> =
            case![State::C(x, y)].endpoint(|(x, str): (i32, &'static str)| async move {
                assert_eq!(x, 42);
                assert_eq!(str, "abc");
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_single_struct_variant() {
        let input = State::D { foo: 42 };
        let h: crate::Handler<_, _> = case![State::D { foo }].endpoint(|x: i32| async move {
            assert_eq!(x, 42);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_single_struct_variant_trailing_comma() {
        let input = State::D { foo: 42 };
        #[rustfmt::skip] // rustfmt removes the trailing comma from `State::D { foo, }`, but it plays a vital role in this test.
        let h: crate::Handler<_, _> = case![State::D { foo, }].endpoint(|(x,): (i32,)| async move {
            assert_eq!(x, 42);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_struct_variant() {
        let input = State::E { foo: 42, bar: "abc" };
        let h: crate::Handler<_, _> =
            case![State::E { foo, bar }].endpoint(|(x, str): (i32, &'static str)| async move {
                assert_eq!(x, 42);
                assert_eq!(str, "abc");
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }
}
