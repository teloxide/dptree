//! An implementation of the [chain (tree) of responsibility] pattern.
//!
//! [[`examples/web_server.rs`](https://github.com/teloxide/dptree/blob/master/examples/web_server.rs)]
//! ```
//! use dptree::prelude::*;
//!
//! type WebHandler = Endpoint<'static, String>;
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
//! For a high-level overview, please see [`README.md`](https://github.com/teloxide/dptree).
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
/// let h: Handler<_> = dptree::entry()
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
    ($($entry:tt)*) => {
        $crate::filter_map(|x| match x {
            $($entry)*
            => Some($crate::_extract_values!($($entry)*)),
            _ => None,
        })
    };
}

/// A helper macro for [`case!`] to extract values recursively from structs and enums and returns a
/// tuple with all of the matched values.
///
/// [`case!`]: crate::case
//
// This looks confusing, so here is how this works:
//
// It's just a recursive TT muncher that takes all of the "endpoint" values in rusts pattern
// matching syntax.
//
// The first three rules are just for `..` support and trailing comma support. ".." and ", .."
// differ because ".." can be at the front, where a lack of a value is an error, but a
// comma indicates that the value before is filled in, and we can return nothing.
//
// The fourth rule just removes the comma, all other cases don't care about it, so, to converge the
// two possibilities, we just remove it.
//
// The sixth, seventh and eighth rules are escapes from the recursion.
// All branches end in either a variable or an enum variant with no values (for filtering reasons).
//
// Ninth and tenth rules just extract the inner comma separated values from structs and enums.
//
// Eleventh and twelfth rules take the comma separated values from ninth and tenth rules and TT
// munch on every single one of the values.
//
// Thirteenth and forteenth rules do the same, but if the first field doesn't have a key, like in a
// tuple struct.
//
// Fifteenth and sixteenth rules are for typed values, with and without a key.
//
// The last rule just extracts `key: value` to `value`, the `key` is not needed in the return tuple.
#[macro_export]
macro_rules! _extract_values {
    // 1. Two dots mean expansion. This has to go at the end, otherwise it will mess up the commas.
    (..) => {
        ()
    };
    // 2. Same thing, but no need to return anything because of the comma
    (, ..) => {
        // This is a workaround, rust expects an expression here, but just () can't be returned, so
        // we trick the compiler to think that we are returning an expression, while not doing it
        #[cfg(any())] { () }
    };
    // 3. Trailing comma support
    (,) => {
        #[cfg(any())] { () }
    };
    // 4. Removing the comma, converging it to all the other cases
    (, $($rest:tt)*) => {
        $crate::_extract_values!($($rest)*)
    };
    // 5. Just a nice panic
    (None) => {
        { const _: () = panic!("Cannot infer the type of None, please add None::<T>"); }
    };
    // 6. Singular value
    ($value:ident) => {
        $value
    };
    // 7. Singular value with a type. Mostly for None::<T> support, but generalized.
    ($value:ident::<$($ty:ty),*>) => {
        $value::<$($ty)*>
    };
    // 8. Enum without any values
    ($inner:ident::$variant:ident) => {
        $inner::$variant
    };
    // 9. Multiple fields in struct/enum in braces
    ($inner:ident$(::$variant:ident)? { $($rest:tt)* }) => {
        $crate::_extract_values!($($rest)*)
    };
    // 10. Multiple fields in struct/enum in parentheses
    ($value:ident$(::$variant:ident)? ($($rest:tt)*)) => {
        $crate::_extract_values!($($rest)*)
    };
    // 11. This takes a comma separated token list and wraps a recursion around every item.
    //
    // This basically filters for all of:
    // 1. Just an endpoint value: `foo` in `foo, ..`
    // 2. A key to a value: `SomeVal` in `foo: SomeVal, ..`
    // 3. A key to an enum variant: `State::A` in `foo: State::A, ..`
    // 4. A key to a struct enum variant: `State::A { bar }` in `foo: State::A { bar }, ..`
    // 5. A key to an struct: `Pos { x, y }` in `foo: Pos { x, y }, ..`
    //
    // And passes the rest to the TT muncher.
    //
    // A comma is added in the front of the second `_extract_values` for the 2nd and 3rd rules.
    //
    // It's impossible to just put everything before a comma under $($var:tt)* because of the
    // ambiguity due to $outer.
    ($param:ident $(: $value:ident$(::$variant:ident)? $({$($inner:tt)*})? )?, $($outer:tt)*) => {
        ($crate::_extract_values!($param $(: $value$(::$variant)? $({$($inner)*})? )?), $crate::_extract_values!(, $($outer)*))
    };
    // 12. Same thing, it just works with parentheses instead of braces.
    //
    // We don't need an extra optional group around the braced part to filter the enum variants,
    // the previous rule does that already.
    ($param:ident $(: $value:ident$(::$variant:ident)? ($($inner:tt)*) )?, $($outer:tt)*) => {
        ($crate::_extract_values!($param $(: $value$(::$variant)? ($($inner)*) )?), $crate::_extract_values!(, $($outer)*))
    };
    // 13. Same as 11, but times where the first value doesn't have a key, like in tuple structs
    ($value:ident$(::$variant:ident)? $({$($inner:tt)*})?, $($outer:tt)*) => {
        ($crate::_extract_values!($value$(::$variant)? $({$($inner)*})?), $crate::_extract_values!(, $($outer)*))
    };
    // 14. Same as 13, but with parantheses
    ($value:ident$(::$variant:ident)? ($($inner:tt)*), $($outer:tt)*) => {
        ($crate::_extract_values!($value$(::$variant)? ($($inner)*)), $crate::_extract_values!(, $($outer)*))
    };
    // 15. If the first key is a typed value
    ($key:ident : $value:ident::<$($ty:ty),*>, $($outer:tt)*) => {
        ($crate::_extract_values!($key: $value::<$($ty),*>), $crate::_extract_values!(, $($outer)*))
    };
    // 16. If the first value is a typed value
    ($value:ident::<$($ty:ty),*>, $($outer:tt)*) => {
        ($crate::_extract_values!($value::<$($ty),*>), $crate::_extract_values!(, $($outer)*))
    };
    // 17. Extracts `key: value` to `value`
    ($param:ident : $($value:tt)*) => {
        $crate::_extract_values!($($value)*)
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

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    enum ComplexState {
        A(State),
        B(i32, State),
        C { foo: TestStruct, bar: i32 },
        D(Option<i32>),
        E { foo: TestTupleStruct },
        F(Option<i32>, i32),
        Other,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    struct TestStruct {
        a: u32,
        b: State,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    struct TestTupleStruct(i32, State);

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    struct TestTupleStruct2(State, i32);

    #[tokio::test]
    async fn handler_empty_variant() {
        let input = State::A;
        let h: crate::Handler<_> = case![State::A].endpoint(|| async move { 123 });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_single_fn_variant() {
        let input = State::B(42);
        let h: crate::Handler<_> = case![State::B(x)].endpoint(|x: i32| async move {
            assert_eq!(x, 42);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_single_fn_variant_trailing_comma() {
        let input = State::B(42);
        let h: crate::Handler<_> = case![State::B(x,)].endpoint(|(x,): (i32,)| async move {
            assert_eq!(x, 42);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_fn_variant() {
        let input = State::C(42, "abc");
        let h: crate::Handler<_> =
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
        let h: crate::Handler<_> = case![State::D { foo }].endpoint(|x: i32| async move {
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
        let h: crate::Handler<_> = case![State::D { foo, }].endpoint(|(x,): (i32,)| async move {
            assert_eq!(x, 42);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_struct_variant() {
        let input = State::E { foo: 42, bar: "abc" };
        let h: crate::Handler<_> =
            case![State::E { foo, bar }].endpoint(|(x, str): (i32, &'static str)| async move {
                assert_eq!(x, 42);
                assert_eq!(str, "abc");
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_struct() {
        let input = TestStruct { a: 5, b: State::A };
        let h: crate::Handler<_> =
            case![TestStruct { a, b: State::A }].endpoint(|(a, state): (u32, State)| async move {
                assert_eq!(a, 5);
                assert_eq!(state, State::A);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![TestStruct { a: 5, b: State::Other }]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_expansion() {
        let input = State::E { foo: 42, bar: "abc" };
        let h: crate::Handler<_> = case![State::E { .. }].endpoint(|| async move { 123 });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![State::Other]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_dots_expand_struct() {
        let input = TestStruct { a: 5, b: State::A };
        let h: crate::Handler<_> =
            case![TestStruct { b: State::A, .. }].endpoint(|(state,): (State,)| async move {
                assert_eq!(state, State::A);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![TestStruct { a: 5, b: State::Other }]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_tuple_struct() {
        let input = TestTupleStruct(6, State::A);
        let h: crate::Handler<_> =
            case![TestTupleStruct(x, State::A)].endpoint(|(x, state): (i32, State)| async move {
                assert_eq!(x, 6);
                assert_eq!(state, State::A);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![TestTupleStruct(6, State::Other)]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_tuple_struct2() {
        // To test for the enum in the first field
        let input = TestTupleStruct2(State::A, 6);
        let h: crate::Handler<_> =
            case![TestTupleStruct2(State::A, x)].endpoint(|(state, x): (State, i32)| async move {
                assert_eq!(state, State::A);
                assert_eq!(x, 6);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![TestTupleStruct2(State::Other, 6)]).await,
            ControlFlow::Continue(_)
        ));
    }

    // ComplexState tests

    #[tokio::test]
    async fn handler_nested_fn_variant() {
        let input = ComplexState::A(State::A);
        let h: crate::Handler<_> =
            case![ComplexState::A(State::A)].endpoint(|state: State| async move {
                assert_eq!(state, State::A);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![ComplexState::Other]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_nested_multiple_fn_variant() {
        let input = ComplexState::B(1, State::B(2));
        let h: crate::Handler<_> =
            case![ComplexState::B(x, State::B(y))].endpoint(|(x, y): (i32, i32)| async move {
                assert_eq!(x, 1);
                assert_eq!(y, 2);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![ComplexState::Other]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_nested_struct() {
        let input = ComplexState::C { foo: TestStruct { a: 10, b: State::C(1, "abc") }, bar: 11 };
        let h: crate::Handler<_> =
            case![ComplexState::C { foo: TestStruct { a, b: State::C(c, d) }, bar }].endpoint(
                |((a, (c, d)), bar): ((u32, (i32, &'static str)), i32)| async move {
                    assert_eq!(a, 10);
                    assert_eq!(c, 1);
                    assert_eq!(d, "abc");
                    assert_eq!(bar, 11);
                    123
                },
            );

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![ComplexState::Other]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_nested_option_some() {
        let input = ComplexState::D(Some(2));
        let h: crate::Handler<_> = case![ComplexState::D(Some(x))].endpoint(|x: i32| async move {
            assert_eq!(x, 2);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![ComplexState::Other]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_option_none() {
        let input = None::<i32>;
        let h: crate::Handler<_> = case![None::<i32>].endpoint(|x: Option<i32>| async move {
            assert_eq!(x, None);
            123
        });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![Some(13)]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_nested_option_none() {
        let input = ComplexState::D(None);
        let h: crate::Handler<_> =
            case![ComplexState::D(None::<i32>)].endpoint(|x: Option<i32>| async move {
                assert_eq!(x, None);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![ComplexState::Other]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_tuple_struct_none() {
        // To test for a typed variable in the first field
        let input = ComplexState::F(None, 6);
        let h: crate::Handler<_> = case![ComplexState::F(None::<i32>, x)].endpoint(
            |(x, y): (Option<i32>, i32)| async move {
                assert_eq!(x, None);
                assert_eq!(y, 6);
                123
            },
        );

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(h.dispatch(crate::deps![Some(13)]).await, ControlFlow::Continue(_)));
    }

    #[tokio::test]
    async fn handler_nested_tuple_struct() {
        let input = ComplexState::E { foo: TestTupleStruct(100, State::B(9)) };
        let h: crate::Handler<_> = case![ComplexState::E { foo: TestTupleStruct(x, y) }].endpoint(
            |(x, state): (i32, State)| async move {
                assert_eq!(x, 100);
                assert_eq!(state, State::B(9));
                123
            },
        );

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![ComplexState::Other]).await,
            ControlFlow::Continue(_)
        ));
    }

    #[tokio::test]
    async fn handler_nested_tuple_struct_trailing_comma() {
        let input = ComplexState::E { foo: TestTupleStruct(100, State::A) };
        // Here rustfmt again removes a crutial comma
        #[rustfmt::skip]
        let h: crate::Handler<_> = case![ComplexState::E { foo: TestTupleStruct(x, State::A), }]
            .endpoint(|((x, state),): ((i32, State),)| async move {
                assert_eq!(x, 100);
                assert_eq!(state, State::A);
                123
            });

        assert_eq!(h.dispatch(crate::deps![input]).await, ControlFlow::Break(123));
        assert!(matches!(
            h.dispatch(crate::deps![ComplexState::Other]).await,
            ControlFlow::Continue(_)
        ));
    }
}
