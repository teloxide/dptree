//! The module contains core part of the library: handlers.
//!
//! Handlers is a entity that can handle some `Data`. Handlers are represent as a tree, where
//! `Node` handler tries to feed each next `Handler` the input `Data`. Next handlers in that case
//! can be any type that implements `Handler` trait. It can be, for example, another `Node` that do
//! same as another `Node`'s, or `Leaf` that cannot breaks the handles and can only handle the
//! incoming `Data`. Another available handlers is `Filter` handler that filter incoming `Data` by
//! condition and breaks handles in that branch of tree if `Data` does not satisfy the condition,
//! and `Parser` which tries to parse incoming `Data` to another `ParsedData` in that branch of a
//! tree. You can define your own handlers if you want.
//!
//! Suppose we have a tree:
//! ```
//!          Node(1)
//!         /       \
//!     Node(2)     Leaf(3)
//!    /       \
//! Filter(1)   Leaf(2)
//!   |
//!  Leaf(1)
//! ```
//! Let's try to imagine what happens when `Data` incomes.
//!
//! First, it passed into the root of the tree `Node(1)`. Because that is `Node`, it only pass the
//! input to others nodes in a list. So, it pass the `Data` to the next node in a list: `Node(2)`.
//! It pass data to the next node: `Filter(1)`. Filter, as described above, filter the data by a
//! condition, and if the data satisfy the condition, it passes the data forward to the next
//! handler. Next handler is the `Leaf(1)`. Because that is `Leaf`, if data satisfy the condition
//! in the `Filter(1)`, data passes to the `Leaf(1)` and handling ends. If data does not satisfy
//! the condition, data returns to the `Node(2)`. `Node(2)` then pass data to the next handler:
//! `Leaf(2)`. Because it's a `Leaf`, the handling end at this point. `Leaf(3)` unreachable,
//! as you can see, due to `Leaf(2)` handles all the incoming data that not handles by other
//! handlers.

mod filter;
mod node;
mod leaf;
mod parser;

pub use filter::Filter;
pub use node::Node;
pub use leaf::Leaf;
pub use parser::Parser;

use futures::future::BoxFuture;
use futures::Future;

// Note that future must have 'static lifetime.
pub type HandlerFuture<Res, Data> = BoxFuture<'static, Result<Res, Data>>;

/// The trait is used to define handler which can handle some `Data`.
///
/// The handler must return `Res` in case of successful handling. Successful handling is
/// considered a case when a user-defined handler receives the input `Data`. If the user-defined
/// handler can returns error, `Res` can be specified as `Result<OkValue, Error>`. `Err` must be
/// returned iff `Data` cannot be processed by this handler. In that case it will be tried to
/// handles by other handlers in a tree. For more information see top-level documentation.
pub trait Handler<Data, Res> {
    fn handle(&self, data: Data) -> HandlerFuture<Res, Data>;
}

pub type BoxHandler<Data, Res> = Box<dyn Handler<Data, Res> + Send + Sync>;

impl<Func, Data, Res, Fut> Handler<Data, Res> for Func
where
    Func: Fn(Data) -> Fut,
    Fut: Future<Output = Result<Res, Data>> + Send + 'static,
{
    fn handle(&self, data: Data) -> HandlerFuture<Res, Data> {
        Box::pin(self(data))
    }
}
