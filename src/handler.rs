//! The module contains core part of the library: handlers.
//!
//! Handlers is a entity that can handle some `Data`. Handlers are represented as a tree, where
//! `Dispatcher` handler tries to feed each next `Handler` the input `Data`. Next handlers in that case
//! can be any type that implements `Handler` trait. It can be, for example, another `Dispatcher` that do
//! same as another `Dispatcher`'s, or `Endpoint` that cannot break the handles and can only handle the
//! incoming `Data`. Another available handlers is `Filter` handler that filter incoming `Data` by
//! condition and breaks handles in that branch of tree if `Data` does not satisfy the condition,
//! and `Parser` which tries to parse incoming `Data` to another `ParsedData` in that branch of a
//! tree. You can define your own handlers if you want.
//!
//! Suppose we have a tree:
//! ```text
//!          Dispatcher(1)
//!         /              \
//!     Dispatcher(2)     Endpoint(3)
//!    /           \
//! Filter(1)    Endpoint(2)
//!   |
//!  Endpoint(1)
//! ```
//! Let's try to imagine what happens when `Data` incomes.
//!
//! First, it passed into the root of the tree `Dispatcher(1)`. Because that is `Dispatcher`, it only pass the
//! input to others nodes in a list. So, it pass the `Data` to the next node in a list: `Dispatcher(2)`.
//! It pass data to the next node: `Filter(1)`. Filter, as described above, filter the data by a
//! condition, and if the data satisfy the condition, it passes the data forward to the next
//! handler. Next handler is the `Endpoint(1)`. Because that is `Endpoint`, if data satisfy the condition
//! in the `Filter(1)`, data passes to the `Endpoint(1)` and handling ends. If data does not satisfy
//! the condition, data returns to the `Dispatcher(2)`. `Dispatcher(2)` then pass data to the next handler:
//! `Endpoint(2)`. Because it's a `Endpoint`, the handling end at this point. `Endpoint(3)` unreachable,
//! as you can see, due to `Endpoint(2)` handles all the incoming data that not handles by other
//! handlers.

pub mod dispatcher;
pub mod endpoint;
pub mod filter;
pub mod parser;

pub use endpoint::Endpoint;

use futures::future::BoxFuture;
use futures::Future;

/// Future returned from `Handler` trait.
///
/// Note that future must have 'static lifetime.
pub type HandlerFuture<Output, Data> = BoxFuture<'static, Result<Output, Data>>;

/// The trait is used to define handler which can handle some `Data`.
///
/// The handler must return `Res` in case of successful handling. Successful handling is
/// considered a case when a user-defined handler receives the input `Data`. If the user-defined
/// handler can returns error, `Res` can be specified as `Result<OkValue, Error>`. `Err` must be
/// returned iff `Data` cannot be processed by this handler. In that case it will be tried to
/// handles by other handlers in a tree. For more information see top-level documentation.
pub trait Handler<Data> {
    type Output;

    fn handle(&self, data: Data) -> HandlerFuture<Self::Output, Data>;
}

pub type BoxHandler<Data, Output> = Box<dyn Handler<Data, Output = Output> + Send + Sync>;

impl<Func, Data, Res, Fut> Handler<Data> for Func
where
    Func: Fn(Data) -> Fut,
    Fut: Future<Output = Result<Res, Data>> + Send + 'static,
{
    type Output = Res;
    fn handle(&self, data: Data) -> HandlerFuture<Res, Data> {
        Box::pin(self(data))
    }
}
