//! Let's create a dispatcher with an explanation of each step.
//!
//! # Do I need a dispatcher?
//!
//! First, let's figure out what a dispatcher is and when it's need. A dispatcher is a structure that
//! allows you to distribute events to different handlers under certain conditions. This is a common
//! programming task. For example, a dispatcher requires to handle a request by REST api. The
//! conditions here are the endpoints of the requests. A similar way used in `actix-web`, `warp` and
//! other web frameworks.
//!
//! So, if you want a way to distribute requests, you need a dispatcher.
//!
//! # Let's create a dispatchers
//!
//! In this section, we create 2 kinds of dispatchers. The first will be a stateless dispatcher for
//! computing mathematical expressions with 2 operands, and the second will be a more traditional
//! CRUD-similar dispatcher for weather storing.
//!
//! ## A stateless dispatcher for mathematical expressions
//!
//! Let's start! The first thing we need is to split our program into `events` and `handlers`. The
//! `events` in our case will be mathematical expressions that we need to solve, and `handlers` are
//! functions that solve these mathematical expressions.
//!
//! Our `event` type is as following:
//! ```
//! struct Expression {
//!     operand1: i32,
//!     operand2: i32,
//!     operation: Operation,
//! }
//! enum Operation {
//!     Add,
//!     Multiply,
//! }
//! ```
//!
//! Now we can start to define our handlers. We want to define one handler per event. In our case
//! this means one handler per operation.
//!
//! There are many types of handlers which are concatenating with each other forming a chain.
//! It is called [`Chain of Responsibility Pattern`]. For now we create only constructors for
//! handlers, and then we will create every handler step-by-step.
//!
//! [`Chain of Responsibility Pattern`]: https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern
//! ```
//! use dptree::Handler;
//! # struct Expression;
//! // `Expression` is input event type here, i32 - return type
//! fn handler_add() -> impl Handler<Expression, Res = i32> {
//!     # dptree::endpoint(|_| async move { unimplemented!() })
//! }
//! // `Expression` is input event type here, i32 - return type
//! fn handler_multiply() -> impl Handler<Expression, Res = i32> {
//!     # dptree::endpoint(|_| async move { unimplemented!() })
//! }
//! ```
//!
//! ### Implementing handlers
//! Let's implement handler for our `add` operation. First we need is to find a way to filter from
//! all incoming events only contains `Operation::Add` variant. For that purpose we use `dptree::filter`:
//! ```
//! use dptree::{Handler, HandlerBuilder};
//! # struct Expression { operand1: i32, operand2: i32, operation: Operation }
//! # enum Operation { Add, Multiply }
//! fn handler_add() -> impl Handler<Expression, Res = i32> {
//!     dptree::filter(|exp: &Expression| matches!(exp.operation, Operation::Add))
//!         # .endpoint(|exp: Expression| async move {  unimplemented!() })
//! }
//! ```
//!
//! `dptree::filter` creates a builder for the `Filter` struct that allow us to filter input event by
//! some condition. In our case we filter only `add` events.
//!
//! Last handler we need is `Endpoint`. `Endpoint` is a handler that process an event. In our case
//! it adds 2 numbers:
//!
//! ```
//! use dptree::{Handler, HandlerBuilder};
//! # struct Expression { operand1: i32, operand2: i32, operation: Operation }
//! # enum Operation { Add, Multiply }
//! fn handler_add() -> impl Handler<Expression, Res = i32> {
//!     dptree::filter(|exp: &Expression| matches!(exp.operation, Operation::Add))
//!         .endpoint(|exp: Expression| async move { exp.operand1 + exp.operand2 })
//! }
//! ```
//!
//! Let's implement other handlers:
//! ```
//! use dptree::{Handler, HandlerBuilder};
//! # struct Expression { operand1: i32, operand2: i32, operation: Operation }
//! # enum Operation { Add, Multiply }
//! fn handler_multiply() -> impl Handler<Expression, Res = i32> {
//!     dptree::filter(|exp: &Expression| matches!(exp.operation, Operation::Multiply))
//!         .endpoint(|exp: Expression| async move { exp.operand1 * exp.operand2 })
//! }
//! ```
//!
//! Now, you may ask: how does this design differ from the simple `match` construction?
//! This is explained in the `Pros and Cons` section.
//!
//! ### Make dispatcher
//! When we create all our handlers, we need to combine them into one handler. For that purpose
//! we use `dptree::dispatch` fn that creates `Dispatcher` that allow us to dispatch input event
//! into many other handlers:
//! ```
//! # use dptree::Handler;
//! # struct Expression;
//! # fn handler_add() -> impl Handler<Expression, Res = i32> { dptree::endpoint(|_| async move { unimplemented!() }) }
//! # fn handler_multiply() -> impl Handler<Expression, Res = i32> { dptree::endpoint(|_| async move { unimplemented!() }) }
//! let dispatcher = dptree::dispatch()
//!     .to(handler_add())
//!     .to(handler_multiply())
//!     .build();
//! ```
//!
//! After creating `Dispatcher` we adds 2 handlers to and then build our dispatcher. That's all! Now
//! we can use dispatcher to compute different expressions:
//! ```
//! use dptree::{Handler};
//! # use dptree::HandlerBuilder;
//! # #[derive(Debug, PartialEq)] struct Expression { operand1: i32, operand2: i32, operation: Operation }
//! # #[derive(Debug, PartialEq)] enum Operation { Add, Multiply }
//! # fn handler_add() -> impl Handler<Expression, Res = i32> {
//! #     dptree::filter(|exp: &Expression| matches!(exp.operation, Operation::Add))
//! #         .endpoint(|exp: Expression| async move { exp.operand1 + exp.operand2 })
//! # }
//! # fn handler_multiply() -> impl Handler<Expression, Res = i32> {
//! #     dptree::filter(|exp: &Expression| matches!(exp.operation, Operation::Multiply))
//! #         .endpoint(|exp: Expression| async move { exp.operand1 * exp.operand2 })
//! # }
//! # #[tokio::main]
//! # async fn main() {
//! #     let dispatcher = dptree::dispatch().to(handler_add()).to(handler_multiply()).build();
//! let add = Expression { operand1: 1, operand2: 3, operation: Operation::Add };
//! assert_eq!(dispatcher.handle(add).await, Ok(4));
//! let mult = Expression { operand1: 99, operand2: 0, operation: Operation::Multiply };
//! assert_eq!(dispatcher.handle(mult).await, Ok(0));
//! # }
//! ```
//!

fn _empty() {}
