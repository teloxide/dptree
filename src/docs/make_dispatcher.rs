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
//! computing mathematical expressions, and the second will be a more traditional CRUD-similar
//! dispatcher for weather storing.
//!
//! ## A stateless dispatcher for mathematical expressions
//!
//! Let's start! The first thing we need is to split our program into `events` and `handlers`. The
//! `events` in our case will be mathematical expressions that we need to solve, and `handlers` are
//! functions that solve these mathematical expressions.
//!
//! Our `event` type is as following:
//! ```
//! enum Expression {
//!     Add(Add),
//!     Multiply(Multiply),
//! }
//! struct Add(i32, i32);
//! struct Multiply(i32, i32);
//! ```
//!

fn _empty() {}
