use crate::handler::leaf::by_empty::{LeafByEmpty, LeafEmptyEnter};
use crate::handler::leaf::by_event::{LeafByEvent, LeafEventEnter};
use crate::handler::{Handler, HandlerFuture, Leaf};
use crate::parser::Handlerable;
use futures::TryFutureExt;
use std::future::Future;
use std::marker::PhantomData;

/// Parser is used to parse incoming `Data` into other `ParsedData`. Next handlers in the tree
/// will receive the `ParsedData` as the `Data` argument.
///
/// `Data` should implement `Handlerable<ParsedData, Rest>` where `ParsedData` is the parsed data
/// from the incoming `Data` and `Rest` is information that needs to reconstruct `ParsedData` back
/// to the `Data`. It reconstructs iff next handler returns an `Err` that means that handler cannot
/// handle the incoming data.
pub struct Parser<H, From, To> {
    handler: H,
    _phantom: PhantomData<(From, To)>,
}

impl<H, From, To> Parser<H, From, To>
where
    H: Handler<To>,
    From: Handlerable<To>,
{
    pub fn new(handler: H) -> Self {
        Parser {
            handler,
            _phantom: PhantomData,
        }
    }
}

impl<H, Res, From, To> Handler<From> for Parser<H, From, To>
where
    H: Handler<To, Res = Res> + Send + Sync,
    Res: Send + 'static,
    From: Handlerable<To> + Send + Sync + 'static,
    <From as Handlerable<To>>::Rest: Send,
    To: Send + Sync + 'static,
{
    type Res = Res;
    fn handle(&self, data: From) -> HandlerFuture<Res, From> {
        match data.parse() {
            Ok((data, rest)) => Box::pin(
                self.handler
                    .handle(data)
                    .map_err(|to| From::recombine((to, rest))),
            ),
            Err(data) => Box::pin(futures::future::err(data)),
        }
    }
}

pub struct ParserBuilder<From, To> {
    _phantom: PhantomData<(From, To)>,
}

impl<FromT, To> ParserBuilder<FromT, To>
where
    FromT: Handlerable<To>,
{
    pub fn new() -> Self {
        ParserBuilder {
            _phantom: PhantomData,
        }
    }

    pub fn and_then<H>(self, handler: H) -> Parser<H, FromT, To>
    where
        H: Handler<To>,
    {
        Parser::new(handler)
    }

    /// Shortcut for `builder.and_then(Leaf::enter_event(func))`.
    pub fn leaf_event<Func>(self, func: Func) -> Parser<LeafByEvent<Func>, FromT, To>
    where
        LeafByEvent<Func>: Handler<To>,
    {
        self.and_then(Leaf::enter_event(func))
    }

    /// Shortcut for `builder.and_then(Leaf::enter_event(func))`.
    pub fn leaf_empty<Func, Fut>(self, func: Func) -> Parser<LeafByEmpty<Func>, FromT, To>
    where
        Func: Fn() -> Fut,
        Fut: Future + Send + 'static,
        To: 'static,
    {
        self.and_then(Leaf::enter_empty(func))
    }
}

pub fn parser<From, To>() -> ParserBuilder<From, To>
where
    From: Handlerable<To>,
{
    ParserBuilder::new()
}
