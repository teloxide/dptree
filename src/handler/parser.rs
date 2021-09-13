use crate::handler::endpoint::by_event::{EndpointByEvent, EndpointByEventEnter};
use crate::handler::endpoint::by_store::{EndpointByStore, EndpointByStoreEnter};
use crate::handler::{Endpoint, Handler, HandlerFuture};
use crate::HandlerBuilder;
use futures::TryFutureExt;
use std::marker::PhantomData;

/// Parser is used to parse incoming `Data` into other `ParsedData`. Next handlers in the tree
/// will receive the `ParsedData` as the `Data` argument.
///
/// `Data` should implement `Handlerable<ParsedData, Rest>` where `ParsedData` is the parsed data
/// from the incoming `Data` and `Rest` is information that needs to reconstruct `ParsedData` back
/// to the `Data`. It reconstructs iff next handler returns an `Err` that means that handler cannot
/// handle the incoming data.
///
/// Basic usage:
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dptree::Handler;
///
/// #[derive(Debug, PartialEq)]
/// enum Event {
///     Ping,
///     Multiply(u32, u32),
/// }
///
/// fn parse_multiply_event(event: &Event) -> Option<(u32, u32)> {
///     match event {
///         Event::Multiply(x, y) => Some((*x, *y)),
///         _ => None,
///     }
/// }
///
/// let parser = dptree::parser(parse_multiply_event)
///     .endpoint(|(x, y)| async move { x * y });
///
/// assert_eq!(parser.handle(Event::Multiply(5, 4)).await, Ok(20));
/// assert!(parser.handle(Event::Ping).await.is_err());
/// # }
/// ```
pub struct Parser<P, H, From> {
    parser: P,
    handler: H,
    _phantom: PhantomData<From>,
}

impl<P, H, From> Parser<P, H, From> {
    pub fn new(parser: P, handler: H) -> Self {
        Parser {
            parser,
            handler,
            _phantom: PhantomData,
        }
    }
}

impl<P, H, Res, From, To> Handler<From> for Parser<P, H, From>
where
    P: Parse<From, To = To>,
    H: Handler<To, Res = Res>,
    From: Send + 'static,
    Res: Send + 'static,
    To: 'static,
{
    type Res = Res;
    fn handle(&self, data: From) -> HandlerFuture<Res, From> {
        match self.parser.parse(&data) {
            Some(to) => Box::pin(self.handler.handle(to).map_err(|_| data)),
            None => Box::pin(futures::future::err(data)),
        }
    }
}

/// Builder for the `Parser` struct.
///
/// For more info see `Parser` struct.
pub struct ParserBuilder<P, From, To> {
    parser: P,
    _phantom: PhantomData<(From, To)>,
}

impl<P, FromT, To> ParserBuilder<P, FromT, To>
where
    P: Parse<FromT, To = To>,
{
    pub fn new(parser: P) -> Self {
        ParserBuilder {
            parser,
            _phantom: PhantomData,
        }
    }

    /// Constructs `Parser` with following handler.
    pub fn and_then<H>(self, handler: H) -> Parser<P, H, FromT>
    where
        H: Handler<To>,
    {
        Parser::new(self.parser, handler)
    }

    /// Shortcut for `builder.and_then(Endpoint::by_event(func))`.
    pub fn endpoint<Func, Need>(self, func: Func) -> Parser<P, EndpointByEvent<Func, Need>, FromT>
    where
        EndpointByEvent<Func, Need>: Handler<To>,
    {
        self.and_then(Endpoint::by_event(func))
    }

    /// Shortcut for `builder.and_then(Endpoint::by_store(func))`.
    pub fn endpoint_by_store<Func, Args, Store>(
        self,
        func: Func,
    ) -> Parser<P, EndpointByStore<Func, Args>, FromT>
    where
        Endpoint<Store>: EndpointByStoreEnter<Func, Args>,
        EndpointByStore<Func, Args>: Handler<To>,
    {
        self.and_then(Endpoint::by_store(func))
    }
}

impl<P, FromT, To, H, Res> HandlerBuilder<FromT, H> for ParserBuilder<P, FromT, To>
where
    P: Parse<FromT, To = To>,
    H: Handler<To, Res = Res>,
    Parser<P, H, FromT>: Handler<FromT>,
{
    type OutEvent = To;
    type ResultAndThen = Parser<P, H, FromT>;

    fn and_then(self, handler: H) -> Self::ResultAndThen {
        Parser::new(self.parser, handler)
    }
}

/// Shortcut for `ParserBuilder::new()`.
pub fn parser<P, From, To>(parser: P) -> ParserBuilder<P, From, To>
where
    P: Parse<From, To = To>,
{
    ParserBuilder::new(parser)
}

pub trait Parse<From> {
    type To;
    fn parse(&self, from: &From) -> Option<Self::To>;
}

impl<F, From, To> Parse<From> for F
where
    F: Fn(&From) -> Option<To>,
{
    type To = To;

    fn parse(&self, from: &From) -> Option<Self::To> {
        self(from)
    }
}
