use crate::handler::end_point::by_event::{EndPointByEvent, EndPointByEventEnter};
use crate::handler::end_point::by_store::{EndPointByStore, EndPointByStoreEnter};
use crate::handler::{EndPoint, Handler, HandlerFuture};
use crate::parser::Parseable;
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
/// use dptree::parser::Parseable;
///
/// #[derive(Debug, PartialEq)]
/// enum Event {
///     Ping,
///     Multiply(Multiply),
/// }
/// #[derive(Debug, PartialEq)]
/// struct Multiply(u32, u32);
///
/// impl Parseable<Multiply> for Event {
///     type Rest = ();
///
///     fn parse(self) -> Result<(Multiply, Self::Rest), Self> {
///         match self {
///             Event::Multiply(mult) => Ok((mult, ())),
///             this => Err(this)
///         }
///     }
///     fn recombine(data: (Multiply, Self::Rest)) -> Self {
///         Event::Multiply(data.0)
///     }
/// }
///
/// let parser = dptree::parser::<Event, Multiply>()
///     .end_point(|Multiply(x, y): Multiply| async move { x * y });
///
/// assert_eq!(parser.handle(Event::Multiply(Multiply(5, 4))).await, Ok(20));
/// assert!(parser.handle(Event::Ping).await.is_err());
/// # }
/// ```
pub struct Parser<H, From, To> {
    handler: H,
    _phantom: PhantomData<(From, To)>,
}

impl<H, From, To> Parser<H, From, To>
where
    H: Handler<To>,
    From: Parseable<To>,
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
    From: Parseable<To> + Send + Sync + 'static,
    <From as Parseable<To>>::Rest: Send,
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

/// Builder for the `Parser` struct.
///
/// For more info see `Parser` struct.
pub struct ParserBuilder<From, To> {
    _phantom: PhantomData<(From, To)>,
}

impl<FromT, To> ParserBuilder<FromT, To>
where
    FromT: Parseable<To>,
{
    pub fn new() -> Self {
        ParserBuilder {
            _phantom: PhantomData,
        }
    }

    /// Constructs `Parser` with following handler.
    pub fn and_then<H>(self, handler: H) -> Parser<H, FromT, To>
    where
        H: Handler<To>,
    {
        Parser::new(handler)
    }

    /// Shortcut for `builder.and_then(EndPoint::by_event(func))`.
    pub fn end_point<Func, Need>(self, func: Func) -> Parser<EndPointByEvent<Func, Need>, FromT, To>
    where
        EndPointByEvent<Func, Need>: Handler<To>,
    {
        self.and_then(EndPoint::by_event(func))
    }

    /// Shortcut for `builder.and_then(EndPoint::by_store(func))`.
    pub fn end_point_by_store<Func, Args, Store>(
        self,
        func: Func,
    ) -> Parser<EndPointByStore<Func, Args>, FromT, To>
    where
        EndPoint<Store>: EndPointByStoreEnter<Func, Args>,
        EndPointByStore<Func, Args>: Handler<To>,
    {
        self.and_then(EndPoint::by_store(func))
    }
}

impl<FromT, To, H, Res> HandlerBuilder<FromT, H> for ParserBuilder<FromT, To>
where
    FromT: Parseable<To>,
    H: Handler<To, Res = Res>,
    Parser<H, FromT, To>: Handler<FromT>,
{
    type OutEvent = To;
    type ResultAndThen = Parser<H, FromT, To>;

    fn and_then(self, handler: H) -> Self::ResultAndThen {
        Parser::new(handler)
    }
}

/// Shortcut for `ParserBuilder::new()`.
pub fn parser<From, To>() -> ParserBuilder<From, To>
where
    From: Parseable<To>,
{
    ParserBuilder::new()
}
