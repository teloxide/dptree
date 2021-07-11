use std::marker::PhantomData;
use crate::handler::{Handler, HandlerFuture};
use futures::future::BoxFuture;
use crate::parser::Handlerable;
use futures::TryFutureExt;

/// Parser is used to parse incoming `Data` into other `ParsedData`. Next handlers in the tree
/// will receive the `ParsedData` as the `Data` argument.
///
/// `Data` should implement `Handlerable<ParsedData, Rest>` where `ParsedData` is the parsed data
/// from the incoming `Data` and `Rest` is information that needs to reconstruct `ParsedData` back
/// to the `Data`. It reconstructs iff next handler returns an `Err` that means that handler cannot
/// handle the incoming data.
pub struct Parser<H, From, To, Rest> {
    handler: H,
    _phantom: PhantomData<(From, To, Rest)>
}

impl<H, From, To, Rest> Parser<H, From, To, Rest> {
    pub fn new(handler: H) -> Self {
        Parser { handler, _phantom: PhantomData }
    }
}

impl<H, Res, From, To, Rest> Handler<From, Res> for Parser<H, From, To, Rest>
where
    H: Handler<To, Res> + Send + Sync,
    Res: Send + 'static,
    From: Handlerable<To, Rest> + Send + Sync + 'static,
    To: Send + Sync + 'static,
    Rest: Send + Sync + 'static,
{
    fn handle(&self, data: From) -> HandlerFuture<Res, From> {
        match data.parse() {
            Ok((data, rest)) => {
                Box::pin(self.handler.handle(data).map_err(|to| From::recombine((to, rest))))
            },
            Err(data) => Box::pin(futures::future::err(data))
        }
    }
}
