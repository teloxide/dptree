use std::marker::PhantomData;
use crate::handler::Handler;
use futures::future::BoxFuture;
use crate::parser::Handlerable;

pub struct ParserHandler<H, From, To, Rest> {
    handler: H,
    _phantom: PhantomData<(From, To, Rest)>
}

impl<H, Res, From, To, Rest> Handler<From, Res> for ParserHandler<H, From, To, Rest>
where
    H: Handler<To, Res> + Send + Sync,
    From: Handlerable<To, Rest>,
    Res: Send,
    From: Send + Sync,
    To: Send + Sync,
    Rest: Send + Sync,
{
    fn handle(&self, data: From) -> BoxFuture<Result<Res, From>> {
        match data.parse() {
            Ok((data, rest)) => Box::pin(async move {
                let inner_handler_result = self.handler.handle(data).await;
                inner_handler_result.map_err(|to| From::recombine((to, rest)))
            }),
            Err(data) => Box::pin(futures::future::err(data))
        }
    }
}
