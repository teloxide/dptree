mod filter;
mod node;
mod leaf;
mod parser;

use futures::future::BoxFuture;
use futures::Future;

pub trait Handler<Data, Res> {
    fn handle(&self, data: Data) -> BoxFuture<Result<Res, Data>>;
}

pub type BoxHandler<Data, Res> = Box<dyn Handler<Data, Res> + Send + Sync>;

impl<Func, Data, Res, Fut> Handler<Data, Res> for Func
where
    Func: Fn(Data) -> Fut,
    Fut: Future<Output = Result<Res, Data>> + Send + 'static,
{
    fn handle(&self, data: Data) -> BoxFuture<Result<Res, Data>> {
        Box::pin(self(data))
    }
}
