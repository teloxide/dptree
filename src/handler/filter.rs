use crate::handler::Handler;
use futures::future::{BoxFuture};

/// Struct that filtering event by some condition
pub struct Filter<F, H> {
    condition: F,
    handler: H,
}

impl<F, H> Filter<F, H> {
    pub fn new(condition: F, handler: H) -> Self {
        Filter { condition, handler }
    }

    pub fn into_inner(self) -> (F, H) {
        (self.condition, self.handler)
    }
}

impl<F, H> Filter<F, H> {
    pub fn and<'a, Data, Cond>(self, cond2: Cond) -> Filter<impl Fn(&'a Data) -> bool, H>
    where
        F: Fn(&'a Data) -> bool + 'a,
        Cond: Fn(&'a Data) -> bool + 'a,
        Data: 'a,
    {
        let (cond1, handler) = self.into_inner();
        let new_cond = move |data| cond1(data) && cond2(data);
        Filter::new(new_cond, handler)
    }
}

impl<F, H, Data, Res> Handler<Data, Res> for Filter<F, H>
where
    F: Fn(&Data) -> bool + Send + Sync,
    H: Handler<Data, Res> + Send + Sync,
    Data: Send + Sync + 'static,
{
    fn handle(&self, data: Data) -> BoxFuture<Result<Res, Data>> {
        Box::pin(async move {
            match (self.condition)(&data) {
                true => self.handler.handle(data).await,
                false => Err(data),
            }
        })
    }
}
