use crate::handler::{Handler, HandlerFuture};

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

impl<F, H, Data, Res> Handler<Data> for Filter<F, H>
where
    F: Fn(&Data) -> bool + Send + Sync,
    H: Handler<Data, Res = Res> + Send + Sync,
    Data: Send + Sync + 'static,
    Res: Send + 'static,
{
    type Res = Res;
    fn handle(&self, data: Data) -> HandlerFuture<Res, Data> {
        match (self.condition)(&data) {
            true => Box::pin(self.handler.handle(data)),
            false => Box::pin(futures::future::err(data)),
        }
    }
}
