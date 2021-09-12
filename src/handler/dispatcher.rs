use crate::handler::{BoxHandler, Handler, HandlerFuture};
use std::sync::Arc;

/// `Dispatcher` is a handler containing many subsequent handlers and provides a way to branching
/// handling.
///
/// `Dispatcher` in dispatching tree means node that just try to put input event in the following
/// handlers.
///
/// Basic usage:
/// ```
/// use dptree::Handler;
///
/// # #[tokio::main]
/// # async fn main() {
/// // Creating handler that multiply input number if it is bigger than 5.
/// let multiply = dptree::filter(|&num: &i32| num > 5)
///     .end_point(|num: i32| async move { num * 2 });
///
/// // Creating handler that divide input number if it less than -5.
/// let divide = dptree::filter(|&num: &i32| num < -5)
///     .end_point(|num: i32| async move { num / 2 });
///
/// // Creating dispatcher.
/// let dispatcher = dptree::dispatch()
///     .to(multiply)
///     .to(divide)
///     .build();
///
/// assert_eq!(dispatcher.handle(10).await, Ok(20));
/// assert_eq!(dispatcher.handle(-6).await, Ok(-3));
/// // There are no handler that accepts 0 so `Dispatcher` returns an error.
/// assert_eq!(dispatcher.handle(0).await, Err(0));
/// # }
pub struct Dispatcher<Data, Res> {
    children: Arc<Vec<BoxHandler<Data, Res>>>,
}

impl<Data, Res> Dispatcher<Data, Res> {
    pub fn new(children: Arc<Vec<BoxHandler<Data, Res>>>) -> Self {
        Dispatcher { children }
    }
}

impl<Data, Res> Handler<Data> for Dispatcher<Data, Res>
where
    Data: Send + Sync + 'static,
    Res: 'static,
{
    type Res = Res;
    fn handle(&self, data: Data) -> HandlerFuture<Res, Data> {
        let children = self.children.clone();
        Box::pin(async move {
            let mut data = data;
            for handler in children.iter() {
                match handler.handle(data).await {
                    Ok(res) => return Ok(res),
                    Err(d) => {
                        data = d;
                    }
                }
            }
            Err(data)
        })
    }
}

/// Builder for the `Dispatcher` struct.
///
/// Basic usage:
/// ```
/// # let handler1 = |event: u32| async move { Ok(()) };
/// # let handler2 = |event: u32| async move { Err(event) };
/// let dispatcher = dptree::dispatch()
///     .to(handler1)
///     .to(handler2)
///     .build();
/// ```
pub struct DispatcherBuilder<Data, Res> {
    children: Vec<BoxHandler<Data, Res>>,
}

impl<Data, Res> DispatcherBuilder<Data, Res> {
    pub fn new() -> Self {
        DispatcherBuilder { children: vec![] }
    }

    /// Adds a handler to the end of queue.
    pub fn to<H>(mut self, handler: H) -> Self
    where
        H: Handler<Data, Res = Res> + Send + Sync + 'static,
    {
        self.push(handler);
        self
    }

    /// Adds a handler to the end of queue.
    pub fn push<H>(&mut self, handler: H)
    where
        H: Handler<Data, Res = Res> + Send + Sync + 'static,
    {
        self.children.push(Box::new(handler));
    }

    /// Builds a `Dispatcher` with the handlers.
    pub fn build(self) -> Dispatcher<Data, Res> {
        Dispatcher::new(Arc::new(self.children))
    }
}

/// Shortcut for `DispatcherBuilder::new()`
pub fn dispatch<Data, Res>() -> DispatcherBuilder<Data, Res> {
    DispatcherBuilder::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_dispatcher_handler() {
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);
        let tx = Arc::new(tx);
        let handlers: Vec<BoxHandler<i32, ()>> = vec![Box::new(move |x: i32| {
            let tx = tx.clone();
            async move {
                tx.send(x == 0).await.unwrap();
                Ok(())
            }
        })];
        let dispatcher = Dispatcher::new(Arc::new(handlers));
        dispatcher.handle(0).await.unwrap();

        std::mem::drop(dispatcher);

        assert!(rx.recv().await.unwrap());
    }
}
