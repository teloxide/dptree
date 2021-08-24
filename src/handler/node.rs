use crate::handler::{BoxHandler, Handler, HandlerFuture};
use std::sync::Arc;

/// `Node` is a handler containing many subsequent handlers and provides a way to branching
/// handling.
///
/// `Node` in dispatching tree means node that just try to put input event in the following
/// handlers.
///
/// Basic usage:
/// ```
/// extern crate dispatch_tree as dptree;
/// use dispatch_tree::Handler;
///
/// # #[tokio::main]
/// # async fn main() {
/// // Creating handler that multiply input number if it bigger than 5.
/// let multiply = dptree::filter(|&num: &i32| num > 5)
///     .leaf(|num: i32| async move { num * 2 });
///
/// // Creating handler that divide input number if it less than -5.
/// let divide = dptree::filter(|&num: &i32| num < -5)
///     .leaf(|num: i32| async move { num / 2 });
///
/// // Creating node.
/// let node = dptree::node()
///     .and(multiply)
///     .and(divide)
///     .build();
///
/// assert_eq!(node.handle(10).await, Ok(20));
/// assert_eq!(node.handle(-6).await, Ok(-3));
/// // There are no handler that accepts 0 so `Node` returns an error.
/// assert_eq!(node.handle(0).await, Err(0));
/// # }
pub struct Node<Data, Res> {
    children: Arc<Vec<BoxHandler<Data, Res>>>,
}

impl<Data, Res> Node<Data, Res> {
    pub fn new(children: Arc<Vec<BoxHandler<Data, Res>>>) -> Self {
        Node { children }
    }
}

impl<Data, Res> Handler<Data> for Node<Data, Res>
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

/// Builder for the `Node` struct.
///
/// Basic usage:
/// ```
/// let node = dispatch_tree::node()
///     .and(|event: u32| async move { Ok(()) })
///     .and(|event: u32| async move { Err(event) })
///     .build();
/// ```
pub struct NodeBuilder<Data, Res> {
    children: Vec<BoxHandler<Data, Res>>,
}

impl<Data, Res> NodeBuilder<Data, Res> {
    pub fn new() -> Self {
        NodeBuilder { children: vec![] }
    }

    /// Adds a handler to the end of queue.
    pub fn and<H>(mut self, handler: H) -> Self
    where
        H: Handler<Data, Res = Res> + Send + Sync + 'static,
    {
        self.children.push(Box::new(handler));
        self
    }

    /// Builds a `Node` with the handlers.
    pub fn build(self) -> Node<Data, Res> {
        Node::new(Arc::new(self.children))
    }
}

/// Shortcut for `NodeBuilder::new()`
pub fn node<Data, Res>() -> NodeBuilder<Data, Res> {
    NodeBuilder::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_node_handler() {
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);
        let tx = Arc::new(tx);
        let handlers: Vec<BoxHandler<i32, ()>> = vec![Box::new(move |x: i32| {
            let tx = tx.clone();
            async move {
                tx.send(x == 0).await.unwrap();
                Ok(())
            }
        })];
        let node = Node::new(Arc::new(handlers));
        node.handle(0).await.unwrap();

        std::mem::drop(node);

        assert!(rx.recv().await.unwrap());
    }
}
