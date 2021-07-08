use crate::handler::{Handler, BoxHandler, HandlerFuture};
use futures::future::BoxFuture;
use std::sync::Arc;

/// Node is a node.
pub struct Node<Data, Res> {
    children: Arc<Vec<BoxHandler<Data, Res>>>
}

impl<Data, Res> Node<Data, Res> {
    pub fn new(children: Arc<Vec<BoxHandler<Data, Res>>>) -> Self {
        Node { children }
    }
}

impl<Data, Res> Handler<Data, Res> for Node<Data, Res>
where
    Data: Send + Sync + 'static,
    Res: 'static,
{
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_node_handler() {
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);
        let tx = Arc::new(tx);
        let handlers: Vec<BoxHandler<i32, ()>> = vec![
            Box::new(move |x: i32| {
                let tx = tx.clone();
                async move {
                    tx.send(x == 0).await.unwrap();
                    Ok(())
                }
            })
        ];
        let node = Node::new(Arc::new(handlers));
        node.handle(0).await.unwrap();

        std::mem::drop(node);

        assert!(rx.recv().await.unwrap());
    }
}
