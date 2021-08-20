use crate::handler::leaf::by_event::{LeafByEvent, LeafEventEnter};
use crate::handler::{Handler, HandlerFuture, Leaf};
use std::marker::PhantomData;

/// Struct that filtering event by a condition.
///
/// If the event satisfy the condition then handler will be called. Otherwise event will be sent up.
///
/// Basic usage:
/// ```
/// # #[tokio::main]
/// # async fn main() {
/// use dispatch_tree::handler::filter::Filter;
/// use dispatch_tree::Handler;
///
/// let filter = Filter::new(
///     |&data: &u32| data == 10, // filter event by condition
///     |data: u32| async move { Ok(()) }, // just return Ok(()) if event satisfy the condition
/// );
///
/// let result_with_10 = filter.handle(10u32).await;
/// assert_eq!(result_with_10, Ok(()));
///
/// let result_with_2 = filter.handle(2u32).await;
/// assert_eq!(result_with_2, Err(2));
/// # }
/// ```
pub struct Filter<F, H> {
    condition: F,
    handler: H,
}

impl<F, H> Filter<F, H> {
    /// Condition must be function `Fn(&Data) -> bool`, handler must implements `Handler<Data>`
    pub fn new(condition: F, handler: H) -> Self {
        Filter { condition, handler }
    }

    pub fn into_inner(self) -> (F, H) {
        (self.condition, self.handler)
    }
}
/* TODO: this doesn't compile
impl<F, H> Filter<F, H> {
    /// Adds new condition to an existing filter.
    ///
    /// Basic usage:
    /// ```
    /// # #[tokio::main]
    /// # async fn main() {
    /// use dispatch_tree::handler::filter::Filter;
    /// use dispatch_tree::Handler;
    ///
    /// let filter = Filter::new(
    ///     |&data: &u32| data % 2 == 0, // filter odds
    ///     |data: u32| async move { Ok(()) },
    /// );
    ///
    /// assert_eq!(filter.handle(10u32).await, Ok(()));
    ///
    /// // additionally filter numbers that are completely divisible by three
    /// let new_filter = filter.and(|&data| data % 3 == 0);
    ///
    /// assert_eq!(new_filter.handle(10u32).await, Err(10));
    /// assert_eq!(new_filter.handle(12u32).await, Ok(()));
    /// # }
    /// ```
    pub fn and<Data, Cond>(self, cond2: Cond) -> Filter<impl for<'a> Fn(&Data) -> bool + Send + Sync, H>
    where
        F: for<'a> Fn(&'a Data) -> bool + Send + Sync,
        Cond: for<'a> Fn(&'a Data) -> bool + Send + Sync,
    {
        let (cond1, handler) = self.into_inner();
        let new_cond = move |data| cond1(data) && cond2(data);
        Filter::new(new_cond, handler)
    }
}
*/
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

/// Builder for the `Filter` struct
///
/// Basic usage:
/// ```
/// use dispatch_tree::handler::filter::FilterBuilder;
///
/// let filter1 = FilterBuilder::new(|&data: &u32| data == 0)
///     .and_then(|data: u32| async move { Ok(()) });
///
/// let filter2 = FilterBuilder::new(|&data: &u32| data == 0)
///     .leaf(|data: u32| async move { data * 2 });
/// ```
pub struct FilterBuilder<F, Data> {
    condition: F,
    _phantom: PhantomData<Data>,
}

impl<F, Data> FilterBuilder<F, Data>
where
    F: Fn(&Data) -> bool + Send + Sync,
{
    /// Creates `FilterBuilder`. Requires a condition for construction.
    pub fn new(condition: F) -> Self {
        FilterBuilder {
            condition,
            _phantom: PhantomData,
        }
    }

    /// Builds `Filter` with the handler.
    pub fn and_then<H>(self, handler: H) -> Filter<F, H>
    where
        H: Handler<Data>,
    {
        Filter {
            condition: self.condition,
            handler,
        }
    }

    /// Shortcut for `builder.and_then(Leaf::enter_event(func))`.
    pub fn leaf<Func, Need>(self, func: Func) -> Filter<F, LeafByEvent<Func, Need>>
    where
        LeafByEvent<Func, Need>: Handler<Data>,
    {
        self.and_then(Leaf::enter_event(func))
    }
}

/// Shortcut for `FilterBuilder::new`.
pub fn filter<F, Data>(condition: F) -> FilterBuilder<F, Data>
where
    F: Fn(&Data) -> bool + Send + Sync,
{
    FilterBuilder::new(condition)
}
