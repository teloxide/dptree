use crate::container::DiContainer;
use futures::future::BoxFuture;
use std::{future::Future, sync::Arc};

pub trait IntoDiFn<Input, Output, FnArgs> {
    fn into(self) -> DiFn<Input, Output>;
}

pub type DiFn<Input, Output> =
    Arc<dyn for<'a> Fn(&'a Input) -> BoxFuture<'a, Output> + Send + Sync + 'static>;

macro_rules! impl_into_di {
    ($($generic:ident),*) => {
        impl<Func, Input, Output, Fut, $($generic),*> IntoDiFn<Input, Output, (Fut, $($generic),*)> for Func
        where
            Input: $(DiContainer<$generic> +)*,
            Func: Fn($(Arc<$generic>),*) -> Fut + Send + Sync + 'static,
            Fut: Future<Output = Output> + Send + Sync + 'static,
            $($generic: Send + Sync),*
        {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn into(self) -> DiFn<Input, Output> {
                let this = Arc::new(self);
                Arc::new(move |input: &Input| {
                    let this = this.clone();
                    $(let $generic = input.get();)*
                    let fut = this( $( $generic ),* );
                    Box::pin(fut)
                })
            }
        }
    };
}

impl_into_di!();
impl_into_di!(A);
impl_into_di!(A, B);
impl_into_di!(A, B, C);
impl_into_di!(A, B, C, D);
impl_into_di!(A, B, C, D, E);
impl_into_di!(A, B, C, D, E, F);
impl_into_di!(A, B, C, D, E, F, G);
impl_into_di!(A, B, C, D, E, F, G, H);
impl_into_di!(A, B, C, D, E, F, G, H, I);
