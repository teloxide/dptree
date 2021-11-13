//! Function that can be used with DI. Used in such handlers.

use crate::container::DiContainer;
use futures::future::BoxFuture;
use std::{future::Future, sync::Arc};

#[rustfmt::skip] // rustfmt too bad in formatting lists
/// The trait is used to convert functions into `DiFn`.
///
/// The function must follow some rules, to be usable with DI:
///
/// 1. All input values must be wrapped around `Arc`. It is requirement of the
/// `DiContainer` trait.
/// 2. Function must have 0-9 arguments.
/// 3. Function must return `Future`.
pub trait IntoDiFn<Input, Output, FnArgs> {
    fn into(self) -> DiFn<Input, Output>;
}

/// The function to which the reference to the container is passed.
pub type DiFn<Input, Output> =
    Arc<dyn for<'a> Fn(&'a Input) -> BoxFuture<'a, Output> + Send + Sync + 'static>;

macro_rules! impl_into_di {
    ($($generic:ident),*) => {
        impl<Func, Input, Output, Fut, $($generic),*> IntoDiFn<Input, Output, (Fut, $($generic),*)> for Func
        where
            Input: $(DiContainer<$generic> +)*,
            Func: Fn($(Arc<$generic>),*) -> Fut + Send + Sync + 'static,
            Fut: Future<Output = Output> + Send + 'static,
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
