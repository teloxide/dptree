use crate::parser::Handlerable;

/// The trait is used to recognize which update store the `Context` type.
/// You can look at it as ad-hoc HKT.
pub trait Context {
    type Event;

    fn get_event(&self) -> &Self::Event;
    fn into_event(self) -> Self::Event;
}

/// The trait is used to define the Self with another update as an element. It
/// is the hack to simulate the `GAT`.
pub trait ContextWith<Elem>: Context {
    type Context: Context<Event = Elem>;
}

/// The trait is used to parse a context with `Event1` to context with `Event2`.
pub trait ParseContext<Event2, Rest>: ContextWith<Event2> + Sized
where
    Self::Event: Handlerable<Event2, Rest>
{
    fn parse(self) -> Result<(Self::Context, Rest), Self>;

    fn recombine(data: (Self::Context, Rest)) -> Self;
}

/// The trait is used to get the data from the context and pass it to the
/// handler.
///
/// All types thet implement the trait can be passed to the handler function.
/// E.g. if you have your own `Typ` thet implement `FromContext` and you point
/// it in the handler function signature, the `from_context` function will be
/// called. If it return `None` than handling will be stop and update pass to
/// the other handler functions.
///
/// Usually you want to define `Ctx` generic and use `GetCtx` trait with needed
/// context as first type argument. This is need to get your type from the
/// different kinds of contexts from different dispatchers.
pub trait FromContext<Ctx>: Sized {
    fn from_context(context: &Ctx) -> Option<Self>;
}

/// The trait is used to get the data from the context and pass it to the
/// handler by ownership.
///
/// It calls after `FromContext` trait before pass to the handler.
/// `FromContextOwn` argument must be first in the handler.
///
/// `RequireCtx` - the Ctx which you want to get from the `Ctx`. It is needed
/// only if your `RequireCtx` contains generic arguments without depends from
/// the `Self`.
pub trait FromContextOwn<Ctx, RequireCtx = Ctx>: Sized {
    fn from_context(context: Ctx) -> Self;
}

#[derive(Debug)]
pub struct IdentityContext<T>(T);

impl<T> IdentityContext<T> {
    pub fn new(event: T) -> Self {
        Self(event)
    }
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Context for IdentityContext<T> {
    type Event = T;

    fn get_event(&self) -> &Self::Event {
        &self.0
    }

    fn into_event(self) -> Self::Event {
        self.into_inner()
    }
}

impl<T, U> ContextWith<U> for IdentityContext<T> {
    type Context = IdentityContext<U>;
}

impl<T, Event2, Rest> ParseContext<Event2, Rest> for IdentityContext<T>
where
    T: Handlerable<Event2, Rest>
{
    fn parse(self) -> Result<(Self::Context, Rest), Self> {
        self
            .into_inner()
            .parse()
            .map(|(event2, rest)| (IdentityContext(event2), rest))
            .map_err(IdentityContext)
    }

    fn recombine((ctx, rest): (IdentityContext<Event2>, Rest)) -> Self {
        IdentityContext(T::recombine((ctx.into_inner(), rest)))
    }
}
