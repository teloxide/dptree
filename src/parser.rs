/// Trait is used to parse some data into some other data with
/// possibility of recombine it.
pub trait Parseable<To, Rest>: Sized {
    fn parse(self) -> Result<(To, Rest), Self>;
}

/// Trait is used to recombine [`Parseable`] output back to the input.
pub trait RecombineFrom<To, Rest>: Sized {
    fn recombine(data: (To, Rest)) -> Self;
}

pub trait Handlerable<To, Rest>: Parseable<To, Rest> + RecombineFrom<To, Rest> {}

impl<T, To, Rest> Handlerable<To, Rest> for T where T: Parseable<To, Rest> + RecombineFrom<To, Rest> {}
