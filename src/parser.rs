//! The module contains traits that used in [`Parser`] handler.
//!
//! [`Parser`]: handler::Parser

/// The trait is used to parse some data into some other data with
/// possibility of its reverse recombination.
pub trait Parseable<To>: Sized {
    type Rest;
    fn parse(self) -> Result<(To, Self::Rest), Self>;
}

/// The trait is used to recombine [`Parseable`] output back to the input.
pub trait RecombineFrom<To>: Sized {
    type Rest;
    fn recombine(data: (To, Self::Rest)) -> Self;
}

/// The trait is used to unite both `Parseable` and `RecombineFrom` trait into one trait.
pub trait Handlerable<To>:
    Parseable<To, Rest = <Self as Handlerable<To>>::Rest>
    + RecombineFrom<To, Rest = <Self as Handlerable<To>>::Rest>
{
    type Rest;
}

impl<T, To, Rest> Handlerable<To> for T
where
    T: Parseable<To, Rest = Rest> + RecombineFrom<To, Rest = Rest>,
{
    type Rest = Rest;
}
