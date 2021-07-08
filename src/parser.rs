//! The module contains traits that used in [`Parser`] handler.
//!
//! [`Parser`]: handler::Parser

/// The trait is used to parse some data into some other data with
/// possibility of its reverse recombination.
pub trait Parseable<To, Rest>: Sized {
    fn parse(self) -> Result<(To, Rest), Self>;
}

/// The trait is used to recombine [`Parseable`] output back to the input.
pub trait RecombineFrom<To, Rest>: Sized {
    fn recombine(data: (To, Rest)) -> Self;
}

/// The trait is used to unite both `Parseable` and `RecombineFrom` trait into one trait.
pub trait Handlerable<To, Rest>: Parseable<To, Rest> + RecombineFrom<To, Rest> {}

impl<T, To, Rest> Handlerable<To, Rest> for T where T: Parseable<To, Rest> + RecombineFrom<To, Rest> {}
