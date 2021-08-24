//! The module contains traits that used in [`Parser`] handler.
//!
//! [`Parser`]: handler::Parser

/// The trait is used to parse some data into some other data with
/// possibility of its reverse recombination.
pub trait Parseable<To>: Sized {
    type Rest;
    fn parse(self) -> Result<(To, Self::Rest), Self>;
    fn recombine(data: (To, Self::Rest)) -> Self;
}
