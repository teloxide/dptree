//! The [`Fallible`] trait, used by the fallible handlers

/// A handler `Output` that can represent a failure.
///
/// The fallible handlers ([`try_filter`], [`try_map`], [`try_filter_map`])
/// take a function that returns [`Result`]. On `Ok`, execution continues as
/// usual; on `Err(e)`, the chain short-circuits and ends the dispatch with the
/// error as the output; the same way [`endpoint`](crate::endpoint) ends
/// it with a value.
///
/// [`Result`] implements this out of the box. Implement it yourself if your
/// `Output` is another type that can still represent an error (e.g. an HTTP
/// response that encodes it in the status code).
///
/// [`try_filter`]: crate::try_filter
/// [`try_map`]: crate::try_map
/// [`try_filter_map`]: crate::try_filter_map
pub trait Fallible {
    /// The error type carried by this output.
    type Error: 'static;

    /// Constructs an output value representing the given error.
    fn from_error(error: Self::Error) -> Self;
}

impl<T, E: 'static> Fallible for Result<T, E> {
    type Error = E;

    fn from_error(error: E) -> Self {
        Err(error)
    }
}
