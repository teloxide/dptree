/// Returns function that uses `matches!` on the argument.
///
/// Basic usage:
/// ```
/// let check_zero = dptree::matches!(0);
/// assert!(check_zero(0));
/// ```
#[macro_export]
macro_rules! matches {
    ($pattern:pat) => {
        |x| match x {
            $pattern => true,
            _ => false,
        }
    };
}
