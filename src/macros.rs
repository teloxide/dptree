#[macro_export]
macro_rules! matches {
    ($pattern:pat) => {
        |x| match x {
            $pattern => true,
            _ => false,
        }
    };
}
