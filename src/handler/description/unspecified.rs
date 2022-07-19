use crate::HandlerDescription;

/// Uninformative handler description.
#[derive(Debug, PartialEq, Eq)]
pub struct Unspecified(());

impl HandlerDescription for Unspecified {
    fn entry() -> Self {
        Self(())
    }

    fn user_defined() -> Self {
        Self(())
    }

    fn merge_chain(&self, _other: &Self) -> Self {
        Self(())
    }

    fn merge_branch(&self, _other: &Self) -> Self {
        Self(())
    }
}
