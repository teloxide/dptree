use std::any::TypeId;

use thiserror::Error;

#[derive(Debug, PartialEq, Error)]
pub enum Error {
    #[error(r#"Value of type "{}" not found in the dependencies"#, type_name)]
    ValueNotFound { type_id: TypeId, type_name: &'static str },
}
