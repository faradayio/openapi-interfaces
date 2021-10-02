//! Tools to help make `serde` and `serde_yaml` a bit better suited for our
//! purposes.

use serde::{de::DeserializeOwned, Deserializer};

/// Helper function for `serde` defaults. Always returns `true`.
pub fn default_as_true() -> bool {
    true
}

/// Deserialize `yaml` as type `T`.
///
/// This is used to deserialize tagged enums while providing semi-tolerable
/// error messages. For more background, see:
///
/// - https://users.rust-lang.org/t/serde-untagged-enum-ruins-precise-errors/54128
/// - https://github.com/serde-rs/serde/issues/773
/// - https://github.com/serde-rs/serde/pull/1544
/// - https://github.com/dtolnay/serde-yaml/pull/201 (why we can't get line numbers)
pub fn deserialize_enum_helper<'de, D, T>(
    expected: &str,
    yaml: serde_yaml::Mapping,
) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: DeserializeOwned,
{
    use serde::de::Error;
    use serde_yaml::Value;

    match serde_yaml::from_value::<T>(Value::Mapping(yaml.clone())) {
        Ok(val) => Ok(val),
        Err(err) => Err(D::Error::custom(format!(
            "expected {}: {}\nerror occurred in:\n{}",
            expected,
            err,
            serde_yaml::to_string(&yaml).expect("error serializing YAML for error"),
        ))),
    }
}
