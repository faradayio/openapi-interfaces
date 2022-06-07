//! Tools to help make `serde` and `serde_yaml` a bit better suited for our
//! purposes.

use serde::{de::DeserializeOwned, Deserializer};

/// Helper function for `serde` defaults. Always returns `true`.
pub fn default_as_true() -> bool {
    true
}

/// Label that we use to identify errors while parsing `enum` types.
static ERROR_LOCATION_DESCRIPTION_LABEL: &str = "error occurred when expecting";

/// Deserialize `yaml` as type `T`.
///
/// This is used to deserialize tagged enums while providing semi-tolerable
/// error messages. For more background, see:
///
/// - https://github.com/faradayio/openapi-interfaces/issues/28 (tracking issue)
/// - https://users.rust-lang.org/t/serde-untagged-enum-ruins-precise-errors/54128
/// - https://github.com/serde-rs/serde/issues/773
/// - https://github.com/serde-rs/serde/pull/1544
/// - https://github.com/dtolnay/serde-yaml/pull/201 (why we can't get line numbers)
pub fn deserialize_enum_helper<'de, D, T>(
    expected: &str,
    yaml: serde_yaml::Value,
) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: DeserializeOwned,
{
    use serde::de::Error;

    match serde_yaml::from_value::<T>(yaml.clone()) {
        Ok(val) => Ok(val),

        Err(err) => {
            // Dump our YAML back to a string and indent it nicely.
            let mut indented_source = "  ".to_string();
            indented_source.push_str(
                &serde_yaml::to_string(&yaml)
                    .expect("error serializing YAML for error")
                    .replace('\n', "\n  "),
            );
            indented_source = indented_source.trim_end().to_owned();
            indented_source.push_str("\n  ---");

            let old_msg = format!("{err}");
            let new_msg = if old_msg.contains(ERROR_LOCATION_DESCRIPTION_LABEL) {
                // We've already annotated this error once.
                if old_msg.contains(&indented_source) {
                    // In fact, we annotated with `indented_source`, vebatim, so
                    // just leave it alone.
                    old_msg
                } else {
                    // We annotated with a different (smaller) source code, so
                    // let's see if adding more context helps.
                    format!("{old_msg}\n...which appeared when expecting {expected} in:\n{indented_source}")
                }
            } else {
                // We haven't annotated this error yet, so let's try to provide some
                // context.
                format!(
                    "{old_msg}\n{ERROR_LOCATION_DESCRIPTION_LABEL} {expected} in:\n{indented_source}"
                )
            };
            Err(D::Error::custom(new_msg))
        }
    }
}
