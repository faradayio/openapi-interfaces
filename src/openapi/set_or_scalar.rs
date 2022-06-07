//! Helper module for (de)serializing values that may be either `T` or `Vec<T>`
//! as a `BTreeSet<T>`.

use std::collections::BTreeSet;

use serde::{
    de::DeserializeOwned, ser::SerializeSeq, Deserialize, Deserializer, Serialize,
    Serializer,
};

use crate::openapi::serde_helpers::deserialize_enum_helper;

use super::ref_or::ExpectedWhenParsing;

/// Deserialize a value of type `T` or `Vec<T>` as a `Vec<T>`.
pub fn deserialize<'de, T, D>(deserializer: D) -> Result<BTreeSet<T>, D::Error>
where
    T: DeserializeOwned + Eq + Ord + ExpectedWhenParsing,
    D: Deserializer<'de>,
{
    let mut set = BTreeSet::new();
    match VecOrScalar::deserialize(deserializer)? {
        VecOrScalar::<T>::Vec(v) => {
            for s in v {
                set.insert(s);
            }
        }
        VecOrScalar::<T>::Scalar(s) => {
            set.insert(s);
        }
    }
    Ok(set)
}

/// Serialize a `BTreeSet<T>` as a `T` if contains exactly one element, or
/// otherwise as sequenca `Vec<e of `T`.
pub fn serialize<T, S>(set: &BTreeSet<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Eq + Ord + Serialize,
    S: Serializer,
{
    if set.len() == 1 {
        let s = set.iter().next().expect("did not contain element");
        s.serialize(serializer)
    } else {
        let mut seq_serializer = serializer.serialize_seq(Some(set.len()))?;
        for s in set {
            seq_serializer.serialize_element(s)?;
        }
        seq_serializer.end()
    }
}

/// Helper type used for deserializing two possibilities using `serde`
/// magic.
enum VecOrScalar<T: DeserializeOwned> {
    /// Try deserializing as a `Vec<T>` first.
    Vec(Vec<T>),
    /// Otherwise this should be a plain `T`.
    Scalar(T),
}

impl<'de, T: ExpectedWhenParsing + DeserializeOwned> Deserialize<'de>
    for VecOrScalar<T>
{
    // Manually deserialize for slightly better error messages. See
    // https://github.com/faradayio/openapi-interfaces/issues/28 for the whole
    // horrifying story.
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde_yaml::Value;

        // Parse it as raw YAML.
        let yaml = Value::deserialize(deserializer)?;

        // If this is a vec, handle it as such.
        if yaml.is_sequence() {
            Ok(VecOrScalar::Vec(deserialize_enum_helper::<D, _>(
                &format!(
                    "array of {}",
                    <T as ExpectedWhenParsing>::expected_when_parsing(),
                ),
                yaml,
            )?))
        } else {
            Ok(VecOrScalar::Scalar(deserialize_enum_helper::<D, _>(
                <T as ExpectedWhenParsing>::expected_when_parsing(),
                yaml,
            )?))
        }
    }
}
