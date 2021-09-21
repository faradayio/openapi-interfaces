//! Helper module for (de)serializing values that may be either `T` or `Vec<T>`
//! as a `BTreeSet<T>`.

use std::collections::BTreeSet;

use serde::{ser::SerializeSeq, Deserialize, Deserializer, Serialize, Serializer};

/// Deserialize a value of type `T` or `Vec<T>` as a `Vec<T>`.
pub fn deserialize<'de, T, D>(deserializer: D) -> Result<BTreeSet<T>, D::Error>
where
    T: Deserialize<'de> + Eq + Ord + 'static,
    D: Deserializer<'de>,
{
    /// Helper type used for deserializing two possibilities using `serde`
    /// magic.
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum VecOrScalar<T: 'static> {
        /// Try deserializing as a `Vec<T>` first.
        Vec(Vec<T>),
        /// Otherwise this should be a plain `T`.
        Scalar(T),
    }

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
