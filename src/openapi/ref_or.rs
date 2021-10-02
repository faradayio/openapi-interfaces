//! Support for values that might be replaced by `$ref` or `$interface`.

use std::{collections::BTreeMap, fmt::Debug};

use anyhow::format_err;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::Value;

use super::{
    interface::InterfaceVariant, serde_helpers::deserialize_enum_helper, Scope,
    Transpile,
};

/// Support for better error messages when parsing.
pub(crate) trait ExpectedWhenParsing {
    /// A string describing what type of value we expect when parsing a type
    /// that implements this interface.
    fn expected_when_parsing() -> &'static str;
}

/// Either a $ref, an $interface or a value of type T.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum RefOr<T>
where
    T: Clone + Debug + Eq + PartialEq + Serialize,
{
    /// A reference to a component defined elsewhere. When working with OpenAPI,
    /// it is important _not_ to evaluate `$ref`, because OpenAPI code
    /// generators handle `$ref` specially.
    Ref(Ref),
    /// A new-style `$interface` reference. Analogous to `$ref`, but it works
    /// with interface types.
    InterfaceRef(InterfaceRef),
    /// A primitive value.
    Value(T),
}

impl<'de, T> Deserialize<'de> for RefOr<T>
where
    T: Clone
        + Debug
        + DeserializeOwned
        + Eq
        + ExpectedWhenParsing
        + PartialEq
        + Serialize,
{
    // Manually deserialize for slightly better error messages.
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde_yaml::{Mapping, Value};

        // Parse it as raw YAML.
        let yaml = Mapping::deserialize(deserializer)?;

        // Helper to construct YAML hash keys.
        let yaml_str = |s| Value::String(String::from(s));

        // Look for `$includes` or `$ref`, otherwise fall back.
        if yaml.contains_key(&yaml_str("$ref")) {
            Ok(RefOr::Ref(deserialize_enum_helper::<D, _>(
                "$ref schema",
                yaml,
            )?))
        } else if yaml.contains_key(&yaml_str("$interface")) {
            Ok(RefOr::InterfaceRef(deserialize_enum_helper::<D, _>(
                "$interface schema",
                yaml,
            )?))
        } else {
            Ok(RefOr::Value(deserialize_enum_helper::<D, _>(
                &format!(
                    "expected $ref, $interface or {}",
                    <T as ExpectedWhenParsing>::expected_when_parsing(),
                ),
                yaml,
            )?))
        }
    }
}

impl<T> Transpile for RefOr<T>
where
    T: Clone + Debug + Eq + PartialEq + Serialize + Transpile<Output = T>,
{
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        match self {
            RefOr::InterfaceRef(r) => Ok(RefOr::Ref(r.transpile(scope)?)),
            RefOr::Ref(r) => Ok(RefOr::Ref(r.transpile(scope)?)),
            RefOr::Value(val) => Ok(RefOr::Value(val.transpile(scope)?)),
        }
    }
}

/// A `$ref` schema.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Ref {
    /// Path to reference.
    #[serde(rename = "$ref")]
    target: String,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl Transpile for Ref {
    type Output = Self;

    fn transpile(&self, _scope: &Scope) -> anyhow::Result<Self::Output> {
        if !self.unknown_fields.is_empty() {
            return Err(format_err!("`$ref:` must not have any sibling values"));
        }

        Ok(self.clone())
    }
}

/// Our custom `$interface` schema. Analogous to `$ref`, but refers to a specifc
/// variant of an interface.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct InterfaceRef {
    /// Path to reference.
    #[serde(rename = "$interface")]
    target: String,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl Transpile for InterfaceRef {
    /// `InterfaceRef` values transpile to regular `Ref` value.
    type Output = Ref;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        // This type is defined by us, so let's enforce this rule.
        if !self.unknown_fields.is_empty() {
            return Err(format_err!("`$include:` must not have any sibling values"));
        }

        // Figure out which interface variant to use.
        let fragment_pos = self.target.find('#').unwrap_or_else(|| self.target.len());
        let fragment = &self.target[fragment_pos..];
        let variety = if fragment == "#SameAsInterface" {
            // Get the interface variant from the surrounding scope.
            if let Some(variant) = scope.variant {
                variant
            } else {
                return Err(format_err!(
                    "cannot use #SameAsInterface outside of a `components.interfaces` declaration"
                ));
            }
        } else {
            self.target[fragment_pos..].parse::<InterfaceVariant>()?
        };

        // Build our ref.
        Ok(Ref {
            target: format!(
                "#/components/schemas/{}{}",
                &self.target[..fragment_pos],
                variety.to_schema_suffix_str()
            ),
            unknown_fields: BTreeMap::new(),
        })
    }
}
