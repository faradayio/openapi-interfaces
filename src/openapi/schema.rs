//! OpenAPI schemas.
//!
//! The types in this file are based on [JSON Schema Specification Draft
//! 2020-12](https://tools.ietf.org/html/draft-bhutton-json-schema-00#section-4.2.1).

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};

use crate::openapi::serde_helpers::deserialize_enum_helper;

use super::{
    ref_or::{ExpectedWhenParsing, RefOr},
    scalar_or_vec, Scope, Transpile,
};

/// Interface for schema types that might be able to match against `null`.
pub(crate) trait Nullable {
    /// Construct a simple schema that only allows `null` values.
    fn null() -> Self;

    /// Construct a version of this schema that allows `null`, as well as any
    /// other values it might have allowed before.
    fn allowing_null(&self) -> Self;

    /// (Normally internal.) Does this schema allow a null value without
    /// resolving `$ref` or `$interface` links?
    ///
    /// (Most OpenAPI-based code generators rely on not resolving `$ref`, and
    /// `$interface` compiles to `$ref`.)
    fn allows_local_null(&self) -> bool;
}

/// Different possibilities for a schema.
pub(crate) type Schema = RefOr<BasicSchema>;

impl Nullable for Schema {
    fn null() -> Self {
        RefOr::Value(BasicSchema::null())
    }

    fn allowing_null(&self) -> Schema {
        match self {
            RefOr::Ref(_) | RefOr::InterfaceRef(_) => {
                RefOr::Value(BasicSchema::OneOf(OneOf::new(vec![
                    self.clone(),
                    Schema::null(),
                ])))
            }
            RefOr::Value(val) => RefOr::Value(val.allowing_null()),
        }
    }

    fn allows_local_null(&self) -> bool {
        match self {
            RefOr::Ref(_) | RefOr::InterfaceRef(_) => false,
            RefOr::Value(value) => value.allows_local_null(),
        }
    }
}

#[test]
fn allowing_null_turns_refs_into_oneof() {
    use super::ref_or::Ref;

    let schema = RefOr::<BasicSchema>::Ref(Ref::new("#/components/schemas/widget"));
    assert_eq!(
        schema.allowing_null(),
        RefOr::Value(BasicSchema::OneOf(OneOf::new(
            vec![schema, Schema::null(),]
        )))
    )
}

#[test]
fn deserializes_array_schema() {
    let yaml = r#"
type: array
items:
  $interface: "Dataset"
"#;
    serde_yaml::from_str::<Schema>(yaml).unwrap();
}

/// Different possibilities for a schema.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum BasicSchema {
    /// A value must match all of the specified schemas.
    AllOf(AllOf),
    /// A value must match at least one of the specified schemas.
    OneOf(OneOf),
    /// A basic schema containing `type` and additional fields.
    Primitive(Box<PrimativeSchema>),
}

impl<'de> Deserialize<'de> for BasicSchema {
    // Manually deserialize for slightly better error messages.
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        use serde_yaml::{Mapping, Value};

        // Parse it as raw YAML.
        let yaml = Mapping::deserialize(deserializer)?;

        // Helper to construct YAML hash keys.
        let yaml_str = |s| Value::String(String::from(s));

        // Look for `$includes`.
        if yaml.contains_key(&yaml_str("allOf")) {
            Ok(BasicSchema::AllOf(deserialize_enum_helper::<D, _>(
                "allOf schema",
                yaml,
            )?))
        } else if yaml.contains_key(&yaml_str("oneOf")) {
            Ok(BasicSchema::OneOf(deserialize_enum_helper::<D, _>(
                "oneOf schema",
                yaml,
            )?))
        } else if yaml.contains_key(&yaml_str("type")) {
            Ok(BasicSchema::Primitive(deserialize_enum_helper::<D, _>(
                "schema", yaml,
            )?))
        } else {
            Err(D::Error::custom(format!(
                "one of allOf, oneOf, or type in:\n{}",
                serde_yaml::to_string(&yaml).expect("error serializing YAML")
            )))
        }
    }
}

impl ExpectedWhenParsing for BasicSchema {
    fn expected_when_parsing() -> &'static str {
        "a schema with one of allOf, oneOf, or type"
    }
}

impl Nullable for BasicSchema {
    fn null() -> BasicSchema {
        BasicSchema::Primitive(Box::new(PrimativeSchema::null()))
    }

    fn allows_local_null(&self) -> bool {
        match self {
            BasicSchema::AllOf(all_of) => {
                all_of.schemas.iter().all(|s| s.allows_local_null())
            }
            BasicSchema::OneOf(one_of) => {
                one_of.schemas.iter().any(|s| s.allows_local_null())
            }
            BasicSchema::Primitive(base) => base.types.contains(&Type::Null),
        }
    }

    fn allowing_null(&self) -> BasicSchema {
        match self {
            // We already allow `null` (without following refs), so do nothing.
            schema if schema.allows_local_null() => schema.to_owned(),

            // We have a `BaseSchema`, so we can just add `null` to our existing
            // `type` list.
            //
            // However, `openapi-typescript` (which we care about) does not
            // currently support a list for `types`, so we're careful not to
            // introduce an extra element if we have exactly one.
            BasicSchema::Primitive(base) if base.types.len() != 1 => {
                let mut base = base.as_ref().to_owned();
                base.types.insert(Type::Null);
                BasicSchema::Primitive(Box::new(base))
            }

            // We have a `OneOf` schema, so just add `null` **at the end**.
            BasicSchema::OneOf(one_of) => {
                let mut one_of = one_of.to_owned();
                one_of.schemas.push(Schema::null());
                BasicSchema::OneOf(one_of)
            }

            // We have some other schema type, so we'll need to create a `OneOf` node.
            schema => BasicSchema::OneOf(OneOf::new(vec![
                RefOr::Value(schema.to_owned()),
                Schema::null(),
            ])),
        }
    }
}

impl Transpile for BasicSchema {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        match self {
            BasicSchema::AllOf(all_of) => {
                Ok(BasicSchema::AllOf(all_of.transpile(scope)?))
            }
            BasicSchema::OneOf(one_of) => {
                Ok(BasicSchema::OneOf(one_of.transpile(scope)?))
            }
            BasicSchema::Primitive(schema) => {
                Ok(BasicSchema::Primitive(Box::new(schema.transpile(scope)?)))
            }
        }
    }
}

/// An `allOf` schema.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AllOf {
    /// Our child schemas.
    #[serde(rename = "allOf")]
    schemas: Vec<Schema>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl Transpile for AllOf {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        Ok(Self {
            schemas: self.schemas.transpile(scope)?,
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// A `oneOf` schema.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct OneOf {
    /// Our child schemas.
    #[serde(rename = "oneOf")]
    schemas: Vec<Schema>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl OneOf {
    /// Create a new `OneOf` schema.
    fn new(schemas: Vec<Schema>) -> OneOf {
        OneOf {
            schemas,
            unknown_fields: Default::default(),
        }
    }
}

impl Transpile for OneOf {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        Ok(Self {
            schemas: self.schemas.transpile(scope)?,
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// A basic JSON Schema fragment.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PrimativeSchema {
    /// A set of value types which this schema will match.
    #[serde(rename = "type", with = "scalar_or_vec")]
    pub(crate) types: BTreeSet<Type>,

    /// For `Type::Object`, a list of properties which must always be present.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) required: Vec<String>,

    /// For `Type::Object`, a list of property schemas.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(crate) properties: BTreeMap<String, Schema>,

    /// A schema describing any additional properties not in `properties`.
    #[serde(default, skip_serializing_if = "AdditionalProperties::is_default")]
    pub(crate) additional_properties: AdditionalProperties,

    /// Array item type.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) items: Option<Schema>,

    /// Older OpenAPI way of specifying nullable fields.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) nullable: Option<bool>,

    /// A description of this type.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) description: Option<String>,

    /// Example data for this type.
    ///
    /// TODO: We'll need multiple versions for different variants, sadly.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) example: Option<Value>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    pub(crate) unknown_fields: BTreeMap<String, Value>,
}

impl PrimativeSchema {
    /// Construct a `BaseSchema` that matches `null`.
    fn null() -> PrimativeSchema {
        let mut types = BTreeSet::new();
        types.insert(Type::Null);
        PrimativeSchema {
            types,
            required: Default::default(),
            properties: Default::default(),
            additional_properties: Default::default(),
            items: Default::default(),
            nullable: None,
            description: Default::default(),
            example: Default::default(),
            unknown_fields: Default::default(),
        }
    }
}

impl Transpile for PrimativeSchema {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        // Fix old-style nullability.
        let mut types = self.types.clone();
        if self.nullable == Some(true) {
            types.insert(Type::Null);
        }

        Ok(Self {
            types,
            required: self.required.clone(),
            properties: self.properties.transpile(scope)?,
            additional_properties: self.additional_properties.transpile(scope)?,
            items: self.items.transpile(scope)?,
            nullable: None,
            description: self.description.clone(),
            example: self.example.clone(),
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// Primitive JSON types. These serialize as lowercase strings, as per the JSON
/// Schema conventions.
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(rename_all = "lowercase")]
#[allow(clippy::missing_docs_in_private_items)]
pub enum Type {
    String,
    Number,
    Integer,
    Object,
    Array,
    Boolean,
    Null,
}

/// An `additionalProperties` value.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum AdditionalProperties {
    /// `true` (allowing any property) or `false` (allowing none).
    Bool(bool),
    /// All unknown property values must match the specified schema.
    Schema(Schema),
}

impl AdditionalProperties {
    /// Is this the default value for `additionalProperties`?
    fn is_default(&self) -> bool {
        matches!(self, &AdditionalProperties::Bool(true))
    }
}

impl Default for AdditionalProperties {
    fn default() -> Self {
        AdditionalProperties::Bool(true)
    }
}

impl Transpile for AdditionalProperties {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        match self {
            AdditionalProperties::Bool(b) => Ok(AdditionalProperties::Bool(*b)),
            AdditionalProperties::Schema(s) => {
                Ok(AdditionalProperties::Schema(s.transpile(scope)?))
            }
        }
    }
}
