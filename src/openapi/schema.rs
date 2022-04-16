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
pub trait Nullable: Sized {
    /// Construct a schema which matches only `null`.
    ///
    /// Include documentation fields included that makes the auto-generated
    /// documentation look nice when used in a `MergePatch`.
    fn new_schema_matching_only_null_for_merge_patch() -> Self;

    /// Construct a version of this schema that has documentation suitable for
    /// use inside `oneOf` inside a `MergePatch` type, and return the original
    /// `description` if any.
    fn new_schema_with_merge_patch_documentation(&self) -> (Self, Option<String>);

    /// Construct a version of this schema that allows `null`, as well as any
    /// other values it might have allowed before.
    fn new_schema_matching_current_or_null_for_merge_patch(&self) -> Self;

    /// (Normally internal.) Does this schema allow a null value without
    /// resolving `$ref` or `$interface` links?
    ///
    /// (Most OpenAPI-based code generators rely on not resolving `$ref`, and
    /// `$interface` compiles to `$ref`.)
    fn allows_local_null(&self) -> bool;
}

/// Different possibilities for a schema.
pub type Schema = RefOr<BasicSchema>;

impl Schema {
    /// Does this schema match only an empty object?
    pub fn matches_only_empty_object(&self) -> bool {
        match self {
            RefOr::Ref(_) | RefOr::InterfaceRef(_) => false,
            RefOr::Value(s) => s.matches_only_empty_object(),
        }
    }
}

impl Nullable for Schema {
    fn new_schema_matching_only_null_for_merge_patch() -> Self {
        RefOr::Value(BasicSchema::new_schema_matching_only_null_for_merge_patch())
    }

    fn new_schema_matching_current_or_null_for_merge_patch(&self) -> Schema {
        match self {
            RefOr::Ref(_) | RefOr::InterfaceRef(_) => RefOr::Value(
                BasicSchema::OneOf(OneOf::new_schema_or_null_for_merge_patch(self)),
            ),
            RefOr::Value(val) => {
                RefOr::Value(val.new_schema_matching_current_or_null_for_merge_patch())
            }
        }
    }

    fn new_schema_with_merge_patch_documentation(&self) -> (Self, Option<String>) {
        match self {
            RefOr::Ref(_) | RefOr::InterfaceRef(_) => (self.clone(), None),
            RefOr::Value(val) => {
                let (schema, description) =
                    val.new_schema_with_merge_patch_documentation();
                (RefOr::Value(schema), description)
            }
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

    let schema =
        RefOr::<BasicSchema>::Ref(Ref::new("#/components/schemas/widget", None));
    assert_eq!(
        schema.new_schema_matching_current_or_null_for_merge_patch(),
        RefOr::Value(BasicSchema::OneOf(OneOf {
            schemas: vec![
                schema,
                Schema::new_schema_matching_only_null_for_merge_patch()
            ],
            description: None,
            discriminator: None,
            unknown_fields: Default::default(),
        }))
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
pub enum BasicSchema {
    /// A value must match all of the specified schemas.
    AllOf(AllOf),
    /// A value must match at least one of the specified schemas.
    OneOf(OneOf),
    /// A basic schema containing `type` and additional fields.
    Primitive(Box<PrimitiveSchema>),
}

impl BasicSchema {
    /// Does this schema match only an empty object?
    fn matches_only_empty_object(&self) -> bool {
        match self {
            BasicSchema::AllOf(all_of) => {
                all_of.schemas.iter().any(|s| s.matches_only_empty_object())
            }
            BasicSchema::OneOf(one_of) => {
                one_of.schemas.iter().all(|s| s.matches_only_empty_object())
            }
            BasicSchema::Primitive(s) => s.matches_only_empty_object(),
        }
    }
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
    fn new_schema_matching_only_null_for_merge_patch() -> Self {
        BasicSchema::Primitive(Box::new(PrimitiveSchema::null_for_merge_patch()))
    }

    fn new_schema_with_merge_patch_documentation(&self) -> (Self, Option<String>) {
        match self {
            BasicSchema::AllOf(_) | BasicSchema::OneOf(_) => (self.clone(), None),
            BasicSchema::Primitive(base) => {
                let (base, description) = base.with_merge_patch_documentation();
                (BasicSchema::Primitive(Box::new(base)), description)
            }
        }
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

    fn new_schema_matching_current_or_null_for_merge_patch(&self) -> BasicSchema {
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
                one_of
                    .schemas
                    .push(Schema::new_schema_matching_only_null_for_merge_patch());
                BasicSchema::OneOf(one_of)
            }

            // We have some other schema type, so we'll need to create a `OneOf` node.
            _ => BasicSchema::OneOf(OneOf::new_schema_or_null_for_merge_patch(
                &RefOr::Value(self.to_owned()),
            )),
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
    pub schemas: Vec<Schema>,

    /// Optional description of the schema.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// How to differentiate between our child schemas.
    pub discriminator: Option<Discriminator>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    pub unknown_fields: BTreeMap<String, Value>,
}

impl OneOf {
    /// Create a `oneOf` schema allowing either `schema` or a `null` value, and
    /// set up the `description` and `title` fields on everything in a way that
    /// looks good in a merge patch type.
    fn new_schema_or_null_for_merge_patch(schema: &Schema) -> OneOf {
        let (schema, description) = schema.new_schema_with_merge_patch_documentation();
        let schemas = vec![
            schema,
            Schema::new_schema_matching_only_null_for_merge_patch(),
        ];
        OneOf {
            schemas,
            description,
            discriminator: None,
            unknown_fields: Default::default(),
        }
    }
}

impl Transpile for OneOf {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        Ok(Self {
            schemas: self.schemas.transpile(scope)?,
            description: self.description.clone(),
            discriminator: self.discriminator.clone(),
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// Information about the discriminator for a `OneOf`.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Discriminator {
    /// The property name that distinguishes the types.
    pub property_name: String,

    /// If the values in the field specified by `property_name` do not match the
    /// names of the schemas, you can override them using `mapping`.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub mapping: BTreeMap<String, String>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    pub unknown_fields: BTreeMap<String, Value>,
}

/// A basic JSON Schema fragment.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PrimitiveSchema {
    /// A set of value types which this schema will match.
    #[serde(rename = "type", with = "scalar_or_vec")]
    pub types: BTreeSet<Type>,

    /// For `Type::Object`, a list of properties which must always be present.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub required: Vec<String>,

    /// For `Type::Object`, a list of property schemas.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub properties: BTreeMap<String, Schema>,

    /// A schema describing any additional properties not in `properties`.
    #[serde(default, skip_serializing_if = "AdditionalProperties::is_default")]
    pub additional_properties: AdditionalProperties,

    /// Array item type.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub items: Option<Schema>,

    /// Older OpenAPI way of specifying nullable fields.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub nullable: Option<bool>,

    /// A description of this type.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// A title, typically used to label the choices in a `oneOf`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    /// Example data for this type.
    ///
    /// TODO: We'll need multiple versions for different variants, sadly.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub example: Option<Value>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    pub unknown_fields: BTreeMap<String, Value>,
}

impl PrimitiveSchema {
    /// Does this schema match only an empty object?
    fn matches_only_empty_object(&self) -> bool {
        self.types.contains(&Type::Object)
            && self.types.len() == 1
            && self.properties.is_empty()
            && self.additional_properties == AdditionalProperties::Bool(false)
    }

    /// Construct a `BaseSchema` that matches `null`.
    fn null() -> PrimitiveSchema {
        let mut types = BTreeSet::new();
        types.insert(Type::Null);
        PrimitiveSchema {
            types,
            required: Default::default(),
            properties: Default::default(),
            additional_properties: Default::default(),
            items: Default::default(),
            nullable: None,
            description: Default::default(),
            title: Default::default(),
            example: Default::default(),
            unknown_fields: Default::default(),
        }
    }

    /// Create a version of the schema returned by [`Self::null()`], but with
    /// special `title` and `description` fields for use when we're injecting
    /// `null` support into `MergePatch` types.
    fn null_for_merge_patch() -> Self {
        let mut null_schema = Self::null();
        null_schema.title = Some("Clear".to_owned());
        null_schema.description =
            Some("Pass `null` to clear this field's existing value.".to_owned());
        null_schema
    }

    /// Construct a version of this schema for use in a `oneOf` variant inside
    /// of a `MergePatch`. This involves adding some fields that look nice in
    /// the generated API docs, and changing the description.
    ///
    /// We return the old description.
    fn with_merge_patch_documentation(&self) -> (Self, Option<String>) {
        // "The name of this widget.",
        // "Pass this value to overwrite the existing value.",
        // title: None,
        // title: Some("Overwrite"
        let mut merge_patch_schema = self.clone();
        merge_patch_schema.title = Some("Overwrite".to_owned());
        merge_patch_schema.description =
            Some("Pass this value to overwrite the existing value.".to_owned());
        (merge_patch_schema, self.description.clone())
    }
}

impl Transpile for PrimitiveSchema {
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
            title: self.title.clone(),
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
pub enum AdditionalProperties {
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
