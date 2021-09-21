//! OpenAPI schemas.
//!
//! The types in this file are based on [JSON Schema Specification Draft
//! 2020-12](https://tools.ietf.org/html/draft-bhutton-json-schema-00#section-4.2.1).

use anyhow::format_err;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};

use super::{interface::InterfaceVariant, scalar_or_vec, Scope, Transpile};

/// Different possibilities for a schema.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Schema {
    /// A value must match all of the specified schemas.
    AllOf(AllOf),
    /// A value must match at least one of the specified schemas.
    OneOf(OneOf),
    /// A basic schema containing `type` and additional fields.
    Basic(Box<BasicSchema>),
    /// A schema containing our `$interface` extension. Analogous to `$ref`, but
    /// it works with interface types.
    InterfaceRef(InterfaceRef),
    /// A reference to a schema fragment defined elsewhere. When working with
    /// OpenAPI, it is important _not_ to evaluate `$ref`, because OpenAPI code
    /// generators handle `$ref` specially.
    Ref(Ref),
}

impl Schema {
    /// Construct a simple schema which matches `null`.
    fn null() -> Schema {
        Schema::Basic(Box::new(BasicSchema::null()))
    }

    /// Does this schema allow a null value without resolving `$ref` or
    /// `$interface` links?
    ///
    /// (Most OpenAPI-based code generators rely on not resolving `$ref`, and
    /// `$interface` compiles to `$ref`.)
    fn allows_local_null(&self) -> bool {
        match self {
            Schema::AllOf(all_of) => all_of.schemas.iter().all(|s| s.allows_local_null()),
            Schema::OneOf(one_of) => one_of.schemas.iter().any(|s| s.allows_local_null()),
            Schema::Basic(base) => base.types.contains(&Type::Null),
            Schema::InterfaceRef(_) => false,
            Schema::Ref(_) => false,
        }
    }

    /// Construct a version of this schema that allows `null`, as well as any
    /// other values it might have allowed before.
    pub fn allowing_null(&self) -> Schema {
        match self {
            // We already allow `null` (without following refs), so do nothing.
            schema if schema.allows_local_null() => schema.to_owned(),

            // We have a `BaseSchema`, so just add `null` to our existing `type` list.
            Schema::Basic(base) => {
                let mut base = base.as_ref().to_owned();
                base.types.insert(Type::Null);
                Schema::Basic(Box::new(base))
            }

            // We have a `OneOf` schema, so just add `null` **at the end**.
            Schema::OneOf(one_of) => {
                let mut one_of = one_of.to_owned();
                one_of.schemas.push(Schema::null());
                Schema::OneOf(one_of)
            }

            // We have some other schema type, so we'll need to create a `OneOf` node.
            schema => Schema::OneOf(OneOf {
                schemas: vec![schema.to_owned(), Schema::null()],
                unknown_fields: Default::default(),
            }),
        }
    }
}

impl Transpile for Schema {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        match self {
            Schema::AllOf(all_of) => Ok(Schema::AllOf(all_of.transpile(scope)?)),
            Schema::OneOf(one_of) => Ok(Schema::OneOf(one_of.transpile(scope)?)),
            Schema::Basic(schema) => Ok(Schema::Basic(Box::new(schema.transpile(scope)?))),
            Schema::InterfaceRef(r) => Ok(Schema::Ref(r.transpile(scope)?)),
            Schema::Ref(r) => Ok(Schema::Ref(r.transpile(scope)?)),
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
pub struct BasicSchema {
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

    /// Older OpenAPI way of specifying nullable fields.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) nullable: Option<bool>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    pub(crate) unknown_fields: BTreeMap<String, Value>,
}

impl BasicSchema {
    /// Construct a `BaseSchema` that matches `null`.
    fn null() -> BasicSchema {
        let mut types = BTreeSet::new();
        types.insert(Type::Null);
        BasicSchema {
            types,
            required: Default::default(),
            properties: Default::default(),
            additional_properties: Default::default(),
            nullable: None,
            unknown_fields: Default::default(),
        }
    }
}

impl Transpile for BasicSchema {
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
            nullable: None,
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
