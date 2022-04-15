//! Custom interface type used to extend OpenAPI.

use std::{
    collections::{BTreeMap, BTreeSet},
    str::FromStr,
};

use anyhow::{format_err, Context, Result};
use json_patch::merge as json_merge_patch;
use log::warn;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use topological_sort::TopologicalSort;

use crate::openapi::{
    ref_or::RefOr,
    schema::{AdditionalProperties, BasicSchema, PrimitiveSchema, Type},
    serde_helpers::{default_as_true, deserialize_enum_helper},
};

use super::{
    ref_or::transpile_interface_ref_to_ref,
    schema::{Discriminator, Nullable, OneOf, Schema},
    Scope, Transpile,
};

/// Which version of an interface are we working with?
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InterfaceVariant {
    /// The version of the interface returned from the server (by `GET` for
    /// example).
    Get,
    /// The version of the interface submitted to the server to create a new
    /// object.
    Post,
    /// The version of the interface submitted to update an object in place,
    /// overwriting all settable fields. Normally you should prefer
    /// `MergePatch`. The `Put` type is also the _base_ into which `MergePatch`
    /// is applied.
    Put,
    /// A JSON Merge Patch (RFC 7396)-compatible schema that can be used to
    /// update an existing resource using `PATCH`.
    MergePatch,
}

/// All interface variants.
const INTERFACE_VARIANTS: &[InterfaceVariant] = &[
    InterfaceVariant::Get,
    InterfaceVariant::Post,
    InterfaceVariant::Put,
    InterfaceVariant::MergePatch,
];

impl InterfaceVariant {
    /// The URL-style "fragment" string we'd use to specify this interface.
    pub fn to_fragment_str(self) -> &'static str {
        match self {
            InterfaceVariant::Get => "",
            InterfaceVariant::Post => "#Post",
            InterfaceVariant::Put => "#Put",
            InterfaceVariant::MergePatch => "#MergePatch",
        }
    }

    /// A suffix which we can append to an interface name to get an appropriate
    /// schema name.
    pub fn to_schema_suffix_str(self) -> &'static str {
        let s = self.to_fragment_str();
        if s.is_empty() {
            s
        } else {
            assert!(s.starts_with('#'));
            &s[1..]
        }
    }
}

impl FromStr for InterfaceVariant {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "" => Ok(InterfaceVariant::Get),
            "#Post" => Ok(InterfaceVariant::Post),
            "#Put" => Ok(InterfaceVariant::Put),
            "#MergePatch" => Ok(InterfaceVariant::MergePatch),
            _ => Err(format_err!("unknown interface variety: {:?}", s)),
        }
    }
}

/// Our new `components.interfaces` section of the file.
///
/// We give this its own type so that we can provide a custom `Transpile`
/// implementation.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(transparent)]
pub struct Interfaces(BTreeMap<String, Interface>);

impl Interfaces {
    /// Is the `interfaces` section empty?
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Transpile for Interfaces {
    type Output = BTreeMap<String, Schema>;

    fn transpile(&self, scope: &Scope) -> anyhow::Result<Self::Output> {
        // Use `TopologicalSort` to sort our interfaces so that included
        // interfaces come before interfaces that include them.
        let mut sort = TopologicalSort::<&str>::new();
        for (name, interface) in &self.0 {
            if let Interface::Includes(inclusion) = interface {
                if !self.0.contains_key(&inclusion.base) {
                    return Err(format_err!(
                        "interface {:?} includes {:?}, but that interface isn't defined",
                        name,
                        inclusion.base,
                    ));
                }
                sort.add_dependency(inclusion.base.as_str(), name.as_str());
            } else {
                sort.insert(name.as_str());
            }
        }

        // Expand `$includes` using JSON Merge Patch.
        let mut expanded: BTreeMap<&str, BasicInterface> = BTreeMap::new();
        let mut to_generate: BTreeMap<&str, Box<dyn GenerateSchemaVariant>> =
            BTreeMap::new();
        for name in sort {
            let interface = self
                .0
                .get(name)
                .expect("interface should always be in hash table");
            match interface {
                Interface::Includes(inclusion) => {
                    let mut doc =
                        serde_json::to_value(expanded.get(inclusion.base.as_str()))?;
                    let patch = Value::Object(inclusion.merge_patch.clone());
                    json_merge_patch(&mut doc, &patch);
                    let mut reparsed = serde_json::from_value::<BasicInterface>(doc)
                        .with_context(|| {
                        format!("error parsing merged {:?}", name)
                    })?;
                    reparsed.emit = inclusion.emit; // This is never merged.
                    expanded.insert(name, reparsed.clone());
                    to_generate.insert(name, Box::new(reparsed));
                }
                Interface::Basic(base) => {
                    expanded.insert(name, base.clone());
                    to_generate.insert(name, Box::new(base.clone()));
                }
                Interface::OneOf(one_of) => {
                    to_generate.insert(name, Box::new(one_of.clone()));
                }
            }
        }

        // Generate schemas for all variants of all interfaces unless indicated
        // otherwise.
        let mut schemas = BTreeMap::new();
        for (name, interface) in to_generate {
            if !interface.should_emit() {
                continue;
            }
            for variant in INTERFACE_VARIANTS.iter().cloned() {
                let schema_name = interface.schema_variant_name(name, variant);
                let schema =
                    interface.generate_schema_variant(scope, name, variant)?;

                if schema.matches_only_empty_object() {
                    warn!(
                        "output schema {} would match only empty objects, skipping",
                        schema_name
                    );
                    continue;
                }

                if schemas.insert(schema_name.clone(), schema).is_some() {
                    return Err(format_err!(
                        "generated multiple schemas named {:?}",
                        &schema_name
                    ));
                }
            }
        }
        Ok(schemas)
    }
}

/// Custom interface type.
///
/// This is our main extension to OpenAPI. It allows specifying an object schema
/// in way that's less "validation-like" and more "type-like".
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Interface {
    /// An interface that `$includes` another interface. We can't parse this
    /// until the inclusion has been computed.
    Includes(IncludesInterface),
    /// A fully-resolved interface definition.
    Basic(BasicInterface),
    /// An type-union interface. This exists so it can use `$interface:
    /// ...#SameAsInterface` in a type union.
    OneOf(OneOfInterface),
}

impl<'de> Deserialize<'de> for Interface {
    // Manually deserialize for slightly better error messages.
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde_yaml::{Mapping, Value};

        // Parse it as raw YAML.
        let yaml = Mapping::deserialize(deserializer)?;

        // Look for `$includes`.
        let includes_key = Value::String(String::from("$includes"));
        let oneof_key = Value::String(String::from("oneOf"));
        if yaml.contains_key(&includes_key) {
            Ok(Interface::Includes(deserialize_enum_helper::<D, _>(
                "`$includes` interface",
                yaml,
            )?))
        } else if yaml.contains_key(&oneof_key) {
            Ok(Interface::OneOf(deserialize_enum_helper::<D, _>(
                "oneOf interface",
                yaml,
            )?))
        } else {
            Ok(Interface::Basic(deserialize_enum_helper::<D, _>(
                "interface",
                yaml,
            )?))
        }
    }
}

/// Methods used to compile interfaces into multiple schemas.
trait GenerateSchemaVariant {
    /// Should be emit this schema in our output file?
    fn should_emit(&self) -> bool {
        true
    }

    /// What name should we use for the specified variant of this schema?
    fn schema_variant_name(&self, name: &str, variant: InterfaceVariant) -> String {
        format!("{}{}", name, variant.to_schema_suffix_str())
    }

    /// Generate a specific schema variant from this interface.
    fn generate_schema_variant(
        &self,
        scope: &Scope,
        name: &str,
        variant: InterfaceVariant,
    ) -> Result<Schema>;
}

/// An interface which `$includes` another.
///
/// This
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IncludesInterface {
    /// The base interface we include.
    #[serde(rename = "$includes")]
    base: String,

    /// Should we include this interface in the generated output?
    ///
    /// We don't allow this to be included in `merge_patch`, because it's local
    /// to this specific interface and will not participate in the merge.
    ///
    /// TODO: We're going to work some more on the design of this, perhaps to
    /// allow emitting only specific variants of an interface.
    #[serde(default = "default_as_true")]
    emit: bool,

    /// Unparsed JSON data for the interface. We can't parse this yet, because
    /// this is actually a JSON Merge Patch over the interface referred to by
    /// `$includes`. We'll finishing parsing it after merging.
    #[serde(flatten)]
    merge_patch: Map<String, Value>,
}

/// A basic interface, fully merged.
//
/// This is roughly analogous to a schema definition, except that it has
/// multiple versions, one per `InterfaceVariant`. Also, unlike OpenAPI/JSON
/// schemas, this really _is_ an interface schema, and not actually a set of
/// complex validation rules.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct BasicInterface {
    /// Should we include this interface in the generated output?
    ///
    /// TODO: We're going to work some more on the design of this, perhaps to
    /// allow emitting only specific variants of an interface.
    #[serde(default = "default_as_true")]
    emit: bool,

    /// Members of this interface.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    members: BTreeMap<String, Member>,

    /// Analogous to `additionalProperties` in JSON Schema, except it allows you
    /// to define "members" (see above), not just properties. Note that unlike
    /// JSON Schema, if you don't include this, additional members will _not_ be
    /// allowed by default.
    ///
    /// This is because we want to be able to detect mispelled properties and
    /// report them as errors, instead of silently ignoring them the way OpenAPI
    /// schemas do by default. You can always explicitly declare an
    /// `additionalMembers` if you want to ignore unknown members.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    additional_members: Option<Member>,

    /// A description of this type.
    #[serde(default)]
    description: Option<String>,

    /// Example data for this type.
    ///
    /// TODO: We'll need multiple versions for different variants, sadly.
    #[serde(default)]
    example: Option<Value>,
}

impl GenerateSchemaVariant for BasicInterface {
    fn should_emit(&self) -> bool {
        self.emit
    }

    /// Generate a specific schema variant from this interface.
    fn generate_schema_variant(
        &self,
        scope: &Scope,
        name: &str,
        variant: InterfaceVariant,
    ) -> Result<Schema> {
        // We always have type "object".
        let mut types = BTreeSet::new();
        types.insert(Type::Object);

        // Check to see whether we should just generate a placeholder.
        if scope.use_generic_merge_patch_types
            && variant == InterfaceVariant::MergePatch
        {
            let schema = PrimitiveSchema {
                types,
                required: vec![],
                properties: Default::default(),
                additional_properties: AdditionalProperties::Bool(true),
                items: None,
                nullable: None,
                description: Some(format!(
                    "A patch to `{}Put` in JSON Merge Patch format (RFC 7396).",
                    name
                )),
                title: None,
                example: None,
                unknown_fields: BTreeMap::default(),
            };
            return Ok(RefOr::Value(BasicSchema::Primitive(Box::new(schema))));
        }

        // Build our properties.
        let mut required = vec![];
        let mut properties = BTreeMap::new();
        for (name, member) in &self.members {
            if let Some(schema) = member.schema_for(scope, variant)? {
                properties.insert(name.to_owned(), schema);
                if member.is_required_for(variant) {
                    required.push(name.to_owned());
                }
            }
        }

        // Build our "additional properties" field, if we have one.
        let additional_properties = match &self.additional_members {
            Some(additional_members) if additional_members.required => {
                return Err(format_err!(
                    "cannot use `required` with `additional_members` in {}",
                    name,
                ));
            }
            Some(additional_members) => {
                if let Some(schema) = additional_members.schema_for(scope, variant)? {
                    AdditionalProperties::Schema(schema)
                } else {
                    AdditionalProperties::Bool(false)
                }
            }
            // This may be controversial, but since we're generating lovely
            // schemas, we should disallow anything which doesn't appear in the
            // interface, and which doesn't have a type specified.
            None => AdditionalProperties::Bool(false),
        };

        // TODO: Only include the description on the base type for now.
        let description = if variant == InterfaceVariant::Get {
            self.description.clone()
        } else {
            None
        };

        // TODO: Only include the example on the POST type now. We **will**
        // break this.
        let example = if variant == InterfaceVariant::Post {
            self.example.clone()
        } else {
            None
        };

        // Build a schema for this interface.
        let schema = PrimitiveSchema {
            types,
            required,
            properties,
            additional_properties,
            items: None,
            nullable: None,
            description,
            title: None,
            example,
            unknown_fields: BTreeMap::default(),
        };
        Ok(RefOr::Value(BasicSchema::Primitive(Box::new(schema))))
    }
}

#[test]
fn generates_generic_merge_patch_types_when_necessary() {
    let mut members = BTreeMap::new();
    members.insert(
        "field".to_owned(),
        Member {
            required: false,
            mutable: true,
            initializable: None,
            // Literally any schema would work here.
            schema: Schema::new_schema_matching_only_null_for_merge_patch(),
        },
    );
    let iface = BasicInterface {
        emit: true,
        members,
        additional_members: None,
        description: None,
        example: None,
    };

    let scope = Scope {
        use_generic_merge_patch_types: true,
        ..Scope::default()
    };
    let generated = iface
        .generate_schema_variant(&scope, "Example", InterfaceVariant::MergePatch)
        .unwrap();

    let expected_yaml = r#"
type: object
description: A patch to `ExamplePut` in JSON Merge Patch format (RFC 7396).
"#;
    let expected = serde_yaml::from_str(expected_yaml).unwrap();
    assert_eq!(generated, expected);
}

/// A member of an interface. Analogous to a property, but with more metadata
/// and a few restrictions.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Member {
    /// Is this member normally required to be present?
    #[serde(default)]
    required: bool,

    /// Is this member mutable once the resource has been created?
    #[serde(default)]
    mutable: bool,

    /// Can this resource be set at initialization time? If not specified,
    /// we'll use the value `mutable` instead.
    #[serde(default)]
    initializable: Option<bool>,

    /// A regular JSON Schema for this member.
    schema: Schema,
}

impl Member {
    /// Is this member settable at initialization time?
    fn is_initializable(&self) -> bool {
        self.initializable.unwrap_or(self.mutable)
    }

    /// Should this member be marked as `required` in this variant?
    fn is_required_for(&self, variant: InterfaceVariant) -> bool {
        match variant {
            InterfaceVariant::Get => self.required,
            InterfaceVariant::Post => self.required && self.is_initializable(),
            InterfaceVariant::Put => self.required && self.mutable,
            InterfaceVariant::MergePatch => false,
        }
    }

    /// A JSON Schema to use for this property when generating the specified
    /// variant. Returns `None` if this property is not available in the
    /// specified variant.
    fn schema_for(
        &self,
        scope: &Scope,
        variant: InterfaceVariant,
    ) -> Result<Option<Schema>> {
        let scope = scope.with_variant(variant);
        Ok(match variant {
            InterfaceVariant::Get => Some(self.schema.transpile(&scope)?),
            InterfaceVariant::Post if self.is_initializable() => {
                Some(self.schema.transpile(&scope)?)
            }
            InterfaceVariant::Post => None,
            InterfaceVariant::Put if self.mutable => {
                Some(self.schema.transpile(&scope)?)
            }
            InterfaceVariant::Put => None,
            InterfaceVariant::MergePatch if self.mutable => {
                let schema = self.schema.transpile(&scope)?;
                if self.required {
                    // Required fields become optional, but we don't go out of
                    // our way to allow `null` in a `MergePatch` variant,
                    // because that would cause a required field to be removed
                    // when merging.
                    Some(schema)
                } else {
                    // Optional fields become nullable.
                    Some(schema.new_schema_matching_current_or_null_for_merge_patch())
                }
            }
            InterfaceVariant::MergePatch => None,
        })
    }
}

/// An interface which specifies a discriminated type union.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct OneOfInterface {
    /// A description of this type.
    #[serde(default)]
    description: Option<String>,

    /// Allowable types that can be used for this interface.
    one_of: Vec<Schema>,

    /// How to tell the allowable types apart.
    discriminator: InterfaceDiscriminator,
}

impl GenerateSchemaVariant for OneOfInterface {
    fn generate_schema_variant(
        &self,
        scope: &Scope,
        _name: &str,
        variant: InterfaceVariant,
    ) -> Result<Schema> {
        let scope = scope.with_variant(variant);

        let schemas = self
            .one_of
            .iter()
            .map(|schema| schema.transpile(&scope))
            .collect::<Result<Vec<_>>>()?;

        let discriminator = Some(self.discriminator.transpile(&scope)?);

        Ok(Schema::Value(BasicSchema::OneOf(OneOf {
            schemas,
            description: None,
            discriminator,
            unknown_fields: Default::default(),
        })))
    }
}

/// Information about the discriminator for a `OneOfInterface`.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct InterfaceDiscriminator {
    /// The property name that distinguishes the types.
    property_name: String,

    /// If the values in the field specified by `property_name` do not match the
    /// names of the schemas, you can override them using `mapping`.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    mapping: BTreeMap<String, String>,
}

impl Transpile for InterfaceDiscriminator {
    type Output = Discriminator;

    fn transpile(&self, scope: &Scope) -> Result<Self::Output> {
        // Convert a map like:
        //
        //     "square": "SquareShapeOptions#SameAsInterface"
        //
        // To a map like:
        //
        //     "square": "#/components/schemas/SquareShapeOptions"
        let mut mapping = BTreeMap::new();
        for (property_value, interface_ref) in self.mapping.iter() {
            mapping.insert(
                property_value.clone(),
                transpile_interface_ref_to_ref(interface_ref, scope)?,
            );
        }

        Ok(Discriminator {
            property_name: self.property_name.clone(),
            mapping,
            unknown_fields: Default::default(),
        })
    }
}

#[test]
fn parses_one_of_example() {
    use crate::openapi::OpenApi;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    let parsed =
        OpenApi::from_path(Path::new("./examples/oneof_example.yml")).unwrap();
    //println!("{:#?}", parsed);
    let transpiled = parsed.transpile(&Scope::default()).unwrap();
    println!("{}", serde_yaml::to_string(&transpiled).unwrap());
    let expected =
        OpenApi::from_path(Path::new("./examples/oneof_example_output.yml")).unwrap();
    assert_eq!(transpiled, expected);
}
