//! OpenAPI specification support.
//!
//! We represent the OpenAPI document using a simplified `serde` wrapper. This
//! parses the fields we care about, and stores unknown fields as raw JSON.
//!
//! The types in this file correspond to types in the [OpenAPI 3.1.0
//! specification](https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md).

use anyhow::{format_err, Context, Error, Result};
use log::debug;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use serde_with::{serde_as, DisplayFromStr};
use std::io;
use std::{collections::BTreeMap, fs, path::Path, sync::Arc};

mod interface;
mod ref_or;
mod scalar_or_vec;
mod schema;
mod serde_helpers;
mod transpile;

use crate::parse_error::{Annotation, FileInfo, ParseError};

use self::interface::Interfaces;
use self::ref_or::{ExpectedWhenParsing, RefOr};
use self::schema::Schema;
pub use self::transpile::{Scope, Transpile};

/// An OpenAPI file, with our extensions.
#[serde_as]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct OpenApi {
    /// OpenAPI version number.
    #[serde_as(as = "DisplayFromStr")]
    openapi: Version,

    /// REST path declarations.
    paths: BTreeMap<String, BTreeMap<Method, Operation>>,

    /// Re-usable OpenAPI components.
    components: Components,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl OpenApi {
    /// Read an OpenAPI file from the specified path.
    pub fn from_path(path: &Path) -> Result<Self> {
        let display_path = path.display().to_string();
        let contents = fs::read_to_string(path)
            .with_context(|| format!("error reading {}", display_path))?;

        // Parse our YAML.
        let api =
            serde_yaml::from_str::<OpenApi>(&contents).map_err(|err| -> Error {
                let mut annotations = vec![];
                if let Some(loc) = err.location() {
                    annotations
                        .push(Annotation::primary(loc.index(), "error occurred here"));
                }
                let parse_error = ParseError::new(
                    Arc::new(FileInfo::new(display_path, contents)),
                    annotations,
                    err.to_string(),
                );
                debug!("parse error: {}", parse_error);
                parse_error.into()
            })?;

        // Check our version number.
        let vers_req = VersionReq::parse("^3.0").unwrap();
        if vers_req.matches(&api.openapi) {
            Ok(api)
        } else {
            Err(format_err!("OpenAPI 3.x supported, found {}", &api.openapi))
        }
    }

    /// Can we use `type: "null"` in this OpenAPI file?
    pub fn supports_type_null(&self) -> bool {
        let vers_req = VersionReq::parse("^3.1").unwrap();
        vers_req.matches(&self.openapi)
    }

    /// Write an OpenAPI file to `writer`.
    pub fn to_writer(&self, writer: &mut dyn io::Write) -> Result<()> {
        writeln!(writer, "# AUTOMATICALLY GENERATED. DO NOT EDIT.")?;
        serde_yaml::to_writer(writer, self).map_err(|e| e.into())
    }
}

// Our main transpiler interface. See `trait Transpile` for documentation.
impl Transpile for OpenApi {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> Result<Self> {
        Ok(Self {
            openapi: self.openapi.clone(),
            paths: self.paths.transpile(scope)?,
            components: self.components.transpile(scope)?,
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// Reusable OpenAPI components.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct Components {
    /// Reuable response bodies.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    responses: BTreeMap<String, ResponseBody>,

    /// Re-usable data type definitions.
    #[serde(default)]
    schemas: BTreeMap<String, Schema>,

    /// Higher-level interface definitions (extension).
    #[serde(default, skip_serializing_if = "Interfaces::is_empty")]
    interfaces: Interfaces,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl Transpile for Components {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> Result<Self> {
        let responses = self.responses.transpile(scope)?;

        let mut schemas = self.schemas.transpile(scope)?;
        let interface_schemas = self.interfaces.transpile(scope)?;
        for (name, schema) in interface_schemas {
            if schemas.insert(name.clone(), schema).is_some() {
                return Err(format_err!(
                    "existing schema {:?} would be overwritten by a generated schema",
                    name
                ));
            }
        }

        Ok(Self {
            responses,
            schemas,
            interfaces: Interfaces::default(),
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// HTTP methods.
///
/// Feel free to add more as needed.
#[derive(
    Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize,
)]
#[serde(rename_all = "lowercase")]
#[allow(clippy::missing_docs_in_private_items)]
enum Method {
    Connect,
    Delete,
    Get,
    Head,
    Options,
    Patch,
    Post,
    Put,
    Trace,
}

/// Definitions associated with an HTTP path plus a `Method`.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct Operation {
    /// Our HTTP request body definition.
    #[serde(skip_serializing_if = "Option::is_none")]
    request_body: Option<RequestBody>,

    /// Our HTTP response body definitions (by response code).
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    responses: BTreeMap<u16, RefOr<ResponseBody>>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl Transpile for Operation {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> Result<Self> {
        Ok(Self {
            request_body: self.request_body.transpile(scope)?,
            responses: self.responses.transpile(scope)?,
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

#[test]
fn deserializes_operation_responses_without_refs() {
    let yaml = r#"
responses:
  200:
    description: A list of datasets
    content:
      application/json:
        schema:
          type: array
          items:
            $interface: "Dataset"
"#;
    serde_yaml::from_str::<Operation>(yaml).unwrap();
}

#[test]
fn deserializes_operation_responses_with_refs() {
    let yaml = r##"
responses:
  200:
    $ref: "#/components/responses/Ok"
"##;
    serde_yaml::from_str::<Operation>(yaml).unwrap();
}

/// Specification of an HTTP request body.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct RequestBody {
    /// Allowable content, keyed by MIME type.
    content: BTreeMap<String, MediaType>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl Transpile for RequestBody {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> Result<Self> {
        Ok(Self {
            content: self.content.transpile(scope)?,
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// Specification of an HTTP response body.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct ResponseBody {
    /// Allowable content, keyed by MIME type.
    content: BTreeMap<String, MediaType>,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl ExpectedWhenParsing for ResponseBody {
    fn expected_when_parsing() -> &'static str {
        "a response body definition"
    }
}

impl Transpile for ResponseBody {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> Result<Self> {
        Ok(Self {
            content: self.content.transpile(scope)?,
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

/// Information about a given media type allowed in a request or response body.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct MediaType {
    /// A JSON schema specifying what can appear in this body.
    schema: Schema,

    /// YAML fields we want to pass through blindly.
    #[serde(flatten)]
    unknown_fields: BTreeMap<String, Value>,
}

impl Transpile for MediaType {
    type Output = Self;

    fn transpile(&self, scope: &Scope) -> Result<Self> {
        Ok(Self {
            schema: self.schema.transpile(scope)?,
            unknown_fields: self.unknown_fields.clone(),
        })
    }
}

#[test]
fn parses_example() {
    use pretty_assertions::assert_eq;

    let parsed = OpenApi::from_path(Path::new("./example.yml")).unwrap();
    //println!("{:#?}", parsed);
    let transpiled = parsed.transpile(&Scope::default()).unwrap();
    let expected = OpenApi::from_path(Path::new("./example_output.yml")).unwrap();
    assert_eq!(transpiled, expected);
}

#[test]
fn parses_long_example() {
    use pretty_assertions::assert_eq;

    let parsed =
        OpenApi::from_path(Path::new("./src/openapi/long_example.yml")).unwrap();
    //println!("{:#?}", parsed);
    let transpiled = parsed.transpile(&Scope::default()).unwrap();
    let expected =
        OpenApi::from_path(Path::new("./src/openapi/long_example_output.yml"))
            .unwrap();
    assert_eq!(transpiled, expected);
}
