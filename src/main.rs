//! `openapi-interfaces`: Generate GET, POST, PUT and Merge Patch versions of an
//! interface.
//!
//! This is an experimental CLI tool.

#![warn(missing_docs, clippy::missing_docs_in_private_items)]

use anyhow::{Context, Result};
use log::{debug, trace};
use std::{fs::File, io, path::PathBuf, process};
use structopt::StructOpt;

mod openapi;
mod parse_error;

use openapi::{OpenApi, Scope, Transpile};

use crate::openapi::included_files::resolve_included_files;

/// Command-line options.
#[derive(Debug, StructOpt)]
#[structopt(about = "Update OpenAPI schema with POST, PUT, etc., specific types")]
struct Opt {
    /// Input file in YAML format.
    input: PathBuf,

    /// Optional output file name. Defaults to standard output.
    #[structopt(short = "o", long = "out-file")]
    output: Option<PathBuf>,

    /// Do not introduce `type: "null"` in the output. This is automatic for
    /// OpenAPI 3.0. This option will result in generic `MergePatch` types.
    ///
    /// Useful for compatibility with readme.com and other OpenAPI 3.0-only
    /// tools.
    #[structopt(long = "avoid-type-null")]
    avoid_type_null: bool,
}

/// Main entry point. Really just a wrapper for `run` which sets up logging and
/// prints errors.
fn main() {
    env_logger::init();

    let opt = Opt::from_args();
    debug!("Command-line options: {:?}source", opt);
    if let Err(err) = run(&opt) {
        eprintln!("ERROR: {}", err);
        let mut next_source = err.source();
        while let Some(source) = next_source {
            eprintln!("  caused by: {}", source);
            next_source = source.source();
        }
        process::exit(1);
    }
}

/// Our real entry point.
fn run(opt: &Opt) -> Result<()> {
    let mut openapi = OpenApi::from_path(&opt.input)?;
    let mut scope = Scope::default();
    if !openapi.supports_type_null() || opt.avoid_type_null {
        // We don't support `type: "null"`, so don't introduce it.
        scope.use_generic_merge_patch_types = true;
    }
    trace!("Parsed: {:#?}", openapi);
    resolve_included_files(&mut openapi, &opt.input)?;
    trace!("With includes: {:#?}", openapi);
    let transpiled = openapi.transpile(&scope)?;
    if let Some(output_path) = &opt.output {
        let mut out = File::create(output_path)
            .with_context(|| format!("could not create {}", output_path.display()))?;
        transpiled.to_writer(&mut out)?;
    } else {
        let out = io::stdout();
        transpiled.to_writer(&mut out.lock())?;
    }
    Ok(())
}
