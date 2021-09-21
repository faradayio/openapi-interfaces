//! `openapi-interfaces`: Generate GET, POST, PUT and Merge Patch versions of an
//! interface.
//!
//! This is an experimental CLI tool.

#![warn(missing_docs, clippy::missing_docs_in_private_items)]

use anyhow::{Context, Result};
use log::debug;
use std::{fs::File, io, path::PathBuf, process};
use structopt::StructOpt;

mod openapi;
mod parse_error;

use openapi::{OpenApi, Scope, Transpile};

/// Command-line options.
#[derive(Debug, StructOpt)]
#[structopt(about = "Update OpenAPI schema with POST, PUT, etc., specific types")]
struct Opt {
    /// Input file in YAML format.
    input: PathBuf,

    /// Optional output file name. Defaults to standard output.
    #[structopt(short = "o", long = "out-file")]
    output: Option<PathBuf>,
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
    let openapi = OpenApi::from_path(&opt.input)?;
    let transpiled = openapi.transpile(&Scope::default())?;
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
