//! Keeping track of multiple files.
//!
use std::{
    collections::{BTreeMap, BTreeSet},
    env::current_dir,
    path::{Path, PathBuf},
};

use anyhow::{format_err, Context};
use log::debug;

use super::OpenApi;
use crate::Result;

/// Resolve all `$includedFiles` that appear in `base`, recursively.
pub fn resolve_included_files(base: &mut OpenApi, base_path: &Path) -> Result<()> {
    let mut included = IncludedFiles::default();
    included.process_includes(base, base_path)?;
    included.merge_into_base(base, base_path)
}

/// All the OpenAPI files mentioned in any `$includedFiles` lists.
#[derive(Debug, Default)]
struct IncludedFiles {
    /// Mapping from path names to parsed files.
    ///
    /// Pathnames should be relative to the current working directory, not the
    /// file they originally appeared in.
    included_files: BTreeMap<PathBuf, Option<OpenApi>>,
}

impl IncludedFiles {
    /// Process all includes in `from` recursively, using `base_path` as
    /// the base for all the paths.
    fn process_includes(&mut self, base: &OpenApi, base_path: &Path) -> Result<()> {
        for path in &base.include_files {
            // Construct a relative path (which may include messy things like
            // "..") and make sure it points to a real file.
            let messy_relative_path = base_path
                .parent()
                .ok_or_else(|| {
                    format_err!(
                        "cannot find parent directory of {}",
                        base_path.display()
                    )
                })?
                .join(path);
            if !&messy_relative_path.exists() {
                return Err(format_err!(
                    "cannot find file {}",
                    messy_relative_path.display()
                ));
            }

            // First construct an absolute path pointing to our input file, so
            // that tricky mixes of symlinks and "../" path segments don't
            // confuse us into include the same file using two equivalent
            // relative paths.
            let absolute_path = messy_relative_path
                // Make an absolute path, getting rid of symlinks, etc.
                .canonicalize()
                .context("cannot determine absolute path")?;

            // Then make the path relative to the `cwd` if we can, just to be
            // nice.
            let cwd = current_dir().context("can't get current directory")?;
            let cwd_relative_path = absolute_path
                // Try to make it relative.
                .strip_prefix(cwd)
                // If we succeed, make sure we own it, so we drop all borrows of
                // `abs_path`.
                .map(|p| p.to_owned())
                // If we failed, just use `abs_path`.
                .unwrap_or(absolute_path);

            self.include_file(cwd_relative_path)?;
        }
        Ok(())
    }

    /// Include a new file.
    fn include_file(&mut self, normalized_path: PathBuf) -> Result<()> {
        match self.included_files.get(&normalized_path) {
            Some(None) => Err(format_err!(
                "tried to load {} while we were already loading it, do you have circular includes?",
                normalized_path.display()
            )),
            Some(Some(_)) => {
                debug!("already included {}, skipping", normalized_path.display());
                Ok(())
            },
            None => {
                // Indicate that we know about this file but haven't finished
                // loading it yet.
                self.included_files.insert(normalized_path.to_owned(), None);

                // Load the file.
                let parsed = OpenApi::from_path(&normalized_path)
                    .with_context(|| {
                        format!("error while trying to load {}", normalized_path.display())
                    })?;

                // Process any includes recusively.
                self.process_includes(&parsed, &normalized_path)?;

                // Store our parsed file
                self.included_files.insert(normalized_path, Some(parsed));
                Ok(())
            },
        }
    }

    /// Take all the files we know about, and merge them into `base`.
    ///
    /// The is a very conservative function. It doesn't do any of the funky JSON
    /// Merge Path stuff supported by `$include` (because doing that across
    /// files would be confusing). And it errors on duplicate definitions and
    /// conflicts.
    fn merge_into_base(&self, base: &mut OpenApi, base_path: &Path) -> Result<()> {
        for (included_path, included) in &self.included_files {
            let included = included.as_ref().ok_or_else(|| {
                format_err!(
                    "failed to finishing loading {}, so we can't merge it",
                    included_path.display()
                )
            })?;

            // Merge `paths`.
            for (path, methods) in &included.paths {
                if base
                    .paths
                    .insert(path.to_owned(), methods.to_owned())
                    .is_some()
                {
                    return Err(format_err!(
                        "duplicate definitions of `paths[{:?}]` in {} and {}",
                        path,
                        base_path.display(),
                        included_path.display()
                    ));
                }
            }

            // Merge `components.schemas`.
            for (name, schema) in &included.components.schemas {
                if base
                    .components
                    .schemas
                    .insert(name.to_owned(), schema.to_owned())
                    .is_some()
                {
                    return Err(format_err!(
                        "duplicate definitions of `components.schemas.{}` in {} and {}",
                        name,
                        base_path.display(),
                        included_path.display()
                    ));
                }
            }

            // Merge `components.interfaces`.
            for (name, iface) in &*included.components.interfaces {
                if base
                    .components
                    .interfaces
                    .insert(name.to_owned(), iface.to_owned())
                    .is_some()
                {
                    return Err(format_err!(
                        "duplicate definitions of `components.interfaces.{}` in {} and {}",
                        name,
                        base_path.display(),
                        included_path.display()
                    ));
                }
            }

            // Fail if there are any other values we don't know how to merge.
            if !included.unknown_fields.is_empty() {
                let keys = included
                    .unknown_fields
                    .keys()
                    // Avoid generating code just for `BTreeSet<&String>`.
                    .cloned()
                    .collect::<BTreeSet<String>>();
                return Err(format_err!(
                    "cannot specify {:?} in included file {} (put it in {} instead)",
                    keys,
                    included_path.display(),
                    base_path.display(),
                ));
            }
            if !included.components.unknown_fields.is_empty() {
                let keys = included
                    .components
                    .unknown_fields
                    .keys()
                    .map(|k| format!("components.{}", k))
                    .collect::<BTreeSet<String>>();
                return Err(format_err!(
                    "cannot specify {:?} in included file {} (put it in {} instead)",
                    keys,
                    included_path.display(),
                    base_path.display(),
                ));
            }
        }

        // And remove our inclusion declarations, now that we no longer need
        // them.
        base.include_files.clear();
        Ok(())
    }
}
