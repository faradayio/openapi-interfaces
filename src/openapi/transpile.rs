//! Shared transpiler support.

use std::collections::BTreeMap;

use anyhow::Result;

use super::interface::InterfaceVariant;

/// Audience filtering mode.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AudienceFilterMode {
    /// Include only items with matching audiences.
    Include,
    /// Exclude items with matching audiences.
    Exclude,
}

impl Default for AudienceFilterMode {
    fn default() -> Self {
        AudienceFilterMode::Include
    }
}

/// Current transpilation scope. Contains state that applies to everything
/// within a block, recursively. Think of this in terms of "local variable
/// scope" in a compiler for a regular programming language.
#[non_exhaustive]
pub struct Scope {
    /// OpenApi 3.0.X indicated nullable fields via `nullable: true` whereas 3.1
    /// uses `oneOf: { type: "null" }`. This boolean generates the 3.0 syntax.
    pub use_nullable_for_merge_patch: bool,

    /// The `InterfaceVariant` of the containing interface.
    pub variant: Option<InterfaceVariant>,

    /// Audiences to filter by.
    pub filter_audiences: Option<Vec<String>>,

    /// How to filter audiences.
    pub audience_filter_mode: AudienceFilterMode,
}

impl Scope {
    /// Derive a new scope from the current scope, setting the interface
    /// variant.
    pub fn with_variant(&self, variant: InterfaceVariant) -> Scope {
        Self {
            use_nullable_for_merge_patch: self.use_nullable_for_merge_patch,
            variant: Some(variant),
            filter_audiences: self.filter_audiences.clone(),
            audience_filter_mode: self.audience_filter_mode.clone(),
        }
    }

    /// Check if an item should be included based on its audiences.
    /// Returns true if the item should be included, false if it should be filtered out.
    pub fn should_include_item(&self, item_audiences: &Option<Vec<String>>) -> bool {
        // If no filter is configured, include everything
        let Some(ref filter_audiences) = self.filter_audiences else {
            return true;
        };

        // If item has no audiences, it's considered public/default
        let Some(ref audiences) = item_audiences else {
            // Items without audiences are always included
            // - In include mode: they're public/default, so include them
            // - In exclude mode: they don't have the excluded audience, so include them
            return true;
        };

        // Check if there's any overlap between filter audiences and item audiences
        let has_match = audiences.iter().any(|aud| filter_audiences.contains(aud));

        match self.audience_filter_mode {
            AudienceFilterMode::Include => has_match,
            AudienceFilterMode::Exclude => !has_match,
        }
    }
}

impl Default for Scope {
    /// Create an empty default scope.
    fn default() -> Self {
        Self {
            use_nullable_for_merge_patch: false,
            variant: None,
            filter_audiences: None,
            audience_filter_mode: AudienceFilterMode::default(),
        }
    }
}

/// Interface for transpiling a node.
pub trait Transpile {
    /// The output of transpiling this type. For regular OpenAPI types, this
    /// will normally be the same type. For our extension types, it will
    /// typically be a related OpenAPI type.
    type Output: Sized;

    /// Create a transpiled copy of this node.
    fn transpile(&self, scope: &Scope) -> Result<Self::Output>;
}

// Generic transpiler for `Option<T>`.
impl<T> Transpile for Option<T>
where
    T: Transpile,
{
    type Output = Option<<T as Transpile>::Output>;

    fn transpile(&self, scope: &Scope) -> Result<Self::Output> {
        match self {
            Some(val) => Ok(Some(val.transpile(scope)?)),
            None => Ok(None),
        }
    }
}

// Generic transpiler for array slices.
impl<T> Transpile for &'_ [T]
where
    T: Transpile,
{
    type Output = Vec<<T as Transpile>::Output>;

    fn transpile(&self, scope: &Scope) -> Result<Self::Output> {
        let mut result = vec![];
        for elem in self.iter() {
            result.push(elem.transpile(scope)?)
        }
        Ok(result)
    }
}

// Generic transpiler for `Vec`.
impl<T> Transpile for Vec<T>
where
    T: Transpile,
{
    type Output = Vec<<T as Transpile>::Output>;

    fn transpile(&self, scope: &Scope) -> Result<Self::Output> {
        self.as_slice().transpile(scope)
    }
}

// Generic transpiler for simple hashes that don't need special treatment.
impl<K, V> Transpile for BTreeMap<K, V>
where
    K: Clone + Eq + Ord,
    V: Transpile,
{
    type Output = BTreeMap<K, <V as Transpile>::Output>;

    fn transpile(&self, scope: &Scope) -> Result<Self::Output> {
        let mut result = BTreeMap::new();
        for (k, v) in self {
            result.insert(k.to_owned(), v.transpile(scope)?);
        }
        Ok(result)
    }
}
