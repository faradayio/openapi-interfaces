//! Shared transpiler support.

use std::collections::BTreeMap;

use anyhow::Result;

use super::interface::InterfaceVariant;

/// Current transpilation scope. Contains state that applies to everything
/// within a block, recursively. Think of this in terms of "local variable
/// scope" in a compiler for a regular programming language.
#[non_exhaustive]
pub struct Scope {
    /// The `InterfaceVariant` of the containing interface.
    pub variant: Option<InterfaceVariant>,
}

impl Scope {
    /// Derive a new scope from the current scope, setting the interface
    /// variant.
    pub fn with_variant(&self, variant: InterfaceVariant) -> Scope {
        Self {
            variant: Some(variant),
        }
    }
}

impl Default for Scope {
    /// Create an empty default scope.
    fn default() -> Self {
        Self { variant: None }
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
