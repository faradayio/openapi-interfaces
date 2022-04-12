# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.2.1 - 2022-04-12

### Added

- Allow a [`description` field](https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#fixed-fields-19) as an optional sibling to`$interface` and `$ref`. This will be included in the generated `$ref` field.

## 0.2.0 - 2021-10-02

### Added

- If `--avoid-type-null` is passed, generate `MergePatch` types that with the schema `type: object`. This allows basic compatibility with OpenAPI 3.0, but it skips all merge patch validation.
- When run on OpenAPI 3.0 files, act as if `--avoid-type-null` were set.

### Changed

- BREAKING: Don't generate schemas that would only match the empty object. For example, if an object has no mutable fields, don't generate the `MergePatch` or `Put` variants.

## 0.1.3 - 2021-10-02

More fixes for 0.1.1. It looks like we need more regression tests for the corner cases.

### Fixed

- Fixed bug introduced in 0.1.1 that broke `$ref` and `$interface` in `MergePatch` types.

## 0.1.2 - 2021-10-02

### Fixed

- Fixed bug introduced in 0.1.1 that prevented using `$ref` and `$interface` in with array schemas.
- Cleaned up some error messages.

## 0.1.1 - 2021-10-01

### Added

- Added support for `components.responses`.
- Slightly improved error messages for interfaces and schemas.

## 0.1.0 - 2021-09-23

### Added

- Initial implementation of interface generation.
