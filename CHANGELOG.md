# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.3.0-rc.2 - 2022-06-09

### Added

- Fix bug where we generated spurious `type: ~` entries for certain schema types using `oneOf`.

## 0.3.0-rc.1 - 2022-06-08

### Added

- Binaries for Linux and Mac systems ([#27](https://github.com/faradayio/openapi-interfaces/issues/27)).
- Allow including definitions from external files using `$includeFiles` ([#24](https://github.com/faradayio/openapi-interfaces/issues/24)). See [`examples/include_files/`](https://github.com/faradayio/openapi-interfaces/tree/main/examples/include_files) for an example of how this works. Start from `base.yml`.

### Changed

- See the "Changed" section in v0.2.2-beta.4. This should only affect people using `discriminator` and `oneOf` interfaces.

### Fixed

- Improved error messages in many cases ([#28](https://github.com/faradayio/openapi-interfaces/issues/28)).

## 0.2.2-beta.5 - 2022-06-03

### Fixed

- Published release is made from the public `main` branch.

## 0.2.2-beta.4 - 2022-06-03 [YANKED]

### Fixed

- Copy the `description` of an interface to all the variants we generate ([#23](https://github.com/faradayio/openapi-interfaces/issues/23)). This will allow removing the `allOf` workaround that has been used with Readme.com.
- Emit `type: "object"` for interfaces using `oneOf` ([#13](https://github.com/faradayio/openapi-interfaces/issues/13)).

### Changed

- BREAKING: Completely overhaul how we handle `oneOf` interfaces and discriminators ([#18](https://github.com/faradayio/openapi-interfaces/issues/18)). This work is ongoing and not yet fully documented, but you can find a working example in [`examples/oneof_example.yml`](https://github.com/faradayio/openapi-interfaces/blob/main/examples/oneof_example.yml). This will require changing existing YAML files to use the new, TypeScript-like style. However, this feature is still **incomplete**. For ongoing discussion, see ([#26](https://github.com/faradayio/openapi-interfaces/issues/26)).

## 0.2.2-beta.3 - 2022-04-22

### Added

- Added support for `title` in interfaces.

### Fixed

- Pass through `description` in interfaces.
- Do not output `discriminator: null` if no discriminator is present.

### Fixed

- Pass through

## 0.2.2-beta.2 - 2022-04-16

### Added

- **Experimental:** Add `title` and `description` to the `oneOf` schemas introduced for `MergePatch` types. The goal here is to make auto-generated documentation prettier.

## 0.2.2-beta.1 - 2022-04-15

### Added

- **Experimental:** Add support for using `oneOf` in interfaces. See [`oneof_example.yml`](https://github.com/faradayio/openapi-interfaces/blob/main/examples/oneof_example.yml) and [`oneof_example_output.yml`](https://github.com/faradayio/openapi-interfaces/blob/main/examples/oneof_example_output.yml) for example input and output.

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
