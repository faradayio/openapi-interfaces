# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
