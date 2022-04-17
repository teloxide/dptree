# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

### Added

 - Introspection facilities:
   - The `HandlerDescription` trait.
   - Build-in description types: `crate::description::{Unspecified`, `EventKind}`.
   - Functions constructing handlers with descriptions: `filter_async_with_description`, `filter_map_async_with_description`, `filter_map_with_description`, `filter_with_description`, `from_fn_with_description`, `map_async_with_description`, `map_with_description`.

## 0.1.2 - 2022-04-04

### Added

 - `DependencyMap::insert_container` ([PR #7](https://github.com/p0lunin/dptree/pull/7)).
 - `map` and `map_async` ([PR #8](https://github.com/p0lunin/dptree/pull/8)).

## 0.1.1 - 2022-03-21

### Fixed

 - Emit a full list of available types on `DependencyMap::get` panic ([PR #6](https://github.com/p0lunin/dptree/pull/6)).

## 0.1.0 - 2022-02-05

### Added

 - This badass library.
