# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

## 0.4.1 - 2025-06-02

### Fixed

 - The failing documentation build and outdated documentation.

## 0.4.0 - 2025-06-02

### Added

 - The `type_check` function that performs run-time type checking. Call this function to ensure that all types required by your handler are present in `di::DependencyMap` ([PR #23](https://github.com/teloxide/dptree/pull/23)).
 - The `HandlerSignature` and `Type` types.
 - The `Handler::sig` function.
 - The `di::Injectable::input_types` associated function [**BC**].
 - The `DependencyMap::try_get` method ([PR #29](https://github.com/teloxide/dptree/pull/29)).

### Changed

 - The `Output` generic of `di::Injectable` is now required to be `'static` [**BC**].
 - Function parameters in the implementation of `di::Injectable` for functions are now required to be `'static` [**BC**].
 - `Handler::{chain, branch}` now panic if the second handler is `dptree::entry()` [**BC**].
 - The following functions now accept the `sig: HandlerSignature` parameter [**BC**]:
   - `from_fn`
   - `from_fn_with_description`
 - Always use `di::DependencyMap` instead of a generic `Input` type.
 - The handlers now can accept up to 12 parameters instead of 9 ([PR #28](([PR 28](https://github.com/teloxide/dptree/pull/28)))).

### Removed

 - The `di::{Insert, DependencySupplier}` traits.

## 0.3.0 - 2022-07-19

### Added

 - `inspect{,_async}{,_with_description}` functions for creating inspection handlers.
 - `Handler::{filter,filter_map,map,inspect}{,_async}` convenience methods.

### Changed

 - Removed a few useless bounds from several methods ([PR #13](https://github.com/teloxide/dptree/pull/13)).
 - `Cont` is now `FnOnce` instead of `Fn` ([PR #15](https://github.com/teloxide/dptree/pull/15)).

### Fixed

 - Fix the inference algorithm of allowed updates ([PR #16](https://github.com/teloxide/dptree/pull/16)).

## 0.2.1 - 2022-04-27

### Added

 - The `dptree::case!` macro for enumeration destructuring.

## 0.2.0 - 2022-04-18

### Added

 - Introspection facilities:
   - The `HandlerDescription` trait.
   - Build-in description types: `crate::description::{Unspecified, EventKind}`.
   - Functions constructing handlers with descriptions: `filter_async_with_description`, `filter_map_async_with_description`, `filter_map_with_description`, `filter_with_description`, `from_fn_with_description`, `map_async_with_description`, `map_with_description`.

## 0.1.2 - 2022-04-04

### Added

 - `DependencyMap::insert_container` ([PR #7](https://github.com/teloxide/dptree/pull/7)).
 - `map` and `map_async` ([PR #8](https://github.com/teloxide/dptree/pull/8)).

## 0.1.1 - 2022-03-21

### Fixed

 - Emit a full list of available types on `DependencyMap::get` panic ([PR #6](https://github.com/teloxide/dptree/pull/6)).

## 0.1.0 - 2022-02-05

### Added

 - This badass library.
