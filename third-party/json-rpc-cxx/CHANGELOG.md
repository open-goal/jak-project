# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.1] - 2022-04-19
### Changed
- Updated cpp-httplib to 0.10.6
- Updated nlohmann/json to 3.10.5

### Fixed
- Updated doctest to 2.4.8 (#31)

## [0.3.0] - 2021-03-13
### Changed
- Updated cpp-httplib to v0.8.4
- Migrated from Catch2 to doctest

### Added
- Enum for specified JSON-RPC error codes (#17)

### Fixed
- RPC methods expecting floats/doubles could be called with an integral type which would throw an exception (#18)

## [0.2.1] - 2020-10-21
### Changed
- Updated Catch to version 2.13.2
- Updated nlohmann_json to 3.9.1

### Fixed
- Typemapper failed to convert enum parameters on top-level (#10)

## [0.2.0] - 2020-10-14

### Added
-   Support for `int64` (#9)
-   Support for registering `void` methods (#8)

### Changed
-   Differ between empty lists and calls with no params (#12)
-   Enable and fix compiler warnings (#11)

### Fixed
-   Remove `#include "common.h"` inside `"common.h"`
-   Allow strings as error object


## [0.1.0] - 2019-07-02

### Added
-   Initial implementation of 1.0 and 2.0 server
-   Initial implementation of 1.0 and 2.0 client
-   Test suite for server and client
-   Example Application with HTTP client and server connectors
