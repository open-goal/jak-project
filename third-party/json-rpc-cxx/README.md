# json-rpc-cxx


[![Build status](https://ci.appveyor.com/api/projects/status/c6rv869h984m1eo2?svg=true)](https://ci.appveyor.com/project/cinemast/json-rpc-cxx)
[![CircleCI](https://circleci.com/gh/jsonrpcx/json-rpc-cxx.svg?style=svg)](https://circleci.com/gh/jsonrpcx/json-rpc-cxx)
![GitHub](https://img.shields.io/github/license/jsonrpcx/json-rpc-cxx.svg)
[![codecov](https://codecov.io/gh/jsonrpcx/json-rpc-cxx/branch/master/graph/badge.svg)](https://codecov.io/gh/jsonrpcx/json-rpc-cxx)
[![Language grade: C/C++](https://img.shields.io/lgtm/grade/cpp/g/jsonrpcx/json-rpc-cxx.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/jsonrpcx/json-rpc-cxx/context:cpp)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/16b095ad49964288b524bc0b499c4efb)](https://www.codacy.com/app/cinemast/json-rpc-cxx?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=jsonrpcx/json-rpc-cxx&amp;utm_campaign=Badge_Grade)
![GitHub tag (latest SemVer)](https://img.shields.io/github/tag/jsonrpcx/json-rpc-cxx.svg)

![json-rpc-cxx-icon](doc/icon.png)

A [JSON-RPC](https://www.jsonrpc.org/) (1.0 & 2.0) framework implemented in C++17 using the [nlohmann's json for modern C++](https://github.com/nlohmann/json).

-   JSON-RPC 1.0 and 2.0 compliant client
-   JSON-RPC 1.0 and 2.0 compliant server
-   Transport agnostic interfaces
-   Compile time type mapping (using [nlohmann's arbitrary type conversion](https://github.com/nlohmann/json#arbitrary-types-conversions))
-   Runtime type checking
-   Cross-platform (Windows, Linux, OSX)

## Installation

-   Copy [include/jsonrpccxx](include) to your include path
-   Alternatively use CMake install mechanism

```bash
mkdir build && cd build
cmake ..
sudo make install
```

## Usage

-   [examples/warehouse/main.cpp](examples/warehouse/main.cpp)

## Design goals

-   Easy to use interface
-   Type safety where possible
-   Avoid errors at compile time where possible
-   Test driven development
-   Choose expressiveness over speed
-   Minimal dependencies

## License

This framework is licensed under [MIT](LICENSE).

### Dependencies

-   [nlohmann's JSON for modern C++](https://github.com/nlohmann/json) is licensed under MIT.
-   Optional: [doctest](https://github.com/onqtam/doctest) is licensed under MIT.

## Developer information

-   [CONTRIBUTING.md](CONTRIBUTING.md)
-   [CHANGELOG.md](CHANGELOG.md)
