# Zyan Core Library for C

Internal library providing platform independent types, macros and a fallback for environments without LibC.

## Features

- Platform independent types
  - Integer types (`ZyanU8`, `ZyanI32`, `ZyanUSize`, ...)
  - `ZyanBool` (+ `ZYAN_FALSE`, `ZYAN_TRUE`)
  - `ZYAN_NULL`
- Macros
  - Compiler/Platform/Architecture detection
  - Asserts and static asserts
  - Utils (`ARRAY_LENGTH`, `FALLTHROUGH`, `UNUSED`, ...)
- Common types
  - `ZyanBitset`
  - `ZyanString`/`ZyanStringView`
- Container types
  - `ZyanVector`
  - `ZyanList`
- LibC abstraction (WiP)

## License

Zycore is licensed under the MIT license.
