#pragma once

/*!
 * @file deftype.h
 * Parser for the GOAL "deftype" form.
 * This is used both in the compiler and in the decompiler for the type definition file.
 */

#include "TypeSystem.h"
#include "common/goos/Object.h"

struct TypeFlags {
  union {
    uint64_t flag = 0;
    struct {
      uint16_t size;
      uint16_t heap_base;
      uint16_t methods;
      uint16_t pad;
    };
  };
};

struct DeftypeResult {
  TypeFlags flags;
  TypeSpec type;
  Type* type_info = nullptr;
  bool create_runtime_type = true;
};

DeftypeResult parse_deftype(const goos::Object& deftype, TypeSystem* ts);
TypeSpec parse_typespec(TypeSystem* type_system, const goos::Object& src);
