#pragma once

/*!
 * @file deftype.h
 * Parser for the GOAL "deftype" form.
 * This is used both in the compiler and in the decompiler for the type definition file.
 */

#include "TypeSystem.h"

#include "common/goos/Object.h"

struct DeftypeResult {
  TypeFlags flags;
  TypeSpec type;
  Type* type_info = nullptr;
  bool create_runtime_type = true;
};

DeftypeResult parse_deftype(const goos::Object& deftype,
                            TypeSystem* ts,
                            goos::EnvironmentMap* constants = nullptr);
TypeSpec parse_typespec(const TypeSystem* type_system, const goos::Object& src);
