#pragma once

#ifndef JAK_DISASSEMBLER_TYPESPEC_H
#define JAK_DISASSEMBLER_TYPESPEC_H

#include <string>
#include <vector>
#include "decompiler/util/LispPrint.h"

class TypeSpec {
 public:
  TypeSpec() = default;
  explicit TypeSpec(std::string base_type) : m_base_type(std::move(base_type)) {}
  TypeSpec(std::string base_type, std::vector<TypeSpec> args)
      : m_base_type(std::move(base_type)), m_args(std::move(args)) {}

  std::string to_string() const;
  std::shared_ptr<Form> to_form() const;

  bool operator==(const TypeSpec& other) const;
  bool operator!=(const TypeSpec& other) const;

 private:
  std::string m_base_type;
  std::vector<TypeSpec> m_args;
};

#endif  // JAK_DISASSEMBLER_TYPESPEC_H
