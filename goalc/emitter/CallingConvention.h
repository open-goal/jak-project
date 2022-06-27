#pragma once

#include <optional>
#include <vector>

#include "common/type_system/TypeSystem.h"

#include "goalc/emitter/Register.h"

struct CallingConvention {
  std::vector<emitter::Register> arg_regs;
  std::optional<emitter::Register> return_reg;
};

std::vector<emitter::Register> get_arg_registers(const TypeSystem& type_system,
                                                 const std::vector<TypeSpec>& arg_types);
CallingConvention get_function_calling_convention(const TypeSpec& function_type,
                                                  const TypeSystem& type_system);