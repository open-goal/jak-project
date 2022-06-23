#include "CallingConvention.h"

#include "common/util/Assert.h"

CallingConvention get_function_calling_convention(const TypeSpec& function_type,
                                                  const TypeSystem& type_system) {
  ASSERT(function_type.base_type() == "function");
  ASSERT(function_type.arg_count() > 0);
  ASSERT(function_type.arg_count() <= 9);

  int gpr_idx = 0;
  int xmm_idx = 0;

  CallingConvention cc;

  if (function_type.arg_count() == 2 && function_type.get_arg(0).print() == "_varargs_") {
    for (int i = 0; i < 8; i++) {
      cc.arg_regs.push_back(emitter::gRegInfo.get_gpr_arg_reg(gpr_idx++));
    }
  } else {
    for (int i = 0; i < (int)function_type.arg_count() - 1; i++) {
      auto info = type_system.lookup_type_allow_partial_def(function_type.get_arg(i));
      auto load_size = type_system.get_load_size_allow_partial_def(function_type.get_arg(i));
      if (dynamic_cast<const ValueType*>(info) && load_size == 16) {
        cc.arg_regs.push_back(emitter::gRegInfo.get_xmm_arg_reg(xmm_idx++));
      } else {
        cc.arg_regs.push_back(emitter::gRegInfo.get_gpr_arg_reg(gpr_idx++));
      }
    }
  }

  if (function_type.last_arg() != TypeSpec("none")) {
    if (type_system.get_load_size_allow_partial_def(function_type.last_arg()) == 16) {
      cc.return_reg = emitter::gRegInfo.get_xmm_ret_reg();
    } else {
      cc.return_reg = emitter::gRegInfo.get_gpr_ret_reg();
    }
  }

  return cc;
}

std::vector<emitter::Register> get_arg_registers(const TypeSystem& type_system,
                                                 const std::vector<TypeSpec>& arg_types) {
  std::vector<emitter::Register> result;
  int gpr_idx = 0;
  int xmm_idx = 0;
  for (auto& type : arg_types) {
    auto load_size = type_system.get_load_size_allow_partial_def(type);
    if (load_size == 16) {
      result.push_back(emitter::gRegInfo.get_xmm_arg_reg(xmm_idx++));
    } else {
      result.push_back(emitter::gRegInfo.get_gpr_arg_reg(gpr_idx++));
    }
  }
  return result;
}
