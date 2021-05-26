#include "common/util/assert.h"

#include "CallingConvention.h"

CallingConvention get_function_calling_convention(const TypeSpec& function_type,
                                                  const TypeSystem& type_system) {
  assert(function_type.base_type() == "function");
  assert(function_type.arg_count() > 0);
  assert(function_type.arg_count() <= 9);

  int gpr_idx = 0;
  int xmm_idx = 0;

  CallingConvention cc;

  if (function_type.arg_count() == 2 && function_type.get_arg(0).print() == "_varargs_") {
    for (int i = 0; i < 8; i++) {
      cc.arg_regs.push_back(emitter::gRegInfo.get_gpr_arg_reg(gpr_idx++));
    }
  } else {
    for (int i = 0; i < (int)function_type.arg_count() - 1; i++) {
      auto info = type_system.lookup_type(function_type.get_arg(i));
      if (dynamic_cast<const ValueType*>(info) && info->get_load_size() == 16) {
        cc.arg_regs.push_back(emitter::gRegInfo.get_xmm_arg_reg(xmm_idx++));
      } else {
        cc.arg_regs.push_back(emitter::gRegInfo.get_gpr_arg_reg(gpr_idx++));
      }
    }
  }

  if (function_type.last_arg() != TypeSpec("none")) {
    if (type_system.lookup_type(function_type.last_arg())->get_load_size() == 16) {
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
    auto info = type_system.lookup_type(type);
    if (info->get_load_size() == 16) {
      result.push_back(emitter::gRegInfo.get_xmm_arg_reg(xmm_idx++));
    } else {
      result.push_back(emitter::gRegInfo.get_gpr_arg_reg(gpr_idx++));
    }
  }
  return result;
}