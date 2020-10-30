#include <decompiler/Disasm/InstructionMatching.h>
#include "IR.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "third-party/fmt/core.h"

bool IR::get_type_of_expr(const TypeMap& reg_types,
                          DecompilerTypeSystem& dts,
                          LinkedObjectFile& file,
                          TypeSpec* out) const {
  (void)reg_types;
  (void)dts;
  (void)file;
  (void)out;
  return false;
}

bool IR_Register::get_type_of_expr(const TypeMap& reg_types,
                                   DecompilerTypeSystem& dts,
                                   LinkedObjectFile& file,
                                   TypeSpec* out) const {
  (void)dts;
  (void)file;
  auto kv = reg_types.find(reg);
  if (kv != reg_types.end()) {
    *out = kv->second;
    return true;
  }
  return false;
}

bool IR_Set::update_types(TypeMap& reg_types,
                          DecompilerTypeSystem& dts,
                          LinkedObjectFile& file) const {
  auto dest_as_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dest_as_reg) {
    TypeSpec src_type;
    if (!src->get_type_of_expr(reg_types, dts, file, &src_type)) {
      return false;
    }
    reg_types[dest_as_reg->reg] = src_type;
    return true;
  }

  return false;
}

bool IR_Load::get_type_of_expr(const TypeMap& reg_types,
                               DecompilerTypeSystem& dts,
                               LinkedObjectFile& file,
                               TypeSpec* out) const {
  auto loc_as_static = dynamic_cast<IR_StaticAddress*>(location.get());
  if (loc_as_static) {
    // this will need to get upgraded once we have good support for static data.
    // but for now we will do a "best guess" that should cover common cases.
    if (kind == FLOAT) {
      // the FLOAT kind is for an instruction that loads directly into a floating point register.
      // so we know it's either float or a child of float.
      // this can be two cases, loading a floating point constant, or loading a float from
      // a static object.  In either case, we don't yet have enough information to get a more
      // specific type, so "float" is a safe fallback.
      *out = dts.ts.make_typespec("float");
      return true;
    }
  }

  TypeSpec loc_type;
  if (location->get_type_of_expr(reg_types, dts, file, &loc_type)) {
    ReverseDerefInputInfo info;
    info.mem_deref = true;
    info.input_type = loc_type;
    info.offset = 0;
    info.load_size = size;
    info.sign_extend = kind == SIGNED;
    switch (kind) {
      case UNSIGNED:
      case SIGNED:
        info.reg = RegKind::GPR_64;
        break;
      case FLOAT:
        info.reg = RegKind::FLOAT;
        break;
      default:
        assert(false);
    }
    auto result = dts.ts.get_reverse_deref_info(info);
    if (result.success) {
      *out = result.result_type;
      return true;
    }
  }

  return false;
}

namespace {
bool is_type(const TypeSpec& type, const std::string& name, TypeSystem& ts) {
  return ts.typecheck(ts.make_typespec(name), type, "", false, false);
}

bool is_float(const TypeSpec& type, TypeSystem& ts) {
  return ts.typecheck(ts.make_typespec("float"), type, "", false, false);
}

bool is_int_or_uint(const TypeSpec& type, TypeSystem& ts) {
  return is_type(type, "int", ts) || is_type(type, "uint", ts);
}

}  // namespace

bool IR_FloatMath2::get_type_of_expr(const TypeMap& reg_types,
                                     DecompilerTypeSystem& dts,
                                     LinkedObjectFile& file,
                                     TypeSpec* out) const {
  // to be paranoid, we should check that our arguments are both floats.
  for (auto& arg : {arg0, arg1}) {
    TypeSpec arg_type;
    if (!arg->get_type_of_expr(reg_types, dts, file, &arg_type)) {
      return false;
    }
    if (!is_float(arg_type, dts.ts)) {
      return false;
    }
  }

  *out = dts.ts.make_typespec("float");
  return true;
}

bool IR_IntMath2::get_type_of_expr(const TypeMap& reg_types,
                                   DecompilerTypeSystem& dts,
                                   LinkedObjectFile& file,
                                   TypeSpec* out) const {
  TypeSpec arg0_type;
  TypeSpec arg1_type;

  // special case for subtraction with r0
  //  auto arg0_as_reg = dynamic_cast<IR_Register*>(arg0.get());
  //  if(arg0_as_reg && arg0_as_reg->reg == make_gpr(Reg::R0) && kind == SUB) {
  //    if (!arg1->get_type_of_expr(reg_types, dts, file, &arg1_type)) {
  //      return false;
  //    }
  //
  //    if(is_int_or_uint(arg1_type, dts.ts)) {
  //      *out = arg1_type;
  //      return true;
  //    }
  //  }

  if (!arg0->get_type_of_expr(reg_types, dts, file, &arg0_type)) {
    return false;
  }

  if (!arg1->get_type_of_expr(reg_types, dts, file, &arg1_type)) {
    fmt::print("a1 fail\n");
    return false;
  }

  if (is_int_or_uint(arg0_type, dts.ts) && is_int_or_uint(arg1_type, dts.ts)) {
    // the arg0 wins
    *out = arg0_type;
    return true;
  }

  auto arg1_as_int = dynamic_cast<IR_IntegerConstant*>(arg1.get());
  if (kind == ADD && arg1_as_int) {
    // it's a memory thing...
    ReverseDerefInputInfo info;
    info.mem_deref = false;
    info.input_type = arg0_type;
    info.offset = arg1_as_int->value;
    info.load_size = 0;
    info.sign_extend = false;
    info.reg = RegKind::GPR_64;
    auto result = dts.ts.get_reverse_deref_info(info);
    if (result.success) {
      *out = result.result_type;
      return true;
    }
  }

  //  auto arg0_as_int = dynamic_cast<IR_IntegerConstant*>(arg0.get());
  //  if (kind == ADD && arg0_as_int) {
  //    // it's a memory thing...
  //    ReverseDerefInputInfo info;
  //    info.mem_deref = false;
  //    info.input_type = arg1_type;
  //    info.offset = arg0_as_int->value;
  //    info.load_size = 0;
  //    info.sign_extend = false;
  //    info.reg = RegKind::GPR_64;
  //    auto result = dts.ts.get_reverse_deref_info(info);
  //    if (result.success) {
  //      *out = result.result_type;
  //      return true;
  //    } else {
  //      fmt::print("deref fail!\n");
  //    }
  //  }

  return false;
}

bool IR_IntMath1::get_type_of_expr(const TypeMap& reg_types,
                                   DecompilerTypeSystem& dts,
                                   LinkedObjectFile& file,
                                   TypeSpec* out) const {
  TypeSpec arg_type;

  if (!arg->get_type_of_expr(reg_types, dts, file, &arg_type)) {
    return false;
  }

  if (is_int_or_uint(arg_type, dts.ts)) {
    *out = arg_type;
    return true;
  }

  return false;
}

bool IR_Branch::update_types(TypeMap& reg_types,
                             DecompilerTypeSystem& dts,
                             LinkedObjectFile& file) const {
  (void)file;
  switch (branch_delay.kind) {
    case BranchDelay::DSLLV: {
      auto dst_as_reg = dynamic_cast<IR_Register*>(branch_delay.destination.get());
      if (dst_as_reg) {
        reg_types[dst_as_reg->reg] = dts.ts.make_typespec("int");  // todo?
        return true;
      }
    } break;
    case BranchDelay::NEGATE: {
      auto dst_as_reg = dynamic_cast<IR_Register*>(branch_delay.destination.get());
      if (dst_as_reg) {
        reg_types[dst_as_reg->reg] = dts.ts.make_typespec("int");  // todo?
        return true;
      }
    } break;
    case BranchDelay::SET_REG_FALSE: {
      auto dst_as_reg = dynamic_cast<IR_Register*>(branch_delay.destination.get());
      if (dst_as_reg) {
        // this probably will break a lot of things when using the result of an if.
        reg_types[dst_as_reg->reg] = dts.ts.make_typespec("basic");  // todo?
        return true;
      }
    } break;
    case BranchDelay::NOP:
      return true;
    case BranchDelay::SET_REG_REG: {
      auto dst_as_reg = dynamic_cast<IR_Register*>(branch_delay.destination.get());
      if (dst_as_reg) {
        // this probably will break a lot of things when using the result of an if.
        auto src_as_reg = dynamic_cast<IR_Register*>(branch_delay.source.get());
        if (src_as_reg) {
          auto src_kv = reg_types.find(src_as_reg->reg);
          if (src_kv != reg_types.end()) {
            reg_types[dst_as_reg->reg] = reg_types[src_as_reg->reg];
            return true;
          }
        }
      }
      return false;
    } break;
    default:
      return false;
  }
  return false;
}

bool IR_Symbol::get_type_of_expr(const TypeMap& reg_types,
                                 DecompilerTypeSystem& dts,
                                 LinkedObjectFile& file,
                                 TypeSpec* out) const {
  (void)reg_types;
  (void)file;
  *out = dts.ts.make_typespec("symbol");
  return true;
}

bool IR_SymbolValue::get_type_of_expr(const TypeMap& reg_types,
                                      DecompilerTypeSystem& dts,
                                      LinkedObjectFile& file,
                                      TypeSpec* out) const {
  (void)reg_types;
  (void)file;
  auto kv = dts.symbol_types.find(name);
  if (kv != dts.symbol_types.end()) {
    *out = kv->second;
    return true;
  }
  return false;
}

bool IR_IntegerConstant::get_type_of_expr(const TypeMap& reg_types,
                                          DecompilerTypeSystem& dts,
                                          LinkedObjectFile& file,
                                          TypeSpec* out) const {
  (void)reg_types;
  (void)file;
  *out = dts.ts.make_typespec("int");
  return true;
}

bool IR_Compare::get_type_of_expr(const TypeMap& reg_types,
                                  DecompilerTypeSystem& dts,
                                  LinkedObjectFile& file,
                                  TypeSpec* out) const {
  (void)reg_types;
  (void)file;
  // always returns #t or #f
  *out = dts.ts.make_typespec("symbol");
  return true;
}