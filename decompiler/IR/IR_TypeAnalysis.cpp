#include "IR.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "third-party/fmt/core.h"
#include "common/goos/Object.h"
#include "decompiler/util/TP_Type.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace {
bool is_plain_type(const TP_Type& type, const TypeSpec& ts) {
  return type.as_typespec() == ts;
}

bool is_integer_type(const TP_Type& type) {
  return is_plain_type(type, TypeSpec("int")) || is_plain_type(type, TypeSpec("uint"));
}

/*!
 * If first arg is unsigned, make the result unsigned.
 * Otherwise signed. This is the default GOAL behavior I guess.
 */
TP_Type get_int_type(const TP_Type& one) {
  if (is_plain_type(one, TypeSpec("uint"))) {
    return one;
  } else {
    return TP_Type(TypeSpec("int"));
  }
}

struct RegOffset {
  Register reg;
  int offset;
};

bool get_as_reg_offset(const IR* ir, RegOffset* out) {
  auto as_reg = dynamic_cast<const IR_Register*>(ir);
  if (as_reg) {
    out->reg = as_reg->reg;
    out->offset = 0;
    return true;
  }

  auto as_math = dynamic_cast<const IR_IntMath2*>(ir);
  if (as_math && as_math->kind == IR_IntMath2::ADD) {
    auto first_as_reg = dynamic_cast<const IR_Register*>(as_math->arg0.get());
    auto second_as_const = dynamic_cast<const IR_IntegerConstant*>(as_math->arg1.get());
    if (first_as_reg && second_as_const) {
      out->reg = first_as_reg->reg;
      out->offset = second_as_const->value;
      return true;
    }
  }
  return false;
}

RegKind get_reg_kind(const Register& r) {
  switch (r.get_kind()) {
    case Reg::GPR:
      return RegKind::GPR_64;
    case Reg::FPR:
      return RegKind::FLOAT;
    default:
      assert(false);
  }
}
}  // namespace

void IR_Atomic::propagate_types(const TypeState& input,
                                const LinkedObjectFile& file,
                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  throw std::runtime_error(
      fmt::format("Could not propagate types for {}, not yet implemented", print(file)));
}

TP_Type IR::get_expression_type(const TypeState& input,
                                const LinkedObjectFile& file,
                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  throw std::runtime_error(
      fmt::format("Could not get expression types for {}, not yet implemented", print(file)));
}

void IR_Set_Atomic::propagate_types(const TypeState& input,
                                    const LinkedObjectFile& file,
                                    DecompilerTypeSystem& dts) {
  // pass through types
  end_types = input;
  // modify as needed
  switch (kind) {
    case IR_Set::REG_64:
    case IR_Set::LOAD:
    case IR_Set::GPR_TO_FPR:
    case IR_Set::FPR_TO_GPR64:
    case IR_Set::REG_FLT:
    case IR_Set::SYM_LOAD: {
      auto as_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(as_reg);
      auto t = src->get_expression_type(input, file, dts);
      end_types.get(as_reg->reg) = t;
    } break;

    case IR_Set::SYM_STORE: {
      auto as_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(!as_reg);
      return;
    }
    default:
      throw std::runtime_error(fmt::format(
          "Could not propagate types through IR_Set_Atomic, kind not handled {}", print(file)));
  }
}

TP_Type IR_Register::get_expression_type(const TypeState& input,
                                         const LinkedObjectFile& file,
                                         DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  return input.get(reg);
}

TP_Type IR_Load::get_expression_type(const TypeState& input,
                                     const LinkedObjectFile& file,
                                     DecompilerTypeSystem& dts) {
  (void)input;
  auto as_static = dynamic_cast<IR_StaticAddress*>(location.get());
  if (as_static) {
    if (kind == FLOAT) {
      // loading static data with a FLOAT kind load (lwc1), assume result is a float.
      return TP_Type(dts.ts.make_typespec("float"));
    }

    if (size == 8) {
      // kinda hacky
      if (kind == SIGNED) {
        return TP_Type(dts.ts.make_typespec("int"));
      } else if (kind == UNSIGNED) {
        return TP_Type(dts.ts.make_typespec("uint"));
      }
    }
  }

  RegOffset ro;
  if (get_as_reg_offset(location.get(), &ro)) {
    auto& input_type = input.get(ro.reg);

    if (input_type.kind == TP_Type::TYPE_OBJECT && ro.offset >= 16 && (ro.offset & 3) == 0 &&
        size == 4 && kind == UNSIGNED) {
      // method get
      auto method_id = (ro.offset - 16) / 4;
      if (input_type.ts.base_type() == "object" && method_id == GOAL_NEW_METHOD) {
        // remember that we're an object new.
        auto method_info = dts.ts.lookup_method(input_type.ts.print(), method_id);
        auto result = TP_Type(method_info.type.substitute_for_method_call(input_type.ts.print()));
        result.kind = TP_Type::METHOD_NEW_OF_OBJECT;
        return result;
      }
      auto method_info = dts.ts.lookup_method(input_type.ts.print(), method_id);
      return TP_Type(method_info.type.substitute_for_method_call(input_type.ts.print()));
    }

    if (input_type.kind == TP_Type::OBJECT_OF_TYPE &&
        input_type.as_typespec() == TypeSpec("type") && ro.offset >= 16 && (ro.offset & 3) == 0 &&
        size == 4 && kind == UNSIGNED) {
      // method get
      auto method_id = (ro.offset - 16) / 4;
      auto method_info = dts.ts.lookup_method("object", method_id);
      return TP_Type(method_info.type.substitute_for_method_call("object"));
    }

    if (input_type.kind == TP_Type::OBJECT_OF_TYPE &&
        input_type.as_typespec() == TypeSpec("pointer")) {
      // we got a plain pointer. let's just assume we're loading an integer.
      // perhaps we should disable this feature by default on 4-byte loads if we're getting
      // lots of false positives for loading pointers from plain pointers.
      switch (kind) {
        case UNSIGNED:
          switch (size) {
            case 1:
              return TP_Type(TypeSpec("uint"));
            case 2:
              return TP_Type(TypeSpec("uint"));
            case 4:
              return TP_Type(TypeSpec("uint"));
            case 8:
              return TP_Type(TypeSpec("uint"));
            case 16:
              return TP_Type(TypeSpec("uint"));
            default:
              assert(false);
          }
          break;
        case SIGNED:
          switch (size) {
            case 1:
              return TP_Type(TypeSpec("int"));
            case 2:
              return TP_Type(TypeSpec("int"));
            case 4:
              return TP_Type(TypeSpec("int"));
            case 8:
              return TP_Type(TypeSpec("int"));
            case 16:
              return TP_Type(TypeSpec("int"));
            default:
              assert(false);
          }
          break;
        case FLOAT:
          return TP_Type(TypeSpec("float"));
        default:
          assert(false);
      }
    }

    if (input_type.kind == TP_Type::PARTIAL_METHOD_TABLE_ACCESS && ro.offset == 16) {
      // access method vtable
      return TP_Type(TypeSpec("function"));
    } else if (input_type.kind == TP_Type::OBJ_PLUS_PRODUCT) {
      // note, we discard and completely ignore the stride here.
      ReverseDerefInputInfo rd_in;
      rd_in.mem_deref = true;
      rd_in.input_type = input_type.ts;
      rd_in.reg = get_reg_kind(ro.reg);  // bleh
      rd_in.offset = ro.offset;
      rd_in.sign_extend = kind == SIGNED;
      rd_in.load_size = size;
      auto rd = dts.ts.get_reverse_deref_info(rd_in);

      if (rd.success) {
        return TP_Type(coerce_to_reg_type(rd.result_type));
      }
    } else {
      if (input_type.as_typespec() == TypeSpec("object") && ro.offset == -4 && kind == UNSIGNED &&
          size == 4 && ro.reg.get_kind() == Reg::GPR) {
        // get type of basic likely, but misrecognized as an object.
        // occurs often in typecase-like structures because other possible types are "stripped".
        return TP_Type(TypeSpec("type"));
      }

      // nice
      ReverseDerefInputInfo rd_in;
      rd_in.mem_deref = true;
      rd_in.input_type = input_type.as_typespec();
      rd_in.reg = get_reg_kind(ro.reg);  // bleh
      rd_in.offset = ro.offset;
      rd_in.sign_extend = kind == SIGNED;
      rd_in.load_size = size;

      auto rd = dts.ts.get_reverse_deref_info(rd_in);
      if (!rd.success && !dts.type_prop_settings.allow_pair) {
        printf("input type is %s, offset is %d, sign %d size %d\n",
               rd_in.input_type.print().c_str(), rd_in.offset, rd_in.sign_extend, rd_in.load_size);
        throw std::runtime_error(
            fmt::format("Could not get type of load: {}. Reverse Deref Failed.", print(file)));
      }

      if (rd.success) {
        return TP_Type(coerce_to_reg_type(rd.result_type));
      }

      if (dts.type_prop_settings.allow_pair) {
        if (kind == SIGNED && size == 4 &&
            (input_type.as_typespec() == TypeSpec("object") ||
             input_type.as_typespec() == TypeSpec("pair"))) {
          // pair access!
          if (ro.offset == 2) {
            return TP_Type(TypeSpec("pair"));
          } else if (ro.offset == -2) {
            return TP_Type(TypeSpec("object"));
          }
        }
      }
    }
  }

  throw std::runtime_error(
      fmt::format("Could not get type of load: {}. Not handled.", print(file)));
}

TP_Type IR_FloatMath2::get_expression_type(const TypeState& input,
                                           const LinkedObjectFile& file,
                                           DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;

  // regardless of input types, the output is going to be a float.
  switch (kind) {
    case DIV:
    case MUL:
    case ADD:
    case SUB:
    case MIN:
    case MAX:
      return TP_Type(dts.ts.make_typespec("float"));
    default:
      assert(false);
  }
}

TP_Type IR_IntMath2::get_expression_type(const TypeState& input,
                                         const LinkedObjectFile& file,
                                         DecompilerTypeSystem& dts) {
  auto arg0_type = arg0->get_expression_type(input, file, dts);
  auto arg1_type = arg1->get_expression_type(input, file, dts);

  if (is_integer_type(arg0_type) && is_integer_type(arg1_type)) {
    // case where both arguments are integers.
    // in this case we assume we're actually doing math.
    switch (kind) {
      case ADD:
      case SUB:
      case AND:
      case OR:
      case NOR:
      case XOR:
        // we don't know if we're signed or unsigned. so let's just go with the first type.
        return get_int_type(arg0_type);
      case MUL_SIGNED:
      case DIV_SIGNED:
      case RIGHT_SHIFT_ARITH:
      case MOD_SIGNED:
      case MIN_SIGNED:
      case MAX_SIGNED:
        // result is going to be signed, regardless of inputs.
        return TP_Type(TypeSpec("int"));

      case MUL_UNSIGNED:
      case RIGHT_SHIFT_LOGIC:
        // result is going to be unsigned, regardless of inputs.
        return TP_Type(TypeSpec("uint"));

      case LEFT_SHIFT: {
        // multiply!
        auto as_const = dynamic_cast<IR_IntegerConstant*>(arg1.get());
        if (as_const) {
          // shift by constant integer. could be accessing the method array.
          TP_Type result;
          result.kind = TP_Type::PRODUCT;
          result.ts = get_int_type(arg0_type).ts;
          result.multiplier = (1 << as_const->value);
          return result;
        } else {
          // normal variable shift.
          return get_int_type(arg0_type);
        }
      }
      default:
        break;
    }
  }

  if (kind == ADD && arg0_type.kind == TP_Type::PRODUCT && arg1_type.is_object_of_type()) {
    // access the methods!
    return TP_Type::make_partial_method_table_access();
  }

  auto a1_const = dynamic_cast<IR_IntegerConstant*>(arg1.get());
  if (a1_const && kind == ADD && arg0_type.kind == TP_Type::OBJECT_OF_TYPE) {
    // access a field.
    ReverseDerefInputInfo rd_in;
    rd_in.mem_deref = false;
    rd_in.input_type = arg0_type.as_typespec();
    rd_in.offset = a1_const->value;
    rd_in.load_size = 0;
    auto rd = dts.ts.get_reverse_deref_info(rd_in);

    if (rd.success) {
      return TP_Type(coerce_to_reg_type(rd.result_type));
    }
  }

  if (kind == ADD && is_integer_type(arg0_type) && arg1_type.kind == TP_Type::OBJECT_OF_TYPE) {
    // product + object with multiplier 1 (access array of bytes for example)
    TP_Type result;
    result.kind = TP_Type::OBJ_PLUS_PRODUCT;
    result.ts = arg1_type.as_typespec();
    result.multiplier = 1;
    return result;
  }

  if (kind == ADD && arg0_type.kind == TP_Type::PRODUCT &&
      arg1_type.kind == TP_Type::OBJECT_OF_TYPE) {
    TP_Type result;
    result.kind = TP_Type::OBJ_PLUS_PRODUCT;
    result.ts = arg1_type.as_typespec();
    result.multiplier = arg0_type.multiplier;
    return result;
  }

  if ((arg0_type.as_typespec() == TypeSpec("object") ||
       arg0_type.as_typespec() == TypeSpec("pair")) &&
      is_integer_type(arg1_type)) {
    // boxed object tag trick
    return TP_Type(TypeSpec("int"));
  }

  if (dts.ts.typecheck(TypeSpec("pointer"), arg0_type.as_typespec(), "", false, false) &&
      is_integer_type(arg1_type)) {
    return arg0_type;
  }

  throw std::runtime_error(
      fmt::format("Can't get_expression_type on this IR_IntMath2: {}, args {} and {}", print(file),
                  arg0_type.print(), arg1_type.print()));
}

void BranchDelay::type_prop(TypeState& output,
                            const LinkedObjectFile& file,
                            DecompilerTypeSystem& dts) {
  (void)dts;
  switch (kind) {
    case DSLLV: {
      // I think this is only used in ash, in which case the output should be an int/uint
      // welll
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      auto src = dynamic_cast<IR_Register*>(source.get());
      assert(src);
      if (is_plain_type(output.get(src->reg), TypeSpec("uint"))) {
        // todo, this won't catch child uint types. I think this doesn't matter though.
        output.get(dst->reg) = TP_Type(TypeSpec("uint"));
      }
      output.get(dst->reg) = TP_Type(TypeSpec("int"));
    } break;
    case NEGATE: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg) = TP_Type(TypeSpec("int"));
    } break;
    case SET_REG_FALSE: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg).kind = TP_Type::FALSE;
    } break;
    case SET_REG_REG: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      auto src = dynamic_cast<IR_Register*>(source.get());
      assert(src);
      output.get(dst->reg) = output.get(src->reg);
      break;
    }
    case SET_REG_TRUE: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg) = TP_Type(TypeSpec("symbol"));
    } break;

    case SET_BINTEGER: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg) = TP_Type::make_type_object("binteger");
    } break;

    case SET_PAIR: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg) = TP_Type::make_type_object("pair");
    } break;

    case NOP:
      break;

    default:
      throw std::runtime_error("Unhandled branch delay in type_prop: " + to_form(file).print());
  }
}

void IR_Branch_Atomic::propagate_types(const TypeState& input,
                                       const LinkedObjectFile& file,
                                       DecompilerTypeSystem& dts) {
  // pass through types
  end_types = input;
  branch_delay.type_prop(end_types, file, dts);
  // todo clobbers.
}

TP_Type IR_IntMath1::get_expression_type(const TypeState& input,
                                         const LinkedObjectFile& file,
                                         DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  auto arg_type = arg->get_expression_type(input, file, dts);
  switch (kind) {
    case NEG:
      // if we negate a thing, let's just make it a signed integer.
      return TP_Type(TypeSpec("int"));
    case NOT:
      return get_int_type(arg_type);
    default:
      throw std::runtime_error("IR_IntMath1::get_expression_type case not handled: " +
                               to_form(file).print());
  }
}

TP_Type IR_SymbolValue::get_expression_type(const TypeState& input,
                                            const LinkedObjectFile& file,
                                            DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  if (name == "#f") {
    TP_Type result;
    result.kind = TP_Type::FALSE;
    return result;
  } else if (name == "__START-OF-TABLE__") {
    return TP_Type(TypeSpec("uint"));
  }

  auto type = dts.symbol_types.find(name);
  if (type == dts.symbol_types.end()) {
    throw std::runtime_error("Don't have the type of symbol " + name);
  }

  if (type->second == TypeSpec("type")) {
    // let's remember what we got this from.
    return TP_Type::make_type_object(name);
  }

  return TP_Type(type->second);
}

TP_Type IR_Symbol::get_expression_type(const TypeState& input,
                                       const LinkedObjectFile& file,
                                       DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  if (name == "#f") {
    TP_Type result;
    result.kind = TP_Type::FALSE;
    return result;
  }

  return TP_Type(TypeSpec("symbol"));
}

TP_Type IR_IntegerConstant::get_expression_type(const TypeState& input,
                                                const LinkedObjectFile& file,
                                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  return TP_Type(TypeSpec("int"));
}

TP_Type IR_Compare::get_expression_type(const TypeState& input,
                                        const LinkedObjectFile& file,
                                        DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  return TP_Type(TypeSpec("symbol"));
}

void IR_Nop_Atomic::propagate_types(const TypeState& input,
                                    const LinkedObjectFile& file,
                                    DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  end_types = input;
}

void IR_Call_Atomic::propagate_types(const TypeState& input,
                                     const LinkedObjectFile& file,
                                     DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  // todo clobber
  end_types = input;

  auto in_tp = input.get(Register(Reg::GPR, Reg::T9));
  if (in_tp.kind == TP_Type::METHOD_NEW_OF_OBJECT &&
      !dts.type_prop_settings.current_method_type.empty()) {
    end_types.get(Register(Reg::GPR, Reg::V0)) =
        TP_Type(dts.type_prop_settings.current_method_type);
    return;
  }
  auto in_type = in_tp.as_typespec();
  if (in_type.base_type() != "function") {
    throw std::runtime_error("Called something that wasn't a function: " + in_type.print());
  }

  if (in_type.arg_count() < 1) {
    throw std::runtime_error("Called a function, but we don't know its type");
  }

  end_types.get(Register(Reg::GPR, Reg::V0)) = TP_Type(in_type.last_arg());
}

void IR_Store_Atomic::propagate_types(const TypeState& input,
                                      const LinkedObjectFile& file,
                                      DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  end_types = input;
}

TP_Type IR_StaticAddress::get_expression_type(const TypeState& input,
                                              const LinkedObjectFile& file,
                                              DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  auto label = file.labels.at(label_id);
  if ((label.offset & 0xf) == 4) {
    // it's a basic! probably.
    const auto& word = file.words_by_seg.at(label.target_segment).at((label.offset - 4) / 4);
    if (word.kind == LinkedWord::TYPE_PTR) {
      return TP_Type(TypeSpec(word.symbol_name));
    }
  }

  throw std::runtime_error("IR_StaticAddress couldn't figure out the type: " + label.name);
}

void IR_AsmOp_Atomic::propagate_types(const TypeState& input,
                                      const LinkedObjectFile& file,
                                      DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  auto dst_reg = dynamic_cast<IR_Register*>(dst.get());
  end_types = input;
  if (dst_reg) {
    if (name == "daddu") {
      end_types.get(dst_reg->reg) = TP_Type(TypeSpec("uint"));
    }
  }
}