#include "third-party/fmt/core.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "common/log/log.h"
#include "AtomicOp.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

namespace {
bool tc(const DecompilerTypeSystem& dts, const TypeSpec& expected, const TP_Type& actual) {
  return dts.ts.typecheck(expected, actual.typespec(), "", false, false);
}

bool is_int_or_uint(const DecompilerTypeSystem& dts, const TP_Type& type) {
  return tc(dts, TypeSpec("int"), type) || tc(dts, TypeSpec("uint"), type);
}

bool is_signed(const DecompilerTypeSystem& dts, const TP_Type& type) {
  return tc(dts, TypeSpec("int"), type) && !tc(dts, TypeSpec("uint"), type);
}

RegClass get_reg_kind(const Register& r) {
  switch (r.get_kind()) {
    case Reg::GPR:
      return RegClass::GPR_64;
    case Reg::FPR:
      return RegClass::FLOAT;
    default:
      assert(false);
  }
}

}  // namespace

std::string AtomicOp::reg_type_info_as_string(const TypeState& init_types,
                                              const TypeState& end_types) const {
  std::string result;

  auto read_mask = regs_to_gpr_mask(m_read_regs);
  auto write_mask = regs_to_gpr_mask(m_write_regs);

  result += fmt::format("[{}] -> [{}]", init_types.print_gpr_masked(read_mask),
                        end_types.print_gpr_masked(write_mask));
  //  auto clobber_mask = regs_to_gpr_mask(m_clobber_regs);
  //  if (clobber_mask) {
  //    result += "cl: ";
  //    for (auto& reg : m_clobber_regs) {
  //      result += reg.to_string();
  //      result += ' ';
  //    }
  //  }

  return result;
}

TP_Type SimpleAtom::get_type(const TypeState& input,
                             const Env& env,
                             const DecompilerTypeSystem& dts) const {
  switch (m_kind) {
    case Kind::EMPTY_LIST:
      return TP_Type::make_from_ts("pair");
    case Kind::VARIABLE:
      return input.get(var().reg());
    case Kind::INTEGER_CONSTANT:
      return TP_Type::make_from_integer(m_int);
    case Kind::SYMBOL_PTR:
      if (m_string == "#f") {
        return TP_Type::make_false();
      } else {
        return TP_Type::make_from_ts("symbol");
      }
    case Kind::SYMBOL_VAL: {
      if (m_string == "#f") {
        // if we ever read the false symbol, it should contain the false symbol as its value.
        return TP_Type::make_false();
      } else if (m_string == "__START-OF-TABLE__") {
        // another annoying special case. We have a fake symbol called __START-OF-TABLE__
        // which actually means that you get the first address in the symbol table.
        // it's not really a linked symbol, but the basic op builder represents it as one.
        return TP_Type::make_from_ts(TypeSpec("pointer"));
      }

      // look up the type of the symbol
      auto type = dts.symbol_types.find(m_string);
      if (type == dts.symbol_types.end()) {
        throw std::runtime_error("Don't have the type of symbol " + m_string);
      }

      if (type->second == TypeSpec("type")) {
        // if we get a type by symbol, we should remember which type we got it from.
        return TP_Type::make_type_no_virtual_object(TypeSpec(m_string));
      }

      if (type->second == TypeSpec("function")) {
        lg::warn("Function {} has unknown type", m_string);
      }

      // otherwise, just return a normal typespec
      return TP_Type::make_from_ts(type->second);
    }
    case Kind::STATIC_ADDRESS: {
      auto label = env.file->labels.at(m_int);
      // strings are 16-byte aligned, but functions are 8 byte aligned?
      if ((label.offset & 7) == BASIC_OFFSET) {
        // it's a basic! probably.
        const auto& word =
            env.file->words_by_seg.at(label.target_segment).at((label.offset - 4) / 4);
        if (word.kind == LinkedWord::TYPE_PTR) {
          if (word.symbol_name == "string") {
            return TP_Type::make_from_string(env.file->get_goal_string_by_label(label));
          } else {
            // otherwise, some other static basic.
            return TP_Type::make_from_ts(TypeSpec(word.symbol_name));
          }
        }
      } else if ((label.offset & 7) == PAIR_OFFSET) {
        return TP_Type::make_from_ts(TypeSpec("pair"));
      }
      // throw std::runtime_error("IR_StaticAddress couldn't figure out the type: " + label.name);
      lg::error("IR_StaticAddress doesn't know the type of {}", label.name);
      return TP_Type::make_from_ts("object");
    }
    case Kind::INVALID:
    default:
      assert(false);
  }
  return {};
}

TP_Type SimpleExpression::get_type(const TypeState& input,
                                   const Env& env,
                                   const DecompilerTypeSystem& dts) const {
  switch (m_kind) {
    case Kind::IDENTITY:
      return m_args[0].get_type(input, env, dts);
    case Kind::GPR_TO_FPR: {
      const auto& in_type = input.get(get_arg(0).var().reg());
      if (in_type.is_integer_constant(0)) {
        // GOAL is smart enough to use binary 0b0 as floating point 0.
        return TP_Type::make_from_ts("float");
      }
      return in_type;
    }
    case Kind::FPR_TO_GPR:
    case Kind::DIV_S:
    case Kind::SUB_S:
    case Kind::MUL_S:
    case Kind::ADD_S:
    case Kind::SQRT_S:
    case Kind::ABS_S:
    case Kind::NEG_S:
    case Kind::INT_TO_FLOAT:
    case Kind::MIN_S:
    case Kind::MAX_S:
      return TP_Type::make_from_ts("float");
    case Kind::FLOAT_TO_INT:
      return TP_Type::make_from_ts("int");
    case Kind::ADD:
    case Kind::SUB:
    case Kind::MUL_SIGNED:
    case Kind::DIV_SIGNED:
    case Kind::RIGHT_SHIFT_ARITH:
    case Kind::RIGHT_SHIFT_LOGIC:
    case Kind::MOD_SIGNED:
    case Kind::MIN_SIGNED:
    case Kind::MAX_SIGNED:
    case Kind::OR:
    case Kind::AND:
    case Kind::NOR:
    case Kind::XOR:
    case Kind::LEFT_SHIFT:
    case Kind::MUL_UNSIGNED:
      return get_type_int2(input, env, dts);
    case Kind::NEG:
    case Kind::LOGNOT:
      return get_type_int1(input, env, dts);
    default:
      throw std::runtime_error("Simple expression can't get_type: " +
                               to_form(env.file->labels, env).print());
  }
  return {};
}

TP_Type SimpleExpression::get_type_int1(const TypeState& input,
                                        const Env& env,
                                        const DecompilerTypeSystem& dts) const {
  (void)input;
  (void)dts;
  auto arg_type = m_args[0].get_type(input, env, dts);
  if (is_int_or_uint(dts, arg_type)) {
    switch (m_kind) {
      case Kind::NEG:
        // if we negate a thing, let's just make it a signed integer.
        return TP_Type::make_from_ts(TypeSpec("int"));
        //      case Kind:::
        //        // if we take the absolute value of a thing, just make it signed.
        //        return TP_Type::make_from_ts(TypeSpec("int"));
      case Kind::LOGNOT:
        // otherwise, make it int/uint as needed (this works because we check is_int_or_uint
        // above)
        return TP_Type::make_from_ts(arg_type.typespec());
      default:
        break;
    }
  }

  throw std::runtime_error("IR_IntMath1::get_expression_type case not handled: " +
                           to_form(env.file->labels, env).print() + " " + arg_type.print());
}

/*!
 * Special case for "integer math".
 */
TP_Type SimpleExpression::get_type_int2(const TypeState& input,
                                        const Env& env,
                                        const DecompilerTypeSystem& dts) const {
  auto arg0_type = m_args[0].get_type(input, env, dts);
  auto arg1_type = m_args[1].get_type(input, env, dts);

  // special cases for integers
  switch (m_kind) {
    case Kind::LEFT_SHIFT:
      // multiplication by constant power of two, optimized to a shift.
      if (m_args[1].is_int() && is_int_or_uint(dts, arg0_type)) {
        assert(m_args[1].get_int() >= 0);
        assert(m_args[1].get_int() < 64);
        return TP_Type::make_from_product(1ull << m_args[1].get_int(), is_signed(dts, arg0_type));
      }
      break;

    case Kind::MUL_SIGNED: {
      if (arg0_type.is_integer_constant() && is_int_or_uint(dts, arg1_type)) {
        return TP_Type::make_from_product(arg0_type.get_integer_constant(),
                                          is_signed(dts, arg0_type));
      } else if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
        // signed multiply will always return a signed number.
        return TP_Type::make_from_ts("int");
      }
    } break;

    case Kind::MUL_UNSIGNED: {
      // unsigned multiply will always return a unsigned number.
      return TP_Type::make_from_ts("uint");
    } break;

    case Kind::DIV_SIGNED:
    case Kind::MOD_SIGNED: {
      if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
        // signed division will always return a signed number.
        return TP_Type::make_from_ts("int");
      }
    } break;

    case Kind::ADD:
      if (arg0_type.is_product_with(4) && tc(dts, TypeSpec("type"), arg1_type)) {
        // dynamic access into the method array with shift, add, offset-load
        // no need to track the type because we don't know the method index anyway.
        return TP_Type::make_partial_dyanmic_vtable_access();
      }

      if (arg1_type.is_integer_constant() && is_int_or_uint(dts, arg0_type)) {
        return TP_Type::make_from_integer_constant_plus_var(arg1_type.get_integer_constant(),
                                                            arg0_type.typespec());
      }
      break;

    default:
      break;
  }

  if (arg0_type == arg1_type && is_int_or_uint(dts, arg0_type)) {
    // both are the same type and both are int/uint, so we assume that we're doing integer math.
    // we strip off any weird things like multiplication or integer constant.
    return TP_Type::make_from_ts(arg0_type.typespec());
  }

  if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // usually we would want to use arg0's type as the "winning" type.
    // but we use arg1's if arg0 is an integer constant
    // in either case, strip off weird stuff.
    if (arg0_type.is_integer_constant() && !arg1_type.is_integer_constant()) {
      return TP_Type::make_from_ts(arg1_type.typespec());
    }
    return TP_Type::make_from_ts(arg0_type.typespec());
  }

  if (tc(dts, TypeSpec("binteger"), arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // if arg0 is a binteger, the result is probably a binteger as well
    return TP_Type::make_from_ts("binteger");
  }

  // special cases for non-integers
  if ((arg0_type.typespec() == TypeSpec("object") || arg0_type.typespec() == TypeSpec("pair")) &&
      (arg1_type.is_integer_constant(62) || arg1_type.is_integer_constant(61))) {
    // boxed object tag trick.
    return TP_Type::make_from_ts("int");
  }

  if (m_args[1].is_int() && m_kind == Kind::ADD && arg0_type.kind == TP_Type::Kind::TYPESPEC) {
    // access a field.
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;
    rd_in.stride = 0;
    rd_in.offset = m_args[1].get_int();
    rd_in.base_type = arg0_type.typespec();
    auto rd = dts.ts.reverse_field_lookup(rd_in);

    if (rd.success) {
      return TP_Type::make_from_ts(coerce_to_reg_type(rd.result_type));
    }
  }

  if (m_kind == Kind::ADD && arg0_type.is_product() && arg1_type.kind == TP_Type::Kind::TYPESPEC) {
    return TP_Type::make_object_plus_product(arg1_type.typespec(), arg0_type.get_multiplier());
  }

  if (m_kind == Kind::ADD && arg1_type.is_product() && arg0_type.kind == TP_Type::Kind::TYPESPEC) {
    return TP_Type::make_object_plus_product(arg0_type.typespec(), arg1_type.get_multiplier());
  }

  if (m_kind == Kind::ADD && arg0_type.typespec().base_type() == "pointer" &&
      tc(dts, TypeSpec("integer"), arg1_type)) {
    // plain pointer plus integer = plain pointer
    return TP_Type::make_from_ts(TypeSpec("pointer"));
  }

  if (m_kind == Kind::ADD && arg1_type.typespec().base_type() == "pointer" &&
      tc(dts, TypeSpec("integer"), arg0_type)) {
    // plain pointer plus integer = plain pointer
    return TP_Type::make_from_ts(TypeSpec("pointer"));
  }

  if (tc(dts, TypeSpec("structure"), arg1_type) && !m_args[0].is_int() &&
      is_int_or_uint(dts, arg0_type)) {
    if (arg1_type.typespec() == TypeSpec("symbol") &&
        arg0_type.is_integer_constant(SYM_INFO_OFFSET + POINTER_SIZE)) {
      // symbol -> GOAL String
      // NOTE - the offset doesn't fit in a s16, so it's loaded into a register first.
      // so we expect the arg to be a variable, and the type propagation will figure out the
      // integer constant.
      return TP_Type::make_from_ts(dts.ts.make_pointer_typespec("string"));
    } else {
      // byte access of offset array field trick.
      // arg1 holds a structure.
      // arg0 is an integer in a register.
      return TP_Type::make_object_plus_product(arg1_type.typespec(), 1);
    }
  }

  if (m_kind == Kind::AND) {
    // base case for and. Just get an integer.
    return TP_Type::make_from_ts(TypeSpec("int"));
  }

  if (m_kind == Kind::SUB && tc(dts, TypeSpec("pointer"), arg0_type) &&
      tc(dts, TypeSpec("pointer"), arg1_type)) {
    return TP_Type::make_from_ts(TypeSpec("int"));
  }

  throw std::runtime_error(fmt::format("Can't get_type_int2: {}, args {} and {}",
                                       to_form(env.file->labels, env).print(), arg0_type.print(),
                                       arg1_type.print()));
}

TypeState IR2_BranchDelay::propagate_types(const TypeState& input,
                                           const Env& env,
                                           DecompilerTypeSystem& dts) const {
  TypeState output = input;
  switch (m_kind) {
    case Kind::DSLLV: {
      // I believe this is only used in ash. We ignore the shift amount's type and just look
      // at the input value. If it's a uint/int based type, we just return uint/int (not the type)
      // this will kill any weird stuff like product, etc.
      // if it's not an integer type, it's currently an error.
      auto dst = m_var[0]->reg();
      auto src = m_var[1]->reg();
      if (tc(dts, TypeSpec("uint"), output.get(src))) {
        output.get(dst) = TP_Type::make_from_ts("uint");
      } else if (tc(dts, TypeSpec("int"), output.get(src))) {
        output.get(dst) = TP_Type::make_from_ts("int");
      } else {
        throw std::runtime_error("BranchDelay::type_prop DSLLV for src " + output.get(src).print());
      }
    } break;
    case Kind::NEGATE:
      // to match the behavior in IntMath1, assume signed when negating.
      output.get(m_var[0]->reg()) = TP_Type::make_from_ts("int");
      break;
    case Kind::SET_REG_FALSE:
      output.get(m_var[0]->reg()) = TP_Type::make_false();
      break;
    case Kind::SET_REG_REG:
      output.get(m_var[0]->reg()) = output.get(m_var[1]->reg());
      break;
    case Kind::SET_REG_TRUE:
      output.get(m_var[0]->reg()) = TP_Type::make_from_ts(TypeSpec("symbol"));
      break;
    case Kind::SET_BINTEGER:
      output.get(m_var[0]->reg()) = TP_Type::make_type_no_virtual_object(TypeSpec("binteger"));
      break;
    case Kind::SET_PAIR:
      output.get(m_var[0]->reg()) = TP_Type::make_type_no_virtual_object(TypeSpec("pair"));
      break;
    case Kind::NOP:
    case Kind::NO_DELAY:
      break;
    default:
      throw std::runtime_error("Unhandled branch delay in type_prop: " +
                               to_form(env.file->labels, env).print());
  }
  return output;
}

/////////////////////////////////////////
// Implementations of propagate_types_internal
/////////////////////////////////////////

TypeState AtomicOp::propagate_types(const TypeState& input,
                                    const Env& env,
                                    DecompilerTypeSystem& dts) {
  // do op-specific type propagation
  TypeState result = propagate_types_internal(input, env, dts);
  // clobber
  for (auto reg : m_clobber_regs) {
    result.get(reg) = TP_Type::make_uninitialized();
  }
  return result;
}

TypeState SetVarOp::propagate_types_internal(const TypeState& input,
                                             const Env& env,
                                             DecompilerTypeSystem& dts) {
  TypeState result = input;
  if (m_dst.reg().get_kind() == Reg::FPR && m_src.is_identity() && m_src.get_arg(0).is_int() &&
      m_src.get_arg(0).get_int() == 0) {
    // mtc fX, r0 should be a float type. GOAL was smart enough to do this.
    result.get(m_dst.reg()) = TP_Type::make_from_ts("float");
    return result;
  }
  result.get(m_dst.reg()) = m_src.get_type(input, env, dts);
  return result;
}

TypeState AsmOp::propagate_types_internal(const TypeState& input,
                                          const Env& env,
                                          DecompilerTypeSystem& dts) {
  (void)env;
  (void)dts;
  TypeState result = input;
  if (m_dst.has_value()) {
    auto kind = m_dst->reg().get_kind();
    if (kind == Reg::GPR || kind == Reg::FPR) {
      result.get(m_dst->reg()) = TP_Type::make_from_ts("int");
    }
  }
  return result;
}

TypeState SetVarConditionOp::propagate_types_internal(const TypeState& input,
                                                      const Env& env,
                                                      DecompilerTypeSystem& dts) {
  (void)env;
  (void)dts;
  TypeState result = input;
  result.get(m_dst.reg()) = TP_Type::make_from_ts("symbol");
  return result;
}

TypeState StoreOp::propagate_types_internal(const TypeState& input,
                                            const Env& env,
                                            DecompilerTypeSystem& dts) {
  (void)env;
  (void)dts;
  return input;
}

TP_Type LoadVarOp::get_src_type(const TypeState& input,
                                const Env& env,
                                DecompilerTypeSystem& dts) const {
  if (m_src.is_identity()) {
    auto& src = m_src.get_arg(0);
    if (src.is_static_addr()) {
      if (m_kind == Kind::FLOAT) {
        // assume anything loaded from floating point will be a float.
        return TP_Type::make_from_ts("float");
      }

      if (m_size == 8) {
        // 8 byte integer constants are always loaded from a static pool
        // this could technically hide loading a different type from inside of a static basic.
        return TP_Type::make_from_ts(dts.ts.make_typespec("uint"));
      }
    }
  }

  ///////////////////////////////////////
  // REGISTER + OFFSET (possibly 0)
  ///////////////////////////////////////
  IR2_RegOffset ro;
  if (get_as_reg_offset(m_src, &ro)) {
    auto& input_type = input.get(ro.reg);

    if ((input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD ||
         input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL) &&
        ro.offset >= 16 && (ro.offset & 3) == 0 && m_size == 4 && m_kind == Kind::UNSIGNED) {
      // method get of fixed type
      auto type_name = input_type.get_type_objects_typespec().base_type();
      auto method_id = (ro.offset - 16) / 4;
      auto method_info = dts.ts.lookup_method(type_name, method_id);
      auto method_type = method_info.type.substitute_for_method_call(type_name);
      if (type_name == "object" && method_id == GOAL_NEW_METHOD) {
        // remember that we're an object new.
        return TP_Type::make_object_new(method_type);
      }
      if (method_id == GOAL_NEW_METHOD ||
          input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL) {
        return TP_Type::make_from_ts(method_type);
      } else {
        return TP_Type::make_method(method_type);
      }
    }

    if (input_type.kind == TP_Type::Kind::TYPESPEC && input_type.typespec() == TypeSpec("type") &&
        ro.offset >= 16 && (ro.offset & 3) == 0 && m_size == 4 && m_kind == Kind::UNSIGNED) {
      // method get of an unknown type. We assume the most general "object" type.
      auto method_id = (ro.offset - 16) / 4;
      auto method_info = dts.ts.lookup_method("object", method_id);
      if (method_id != GOAL_NEW_METHOD && method_id != GOAL_RELOC_METHOD) {
        // this can get us the wrong thing for `new` methods.  And maybe relocate?
        return TP_Type::make_from_ts(method_info.type.substitute_for_method_call("object"));
      }
    }

    if (input_type.typespec() == TypeSpec("pointer")) {
      // we got a plain pointer. let's just assume we're loading an integer.
      // perhaps we should disable this feature by default on 4-byte loads if we're getting
      // lots of false positives for loading pointers from plain pointers.

      switch (m_kind) {
        case Kind::UNSIGNED:
          switch (m_size) {
            case 1:
            case 2:
            case 4:
            case 8:
              return TP_Type::make_from_ts(TypeSpec("uint"));
            default:
              break;
          }
          break;
        case Kind::SIGNED:
          switch (m_size) {
            case 1:
            case 2:
            case 4:
            case 8:
              return TP_Type::make_from_ts(TypeSpec("int"));
            default:
              break;
          }
          break;
        case Kind::FLOAT:
          return TP_Type::make_from_ts(TypeSpec("float"));
        default:
          assert(false);
      }
    }

    if (input_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = m_kind == Kind::SIGNED;
      dk.size = m_size;
      rd_in.deref = dk;
      rd_in.base_type = input_type.get_obj_plus_const_mult_typespec();
      rd_in.stride = input_type.get_multiplier();
      rd_in.offset = ro.offset;
      auto rd = dts.ts.reverse_field_lookup(rd_in);

      if (rd.success) {
        //        load_path_set = true;
        //        load_path_addr_of = rd.addr_of;
        //        load_path_base = ro.reg_ir;
        //        for (auto& x : rd.tokens) {
        //          load_path.push_back(x.print());
        //        }
        return TP_Type::make_from_ts(coerce_to_reg_type(rd.result_type));
      }
    }

    if (input_type.kind == TP_Type::Kind::TYPESPEC && ro.offset == -4 && m_kind == Kind::UNSIGNED &&
        m_size == 4 && ro.reg.get_kind() == Reg::GPR) {
      // get type of basic likely, but misrecognized as an object.
      // occurs often in typecase-like structures because other possible types are
      // "stripped".
      //      load_path_base = ro.reg_ir;
      //      load_path_addr_of = false;
      //      load_path.push_back("type");
      //      load_path_set = true;

      return TP_Type::make_type_allow_virtual_object(input_type.typespec().base_type());
    }

    if (input_type.kind == TP_Type::Kind::DYNAMIC_METHOD_ACCESS && ro.offset == 16) {
      // access method vtable. The input is type + (4 * method), and the 16 is the offset
      // of method 0.
      return TP_Type::make_from_ts(TypeSpec("function"));
    }
    // Assume we're accessing a field of an object.
    FieldReverseLookupInput rd_in;
    DerefKind dk;
    dk.is_store = false;
    dk.reg_kind = get_reg_kind(ro.reg);
    dk.sign_extend = m_kind == Kind::SIGNED;
    dk.size = m_size;
    rd_in.deref = dk;
    rd_in.base_type = input_type.typespec();
    rd_in.stride = 0;
    rd_in.offset = ro.offset;
    auto rd = dts.ts.reverse_field_lookup(rd_in);

    // only error on failure if "pair" is disabled. otherwise it might be a pair.
    if (!rd.success && !dts.type_prop_settings.allow_pair) {
      printf("input type is %s, offset is %d, sign %d size %d\n", rd_in.base_type.print().c_str(),
             rd_in.offset, rd_in.deref.value().sign_extend, rd_in.deref.value().size);
      throw std::runtime_error(fmt::format("Could not get type of load: {}. Reverse Deref Failed.",
                                           to_form(env.file->labels, env).print()));
    }

    if (rd.success) {
      //      load_path_set = true;
      //      load_path_addr_of = rd.addr_of;
      //      load_path_base = ro.reg_ir;
      //      for (auto& x : rd.tokens) {
      //        load_path.push_back(x.print());
      //      }
      return TP_Type::make_from_ts(coerce_to_reg_type(rd.result_type));
    }

    // rd failed, try as pair.
    if (dts.type_prop_settings.allow_pair) {
      // we are strict here - only permit pair-type loads from object or pair.
      // object is permitted for stuff like association lists where the car is also a pair.
      if (m_kind == Kind::SIGNED && m_size == 4 &&
          (input_type.typespec() == TypeSpec("object") ||
           input_type.typespec() == TypeSpec("pair"))) {
        // these rules are of course not always correct or the most specific, but it's the best
        // we can do.
        if (ro.offset == 2) {
          // cdr = another pair.
          return TP_Type::make_from_ts(TypeSpec("pair"));
        } else if (ro.offset == -2) {
          // car = some object.
          return TP_Type::make_from_ts(TypeSpec("object"));
        }
      }
    }
  }

  throw std::runtime_error(
      fmt::format("Could not get type of load: {}. ", to_form(env.file->labels, env).print()));

  throw std::runtime_error("LoadVarOp can't get_src_type: " +
                           to_form(env.file->labels, env).print());
}

TypeState LoadVarOp::propagate_types_internal(const TypeState& input,
                                              const Env& env,
                                              DecompilerTypeSystem& dts) {
  TypeState result = input;
  result.get(m_dst.reg()) = get_src_type(input, env, dts);
  return result;
}

TypeState BranchOp::propagate_types_internal(const TypeState& input,
                                             const Env& env,
                                             DecompilerTypeSystem& dts) {
  return m_branch_delay.propagate_types(input, env, dts);
}

TypeState SpecialOp::propagate_types_internal(const TypeState& input,
                                              const Env& env,
                                              DecompilerTypeSystem& dts) {
  (void)env;
  (void)dts;
  // none of these write anything. Suspend clobbers, but this is taken care of automatically
  switch (m_kind) {
    case Kind::NOP:
    case Kind::BREAK:
    case Kind::CRASH:
    case Kind::SUSPEND:
      return input;
    default:
      assert(false);
  }
}

TypeState CallOp::propagate_types_internal(const TypeState& input,
                                           const Env& env,
                                           DecompilerTypeSystem& dts) {
  (void)dts;
  (void)env;
  const Reg::Gpr arg_regs[8] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3,
                                Reg::T0, Reg::T1, Reg::T2, Reg::T3};

  m_is_virtual_method = false;
  TypeState end_types = input;

  auto in_tp = input.get(Register(Reg::GPR, Reg::T9));
  if (in_tp.kind == TP_Type::Kind::OBJECT_NEW_METHOD &&
      !dts.type_prop_settings.current_method_type.empty()) {
    // calling object new method. Set the result to a new object of our type
    end_types.get(Register(Reg::GPR, Reg::V0)) =
        TP_Type::make_from_ts(dts.type_prop_settings.current_method_type);
    // update the call type
    m_call_type = in_tp.get_method_new_object_typespec();
    m_call_type.get_arg(m_call_type.arg_count() - 1) =
        TypeSpec(dts.type_prop_settings.current_method_type);
    m_call_type_set = true;

    m_read_regs.clear();
    m_arg_vars.clear();
    m_read_regs.emplace_back(Reg::GPR, Reg::T9);
    for (int i = 0; i < int(m_call_type.arg_count()) - 1; i++) {
      m_read_regs.emplace_back(Reg::GPR, arg_regs[i]);
      m_arg_vars.push_back(Variable(VariableMode::READ, m_read_regs.back(), m_my_idx));
    }
    return end_types;
  }

  auto in_type = in_tp.typespec();

  if (in_type.base_type() != "function") {
    throw std::runtime_error("Called something that wasn't a function: " + in_type.print());
  }

  if (in_type.arg_count() < 1) {
    throw std::runtime_error("Called a function, but we don't know its type");
  }

  if (in_type.arg_count() == 2 && in_type.get_arg(0) == TypeSpec("_varargs_")) {
    // we're calling a varags function, which is format. We can determine the argument count
    // by looking at the format string, if we can get it.
    auto arg_type = input.get(Register(Reg::GPR, Reg::A1));
    if (arg_type.is_constant_string() || arg_type.is_format_string()) {
      int arg_count = -1;

      if (arg_type.is_constant_string()) {
        auto& str = arg_type.get_string();
        arg_count = dts.get_format_arg_count(str);
      } else {
        // is format string.
        arg_count = arg_type.get_format_string_arg_count();
      }

      TypeSpec format_call_type("function");
      format_call_type.add_arg(TypeSpec("object"));  // destination
      format_call_type.add_arg(TypeSpec("string"));  // format string
      for (int i = 0; i < arg_count; i++) {
        format_call_type.add_arg(TypeSpec("object"));
      }
      format_call_type.add_arg(TypeSpec("object"));
      arg_count += 2;  // for destination and format string.
      m_call_type = format_call_type;
      m_call_type_set = true;

      end_types.get(Register(Reg::GPR, Reg::V0)) = TP_Type::make_from_ts(in_type.last_arg());

      // we can also update register usage here.
      m_read_regs.clear();
      m_arg_vars.clear();
      m_read_regs.emplace_back(Reg::GPR, Reg::T9);
      for (int i = 0; i < arg_count; i++) {
        m_read_regs.emplace_back(Reg::GPR, arg_regs[i]);
        m_arg_vars.push_back(Variable(VariableMode::READ, m_read_regs.back(), m_my_idx));
      }

      return end_types;
    } else {
      throw std::runtime_error("Failed to get string for _varags_ call, got " + arg_type.print());
    }
  }
  // set the call type!
  m_call_type = in_type;
  m_call_type_set = true;

  end_types.get(Register(Reg::GPR, Reg::V0)) = TP_Type::make_from_ts(in_type.last_arg());

  // we can also update register usage here.
  m_read_regs.clear();
  m_arg_vars.clear();
  m_read_regs.emplace_back(Reg::GPR, Reg::T9);

  for (uint32_t i = 0; i < in_type.arg_count() - 1; i++) {
    m_read_regs.emplace_back(Reg::GPR, arg_regs[i]);
    m_arg_vars.push_back(Variable(VariableMode::READ, m_read_regs.back(), m_my_idx));
    if (i == 0 && in_tp.kind == TP_Type::Kind::METHOD) {
      m_read_regs.pop_back();
      m_arg_vars.pop_back();
      m_is_virtual_method = true;
    }
  }

  m_write_regs.clear();
  if (in_type.last_arg() != TypeSpec("none")) {
    m_write_regs.emplace_back(Reg::GPR, Reg::V0);
  }

  return end_types;
}

TypeState ConditionalMoveFalseOp::propagate_types_internal(const TypeState& input,
                                                           const Env& env,
                                                           DecompilerTypeSystem& dts) {
  (void)env;
  (void)dts;
  // these should only appear when paired with a (set! dest #t) earlier, so this expression
  // shouldn't set any types.  Still, double check and override if this fails.
  TypeState result = input;
  if (result.get(m_dst.reg()).typespec() != TypeSpec("symbol")) {
    lg::warn("Conditional Moved #f into something of type {}",
             result.get(m_dst.reg()).typespec().print());
    result.get(m_dst.reg()) = TP_Type::make_from_ts("symbol");
  }

  return result;
}

TypeState FunctionEndOp::propagate_types_internal(const TypeState& input,
                                                  const Env&,
                                                  DecompilerTypeSystem&) {
  return input;
}

void FunctionEndOp::mark_function_as_no_return_value() {
  m_read_regs.clear();
  m_function_has_return_value = false;
}

}  // namespace decompiler