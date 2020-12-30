#include "IR.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "third-party/fmt/core.h"
#include "common/goos/Object.h"
#include "decompiler/util/TP_Type.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace {
// bool is_plain_type(const TP_Type& type, const TypeSpec& ts) {
//  return type.as_typespec() == ts;
//}
//
// bool is_integer_type(const TP_Type& type) {
//  return is_plain_type(type, TypeSpec("int")) || is_plain_type(type, TypeSpec("uint"));
//}
//
///*!
// * If first arg is unsigned, make the result unsigned.
// * Otherwise signed. This is the default GOAL behavior I guess.
// * This strips away any fancy stuff like [uint x 4]
// */
// TP_Type get_int_type(const TP_Type& one) {
//  if (is_plain_type(one, TypeSpec("uint"))) {
//    return TP_Type(one.as_typespec());
//  } else {
//    return TP_Type(TypeSpec("int"));
//  }
//}
//

bool tc(DecompilerTypeSystem& dts, const TypeSpec& expected, const TP_Type& actual) {
  return dts.ts.typecheck(expected, actual.typespec(), "", false, false);
}

bool is_int_or_uint(DecompilerTypeSystem& dts, const TP_Type& type) {
  return tc(dts, TypeSpec("int"), type) || tc(dts, TypeSpec("uint"), type);
}

struct RegOffset {
  Register reg;
  std::shared_ptr<IR_Register> reg_ir;
  int offset;
};

bool get_as_reg_offset(const IR* ir, RegOffset* out) {
  auto as_reg = dynamic_cast<const IR_Register*>(ir);
  if (as_reg) {
    out->reg = as_reg->reg;
    out->reg_ir = std::make_shared<IR_Register>(*as_reg);
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
      out->reg_ir = std::dynamic_pointer_cast<IR_Register>(as_math->arg0);
      return true;
    }
  }
  return false;
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

/*!
 * Default implementation of propagate types, throw an NYI error.
 */
void IR_Atomic::propagate_types(const TypeState& input,
                                const LinkedObjectFile& file,
                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  throw std::runtime_error(
      fmt::format("Could not propagate types for {}, not yet implemented", print(file)));
}

/*!
 * Default implementation of get_expression_type.
 */
TP_Type IR::get_expression_type(const TypeState& input,
                                const LinkedObjectFile& file,
                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  throw std::runtime_error(
      fmt::format("Could not get expression types for {}, not yet implemented", print(file)));
}

/*!
 * Propagate types through a set! operation.
 */
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
      // all these should set a register.
      auto as_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(as_reg);
      // get the type of the source,
      auto t = src->get_expression_type(input, file, dts);
      // set the type of the register.
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

/*!
 * Get the type of a register.
 */
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
  clear_load_path();

  ////////////////////
  // STATIC
  ////////////////////
  auto as_static = dynamic_cast<IR_StaticAddress*>(location.get());
  if (as_static) {
    // todo - we should map out static data and use an actual type system lookup to figure this out.
    // but for now, this is probably good enough.
    if (kind == FLOAT) {
      // loading static data with a FLOAT kind load (lwc1), assume result is a float.
      return TP_Type::make_from_typespec(dts.ts.make_typespec("float"));
    }

    if (size == 8) {
      // 8 byte integer constants are always loaded from a static pool
      // this could technically hide loading a different type from inside of a static basic.
      return TP_Type::make_from_typespec(dts.ts.make_typespec("uint"));
    }
  }

  ///////////////////////////////////////
  // REGISTER + OFFSET (possibly 0)
  ///////////////////////////////////////
  RegOffset ro;
  if (get_as_reg_offset(location.get(), &ro)) {
    auto& input_type = input.get(ro.reg);

    if (input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD && ro.offset >= 16 &&
        (ro.offset & 3) == 0 && size == 4 && kind == UNSIGNED) {
      // method get of fixed type
      auto type_name = input_type.get_type_objects_typespec().base_type();
      auto method_id = (ro.offset - 16) / 4;
      auto method_info = dts.ts.lookup_method(type_name, method_id);
      auto method_type = method_info.type.substitute_for_method_call(type_name);
      if (type_name == "object" && method_id == GOAL_NEW_METHOD) {
        // remember that we're an object new.
        return TP_Type::make_object_new(method_type);
      }
      return TP_Type::make_from_typespec(method_type);
    }

    if (input_type.kind == TP_Type::Kind::TYPESPEC && input_type.typespec() == TypeSpec("type") &&
        ro.offset >= 16 && (ro.offset & 3) == 0 && size == 4 && kind == UNSIGNED) {
      // method get of an unknown type. We assume the most general "object" type.
      auto method_id = (ro.offset - 16) / 4;
      auto method_info = dts.ts.lookup_method("object", method_id);
      if (method_id != GOAL_NEW_METHOD && method_id != GOAL_RELOC_METHOD) {
        // this can get us the wrong thing for `new` methods.  And maybe relocate?
        return TP_Type::make_from_typespec(method_info.type.substitute_for_method_call("object"));
      }
    }

    if (input_type.typespec() == TypeSpec("pointer")) {
      // we got a plain pointer. let's just assume we're loading an integer.
      // perhaps we should disable this feature by default on 4-byte loads if we're getting
      // lots of false positives for loading pointers from plain pointers.

      switch (kind) {
        case UNSIGNED:
          switch (size) {
            case 1:
            case 2:
            case 4:
            case 8:
              return TP_Type::make_from_typespec(TypeSpec("uint"));
            default:
              break;
          }
          break;
        case SIGNED:
          switch (size) {
            case 1:
            case 2:
            case 4:
            case 8:
              return TP_Type::make_from_typespec(TypeSpec("int"));
            default:
              break;
          }
          break;
        case FLOAT:
          return TP_Type::make_from_typespec(TypeSpec("float"));
        default:
          assert(false);
      }
    }

    if (input_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = kind == SIGNED;
      dk.size = size;
      rd_in.deref = dk;
      rd_in.base_type = input_type.get_obj_plus_const_mult_typespec();
      rd_in.stride = input_type.get_multiplier();
      rd_in.offset = ro.offset;
      auto rd = dts.ts.reverse_field_lookup(rd_in);

      if (rd.success) {
        load_path_set = true;
        load_path_addr_of = rd.addr_of;
        load_path_base = ro.reg_ir;
        for (auto& x : rd.tokens) {
          load_path.push_back(x.print());
        }
        return TP_Type::make_from_typespec(coerce_to_reg_type(rd.result_type));
      }
    }

    if (input_type.kind == TP_Type::Kind::TYPESPEC && ro.offset == -4 && kind == UNSIGNED &&
        size == 4 && ro.reg.get_kind() == Reg::GPR) {
      // get type of basic likely, but misrecognized as an object.
      // occurs often in typecase-like structures because other possible types are
      // "stripped".
      load_path_base = ro.reg_ir;
      load_path_addr_of = false;
      load_path.push_back("type");
      load_path_set = true;

      return TP_Type::make_type_object(input_type.typespec().base_type());
    }
    //
    //      if (input_type.as_typespec() == TypeSpec("object") && ro.offset == -4 && kind ==
    //      UNSIGNED
    //      &&
    //          size == 4 && ro.reg.get_kind() == Reg::GPR) {
    //        // get type of basic likely, but misrecognized as an object.
    //        // occurs often in typecase-like structures because other possible types are
    //        "stripped". return TP_Type(TypeSpec("type"));
    //      }
    //

    if (input_type.kind == TP_Type::Kind::DYNAMIC_METHOD_ACCESS && ro.offset == 16) {
      // access method vtable. The input is type + (4 * method), and the 16 is the offset
      // of method 0.
      return TP_Type::make_from_typespec(TypeSpec("function"));
    }
    // Assume we're accessing a field of an object.
    FieldReverseLookupInput rd_in;
    DerefKind dk;
    dk.is_store = false;
    dk.reg_kind = get_reg_kind(ro.reg);
    dk.sign_extend = kind == SIGNED;
    dk.size = size;
    rd_in.deref = dk;
    rd_in.base_type = input_type.typespec();
    rd_in.stride = 0;
    rd_in.offset = ro.offset;
    auto rd = dts.ts.reverse_field_lookup(rd_in);

    // only error on failure if "pair" is disabled. otherwise it might be a pair.
    if (!rd.success && !dts.type_prop_settings.allow_pair) {
      printf("input type is %s, offset is %d, sign %d size %d\n", rd_in.base_type.print().c_str(),
             rd_in.offset, rd_in.deref.value().sign_extend, rd_in.deref.value().size);
      throw std::runtime_error(
          fmt::format("Could not get type of load: {}. Reverse Deref Failed.", print(file)));
    }

    if (rd.success) {
      load_path_set = true;
      load_path_addr_of = rd.addr_of;
      load_path_base = ro.reg_ir;
      for (auto& x : rd.tokens) {
        load_path.push_back(x.print());
      }
      return TP_Type::make_from_typespec(coerce_to_reg_type(rd.result_type));
    }

    // rd failed, try as pair.
    if (dts.type_prop_settings.allow_pair) {
      // we are strict here - only permit pair-type loads from object or pair.
      // object is permitted for stuff like association lists where the car is also a pair.
      if (kind == SIGNED && size == 4 &&
          (input_type.typespec() == TypeSpec("object") ||
           input_type.typespec() == TypeSpec("pair"))) {
        // these rules are of course not always correct or the most specific, but it's the best
        // we can do.
        if (ro.offset == 2) {
          // cdr = another pair.
          return TP_Type::make_from_typespec(TypeSpec("pair"));
        } else if (ro.offset == -2) {
          // car = some object.
          return TP_Type::make_from_typespec(TypeSpec("object"));
        }
      }
    }
  }

  throw std::runtime_error(fmt::format("Could not get type of load: {}. Not handled: {}",
                                       print(file), location->print(file)));
}

TP_Type IR_FloatMath2::get_expression_type(const TypeState& input,
                                           const LinkedObjectFile& file,
                                           DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;

  // regardless of input types, the output is going to be a float.
  // todo - if we ever support meters we should do something better here.
  switch (kind) {
    case DIV:
    case MUL:
    case ADD:
    case SUB:
    case MIN:
    case MAX:
      return TP_Type::make_from_typespec(dts.ts.make_typespec("float"));
    default:
      assert(false);
  }
}

TP_Type IR_FloatMath1::get_expression_type(const TypeState& input,
                                           const LinkedObjectFile& file,
                                           DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  // FLOAT_TO_INT, INT_TO_FLOAT, ABS, NEG, SQRT
  switch (kind) {
    case FLOAT_TO_INT:
      return TP_Type::make_from_typespec(TypeSpec("int"));
    case INT_TO_FLOAT:
    case ABS:
    case NEG:
    case SQRT:
      return TP_Type::make_from_typespec(TypeSpec("float"));
    default:
      assert(false);
  }
}

TP_Type IR_IntMath2::get_expression_type(const TypeState& input,
                                         const LinkedObjectFile& file,
                                         DecompilerTypeSystem& dts) {
  auto arg0_type = arg0->get_expression_type(input, file, dts);
  auto arg1_type = arg1->get_expression_type(input, file, dts);

  // special cases for integers
  switch (kind) {
    case LEFT_SHIFT:
      // multiply!
      {
        auto as_const = dynamic_cast<IR_IntegerConstant*>(arg1.get());
        if (as_const && is_int_or_uint(dts, arg0_type)) {
          assert(as_const->value >= 0);
          assert(as_const->value < 64);
          return TP_Type::make_from_product((1ull << as_const->value));
        }
        break;
      }

    case MUL_SIGNED: {
      if (arg0_type.is_integer_constant() && is_int_or_uint(dts, arg1_type)) {
        return TP_Type::make_from_product(arg0_type.get_integer_constant());
      }
    } break;

    case ADD:
      if (arg0_type.is_product_with(4) && tc(dts, TypeSpec("type"), arg1_type)) {
        // dynamic access into the method array with shift, add, offset-load
        // no need to track the type because we don't know the method index anyway.
        return TP_Type::make_partial_dyanmic_vtable_access();
      }
      break;

    default:
      break;
  }

  if (arg0_type == arg1_type && is_int_or_uint(dts, arg0_type)) {
    // both are the same type and both are int/uint, so we assume that we're doing integer math.
    // we strip off any weird things like multiplication or integer constant.
    return TP_Type::make_from_typespec(arg0_type.typespec());
  }

  if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // usually we would want to use arg0's type as the "winning" type.
    // but we use arg1's if arg0 is an integer constant
    // in either case, strip off weird stuff.
    if (arg0_type.is_integer_constant() && !arg1_type.is_integer_constant()) {
      return TP_Type::make_from_typespec(arg1_type.typespec());
    }
    return TP_Type::make_from_typespec(arg0_type.typespec());
  }

  if (tc(dts, TypeSpec("binteger"), arg0_type) && is_int_or_uint(dts, arg1_type)) {
    return TP_Type::make_from_typespec(TypeSpec("binteger"));
  }

  // special cases for non-integers
  if ((arg0_type.typespec() == TypeSpec("object") || arg0_type.typespec() == TypeSpec("pair")) &&
      (arg1_type.is_integer_constant(62) || arg1_type.is_integer_constant(61))) {
    // boxed object tag trick.
    return TP_Type::make_from_typespec(TypeSpec("int"));
  }

  //
  //  if (is_integer_type(arg0_type) && is_integer_type(arg1_type)) {
  //    // case where both arguments are integers.
  //    // in this case we assume we're actually doing math.
  //    switch (kind) {
  //      case ADD:
  //      case SUB:
  //      case AND:
  //      case OR:
  //      case NOR:
  //      case XOR:
  //        // we don't know if we're signed or unsigned. so let's just go with the first type.
  //        return get_int_type(arg0_type);
  //      case MUL_SIGNED:
  //      case DIV_SIGNED:
  //      case RIGHT_SHIFT_ARITH:
  //      case MOD_SIGNED:
  //      case MIN_SIGNED:
  //      case MAX_SIGNED:
  //        // result is going to be signed, regardless of inputs.
  //        return TP_Type(TypeSpec("int"));
  //
  //      case MUL_UNSIGNED:
  //      case RIGHT_SHIFT_LOGIC:
  //        // result is going to be unsigned, regardless of inputs.
  //        return TP_Type(TypeSpec("uint"));
  //
  //      case LEFT_SHIFT: {
  //        // multiply!
  //        auto as_const = dynamic_cast<IR_IntegerConstant*>(arg1.get());
  //        if (as_const) {
  //          // shift by constant integer. could be accessing the method array.
  //          TP_Type result;
  //          result.kind = TP_Type::PRODUCT;
  //          result.ts = get_int_type(arg0_type).ts;
  //          result.multiplier = (1 << as_const->value);
  //          return result;
  //        } else {
  //          // normal variable shift.
  //          return get_int_type(arg0_type);
  //        }
  //      }
  //      default:
  //        break;
  //    }
  //  }
  //
  //
  auto a1_const = dynamic_cast<IR_IntegerConstant*>(arg1.get());
  if (a1_const && kind == ADD && arg0_type.kind == TP_Type::Kind::TYPESPEC) {
    // access a field.
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;
    rd_in.stride = 0;
    rd_in.offset = a1_const->value;
    rd_in.base_type = arg0_type.typespec();
    auto rd = dts.ts.reverse_field_lookup(rd_in);

    if (rd.success) {
      // todo, load path.
      return TP_Type::make_from_typespec(coerce_to_reg_type(rd.result_type));
    }
  }
  //
  //  if (kind == ADD && is_integer_type(arg0_type) && arg1_type.kind == TP_Type::OBJECT_OF_TYPE)
  //  {
  //    // product + object with multiplier 1 (access array of bytes for example)
  //    TP_Type result;
  //    result.kind = TP_Type::OBJ_PLUS_PRODUCT;
  //    result.ts = arg1_type.as_typespec();
  //    result.multiplier = 1;
  //    return result;
  //  }
  //
  if (kind == ADD && arg0_type.is_product() && arg1_type.kind == TP_Type::Kind::TYPESPEC) {
    return TP_Type::make_object_plus_product(arg1_type.typespec(), arg0_type.get_multiplier());
  }

  if (kind == ADD && arg1_type.is_product() && arg0_type.kind == TP_Type::Kind::TYPESPEC) {
    return TP_Type::make_object_plus_product(arg0_type.typespec(), arg1_type.get_multiplier());
  }

  if (kind == ADD && arg0_type.typespec().base_type() == "pointer" &&
      tc(dts, TypeSpec("integer"), arg1_type)) {
    // plain pointer plus integer = plain pointer
    return TP_Type::make_from_typespec(TypeSpec("pointer"));
  }

  if (kind == ADD && arg1_type.typespec().base_type() == "pointer" &&
      tc(dts, TypeSpec("integer"), arg0_type)) {
    // plain pointer plus integer = plain pointer
    return TP_Type::make_from_typespec(TypeSpec("pointer"));
  }

  if (tc(dts, TypeSpec("structure"), arg1_type) && !dynamic_cast<IR_IntegerConstant*>(arg0.get()) &&
      is_int_or_uint(dts, arg0_type)) {
    if (arg1_type.typespec() == TypeSpec("symbol") &&
        arg0_type.is_integer_constant(SYM_INFO_OFFSET + POINTER_SIZE)) {
      // symbol -> GOAL String
      return TP_Type::make_from_typespec(dts.ts.make_pointer_typespec("string"));
    } else {
      // byte access of offset array field trick.
      // arg1 holds a structure.
      // arg0 is an integer in a register.
      return TP_Type::make_object_plus_product(arg1_type.typespec(), 1);
    }
  }

  if (kind == AND) {
    // base case for and. Just get an integer.
    return TP_Type::make_from_typespec(TypeSpec("int"));
  }

  //
  //  if (kind == ADD &&
  //      dts.ts.typecheck(TypeSpec("pointer"), arg0_type.as_typespec(), "", false, false) &&
  //      is_integer_type(arg1_type)) {
  //    return arg0_type;
  //  }
  //
  //  if ((kind == ADD || kind == AND) &&
  //      dts.ts.typecheck(TypeSpec("pointer"), arg1_type.as_typespec(), "", false, false) &&
  //      is_integer_type(arg0_type)) {
  //    return arg1_type;
  //  }
  //
  //  if (kind == ADD &&
  //      dts.ts.typecheck(TypeSpec("binteger"), arg0_type.as_typespec(), "", false, false) &&
  //      is_integer_type(arg1_type)) {
  //    return arg0_type;
  //  }
  //
  if (kind == SUB && tc(dts, TypeSpec("pointer"), arg0_type) &&
      tc(dts, TypeSpec("pointer"), arg1_type)) {
    return TP_Type::make_from_typespec(TypeSpec("int"));
  }

  throw std::runtime_error(
      fmt::format("Can't get_expression_type on this IR_IntMath2: {}, args {} and {}", print(file),
                  arg0_type.print(), arg1_type.print()));
}

void BranchDelay::type_prop(TypeState& output,
                            const LinkedObjectFile& file,
                            DecompilerTypeSystem& dts) {
  //  (void)dts;
  switch (kind) {
    case DSLLV: {
      // I believe this is only used in ash. We ignore the shift amount's type and just look
      // at the input value. If it's a uint/int based type, we just return uint/int (not the type)
      // this will kill any weird stuff like product, etc.
      // if it's not an integer type, it's currently an error.
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      auto src = dynamic_cast<IR_Register*>(source.get());
      assert(src);
      if (tc(dts, TypeSpec("uint"), output.get(src->reg))) {
        output.get(dst->reg) = TP_Type::make_from_typespec(TypeSpec("uint"));
      } else if (tc(dts, TypeSpec("int"), output.get(src->reg))) {
        output.get(dst->reg) = TP_Type::make_from_typespec(TypeSpec("int"));
      } else {
        throw std::runtime_error("BranchDelay::type_prop DSLLV for src " +
                                 output.get(src->reg).print());
      }
    } break;
    case NEGATE: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      // to match the behavior in IntMath1, assume signed when negating.
      output.get(dst->reg) = TP_Type::make_from_typespec(TypeSpec("int"));
    } break;
    case SET_REG_FALSE: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg) = TP_Type::make_false();
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
      output.get(dst->reg) = TP_Type::make_from_typespec(TypeSpec("symbol"));
    } break;

    case SET_BINTEGER: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg) = TP_Type::make_type_object(TypeSpec("binteger"));
    } break;

    case SET_PAIR: {
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      output.get(dst->reg) = TP_Type::make_type_object(TypeSpec("pair"));
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
  if (is_int_or_uint(dts, arg_type)) {
    switch (kind) {
      case NEG:
        // if we negate a thing, let's just make it a signed integer.
        return TP_Type::make_from_typespec(TypeSpec("int"));
      case ABS:
        // if we take the absolute value of a thing, just make it signed.
        return TP_Type::make_from_typespec(TypeSpec("int"));
      case NOT:
        // otherwise, make it int/uint as needed (this works because we check is_int_or_uint
        // above)
        return TP_Type::make_from_typespec(arg_type.typespec());
    }
  }

  throw std::runtime_error("IR_IntMath1::get_expression_type case not handled: " +
                           to_form(file).print() + " " + arg_type.print());
}

TP_Type IR_SymbolValue::get_expression_type(const TypeState& input,
                                            const LinkedObjectFile& file,
                                            DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  if (name == "#f") {
    // if we ever read the false symbol, it should contain the false symbol as its value.
    return TP_Type::make_false();
  } else if (name == "__START-OF-TABLE__") {
    // another annoying special case. We have a fake symbol called __START-OF-TABLE__
    // which actually means that you get the first address in the symbol table.
    // it's not really a linked symbol, but the basic op builder represents it as one.
    return TP_Type::make_from_typespec(TypeSpec("pointer"));
  }

  // look up the type of the symbol
  auto type = dts.symbol_types.find(name);
  if (type == dts.symbol_types.end()) {
    throw std::runtime_error("Don't have the type of symbol " + name);
  }

  if (type->second == TypeSpec("type")) {
    // if we get a type by symbol, we should remember which type we got it from.
    return TP_Type::make_type_object(TypeSpec(name));
  }

  // otherwise, just return a normal typespec
  return TP_Type::make_from_typespec(type->second);
}

TP_Type IR_Symbol::get_expression_type(const TypeState& input,
                                       const LinkedObjectFile& file,
                                       DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  if (name == "#f") {
    return TP_Type::make_false();
  }

  return TP_Type::make_from_typespec(TypeSpec("symbol"));
}

TP_Type IR_IntegerConstant::get_expression_type(const TypeState& input,
                                                const LinkedObjectFile& file,
                                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  return TP_Type::make_from_integer(value);
}

TP_Type IR_Compare::get_expression_type(const TypeState& input,
                                        const LinkedObjectFile& file,
                                        DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  // really a boolean.
  return TP_Type::make_from_typespec(TypeSpec("symbol"));
}

void IR_Nop_Atomic::propagate_types(const TypeState& input,
                                    const LinkedObjectFile& file,
                                    DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  end_types = input;
}

void IR_Suspend_Atomic::propagate_types(const TypeState& input,
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
  const Reg::Gpr arg_regs[8] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3,
                                Reg::T0, Reg::T1, Reg::T2, Reg::T3};
  const Reg::Gpr goal_function_clobber_regs[] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3,
                                                 Reg::T0, Reg::T1, Reg::T2, Reg::T3,
                                                 Reg::T4, Reg::V1, Reg::T9};
  end_types = input;

  auto in_tp = input.get(Register(Reg::GPR, Reg::T9));
  if (in_tp.kind == TP_Type::Kind::OBJECT_NEW_METHOD &&
      !dts.type_prop_settings.current_method_type.empty()) {
    // calling object new method. Set the result to a new object of our type
    end_types.get(Register(Reg::GPR, Reg::V0)) =
        TP_Type::make_from_typespec(dts.type_prop_settings.current_method_type);
    // update the call type
    call_type = in_tp.get_method_new_object_typespec();
    call_type.get_arg(call_type.arg_count() - 1) =
        TypeSpec(dts.type_prop_settings.current_method_type);
    call_type_set = true;
    return;
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
      call_type = format_call_type;
      call_type_set = true;

      end_types.get(Register(Reg::GPR, Reg::V0)) = TP_Type::make_from_typespec(in_type.last_arg());

      // we can also update register usage here.
      read_regs.clear();
      read_regs.emplace_back(Reg::GPR, Reg::T9);
      for (int i = 0; i < arg_count; i++) {
        read_regs.emplace_back(Reg::GPR, arg_regs[i]);
      }

      for (auto reg : goal_function_clobber_regs) {
        end_types.get(Register(Reg::GPR, reg)) = TP_Type::make_uninitialized();
      }
      return;
    } else {
      throw std::runtime_error("Failed to get string for _varags_ call, got " + arg_type.print());
    }
  }
  // set the call type!
  call_type = in_type;
  call_type_set = true;

  end_types.get(Register(Reg::GPR, Reg::V0)) = TP_Type::make_from_typespec(in_type.last_arg());

  // we can also update register usage here.
  read_regs.clear();
  read_regs.emplace_back(Reg::GPR, Reg::T9);

  for (uint32_t i = 0; i < in_type.arg_count() - 1; i++) {
    read_regs.emplace_back(Reg::GPR, arg_regs[i]);
  }
  for (auto reg : goal_function_clobber_regs) {
    end_types.get(Register(Reg::GPR, reg)) = TP_Type::make_uninitialized();
  }
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
  // todo - we should map out static data and use a real type system lookup here.

  auto label = file.labels.at(label_id);
  // strings are 16-byte aligned, but functions are 8 byte aligned?
  if ((label.offset & 7) == BASIC_OFFSET) {
    // it's a basic! probably.
    const auto& word = file.words_by_seg.at(label.target_segment).at((label.offset - 4) / 4);
    if (word.kind == LinkedWord::TYPE_PTR) {
      if (word.symbol_name == "string") {
        return TP_Type::make_from_string(file.get_goal_string_by_label(label));
      } else {
        // otherwise, some other static basic.
        return TP_Type::make_from_typespec(TypeSpec(word.symbol_name));
      }
    }
  } else if ((label.offset & 7) == PAIR_OFFSET) {
    return TP_Type::make_from_typespec(TypeSpec("pair"));
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
      end_types.get(dst_reg->reg) = TP_Type::make_from_typespec(TypeSpec("uint"));
    }
  }
}

void IR_Breakpoint_Atomic::propagate_types(const TypeState& input,
                                           const LinkedObjectFile& file,
                                           DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  end_types = input;
}

TP_Type IR_EmptyPair::get_expression_type(const TypeState& input,
                                          const LinkedObjectFile& file,
                                          DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  // GOAL's empty pair is actually a pair type, containing the empty pair as the car and cdr
  return TP_Type::make_from_typespec(TypeSpec("pair"));
}

TP_Type IR_CMoveF::get_expression_type(const TypeState& input,
                                       const LinkedObjectFile& file,
                                       DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;
  (void)dts;
  return TP_Type::make_from_typespec(TypeSpec("symbol"));
}