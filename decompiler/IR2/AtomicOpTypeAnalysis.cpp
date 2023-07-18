#include "AtomicOp.h"

#include "common/log/log.h"
#include "common/type_system/state.h"
#include "common/util/BitUtils.h"

#include "decompiler/IR2/bitfields.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/TP_Type.h"
#include "decompiler/util/type_utils.h"

#include "third-party/fmt/core.h"

namespace decompiler {

namespace {
bool tc(const DecompilerTypeSystem& dts, const TypeSpec& expected, const TP_Type& actual) {
  return dts.ts.tc(expected, actual.typespec());
}

bool is_int_or_uint(const DecompilerTypeSystem& dts, const TP_Type& type) {
  return tc(dts, TypeSpec("integer"), type) || tc(dts, TypeSpec("uint"), type);
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
      ASSERT(false);
      return RegClass::INVALID;
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
        return TP_Type::make_symbol(m_string);
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
      } else if (m_string == "enter-state") {
        return TP_Type::make_enter_state();
      } else if (m_string == "run-function-in-process") {
        return TP_Type::make_run_function_in_process_function();
      } else if (m_string == "set-to-run" && env.func->name() != "enter-state") {
        return TP_Type::make_set_to_run_function();
      }

      // look up the type of the symbol
      auto type = dts.symbol_types.find(m_string);
      if (type == dts.symbol_types.end()) {
        throw std::runtime_error("Do not have the type of symbol " + m_string);
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
      const auto& hint = env.file->label_db->lookup(m_int);
      if (!hint.known) {
        throw std::runtime_error(
            fmt::format("Label {} was unknown in AtomicOpTypeAnalysis.", hint.name));
      }
      if (hint.result_type.base_type() == "string") {
        // we special case strings because the type pass will constant propagate them as needed to
        // figure out the argument count for calls for format.
        auto label = env.file->labels.at(m_int);
        return TP_Type::make_from_string(env.file->get_goal_string_by_label(label));
      }
      if (hint.is_value) {
        // todo, do we really need this? should be use something else instead?
        // return TP_Type::make_from_ts(TypeSpec("pointer", {hint.result_type}));
        return TP_Type::make_label_addr(m_int);
      } else {
        return TP_Type::make_from_ts(hint.result_type);
      }

      /*
      auto label = env.file->labels.at(m_int);
      // strings are 16-byte aligned, but functions are 8 byte aligned?
      if ((label.offset & 7) == BASIC_OFFSET) {
        // it's a basic! probably.
        const auto& word =
            env.file->words_by_seg.at(label.target_segment).at((label.offset - 4) / 4);
        if (word.kind == LinkedWord::TYPE_PTR) {
          if (word.symbol_name == "string") {
            return TP_Type::make_from_string(env.file->get_goal_string_by_label(label));
          } else if (word.symbol_name == "function") {
            // let's see if the user marked this as a lambda and if we can get a more specific type.
            auto hint_kv = env.label_types().find(label.name);
            if (hint_kv != env.label_types().end() && hint_kv->second.type_name == "_lambda_") {
              auto func = env.file->try_get_function_at_label(m_int);
              if (func) {
                return TP_Type::make_from_ts(func->type);
              }
            }
          }
          // otherwise, some other static basic.
          return TP_Type::make_from_ts(TypeSpec(word.symbol_name));
        }
      } else if ((label.offset & 7) == PAIR_OFFSET) {
        return TP_Type::make_from_ts(TypeSpec("pair"));
      }

      auto hint_kv = env.label_types().find(label.name);
      if (hint_kv != env.label_types().end()) {
        return TP_Type::make_from_ts(dts.parse_type_spec(hint_kv->second.type_name));
      }
      // todo: should we take out this warning?
      lg::warn("IR_StaticAddress does not know the type of {}", label.name);
      return TP_Type::make_label_addr(m_int);
       */
    }
    case Kind::INVALID:
    default:
      ASSERT(false);
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
      // new for jak 2:
      if (env.version == GameVersion::Jak2 && in_type.is_integer_constant() &&
          (s64)((s32)in_type.get_integer_constant()) == (s64)in_type.get_integer_constant()) {
        return TP_Type::make_from_ts("float");
      }
      return in_type;
    }
    case Kind::FPR_TO_GPR:
      return m_args[0].get_type(input, env, dts);
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
    case Kind::PCPYLD:
      return get_type_int2(input, env, dts);
    case Kind::NEG:
    case Kind::LOGNOT:
      return get_type_int1(input, env, dts);
    case Kind::DIV_UNSIGNED:
    case Kind::MOD_UNSIGNED:
      return TP_Type::make_from_ts("uint");
    case Kind::VECTOR_PLUS:
    case Kind::VECTOR_MINUS:
    case Kind::VECTOR_CROSS:
      return TP_Type::make_from_ts("vector");
    case Kind::VECTOR_FLOAT_PRODUCT:
      return TP_Type::make_from_ts("vector");
    case Kind::SUBU_L32_S7:
      return TP_Type::make_from_ts("int");
    case Kind::VECTOR_3_DOT:
    case Kind::VECTOR_4_DOT:
    case Kind::VECTOR_LENGTH:
      return TP_Type::make_from_ts("float");
    default:
      throw std::runtime_error("Simple expression cannot get_type: " +
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

namespace {
/*!
 * Get the type of sp + offset.
 */
TP_Type get_stack_type_at_constant_offset(int offset,
                                          const Env& env,
                                          const DecompilerTypeSystem& dts,
                                          const TypeState& types) {
  (void)dts;

  // first look for a stack structure
  for (auto& structure : env.stack_structure_hints()) {
    if (offset < structure.hint.stack_offset ||
        offset >= (structure.hint.stack_offset + structure.size)) {
      continue;  // reject, it isn't in this variable
    }

    if (offset == structure.hint.stack_offset) {
      // special case just getting the variable

      return TP_Type::make_from_ts(coerce_to_reg_type(structure.ref_type));
    }

    // Note: GOAL doesn't seem to constant propagate memory access on the stack, so the code
    // below should never be needed.
    /*
    // yes, it is in the variable!
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;                     // not a deref
    rd_in.stride = 0;                               // not a strided access
    rd_in.offset = offset - var.hint.stack_offset;  // offset into this var
    rd_in.base_type = var.ref_type;                 // use ref type for ptr.
    auto rd = dts.ts.reverse_field_lookup(rd_in);
    if (rd.success) {
      auto result = TP_Type::make_from_ts(coerce_to_reg_type(rd.result_type));
      lg::print("Matched a stack variable! {}\n", result.print());
      return result;
    }
     */
    // if we fail, keep trying others. This lets us have overlays in stack memory.
  }

  // look for a stack variable
  auto kv = types.spill_slots.find(offset);
  if (kv != types.spill_slots.end()) {
    return TP_Type::make_from_ts(TypeSpec("pointer", {kv->second.typespec()}));
  }

  throw std::runtime_error(
      fmt::format("Failed to find a stack variable or structure at offset {}", offset));
}

}  // namespace

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
        ASSERT(m_args[1].get_int() >= 0);
        ASSERT(m_args[1].get_int() < 64);
        // this could be a bitfield access or a multiply.
        // we pick bitfield access if the parent is a bitfield.
        if (dynamic_cast<BitFieldType*>(dts.ts.lookup_type(arg0_type.typespec()))) {
          return TP_Type::make_from_left_shift_bitfield(arg0_type.typespec(), m_args[1].get_int(),
                                                        false);
        } else if (arg0_type.kind == TP_Type::Kind::PCPYUD_BITFIELD) {
          return TP_Type::make_from_left_shift_bitfield(arg0_type.get_bitfield_type(),
                                                        m_args[1].get_int(), true);
        } else {
          return TP_Type::make_from_product(1ull << m_args[1].get_int(), is_signed(dts, arg0_type));
        }
      }

      if (m_args[1].is_int() && dts.ts.tc(TypeSpec("pointer"), arg0_type.typespec())) {
        // allow shifting a pointer to put it in a bitfield.
        return TP_Type::make_from_ts(TypeSpec("uint"));
      }
      break;

    case Kind::RIGHT_SHIFT_ARITH:
    case Kind::RIGHT_SHIFT_LOGIC: {
      bool is_unsigned = m_kind == Kind::RIGHT_SHIFT_LOGIC;

      if (m_args[1].is_int()) {
        auto bf = dynamic_cast<BitFieldType*>(dts.ts.lookup_type(arg0_type.typespec()));
        if (bf && arg0_type.typespec() != TypeSpec("time-frame")) {
          int shift_size = 64;
          int size = shift_size - m_args[1].get_int();
          int start_bit = shift_size - size;
          auto field = find_field(dts.ts, bf, start_bit, size, is_unsigned);
          return TP_Type::make_from_ts(coerce_to_reg_type(field.type()));
        }
      }

      if (arg0_type.kind == TP_Type::Kind::LEFT_SHIFTED_BITFIELD && m_args[1].is_int()) {
        // second op in left/right shift combo
        int end_bit = 64 - arg0_type.get_left_shift();
        if (arg0_type.pcpyud()) {
          end_bit += 64;
        }

        int size = 64 - m_args[1].get_int();
        int start_bit = end_bit - size;
        if (start_bit < 0) {
          throw std::runtime_error("get_type_int2: Bad bitfield start bit");
        }

        auto type = dts.ts.lookup_type(arg0_type.get_bitfield_type());
        auto as_bitfield = dynamic_cast<BitFieldType*>(type);
        ASSERT(as_bitfield);
        auto field = find_field(dts.ts, as_bitfield, start_bit, size, is_unsigned);
        return TP_Type::make_from_ts(coerce_to_reg_type(field.type()));
      }

      if (m_kind == Kind::RIGHT_SHIFT_ARITH) {
        if (env.version == GameVersion::Jak2 && arg0_type.typespec().base_type() == "float") {
          return TP_Type::make_from_ts(TypeSpec("float"));
        }
        return TP_Type::make_from_ts(TypeSpec("int"));
      }
    } break;

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
      if (arg0_type.is_integer_constant() && is_int_or_uint(dts, arg1_type)) {
        return TP_Type::make_from_product(arg0_type.get_integer_constant(),
                                          is_signed(dts, arg0_type));
      } else if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
        // unsigned multiply will always return a unsigned number.
        return TP_Type::make_from_ts("uint");
      }
    } break;

    case Kind::DIV_SIGNED:
    case Kind::MOD_SIGNED: {
      if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
        // signed division will always return a signed number.
        return TP_Type::make_from_ts("int");
      }
    } break;

    case Kind::ADD:
      // get stack address:
      if (m_args[0].is_var() && m_args[0].var().reg() == Register(Reg::GPR, Reg::SP) &&
          m_args[1].is_int()) {
        return get_stack_type_at_constant_offset(m_args[1].get_int(), env, dts, input);
      }
      if (arg0_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT &&
          arg1_type.typespec().base_type() == "pointer") {
        return TP_Type::make_from_ts(TypeSpec("int"));
      }

      if (arg0_type.is_product_with(4) && tc(dts, TypeSpec("type"), arg1_type) &&
          env.func->name() != "overrides-parent-method?") {
        // dynamic access into the method array with shift, add, offset-load
        // no need to track the type because we don't know the method index anyway.
        return TP_Type::make_partial_dyanmic_vtable_access();
      }

      if (arg1_type.is_integer_constant() &&
          arg0_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT) {
        return TP_Type::make_from_integer_constant_plus_product(
            arg1_type.get_integer_constant(), arg0_type.typespec(), arg0_type.get_multiplier());
      }

      if (arg1_type.is_integer_constant() && is_int_or_uint(dts, arg0_type)) {
        TypeSpec sum_type = arg0_type.typespec();
        FieldReverseLookupInput rd_in;
        rd_in.offset = arg1_type.get_integer_constant();
        rd_in.stride = 0;
        rd_in.base_type = arg0_type.typespec();
        auto out = env.dts->ts.reverse_field_lookup(rd_in);
        if (out.success) {
          sum_type = coerce_to_reg_type(out.result_type);
        }

        return TP_Type::make_from_integer_constant_plus_var(arg1_type.get_integer_constant(),
                                                            arg0_type.typespec(), sum_type);
      }

      break;

    case Kind::MIN_SIGNED:
      return TP_Type::make_from_ts("int");

    default:
      break;
  }

  if (arg0_type.kind == TP_Type::Kind::PCPYUD_BITFIELD &&
      (m_kind == Kind::AND || m_kind == Kind::OR)) {
    // anding a bitfield should return the bitfield type.
    return TP_Type::make_from_pcpyud_bitfield(arg0_type.get_bitfield_type());
  }

  // this is right but breaks something else right now.
  if (m_kind == Kind::PCPYLD && arg0_type.kind == TP_Type::Kind::PCPYUD_BITFIELD) {
    return arg1_type;
  }

  if (m_kind == Kind::PCPYLD) {
    return TP_Type::make_from_ts("uint");
  }

  if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT && m_kind == Kind::ADD) {
    FieldReverseLookupInput rd_in;
    rd_in.offset = arg0_type.get_add_int_constant();
    rd_in.stride = arg0_type.get_mult_int_constant();
    rd_in.base_type = arg1_type.typespec();
    auto out = env.dts->ts.reverse_field_lookup(rd_in);
    if (out.success) {
      return TP_Type::make_from_ts(coerce_to_reg_type(out.result_type));
    }
  } else if (arg1_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT &&
             m_kind == Kind::ADD) {
    FieldReverseLookupInput rd_in;
    rd_in.offset = arg1_type.get_add_int_constant();
    rd_in.stride = arg1_type.get_mult_int_constant();
    rd_in.base_type = arg0_type.typespec();
    auto out = env.dts->ts.reverse_field_lookup(rd_in);
    if (out.success) {
      return TP_Type::make_from_ts(coerce_to_reg_type(out.result_type));
    }
  }

  if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR && m_kind == Kind::ADD) {
    FieldReverseLookupInput rd_in;
    rd_in.offset = arg0_type.get_integer_constant();
    rd_in.stride = 1;
    rd_in.base_type = arg1_type.typespec();
    auto out = env.dts->ts.reverse_field_lookup(rd_in);
    if (out.success) {
      return TP_Type::make_from_ts(coerce_to_reg_type(out.result_type));
    }
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
  if ((arg0_type.typespec() == TypeSpec("object") || arg0_type.typespec() == TypeSpec("pair") ||
       tc(dts, TypeSpec("basic"), arg0_type)) &&
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

  // access with just product and no offset.
  if (m_kind == Kind::ADD && arg0_type.kind == TP_Type::Kind::TYPESPEC &&
      arg0_type.typespec().base_type() == "inline-array" &&
      arg1_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT) {
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;
    rd_in.stride = arg1_type.get_multiplier();
    rd_in.offset = 0;
    rd_in.base_type = arg0_type.typespec();
    auto rd = dts.ts.reverse_field_multi_lookup(rd_in);

    for (int i = 0; i < (int)rd.results.size(); i++) {
      if (rd.results.at(i).has_variable_token()) {
        return TP_Type::make_from_ts(coerce_to_reg_type(rd.results.at(i).result_type));
        break;
      }
    }
  }

  if (m_kind == Kind::ADD && arg1_type.kind == TP_Type::Kind::TYPESPEC &&
      arg1_type.typespec().base_type() == "inline-array" &&
      arg0_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT) {
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;
    rd_in.stride = arg0_type.get_multiplier();
    rd_in.offset = 0;
    rd_in.base_type = arg1_type.typespec();
    auto rd = dts.ts.reverse_field_multi_lookup(rd_in);

    for (int i = 0; i < (int)rd.results.size(); i++) {
      if (rd.results.at(i).has_variable_token()) {
        return TP_Type::make_from_ts(coerce_to_reg_type(rd.results.at(i).result_type));
        break;
      }
    }
  }

  if (m_kind == Kind::ADD && arg0_type.is_product() && arg1_type.kind == TP_Type::Kind::TYPESPEC) {
    return TP_Type::make_object_plus_product(arg1_type.typespec(), arg0_type.get_multiplier(),
                                             true);
  }

  if (m_kind == Kind::ADD && arg1_type.is_product() && arg0_type.kind == TP_Type::Kind::TYPESPEC) {
    return TP_Type::make_object_plus_product(arg0_type.typespec(), arg1_type.get_multiplier(),
                                             false);
  }

  if ((m_kind == Kind::ADD || m_kind == Kind::SUB) &&
      arg0_type.typespec().base_type() == "pointer" && tc(dts, TypeSpec("integer"), arg1_type)) {
    if (m_kind == Kind::ADD && !m_args[1].is_int()) {
      return TP_Type::make_object_plus_product(arg0_type.typespec(), 1, false);
    }
    // plain pointer plus integer = plain pointer
    return TP_Type::make_from_ts(arg0_type.typespec());
  }

  if (m_kind == Kind::ADD && arg1_type.typespec().base_type() == "pointer" &&
      tc(dts, TypeSpec("integer"), arg0_type)) {
    // plain pointer plus integer = plain pointer
    return TP_Type::make_from_ts(arg1_type.typespec());
  }

  if (m_kind == Kind::SUB && arg1_type.typespec().base_type() == "pointer" &&
      tc(dts, TypeSpec("integer"), arg0_type)) {
    // plain pointer plus integer = plain pointer
    return TP_Type::make_from_ts(arg0_type.typespec());
  }

  if ((m_kind == Kind::ADD || m_kind == Kind::SUB) && tc(dts, TypeSpec("structure"), arg0_type) &&
      arg1_type.is_integer_constant()) {
    auto type_info = dts.ts.lookup_type(arg0_type.typespec());

    // get next in memory, allow this as &+/&-
    if ((s64)type_info->get_size_in_memory() == std::abs((s64)arg1_type.get_integer_constant())) {
      return TP_Type::make_from_ts(arg0_type.typespec());
    }

    // also allow it, if 16-byte aligned stride.
    if ((u64)align16(type_info->get_size_in_memory()) == arg1_type.get_integer_constant()) {
      return TP_Type::make_from_ts(arg0_type.typespec());
    }
  }

  if (env.version == GameVersion::Jak2 && tc(dts, TypeSpec("symbol"), arg1_type) &&
      !m_args[0].is_int() && is_int_or_uint(dts, arg0_type)) {
    if (arg0_type.is_integer_constant(jak2::SYM_TO_STRING_OFFSET)) {
      // symbol -> GOAL String
      // NOTE - the offset doesn't fit in a s16, so it's loaded into a register first.
      // so we expect the arg to be a variable, and the type propagation will figure out the
      // integer constant.
      return TP_Type::make_from_ts(dts.ts.make_pointer_typespec("string"));
    }
  }

  if (tc(dts, TypeSpec("structure"), arg1_type) && !m_args[0].is_int() &&
      is_int_or_uint(dts, arg0_type)) {
    if (allowable_base_type_for_symbol_to_string(arg1_type.typespec()) &&
        arg0_type.is_integer_constant(SYMBOL_TO_STRING_MEM_OFFSET_DECOMP[env.version])) {
      // symbol -> GOAL String
      // NOTE - the offset doesn't fit in a s16, so it's loaded into a register first.
      // so we expect the arg to be a variable, and the type propagation will figure out the
      // integer constant.
      return TP_Type::make_from_ts(dts.ts.make_pointer_typespec("string"));
    } else {
      // byte access of offset array field trick.
      // arg1 holds a structure.
      // arg0 is an integer in a register.
      // return TP_Type::make_object_plus_product(arg1_type.typespec(), 1, true);
      if (arg0_type.is_integer_constant()) {
        TypeSpec sum_type = arg1_type.typespec();
        FieldReverseLookupInput rd_in;
        rd_in.offset = arg0_type.get_integer_constant();
        rd_in.stride = 0;
        rd_in.base_type = arg1_type.typespec();
        auto out = env.dts->ts.reverse_field_lookup(rd_in);
        if (out.success) {
          sum_type = coerce_to_reg_type(out.result_type);
        }
        return TP_Type::make_from_integer_constant_plus_var(arg0_type.get_integer_constant(),
                                                            arg1_type.typespec(), sum_type);
      } else {
        return TP_Type::make_object_plus_product(arg1_type.typespec(), 1, true);
      }
    }
  }

  if (m_kind == Kind::AND) {
    if (arg0_type.typespec().base_type() == "pointer" && tc(dts, TypeSpec("integer"), arg1_type)) {
      // pointer logand integer = pointer
      return TP_Type::make_from_ts(arg0_type.typespec());
    } else if (arg1_type.typespec().base_type() == "pointer" &&
               tc(dts, TypeSpec("integer"), arg0_type)) {
      // integer logand pointer = pointer
      return TP_Type::make_from_ts(arg1_type.typespec());
    }
    // base case for and. Just get an integer.
    return TP_Type::make_from_ts(TypeSpec("int"));
  }

  if ((m_kind == Kind::ADD || m_kind == Kind::SUB) && tc(dts, TypeSpec("pointer"), arg0_type) &&
      tc(dts, TypeSpec("pointer"), arg1_type)) {
    return TP_Type::make_from_ts(TypeSpec("int"));
  }

  if (m_kind == Kind::RIGHT_SHIFT_LOGIC && arg0_type.typespec() == TypeSpec("float") &&
      arg1_type.is_integer_constant(63)) {
    //
    return TP_Type::make_from_ts(TypeSpec("int"));
  }

  if (m_kind == Kind::OR && arg0_type.typespec() == TypeSpec("float") &&
      arg1_type.typespec() == TypeSpec("float")) {
    env.func->warnings.warning("Using logior on floats");
    // returning int instead of uint because they like to use the float sign bit as an integer sign
    // bit.
    return TP_Type::make_from_ts(TypeSpec("float"));
  }

  auto& name = env.func->guessed_name;
  if (name.kind == FunctionName::FunctionKind::METHOD && name.method_id == 7 &&
      env.func->type.arg_count() == 3) {
    if (m_kind == Kind::ADD && arg1_type.typespec() == TypeSpec("int")) {
      return arg0_type;
    }
  }

  // allow shifting stuff for setting bitfields
  if (m_kind == Kind::LEFT_SHIFT) {
    return TP_Type::make_from_ts(TypeSpec("int"));
  }

  throw std::runtime_error(fmt::format("Cannot get_type_int2: {}, args {} and {}",
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
    m_source_type = TypeSpec("float");
    return result;
  }
  auto type = m_src.get_type(input, env, dts);
  result.get(m_dst.reg()) = type;
  m_source_type = type.typespec();
  return result;
}

TypeState AsmOp::propagate_types_internal(const TypeState& input,
                                          const Env& env,
                                          DecompilerTypeSystem& dts) {
  (void)env;
  (void)dts;
  TypeState result = input;

  if (m_instr.kind == InstructionKind::QMFC2) {
    ASSERT(m_dst);
    result.get(m_dst->reg()) = TP_Type::make_from_ts("float");
    return result;
  }

  if (m_instr.kind == InstructionKind::PCPYUD) {
    if (m_src[1] && m_src[1]->reg() == Register(Reg::GPR, Reg::R0)) {
      ASSERT(m_src[0]);
      auto& in_type = result.get(m_src[0]->reg());
      auto bf = dynamic_cast<BitFieldType*>(dts.ts.lookup_type(in_type.typespec()));
      if (bf) {
        ASSERT(m_dst);
        result.get(m_dst->reg()) = TP_Type::make_from_pcpyud_bitfield(in_type.typespec());
        return result;
      }
    }
  }

  // pextuw t0, r0, gp
  if (m_instr.kind == InstructionKind::PEXTUW) {
    if (m_src[0] && m_src[0]->reg() == Register(Reg::GPR, Reg::R0)) {
      ASSERT(m_src[1]);
      auto type = dts.ts.lookup_type(result.get(m_src[1]->reg()).typespec());
      auto as_bitfield = dynamic_cast<BitFieldType*>(type);
      if (as_bitfield) {
        auto field = find_field(dts.ts, as_bitfield, 64, 32, true);
        ASSERT(m_dst);
        result.get(m_dst->reg()) = TP_Type::make_from_ts(field.type());
        return result;
      }
    }
  }

  // sllv out, in, r0
  if (m_instr.kind == InstructionKind::SLLV &&
      instruction().src[1].is_reg(Register(Reg::GPR, Reg::R0))) {
    auto type = dts.ts.lookup_type(result.get(m_src[0]->reg()).typespec());
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    if (as_bitfield) {
      auto field = find_field(dts.ts, as_bitfield, 0, 32, {});
      result.get(m_dst->reg()) = TP_Type::make_from_ts(field.type());
      return result;
    }
  }

  // srl out, bitfield, int
  if (m_instr.kind == InstructionKind::SRL) {
    auto type = dts.ts.lookup_type(result.get(m_src[0]->reg()).typespec());
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    if (as_bitfield) {
      int sa = m_instr.src[1].get_imm();
      int offset = sa;
      int size = 32 - offset;
      auto field = find_field(dts.ts, as_bitfield, offset, size, {});
      result.get(m_dst->reg()) = TP_Type::make_from_ts(coerce_to_reg_type(field.type()));
      return result;
    }
  }

  if (m_dst.has_value()) {
    auto kind = m_dst->reg().get_kind();
    if (kind == Reg::FPR) {
      result.get(m_dst->reg()) = TP_Type::make_from_ts("float");
    } else if (kind == Reg::GPR) {
      for (auto& x : m_src) {
        if (x && x->reg().get_kind() == Reg::GPR) {
          auto src_type = result.get(x->reg()).typespec();
          if (dts.ts.tc(TypeSpec("int128"), src_type) || dts.ts.tc(TypeSpec("uint128"), src_type)) {
            result.get(m_dst->reg()) = TP_Type::make_from_ts("uint128");
            return result;
          }
        }
        result.get(m_dst->reg()) = TP_Type::make_from_ts("int");
      }
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
  TypeState output = input;

  // look for setting the next state of the current process
  IR2_RegOffset ro;
  if (get_as_reg_offset(m_addr, &ro)) {
    if (ro.reg == Register(Reg::GPR, Reg::S6) && ro.offset == 72) {
      output.next_state_type = m_value.get_type(input, env, dts);
    }
  }
  (void)env;
  (void)dts;
  return output;
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

      // todo labeldb
      auto label_name = env.file->labels.at(src.label()).name;
      const auto& hint = env.file->label_db->lookup(label_name);
      if (!hint.known) {
        throw std::runtime_error(
            fmt::format("Label {} was unknown in AtomicOpTypeAnalysis (type).", hint.name));
      }

      if (!hint.is_value) {
        throw std::runtime_error(
            fmt::format("Label {} was used as a value, but wasn't marked as one", hint.name));
      }

      return TP_Type::make_from_ts(coerce_to_reg_type(hint.result_type));

      //      if (m_size == 8) {
      //        // 8 byte integer constants are always loaded from a static pool
      //        // this could technically hide loading a different type from inside of a static
      //        basic. return TP_Type::make_from_ts(dts.ts.make_typespec("uint"));
      //      }
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
      if (method_id == GOAL_NEW_METHOD) {
        return TP_Type::make_non_object_new(method_type, TypeSpec(type_name));
      } else if (input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL) {
        return TP_Type::make_non_virtual_method(method_type, TypeSpec(type_name), method_id);
      } else {
        return TP_Type::make_virtual_method(method_type, TypeSpec(type_name), method_id);
      }
    }

    if (input_type.kind == TP_Type::Kind::TYPESPEC && input_type.typespec() == TypeSpec("type") &&
        ro.offset >= 16 && (ro.offset & 3) == 0 && m_size == 4 && m_kind == Kind::UNSIGNED) {
      // method get of an unknown type. We assume the most general "object" type.
      auto method_id = (ro.offset - 16) / 4;
      if (method_id <= (int)GOAL_MEMUSAGE_METHOD) {
        auto method_info = dts.ts.lookup_method("object", method_id);
        if (method_id != GOAL_NEW_METHOD && method_id != GOAL_RELOC_METHOD) {
          // this can get us the wrong thing for `new` methods.  And maybe relocate?
          return TP_Type::make_non_virtual_method(
              method_info.type.substitute_for_method_call("object"), TypeSpec("object"), method_id);
        }
      }
    }

    if (input_type.typespec() == TypeSpec("pointer") &&
        input_type.kind != TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
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
          ASSERT(false);
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
    // if we are a pair with sloppy typing, don't use this and instead use the case down below.
    if (input_type.typespec() != TypeSpec("pair") || !env.allow_sloppy_pair_typing()) {
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

      if (rd.success) {
        if (rd_in.base_type.base_type() == "state" && rd.tokens.size() == 1 &&
            rd.tokens.front().kind == FieldReverseLookupOutput::Token::Kind::FIELD &&
            rd.tokens.front().name == "enter" && rd_in.base_type.arg_count() > 0) {
          // special case for accessing the enter field of state
          return TP_Type::make_from_ts(state_to_go_function(rd_in.base_type, TypeSpec("none")));
        } else {
          return TP_Type::make_from_ts(coerce_to_reg_type(rd.result_type));
        }
      }
    }

    if (input_type.typespec() == TypeSpec("pointer") ||
        input_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
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
            case 16:
              return TP_Type::make_from_ts(TypeSpec("uint128"));
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
            case 16:
              return TP_Type::make_from_ts(TypeSpec("int128"));
            default:
              break;
          }
          break;
        case Kind::FLOAT:
          return TP_Type::make_from_ts(TypeSpec("float"));
        default:
          ASSERT(false);
      }
    }

    if (input_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR &&
        input_type.get_integer_constant() == 0) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = m_kind == LoadVarOp::Kind::SIGNED;
      dk.size = m_size;
      rd_in.deref = dk;
      rd_in.base_type = input_type.get_objects_typespec();
      rd_in.offset = ro.offset;
      auto rd = dts.ts.reverse_field_lookup(rd_in);
      if (rd.success) {
        return TP_Type::make_from_ts(coerce_to_reg_type(rd.result_type));
      }
    }

    if (input_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR && ro.offset == 0) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = kind() == LoadVarOp::Kind::SIGNED;
      dk.size = size();
      rd_in.deref = dk;
      rd_in.base_type = input_type.get_objects_typespec();
      rd_in.stride = 0;
      rd_in.offset = input_type.get_integer_constant();
      auto rd = dts.ts.reverse_field_lookup(rd_in);
      if (rd.success) {
        return TP_Type::make_from_ts(coerce_to_reg_type(rd.result_type));
      }
    }

    // rd failed, try as pair.
    if (env.allow_sloppy_pair_typing()) {
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

  throw std::runtime_error("LoadVarOp cannot get_src_type: " +
                           to_form(env.file->labels, env).print());
}

TypeState LoadVarOp::propagate_types_internal(const TypeState& input,
                                              const Env& env,
                                              DecompilerTypeSystem& dts) {
  if (m_dst.reg().get_kind() == Reg::FPR || m_dst.reg().get_kind() == Reg::GPR) {
    TypeState result = input;
    auto load_type = get_src_type(input, env, dts);
    result.get(m_dst.reg()) = load_type;
    m_type = load_type.typespec();
    return result;
  } else {
    // vector float loads show up as LoadVarOps, but we don't want to track types in the
    // vector float registers.
    return input;
  }
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
      ASSERT(false);
      return input;
  }
}

TypeState CallOp::propagate_types_internal(const TypeState& input,
                                           const Env& env,
                                           DecompilerTypeSystem& dts) {
  (void)dts;
  (void)env;
  const Reg::Gpr arg_regs[8] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3,
                                Reg::T0, Reg::T1, Reg::T2, Reg::T3};
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
      m_arg_vars.push_back(RegisterAccess(AccessMode::READ, m_read_regs.back(), m_my_idx));
    }
    return end_types;
  }

  auto in_type = in_tp.typespec();

  if (in_type.base_type() != "function") {
    throw std::runtime_error("Called something that was not a function: " + in_type.print());
  }

  // If we call enter-state, update our type.
  if (in_tp.kind == TP_Type::Kind::ENTER_STATE_FUNCTION) {
    // this is a GO!
    auto state_type = input.next_state_type.typespec();
    if (state_type.base_type() != "state") {
      throw std::runtime_error(
          fmt::format("At op {}, called enter-state, but the current next-state has type {}, which "
                      "is not a valid state.",
                      m_my_idx, input.next_state_type.print()));
    }

    if (state_type.arg_count() == 0) {
      throw std::runtime_error(fmt::format(
          "At op {}, tried to enter-state, but the type of (-> s6 next-state) is just a plain "
          "state.  The decompiler must know the specific state type.",
          m_my_idx));
    }
    in_type = state_to_go_function(state_type, TypeSpec("object"));
  }

  if (in_tp.kind == TP_Type::Kind::RUN_FUNCTION_IN_PROCESS_FUNCTION ||
      in_tp.kind == TP_Type::Kind::SET_TO_RUN_FUNCTION) {
    auto func_to_run_type = input.get(Register(Reg::GPR, arg_regs[1]));
    auto func_to_run_ts = func_to_run_type.typespec();
    if (func_to_run_ts.base_type() != "function" || func_to_run_ts.arg_count() == 0 ||
        func_to_run_ts.arg_count() > 7) {
      throw std::runtime_error(
          fmt::format("Call to run-function-in-process or set-to-run at op {} with an invalid "
                      "function type: {}",
                      m_my_idx, func_to_run_type.print()));
    }

    std::vector<TypeSpec> new_arg_types;
    if (in_tp.kind == TP_Type::Kind::RUN_FUNCTION_IN_PROCESS_FUNCTION) {
      new_arg_types.push_back(TypeSpec("process"));
    } else {
      new_arg_types.push_back(TypeSpec("thread"));
    }
    new_arg_types.push_back(TypeSpec("function"));

    for (size_t i = 0; i < func_to_run_ts.arg_count() - 1; i++) {
      new_arg_types.push_back(func_to_run_ts.get_arg(i));
    }
    new_arg_types.push_back(TypeSpec("none"));
    in_type = TypeSpec("function", new_arg_types);
  }

  if (in_type.arg_count() < 1) {
    throw std::runtime_error("Called a function, but we do not know its type");
  }

  if (in_type.arg_count() == 2 && in_type.get_arg(0) == TypeSpec("_varargs_")) {
    // we're calling a varags function, which is format. We can determine the argument count
    // by looking at the format string, if we can get it.
    auto arg_type = input.get(Register(Reg::GPR, Reg::A1));
    auto can_determine_argc = arg_type.can_be_format_string();
    auto dynamic_string = false;
    if (!can_determine_argc && arg_type.typespec() == TypeSpec("string")) {
      // dynamic string. use manual lookup table.
      dynamic_string = true;
    }
    if (can_determine_argc || dynamic_string) {
      int arg_count = -1;

      if (dynamic_string) {
        arg_count = dts.get_dynamic_format_arg_count(env.func->name(), m_my_idx);
      } else if (arg_type.is_constant_string()) {
        auto& str = arg_type.get_string();
        arg_count = dts.get_format_arg_count(str);
      } else {
        // is format string.
        arg_count = arg_type.get_format_string_arg_count();
      }

      if (arg_count + 2 > 8) {
        throw std::runtime_error(
            "Call to `format` pushed the arg-count beyond the acceptable arg limit (8), do you "
            "need to add "
            "a code to the ignore lists?");
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
        m_arg_vars.push_back(RegisterAccess(AccessMode::READ, m_read_regs.back(), m_my_idx));
      }

      return end_types;
    } else {
      throw std::runtime_error("Failed to get appropriate string for _varags_ call, got " +
                               arg_type.print());
    }
  }
  // set the call type!
  m_call_type = in_type;
  m_call_type_set = true;

  end_types.get(Register(Reg::GPR, Reg::V0)) = TP_Type::make_from_ts(in_type.last_arg());

  if (in_tp.kind == TP_Type::Kind::NON_OBJECT_NEW_METHOD &&
      in_type.last_arg() == TypeSpec("array")) {
    // array new:
    auto& a2 = input.get(Register(Reg::GPR, arg_regs[2]));  // elt type
    auto& a0 = input.get(Register(Reg::GPR, arg_regs[0]));  // allocation

    if (a2.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL &&
        in_tp.method_from_type() == TypeSpec("array") && a0.is_symbol()) {
      end_types.get(Register(Reg::GPR, Reg::V0)) =
          TP_Type::make_from_ts(TypeSpec("array", {a2.get_type_objects_typespec()}));
    }
  }

  // we can also update register usage here.
  m_read_regs.clear();
  m_arg_vars.clear();
  m_read_regs.emplace_back(Reg::GPR, Reg::T9);

  for (uint32_t i = 0; i < in_type.arg_count() - 1; i++) {
    m_read_regs.emplace_back(Reg::GPR, arg_regs[i]);
    m_arg_vars.push_back(RegisterAccess(AccessMode::READ, m_read_regs.back(), m_my_idx));
  }

  // _always_ write the v0 register, even if the function returns none.
  // GOAL seems to insert coloring moves even on functions returning none.
  m_write_regs.clear();
  m_write_regs.emplace_back(Reg::GPR, Reg::V0);

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

TypeState AsmBranchOp::propagate_types_internal(const TypeState& input,
                                                const Env& env,
                                                DecompilerTypeSystem& dts) {
  if (m_branch_delay) {
    return m_branch_delay->propagate_types(input, env, dts);
  }
  // for now, just make everything uint
  TypeState output = input;
  for (auto x : m_write_regs) {
    if (x.allowed_local_gpr()) {
      output.get(x) = TP_Type::make_from_ts("uint");
    }
  }

  return output;
}

TypeState StackSpillLoadOp::propagate_types_internal(const TypeState& input,
                                                     const Env& env,
                                                     DecompilerTypeSystem&) {
  // stack slot load
  auto& info = env.stack_spills().lookup(m_offset);
  if (info.size != m_size) {
    env.func->warnings.warning("Stack slot load at {} mismatch: defined as size {}, got size {}",
                               m_offset, info.size, m_size);
  }

  if (info.is_signed != m_is_signed) {
    env.func->warnings.warning("Stack slot offset {} signed mismatch", m_offset);
  }

  auto& loaded_type = input.get_slot(m_offset);
  auto result = input;
  result.get(m_dst.reg()) = loaded_type;
  return result;
}

TypeState StackSpillStoreOp::propagate_types_internal(const TypeState& input,
                                                      const Env& env,
                                                      DecompilerTypeSystem& dts) {
  auto& info = env.stack_spills().lookup(m_offset);
  if (info.size != m_size) {
    env.func->warnings.error("Stack slot store mismatch: defined as size {}, got size {}\n",
                             info.size, m_size);
  }

  auto stored_type = m_value.get_type(input, env, dts);
  auto result = input;
  result.spill_slots[m_offset] = stored_type;
  return result;
}

}  // namespace decompiler
