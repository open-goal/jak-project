#include "AtomicOp.h"
#include "Form.h"
#include "common/type_system/TypeSystem.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

namespace {
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

ConditionElement* BranchOp::get_condition_as_form(FormPool& pool, const Env& env) const {
  return m_condition.get_as_form(pool, env, m_my_idx);
}

ConditionElement* IR2_Condition::get_as_form(FormPool& pool, const Env& env, int my_idx) const {
  RegSet consumed;
  if (env.has_reg_use()) {
    consumed = env.reg_use().op.at(my_idx).consumes;
  }

  std::optional<SimpleAtom> vars[2];
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    vars[i] = m_src[i];
  }
  return pool.alloc_element<ConditionElement>(m_kind, vars[0], vars[1], consumed, m_flipped_eval);
}

FormElement* SetVarOp::get_as_form(FormPool& pool, const Env& env) const {
  if (env.has_type_analysis() && m_src.args() == 2 && m_src.get_arg(1).is_int() &&
      m_src.get_arg(0).is_var() && m_src.kind() == SimpleExpression::Kind::ADD) {
    auto arg0_type = env.get_types_before_op(m_my_idx).get(m_src.get_arg(0).var().reg());
    if (arg0_type.kind == TP_Type::Kind::TYPESPEC) {
      // access a field.
      FieldReverseLookupInput rd_in;
      rd_in.deref = std::nullopt;
      rd_in.stride = 0;
      rd_in.offset = m_src.get_arg(1).get_int();
      rd_in.base_type = arg0_type.typespec();
      auto rd = env.dts->ts.reverse_field_lookup(rd_in);

      if (rd.success) {
        auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
            nullptr, SimpleAtom::make_var(m_src.get_arg(0).var()).as_expr(), m_my_idx);
        std::vector<DerefToken> tokens;
        for (auto& x : rd.tokens) {
          tokens.push_back(to_token(x));
        }
        auto load =
            pool.alloc_single_element_form<DerefElement>(nullptr, source, rd.addr_of, tokens);
        return pool.alloc_element<SetVarElement>(m_dst, load, true);
      }
    }
  }

  // create element
  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src, m_my_idx);
  auto result = pool.alloc_element<SetVarElement>(m_dst, source, is_sequence_point());

  // do some analysis to look for coloring moves which are already eliminated,
  // dead sets, and dead set falses.
  if (m_src.kind() == SimpleExpression::Kind::IDENTITY && env.has_local_vars() &&
      env.has_reg_use()) {
    if (env.op_id_is_eliminated_coloring_move(m_my_idx)) {
      result->eliminate_as_coloring_move();
    } else if (m_src.get_arg(0).is_var()) {
      auto& src_var = m_src.get_arg(0).var();
      auto& ri = env.reg_use().op.at(m_my_idx);
      if (ri.consumes.find(src_var.reg()) != ri.consumes.end() &&
          ri.written_and_unused.find(dst().reg()) != ri.written_and_unused.end()) {
        result->mark_as_dead_set();
        // fmt::print("marked {} as dead set\n", to_string(env));
      }
    } else if (m_src.get_arg(0).is_sym_val() && m_src.get_arg(0).get_str() == "#f") {
      auto& ri = env.reg_use().op.at(m_my_idx);
      if (ri.written_and_unused.find(dst().reg()) != ri.written_and_unused.end()) {
        result->mark_as_dead_false();
        // fmt::print("marked {} as dead set false\n", to_string(env));
      }
    }
  }

  return result;
}

FormElement* AsmOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<AsmOpElement>(this);
}

FormElement* SetVarConditionOp::get_as_form(FormPool& pool, const Env& env) const {
  return pool.alloc_element<SetVarElement>(
      m_dst, pool.alloc_single_form(nullptr, m_condition.get_as_form(pool, env, m_my_idx)),
      is_sequence_point());
}

FormElement* StoreOp::get_as_form(FormPool& pool, const Env& env) const {
  if (env.has_type_analysis()) {
    if (m_addr.is_identity() && m_addr.get_arg(0).is_sym_val()) {
      // we are storing a value in a global symbol. This is something like sw rx, offset(s7)
      // so the source can only be a variable, r0 (integer 0), or false (s7)
      // we want to know both: what cast (if any) do we need for a set!, and what cast (if any)
      // do we need for a define.

      auto symbol_type = env.dts->lookup_symbol_type(m_addr.get_arg(0).get_str());
      auto symbol_type_info = env.dts->ts.lookup_type(symbol_type);

      switch (m_value.get_kind()) {
        case SimpleAtom::Kind::VARIABLE: {
          auto src_type = env.get_types_before_op(m_my_idx).get(m_value.var().reg()).typespec();
          std::optional<TypeSpec> cast_for_set, cast_for_define;

          if (src_type != symbol_type) {
            // the define will need a cast to the exactly right type.
            cast_for_define = symbol_type;
          }

          if (!env.dts->ts.tc(symbol_type, src_type)) {
            // we fail the typecheck for a normal set!, so add a cast.
            cast_for_set = symbol_type;
          }

          return pool.alloc_element<StoreInSymbolElement>(m_addr.get_arg(0).get_str(),
                                                          m_value.as_expr(), cast_for_set,
                                                          cast_for_define, m_my_idx);
        } break;
        case SimpleAtom::Kind::INTEGER_CONSTANT: {
          std::optional<TypeSpec> cast_for_set, cast_for_define;
          bool sym_int_or_uint = env.dts->ts.tc(TypeSpec("integer"), symbol_type);
          bool sym_uint = env.dts->ts.tc(TypeSpec("uinteger"), symbol_type);
          bool sym_int = sym_int_or_uint && !sym_uint;

          if (TypeSpec("int") != symbol_type) {
            // the define will need a cast to the exactly right type.
            cast_for_define = symbol_type;
          }

          if (sym_int) {
            // do nothing for set.
          } else {
            // for uint or other
            cast_for_set = symbol_type;
          }

          return pool.alloc_element<StoreInSymbolElement>(m_addr.get_arg(0).get_str(),
                                                          m_value.as_expr(), cast_for_set,
                                                          cast_for_define, m_my_idx);
        } break;

        case SimpleAtom::Kind::SYMBOL_PTR:
        case SimpleAtom::Kind::SYMBOL_VAL: {
          assert(m_value.get_str() == "#f");
          std::optional<TypeSpec> cast_for_set, cast_for_define;
          if (symbol_type != TypeSpec("symbol")) {
            cast_for_define = symbol_type;
            // explicitly cast if we're not using a reference type, including pointers.
            // otherwise, we allow setting references to #f.
            if (!symbol_type_info->is_reference()) {
              cast_for_set = symbol_type;
            }
          }
          return pool.alloc_element<StoreInSymbolElement>(m_addr.get_arg(0).get_str(),
                                                          m_value.as_expr(), cast_for_set,
                                                          cast_for_define, m_my_idx);
        } break;

        default:
          assert(false);
      }
    }

    IR2_RegOffset ro;
    if (get_as_reg_offset(m_addr, &ro)) {
      auto& input_type = env.get_types_before_op(m_my_idx).get(ro.reg);

      if (env.allow_sloppy_pair_typing() && m_size == 4 &&
          (input_type.typespec() == TypeSpec("object") ||
           input_type.typespec() == TypeSpec("pair"))) {
        if (ro.offset == 2) {
          return pool.alloc_element<StoreInPairElement>(false, ro.var, m_value.as_expr(), m_my_idx);
        } else if (ro.offset == -2) {
          return pool.alloc_element<StoreInPairElement>(true, ro.var, m_value.as_expr(), m_my_idx);
        }
      }

      if (input_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
        FieldReverseLookupInput rd_in;
        DerefKind dk;
        dk.is_store = true;
        dk.reg_kind = get_reg_kind(ro.reg);
        dk.size = m_size;
        rd_in.deref = dk;
        rd_in.base_type = input_type.get_obj_plus_const_mult_typespec();
        rd_in.stride = input_type.get_multiplier();
        rd_in.offset = ro.offset;
        auto rd = env.dts->ts.reverse_field_lookup(rd_in);

        if (rd.success) {
          std::vector<DerefToken> tokens;
          assert(!rd.tokens.empty());
          for (auto& token : rd.tokens) {
            tokens.push_back(to_token(token));
          }

          // we pass along the register offset because code generation seems to be a bit
          // different in different cases.
          auto source = pool.alloc_element<ArrayFieldAccess>(
              ro.var, tokens, input_type.get_multiplier(), ro.offset);

          //          auto val = pool.alloc_single_element_form<SimpleExpressionElement>(
          //              nullptr, m_value.as_expr(), m_my_idx);

          assert(!rd.addr_of);
          return pool.alloc_element<StoreArrayAccess>(source, m_value.as_expr(), m_my_idx, ro.var);
        }
      }

      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = true;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.size = m_size;
      rd_in.deref = dk;
      rd_in.base_type = input_type.typespec();
      rd_in.stride = 0;
      rd_in.offset = ro.offset;
      auto rd = env.dts->ts.reverse_field_lookup(rd_in);

      if (rd.success) {
        auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
            nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
        std::vector<DerefToken> tokens;
        for (auto& x : rd.tokens) {
          tokens.push_back(to_token(x));
        }
        assert(!rd.addr_of);
        auto addr = pool.alloc_element<DerefElement>(source, rd.addr_of, tokens);
        return pool.alloc_element<StorePlainDeref>(addr, m_value.as_expr(), m_my_idx, ro.var,
                                                   std::nullopt);
      }

      std::string cast_type;
      if (input_type.typespec() == TypeSpec("pointer") && ro.offset == 0) {
        switch (m_size) {
          case 1:
            cast_type = "int8";
            break;
          case 2:
            cast_type = "int16";
            break;
          case 4:
            cast_type = "int32";
            break;
          case 8:
            cast_type = "int64";
            break;
          default:
            assert(false);
        }

        if (m_value.is_var()) {
          auto input_var_type = env.get_types_before_op(m_my_idx).get(m_value.var().reg());
          if (env.dts->ts.tc(TypeSpec("uinteger"), input_var_type.typespec())) {
            cast_type.insert(cast_type.begin(), 'u');
          }
        }

        auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
            nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
        auto cast_source = pool.alloc_single_element_form<CastElement>(
            nullptr, TypeSpec("pointer", {TypeSpec(cast_type)}), source);
        auto deref =
            pool.alloc_element<DerefElement>(cast_source, false, std::vector<DerefToken>());
        return pool.alloc_element<StorePlainDeref>(deref, m_value.as_expr(), m_my_idx, ro.var,
                                                   TypeSpec("pointer", {TypeSpec(cast_type)}));
      }
    }
  }
  return pool.alloc_element<StoreElement>(this);
}

FormElement* LoadVarOp::get_as_form(FormPool& pool, const Env& env) const {
  if (env.has_type_analysis()) {
    IR2_RegOffset ro;
    if (get_as_reg_offset(m_src, &ro)) {
      auto& input_type = env.get_types_before_op(m_my_idx).get(ro.reg);

      if ((input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL ||
           input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD) &&
          ro.offset >= 16 && (ro.offset & 3) == 0 && m_size == 4 && m_kind == Kind::UNSIGNED) {
        // method get of fixed type
        auto type_name = input_type.get_type_objects_typespec().base_type();
        auto method_id = (ro.offset - 16) / 4;
        auto method_info = env.dts->ts.lookup_method(type_name, method_id);

        std::vector<DerefToken> tokens;
        tokens.push_back(DerefToken::make_field_name("methods-by-name"));
        tokens.push_back(DerefToken::make_field_name(method_info.name));
        auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
            nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
        auto load = pool.alloc_single_element_form<DerefElement>(nullptr, source, false, tokens);
        return pool.alloc_element<SetVarElement>(m_dst, load, true);
      }

      // todo structure method

      // todo product trick
      // todo type of basic fallback

      if (input_type.kind == TP_Type::Kind::DYNAMIC_METHOD_ACCESS && ro.offset == 16) {
        // access method vtable. The input is type + (4 * method), and the 16 is the offset
        // of method 0.
        auto load = pool.alloc_single_element_form<DynamicMethodAccess>(nullptr, ro.var);
        return pool.alloc_element<SetVarElement>(m_dst, load, true);
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
        auto rd = env.dts->ts.reverse_field_lookup(rd_in);

        if (rd.success) {
          std::vector<DerefToken> tokens;
          assert(!rd.tokens.empty());
          for (auto& token : rd.tokens) {
            tokens.push_back(to_token(token));
          }

          // we pass along the register offset because code generation seems to be a bit
          // different in different cases.
          auto load = pool.alloc_single_element_form<ArrayFieldAccess>(
              nullptr, ro.var, tokens, input_type.get_multiplier(), ro.offset);
          return pool.alloc_element<SetVarElement>(m_dst, load, true);
        }
      }

      if (env.allow_sloppy_pair_typing() && m_kind == Kind::SIGNED && m_size == 4 &&
          (input_type.typespec() == TypeSpec("object") ||
           input_type.typespec() == TypeSpec("pair"))) {
        // these rules are of course not always correct or the most specific, but it's the best
        // we can do.
        if (ro.offset == 2) {
          auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
              nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
          auto load = pool.alloc_single_element_form<GenericElement>(
              nullptr, GenericOperator::make_fixed(FixedOperatorKind::CDR), source);
          // cdr = another pair.
          return pool.alloc_element<SetVarElement>(m_dst, load, true);
        } else if (ro.offset == -2) {
          // car = some object.
          auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
              nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
          auto load = pool.alloc_single_element_form<GenericElement>(
              nullptr, GenericOperator::make_fixed(FixedOperatorKind::CAR), source);
          // cdr = another pair.
          return pool.alloc_element<SetVarElement>(m_dst, load, true);
        }
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
      auto rd = env.dts->ts.reverse_field_lookup(rd_in);

      // todo, error here?

      if (rd.success) {
        auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
            nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
        std::vector<DerefToken> tokens;
        for (auto& x : rd.tokens) {
          tokens.push_back(to_token(x));
        }

        auto load =
            pool.alloc_single_element_form<DerefElement>(nullptr, source, rd.addr_of, tokens);
        return pool.alloc_element<SetVarElement>(m_dst, load, true);
      }

      if (input_type.typespec() == TypeSpec("pointer")) {
        std::string cast_type;
        switch (m_size) {
          case 1:
            cast_type = "int8";
            break;
          case 2:
            cast_type = "int16";
            break;
          case 4:
            cast_type = "int32";
            break;
          case 8:
            cast_type = "int64";
            break;
          default:
            assert(false);
        }
        if (m_kind == Kind::UNSIGNED) {
          cast_type = "u" + cast_type;
        } else if (m_kind == Kind::FLOAT) {
          cast_type = "float";
        }

        auto dest = pool.alloc_single_element_form<SimpleExpressionElement>(
            nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
        auto cast_dest = pool.alloc_single_element_form<CastElement>(
            nullptr, TypeSpec("pointer", {TypeSpec(cast_type)}), dest);
        auto deref = pool.alloc_single_element_form<DerefElement>(nullptr, cast_dest, false,
                                                                  std::vector<DerefToken>());
        return pool.alloc_element<SetVarElement>(m_dst, deref, true);
      }
    }
  }

  if (m_src.is_identity() && m_src.get_arg(0).is_label()) {
    // try to see if we're loading a constant
    auto label = env.file->labels.at(m_src.get_arg(0).label());
    auto label_name = label.name;
    auto hint = env.label_types().find(label_name);
    if (hint != env.label_types().end()) {
      if (hint->second.is_const) {
        if ((m_kind == Kind::FLOAT || m_kind == Kind::SIGNED) && m_size == 4 &&
            hint->second.type_name == "float") {
          assert((label.offset % 4) == 0);
          auto word = env.file->words_by_seg.at(label.target_segment).at(label.offset / 4);
          assert(word.kind == LinkedWord::PLAIN_DATA);
          float value;
          memcpy(&value, &word.data, 4);
          auto float_elt = pool.alloc_single_element_form<ConstantFloatElement>(nullptr, value);
          return pool.alloc_element<SetVarElement>(m_dst, float_elt, true);
        } else if (hint->second.type_name == "uint64" && m_kind != Kind::FLOAT && m_size == 8) {
          assert((label.offset % 8) == 0);
          auto word0 = env.file->words_by_seg.at(label.target_segment).at(label.offset / 4);
          auto word1 = env.file->words_by_seg.at(label.target_segment).at(1 + (label.offset / 4));
          assert(word0.kind == LinkedWord::PLAIN_DATA);
          assert(word1.kind == LinkedWord::PLAIN_DATA);
          u64 value;

          memcpy(&value, &word0.data, 4);
          memcpy(((u8*)&value) + 4, &word1.data, 4);
          auto val_elt = pool.alloc_single_element_form<ConstantTokenElement>(
              nullptr, fmt::format("#x{:x}", value));
          return pool.alloc_element<SetVarElement>(m_dst, val_elt, true);
        }
      }
    }
  }

  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src, m_my_idx);
  auto load = pool.alloc_single_element_form<LoadSourceElement>(nullptr, source, m_size, m_kind);
  return pool.alloc_element<SetVarElement>(m_dst, load, true);
}

FormElement* BranchOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<BranchElement>(this);
}

FormElement* SpecialOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<AtomicOpElement>(this);
}

FormElement* CallOp::get_as_form(FormPool& pool, const Env& env) const {
  auto call = pool.alloc_element<FunctionCallElement>(this);
  if (m_write_regs.empty() && m_call_type_set == true) {
    return call;
  } else if (m_write_regs.size() == 1 || !m_call_type_set) {
    if (env.has_reg_use() && m_write_regs.size() == 1) {
      auto& written_and_unused = env.reg_use().op.at(m_my_idx).written_and_unused;
      if (written_and_unused.find(m_write_regs.front()) != written_and_unused.end()) {
        return call;
      }
    }

    // this is a little scary in the case that type analysis doesn't run and relies on the fact
    // that CallOp falls back to writing v0 in the case where the function type isn't known.
    RegisterAccess out_var(AccessMode::WRITE, Register(Reg::GPR, Reg::V0), m_my_idx);
    return pool.alloc_element<SetVarElement>(out_var, pool.alloc_single_form(nullptr, call), true);
  } else {
    throw std::runtime_error("CallOp::get_as_expr not yet implemented");
  }
}

FormElement* ConditionalMoveFalseOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<ConditionalMoveFalseElement>(m_dst, m_old_value, m_src, m_on_zero);
}

FormElement* FunctionEndOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<AtomicOpElement>(this);
}

FormElement* AsmBranchOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<AtomicOpElement>(this);
}
}  // namespace decompiler