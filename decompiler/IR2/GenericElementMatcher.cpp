#include "GenericElementMatcher.h"

namespace decompiler {
Matcher Matcher::any_reg(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_REG;
  m.m_reg_out_id = match_id;
  return m;
}

Matcher Matcher::same_var(int match_id) {
  Matcher m;
  m.m_kind = Kind::SAME_VAR;
  m.m_reg_out_id = match_id;
  return m;
}

Matcher Matcher::var_name(const std::string& name) {
  Matcher m;
  m.m_kind = Kind::VAR_NAME;
  m.m_str = name;
  return m;
}

Matcher Matcher::any_label(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_LABEL;
  m.m_label_out_id = match_id;
  return m;
}

Matcher Matcher::reg(Register reg) {
  Matcher m;
  m.m_kind = Kind::REG;
  m.m_reg = reg;
  return m;
}

Matcher Matcher::s6() {
  return Matcher::reg(Register(Reg::GPR, Reg::S6));
}

Matcher Matcher::op(const GenericOpMatcher& op, const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::GENERIC_OP;
  m.m_gen_op_matcher = std::make_shared<GenericOpMatcher>(op);
  m.m_sub_matchers = args;
  return m;
}

Matcher Matcher::func(const Matcher& matcher, const std::vector<Matcher>& args) {
  return Matcher::op(GenericOpMatcher::func(matcher), args);
}

Matcher Matcher::func(const std::string& name, const std::vector<Matcher>& args) {
  return Matcher::op(GenericOpMatcher::func(Matcher::constant_token(name)), args);
}

Matcher Matcher::op_fixed(FixedOperatorKind op, const std::vector<Matcher>& args) {
  return Matcher::op(GenericOpMatcher::fixed(op), args);
}

Matcher Matcher::op_with_rest(const GenericOpMatcher& op, const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::GENERIC_OP_WITH_REST;
  m.m_gen_op_matcher = std::make_shared<GenericOpMatcher>(op);
  m.m_sub_matchers = args;
  return m;
}

Matcher Matcher::func_with_rest(const Matcher& matcher, const std::vector<Matcher>& args) {
  return Matcher::op_with_rest(GenericOpMatcher::func(matcher), args);
}

Matcher Matcher::func_with_rest(const std::string& name, const std::vector<Matcher>& args) {
  return Matcher::op_with_rest(GenericOpMatcher::func(Matcher::constant_token(name)), args);
}

Matcher Matcher::match_or(const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::OR;
  m.m_sub_matchers = args;
  return m;
}

Matcher Matcher::cast(const std::string& type, Matcher value) {
  Matcher m;
  m.m_kind = Kind::CAST;
  m.m_str = type;
  m.m_sub_matchers = {value};
  return m;
}

Matcher Matcher::cast_to_any(int type_out, Matcher value) {
  Matcher m;
  m.m_kind = Kind::CAST_TO_ANY;
  m.m_string_out_id = type_out;
  m.m_sub_matchers = {value};
  return m;
}

Matcher Matcher::any(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY;
  m.m_form_match = match_id;
  return m;
}

Matcher Matcher::integer(std::optional<int> value) {
  Matcher m;
  m.m_kind = Kind::INT;
  m.m_int_match = value;
  return m;
}

Matcher Matcher::any_integer(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_INT;
  m.m_int_out_id = match_id;
  return m;
}

Matcher Matcher::single(std::optional<float> value) {
  Matcher m;
  m.m_kind = Kind::FLOAT;
  m.m_float_match = value;
  return m;
}

Matcher Matcher::any_single(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_FLOAT;
  m.m_float_out_id = match_id;
  return m;
}

Matcher Matcher::any_quoted_symbol(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_QUOTED_SYMBOL;
  m.m_string_out_id = match_id;
  return m;
}

Matcher Matcher::any_symbol(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_SYMBOL;
  m.m_string_out_id = match_id;
  return m;
}

Matcher Matcher::quoted_symbol(const std::string& name) {
  Matcher m;
  m.m_kind = Kind::QUOTED_SYMBOL;
  m.m_str = name;
  return m;
}

Matcher Matcher::symbol(const std::string& name) {
  Matcher m;
  m.m_kind = Kind::SYMBOL;
  m.m_str = name;
  return m;
}

Matcher Matcher::if_with_else(const Matcher& condition,
                              const Matcher& true_case,
                              const Matcher& false_case) {
  Matcher m;
  m.m_kind = Kind::IF_WITH_ELSE;
  m.m_sub_matchers = {condition, true_case, false_case};
  return m;
}

Matcher Matcher::if_no_else(const Matcher& condition, const Matcher& true_case) {
  Matcher m;
  m.m_kind = Kind::IF_NO_ELSE;
  m.m_sub_matchers = {condition, true_case};
  return m;
}

Matcher Matcher::or_expression(const std::vector<Matcher>& elts) {
  Matcher m;
  m.m_kind = Kind::SC_OR;
  m.m_sub_matchers = elts;
  return m;
}

Matcher Matcher::deref(const Matcher& root,
                       bool is_addr_of,
                       const std::vector<DerefTokenMatcher>& tokens) {
  Matcher m;
  m.m_kind = Kind::DEREF_OP;
  m.m_sub_matchers = {root};
  m.m_deref_is_addr_of = is_addr_of;
  m.m_token_matchers = tokens;
  return m;
}

Matcher Matcher::set(const Matcher& dst, const Matcher& src) {
  Matcher m;
  m.m_kind = Kind::SET;
  m.m_sub_matchers = {dst, src};
  return m;
}

Matcher Matcher::set_var(const Matcher& src, int dst_match_id) {
  Matcher m;
  m.m_kind = Kind::SET_VAR;
  m.m_sub_matchers = {src};
  m.m_reg_out_id = dst_match_id;
  return m;
}

Matcher Matcher::while_loop(const Matcher& condition, const Matcher& body) {
  Matcher m;
  m.m_kind = Kind::WHILE_LOOP;
  m.m_sub_matchers = {condition, body};
  return m;
}

Matcher Matcher::until_loop(const Matcher& condition, const Matcher& body) {
  Matcher m;
  m.m_kind = Kind::UNTIL_LOOP;
  m.m_sub_matchers = {condition, body};
  return m;
}

Matcher Matcher::any_constant_token(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_CONSTANT_TOKEN;
  m.m_string_out_id = match_id;
  return m;
}

Matcher Matcher::constant_token(const std::string& name) {
  Matcher m;
  m.m_kind = Kind::CONSTANT_TOKEN;
  m.m_str = name;
  return m;
}

Matcher Matcher::begin(const std::vector<Matcher>& elts) {
  Matcher m;
  m.m_kind = Kind::BEGIN;
  m.m_sub_matchers = elts;
  return m;
}

Matcher Matcher::let(bool is_star,
                     const std::vector<LetEntryMatcher>& entries,
                     const std::vector<Matcher>& elts) {
  Matcher m;
  m.m_kind = Kind::LET;
  m.m_let_is_star = is_star;
  m.m_entry_matchers = entries;
  m.m_sub_matchers = elts;
  return m;
}

Matcher Matcher::unmerged_let(const std::vector<LetEntryMatcher>& entries,
                              const std::vector<Matcher>& elts) {
  Matcher m;
  m.m_kind = Kind::UNMERGED_LET;
  m.m_entry_matchers = entries;
  m.m_sub_matchers = elts;
  return m;
}

bool Matcher::do_match(Form* input, MatchResult::Maps* maps_out, const Env* const env) const {
  switch (m_kind) {
    case Kind::ANY:
      if (m_form_match != -1) {
        maps_out->forms[m_form_match] = input;
      }
      return true;
    case Kind::ANY_REG:
    case Kind::VAR_NAME:
    case Kind::SAME_VAR:
    case Kind::REG: {
      bool got = false;
      RegisterAccess result;

      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_var()) {
          got = true;
          result = as_simple_atom->atom().var();
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
        if (atom.is_var()) {
          got = true;
          result = atom.var();
        }
      }

      if (got) {
        if (m_kind == Kind::REG) {
          return result.reg() == *m_reg;
        } else if (m_kind == Kind::VAR_NAME) {
          return env && env->get_variable_name_name_only(result) == m_str;
        } else if (m_reg_out_id != -1) {
          if (m_kind == Kind::SAME_VAR && (int)maps_out->regs.size() > m_reg_out_id &&
              maps_out->regs.at(m_reg_out_id)) {
            return env && env->get_variable_name_name_only(result) ==
                              env->get_variable_name_name_only(*maps_out->regs.at(m_reg_out_id));
          } else {
            maps_out->regs.resize(std::max(size_t(m_reg_out_id + 1), maps_out->regs.size()));
            maps_out->regs.at(m_reg_out_id) = result;
          }
        }
        return true;
      } else {
        return false;
      }
    } break;

    case Kind::ANY_LABEL: {
      bool got = false;
      int result;

      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_label()) {
          got = true;
          result = as_simple_atom->atom().label();
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
        if (atom.is_label()) {
          got = true;
          result = atom.label();
        }
      }

      if (got) {
        if (m_label_out_id != -1) {
          maps_out->label[m_label_out_id] = result;
        }
        return true;
      } else {
        return false;
      }
    } break;

    case Kind::GENERIC_OP:
    case Kind::GENERIC_OP_WITH_REST: {
      auto as_generic = dynamic_cast<GenericElement*>(input->try_as_single_active_element());
      if (as_generic) {
        if (!m_gen_op_matcher->do_match(as_generic->op(), maps_out, env)) {
          return false;
        }

        if ((m_kind == Kind::GENERIC_OP && as_generic->elts().size() != m_sub_matchers.size()) ||
            (m_kind == Kind::GENERIC_OP_WITH_REST &&
             as_generic->elts().size() < m_sub_matchers.size())) {
          return false;
        }

        for (size_t i = 0; i < m_sub_matchers.size(); i++) {
          if (!m_sub_matchers.at(i).do_match(as_generic->elts().at(i), maps_out, env)) {
            return false;
          }
        }
        return true;
      }
      return false;
    } break;

    case Kind::OR: {
      for (auto& matcher : m_sub_matchers) {
        if (matcher.do_match(input, maps_out, env)) {
          return true;
        }
      }
      return false;
    } break;

    case Kind::ANY_CONSTANT_TOKEN: {
      auto as_ct = input->try_as_element<ConstantTokenElement>();
      if (as_ct) {
        if (m_string_out_id != -1) {
          maps_out->strings[m_string_out_id] = as_ct->value();
        }
        return true;
      } else {
        return false;
      }
    } break;

    case Kind::CONSTANT_TOKEN: {
      auto as_ct = input->try_as_element<ConstantTokenElement>();
      if (as_ct) {
        return as_ct->value() == m_str;
      }
      return false;
    } break;

    case Kind::CAST: {
      auto as_cast = dynamic_cast<CastElement*>(input->try_as_single_active_element());
      if (as_cast) {
        if (as_cast->type().print() == m_str) {
          return m_sub_matchers.at(0).do_match(as_cast->source(), maps_out, env);
        }
      }
      return false;
    } break;

    case Kind::CAST_TO_ANY: {
      auto as_cast = dynamic_cast<CastElement*>(input->try_as_single_active_element());
      if (as_cast) {
        maps_out->strings[m_string_out_id] = as_cast->type().print();
        return m_sub_matchers.at(0).do_match(as_cast->source(), maps_out, env);
      }
      return false;
    } break;

    case Kind::INT: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_int()) {
          if (!m_int_match.has_value()) {
            return true;
          }
          return as_simple_atom->atom().get_int() == *m_int_match;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        const auto& atom = as_expr->expr().get_arg(0);
        if (atom.is_int()) {
          if (!m_int_match.has_value()) {
            return true;
          }
          return atom.get_int() == *m_int_match;
        }
      }

      return false;
    } break;

    case Kind::FLOAT: {
      auto as_const_float =
          dynamic_cast<ConstantFloatElement*>(input->try_as_single_active_element());
      if (as_const_float) {
        if (!m_float_match.has_value()) {
          return true;
        }
        return as_const_float->value() == *m_float_match;
      }

      return false;
    } break;

    case Kind::ANY_INT: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_int()) {
          if (m_int_out_id != -1) {
            maps_out->ints[m_int_out_id] = as_simple_atom->atom().get_int();
          }
          return true;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        const auto& atom = as_expr->expr().get_arg(0);
        if (atom.is_int()) {
          if (m_int_out_id != -1) {
            maps_out->ints[m_int_out_id] = atom.get_int();
          }
          return true;
        }
      }

      return false;
    } break;

    case Kind::ANY_FLOAT: {
      auto as_const_float =
          dynamic_cast<ConstantFloatElement*>(input->try_as_single_active_element());
      if (as_const_float) {
        if (m_float_out_id != -1) {
          maps_out->floats[m_float_out_id] = as_const_float->value();
        }
        return true;
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        const auto& atom = as_expr->expr().get_arg(0);
        if (atom.is_integer_promoted_to_float()) {
          if (m_float_out_id != -1) {
            maps_out->floats[m_float_out_id] = atom.get_integer_promoted_to_float();
          }
          return true;
        }
      }

      return false;
    } break;

    case Kind::ANY_QUOTED_SYMBOL: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_sym_ptr()) {
          if (m_string_out_id != -1) {
            maps_out->strings[m_string_out_id] = as_simple_atom->atom().get_str();
          }
          return true;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        const auto& atom = as_expr->expr().get_arg(0);
        if (atom.is_sym_ptr()) {
          if (m_string_out_id != -1) {
            maps_out->strings[m_string_out_id] = atom.get_str();
          }
          return true;
        }
      }
      return false;
    }

    case Kind::ANY_SYMBOL: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_sym_val()) {
          if (m_string_out_id != -1) {
            maps_out->strings[m_string_out_id] = as_simple_atom->atom().get_str();
          }
          return true;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        const auto& atom = as_expr->expr().get_arg(0);
        if (atom.is_sym_val()) {
          if (m_string_out_id != -1) {
            maps_out->strings[m_string_out_id] = atom.get_str();
          }
          return true;
        }
      }
      return false;
    }

    case Kind::QUOTED_SYMBOL: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_sym_ptr()) {
          return as_simple_atom->atom().get_str() == m_str;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        const auto& atom = as_expr->expr().get_arg(0);
        if (atom.is_sym_ptr()) {
          return atom.get_str() == m_str;
        }
      }
      return false;
    }

    case Kind::SYMBOL: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_active_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_sym_val()) {
          return as_simple_atom->atom().get_str() == m_str;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_active_element());
      if (as_expr && as_expr->expr().is_identity()) {
        const auto& atom = as_expr->expr().get_arg(0);
        if (atom.is_sym_val()) {
          return atom.get_str() == m_str;
        }
      }
      return false;
    }

    case Kind::DEREF_OP: {
      auto as_deref = dynamic_cast<DerefElement*>(input->try_as_single_active_element());
      if (as_deref) {
        if (as_deref->is_addr_of() != m_deref_is_addr_of) {
          return false;
        }
        if (!m_sub_matchers.at(0).do_match(as_deref->base(), maps_out, env)) {
          return false;
        }
        if (as_deref->tokens().size() != m_token_matchers.size()) {
          return false;
        }
        for (size_t i = 0; i < as_deref->tokens().size(); i++) {
          if (!m_token_matchers.at(i).do_match(as_deref->tokens().at(i), maps_out)) {
            return false;
          }
        }
        return true;
      }
      return false;
    }

    case Kind::SET: {
      auto as_set = dynamic_cast<SetFormFormElement*>(input->try_as_single_active_element());
      if (!as_set) {
        return false;
      }
      if (!m_sub_matchers.at(0).do_match(as_set->dst(), maps_out, env)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_set->src(), maps_out, env)) {
        return false;
      } else {
        return true;
      }
    } break;

    case Kind::IF_WITH_ELSE: {
      auto as_cond = dynamic_cast<CondWithElseElement*>(input->try_as_single_active_element());
      if (!as_cond) {
        return false;
      }

      if (as_cond->entries.size() != 1) {
        return false;
      }

      if (!m_sub_matchers.at(0).do_match(as_cond->entries.front().condition, maps_out, env)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_cond->entries.front().body, maps_out, env)) {
        return false;
      }

      if (!m_sub_matchers.at(2).do_match(as_cond->else_ir, maps_out, env)) {
        return false;
      }
      return true;
    } break;

    case Kind::IF_NO_ELSE: {
      auto as_cond = dynamic_cast<CondNoElseElement*>(input->try_as_single_active_element());
      if (!as_cond) {
        return false;
      }

      if (as_cond->entries.size() != 1) {
        return false;
      }

      if (!m_sub_matchers.at(0).do_match(as_cond->entries.front().condition, maps_out, env)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_cond->entries.front().body, maps_out, env)) {
        return false;
      }

      return true;
    } break;

    case Kind::SC_OR: {
      auto as_sc = dynamic_cast<ShortCircuitElement*>(input->try_as_single_active_element());
      if (!as_sc || as_sc->kind != ShortCircuitElement::OR) {
        return false;
      }
      if (as_sc->entries.size() != m_sub_matchers.size()) {
        return false;
      }

      for (size_t i = 0; i < m_sub_matchers.size(); i++) {
        if (!m_sub_matchers.at(i).do_match(as_sc->entries.at(i).condition, maps_out, env)) {
          return false;
        }
      }

      return true;
    } break;

    case Kind::WHILE_LOOP: {
      auto as_while = dynamic_cast<WhileElement*>(input->try_as_single_active_element());
      if (!as_while) {
        return false;
      }

      if (!m_sub_matchers.at(0).do_match(as_while->condition, maps_out, env)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_while->body, maps_out, env)) {
        return false;
      }
      return true;
    } break;

    case Kind::UNTIL_LOOP: {
      auto as_until = dynamic_cast<UntilElement*>(input->try_as_single_active_element());
      if (!as_until) {
        return false;
      }

      if (!m_sub_matchers.at(0).do_match(as_until->condition, maps_out, env)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_until->body, maps_out, env)) {
        return false;
      }
      return true;
    } break;

    case Kind::BEGIN: {
      if ((int)m_sub_matchers.size() != input->size()) {
        return false;
      }

      for (int i = 0; i < input->size(); i++) {
        Form fake;
        fake.elts().push_back(input->elts().at(i));
        if (!m_sub_matchers.at(i).do_match(&fake, maps_out, env)) {
          return false;
        }
      }
      return true;
    }

    case Kind::SET_VAR: {
      auto as_set = dynamic_cast<SetVarElement*>(input->try_as_single_active_element());
      if (!as_set) {
        return false;
      }

      if (!m_sub_matchers.at(0).do_match(as_set->src(), maps_out, env)) {
        return false;
      }

      if (m_reg_out_id != -1) {
        maps_out->regs.resize(std::max((int)maps_out->regs.size(), m_reg_out_id + 1));
        maps_out->regs.at(m_reg_out_id) = as_set->dst();
      }
      return true;
    }

    case Kind::LET: {
      auto as_let = dynamic_cast<LetElement*>(input->try_as_single_active_element());
      if (as_let) {
        // fail if we have wrong number of entries/body elts or recursive marker
        if ((m_entry_matchers.size() != as_let->entries().size()) ||
            ((int)m_sub_matchers.size() != as_let->body()->size()) ||
            (as_let->is_star() != m_let_is_star)) {
          return false;
        }
        // match entries first
        for (size_t i = 0; i < m_entry_matchers.size(); ++i) {
          if (!m_entry_matchers.at(i).do_match(as_let->entries().at(i), maps_out, env)) {
            return false;
          }
        }
        // now match body
        for (int i = 0; i < (int)m_sub_matchers.size(); ++i) {
          Form fake;
          fake.elts().push_back(as_let->body()->elts().at(i));
          if (!m_sub_matchers.at(i).do_match(&fake, maps_out, env)) {
            return false;
          }
        }
        return true;
      }
      return false;
    }

    case Kind::UNMERGED_LET: {
      auto as_let = dynamic_cast<LetElement*>(input->try_as_single_active_element());
      if (as_let) {
        size_t entries_matched = 0;
        Form* innermost_let_body = nullptr;
        // first try to find the innermost let, matching all let entries with the entry matchers
        // throughout
        as_let->apply_form([&](Form* form) {
          for (int idx = 0; idx < form->size(); idx++) {
            auto* f = form->at(idx);
            // if this is the entry of the outermost let, try to do the first match
            if (f->parent_form->parent_element == as_let) {
              if (m_entry_matchers.at(entries_matched)
                      .do_match(as_let->entries().at(0), maps_out, env)) {
                entries_matched++;
              } else {
                return;
              }
            }
            auto let = dynamic_cast<LetElement*>(f);
            if (!let) {
              continue;
            }

            auto let_body = dynamic_cast<LetElement*>(let->body()->at(0));
            if (!let_body) {
              break;
            }

            if (entries_matched == m_entry_matchers.size()) {
              break;
            }

            if (m_entry_matchers.at(entries_matched)
                    .do_match(let_body->entries().at(0), maps_out, env)) {
              entries_matched++;
            } else {
              return;
            }

            if (entries_matched == m_entry_matchers.size()) {
              innermost_let_body = let_body->body();
              return;
            }
          }
        });

        if (entries_matched == m_entry_matchers.size() && innermost_let_body) {
          // now match body of innermost let
          for (int i = 0; i < (int)m_sub_matchers.size(); ++i) {
            Form fake;
            fake.elts().push_back(innermost_let_body->elts().at(i));
            if (!m_sub_matchers.at(i).do_match(&fake, maps_out, env)) {
              return false;
            }
          }
          return true;
        }
      }
      return false;
    }

    default:
      ASSERT(false);
      return false;
  }
}

Matcher Matcher::any_reg_cast_to_int_or_uint(int match_id) {
  return match_or(
      {any_reg(match_id), cast("uint", any_reg(match_id)), cast("int", any_reg(match_id))});
}

MatchResult match(const Matcher& spec, Form* input, const Env* const env) {
  MatchResult result;
  result.matched = spec.do_match(input, &result.maps, env);
  return result;
}

MatchResult match(const Matcher& spec, FormElement* input, const Env* const env) {
  Form hack;
  hack.elts().push_back(input);
  MatchResult result;
  result.matched = spec.do_match(&hack, &result.maps, env);
  return result;
}

DerefTokenMatcher DerefTokenMatcher::string(const std::string& str) {
  DerefTokenMatcher result;
  result.m_kind = Kind::STRING;
  result.m_str = str;
  return result;
}

DerefTokenMatcher DerefTokenMatcher::integer(int value) {
  DerefTokenMatcher result;
  result.m_kind = Kind::CONSTANT_INTEGER;
  result.m_int = value;
  return result;
}

DerefTokenMatcher DerefTokenMatcher::any_string(int match_id) {
  DerefTokenMatcher result;
  result.m_kind = Kind::ANY_STRING;
  result.m_str_out_id = match_id;
  return result;
}

DerefTokenMatcher DerefTokenMatcher::any_integer(int match_id) {
  DerefTokenMatcher result;
  result.m_kind = Kind::ANY_INTEGER;
  result.m_str_out_id = match_id;
  return result;
}

DerefTokenMatcher DerefTokenMatcher::any_expr(int match_id) {
  DerefTokenMatcher result;
  result.m_kind = Kind::ANY_EXPR;
  result.m_str_out_id = match_id;
  return result;
}

DerefTokenMatcher DerefTokenMatcher::any_expr_or_int(int match_id) {
  DerefTokenMatcher result;
  result.m_kind = Kind::ANY_EXPR_OR_INT;
  result.m_str_out_id = match_id;
  return result;
}

bool DerefTokenMatcher::do_match(DerefToken& input, MatchResult::Maps* maps_out) const {
  switch (m_kind) {
    case Kind::STRING:
      return input.kind() == DerefToken::Kind::FIELD_NAME && input.field_name() == m_str;
    case Kind::ANY_STRING:
      if (input.kind() == DerefToken::Kind::FIELD_NAME) {
        if (m_str_out_id != -1) {
          maps_out->strings[m_str_out_id] = input.field_name();
        }
        return true;
      }
      return false;
    case Kind::CONSTANT_INTEGER:
      return input.kind() == DerefToken::Kind::INTEGER_CONSTANT && input.is_int(m_int);
    case Kind::ANY_EXPR:
    case Kind::ANY_EXPR_OR_INT:
      if (input.is_expr()) {
        if (m_str_out_id != -1) {
          maps_out->forms[m_str_out_id] = input.expr();
        }
        return true;
      }
      // NOTE intentional fallthrough
      if (m_kind != Kind::ANY_EXPR_OR_INT) {
        return false;
      }
    case Kind::ANY_INTEGER:
      if (input.kind() == DerefToken::Kind::INTEGER_CONSTANT) {
        if (m_str_out_id != -1) {
          maps_out->ints[m_str_out_id] = input.int_constant();
        }
        return true;
      }
      return false;
    default:
      ASSERT(false);
      return false;
  }
}

GenericOpMatcher GenericOpMatcher::fixed(FixedOperatorKind kind) {
  GenericOpMatcher m;
  m.m_kind = Kind::FIXED;
  m.m_fixed_kind = kind;
  return m;
}

GenericOpMatcher GenericOpMatcher::func(const Matcher& func_matcher) {
  GenericOpMatcher m;
  m.m_kind = Kind::FUNC;
  m.m_func_matcher = func_matcher;
  return m;
}

GenericOpMatcher GenericOpMatcher::condition(IR2_Condition::Kind condition) {
  GenericOpMatcher m;
  m.m_kind = Kind::CONDITION;
  m.m_condition_kind = condition;
  return m;
}

GenericOpMatcher GenericOpMatcher::or_match(const std::vector<GenericOpMatcher>& matchers) {
  GenericOpMatcher m;
  m.m_kind = Kind::OR;
  m.m_sub_matchers = matchers;
  return m;
}

bool GenericOpMatcher::do_match(GenericOperator& input,
                                MatchResult::Maps* maps_out,
                                const Env* const env) const {
  switch (m_kind) {
    case Kind::FIXED:
      if (input.kind() == GenericOperator::Kind::FIXED_OPERATOR) {
        return input.fixed_kind() == m_fixed_kind;
      }
      return false;
    case Kind::FUNC:
      if (input.kind() == GenericOperator::Kind::FUNCTION_EXPR) {
        return m_func_matcher.do_match(input.func(), maps_out, env);
      }
      return false;
    case Kind::CONDITION:
      if (input.kind() == GenericOperator::Kind::CONDITION_OPERATOR) {
        return input.condition_kind() == m_condition_kind;
      }
      return false;
    case Kind::OR:
      for (auto& m : m_sub_matchers) {
        if (m.do_match(input, maps_out, env)) {
          return true;
        }
      }
      return false;
    default:
      ASSERT(false);
      return false;
  }
}

LetEntryMatcher LetEntryMatcher::name(std::optional<Matcher> src, const std::string& name) {
  LetEntryMatcher result;
  result.m_kind = Kind::NAME;
  result.m_reg_name = name;
  result.m_src_matcher = src;
  return result;
}

LetEntryMatcher LetEntryMatcher::any(std::optional<Matcher> src, int match_id) {
  LetEntryMatcher result;
  result.m_kind = Kind::ANY;
  result.m_reg_out_id = match_id;
  result.m_src_matcher = src;
  return result;
}

bool LetEntryMatcher::do_match(const LetElement::Entry& input,
                               MatchResult::Maps* maps_out,
                               const Env* const env) const {
  switch (m_kind) {
    case Kind::ANY:
    case Kind::NAME:
      if (m_reg_out_id != -1) {
        if (m_kind == Kind::NAME && (int)maps_out->regs.size() > m_reg_out_id &&
            maps_out->regs.at(m_reg_out_id)) {
          return env && env->get_variable_name_name_only(input.dest) ==
                            env->get_variable_name_name_only(*maps_out->regs.at(m_reg_out_id));
        } else {
          maps_out->regs.resize(std::max(size_t(m_reg_out_id + 1), maps_out->regs.size()));
          maps_out->regs.at(m_reg_out_id) = input.dest;
        }
      }
      if (m_src_matcher) {
        return m_src_matcher->do_match(input.src, maps_out, env);
      }
      return true;
    default:
      ASSERT(false);
      return false;
  }
}

}  // namespace decompiler
