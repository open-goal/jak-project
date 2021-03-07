#include "GenericElementMatcher.h"

namespace decompiler {
Matcher Matcher::any_reg(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_REG;
  m.m_reg_out_id = match_id;
  return m;
}

Matcher Matcher::any_label(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_LABEL;
  m.m_label_out_id = match_id;
  return m;
}

Matcher Matcher::op(const GenericOpMatcher& op, const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::GENERIC_OP;
  m.m_gen_op_matcher = std::make_shared<GenericOpMatcher>(op);
  m.m_sub_matchers = args;
  return m;
}

Matcher Matcher::op_with_rest(const GenericOpMatcher& op, const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::GENERIC_OP_WITH_REST;
  m.m_gen_op_matcher = std::make_shared<GenericOpMatcher>(op);
  m.m_sub_matchers = args;
  return m;
}

Matcher Matcher::fixed_op(FixedOperatorKind op, const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::GENERIC_OP;
  m.m_gen_op_matcher = std::make_shared<GenericOpMatcher>(GenericOpMatcher::fixed(op));
  m.m_sub_matchers = args;
  return m;
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

Matcher Matcher::while_loop(const Matcher& condition, const Matcher& body) {
  Matcher m;
  m.m_kind = Kind::WHILE_LOOP;
  m.m_sub_matchers = {condition, body};
  return m;
}

bool Matcher::do_match(Form* input, MatchResult::Maps* maps_out) const {
  switch (m_kind) {
    case Kind::ANY:
      if (m_form_match != -1) {
        maps_out->forms[m_form_match] = input;
      }
      return true;
    case Kind::ANY_REG: {
      bool got = false;
      RegisterAccess result;

      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_var()) {
          got = true;
          result = as_simple_atom->atom().var();
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
        if (atom.is_var()) {
          got = true;
          result = atom.var();
        }
      }

      if (got) {
        if (m_reg_out_id != -1) {
          maps_out->regs.resize(std::max(size_t(m_reg_out_id + 1), maps_out->regs.size()));
          maps_out->regs.at(m_reg_out_id) = result;
        }
        return true;
      } else {
        return false;
      }
    } break;

    case Kind::ANY_LABEL: {
      bool got = false;
      int result;

      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_label()) {
          got = true;
          result = as_simple_atom->atom().label();
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_element());
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

    case Kind::GENERIC_OP: {
      auto as_generic = dynamic_cast<GenericElement*>(input->try_as_single_element());
      if (as_generic) {
        if (!m_gen_op_matcher->do_match(as_generic->op(), maps_out)) {
          return false;
        }

        if (as_generic->elts().size() != m_sub_matchers.size()) {
          return false;
        }

        for (size_t i = 0; i < m_sub_matchers.size(); i++) {
          if (!m_sub_matchers.at(i).do_match(as_generic->elts().at(i), maps_out)) {
            return false;
          }
        }
        return true;
      }
      return false;
    } break;

    case Kind::GENERIC_OP_WITH_REST: {
      auto as_generic = dynamic_cast<GenericElement*>(input->try_as_single_element());
      if (as_generic) {
        if (!m_gen_op_matcher->do_match(as_generic->op(), maps_out)) {
          return false;
        }

        if (as_generic->elts().size() < m_sub_matchers.size()) {
          return false;
        }

        for (size_t i = 0; i < m_sub_matchers.size(); i++) {
          if (!m_sub_matchers.at(i).do_match(as_generic->elts().at(i), maps_out)) {
            return false;
          }
        }
        return true;
      }
      return false;
    } break;

    case Kind::OR: {
      for (auto& matcher : m_sub_matchers) {
        if (matcher.do_match(input, maps_out)) {
          return true;
        }
      }
      return false;
    } break;

    case Kind::CAST: {
      auto as_cast = dynamic_cast<CastElement*>(input->try_as_single_element());
      if (as_cast) {
        if (as_cast->type().print() == m_str) {
          return m_sub_matchers.at(0).do_match(as_cast->source(), maps_out);
        }
      }
      return false;
    } break;

    case Kind::INT: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_int()) {
          if (!m_int_match.has_value()) {
            return true;
          }
          return as_simple_atom->atom().get_int() == *m_int_match;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
        if (atom.is_int()) {
          if (!m_int_match.has_value()) {
            return true;
          }
          return atom.get_int() == *m_int_match;
        }
      }

      return false;
    } break;

    case Kind::ANY_INT: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_int()) {
          if (m_int_out_id != -1) {
            maps_out->ints[m_int_out_id] = as_simple_atom->atom().get_int();
          }
          return true;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
        if (atom.is_int()) {
          if (m_int_out_id != -1) {
            maps_out->ints[m_int_out_id] = atom.get_int();
          }
          return true;
        }
      }

      return false;
    } break;

    case Kind::ANY_QUOTED_SYMBOL: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_sym_ptr()) {
          if (m_string_out_id != -1) {
            maps_out->strings[m_string_out_id] = as_simple_atom->atom().get_str();
          }
          return true;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
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
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_sym_val()) {
          if (m_string_out_id != -1) {
            maps_out->strings[m_string_out_id] = as_simple_atom->atom().get_str();
          }
          return true;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
        if (atom.is_sym_val()) {
          if (m_string_out_id != -1) {
            maps_out->strings[m_string_out_id] = atom.get_str();
          }
          return true;
        }
      }
      return false;
    }

    case Kind::SYMBOL: {
      auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(input->try_as_single_element());
      if (as_simple_atom) {
        if (as_simple_atom->atom().is_sym_val()) {
          return as_simple_atom->atom().get_str() == m_str;
        }
      }

      auto as_expr = dynamic_cast<SimpleExpressionElement*>(input->try_as_single_element());
      if (as_expr && as_expr->expr().is_identity()) {
        auto atom = as_expr->expr().get_arg(0);
        if (atom.is_sym_val()) {
          return atom.get_str() == m_str;
        }
      }
      return false;
    }

    case Kind::DEREF_OP: {
      auto as_deref = dynamic_cast<DerefElement*>(input->try_as_single_element());
      if (as_deref) {
        if (as_deref->is_addr_of() != m_deref_is_addr_of) {
          return false;
        }
        if (!m_sub_matchers.at(0).do_match(as_deref->base(), maps_out)) {
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
      auto as_set = dynamic_cast<SetFormFormElement*>(input->try_as_single_element());
      if (!as_set) {
        return false;
      }
      if (!m_sub_matchers.at(0).do_match(as_set->dst(), maps_out)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_set->src(), maps_out)) {
        return false;
      } else {
        return true;
      }
    } break;

    case Kind::IF_WITH_ELSE: {
      auto as_cond = dynamic_cast<CondWithElseElement*>(input->try_as_single_element());
      if (!as_cond) {
        return false;
      }

      if (as_cond->entries.size() != 1) {
        return false;
      }

      if (!m_sub_matchers.at(0).do_match(as_cond->entries.front().condition, maps_out)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_cond->entries.front().body, maps_out)) {
        return false;
      }

      if (!m_sub_matchers.at(2).do_match(as_cond->else_ir, maps_out)) {
        return false;
      }
      return true;
    } break;

    case Kind::WHILE_LOOP: {
      auto as_while = dynamic_cast<WhileElement*>(input->try_as_single_element());
      if (!as_while) {
        return false;
      }

      if (!m_sub_matchers.at(0).do_match(as_while->condition, maps_out)) {
        return false;
      }

      if (!m_sub_matchers.at(1).do_match(as_while->body, maps_out)) {
        return false;
      }
      return true;
    } break;

    default:
      assert(false);
      return false;
  }
}

Matcher Matcher::any_reg_cast_to_int_or_uint(int match_id) {
  return match_or(
      {any_reg(match_id), cast("uint", any_reg(match_id)), cast("int", any_reg(match_id))});
}

MatchResult match(const Matcher& spec, Form* input) {
  MatchResult result;
  result.matched = spec.do_match(input, &result.maps);
  return result;
}

DerefTokenMatcher DerefTokenMatcher::string(const std::string& str) {
  DerefTokenMatcher result;
  result.m_kind = Kind::STRING;
  result.m_str = str;
  return result;
}

DerefTokenMatcher DerefTokenMatcher::any_string(int match_id) {
  DerefTokenMatcher result;
  result.m_kind = Kind::ANY_STRING;
  result.m_str_out_id = match_id;
  return result;
}

bool DerefTokenMatcher::do_match(const DerefToken& input, MatchResult::Maps* maps_out) const {
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
    default:
      assert(false);
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

bool GenericOpMatcher::do_match(GenericOperator& input, MatchResult::Maps* maps_out) const {
  switch (m_kind) {
    case Kind::FIXED:
      if (input.kind() == GenericOperator::Kind::FIXED_OPERATOR) {
        return input.fixed_kind() == m_fixed_kind;
      }
      return false;
    case Kind::FUNC:
      if (input.kind() == GenericOperator::Kind::FUNCTION_EXPR) {
        return m_func_matcher.do_match(input.func(), maps_out);
      }
      return false;
    case Kind::CONDITION:
      if (input.kind() == GenericOperator::Kind::CONDITION_OPERATOR) {
        return input.condition_kind() == m_condition_kind;
      }
      return false;
    default:
      assert(false);
      return false;
  }
}

}  // namespace decompiler