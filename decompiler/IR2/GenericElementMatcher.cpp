#include "GenericElementMatcher.h"

namespace decompiler {
Matcher Matcher::any_reg(int match_id) {
  Matcher m;
  m.m_kind = Kind::ANY_REG;
  m.m_reg_out_id = match_id;
  return m;
}

Matcher Matcher::op(GenericOperator op, const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::GENERIC_OP;
  m.m_gen_op = op;
  m.m_sub_matchers = args;
  return m;
}

Matcher Matcher::fixed_op(FixedOperatorKind op, const std::vector<Matcher>& args) {
  Matcher m;
  m.m_kind = Kind::GENERIC_OP;
  m.m_gen_op = GenericOperator::make_fixed(op);
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

Matcher Matcher::any() {
  Matcher m;
  m.m_kind = Kind::ANY;
  return m;
}

Matcher Matcher::integer(std::optional<int> value) {
  Matcher m;
  m.m_kind = Kind::INT;
  m.m_int_match = value;
  return m;
}

bool Matcher::do_match(const Form* input, MatchResult::Maps* maps_out) const {
  switch (m_kind) {
    case Kind::ANY_REG: {
      bool got = false;
      Variable result;

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

    case Kind::GENERIC_OP: {
      auto as_generic = dynamic_cast<GenericElement*>(input->try_as_single_element());
      if (as_generic) {
        if (as_generic->op() != m_gen_op) {
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
    }

    default:
      assert(false);
  }
}

Matcher Matcher::any_reg_cast_to_int_or_uint(int match_id) {
  return match_or(
      {any_reg(match_id), cast("uint", any_reg(match_id)), cast("int", any_reg(match_id))});
}

MatchResult match(const Matcher& spec, const Form* input) {
  MatchResult result;
  result.matched = spec.do_match(input, &result.maps);
  return result;
}
}  // namespace decompiler