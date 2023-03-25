#include "FormStack.h"

#include <algorithm>

#include "Form.h"
#include "GenericElementMatcher.h"

#include "decompiler/Function/Function.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
std::string FormStack::StackEntry::print(const Env& env) const {
  if (destination.has_value()) {
    ASSERT(source && !elt);
    if (active) {
      return fmt::format("d: {} s: {} | {} <- {} f: {} w: {}", active, sequence_point,
                         destination.value().reg().to_charp(), source->to_string(env),
                         non_seq_source.has_value(), is_compactable);
    } else {
      return fmt::format("  d: {} s: {} | {} <- {} f: {} w: {}", active, sequence_point,
                         destination.value().reg().to_charp(), source->to_string(env),
                         non_seq_source.has_value(), is_compactable);
    }

  } else {
    ASSERT(elt && !source);
    if (active) {
      return fmt::format("d: {} s: {} | {} f: {}", active, sequence_point, elt->to_string(env),
                         non_seq_source.has_value());
    } else {
      return fmt::format("  d: {} s: {} | {} f: {}", active, sequence_point, elt->to_string(env),
                         non_seq_source.has_value());
    }
  }
}

std::string FormStack::print(const Env& env) {
  std::string result;
  for (auto& x : m_stack) {
    result += x.print(env);
    result += '\n';
  }
  return result;
}

void FormStack::push_value_to_reg(RegisterAccess var,
                                  Form* value,
                                  bool sequence_point,
                                  TypeSpec type,
                                  const SetVarInfo& info) {
  ASSERT(value);
  StackEntry entry;
  entry.active = true;  // by default, we should display everything!
  entry.sequence_point = sequence_point;
  entry.destination = var;
  entry.source = value;
  entry.set_type = type;
  entry.set_info = info;
  m_stack.push_back(entry);
}

void FormStack::push_value_to_reg_dead(RegisterAccess var,
                                       Form* value,
                                       bool sequence_point,
                                       TypeSpec type,
                                       const SetVarInfo& info) {
  ASSERT(value);
  StackEntry entry;
  entry.active = false;
  entry.sequence_point = sequence_point;
  entry.destination = var;
  entry.source = value;
  entry.set_type = type;
  entry.set_info = info;
  m_stack.push_back(entry);
}

void FormStack::push_non_seq_reg_to_reg(const RegisterAccess& dst,
                                        const RegisterAccess& src,
                                        Form* src_as_form,
                                        TypeSpec type,
                                        const SetVarInfo& info) {
  ASSERT(src_as_form);
  StackEntry entry;
  entry.active = true;
  entry.sequence_point = false;
  entry.destination = dst;
  entry.non_seq_source = src;
  entry.source = src_as_form;
  entry.set_type = type;
  entry.set_info = info;
  entry.is_compactable = true;
  m_stack.push_back(entry);
}

bool FormStack::is_single_expression() {
  int count = 0;
  for (auto& e : m_stack) {
    if (e.active) {
      count++;
    }
  }
  return count == 1;
}

void FormStack::push_form_element(FormElement* elt, bool sequence_point) {
  StackEntry entry;
  entry.active = true;
  entry.elt = elt;
  entry.sequence_point = sequence_point;
  m_stack.push_back(entry);
}

Form* FormStack::pop_reg(const RegisterAccess& var,
                         const RegSet& barrier,
                         const Env& env,
                         bool allow_side_effects,
                         int begin_idx) {
  return pop_reg(var.reg(), barrier, env, allow_side_effects, begin_idx);
}

namespace {
bool nonempty_intersection(const RegSet& a, const RegSet& b) {
  for (auto x : a) {
    if (b.find(x) != b.end()) {
      return true;
    }
  }
  return false;
}
}  // namespace

Form* FormStack::pop_reg(Register reg,
                         const RegSet& barrier,
                         const Env& env,
                         bool allow_side_effects,
                         int begin_idx,
                         RegisterAccess* orig_out,
                         bool* found_orig_out) {
  if (found_orig_out) {
    *found_orig_out = false;
  }
  ASSERT(allow_side_effects);
  (void)env;  // keep this for easy debugging.
  RegSet modified;
  size_t begin = m_stack.size();
  if (begin_idx >= 0) {
    begin = begin_idx;
  }
  for (size_t i = begin; i-- > 0;) {
    auto& entry = m_stack.at(i);
    if (entry.active) {
      if (entry.destination.has_value() && entry.destination->reg() == reg) {
        entry.source->get_modified_regs(modified);
        if (!allow_side_effects && entry.source->has_side_effects()) {
          // the source of the set! has a side effect and that's not allowed, so abort.
          return nullptr;
        }
        if (nonempty_intersection(modified, barrier)) {
          // violating the barrier registers.
          return nullptr;
        }
        entry.active = false;
        ASSERT(entry.source);
        if (entry.non_seq_source.has_value()) {
          ASSERT(entry.sequence_point == false);
          auto result = pop_reg(entry.non_seq_source->reg(), barrier, env, allow_side_effects, i);
          if (result) {
            if (found_orig_out) {
              *found_orig_out = true;
              *orig_out = *entry.destination;
            }
            return result;
          }
        }

        if (found_orig_out) {
          *found_orig_out = true;
          *orig_out = *entry.destination;
        }
        return entry.source;
      } else {
        // we didn't match
        if (entry.sequence_point) {
          // and it's a sequence point! can't look any more back than this.
          return nullptr;
        }
        // no match, and not a sequence:
        if (entry.source) {
          ASSERT(!entry.elt);
          entry.source->get_modified_regs(modified);
          if (!allow_side_effects) {
            // shouldn't allow skipping past a set! (may be too conservative?)
            return nullptr;
          }
        } else {
          ASSERT(entry.elt);
          entry.elt->get_modified_regs(modified);
          if (!allow_side_effects && entry.elt->has_side_effects()) {
            // shouldn't allow skipping past something with a set! (also may be too conservative?)
            return nullptr;
          }
        }
      }
    } else {
      if (entry.destination.has_value() && entry.destination->reg() == reg) {
        return nullptr;
      }
    }
  }
  // we didn't have it...
  return nullptr;
}

FormElement* FormStack::pop_back(FormPool& pool) {
  auto& back = m_stack.back();
  ASSERT(back.active);
  back.active = false;
  if (back.elt) {
    return back.elt;
  } else {
    ASSERT(back.destination.has_value());
    auto elt = pool.alloc_element<SetVarElement>(*back.destination, back.source,
                                                 back.sequence_point, back.set_type, back.set_info);
    back.source->parent_element = elt;
    return elt;
  }
}

namespace {
bool is_op_in_place(SetVarElement* elt,
                    FixedOperatorKind op,
                    const Env& env,
                    RegisterAccess* base_out,
                    Form** val_out) {
  auto matcher = Matcher::op(GenericOpMatcher::fixed(op), {Matcher::any_reg(0), Matcher::any(1)});
  auto result = match(matcher, elt->src());
  if (result.matched) {
    auto first = result.maps.regs.at(0);
    ASSERT(first.has_value());

    if (first->reg() != elt->dst().reg()) {
      return false;
    }

    if (first->idx() != elt->dst().idx()) {
      return false;
    }

    auto src_var = env.get_variable_name(*first);
    auto dst_var = env.get_variable_name(elt->dst());
    if (src_var != dst_var) {
      // something like daddu a1-1, a1-0, v0 isn't safe to turn into an in-place, but will pass
      // the previous two checks.
      return false;
    }

    // also check with casts. This avoid something like
    // (set! x (+ (the <blah> x) 1))
    auto src_var_cast = env.get_variable_name_with_cast(*first);
    if (src_var_cast.print() != dst_var) {
      return false;
    }

    *val_out = result.maps.forms.at(1);
    *base_out = first.value();
    return true;
  }
  return false;
}

FormElement* rewrite_set_op_in_place_for_kind(SetVarElement* in,
                                              const Env& env,
                                              FormPool& pool,
                                              FixedOperatorKind first_kind,
                                              FixedOperatorKind in_place_kind) {
  Form* val = nullptr;
  RegisterAccess base;

  if (is_op_in_place(in, first_kind, env, &base, &val)) {
    return pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(in_place_kind),
        std::vector<Form*>{
            pool.alloc_single_element_form<SimpleAtomElement>(nullptr, SimpleAtom::make_var(base)),
            val});
  }
  return in;
}

FormElement* try_rewrites_in_place(SetVarElement* in, const Env& env, FormPool& pool) {
  auto out = rewrite_set_op_in_place_for_kind(in, env, pool, FixedOperatorKind::ADDITION,
                                              FixedOperatorKind::ADDITION_IN_PLACE);
  if (out != in) {
    return out;
  }

  out = rewrite_set_op_in_place_for_kind(in, env, pool, FixedOperatorKind::ADDITION_PTR,
                                         FixedOperatorKind::ADDITION_PTR_IN_PLACE);
  if (out != in) {
    return out;
  }

  return in;
}
}  // namespace

std::vector<FormElement*> FormStack::rewrite(FormPool& pool, const Env& env) const {
  std::vector<FormElement*> result;

  for (auto& e : m_stack) {
    if (!e.active) {
      continue;
    }

    if (e.destination.has_value()) {
      // (set! x (+ x y)) -> (+! x y)

      // we want to untangle coloring moves here
      auto simplified_source = e.source;
      auto type = e.set_type;
      auto src_as_var = dynamic_cast<SimpleExpressionElement*>(e.source->try_as_single_element());
      if (src_as_var && src_as_var->expr().is_var() && e.is_compactable) {
        bool keep_going = true;
        auto var_to_get = src_as_var->expr().var();
        while (keep_going && !result.empty()) {
          keep_going = false;
          auto last_op_as_set = dynamic_cast<SetVarElement*>(result.back());
          if (last_op_as_set && last_op_as_set->dst().reg() == var_to_get.reg()) {
            result.pop_back();
            auto as_one = dynamic_cast<SimpleExpressionElement*>(
                last_op_as_set->src()->try_as_single_element());
            if (as_one && as_one->expr().is_identity() && as_one->expr().is_var() &&
                !result.empty()) {
              keep_going = true;
              var_to_get = as_one->expr().var();
            }
            simplified_source = last_op_as_set->src();
            // because we are eliminating, we need to use the source's cast.
            // to make the code cleaner, we drop casts that would occur in the middle
            type = last_op_as_set->src_type();
            // result = last_op_as_set->src()->elts();
          }
        }
      }

      auto expected_type = env.get_variable_type(*e.destination, true);
      if (!env.dts->ts.tc(expected_type, type)) {
        // we would cast. let's see if we can simplify the source to avoid this.
        auto casted = try_cast_simplify(simplified_source, expected_type, pool, env);
        if (casted) {
          simplified_source = casted;
          type = expected_type;
        }
      }

      auto elt = pool.alloc_element<SetVarElement>(*e.destination, simplified_source,
                                                   e.sequence_point, type, e.set_info);

      auto final_elt = try_rewrites_in_place(elt, env, pool);
      simplified_source->parent_element = final_elt;
      result.push_back(final_elt);
    } else {
      result.push_back(e.elt);
    }
  }

  if (result.empty()) {
    result.push_back(pool.alloc_element<EmptyElement>());
  }
  return result;
}

std::optional<RegisterAccess> rewrite_to_get_var(std::vector<FormElement*>& default_result,
                                                 FormPool& pool,
                                                 const RegisterAccess& var,
                                                 const Env& env) {
  bool keep_going = true;
  RegisterAccess var_to_get = var;

  std::vector<FormElement*> result;
  std::optional<RegisterAccess> result_access;

  bool first = true;
  while (keep_going && !default_result.empty()) {
    keep_going = false;
    auto last_op_as_set = dynamic_cast<SetVarElement*>(default_result.back());
    if (last_op_as_set && last_op_as_set->dst().reg() == var_to_get.reg() &&
        (first || last_op_as_set->info().is_compactable)) {
      default_result.pop_back();
      auto as_one =
          dynamic_cast<SimpleExpressionElement*>(last_op_as_set->src()->try_as_single_element());
      if (as_one && as_one->expr().is_identity() && as_one->expr().is_var() &&
          !default_result.empty()) {
        keep_going = true;
        var_to_get = as_one->expr().var();
      }

      auto cast = last_op_as_set->required_cast(env);
      if (cast && cast == TypeSpec("none")) {
        env.func->warnings.warning(
            "rewrite_to_get_var got a none typed variable. Is there unreachable code? [OP: {}]",
            last_op_as_set->dst().idx());
        cast = std::nullopt;
      }
      if (cast) {
        result = {pool.alloc_element<CastElement>(
            *cast, pool.alloc_sequence_form(nullptr, last_op_as_set->src()->elts()))};
      } else {
        result = last_op_as_set->src()->elts();
      }
      result_access = last_op_as_set->dst();
    }
    first = false;
  }

  if (result.empty()) {
    default_result.push_back(pool.alloc_element<SimpleAtomElement>(SimpleAtom::make_var(var)));
    return {};
  } else {
    for (auto x : result) {
      default_result.push_back(x);
    }
    return result_access;
  }
}

std::vector<FormElement*> rewrite_to_get_var(FormStack& stack,
                                             FormPool& pool,
                                             const RegisterAccess& var,
                                             const Env& env,
                                             std::optional<RegisterAccess>* used_var) {
  auto default_result = stack.rewrite(pool, env);
  auto uv = rewrite_to_get_var(default_result, pool, var, env);
  if (used_var) {
    *used_var = uv;
  }
  return default_result;
}

}  // namespace decompiler
