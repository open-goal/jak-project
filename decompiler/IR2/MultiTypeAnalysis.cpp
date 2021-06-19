/*!
 * @file MultiTypeAnalysis.cpp
 * The "new" type analysis pass which considers multiple possible types that can be at each
 * register, due to overlapping fields in types.  When it encounters a function call, set, or
 * certain math operation, it will attempt to prune the decision tree to remove incompatible types.
 *
 * When there are multiple ways to get the same type, or the type is ambiguous, it will use the one
 * with the highest score.
 *
 * Compared to the previous type analysis pass, there is more of an focus on being fast, as this is
 * historically the slowest part of decompilation.
 *
 * It will attempt to propagate these decision trees across basic block boundaries, but any time
 * there is a "phi node" where a registers can possible come from two different sources, it will
 * prune the tree to a single decision there.
 */

#include <limits>

#include "common/util/assert.h"
#include "decompiler/Function/Warnings.h"
#include "MultiTypeAnalysis.h"
#include "decompiler/IR2/Env.h"

namespace decompiler {

using RegState = CopyOnWrite<RegisterTypeState>;

bool DerefHint::matches(const FieldReverseLookupOutput& value) const {
  if (value.tokens.size() != tokens.size()) {
    return false;
  }

  for (size_t i = 0; i < value.tokens.size(); i++) {
    if (!tokens[i].matches(value.tokens[i])) {
      return false;
    }
  }

  return true;
}

bool DerefHint::Token::matches(const FieldReverseLookupOutput::Token& other) const {
  switch (kind) {
    case Kind::INTEGER:
      return other.kind == FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX &&
             other.idx == integer;
    case Kind::FIELD:
      return other.kind == FieldReverseLookupOutput::Token::Kind::FIELD && other.name == name;
    case Kind::VAR:
      return other.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX;
    default:
      assert(false);
  }
}

/*!
 * Safely access the decision referenced by this TypeDecisionParent.
 * This will work even if the actual RegisterTypeState has been modified since the reference was
 * created.
 */
const PossibleType& TypeDecisionParent::get() const {
  return instruction->get_const(reg).possible_types.at(type_index);
}

/*!
 * Figure out if this has been eliminated or not. Caches the result to avoid looking it up again and
 * again. Elimination cannot be undone.
 */
bool PossibleType::is_valid() const {
  if (!m_valid_cache) {
    return false;
  }

  if (parent.instruction) {
    // we have a parent in the tree, check if that parent is eliminated.
    if (!parent.get().is_valid()) {
      m_valid_cache = false;
      return false;
    }
  }

  return true;
}

/*!
 * If we have multiple types, pick the one with the highest deref path score.
 * If warnings is set, and we have to throw away a valid type, prints a warning that we made a
 * somewhat arbitrary decision to throw a possible type.
 *
 * After calling this, you can use get_single_tp_type and get_single_type_decision.
 */
void RegisterTypeState::reduce_to_single_type(DecompWarnings* warnings,
                                              int op_idx,
                                              const DerefHint* hint) {
  double best_score = -std::numeric_limits<double>::infinity();
  int best_idx = -1;
  bool printed_first_warning = false;
  std::string warning_string;

  // find the highest score that's valid.
  for (int i = 0; i < (int)possible_types.size(); i++) {
    if (possible_types[i].deref_score > best_score && possible_types[i].is_valid()) {
      best_idx = i;
      best_score = possible_types[i].deref_score;
    }

    // if we match the hint, just use that.
    if (possible_types[i].deref_path && hint->matches(*possible_types[i].deref_path)) {
      best_idx = i;
      warnings = nullptr;  // never warn if we take the hint
      break;
    }
  }
  assert(best_idx != -1);

  // eliminate stuff that isn't the best.
  for (int i = 0; i < (int)possible_types.size(); i++) {
    if (i != best_idx) {
      // warn if we eliminate something that is possibly valid.
      if (warnings && possible_types[i].is_valid()) {
        if (!printed_first_warning) {
          warning_string += fmt::format("Ambiguous type selection at op {}\n", op_idx);
          printed_first_warning = true;
        }
        if (possible_types[best_idx].deref_path) {
          warning_string += fmt::format("  {}\n", possible_types[best_idx].deref_path->print());
        } else {
          warning_string += fmt::format("  {}\n", possible_types[best_idx].type.print());
        }
      }

      possible_types[i].eliminate();
    }
  }

  // cache the winner
  single_type_cache = best_idx;

  if (warnings && printed_first_warning) {
    warnings->general_warning(warning_string);
  }
}

/*!
 * After this has been pruned to a single type, gets that type decision.
 */
const PossibleType& RegisterTypeState::get_single_type_decision() const {
  assert(single_type_cache.has_value());
  assert(possible_types.at(*single_type_cache).is_valid());  // todo remove.
  return possible_types[*single_type_cache];
}

/*!
 * After this has been pruned to a single type, gets it as a TP_Type.
 */
const TP_Type& RegisterTypeState::get_single_tp_type() const {
  return get_single_type_decision().type;
}

/*!
 * If there is at least one possibility to get a desired_type, removes anything that's not a
 * desired_type. If it's not possible to get a desired type, does nothing.
 */
void RegisterTypeState::try_elimination(const TypeSpec& desired_types, const TypeSystem& ts) {
  std::vector<int> to_eliminate;
  int keep_count = 0;
  for (int i = 0; i < (int)possible_types.size(); i++) {
    const auto& possibility = possible_types[i];
    if (possibility.is_valid()) {
      if (ts.tc(desired_types, possibility.type.typespec())) {
        keep_count++;
      } else {
        to_eliminate.push_back(i);
      }
    }
  }

  if (keep_count > 0) {
    for (auto idx : to_eliminate) {
      possible_types.at(idx).eliminate();
    }
  }
}

namespace {

/*!
 * Create a register type state with no parent and the given typespec.
 */
RegState make_typespec_parent_regstate(const TypeSpec& typespec) {
  RegState result = make_cow
}

/*!
 * Create an instruction type state for the first instruction of a function.
 */
InstrTypeState construct_initial_typestate(const TypeSpec& function_type,
                                           const Env& env,
                                           const RegState& uninitialized) {
  // start with everything unintialized
  InstrTypeState result(uninitialized);
  assert(function_type.base_type() == "function");
  assert(function_type.arg_count() >= 1);      // must know the function type.
  assert(function_type.arg_count() <= 8 + 1);  // 8 args + 1 return.

  for (int i = 0; i < int(function_type.arg_count()) - 1; i++) {
    auto reg_id = Register::get_arg_reg(i);
    const auto& reg_type = function_type.get_arg(i);
    result.get(Register(Reg::GPR, reg_id)) = TP_Type::make_from_ts(reg_type);
  }

}

}  // namespace

}  // namespace decompiler