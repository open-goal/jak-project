/*!
 * @file GoalUtil.cpp
 * Various GOAL utility functions related to types which do not cleanly fit anywhere else.
 */

#include "Goal.h"

/*!
 * Can source be stored in destination?
 * Checks only that the base type matches.
 * Throws a compile error if not.
 */
void Goal::typecheck_base_only(const Object& form,
                               TypeSpec& destination_type,
                               TypeSpec& source_type,
                               const std::string& error) {
  // source can be more specific
  if (!destination_type.typecheck_base_only(source_type, types)) {
    throw_compile_error(form, "Type check failure on " + error + ":\n" + destination_type.print() +
                                  " and " + source_type.print());
  }
}

void Goal::typecheck_for_set(const Object& form,
                             TypeSpec& destination_type,
                             TypeSpec& source_type,
                             const std::string& error) {
  if (source_type.type->get_name() == "integer") {
    if (is_integer(destination_type)) {
      return;
    }
  }

  if (source_type.type->get_name() == "boolean") {
    return;
  }

  typecheck_base_only(form, destination_type, source_type, error);
}

/*!
 * Get a base TypeSpec for a type with the given name.
 * Error if the type doesn't exist.
 */
TypeSpec Goal::get_base_typespec(const std::string& name) {
  auto t = types.types.find(name);
  if (t == types.types.end()) {
    throw std::runtime_error("could not find type " + name);
  }
  TypeSpec ts(t->second);
  return ts;
}

TypeSpec Goal::get_base_of_inline_array(TypeSpec ts) {
  if (ts.type->get_name() != "inline-array") {
    throw std::runtime_error("tried to get_base_of_inline_array of " + ts.print());
  }
  if (ts.ts_args.size() != 1) {
    throw std::runtime_error("invalid inline-array ts: " + ts.print());
  }
  return ts.ts_args.front();
}

TypeSpec Goal::get_base_of_pointer(TypeSpec ts) {
  if (ts.type->get_name() != "pointer") {
    throw std::runtime_error("tried to get_base_of_pointer of " + ts.print());
  }
  if (ts.ts_args.size() != 1) {
    throw std::runtime_error("invalid pointer ts: " + ts.print());
  }
  return ts.ts_args.front();
}

/*!
 * Get the list of all parent types.
 */
std::vector<std::shared_ptr<GoalType>> Goal::get_parents(std::shared_ptr<GoalType> t) {
  auto parent = t->parent;
  std::vector<std::shared_ptr<GoalType>> result;
  result.push_back(t);
  while (!parent.empty()) {
    auto pt = get_base_typespec(parent).type;
    result.push_back(pt);
    parent = pt->parent;
  }
  return result;
}

TypeSpec Goal::lowest_common_ancestor(TypeSpec a, TypeSpec b) {
  if (a.type == b.type) {
    if (a == b) {
      return a;
    } else {
      return get_base_typespec(a.type->get_name());
    }
  }

  auto a_up = get_parents(a.type);
  auto b_up = get_parents(b.type);

  int ai = a_up.size() - 1;
  int bi = b_up.size() - 1;

  std::shared_ptr<GoalType> parent_type = nullptr;
  while (ai >= 0 && bi >= 0) {
    if (a_up.at(ai) == b_up.at(bi)) {
      parent_type = a_up.at(ai);
    } else {
      break;
    }
    ai--;
    bi--;
  }

  if (!parent_type) {
    throw std::runtime_error("invalid types in lowest_common_ancestor: " + a.print() + " AND " +
                             b.print());
  }

  return get_base_typespec(parent_type->get_name());
}

TypeSpec Goal::lowest_common_ancestor(std::vector<TypeSpec> ts) {
  if (ts.empty()) {
    return get_base_typespec("none");
  }

  for (auto& x : ts) {
    if (x.type->get_name() == "none") {
      return get_base_typespec("none");
    }
  }

  TypeSpec result = ts.front();

  for (uint32_t i = 1; i < ts.size(); i++) {
    result = lowest_common_ancestor(result, ts.at(i));
  }

  return result;
}

bool Goal::is_basic(TypeSpec& ts) {
  return get_base_typespec("basic").typecheck_base_only(ts, types);
}