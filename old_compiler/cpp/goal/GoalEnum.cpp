/*!
 * @file GoalEnum.cpp
 * Implementation of GOAL Enums
 */

#include "logger/Logger.h"
#include "GoalEnum.h"
#include "Goal.h"

/*!
 * Compile a enum definition.
 */
std::shared_ptr<Place> Goal::compile_defenum(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env) {
  // format is (defenum name [options] [entries])
  (void)env;

  // name
  auto enum_name = symbol_string(pair_car(rest));
  rest = pair_cdr(rest);

  // default enum type will be int32.
  auto enum_type = get_base_typespec("int32");
  bool is_bitfield = false;

  auto current = pair_car(rest);
  while (current.type == SYMBOL && symbol_string(current).at(0) == ':') {
    auto option_name = symbol_string(current);
    rest = pair_cdr(rest);
    auto option_value = pair_car(rest);
    rest = pair_cdr(rest);

    current = pair_car(rest);

    if (option_name == ":type") {
      enum_type = compile_typespec(option_value);
    } else if (option_name == ":bitfield") {
      if (symbol_string(option_value) == "#t") {
        is_bitfield = true;
      } else if (symbol_string(option_value) == "#f") {
        is_bitfield = false;
      } else {
        throw_compile_error(form, "invalid option to :bitfield option");
      }
    } else {
      throw_compile_error(form, "unknown option for defenum: " + option_name);
    }
  }

  GoalEnum new_enum;
  new_enum.ts = enum_type;
  new_enum.is_bitfield = is_bitfield;

  while (rest.type != EMPTY_LIST) {
    auto def = pair_car(rest);
    auto name = symbol_string(pair_car(def));
    def = pair_cdr(def);
    auto value = pair_car(def);
    if (value.type != INTEGER) {
      throw_compile_error(def, "expect integer");
    }

    def = pair_cdr(def);
    if (def.type != EMPTY_LIST) {
      throw_compile_error(def, "too many values in enum value definition");
    }

    new_enum.entries[name] = value.integer_obj.value;
    rest = pair_cdr(rest);
  }

  auto existing_kv = enums.find(enum_name);
  if (existing_kv != enums.end() && existing_kv->second != new_enum) {
    gLogger.log(MSG_WARN, "defenum changes the definition of existing enum %s", enum_name.c_str());
  }
  enums[enum_name] = new_enum;

  return get_none();
}

/*!
 * Look up a value from an enum.
 */
std::shared_ptr<Place> Goal::compile_enum_lookup(GoalEnum& e,
                                                 Object rest,
                                                 std::shared_ptr<GoalEnv> env) {
  if (e.is_bitfield) {
    int64_t value = 0;
    for_each_in_list(rest, [&](Object o) {
      auto kv = e.entries.find(symbol_string(o));
      if (kv == e.entries.end()) {
        throw_compile_error(o, "value " + symbol_string(o) + " not found in enum");
      }
      value |= (1 << kv->second);
    });

    auto result = compile_integer(value, env);
    result->type = e.ts;
    return result;

  } else {
    throw_compile_error(rest, "enum non-bitfield lookup nyi");
  }

  return get_none();
}

/*!
 * Are two enums identical?
 */
bool GoalEnum::operator==(const GoalEnum& other) {
  return (ts == other.ts) && (is_bitfield == other.is_bitfield) && (entries == other.entries);
}

/*!
 * Are two enums different?
 */
bool GoalEnum::operator!=(const GoalEnum& other) {
  return !(*this == other);
}