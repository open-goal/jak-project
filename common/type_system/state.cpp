#include "state.h"

#include "common/type_system/TypeSystem.h"

/*!
 * Convert a (state <blah> ...) to the function required to go. Must be state.
 */
TypeSpec state_to_go_function(const TypeSpec& state_type, const TypeSpec& return_type) {
  ASSERT(state_type.base_type() == "state");
  std::vector<TypeSpec> arg_types;
  for (int i = 0; i < (int)state_type.arg_count() - 1; i++) {
    arg_types.push_back(state_type.get_arg(i));
  }

  arg_types.push_back(return_type);  // for the return.
  auto result = TypeSpec("function", arg_types);
  result.add_new_tag("behavior", state_type.last_arg().base_type());
  return result;
}

StateHandler handler_keyword_to_kind(std::string keyword) {
  // Remove the first character (should be a :)
  return handler_name_to_kind(keyword.erase(0, 1));
}

StateHandler handler_name_to_kind(const std::string& name) {
  if (name == "enter") {
    return StateHandler::ENTER;
  } else if (name == "exit") {
    return StateHandler::EXIT;
  } else if (name == "code") {
    return StateHandler::CODE;
  } else if (name == "event") {
    return StateHandler::EVENT;
  } else if (name == "trans") {
    return StateHandler::TRANS;
  } else if (name == "post") {
    return StateHandler::POST;
  } else {
    ASSERT(false);
  }
}

std::string handler_kind_to_name(StateHandler kind) {
  switch (kind) {
    case StateHandler::ENTER:
      return "enter";
    case StateHandler::EXIT:
      return "exit";
    case StateHandler::CODE:
      return "code";
    case StateHandler::EVENT:
      return "event";
    case StateHandler::TRANS:
      return "trans";
    case StateHandler::POST:
      return "post";
    default:
      ASSERT(false);
  }
}

TypeSpec get_state_handler_type(const std::string& handler_name, const TypeSpec& state_type) {
  return get_state_handler_type(handler_name_to_kind(handler_name), state_type);
}

TypeSpec get_state_handler_type(StateHandler kind, const TypeSpec& state_type) {
  TypeSpec result;
  switch (kind) {
    case StateHandler::CODE:
    case StateHandler::ENTER:
      result = state_to_go_function(state_type, TypeSpec("none"));
      break;

    case StateHandler::TRANS:
    case StateHandler::POST:
    case StateHandler::EXIT:
      result = TypeSpec("function", {TypeSpec("none")});
      break;

    case StateHandler::EVENT:
      result = TypeSpec("function", {TypeSpec("process"), TypeSpec("int"), TypeSpec("symbol"),
                                     TypeSpec("event-message-block"), TypeSpec("object")});
      break;

    default:
      ASSERT(false);
  }
  result.add_or_modify_tag("behavior", state_type.last_arg().base_type());
  return result;
}

std::vector<std::string> get_state_handler_arg_names(StateHandler kind) {
  switch (kind) {
    case StateHandler::CODE:
      // can have args, but are arbitrary
    case StateHandler::ENTER:
    case StateHandler::TRANS:
    case StateHandler::POST:
    case StateHandler::EXIT:
      return {};
    case StateHandler::EVENT:
      return {"proc", "argc", "message", "block"};
    default:
      ASSERT(false);
  }
}

namespace {
TypeSpec func_to_state_type(const TypeSpec& func_type, const TypeSpec& proc_type) {
  TypeSpec result("state");
  for (int i = 0; i < ((int)func_type.arg_count()) - 1; i++) {
    result.add_arg(func_type.get_arg(i));
  }
  result.add_arg(proc_type);
  return result;
}
}  // namespace

std::optional<TypeSpec> get_state_type_from_enter_and_code(const TypeSpec& enter_func_type,
                                                           const TypeSpec& code_func_type,
                                                           const TypeSpec& proc_type,
                                                           const TypeSystem& ts) {
  bool enter_real_func =
      enter_func_type.base_type() == "function" && enter_func_type.arg_count() > 0;
  bool code_real_func = code_func_type.base_type() == "function" && code_func_type.arg_count() > 0;

  if (enter_real_func && code_real_func) {
    int i = 0;
    TypeSpec result("state");
    for (; i < std::min((int)enter_func_type.arg_count(), (int)code_func_type.arg_count()) - 1;
         i++) {
      result.add_arg(
          ts.lowest_common_ancestor(enter_func_type.get_arg(i), code_func_type.get_arg(i)));
    }

    for (; i < ((int)enter_func_type.arg_count()) - 1; i++) {
      result.add_arg(enter_func_type.get_arg(i));
    }

    for (; i < ((int)code_func_type.arg_count()) - 1; i++) {
      result.add_arg(code_func_type.get_arg(i));
    }

    result.add_arg(proc_type);
    return result;
  } else if (enter_real_func) {
    return func_to_state_type(enter_func_type, proc_type);
  } else if (code_real_func) {
    return func_to_state_type(code_func_type, proc_type);
  } else {
    return {};
  }
}

std::optional<TypeSpec> get_state_type_from_func(const TypeSpec& func_type,
                                                 const TypeSpec& proc_type) {
  bool real_func = func_type.base_type() == "function" && func_type.arg_count() > 0;

  if (real_func) {
    TypeSpec result("state");
    for (int i = 0; i < (int)func_type.arg_count() - 1; i++) {
      result.add_arg(func_type.get_arg(i));
    }
    result.add_arg(proc_type);
    return result;
  } else {
    return {};
  }
}
