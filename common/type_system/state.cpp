#include "state.h"

/*!
 * Convert a (state <blah> ...) to the function required to go. Must be state.
 */
TypeSpec state_to_go_function(const TypeSpec& state_type) {
  assert(state_type.base_type() == "state");
  std::vector<TypeSpec> arg_types;
  for (int i = 0; i < (int)state_type.arg_count() - 1; i++) {
    arg_types.push_back(state_type.get_arg(i));
  }

  arg_types.push_back(TypeSpec("none"));  // none for the return.
  auto result = TypeSpec("function", arg_types);
  result.add_new_tag("behavior", state_type.last_arg().base_type());
  return result;
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
    assert(false);
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
      assert(false);
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
      result = state_to_go_function(state_type);
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
      assert(false);
  }
  result.add_or_modify_tag("behavior", state_type.last_arg().base_type());
  return result;
}