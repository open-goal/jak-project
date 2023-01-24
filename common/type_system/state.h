#pragma once

#include <optional>

#include "common/type_system/TypeSpec.h"

/*!
 * This contains type system utilities related to state and process
 */

enum class StateHandler { ENTER, EXIT, CODE, TRANS, POST, EVENT };

class TypeSystem;

TypeSpec state_to_go_function(const TypeSpec& state_type, const TypeSpec& return_type);
StateHandler handler_keyword_to_kind(std::string keyword);
StateHandler handler_name_to_kind(const std::string& name);
std::string handler_kind_to_name(StateHandler kind);
TypeSpec get_state_handler_type(const std::string& handler_name, const TypeSpec& state_type);
TypeSpec get_state_handler_type(StateHandler kind, const TypeSpec& state_type);
std::vector<std::string> get_state_handler_arg_names(StateHandler kind);

std::optional<TypeSpec> get_state_type_from_enter_and_code(const TypeSpec& enter_func_type,
                                                           const TypeSpec& code_func_type,
                                                           const TypeSpec& proc_type,
                                                           const TypeSystem& ts);
std::optional<TypeSpec> get_state_type_from_func(const TypeSpec& func_type,
                                                 const TypeSpec& proc_type);
