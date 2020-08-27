#include <util.h>
#include "Goal.h"

static int32_t get_offset_of_method(uint8_t id) {
  return 16 + id * 4;
}

std::shared_ptr<Place> Goal::compile_get_method_of_type(TypeSpec type,
                                                        const std::string& name,
                                                        std::shared_ptr<GoalEnv> env) {
  auto info = types.get_method_info(type.type->get_name(), name);
  auto get_sym_ir = make_unique<IR_GetSymbolValue>();
  TypeSpec symbol_type = get_base_typespec("symbol");
  get_sym_ir->symbol = std::make_shared<SymbolPlace>(type.type->get_name(), symbol_type);
  get_sym_ir->dest = env->alloc_reg(get_base_typespec("type"));
  auto dest = get_sym_ir->dest;
  env->emit(std::move(get_sym_ir));

  // next get the method pointer.
  // todo - not this
  int32_t offset = get_offset_of_method(info.id);
  auto result = env->alloc_reg(info.type);
  env->emit(make_unique<IR_LoadConstOffset>(result, dest, offset, 4, false));
  return result;
}

std::shared_ptr<Place> Goal::compile_get_method_of_object(std::shared_ptr<Place> object,
                                                          const std::string& method_name,
                                                          std::shared_ptr<GoalEnv> env) {
  auto& compile_time_type = object->type;
  object = resolve_to_gpr(object, env);

  // look up method info using compile time type
  auto method_info = types.get_method_info(compile_time_type.type->get_name(), method_name);

  // will contain the most accurate type at runtime
  std::shared_ptr<Place> runtime_type;
  if (is_basic(compile_time_type)) {
    // can lookup at runtime!
    runtime_type = env->alloc_reg(get_base_typespec("type"));
    env->emit(make_unique<IR_LoadConstOffset>(runtime_type, object, -4, 4, false));
  } else {
    // can't look up at runtime
    runtime_type = compile_get_sym_val(compile_time_type.type->get_name(), env);
  }

  // todo - not this!
  int32_t offset = get_offset_of_method(method_info.id);
  auto method = env->alloc_reg(method_info.type);  // so the method has the correct type
  env->emit(make_unique<IR_LoadConstOffset>(method, runtime_type, offset, 4, false));
  return method;
}

std::shared_ptr<Place> Goal::compile_method(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  (void)form;
  // (method obj method-name)
  //   OR
  // (method type method-name)

  auto arg = pair_car(rest);
  rest = pair_cdr(rest);
  auto method_name = symbol_string(pair_car(rest));
  expect_empty_list(pair_cdr(rest));

  if (arg.type == SYMBOL) {
    auto kv = types.types.find(symbol_string(arg));
    if (kv != types.types.end()) {
      return compile_get_method_of_type(compile_typespec(arg), method_name, env);
    }
  }

  auto obj = compile_error_guard(arg, env);
  return compile_get_method_of_object(obj, method_name, env);
}