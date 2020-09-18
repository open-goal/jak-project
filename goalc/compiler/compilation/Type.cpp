#include "goalc/compiler/Compiler.h"
#include "common/type_system/deftype.h"

RegVal* Compiler::compile_get_method_of_type(const TypeSpec& type,
                                             const std::string& method_name,
                                             Env* env) {
  auto info = m_ts.lookup_method(type.base_type(), method_name);
  auto offset_of_method = 16 + 4 * info.id;  // todo, something more flexible?

  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto typ = compile_get_symbol_value(type.base_type(), env)->to_gpr(env);
  MemLoadInfo load_info;
  load_info.sign_extend = false;
  load_info.size = POINTER_SIZE;

  return fe
      ->alloc_val<MemoryOffsetConstantVal>(typ->type(), typ, offset_of_method, load_info, info.type)
      ->to_reg(env);
}

Val* Compiler::compile_deftype(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)env;

  auto result = parse_deftype(rest, &m_ts);

  auto kv = m_symbol_types.find(result.type.base_type());
  if (kv != m_symbol_types.end() && kv->second.base_type() != "type") {
    fmt::print("[Warning] deftype will redefined {} from {} to a type.\n", result.type.base_type(),
               kv->second.print());
  }
  m_symbol_types[result.type.base_type()] = m_ts.make_typespec("type");

  auto new_type_method = compile_get_method_of_type(m_ts.make_typespec("type"), "new", env);
  auto new_type_symbol = compile_get_sym_obj(result.type.base_type(), env)->to_gpr(env);
  auto parent_type = compile_get_symbol_value(result.type_info->get_parent(), env)->to_gpr(env);
  auto flags_int = compile_integer(result.flags.flag, env)->to_gpr(env);
  return compile_real_function_call(form, new_type_method,
                                    {new_type_symbol, parent_type, flags_int}, env);
}
