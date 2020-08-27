#include "Goal.h"
#include "util.h"

// (new 'global ...
// (new 'static ...
// (new 'stack

std::shared_ptr<Place> Goal::compile_new(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  auto allocation = quoted_sym_as_string(pair_car(rest));
  rest = pair_cdr(rest);

  // auto type_of_obj = get_base_typespec(quoted_sym_as_string(pair_car(rest)));
  auto type_as_string = quoted_sym_as_string(pair_car(rest));
  rest = pair_cdr(rest);

  if (allocation == "global" || allocation == "debug") {
    if (type_as_string == "inline-array") {
      auto elt_type = get_base_typespec(quoted_sym_as_string(pair_car(rest)));
      rest = pair_cdr(rest);
      auto elt_count_obj = pair_car(rest);
      expect_empty_list(pair_cdr(rest));

      //      if(elt_count_obj.type != INTEGER) {
      //        throw_compile_error(form, "array size must be integer");
      //      }
      //      auto elt_count = elt_count_obj.integer_obj.value;
      auto elt_count = compile_to_integer_constant(elt_count_obj, env);
      auto mem_size = elt_count * elt_type.type->size;  // it's an inline array
      //      printf("new inline array of size %ld (= %ld * %d)", mem_size, elt_count,
      //      elt_type.type->size);

      auto malloc_func = compile_get_sym_val("malloc", env);

      std::vector<std::shared_ptr<Place>> args;
      args.push_back(compile_get_sym_obj(allocation, env));
      args.push_back(compile_integer_to_gpr(mem_size, env));

      auto result = compile_real_function_call(form, malloc_func, args, env);
      result->type = TypeSpec(get_base_typespec("inline-array").type, {elt_type});
      return result;
    } else {
      auto type_of_obj = get_base_typespec(type_as_string);
      std::vector<std::shared_ptr<Place>> args;
      // allocation
      args.push_back(compile_get_sym_obj(allocation, env));
      // type
      args.push_back(compile_get_sym_val(type_of_obj.type->get_name(), env));
      // the other arguments
      for_each_in_list(rest, [&](Object o) { args.push_back(compile_error_guard(o, env)); });

      auto new_method = compile_get_method_of_type(type_of_obj, "new", env);

      auto new_obj = compile_real_function_call(form, new_method, args, env);
      new_obj->type = type_of_obj;
      return new_obj;
    }
  } else if (allocation == "static") {
    auto type_of_obj = get_base_typespec(type_as_string);
    return compile_make_static_object_of_type(form, type_of_obj, rest, env);
  }

  throw_compile_error(form, "unsupported new form");
  return get_none();
}