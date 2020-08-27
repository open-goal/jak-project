#include <util.h>
#include "Goal.h"

void Goal::generate_inspector_format_call(const std::string& format,
                                          std::vector<std::shared_ptr<Place>> args,
                                          std::shared_ptr<GoalEnv> env) {
  // get the format object
  auto format_func = compile_get_sym_val("format", env);
  auto true_sym = compile_get_sym_obj("#t", env);
  auto format_string = compile_string(format, env);
  auto el_hack = EmptyListObject::make_new();
  std::vector<std::shared_ptr<Place>> arg_regs = {true_sym, format_string};
  for (auto& a : args) {
    arg_regs.push_back(a);
  }
  compile_real_function_call(el_hack, format_func, arg_regs, env);
}

std::shared_ptr<Place> Goal::generate_inspector_for_type(std::shared_ptr<StructureType> type,
                                                         std::shared_ptr<GoalEnv> env) {
  TypeSpec this_typespec(get_base_typespec(type->get_name()));
  TypeSpec lambda_ts = get_base_typespec("function");

  // return and arg
  lambda_ts.ts_args.push_back(this_typespec);
  lambda_ts.ts_args.push_back(this_typespec);
  auto place = std::make_shared<LambdaPlace>(lambda_ts);

  GoalLambda& lambda = place->lambda;
  lambda.params.emplace_back("obj", this_typespec);

  auto new_func_env = std::make_shared<FunctionEnv>(type->get_name() + "-inspector-autogen");
  new_func_env->parent = env;
  new_func_env->segment = DEBUG_SEGMENT;  // todo not this
  RegConstraint constr;
  constr.instr_id = 0;
  constr.var_id = new_func_env->vars.size();
  constr.ass.kind = REGISTER;
  constr.ass.reg_id = ARG_REGS[0];
  auto obj = new_func_env->alloc_reg(get_base_typespec(type->get_name()));
  new_func_env->params["obj"] = obj;
  new_func_env->constrain_reg(constr);

  place->func = new_func_env;
  new_func_env->emit(make_unique<IR_FunctionBegin>(place));
  auto return_reg = new_func_env->alloc_reg(get_base_typespec(type->get_name()));

  bool is_basic = (std::dynamic_pointer_cast<BasicType>(type) != nullptr);
  bool is_structure = (std::dynamic_pointer_cast<StructureType>(type) != nullptr);
  int offset = 0;
  if (is_basic) {
    // (format #t "[~8x] ~A~%" obj (->4 obj -4))
    offset = -4;
    auto type_ptr_ir = make_unique<IR_LoadConstOffset>(
        new_func_env->alloc_reg(get_base_typespec("type")), obj, -4, 4, false);
    auto dst = type_ptr_ir->dst;
    new_func_env->emit(std::move(type_ptr_ir));
    generate_inspector_format_call("[~8x] ~A~%", {obj, dst}, new_func_env);

  } else {
    generate_inspector_format_call("[~8x] " + type->get_name() + "~%", {obj}, new_func_env);
  }

  for (auto& field : type->fields) {
    if (is_basic && field.name == "type")
      continue;

    std::string format_string = "~T" + field.name + ": ";
    // the char
    if (field.type.type->is_boxed) {
      format_string += "~A";
    } else {
      auto ts = TypeSpec(field.type);
      if (field.type.type->get_name() == "float") {
        format_string += "~f";
      } else if (is_integer(ts)) {
        format_string += "~d";
      } else if (field.type.type->get_name() == "pointer") {
        format_string += "#x~X";
      }

      else if (is_structure) {
        format_string += "#<" + field.type.type->get_name() + " @ #x~X>";
      }

      else {
        throw std::runtime_error("don't know how to generate an inspector for field of type " +
                                 field.type.type->get_name());
      }
    }
    format_string += "~%";

    if (!field.is_inline) {
      auto get_val_ir = make_unique<IR_LoadConstOffset>(
          new_func_env->alloc_reg(field.type.type), obj, offset + field.offset,
          field.type.type->load_size, field.type.type->load_signed);
      auto dst = get_val_ir->dst;
      new_func_env->emit(std::move(get_val_ir));
      generate_inspector_format_call(format_string, {dst}, new_func_env);
    } else {
      auto dest_reg = compile_integer_to_gpr(offset + field.offset, new_func_env);
      auto get_val_ir = make_unique<IR_IntegerMath>(ADD_64, dest_reg, obj);
      new_func_env->emit(std::move(get_val_ir));
      generate_inspector_format_call(format_string, {dest_reg}, new_func_env);
    }
  }

  auto return_ir = make_unique<IR_Return>(resolve_to_gpr(obj, new_func_env), return_reg);
  return_reg->type = return_ir->value->type;
  new_func_env->emit(std::move(return_ir));
  new_func_env->finish();
  auto obj_env = get_parent_env_of_type<ObjectFileEnv>(new_func_env);
  obj_env->functions.push_back(new_func_env);

  return place;
}