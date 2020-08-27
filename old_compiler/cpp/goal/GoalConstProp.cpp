/*!
 * @file GoalConstProp.cpp
 * Utility functions to deal with constant propagation, including integers, memory addresses,
 * address of, and resolving various optimized const-prop Places into actual register Places.
 */

#include <util.h>
#include "Goal.h"

/*!
 * Convert a BitField place into a gpr.
 * Return nullptr if this cannot be done.
 */
std::shared_ptr<Place> Goal::try_resolve_bitfield_to_gpr(std::shared_ptr<Place> in,
                                                         std::shared_ptr<GoalEnv> env) {
  auto as_bitfield = std::dynamic_pointer_cast<BitfieldPlace>(in);
  if (as_bitfield) {
    return resolve_bitfield_to_gpr(as_bitfield, env);
  }
  return nullptr;
}

/*!
 * Convert a static (or address of a static) into a GPR.
 * Return nullptr if this cannot be done, or if "in" is not a static.
 */
std::shared_ptr<Place> Goal::try_resolve_static_to_gpr(std::shared_ptr<Place> in,
                                                       std::shared_ptr<GoalEnv> env) {
  auto as_static = std::dynamic_pointer_cast<StaticPlace>(in);
  if (as_static) {
    if (as_static->object->load_size() == -1) {
      // not a value type
      auto ir = make_unique<IR_StaticVarAddr>();
      ir->dest = env->alloc_reg(as_static->type);
      ir->src = as_static;
      auto dest = ir->dest;
      env->emit(std::move(ir));
      auto result_as_gpr = std::dynamic_pointer_cast<GprPlace>(dest);
      assert(result_as_gpr);
      return result_as_gpr;
    } else if (as_static->object->load_size() == 4) {
      // is a value type of size 4.
      auto ir = make_unique<IR_StaticVar32>();
      ir->dest = env->alloc_reg(as_static->type);
      ir->src = as_static;
      auto dest = ir->dest;
      env->emit(std::move(ir));
      auto result_as_gpr = std::dynamic_pointer_cast<GprPlace>(dest);
      assert(result_as_gpr);
      return result_as_gpr;
    } else {
      // unhandled value type size - TODO!
      ice("unhandled value type size in Goal::try_resolve_static_to_gpr");
      assert(false);
      return nullptr;
    }
  } else {
    return nullptr;
  }
}

/*!
 * Convert a lambda into a GPR containing the function address.
 * Return nullptr if this cannot be done, or if "in" is not a lambda.
 */
std::shared_ptr<Place> Goal::try_resolve_lambda_to_gpr(std::shared_ptr<Place> in,
                                                       std::shared_ptr<GoalEnv> env) {
  auto as_func = std::dynamic_pointer_cast<LambdaPlace>(in);
  if (as_func) {
    auto ir = make_unique<IR_FunctionAddr>();
    ir->dest = env->alloc_reg(as_func->type);
    ir->src = as_func;
    auto dest = ir->dest;
    env->emit(std::move(ir));
    auto result_as_gpr = std::dynamic_pointer_cast<GprPlace>(dest);
    assert(result_as_gpr);
    return result_as_gpr;
  } else {
    return nullptr;
  }
}

/*!
 * Convert an XMM register into a GPR.
 * Return nullptr if this cannot be done, or if "in" is not an xmm register.
 */
std::shared_ptr<Place> Goal::try_resolve_xmm_to_gpr(std::shared_ptr<Place> in,
                                                    std::shared_ptr<GoalEnv> env) {
  auto as_xmm = std::dynamic_pointer_cast<XmmPlace>(in);
  if (as_xmm) {
    auto ir = make_unique<IR_Set>();
    ir->dest = env->alloc_reg(as_xmm->type);
    ir->src = as_xmm;
    auto dest = ir->dest;
    env->emit(std::move(ir));
    auto result_as_gpr = std::dynamic_pointer_cast<GprPlace>(dest);
    assert(result_as_gpr);
    return result_as_gpr;
  } else {
    return nullptr;
  }
}

/*!
 * Convert a memory place into a GPR containing the value.
 * Return nullptr if this cannot be done, or if "in" is not a memory place.
 */
std::shared_ptr<Place> Goal::try_resolve_mem_to_gpr(std::shared_ptr<Place> in,
                                                    std::shared_ptr<GoalEnv> env) {
  auto as_mem_deref = std::dynamic_pointer_cast<MemoryDerefPlace>(in);
  if (as_mem_deref) {
    auto ptr = as_mem_deref->base;

    // optimization!
    auto ptr_as_mem_c_offset = std::dynamic_pointer_cast<MemoryOffsetConstPlace>(ptr);
    if (ptr_as_mem_c_offset) {
      auto ir = make_unique<IR_LoadConstOffset>(
          env->alloc_reg(as_mem_deref->type), resolve_to_gpr(ptr_as_mem_c_offset->base, env),
          ptr_as_mem_c_offset->offset, as_mem_deref->type.type->load_size,
          as_mem_deref->type.type->load_signed);
      auto dest = ir->dst;
      env->emit(std::move(ir));
      return dest;
    }

    // no optimization is implemented yet.
    ptr = resolve_to_gpr(ptr, env);
    auto ir = make_unique<IR_LoadConstOffset>(env->alloc_reg(as_mem_deref->type), ptr,
                                              0,  // todo, optimize for this case
                                              as_mem_deref->type.type->load_size,
                                              as_mem_deref->type.type->load_signed);
    auto dest = ir->dst;
    env->emit(std::move(ir));
    return dest;
  }

  auto as_pair_ref = std::dynamic_pointer_cast<PairPlace>(in);
  if (as_pair_ref) {
    auto ptr = as_pair_ref->base;
    ptr = resolve_to_gpr(ptr, env);
    auto ir = make_unique<IR_LoadConstOffset>(
        env->alloc_reg(as_pair_ref->type), ptr, as_pair_ref->is_car ? -2 : 2, 4,
        false);  // todo, do we really want a signed load here?
    auto dest = ir->dst;
    env->emit(std::move(ir));
    return dest;
  }

  auto as_mem_c_offset = std::dynamic_pointer_cast<MemoryOffsetConstPlace>(in);
  if (as_mem_c_offset) {
    auto base = resolve_to_gpr(as_mem_c_offset->base, env);
    auto result = compile_integer_to_gpr(as_mem_c_offset->offset, env);
    env->emit(make_unique<IR_IntegerMath>(ADD_64, result, base));
    result->type = in->type;
    return result;
  }

  auto as_mem_v_offset = std::dynamic_pointer_cast<MemoryOffsetVarPlace>(in);
  if (as_mem_v_offset) {
    auto base = resolve_to_gpr(as_mem_v_offset->base, env);
    auto offset = resolve_to_gpr(as_mem_v_offset->offset, env);
    auto result = env->alloc_reg(in->type);
    env->emit(make_unique<IR_Set>(result, base));
    env->emit(make_unique<IR_IntegerMath>(ADD_64, result, offset));
    result->type = in->type;
    return result;
  }

  return nullptr;
}

/*!
 * Convert a static to an xmm register.
 * Return nullptr if this cannot be done, or if "in" is not a static.
 */
std::shared_ptr<Place> Goal::try_resolve_static_to_xmm(std::shared_ptr<Place> in,
                                                       std::shared_ptr<GoalEnv> env) {
  auto as_static = std::dynamic_pointer_cast<StaticPlace>(in);
  if (as_static) {
    if (as_static->object->load_size() == 4) {
      auto fenv = get_parent_env_of_type<FunctionEnv>(env);
      auto ir = make_unique<IR_StaticVar32>();
      ir->dest = fenv->alloc_xmm_reg(as_static->type);
      ir->src = as_static;
      auto dest = ir->dest;
      env->emit(std::move(ir));
      auto result_as_xmm = std::dynamic_pointer_cast<XmmPlace>(dest);
      assert(result_as_xmm);
      return result_as_xmm;
    } else {
      assert(false);
      return nullptr;
    }
  } else {
    return nullptr;
  }
}

/*!
 * Convert a gpr to an xmm register.
 * Return nullptr if this cannot be done, or if "in" is not a gpr.
 */
std::shared_ptr<Place> Goal::try_resolve_gpr_to_xmm(std::shared_ptr<Place> in,
                                                    std::shared_ptr<GoalEnv> env) {
  auto as_gpr = std::dynamic_pointer_cast<GprPlace>(in);
  if (as_gpr) {
    auto fenv = get_parent_env_of_type<FunctionEnv>(env);
    auto ir = make_unique<IR_Set>();
    ir->dest = fenv->alloc_xmm_reg(as_gpr->type);
    ir->src = as_gpr;
    auto dest = ir->dest;
    env->emit(std::move(ir));
    auto result_as_xmm = std::dynamic_pointer_cast<XmmPlace>(dest);
    assert(result_as_xmm);
    return result_as_xmm;
  } else {
    return nullptr;
  }
}

/*!
 * Convert an integer constant to gpr, if possible.
 */
std::shared_ptr<Place> Goal::try_resolve_integer_to_gpr(std::shared_ptr<Place> in,
                                                        std::shared_ptr<GoalEnv> env) {
  auto as_integer = std::dynamic_pointer_cast<IntegerConstantPlace>(in);
  if (as_integer) {
    auto result = compile_integer_to_gpr(as_integer->value, env);
    result->type = as_integer->type;
    return result;
  }
  return nullptr;
}

/*!
 * Convert a Place to a GPR place.
 * Except for NonePlace, which remains as none.
 * Error if cannot be converted to a gpr place.
 */
std::shared_ptr<Place> Goal::resolve_to_gpr(std::shared_ptr<Place> in,
                                            std::shared_ptr<GoalEnv> env) {
  // is it an alias, but maybe not a GPR alias?
  auto as_alias = std::dynamic_pointer_cast<AliasPlace>(in);
  if (as_alias) {
    auto result = resolve_to_gpr(as_alias->base, env);
    return std::make_shared<GprAliasPlace>(result, as_alias->type);
  }

  // is it an alias
  auto as_gpr_alias = std::dynamic_pointer_cast<GprAliasPlace>(in);
  if (as_gpr_alias) {
    return as_gpr_alias;
  }

  auto as_xmm_alias = std::dynamic_pointer_cast<XmmAliasPlace>(in);
  if (as_xmm_alias) {
    auto result = resolve_to_gpr(as_xmm_alias->parent, env);
    return std::make_shared<GprAliasPlace>(result, as_xmm_alias->type);
  }

  // is it already a GPR?
  auto as_gpr = std::dynamic_pointer_cast<GprPlace>(in);
  if (as_gpr)
    return as_gpr;

  // try as a static
  auto as_static = try_resolve_static_to_gpr(in, env);
  if (as_static)
    return as_static;

  // try as a lambda
  auto as_lambda = try_resolve_lambda_to_gpr(in, env);
  if (as_lambda)
    return as_lambda;

  // try as xmm register
  auto as_xmm = try_resolve_xmm_to_gpr(in, env);
  if (as_xmm)
    return as_xmm;

  // try as memory
  auto as_mem = try_resolve_mem_to_gpr(in, env);
  if (as_mem)
    return as_mem;

  // try as none
  auto as_none = std::dynamic_pointer_cast<NonePlace>(in);
  if (as_none)
    return as_none;

  // try as int
  auto as_int = try_resolve_integer_to_gpr(in, env);
  if (as_int)
    return as_int;

  auto as_bitfield = try_resolve_bitfield_to_gpr(in, env);
  if (as_bitfield) {
    return as_bitfield;
  }

  throw std::runtime_error("unable to resolve " + in->print() + " to gpr");
}

/*!
 * Convert a place to an XMM place.
 * If it is not possible, throw compiler error./
 */
std::shared_ptr<Place> Goal::resolve_to_xmm(std::shared_ptr<Place> in,
                                            std::shared_ptr<GoalEnv> env) {
  auto as_alias = std::dynamic_pointer_cast<AliasPlace>(in);
  if (as_alias) {
    auto result = resolve_to_xmm(as_alias->base, env);
    return std::make_shared<XmmAliasPlace>(result, as_alias->type);
  }

  auto as_xmm_alias = std::dynamic_pointer_cast<XmmAliasPlace>(in);
  if (as_xmm_alias) {
    return as_xmm_alias;
  }

  // is it already an xmm?
  auto as_xmm = std::dynamic_pointer_cast<XmmPlace>(in);
  if (as_xmm)
    return as_xmm;

  // try as a static
  auto as_static = try_resolve_static_to_xmm(in, env);
  if (as_static)
    return as_static;

  // try as a gpr
  auto as_gpr = try_resolve_gpr_to_xmm(in, env);
  if (as_gpr)
    return as_gpr;

  auto conv_gpr = resolve_to_gpr(in, env);
  if (conv_gpr) {
    as_gpr = try_resolve_gpr_to_xmm(conv_gpr, env);
    if (as_gpr)
      return as_gpr;
  }

  throw std::runtime_error("unable to resolve " + in->print() + " to xmm");
}

/*!
 * Convert a place to an XMM or a GPR.
 * This is designed for the case where you don't yet know which one you want.
 * It prefers doing nothing over converting
 * It prefers loading functions into GPRs
 * It will prefer statics which mark themselves as prefer xmm as loading into xmms
 * It will prefer GPRs over XMMs in cases where there is no clear winner.
 * Memory loads are always into gprs currently.
 */
std::shared_ptr<Place> Goal::resolve_to_gpr_or_xmm(std::shared_ptr<Place> in,
                                                   std::shared_ptr<GoalEnv> env) {
  auto as_alias = std::dynamic_pointer_cast<AliasPlace>(in);
  if (as_alias) {
    auto result = resolve_to_gpr_or_xmm(as_alias->base, env);
    if (std::dynamic_pointer_cast<XmmPlace>(result) ||
        std::dynamic_pointer_cast<XmmAliasPlace>(result)) {
      return std::make_shared<XmmAliasPlace>(result, as_alias->type);
    } else {
      return std::make_shared<GprAliasPlace>(result, as_alias->type);
    }
    // result->type = as_alias->type;
    return result;
  }

  auto as_xmm = std::dynamic_pointer_cast<XmmPlace>(in);
  if (as_xmm) {
    return as_xmm;
  }

  auto as_gpr = std::dynamic_pointer_cast<GprPlace>(in);
  if (as_gpr)
    return as_gpr;

  auto as_lambda = try_resolve_lambda_to_gpr(in, env);
  if (as_lambda)
    return as_lambda;

  // for statics, we can do either gpr or xmm:
  if (in->type.type->load_xmm_32_prefer) {
    auto as_static_xmm = try_resolve_static_to_xmm(in, env);
    if (as_static_xmm)
      return as_static_xmm;
    // todo try resolve memory to xmm.
  }

  // try as a static
  auto as_static = try_resolve_static_to_gpr(in, env);
  if (as_static)
    return as_static;

  // try as memory
  auto as_mem = try_resolve_mem_to_gpr(in, env);
  if (as_mem)
    return as_mem;

  // try as integer
  auto as_int = try_resolve_integer_to_gpr(in, env);
  if (as_int)
    return as_int;

  auto as_bitfield = try_resolve_bitfield_to_gpr(in, env);
  if (as_bitfield)
    return as_bitfield;

  throw std::runtime_error("unable to resolve " + in->print() + " to gpr/xmm");
}

/*!
 * Try to take the address of a place.  Only works if its a MemoryDerefPlace
 */
std::shared_ptr<Place> Goal::addr_of(std::shared_ptr<Place> plc, std::shared_ptr<GoalEnv> env) {
  (void)env;
  auto x = std::dynamic_pointer_cast<MemoryDerefPlace>(plc);
  if (!x) {
    throw std::runtime_error("cannot take the address of " + plc->print());
  }

  return x->base;
}

/*!
 * Attempt to compile an expression into an integer constant.
 * Errors if it doesn't get an integer constant, or if the comilation requires code to be executed.
 */
int64_t Goal::compile_to_integer_constant(Object form, std::shared_ptr<GoalEnv> env) {
  auto new_env = std::make_shared<NoEmitEnv>();
  new_env->parent = env;

  auto result = compile_error_guard(form, new_env);
  auto as_int_constant = std::dynamic_pointer_cast<IntegerConstantPlace>(result);
  if (!as_int_constant) {
    throw_compile_error(form, "unable to convert to integer constant!");
  }

  return as_int_constant->value;
}

/*!
 * Try to convert a place to an integer constant. If successful, return true and set out.
 * Otherwise return false.
 */
bool Goal::try_converting_to_integer_constant(Place& in, int64_t* out) {
  auto as_integer_constant = dynamic_cast<IntegerConstantPlace*>(&in);
  if (as_integer_constant) {
    *out = as_integer_constant->value;
    return true;
  } else {
    return false;
  }
}