#include "Goal.h"

std::shared_ptr<Place> Goal::resolve_bitfield_to_gpr(std::shared_ptr<BitfieldPlace> in,
                                                     std::shared_ptr<GoalEnv> env) {
  auto result = env->alloc_reg(in->type);
  int field_offset = in->field.offset;
  int field_size = in->field.size;
  int field_left = 64 - (field_offset + field_size);
  assert(field_left >= 0);

  result = compile_fixed_shift(in->base, field_left, env, true, false);
  result = compile_fixed_shift(result, field_left + field_offset, env, false,
                               in->type.type->load_signed);
  result->type = in->type;
  return result;
}

static uint64_t build_mask(int size, int offset) {
  return ~(((1ll << size) - 1) << offset);
}

std::shared_ptr<Place> Goal::set_bitfield(std::shared_ptr<BitfieldPlace> dest,
                                          std::shared_ptr<Place> value,
                                          std::shared_ptr<GoalEnv> env) {
  value = resolve_to_gpr(value, env);
  int field_offset = dest->field.offset;
  int field_size = dest->field.size;
  int field_left = 64 - (field_offset + field_size);

  // check for sext needed
  auto& dest_type = dest->type.type;
  if (dest_type->load_signed) {
    ice("unsupported sext needed in set bitfield");
  }

  // kill the extra bits
  value->type = get_base_typespec("integer");
  auto left_shifted = compile_fixed_shift(value, (64 - field_size), env, true, false);
  // move to right spot
  auto located = compile_fixed_shift(left_shifted, field_left, env, false, false);

  // get the mask to clear the destination
  auto mask = compile_integer_to_gpr(build_mask(field_size, field_offset), env);

  // CLEAR
  env->emit(make_unique<IR_IntegerMath>(AND_64, dest->base, mask));

  // WRITE
  env->emit(make_unique<IR_IntegerMath>(OR_64, dest->base, located));

  // ???
  return dest->base;
}