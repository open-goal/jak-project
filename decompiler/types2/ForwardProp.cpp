#include "common/log/log.h"
#include "common/util/BitUtils.h"

#include "decompiler/IR2/AtomicOp.h"
#include "decompiler/IR2/bitfields.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/types2/types2.h"
#include "decompiler/util/type_utils.h"

/*!
 * This file contains implementations of forward type propagation.
 */

namespace decompiler {

bool tc(const DecompilerTypeSystem& dts, const TypeSpec& expected, const TP_Type& actual) {
  return dts.ts.tc(expected, actual.typespec());
}

bool is_int_or_uint(const DecompilerTypeSystem& dts, const TP_Type& type) {
  return tc(dts, TypeSpec("integer"), type) || tc(dts, TypeSpec("uint"), type);
}
bool is_signed(const DecompilerTypeSystem& dts, const TP_Type& type) {
  return tc(dts, TypeSpec("int"), type) && !tc(dts, TypeSpec("uint"), type);
}

/*!
 * Set up an instruction which sets its result to an ambiguous deref.
 * The possibilities are specified in FieldReverseMultiLookupOutput.
 */
void types2_from_ambiguous_deref(types2::Instruction& instr,
                                 types2::Type& type,
                                 const std::vector<FieldReverseLookupOutput>& out,
                                 bool tag_lock) {
  ASSERT(!out.empty());

  // HACK - this is disabled for now. This probably works, but the expression pass needs
  // a way to get the decisions.
  type.type = TP_Type::make_from_ts(coerce_to_reg_type(out.front().result_type));
  return;

  // see if we've tagged this instruction in a previous iteration..
  if (instr.field_access_tag) {
    // we did. we should see if the tag tells us which option to pick
    auto& tag = instr.field_access_tag;
    if (tag->selected_possibility >= 0) {
      // it does!
      // there's a chance that we are a different deref than last time, so do our best to
      // find a matching type.
      auto& desired_type = tag->possibilities.at(tag->selected_possibility).type;
      for (auto& sel : out) {
        if (sel.result_type == desired_type) {
          // found one, take it.
          type.type = TP_Type::make_from_ts(coerce_to_reg_type(desired_type));
          return;
        }
      }
      // the previously selected type is gone... not sure what we can do here, but complain and
      // use the first one (highest scored).
      lg::print("type2_from_ambiguous_deref: wanted type {}, but couldn't find it.\n",
                desired_type.print());
      type.type = TP_Type::make_from_ts(coerce_to_reg_type(out.front().result_type));
      return;
    } else {
      // we've got a tag, but no info, just pick the first.
      type.type = TP_Type::make_from_ts(coerce_to_reg_type(out.front().result_type));
      return;
    }
  } else {
    // no tag, let's create one!
    if (!tag_lock) {  // but only if we're in the first pass.
      instr.field_access_tag = std::make_unique<types2::AmbiguousFieldAccess>();
      auto& tag = instr.field_access_tag;
      for (auto& poss : out) {
        auto& slot = tag->possibilities.emplace_back();
        slot.type = poss.result_type;
      }
      type.tag.kind = types2::Tag::FIELD_ACCESS;
      type.tag.field_access = tag.get();
    } else {
      // don't think this should be possible
      lg::warn("Tag lock prevented the creation of a tag in types2_from_ambiguous_deref");
    }
    type.type = TP_Type::make_from_ts(coerce_to_reg_type(out.front().result_type));
    return;
  }
}

// Note that there are "get_type" and "types2" functions that look somewhat similar.
// the difference is that "get_type" just tries to figure out a TP_Type, and the "types2" function
// will set up/resolve tags, and they are intended to be used to update the type state with their
// result.

/*!
 * Try to figure the type of a label.
 * Note: for array labels, we just return a "label_addr" type. The purpose of this is to allow
 * far label loads and array accesses to pass through. This might cause the backprop to work
 * slightly worse on array labels, but I think we can add some extra info if that comes up.
 */
std::optional<TP_Type> try_get_type_of_label(int label_idx, const Env& env) {
  const auto& label_db_lookup = env.file->label_db->lookup(label_idx);
  if (label_db_lookup.known) {
    if (label_db_lookup.result_type.base_type() == "string") {
      // strings have a special case to count format arguments, so calls to format can figure
      // out their argument count.
      return TP_Type::make_from_string(env.file->get_goal_string_by_label(label_idx));
    }

    if (label_db_lookup.is_value) {
      // accessing a static array is handled later, and we just make this a placeholder.
      // this is to help with far labels.
      return TP_Type::make_label_addr(label_idx);
    } else {
      return TP_Type::make_from_ts(label_db_lookup.result_type);
    }
  }

  return {};
}

/*!
 * Try to figure out the type of the value in a symbol with the given name.
 */
std::optional<TP_Type> try_get_type_symbol_val(const std::string& name,
                                               const DecompilerTypeSystem& dts,
                                               const Env& env) {
  if (name == "#f") {
    // if we ever read the false symbol, it should contain the false symbol as its value.
    return TP_Type::make_false();
  } else if (name == "__START-OF-TABLE__") {
    // another annoying special case. We have a fake symbol called __START-OF-TABLE__
    // which actually means that you get the first address in the symbol table.
    // it's not really a linked symbol, but the basic op builder represents it as one.
    return TP_Type::make_from_ts(TypeSpec("pointer"));
  } else if (name == "enter-state") {
    return TP_Type::make_enter_state();
  } else if (name == "run-function-in-process") {
    return TP_Type::make_run_function_in_process_function();
  } else if (name == "set-to-run" && env.func->name() != "enter-state") {
    return TP_Type::make_set_to_run_function();
  } else if (name == "find-parent-method") {
    return TP_Type::make_find_parent_method_function();
  }

  // look up the type of the symbol
  auto type = dts.symbol_types.find(name);
  if (type == dts.symbol_types.end()) {
    // we don't know it, failed.
    return {};
  }

  if (type->second == TypeSpec("type")) {
    // if we get a type by symbol, we should remember which type we got it from.
    return TP_Type::make_type_no_virtual_object(TypeSpec(name));
  }

  if (type->second == TypeSpec("function")) {
    // warn if we're accessing a function, but we don't know a more specific type.
    // 99% of the time, we're about to call the function, and this will give the user
    // a more specific error message that contains the symbol the name.
    lg::warn("Function {} has unknown type", name);
  }

  // otherwise, just return a normal typespec
  return TP_Type::make_from_ts(type->second);
}

/*!
 * Get the type of a symbol, throw if we don't know it.
 */
TP_Type get_type_symbol_val(const std::string& name,
                            const DecompilerTypeSystem& dts,
                            const Env& env) {
  auto result = try_get_type_symbol_val(name, dts, env);
  if (!result) {
    throw std::runtime_error(fmt::format("Unknown symbol: {}", name));
  } else {
    return *result;
  }
}

/*!
 * Get the type of a symbol pointer. Always succeeds
 */
TP_Type get_type_symbol_ptr(const std::string& name) {
  // usually just symbol, but we have a special case for #f
  if (name == "#f") {
    return TP_Type::make_false();
  } else {
    return TP_Type::make_symbol(name);
  }
}

TP_Type get_type_symbol_val_ptr(const std::string& name,
                                const DecompilerTypeSystem& dts,
                                const Env& env) {
  return TP_Type::make_from_ts(
      TypeSpec("pointer", {get_type_symbol_val(name, dts, env).typespec()}));
}

/*!
 * Try to figure out the type of an atom.
 */
std::optional<TP_Type> try_get_type_of_atom(const types2::TypeState& type_state,
                                            const SimpleAtom& atom,
                                            const Env& env,
                                            const DecompilerTypeSystem& dts) {
  switch (atom.get_kind()) {
    case SimpleAtom::Kind::VARIABLE: {
      return type_state[atom.var().reg()]->type;
    }
    case SimpleAtom::Kind::SYMBOL_VAL:
      return try_get_type_symbol_val(atom.get_str(), dts, env);
    case SimpleAtom::Kind::INTEGER_CONSTANT: {
      return TP_Type::make_from_integer(atom.get_int());
    } break;
    case SimpleAtom::Kind::STATIC_ADDRESS:
      return try_get_type_of_label(atom.label(), env);
    default:
      ASSERT_MSG(false,
                 fmt::format("unknown kind in try_get_type_of_atom: {}", atom.to_string(env)));
  }
}

/*!
 * Try to figure out the type of an expression. Can return 0, 1, or multiple.
 * If there are multiple, the first one will be the "best".
 */
std::vector<TP_Type> try_get_type_of_expr(const types2::TypeState& type_state,
                                          const SimpleExpression& expr,
                                          const Env& env,
                                          const DecompilerTypeSystem& dts) {
  switch (expr.kind()) {
    case SimpleExpression::Kind::IDENTITY: {
      auto atom_type = try_get_type_of_atom(type_state, expr.get_arg(0), env, dts);
      if (atom_type) {
        return {*atom_type};
      } else {
        return {};
      }
    }
    case SimpleExpression::Kind::ADD:
      return {};  // temp, todo something better here.

    default:
      ASSERT_MSG(false, fmt::format("unknown kind in try_get_type_of_expr: {} {}",
                                    expr.to_string(env), (int)expr.kind()));
  }
}

namespace types2 {
/*!
 * Given a tagged type, and an expectation for what it should be, backprop constraints.
 */
bool backprop_tagged_type(const TP_Type& expected_type,
                          types2::Type& actual_type,
                          const DecompilerTypeSystem& dts) {
  switch (actual_type.tag.kind) {
    case types2::Tag::NONE:
      return false;
    case types2::Tag::INT_OR_FLOAT: {
      auto type = expected_type.typespec();
      // only update if we're actually changing something.
      if (type.base_type() == "float" &&
          (!actual_type.tag.int_or_float->is_float ||
           actual_type.tag.int_or_float->is_float.value() == false)) {
        actual_type.tag.int_or_float->is_float = true;
        actual_type.type = TP_Type::make_from_ts("float");
        return true;
      }
      return false;
    }

    case types2::Tag::BLOCK_ENTRY:
      // don't update if we're updating to exactly the same thing.
      if (actual_type.tag.block_entry->selected_type) {
        if (actual_type.tag.block_entry->selected_type == expected_type) {
          return false;
        }
      }

      {
        if (!actual_type.tag.block_entry->selected_type) {
          actual_type.tag.block_entry->selected_type = expected_type;
          actual_type.tag.block_entry->updated = true;
          return true;
        } else {
          bool changed = false;
          actual_type.tag.block_entry->selected_type = dts.tp_lca(
              actual_type.tag.block_entry->selected_type.value(), expected_type, &changed);
          actual_type.tag.block_entry->updated = changed;
          return changed;
        }
      }

    case types2::Tag::UNKNOWN_LABEL:
      if (actual_type.tag.unknown_label->selected_type &&
          actual_type.tag.unknown_label->selected_type == expected_type.typespec()) {
        return false;  // no need to update
      } else {
        actual_type.tag.unknown_label->selected_type = expected_type.typespec();
        return true;
      }

    case types2::Tag::UNKNOWN_STACK_STRUCTURE:
      if (actual_type.tag.unknown_stack_structure->selected_type &&
          actual_type.tag.unknown_stack_structure->selected_type == expected_type.typespec()) {
        return false;  // no need to update
      } else {
        actual_type.tag.unknown_stack_structure->selected_type = expected_type.typespec();
        return true;
      }

    case types2::Tag::FIELD_ACCESS: {
      ASSERT(false);  // this code works, but the later stuff can't get use it yet.
      auto* tag = actual_type.tag.field_access;
      bool needs_redo = false;
      auto expected_typespec = expected_type.typespec();
      if (tag->selected_possibility >= 0) {
        if (!dts.ts.tc(expected_typespec, tag->possibilities.at(tag->selected_possibility).type)) {
          // we picked something, but it no longer matches this constraint...
          needs_redo = true;
        }
      }
      if (tag->selected_possibility < 0) {
        needs_redo = true;
      }
      if (needs_redo) {
        int best_idx = 0;  // if nothing else matches, use 0.
        for (size_t ci = 0; ci < tag->possibilities.size(); ci++) {
          if (dts.ts.tc(expected_typespec, tag->possibilities.at(ci).type)) {
            // match!
            best_idx = ci;  // first is highest scored, so don't bother with the rest.
            break;
          }
        }

        if (best_idx != tag->selected_possibility) {
          tag->selected_possibility = best_idx;
          return true;
        } else {
          // failed again, in the same way, no need to update (it'll become a cast...)
          return false;
        }
      } else {
        return false;  // no need to update
      }

    } break;

    default:
      ASSERT_MSG(false, fmt::format("unhandled tag: {}\n", (int)actual_type.tag.kind));
  }
}
}  // namespace types2

/*!
 * Update a type state so the given type_out has the type for the specified label.
 * If the label is unknown, this will set up a tag. But that's not implemented yet...
 */
void types2_for_label(types2::Type& type_out,
                      types2::Instruction& instr,
                      int label_idx,
                      const Env& env,
                      const types2::TypePropExtras& extras) {
  // first, see if the label type is known (either through obvious auto-detect, or a cast)

  auto known_type = try_get_type_of_label(label_idx, env);
  if (known_type) {
    type_out.type = known_type;
    return;
  } else {
    if (instr.unknown_label_tag) {
      auto& tag = instr.unknown_label_tag;
      if (tag->selected_type) {
        // resolved tag, use that
        type_out.type = TP_Type::make_from_ts(*tag->selected_type);
        return;
      } else {
        // unresolved tag, do nothing and hope for the best.
        type_out.type = {};
        return;
      }
    } else {
      // no tag.
      if (extras.tags_locked) {
        // this really shouldn't happen.
        throw std::runtime_error(fmt::format("Encountered unknown label {}, but tags were locked.",
                                             env.file->labels.at(label_idx).name));
      } else {
        auto& name = env.file->labels.at(label_idx).name;
        // lg::print("Encountered unknown label: {}\n", name);
        instr.unknown_label_tag = std::make_unique<types2::UnknownLabel>();
        instr.unknown_label_tag->label_idx = label_idx;
        instr.unknown_label_tag->label_name = name;
        type_out.tag.unknown_label = instr.unknown_label_tag.get();
        type_out.tag.kind = types2::Tag::UNKNOWN_LABEL;
        type_out.type = {};
        return;
      }
    }
  }
}

bool common_int2_case(types2::Type& type_out,
                      const DecompilerTypeSystem& dts,
                      TP_Type& arg0_type,
                      TP_Type& arg1_type) {
  if (arg0_type == arg1_type && is_int_or_uint(dts, arg0_type)) {
    // both are the same type and both are int/uint, so we assume that we're doing integer math.
    // we strip off any weird things like multiplication or integer constant.
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return true;
  }

  if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // usually we would want to use arg0's type as the "winning" type.
    // but we use arg1's if arg0 is an integer constant
    // in either case, strip off weird stuff.
    if (arg0_type.is_integer_constant() && !arg1_type.is_integer_constant()) {
      type_out.type = TP_Type::make_from_ts(arg1_type.typespec());
      return true;
    }
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return true;
  }

  return false;
}

/*!
 * Update a type state so the given type_out has the type for the result of a right shift.
 */
void types2_for_right_shift(types2::Type& type_out,
                            const SimpleExpression& expr,
                            bool is_unsigned,
                            const Env& env,
                            types2::TypeState& input_types,
                            const DecompilerTypeSystem& dts) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  // bitfield access, with a single shift

  if (expr.get_arg(1).is_int()) {
    auto bf = dynamic_cast<BitFieldType*>(dts.ts.lookup_type(arg0_type.typespec()));
    // skip time-frame: we're probably doing math and we know it has no fields in it.
    // note: we could be a bit more robust with this detection...
    if (bf && arg0_type.typespec() != TypeSpec("time-frame")) {
      int shift_size = 64;
      int size = shift_size - expr.get_arg(1).get_int();
      int start_bit = shift_size - size;
      auto field = find_field(dts.ts, bf, start_bit, size, is_unsigned);
      type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(field.type()));
      return;
    }
  }

  if (arg0_type.kind == TP_Type::Kind::LEFT_SHIFTED_BITFIELD && expr.get_arg(1).is_int()) {
    // second op in left/right shift combo
    int end_bit = 64 - arg0_type.get_left_shift();
    if (arg0_type.pcpyud()) {
      end_bit += 64;
    }

    int size = 64 - expr.get_arg(1).get_int();
    int start_bit = end_bit - size;
    if (start_bit < 0) {
      // throw std::runtime_error("Bad bitfield start bit");
      size += start_bit;
      start_bit = 0;
    }

    auto type = dts.ts.lookup_type(arg0_type.get_bitfield_type());
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    auto field = find_field(dts.ts, as_bitfield, start_bit, size, is_unsigned);
    type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(field.type()));
    return;
  }

  if (is_signed(dts, arg0_type)) {
    type_out.type = TP_Type::make_from_ts("int");
  } else {
    type_out.type = TP_Type::make_from_ts("uint");
  }

  // common_int2_case(type_out, dts, arg0_type, arg1_type);
}

void types2_for_left_shift(types2::Type& type_out,
                           const SimpleExpression& expr,
                           const Env& env,
                           types2::TypeState& input_types,
                           const DecompilerTypeSystem& dts) {
  auto arg0_type = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type) {
    // fail!
    type_out.type = {};
    return;
  }

  // multiplication by constant power of two, optimized to a shift.
  if (expr.get_arg(1).is_int() && is_int_or_uint(dts, *arg0_type)) {
    ASSERT(expr.get_arg(1).get_int() >= 0);
    ASSERT(expr.get_arg(1).get_int() < 64);
    // this could be a bitfield access or a multiply.
    // we pick bitfield access if the parent is a bitfield.
    if (dynamic_cast<BitFieldType*>(dts.ts.lookup_type(arg0_type->typespec()))) {
      type_out.type = TP_Type::make_from_left_shift_bitfield(arg0_type->typespec(),
                                                             expr.get_arg(1).get_int(), false);
      return;
    } else if (arg0_type->kind == TP_Type::Kind::PCPYUD_BITFIELD) {
      type_out.type = TP_Type::make_from_left_shift_bitfield(arg0_type->get_bitfield_type(),
                                                             expr.get_arg(1).get_int(), true);
      return;
    } else {
      type_out.type =
          TP_Type::make_from_product(1ull << expr.get_arg(1).get_int(), is_signed(dts, *arg0_type));
      return;
    }
  }

  if (expr.get_arg(1).is_int() && dts.ts.tc(TypeSpec("pointer"), arg0_type->typespec())) {
    // allow shifting a pointer to put it in a bitfield.
    type_out.type = TP_Type::make_from_ts(TypeSpec("uint"));
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg1_type = *arg1_type_info;

  if (arg0_type == arg1_type && is_int_or_uint(dts, *arg0_type)) {
    // both are the same type and both are int/uint, so we assume that we're doing integer math.
    // we strip off any weird things like multiplication or integer constant.
    type_out.type = TP_Type::make_from_ts(arg0_type->typespec());
    return;
  }

  if (is_int_or_uint(dts, *arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // usually we would want to use arg0's type as the "winning" type.
    // but we use arg1's if arg0 is an integer constant
    // in either case, strip off weird stuff.
    if (arg0_type->is_integer_constant() && !arg1_type.is_integer_constant()) {
      type_out.type = TP_Type::make_from_ts(arg1_type.typespec());
      return;
    }
    type_out.type = TP_Type::make_from_ts(arg0_type->typespec());
    return;
  }

  // last fallback
  type_out.type = TP_Type::make_from_ts("int");
}

void types2_for_int_constant(types2::Type& type_out,
                             types2::Instruction& output_instr,
                             SimpleAtom& atom,
                             bool tag_lock) {
  if (output_instr.int_or_float) {
    auto& tag = output_instr.int_or_float;
    if (tag->is_float.has_value()) {
      bool is_float = tag->is_float.value();
      if (is_float) {
        atom.mark_as_float();
        type_out.type = TP_Type::make_from_ts("float");
        return;
      } else {
        type_out.type = TP_Type::make_from_integer(atom.get_int());
        return;
      }
    } else {
      // don't know, just guess int
      type_out.type = TP_Type::make_from_integer(atom.get_int());
      return;
    }
  } else {
    // no tag, add one and guess int.
    if (!tag_lock) {
      output_instr.int_or_float = std::make_unique<types2::AmbiguousIntOrFloatConstant>();
      auto& tag = output_instr.int_or_float;

      type_out.tag.kind = types2::Tag::INT_OR_FLOAT;
      type_out.tag.int_or_float = tag.get();
    } else {
      // this one really shouldn't be possible...
      lg::warn("Tag lock prevented tag creation in types2_for_int_constant");
    }

    type_out.type = TP_Type::make_from_integer(atom.get_int());

    return;
  }
}

void types2_for_atom(types2::Type& type_out,
                     types2::Instruction& output_instr,
                     types2::TypeState& input_types,
                     SimpleAtom& atom,
                     const Env& env,
                     const DecompilerTypeSystem& dts,
                     types2::TypePropExtras& extras) {
  switch (atom.get_kind()) {
    case SimpleAtom::Kind::STATIC_ADDRESS:
      types2_for_label(type_out, output_instr, atom.label(), env, extras);
      return;
    case SimpleAtom::Kind::SYMBOL_VAL: {
      auto type = get_type_symbol_val(atom.get_str(), dts, env);
      type_out.type = type;
    } break;
    case SimpleAtom::Kind::SYMBOL_PTR: {
      auto type = get_type_symbol_ptr(atom.get_str());
      type_out.type = type;
    } break;
    case SimpleAtom::Kind::SYMBOL_VAL_PTR: {
      auto type = get_type_symbol_val_ptr(atom.get_str(), dts, env);
      type_out.type = type;
    } break;
    case SimpleAtom::Kind::INTEGER_CONSTANT: {
      // auto type = TP_Type::make_from_integer(atom.get_int());
      // type_out.type = type;
      types2_for_int_constant(type_out, output_instr, atom, extras.tags_locked);
    } break;
    case SimpleAtom::Kind::EMPTY_LIST: {
      auto type = TP_Type::make_from_ts("pair");
      type_out.type = type;
    } break;
    case SimpleAtom::Kind::VARIABLE: {
      auto& in = input_types[atom.var().reg()];
      type_out.type = in->type;
      // not 100% sure how this should work...
      type_out.tag = in->tag;
    } break;
    default:
      ASSERT_MSG(false, fmt::format("Unhandled types2_for_atom: {}\n", atom.to_string(env)));
  }
}

void types2_for_fpr_to_gpr(types2::Type& type_out,
                           types2::Instruction& output_instr,
                           SimpleExpression& expr,
                           const Env& env,
                           types2::TypeState& input_types,
                           const DecompilerTypeSystem& dts,
                           types2::TypePropExtras& extras) {
  // for now, we'll just copy the type.
  types2_for_atom(type_out, output_instr, input_types, expr.get_arg(0), env, dts, extras);
}

void types2_for_gpr_to_fpr(types2::Type& type_out,
                           types2::Instruction& output_instr,
                           SimpleExpression& expr,
                           const Env& env,
                           types2::TypeState& input_types,
                           const DecompilerTypeSystem& dts,
                           types2::TypePropExtras& extras) {
  // for now, we'll just copy the type.
  types2_for_atom(type_out, output_instr, input_types, expr.get_arg(0), env, dts, extras);
}

void types2_for_integer_mul(types2::Type& type_out,
                            const SimpleExpression& expr,
                            const Env& env,
                            types2::TypeState& input_types,
                            const DecompilerTypeSystem& dts,
                            bool is_unsigned) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;

  if (arg0_type.is_integer_constant() && is_int_or_uint(dts, arg1_type)) {
    type_out.type =
        TP_Type::make_from_product(arg0_type.get_integer_constant(), is_signed(dts, arg0_type));
    return;
  } else if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // signed multiply will always return a signed number.
    type_out.type = TP_Type::make_from_ts(is_unsigned ? "uint" : "int");
    return;
  }

  if (common_int2_case(type_out, dts, arg0_type, arg1_type)) {
    return;
  }

  throw std::runtime_error(
      fmt::format("Couldn't figure out integer multiplication: {} ({} and {})\n",
                  expr.to_string(env), arg0_type.print(), arg1_type.print()));
}

void types2_for_logior(types2::Type& type_out,
                       const SimpleExpression& expr,
                       const Env& env,
                       types2::TypeState& input_types,
                       const DecompilerTypeSystem& dts) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;

  if (arg0_type.kind == TP_Type::Kind::PCPYUD_BITFIELD) {
    // anding a bitfield should return the bitfield type.
    type_out.type = TP_Type::make_from_pcpyud_bitfield(arg0_type.get_bitfield_type());
    return;
  }

  if (arg0_type.typespec() == TypeSpec("float") && arg1_type.typespec() == TypeSpec("float")) {
    env.func->warnings.warning("Using logior on floats");
    // returning int instead of uint because they like to use the float sign bit as an integer sign
    // bit.
    type_out.type = TP_Type::make_from_ts(TypeSpec("float"));
    return;
  }

  if (common_int2_case(type_out, dts, arg0_type, arg1_type)) {
    return;
  }
  throw std::runtime_error(fmt::format("Couldn't figure out logior: {} ({} and {})\n",
                                       expr.to_string(env), arg0_type.print(), arg1_type.print()));
}

void types2_for_logand(types2::Type& type_out,
                       const SimpleExpression& expr,
                       const Env& env,
                       types2::TypeState& input_types,
                       const DecompilerTypeSystem& dts) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;

  if (arg0_type.kind == TP_Type::Kind::PCPYUD_BITFIELD) {
    // anding a bitfield should return the bitfield type.
    type_out.type = TP_Type::make_from_pcpyud_bitfield(arg0_type.get_bitfield_type());
    return;
  }

  if (arg0_type == arg1_type && is_int_or_uint(dts, arg0_type)) {
    // both are the same type and both are int/uint, so we assume that we're doing integer math.
    // we strip off any weird things like multiplication or integer constant.
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  }

  if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // usually we would want to use arg0's type as the "winning" type.
    // but we use arg1's if arg0 is an integer constant
    // in either case, strip off weird stuff.
    if (arg0_type.is_integer_constant() && !arg1_type.is_integer_constant()) {
      type_out.type = TP_Type::make_from_ts(arg1_type.typespec());
      return;
    }
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  }

  if (arg0_type.typespec().base_type() == "pointer" && tc(dts, TypeSpec("integer"), arg1_type)) {
    // pointer logand integer = pointer
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  } else if (arg1_type.typespec().base_type() == "pointer" &&
             tc(dts, TypeSpec("integer"), arg0_type)) {
    // integer logand pointer = pointer
    type_out.type = TP_Type::make_from_ts(arg1_type.typespec());
    return;
  }
  // base case for and. Just get an integer.
  type_out.type = TP_Type::make_from_ts(TypeSpec("int"));
}

void types2_for_normal_int2(types2::Type& type_out,
                            const SimpleExpression& expr,
                            const Env& env,
                            types2::TypeState& input_types,
                            const DecompilerTypeSystem& dts) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;

  if (common_int2_case(type_out, dts, arg0_type, arg1_type)) {
    return;
  }
}

void types2_for_div_mod_signed(types2::Type& type_out,
                               const SimpleExpression& expr,
                               const Env& env,
                               types2::TypeState& input_types,
                               const DecompilerTypeSystem& dts) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;

  if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // signed division will always return a signed number.
    type_out.type = TP_Type::make_from_ts("int");
    return;
  }

  if (common_int2_case(type_out, dts, arg0_type, arg1_type)) {
    return;
  }

  throw std::runtime_error(fmt::format("Couldn't figure out integer mod/divide: {} ({} and {})\n",
                                       expr.to_string(env), arg0_type.print(), arg1_type.print()));
}

void types2_for_div_mod_unsigned(types2::Type& type_out) {
  type_out.type = TP_Type::make_from_ts("uint");
}

void types2_for_pcpyld(types2::Type& type_out,
                       const SimpleExpression& expr,
                       const Env& env,
                       types2::TypeState& input_types,
                       const DecompilerTypeSystem& dts) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;

  if (arg0_type.kind == TP_Type::Kind::PCPYUD_BITFIELD) {
    type_out.type = arg1_type;
    return;
  }

  type_out.type = TP_Type::make_from_ts("uint");
}

void types2_for_sub(types2::Type& type_out,
                    const SimpleExpression& expr,
                    const Env& env,
                    types2::TypeState& input_types,
                    const DecompilerTypeSystem& dts) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;

  if (arg0_type == arg1_type && is_int_or_uint(dts, arg0_type)) {
    // both are the same type and both are int/uint, so we assume that we're doing integer math.
    // we strip off any weird things like multiplication or integer constant.
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  }

  if (is_int_or_uint(dts, arg0_type) && is_int_or_uint(dts, arg1_type)) {
    // usually we would want to use arg0's type as the "winning" type.
    // but we use arg1's if arg0 is an integer constant
    // in either case, strip off weird stuff.
    if (arg0_type.is_integer_constant() && !arg1_type.is_integer_constant()) {
      type_out.type = TP_Type::make_from_ts(arg1_type.typespec());
      return;
    }
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  }

  if (arg0_type.typespec().base_type() == "pointer" && tc(dts, TypeSpec("integer"), arg1_type)) {
    // plain pointer plus integer = plain pointer
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  }

  if (arg1_type.typespec().base_type() == "pointer" && tc(dts, TypeSpec("integer"), arg0_type)) {
    // plain pointer plus integer = plain pointer
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  }

  if (tc(dts, TypeSpec("structure"), arg0_type) && arg1_type.is_integer_constant()) {
    auto type_info = dts.ts.lookup_type(arg0_type.typespec());

    // get next in memory, allow this as &+/&-
    if ((s64)type_info->get_size_in_memory() == std::abs((s64)arg1_type.get_integer_constant())) {
      type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
      return;
    }

    // also allow it, if 16-byte aligned stride.
    if ((u64)align16(type_info->get_size_in_memory()) == arg1_type.get_integer_constant()) {
      type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
      return;
    }
  }

  if (tc(dts, TypeSpec("pointer"), arg0_type) && tc(dts, TypeSpec("pointer"), arg1_type)) {
    type_out.type = TP_Type::make_from_ts(TypeSpec("int"));
    return;
  }

  if (tc(dts, TypeSpec("structure"), arg1_type) && !expr.get_arg(0).is_int() &&
      is_int_or_uint(dts, arg0_type)) {
    // byte access of offset array field trick.
    // arg1 holds a structure.
    // arg0 is an integer in a register.
    type_out.type = TP_Type::make_object_plus_product(arg1_type.typespec(), 1, true);
    return;
  }

  if (common_int2_case(type_out, dts, arg0_type, arg1_type)) {
    return;
  }
  throw std::runtime_error(fmt::format("Couldn't figure out integer subtract: {} ({} and {})\n",
                                       expr.to_string(env), arg0_type.print(), arg1_type.print()));
}

void types2_addr_on_stack(types2::Type& type_out,
                          types2::Instruction& instr,
                          int offset,
                          const Env& env,
                          const DecompilerTypeSystem& dts,
                          const types2::TypeState& types,
                          bool tag_lock) {
  (void)dts;

  // first look for a stack structure
  for (auto& structure : env.stack_structure_hints()) {
    if (offset < structure.hint.stack_offset ||
        offset >= (structure.hint.stack_offset + structure.size)) {
      continue;  // reject, it isn't in this variable
    }

    if (offset == structure.hint.stack_offset) {
      // special case just getting the variable
      type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(structure.ref_type));
      return;
    }
  }

  // look for a stack variable
  auto sss = types.try_find_stack_spill_slot(offset);
  if (sss && sss->type) {
    type_out.type = TP_Type::make_from_ts(TypeSpec("pointer", {sss->type->typespec()}));
    return;
  }

  // Neither matched... see if there's a tag.
  if (instr.unknown_stack_structure_tag) {
    auto& tag = instr.unknown_stack_structure_tag;
    if (tag->selected_type) {
      // use the resolved tag!
      type_out.type = TP_Type::make_from_ts(*tag->selected_type);
      return;
    } else {
      // unresolved tag, do nothing and hope for the best.
      type_out.type = {};
      return;
    }
  } else {
    // no tag. can we create one?
    if (tag_lock) {
      // nope.
      throw std::runtime_error(
          fmt::format("Failed to find a stack variable or structure at offset {}", offset));
    } else {
      // lg::print("Encountered unknown stack address {} : {}\n", env.func->name(), offset);
      instr.unknown_stack_structure_tag = std::make_unique<types2::UnknownStackStructure>();
      instr.unknown_stack_structure_tag->stack_offset = offset;
      type_out.tag.unknown_stack_structure = instr.unknown_stack_structure_tag.get();
      type_out.tag.kind = types2::Tag::UNKNOWN_STACK_STRUCTURE;
      type_out.type = {};
      return;
    }
  }
}

void types2_for_add(types2::Type& type_out,
                    types2::Instruction& output_instr,
                    const SimpleExpression& expr,
                    const Env& env,
                    types2::TypeState& input_types,
                    const DecompilerTypeSystem& dts,
                    types2::TypePropExtras& extras) {
  auto arg0_type_info = try_get_type_of_atom(input_types, expr.get_arg(0), env, dts);
  if (!arg0_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto arg1_type_info = try_get_type_of_atom(input_types, expr.get_arg(1), env, dts);
  if (!arg1_type_info) {
    // fail!
    type_out.type = {};
    return;
  }

  auto& arg0_type = *arg0_type_info;
  auto& arg1_type = *arg1_type_info;
  auto& arg0 = expr.get_arg(0);
  auto& arg1 = expr.get_arg(1);

  // the approach here is just to try a bunch of things.
  // this is a bit of a mess, but I don't know another way. Add is a very useful instruction.

  // access the stack - either a stack variable or structure.
  if (arg0.is_var() && arg0.var().reg() == Register(Reg::GPR, Reg::SP) && arg1.is_int()) {
    // get_stack_type_at_constant_offset(arg1.get_int(), env, dts, input);
    types2_addr_on_stack(type_out, output_instr, arg1.get_int(), env, dts, input_types,
                         extras.tags_locked);
    return;
  }

  // if things go wrong, and we add a pointer to another address, just return int
  // honestly not sure why I have this one... let's have it abort for now.
  if (arg0_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT &&
      arg1_type.typespec().base_type() == "pointer") {
    // ASSERT(false);
    type_out.type = TP_Type::make_from_ts(TypeSpec("int"));
    return;
  }

  // special case: dynamic access to the method table, to look up a method by ID.
  if (arg0_type.is_product_with(4) && tc(dts, TypeSpec("type"), arg1_type) &&
      env.func->name() != "overrides-parent-method?"  // hack!
  ) {
    // dynamic access into the method array with shift, add, offset-load
    // no need to track the type because we don't know the method index anyway.
    type_out.type = TP_Type::make_partial_dyanmic_vtable_access();
    return;
  }

  // propagate integer math: a * C1 + C2
  if (arg1_type.is_integer_constant() && arg0_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT) {
    type_out.type = TP_Type::make_from_integer_constant_plus_product(
        arg1_type.get_integer_constant(), arg0_type.typespec(), arg0_type.get_multiplier());
    return;
  }

  // propagate integer math: a + C1
  if (arg1_type.is_integer_constant() && is_int_or_uint(dts, arg0_type)) {
    TypeSpec sum_type = arg0_type.typespec();

    FieldReverseLookupInput rd_in;
    rd_in.offset = arg1_type.get_integer_constant();
    rd_in.stride = 0;
    rd_in.base_type = arg0_type.typespec();
    auto out = env.dts->ts.reverse_field_lookup(rd_in);
    if (out.success) {
      sum_type = coerce_to_reg_type(out.result_type);
    }

    type_out.type = TP_Type::make_from_integer_constant_plus_var(arg1_type.get_integer_constant(),
                                                                 arg0_type.typespec(), sum_type);
    return;
  }

  // get addr of field using obj + C1 + a * C2
  if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT) {
    FieldReverseLookupInput rd_in;
    rd_in.offset = arg0_type.get_add_int_constant();
    rd_in.stride = arg0_type.get_mult_int_constant();
    rd_in.base_type = arg1_type.typespec();
    auto out = env.dts->ts.reverse_field_multi_lookup(rd_in);
    if (out.success) {
      if (out.results.size() == 1) {
        type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(out.results.front().result_type));
        return;
      } else {
        types2_from_ambiguous_deref(output_instr, type_out, out.results, extras.tags_locked);
        return;
      }
    }
    // flipped version of the above
  } else if (arg1_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT) {
    FieldReverseLookupInput rd_in;
    rd_in.offset = arg1_type.get_add_int_constant();
    rd_in.stride = arg1_type.get_mult_int_constant();
    rd_in.base_type = arg0_type.typespec();
    auto out = env.dts->ts.reverse_field_multi_lookup(rd_in);
    if (out.success) {
      if (out.results.size() == 1) {
        type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(out.results.front().result_type));
        return;
      } else {
        types2_from_ambiguous_deref(output_instr, type_out, out.results, extras.tags_locked);
        return;
      }
    }
  }

  // get addr of field using obj + C1 + a (effectively a stride of 1)
  if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR) {
    FieldReverseLookupInput rd_in;
    rd_in.offset = arg0_type.get_integer_constant();
    rd_in.stride = 1;
    rd_in.base_type = arg1_type.typespec();
    auto out = env.dts->ts.reverse_field_multi_lookup(rd_in);
    if (out.success) {
      if (out.results.size() == 1) {
        type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(out.results.front().result_type));
        return;
      } else {
        types2_from_ambiguous_deref(output_instr, type_out, out.results, extras.tags_locked);
        return;
      }
    }
  }

  if (common_int2_case(type_out, dts, arg0_type, arg1_type)) {
    return;
  }

  // get addr of field with a constant: obj + C1
  if (arg1.is_int() && arg0_type.kind == TP_Type::Kind::TYPESPEC) {
    // access a field.
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;
    rd_in.stride = 0;
    rd_in.offset = arg1.get_int();
    rd_in.base_type = arg0_type.typespec();
    auto out = env.dts->ts.reverse_field_multi_lookup(rd_in);
    if (out.success) {
      if (out.results.size() == 1) {
        type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(out.results.front().result_type));
        return;
      } else {
        types2_from_ambiguous_deref(output_instr, type_out, out.results, extras.tags_locked);
        return;
      }
    }
  }

  // access with just product and no offset.
  if (arg0_type.kind == TP_Type::Kind::TYPESPEC &&
      arg0_type.typespec().base_type() == "inline-array" &&
      arg1_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT) {
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;
    rd_in.stride = arg1_type.get_multiplier();
    rd_in.offset = 0;
    rd_in.base_type = arg0_type.typespec();
    auto rd = dts.ts.reverse_field_multi_lookup(rd_in);

    std::vector<FieldReverseLookupOutput> filtered_results;
    for (auto& result : rd.results) {
      if (result.has_variable_token()) {
        filtered_results.push_back(result);
      }
    }

    if (filtered_results.size() == 1) {
      type_out.type =
          TP_Type::make_from_ts(coerce_to_reg_type(filtered_results.front().result_type));
      return;
    } else if (!filtered_results.empty()) {
      types2_from_ambiguous_deref(output_instr, type_out, filtered_results, extras.tags_locked);
      return;
    }
  }

  // flipped of above
  if (arg1_type.kind == TP_Type::Kind::TYPESPEC &&
      arg1_type.typespec().base_type() == "inline-array" &&
      arg0_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT) {
    FieldReverseLookupInput rd_in;
    rd_in.deref = std::nullopt;
    rd_in.stride = arg0_type.get_multiplier();
    rd_in.offset = 0;
    rd_in.base_type = arg1_type.typespec();
    auto rd = dts.ts.reverse_field_multi_lookup(rd_in);
    std::vector<FieldReverseLookupOutput> filtered_results;
    for (auto& result : rd.results) {
      if (result.has_variable_token()) {
        filtered_results.push_back(result);
      }
    }

    if (filtered_results.size() == 1) {
      type_out.type =
          TP_Type::make_from_ts(coerce_to_reg_type(filtered_results.front().result_type));
      return;
    } else if (!filtered_results.empty()) {
      types2_from_ambiguous_deref(output_instr, type_out, filtered_results, extras.tags_locked);
      return;
    }
  }

  if (arg0_type.is_product() && arg1_type.kind == TP_Type::Kind::TYPESPEC) {
    type_out.type =
        TP_Type::make_object_plus_product(arg1_type.typespec(), arg0_type.get_multiplier(), true);
    return;
  }

  if (arg1_type.is_product() && arg0_type.kind == TP_Type::Kind::TYPESPEC) {
    type_out.type =
        TP_Type::make_object_plus_product(arg0_type.typespec(), arg1_type.get_multiplier(), false);
    return;
  }

  if (arg0_type.typespec().base_type() == "pointer" && tc(dts, TypeSpec("integer"), arg1_type)) {
    if (!arg1.is_int()) {
      type_out.type = TP_Type::make_object_plus_product(arg0_type.typespec(), 1, false);
      return;
    }
    // plain pointer plus integer = plain pointer
    type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
    return;
  }

  if (arg1_type.typespec().base_type() == "pointer" && tc(dts, TypeSpec("integer"), arg0_type)) {
    // plain pointer plus integer = plain pointer
    type_out.type = TP_Type::make_from_ts(arg1_type.typespec());
    return;
  }

  if (tc(dts, TypeSpec("structure"), arg0_type) && arg1_type.is_integer_constant()) {
    auto type_info = dts.ts.lookup_type(arg0_type.typespec());

    // get next in memory, allow this as &+/&-
    if ((s64)type_info->get_size_in_memory() == std::abs((s64)arg1_type.get_integer_constant())) {
      type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
      return;
    }

    // also allow it, if 16-byte aligned stride.
    if ((u64)align16(type_info->get_size_in_memory()) == arg1_type.get_integer_constant()) {
      type_out.type = TP_Type::make_from_ts(arg0_type.typespec());
      return;
    }
  }

  if (tc(dts, TypeSpec("pointer"), arg0_type) && tc(dts, TypeSpec("pointer"), arg1_type)) {
    type_out.type = TP_Type::make_from_ts(TypeSpec("int"));
    return;
  }

  auto& name = env.func->guessed_name;
  if (name.kind == FunctionName::FunctionKind::METHOD && name.method_id == 7 &&
      env.func->type.arg_count() == 3) {
    if (arg1_type.typespec() == TypeSpec("int")) {
      type_out.type = arg0_type;
      return;
    }
  }

  if (tc(dts, TypeSpec("structure"), arg1_type) && !expr.get_arg(0).is_int() &&
      is_int_or_uint(dts, arg0_type)) {
    if (allowable_base_type_for_symbol_to_string(arg1_type.typespec()) &&
        arg0_type.is_integer_constant(SYMBOL_TO_STRING_MEM_OFFSET_DECOMP[env.version])) {
      // symbol -> GOAL String
      // NOTE - the offset doesn't fit in a s16, so it's loaded into a register first.
      // so we expect the arg to be a variable, and the type propagation will figure out the
      // integer constant.
      type_out.type = TP_Type::make_from_ts(dts.ts.make_pointer_typespec("string"));
      return;
    } else {
      // byte access of offset array field trick.
      // arg1 holds a structure.
      // arg0 is an integer in a register.

      // TODO port to old type pass too
      if (arg0_type.is_integer_constant()) {
        TypeSpec sum_type = arg1_type.typespec();
        FieldReverseLookupInput rd_in;
        rd_in.offset = arg0_type.get_integer_constant();
        rd_in.stride = 0;
        rd_in.base_type = arg1_type.typespec();
        auto out = env.dts->ts.reverse_field_lookup(rd_in);
        if (out.success) {
          sum_type = coerce_to_reg_type(out.result_type);
        }
        type_out.type = TP_Type::make_from_integer_constant_plus_var(
            arg0_type.get_integer_constant(), arg1_type.typespec(), sum_type);
        return;
      } else {
        type_out.type = TP_Type::make_object_plus_product(arg1_type.typespec(), 1, true);
        return;
      }
    }
  }

  if (env.version == GameVersion::Jak2 && tc(dts, TypeSpec("symbol"), arg1_type) &&
      is_int_or_uint(dts, arg0_type)) {
    if (arg0_type.is_integer_constant(jak2::SYM_TO_STRING_OFFSET)) {
      // symbol -> GOAL String
      // NOTE - the offset doesn't fit in a s16, so it's loaded into a register first.
      // so we expect the arg to be a variable, and the type propagation will figure out the
      // integer constant.
      type_out.type = TP_Type::make_from_ts(dts.ts.make_pointer_typespec("string"));
      return;
    }
  }

  lg::print("checks: {} {} {}\n", tc(dts, TypeSpec("structure"), arg1_type),
            !expr.get_arg(0).is_int(), is_int_or_uint(dts, arg0_type));

  throw std::runtime_error(
      fmt::format("add failed: {} {}\n", arg0_type.print(), arg1_type.print()));
  // ASSERT_MSG(false, fmt::format("add failed: {} {}\n", arg0_type.print(), arg1_type.print()));
}

void types2_for_normal_all_float(types2::Type& type_out,
                                 const SimpleExpression& expr,
                                 types2::TypeState& input_types,
                                 const DecompilerTypeSystem& dts,
                                 types2::TypePropExtras& extras) {
  // backprop to make inputs floats
  for (int i = 0; i < expr.args(); i++) {
    auto& arg = expr.get_arg(i);
    auto& arg_type = input_types[arg.var().reg()];
    if (arg_type->tag.has_tag()) {
      if (types2::backprop_tagged_type(TP_Type::make_from_ts("float"), *arg_type, dts)) {
        extras.needs_rerun = true;
      }
    }
  }

  type_out.type = TP_Type::make_from_ts(TypeSpec("float"));
}

void types2_for_vectors_in_float_out(types2::Type& type_out,
                                     const SimpleExpression& expr,
                                     types2::TypeState& input_types,
                                     const DecompilerTypeSystem& dts,
                                     types2::TypePropExtras& extras) {
  // backprop to make inputs vector
  for (int i = 0; i < expr.args(); i++) {
    auto& arg = expr.get_arg(i);
    auto& arg_type = input_types[arg.var().reg()];
    if (arg_type->tag.has_tag()) {
      if (types2::backprop_tagged_type(TP_Type::make_from_ts("vector"), *arg_type, dts)) {
        extras.needs_rerun = true;
      }
    }
  }

  type_out.type = TP_Type::make_from_ts(TypeSpec("float"));
}

void types2_for_vector_in_and_out(types2::Type& type_out,
                                  const SimpleExpression& expr,
                                  types2::TypeState& input_types,
                                  const DecompilerTypeSystem& dts,
                                  types2::TypePropExtras& extras) {
  // backprop to make inputs vector
  for (int i = 0; i < expr.args(); i++) {
    auto& arg = expr.get_arg(i);
    auto& arg_type = input_types[arg.var().reg()];
    if (arg_type->tag.has_tag()) {
      if (types2::backprop_tagged_type(TP_Type::make_from_ts("vector"), *arg_type, dts)) {
        extras.needs_rerun = true;
      }
    }
  }

  type_out.type = TP_Type::make_from_ts(TypeSpec("vector"));
}

void types2_for_vector_float_product(types2::Type& type_out,
                                     const SimpleExpression& expr,
                                     types2::TypeState& input_types,
                                     const DecompilerTypeSystem& dts,
                                     types2::TypePropExtras& extras) {
  // backprop to make inputs vector
  for (int i = 0; i < expr.args(); i++) {
    auto& arg = expr.get_arg(i);
    auto& arg_type = input_types[arg.var().reg()];
    if (arg_type->tag.has_tag()) {
      if (types2::backprop_tagged_type(TP_Type::make_from_ts(i == 2 ? "float" : "vector"),
                                       *arg_type, dts)) {
        extras.needs_rerun = true;
      }
    }
  }

  type_out.type = TP_Type::make_from_ts(TypeSpec("vector"));
}

void types2_for_vector_plus_float_times(types2::Type& type_out,
                                        const SimpleExpression& expr,
                                        types2::TypeState& input_types,
                                        const DecompilerTypeSystem& dts,
                                        types2::TypePropExtras& extras) {
  // backprop to make inputs vector
  for (int i = 0; i < expr.args(); i++) {
    auto& arg = expr.get_arg(i);
    auto& arg_type = input_types[arg.var().reg()];
    if (arg_type->tag.has_tag()) {
      if (types2::backprop_tagged_type(TP_Type::make_from_ts(i == 3 ? "float" : "vector"),
                                       *arg_type, dts)) {
        extras.needs_rerun = true;
      }
    }
  }

  type_out.type = TP_Type::make_from_ts(TypeSpec("vector"));
}

void types2_for_float_to_int(types2::Type& type_out,
                             const SimpleExpression& expr,
                             types2::TypeState& input_types,
                             const DecompilerTypeSystem& dts,
                             types2::TypePropExtras& extras) {
  auto& arg = expr.get_arg(0);
  auto& arg_type = input_types[arg.var().reg()];
  if (arg_type->tag.has_tag()) {
    if (types2::backprop_tagged_type(TP_Type::make_from_ts("float"), *arg_type, dts)) {
      extras.needs_rerun = true;
    }
  }
  type_out.type = TP_Type::make_from_ts(TypeSpec("int"));
}

void types2_for_int_to_float(types2::Type& type_out,
                             const SimpleExpression& expr,
                             types2::TypeState& input_types,
                             const DecompilerTypeSystem& dts,
                             types2::TypePropExtras& extras) {
  // backprop here might be bad...
  auto& arg = expr.get_arg(0);
  auto& arg_type = input_types[arg.var().reg()];
  if (arg_type->tag.has_tag()) {
    if (types2::backprop_tagged_type(TP_Type::make_from_ts("int"), *arg_type, dts)) {
      extras.needs_rerun = true;
    }
  }
  type_out.type = TP_Type::make_from_ts(TypeSpec("float"));
}

void types2_for_normal_int1(types2::Type& type_out,
                            const SimpleExpression& expr,
                            types2::TypeState& input_types) {
  type_out.type = {};
  auto& input_type = input_types[expr.get_arg(0).var().reg()];
  if (input_type->type) {
    type_out.type = input_type->type;
  }
}

void types2_for_expr(types2::Type& type_out,
                     types2::Instruction& output_instr,
                     types2::TypeState& input_types,
                     SimpleExpression& expr,
                     const Env& env,
                     const DecompilerTypeSystem& dts,
                     types2::TypePropExtras& extras) {
  switch (expr.kind()) {
    case SimpleExpression::Kind::IDENTITY:
      types2_for_atom(type_out, output_instr, input_types, expr.get_arg(0), env, dts, extras);
      break;
    case SimpleExpression::Kind::RIGHT_SHIFT_ARITH:
      types2_for_right_shift(type_out, expr, false, env, input_types, dts);
      break;
    case SimpleExpression::Kind::RIGHT_SHIFT_LOGIC:
      types2_for_right_shift(type_out, expr, true, env, input_types, dts);
      break;
    case SimpleExpression::Kind::LEFT_SHIFT:
      types2_for_left_shift(type_out, expr, env, input_types, dts);
      break;
    case SimpleExpression::Kind::FPR_TO_GPR:
      types2_for_fpr_to_gpr(type_out, output_instr, expr, env, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::ADD:
      types2_for_add(type_out, output_instr, expr, env, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::GPR_TO_FPR:
      types2_for_gpr_to_fpr(type_out, output_instr, expr, env, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::DIV_S:
    case SimpleExpression::Kind::MIN_S:
    case SimpleExpression::Kind::SUB_S:
    case SimpleExpression::Kind::MAX_S:
    case SimpleExpression::Kind::MUL_S:
    case SimpleExpression::Kind::ADD_S:
    case SimpleExpression::Kind::ABS_S:
    case SimpleExpression::Kind::NEG_S:
    case SimpleExpression::Kind::SQRT_S:
      types2_for_normal_all_float(type_out, expr, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::SUB:
      types2_for_sub(type_out, expr, env, input_types, dts);
      break;
    case SimpleExpression::Kind::MUL_SIGNED:
      types2_for_integer_mul(type_out, expr, env, input_types, dts, false);
      break;
    case SimpleExpression::Kind::MUL_UNSIGNED:
      types2_for_integer_mul(type_out, expr, env, input_types, dts, true);
      break;
    case SimpleExpression::Kind::DIV_SIGNED:
    case SimpleExpression::Kind::MOD_SIGNED:
      types2_for_div_mod_signed(type_out, expr, env, input_types, dts);
      break;
    case SimpleExpression::Kind::DIV_UNSIGNED:
    case SimpleExpression::Kind::MOD_UNSIGNED:
      types2_for_div_mod_unsigned(type_out);
      break;
    case SimpleExpression::Kind::NEG:
    case SimpleExpression::Kind::MIN_SIGNED:
    case SimpleExpression::Kind::MAX_SIGNED:
    case SimpleExpression::Kind::SUBU_L32_S7:
      type_out.type = TP_Type::make_from_ts("int");  // ?
      break;
    case SimpleExpression::Kind::OR:
      types2_for_logior(type_out, expr, env, input_types, dts);
      break;
    case SimpleExpression::Kind::AND:
      types2_for_logand(type_out, expr, env, input_types, dts);
      break;
    case SimpleExpression::Kind::NOR:
    case SimpleExpression::Kind::XOR:
      types2_for_normal_int2(type_out, expr, env, input_types, dts);
      break;
    case SimpleExpression::Kind::LOGNOT:
      types2_for_normal_int1(type_out, expr, input_types);
      break;
    case SimpleExpression::Kind::FLOAT_TO_INT:
      types2_for_float_to_int(type_out, expr, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::INT_TO_FLOAT:
      types2_for_int_to_float(type_out, expr, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::VECTOR_3_DOT:
    case SimpleExpression::Kind::VECTOR_4_DOT:
    case SimpleExpression::Kind::VECTOR_LENGTH:
      types2_for_vectors_in_float_out(type_out, expr, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::VECTOR_CROSS:
    case SimpleExpression::Kind::VECTOR_MINUS:
    case SimpleExpression::Kind::VECTOR_PLUS:
      types2_for_vector_in_and_out(type_out, expr, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::VECTOR_FLOAT_PRODUCT:
      types2_for_vector_float_product(type_out, expr, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::VECTOR_PLUS_FLOAT_TIMES:
      types2_for_vector_plus_float_times(type_out, expr, input_types, dts, extras);
      break;
    case SimpleExpression::Kind::PCPYLD:
      types2_for_pcpyld(type_out, expr, env, input_types, dts);
      break;
    default:
      throw std::runtime_error(
          fmt::format("Unhandled types2_for_expr: {} {}\n", expr.to_string(env), (int)expr.kind()));
  }
}

void SetVarOp::propagate_types2(types2::Instruction& instr,
                                const Env& env,
                                types2::TypeState& input_types,
                                DecompilerTypeSystem& dts,
                                types2::TypePropExtras& extras) {
  // propagate types on the expression. Will also handle backprop of constraints, etc.
  auto* type_out = instr.types[m_dst.reg()];
  types2_for_expr(*type_out, instr, input_types, m_src, env, dts, extras);

  // remember this. It's used later, to insert better looking casts.
  if (type_out->type) {
    m_source_type = type_out->type->typespec();
  }

  // update clobbers.
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }
}

void AsmOp::propagate_types2(types2::Instruction& instr,
                             const Env& /*env*/,
                             types2::TypeState& input_types,
                             DecompilerTypeSystem& dts,
                             types2::TypePropExtras& /*extras*/) {
  // update clobbers.
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }

  auto& out = instr.types;

  if (m_instr.kind == InstructionKind::QMFC2) {
    ASSERT(m_dst);
    out[m_dst->reg()]->type = TP_Type::make_from_ts("float");
    // ASSERT(false);  // hack... not sure float is right here... lets do more testing first.
    return;
  }

  // can be used for asm or bitfield. earlier passes mark it asm, we catch it in types,
  // and set the tp_types.
  if (m_instr.kind == InstructionKind::PCPYUD) {
    if (m_src[1] && m_src[1]->reg() == Register(Reg::GPR, Reg::R0)) {
      ASSERT(m_src[0]);
      auto& in_type = input_types[m_src[0]->reg()];
      if (in_type->type) {
        auto bf = dynamic_cast<BitFieldType*>(dts.ts.lookup_type(in_type->type->typespec()));
        if (bf) {
          ASSERT(m_dst);
          // just mark as pcpyud, it's part of a longer chain...
          out[m_dst->reg()]->type = TP_Type::make_from_pcpyud_bitfield(in_type->type->typespec());
          return;
        }
      }
    }
  }

  // pextuw t0, r0, gp
  if (m_instr.kind == InstructionKind::PEXTUW) {
    if (m_src[0] && m_src[0]->reg() == Register(Reg::GPR, Reg::R0)) {
      ASSERT(m_src[1]);
      auto& in_type = input_types[m_src[1]->reg()]->type;
      if (in_type) {
        auto type = dts.ts.lookup_type(in_type->typespec());
        auto as_bitfield = dynamic_cast<BitFieldType*>(type);
        if (as_bitfield) {
          auto field = find_field(dts.ts, as_bitfield, 64, 32, true);
          ASSERT(m_dst);
          out[m_dst->reg()]->type = TP_Type::make_from_ts(field.type());
          return;
        }
      }
    }
  }

  // sllv out, in, r0
  // this is recognized as asm (because it usually is) but it can be used in bitfields to extract
  // the low 32-bits
  if (m_instr.kind == InstructionKind::SLLV &&
      instruction().src[1].is_reg(Register(Reg::GPR, Reg::R0))) {
    // check input:
    auto& src_type = input_types[m_src[0]->reg()];
    if (src_type->type) {
      auto type = dts.ts.lookup_type(src_type->type->typespec());
      // see if it's a bitfield type
      auto as_bitfield = dynamic_cast<BitFieldType*>(type);
      if (as_bitfield) {
        // see if we can find a field (throws if we can't)
        auto field = find_field(dts.ts, as_bitfield, 0, 32, {});
        // this is a complete extraction, can set the type as the result.
        out[m_dst->reg()]->type = TP_Type::make_from_ts(field.type());
        return;
      }
    }
  }

  // srl out, bitfield, int
  if (m_instr.kind == InstructionKind::SRL) {
    auto& src_type = input_types[m_src[0]->reg()];
    if (src_type) {
      auto type = dts.ts.lookup_type(src_type->type->typespec());
      auto as_bitfield = dynamic_cast<BitFieldType*>(type);
      if (as_bitfield) {
        int sa = m_instr.src[1].get_imm();
        int offset = sa;
        int size = 32 - offset;
        auto field = find_field(dts.ts, as_bitfield, offset, size, {});
        out[m_dst->reg()]->type = TP_Type::make_from_ts(coerce_to_reg_type(field.type()));
        return;
      }
    }
  }

  // general case fallback: if we write a register we should at least set something...
  if (m_dst.has_value()) {
    auto kind = m_dst->reg().get_kind();
    if (kind == Reg::FPR) {
      // if we set a fpr, just assume float
      out[m_dst->reg()]->type = TP_Type::make_from_ts("float");
    } else if (kind == Reg::GPR) {
      // if we're a gpr, and any
      for (auto& x : m_src) {
        if (x && x->reg().get_kind() == Reg::GPR) {
          // if any sources are known to be 128-bit integers, use uint128 as the output
          auto src_type = input_types[x->reg()];
          if (!src_type->type) {
            continue;
          }
          auto src_ts = src_type->type->typespec();
          if (dts.ts.tc(TypeSpec("int128"), src_ts) || dts.ts.tc(TypeSpec("uint128"), src_ts)) {
            out[m_dst->reg()]->type = TP_Type::make_from_ts("uint128");
            return;
          }
        }
        // otherwise just use int...
        out[m_dst->reg()]->type = TP_Type::make_from_ts("int");
      }
    }
  }
}

void SetVarConditionOp::propagate_types2(types2::Instruction& instr,
                                         const Env& /*env*/,
                                         types2::TypeState& /*input_types*/,
                                         DecompilerTypeSystem& /*dts*/,
                                         types2::TypePropExtras& /*extras*/) {
  // update clobbers.
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }
  instr.types[m_dst.reg()]->type = TP_Type::make_from_ts("symbol");
}

/*!
 * Run type propagation on a store to memory (does not include stack spill stores)
 * Normally, we don't have to do anything here, but there are 3 special cases:
 * - if we're storing something in the current process next-state as part of a go, the type pass
 *   needs to track this, to determine the number of arguments for the updating go.
 * - we can backprop type information on the value being stored
 * - we can backprop type information on the location we're storing to.
 */
void StoreOp::propagate_types2(types2::Instruction& instr,
                               const Env& env,
                               types2::TypeState& input_types,
                               DecompilerTypeSystem& dts,
                               types2::TypePropExtras& extras) {
  // backprop on the value being stored.
  {
    if (m_value.is_var()) {  // only applicable if we're storing a var
      auto reg = m_value.var().reg();
      if (reg.get_kind() != Reg::VF) {
        const auto& value_type = input_types[m_value.var().reg()];
        if (value_type->tag.has_tag()) {  // don't bother if we don't have a tag to resolve
          if (m_kind == Kind::FLOAT) {
            if (backprop_tagged_type(TP_Type::make_from_ts("float"), *value_type, dts)) {
              extras.needs_rerun = true;
            }
          } else {
            if (m_addr.is_identity() && !m_addr.get_arg(0).is_var()) {
              auto location_type = try_get_type_of_expr(input_types, m_addr, env, dts);
              if (!location_type.empty()) {  // need to know where we're storing

                // temp warning if we have multiple store types
                if (location_type.size() > 1) {
                  lg::print("StoreOp::propagate_types2: multiple possible store types: ");
                  for (auto& t : location_type) {
                    lg::print("{} ", t.print());
                  }
                  lg::print("\n");
                }

                if (backprop_tagged_type(location_type.at(0), *value_type, dts)) {
                  extras.needs_rerun = true;
                }
              }
            }
          }
        }
      }
    }
  }

  // backprop on the store type
  {
    // TODO implement this.
    // it's a little tricky if there's an offset, let's implement this as we find examples.
  }

  // handle the next-state thing
  // look for setting the next state of the current process
  IR2_RegOffset ro;
  if (get_as_reg_offset(m_addr, &ro)) {
    if (ro.reg == Register(Reg::GPR, Reg::S6) &&
        ro.offset == OFFSET_OF_NEXT_STATE_STORE[env.version]) {
      ASSERT(m_value.is_var());
      auto& store_type = input_types[m_value.var().reg()]->type;
      if (store_type) {
        instr.types.next_state_type->type = store_type;
      } else {
        instr.types.next_state_type->type = TP_Type::make_from_ts("state");  // idk
      }
    }
  }

  // update clobbers.
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }
}

RegClass get_reg_kind(const Register& r) {
  switch (r.get_kind()) {
    case Reg::GPR:
      return RegClass::GPR_64;
    case Reg::FPR:
      return RegClass::FLOAT;
    default:
      ASSERT(false);
      return RegClass::INVALID;
  }
}

bool load_var_op_determine_type(types2::Type& type_out,
                                types2::Instruction& output_instr,
                                types2::TypeState& input_types,
                                const LoadVarOp& op,
                                const Env& env,
                                const DecompilerTypeSystem& dts,
                                types2::TypePropExtras& extras) {
  if (op.src().is_identity()) {
    auto& src = op.src().get_arg(0);
    if (src.is_static_addr()) {
      if (op.kind() == LoadVarOp::Kind::FLOAT) {
        // assume anything loaded directly to floating point register is a float, and skip the
        // rest.
        type_out.type = TP_Type::make_from_ts("float");
        return true;
      }

      auto label_name = env.file->labels.at(src.label()).name;
      const auto& hint = env.file->label_db->lookup(label_name);
      if (!hint.known) {
        ASSERT(false);  // todo
        throw std::runtime_error(
            fmt::format("Label {} was unknown in AtomicOpTypeAnalysis (type).", hint.name));
      }

      if (!hint.is_value) {
        // this one seems fatal
        throw std::runtime_error(
            fmt::format("Label {} was used as a value, but wasn't marked as one", hint.name));
      }

      type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(hint.result_type));
      return true;
    }
  }

  IR2_RegOffset ro;
  if (get_as_reg_offset(op.src(), &ro)) {
    auto& input_type_info = input_types[ro.reg];
    if (!input_type_info->type) {
      // todo: could try some basic stuff to resolve float loads here...
      // ASSERT(false);
      return false;
    }
    auto& input_type = input_type_info->type.value();

    // check loading a method of a known type.
    // the TP_Type system will track individual types, both as a "most specific known at decompile
    // time" (TYPE_OF_TYPE_OR_CHILD) or "exact" version (TYPE_OF_TYPE_NO_VIRTUAL)
    if ((input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD ||
         input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL)  // is known type
        && ro.offset >= 16       // is in the method table (16 is the offset of the first method)
        && (ro.offset & 3) == 0  // is aligned
        && op.size() == 4        // loading a pointer
        && op.kind() == LoadVarOp::Kind::UNSIGNED  // GOAL convention to load pointers as unsigned
    ) {
      // method get of fixed type
      // here, we look up the actual type signature of the method, which depends on the type.
      auto type_name = input_type.get_type_objects_typespec().base_type();
      auto method_id = (ro.offset - 16) / 4;
      auto method_info = dts.ts.lookup_method(type_name, method_id);
      auto method_type = method_info.type.substitute_for_method_call(type_name);

      // special case: the new method of "object" is the general heap allocation function,
      // and is used in (new 'heap 'foo ... ) expressions.
      // we'll use a special type for these.
      if (type_name == "object" && method_id == GOAL_NEW_METHOD) {
        type_out.type = TP_Type::make_object_new(method_type);
        return true;
      }

      // another special case: calling the new method is never done virtually. so just handle it
      // without paying attention to virtual/non-virtual
      if (method_id == GOAL_NEW_METHOD) {
        // special flag so later code knows
        type_out.type = TP_Type::make_non_object_new(method_type, TypeSpec(type_name));
        return true;
      } else if (input_type.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL) {
        // normal non-virtual method access
        type_out.type =
            TP_Type::make_non_virtual_method(method_type, TypeSpec(type_name), method_id);
        return true;
      } else {
        // normal virtual method access.
        // first check special cases
        if (type_name == "art" || type_name == "art-group") {
          if (method_id == 10) {
            type_out.type =
                TP_Type::make_get_art_by_name(method_type, TypeSpec(type_name), method_id);
            return true;
          }
        }
        // nope
        type_out.type = TP_Type::make_virtual_method(method_type, TypeSpec(type_name), method_id);
        return true;
      }
    }

    // finally, we have a backup to allow some method access if we have no idea of the exact type.
    // we can only do this safely for a few "built in" methods that always take the same arguments
    // on all types.
    if (input_type.kind == TP_Type::Kind::TYPESPEC    // if we've fallen back to plain type
        && input_type.typespec() == TypeSpec("type")  // and we think it's a type
                                                      // and the usual load check
        && ro.offset >= 16 && (ro.offset & 3) == 0 && op.size() == 4 &&
        op.kind() == LoadVarOp::Kind::UNSIGNED) {
      // method get of an unknown type. We assume the most general "object" type because that
      // will have the correct arguments for built-in methods
      auto method_id = (ro.offset - 16) / 4;
      // only allow up to MEMUSAGE, the last built-in method.
      if (method_id <= (int)GOAL_MEMUSAGE_METHOD) {
        auto method_info = dts.ts.lookup_method("object", method_id);
        // also block new: you can override the arguments, and relocate: it has two uses: login and
        // actual relocated, and it takes different arguments for these cases.
        if (method_id != GOAL_NEW_METHOD && method_id != GOAL_RELOC_METHOD) {
          // this can get us the wrong thing for `new` methods.  And maybe relocate?
          type_out.type = TP_Type::make_non_virtual_method(
              method_info.type.substitute_for_method_call("object"), TypeSpec("object"), method_id);
          return true;
        }
      }
    }

    if (input_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = op.kind() == LoadVarOp::Kind::SIGNED;
      dk.size = op.size();
      rd_in.deref = dk;
      rd_in.base_type = input_type.get_obj_plus_const_mult_typespec();
      rd_in.stride = input_type.get_multiplier();
      rd_in.offset = ro.offset;
      auto rd = dts.ts.reverse_field_multi_lookup(rd_in);
      if (rd.success) {
        if (rd.results.size() == 1) {
          type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(rd.results.front().result_type));
          return true;
        } else {
          types2_from_ambiguous_deref(output_instr, type_out, rd.results, extras.tags_locked);
          return true;
        }
      }
    }

    if (input_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR &&
        input_type.get_integer_constant() == 0) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = op.kind() == LoadVarOp::Kind::SIGNED;
      dk.size = op.size();
      rd_in.deref = dk;
      rd_in.base_type = input_type.get_objects_typespec();
      rd_in.offset = ro.offset;
      auto rd = dts.ts.reverse_field_multi_lookup(rd_in);
      if (rd.success) {
        if (rd.results.size() == 1) {
          type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(rd.results.front().result_type));
          return true;
        } else {
          types2_from_ambiguous_deref(output_instr, type_out, rd.results, extras.tags_locked);
          return true;
        }
      }
    }

    if (input_type.kind == TP_Type::Kind::TYPESPEC && ro.offset == -4 &&
        op.kind() == LoadVarOp::Kind::UNSIGNED && op.size() == 4 && ro.reg.get_kind() == Reg::GPR) {
      // get type of basic likely, but misrecognized as an object.

      type_out.type = TP_Type::make_type_allow_virtual_object(input_type.typespec().base_type());
      return true;
    }

    if (input_type.kind == TP_Type::Kind::DYNAMIC_METHOD_ACCESS && ro.offset == 16) {
      // access method vtable. The input is type + (4 * method), and the 16 is the offset
      // of method 0.
      type_out.type = TP_Type::make_from_ts(TypeSpec("function"));
      return true;
    }

    // Assume we're accessing a field of an object.
    // if we are a pair with sloppy typing, don't use this and instead use the case down below.
    if (input_type.typespec() != TypeSpec("pair") || !env.allow_sloppy_pair_typing()) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = op.kind() == LoadVarOp::Kind::SIGNED;
      dk.size = op.size();
      rd_in.deref = dk;
      rd_in.base_type = input_type.typespec();
      rd_in.stride = 0;
      rd_in.offset = ro.offset;
      auto rd = dts.ts.reverse_field_multi_lookup(rd_in);

      if (rd.success) {
        if (rd.results.size() == 1) {
          if (rd_in.base_type.base_type() == "state" && rd.results.front().tokens.size() == 1 &&
              rd.results.front().tokens.front().kind ==
                  FieldReverseLookupOutput::Token::Kind::FIELD &&
              rd.results.front().tokens.front().name == "enter" &&
              rd_in.base_type.arg_count() > 0) {
            // special case for accessing the enter field of state
            type_out.type =
                TP_Type::make_from_ts(state_to_go_function(rd_in.base_type, TypeSpec("none")));
            return true;
          } else {
            type_out.type =
                TP_Type::make_from_ts(coerce_to_reg_type(rd.results.front().result_type));
            return true;
          }
        } else {
          /*
          lg::print("ambiguous deref. Choices are:\n");
          for (auto& result : rd.results) {
            lg::print(" {} : ", result.result_type.print());
            for (auto& tok : result.tokens) {
              lg::print("{} ", tok.print());
            }
            lg::print("\n");
          }
           */

          types2_from_ambiguous_deref(output_instr, type_out, rd.results, extras.tags_locked);
          return true;
        }
      }
    }

    if (input_type.typespec() == TypeSpec("pointer") ||
        // this seems like a bit of a hack, but we did it in the old type pass...
        input_type.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
      // we got a plain pointer. let's just assume we're loading an integer.
      // perhaps we should disable this feature by default on 4-byte loads if we're getting
      // lots of false positives for loading pointers from plain pointers.

      switch (op.kind()) {
        case LoadVarOp::Kind::UNSIGNED:
          switch (op.size()) {
            case 1:
            case 2:
            case 4:
            case 8:
              type_out.type = TP_Type::make_from_ts(TypeSpec("uint"));
              return true;
            case 16:
              type_out.type = TP_Type::make_from_ts(TypeSpec("uint128"));
              return true;
            default:
              break;
          }
          break;
        case LoadVarOp::Kind::SIGNED:
          switch (op.size()) {
            case 1:
            case 2:
            case 4:
            case 8:
              type_out.type = TP_Type::make_from_ts(TypeSpec("int"));
              return true;
            case 16:
              type_out.type = TP_Type::make_from_ts(TypeSpec("int128"));
              return true;
            default:
              break;
          }
          break;
        case LoadVarOp::Kind::FLOAT:
          type_out.type = TP_Type::make_from_ts(TypeSpec("float"));
          return true;
        default:
          ASSERT(false);
      }
    }

    // rd failed, try as pair.
    if (env.allow_sloppy_pair_typing()) {
      // we are strict here - only permit pair-type loads from object or pair.
      // object is permitted for stuff like association lists where the car is also a pair.
      if (op.kind() == LoadVarOp::Kind::SIGNED && op.size() == 4 &&
          (input_type.typespec() == TypeSpec("object") ||
           input_type.typespec() == TypeSpec("pair"))) {
        // these rules are of course not always correct or the most specific, but it's the best
        // we can do.
        if (ro.offset == 2) {
          // cdr = another pair.
          type_out.type = TP_Type::make_from_ts(TypeSpec("pair"));
          return true;
        } else if (ro.offset == -2) {
          // car = some object.
          type_out.type = TP_Type::make_from_ts(TypeSpec("object"));
          return true;
        }
      }
    }

    if (input_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR && ro.offset == 0) {
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = op.kind() == LoadVarOp::Kind::SIGNED;
      dk.size = op.size();
      rd_in.deref = dk;
      rd_in.base_type = input_type.get_objects_typespec();
      rd_in.stride = 0;
      rd_in.offset = input_type.get_integer_constant();
      auto rd = dts.ts.reverse_field_multi_lookup(rd_in);
      if (rd.success) {
        if (rd.results.size() == 1) {
          type_out.type = TP_Type::make_from_ts(coerce_to_reg_type(rd.results.front().result_type));
          return true;
        } else {
          types2_from_ambiguous_deref(output_instr, type_out, rd.results, extras.tags_locked);
          return true;
        }
      }
    }

    throw std::runtime_error(fmt::format("Could not figure out load: {}", op.to_string(env)));
  } else {
    throw std::runtime_error(fmt::format("Could not figure out load (2): {}", op.to_string(env)));
  }
}

void LoadVarOp::propagate_types2(types2::Instruction& instr,
                                 const Env& env,
                                 types2::TypeState& input_types,
                                 DecompilerTypeSystem& dts,
                                 types2::TypePropExtras& extras) {
  // update clobbers.
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }

  if (m_dst.reg().get_kind() == Reg::VF) {
    // ignore vf registers in type pass.
    return;
  }

  auto& type_out = instr.types[m_dst.reg()];
  load_var_op_determine_type(*type_out, instr, input_types, *this, env, dts, extras);
  m_type = type_out->type ? type_out->type->typespec() : std::optional<TypeSpec>();
}

void branch_delay_types2(IR2_BranchDelay& delay,
                         types2::Instruction& instr,
                         types2::TypeState& input_types) {
  switch (delay.kind()) {
    case IR2_BranchDelay::Kind::NOP:
    case IR2_BranchDelay::Kind::NO_DELAY:
      break;
    case IR2_BranchDelay::Kind::SET_REG_FALSE:
      instr.types[delay.var(0).reg()]->type = TP_Type::make_false();
      break;
    case IR2_BranchDelay::Kind::SET_REG_TRUE:
      instr.types[delay.var(0).reg()]->type = TP_Type::make_from_ts("symbol");
      break;
    case IR2_BranchDelay::Kind::SET_REG_REG:
      instr.types[delay.var(0).reg()]->type = input_types[delay.var(1).reg()]->type;
      break;
    default:
      ASSERT_MSG(false, fmt::format("propagate_types2 BranchOp unknown branch delay: {}",
                                    (int)delay.kind()));
  }
}

void BranchOp::propagate_types2(types2::Instruction& instr,
                                const Env& /*env*/,
                                types2::TypeState& input_types,
                                DecompilerTypeSystem& dts,
                                types2::TypePropExtras& extras) {
  // update clobbers.
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }

  switch (m_condition.kind()) {
    case IR2_Condition::Kind::FLOAT_EQUAL:
    case IR2_Condition::Kind::FLOAT_GEQ:
    case IR2_Condition::Kind::FLOAT_GREATER_THAN:
    case IR2_Condition::Kind::FLOAT_LEQ:
    case IR2_Condition::Kind::FLOAT_LESS_THAN:
    case IR2_Condition::Kind::FLOAT_NOT_EQUAL:
      for (int i = 0; i < 2; i++) {
        auto& arg = m_condition.src(i);
        auto& arg_type = input_types[arg.var().reg()];
        if (arg_type->tag.has_tag()) {
          if (types2::backprop_tagged_type(TP_Type::make_from_ts("float"), *arg_type, dts)) {
            extras.needs_rerun = true;
          }
        }
      }
      break;
    default:
      break;
  }

  branch_delay_types2(m_branch_delay, instr, input_types);
}

void AsmBranchOp::propagate_types2(types2::Instruction& instr,
                                   const Env& env,
                                   types2::TypeState& input_types,
                                   DecompilerTypeSystem& dts,
                                   types2::TypePropExtras& extras) {
  if (m_branch_delay) {
    m_branch_delay->propagate_types2(instr, env, input_types, dts, extras);
  }
  // for now, just make everything uint
  for (auto x : m_write_regs) {
    if (x.allowed_local_gpr()) {
      instr.types[x]->type = TP_Type::make_from_ts("uint");
    }
  }
}

void SpecialOp::propagate_types2(types2::Instruction& instr,
                                 const Env& /*env*/,
                                 types2::TypeState& /*input_types*/,
                                 DecompilerTypeSystem& /*dts*/,
                                 types2::TypePropExtras& /*extras*/) {
  // update clobbers.
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }
  switch (m_kind) {
    case Kind::NOP:
    case Kind::BREAK:
    case Kind::CRASH:
    case Kind::SUSPEND:
      return;
    default:
      ASSERT(false);
  }
}

void CallOp::propagate_types2(types2::Instruction& instr,
                              const Env& env,
                              types2::TypeState& input_types,
                              DecompilerTypeSystem& dts,
                              types2::TypePropExtras& /*extras*/) {
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }

  const Reg::Gpr arg_regs[8] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3,
                                Reg::T0, Reg::T1, Reg::T2, Reg::T3};
  auto& out_types = instr.types;

  // see what's in the function register
  auto in_tp_info = input_types[Register(Reg::GPR, Reg::T9)];
  if (!in_tp_info->type) {
    // no idea what's there, can't do anything...
    out_types[Register(Reg::GPR, Reg::V0)]->type = {};
    return;
  }
  auto& in_tp = in_tp_info->type.value();

  // special case: call object new method inside of a new method.
  if (in_tp.kind == TP_Type::Kind::OBJECT_NEW_METHOD &&
      !dts.type_prop_settings.current_method_type.empty()) {
    // calling object new method. Set the result to a new object of our type
    out_types[Register(Reg::GPR, Reg::V0)]->type =
        TP_Type::make_from_ts(dts.type_prop_settings.current_method_type);
    // update the call type
    m_call_type = in_tp.get_method_new_object_typespec();
    m_call_type.get_arg(m_call_type.arg_count() - 1) =
        TypeSpec(dts.type_prop_settings.current_method_type);
    m_call_type_set = true;

    // update function call info info
    m_read_regs.clear();
    m_arg_vars.clear();
    m_read_regs.emplace_back(Reg::GPR, Reg::T9);
    for (int i = 0; i < int(m_call_type.arg_count()) - 1; i++) {
      m_read_regs.emplace_back(Reg::GPR, arg_regs[i]);
      m_arg_vars.push_back(RegisterAccess(AccessMode::READ, m_read_regs.back(), m_my_idx));
    }
    return;
  }

  auto in_type = in_tp.typespec();

  if (in_type.base_type() != "function") {
    throw std::runtime_error("Called something that was not a function: " + in_type.print());
  }

  // backprop
  bool can_backprop = true;

  // special case: go
  // If we call enter-state, update our type.
  if (in_tp.kind == TP_Type::Kind::ENTER_STATE_FUNCTION) {
    can_backprop = false;  // for now... can special case this later.
    // this is a GO!
    const auto& state_type = instr.types.next_state_type;
    if (!state_type->type) {
      throw std::runtime_error(
          fmt::format("At op {}, called enter-state, but we have no idea what the type of the "
                      "next-states is. This can probably be fixed by providing some more casts, or "
                      "improving the decompiler to backtrack go's.",
                      m_my_idx));
    }
    auto state_typespec = state_type->type->typespec();
    if (state_typespec.base_type() != "state") {
      throw std::runtime_error(
          fmt::format("At op {}, called enter-state, but the current next-state has type {}, which "
                      "is not a valid state.",
                      m_my_idx, state_typespec.print()));
    }

    if (state_typespec.arg_count() == 0) {
      throw std::runtime_error(fmt::format(
          "At op {}, tried to enter-state, but the type of (-> s6 next-state) is just a plain "
          "state.  The decompiler must know the specific state type.",
          m_my_idx));
    }
    in_type = state_to_go_function(state_typespec, TypeSpec("object"));
  }

  if (in_tp.kind == TP_Type::Kind::GET_ART_BY_NAME_METHOD) {
    can_backprop = false;
    // let's see what a2 holds...
    auto& arg2_type = input_types[Register(Reg::GPR, arg_regs[2])];
    if (arg2_type->type) {
      auto& tpt = *arg2_type->type;
      if (tpt.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL) {
        ASSERT(in_type.last_arg() == TypeSpec("basic"));  // just to double check right function
        in_type.last_arg() = tpt.get_type_objects_typespec();
      }
    }
  }

  // special case: process initialization
  if (in_tp.kind == TP_Type::Kind::RUN_FUNCTION_IN_PROCESS_FUNCTION ||
      in_tp.kind == TP_Type::Kind::SET_TO_RUN_FUNCTION) {
    can_backprop = false;  // for now... can special case this later.
    auto func_to_run_type = input_types[Register(Reg::GPR, arg_regs[1])];
    auto func_to_run_ts =
        func_to_run_type->type ? func_to_run_type->type->typespec() : TypeSpec("object");
    if (func_to_run_ts.base_type() != "function" || func_to_run_ts.arg_count() == 0 ||
        func_to_run_ts.arg_count() > 7) {
      throw std::runtime_error(
          fmt::format("Call to run-function-in-process or set-to-run at op {} with an invalid "
                      "function type: {}",
                      m_my_idx, func_to_run_ts.print()));
    }

    std::vector<TypeSpec> new_arg_types;
    if (in_tp.kind == TP_Type::Kind::RUN_FUNCTION_IN_PROCESS_FUNCTION) {
      new_arg_types.push_back(TypeSpec("process"));
    } else {
      new_arg_types.push_back(TypeSpec("thread"));
    }
    new_arg_types.push_back(TypeSpec("function"));

    for (size_t i = 0; i < func_to_run_ts.arg_count() - 1; i++) {
      new_arg_types.push_back(func_to_run_ts.get_arg(i));
    }
    new_arg_types.push_back(TypeSpec("none"));
    in_type = TypeSpec("function", new_arg_types);
  }

  // special case: variable argument count
  if (in_type.arg_count() == 2 && in_type.get_arg(0) == TypeSpec("_varargs_")) {
    can_backprop = false;  // for now... can special case this later.
    // we're calling a varags function, which is format. We can determine the argument count
    // by looking at the format string, if we can get it.
    TP_Type arg_type = TP_Type::make_uninitialized();
    if (input_types[Register(Reg::GPR, Reg::A1)]->type) {
      arg_type = *input_types[Register(Reg::GPR, Reg::A1)]->type;
    }

    auto can_determine_argc = arg_type.can_be_format_string();
    auto dynamic_string = false;
    if (!can_determine_argc && arg_type.typespec() == TypeSpec("string")) {
      // dynamic string. use manual lookup table.
      dynamic_string = true;
    }
    if (can_determine_argc || dynamic_string) {
      int arg_count = -1;

      if (dynamic_string) {
        arg_count = dts.get_dynamic_format_arg_count(env.func->name(), m_my_idx);
      } else if (arg_type.is_constant_string()) {
        auto& str = arg_type.get_string();
        arg_count = dts.get_format_arg_count(str);
      } else {
        // is format string.
        arg_count = arg_type.get_format_string_arg_count();
      }

      if (arg_count + 2 > 8) {
        throw std::runtime_error(
            "Call to `format` pushed the arg-count beyond the acceptable arg limit (8), do you "
            "need to add "
            "a code to the ignore lists?");
      }

      TypeSpec format_call_type("function");
      format_call_type.add_arg(TypeSpec("object"));  // destination
      format_call_type.add_arg(TypeSpec("string"));  // format string
      for (int i = 0; i < arg_count; i++) {
        format_call_type.add_arg(TypeSpec("object"));
      }
      format_call_type.add_arg(TypeSpec("object"));
      arg_count += 2;  // for destination and format string.

      m_call_type = format_call_type;
      m_call_type_set = true;

      out_types[Register(Reg::GPR, Reg::V0)]->type = TP_Type::make_from_ts(in_type.last_arg());

      // we can also update register usage here.
      m_read_regs.clear();
      m_arg_vars.clear();
      m_read_regs.emplace_back(Reg::GPR, Reg::T9);
      for (int i = 0; i < arg_count; i++) {
        m_read_regs.emplace_back(Reg::GPR, arg_regs[i]);
        m_arg_vars.push_back(RegisterAccess(AccessMode::READ, m_read_regs.back(), m_my_idx));
      }

      return;
    } else {
      throw std::runtime_error("Failed to get appropriate string for _varags_ call, got " +
                               arg_type.print());
    }
  }
  bool use_normal_last_arg = true;

  if (in_tp.kind == TP_Type::Kind::FIND_PARENT_METHOD_FUNCTION) {
    bool can_use_call_parent = true;
    TypeSpec call_parent_result_type;
    const auto& guessed_name = env.func->guessed_name;

    if (guessed_name.kind == FunctionName::FunctionKind::METHOD ||
        guessed_name.kind == FunctionName::FunctionKind::V_STATE) {
      // should call something like:
      // (find-parent-method sharkey 39)
      const auto& type_arg = input_types[Register(Reg::GPR, arg_regs[0])]->type;
      if (can_use_call_parent && type_arg->kind != TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL) {
        lg::warn(
            "Can't use call-parent-method because the first argument to find-parent-method is a "
            "{}, which should be a type",
            type_arg->print());
        can_use_call_parent = false;
      }

      if (can_use_call_parent && type_arg->get_type_objects_typespec() != guessed_name.type_name) {
        lg::warn(
            "Can't use call-parent-method because the first argument type is wrong: got {}, but "
            "expected {}",
            type_arg->get_type_objects_typespec().print(), guessed_name.type_name);
        can_use_call_parent = false;
      }
      const auto& id_arg = input_types[Register(Reg::GPR, arg_regs[1])]->type;
      int expected_id = -1;
      if (guessed_name.kind == FunctionName::FunctionKind::V_STATE) {
        auto state_info = dts.ts.lookup_method(guessed_name.type_name, guessed_name.state_name);
        expected_id = state_info.id;
        call_parent_result_type =
            state_info.type.substitute_for_method_call(guessed_name.type_name);
      } else {
        expected_id = guessed_name.method_id;
        call_parent_result_type = env.func->type;  // same type as this method!
      }
      if (can_use_call_parent && !id_arg->is_integer_constant(expected_id)) {
        lg::warn(
            "Can't use call-parent-method because the second argument is wrong: got {}, but "
            "expected a constant integer {}",
            id_arg->print(), expected_id);
        can_use_call_parent = false;
      }

    } else {
      can_use_call_parent = false;
      lg::warn(
          "Can't use call-parent-method because find-parent-method was called in {}, which isn't a "
          "method or state handler.",
          env.func->name());
    }

    if (can_use_call_parent) {
      out_types[Register(Reg::GPR, Reg::V0)]->type = TP_Type::make_from_ts(call_parent_result_type);
      printf("used special %s\n", call_parent_result_type.print().c_str());
      use_normal_last_arg = false;
    }
  }

  if (in_type.arg_count() < 1) {
    throw std::runtime_error("Called a function, but we do not know its type");
  }

  if (use_normal_last_arg) {
    out_types[Register(Reg::GPR, Reg::V0)]->type = TP_Type::make_from_ts(in_type.last_arg());
  }
  // set the call type!
  m_call_type = in_type;
  m_call_type_set = true;

  if (in_tp.kind == TP_Type::Kind::NON_OBJECT_NEW_METHOD &&
      in_tp.method_from_type() == TypeSpec("array") &&
      input_types[Register(Reg::GPR, arg_regs[2])]) {
    // array new:
    auto& a2 = input_types[Register(Reg::GPR, arg_regs[2])];
    auto& a1 = input_types[Register(Reg::GPR, arg_regs[1])];
    auto& a0 = input_types[Register(Reg::GPR, arg_regs[0])];

    if (a0->type && a0->type->is_symbol() && a2->type &&
        a2->type->kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL && a1->type) {
      out_types[Register(Reg::GPR, Reg::V0)]->type = TP_Type::make_from_ts(TypeSpec(
          "array",
          {input_types[Register(Reg::GPR, arg_regs[2])]->type->get_type_objects_typespec()}));
    }
  }

  // we can also update register usage here.
  m_read_regs.clear();
  m_arg_vars.clear();
  m_read_regs.emplace_back(Reg::GPR, Reg::T9);

  for (uint32_t i = 0; i < in_type.arg_count() - 1; i++) {
    m_read_regs.emplace_back(Reg::GPR, arg_regs[i]);
    m_arg_vars.push_back(RegisterAccess(AccessMode::READ, m_read_regs.back(), m_my_idx));
  }

  // _always_ write the v0 register, even if the function returns none.
  // GOAL seems to insert coloring moves even on functions returning none.
  m_write_regs.clear();
  m_write_regs.emplace_back(Reg::GPR, Reg::V0);

  if (can_backprop) {
    bool is_new_method = in_tp.kind == TP_Type::Kind::NON_OBJECT_NEW_METHOD;
    for (int i = 0; i < int(m_call_type.arg_count()) - 1; i++) {
      auto& expected_type = m_call_type.get_arg(i);
      auto& actual_type = input_types[Register(Reg::GPR, arg_regs[i])];
      if (actual_type->tag.has_tag()) {
        if (is_new_method && i == 0) {
          // special case - new method first argument can be a stack structure
          types2::backprop_tagged_type(TP_Type::make_from_ts(in_tp.method_from_type()),
                                       *actual_type, dts);
        } else {
          // normal backprop
          types2::backprop_tagged_type(TP_Type::make_from_ts(expected_type), *actual_type, dts);
        }
      }
    }
  }
}

void FunctionEndOp::propagate_types2(types2::Instruction& instr,
                                     const Env& /*env*/,
                                     types2::TypeState& /*input_types*/,
                                     DecompilerTypeSystem& /*dts*/,
                                     types2::TypePropExtras& /*extras*/) {
  for (auto& clobber : m_clobber_regs) {
    instr.types[clobber]->type = TP_Type::make_uninitialized();
  }
}

void StackSpillStoreOp::propagate_types2(types2::Instruction& instr,
                                         const Env& env,
                                         types2::TypeState& input_types,
                                         DecompilerTypeSystem& dts,
                                         types2::TypePropExtras& extras) {
  auto& info = env.stack_spills().lookup(m_offset);
  if (info.size != m_size) {
    env.func->warnings.error("Stack slot store mismatch: defined as size {}, got size {}\n",
                             info.size, m_size);
  }

  // auto stored_type = m_value.get_type(input, env, dts);
  auto* type_out = instr.types.try_find_stack_spill_slot(m_offset);
  ASSERT(type_out);
  types2_for_atom(*type_out, instr, input_types, m_value, env, dts, extras);
}

void StackSpillLoadOp::propagate_types2(types2::Instruction& instr,
                                        const Env& env,
                                        types2::TypeState& input_types,
                                        DecompilerTypeSystem& /*dts*/,
                                        types2::TypePropExtras& /*extras*/) {
  // stack slot load
  auto& info = env.stack_spills().lookup(m_offset);
  if (info.size != m_size) {
    env.func->warnings.error("Stack slot load at {} mismatch: defined as size {}, got size {}",
                             m_offset, info.size, m_size);
  }

  if (info.is_signed != m_is_signed) {
    env.func->warnings.warning("Stack slot offset {} signed mismatch", m_offset);
  }

  auto* type_in = input_types.try_find_stack_spill_slot(m_offset);
  ASSERT(type_in);
  instr.types[m_dst.reg()]->type = type_in->type;
}

void ConditionalMoveFalseOp::propagate_types2(types2::Instruction& instr,
                                              const Env& /*env*/,
                                              types2::TypeState& /*input_types*/,
                                              DecompilerTypeSystem& /*dts*/,
                                              types2::TypePropExtras& /*extras*/) {
  auto& typ = instr.types[m_dst.reg()]->type;
  if (typ) {
    if (typ->typespec() != TypeSpec("symbol")) {
      lg::warn("Conditional Moved #f into something of type {}", typ->typespec().print());
      // result.get(m_dst.reg()) = TP_Type::make_from_ts("symbol");
    }
  }
  typ = TP_Type::make_from_ts("symbol");
}

}  // namespace decompiler
