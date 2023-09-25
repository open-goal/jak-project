#include "types2.h"

#include <set>

#include "common/log/log.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/types2/types2.h"
#include "decompiler/util/type_utils.h"

namespace decompiler::types2 {

/*!
 * Construct a typestate from the types at the start of a block.
 */
TypeState make_typestate_from_block_types(BlockStartTypes& block_start_types) {
  // create references to the gpr, fpr, and stack slot types of the start of the block.
  TypeState result;
  for (int i = 0; i < 32; i++) {
    result.gpr_types[i] = &block_start_types.gpr_types[i];
    result.fpr_types[i] = &block_start_types.fpr_types[i];
  }
  for (auto& s : block_start_types.stack_slot_types) {
    result.stack_slot_types.push_back(&s);
  }
  result.next_state_type = &block_start_types.next_state_type;
  return result;
}

/*!
 * Find all of the used stack spill slots in a function.
 * They are represented as byte offsets.
 */
std::set<int> find_stack_spill_slots(const Function& f) {
  std::set<int> result;
  for (auto& op : f.ir2.atomic_ops->ops) {
    auto as_stack_spill_store = dynamic_cast<StackSpillStoreOp*>(op.get());
    if (as_stack_spill_store) {
      result.insert(as_stack_spill_store->offset());
    }

    auto as_stack_spill_load = dynamic_cast<StackSpillLoadOp*>(op.get());
    if (as_stack_spill_load) {
      result.insert(as_stack_spill_load->offset());
    }
  }
  return result;
}

/*!
 * Set up types for the entry of a function.
 */
void construct_function_entry_types(BlockStartTypes& result,
                                    const TypeSpec& f_ts,
                                    const std::set<int>& stack_slots) {
  for (auto& x : result.gpr_types) {
    x.type = TP_Type::make_uninitialized();
  }
  for (auto& x : result.fpr_types) {
    x.type = TP_Type::make_uninitialized();
  }
  result.next_state_type.type = TP_Type::make_uninitialized();

  for (auto x : stack_slots) {
    auto slot = result.try_find_stack_spill_slot(x);
    ASSERT(slot);
    slot->slot = x;
    slot->type.type = TP_Type::make_uninitialized();
  }

  int goal_args[] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3, Reg::T0, Reg::T1, Reg::T2, Reg::T3};
  ASSERT(f_ts.base_type() == "function");
  ASSERT(f_ts.arg_count() >= 1);
  ASSERT(f_ts.arg_count() <= 8 + 1);  // 8 args + 1 return.
  for (int i = 0; i < int(f_ts.arg_count()) - 1; i++) {
    auto reg_id = goal_args[i];
    const auto& reg_type = f_ts.get_arg(i);
    result[Register(Reg::GPR, reg_id)].type = TP_Type::make_from_ts(reg_type);
  }

  result[Register(Reg::GPR, Reg::S6)].type =
      TP_Type::make_from_ts(TypeSpec(f_ts.try_get_tag("behavior").value_or("process")));

  // initialize stack slots as uninitialized (I think safe to skip)
}

/*!
 * Set up function cache data structure.
 */
void build_function(FunctionCache& function_cache,
                    Function& func,
                    const std::set<int>& stack_slots) {
  ASSERT(func.ir2.atomic_ops && func.ir2.atomic_ops_succeeded);
  auto& aops = func.ir2.atomic_ops->ops;

  // set up the instruction and block structures
  function_cache.blocks.resize(func.basic_blocks.size());
  function_cache.instructions.resize(aops.size());
  for (size_t i = 0; i < aops.size(); i++) {
    function_cache.instructions[i].aop_idx = i;
  }
  for (size_t block_idx = 0; block_idx < function_cache.blocks.size(); block_idx++) {
    for (int instr_idx = func.ir2.atomic_ops->block_id_to_first_atomic_op.at(block_idx);
         instr_idx < func.ir2.atomic_ops->block_id_to_end_atomic_op.at(block_idx); instr_idx++) {
      function_cache.blocks[block_idx].instructions.push_back(
          &function_cache.instructions.at(instr_idx));
    }
  }

  // figure out the order we'll visit all blocks
  // todo: do something with unreachables?
  function_cache.block_visit_order = func.bb_topo_sort().vist_order;

  // to save time, we store types at the entry of each block, then in the instructions inside
  // each block, store types sparsely. This saves very slow copying around of types.
  for (int block_idx : function_cache.block_visit_order) {
    auto& block = function_cache.blocks.at(block_idx);

    // add placeholders for all stack slots.
    for (auto slot_addr : stack_slots) {
      auto& new_slot = block.start_types.stack_slot_types.emplace_back();
      new_slot.slot = slot_addr;
    }

    // this will contain pointers to the most recent types for each register.
    // it gets initialized to the types on block entry (we store a copy of these)
    TypeState state = make_typestate_from_block_types(block.start_types);
    block.start_type_state = state;  // stash this here, just makes it easier for later.
    ASSERT(block.start_type_state.fpr_types[0]);

    // loop through instructions, allocating new types for written registers.
    for (auto instr : block.instructions) {
      auto& aop = aops.at(instr->aop_idx);

      // allocate types that we'll write/clobber
      for (auto& reg : aop->write_regs()) {
        RegType rt;
        rt.reg = reg;
        instr->written_reg_types.push_back(rt);
      }
      for (auto& reg : aop->clobber_regs()) {
        RegType rt;
        rt.reg = reg;
        instr->written_reg_types.push_back(rt);
      }

      // now link the register types
      for (auto& written_reg_type : instr->written_reg_types) {
        auto reg_kind = written_reg_type.reg.get_kind();
        // ignore weird registers
        if (reg_kind == Reg::GPR || reg_kind == Reg::FPR) {
          state[written_reg_type.reg] = &written_reg_type.type;
        }
      }

      // do the same for stack spill types
      auto as_stack_spill_store = dynamic_cast<StackSpillStoreOp*>(aop.get());
      if (as_stack_spill_store) {
        // make a written_stack_slot_type
        StackSlotType ss;
        ss.slot = as_stack_spill_store->offset();
        instr->written_stack_slot_type = ss;

        // and update state!
        bool found = false;
        for (auto& slot : state.stack_slot_types) {
          if (slot->slot == ss.slot) {
            ASSERT(!found);
            slot = &instr->written_stack_slot_type.value();
            found = true;
          }
        }
        ASSERT(found);
      }

      // do the same for next state (maybe)
      auto as_store_op = dynamic_cast<StoreOp*>(aop.get());
      if (as_store_op) {
        IR2_RegOffset ro;
        // note that this isn't 100% sure to actually be a next state.
        // the implementation of StoreOp will have to notice these false positives and copy
        // the next state type (not a big deal).
        if (get_as_reg_offset(as_store_op->addr(), &ro)) {
          if (ro.reg == Register(Reg::GPR, Reg::S6) &&
              ro.offset == OFFSET_OF_NEXT_STATE_STORE[func.ir2.env.version]) {
            instr->written_next_state_type = types2::Type();
            state.next_state_type = &instr->written_next_state_type.value();
          }
        }
      }

      // now store the state:
      instr->types = state;
    }
  }
}

/*!
 * Wrapper around a TypeState* that temporarily modifies types for a cast.
 * When this is destroyed, the casts will be reverted.
 */
class TypeStateCasted {
 public:
  TypeStateCasted(TypeState* state) : m_state(state) {}
  TypeStateCasted(TypeState* state, const Env& env, int aop_idx, DecompilerTypeSystem& dts)
      : TypeStateCasted(state) {
    const auto& reg_cast_it = env.casts().find(aop_idx);
    if (reg_cast_it != env.casts().end()) {
      // apply register casts!
      for (auto& cast : reg_cast_it->second) {
        push_reg_cast(cast.reg, dts.parse_type_spec(cast.type_name));
      }
    }

    for (const auto& [offset, cast] : env.stack_casts()) {
      push_stack_cast(offset, dts.parse_type_spec(cast.type_name), env.func);
    }
  }
  TypeStateCasted(const TypeStateCasted&) = delete;
  TypeStateCasted& operator=(const TypeStateCasted&) = delete;

  void push_reg_cast(Register reg, const TypeSpec& type) {
    auto& cast = m_restores.emplace_back();
    cast.reg = reg;
    cast.is_reg = true;
    cast.previous = (*m_state)[reg]->type;
    (*m_state)[reg]->type = TP_Type::make_from_ts(type);
  }

  void push_stack_cast(int slot, const TypeSpec& type, const Function* func) {
    auto& cast = m_restores.emplace_back();
    cast.stack_slot = slot;
    cast.is_reg = false;
    auto spill_slot = m_state->try_find_stack_spill_slot(slot);
    ASSERT_MSG(spill_slot, fmt::format("Function {} has no stack slot at {}", func->name(), slot));
    cast.previous = spill_slot->type;
    spill_slot->type = TP_Type::make_from_ts(type);
  }

  ~TypeStateCasted() {
    for (auto it = m_restores.rbegin(); it != m_restores.rend(); it++) {
      if (it->is_reg) {
        (*m_state)[it->reg]->type = it->previous;
      } else {
        m_state->try_find_stack_spill_slot(it->stack_slot)->type = it->previous;
      }
    }
  }

 private:
  struct Cast {
    Register reg;
    int stack_slot;
    std::optional<TP_Type> previous;
    bool is_reg;
  };
  std::vector<Cast> m_restores;
  TypeState* m_state;
};

void backprop_from_preds(FunctionCache& cache,
                         int block_idx,
                         Function& func,
                         TypeState* block_end_typestate,
                         DecompilerTypeSystem& dts) {
  auto& cblock = cache.blocks.at(block_idx);
  auto& block = func.basic_blocks.at(block_idx);

  // first, we'll see if we can get any information by looking at our successors.
  // if not, we'll notify our successors to prepare information for us, and we'll get it next time.

  // loop over registers (both gpr and fpr)
  for (auto reg_type : {Reg::GPR, Reg::FPR}) {
    for (int i = 0; i < 32; i++) {
      auto reg = Register(reg_type, i);

      // we're only interested in types with a "tag" that indicates they need more info.
      if (block_end_typestate->operator[](reg)->tag.has_tag()) {
        std::optional<TP_Type> resolve_type;

        // loop over successors
        for (auto& succ_idx : {block.succ_branch, block.succ_ft}) {
          if (succ_idx >= 0) {
            // the successor will use block entry tags to report back this info.
            // we'll check for these, use them if they exist, and add them if they don't.
            bool this_succ_has_tag = false;
            auto& succ_cblock = cache.blocks.at(succ_idx);

            // loop over all block entry tags - these are used within the block to backpropagate
            // constraints to the start of the block.
            for (auto& succ_tag : succ_cblock.block_entry_tags) {
              // see if it matches...
              if (succ_tag->is_reg && succ_tag->reg == reg) {
                // remember the tag exists so we don't add another.
                this_succ_has_tag = true;

                // see if the tag has been resolved
                if (succ_tag->selected_type) {
                  // it has, we can use this to update our guess of the type
                  if (resolve_type) {
                    // we already have 1 guess, lca of the guesses.
                    bool change;
                    resolve_type = dts.tp_lca(*resolve_type, *succ_tag->selected_type, &change);
                  } else {
                    // no existing guess, just use this one.
                    resolve_type = succ_tag->selected_type;
                  }
                }
              }
            }

            // if we didn't find a tag, we should add one
            if (!this_succ_has_tag) {
              // allocate it in the block
              succ_cblock.block_entry_tags.push_back(std::make_unique<BlockEntryType>());
              auto* tag = succ_cblock.block_entry_tags.back().get();
              // set up for the register
              tag->is_reg = true;
              tag->reg = reg;
              tag->type_to_clear = &cache.blocks.at(succ_idx).start_type_state[reg]->type;
              // lg::print("mark to clear {}\n", succ_idx);
              // add the tag to the type.
              auto& st = succ_cblock.start_types[reg];
              ASSERT(!st.tag.has_tag());
              st.tag.kind = Tag::BLOCK_ENTRY;
              st.tag.block_entry = tag;
              succ_cblock.needs_run = true;
            }
          }
        }

        // we got info from the successor, pass it back into this block
        if (resolve_type) {
          if (backprop_tagged_type(*resolve_type, *(*block_end_typestate)[reg], dts)) {
            // if we've changed things, mark this block to be re-ran.
            cblock.needs_run = true;
          }
        }
      }
    }
  }

  // if we updated somebody else's tags
  bool tags_updated = false;
  for (auto& my_tag : cblock.block_entry_tags) {
    if (my_tag->updated) {
      tags_updated = true;
      my_tag->updated = false;
      // lg::print("clearing {}\n", block_idx);
      cblock.needs_run = true;      // maybe?
      *my_tag->type_to_clear = {};  // meh..
    }
  }

  if (tags_updated) {
    for (auto& pred : block.pred) {
      cache.blocks.at(pred).needs_run = true;
    }
  }
}

std::optional<TP_Type> tp_lca(const types2::Type& existing,
                              const types2::Type& add,
                              bool* changed,
                              DecompilerTypeSystem& dts) {
  if (existing.type && add.type) {
    return dts.tp_lca(*existing.type, *add.type, changed);
  } else if (existing.type && !add.type) {
    *changed = false;
    return existing.type;
  } else if (!existing.type && add.type) {
    *changed = true;
    return add.type;
  } else {
    // neither
    *changed = false;
    return {};
  }
}

/*!
 * Find the least common ancestor of an entire typestate.
 */
bool tp_lca(types2::TypeState* combined, const types2::TypeState& add, DecompilerTypeSystem& dts) {
  bool result = false;
  for (int i = 0; i < 32; i++) {
    bool diff = false;
    auto new_type = tp_lca(*combined->gpr_types[i], *add.gpr_types[i], &diff, dts);
    if (diff) {
      result = true;
      combined->gpr_types[i]->type = new_type;
    }
  }

  for (int i = 0; i < 32; i++) {
    bool diff = false;
    auto new_type = tp_lca(*combined->fpr_types[i], *add.fpr_types[i], &diff, dts);
    if (diff) {
      result = true;
      combined->fpr_types[i]->type = new_type;
    }
  }

  for (auto& x : add.stack_slot_types) {
    bool diff = false;
    auto comb = combined->try_find_stack_spill_slot(x->slot);
    if (!comb) {
      lg::print("failed to find {}\n", x->slot);
      for (auto& x : combined->stack_slot_types) {
        lg::print("x = {}\n", x->slot);
      }
    }
    ASSERT(comb);
    auto new_type = tp_lca(*comb, x->type, &diff, dts);
    if (diff) {
      result = true;
      comb->type = new_type;
    }
  }

  bool diff = false;
  auto new_type = tp_lca(*combined->next_state_type, *add.next_state_type, &diff, dts);
  if (diff) {
    result = true;
    combined->next_state_type->type = new_type;
  }

  return result;
}

/*!
 * Propagate types from the beginning of this block.
 */
bool propagate_block(FunctionCache& cache,
                     int block_idx,
                     Function& func,
                     DecompilerTypeSystem& dts,
                     bool tag_lock) {
  auto& cblock = cache.blocks.at(block_idx);
  auto& block = func.basic_blocks.at(block_idx);
  // for now, assume we'll be done. something might change this later, we'll see
  cblock.needs_run = false;

  // propagate through instructions
  TypeState* previous_typestate = &cblock.start_type_state;
  for (auto instr : cblock.instructions) {
    {
      TypeStateCasted casted(previous_typestate, func.ir2.env, instr->aop_idx, *func.ir2.env.dts);
      auto& aop = func.ir2.atomic_ops->ops.at(instr->aop_idx);
      TypePropExtras extras;
      extras.tags_locked = tag_lock;
      // lg::print("run: {}\n", aop->to_string(func.ir2.env));

      try {
        aop->propagate_types2(*instr, func.ir2.env, *previous_typestate, *func.ir2.env.dts, extras);
      } catch (const std::exception& e) {
        auto error = fmt::format("failed type prop at {}: {}", instr->aop_idx, e.what());
        func.warnings.error(error);
        lg::error("Function {} {}", func.name(), error);
        return false;
      }
      if (extras.needs_rerun) {
        cblock.needs_run = true;
      }
      // propagate forward
      // TODO
      // handle constraints
    }
    previous_typestate = &instr->types;
  }

  // now that we've reached the end, handle backprop across blocks
  if (!tag_lock) {
    backprop_from_preds(cache, block_idx, func, previous_typestate, *func.ir2.env.dts);
  }

  // deal with end crap

  // lca

  // set tags on succs/backprop from succs

  for (auto succ_block_id : {block.succ_ft, block.succ_branch}) {
    if (succ_block_id != -1) {
      // set types to LCA (current, new)
      if (tp_lca(&cache.blocks.at(succ_block_id).start_type_state, *previous_typestate, dts)) {
        // if something changed, run again!
        cache.blocks.at(succ_block_id).needs_run = true;
      }
    }
  }
  return true;
}

bool convert_to_old_format(TP_Type& out, const types2::Type* in, bool recovery_mode) {
  if (!in->type) {
    if (recovery_mode) {
      out = TP_Type::make_uninitialized();
      return true;
    } else {
      return false;
    }
  } else {
    out = *in->type;
    return true;
  }
}

bool convert_to_old_format(::decompiler::TypeState& out,
                           const types2::TypeState& in,
                           std::string& error_string,
                           int my_idx,
                           const std::unordered_map<int, std::vector<RegisterTypeCast>>& casts,
                           const std::unordered_map<int, StackTypeCast>& stack_casts,
                           const DecompilerTypeSystem& dts,
                           bool recovery_mode) {
  for (int i = 0; i < 32; i++) {
    ASSERT(in.fpr_types[i]);
    if (!convert_to_old_format(out.fpr_types[i], in.fpr_types[i], recovery_mode)) {
      error_string += fmt::format("Failed to convert FPR: {} ", i);
      return false;
    }
    if (!convert_to_old_format(out.gpr_types[i], in.gpr_types[i], recovery_mode)) {
      error_string += fmt::format("Failed to convert GPR: {} ", Register(Reg::GPR, i).to_string());
      return false;
    }
  }

  if (!convert_to_old_format(out.next_state_type, in.next_state_type, recovery_mode)) {
    error_string += "Failed to convert next state ";
    return false;
  }

  const auto& reg_casts = casts.find(my_idx);
  if (reg_casts != casts.end()) {
    for (auto& cast : reg_casts->second) {
      out.get(cast.reg) = TP_Type::make_from_ts(dts.parse_type_spec(cast.type_name));
    }
  }

  for (auto& x : in.stack_slot_types) {
    TP_Type temp;
    if (!convert_to_old_format(temp, &x->type, recovery_mode)) {
      error_string += fmt::format("Failed to convert stack slot: {} ", x->slot);
      return false;
    }
    out.spill_slots[x->slot] = temp;
  }

  for (auto& [offset, type] : stack_casts) {
    out.spill_slots[offset] = TP_Type::make_from_ts(dts.parse_type_spec(type.type_name));
  }
  return true;
}

bool convert_to_old_format(Output& out,
                           FunctionCache& in,
                           std::string& error_string,
                           const std::unordered_map<int, std::vector<RegisterTypeCast>>& casts,
                           const std::unordered_map<int, StackTypeCast>& stack_casts,
                           const DecompilerTypeSystem& dts,
                           bool recovery_mode) {
  // for (auto& block : in.blocks) {
  out.op_end_types.resize(in.instructions.size());
  out.block_init_types.resize(in.blocks.size());
  for (int block_idx : in.block_visit_order) {
    auto& block = in.blocks[block_idx];
    if (!convert_to_old_format(out.block_init_types.at(block_idx), block.start_type_state,
                               error_string, block.instructions.at(0)->aop_idx, casts, stack_casts,
                               dts, recovery_mode)) {
      error_string += fmt::format(" at the start of block {}\n", block_idx);
      return false;
    }

    for (auto& instr : block.instructions) {
      if (!convert_to_old_format(out.op_end_types.at(instr->aop_idx), instr->types, error_string,
                                 instr->aop_idx + 1, casts, stack_casts, dts, recovery_mode)) {
        error_string += fmt::format(" at op {}\n", instr->aop_idx);
        return false;
      }
    }
  }

  return true;
}

/*!
 * Main Types2 Analysis pass.
 */
void run(Output& out, const Input& input) {
  // First, construct our graph
  FunctionCache function_cache;
  auto stack_slots = find_stack_spill_slots(*input.func);
  build_function(function_cache, *input.func, stack_slots);

  // annoying hack
  if (input.func->guessed_name.kind == FunctionName::FunctionKind::METHOD) {
    input.dts->type_prop_settings.current_method_type = input.func->guessed_name.type_name;
  }

  if (input.function_type.last_arg() == TypeSpec("none")) {
    auto as_end = dynamic_cast<FunctionEndOp*>(input.func->ir2.atomic_ops->ops.back().get());
    ASSERT(as_end);
    as_end->mark_function_as_no_return_value();
  }

  // mark the entry block
  function_cache.blocks.at(0).needs_run = true;
  construct_function_entry_types(function_cache.blocks.at(0).start_types, input.function_type,
                                 stack_slots);

  // Run propagation, until we get through an iteration with no changes
  [[maybe_unused]] int blocks_run = 0;
  [[maybe_unused]] int outer_iterations = 0;
  bool needs_rerun = true;
  bool hit_error = false;
  while (needs_rerun) {
    outer_iterations++;
    needs_rerun = false;

    for (auto block_idx : function_cache.block_visit_order) {
      if (function_cache.blocks.at(block_idx).needs_run) {
        blocks_run++;
        needs_rerun = true;
        if (!propagate_block(function_cache, block_idx, *input.func, *input.func->ir2.env.dts,
                             false)) {
          hit_error = true;
          goto end_type_pass;
        }
      }
    }

    auto& return_type = input.function_type.last_arg();
    if (return_type != TypeSpec("none")) {
      auto& last_instr = function_cache.instructions.back().types[Register(Reg::GPR, Reg::V0)];
      if (last_instr->tag.has_tag()) {
        if (!last_instr->type || !input.dts->ts.tc(return_type, last_instr->type->typespec())) {
          if (backprop_tagged_type(TP_Type::make_from_ts(return_type), *last_instr, *input.dts)) {
            needs_rerun = true;
          }
        }
      }
    }
  }
  for (auto block_idx : function_cache.block_visit_order) {
    if (block_idx != 0) {
      auto& cblock = function_cache.blocks.at(block_idx).start_types;
      for (auto& x : cblock.gpr_types) {
        x.type = TP_Type::make_uninitialized();
      }
      for (auto& x : cblock.fpr_types) {
        x.type = TP_Type::make_uninitialized();
      }

      for (auto x : stack_slots) {
        auto slot = cblock.try_find_stack_spill_slot(x);
        ASSERT(slot);
        slot->type.type = TP_Type::make_uninitialized();
      }

      cblock.next_state_type.type = TP_Type::make_uninitialized();
    }
  }

  needs_rerun = true;
  function_cache.blocks.at(0).needs_run = true;
  while (needs_rerun) {
    outer_iterations++;
    needs_rerun = false;
    for (auto block_idx : function_cache.block_visit_order) {
      if (function_cache.blocks.at(block_idx).needs_run) {
        blocks_run++;
        needs_rerun = true;
        if (!propagate_block(function_cache, block_idx, *input.func, *input.func->ir2.env.dts,
                             true)) {
          hit_error = true;
          goto end_type_pass;
        }
      }
    }
  }

end_type_pass:
  std::string error;
  if (!convert_to_old_format(out, function_cache, error, input.func->ir2.env.casts(),
                             input.func->ir2.env.stack_casts(), *input.dts, hit_error)) {
    lg::print("Failed convert_to_old_format: {}\n", error);
  } else {
    input.func->ir2.env.types_succeeded = true;
    auto last_type = out.op_end_types.back().get(Register(Reg::GPR, Reg::V0)).typespec();
    if (last_type != input.function_type.last_arg()) {
      input.func->warnings.warning("Return type mismatch {} vs {}.", last_type.print(),
                                   input.function_type.last_arg().print());
    }
  }

  // figure out the types of stack spill variables:
  auto& env = input.func->ir2.env;
  bool changed;
  for (auto& type_info : out.op_end_types) {
    for (auto& spill : type_info.spill_slots) {
      auto& slot_info = env.stack_slot_entries[spill.first];
      slot_info.tp_type =
          input.dts->tp_lca(env.stack_slot_entries[spill.first].tp_type, spill.second, &changed);
      slot_info.offset = spill.first;
    }
  }

  for (auto& type_info : out.block_init_types) {
    for (auto& spill : type_info.spill_slots) {
      auto& slot_info = env.stack_slot_entries[spill.first];
      slot_info.tp_type =
          input.dts->tp_lca(env.stack_slot_entries[spill.first].tp_type, spill.second, &changed);
      slot_info.offset = spill.first;
    }
  }

  // convert to typespec
  for (auto& info : env.stack_slot_entries) {
    info.second.typespec = info.second.tp_type.typespec();
    //     debug
    // lg::print("STACK {} : {} ({})\n", info.first, info.second.typespec.print(),
    //         info.second.tp_type.print());
  }

  // notify the label db of guessed labels
  for (auto& instr : function_cache.instructions) {
    if (instr.unknown_label_tag) {
      if (!instr.unknown_label_tag->selected_type) {
        env.func->warnings.error("Failed to guess label use for {} in {}:{}",
                                 instr.unknown_label_tag->label_name, input.func->name(),
                                 instr.aop_idx);
        out.succeeded = false;
        return;  // abort here - the type analysis above likely failed
      }
      auto& type = instr.unknown_label_tag->selected_type.value();
      int idx = instr.unknown_label_tag->label_idx;
      env.file->label_db->set_and_get_previous(idx, type, false, {});
    }

    if (instr.unknown_stack_structure_tag) {
      if (!instr.unknown_stack_structure_tag->selected_type) {
        env.func->warnings.error("Failed to guess stack use for {} in {}:{}",
                                 instr.unknown_stack_structure_tag->stack_offset,
                                 input.func->name(), instr.aop_idx);
        out.succeeded = false;
        return;  // abort here - the type analysis above likely failed
      }

      auto& type = instr.unknown_stack_structure_tag->selected_type.value();
      int offset = instr.unknown_stack_structure_tag->stack_offset;
      if (type.base_type() != "pointer") {
        StackStructureHint hint;
        hint.stack_offset = offset;
        hint.container_type = StackStructureHint::ContainerType::NONE;
        hint.element_type = type.print();
        env.add_stack_structure_hint(hint);
      }
    }
  }

  out.succeeded = !hit_error;
}

}  // namespace decompiler::types2
