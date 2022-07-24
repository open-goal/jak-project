#include "types2.h"

#include <set>

#include "decompiler/types2/Type.h"

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
BlockStartTypes construct_function_entry_types(const TypeSpec& f_ts,
                                               const std::set<int>& stack_slots) {
  BlockStartTypes result;
  for (auto& x : result.gpr_types) {
    x.type = TP_Type::make_uninitialized();
  }
  for (auto& x : result.fpr_types) {
    x.type = TP_Type::make_uninitialized();
  }

  for (auto x : stack_slots) {
    auto& slot = result.stack_slot_types.emplace_back();
    slot.slot = x;
    slot.type.type = TP_Type::make_uninitialized();
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
  return result;
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
        state[written_reg_type.reg] = &written_reg_type.type;
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

bool backprop_tagged_type(const TP_Type& expected_type, types2::Type& actual_type);

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
          if (backprop_tagged_type(*resolve_type, *(*block_end_typestate)[reg])) {
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
    auto new_type = tp_lca(combined->stack_slot_types[x->slot]->type, x->type, &diff, dts);
    if (diff) {
      result = true;
      combined->stack_slot_types[x->slot]->type.type = new_type;
    }
  }

  // TODO...
  //  bool diff = false;
  //  auto new_type = tp_lca(combined->next_state_type, add.next_state_type, &diff);
  //  if (diff) {
  //    result = true;
  //    combined->next_state_type = new_type;
  //  }

  return result;
}

/*!
 * Propagate types from the beginning of this block.
 */
void propagate_block(FunctionCache& cache,
                     int block_idx,
                     Function& func,
                     DecompilerTypeSystem& dts) {
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
      fmt::print("run: {}\n", aop->to_string(func.ir2.env));
      aop->propagate_types2(*instr, func.ir2.env, *previous_typestate, *func.ir2.env.dts, extras);
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
  backprop_from_preds(cache, block_idx, func, previous_typestate, *func.ir2.env.dts);

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
}

bool convert_to_old_format(TP_Type& out, const types2::Type* in) {
  if (!in->type) {
    return false;
  } else {
    out = *in->type;
    return true;
  }
}

bool convert_to_old_format(::decompiler::TypeState& out,
                           const types2::TypeState& in,
                           std::string& error_string) {
  for (int i = 0; i < 32; i++) {
    if (!convert_to_old_format(out.fpr_types[i], in.fpr_types[i])) {
      error_string += fmt::format("Failed to convert FPR: {} ", i);
      return false;
    }
    if (!convert_to_old_format(out.gpr_types[i], in.gpr_types[i])) {
      error_string += fmt::format("Failed to convert GPR: {} ", Register(Reg::GPR, i).to_string());
      return false;
    }
  }
  for (auto& x : in.stack_slot_types) {
    TP_Type temp;
    if (!convert_to_old_format(temp, &x->type)) {
      error_string += fmt::format("Failed to convert stack slot: {} ", x->slot);
      return false;
    }
    out.spill_slots[x->slot] = temp;
  }
  return true;
}

bool convert_to_old_format(Output& out, FunctionCache& in, std::string& error_string) {
  // for (auto& block : in.blocks) {
  for (size_t block_idx = 0; block_idx < in.blocks.size(); block_idx++) {
    auto& block = in.blocks[block_idx];
    auto& state = out.block_init_types.emplace_back();
    if (!convert_to_old_format(state, block.start_type_state, error_string)) {
      error_string += fmt::format(" at the start of block {}\n", block_idx);
      return false;
    }
  }

  for (auto& instr : in.instructions) {
    auto& state = out.op_end_types.emplace_back();
    if (!convert_to_old_format(state, instr.types, error_string)) {
      error_string += fmt::format(" at op {}\n", instr.aop_idx);
      return false;
    }
  }
  return true;
}

/*!
 * Main Types2 Analysis pass.
 */
void run(Output& out, const Input& input) {
  fmt::print("types: {}\n", input.func->name());
  // First, construct our graph
  FunctionCache function_cache;
  auto stack_slots = find_stack_spill_slots(*input.func);
  build_function(function_cache, *input.func, stack_slots);

  // mark the entry block
  function_cache.blocks.at(0).needs_run = true;
  function_cache.blocks.at(0).start_types =
      construct_function_entry_types(input.function_type, stack_slots);

  // Run propagation, until we get through an iteration with no changes
  int blocks_run = 0;
  int outer_iterations = 0;
  bool needs_rerun = true;
  while (needs_rerun) {
    outer_iterations++;
    needs_rerun = false;

    for (auto block_idx : function_cache.block_visit_order) {
      if (function_cache.blocks.at(block_idx).needs_run) {
        blocks_run++;
        needs_rerun = true;
        propagate_block(function_cache, block_idx, *input.func, *input.func->ir2.env.dts);
      }
    }
  }

  std::string error;
  if (!convert_to_old_format(out, function_cache, error)) {
    fmt::print("Failed convert_to_old_format: {}\n", error);
  }

  // final casts

  // stack spill var things

  // warn on return type
}

}  // namespace decompiler::types2