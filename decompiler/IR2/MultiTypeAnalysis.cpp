/*!
 * @file MultiTypeAnalysis.cpp
 * The "new" type analysis pass which considers multiple possible types that can be at each
 * register, due to overlapping fields in types.  When it encounters a function call, set, or
 * certain math operation, it will attempt to prune the decision tree to remove incompatible types.
 *
 * Compared to
 *
 * When there are multiple ways to get the same type, or the type is ambiguous, it will use the one
 * with the highest score.
 *
 * Compared to the previous type analysis pass, there is more of an focus on being fast, as this is
 * historically the slowest part of decompilation.
 *
 * It will attempt to propagate these decision trees across basic block boundaries, but any time
 * there is a "phi node" where a registers can possible come from two different sources, it will
 * prune the tree to a single decision there.
 */

#include <limits>

#include "common/util/assert.h"
#include "decompiler/Function/Warnings.h"
#include "MultiTypeAnalysis.h"
#include "decompiler/IR2/Env.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/Function/Function.h"

namespace decompiler {

RegisterTypeState* TypeAnalysisGraph::alloc_regstate() {
  node_pool.push_back(std::make_unique<RegisterTypeState>());
  return node_pool.back().get();
}

std::shared_ptr<TypeAnalysisGraph> allocate_analysis_graph(const TypeSpec& my_type,
                                                           DecompilerTypeSystem& dts,
                                                           Function& func,
                                                           bool verbose) {
  auto result = std::make_unique<TypeAnalysisGraph>();
  auto clobber_type = result->alloc_regstate();
  *clobber_type = RegisterTypeState(PossibleType(TP_Type::make_uninitialized()));

  InstrTypeState default_state;
  for (auto& slot : func.ir2.env.stack_spills().map()) {
    default_state.add_stack_slot(slot.first);
  }

  // approximate the size and complain if it's huge.
  int state_size =
      sizeof(InstrTypeState) + default_state.stack_slot_count() * (sizeof(RegisterNode) + 4);

  result->block_start_types.resize(func.basic_blocks.size(), default_state);
  result->after_op_types.resize(func.ir2.atomic_ops->ops.size(), default_state);

  int instr_count = result->block_start_types.size() + result->after_op_types.size();
  int ref_size_kb = (instr_count * state_size) / 1024;

  if (verbose) {
    if (ref_size_kb > 500) {
      lg::info(
          "Func {} has {} instr states, each {} bytes, for a total of {} kb in just references.",
          func.guessed_name.to_string(), instr_count, state_size, ref_size_kb);
    }
  }

  result->topo_sort = func.bb_topo_sort();
  if (verbose) {
    if (result->topo_sort.vist_order.size() > 100) {
      lg::info("Func {} has {} basic blocks.", func.guessed_name.to_string(),
               result->topo_sort.vist_order.size());
    }
  }

  // set up the initial state:
  int allocation_count = 0;
  int uid = 1;

  auto& initial_state = result->block_start_types.at(0);
  for (auto& r : initial_state.regs()) {
    // okay to leave these as uninitialized - the function setup stuff will take care of this.
    r.set_alloc(result->alloc_regstate());
    r.set_uid(uid++);
    allocation_count++;
  }
  for (auto& s : initial_state.slots()) {
    s.second.set_alloc(result->alloc_regstate());
    s.second.set_uid(uid++);
    allocation_count++;
  }

  // do allocations
  auto& aop = func.ir2.atomic_ops;
  bool run_again = true;
  int iterations = 0;

  auto alloc_and_write_here = [&](RegisterNode& node) {
    if (!node.alloc()) {
      allocation_count++;
      run_again = true;
      node.set_alloc(result->alloc_regstate());
    }
    node.set_uid(uid++);
  };

  while (run_again) {
    iterations++;
    run_again = false;
    // do each block in the topological sort order:
    for (auto block_id : result->topo_sort.vist_order) {
      auto& block = func.basic_blocks.at(block_id);
      auto* init_types = &result->block_start_types.at(block_id);
      for (int op_id = aop->block_id_to_first_atomic_op.at(block_id);
           op_id < aop->block_id_to_end_atomic_op.at(block_id); op_id++) {
        AtomicOp* op = aop->ops.at(op_id).get();

        result->after_op_types.at(op_id) = *init_types;
        auto& after_types = result->after_op_types.at(op_id);

        // if we're a stack store, alloc a node for our write to the stack
        auto* op_as_stack_store = dynamic_cast<StackSpillStoreOp*>(op);
        if (op_as_stack_store) {
          alloc_and_write_here(after_types.get_slot(op_as_stack_store->offset()));
        }

        // if we're a register write, alloc nodes for our writes
        for (const auto& reg : op->write_regs()) {
          if (reg.reg_id() >= Reg::MAX_VAR_REG_ID) {
            continue;
          }
          alloc_and_write_here(after_types.get(reg));
        }

        // trick for clobbers.
        for (const auto& reg : op->clobber_regs()) {
          if (reg.reg_id() >= Reg::MAX_VAR_REG_ID) {
            continue;
          }
          auto& c = after_types.get(reg);
          c.set_clobber(clobber_type);
          c.set_uid(uid++);
        }

        // for the next op...
        init_types = &after_types;
      }

      // propagate the types: for each possible succ
      for (auto succ_block_id : {block.succ_ft, block.succ_branch}) {
        if (succ_block_id != -1) {
          // set types to LCA (current, new)
          auto& succ_types = result->block_start_types.at(succ_block_id);
          for (size_t i = 0; i < succ_types.regs().size(); i++) {
            auto& succ = succ_types.regs()[i];
            auto& end = init_types->regs()[i];

            if (succ.uid() == -1) {
              succ.set_uid(end.uid());
            } else {
              if (succ.uid() == end.uid()) {
                // nice!!
                // lg::info("Saved allocation");
              } else {
                succ.set_uid(uid++);
                if (!succ.alloc()) {
                  run_again = true;
                  succ.set_alloc(result->alloc_regstate());
                  allocation_count++;
                }
              }
            }
          }

          for (size_t i = 0; i < succ_types.slots().size(); i++) {
            auto& succ = succ_types.slots()[i];
            auto& end = init_types->slots().at(i);

            if (succ.second.uid() == -1) {
              succ.second.set_uid(end.second.uid());
            } else {
              if (succ.second.uid() == end.second.uid()) {
                // nice!!
                // lg::info("Saved allocation");
              } else {
                succ.second.set_uid(uid++);
                if (!succ.second.alloc()) {
                  run_again = true;
                  succ.second.set_alloc(result->alloc_regstate());
                  allocation_count++;
                }
              }
            }
          }
        }
      }
    }
  }

  int allocations_size_kb = (allocation_count * sizeof(RegisterTypeState)) / 1024;
  int total_size_new_method_kb = allocations_size_kb + ref_size_kb;
  int total_size_old_method_kb = (instr_count * 64 * sizeof(RegisterTypeState)) / 1024;

  if (total_size_old_method_kb > 1000) {
    lg::info("Function {} new {} kb old {} kb, {} allocs", func.guessed_name.to_string(),
             total_size_new_method_kb, total_size_old_method_kb, allocation_count);
  }

  return result;
}

bool DerefHint::matches(const FieldReverseLookupOutput& value) const {
  if (value.tokens.size() != tokens.size()) {
    return false;
  }

  for (size_t i = 0; i < value.tokens.size(); i++) {
    if (!tokens[i].matches(value.tokens[i])) {
      return false;
    }
  }

  return true;
}

bool DerefHint::Token::matches(const FieldReverseLookupOutput::Token& other) const {
  switch (kind) {
    case Kind::INTEGER:
      return other.kind == FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX &&
             other.idx == integer;
    case Kind::FIELD:
      return other.kind == FieldReverseLookupOutput::Token::Kind::FIELD && other.name == name;
    case Kind::VAR:
      return other.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX;
    default:
      assert(false);
  }
}

const PossibleType& TypeChoiceParent::get() const {
  return reg_type->possible_types.at(idx_in_parent);
}

PossibleType& TypeChoiceParent::get() {
  return reg_type->possible_types.at(idx_in_parent);
}

void TypeChoiceParent::remove_ref() {
  assert(get().child_count > 0);
  get().child_count--;
  if (get().child_count == 0) {
    get().eliminate();
  }
}

/*!
 * Have we been eliminated or not?
 */
bool PossibleType::is_valid() const {
  if (!m_valid_cache) {
    // either explicitly eliminated, or we cached this from last time.
    return false;
  }

  if (parent.reg_type) {
    if (!parent.get().is_valid()) {
      // some parent is eliminated, so are we. cache it.
      m_valid_cache = false;
      return false;
    }
  }

  return true;
}

void PossibleType::eliminate() {
  assert(is_valid());  // todo remove
  // make us invalid
  m_valid_cache = false;
  if (parent.reg_type) {
    parent.remove_ref();
  }
}

/*!
 * If we have multiple types, pick the one with the highest deref path score.
 * If warnings is set, and we have to throw away a valid type, prints a warning that we made a
 * somewhat arbitrary decision to throw a possible type.
 *
 * After calling this, you can use get_single_tp_type and get_single_type_decision.
 */
void RegisterTypeState::reduce_to_single_best_type(DecompWarnings* warnings,
                                                   int op_idx,
                                                   const DerefHint* hint) {
  if (is_temp_node) {
    assert(single_type_cache);
    return;
  }
  double best_score = -std::numeric_limits<double>::infinity();
  int best_idx = -1;
  bool printed_first_warning = false;
  std::string warning_string;

  // find the highest score that's valid.
  for (int i = 0; i < (int)possible_types.size(); i++) {
    if (possible_types[i].score > best_score && possible_types[i].is_valid()) {
      best_idx = i;
      best_score = possible_types[i].score;
    }

    // if we match the hint, just use that.
    if (hint && possible_types[i].deref_path && hint->matches(*possible_types[i].deref_path)) {
      best_idx = i;
      warnings = nullptr;  // never warn if we take the hint
      break;
    }
  }
  assert(best_idx != -1);

  // eliminate stuff that isn't the best.
  for (int i = 0; i < (int)possible_types.size(); i++) {
    if (i != best_idx) {
      // warn if we eliminate something that is possibly valid.
      if (warnings && possible_types[i].is_valid()) {
        if (!printed_first_warning) {
          warning_string += fmt::format("Ambiguous type selection at op {}\n", op_idx);
          printed_first_warning = true;
        }
        if (possible_types[best_idx].deref_path) {
          warning_string += fmt::format("  {}\n", possible_types[best_idx].deref_path->print());
        } else {
          warning_string += fmt::format("  {}\n", possible_types[best_idx].type.print());
        }
      }

      possible_types[i].eliminate();
    }
  }

  // cache the winner
  single_type_cache = best_idx;

  if (warnings && printed_first_warning) {
    warnings->general_warning(warning_string);
  }
}

/*!
 * After this has been pruned to a single type, gets that type decision.
 */
const PossibleType& RegisterTypeState::get_single_type_decision() const {
  assert(single_type_cache.has_value());
  assert(possible_types.at(*single_type_cache).is_valid());  // todo remove.
  return possible_types[*single_type_cache];
}

/*!
 * After this has been pruned to a single type, gets it as a TP_Type.
 */
const TP_Type& RegisterTypeState::get_single_tp_type() const {
  return get_single_type_decision().type;
}

/*!
 * If there is at least one possibility to get a desired_type, removes anything that's not a
 * desired_type. If it's not possible to get a desired type, does nothing.
 */
bool RegisterTypeState::try_elimination(const TypeSpec& desired_types, const TypeSystem& ts) {
  std::vector<int> to_eliminate;
  int keep_count = 0;
  for (int i = 0; i < (int)possible_types.size(); i++) {
    const auto& possibility = possible_types[i];
    if (possibility.is_valid()) {
      if (ts.tc(desired_types, possibility.type.typespec())) {
        keep_count++;
      } else {
        to_eliminate.push_back(i);
      }
    }
  }

  if (keep_count > 0) {
    for (auto idx : to_eliminate) {
      possible_types.at(idx).eliminate();
    }
    return true;
  }
  return false;
}

bool RegisterTypeState::can_eliminate_to_get(const TypeSpec& desired_types,
                                             const TypeSystem& ts) const {
  for (int i = 0; i < (int)possible_types.size(); i++) {
    const auto& possibility = possible_types[i];
    if (possibility.is_valid()) {
      if (ts.tc(desired_types, possibility.type.typespec())) {
        return true;
      }
    }
  }
  return false;
}

void InstrTypeState::assign(const Register& reg, const RegisterTypeState& value) {
  auto& node = get(reg);
  assert(node.is_alloc_point());
  *node.ptr() = value;
}

namespace {

template <typename T>
void for_each_regnode(InstrTypeState& state, const T& f) {
  for (auto& reg : state.regs()) {
    f(reg);
  }

  for (auto& slot : state.slots()) {
    f(slot.second);
  }
}

template <typename T>
void for_each_regnode_pair(InstrTypeState& state_a, InstrTypeState& state_b, const T& f) {
  for (size_t i = 0; i < state_a.regs().size(); i++) {
    f(state_a.regs()[i], state_b.regs()[i]);
  }

  assert(state_a.slots().size() == state_b.slots().size());
  for (size_t i = 0; i < state_a.slots().size(); i++) {
    auto& a = state_a.slots()[i];
    auto& b = state_b.slots()[i];
    assert(a.first == b.first);
    f(a.second, b.second);
  }
}

/*!
 * Create a register type state with no parent and the given typespec.
 */
RegisterTypeState make_typespec_parent_regstate(const TypeSpec& typespec) {
  return RegisterTypeState(TP_Type::make_from_ts(typespec));
}

/*!
 * Create a register type state with no parent and the given typespec.
 */
RegisterTypeState make_typespec_parent_regstate(const TP_Type& typespec) {
  return RegisterTypeState(typespec);
}

/*!
 * Create an instruction type state for the first instruction of a function.
 */
void construct_initial_typestate(InstrTypeState* result,
                                 const TypeSpec& function_type,
                                 const TypeSpec& behavior_type,
                                 const Env& env,
                                 const RegisterTypeState& uninitialized) {
  // start with everything uninitialized
  for_each_regnode(*result, [&](RegisterNode& node) {
    assert(node.is_alloc_point());
    *node.ptr() = uninitialized;
  });

  assert(function_type.base_type() == "function");
  assert(function_type.arg_count() >= 1);      // must know the function type.
  assert(function_type.arg_count() <= 8 + 1);  // 8 args + 1 return.

  for (int i = 0; i < int(function_type.arg_count()) - 1; i++) {
    auto reg_id = Register::get_arg_reg(i);
    const auto& reg_type = function_type.get_arg(i);
    result->assign(reg_id, make_typespec_parent_regstate(reg_type));
    ;
  }

  if (behavior_type != TypeSpec("none")) {
    result->assign(Register(Reg::GPR, Reg::S6), make_typespec_parent_regstate(behavior_type));
  }
}

/*!
 * Modify the state to include user cases.  Will prune as needed.
 * If we can't make it with pruning, modify.
 * TODO: I don't know if this temp nodes thing is a good idea or not
 */
InstrTypeState get_input_types_with_user_casts(
    const std::vector<RegisterTypeCast>* user_casts,
    const std::unordered_map<int, StackTypeCast>* stack_casts,
    InstrTypeState& state,
    const DecompilerTypeSystem& dts,
    std::vector<std::unique_ptr<RegisterTypeState>>& temp_nodes) {
  // we parse a string from a JSON config file here, so do this in a try/catch
  try {
    // first, see if pruning can help us get closer...
    if (user_casts) {
      for (const auto& cast : *user_casts) {
        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);
        // first, let's see if we can just prune the tree:
        // TODO: maybe there should be an option to avoid this?
        if (state.get_state(cast.reg).can_eliminate_to_get(type_from_cast, dts.ts)) {
          // we can! Just prune. This modifies the input, which is what we want.
          bool success = state.get_state(cast.reg).try_elimination(type_from_cast, dts.ts);
          assert(success);
        }
      }
    }

    if (stack_casts) {
      for (const auto& [offset, cast] : *stack_casts) {
        auto stack_state = state.get_slot_state(offset);

        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);
        if (stack_state.can_eliminate_to_get(type_from_cast, dts.ts)) {
          bool success = stack_state.try_elimination(type_from_cast, dts.ts);
          assert(success);
        }
      }
    }

    // now we need to make modifications:
    InstrTypeState result = state;

    auto make_temp = [&]() {
      temp_nodes.push_back(std::make_unique<RegisterTypeState>());
      auto* result = temp_nodes.back().get();
      result->is_temp_node = true;
      return result;
    };

    if (user_casts) {
      for (const auto& cast : *user_casts) {
        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);

        if (!state.get_state(cast.reg).can_eliminate_to_get(type_from_cast, dts.ts)) {
          // nope we can't make it work.
          // need to make a change here. It's fine to lose our decision history here because
          // we showed that there is no way to get what the user wants by pruning.
          auto temp = make_temp();
          *temp = make_typespec_parent_regstate(type_from_cast);
          result.get(cast.reg).set_cast_temp_ptr(temp);
        }
      }
    }

    if (stack_casts) {
      for (const auto& [offset, cast] : *stack_casts) {
        auto& stack_state = state.get_slot_state(offset);
        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);
        if (!stack_state.can_eliminate_to_get(type_from_cast, dts.ts)) {
          auto temp = make_temp();
          *temp = make_typespec_parent_regstate(type_from_cast);
          result.get_slot(offset).set_cast_temp_ptr(temp);
        }
      }
    }

    return result;

  } catch (std::exception& e) {
    lg::die("Failed to parse type cast hint: {}\n", e.what());
    throw;
  }
}

void simplify_to_single(int idx, DecompWarnings* warnings, DerefHint* hint, InstrTypeState& state) {
  for_each_regnode(state, [&](RegisterNode& node) {
    node.ptr()->reduce_to_single_best_type(warnings, idx, hint);
  });
}

/*!
 * Set combined to lca(combined, add) and do single simplification.
 */
bool multi_lca(InstrTypeState& combined,
               InstrTypeState& add,
               int pred_idx,
               int succ_idx,
               DecompWarnings* warnings,
               DecompilerTypeSystem& dts) {
  bool result = false;
  // first, simplify add:
  simplify_to_single(pred_idx, warnings, nullptr, add);

  for_each_regnode_pair(combined, add, [&](RegisterNode& c, const RegisterNode& a) {
    assert(c.is_alloc_point());

    bool diff = false;
    auto new_type = dts.tp_lca(c.ptr()->get_single_tp_type(), a.ptr()->get_single_tp_type(), &diff);
    if (diff) {
      result = true;
      // we checked is_alloc_point above.
      *c.ptr() = make_typespec_parent_regstate(new_type);
    }
  });
  return result;
}

TypeState convert_to_old_format(const InstrTypeState& input) {
  TypeState t;
  for (int regid = 0; regid < Reg::MAX_VAR_REG_ID; regid++) {
    t.get(Register(regid)) = input.regs().at(regid).ptr()->get_single_tp_type();
  }

  for (const auto& [offset, slot] : input.slots()) {
    t.get_slot(offset) = slot.ptr()->get_single_tp_type();
  }
  return t;
}

std::vector<TypeState> convert_to_old_format(const std::vector<InstrTypeState>& input) {
  std::vector<TypeState> result;
  result.reserve(input.size());
  for (auto& x : input) {
    result.push_back(convert_to_old_format(x));
  }
  return result;
}

bool dbg_types = true;
}  // namespace

bool run_multi_type_analysis(const TypeSpec& my_type,
                             DecompilerTypeSystem& dts,
                             Function& func,
                             TypeAnalysisGraph& graph) {
  if (dbg_types) {
    fmt::print("mtyp {}\n", func.guessed_name.to_string());
  }
  // STEP 0 - set decompiler type system settings for this function. these should be cleaned up
  // eventually...
  if (func.guessed_name.kind == FunctionName::FunctionKind::METHOD) {
    dts.type_prop_settings.current_method_type = func.guessed_name.type_name;
  }

  // set up none-returning function junk.
  if (my_type.last_arg() == TypeSpec("none")) {
    auto as_end = dynamic_cast<FunctionEndOp*>(func.ir2.atomic_ops->ops.back().get());
    assert(as_end);
    as_end->mark_function_as_no_return_value();
  }

  auto& block_init_types = graph.block_start_types;
  auto& op_types = graph.after_op_types;
  auto& aop = func.ir2.atomic_ops;

  // STEP 1 - topological sort the blocks. This gives us an order where we:
  // - never visit unreachable blocks (we can't type propagate these)
  // - always visit at least one predecessor of a block before that block
  const auto& order = graph.topo_sort;
  assert(!order.vist_order.empty());
  assert(order.vist_order.front() == 0);

  // STEP 2 - initialize type state for the first block to the function argument types.
  auto uninitialized = RegisterTypeState(PossibleType(TP_Type::make_uninitialized()));
  construct_initial_typestate(&block_init_types.at(0), my_type, TypeSpec("process"), func.ir2.env,
                              uninitialized);

  // STEP 3 - propagate types until the result stops changing
  bool run_again = true;
  while (run_again) {
    fmt::print("ITER\n");
    run_again = false;
    // do each block in the topological sort order:
    for (auto block_id : order.vist_order) {
      fmt::print("BLOCK {}\n", block_id);
      auto& block = func.basic_blocks.at(block_id);
      // pointer to the types (no user casts) before the op.
      auto* preceding_types = &block_init_types.at(block_id);

      // ops in block, in order
      for (int op_id = aop->block_id_to_first_atomic_op.at(block_id);
           op_id < aop->block_id_to_end_atomic_op.at(block_id); op_id++) {
        auto& op = aop->ops.at(op_id);
        // look for hints:
        const std::vector<RegisterTypeCast>* user_casts = nullptr;
        const std::unordered_map<int, StackTypeCast>* stack_casts = nullptr;
        const auto& cast_it = func.ir2.env.casts().find(op_id);
        if (cast_it != func.ir2.env.casts().end()) {
          user_casts = &cast_it->second;
        }

        if (!func.ir2.env.stack_casts().empty()) {
          stack_casts = &func.ir2.env.stack_casts();
        }

        try {
          auto& dest = op_types.at(op_id);
          dest = *preceding_types;
          if (stack_casts || user_casts) {
            std::vector<std::unique_ptr<RegisterTypeState>> temp_nodes;
            auto casted = get_input_types_with_user_casts(user_casts, stack_casts, *preceding_types,
                                                          dts, temp_nodes);
            op->multi_types(&dest, casted, func.ir2.env, dts);
          } else {
            op->multi_types(&dest, *preceding_types, func.ir2.env, dts);
          }
        } catch (std::runtime_error& e) {
          lg::warn("Function {} failed type prop at op {}: {}", func.guessed_name.to_string(),
                   op_id, e.what());
          func.warnings.type_prop_warning("{}", e.what());
          // func.ir2.env.set_types(block_init_types, op_types, *func.ir2.atomic_ops, my_type);
          return false;
        }

        // for the next op...
        preceding_types = &op_types.at(op_id);
      }

      // propagate the types: for each possible succ
      for (auto succ_block_id : {block.succ_ft, block.succ_branch}) {
        if (succ_block_id != -1) {
          // set types to LCA (current, new)
          if (multi_lca(block_init_types.at(succ_block_id), *preceding_types, block_id,
                        succ_block_id, nullptr, dts)) {
            run_again = true;
          }
        }
      }
    }
  }

  auto last_type =
      op_types.back().get(Register(Reg::GPR, Reg::V0)).ptr()->get_single_tp_type().typespec();
  if (last_type != my_type.last_arg()) {
    func.warnings.info("Return type mismatch {} vs {}.", last_type.print(),
                       my_type.last_arg().print());
  }

  // and apply final casts:
  for (auto block_id : order.vist_order) {
    for (int op_id = aop->block_id_to_first_atomic_op.at(block_id);
         op_id < aop->block_id_to_end_atomic_op.at(block_id); op_id++) {
      const std::vector<RegisterTypeCast>* user_casts = nullptr;
      const std::unordered_map<int, StackTypeCast>* stack_casts = nullptr;
      const auto& cast_it = func.ir2.env.casts().find(op_id);
      if (cast_it != func.ir2.env.casts().end()) {
        user_casts = &cast_it->second;
      }

      if (!func.ir2.env.stack_casts().empty()) {
        stack_casts = &func.ir2.env.stack_casts();
      }

      if (user_casts || stack_casts) {
        if (op_id == aop->block_id_to_first_atomic_op.at(block_id)) {
          block_init_types.at(block_id) = get_input_types_with_user_casts(
              user_casts, stack_casts, block_init_types.at(block_id), dts, graph.final_cast_nodes);

        } else {
          op_types.at(op_id - 1) = get_input_types_with_user_casts(
              user_casts, stack_casts, op_types.at(op_id - 1), dts, graph.final_cast_nodes);
        }
      }
    }
  }

  // figure out the types of stack spill variables:
  auto& env = func.ir2.env;
  bool changed;
  for (auto& type_info : op_types) {
    for (auto& spill : type_info.slots()) {
      auto& slot_info = env.stack_slot_entries[spill.first];
      slot_info.tp_type = dts.tp_lca(env.stack_slot_entries[spill.first].tp_type,
                                     spill.second.ptr()->get_single_tp_type(), &changed);
      slot_info.offset = spill.first;
    }
  }

  for (auto& type_info : block_init_types) {
    for (auto& spill : type_info.slots()) {
      auto& slot_info = env.stack_slot_entries[spill.first];
      slot_info.tp_type = dts.tp_lca(env.stack_slot_entries[spill.first].tp_type,
                                     spill.second.ptr()->get_single_tp_type(), &changed);
      slot_info.offset = spill.first;
    }
  }

  // convert to typespec
  for (auto& info : env.stack_slot_entries) {
    info.second.typespec = info.second.tp_type.typespec();
    //     debug
    // fmt::print("STACK {} : {} ({})\n", info.first, info.second.typespec.print(),
    //         info.second.tp_type.print());
  }

  func.ir2.env.set_types(convert_to_old_format(block_init_types), convert_to_old_format(op_types),
                         *func.ir2.atomic_ops, my_type);

  return true;
}

void AtomicOp::multi_types(InstrTypeState* output,
                           InstrTypeState& input,
                           const Env& env,
                           DecompilerTypeSystem& dts) {
  for (auto& reg : clobber_regs()) {
    assert(output->get(reg).is_clobber());
  }

  multi_types_internal(output, input, env, dts);
}

void AtomicOp::multi_types_internal(InstrTypeState*,
                                    InstrTypeState&,
                                    const Env&,
                                    DecompilerTypeSystem&) {
  throw std::runtime_error(
      fmt::format("multi_type_internal not yet implemented for {}", typeid(*this).name()));
}

RegisterTypeState SimpleAtom::get_type(InstrTypeState& input,
                                       const Env& env,
                                       const DecompilerTypeSystem& dts) const {
  switch (m_kind) {
    case Kind::VARIABLE:
      // just get the type in the variable.
      return input.get_state(var().reg());
    default:
      throw std::runtime_error("Simple atom cannot get_type (multi types): " + to_string(env));
  }
}

RegisterTypeState SimpleExpression::get_type(InstrTypeState& input,
                                             const Env& env,
                                             const DecompilerTypeSystem& dts) const {
  switch (m_kind) {
    case Kind::IDENTITY:
      // this expression is just an atom, so return the atom's type.
      return m_args[0].get_type(input, env, dts);
    default:
      throw std::runtime_error("Simple expression cannot get_type (multi types): " +
                               to_string(env));
  }
}

void SetVarOp::multi_types_internal(InstrTypeState* output,
                                    InstrTypeState& input,
                                    const Env& env,
                                    DecompilerTypeSystem& dts) {
  // we have special cases where we can infer something about the source type from the dest

  // GOAL will use mfc, fX, r0 to set a float to 0.
  if (m_dst.reg().get_kind() == Reg::FPR && m_src.is_identity() && m_src.get_arg(0).is_int() &&
      m_src.get_arg(0).get_int() == 0) {
    output->assign(m_dst.reg(), RegisterTypeState("float"));
  } else {
    output->assign(m_dst.reg(), m_src.get_type(input, env, dts));
  }

  // it's safe to do this, though a little confusing.
  // if this type is based on a cast, we can't have the possibility of referencing the temporary
  // cast. Instead we copy the cast (or the result of using the temporary cast) to the output.
  // If the next op casts this, it won't touch this because it will use its own temporary cast.

  // In the final cast application, this is also okay because this node will be kept, but replaced
  // in the main graph.  This will refer to the type without the cast, which is what we want.
  auto& out_node = output->get(m_dst.reg());
  assert(out_node.is_alloc_point() && !out_node.is_clobber() && !out_node.is_cast());
  m_source_type_new = &output->get_state(m_dst.reg());
}

void FunctionEndOp::multi_types_internal(InstrTypeState*,
                                         InstrTypeState&,
                                         const Env&,
                                         DecompilerTypeSystem&) {}

}  // namespace decompiler