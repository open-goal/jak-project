/*!
 * @file MultiTypeAnalysis.cpp
 * The "new" type analysis pass which considers multiple possible types that can be at each
 * register, due to overlapping fields in types.  When it encounters a function call, set, or
 * certain math operation, it will attempt to prune the decision tree to remove incompatible types.
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

//RegisterTypeState* TypeAnalysisGraph::alloc_regstate() {
//  node_pool.push_back(std::make_unique<RegisterTypeState>());
//  return node_pool.back().get();
//}
//
//TypeAnalysisGraph make_analysis_graph(const TypeSpec& my_type,
//                                      DecompilerTypeSystem& dts,
//                                      Function& func,
//                                      bool verbose) {
//  TypeAnalysisGraph result;
//  auto clobber_type = result.alloc_regstate();
//  *clobber_type = RegisterTypeState(PossibleType(TP_Type::make_uninitialized()));
//
//  InstrTypeState default_state;
//  for (auto& slot : func.ir2.env.stack_spills().map()) {
//    default_state.add_stack_slot(slot.first);
//  }
//
//  // approximate the size and complain if it's huge.
//  int state_size =
//      sizeof(InstrTypeState) + default_state.stack_slot_count() * (sizeof(RegisterNode) + 4);
//
//  result.block_start_types.resize(func.basic_blocks.size());
//  result.after_op_types.resize(func.ir2.atomic_ops->ops.size());
//
//  int instr_count = result.block_start_types.size() + result.after_op_types.size();
//  int ref_size_kb = (instr_count * state_size) / 1024;
//
//  if (verbose) {
//    if (ref_size_kb > 500) {
//      lg::info(
//          "Func {} has {} instr states, each {} bytes, for a total of {} kb in just references.",
//          func.guessed_name.to_string(), instr_count, state_size, ref_size_kb);
//    }
//  }
//
//  result.topo_sort = func.bb_topo_sort();
//  if (verbose) {
//    if (result.topo_sort.vist_order.size() > 100) {
//      lg::info("Func {} has {} basic blocks.", func.guessed_name.to_string(),
//               result.topo_sort.vist_order.size());
//    }
//  }
//
//  // set up the initial state:
//  int allocation_count = 0;
//  int uid = 1;
//
//  auto& initial_state = result.block_start_types.at(0);
//  for (auto& r : initial_state.regs()) {
//    // okay to leave these as uninitialized - the function setup stuff will take care of this.
//    r.set_alloc(result.alloc_regstate());
//    r.set_uid(uid++);
//    allocation_count++;
//  }
//  for (auto& s : initial_state.slots()) {
//    s.second.set_alloc(result.alloc_regstate());
//    s.second.set_uid(uid++);
//    allocation_count++;
//  }
//
//  // do allocations
//  auto& aop = func.ir2.atomic_ops;
//  bool run_again = true;
//  int iterations = 0;
//  while (run_again) {
//    iterations++;
//    run_again = false;
//    // do each block in the topological sort order:
//    for (auto block_id : result.topo_sort.vist_order) {
//      auto& block = func.basic_blocks.at(block_id);
//      auto* init_types = &result.block_start_types.at(block_id);
//      for (int op_id = aop->block_id_to_first_atomic_op.at(block_id);
//           op_id < aop->block_id_to_end_atomic_op.at(block_id); op_id++) {
//        AtomicOp* op = aop->ops.at(op_id).get();
//
//        result.after_op_types.at(op_id) = *init_types;
//
//        // todo write stack slots.
//        auto* op_as_stack_store = dynamic_cast<StackSpillStoreOp*>(op);
//        if (op_as_stack_store) {
//          auto& state = init_types->get_slot(op_as_stack_store->offset());
//          if (!state.alloc()) {
//            allocation_count++;
//            run_again = true;
//            state.set_alloc(result.alloc_regstate());
//          }
//          state.set_uid(uid++);
//        }
//
//        for (const auto& reg : op->write_regs()) {
//          if (reg.reg_id() >= Reg::MAX_VAR_REG_ID) {
//            continue;
//          }
//          auto& state = init_types->get(reg);
//          if (!state.alloc()) {
//            allocation_count++;
//            run_again = true;
//            state.set_alloc(result.alloc_regstate());
//          }
//          state.set_uid(uid++);
//        }
//
//        for (const auto& reg : op->clobber_regs()) {
//          if (reg.reg_id() >= Reg::MAX_VAR_REG_ID) {
//            continue;
//          }
//          auto& c = init_types->get(reg);
//          c.set_alloc(clobber_type);
//          c.set_uid(uid++);
//        }
//
//        // for the next op...
//        init_types = &result.after_op_types.at(op_id);
//      }
//
//      // propagate the types: for each possible succ
//      for (auto succ_block_id : {block.succ_ft, block.succ_branch}) {
//        if (succ_block_id != -1) {
//          // set types to LCA (current, new)
//          auto& succ_types = result.block_start_types.at(succ_block_id);
//          for (size_t i = 0; i < succ_types.regs().size(); i++) {
//            auto& succ = succ_types.regs()[i];
//            auto& end = init_types->regs()[i];
//
//            if (succ.uid() == -1) {
//              succ.set_uid(end.uid());
//            } else {
//              if (succ.uid() == end.uid()) {
//                // nice!!
//                // lg::info("Saved allocation");
//              } else {
//                succ.set_uid(uid++);
//                if (!succ.alloc()) {
//                  run_again = true;
//                  succ.set_alloc(result.alloc_regstate());
//                  allocation_count++;
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//  }
//
//  int allocations_size_kb = (allocation_count * sizeof(RegisterTypeState)) / 1024;
//  int total_size_new_method_kb = allocations_size_kb + ref_size_kb;
//  int total_size_old_method_kb = (instr_count * 64 * sizeof(RegisterTypeState)) / 1024;
//
//  if (total_size_old_method_kb > 1000) {
//    lg::info("Function {} new {} kb old {} kb, {} allocs", func.guessed_name.to_string(),
//             total_size_new_method_kb, total_size_old_method_kb, allocation_count);
//  }
//
//  return result;
//}

// using RegState = CopyOnWrite<RegisterTypeState>;
//
// bool DerefHint::matches(const FieldReverseLookupOutput& value) const {
//   if (value.tokens.size() != tokens.size()) {
//     return false;
//   }
//
//   for (size_t i = 0; i < value.tokens.size(); i++) {
//     if (!tokens[i].matches(value.tokens[i])) {
//       return false;
//     }
//   }
//
//   return true;
// }
//
// bool DerefHint::Token::matches(const FieldReverseLookupOutput::Token& other) const {
//   switch (kind) {
//     case Kind::INTEGER:
//       return other.kind == FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX &&
//              other.idx == integer;
//     case Kind::FIELD:
//       return other.kind == FieldReverseLookupOutput::Token::Kind::FIELD && other.name == name;
//     case Kind::VAR:
//       return other.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX;
//     default:
//       assert(false);
//   }
// }
//
///*!
// * Safely access the decision referenced by this TypeDecisionParent.
// * This will work even if the actual RegisterTypeState has been modified since the reference was
// * created.
// */
// const PossibleType& TypeDecisionParent::get() const {
//  return instruction->get_const(reg).possible_types.at(type_index);
//}
//
// PossibleType& TypeDecisionParent::get() {
//  return instruction->get(reg).mut()->possible_types.at(type_index);
//}
//
///*!
// * Figure out if this has been eliminated or not. Caches the result to avoid looking it up again
// and
// * again. Elimination cannot be undone.
// */
// bool PossibleType::is_valid() const {
//  if (!m_valid_cache) {
//    return false;
//  }
//
//  if (child_count == 0) {
//    m_valid_cache = false;
//    return false;
//  }
//
//  if (parent.instruction) {
//    // we have a parent in the tree, check if that parent is eliminated.
//    if (!parent.get().is_valid()) {
//      m_valid_cache = false;
//      return false;
//    }
//  }
//
//  return true;
//}
//
// void PossibleType::eliminate() {
//  assert(is_valid());
//  if (parent.instruction) {
//    auto& par = parent.get();
//    par.child_count--;
//    assert(par.child_count >= 0);
//    if (!par.child_count) {
//      par.eliminate();
//    }
//  }
//}
//
///*!
// * If we have multiple types, pick the one with the highest deref path score.
// * If warnings is set, and we have to throw away a valid type, prints a warning that we made a
// * somewhat arbitrary decision to throw a possible type.
// *
// * After calling this, you can use get_single_tp_type and get_single_type_decision.
// */
// void RegisterTypeState::reduce_to_single_type(DecompWarnings* warnings,
//                                              int op_idx,
//                                              const DerefHint* hint) {
//  double best_score = -std::numeric_limits<double>::infinity();
//  int best_idx = -1;
//  bool printed_first_warning = false;
//  std::string warning_string;
//
//  // find the highest score that's valid.
//  for (int i = 0; i < (int)possible_types.size(); i++) {
//    if (possible_types[i].deref_score > best_score && possible_types[i].is_valid()) {
//      best_idx = i;
//      best_score = possible_types[i].deref_score;
//    }
//
//    // if we match the hint, just use that.
//    if (possible_types[i].deref_path && hint->matches(*possible_types[i].deref_path)) {
//      best_idx = i;
//      warnings = nullptr;  // never warn if we take the hint
//      break;
//    }
//  }
//  assert(best_idx != -1);
//
//  // eliminate stuff that isn't the best.
//  for (int i = 0; i < (int)possible_types.size(); i++) {
//    if (i != best_idx) {
//      // warn if we eliminate something that is possibly valid.
//      if (warnings && possible_types[i].is_valid()) {
//        if (!printed_first_warning) {
//          warning_string += fmt::format("Ambiguous type selection at op {}\n", op_idx);
//          printed_first_warning = true;
//        }
//        if (possible_types[best_idx].deref_path) {
//          warning_string += fmt::format("  {}\n", possible_types[best_idx].deref_path->print());
//        } else {
//          warning_string += fmt::format("  {}\n", possible_types[best_idx].type.print());
//        }
//      }
//
//      possible_types[i].eliminate();
//    }
//  }
//
//  // cache the winner
//  single_type_cache = best_idx;
//
//  if (warnings && printed_first_warning) {
//    warnings->general_warning(warning_string);
//  }
//}
//
///*!
// * After this has been pruned to a single type, gets that type decision.
// */
// const PossibleType& RegisterTypeState::get_single_type_decision() const {
//  assert(single_type_cache.has_value());
//  assert(possible_types.at(*single_type_cache).is_valid());  // todo remove.
//  return possible_types[*single_type_cache];
//}
//
///*!
// * After this has been pruned to a single type, gets it as a TP_Type.
// */
// const TP_Type& RegisterTypeState::get_single_tp_type() const {
//  return get_single_type_decision().type;
//}
//
///*!
// * If there is at least one possibility to get a desired_type, removes anything that's not a
// * desired_type. If it's not possible to get a desired type, does nothing.
// */
// bool RegisterTypeState::try_elimination(const TypeSpec& desired_types, const TypeSystem& ts) {
//  std::vector<int> to_eliminate;
//  int keep_count = 0;
//  for (int i = 0; i < (int)possible_types.size(); i++) {
//    const auto& possibility = possible_types[i];
//    if (possibility.is_valid()) {
//      if (ts.tc(desired_types, possibility.type.typespec())) {
//        keep_count++;
//      } else {
//        to_eliminate.push_back(i);
//      }
//    }
//  }
//
//  if (keep_count > 0) {
//    for (auto idx : to_eliminate) {
//      possible_types.at(idx).eliminate();
//    }
//    return true;
//  }
//  return false;
//}
//
// bool RegisterTypeState::can_eliminate_to_get(const TypeSpec& desired_types,
//                                             const TypeSystem& ts) const {
//  for (int i = 0; i < (int)possible_types.size(); i++) {
//    const auto& possibility = possible_types[i];
//    if (possibility.is_valid()) {
//      if (ts.tc(desired_types, possibility.type.typespec())) {
//        return true;
//      }
//    }
//  }
//  return false;
//}
//
// void InstrTypeState::inherit(InstrTypeState& prev) {
//  for (size_t i = 0; i < m_regs.size(); i++) {
//
//  }
//}
//
// namespace {
//
///*!
// * Create a register type state with no parent and the given typespec.
// */
// RegState make_typespec_parent_regstate(const TypeSpec& typespec) {
//  auto result = make_cow<RegisterTypeState>(TP_Type::make_from_ts(typespec));
//  result.mut()->reduce_to_single_type(nullptr, -1, nullptr);
//  return result;
//}
//
///*!
// * Create a register type state with no parent and the given typespec.
// */
// RegState make_typespec_parent_regstate(const TP_Type& typespec) {
//  auto result = make_cow<RegisterTypeState>(typespec);
//  result.mut()->reduce_to_single_type(nullptr, -1, nullptr);
//  return result;
//}
//
///*!
// * Create an instruction type state for the first instruction of a function.
// */
// InstrTypeState construct_initial_typestate(const TypeSpec& function_type,
//                                           const TypeSpec& behavior_type,
//                                           const Env& env,
//                                           const RegState& uninitialized) {
//  // start with everything uninitialized
//  InstrTypeState result(uninitialized);
//  assert(function_type.base_type() == "function");
//  assert(function_type.arg_count() >= 1);      // must know the function type.
//  assert(function_type.arg_count() <= 8 + 1);  // 8 args + 1 return.
//
//  for (int i = 0; i < int(function_type.arg_count()) - 1; i++) {
//    auto reg_id = Register::get_arg_reg(i);
//    const auto& reg_type = function_type.get_arg(i);
//    result.get(reg_id) = make_typespec_parent_regstate(reg_type);
//  }
//
//  if (behavior_type != TypeSpec("none")) {
//    result.get(Register(Reg::GPR, Reg::S6)) = make_typespec_parent_regstate(behavior_type);
//  }
//
//  // set stack slots as uninitialized too.
//  for (auto slot_info : env.stack_spills().map()) {
//    result.add_stack_slot(slot_info.first, uninitialized);
//  }
//
//  return result;
//}
//
///*!
// * Modify the state to include user cases.  Will prune as needed.
// * If we can't make it with pruning, modify.
// */
// InstrTypeState get_input_types_with_user_casts(
//    const std::vector<RegisterTypeCast>* user_casts,
//    const std::unordered_map<int, StackTypeCast>* stack_casts,
//    InstrTypeState& state,
//    const DecompilerTypeSystem& dts) {
//  // we parse a string from a JSON config file here, so do this in a try/catch
//  try {
//    // first, see if pruning can help us get closer...
//    if (user_casts) {
//      for (const auto& cast : *user_casts) {
//        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);
//        // first, let's see if we can just prune the tree:
//        // TODO: maybe there should be an option to avoid this?
//        if (state.get_const(cast.reg).can_eliminate_to_get(type_from_cast, dts.ts)) {
//          // we can! Just prune. This modifies the input, which is what we want.
//          bool success = state.get(cast.reg).mut()->try_elimination(type_from_cast, dts.ts);
//          assert(success);
//        }
//      }
//    }
//
//    if (stack_casts) {
//      for (const auto& [offset, cast] : *stack_casts) {
//        auto stack_state = state.get_stack_slot_const(offset);
//        if (!stack_state) {
//          throw std::runtime_error(fmt::format(
//              "Got a stack cast at offset {}, but didn't find a variable there.", offset));
//        }
//        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);
//        if (stack_state->can_eliminate_to_get(type_from_cast, dts.ts)) {
//          bool success =
//              state.get_stack_slot(offset)->mut()->try_elimination(type_from_cast, dts.ts);
//          assert(success);
//        }
//      }
//    }
//
//    // now we need to make modifications:
//    InstrTypeState result = state;
//
//    if (user_casts) {
//      for (const auto& cast : *user_casts) {
//        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);
//
//        if (!state.get_const(cast.reg).can_eliminate_to_get(type_from_cast, dts.ts)) {
//          // nope we can't make it work.
//          // need to make a change here. It's fine to lose our decision history here because
//          // we showed that there is no way to get what the user wants by pruning.
//          result.get(cast.reg) = make_typespec_parent_regstate(type_from_cast);
//        }
//      }
//    }
//
//    if (stack_casts) {
//      for (const auto& [offset, cast] : *stack_casts) {
//        auto stack_state = state.get_stack_slot_const(offset);
//        assert(stack_state);
//        TypeSpec type_from_cast = dts.parse_type_spec(cast.type_name);
//        if (!stack_state->can_eliminate_to_get(type_from_cast, dts.ts)) {
//          *result.get_stack_slot(offset) = make_typespec_parent_regstate(type_from_cast);
//        }
//      }
//    }
//
//    return result;
//
//  } catch (std::exception& e) {
//    lg::die("Failed to parse type cast hint: {}\n", e.what());
//    throw;
//  }
//}
//
//
//
// void simplify_to_single(int idx, DecompWarnings* warnings, DerefHint* hint, InstrTypeState&
// state) {
//  for (auto& reg : state.reg_array()) {
//    reg.mut()->reduce_to_single_type(warnings, idx, hint);
//  }
//
//  for (auto& stack : state.stack_slots()) {
//    stack.second.mut()->reduce_to_single_type(warnings, idx, hint);
//  }
//}
//
///*!
// * Set combined to lca(combined, add) and do single simplification.
// */
// bool multi_lca(InstrTypeState& combined,
//               InstrTypeState& add,
//               int pred_idx,
//               int succ_idx,
//               DecompWarnings* warnings,
//               DecompilerTypeSystem& dts) {
//  bool result = false;
//  // first, simplify add:
//  simplify_to_single(pred_idx, warnings, nullptr, add);
//
//  for (size_t idx = 0; idx < add.reg_array().size(); idx++) {
//    bool diff = false;
//    auto new_type = dts.tp_lca(combined.reg_array()[idx]->get_single_tp_type(),
//                               add.reg_array()[idx]->get_single_tp_type(), &diff);
//    if (diff) {
//      result = true;
//      combined.reg_array()[idx] = make_typespec_parent_regstate(new_type);
//    }
//  }
//}
//
//
//
//}  // namespace
//
// bool run_multi_type_analysis(const TypeSpec& my_type, DecompilerTypeSystem& dts, Function& func)
// {
//  // STEP 0 - set decompiler type system settings for this function. these should be cleaned up
//  // eventually...
//  if (func.guessed_name.kind == FunctionName::FunctionKind::METHOD) {
//    dts.type_prop_settings.current_method_type = func.guessed_name.type_name;
//  }
//
//  // set up none-returning function junk.
//  if (my_type.last_arg() == TypeSpec("none")) {
//    auto as_end = dynamic_cast<FunctionEndOp*>(func.ir2.atomic_ops->ops.back().get());
//    assert(as_end);
//    as_end->mark_function_as_no_return_value();
//  }
//
//  std::vector<InstrTypeState> block_init_types, op_types;
//  block_init_types.resize(func.basic_blocks.size());
//  op_types.resize(func.ir2.atomic_ops->ops.size());
//  auto& aop = func.ir2.atomic_ops;
//
//  // STEP 1 - topological sort the blocks. This gives us an order where we:
//  // - never visit unreachable blocks (we can't type propagate these)
//  // - always visit at least one predecessor of a block before that block
//  auto order = func.bb_topo_sort();
//  assert(!order.vist_order.empty());
//  assert(order.vist_order.front() == 0);
//
//  // STEP 2 - initialize type state for the first block to the function argument types.
//  auto uninitialized = make_cow<RegisterTypeState>(PossibleType(TP_Type::make_uninitialized()));
//  // TODO: behavior types.
//  block_init_types.at(0) =
//      construct_initial_typestate(my_type, TypeSpec("process"), func.ir2.env, uninitialized);
//
//  // STEP 3 - propagate types until the result stops changing
//  bool run_again = true;
//  while (run_again) {
//    run_again = false;
//    // do each block in the topological sort order:
//    for (auto block_id : order.vist_order) {
//      auto& block = func.basic_blocks.at(block_id);
//      // pointer to the types (no user casts) before the op.
//      auto* preceding_types = &block_init_types.at(block_id);
//
//      // ops in block, in order
//      for (int op_id = aop->block_id_to_first_atomic_op.at(block_id);
//           op_id < aop->block_id_to_end_atomic_op.at(block_id); op_id++) {
//        auto& op = aop->ops.at(op_id);
//        // look for hints:
//        const std::vector<RegisterTypeCast>* user_casts = nullptr;
//        const std::unordered_map<int, StackTypeCast>* stack_casts = nullptr;
//        const auto& cast_it = func.ir2.env.casts().find(op_id);
//        if (cast_it != func.ir2.env.casts().end()) {
//          user_casts = &cast_it->second;
//        }
//
//        if (!func.ir2.env.stack_casts().empty()) {
//          stack_casts = &func.ir2.env.stack_casts();
//        }
//
//        try {
//          std::vector<std::pair<Register, CopyOnWrite<RegisterTypeState>>> reg_updates;
//          std::vector<std::pair<int, CopyOnWrite<RegisterTypeState>>> stack_updates;
//          if (stack_casts || user_casts) {
//            auto casted =
//                get_input_types_with_user_casts(user_casts, stack_casts, *preceding_types, dts);
//            op->multi_types(casted, preceding_types, &reg_updates, &stack_updates);
//          } else {
//            op->multi_types(*preceding_types, preceding_types, &reg_updates, &stack_updates);
//          }
//          auto& dest = op_types.at(op_id);
//          dest = *preceding_types;
//          for (auto& update : reg_updates) {
//            dest.get(update.first) = update.second;
//          }
//          for (auto& update : stack_updates) {
//            *dest.get_stack_slot(update.first) = update.second;
//          }
//        } catch (std::runtime_error& e) {
//          lg::warn("Function {} failed type prop at op {}: {}", func.guessed_name.to_string(),
//                   op_id, e.what());
//          func.warnings.type_prop_warning("{}", e.what());
//          // func.ir2.env.set_types(block_init_types, op_types, *func.ir2.atomic_ops, my_type);
//          return false;
//        }
//
//        // for the next op...
//        preceding_types = &op_types.at(op_id);
//      }
//
//      // propagate the types: for each possible succ
//      for (auto succ_block_id : {block.succ_ft, block.succ_branch}) {
//        if (succ_block_id != -1) {
//          // set types to LCA (current, new)
//          if (dts.tp_lca(&block_init_types.at(succ_block_id), *preceding_types)) {
//            // if something changed, run again!
//            run_again = true;
//          }
//        }
//      }
//    }
//  }
//
//  auto last_type = op_types.back().get(Register(Reg::GPR, Reg::V0)).typespec();
//  if (last_type != my_type.last_arg()) {
//    func.warnings.info("Return type mismatch {} vs {}.", last_type.print(),
//                       my_type.last_arg().print());
//  }
//
//  // and apply final casts:
//  for (auto block_id : order.vist_order) {
//    for (int op_id = aop->block_id_to_first_atomic_op.at(block_id);
//         op_id < aop->block_id_to_end_atomic_op.at(block_id); op_id++) {
//      if (op_id == aop->block_id_to_first_atomic_op.at(block_id)) {
//        try_modify_input_types_for_casts(op_id, func.ir2.env.casts(), func.ir2.env.stack_casts(),
//                                         &block_init_types.at(block_id), nullptr, dts);
//      } else {
//        try_modify_input_types_for_casts(op_id, func.ir2.env.casts(), func.ir2.env.stack_casts(),
//                                         &op_types.at(op_id - 1), nullptr, dts);
//      }
//    }
//  }
//
//  // figure out the types of stack spill variables:
//  auto& env = func.ir2.env;
//  bool changed;
//  for (auto& type_info : op_types) {
//    for (auto& spill : type_info.spill_slots) {
//      auto& slot_info = env.stack_slot_entries[spill.first];
//      slot_info.tp_type =
//          dts.tp_lca(env.stack_slot_entries[spill.first].tp_type, spill.second, &changed);
//      slot_info.offset = spill.first;
//    }
//  }
//
//  for (auto& type_info : block_init_types) {
//    for (auto& spill : type_info.spill_slots) {
//      auto& slot_info = env.stack_slot_entries[spill.first];
//      slot_info.tp_type =
//          dts.tp_lca(env.stack_slot_entries[spill.first].tp_type, spill.second, &changed);
//      slot_info.offset = spill.first;
//    }
//  }
//
//  // convert to typespec
//  for (auto& info : env.stack_slot_entries) {
//    info.second.typespec = info.second.tp_type.typespec();
//    //     debug
//    // fmt::print("STACK {} : {} ({})\n", info.first, info.second.typespec.print(),
//    //         info.second.tp_type.print());
//  }
//
//  func.ir2.env.set_types(block_init_types, op_types, *func.ir2.atomic_ops, my_type);
//
//  return true;
//}

}  // namespace decompiler