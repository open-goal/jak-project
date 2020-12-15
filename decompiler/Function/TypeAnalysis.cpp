#include "TypeAnalysis.h"
#include "decompiler/IR/IR.h"
#include "third-party/fmt/core.h"
#include "decompiler/config.h"

namespace {
TypeState construct_initial_typestate(const TypeSpec& f_ts) {
  TypeState result;
  int goal_args[] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3, Reg::T0, Reg::T1, Reg::T2, Reg::T3};
  assert(f_ts.base_type() == "function");
  assert(f_ts.arg_count() >= 1);
  assert(f_ts.arg_count() <= 8 + 1);  // 8 args + 1 return.
  for (int i = 0; i < int(f_ts.arg_count()) - 1; i++) {
    auto reg_id = goal_args[i];
    auto reg_type = f_ts.get_arg(i);
    result.gpr_types[reg_id] = TP_Type::make_from_typespec(reg_type);
  }
  return result;
}

void apply_hints(const std::vector<TypeHint>& hints, TypeState* state, DecompilerTypeSystem& dts) {
  for (auto& hint : hints) {
    try {
      state->get(hint.reg) = TP_Type::make_from_typespec(dts.parse_type_spec(hint.type_name));
    } catch (std::exception& e) {
      printf("failed to parse hint: %s\n", e.what());
      assert(false);
    }
  }
}

void try_apply_hints(int idx,
                     const std::unordered_map<int, std::vector<TypeHint>>& hints,
                     TypeState* state,
                     DecompilerTypeSystem& dts) {
  auto kv = hints.find(idx);
  if (kv != hints.end()) {
    apply_hints(kv->second, state, dts);
  }
}
}  // namespace

bool Function::run_type_analysis(const TypeSpec& my_type,
                                 DecompilerTypeSystem& dts,
                                 LinkedObjectFile& file,
                                 const std::unordered_map<int, std::vector<TypeHint>>& hints) {
  // STEP 0 - setup settings
  dts.type_prop_settings.reset();
  if (get_config().pair_functions_by_name.find(guessed_name.to_string()) !=
      get_config().pair_functions_by_name.end()) {
    dts.type_prop_settings.allow_pair = true;
  }

  if (guessed_name.kind == FunctionName::FunctionKind::METHOD) {
    dts.type_prop_settings.current_method_type = guessed_name.type_name;
  }

  // STEP 1 - get the topo sort.
  auto order = bb_topo_sort();
  //  fmt::print("blocks: {}\n  ", basic_blocks.size());
  //  for (auto x : order.vist_order) {
  //    fmt::print("{} ", x);
  //  }
  //  fmt::print("\n");

  // STEP 2 - establish visit order
  assert(!order.vist_order.empty());
  assert(order.vist_order.front() == 0);

  // STEP 3 - initialize type state.
  basic_blocks.at(0).init_types = construct_initial_typestate(my_type);
  // and add hints:
  try_apply_hints(0, hints, &basic_blocks.at(0).init_types, dts);

  // STEP 2 - loop while types are changing
  bool run_again = true;
  while (run_again) {
    run_again = false;
    // each block in order now.
    for (auto block_id : order.vist_order) {
      auto& block = basic_blocks.at(block_id);
      TypeState* init_types = &block.init_types;
      for (int op_id = block.start_basic_op; op_id < block.end_basic_op; op_id++) {
        auto& op = basic_ops.at(op_id);

        // apply type hints only if we are not the first op.
        if (op_id != block.start_basic_op) {
          try_apply_hints(op_id, hints, init_types, dts);
        }

        // while the implementation of propagate_types is in progress, it may throw
        // for unimplemented cases.  Eventually this try/catch should be removed.
        try {
          op->propagate_types(*init_types, file, dts);
        } catch (std::runtime_error& e) {
          fmt::print("Type prop fail on {}: {}\n", guessed_name.to_string(), e.what());
          warnings += ";; Type prop attempted and failed.\n";
          return false;
        }

        // todo, set run again??

        // for the next op...
        init_types = &op->end_types;
      }

      // propagate the types: for each possible succ
      for (auto succ_block_id : {block.succ_ft, block.succ_branch}) {
        if (succ_block_id != -1) {
          auto& succ_block = basic_blocks.at(succ_block_id);
          // apply hint
          try_apply_hints(succ_block.start_basic_op, hints, init_types, dts);

          // set types to LCA (current, new)
          if (dts.tp_lca(&succ_block.init_types, *init_types)) {
            // if something changed, run again!
            run_again = true;
          }
        }
      }
    }
  }

  auto last_op = basic_ops.back();
  auto last_type = last_op->end_types.get(Register(Reg::GPR, Reg::V0)).typespec();
  if (last_type != my_type.last_arg()) {
    warnings += fmt::format(";; return type mismatch {} vs {}.  ", last_type.print(),
                            my_type.last_arg().print());
  }

  return true;
}