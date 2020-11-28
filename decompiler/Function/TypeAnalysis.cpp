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
  assert(f_ts.arg_count() <= 8);
  for (int i = 0; i < int(f_ts.arg_count()) - 1; i++) {
    auto reg_id = goal_args[i];
    auto reg_type = f_ts.get_arg(i);
    result.gpr_types[reg_id].ts = reg_type;
    result.gpr_types[reg_id].kind = TP_Type::OBJECT_OF_TYPE;
  }
  return result;
}
}  // namespace

bool Function::run_type_analysis(const TypeSpec& my_type,
                                 DecompilerTypeSystem& dts,
                                 LinkedObjectFile& file) {
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

        // while the implementation of propagate_types is in progress, it may throw
        // for unimplemented cases.  Eventually this try/catch should be removed.
        try {
          op->propagate_types(*init_types, file, dts);
        } catch (std::runtime_error& e) {
          fmt::print("Type prop fail: {}\n\n\n", e.what());
          warnings += "Type prop attempted and failed.  ";
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
  auto last_type = last_op->end_types.get(Register(Reg::GPR, Reg::V0)).as_typespec();
  if (last_type != my_type.last_arg()) {
    warnings += fmt::format("return type mismatch {} vs {}.  ", last_type.print(),
                            my_type.last_arg().print());
  }

  return true;
}