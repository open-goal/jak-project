/*!
 * @file ObjectFileDB_IR2.cpp
 * This runs the IR2 analysis passes.
 */

#include "common/link_types.h"
#include "ObjectFileDB.h"
#include "common/log/log.h"
#include "common/util/Timer.h"
#include "common/util/FileUtil.h"
#include "decompiler/Function/TypeInspector.h"
#include "decompiler/analysis/type_analysis.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/analysis/insert_lets.h"
#include "decompiler/analysis/label_types.h"
#include "decompiler/analysis/find_defstates.h"
#include "decompiler/analysis/variable_naming.h"
#include "decompiler/analysis/cfg_builder.h"
#include "decompiler/analysis/final_output.h"
#include "decompiler/analysis/expression_build.h"
#include "decompiler/analysis/inline_asm_rewrite.h"
#include "decompiler/analysis/stack_spill.h"
#include "decompiler/analysis/static_refs.h"
#include "decompiler/analysis/symbol_def_map.h"
#include "decompiler/analysis/find_skelgroups.h"
#include "decompiler/analysis/find_defpartgroup.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/analysis/mips2c.h"

namespace decompiler {

/*!
 * Main IR2 analysis pass.
 * At this point, we assume that the files are loaded and we've run find_code to locate all
 * functions, but nothing else.
 */
void ObjectFileDB::analyze_functions_ir2(
    const std::string& output_dir,
    const Config& config,
    const std::unordered_set<std::string>& skip_functions,
    const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states) {
  // First, do basic analysis on the top level:
  lg::info("Using IR2 analysis...");

  lg::info("Processing top-level functions...");
  ir2_top_level_pass(config);

  int total_file_count = 0;
  for (auto& f : obj_files_by_name) {
    total_file_count += f.second.size();
  }
  int file_idx = 1;
  for_each_obj([&](ObjectFileData& data) {
    Timer file_timer;
    fmt::print("[{:3d}/{}]------ {}\n", file_idx++, total_file_count, data.to_unique_name());
    ir2_do_segment_analysis_phase1(TOP_LEVEL_SEGMENT, config, data);
    ir2_do_segment_analysis_phase1(DEBUG_SEGMENT, config, data);
    ir2_do_segment_analysis_phase1(MAIN_SEGMENT, config, data);
    ir2_setup_labels(config, data);
    ir2_do_segment_analysis_phase2(TOP_LEVEL_SEGMENT, config, data);
    try {
      if (data.linked_data.functions_by_seg.size() == 3) {
        run_defpartgroup(data.linked_data.functions_by_seg.at(TOP_LEVEL_SEGMENT).front());
      }
    } catch (const std::exception& e) {
      lg::error("Failed to find defpartgroups: {}", e.what());
    }
    try {
      if (data.linked_data.functions_by_seg.size() == 3) {
        run_defstate(data.linked_data.functions_by_seg.at(TOP_LEVEL_SEGMENT).front(), skip_states);
      }
    } catch (const std::exception& e) {
      lg::error("Failed to find defstates: {}", e.what());
    }
    try {
      if (data.linked_data.functions_by_seg.size() == 3) {
        run_defskelgroups(data.linked_data.functions_by_seg.at(TOP_LEVEL_SEGMENT).front());
      }
    } catch (const std::exception& e) {
      lg::error("Failed to find defskelgroups: {}", e.what());
    }
    ir2_do_segment_analysis_phase2(DEBUG_SEGMENT, config, data);
    ir2_do_segment_analysis_phase2(MAIN_SEGMENT, config, data);

    ir2_insert_anonymous_functions(DEBUG_SEGMENT, data);
    ir2_insert_anonymous_functions(MAIN_SEGMENT, data);
    ir2_insert_anonymous_functions(TOP_LEVEL_SEGMENT, data);

    ir2_run_mips2c(config, data);

    ir2_symbol_definition_map(data);

    if (!output_dir.empty()) {
      ir2_write_results(output_dir, config, data);
    } else {
      if (!skip_functions.empty()) {
        data.output_with_skips = ir2_final_out(data, skip_functions);
      }
      data.full_output = ir2_final_out(data);
    }

    for_each_function_def_order_in_obj(data, [&](Function& f, int) { f.ir2 = {}; });

    fmt::print("Done in {:.2f}ms\n", file_timer.getMs());
  });

  if (config.generate_symbol_definition_map) {
    lg::info("Generating symbol definition map...");
    map_builder.build_map();
    std::string result = map_builder.convert_to_json();
    auto file_name = file_util::combine_path(output_dir, "symbol_map.json");
    file_util::write_text_file(file_name, result);
  }
}

void ObjectFileDB::ir2_do_segment_analysis_phase1(int seg,
                                                  const Config& config,
                                                  ObjectFileData& data) {
  ir2_basic_block_pass(seg, config, data);
  ir2_stack_spill_slot_pass(seg, data);
  ir2_atomic_op_pass(seg, config, data);
}

void ObjectFileDB::ir2_do_segment_analysis_phase2(int seg,
                                                  const Config& config,
                                                  ObjectFileData& data) {
  ir2_type_analysis_pass(seg, config, data);
  ir2_register_usage_pass(seg, data);
  ir2_variable_pass(seg, data);
  ir2_cfg_build_pass(seg, data);

  ir2_build_expressions(seg, config, data);
  ir2_rewrite_inline_asm_instructions(seg, data);

  ir2_insert_lets(seg, data);
}

void ObjectFileDB::ir2_setup_labels(const Config& config, ObjectFileData& data) {
  if (data.linked_data.segments == 3) {
    std::unordered_map<std::string, LabelConfigInfo> config_labels;
    auto config_it = config.label_types.find(data.to_unique_name());
    if (config_it != config.label_types.end()) {
      config_labels = config_it->second;
    }
    try {
      data.linked_data.label_db =
          std::make_unique<LabelDB>(config_labels, data.linked_data.labels, dts);
      analyze_labels(data.linked_data.label_db.get(), &data.linked_data);
    } catch (const std::exception& e) {
      lg::die("Error parsing labels for {}: {}\n", data.to_unique_name(), e.what());
    }
  }
}

void ObjectFileDB::ir2_run_mips2c(const Config& config, ObjectFileData& data) {
  for_each_function_def_order_in_obj(data, [&](Function& func, int) {
    if (config.hacks.mips2c_functions_by_name.count(func.name())) {
      lg::info("MIPS2C on {}", func.name());
      run_mips2c(&func);
    }

    auto it = config.hacks.mips2c_jump_table_functions.find(func.name());
    if (it != config.hacks.mips2c_jump_table_functions.end()) {
      run_mips2c_jump_table(&func, it->second);
    }
  });
}

/*!
 * Analyze the top level function of each object.
 * - Find global function definitions
 * - Find type definitions
 * - Find method definitions
 * - Warn for non-unique function names.
 */
void ObjectFileDB::ir2_top_level_pass(const Config& config) {
  Timer timer;
  int total_functions = 0;
  int total_named_global_functions = 0;
  int total_methods = 0;
  int total_top_levels = 0;
  int total_unknowns = 0;

  for_each_obj([&](ObjectFileData& data) {
    if (data.linked_data.segments == 3) {
      // the top level segment should have a single function
      assert(data.linked_data.functions_by_seg.at(2).size() == 1);

      auto& func = data.linked_data.functions_by_seg.at(2).front();
      assert(func.guessed_name.empty());
      func.guessed_name.set_as_top_level(data.to_unique_name());
      func.find_global_function_defs(data.linked_data, dts);
      func.find_type_defs(data.linked_data, dts);
      func.find_method_defs(data.linked_data, dts);
    }
  });

  // check for function uniqueness.
  std::unordered_set<std::string> unique_names;
  std::unordered_map<std::string, std::unordered_set<std::string>> duplicated_functions;

  int uid = 1;
  for_each_obj([&](ObjectFileData& data) {
    int func_in_obj = 0;
    for (int segment_id = 0; segment_id < int(data.linked_data.segments); segment_id++) {
      for (auto& func : data.linked_data.functions_by_seg.at(segment_id)) {
        func.guessed_name.unique_id = uid++;
        func.guessed_name.id_in_object = func_in_obj++;
        func.guessed_name.object_name = data.to_unique_name();
        auto name = func.name();

        switch (func.guessed_name.kind) {
          case FunctionName::FunctionKind::METHOD:
            total_methods++;
            break;
          case FunctionName::FunctionKind::GLOBAL:
            total_named_global_functions++;
            break;
          case FunctionName::FunctionKind::TOP_LEVEL_INIT:
            total_top_levels++;
            break;
          case FunctionName::FunctionKind::UNIDENTIFIED:
            total_unknowns++;
            break;
          default:
            assert(false);
        }
        total_functions++;

        if (unique_names.find(name) != unique_names.end()) {
          duplicated_functions[name].insert(data.to_unique_name());
        }

        unique_names.insert(name);

        TypeSpec ts;
        if (lookup_function_type(func.guessed_name, data.to_unique_name(), config, &ts)) {
          func.type = ts;
        } else {
          func.type = TypeSpec("function");
        }

        if (config.hacks.asm_functions_by_name.find(name) !=
            config.hacks.asm_functions_by_name.end()) {
          func.warnings.info("Flagged as asm by config");
          func.suspected_asm = true;
        }

        if (config.hacks.mips2c_functions_by_name.find(name) !=
            config.hacks.mips2c_functions_by_name.end()) {
          func.warnings.info("Flagged as mips2c by config");
          func.suspected_asm = true;
        }
      }
    }
  });

  // we remember duplicates like this so we can warn on all occurances of the duplicate name
  for_each_function([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;
    auto name = func.name();

    if (duplicated_functions.find(name) != duplicated_functions.end()) {
      duplicated_functions[name].insert(data.to_unique_name());
      func.warnings.info("this function exists in multiple non-identical object files");
    }
  });

  lg::info("Found a total of {} functions in {:.2f} ms", total_functions, timer.getMs());
  lg::info("{:4d} unknown {:.2f}%", total_unknowns, 100.f * total_unknowns / total_functions);
  lg::info("{:4d} global  {:.2f}%", total_named_global_functions,
           100.f * total_named_global_functions / total_functions);
  lg::info("{:4d} methods {:.2f}%", total_methods, 100.f * total_methods / total_functions);
  lg::info("{:4d} logins  {:.2f}%\n", total_top_levels, 100.f * total_top_levels / total_functions);
}

/*!
 * Initial Function Analysis Pass to build the control flow graph.
 * - Find basic blocks
 * - Analyze prologue and epilogue
 * - Build control flow graph
 */
void ObjectFileDB::ir2_basic_block_pass(int seg, const Config& config, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    func.ir2.env.file = &data.linked_data;
    func.ir2.env.dts = &dts;
    func.ir2.env.func = &func;

    // first, find basic blocks.
    auto blocks = find_blocks_in_function(data.linked_data, seg, func);
    func.basic_blocks = blocks;

    if (!func.suspected_asm) {
      // find the prologue/epilogue so they can be excluded from basic blocks.
      func.analyze_prologue(data.linked_data);
    } else {
      // manually exclude the type tag from the basic block.
      assert(func.basic_blocks.front().start_word == 0);
      assert(func.basic_blocks.front().end_word >= 1);
      func.basic_blocks.front().start_word = 1;
    }

    if (!func.suspected_asm) {
      // run analysis

      // build a control flow graph, just looking at branch instructions.
      CondWithElseLengthHack hack;
      auto lookup = config.hacks.cond_with_else_len_by_func_name.find(func.name());
      if (lookup != config.hacks.cond_with_else_len_by_func_name.end()) {
        hack = lookup->second;
      }

      std::unordered_set<int> asm_br_blocks;
      auto asm_lookup = config.hacks.blocks_ending_in_asm_branch_by_func_name.find(func.name());
      if (asm_lookup != config.hacks.blocks_ending_in_asm_branch_by_func_name.end()) {
        asm_br_blocks = asm_lookup->second;
      }

      func.cfg = build_cfg(data.linked_data, seg, func, hack, asm_br_blocks);
      if (!func.cfg->is_fully_resolved()) {
        lg::warn("Function {} from {} failed to build control flow graph!", func.name(),
                 data.to_unique_name());
      } else {
        func.cfg_ok = true;
      }
    }

    if (func.suspected_asm) {
      func.warnings.info("Assembly Function");
    }
  });
}

void ObjectFileDB::ir2_stack_spill_slot_pass(int seg, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    if (!func.cfg_ok) {
      return;
    }
    try {
      auto spill_map = build_spill_map(func.instructions, {func.prologue_end, func.epilogue_start});
      func.ir2.env.set_stack_spills(spill_map);
    } catch (std::exception& e) {
      func.warnings.general_warning("stack spill failed: {}", e.what());
    }
  });
}

/*!
 * Conversion of MIPS instructions into AtomicOps. The AtomicOps represent what we
 * think are IR of the original GOAL compiler.
 */
void ObjectFileDB::ir2_atomic_op_pass(int seg, const Config& config, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    if (!func.cfg_ok) {
      return;
    }
    if (!func.suspected_asm) {
      func.ir2.atomic_ops_attempted = true;
      try {
        bool inline_asm = config.hacks.hint_inline_assembly_functions.find(func.name()) !=
                          config.hacks.hint_inline_assembly_functions.end();

        std::unordered_set<int> blocks_ending_in_asm_branch;
        auto asm_branch_it =
            config.hacks.blocks_ending_in_asm_branch_by_func_name.find(func.name());

        if (asm_branch_it != config.hacks.blocks_ending_in_asm_branch_by_func_name.end()) {
          blocks_ending_in_asm_branch = asm_branch_it->second;
        }

        auto ops = convert_function_to_atomic_ops(func, data.linked_data.labels, func.warnings,
                                                  inline_asm, blocks_ending_in_asm_branch);
        func.ir2.atomic_ops = std::make_shared<FunctionAtomicOps>(std::move(ops));
        func.ir2.atomic_ops_succeeded = true;
        func.ir2.env.set_end_var(func.ir2.atomic_ops->end_op().return_var());
      } catch (std::exception& e) {
        lg::warn("Function {} from {} could not be converted to atomic ops: {}", func.name(),
                 data.to_unique_name(), e.what());
        func.warnings.general_warning("Failed to convert to atomic ops: {}", e.what());
      }
    }
  });
}

void ObjectFileDB::ir2_symbol_definition_map(ObjectFileData& data) {
  map_builder.add_object(data);
}

template <typename Key, typename Value>
Value try_lookup(const std::unordered_map<Key, Value>& map, const Key& key) {
  auto lookup = map.find(key);
  if (lookup == map.end()) {
    return Value();
  } else {
    return lookup->second;
  }
}

/*!
 * Analyze registers and determine the type in each register at each instruction.
 * - Figure out the type of each function, from configs.
 * - Propagate types.
 * - NOTE: this will update register info usage more accurately for functions.
 */
void ObjectFileDB::ir2_type_analysis_pass(int seg, const Config& config, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    if (!func.suspected_asm) {
      TypeSpec ts;
      if (lookup_function_type(func.guessed_name, data.to_unique_name(), config, &ts) &&
          func.ir2.atomic_ops_succeeded) {
        func.type = ts;
        // try type analysis here.
        auto func_name = func.name();
        auto register_casts =
            try_lookup(config.register_type_casts_by_function_by_atomic_op_idx, func_name);
        func.ir2.env.set_type_casts(register_casts);
        auto stack_casts =
            try_lookup(config.stack_type_casts_by_function_by_stack_offset, func_name);
        func.ir2.env.set_stack_casts(stack_casts);
        if (config.hacks.pair_functions_by_name.find(func_name) !=
            config.hacks.pair_functions_by_name.end()) {
          func.ir2.env.set_sloppy_pair_typing();
        }

        if (config.hacks.reject_cond_to_value.find(func_name) !=
            config.hacks.reject_cond_to_value.end()) {
          func.ir2.env.aggressively_reject_cond_to_value_rewrite = true;
        }
        func.ir2.env.set_stack_structure_hints(
            try_lookup(config.stack_structure_hints_by_function, func_name));
        if (run_type_analysis_ir2(ts, dts, func)) {
          func.ir2.env.types_succeeded = true;
        } else {
          func.warnings.type_prop_warning("Type analysis failed");
        }
      } else {
        lg::warn("Function {} didn't know its type", func.name());
        func.warnings.type_prop_warning("Function {} has unknown type", func.name());
      }
    }
  });
}

void ObjectFileDB::ir2_register_usage_pass(int seg, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    if (!func.suspected_asm && func.ir2.atomic_ops_succeeded) {
      func.ir2.env.set_reg_use(analyze_ir2_register_usage(func));

      auto block_0_start = func.ir2.env.reg_use().block.at(0).input;
      std::vector<Register> dep_regs;
      for (auto x : block_0_start) {
        dep_regs.push_back(x);
      }

      if (!dep_regs.empty()) {
        std::sort(dep_regs.begin(), dep_regs.end(),
                  [](const Register& a, const Register& b) { return a.reg_id() < b.reg_id(); });

        int end_valid_argument = Register(Reg::GPR, Reg::T3).reg_id() + 1;
        if (func.type.arg_count() > 0) {
          // end_valid_argument = Register::get_arg_reg(func.type.arg_count() - 1).reg_id();
          end_valid_argument = Register(Reg::GPR, Reg::A0).reg_id() + func.type.arg_count() - 1;
        }

        for (auto& x : dep_regs) {
          if ((x.get_kind() == Reg::VF && x.get_vf() != 0) || x.get_kind() == Reg::SPECIAL) {
            lg::error("Bad vf dependency on {} in {}", x.to_charp(), func.name());
            func.warnings.bad_vf_dependency("{}", x.to_string());
            continue;
          }

          if (x == Register(Reg::GPR, Reg::S6) || x == Register(Reg::GPR, Reg::S7) ||
              x == Register(Reg::GPR, Reg::SP) || x == Register(Reg::VF, 0)) {
            continue;
          }

          if (x.reg_id() < end_valid_argument) {
            continue;
          }

          lg::error("Bad register dependency on {} in {}", x.to_charp(), func.name());
          func.warnings.general_warning("Function may read a register that is not set: {}",
                                        x.to_string());
        }
      }
    }
  });
}

void ObjectFileDB::ir2_variable_pass(int seg, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    (void)data;
    if (!func.suspected_asm && func.ir2.atomic_ops_succeeded && func.ir2.env.has_type_analysis()) {
      try {
        auto result =
            run_variable_renaming(func, func.ir2.env.reg_use(), *func.ir2.atomic_ops, dts);
        if (result.has_value()) {
          func.ir2.env.set_local_vars(*result);
        }
      } catch (const std::exception& e) {
        lg::warn("variable pass failed on {}: {}", func.name(), e.what());
      }
    }
  });
}

void ObjectFileDB::ir2_cfg_build_pass(int seg, ObjectFileData& data) {
  Timer timer;
  int total = 0;
  int attempted = 0;
  int successful = 0;
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    (void)data;
    total++;
    if (!func.suspected_asm && func.ir2.atomic_ops_succeeded && func.cfg->is_fully_resolved()) {
      attempted++;
      try {
        build_initial_forms(func);
      } catch (std::exception& e) {
        func.warnings.general_warning("Failed to structure: {}", e.what());
        func.ir2.top_form = nullptr;
      }
    }

    if (func.ir2.top_form) {
      successful++;
    }
  });
}

// void ObjectFileDB::ir2_store_current_forms(int seg) {
//  Timer timer;
//  int total = 0;
//
//  for_each_function_in_seg(seg, [&](Function& func, ObjectFileData& data) {
//    (void)data;
//
//    if (func.ir2.top_form) {
//      total++;
//      func.ir2.debug_form_string =
//          pretty_print::to_string(func.ir2.top_form->to_form(func.ir2.env));
//    }
//  });
//
//  lg::info("Stored debug forms for {} functions in {:.2f} ms\n", total, timer.getMs());
//}
//
void ObjectFileDB::ir2_build_expressions(int seg, const Config& config, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    (void)data;
    if (func.ir2.top_form && func.ir2.env.has_type_analysis() && func.ir2.env.has_local_vars() &&
        func.ir2.env.types_succeeded) {
      auto name = func.name();
      auto arg_config = config.function_arg_names.find(name);
      auto var_config = config.function_var_overrides.find(name);
      if (convert_to_expressions(func.ir2.top_form, *func.ir2.form_pool, func,
                                 arg_config != config.function_arg_names.end()
                                     ? arg_config->second
                                     : std::vector<std::string>{},
                                 var_config != config.function_var_overrides.end()
                                     ? var_config->second
                                     : std::unordered_map<std::string, LocalVarOverride>{},
                                 dts)) {
        func.ir2.print_debug_forms = true;
        func.ir2.expressions_succeeded = true;
      }
    }
  });
}

void ObjectFileDB::ir2_insert_lets(int seg, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    if (func.ir2.expressions_succeeded) {
      try {
        insert_lets(func, func.ir2.env, *func.ir2.form_pool, func.ir2.top_form);
      } catch (const std::exception& e) {
        func.warnings.general_warning(
            fmt::format("Error while inserting lets: {}. Make sure that the return type is not "
                        "none if something is actually returned.",
                        e.what()));
      }
    }
  });
}

void ObjectFileDB::ir2_rewrite_inline_asm_instructions(int seg, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    (void)data;
    if (func.ir2.top_form && func.ir2.env.has_type_analysis()) {
      if (rewrite_inline_asm_instructions(func.ir2.top_form, *func.ir2.form_pool, func, dts)) {
        func.ir2.print_debug_forms = true;
      }
    }
  });
}

void ObjectFileDB::ir2_insert_anonymous_functions(int seg, ObjectFileData& data) {
  for_each_function_in_seg_in_obj(seg, data, [&](Function& func) {
    (void)data;
    if (func.ir2.top_form && func.ir2.env.has_type_analysis()) {
      try {
        insert_static_refs(func.ir2.top_form, *func.ir2.form_pool, func, dts);
      } catch (std::exception& e) {
        func.warnings.general_warning("Failed static ref finding: {}\n", e.what());
        lg::error("Function {} failed static ref: {}\n", func.name(), e.what());
      }
    }
  });
}

void ObjectFileDB::ir2_write_results(const std::string& output_dir,
                                     const Config& config,
                                     ObjectFileData& obj) {
  if (obj.linked_data.has_any_functions()) {
    // todo

    auto file_text = ir2_to_file(obj, config);
    auto file_name = file_util::combine_path(output_dir, obj.to_unique_name() + "_ir2.asm");
    file_util::write_text_file(file_name, file_text);

    auto final = ir2_final_out(obj);
    auto final_name = file_util::combine_path(output_dir, obj.to_unique_name() + "_disasm.gc");
    file_util::write_text_file(final_name, final);
  }
}

std::string ObjectFileDB::ir2_to_file(ObjectFileData& data, const Config& config) {
  std::string result;

  const char* segment_names[] = {"main segment", "debug segment", "top-level segment"};
  assert(data.linked_data.segments <= 3);
  for (int seg = data.linked_data.segments; seg-- > 0;) {
    // segment header
    result += ";------------------------------------------\n;  ";
    result += segment_names[seg];
    result += "\n;------------------------------------------\n\n";

    // functions
    for (auto& func : data.linked_data.functions_by_seg.at(seg)) {
      try {
        result += ir2_function_to_string(data, func, seg);
      } catch (std::exception& e) {
        result += "Failed to write ";
        result += func.name();
        result += ": ";
        result += e.what();
        result += "\n";
      }

      if (func.ir2.top_form && func.ir2.env.has_local_vars()) {
        result += '\n';
        if (func.ir2.env.has_local_vars()) {
          if (!func.ir2.print_debug_forms) {
            result += ";; expression building failed part way through, function may be weird\n";
          }
          result += final_defun_out(func, func.ir2.env, dts);
        } else {
          result += ";; no variable information\n";
          result += pretty_print::to_string(func.ir2.top_form->to_form(func.ir2.env));
        }

        result += '\n';
      } else if (func.ir2.atomic_ops_succeeded) {
        auto& ao = func.ir2.atomic_ops;
        for (size_t i = 0; i < ao->ops.size(); i++) {
          auto& op = ao->ops.at(i);

          if (!dynamic_cast<FunctionEndOp*>(op.get())) {
            auto instr_idx = ao->atomic_op_to_instruction.at(i);

            // check for a label to print
            auto label_id = data.linked_data.get_label_at(seg, (func.start_word + instr_idx) * 4);
            if (label_id != -1) {
              result += fmt::format("(label {})\n", data.linked_data.labels.at(label_id).name);
            }
            // check for no misaligned labels in code segments.
            for (int j = 1; j < 4; j++) {
              assert(data.linked_data.get_label_at(seg, (func.start_word + instr_idx) * 4 + j) ==
                     -1);
            }

            // print assembly ops.
          }

          // print instruction
          result += fmt::format("  {}\n", op->to_string(func.ir2.env));
        }
      }

      // print if it exists, even if it's not okay.
      if (config.print_cfgs && func.cfg) {
        result += fmt::format("Control Flow Graph:\n{}\n\n",
                              pretty_print::to_string(func.cfg->to_form()));
      }

      if (false && func.ir2.print_debug_forms) {
        result += '\n';
        result += ";; DEBUG OUTPUT BELOW THIS LINE:\n";
        result += func.ir2.debug_form_string;
        result += '\n';
      }
    }

    // print data
    for (size_t i = data.linked_data.offset_of_data_zone_by_seg.at(seg);
         i < data.linked_data.words_by_seg.at(seg).size(); i++) {
      for (int j = 0; j < 4; j++) {
        auto label_id = data.linked_data.get_label_at(seg, i * 4 + j);
        if (label_id != -1) {
          result += data.linked_data.labels.at(label_id).name + ":";
          if (j != 0) {
            result += " (offset " + std::to_string(j) + ")";
          }
          result += "\n";
        }
      }

      auto& word = data.linked_data.words_by_seg[seg][i];
      data.linked_data.append_word_to_string(result, word);

      if (word.kind() == LinkedWord::TYPE_PTR && word.symbol_name() == "string") {
        result += "; " + data.linked_data.get_goal_string(seg, i) + "\n";
      }
    }
  }

  return result;
}

namespace {
void append_commented(std::string& line,
                      bool& has_comment,
                      const std::string& to_append,
                      int offset = 0) {
  // minimum length before comment appears.
  constexpr int pre_comment_length = 30;
  // if comment overflows, how much to indent the next one
  constexpr int overflow_indent = 30;

  // pad, and add comment
  if (!has_comment) {
    if (line.length() < pre_comment_length) {
      line.append(pre_comment_length - line.length(), ' ');
    }
    line += ";; ";
    line += to_append;
    has_comment = true;
  } else {
    if (std::max(int(line.length()), offset) + to_append.length() > 120) {
      line += "\n";
      line.append(overflow_indent, ' ');
      line += ";; ";
    } else {
      if (int(line.length()) < offset) {
        line.append(offset - line.length(), ' ');
      }
      line += " ";
    }
    line += to_append;
  }
}
}  // namespace

std::string ObjectFileDB::ir2_function_to_string(ObjectFileData& data, Function& func, int seg) {
  std::string result;
  result += ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
  result += "; .function " + func.name() + "\n";
  result += ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
  result += func.prologue.to_string(2) + "\n";
  if (func.warnings.has_warnings()) {
    result += ";; Warnings:\n" + func.warnings.get_warning_text(true) + "\n";
  }

  if (func.ir2.env.has_local_vars()) {
    result += func.ir2.env.print_local_var_types(func.ir2.top_form);
  }

  bool print_atomics = func.ir2.atomic_ops_succeeded;
  // print each instruction in the function.
  bool in_delay_slot = false;
  int total_instructions_printed = 0;
  int last_instr_printed = 0;

  std::string line;
  auto print_instr_start = [&](int i) {
    // check for a label to print
    auto label_id = data.linked_data.get_label_at(seg, (func.start_word + i) * 4);
    if (label_id != -1) {
      result += data.linked_data.labels.at(label_id).name + ":\n";
    }
    // check for no misaligned labels in code segments.
    for (int j = 1; j < 4; j++) {
      assert(data.linked_data.get_label_at(seg, (func.start_word + i) * 4 + j) == -1);
    }

    // print the assembly instruction
    auto& instr = func.instructions.at(i);
    line = "    " + instr.to_string(data.linked_data.labels);
  };

  auto print_instr_end = [&](int i) {
    auto& instr = func.instructions.at(i);
    result += line;
    result += "\n";

    // print delay slot gap
    if (in_delay_slot) {
      result += "\n";
      in_delay_slot = false;
    }

    // for next time...
    if (gOpcodeInfo[(int)instr.kind].has_delay_slot) {
      in_delay_slot = true;
    }
    total_instructions_printed++;
    assert(last_instr_printed + 1 == i);
    last_instr_printed = i;
  };

  // first, print the prologue. we start at word 1 because word 0 is the type tag
  for (int i = 1; i < func.basic_blocks.front().start_word; i++) {
    print_instr_start(i);
    print_instr_end(i);
  }

  // next, print each basic block
  int end_idx = func.basic_blocks.front().start_word;
  for (int block_id = 0; block_id < int(func.basic_blocks.size()); block_id++) {
    // block number
    result += "B" + std::to_string(block_id) + ":\n";
    auto& block = func.basic_blocks.at(block_id);

    const TypeState* init_types = nullptr;
    if (func.ir2.env.has_type_analysis()) {
      init_types = &func.ir2.env.get_types_at_block_entry(block_id);
    }

    for (int instr_id = block.start_word; instr_id < block.end_word; instr_id++) {
      print_instr_start(instr_id);
      bool printed_comment = false;

      // print atomic op
      int op_id = -1;
      if (print_atomics && func.instr_starts_atomic_op(instr_id)) {
        auto& op = func.get_atomic_op_at_instr(instr_id);
        op_id = func.ir2.atomic_ops->instruction_to_atomic_op.at(instr_id);
        append_commented(line, printed_comment,
                         fmt::format("[{:3d}] {}", op_id,
                                     op.to_form(data.linked_data.labels, func.ir2.env).print()));

        if (func.ir2.env.has_type_analysis()) {
          append_commented(
              line, printed_comment,
              op.reg_type_info_as_string(*init_types, func.ir2.env.get_types_after_op(op_id)), 50);
        }

        /*if (func.ir2.env.has_reg_use()) {
          std::string regs;
          for (auto r : func.ir2.env.reg_use().op.at(op_id).live_in) {
            regs += r.to_charp();
            regs += ' ';
          }
          if (!regs.empty()) {
            append_commented(line, printed_comment, "lvi: " + regs, 50);
          }
        }*/
      }
      auto& instr = func.instructions.at(instr_id);
      // print linked strings
      for (int iidx = 0; iidx < instr.n_src; iidx++) {
        if (instr.get_src(iidx).is_label()) {
          auto lab = data.linked_data.labels.at(instr.get_src(iidx).get_label());
          if (data.linked_data.is_string(lab.target_segment, lab.offset)) {
            append_commented(
                line, printed_comment,
                data.linked_data.get_goal_string(lab.target_segment, lab.offset / 4 - 1));
          }
        }
      }
      print_instr_end(instr_id);

      if (print_atomics && func.ir2.env.has_type_analysis() &&
          func.instr_starts_atomic_op(instr_id)) {
        init_types = &func.ir2.env.get_types_after_op(op_id);
      }
    }
    end_idx = block.end_word;
  }

  for (int i = end_idx; i < func.end_word - func.start_word; i++) {
    print_instr_start(i);
    print_instr_end(i);
  }

  if (func.cfg) {
    if (!func.cfg->is_fully_resolved()) {
      result += func.cfg->to_form_string();
      result += "\n";
      result += func.cfg->to_dot();
      result += "\n";
    }
  }

  if (func.mips2c_output) {
    result += *func.mips2c_output;
  }

  result += "\n";

  assert(total_instructions_printed == (func.end_word - func.start_word - 1));
  return result;
}

/*!
 * Try to look up the type of a function. Looks at the decompiler type info, the hints files,
 * and other GOAL rules.
 */
bool ObjectFileDB::lookup_function_type(const FunctionName& name,
                                        const std::string& obj_name,
                                        const Config& config,
                                        TypeSpec* result) {
  // don't return function types that are explictly flagged as bad in config.
  if (config.hacks.no_type_analysis_functions_by_name.find(name.to_string()) !=
      config.hacks.no_type_analysis_functions_by_name.end()) {
    return false;
  }

  if (name.kind == FunctionName::FunctionKind::GLOBAL) {
    // global GOAL function.
    auto kv = dts.symbol_types.find(name.function_name);
    if (kv != dts.symbol_types.end() && kv->second.arg_count() >= 1) {
      if (kv->second.base_type() != "function") {
        lg::die("Found a function named {} but the symbol has type {}", name.to_string(),
                kv->second.print());
      }
      // good, found a global function with full type information.
      *result = kv->second;
      return true;
    }
  } else if (name.kind == FunctionName::FunctionKind::METHOD) {
    MethodInfo info;

    if (dts.ts.try_lookup_method(name.type_name, name.method_id, &info)) {
      if (info.type.arg_count() >= 1) {
        if (info.type.base_type() != "function") {
          lg::die("Found a method named {} but the symbol has type {}", name.to_string(),
                  info.type.print());
        }
        // substitute the _type_ for the correct type.
        *result = info.type.substitute_for_method_call(name.type_name);
        return true;
      }
    }

  } else if (name.kind == FunctionName::FunctionKind::TOP_LEVEL_INIT) {
    *result = dts.ts.make_function_typespec({}, "none");
    return true;
  } else if (name.kind == FunctionName::FunctionKind::UNIDENTIFIED) {
    // try looking up the object
    const auto& map = config.anon_function_types_by_obj_by_id;
    auto obj_kv = map.find(obj_name);
    if (obj_kv != map.end()) {
      auto func_kv = obj_kv->second.find(name.get_anon_id());
      if (func_kv != obj_kv->second.end()) {
        *result = dts.parse_type_spec(func_kv->second);
        return true;
      }
    }
  } else if (name.kind == FunctionName::FunctionKind::NV_STATE) {
    auto sym_type = dts.symbol_types.find(name.state_name);
    if (sym_type == dts.symbol_types.end()) {
      lg::error("Could not find symbol with name {} for state. This is likely a decompiler bug.",
                name.state_name);
      return false;
    }
    *result = get_state_handler_type(name.handler_kind, sym_type->second);
    return true;
  } else if (name.kind == FunctionName::FunctionKind::V_STATE) {
    auto mi = dts.ts.lookup_method(name.type_name, name.state_name);
    *result = get_state_handler_type(name.handler_kind,
                                     mi.type.substitute_for_method_call(name.type_name));
    return true;
  } else {
    assert(false);
  }
  return false;
}

std::string ObjectFileDB::ir2_final_out(ObjectFileData& data,
                                        const std::unordered_set<std::string>& skip_functions) {
  if (data.obj_version == 3) {
    std::string result;
    result += ";;-*-Lisp-*-\n";
    result += "(in-package goal)\n\n";
    assert(data.linked_data.functions_by_seg.at(TOP_LEVEL_SEGMENT).size() == 1);
    auto top_level = data.linked_data.functions_by_seg.at(TOP_LEVEL_SEGMENT).at(0);
    result += write_from_top_level(top_level, dts, data.linked_data, skip_functions);
    result += "\n\n";
    return result;
  } else {
    return ";; not a code file.";
  }
}

}  // namespace decompiler
