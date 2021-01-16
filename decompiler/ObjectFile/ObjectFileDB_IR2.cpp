/*!
 * @file ObjectFileDB_IR2.cpp
 * This runs the IR2 analysis passes.
 */

#include "ObjectFileDB.h"
#include "common/log/log.h"
#include "common/util/Timer.h"
#include "common/util/FileUtil.h"
#include "decompiler/Function/TypeInspector.h"
#include "decompiler/IR2/reg_usage.h"
#include "decompiler/IR2/variable_naming.h"

namespace decompiler {

/*!
 * Main IR2 analysis pass.
 * At this point, we assume that the files are loaded and we've run find_code to locate all
 * functions, but nothing else.
 */
void ObjectFileDB::analyze_functions_ir2(const std::string& output_dir) {
  lg::info("Using IR2 analysis...");
  lg::info("Processing top-level functions...");
  ir2_top_level_pass();
  lg::info("Processing basic blocks and control flow graph...");
  ir2_basic_block_pass();
  lg::info("Converting to atomic ops...");
  ir2_atomic_op_pass();
  lg::info("Running type analysis...");
  ir2_type_analysis_pass();
  lg::info("Register usage analysis...");
  ir2_register_usage_pass();
  lg::info("Variable analysis...");
  ir2_variable_pass();
  lg::info("Writing results...");
  ir2_write_results(output_dir);
}

/*!
 * Analyze the top level function of each object.
 * - Find global function definitions
 * - Find type definitions
 * - Find method definitions
 * - Warn for non-unique function names.
 */
void ObjectFileDB::ir2_top_level_pass() {
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
      func.guessed_name.set_as_top_level();
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
        auto name = func.guessed_name.to_string();

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

        if (get_config().asm_functions_by_name.find(name) !=
            get_config().asm_functions_by_name.end()) {
          func.warnings += ";; flagged as asm by config\n";
          func.suspected_asm = true;
        }
      }
    }
  });

  // we remember duplicates like this so we can warn on all occurances of the duplicate name
  for_each_function([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;
    auto name = func.guessed_name.to_string();

    if (duplicated_functions.find(name) != duplicated_functions.end()) {
      duplicated_functions[name].insert(data.to_unique_name());
      func.warnings += ";; this function exists in multiple non-identical object files\n";
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
void ObjectFileDB::ir2_basic_block_pass() {
  Timer timer;
  // Main Pass over each function...
  int total_basic_blocks = 0;
  int total_functions = 0;
  int functions_with_one_block = 0;
  int inspect_methods = 0;
  int suspected_asm = 0;
  int failed_to_build_cfg = 0;

  for_each_function_def_order([&](Function& func, int segment_id, ObjectFileData& data) {
    total_functions++;
    func.ir2.env.file = &data.linked_data;

    // first, find basic blocks.
    auto blocks = find_blocks_in_function(data.linked_data, segment_id, func);
    total_basic_blocks += blocks.size();
    if (blocks.size() == 1) {
      functions_with_one_block++;
    }
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
      func.cfg = build_cfg(data.linked_data, segment_id, func);
      if (!func.cfg->is_fully_resolved()) {
        lg::warn("Function {} from {} failed to build control flow graph!",
                 func.guessed_name.to_string(), data.to_unique_name());
        failed_to_build_cfg++;
      }

      // if we got an inspect method, inspect it.
      if (func.is_inspect_method) {
        auto result = inspect_inspect_method(func, func.method_of_type, dts, data.linked_data);
        all_type_defs += ";; " + data.to_unique_name() + "\n";
        all_type_defs += result.print_as_deftype() + "\n";
        inspect_methods++;
      }
    }

    if (func.suspected_asm) {
      func.warnings.append(";; Assembly Function\n");
      suspected_asm++;
    }
  });

  lg::info("Found {} basic blocks in {} functions in {:.2f} ms:", total_basic_blocks,
           total_functions, timer.getMs());
  lg::info(" {} functions ({:.2f}%) failed to build control flow graph", failed_to_build_cfg,
           100.f * failed_to_build_cfg / total_functions);
  lg::info(" {} functions ({:.2f}%) had exactly one basic block", functions_with_one_block,
           100.f * functions_with_one_block / total_functions);
  lg::info(" {} functions ({:.2f}%) were ignored as assembly", suspected_asm,
           100.f * suspected_asm / total_functions);
  lg::info(" {} functions ({:.2f}%) were inspect methods\n", inspect_methods,
           100.f * inspect_methods / total_functions);
}

/*!
 * Conversion of MIPS instructions into AtomicOps. The AtomicOps represent what we
 * think are IR of the original GOAL compiler.
 */
void ObjectFileDB::ir2_atomic_op_pass() {
  Timer timer;
  int total_functions = 0;
  int attempted = 0;
  int successful = 0;
  for_each_function_def_order([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;
    total_functions++;
    if (!func.suspected_asm) {
      func.ir2.atomic_ops_attempted = true;
      attempted++;
      try {
        auto ops = convert_function_to_atomic_ops(func, data.linked_data.labels);
        func.ir2.atomic_ops = std::make_shared<FunctionAtomicOps>(std::move(ops));
        func.ir2.atomic_ops_succeeded = true;
        successful++;
      } catch (std::exception& e) {
        lg::warn("Function {} from {} could not be converted to atomic ops: {}",
                 func.guessed_name.to_string(), data.to_unique_name(), e.what());
        func.warnings.append(";; Failed to convert to atomic ops\n");
      }
    }
  });

  lg::info("{}/{}/{} (successful/attempted/total) functions converted to Atomic Ops in {:.2f} ms",
           successful, attempted, total_functions, timer.getMs());
  lg::info("{:.2f}% were attempted, {:.2f}% of attempted succeeded\n",
           100.f * attempted / total_functions, 100.f * successful / attempted);
}

/*!
 * Analyze registers and determine the type in each register at each instruction.
 * - Figure out the type of each function, from configs.
 * - Propagate types.
 * - NOTE: this will update register info usage more accurately for functions.
 */
void ObjectFileDB::ir2_type_analysis_pass() {
  Timer timer;
  int total_functions = 0;
  int non_asm_functions = 0;
  int attempted_functions = 0;
  int successful_functions = 0;

  for_each_function_def_order([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;
    total_functions++;
    if (!func.suspected_asm) {
      non_asm_functions++;
      TypeSpec ts;
      if (lookup_function_type(func.guessed_name, data.to_unique_name(), &ts)) {
        attempted_functions++;
        // try type analysis here.
        auto hints = get_config().type_hints_by_function_by_idx[func.guessed_name.to_string()];
        if (func.run_type_analysis_ir2(ts, dts, data.linked_data, hints)) {
          successful_functions++;
          func.ir2.has_type_info = true;
        } else {
          func.warnings.append(";; Type analysis failed\n");
        }
      } else {
        // lg::warn("Function {} didn't know its type", func.guessed_name.to_string());
        func.warnings.append(";; Type of function is unknown\n");
      }
    }
  });

  lg::info("{}/{}/{}/{} (success/attempted/non-asm/total) in {:.2f} ms\n", successful_functions,
           attempted_functions, non_asm_functions, total_functions, timer.getMs());
}

void ObjectFileDB::ir2_register_usage_pass() {
  Timer timer;

  int total_funcs = 0, analyzed_funcs = 0;
  for_each_function_def_order([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;
    (void)data;
    total_funcs++;
    if (!func.suspected_asm && func.ir2.atomic_ops_succeeded) {
      analyzed_funcs++;
      func.ir2.reg_use = analyze_ir2_register_usage(func);
      func.ir2.has_reg_use = true;
    }
  });

  lg::info("{}/{} functions had register usage analyzed in {:.2f} ms\n", analyzed_funcs,
           total_funcs, timer.getMs());
}

void ObjectFileDB::ir2_variable_pass() {
  Timer timer;
  int attempted = 0;
  int successful = 0;
  for_each_function_def_order([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;
    (void)data;
    if (!func.suspected_asm && func.ir2.atomic_ops_succeeded) {
      try {
        attempted++;
        auto result = run_variable_renaming(func, func.ir2.reg_use, *func.ir2.atomic_ops, dts);
        if (result.has_value()) {
          successful++;
          func.ir2.env.set_local_vars(*result);
        }
      } catch (const std::exception& e) {
        lg::warn("variable pass failed on {}: {}", func.guessed_name.to_string(), e.what());
      }
    }
  });
  lg::info("{}/{} functions out of attempted passed variable pass in {:.2f} ms\n", successful,
           attempted, timer.getMs());
}

void ObjectFileDB::ir2_write_results(const std::string& output_dir) {
  Timer timer;
  lg::info("Writing IR2 results to file...");
  int total_files = 0;
  int total_bytes = 0;
  for_each_obj([&](ObjectFileData& obj) {
    if (obj.linked_data.has_any_functions()) {
      // todo
      total_files++;
      auto file_text = ir2_to_file(obj);
      total_bytes += file_text.length();
      auto file_name = file_util::combine_path(output_dir, obj.to_unique_name() + "_ir2.asm");

      file_util::write_text_file(file_name, file_text);
    }
  });
  lg::info("Wrote {} files ({:.2f} MB) in {:.2f} ms\n", total_files, total_bytes / float(1 << 20),
           timer.getMs());
}

std::string ObjectFileDB::ir2_to_file(ObjectFileData& data) {
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
      result += ir2_function_to_string(data, func, seg);
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

      if (word.kind == LinkedWord::TYPE_PTR && word.symbol_name == "string") {
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
  result += "; .function " + func.guessed_name.to_string() + "\n";
  result += ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
  result += func.prologue.to_string(2) + "\n";
  if (!func.warnings.empty()) {
    result += ";;Warnings:\n" + func.warnings + "\n";
  }

  if (func.ir2.env.has_local_vars()) {
    result += func.ir2.env.print_local_var_types();
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
                         op.to_string(data.linked_data.labels, &func.ir2.env));

        if (func.ir2.env.has_type_analysis()) {
          append_commented(
              line, printed_comment,
              op.reg_type_info_as_string(*init_types, func.ir2.env.get_types_after_op(op_id)), 50);
        }

        if (func.ir2.has_reg_use) {
          std::string regs;
          for (auto r : func.ir2.reg_use.op.at(op_id).consumes) {
            regs += r.to_charp();
            regs += ' ';
          }
          if (!regs.empty()) {
            append_commented(line, printed_comment, "cs: " + regs, 50);
          }
        }
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
                                        TypeSpec* result) {
  auto& cfg = get_config();

  // don't return function types that are explictly flagged as bad in config.
  if (cfg.no_type_analysis_functions_by_name.find(name.to_string()) !=
      cfg.no_type_analysis_functions_by_name.end()) {
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
    const auto& map = get_config().anon_function_types_by_obj_by_id;
    auto obj_kv = map.find(obj_name);
    if (obj_kv != map.end()) {
      auto func_kv = obj_kv->second.find(name.get_anon_id());
      if (func_kv != obj_kv->second.end()) {
        *result = dts.parse_type_spec(func_kv->second);
        return true;
      }
    }
  } else {
    assert(false);
  }
  return false;
}

}  // namespace decompiler
