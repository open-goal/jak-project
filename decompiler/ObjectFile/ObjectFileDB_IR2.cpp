/*!
 * @file ObjectFileDB_IR2.cpp
 * This runs the IR2 analysis passes.
 */

#include "ObjectFileDB.h"
#include "common/log/log.h"
#include "common/util/Timer.h"
#include "common/util/FileUtil.h"
#include "decompiler/Function/TypeInspector.h"

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
  lg::info("Writing results...");
  ir2_write_results(output_dir);
}

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
      }
    }
  });

  lg::info("{}/{}/{} (successful/attempted/total) functions converted to Atomic Ops in {:.2f} ms",
           successful, attempted, total_functions, timer.getMs());
  lg::info("{:.2f}% were attempted, {:.2f}% of attempted succeeded\n",
           100.f * attempted / total_functions, 100.f * successful / attempted);
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

std::string ObjectFileDB::ir2_function_to_string(ObjectFileData& data, Function& func, int seg) {
  std::string result;
  result += ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
  result += "; .function " + func.guessed_name.to_string() + "\n";
  result += ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
  result += func.prologue.to_string(2) + "\n";
  if (!func.warnings.empty()) {
    result += ";;Warnings:\n" + func.warnings + "\n";
  }

  bool print_atomics = func.ir2.atomic_ops_succeeded;
  // print each instruction in the function.
  bool in_delay_slot = false;

  for (int i = 1; i < func.end_word - func.start_word; i++) {
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
    std::string line = "    " + instr.to_string(data.linked_data.labels);

    //    printf("%d inst %s\n", print_atomics, instr.to_string(data.linked_data.labels).c_str());

    bool printed_comment = false;

    // print atomic op
    if (print_atomics && func.instr_starts_atomic_op(i)) {
      if (line.length() < 30) {
        line.append(30 - line.length(), ' ');
      }
      line +=
          " ;; " + func.get_atomic_op_at_instr(i).to_string(data.linked_data.labels, &func.ir2.env);
      printed_comment = true;
    }

    // print linked strings
    for (int iidx = 0; iidx < instr.n_src; iidx++) {
      if (instr.get_src(iidx).is_label()) {
        auto lab = data.linked_data.labels.at(instr.get_src(iidx).get_label());
        if (data.linked_data.is_string(lab.target_segment, lab.offset)) {
          if (!printed_comment) {
            line += " ;; ";
            printed_comment = true;
          }
          line += " " + data.linked_data.get_goal_string(lab.target_segment, lab.offset / 4 - 1);
        }
      }
    }
    result += line + "\n";

    // print delay slot gap
    if (in_delay_slot) {
      result += "\n";
      in_delay_slot = false;
    }

    // for next time...
    if (gOpcodeInfo[(int)instr.kind].has_delay_slot) {
      in_delay_slot = true;
    }
  }
  result += "\n";

  return result;
}

}  // namespace decompiler
