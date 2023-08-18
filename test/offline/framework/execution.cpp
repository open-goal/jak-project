#include "execution.h"

#include "common/util/string_util.h"

#include "goalc/compiler/Compiler.h"
#include "test/offline/config/config.h"

#include "third-party/fmt/ranges.h"

// TODO - i think these should be partitioned by game name instead of it being in the filename
// (and the names not being consistent)
std::unordered_map<std::string, std::string> game_name_to_all_types1 = {
    {"jak1", "jak1/all-types.gc"},
    {"jak2", "jak2/all-types.gc"}};

void disassemble(OfflineTestDecompiler& dc) {
  dc.db->process_link_data(*dc.config);
  dc.db->find_code(*dc.config);
  dc.db->process_labels();
}

void decompile(OfflineTestDecompiler& dc,
               const OfflineTestConfig& config,
               const std::shared_ptr<OfflineTestThreadStatus> status) {
  dc.db->extract_art_info();
  dc.db->ir2_top_level_pass(*dc.config);
  dc.db->analyze_functions_ir2(
      {}, *dc.config,
      [status](std::string file_name) mutable { status->update_curr_file(file_name); },
      [status]() mutable { status->complete_step(); }, config.skip_compile_functions,
      config.skip_compile_states);
}

/// @brief Removes trailing new-lines and comment lines
std::string clean_decompilation_code(const std::string& in, const bool leave_comments = false) {
  std::string out = in;
  if (!leave_comments) {
    std::vector<std::string> lines = str_util::split(in);
    // Remove all lines that are comments
    // comments are added only by us, meaning this _should_ be consistent
    std::vector<std::string>::iterator line_itr = lines.begin();
    while (line_itr != lines.end()) {
      if (line_itr->rfind(";", 0) == 0) {
        // remove comment line
        line_itr = lines.erase(line_itr);
      } else {
        // iterate
        ++line_itr;
      }
    }
    out = fmt::format("{}", fmt::join(lines, "\n"));
  }

  while (!out.empty() && out.back() == '\n') {
    out.pop_back();
  }
  return out;
}

decompiler::ObjectFileData& get_data(OfflineTestDecompiler& dc,
                                     const std::string& unique_name,
                                     const std::string& name_in_dgo) {
  auto& files = dc.db->obj_files_by_name.at(name_in_dgo);
  auto it = std::find_if(files.begin(), files.end(), [&](const decompiler::ObjectFileData& data) {
    return data.to_unique_name() == unique_name;
  });
  ASSERT(it != files.end());
  return *it;
}

OfflineTestCompareResult compare(OfflineTestDecompiler& dc,
                                 const OfflineTestWorkGroup& work_group,
                                 const OfflineTestConfig& config) {
  OfflineTestCompareResult compare_result;

  for (const auto& file : work_group.work_collection.source_files) {
    work_group.status->update_curr_file(file.name_in_dgo);
    auto& data = get_data(dc, file.unique_name, file.name_in_dgo);
    std::string result = clean_decompilation_code(data.full_output);
    std::string ref = clean_decompilation_code(file_util::read_text_file(file.path.string()));
    compare_result.total_files++;
    compare_result.total_lines += str_util::line_count(result);
    if (result != ref) {
      compare_result.failing_files.push_back({file.unique_name, str_util::diff(ref, result)});
      compare_result.total_pass = false;
      if (config.dump_mode) {
        auto failure_dir = file_util::get_jak_project_dir() / "failures";
        file_util::create_dir_if_needed(failure_dir);
        file_util::write_text_file(failure_dir / fmt::format("{}_REF.gc", file.unique_name),
                                   clean_decompilation_code(data.full_output, true));
      }
    } else {
      compare_result.ok_files++;
    }
    work_group.status->complete_step();
  }

  return compare_result;
}

OfflineTestCompileResult compile(OfflineTestDecompiler& dc,
                                 const OfflineTestWorkGroup& work_group,
                                 const OfflineTestConfig& config) {
  OfflineTestCompileResult result;
  Compiler compiler(game_name_to_version(config.game_name));

  compiler.run_front_end_on_file(
      {"decompiler", "config", game_name_to_all_types1[config.game_name]});
  compiler.run_front_end_on_file(
      {"test", "decompiler", "reference", config.game_name, "decompiler-macros.gc"});
  if (config.game_name == "jak2") {
    compiler.run_front_end_on_file({"goal_src", "jak2", "engine", "data", "art-elts.gc"});
  } else if (config.game_name == "jak1") {
    compiler.run_front_end_on_file({"goal_src", "jak1", "engine", "data", "art-elts.gc"});
  }

  int total_lines = 0;

  for (const auto& file : work_group.work_collection.source_files) {
    work_group.status->update_curr_file(file.name_in_dgo);
    if (config.skip_compile_files.count(file.name_in_dgo)) {
      lg::warn("Skipping {}", file.name_in_dgo);
      continue;
    }

    lg::info("Compiling {}...", file.unique_name);

    auto& data = get_data(dc, file.unique_name, file.name_in_dgo);

    try {
      const auto& src = data.output_with_skips;
      total_lines += str_util::line_count(src);
      compiler.run_full_compiler_on_string_no_save(src, file.name_in_dgo);
    } catch (const std::exception& e) {
      result.ok = false;
      result.failing_files.push_back({file.name_in_dgo, e.what()});
    }
    work_group.status->complete_step();
  }

  result.num_lines = total_lines;
  return result;
}
