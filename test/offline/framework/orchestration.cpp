#include "orchestration.h"

#include "execution.h"
#include "file_management.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/StringUtil.h"
#include "common/util/diff.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "test/offline/config/config.h"

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"
#include "third-party/fmt/ranges.h"

// TODO - this should probably go somewhere common when it's needed eventually
std::unordered_map<std::string, std::string> game_name_to_config = {
    {"jak1", "jak1_ntsc_black_label.jsonc"},
    {"jak2", "jak2_ntsc_v1.jsonc"}};

OfflineTestThreadManager g_offline_test_thread_manager;

OfflineTestDecompiler setup_decompiler(const OfflineTestWorkGroup& work,
                                       const fs::path& iso_data_path,
                                       const OfflineTestConfig& offline_config) {
  // TODO - pull out extractor logic to determine release into common and use here
  OfflineTestDecompiler dc;
  dc.config = std::make_unique<decompiler::Config>(
      decompiler::read_config_file((file_util::get_jak_project_dir() / "decompiler" / "config" /
                                    game_name_to_config[offline_config.game_name])
                                       .string()));

  // TODO - do I need to limit the `inputs.jsonc` as well, or is the decompiler smart enough
  // to lazily load the DGOs as needed based on the allowed objects?

  // modify the config
  std::unordered_set<std::string> object_files;
  for (auto& file : work.work_collection.source_files) {
    object_files.insert(file.name_in_dgo);  // todo, make this work with unique_name
  }
  auto art_group_info = find_art_files(offline_config.game_name);
  for (const auto& [file_name, info] : art_group_info) {
    // Add the names of all art files to the allowed object list
    // TODO - skip this for jak2, it's not yet emitting *-ag files! / art-elts.gc is not
    // comprehensive
    if (offline_config.game_name != "jak2") {
      object_files.insert(file_name);
    }
  }

  dc.config->allowed_objects = object_files;
  // don't try to do this because we can't write the file
  dc.config->generate_symbol_definition_map = false;

  std::vector<fs::path> dgo_paths;
  for (auto& x : offline_config.dgos) {
    dgo_paths.push_back(iso_data_path / x);
  }

  dc.db = std::make_unique<decompiler::ObjectFileDB>(dgo_paths, dc.config->obj_file_name_map_file,
                                                     std::vector<fs::path>{},
                                                     std::vector<fs::path>{}, *dc.config);

  // Set the decompiler's art group info
  // - NOTE - this omits *-vis files and tpage* files.  Is that a problem?
  // TODO - skip this for jak2, it's not yet emitting *-ag files! / art-elts.gc is not comprehensive
  if (offline_config.game_name != "jak2") {
    dc.db->dts.art_group_info = art_group_info;
  }

  std::unordered_set<std::string> db_files;
  for (auto& files_by_name : dc.db->obj_files_by_name) {
    for (auto& f : files_by_name.second) {
      db_files.insert(f.to_unique_name());
    }
  }

  if (db_files.size() != object_files.size()) {
    lg::error("DB file error: has {} entries, but expected {}", db_files.size(),
              object_files.size());
    for (auto& file : work.work_collection.source_files) {
      if (!db_files.count(file.unique_name)) {
        lg::error(
            "didn't find {}, make sure it's part of the DGO inputs and not in the banned objects "
            "list\n",
            file.unique_name);
      }
    }
    exit(1);
  }

  return dc;
}

std::vector<std::future<OfflineTestThreadResult>> distribute_work(
    const OfflineTestConfig& offline_config,
    const std::vector<OfflineTestSourceFile>& files) {
  // First, group files by their DGO so they can be partitioned
  std::unordered_map<std::string, OfflineTestWorkCollection> work_colls = {};

  for (const auto& file : files) {
    if (work_colls.count(file.containing_dgo) == 0) {
      work_colls[file.containing_dgo] = OfflineTestWorkCollection();
    }
    work_colls[file.containing_dgo].source_files.push_back(file);
  }

  // Now partition by DGO so that threads do not consume unnecessary or duplicate resources
  //
  // TODO - additionally, if we have more threads than we can actually utilize we should not
  // reserve them and dynamically adjust the used thread count
  std::vector<OfflineTestWorkGroup> work_groups = {};
  for (int i = 0; i < (int)offline_config.num_threads; i++) {
    auto new_group = OfflineTestWorkGroup();
    new_group.status = std::make_shared<OfflineTestThreadStatus>(offline_config);
    work_groups.push_back(new_group);
    g_offline_test_thread_manager.statuses.push_back(new_group.status);
  }

  g_offline_test_thread_manager.print_current_test_status(offline_config);

  // Count the total number of files.
  // We'll divide the files evenly between workers. We want to avoid the case where all workers need
  // all DGOs, so assign consecutive files (likely to belong to the same dgo) to the same worker.
  int total_files = 0;
  for (const auto& [dgo, work] : work_colls) {
    total_files += work.source_files.size();
  }
  int divisor = (total_files + work_groups.size() - 1) / work_groups.size();

  // Divide up the work
  int file_idx = 0;
  for (const auto& [dgo, work] : work_colls) {
    // source files
    for (auto& source_file : work.source_files) {
      auto& wg = work_groups.at(file_idx / divisor);
      wg.dgo_set.insert(dgo);
      wg.work_collection.source_files.push_back(source_file);
      file_idx++;
    }
  }

  // Create summary of work for pretty printing.
  for (auto& wg : work_groups) {
    wg.status->dgos = wg.dgo_set;
    wg.status->total_steps = wg.work_size() * 3;  // decomp, compare, compile
  }

  // Now we can finally create the futures
  std::vector<std::future<OfflineTestThreadResult>> threads;
  for (auto& work_group : work_groups) {
    threads.push_back(std::async(std::launch::async, [&, work_group]() mutable {
      OfflineTestThreadResult result;
      if (work_group.work_size() == 0) {
        return result;
      }
      Timer total_timer;

      Timer decompiler_timer;
      work_group.status->update_stage(OfflineTestThreadStatus::Stage::PREPARING);
      auto decompiler =
          setup_decompiler(work_group, fs::path(offline_config.iso_data_path), offline_config);
      disassemble(decompiler);

      work_group.status->update_stage(OfflineTestThreadStatus::Stage::DECOMPILING);
      decompile(decompiler, offline_config, work_group.status);

      result.time_spent_decompiling = decompiler_timer.getSeconds();

      work_group.status->update_stage(OfflineTestThreadStatus::Stage::COMPARING);
      result.compare = compare(decompiler, work_group, offline_config);
      if (!result.compare.total_pass) {
        result.exit_code = 1;
        if (offline_config.fail_on_cmp) {
          work_group.status->update_stage(OfflineTestThreadStatus::Stage::FAILED);
          return result;
        }
      }

      // TODO - if anything has failed, fail fast and skip compiling

      Timer compile_timer;
      work_group.status->update_stage(OfflineTestThreadStatus::Stage::COMPILING);
      result.compile = compile(decompiler, work_group, offline_config);
      result.time_spent_compiling = compile_timer.getSeconds();
      if (!result.compile.ok) {
        work_group.status->update_stage(OfflineTestThreadStatus::Stage::FAILED);
        result.exit_code = 1;
      } else {
        work_group.status->update_stage(OfflineTestThreadStatus::Stage::FINISHED);
      }
      result.total_time = total_timer.getSeconds();

      return result;
    }));
  }

  return threads;
}

void OfflineTestThreadStatus::update_stage(Stage new_stage) {
  stage = new_stage;
  g_offline_test_thread_manager.print_current_test_status(config);
}

void OfflineTestThreadStatus::update_curr_file(const std::string& _curr_file) {
  curr_file = _curr_file;
  g_offline_test_thread_manager.print_current_test_status(config);
}

void OfflineTestThreadStatus::complete_step() {
  curr_step++;
  g_offline_test_thread_manager.print_current_test_status(config);
}

std::tuple<fmt::color, std::string> thread_stage_to_str(OfflineTestThreadStatus::Stage stage) {
  switch (stage) {
    case OfflineTestThreadStatus::Stage::IDLE:
      return {fmt::color::gray, "IDLE"};
    case OfflineTestThreadStatus::Stage::PREPARING:
      return {fmt::color::light_gray, "PREPARING"};
    case OfflineTestThreadStatus::Stage::DECOMPILING:
      return {fmt::color::orange, "DECOMPILING"};
    case OfflineTestThreadStatus::Stage::COMPARING:
      return {fmt::color::dark_orange, "COMPARING"};
    case OfflineTestThreadStatus::Stage::COMPILING:
      return {fmt::color::cyan, "COMPILING"};
    case OfflineTestThreadStatus::Stage::FINISHED:
      return {fmt::color::light_green, "FINISHED"};
    case OfflineTestThreadStatus::Stage::FAILED:
      return {fmt::color::red, "FAILED"};
    default:
      return {fmt::color::red, "UNKNOWN"};
  }
}

std::string thread_dgos_to_str(const std::set<std::string>& dgos) {
  std::vector<std::string> ones_to_print = {};
  for (const auto& dgo : dgos) {
    ones_to_print.push_back(dgo);
    if (ones_to_print.size() >= 3) {
      break;
    }
  }
  if (ones_to_print.size() < dgos.size()) {
    return fmt::format("{}, +{} more", fmt::join(ones_to_print, ","),
                       dgos.size() - ones_to_print.size());
  }
  return fmt::format("{}", fmt::join(ones_to_print, ","));
}

std::string thread_progress_bar(u32 curr_step, u32 total_steps) {
  const u32 completion =
      std::floor(static_cast<double>(curr_step) / static_cast<double>(total_steps) * 100.0);
  const u32 completed_segments = completion / 10;
  std::string progress_bar = "";
  int added_segments = 0;
  for (int i = 0; i < (int)completed_segments; i++) {
    progress_bar += "■";
    added_segments++;
  }
  while (added_segments < 10) {
    progress_bar += "□";
    added_segments++;
  }
  return progress_bar;
}

void OfflineTestThreadManager::print_current_test_status(const OfflineTestConfig& config) {
  if (!config.pretty_print) {
    return;
  }
  // [DECOMP] ▰▰▰▰▰▰▱▱▱▱ (PRI, RUI, FOR, +3 more)
  //   [1/30] - target-turret-shot // MUTED TEXT
  std::lock_guard<std::mutex> guard(print_lock);
  fmt::print("\x1b[{}A", g_offline_test_thread_manager.statuses.size() * 2);  // move n lines up
  for (const auto& status : g_offline_test_thread_manager.statuses) {
    // first line
    const auto [color, stage_text] = thread_stage_to_str(status->stage);
    fmt::print(
        "\33[2K\r[{:>12}] {} ({})\n", fmt::styled(stage_text, fmt::fg(color)),
        fmt::styled(thread_progress_bar(status->curr_step, status->total_steps), fmt::fg(color)),
        thread_dgos_to_str(status->dgos));
    // second line
    fmt::print(fmt::fg(fmt::color::gray), "\33[2K\r{:>14} - {}\n",
               fmt::format("[{}/{}]", status->curr_step, status->total_steps), status->curr_file);
  }
}
