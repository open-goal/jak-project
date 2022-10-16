#include <future>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include "common/common_types.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/diff.h"
#include "common/util/json_util.h"
#include <common/util/unicode_util.h>

#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "goalc/compiler/Compiler.h"

#include "third-party/CLI11.hpp"
#include "third-party/fmt/format.h"

// json config file data (previously was in source of offline_test_main.cpp)
struct OfflineTestConfig {
  std::vector<std::string> dgos;
  std::unordered_set<std::string> skip_compile_files;
  std::unordered_set<std::string> skip_compile_functions;
  std::unordered_map<std::string, std::unordered_set<std::string>> skip_compile_states;
};

struct DecompilerFile {
  fs::path path;
  std::string name_in_dgo;
  std::string unique_name;
  std::string reference;
};

struct DecompilerArtFile {
  std::string name_in_dgo;
  std::string unique_name;
};

struct Decompiler {
  std::unique_ptr<decompiler::ObjectFileDB> db;
  std::unique_ptr<decompiler::Config> config;
};

// TODO - this should probably go somewhere common when it's needed eventually
std::unordered_map<std::string, std::string> game_name_to_config = {
    {"jak1", "jak1_ntsc_black_label.jsonc"},
    {"jak2", "jak2_ntsc_v1.jsonc"}};

// TODO - i think these should be partitioned by game name instead of it being in the filename
// (and the names not being consistent)
std::unordered_map<std::string, std::string> game_name_to_all_types = {
    {"jak1", "all-types.gc"},
    {"jak2", "jak2/all-types.gc"}};

Decompiler setup_decompiler(const std::vector<DecompilerFile>& files,
                            const std::vector<DecompilerArtFile>& art_files,
                            const fs::path& iso_data_path,
                            const OfflineTestConfig& offline_config,
                            const std::string& game_name) {
  // TODO - pull out extractor logic to determine release into common and use here
  Decompiler dc;
  decompiler::init_opcode_info();
  dc.config = std::make_unique<decompiler::Config>(decompiler::read_config_file(
      (file_util::get_jak_project_dir() / "decompiler" / "config" / game_name_to_config[game_name])
          .string()));

  // modify the config
  std::unordered_set<std::string> object_files;
  for (auto& file : files) {
    object_files.insert(file.name_in_dgo);  // todo, make this work with unique_name
  }
  for (auto& file : art_files) {
    object_files.insert(file.unique_name);
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

  std::unordered_set<std::string> db_files;
  for (auto& files_by_name : dc.db->obj_files_by_name) {
    for (auto& f : files_by_name.second) {
      db_files.insert(f.to_unique_name());
    }
  }

  if (db_files.size() != files.size() + art_files.size()) {
    lg::error("DB file error: {} {} {}", db_files.size(), files.size(), art_files.size());
    for (auto& f : files) {
      if (!db_files.count(f.unique_name)) {
        lg::error(
            "didn't find {}, make sure it's part of the DGO inputs and not in the banned objects "
            "list\n",
            f.unique_name);
      }
    }
    for (auto& f : art_files) {
      if (!db_files.count(f.unique_name)) {
        lg::error("didn't find {}\n", f.unique_name);
      }
    }
    exit(1);
  }

  return dc;
}

void disassemble(Decompiler& dc) {
  dc.db->process_link_data(*dc.config);
  dc.db->find_code(*dc.config);
  dc.db->process_labels();
}

void decompile(Decompiler& dc, const OfflineTestConfig& config) {
  dc.db->extract_art_info();
  dc.db->ir2_top_level_pass(*dc.config);
  dc.db->analyze_functions_ir2({}, *dc.config, config.skip_compile_functions,
                               config.skip_compile_states);
}

/// @brief Removes trailing new-lines and comment lines
std::string clean_decompilation_code(const std::string& in) {
  std::vector<std::string> lines = split_string(in);
  // Remove all lines that are comments
  // comments are added only by us, meaning this _should_ be consistent
  std::vector<std::string>::iterator line_itr = lines.begin();
  while (line_itr != lines.end()) {
    if (line_itr->rfind(";", 0) == 0) {
      // remove comment line
      line_itr = lines.erase(line_itr);
    } else {
      // iterate
      line_itr++;
    }
  }

  std::string out = fmt::format("{}", fmt::join(lines, "\n"));
  while (!out.empty() && out.back() == '\n') {
    out.pop_back();
  }
  return out;
}

decompiler::ObjectFileData& get_data(Decompiler& dc,
                                     const std::string& unique_name,
                                     const std::string& name_in_dgo) {
  auto& files = dc.db->obj_files_by_name.at(name_in_dgo);
  auto it = std::find_if(files.begin(), files.end(), [&](const decompiler::ObjectFileData& data) {
    return data.to_unique_name() == unique_name;
  });
  ASSERT(it != files.end());
  return *it;
}

int line_count(const std::string& str) {
  int result = 0;
  for (auto& c : str) {
    if (c == '\n') {
      result++;
    }
  }
  return result;
}

struct CompareResult {
  std::vector<std::string> failing_files;
  int total_files = 0;
  int ok_files = 0;
  int total_lines = 0;

  bool total_pass = true;
};

CompareResult compare(Decompiler& dc, const std::vector<DecompilerFile>& refs, bool dump_mode) {
  CompareResult compare_result;

  for (const auto& file : refs) {
    auto& data = get_data(dc, file.unique_name, file.name_in_dgo);
    std::string result = clean_decompilation_code(data.full_output);
    std::string ref = clean_decompilation_code(file_util::read_text_file(file.path.string()));
    compare_result.total_files++;
    compare_result.total_lines += line_count(result);
    if (result != ref) {
      compare_result.failing_files.push_back(file.unique_name);
      compare_result.total_pass = false;
      fmt::print("Reference test failure on {}:\n", file.unique_name);
      fmt::print("{}\n", diff_strings(ref, result));

      if (dump_mode) {
        auto failure_dir = file_util::get_jak_project_dir() / "failures";
        file_util::create_dir_if_needed(failure_dir);
        file_util::write_text_file(failure_dir / fmt::format("{}_REF.gc", file.unique_name),
                                   result);
      }
    } else {
      compare_result.ok_files++;
    }
  }

  return compare_result;
}

bool compile(Decompiler& dc,
             const std::vector<DecompilerFile>& refs,
             const OfflineTestConfig& config,
             const std::string& game_name) {
  fmt::print("Setting up compiler...\n");
  Compiler compiler(game_name_to_version(game_name));

  compiler.run_front_end_on_file({"decompiler", "config", game_name_to_all_types[game_name]});
  compiler.run_front_end_on_file(
      {"test", "decompiler", "reference", game_name, "decompiler-macros.gc"});

  Timer timer;
  int total_lines = 0;
  for (const auto& file : refs) {
    if (config.skip_compile_files.count(file.name_in_dgo)) {
      fmt::print("Skipping {}\n", file.name_in_dgo);
      continue;
    }

    fmt::print("Compiling {}...\n", file.unique_name);

    auto& data = get_data(dc, file.unique_name, file.name_in_dgo);

    try {
      const auto& src = data.output_with_skips;
      total_lines += line_count(src);
      compiler.run_full_compiler_on_string_no_save(src, file.name_in_dgo);
    } catch (const std::exception& e) {
      fmt::print("Compiler exception: {}\n", e.what());
      return false;
    }
  }
  auto time = timer.getSeconds();
  fmt::print("Total Lines Compiled: {}. Lines/second: {:.1f}\n", total_lines,
             (float)total_lines / time);

  return true;
}

std::vector<DecompilerArtFile> find_art_files(const std::string& game_name,
                                              const std::vector<std::string>& dgos) {
  std::vector<DecompilerArtFile> result;

  // use the all_objs.json file to place them in the correct build order
  auto obj_json = parse_commented_json(
      file_util::read_text_file(
          (file_util::get_jak_project_dir() / "goal_src" / game_name / "build" / "all_objs.json")
              .string()),
      "all_objs.json");

  for (const auto& x : obj_json) {
    auto unique_name = x[0].get<std::string>();
    auto version = x[2].get<int>();

    std::vector<std::string> dgoList = x[3].get<std::vector<std::string>>();
    if (version == 4) {
      bool skip_this = false;

      // Check to see if we've included atleast one of the DGO/CGOs in our hardcoded list
      // If not BLOW UP
      bool dgoValidated = false;
      for (int i = 0; i < (int)dgoList.size(); i++) {
        std::string& dgo = dgoList.at(i);
        if (dgo == "NO-XGO") {
          skip_this = true;
          break;
        }
        // can either be in the DGO or CGO folder, and can either end with .CGO or .DGO
        // TODO - Jak 2 Folder structure will be different!
        if (std::find(dgos.begin(), dgos.end(), fmt::format("DGO/{}.DGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("DGO/{}.CGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("CGO/{}.DGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("CGO/{}.CGO", dgo)) != dgos.end()) {
          dgoValidated = true;
        }
      }
      if (skip_this) {
        continue;
      }
      if (!dgoValidated) {
        lg::error(
            "File [{}] is in the following DGOs [{}], and not one of these is in our list! Add "
            "it!",
            unique_name, fmt::join(dgoList, ", "));
        exit(1);
      }

      DecompilerArtFile file;
      file.unique_name = unique_name;
      file.name_in_dgo = x[1];
      result.push_back(file);
    }
  }

  return result;
}

std::vector<DecompilerFile> find_files(const std::string& game_name,
                                       const std::vector<std::string>& dgos,
                                       const std::string& single_file) {
  std::vector<DecompilerFile> result;

  auto base_dir =
      file_util::get_jak_project_dir() / "test" / "decompiler" / "reference" / game_name;
  auto ref_file_paths = file_util::find_files_recursively(base_dir, std::regex(".*_REF\\..*"));
  std::unordered_map<std::string, fs::path> ref_file_names = {};
  for (const auto& path : ref_file_paths) {
    auto ref_name = path.filename().replace_extension().string();
    ref_name.erase(ref_name.begin() + ref_name.find("_REF"), ref_name.end());
    if (single_file.empty() || ref_name == single_file) {
      ref_file_names[ref_name] = path;
    }
  }

  lg::info("Found {} reference files", ref_file_paths.size());

  // use the all_objs.json file to place them in the correct build order
  auto obj_json = parse_commented_json(
      file_util::read_text_file(
          (file_util::get_jak_project_dir() / "goal_src" / game_name / "build" / "all_objs.json")
              .string()),
      "all_objs.json");

  std::unordered_set<std::string> matched_files;
  for (auto& x : obj_json) {
    auto unique_name = x[0].get<std::string>();

    std::vector<std::string> dgoList = x[3].get<std::vector<std::string>>();
    auto it = ref_file_names.find(unique_name);
    if (it != ref_file_names.end()) {
      // Check to see if we've included atleast one of the DGO/CGOs in our hardcoded list
      // If not BLOW UP
      bool dgoValidated = false;
      for (int i = 0; i < (int)dgoList.size(); i++) {
        std::string& dgo = dgoList.at(i);
        // can either be in the DGO or CGO folder, and can either end with .CGO or .DGO
        // TODO - Jak 2 Folder structure will be different!
        if (std::find(dgos.begin(), dgos.end(), fmt::format("DGO/{}.DGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("DGO/{}.CGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("CGO/{}.DGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("CGO/{}.CGO", dgo)) != dgos.end()) {
          dgoValidated = true;
        }
      }
      if (!dgoValidated) {
        lg::error(
            "File [{}] is in the following DGOs [{}], and not one of these is in our list! Add "
            "it!",
            unique_name, fmt::join(dgoList, ", "));
        exit(1);
      }

      DecompilerFile file;
      file.path = it->second;
      file.unique_name = it->first;
      file.name_in_dgo = x[1];
      result.push_back(file);
      matched_files.insert(unique_name);
    }
  }

  if (matched_files.size() != ref_file_names.size()) {
    lg::error("Some REF files were not matched to files in all_objs.json:");
    for (const auto& [path, flag] : ref_file_names) {
      if (matched_files.count(path) == 0) {
        lg::error("- '{}'", path);
      }
    }
    exit(1);
  }

  return result;
}

/*!
 * Read and parse the json config file, config.json, located in test/offline
 */
std::optional<OfflineTestConfig> parse_config(const std::string_view& game_name) {
  lg::info("Reading Configuration...");
  auto json_file_path =
      file_util::get_jak_project_dir() / "test" / "offline" / "config" / game_name / "config.jsonc";
  if (!fs::exists(json_file_path)) {
    lg::error("Couldn't load configuration, '{}' doesn't exist", json_file_path.string());
    return {};
  }
  auto json = parse_commented_json(file_util::read_text_file(json_file_path.string()),
                                   json_file_path.string());
  OfflineTestConfig result;
  result.dgos = json["dgos"].get<std::vector<std::string>>();
  result.skip_compile_files = json["skip_compile_files"].get<std::unordered_set<std::string>>();
  result.skip_compile_functions =
      json["skip_compile_functions"].get<std::unordered_set<std::string>>();
  result.skip_compile_states =
      json["skip_compile_states"]
          .get<std::unordered_map<std::string, std::unordered_set<std::string>>>();
  return std::make_optional(result);
}

/// @brief A simple struct to contain the reason for failure from a thread
struct OfflineTestResult {
  int exit_code;
  std::string reason;

  OfflineTestResult(int _exit_code, std::string _reason) : exit_code(_exit_code), reason(_reason) {}
};

int main(int argc, char* argv[]) {
  ArgumentGuard u8_guard(argc, argv);

  lg::initialize();

  bool dump_current_output = false;
  std::string iso_data_path;
  std::string game_name;
  // Useful for testing in debug mode (dont have to wait for everything to finish)
  int max_files = -1;
  std::string single_file = "";
  uint32_t num_threads = 1;

  CLI::App app{"OpenGOAL - Offline Reference Test Runner"};
  app.add_option("--iso_data_path", iso_data_path, "The path to the folder with the ISO data files")
      ->check(CLI::ExistingPath)
      ->required();
  app.add_option("--game", game_name, "The game name, for example 'jak1'")->required();
  app.add_flag("-d,--dump_current_output", dump_current_output,
               "Output the current output to a folder, use in conjunction with the reference test "
               "files update script");
  app.add_option("-m,--max_files", max_files,
                 "Limit the amount of files ran in a single test, picks the first N");
  app.add_option("-t,--num_threads", num_threads,
                 "The number of threads to partition the offline test work between");
  app.add_option("-f,--file", single_file,
                 "Limit the offline test routine to a single file to decompile/compile -- useful "
                 "when you are just iterating on a single file");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  if (!file_util::setup_project_path(std::nullopt)) {
    lg::error("Couldn't setup project path, tool is supposed to be ran in the jak-project repo!");
    return 1;
  }

  auto config = parse_config(game_name);
  if (!config.has_value()) {
    return 1;
  }

  lg::info("Finding files...");
  auto files = find_files(game_name, config->dgos, single_file);
  if (max_files > 0 && max_files < (int)files.size()) {
    files.erase(files.begin() + max_files, files.end());
  }

  std::vector<DecompilerArtFile> art_files;
  if (game_name == "jak1") {
    art_files = find_art_files(game_name, config->dgos);
  }

  // Create a bunch of threads to disassemble/decompile/compile the files
  if (num_threads < 1) {
    num_threads = 1;
  } else if (num_threads > 1) {
    num_threads = std::min(num_threads, std::thread::hardware_concurrency());
  }
  // First, prepare our batches of files to be processed
  std::vector<std::vector<DecompilerFile>> work_groups = {};
  for (int i = 0; i < num_threads; i++) {
    work_groups.push_back({});
  }
  int total_added = 0;
  for (auto& file : files) {
    work_groups.at(total_added % num_threads).push_back(file);
    total_added++;
  }

  // TODO - nicer printing, very messy with dozens of threads processing the job

  // Now we create a thread to process each group of work, and then await them
  std::vector<std::future<OfflineTestResult>> threads;
  decompiler::init_opcode_info();
  for (const auto& work_group : work_groups) {
    threads.push_back(std::async(std::launch::async, [&]() {
      std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
      lg::info("Setting up decompiler and loading files...");
      auto decompiler = setup_decompiler(work_group, art_files, fs::path(iso_data_path),
                                         config.value(), game_name);
      lg::info("Took {}ms", std::chrono::duration_cast<std::chrono::milliseconds>(
                                std::chrono::steady_clock::now() - begin)
                                .count());

      begin = std::chrono::steady_clock::now();
      lg::info("Disassembling files...");
      disassemble(decompiler);
      lg::info("Took {}ms", std::chrono::duration_cast<std::chrono::milliseconds>(
                                std::chrono::steady_clock::now() - begin)
                                .count());

      begin = std::chrono::steady_clock::now();
      lg::info("Decompiling...");
      decompile(decompiler, config.value());
      // It's about 100ms per file to decompile on average
      // meaning that when we have all 900 files, a full offline test will take 1.5 minutes
      lg::info("Took {}ms", std::chrono::duration_cast<std::chrono::milliseconds>(
                                std::chrono::steady_clock::now() - begin)
                                .count());

      begin = std::chrono::steady_clock::now();
      lg::info("Comparing...");
      auto compare_result = compare(decompiler, work_group, dump_current_output);
      lg::info("Took {}ms", std::chrono::duration_cast<std::chrono::milliseconds>(
                                std::chrono::steady_clock::now() - begin)
                                .count());

      lg::info("Compared {} lines. {}/{} files passed.", compare_result.total_lines,
               compare_result.ok_files, compare_result.total_files);
      lg::info("Dump? {}\n", dump_current_output);

      if (!compare_result.failing_files.empty()) {
        lg::error("Failing files:");
        for (auto& f : compare_result.failing_files) {
          lg::error("- {}", f);
        }
        lg::error("Comparison failed.");
        // No point continuing to compile if the comparison has failed
        return OfflineTestResult(1, "Comparison Failed");
      }

      begin = std::chrono::steady_clock::now();
      lg::info("Compiling...");
      bool compile_result = compile(decompiler, work_group, config.value(), game_name);
      // Compiling on the otherhand, is around 20ms per file
      lg::info("Took {}ms", std::chrono::duration_cast<std::chrono::milliseconds>(
                                std::chrono::steady_clock::now() - begin)
                                .count());

      if (!compile_result) {
        return OfflineTestResult(1, "Compilation Failed");
      }

      return OfflineTestResult(0, "");
    }));
  }

  // Fail fast over any thread tripping over
  for (auto& thread : threads) {
    auto ret = thread.get();
    if (ret.exit_code != 0) {
      lg::error(ret.reason);
      return ret.exit_code;
    }
  }

  return 0;
}
