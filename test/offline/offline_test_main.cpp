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

#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "goalc/compiler/Compiler.h"

#include "third-party/fmt/format.h"

namespace fs = std::filesystem;

// command line arguments
struct OfflineTestArgs {
  bool dump_current_output = false;
  std::string iso_data_path;
  s32 max_files = INT32_MAX;
};

/*!
 * Parse command line arguments.
 */
OfflineTestArgs parse_args(int argc, char* argv[]) {
  OfflineTestArgs result;

  for (int i = 1; i < argc; i++) {
    auto arg = std::string(argv[i]);
    if (arg == "--dump-mode") {
      result.dump_current_output = true;
      continue;
    }

    if (arg == "--max-files") {
      i++;
      if (i >= argc) {
        fmt::print("--max-files must be followed by an integer\n");
        exit(1);
      }
      result.max_files = atoi(argv[i]);
      fmt::print("Limiting to {} files\n", result.max_files);
      continue;
    }

    result.iso_data_path = arg;
    fmt::print("Using {} for ISO data\n", result.iso_data_path);
  }

  return result;
}

// json config file data (previously was in source of offline_test_main.cpp)
struct OfflineTestConfig {
  std::vector<std::string> dgos;
  std::unordered_set<std::string> skip_compile_files;
  std::unordered_set<std::string> skip_compile_functions;
  std::unordered_map<std::string, std::unordered_set<std::string>> skip_compile_states;
};

/*!
 * Read and parse the json config file, config.json, located in test/offline
 */
OfflineTestConfig parse_config() {
  auto json_file_path = file_util::get_jak_project_dir() / "test" / "offline" / "config.jsonc";
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
  return result;
}

struct DecompilerFile {
  std::filesystem::path path;
  std::string name_in_dgo;
  std::string unique_name;
  std::string reference;
};

struct DecompilerArtFile {
  std::string name_in_dgo;
  std::string unique_name;
};

std::string replaceFirstOccurrence(std::string& s,
                                   const std::string& toReplace,
                                   const std::string& replaceWith) {
  std::size_t pos = s.find(toReplace);
  if (pos == std::string::npos)
    return s;
  return s.replace(pos, toReplace.length(), replaceWith);
}

std::vector<DecompilerFile> find_files(const std::vector<std::string>& dgos) {
  std::vector<DecompilerFile> result;

  std::unordered_map<std::string, fs::path> files_with_ref;
  for (auto& p : fs::recursive_directory_iterator(file_util::get_jak_project_dir() / "test" /
                                                  "decompiler" / "reference")) {
    if (p.is_regular_file()) {
      std::string file_name = fs::path(p.path()).replace_extension().filename().string();
      if (file_name.find("_REF") == std::string::npos) {
        continue;
      }
      std::string object_name = replaceFirstOccurrence(file_name, "_REF", "");
      files_with_ref.insert({object_name, p.path()});
    }
  }

  fmt::print("  Found {} reference files\n", files_with_ref.size());

  // use the all_objs.json file to place them in the correct build order
  auto j = parse_commented_json(
      file_util::read_text_file(
          (file_util::get_jak_project_dir() / "goal_src" / "build" / "all_objs.json").string()),
      "all_objs.json");

  std::unordered_set<std::string> matched_files;
  for (auto& x : j) {
    auto unique_name = x[0].get<std::string>();

    std::vector<std::string> dgoList = x[3].get<std::vector<std::string>>();
    //    for (auto& p : reference_files_rough_order) {
    auto it = files_with_ref.find(unique_name);
    if (it != files_with_ref.end()) {
      // Check to see if we've included atleast one of the DGO/CGOs in our hardcoded list
      // If not BLOW UP
      bool dgoValidated = false;
      for (int i = 0; i < (int)dgoList.size(); i++) {
        std::string& dgo = dgoList.at(i);
        // can either be in the DGO or CGO folder, and can either end with .CGO or .DGO
        if (std::find(dgos.begin(), dgos.end(), fmt::format("DGO/{}.DGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("DGO/{}.CGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("CGO/{}.DGO", dgo)) != dgos.end() ||
            std::find(dgos.begin(), dgos.end(), fmt::format("CGO/{}.CGO", dgo)) != dgos.end()) {
          dgoValidated = true;
        }
      }
      if (!dgoValidated) {
        fmt::print(
            "File [{}] is in the following DGOs [{}], and not one of these is in our list! Add "
            "it!\n",
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

  if (matched_files.size() != files_with_ref.size()) {
    fmt::print("Error: some REF files were not matched to files in all_objs.json:\n");
    for (auto& f : files_with_ref) {
      if (matched_files.count(f.first) == 0) {
        fmt::print(" {}\n", f.first);
      }
    }
    exit(1);
  }

  return result;
}

std::vector<DecompilerArtFile> find_art_files(const std::vector<std::string>& dgos) {
  std::vector<DecompilerArtFile> result;

  // use the all_objs.json file to place them in the correct build order
  auto j = parse_commented_json(
      file_util::read_text_file(
          (file_util::get_jak_project_dir() / "goal_src" / "build" / "all_objs.json").string()),
      "all_objs.json");

  for (auto& x : j) {
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
        fmt::print(
            "File [{}] is in the following DGOs [{}], and not one of these is in our list! Add "
            "it!\n",
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

struct Decompiler {
  std::unique_ptr<decompiler::ObjectFileDB> db;
  std::unique_ptr<decompiler::Config> config;
};

Decompiler setup_decompiler(const std::vector<DecompilerFile>& files,
                            const std::vector<DecompilerArtFile>& art_files,
                            const OfflineTestArgs& args,
                            const OfflineTestConfig& offline_config) {
  Decompiler dc;
  decompiler::init_opcode_info();
  dc.config = std::make_unique<decompiler::Config>(decompiler::read_config_file(
      (file_util::get_jak_project_dir() / "decompiler" / "config" / "jak1_ntsc_black_label.jsonc")
          .string(),
      {}));

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

  std::vector<std::string> dgo_paths;
  if (args.iso_data_path.empty()) {
    for (auto& x : offline_config.dgos) {
      dgo_paths.push_back((file_util::get_jak_project_dir() / "iso_data" / "jak1" / x).string());
    }
  } else {
    for (auto& x : offline_config.dgos) {
      dgo_paths.push_back(file_util::combine_path(args.iso_data_path, x));
    }
  }

  dc.db = std::make_unique<decompiler::ObjectFileDB>(dgo_paths, dc.config->obj_file_name_map_file,
                                                     std::vector<std::string>{},
                                                     std::vector<std::string>{}, *dc.config);

  std::unordered_set<std::string> db_files;
  for (auto& files_by_name : dc.db->obj_files_by_name) {
    for (auto& f : files_by_name.second) {
      db_files.insert(f.to_unique_name());
    }
  }

  if (db_files.size() != files.size() + art_files.size()) {
    fmt::print("DB file error.\n");
    for (auto& f : files) {
      if (!db_files.count(f.unique_name)) {
        fmt::print("didn't find {}\n", f.unique_name);
      }
    }
    for (auto& f : art_files) {
      if (!db_files.count(f.unique_name)) {
        fmt::print("didn't find {}\n", f.unique_name);
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

std::string strip_trailing_newlines(const std::string& in) {
  std::string out = in;
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
    std::string result = strip_trailing_newlines(data.full_output);
    std::string ref = strip_trailing_newlines(file_util::read_text_file(file.path.string()));
    compare_result.total_files++;
    compare_result.total_lines += line_count(result);
    if (result != ref) {
      compare_result.failing_files.push_back(file.unique_name);
      compare_result.total_pass = false;
      fmt::print("Reference test failure on {}:\n", file.unique_name);
      fmt::print("{}\n", diff_strings(ref, result));

      if (dump_mode) {
        file_util::create_dir_if_needed("./failures");
        file_util::write_text_file("./failures/" + file.unique_name + "_REF.gc", result);
      }
    } else {
      compare_result.ok_files++;
    }
  }

  return compare_result;
}

bool compile(Decompiler& dc,
             const std::vector<DecompilerFile>& refs,
             const OfflineTestConfig& config) {
  fmt::print("Setting up compiler...\n");
  Compiler compiler;

  compiler.run_front_end_on_file({"decompiler", "config", "all-types.gc"});
  compiler.run_front_end_on_file({"test", "decompiler", "reference", "decompiler-macros.gc"});

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

int main(int argc, char* argv[]) {
  fmt::print("Offline Decompiler Test 2\n");
  lg::initialize();
  if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }

  fmt::print("Reading config...\n");
  auto args = parse_args(argc, argv);
  auto config = parse_config();

  fmt::print("Finding files...\n");
  auto files = find_files(config.dgos);
  if (args.max_files < (int)files.size()) {
    files.erase(files.begin() + args.max_files, files.end());
  }
  auto art_files = find_art_files(config.dgos);

  fmt::print("Setting up decompiler and loading files...\n");
  auto decompiler = setup_decompiler(files, art_files, args, config);

  fmt::print("Disassembling files...\n");
  disassemble(decompiler);

  fmt::print("Decompiling...\n");
  decompile(decompiler, config);

  fmt::print("Comparing...\n");
  auto compare_result = compare(decompiler, files, args.dump_current_output);
  fmt::print("Compared {} lines. {}/{} files passed.\n", compare_result.total_lines,
             compare_result.ok_files, compare_result.total_files);

  if (!compare_result.failing_files.empty()) {
    fmt::print("Failing files:\n");
    for (auto& f : compare_result.failing_files) {
      fmt::print("  {}\n", f);
    }
  }

  bool compile_result = compile(decompiler, files, config);

  if (compare_result.total_pass && compile_result) {
    fmt::print("Pass!\n");
    return 0;
  } else {
    if (!compile_result) {
      fmt::print("Compilation failed.\n");
    }
    if (!compare_result.total_pass) {
      fmt::print("Comparison failed.\n");
    }
  }
  return 1;
}
