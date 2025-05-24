// Iterates through the `all-types` DTS to find types that meet a variety of criteria, such as:
// - type size
// - field types at given offsets
// - parent-types
// - ...

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"
#include "common/util/unicode_util.h"

#include "decompiler/util/DecompilerTypeSystem.h"

#include "fmt/core.h"
#include "third-party/CLI11.hpp"
#include "third-party/json.hpp"

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path output_path;
  std::string game_name = "jak1";
  std::string parent_type = "";
  int method_id_min = -1;
  std::string type_size = "";
  std::string field_json = "";
  bool get_all = false;

  lg::initialize();

  CLI::App app{"OpenGOAL Type Searcher"};
  app.add_option("--output-path", output_path, "Where to output the search results file");
  app.add_flag("-a,--all", get_all, "Just retrieve all possible type names");
  app.add_option("-g,--game", game_name, "Specify the game name, defaults to 'jak1'");
  app.add_option(
      "-s,--size", type_size,
      "The size of the type we are searching for, this can be a range (max-min), assumes decimal");
  app.add_option("-m,--method_id", method_id_min,
                 "Require the provided method id to be supported by the type");
  app.add_option("-p,--parent", parent_type, "The type of which it is an descendent of");
  app.add_option("-f,--fields", field_json,
                 "JSON encoded string specifying which field types and their offsets are required "
                 "- [{offset,type}]");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  auto ok = file_util::setup_project_path({});
  if (!ok) {
    lg::error("couldn't setup project path, exiting");
    return 1;
  }
  lg::info("Loading type definitions from all-types.gc...");

  auto game_version = game_name_to_version(game_name);

  decompiler::DecompilerTypeSystem dts(game_version);

  switch (game_version) {
    case GameVersion::Jak1:
      dts.parse_type_defs({"decompiler", "config", "jak1", "all-types.gc"});
      break;
    case GameVersion::Jak2:
      dts.parse_type_defs({"decompiler", "config", "jak2", "all-types.gc"});
      break;
    case GameVersion::Jak3:
      dts.parse_type_defs({"decompiler", "config", "jak3", "all-types.gc"});
      break;
    default:
      lg::error("unsupported game version");
      return 1;
  }

  auto results = nlohmann::json::array({});

  if (get_all) {
    auto type_names = dts.ts.get_all_type_names();
    for (const auto& name : type_names) {
      fmt::print("{}\n", name);
      results.push_back(name);
    }

    // Output the results as a json list
    file_util::write_text_file(output_path.string(), results.dump());
    return 0;
  }

  std::optional<std::vector<std::string>> potential_types = {};

  // First filter by parent type is available
  if (!parent_type.empty()) {
    potential_types = dts.ts.search_types_by_parent_type(parent_type);
  }

  // Filter by method count
  if (method_id_min != -1) {
    potential_types = dts.ts.search_types_by_minimum_method_id(method_id_min, potential_types);
  }

  // Filter out types by size next
  if (!type_size.empty()) {
    // Check if a range was given
    int min_size = 0;
    std::optional<int> max_size = {};
    if (str_util::contains(type_size, "-")) {
      auto tokens = str_util::split(type_size, '-');
      min_size = std::stoi(tokens[0]);
      max_size = std::stoi(tokens[1]);
    } else {
      min_size = std::stoi(type_size);
    }
    potential_types = dts.ts.search_types_by_size(min_size, max_size, potential_types);
  }

  // Filter out by fields
  if (!field_json.empty()) {
    std::vector<TypeSystem::TypeSearchFieldInput> search_fields = {};
    if (!field_json.empty()) {
      auto data = parse_commented_json(field_json, "--fields arg");
      for (auto& item : data) {
        TypeSystem::TypeSearchFieldInput new_field;
        try {
          new_field.field_offset = item.at("offset").get<int>();
          new_field.field_type_name = item.at("type").get<std::string>();
          search_fields.push_back(new_field);
        } catch (std::exception& ex) {
          fmt::print("Bad field search entry - {}", ex.what());
        }
      }
    }
    potential_types = dts.ts.search_types_by_fields(search_fields, potential_types);
  }

  if (potential_types) {
    for (const auto& val : potential_types.value()) {
      fmt::print("{}\n", val);
      results.push_back(val);
    }
  } else {
    fmt::print("Found Nothing!\n");
  }

  // Output the results as a json list
  file_util::write_text_file(output_path.string(), results.dump());
}
