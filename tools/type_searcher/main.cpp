// Iterates through the `all-types` DTS to find types that meet a variety of criteria, such as:
// - type size
// - field types at given offsets
// - parent-types
// - ...

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/unicode_util.h"

#include "decompiler/util/DecompilerTypeSystem.h"

#include "third-party/CLI11.hpp"
#include "third-party/fmt/core.h"
#include "third-party/json.hpp"

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path output_path;
  std::string game_name = "jak1";
  std::string parent_type = "";
  int type_size = -1;
  std::string field_json = "";

  lg::initialize();

  CLI::App app{"OpenGOAL Type Searcher"};
  app.add_option("--output-path", output_path, "Where to output the search results file");
  app.add_option("-g,--game", game_name, "Specify the game name, defaults to 'jak1'");
  app.add_option("-s,--size", type_size, "The size of the type we are searching for");
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

  // TODO - this could be better (have a jak1 folder)
  if (game_version == GameVersion::Jak1) {
    dts.parse_type_defs({"decompiler", "config", "all-types.gc"});
  } else if (game_version == GameVersion::Jak2) {
    dts.parse_type_defs({"decompiler", "config", "jak2", "all-types.gc"});
  } else {
    lg::error("unsupported game version");
    return 1;
  }

  std::vector<std::string> potential_types = {};

  // First filter by parent type is available
  if (!parent_type.empty()) {
    potential_types = dts.ts.search_types_by_parent_type(parent_type);
  }

  // Filter out types by size next
  if (type_size != -1) {
    potential_types = dts.ts.search_types_by_size(type_size, potential_types);
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

  auto results = nlohmann::json::array({});
  for (const auto& val : potential_types) {
    fmt::print("{}\n", val);
    results.push_back(val);
  }

  // Output the results as a json list
  file_util::write_text_file(output_path.string(), results.dump());
}
