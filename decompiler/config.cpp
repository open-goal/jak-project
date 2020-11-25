#include "config.h"
#include "third-party/json.hpp"
#include "common/util/FileUtil.h"

Config gConfig;

Config& get_config() {
  return gConfig;
}

void set_config(const std::string& path_to_config_file) {
  auto config_str = file_util::read_text_file(path_to_config_file);
  // to ignore comments in json, which may be useful
  auto cfg = nlohmann::json::parse(config_str, nullptr, true, true);

  gConfig.game_version = cfg.at("game_version").get<int>();
  gConfig.dgo_names = cfg.at("dgo_names").get<std::vector<std::string>>();
  gConfig.object_file_names = cfg.at("object_file_names").get<std::vector<std::string>>();
  gConfig.str_file_names = cfg.at("str_file_names").get<std::vector<std::string>>();
  if (cfg.contains("obj_file_name_map_file")) {
    gConfig.obj_file_name_map_file = cfg.at("obj_file_name_map_file").get<std::string>();
  }
  gConfig.write_disassembly = cfg.at("write_disassembly").get<bool>();
  gConfig.write_hexdump = cfg.at("write_hexdump").get<bool>();
  gConfig.write_scripts = cfg.at("write_scripts").get<bool>();
  gConfig.write_hexdump_on_v3_only = cfg.at("write_hexdump_on_v3_only").get<bool>();
  gConfig.disassemble_objects_without_functions =
      cfg.at("disassemble_objects_without_functions").get<bool>();
  gConfig.write_hex_near_instructions = cfg.at("write_hex_near_instructions").get<bool>();
  gConfig.analyze_functions = cfg.at("analyze_functions").get<bool>();
  gConfig.process_tpages = cfg.at("process_tpages").get<bool>();
  gConfig.process_game_text = cfg.at("process_game_text").get<bool>();
  gConfig.process_game_count = cfg.at("process_game_count").get<bool>();
  gConfig.dump_objs = cfg.at("dump_objs").get<bool>();

  std::vector<std::string> asm_functions_by_name =
      cfg.at("asm_functions_by_name").get<std::vector<std::string>>();
  for (const auto& x : asm_functions_by_name) {
    gConfig.asm_functions_by_name.insert(x);
  }

  auto bad_inspect = cfg.at("types_with_bad_inspect_methods").get<std::vector<std::string>>();
  for (const auto& x : bad_inspect) {
    gConfig.bad_inspect_types.insert(x);
  }
}
