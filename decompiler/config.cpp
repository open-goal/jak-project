#include "config.h"
#include "third-party/json.hpp"
#include "util/FileIO.h"

Config gConfig;

Config& get_config() {
  return gConfig;
}

void set_config(const std::string& path_to_config_file) {
  auto config_str = read_text_file(path_to_config_file);
  // to ignore comments in json, which may be useful
  auto cfg = nlohmann::json::parse(config_str, nullptr, true, true);

  gConfig.game_version = cfg.at("game_version").get<int>();
  gConfig.dgo_names = cfg.at("dgo_names").get<std::vector<std::string>>();
  gConfig.write_disassembly = cfg.at("write_disassembly").get<bool>();
  gConfig.write_hexdump = cfg.at("write_hexdump").get<bool>();
  gConfig.write_scripts = cfg.at("write_scripts").get<bool>();
  gConfig.write_hexdump_on_v3_only = cfg.at("write_hexdump_on_v3_only").get<bool>();
  gConfig.disassemble_objects_without_functions =
      cfg.at("disassemble_objects_without_functions").get<bool>();
  gConfig.find_basic_blocks = cfg.at("find_basic_blocks").get<bool>();
  gConfig.write_hex_near_instructions = cfg.at("write_hex_near_instructions").get<bool>();

  std::vector<std::string> asm_functions_by_name =
      cfg.at("asm_functions_by_name").get<std::vector<std::string>>();
  for (const auto& x : asm_functions_by_name) {
    gConfig.asm_functions_by_name.insert(x);
  }
}