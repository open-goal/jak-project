#pragma once

#ifndef JAK2_DISASSEMBLER_CONFIG_H
#define JAK2_DISASSEMBLER_CONFIG_H

#include <string>
#include <vector>
#include <unordered_set>

struct Config {
  int game_version = -1;
  std::vector<std::string> dgo_names;
  std::vector<std::string> object_file_names;
  std::vector<std::string> str_file_names;
  std::unordered_set<std::string> bad_inspect_types;
  std::string obj_file_name_map_file;
  bool write_disassembly = false;
  bool write_hexdump = false;
  bool write_scripts = false;
  bool write_hexdump_on_v3_only = false;
  bool disassemble_objects_without_functions = false;
  bool write_hex_near_instructions = false;
  bool analyze_functions = false;
  bool process_tpages = false;
  bool process_game_text = false;
  bool process_game_count = false;
  bool dump_objs = false;
  bool write_func_json = false;
  std::unordered_set<std::string> asm_functions_by_name;
  // ...
};

Config& get_config();
void set_config(const std::string& path_to_config_file);

#endif  // JAK2_DISASSEMBLER_CONFIG_H
