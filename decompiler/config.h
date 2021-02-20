#pragma once

#ifndef JAK2_DISASSEMBLER_CONFIG_H
#define JAK2_DISASSEMBLER_CONFIG_H

#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include "decompiler/Disasm/Register.h"

namespace decompiler {
struct TypeHint {
  Register reg;
  std::string type_name;
};

struct LabelType {
  std::string type_name;
  bool is_const = false;
};

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
  bool function_type_prop = false;
  bool analyze_expressions = false;
  std::unordered_set<std::string> asm_functions_by_name;
  std::unordered_set<std::string> pair_functions_by_name;
  std::unordered_set<std::string> no_type_analysis_functions_by_name;
  std::unordered_set<std::string> allowed_objects;
  std::unordered_map<std::string, std::unordered_map<int, std::vector<TypeHint>>>
      type_hints_by_function_by_idx;
  std::unordered_map<std::string, std::unordered_map<int, std::string>>
      anon_function_types_by_obj_by_id;
  std::unordered_map<std::string, std::vector<std::string>> function_arg_names;
  std::unordered_map<std::string, std::unordered_map<std::string, std::string>> function_var_names;

  std::unordered_map<std::string, std::unordered_map<std::string, LabelType>> label_types;
  bool run_ir2 = false;
};

Config& get_config();
void set_config(const std::string& path_to_config_file);
}  // namespace decompiler

#endif  // JAK2_DISASSEMBLER_CONFIG_H
