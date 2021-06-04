#pragma once

#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <optional>
#include "decompiler/Disasm/Register.h"

namespace decompiler {
struct RegisterTypeCast {
  int atomic_op_idx = -1;
  Register reg;
  std::string type_name;
};

struct StackTypeCast {
  int stack_offset = -1;
  std::string type_name;
};

struct LabelType {
  std::string type_name;
  bool is_const = false;
  std::optional<int> array_size;
};

struct LocalVarOverride {
  std::string name;
  // this may be left out, indicating that the variable should use the type determined
  // by the type analysis pass.
  std::optional<std::string> type;
};

/*!
 * Information about a structure on the stack.
 */
struct StackStructureHint {
  std::string element_type;  // type of the thing stored
  // todo - is boxed array on the stack supported?
  enum class ContainerType {
    NONE,         // just store the plain thing.
    ARRAY,        // for refs, array of refs. For values, array of values.
    INLINE_ARRAY  // for refs, array of values, for values, invalid
  } container_type = ContainerType::NONE;
  int container_size = -1;  // if container other than NONE, the number of elements.
  int stack_offset = 0;     // where it's located on the stack (relative to sp after prologue)
};

struct CondWithElseLengthHack {
  std::unordered_map<std::string, int> max_length_by_start_block;
};

struct DecompileHacks {
  std::unordered_set<std::string> types_with_bad_inspect_methods;
  std::unordered_set<std::string> no_type_analysis_functions_by_name;
  std::unordered_set<std::string> hint_inline_assembly_functions;
  std::unordered_set<std::string> asm_functions_by_name;
  std::unordered_set<std::string> pair_functions_by_name;
  std::unordered_map<std::string, CondWithElseLengthHack> cond_with_else_len_by_func_name;
  std::unordered_set<std::string> reject_cond_to_value;
};

struct Config {
  int game_version = -1;
  std::vector<std::string> dgo_names;
  std::vector<std::string> object_file_names;
  std::vector<std::string> str_file_names;

  std::string obj_file_name_map_file;

  bool disassemble_code = false;
  bool decompile_code = false;
  bool write_scripts = false;
  bool disassemble_data = false;
  bool process_tpages = false;
  bool process_game_text = false;
  bool process_game_count = false;

  bool regenerate_all_types = false;
  bool write_hex_near_instructions = false;
  bool hexdump_code = false;
  bool hexdump_data = false;
  bool dump_objs = false;
  bool print_cfgs = false;

  bool generate_symbol_definition_map = false;

  std::unordered_set<std::string> allowed_objects;
  std::unordered_map<std::string, std::unordered_map<int, std::vector<RegisterTypeCast>>>
      register_type_casts_by_function_by_atomic_op_idx;
  std::unordered_map<std::string, std::unordered_map<int, StackTypeCast>>
      stack_type_casts_by_function_by_stack_offset;
  std::unordered_map<std::string, std::unordered_map<int, std::string>>
      anon_function_types_by_obj_by_id;
  std::unordered_map<std::string, std::vector<std::string>> function_arg_names;
  std::unordered_map<std::string, std::unordered_map<std::string, LocalVarOverride>>
      function_var_overrides;
  std::unordered_map<std::string, std::unordered_map<std::string, LabelType>> label_types;
  std::unordered_map<std::string, std::vector<StackStructureHint>>
      stack_structure_hints_by_function;

  DecompileHacks hacks;
};

Config read_config_file(const std::string& path_to_config_file);

}  // namespace decompiler
