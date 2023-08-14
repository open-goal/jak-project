#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "common/common_types.h"
#include "common/util/FileUtil.h"
#include "common/versions/versions.h"

#include "decompiler/Disasm/Register.h"
#include "decompiler/data/game_text.h"

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

struct LabelConfigInfo {
  // if the label is a "value" type, it will be loaded directly into a register.
  // in all cases, this is a constant, either a 64-bit integer or a float.
  // For example:
  //  ld v1, L346(fp)
  //  lwc1 f0, L345(fp)
  //  lw a0, L41(fp)

  // if the label is not a value type, it's a reference type, and the GOAL variable is a pointer.
  bool is_value = false;

  // the type of the resulting GOAL variable.
  std::string type_name;

  // if the type is a (pointer x) or (inline-array x), the size must be specified here.
  // For a boxed array (array x), the size will be figured out automatically
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
  std::unordered_map<std::string, std::unordered_set<int>> blocks_ending_in_asm_branch_by_func_name;
  std::unordered_map<std::string, std::vector<std::vector<int>>>
      format_ops_with_dynamic_string_by_func_name;
  std::unordered_set<std::string> mips2c_functions_by_name;
  std::unordered_map<std::string, std::vector<int>> mips2c_jump_table_functions;
  std::unordered_map<std::string, std::vector<std::pair<int, int>>> missing_textures_by_level;
};

struct ObjectPatchInfo {
  u32 crc;
  std::string target_file;
  std::string patch_file;
};

struct Config {
  GameVersion game_version = GameVersion::Jak1;
  std::vector<std::string> dgo_names;
  std::vector<std::string> object_file_names;
  std::vector<std::string> str_file_names;
  std::vector<std::string> str_texture_file_names;

  std::string audio_dir_file_name;
  std::vector<std::string> streamed_audio_file_names;

  std::string obj_file_name_map_file;
  std::string all_types_file;

  bool disassemble_code = false;
  bool decompile_code = false;
  bool write_scripts = false;
  bool disassemble_data = false;
  bool process_tpages = false;
  bool process_game_text = false;
  bool process_game_count = false;
  bool process_art_groups = false;
  bool process_subtitle_text = false;
  bool process_subtitle_images = false;
  bool dump_art_group_info = false;
  bool rip_levels = false;
  bool extract_collision = false;
  bool find_functions = false;
  bool read_spools = false;

  bool write_hex_near_instructions = false;
  bool hexdump_code = false;
  bool hexdump_data = false;
  bool dump_objs = false;
  bool print_cfgs = false;

  bool generate_symbol_definition_map = false;

  bool generate_all_types = false;
  std::optional<std::string> old_all_types_file;

  bool is_pal = false;

  bool write_patches = false;
  bool apply_patches = false;

  std::string game_name;
  std::string expected_elf_name;
  GameTextVersion text_version = GameTextVersion::JAK1_V1;

  std::unordered_set<std::string> allowed_objects;
  std::unordered_set<std::string> banned_objects;
  std::unordered_set<std::string> merged_objects;
  std::unordered_map<std::string, std::unordered_map<int, std::vector<RegisterTypeCast>>>
      register_type_casts_by_function_by_atomic_op_idx;
  std::unordered_map<std::string, std::unordered_map<int, StackTypeCast>>
      stack_type_casts_by_function_by_stack_offset;
  std::unordered_map<std::string, std::unordered_map<int, std::string>>
      anon_function_types_by_obj_by_id;
  std::unordered_map<std::string, std::vector<std::string>> function_arg_names;
  std::unordered_map<std::string, std::unordered_map<std::string, LocalVarOverride>>
      function_var_overrides;
  std::unordered_map<std::string, std::unordered_map<std::string, LabelConfigInfo>> label_types;
  std::unordered_map<std::string, std::vector<StackStructureHint>>
      stack_structure_hints_by_function;
  std::unordered_map<std::string, ObjectPatchInfo> object_patches;

  std::unordered_map<std::string, int> bad_format_strings;

  std::unordered_set<std::string> animated_textures;
  std::unordered_set<int> common_tpages;

  std::vector<std::string> levels_to_extract;
  bool levels_extract;
  bool save_texture_pngs = false;

  DecompileHacks hacks;

  std::unordered_map<std::string, std::string> art_groups_by_file;
  std::unordered_map<std::string, std::string> art_groups_by_function;
  std::unordered_map<std::string, std::unordered_map<int, std::string>> art_group_info_dump;

  std::unordered_map<std::string, std::vector<std::string>> import_deps_by_file;
};

Config read_config_file(const fs::path& path_to_config_file,
                        const std::string& config_game_version,
                        const std::string& override_json = "{}");

}  // namespace decompiler
