#include "config.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "decompiler/util/config_parsers.h"

#include "third-party/fmt/core.h"
#include "third-party/json.hpp"

namespace decompiler {

namespace {
/*!
 * Read an entry from cfg containing the name of a json file, and parse that file.
 * Relative to jak-project directory.
 */
nlohmann::json read_json_file_from_config(const nlohmann::json& json, const std::string& file_key) {
  auto file_name = json.at(file_key).get<std::string>();
  auto file_txt = file_util::read_text_file(file_util::get_file_path({file_name}));
  return parse_commented_json(file_txt, file_name);
}

Config make_config_via_json(nlohmann::json& json) {
  Config config;
  int version_int = json.at("game_version").get<int>();
  ASSERT(version_int == 1 || version_int == 2 || version_int == 3);
  config.game_version = (GameVersion)version_int;
  config.text_version = json.at("text_version").get<GameTextVersion>();
  config.game_name = json.at("game_name").get<std::string>();
  if (json.contains("expected_elf_name")) {
    config.expected_elf_name = json.at("expected_elf_name").get<std::string>();
  }
  config.all_types_file = json.at("all_types_file").get<std::string>();

  auto inputs_json = read_json_file_from_config(json, "inputs_file");
  config.dgo_names = inputs_json.at("dgo_names").get<std::vector<std::string>>();
  config.object_file_names = inputs_json.at("object_file_names").get<std::vector<std::string>>();
  config.str_file_names = inputs_json.at("str_file_names").get<std::vector<std::string>>();

  if (inputs_json.contains("str_texture_file_names")) {
    config.str_texture_file_names =
        inputs_json.at("str_texture_file_names").get<std::vector<std::string>>();
  }

  config.audio_dir_file_name = inputs_json.at("audio_dir_file_name").get<std::string>();
  config.streamed_audio_file_names =
      inputs_json.at("streamed_audio_file_names").get<std::vector<std::string>>();

  if (json.contains("art_group_dump_file")) {
    auto json_data = file_util::read_text_file(
        file_util::get_file_path({json.at("art_group_dump_file").get<std::string>()}));
    std::unordered_map<std::string, std::unordered_map<int, std::string>> serialized =
        parse_commented_json(json_data, "art_group_dump_file");
    config.art_group_info_dump = serialized;
  }

  if (json.contains("joint_node_dump_file")) {
    auto json_data = file_util::read_text_file(
        file_util::get_file_path({json.at("joint_node_dump_file").get<std::string>()}));
    std::unordered_map<std::string, std::unordered_map<int, std::string>> serialized =
        parse_commented_json(json_data, "joint_node_dump_file");
    config.jg_info_dump = serialized;
  }

  if (json.contains("obj_file_name_map_file")) {
    config.obj_file_name_map_file = json.at("obj_file_name_map_file").get<std::string>();
  }
  config.disassemble_code = json.at("disassemble_code").get<bool>();
  config.decompile_code = json.at("decompile_code").get<bool>();
  config.write_hex_near_instructions = json.at("write_hex_near_instructions").get<bool>();
  config.write_scripts = json.at("write_scripts").get<bool>();
  config.disassemble_data = json.at("disassemble_data").get<bool>();
  config.process_tpages = json.at("process_tpages").get<bool>();
  config.process_game_text = json.at("process_game_text").get<bool>();
  config.process_game_count = json.at("process_game_count").get<bool>();
  config.process_art_groups = json.at("process_art_groups").get<bool>();
  if (json.contains("process_subtitle_text")) {
    config.process_subtitle_text = json.at("process_subtitle_text").get<bool>();
  }
  if (json.contains("process_subtitle_images")) {
    config.process_subtitle_images = json.at("process_subtitle_images").get<bool>();
  }
  config.dump_art_group_info = json.at("dump_art_group_info").get<bool>();
  config.dump_joint_geo_info = json.at("dump_joint_geo_info").get<bool>();
  config.hexdump_code = json.at("hexdump_code").get<bool>();
  config.hexdump_data = json.at("hexdump_data").get<bool>();
  config.find_functions = json.at("find_functions").get<bool>();
  config.dump_objs = json.at("dump_objs").get<bool>();
  config.print_cfgs = json.at("print_cfgs").get<bool>();
  config.generate_symbol_definition_map = json.at("generate_symbol_definition_map").get<bool>();
  config.is_pal = json.at("is_pal").get<bool>();
  config.rip_levels = json.at("rip_levels").get<bool>();
  config.extract_collision = json.at("extract_collision").get<bool>();
  config.generate_all_types = json.at("generate_all_types").get<bool>();
  if (json.contains("read_spools")) {
    config.read_spools = json.at("read_spools").get<bool>();
  }
  if (json.contains("old_all_types_file")) {
    config.old_all_types_file = json.at("old_all_types_file").get<std::string>();
  }
  auto allowed = json.at("allowed_objects").get<std::vector<std::string>>();
  for (const auto& x : allowed) {
    config.allowed_objects.insert(x);
  }
  auto banned = json.at("banned_objects").get<std::vector<std::string>>();
  for (const auto& x : banned) {
    config.banned_objects.insert(x);
  }

  auto type_casts_json = read_json_file_from_config(json, "type_casts_file");
  if (json.contains("type_casts_merge_file")) {
    type_casts_json.update(read_json_file_from_config(json, "type_casts_merge_file"));
  }
  for (auto& kv : type_casts_json.items()) {
    auto& function_name = kv.key();
    auto& casts = kv.value();
    for (auto& cast : casts) {
      if (cast.at(0).is_string()) {
        auto cast_name = cast.at(0).get<std::string>();
        if (cast_name == "_stack_") {
          // it's a stack var cast
          StackTypeCast stack_cast;
          stack_cast.stack_offset = cast.at(1).get<int>();
          stack_cast.type_name = cast.at(2).get<std::string>();
          config.stack_type_casts_by_function_by_stack_offset[function_name]
                                                             [stack_cast.stack_offset] = stack_cast;
        } else {
          throw std::runtime_error(fmt::format("Unknown cast type: {}", cast_name));
        }
      } else {
        auto idx_range = parse_json_optional_integer_range(cast.at(0));
        for (auto idx : idx_range) {
          RegisterTypeCast type_cast;
          type_cast.atomic_op_idx = idx;
          type_cast.reg = Register(cast.at(1).get<std::string>());
          type_cast.type_name = cast.at(2).get<std::string>();
          config.register_type_casts_by_function_by_atomic_op_idx[function_name][idx].push_back(
              type_cast);
        }
      }
    }
  }

  auto anon_func_json = read_json_file_from_config(json, "anonymous_function_types_file");
  if (json.contains("anonymous_function_types_merge_file")) {
    anon_func_json.update(read_json_file_from_config(json, "anonymous_function_types_merge_file"));
  }
  for (auto& kv : anon_func_json.items()) {
    auto& obj_file_name = kv.key();
    auto& anon_types = kv.value();
    for (auto& anon_type : anon_types) {
      auto id = anon_type.at(0).get<int>();
      const auto& type_name = anon_type.at(1).get<std::string>();
      config.anon_function_types_by_obj_by_id[obj_file_name][id] = type_name;
    }
  }
  auto var_names_json = read_json_file_from_config(json, "var_names_file");
  for (auto& kv : var_names_json.items()) {
    auto& function_name = kv.key();
    auto arg = kv.value().find("args");
    if (arg != kv.value().end()) {
      for (auto& x : arg.value()) {
        config.function_arg_names[function_name].push_back(x);
      }
    }

    auto var = kv.value().find("vars");
    if (var != kv.value().end()) {
      for (auto& vkv : var->get<std::unordered_map<std::string, nlohmann::json>>()) {
        LocalVarOverride override;
        if (vkv.second.is_string()) {
          override.name = vkv.second.get<std::string>();
        } else if (vkv.second.is_array()) {
          override.name = vkv.second[0].get<std::string>();
          override.type = vkv.second[1].get<std::string>();
        } else {
          throw std::runtime_error("Invalid function var override.");
        }
        config.function_var_overrides[function_name][vkv.first] = override;
      }
    }
  }

  auto label_types_json = read_json_file_from_config(json, "label_types_file");
  if (json.contains("label_types_merge_file")) {
    label_types_json.update(read_json_file_from_config(json, "label_types_merge_file"));
  }
  for (auto& kv : label_types_json.items()) {
    auto& obj_name = kv.key();
    auto& types = kv.value();
    for (auto& x : types) {
      const auto& name = x.at(0).get<std::string>();
      const auto& type_name = x.at(1).get<std::string>();
      bool is_val = false;
      std::optional<int> array_size;
      if (x.size() > 2) {
        if (x.at(2).is_boolean()) {
          is_val = x.at(2).get<bool>();
        } else {
          array_size = x.at(2).get<int>();
        }
      }
      auto& config_entry = config.label_types[obj_name][name];
      config_entry = {is_val, type_name, array_size};
    }
  }

  auto stack_structures_json = read_json_file_from_config(json, "stack_structures_file");
  if (json.contains("stack_structures_merge_file")) {
    stack_structures_json.update(read_json_file_from_config(json, "stack_structures_merge_file"));
  }
  for (auto& kv : stack_structures_json.items()) {
    auto& func_name = kv.key();
    auto& stack_structures = kv.value();
    config.stack_structure_hints_by_function[func_name] =
        parse_stack_structure_hints(stack_structures);
  }

  auto hacks_json = read_json_file_from_config(json, "hacks_file");
  if (json.contains("hacks_merge_file")) {
    // NOTE - here we merge one level deeper because it's worth doing here
    // - chances are you just need to override a few individual hacks
    const auto hack_overrides = read_json_file_from_config(json, "hacks_merge_file");
    for (const auto& entry : hack_overrides.items()) {
      if (hacks_json.contains(entry.key())) {
        // If the parent json file has this, update it
        hacks_json.at(entry.key()).update(entry.value());
      } else {
        // Otherwise, we append it
        hacks_json[entry.key()] = entry.value();
      }
    }
  }
  config.hacks.hint_inline_assembly_functions =
      hacks_json.at("hint_inline_assembly_functions").get<std::unordered_set<std::string>>();
  config.hacks.asm_functions_by_name =
      hacks_json.at("asm_functions_by_name").get<std::unordered_set<std::string>>();
  config.hacks.pair_functions_by_name =
      hacks_json.at("pair_functions_by_name").get<std::unordered_set<std::string>>();
  config.hacks.no_type_analysis_functions_by_name =
      hacks_json.at("no_type_analysis_functions_by_name").get<std::unordered_set<std::string>>();
  config.hacks.types_with_bad_inspect_methods =
      hacks_json.at("types_with_bad_inspect_methods").get<std::unordered_set<std::string>>();
  config.hacks.reject_cond_to_value = hacks_json.at("aggressively_reject_cond_to_value_rewrite")
                                          .get<std::unordered_set<std::string>>();
  config.hacks.blocks_ending_in_asm_branch_by_func_name =
      hacks_json.at("blocks_ending_in_asm_branch")
          .get<std::unordered_map<std::string, std::unordered_set<int>>>();
  config.hacks.format_ops_with_dynamic_string_by_func_name =
      hacks_json.at("dynamic_format_arg_counts")
          .get<std::unordered_map<std::string, std::vector<std::vector<int>>>>();
  config.hacks.mips2c_functions_by_name =
      hacks_json.at("mips2c_functions_by_name").get<std::unordered_set<std::string>>();
  config.hacks.mips2c_jump_table_functions =
      hacks_json.at("mips2c_jump_table_functions")
          .get<std::unordered_map<std::string, std::vector<int>>>();

  for (auto& entry : hacks_json.at("cond_with_else_max_lengths")) {
    auto func_name = entry.at(0).get<std::string>();
    auto cond_name = entry.at(1).get<std::string>();
    auto max_len = entry.at(2).get<int>();
    config.hacks.cond_with_else_len_by_func_name[func_name].max_length_by_start_block[cond_name] =
        max_len;
  }

  for (auto& entry : hacks_json.at("missing_textures")) {
    int tpage = entry.at(1).get<int>();
    int idx = entry.at(2).get<int>();
    config.hacks.missing_textures_by_level[entry.at(0).get<std::string>()].emplace_back(tpage, idx);
  }

  config.bad_format_strings =
      hacks_json.at("bad_format_strings").get<std::unordered_map<std::string, int>>();

  auto merged = hacks_json.at("expected_merged_objs").get<std::vector<std::string>>();
  for (const auto& x : merged) {
    config.merged_objects.insert(x);
  }

  config.levels_to_extract = inputs_json.at("levels_to_extract").get<std::vector<std::string>>();
  config.levels_extract = json.at("levels_extract").get<bool>();
  if (json.contains("save_texture_pngs")) {
    config.save_texture_pngs = json.at("save_texture_pngs").get<bool>();
  }

  if (inputs_json.contains("animated_textures")) {
    config.animated_textures =
        inputs_json.at("animated_textures").get<std::unordered_set<std::string>>();
  }

  if (inputs_json.contains("common_tpages")) {
    config.common_tpages = inputs_json.at("common_tpages").get<std::unordered_set<int>>();
  }

  auto art_info_json = read_json_file_from_config(json, "art_info_file");
  config.art_group_type_remap =
      art_info_json.at("type_remap").get<std::unordered_map<std::string, std::string>>();
  config.joint_node_hacks =
      art_info_json.at("joint_node_hacks").get<std::unordered_map<std::string, std::string>>();

  auto import_deps = read_json_file_from_config(json, "import_deps_file");
  config.import_deps_by_file =
      import_deps.get<std::unordered_map<std::string, std::vector<std::string>>>();

  config.write_patches = json.at("write_patches").get<bool>();
  config.apply_patches = json.at("apply_patches").get<bool>();
  const auto& object_patches = json.at("object_patches");
  for (auto& [obj, pch] : object_patches.items()) {
    ObjectPatchInfo new_pch;
    new_pch.crc = (u32)std::stoull(pch.at("crc32").get<std::string>(), nullptr, 16);
    new_pch.target_file = pch.at("in").get<std::string>();
    new_pch.patch_file = pch.at("out").get<std::string>();
    config.object_patches.insert({obj, new_pch});
  }

  return config;
}
}  // namespace

/*!
 * Parse the main config file and return decompiler config.
 */
Config read_config_file(const fs::path& path_to_config_file,
                        const std::string& config_game_version,
                        const std::string& override_json) {
  Config config;
  auto config_str = file_util::read_text_file(path_to_config_file);
  auto json = parse_commented_json(config_str, path_to_config_file.string());

  // First, check if we need to update the JSON from the game versions overrides
  if (json.contains("version_overrides")) {
    if (!json.at("version_overrides").contains(config_game_version)) {
      throw std::runtime_error(fmt::format(
          "'{}' provided which doesn't correspond with a 'version_overrides", config_game_version));
    }
    lg::info("Game Config Overide: '{}'", config_game_version);
    json.update(json.at("version_overrides").at(config_game_version));
  }

  // Then, update any config overrides
  if (override_json != "{}" && !override_json.empty()) {
    lg::info("Config Overide: '{}'", override_json);
    auto cfg_override = parse_commented_json(override_json, "");
    json.update(cfg_override);
  }

  // debugging, dump the JSON config to a file
  // fs::path debug_path = path_to_config_file.parent_path() / "config-debug.jsonc";
  // file_util::write_text_file(debug_path, json.dump(2));

  // Lastly, update the struct via the JSON
  return make_config_via_json(json);
}

}  // namespace decompiler
