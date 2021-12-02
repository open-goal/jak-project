#include "config.h"
#include "third-party/json.hpp"
#include "third-party/fmt/core.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "decompiler/util/config_parsers.h"

namespace decompiler {

namespace {
/*!
 * Read an entry from cfg containing the name of a json file, and parse that file.
 * Relative to jak-project directory.
 */
nlohmann::json read_json_file_from_config(const nlohmann::json& cfg, const std::string& file_key) {
  auto file_name = cfg.at(file_key).get<std::string>();
  auto file_txt = file_util::read_text_file(file_util::get_file_path({file_name}));
  return parse_commented_json(file_txt, file_name);
}
}  // namespace

/*!
 * Parse the main config file and return decompiler config.
 */
Config read_config_file(const std::string& path_to_config_file) {
  Config config;
  auto config_str = file_util::read_text_file(path_to_config_file);
  auto cfg = parse_commented_json(config_str, path_to_config_file);

  config.game_version = cfg.at("game_version").get<int>();

  auto inputs_json = read_json_file_from_config(cfg, "inputs_file");
  config.dgo_names = inputs_json.at("dgo_names").get<std::vector<std::string>>();
  config.object_file_names = inputs_json.at("object_file_names").get<std::vector<std::string>>();
  config.str_file_names = inputs_json.at("str_file_names").get<std::vector<std::string>>();
  config.audio_dir_file_name = inputs_json.at("audio_dir_file_name").get<std::string>();
  config.streamed_audio_file_names =
      inputs_json.at("streamed_audio_file_names").get<std::vector<std::string>>();

  if (cfg.contains("obj_file_name_map_file")) {
    config.obj_file_name_map_file = cfg.at("obj_file_name_map_file").get<std::string>();
  }
  config.disassemble_code = cfg.at("disassemble_code").get<bool>();
  config.decompile_code = cfg.at("decompile_code").get<bool>();
  config.regenerate_all_types = cfg.at("regenerate_all_types").get<bool>();
  config.write_hex_near_instructions = cfg.at("write_hex_near_instructions").get<bool>();
  config.write_scripts = cfg.at("write_scripts").get<bool>();
  config.disassemble_data = cfg.at("disassemble_data").get<bool>();
  config.process_tpages = cfg.at("process_tpages").get<bool>();
  config.process_game_text = cfg.at("process_game_text").get<bool>();
  config.process_game_count = cfg.at("process_game_count").get<bool>();
  config.hexdump_code = cfg.at("hexdump_code").get<bool>();
  config.hexdump_data = cfg.at("hexdump_data").get<bool>();
  config.dump_objs = cfg.at("dump_objs").get<bool>();
  config.print_cfgs = cfg.at("print_cfgs").get<bool>();
  config.generate_symbol_definition_map = cfg.at("generate_symbol_definition_map").get<bool>();
  config.is_pal = cfg.at("is_pal").get<bool>();

  auto allowed = cfg.at("allowed_objects").get<std::vector<std::string>>();
  for (const auto& x : allowed) {
    config.allowed_objects.insert(x);
  }
  auto banned = cfg.at("banned_objects").get<std::vector<std::string>>();
  for (const auto& x : banned) {
    config.banned_objects.insert(x);
  }

  auto type_casts_json = read_json_file_from_config(cfg, "type_casts_file");
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

  auto anon_func_json = read_json_file_from_config(cfg, "anonymous_function_types_file");
  for (auto& kv : anon_func_json.items()) {
    auto& obj_file_name = kv.key();
    auto& anon_types = kv.value();
    for (auto& anon_type : anon_types) {
      auto id = anon_type.at(0).get<int>();
      const auto& type_name = anon_type.at(1).get<std::string>();
      config.anon_function_types_by_obj_by_id[obj_file_name][id] = type_name;
    }
  }
  auto var_names_json = read_json_file_from_config(cfg, "var_names_file");
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

  auto label_types_json = read_json_file_from_config(cfg, "label_types_file");
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

  auto stack_structures_json = read_json_file_from_config(cfg, "stack_structures_file");
  for (auto& kv : stack_structures_json.items()) {
    auto& func_name = kv.key();
    auto& stack_structures = kv.value();
    config.stack_structure_hints_by_function[func_name] =
        parse_stack_structure_hints(stack_structures);
  }

  auto hacks_json = read_json_file_from_config(cfg, "hacks_file");
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
  config.levels_to_extract = cfg.at("levels_to_extract").get<std::vector<std::string>>();
  return config;
}

}  // namespace decompiler
