#include "config.h"
#include "third-party/json.hpp"
#include "third-party/fmt/core.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "decompiler/util/config_parsers.h"

namespace decompiler {
Config gConfig;

Config& get_config() {
  return gConfig;
}

namespace {
/*!
 * Read an entry from cfg containing the name of a json file, and parse that file.
 * Relative to jak-project directory.
 */
nlohmann::json read_json_file_from_config(const nlohmann::json& cfg, const std::string& file_key) {
  auto file_name = cfg.at(file_key).get<std::string>();
  auto file_txt = file_util::read_text_file(file_util::get_file_path({file_name}));
  return parse_commented_json(file_txt);
}
}  // namespace

/*!
 * Parse the main config file and set the global decompiler configuration.
 */
void set_config(const std::string& path_to_config_file) {
  auto config_str = file_util::read_text_file(path_to_config_file);
  auto cfg = parse_commented_json(config_str);

  gConfig.game_version = cfg.at("game_version").get<int>();
  gConfig.dgo_names = cfg.at("dgo_names").get<std::vector<std::string>>();
  gConfig.object_file_names = cfg.at("object_file_names").get<std::vector<std::string>>();
  gConfig.str_file_names = cfg.at("str_file_names").get<std::vector<std::string>>();
  if (cfg.contains("obj_file_name_map_file")) {
    gConfig.obj_file_name_map_file = cfg.at("obj_file_name_map_file").get<std::string>();
  }
  gConfig.disassemble_code = cfg.at("disassemble_code").get<bool>();
  gConfig.decompile_code = cfg.at("decompile_code").get<bool>();
  gConfig.regenerate_all_types = cfg.at("regenerate_all_types").get<bool>();
  gConfig.write_hex_near_instructions = cfg.at("write_hex_near_instructions").get<bool>();
  gConfig.write_scripts = cfg.at("write_scripts").get<bool>();
  gConfig.disassemble_data = cfg.at("disassemble_data").get<bool>();
  gConfig.process_tpages = cfg.at("process_tpages").get<bool>();
  gConfig.process_game_text = cfg.at("process_game_text").get<bool>();
  gConfig.process_game_count = cfg.at("process_game_count").get<bool>();
  gConfig.hexdump_code = cfg.at("hexdump_code").get<bool>();
  gConfig.hexdump_data = cfg.at("hexdump_data").get<bool>();
  gConfig.dump_objs = cfg.at("dump_objs").get<bool>();
  gConfig.hint_inline_assembly_functions =
      cfg.at("inline_asm_hint").get<std::unordered_set<std::string>>();

  std::vector<std::string> asm_functions_by_name =
      cfg.at("asm_functions_by_name").get<std::vector<std::string>>();
  for (const auto& x : asm_functions_by_name) {
    gConfig.asm_functions_by_name.insert(x);
  }

  std::vector<std::string> pair_functions_by_name =
      cfg.at("pair_functions_by_name").get<std::vector<std::string>>();
  for (const auto& x : pair_functions_by_name) {
    gConfig.pair_functions_by_name.insert(x);
  }

  std::vector<std::string> no_type_analysis_functions_by_name =
      cfg.at("no_type_analysis_functions_by_name").get<std::vector<std::string>>();
  for (const auto& x : no_type_analysis_functions_by_name) {
    gConfig.no_type_analysis_functions_by_name.insert(x);
  }

  auto bad_inspect = cfg.at("types_with_bad_inspect_methods").get<std::vector<std::string>>();
  for (const auto& x : bad_inspect) {
    gConfig.bad_inspect_types.insert(x);
  }

  auto allowed = cfg.at("allowed_objects").get<std::vector<std::string>>();
  for (const auto& x : allowed) {
    gConfig.allowed_objects.insert(x);
  }

  auto type_casts_json = read_json_file_from_config(cfg, "type_casts_file");
  for (auto& kv : type_casts_json.items()) {
    auto& function_name = kv.key();
    auto& casts = kv.value();
    for (auto& cast : casts) {
      auto idx_range = parse_json_optional_integer_range(cast.at(0));
      for (auto idx : idx_range) {
        TypeCast type_cast;
        type_cast.atomic_op_idx = idx;
        type_cast.reg = Register(cast.at(1));
        type_cast.type_name = cast.at(2).get<std::string>();
        gConfig.type_casts_by_function_by_atomic_op_idx[function_name][idx].push_back(type_cast);
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
      gConfig.anon_function_types_by_obj_by_id[obj_file_name][id] = type_name;
    }
  }
  auto var_names_json = read_json_file_from_config(cfg, "var_names_file");
  for (auto& kv : var_names_json.items()) {
    auto& function_name = kv.key();
    auto arg = kv.value().find("args");
    if (arg != kv.value().end()) {
      for (auto& x : arg.value()) {
        gConfig.function_arg_names[function_name].push_back(x);
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
        gConfig.function_var_overrides[function_name][vkv.first] = override;
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
      bool is_const = x.at(2).get<bool>();
      auto& config_entry = gConfig.label_types[obj_name][name];
      config_entry = {type_name, is_const, {}};
      if (x.size() > 3) {
        config_entry.array_size = x.at(3).get<int>();
      }
    }
  }

  auto stack_vars_json = read_json_file_from_config(cfg, "stack_vars_file");
  for (auto& kv : stack_vars_json.items()) {
    auto& func_name = kv.key();
    auto& stack_vars = kv.value();
    gConfig.stack_var_hints_by_function[func_name] = parse_stack_var_hints(stack_vars);
  }
}

}  // namespace decompiler