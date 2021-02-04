#include "config.h"
#include "third-party/json.hpp"
#include "common/util/FileUtil.h"

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
  return nlohmann::json::parse(file_txt, nullptr, true, true);
}
}  // namespace

/*!
 * Parse the main config file and set the global decompiler configuration.
 */
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
  gConfig.write_func_json = cfg.at("write_func_json").get<bool>();
  gConfig.function_type_prop = cfg.at("function_type_prop").get<bool>();
  gConfig.analyze_expressions = cfg.at("analyze_expressions").get<bool>();
  gConfig.run_ir2 = cfg.at("run_ir2").get<bool>();

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

  auto type_hints_json = read_json_file_from_config(cfg, "type_hints_file");
  for (auto& kv : type_hints_json.items()) {
    auto& function_name = kv.key();
    auto& hints = kv.value();
    for (auto& hint : hints) {
      auto idx = hint.at(0).get<int>();
      for (size_t i = 1; i < hint.size(); i++) {
        auto& assignment = hint.at(i);
        TypeHint type_hint;
        type_hint.reg = Register(assignment.at(0).get<std::string>());
        type_hint.type_name = assignment.at(1).get<std::string>();
        gConfig.type_hints_by_function_by_idx[function_name][idx].push_back(type_hint);
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
}
}  // namespace decompiler