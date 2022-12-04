#include "config.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

OfflineTestConfig::OfflineTestConfig(const std::string_view& _game_name,
                                     const std::string& _iso_data_path,
                                     const u32 _num_threads,
                                     const bool _dump_mode,
                                     const bool _fail_on_cmp,
                                     const bool _fail_on_compile,
                                     const bool _pretty_print)
    : game_name(_game_name),
      iso_data_path(_iso_data_path),
      num_threads(_num_threads),
      dump_mode(_dump_mode),
      fail_on_cmp(_fail_on_cmp),
      fail_on_compile(_fail_on_compile),
      pretty_print(_pretty_print) {
  lg::info("Reading Configuration...");
  auto json_file_path =
      file_util::get_jak_project_dir() / "test" / "offline" / "config" / game_name / "config.jsonc";
  if (!fs::exists(json_file_path)) {
    lg::error("Couldn't load configuration, '{}' doesn't exist", json_file_path.string());
    throw std::exception();
  }
  auto json = parse_commented_json(file_util::read_text_file(json_file_path.string()),
                                   json_file_path.string());
  dgos = json["dgos"].get<std::vector<std::string>>();
  skip_compile_files = json["skip_compile_files"].get<std::unordered_set<std::string>>();
  skip_compile_functions = json["skip_compile_functions"].get<std::unordered_set<std::string>>();
  skip_compile_states =
      json["skip_compile_states"]
          .get<std::unordered_map<std::string, std::unordered_set<std::string>>>();
}
