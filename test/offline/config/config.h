#pragma once

#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "common/common_types.h"

class OfflineTestConfig {
 public:
  OfflineTestConfig(const std::string_view& _game_name,
                    const std::string& _iso_data_path,
                    const u32 _num_threads,
                    const bool _dump_mode,
                    const bool _fail_on_cmp,
                    const bool _fail_on_compile,
                    const bool _pretty_print);

  std::string game_name;
  std::string iso_data_path;
  u32 num_threads;
  bool dump_mode;
  bool fail_on_cmp = false;
  bool fail_on_compile = false;  // unused currently!
  bool pretty_print = false;
  std::vector<std::string> dgos;
  std::unordered_set<std::string> skip_compile_files;
  std::unordered_set<std::string> skip_compile_functions;
  std::unordered_map<std::string, std::unordered_set<std::string>> skip_compile_states;
};
