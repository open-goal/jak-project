#pragma once

#include <string>
#include <unordered_map>

#include "decompiler/Function/Function.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

struct TypeInspectorResult {
  bool success = false;
  int type_size = -1;
  int type_method_count = -1;
  int parent_method_count = 9;
  int type_heap_base = -1;

  std::string warnings;
  std::vector<Field> fields_of_type;
  bool is_basic = false;
  bool found_flags = false;

  std::string type_name;
  std::string parent_type_name;
  u64 flags = 0;

  std::string print_as_deftype(
      StructureType* old_game_type,
      std::unordered_map<std::string, TypeInspectorResult>& previous_results);
};

struct TypeInspectorCache {
  std::unordered_map<std::string, TypeInspectorResult> previous_results;
};

std::string inspect_inspect_method(Function& inspect_method,
                                   const std::string& type_name,
                                   DecompilerTypeSystem& dts,
                                   LinkedObjectFile& file,
                                   TypeSystem& previous_game_ts,
                                   TypeInspectorCache& ti_cache);

std::string inspect_top_level_symbol_defines(std::unordered_set<std::string>& already_seen,
                                             Function& top_level,
                                             LinkedObjectFile& file,
                                             DecompilerTypeSystem& dts,
                                             DecompilerTypeSystem& previous_game_ts);

}  // namespace decompiler