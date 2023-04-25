#pragma once

#include <string>
#include <unordered_map>

#include "decompiler/Function/Function.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

struct TypeInspectorResult {
  bool success = false;
  int type_size = -1;
  int type_method_count = -1;
  int parent_method_count = 9;
  std::optional<int> type_heap_base = {};

  std::string warnings;
  std::vector<Field> fields_of_type;
  bool is_basic = false;
  bool found_flags = false;

  std::string type_name;
  std::string parent_type_name;
  u64 flags;

  std::string print_as_deftype(
      StructureType* old_game_type,
      std::unordered_map<std::string, TypeInspectorResult>& previous_results,
      DecompilerTypeSystem& previous_game_ts,
      ObjectFileDB::PerObjectAllTypeInfo& object_file_meta);
};

struct TypeInspectorCache {
  std::unordered_map<std::string, TypeInspectorResult> previous_results;
};

std::string inspect_inspect_method(Function& inspect_method,
                                   const std::string& type_name,
                                   DecompilerTypeSystem& dts,
                                   LinkedObjectFile& file,
                                   DecompilerTypeSystem& previous_game_ts,
                                   TypeInspectorCache& ti_cache,
                                   ObjectFileDB::PerObjectAllTypeInfo& object_file_meta);

void inspect_top_level_for_metadata(Function& top_level,
                                    LinkedObjectFile& file,
                                    DecompilerTypeSystem& dts,
                                    DecompilerTypeSystem& previous_game_ts,
                                    ObjectFileDB::PerObjectAllTypeInfo& object_file_meta);

std::string inspect_top_level_symbol_defines(Function& top_level,
                                             LinkedObjectFile& file,
                                             DecompilerTypeSystem& dts,
                                             DecompilerTypeSystem& previous_game_ts,
                                             ObjectFileDB::PerObjectAllTypeInfo& object_file_meta);

}  // namespace decompiler
