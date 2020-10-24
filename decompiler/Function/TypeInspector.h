#pragma once

/*!
 * @file TypeInspector.h
 * Analyze an auto-generated GOAL inspect method to determine the layout of a type in memory.
 */

#include <vector>
#include "common/type_system/Type.h"

class Function;
class DecompilerTypeSystem;
class LinkedObjectFile;

struct TypeInspectorResult {
  bool success = false;
  int type_size = -1;
  int type_method_count = -1;
  int type_heap_base = -1;

  std::string warnings;
  std::vector<Field> fields_of_type;
  bool is_basic = false;

  std::string type_name;
  std::string parent_type_name;
  u64 flags = 0;

  std::string print_as_deftype();
};

TypeInspectorResult inspect_inspect_method(Function& inspect,
                                           const std::string& type_name,
                                           DecompilerTypeSystem& dts,
                                           LinkedObjectFile& file);
