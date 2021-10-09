#pragma once

#include <string>
#include <unordered_set>
#include <vector>

namespace decompiler {

struct ObjectFileData;
class Function;

class SymbolMapBuilder {
 public:
  void add_object(const ObjectFileData& data);
  void build_map();
  std::string convert_to_json() const;

 private:
  struct SymbolInfo {
    std::string name;
    bool is_type = false;
  };

  struct ObjectSymbolList {
    std::string object_file_name;
    std::vector<SymbolInfo> symbols;
  };

  // symbols that we've seen load/store
  std::unordered_set<std::string> m_seen_symbols;
  // symbol that we've seen used in a deftype
  std::unordered_set<std::string> m_seen_types;

  // the first place we see symbols
  std::vector<ObjectSymbolList> m_first_detections;

  // the output of this tool - a list of where symbols are "defined" meaning:
  // - if it's a type, the location of the deftype
  // - if it's a global variable, the first location where it is read or written
  // - other symbols do not appear.
  std::vector<ObjectSymbolList> m_result;

  void add_load_store_from_function(const Function& f, ObjectSymbolList* output);
  void add_deftypes_from_top_level_function(const Function& f, ObjectSymbolList* output);
};

}  // namespace decompiler