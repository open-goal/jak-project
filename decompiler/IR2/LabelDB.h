#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/link_types.h"
#include "common/type_system/TypeSpec.h"

#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/config.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
struct LabelInfo {
  std::string name;
  TypeSpec result_type;
  int idx = -1;
  bool known = false;
  bool is_value = false;
  bool from_user = false;
  std::optional<int> array_size;

  std::string print() const;
};

class LabelDB {
 public:
  LabelDB(const std::unordered_map<std::string, LabelConfigInfo>& config,
          const std::vector<DecompilerLabel>& labels,
          const DecompilerTypeSystem& dts);
  const LabelInfo& lookup(int idx) const;
  const LabelInfo& lookup(const std::string& name) const;
  int get_index_by_offset(int seg, int offset) const;
  std::optional<int> try_get_index_by_offset(int seg, int offset) const;
  int get_index_by_name(const std::string& name) const;

  // automatic, or from user
  bool label_info_known_by_name(const std::string& name) const;

  LabelInfo set_and_get_previous(int idx,
                                 const TypeSpec& type,
                                 bool is_value,
                                 std::optional<int> array_size);

 private:
  std::vector<LabelInfo> m_info;
  std::unordered_map<std::string, int> m_labels_by_name;
  std::vector<std::unordered_map<int, int>> m_labels_by_offset_into_seg;  // in bytes.
};
}  // namespace decompiler
