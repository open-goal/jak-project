#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "common/common_types.h"

namespace listener {

struct LoadEntry {
  uint32_t segments[3] = {0, 0, 0};
  uint32_t segment_sizes[3] = {0, 0, 0};
  std::string load_string;
  std::string print() const;

  bool overlaps_with(const LoadEntry& other) const;
};

struct MemoryMapEntry {
  u32 start_addr = 0;
  u32 end_addr = 0;
  std::string obj_name;
  u8 seg_id = 0;
  bool empty = false;
  std::string print() const;
};

class MemoryMap {
 public:
  MemoryMap() = default;
  explicit MemoryMap(const std::unordered_map<std::string, LoadEntry>& load_entries);
  explicit MemoryMap(std::vector<MemoryMapEntry> entries) : m_entries(std::move(entries)) {}
  std::string print() const;
  const MemoryMapEntry& lookup(u32 addr);
  bool lookup(const std::string& obj_name, u8 seg_id, MemoryMapEntry* out);

 private:
  std::vector<MemoryMapEntry> m_entries;
};
}  // namespace listener
