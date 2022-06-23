#pragma once

#include <string>
#include <vector>

#include "common/common_types.h"

struct DgoDataEntry {
  std::vector<u8> data;
  std::string internal_name;
  std::string unique_name;
};

class DgoReader {
 public:
  DgoReader(std::string file_name, const std::vector<u8>& data);
  const std::vector<DgoDataEntry> entries() const { return m_entries; }
  std::string description_as_json() const;

 private:
  std::vector<DgoDataEntry> m_entries;
  std::string m_internal_name, m_file_name;
};