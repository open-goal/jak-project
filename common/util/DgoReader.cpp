#include <cstring>
#include <utility>
#include <unordered_set>
#include "DgoReader.h"
#include "BinaryReader.h"
#include "common/link_types.h"
#include "third-party/json.hpp"
#include "dgo_util.h"

DgoReader::DgoReader(std::string file_name, const std::vector<u8>& data)
    : m_file_name(std::move(file_name)) {
  BinaryReader reader(data);
  auto header = reader.read<DgoHeader>();
  m_internal_name = header.name;
  std::unordered_set<std::string> all_unique_names;

  // get all obj files...
  for (uint32_t i = 0; i < header.object_count; i++) {
    auto obj_header = reader.read<ObjectHeader>();
    assert(reader.bytes_left() >= obj_header.size);
    assert_string_empty_after(obj_header.name, 60);

    DgoDataEntry entry;
    entry.internal_name = obj_header.name;

    entry.unique_name = get_object_file_name(entry.internal_name, reader.here(), obj_header.size);
    if (all_unique_names.find(entry.unique_name) != all_unique_names.end()) {
      printf("Warning: there are multiple files named %s\n", entry.unique_name.c_str());
      entry.unique_name += '-';
      entry.unique_name += std::to_string(obj_header.size);
    }

    all_unique_names.insert(entry.unique_name);
    entry.data.resize(obj_header.size);

    assert((reader.get_seek() % 16) == 0);
    memcpy(entry.data.data(), reader.here(), obj_header.size);
    m_entries.push_back(entry);

    reader.ffwd(obj_header.size);
  }

  // check we're at the end
  assert(0 == reader.bytes_left());
  assert(all_unique_names.size() == m_entries.size());
}

std::string DgoReader::description_as_json() const {
  using namespace nlohmann;
  json j;
  j["file_name"] = m_file_name;
  j["internal_name"] = m_internal_name;
  for (auto& entry : m_entries) {
    json entry_desc;
    entry_desc["unique_name"] = entry.unique_name;
    entry_desc["internal_name"] = entry.internal_name;
    j["objects"].push_back(entry_desc);
  }

  return j.dump(4);
}