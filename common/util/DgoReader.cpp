#include "DgoReader.h"

#include <cstring>
#include <unordered_set>
#include <utility>

#include "BinaryReader.h"
#include "BitUtils.h"
#include "dgo_util.h"

#include "common/link_types.h"
#include "common/log/log.h"

#include "third-party/json.hpp"

DgoReader::DgoReader(std::string file_name, const std::vector<u8>& data)
    : m_file_name(std::move(file_name)) {
  BinaryReader reader(data);
  auto header = reader.read<DgoHeader>();
  m_internal_name = header.name;
  std::unordered_set<std::string> all_unique_names;

  // get all obj files...
  for (uint32_t i = 0; i < header.object_count; i++) {
    ObjectHeader obj_header = reader.read<ObjectHeader>();

    if (reader.bytes_left() < obj_header.size && i == header.object_count - 1 &&
        obj_header.size - reader.bytes_left() <= 48) {
      lg::print(
          "Warning: final file {} in DGO {} has a size missing {} bytes.  It will be adjusted from "
          "{} to {} bytes.\n",
          obj_header.name, header.name, obj_header.size - reader.bytes_left(), obj_header.size,
          (int)reader.bytes_left());
      obj_header.size = reader.bytes_left();
    }
    ASSERT(reader.bytes_left() >= obj_header.size);
    assert_string_empty_after(obj_header.name, 60);

    DgoDataEntry entry;
    entry.internal_name = obj_header.name;

    entry.unique_name = get_object_file_name(entry.internal_name, reader.here(), obj_header.size);
    if (all_unique_names.find(entry.unique_name) != all_unique_names.end()) {
      lg::print("Warning: there are multiple files named {}\n", entry.unique_name.c_str());
      entry.unique_name += '-';
      entry.unique_name += std::to_string(obj_header.size);
    }

    all_unique_names.insert(entry.unique_name);
    entry.data.resize(obj_header.size);

    ASSERT((reader.get_seek() % 16) == 0);
    memcpy(entry.data.data(), reader.here(), obj_header.size);
    m_entries.push_back(entry);

    reader.ffwd(align16(obj_header.size));
  }

  // check we're at the end
  ASSERT(0 == reader.bytes_left());
  ASSERT(all_unique_names.size() == m_entries.size());
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
