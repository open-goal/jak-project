#include "LabelDB.h"

#include "fmt/core.h"

namespace decompiler {

std::string LabelInfo::print() const {
  if (!known) {
    return fmt::format("{} unknown", name);
  }

  std::string result = fmt::format("{} {} ", name, result_type.print());
  if (is_value) {
    result += "value ";
  } else {
    result += "ref ";
  }

  if (from_user) {
    result += "from-config ";
  } else {
    result += "auto-detected ";
  }

  if (array_size) {
    result += fmt::format("sz: {}", *array_size);
  }

  return result;
}

LabelDB::LabelDB(const std::unordered_map<std::string, LabelConfigInfo>& config,
                 const std::vector<DecompilerLabel>& labels,
                 const DecompilerTypeSystem& dts) {
  m_labels_by_offset_into_seg.resize(N_SEG);
  // first, copy all labels.
  for (size_t i = 0; i < labels.size(); i++) {
    const auto& existing_info = labels[i];
    LabelInfo info;
    info.name = existing_info.name;
    info.idx = (int)i;
    m_info.push_back(info);
    if (!m_labels_by_name.insert({info.name, i}).second) {
      throw std::runtime_error(
          fmt::format("Label {} appears multiple times, cannot build LabelDB.", info.name));
    }
    m_labels_by_offset_into_seg.at(existing_info.target_segment)[existing_info.offset] = (int)i;
  }

  ASSERT(m_labels_by_name.size() == labels.size());
  size_t total_from_offsets = 0;
  for (int i = 0; i < N_SEG; i++) {
    total_from_offsets += m_labels_by_offset_into_seg[i].size();
  }
  ASSERT(total_from_offsets == labels.size());

  // now config
  for (const auto& config_it : config) {
    const auto& info_it = m_labels_by_name.find(config_it.first);
    if (info_it == m_labels_by_name.end()) {
      throw std::runtime_error(
          fmt::format("Config has an entry for label {}, but it does not exist.", config_it.first));
    }
    auto& info = m_info.at(info_it->second);
    if (info.from_user) {
      throw std::runtime_error(
          fmt::format("Config has multiple entries for label {}.", config_it.first));
    }
    info.from_user = true;
    info.known = true;
    info.result_type = dts.parse_type_spec(config_it.second.type_name);
    info.is_value = config_it.second.is_value;
    info.array_size = config_it.second.array_size;
  }
}

const LabelInfo& LabelDB::lookup(int idx) const {
  return m_info.at(idx);
}

const LabelInfo& LabelDB::lookup(const std::string& name) const {
  return lookup(m_labels_by_name.at(name));
}

LabelInfo LabelDB::set_and_get_previous(int idx,
                                        const TypeSpec& type,
                                        bool is_value,
                                        std::optional<int> array_size) {
  LabelInfo result = m_info.at(idx);

  LabelInfo& mod = m_info.at(idx);
  mod.result_type = type;
  mod.is_value = is_value;
  mod.array_size = array_size;
  mod.from_user = false;
  mod.known = true;

  return result;
}

int LabelDB::get_index_by_offset(int seg, int offset) const {
  return m_labels_by_offset_into_seg.at(seg).at(offset);
}

std::optional<int> LabelDB::try_get_index_by_offset(int seg, int offset) const {
  auto it = m_labels_by_offset_into_seg.at(seg).find(offset);
  if (it == m_labels_by_offset_into_seg.at(seg).end()) {
    return {};
  } else {
    return it->second;
  }
}

int LabelDB::get_index_by_name(const std::string& name) const {
  return m_labels_by_name.at(name);
}

bool LabelDB::label_info_known_by_name(const std::string& name) const {
  auto it = m_labels_by_name.find(name);
  if (it == m_labels_by_name.end()) {
    return false;
  }
  return lookup(it->second).known;
}
}  // namespace decompiler
