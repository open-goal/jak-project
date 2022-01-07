#include "Loader.h"
#include "common/util/Timer.h"
#include "common/util/FileUtil.h"

namespace {
std::string uppercase_string(const std::string& s) {
  std::string result;
  for (auto c : s) {
    result.push_back(toupper(c));
  }
  return result;
}
}  // namespace

tfrag3::Level* Loader::get_tfrag3_level(const std::string& level_name) {
  const auto& existing = m_tfrag3_levels.find(level_name);
  if (existing == m_tfrag3_levels.end()) {
    fmt::print("Loader needs to load tfrag3 level: {}\n", level_name);
    Timer disk_timer;
    auto data = file_util::read_binary_file(
        file_util::get_file_path({fmt::format("assets/{}.fr3", uppercase_string(level_name))}));
    double disk_load_time = disk_timer.getSeconds();

    Timer import_timer;
    auto& result = m_tfrag3_levels[level_name];
    Serializer ser(data.data(), data.size());
    result.serialize(ser);
    double import_time = import_timer.getSeconds();
    fmt::print("------------> Load from file: {:.3f}s, import {:.3f}s\n", disk_load_time,
               import_time);
    return &result;
  } else {
    return &existing->second;
  }
}