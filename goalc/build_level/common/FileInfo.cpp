#include "FileInfo.h"

#include <chrono>

#include "common/versions/versions.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

std::string get_current_time_and_date() {
  auto const now = std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
  auto const time = std::chrono::current_zone()->to_local(now);
  return std::format("{:%a %b %d %H:%M:%S %Y}", time);
}

size_t FileInfo::add_to_object_file(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("file-info");
  size_t offset = gen.current_offset_bytes();
  gen.add_type_tag(file_type);
  gen.add_ref_to_string_in_pool(file_name);
  gen.add_word(major_version);
  gen.add_word(minor_version);
  gen.add_ref_to_string_in_pool(maya_file_name);
  gen.add_ref_to_string_in_pool(tool_debug + " " + get_current_time_and_date() + "\n");
  gen.add_ref_to_string_in_pool(mdb_file_name);

  return offset;
}
