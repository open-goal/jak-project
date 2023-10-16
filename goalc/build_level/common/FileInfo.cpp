#include "FileInfo.h"

#include "common/versions/versions.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

size_t FileInfo::add_to_object_file(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("file-info");
  size_t offset = gen.current_offset_bytes();
  gen.add_type_tag(file_type);
  gen.add_ref_to_string_in_pool(file_name);
  gen.add_word(major_version);
  gen.add_word(minor_version);
  gen.add_ref_to_string_in_pool(maya_file_name);
  gen.add_ref_to_string_in_pool(tool_debug);
  gen.add_ref_to_string_in_pool(tool_debug);

  return offset;
}
