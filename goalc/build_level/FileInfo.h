#pragma once

#include <string>

#include "common/common_types.h"

class DataObjectGenerator;

struct FileInfo {
  //  (file-type      symbol  :offset-assert 4)
  std::string file_type;
  //  (file-name      basic   :offset-assert 8)
  std::string file_name;
  //  (major-version  uint32  :offset-assert 12)
  u32 major_version = 0;
  //  (minor-version  uint32  :offset-assert 16)
  u32 minor_version = 0;
  //  (maya-file-name basic   :offset-assert 20)
  std::string maya_file_name;
  //  (tool-debug     basic   :offset-assert 24)
  std::string tool_debug;
  //  (mdb-file-name  basic   :offset-assert 28)
  std::string mdb_file_name;

  size_t add_to_object_file(DataObjectGenerator& gen) const;
};

FileInfo make_file_info_for_level(const std::string& file_name);