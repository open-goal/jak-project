#include "FileInfo.h"

#include "common/versions/versions.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

namespace jak3 {
FileInfo make_file_info_for_level(const std::string& file_name) {
  FileInfo info;
  info.file_type = "bsp-header";
  info.file_name = file_name;
  info.major_version = versions::jak3::LEVEL_FILE_VERSION;
  info.minor_version = 0;
  info.maya_file_name = "Unknown";
  info.tool_debug = "Created by OpenGOAL buildlevel";
  info.mdb_file_name = "Unknown";
  return info;
}
}  // namespace jak3
