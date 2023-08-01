#pragma once

#include <string>
#include <unordered_map>

#include "common/util/FileUtil.h"

struct OfflineTestSourceFile {
  OfflineTestSourceFile(fs::path _path,
                        std::string _containing_dgo,
                        std::string _name_in_dgo,
                        std::string _unique_name)
      : path(_path),
        containing_dgo(_containing_dgo),
        name_in_dgo(_name_in_dgo),
        unique_name(_unique_name){};
  fs::path path;
  std::string containing_dgo;
  std::string name_in_dgo;
  std::string unique_name;
};

struct OfflineTestArtFile {
  OfflineTestArtFile(std::string _containing_dgo,
                     std::string _name_in_dgo,
                     std::string _unique_name)
      : containing_dgo(_containing_dgo), name_in_dgo(_name_in_dgo), unique_name(_unique_name){};
  std::string containing_dgo;
  std::string name_in_dgo;
  std::string unique_name;
};

std::vector<OfflineTestSourceFile> find_source_files(const std::string& game_name,
                                                     const std::vector<std::string>& dgos,
                                                     const std::string& single_file);
