#pragma once

#include <string>
#include <vector>

namespace decompiler {

struct ObjectFileData;

struct DirTpageResult {
  std::vector<int> lengths;

  std::string to_source() const;
};

DirTpageResult process_dir_tpages(ObjectFileData& data);
}  // namespace decompiler