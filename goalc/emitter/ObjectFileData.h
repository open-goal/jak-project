#pragma once

#ifndef JAK_OBJECTFILEDATA_H
#define JAK_OBJECTFILEDATA_H

#include <array>
#include <vector>

#include "common/common_types.h"
#include "common/link_types.h"

namespace emitter {
struct ObjectFileData {
  std::array<std::vector<u8>, N_SEG> segment_data;
  std::array<std::vector<u8>, N_SEG> link_tables;
  std::vector<u8> header;
  std::vector<u8> to_vector() const;
};
}  // namespace emitter

#endif  // JAK_OBJECTFILEDATA_H
