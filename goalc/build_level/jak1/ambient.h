#pragma once

#include <cstddef>

class DataObjectGenerator;

namespace jak1 {
struct DrawableTreeAmbient {
  static size_t add_to_object_file(DataObjectGenerator& gen, size_t ambient_array);
};
}  // namespace jak1
