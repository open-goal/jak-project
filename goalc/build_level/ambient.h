#pragma once

#include <cstddef>

class DataObjectGenerator;

struct DrawableTreeAmbient {
  static size_t add_to_object_file(DataObjectGenerator& gen, size_t ambient_array);
};