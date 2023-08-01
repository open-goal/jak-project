#pragma once

#include <string>

#include "goalc/build_level/Entity.h"

class DataObjectGenerator;

struct DrawableAmbient {
  EntityAmbient ambient;
};

struct DrawableTreeAmbient {
  size_t add_to_object_file(DataObjectGenerator& gen,
                            size_t ambient_count,
                            size_t ambient_arr_slot) const;
};