#pragma once

#include "tools/build_level/collide_bvh.h"
#include "tools/build_level/collide_pack.h"

class DataObjectGenerator;

struct DrawableTreeCollideFragment {
  CollideFragMeshDataArray packed_frags;
  collide::CollideTree bvh;
  size_t add_to_object_file(DataObjectGenerator& gen) const;
};
