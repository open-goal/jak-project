#pragma once

#include <string>

#include "common/custom_data/Tfrag3Data.h"
#include "tools/build_level/TexturePool.h"

class DataObjectGenerator;

struct DrawableTreeTfrag {
  size_t add_to_object_file(DataObjectGenerator& gen) const;
};

void tfrag_from_gltf(const std::string& filename,
                     DrawableTreeTfrag& out,
                     tfrag3::TfragTree& out_pc,
                     TexturePool* tex_pool);