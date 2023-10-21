#pragma once

#include <string>

#include "gltf_mesh_extract.h"

#include "common/custom_data/Tfrag3Data.h"

#include "goalc/build_level/common/TexturePool.h"

class DataObjectGenerator;

struct DrawableTreeTfrag {
  size_t add_to_object_file(DataObjectGenerator& gen) const;
};

void tfrag_from_gltf(const gltf_mesh_extract::TfragOutput& mesh_extract_out,
                     DrawableTreeTfrag& out,
                     tfrag3::TfragTree& out_pc);