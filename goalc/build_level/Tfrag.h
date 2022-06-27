#pragma once

#include <string>

#include "common/custom_data/Tfrag3Data.h"

#include "goalc/build_level/TexturePool.h"
#include "goalc/build_level/gltf_mesh_extract.h"

class DataObjectGenerator;

struct DrawableTreeTfrag {
  size_t add_to_object_file(DataObjectGenerator& gen) const;
};

void tfrag_from_gltf(const gltf_mesh_extract::TfragOutput& mesh_extract_out,
                     DrawableTreeTfrag& out,
                     tfrag3::TfragTree& out_pc);