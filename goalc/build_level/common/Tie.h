#pragma once

#include "Tie.h"

#include "common/custom_data/Tfrag3Data.h"
#include "common/util/gltf_util.h"

#include "goalc/build_level/common/gltf_mesh_extract.h"
#include "goalc/data_compiler/DataObjectGenerator.h"

void tie_from_gltf(const gltf_mesh_extract::TieOutput& mesh_extract_out,
                   std::vector<tfrag3::TieTree>& out_pc);

class DrawableTreeInstanceTie {
 public:
  size_t add_to_object_file(DataObjectGenerator& gen) const;
};