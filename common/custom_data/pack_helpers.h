#pragma once

#include "common/custom_data/Tfrag3Data.h"

void pack_tfrag_vertices(tfrag3::PackedTfragVertices* result,
                         const std::vector<tfrag3::PreloadedVertex>& vertices);