#pragma once

#include "BspHeader.h"

#include "common/custom_data/Tfrag3Data.h"

namespace decompiler {

void extract_collide_frags(const level_tools::DrawableTreeCollideFragment* tree,
                           const std::vector<const level_tools::DrawableTreeInstanceTie*>& ties,
                           const Config& config,
                           const std::string& debug_name,
                           tfrag3::Level& out);

void extract_collide_frags(const level_tools::CollideHash& chash,
                           const std::vector<const level_tools::DrawableTreeInstanceTie*>& ties,
                           const Config& config,
                           const std::string& debug_name,
                           const decompiler::DecompilerTypeSystem& dts,
                           tfrag3::Level& out);

void set_vertices_for_tri(tfrag3::CollisionMesh::Vertex* out, const math::Vector4f* in);
}  // namespace decompiler
