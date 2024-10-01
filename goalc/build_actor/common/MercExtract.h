#pragma once

#include "common/util/gltf_util.h"

#include "goalc/build_actor/jak1/build_actor.h"

struct MercExtractData {
  gltf_util::TexturePool tex_pool;
  std::vector<u32> new_indices;
  std::vector<tfrag3::PreloadedVertex> new_vertices;
  std::vector<math::Vector<u8, 4>> new_colors;
  std::vector<math::Vector3f> normals;
  std::vector<gltf_util::JointsAndWeights> joints_and_weights;
  tfrag3::MercModel new_model;
};

// Data produced by loading a replacement model
struct MercSwapData {
  std::vector<u32> new_indices;
  std::vector<tfrag3::MercVertex> new_vertices;
  std::vector<tfrag3::Texture> new_textures;
  tfrag3::MercModel new_model;
};

void extract(const std::string& name,
             MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<gltf_util::NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset);
void merc_convert(MercSwapData& out, const MercExtractData& in);
MercSwapData load_merc_model(u32 current_idx_count,
                             u32 current_vtx_count,
                             u32 current_tex_count,
                             const std::string& path,
                             const std::string& name);
std::vector<jak1::CollideMesh> gen_collide_mesh_from_model(
    const tinygltf::Model& model,
    const std::vector<gltf_util::NodeWithTransform>& all_nodes,
    int joint_idx);