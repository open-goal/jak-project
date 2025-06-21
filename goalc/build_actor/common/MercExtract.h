#pragma once

#include "common/util/gltf_util.h"

#include "goalc/build_actor/jak1/build_actor.h"
#include "goalc/build_actor/jak2/build_actor.h"
#include "goalc/build_actor/jak3/build_actor.h"

void extract(const std::string& name,
             gltf_util::MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<gltf_util::NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset);
void merc_convert(gltf_util::MercSwapData& out, const gltf_util::MercExtractData& in);
gltf_util::MercSwapData load_merc_model(u32 current_idx_count,
                                        u32 current_vtx_count,
                                        u32 current_tex_count,
                                        const std::string& path,
                                        const std::string& name);

std::vector<jak1::CollideMesh> gen_collide_mesh_from_model_jak1(
    const tinygltf::Model& model,
    const std::vector<gltf_util::NodeWithTransform>& all_nodes,
    int joint_idx);
std::vector<jak2::CollideMesh> gen_collide_mesh_from_model_jak2(
    const tinygltf::Model& model,
    const std::vector<gltf_util::NodeWithTransform>& all_nodes,
    int joint_idx);
std::vector<jak3::CollideMesh> gen_collide_mesh_from_model_jak3(
    const tinygltf::Model& model,
    const std::vector<gltf_util::NodeWithTransform>& all_nodes,
    int joint_idx);