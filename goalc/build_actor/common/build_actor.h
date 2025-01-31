#pragma once

#include "common/util/gltf_util.h"

#include "goalc/build_actor/common/animation_processing.h"
#include "goalc/build_actor/common/art_types.h"
#include "goalc/build_level/collide/common/collide_common.h"

struct Joint {
  std::string name;
  s32 number;
  int parent;
  math::Matrix4f bind_pose{};

  Joint(const std::string& name, int number, int parent, math::Matrix4f bind_pose) {
    this->name = name;
    this->number = number;
    this->parent = parent;
    this->bind_pose = bind_pose;
  }

  size_t generate(DataObjectGenerator& gen) const;
};

extern std::map<int, size_t> g_joint_map;

struct BuildActorParams {
  bool gen_collide_mesh = false;
  s8 texture_bucket = -1;
  s32 texture_level = -1;
  s32 joint_channel = -1;
  math::Vector4f trans_offset = {0.f, 0.f, 0.f, 1.f};
  std::vector<float> lod_dist{};
};

struct GltfJoint {
  math::Matrix4f bind_pose_T_w;  // inverse bind pose
  std::string name;
  int gltf_node_index = 0;
  int parent = -1;
  std::vector<int> children;
};

tinygltf::Model load_gltf_model(const fs::path& path);
std::vector<GltfJoint> extract_skeleton(const tinygltf::Model& model, int skin_idx);
std::vector<Joint> convert_joints(const std::vector<GltfJoint>& gjoints);
std::vector<anim::CompressedAnim> process_anim(const tinygltf::Model& model,
                                               const std::vector<GltfJoint>& gjoints);