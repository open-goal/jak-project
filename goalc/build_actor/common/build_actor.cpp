#include "build_actor.h"

#include "common/log/log.h"
#include "common/math/geometry.h"

#include "goalc/build_actor/common/MercExtract.h"
#include "goalc/build_actor/common/animation_processing.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

using namespace gltf_util;

std::map<int, size_t> g_joint_map;

size_t Joint::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("joint");
  size_t result = gen.current_offset_bytes();
  gen.add_ref_to_string_in_pool(name);
  gen.add_word(number);
  if (parent == -1) {
    gen.add_symbol_link("#f");
  } else {
    gen.link_word_to_byte(gen.add_word(0), g_joint_map[parent]);
  }
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      gen.add_word_float(bind_pose(i, j));
    }
  }
  return result;
}

/*!
 * Load tinygltf::Model from a .glb file (binary format), fatal error if it fails.
 */
tinygltf::Model load_gltf_model(const fs::path& path) {
  tinygltf::TinyGLTF loader;
  tinygltf::Model model;
  std::string err, warn;
  bool res = loader.LoadBinaryFromFile(&model, &err, &warn, path.string());
  ASSERT_MSG(warn.empty(), warn.c_str());
  ASSERT_MSG(err.empty(), err.c_str());
  ASSERT_MSG(res, "Failed to load GLTF file!");
  return model;
}

/*!
 * Extract the "skeleton" structure from a GLTF model's skin. This requires that the skin's joints
 * are topologically sorted (parents always have lower index than children).
 */
std::vector<GltfJoint> extract_skeleton(const tinygltf::Model& model, int skin_idx) {
  const auto& skin = model.skins.at(skin_idx);
  lg::info("skin name is {}", skin.name);
  lg::info("skeleton root is {}", skin.skeleton);
  auto inverse_bind_matrices = extract_mat4(model, skin.inverseBindMatrices);
  ASSERT(inverse_bind_matrices.size() == skin.joints.size());

  std::map<int, int> node_to_joint;
  std::map<int, int> joint_to_node;
  std::vector<GltfJoint> joints;

  for (size_t i = 0; i < skin.joints.size(); i++) {
    auto joint_node_idx = skin.joints[i];
    const auto& joint_node = model.nodes.at(joint_node_idx);
    // auto ibm = inverse_bind_matrices[i];
    // lg::info(" joint {}", joint_node_idx);
    // lg::info("  {}", joint_node.name);
    // lg::info("\n{}", ibm.to_string_aligned());
    node_to_joint[joint_node_idx] = i;
    joint_to_node[i] = joint_node_idx;

    auto& gjoint = joints.emplace_back();
    gjoint.bind_pose_T_w = inverse_bind_matrices[i];
    gjoint.name = joint_node.name;
    gjoint.gltf_node_index = joint_node_idx;
  }

  for (size_t i = 0; i < skin.joints.size(); i++) {
    auto joint_node_idx = skin.joints[i];
    const auto& joint_node = model.nodes.at(joint_node_idx);

    // set up children
    for (int child_node_idx : joint_node.children) {
      int child_joint_idx = node_to_joint.at(child_node_idx);
      joints.at(i).children.push_back(child_joint_idx);
      auto& child = joints.at(child_joint_idx);
      ASSERT(child.parent == -1);
      child.parent = i;
      ASSERT(child_joint_idx > (int)i);
    }
  }
  ASSERT(joints.at(0).parent == -1);

  // for (auto& joint : joints) {
  //   if (joint.parent == -1) {
  //     lg::warn("parentless {}", joint.name);
  //   } else {
  //     lg::info("joint {}, child of {}", joint.name, joints.at(joint.parent).name);
  //   }
  // }
  lg::info("total of {} joints", joints.size());
  return joints;
}

/*!
 * Convert from GLTF joint format to game joint format.
 * @param joint_index the index of the joint, in the GLTF file.
 * @param prefix_joint_count number of joints to be inserted before GLTF joints in the game
 * @param parent_of_gltf the parent game joint of all GLTF joints.
 */
Joint convert_joint(const GltfJoint& joint,
                    int joint_index,
                    int prefix_joint_count,
                    int parent_of_gltf) {
  // node matrix is p_T_myself
  // p_T_myself = parent_bind_pose_T_w * w_T_bind_pose
  int parent;
  if (joint.parent == -1) {
    parent = parent_of_gltf;
  } else {
    parent = joint.parent + prefix_joint_count;
  }
  math::Matrix4f fixed_matrix = joint.bind_pose_T_w;

  for (int i = 0; i < 3; i++) {
    fixed_matrix(i, 3) *= 4096;
  }

  return Joint(joint.name, joint_index + prefix_joint_count, parent, fixed_matrix.transposed());
}

constexpr int kGltfToGameJointOffset = 1;
/*!
 * Convert GTLF joint list to game joint list.
 * Currently, this inserts a single "align" joint and places the root joint of the GLTF as the
 * prejoint. However, we might want to change this, to allow GLTF files to specify "align" at some
 * point.
 */
std::vector<Joint> convert_joints(const std::vector<GltfJoint>& gjoints) {
  std::vector<Joint> joints;
  joints.emplace_back("align", 0, -1, math::Matrix4f::identity());
  ASSERT(kGltfToGameJointOffset == joints.size());
  for (int gjoint_idx = 0; gjoint_idx < int(gjoints.size()); gjoint_idx++) {
    // using -1 as the parent index since gltf's shouldn't be child of align.
    joints.push_back(convert_joint(gjoints[gjoint_idx], gjoint_idx, kGltfToGameJointOffset, -1));
  }

  return joints;
}

std::vector<anim::CompressedAnim> process_anim(const tinygltf::Model& model,
                                               const std::vector<GltfJoint>& gjoints) {
  if (model.animations.empty()) {
    lg::warn("no animations detected!");  // TODO: make up a dummy one
    return {};
  }

  std::map<int, int> node_to_joint;
  for (size_t i = 0; i < gjoints.size(); i++) {
    node_to_joint[gjoints[i].gltf_node_index] = i + kGltfToGameJointOffset;
  }

  std::vector<anim::CompressedAnim> ret;
  for (auto& anim : model.animations) {
    lg::info("Processing animation {}", anim.name);
    ret.push_back(
        anim::compress_animation(anim::extract_anim_from_gltf(model, anim, node_to_joint, 60)));
  }
  return ret;
}