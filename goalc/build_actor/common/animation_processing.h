#pragma once

#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/math/Vector.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

namespace anim {

struct UncompressedSingleJointAnim {
  std::vector<math::Vector3f> trans_frames;
  std::vector<math::Vector3f> scale_frames;
  std::vector<math::Vector4f> quat_frames;
};

struct UncompressedJointAnim {
  std::string name;
  std::vector<UncompressedSingleJointAnim> joints;
  float framerate = 60;
  int frames = 0;
};

struct CompressedMatrixMetadata {
  bool is_animated[2];
};

struct CompressedFrame {
  std::vector<u16> data16;
  std::vector<u32> data32;
  std::vector<u64> data64;

  int size_bytes() const { return data16.size() * 2 + data32.size() * 4 + data64.size() * 8; }
};

struct CompressedJointMetadata {
  bool animated_trans = false;
  bool animated_quat = false;
  bool animated_scale = false;
  bool big_trans_mode = false;
};

struct CompressedAnim {
  std::string name;
  CompressedFrame fixed;
  std::vector<CompressedFrame> frames;
  bool matrix_animated[2] = {false, false};
  std::vector<CompressedJointMetadata> joint_metadata;
  float framerate = 60;
};

/*!
 * Load animation data from GLTF file.
 * @param model The GLTF model containing the animation
 * @param anim The animation to convert
 * @param node_to_joint Mapping from GLTF node index to the joint index
 * @param framerate Number of key-frames per second. (this doesn't have to match frame rate, the
 * game will interpolate between keyframes as needed.)
 */
UncompressedJointAnim extract_anim_from_gltf(const tinygltf::Model& model,
                                             const tinygltf::Animation& anim,
                                             const std::map<int, int>& node_to_joint,
                                             float framerate);

CompressedAnim compress_animation(const UncompressedJointAnim& in);

}  // namespace anim