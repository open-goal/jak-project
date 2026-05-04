#pragma once
#include "common/common_types.h"
#include "common/math/Vector.h"

namespace level_tools {

// levels may remap textures if they provide one that should be shared
struct TextureRemap {
  u32 original_texid;
  u32 new_texid;
};

struct Joint {
  std::string name;
  int parent_idx = -1;  // -1 for magic ROOT joint.
  math::Matrix4f bind_pose_T_w;
};

struct UncompressedSingleJointAnim {
  std::vector<math::Vector3f> trans_frames;
  std::vector<math::Vector3f> scale_frames;
  std::vector<math::Vector4f> quat_frames;
};

struct UncompressedJointAnim {
  std::string name;
  std::vector<UncompressedSingleJointAnim> joints;
  float framerate = 30;
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

struct JointAnimCompressedHDR {
  u32 control_bits[14];
  u32 num_joints;
  u32 matrix_bits;

  JointAnimCompressedHDR() {
    for (auto& bit : control_bits) {
      bit = 0;
    }
    num_joints = 1;
    matrix_bits = 0;
  }
};

struct JointAnimCompressedFixed {
  JointAnimCompressedHDR hdr;
  u32 offset_64;
  u32 offset_32;
  u32 offset_16;
  u32 reserved;
  math::Vector4f data[133];
  int num_data_qw_used = 0;
  bool mat[2] = {false, false};
  math::Matrix4f mats[2] = {math::Matrix4f::zero(), math::Matrix4f::zero()};
  u64 data64_size;
  u32 data32_size;
  u16 data16_size;
  std::vector<u64> data64;
  std::vector<u32> data32;
  std::vector<u16> data16;

  JointAnimCompressedFixed() {
    offset_64 = 0;
    offset_32 = 0;
    offset_16 = 0;
    reserved = 0;
    data[0] = math::Vector4f(1.0f, 0.0f, 0.0f, 0.0f);
    data[1] = math::Vector4f(0.0f, 1.0f, 0.0f, 0.0f);
    data[2] = math::Vector4f(0.0f, 0.0f, 1.0f, 0.0f);
    data[3] = math::Vector4f(0.0f, 0.0f, 0.0f, 1.0f);
    data[4] = math::Vector4f(1.0f, 0.0f, 0.0f, 0.0f);
    data[5] = math::Vector4f(0.0f, 1.0f, 0.0f, 0.0f);
    data[6] = math::Vector4f(0.0f, 0.0f, 1.0f, 0.0f);
    data[7] = math::Vector4f(0.0f, 0.0f, 0.0f, 1.0f);
    num_data_qw_used = 8;
  }
};

struct JointAnimCompressedFrame {
  u32 offset_64;
  u32 offset_32;
  u32 offset_16;
  u32 reserved;
  // math::Vector4f data[133];
  u32 num_data_qw_used = 0;
  bool mat[2] = {false, false};
  math::Matrix4f mats[2] = {math::Matrix4f::zero(), math::Matrix4f::zero()};
  u64 data64_size;
  u32 data32_size;
  u16 data16_size;
  std::vector<u64> data64;
  std::vector<u32> data32;
  std::vector<u16> data16;

  JointAnimCompressedFrame() {
    offset_64 = 0;
    offset_32 = 0;
    offset_16 = 0;
    reserved = 0;
  }
};

struct JointAnimCompressedControl {
  u32 num_frames;
  u32 fixed_qwc;
  u32 frame_qwc;
  JointAnimCompressedFixed fixed;
  std::vector<JointAnimCompressedFrame> frame;
};

struct ArtJointAnim {
  std::string name;
  float speed;
  float artist_base;
  float artist_step;
  s16 length;
  JointAnimCompressedControl frames;
};

/*!
 * Data extracted from art groups that is not needed for .FR3, but is potentially needed for other
 * stuff (skeleton export).
 */
struct ArtData {
  std::string art_group_name;
  std::string art_name;
  std::vector<Joint> joint_group;
  std::vector<ArtJointAnim> anims;
};

}  // namespace level_tools