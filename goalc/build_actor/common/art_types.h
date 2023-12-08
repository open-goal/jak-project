#pragma once

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/level_extractor/common_formats.h"
#include "goalc/build_level/common/Entity.h"
#include "goalc/build_level/common/FileInfo.h"
#include "goalc/data_compiler/DataObjectGenerator.h"

// basic
struct Joint {
  std::string name;
  int parent = -1;
  math::Matrix4f bind_pose;
};

// basic
struct JointAnim {
  std::string name;
  s16 number;
  s16 length;
};

// basic
struct JointAnimCompressed : JointAnim {
  std::vector<u32> data;
};

struct JointAnimFrame {
  math::Matrix4f matrices[2];
  std::vector<math::Matrix4f> data;
};

struct JointAnimCompressedHDR {
  u32 control_bits[14];
  u32 num_joints;
  u32 matrix_bits;
};

struct JointAnimCompressedFixed {
  JointAnimCompressedHDR hdr;
  u32 offset_64;
  u32 offset_32;
  u32 offset_16;
  u32 reserved;
  math::Vector4f data[133];
};

struct JointAnimCompressedFrame {
  u32 offset_64;
  u32 offset_32;
  u32 offset_16;
  u32 reserved;
  math::Vector4f data[133];
};

struct JointAnimCompressedControl {
  u32 num_frames;
  u32 fixed_qwc;
  u32 frame_qwc;
  JointAnimCompressedFixed fixed;
  JointAnimCompressedFrame frame[1];
};

struct Art {
  std::string name;
  s32 length;
  ResLump* lump;
};

struct ArtElement : Art {
  u8 pad[12];
};

struct ArtJointGeo : ArtElement {
  std::vector<Joint> data;
};

struct MercEyeAnimFrame {
  s8 pupil_trans_x;
  s8 pupil_trans_y;
  s8 blink;
  s8 iris_scale;
  s8 pupil_scale;
  s8 lid_scale;
};

struct MercEyeAnimBlock {
  s16 max_frame;
  std::vector<MercEyeAnimFrame> frames;
};

struct ArtJointAnim : ArtElement {
  MercEyeAnimBlock eye_anim_data;
  float speed;
  float artist_base;
  float artist_step;
  std::string master_art_group_name;
  s32 master_art_group_index;
  u8* blerc_data;
  JointAnimCompressedControl frames;
  std::vector<JointAnimCompressed> data;
};