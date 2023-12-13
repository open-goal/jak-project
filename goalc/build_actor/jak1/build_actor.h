#pragma once

#include "common/util/gltf_util.h"

#include "goalc/build_actor/common/art_types.h"

namespace jak1 {

struct MercExtractData {
  gltf_util::TexturePool tex_pool;
  std::vector<u32> new_indices;
  std::vector<tfrag3::PreloadedVertex> new_vertices;
  std::vector<math::Vector<u8, 4>> new_colors;

  tfrag3::MercModel new_model;
};

// Data produced by loading a replacement model
struct MercSwapData {
  std::vector<u32> new_indices;
  std::vector<tfrag3::MercVertex> new_vertices;
  std::vector<tfrag3::Texture> new_textures;
  tfrag3::MercModel new_model;
};

void extract(MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<gltf_util::NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset);

MercSwapData load_merc_model(u32 current_idx_count,
                             u32 current_vtx_count,
                             u32 current_tex_count,
                             const std::string& path);

struct Joint {
  std::string name;
  s32 number = 0;
  int parent = -1;
  math::Matrix4f bind_pose = math::Matrix4f::identity();

  size_t generate(DataObjectGenerator& gen) const;
};

// basic
struct JointAnim {
  std::string name;
  s16 number;
  s16 length;

  explicit JointAnim(const std::string& name) {
    this->name = name;
    number = 0;
    length = 1;
  }

  // size_t generate(DataObjectGenerator& gen) const;
};

// basic
struct JointAnimCompressed : JointAnim {
  std::vector<u32> data;
  // size_t generate(DataObjectGenerator& gen) const;
};

struct JointAnimFrame {
  math::Matrix4f matrices[2];
  std::vector<math::Matrix4f> data;

  // size_t generate(DataObjectGenerator& gen) const;
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
  size_t generate(DataObjectGenerator& gen) const;
};

struct JointAnimCompressedFixed {
  JointAnimCompressedHDR hdr;
  u32 offset_64;
  u32 offset_32;
  u32 offset_16;
  u32 reserved;
  math::Vector4f data[133];

  JointAnimCompressedFixed() {
    offset_64 = 0;
    offset_32 = 0x88;
    offset_16 = 0x90;
    reserved = 0;
    data[0] = math::Vector4f(1.0f, 0.0f, 0.0f, 0.0f);
    data[1] = math::Vector4f(0.0f, 1.0f, 0.0f, 0.0f);
    data[2] = math::Vector4f(0.0f, 0.0f, 1.0f, 0.0f);
    data[3] = math::Vector4f(0.0f, 0.0f, 0.0f, 1.0f);
    data[4] = math::Vector4f(1.0f, 0.0f, 0.0f, 0.0f);
    data[5] = math::Vector4f(0.0f, 1.0f, 0.0f, 0.0f);
    data[6] = math::Vector4f(0.0f, 0.0f, 1.0f, 0.0f);
    data[7] = math::Vector4f(0.0f, 0.0f, 0.0f, 1.0f);
    data[8] = math::Vector4f(0.0f, 7.0f, 0.0f, 1.0f);
    data[9] = math::Vector4f(1.0f, 0.0f, 0.0f, 0.0f);
  }
  size_t generate(DataObjectGenerator& gen) const;
};

struct JointAnimCompressedFrame {
  u32 offset_64;
  u32 offset_32;
  u32 offset_16;
  u32 reserved;
  math::Vector4f data[133];

  JointAnimCompressedFrame() {
    offset_64 = 0;
    offset_32 = 0;
    offset_16 = 0;
    reserved = 0;
  }

  size_t generate(DataObjectGenerator& gen) const;
};

struct JointAnimCompressedControl {
  u32 num_frames;
  u32 fixed_qwc;
  u32 frame_qwc;
  JointAnimCompressedFixed fixed{};
  JointAnimCompressedFrame frame[1];

  JointAnimCompressedControl() {
    num_frames = 1;
    fixed_qwc = 3;
    frame_qwc = 1;
    fixed = JointAnimCompressedFixed();
  }

  size_t generate(DataObjectGenerator& gen) const;
};

struct ArtJointGeo : ArtElement {
  std::vector<Joint> data;

  explicit ArtJointGeo(const std::string& name) {
    this->name = name + "-lod0";
    length = 1;
    Joint align;
    align.name = "align";
    data.push_back(align);
  }
  size_t generate(DataObjectGenerator& gen) const;
};

struct ArtJointAnim : ArtElement {
  MercEyeAnimBlock eye_anim_data;
  float speed;
  float artist_base;
  float artist_step;
  std::string master_art_group_name;
  s32 master_art_group_index;
  u8* blerc_data = nullptr;
  JointAnimCompressedControl frames;
  std::vector<JointAnimCompressed> data;

  explicit ArtJointAnim(const std::string& name) {
    this->name = name + "-idle";
    speed = 1.0f;
    artist_base = 0.0f;
    artist_step = 1.0f;
    master_art_group_name = name;
    master_art_group_index = 2;
    frames = JointAnimCompressedControl();
  }
  size_t generate(DataObjectGenerator& gen) const;
};

struct ArtGroup : Art {
  GameVersion version;
  FileInfo info;
  std::vector<ArtElement*> elts;

  ArtGroup(const std::string& file_name, GameVersion version) {
    this->version = version;
    info.file_type = "art-group";
    info.file_name = "/src/next/data/art-group6/" + file_name + "-ag.go";
    name = file_name;
    info.major_version = versions::jak1::ART_FILE_VERSION;
    info.minor_version = 0;
    info.tool_debug = "Created by OpenGOAL buildactor";
    info.mdb_file_name = "Unknown";
    info.maya_file_name = "Unknown";
  }
  std::vector<u8> save_object_file() const;
};

bool run_build_actor(const std::string& input_model,
                     const std::string& output_file,
                     const std::string& output_prefix);
}  // namespace jak1
