#pragma once

#include "common/util/gltf_util.h"

#include "goalc/build_actor/common/art_types.h"
#include "goalc/build_level/collide/common/collide_common.h"

namespace jak1 {

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

// basic
struct JointAnim {
  std::string name;
  s16 number;
  s16 length;

  explicit JointAnim(const Joint& joint) {
    this->name = joint.name;
    number = joint.number;
    length = 1;
  }
};

// basic
struct JointAnimCompressed : JointAnim {
  std::vector<u32> data;
  explicit JointAnimCompressed(const Joint& joint) : JointAnim(joint) {
    number = joint.number;
    length = 1;
  }
  size_t generate(DataObjectGenerator& gen) const;
};

struct JointAnimFrame {
  math::Matrix4f matrices[2];
  std::vector<math::Matrix4f> data;

  size_t generate(DataObjectGenerator& gen) const;
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
    fixed_qwc = 0xf;
    frame_qwc = 1;
    fixed = JointAnimCompressedFixed();
    frame[0] = JointAnimCompressedFrame();
  }

  size_t generate(DataObjectGenerator& gen) const;
};

struct CollideMeshTri {
  u8 vert_idx[3];
  u8 unused;
  PatSurface pat;
};

struct CollideMesh {
  s32 joint_id;
  u32 num_tris;
  u32 num_verts;
  std::vector<math::Vector4f> vertices;
  std::vector<CollideMeshTri> tris;

  size_t generate(DataObjectGenerator& gen) const;
  size_t calc_data_size() const {
    // (size-of collide-mesh) + type ptr = 36
    return 36 + 16 * vertices.size() + sizeof(CollideMeshTri) * tris.size();
  }
};

struct ArtJointGeo : ArtElement {
  std::vector<Joint> data;
  std::vector<CollideMesh> cmeshes;
  ResLump lump;
  size_t mesh_slot;

  explicit ArtJointGeo(const std::string& name,
                       std::vector<CollideMesh> cmeshes,
                       std::vector<Joint>& joints) {
    this->name = name + "-lod0";
    length = joints.size();
    for (auto& joint : joints) {
      data.push_back(joint);
    }
    this->cmeshes = std::move(cmeshes);
  }
  void add_res();
  size_t generate(DataObjectGenerator& gen);
  size_t generate_mesh(DataObjectGenerator& gen) const;
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

  ArtJointAnim(const std::string& name, const std::vector<Joint>& joints) {
    this->name = name + "-idle";
    length = joints.size();
    speed = 1.0f;
    artist_base = 0.0f;
    artist_step = 1.0f;
    master_art_group_name = name;
    master_art_group_index = 2;
    frames = JointAnimCompressedControl();
    for (auto& joint : joints) {
      data.emplace_back(joint);
    }
  }
  size_t generate(DataObjectGenerator& gen) const;
};

struct ArtGroup : Art {
  FileInfo info;
  std::vector<ArtElement*> elts;
  std::map<int, size_t> joint_map;

  explicit ArtGroup(const std::string& file_name) {
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
  int get_joint_idx(const std::string& name);
};

bool run_build_actor(const std::string& input_model,
                     const std::string& output_file,
                     bool gen_collide_mesh);
}  // namespace jak1
