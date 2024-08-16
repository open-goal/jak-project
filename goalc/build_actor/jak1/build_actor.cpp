#include "build_actor.h"

#include "common/log/log.h"
#include "common/math/geometry.h"

#include "goalc/build_actor/common/MercExtract.h"
#include "goalc/build_actor/common/animation_processing.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

using namespace gltf_util;
namespace jak1 {

JointAnimCompressedHDR::JointAnimCompressedHDR(const anim::CompressedAnim& anim) {
  matrix_bits = 0;
  if (anim.matrix_animated[0]) {
    matrix_bits |= 1;
  }
  if (anim.matrix_animated[1]) {
    matrix_bits |= 2;
  }

  for (auto& word : control_bits) {
    word = 0;
  }

  for (size_t i = 0; i < anim.joint_metadata.size(); i++) {
    const int word_idx = i / 8;
    if (word_idx >= 14) {
      lg::error("too many joints!!");
      break;
    }

    u32 val = 0;
    const auto& metadata = anim.joint_metadata[i];
    if (metadata.animated_trans) {
      val |= (0b1);
    }
    if (metadata.animated_quat) {
      val |= (0b10);
    }
    if (metadata.animated_scale) {
      val |= (0b100);
    }
    if (metadata.big_trans_mode) {
      val |= (0b1000);
    }
    val = (val << (4 * (i % 8)));
    control_bits[word_idx] |= val;
  }

  num_joints = 2 + anim.joint_metadata.size();
}

JointAnimCompressedFixed::JointAnimCompressedFixed(const anim::CompressedAnim& anim) : hdr(anim) {
  u8* dest = (u8*)data;
  const u8* u64_src = (const u8*)anim.fixed.data64.data();
  const u8* u32_src = (const u8*)anim.fixed.data32.data();
  const u8* u16_src = (const u8*)anim.fixed.data16.data();

  const int u64_size = anim.fixed.data64.size() * sizeof(u64);
  const int u32_size = anim.fixed.data32.size() * sizeof(u32);
  const int u16_size = anim.fixed.data16.size() * sizeof(u16);

  if (u64_size + u32_size + u16_size > 133 * 4 * 4) {
    lg::die("fixed sizes are {} and {}", 133 * 4 * 4, u64_size + u32_size + u16_size);
  }
  ASSERT(u64_size + u32_size + u16_size <= 133 * 4 * 4);

  offset_64 = 0;
  memcpy(dest, u64_src, u64_size);
  dest += u64_size;

  offset_32 = offset_64 + u64_size;
  memcpy(dest, u32_src, u32_size);
  dest += u32_size;

  offset_16 = offset_32 + u32_size;
  memcpy(dest, u16_src, u16_size);
  reserved = 0;

  num_data_qw_used = (15 + u64_size + u32_size + u16_size) / 16;
  ASSERT(num_data_qw_used <= 133);
}

JointAnimCompressedFrame::JointAnimCompressedFrame(const anim::CompressedFrame& frame) {
  reserved = 0;
  u8* dest = (u8*)data;
  const u8* u64_src = (const u8*)frame.data64.data();
  const u8* u32_src = (const u8*)frame.data32.data();
  const u8* u16_src = (const u8*)frame.data16.data();

  const int u64_size = frame.data64.size() * sizeof(u64);
  const int u32_size = frame.data32.size() * sizeof(u32);
  const int u16_size = frame.data16.size() * sizeof(u16);

  if (u64_size + u32_size + u16_size > 133 * 4 * 4) {
    lg::die("frame sizes are {} and {}", 133 * 4 * 4, u64_size + u32_size + u16_size);
  }

  offset_64 = 0;
  memcpy(dest, u64_src, u64_size);
  dest += u64_size;

  offset_32 = offset_64 + u64_size;
  memcpy(dest, u32_src, u32_size);
  dest += u32_size;

  offset_16 = offset_32 + u32_size;
  memcpy(dest, u16_src, u16_size);
  reserved = 0;

  num_data_qw_used = (15 + u64_size + u32_size + u16_size) / 16;
  ASSERT(num_data_qw_used <= 133);
}

JointAnimCompressedControl::JointAnimCompressedControl(const anim::CompressedAnim& anim)
    : fixed(anim) {
  num_frames = anim.frames.size();
  for (auto& in_frame : anim.frames) {
    frame.emplace_back(in_frame);
  }
  fixed_qwc = fixed.num_data_qw_used;
  frame_qwc = frame.at(0).num_data_qw_used;
}

ArtJointAnim::ArtJointAnim(const anim::CompressedAnim& anim, const std::vector<Joint>& joints)
    : frames(anim) {
  this->name = anim.name;
  length = joints.size();
  speed = 1.0f;
  artist_base = 0.0f;
  artist_step = 1.0f;
  master_art_group_name = name;
  master_art_group_index = 2;
  for (auto& joint : joints) {
    data.emplace_back(joint, anim.frames.size());
  }
}

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

size_t JointAnimCompressed::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("joint-anim-compressed");
  size_t result = gen.current_offset_bytes();
  gen.add_ref_to_string_in_pool(name);
  gen.add_word((length << 16) + number);
  // data is always nullptr.
  // for (auto& word : data) {
  //   gen.add_word(word);
  // }
  return result;
}

size_t JointAnimCompressedFrame::generate(DataObjectGenerator& gen) const {
  gen.align(16);  // might have been 8, but 16 doesn't hurt.

  size_t result = gen.current_offset_bytes();
  gen.add_word(offset_64);  // 0
  gen.add_word(offset_32);  // 4
  gen.add_word(offset_16);  // 8
  gen.add_word(reserved);   // 12

  for (u32 i = 0; i < num_data_qw_used; i++) {
    for (int j = 0; j < 4; j++) {
      gen.add_word_float(data[i][j]);
    }
  }
  return result;
}

size_t JointAnimCompressedHDR::generate(DataObjectGenerator& gen) const {
  size_t result = gen.current_offset_bytes();
  for (auto& bit : control_bits) {
    gen.add_word(bit);
  }
  gen.add_word(num_joints);
  gen.add_word(matrix_bits);
  return result;
}

size_t JointAnimCompressedFixed::generate(DataObjectGenerator& gen) const {
  gen.align(16);  // might have been 8, but 16 doesn't hurt.
  size_t result = gen.current_offset_bytes();
  hdr.generate(gen);        // 0-64 (inline)
  gen.add_word(offset_64);  // 64
  gen.add_word(offset_32);  // 68
  gen.add_word(offset_16);  // 72
  gen.add_word(reserved);   // 76
  // default joint poses (taken from money-idle)
  for (int i = 0; i < num_data_qw_used; i++) {
    gen.add_word_float(data[i].x());
    gen.add_word_float(data[i].y());
    gen.add_word_float(data[i].z());
    gen.add_word_float(data[i].w());
  }
  // -- not sure what this is, part of the dummy data maybe?
  gen.add_word(0);
  gen.add_word(0x7fff0000);
  gen.add_word(0x2250000);
  gen.add_word(0x10001000);
  gen.add_word(0x10000000);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.align(4);
  return result;
}

size_t JointAnimCompressedControl::generate(DataObjectGenerator& gen) const {
  gen.align(16);
  size_t result = gen.current_offset_bytes();
  gen.add_word(num_frames);  // 0
  gen.add_word(fixed_qwc);   // 4
  gen.add_word(frame_qwc);   // 8

  auto ja_fixed_slot = gen.add_word(0);
  std::vector<int> ja_frame_slots;
  for (u32 i = 0; i < num_frames; i++) {
    ja_frame_slots.push_back(gen.add_word(0));
  }
  gen.link_word_to_byte(ja_fixed_slot, fixed.generate(gen));
  ASSERT(num_frames == frame.size());
  for (size_t i = 0; i < num_frames; i++) {
    gen.link_word_to_byte(ja_frame_slots[i], frame[i].generate(gen));
  }
  return result;
}

size_t CollideMesh::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("collide-mesh");
  size_t result = gen.current_offset_bytes();
  // gen.add_word(0xffffffff - joint_id);  // 4 (joint-id)
  gen.add_word(joint_id);                // 4 (joint-id)
  gen.add_word(num_tris);                // 8 (num-tris)
  gen.add_word(num_verts);               // 12 (num-verts)
  auto vertices_slot = gen.add_word(0);  // 16 (vertex-data)
  gen.add_word(0);                       // 20 (pad)
  gen.add_word(0);                       // 24 (pad)
  gen.add_word(0);                       // 28 (pad)
  for (auto& tri : tris) {
    u32 word = (tri.vert_idx[0] & 0xff) + ((tri.vert_idx[1] << 8) & 0xff00) +
               ((tri.vert_idx[2] << 16) & 0xff0000);
    gen.add_word(word);         // 0 (tris | vertex-index | unused)
    gen.add_word(tri.pat.val);  // 4 (pat)
  }
  // vertex data start
  gen.link_word_to_byte(vertices_slot, gen.current_offset_bytes());
  for (auto& vert : vertices) {
    gen.add_word_float(vert.x());
    gen.add_word_float(vert.y());
    gen.add_word_float(vert.z());
    gen.add_word_float(vert.w());
  }
  return result;
};

void ArtJointGeo::add_res() {
  if (!cmeshes.empty()) {
    lump.add_res(
        std::make_unique<ResRef>("collide-mesh-group", "array", mesh_slot, DEFAULT_RES_TIME));
  }
  // jgeo.lump.add_res(
  //     std::make_unique<ResInt32>("texture-level", std::vector<s32>{2}, DEFAULT_RES_TIME));
  // jgeo.lump.add_res(std::make_unique<ResVector>(
  //     "trans-offset", std::vector<math::Vector4f>{{0.0f, 2048.0f, 0.0f, 1.0f}},
  //     DEFAULT_RES_TIME));
  // jgeo.lump.add_res(
  //     std::make_unique<ResInt32>("joint-channel", std::vector<s32>{0}, DEFAULT_RES_TIME));
  // jgeo.lump.add_res(std::make_unique<ResFloat>(
  //     "lod-dist", std::vector<float>{5000.0f * METER_LENGTH, 6000.0f * METER_LENGTH},
  //     DEFAULT_RES_TIME));
  lump.sort_res();
}

size_t ArtJointGeo::generate_mesh(DataObjectGenerator& gen) const {
  std::vector<size_t> data_slots;
  std::vector<size_t> content_slots;
  gen.align_to_basic();
  gen.add_type_tag("array");
  size_t result = gen.current_offset_bytes();
  gen.add_word(cmeshes.size());      // 0 (length)
  gen.add_word(cmeshes.size());      // 4 (allocated-length)
  gen.add_type_tag("collide-mesh");  // 8 (content-type)
  content_slots.reserve(cmeshes.size());
  for (auto& data : cmeshes) {
    (void)data;
    content_slots.push_back(gen.add_word(0));  // 12 (data)
  }
  gen.align(4);
  for (size_t i = 0; i < content_slots.size(); i++) {
    data_slots.push_back(cmeshes.at(i).generate(gen));
  }
  for (size_t i = 0; i < content_slots.size(); i++) {
    gen.link_word_to_byte(content_slots.at(i), data_slots.at(i));
  }
  return result;
}

size_t ArtJointGeo::generate(DataObjectGenerator& gen) {
  gen.align_to_basic();
  gen.add_type_tag("art-joint-geo");
  size_t result = gen.current_offset_bytes();
  gen.add_word(0);                      // 4
  gen.add_ref_to_string_in_pool(name);  // 8
  gen.add_word(length);                 // 12
  auto res_slot = gen.add_word(0);      // 16 (res-lump)
  gen.add_word(0);                      // 20
  gen.add_word(0);
  gen.add_word(0);
  std::vector<size_t> joint_slots;
  for (int i = 0; i < length; i++) {
    joint_slots.push_back(gen.add_word(0));
  }
  gen.align(4);
  for (int i = 0; i < length; i++) {
    auto joint = data.at(i).generate(gen);
    gen.link_word_to_byte(joint_slots.at(i), joint);
    g_joint_map[data.at(i).number] = joint;
  }
  mesh_slot = generate_mesh(gen);
  add_res();
  auto res_header = lump.generate_header(gen, "res-lump");
  gen.link_word_to_byte(res_slot, res_header);
  lump.generate_tag_list_and_data(gen, res_header);
  return result;
}

size_t ArtJointAnim::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("art-joint-anim");
  size_t result = gen.current_offset_bytes();
  gen.add_symbol_link("#f");                             // 4 (eye block)
  gen.add_ref_to_string_in_pool(name);                   // 8
  gen.add_word(length);                                  // 12
  gen.add_symbol_link("#f");                             // 16 (res-lump)
  gen.add_word_float(speed);                             // 20
  gen.add_word_float(artist_base);                       // 24
  gen.add_word_float(artist_step);                       // 28
  gen.add_ref_to_string_in_pool(master_art_group_name);  // 32
  gen.add_word(master_art_group_index);                  // 36
  gen.add_symbol_link("#f");                             // 40 (blerc)
  auto ctrl_slot = gen.add_word(0);
  std::vector<size_t> frame_slots;
  for (int i = 0; i < length; i++) {
    frame_slots.push_back(gen.add_word(0));
  }
  gen.align(4);
  gen.link_word_to_byte(ctrl_slot, frames.generate(gen));
  for (int i = 0; i < length; i++) {
    gen.link_word_to_byte(frame_slots.at(i), data.at(i).generate(gen));
  }
  return result;
}

static size_t gen_dummy_frag_geo(DataObjectGenerator& gen) {
  size_t result = gen.current_offset_bytes();
  // frag geo stolen from money-lod0
  static std::vector<u32> words = {
      0xa4320c04, 0x8000026,  0x302a0000, 0x604,      0xa910,     0xac16,     0x100af1c,
      0xb23d,     0x100b585,  0x100b870,  0xbb76,     0xbe52,     0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x0,        0x0,        0x89b78086, 0xf1a910a,  0x3a4b7000, 0x5f818086,
      0xf1a1094,  0x29347014, 0x49648186, 0x91313,    0x4e5d8024, 0x354b8186, 0xf1a6416,
      0x3a497024, 0x40538186, 0x91919,    0x647b8034, 0x24348186, 0xf1a371c,  0x647f7034,
      0x495d8186, 0x91f1f,    0x7a9c8044, 0x35498086, 0xf1a2231,  0x8eb57044, 0x5f7b8186,
      0x92525,    0x83ad8054, 0x5f7f8186, 0xf1a2828,  0x9fcc7054, 0x759c8186, 0x92b2b,
      0x7aa38064, 0x1c257f86, 0x273b2e2e, 0x94b86040, 0xe198186,  0x2737d034, 0x71936038,
      0xe188186,  0x273bcd3a, 0x57676030, 0x1c2a8186, 0x2737613d, 0x34456028, 0xe1b8086,
      0x485d40c7, 0x2e3c5028, 0x293c8186, 0x485d5b43, 0x131a5020, 0x26398186, 0x6e804646,
      0xf164020,  0x4b688186, 0x6e805549, 0x34018,    0x4c688186, 0x979f4c4c, 0x5073018,
      0x72978086, 0x979f4fc1, 0x5073010,  0x73988086, 0x6e807352, 0x34010,    0x4c698186,
      0x485d6d58, 0x5085018,  0x2f488086, 0x273b5e67, 0x21256020, 0x526d8186, 0x2737a66a,
      0x13196018, 0x72988186, 0x485da070, 0x5085010,  0x98c78086, 0x6e80767f, 0xf164008,
      0x95c48186, 0x979fc479, 0x13193008, 0xb0e68186, 0x979f7c7c, 0x2e3b3000, 0xb4ea8186,
      0x6e808282, 0x2b394000, 0x95c48186, 0x485d9d85, 0x131b5008, 0xb0e68186, 0x485d8888,
      0x2e3c5000, 0x8fbb8186, 0x2737978b, 0x212a6008, 0xa2db8186, 0x273b8e8e, 0x34486000,
      0x6c998086, 0x273b9aa3, 0x13186010, 0x87f86,    0x485dcaca, 0x51685030, 0x75a37f86,
      0x90707,    0x4e648000, 0x5f858186, 0x90d0d,    0x45538014, 0x0,        0x0,
      0xcb01005f, 0xcb00fffa, 0xcb010064, 0x5101e01,  0x0,        0x0,        0x306,
      0x60030000, 0x120,      0x0,        0x1cf02c14, 0x2008044,  0x0,        0x0,
      0x34,       0x81010000, 0x0,        0x0,        0x8,        0x6d100000, 0x44,
      0x80,       0x42,       0x30000,    0x86321604, 0x400001c,  0x2422440e, 0x4,
      0x1004f28,  0x1008b4c,  0xb225,     0xca4c,     0x1000128,  0x1000422,  0x1000a2e,
      0x10064ca,  0x6a34,     0x1006d2e,  0x70ca,     0x1007340,  0x7946,     0x7f4c,
      0x100884c,  0x8e4f,     0x9479,     0x9a7c,     0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x0,        0x0,        0x131a8086,
      0x215d0d9d, 0x87c45040, 0x38186,    0x4780a313, 0x65984038, 0x38186,    0x47806116,
      0x3d684030, 0x5078186,  0x709fa919, 0x3e693030, 0x13198186, 0x709f5b1c, 0x1b3c3028,
      0x21208186, 0x96bbaf1f, 0x21452028, 0x34428186, 0x96bf5522, 0xe242020,  0x3a408086,
      0xb6da252e, 0x27411024, 0x647f8186, 0xb6da4628, 0x16261014, 0x647a8186, 0xcbf1402b,
      0x32450014, 0x4e528086, 0xcbf1313a, 0x3b5b0024, 0x29268086, 0xb6da34b5, 0x51811034,
      0x45458186, 0xcbf13737, 0x51860034, 0x64808186, 0xd3ff3d3d, 0x51800040, 0x7aa58186,
      0xcbf14343, 0x3b520000, 0x8ebf8186, 0xb6dac749, 0x27401000, 0x71948186, 0x96bf854c,
      0x132010,   0x57668186, 0x96bb8252, 0x122018,   0x2e3b8186, 0x709f7c58, 0x1a3020,
      0xf168186,  0x4780765e, 0x18394028, 0x94bb8086, 0x96bb91c4, 0xe202008,  0xa7dc8086,
      0x96bf97c1, 0x21422000, 0xf167f86,  0x4780a0a0, 0x8ac74040, 0x5078186,  0x709fbea6,
      0x64983038, 0x13138186, 0x96bfb8ac, 0x446c2030, 0x13128186, 0x96bbbbbb, 0x5e9a2038,
      0x34458186, 0x370707,   0x94d66048, 0x5088186,  0x215d6710, 0x64975038, 0xcb010064,
      0xcb00ffd3, 0xcb010051, 0x5021000,  0x4088003,  0x10306060, 0x3,        0x0,
      0x8d301104, 0x300001f,  0x2624430a, 0x4,        0x910a,     0x100a928,  0xc42b,
      0x1006aa0,  0x70a6,     0x73bb,     0x9a07,     0xa00d,     0x100a3a0,  0x100a6bb,
      0xac34,     0xb237,     0xb83d,     0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x8ccc8186,
      0xf1a4c07,  0x16817074, 0x7bb58186, 0xf1a550a,  0x40b77064, 0x94d68186, 0x2737460d,
      0x46bb6068, 0x81b88186, 0x273b8e10, 0x59db6060, 0x87c48186, 0x485d4013, 0x67e65060,
      0x64978186, 0x485d8816, 0x75f85058, 0x65988186, 0x6e803a19, 0x7afd4058, 0x3d688186,
      0x6e80821c, 0x7afd4050, 0x3e698186, 0x979f341f, 0x75f93050, 0x1b3c8186, 0x979f7c22,
      0x67e73048, 0x21458086, 0xbdbb252e, 0x59e02048, 0xe248086,  0xbdbf2876, 0x46be2040,
      0x27418186, 0xdddaaf2b, 0x40c01044, 0x446c8186, 0xbdbfc731, 0x67ed2050, 0x64988186,
      0x979f3737, 0x75f93058, 0x8ac78186, 0x6e803d3d, 0x6bea4060, 0xa2e58186, 0x485d4343,
      0x4cc45068, 0xa2e88186, 0x273b4949, 0x23996070, 0x679c7f86, 0x95252,    0x2ca38064,
      0x517f8086, 0xf1a5894,  0x51cc7054, 0x5e938186, 0x27378b5b, 0x67e76058, 0x44678086,
      0x273b5e97, 0x67e86050, 0x3e688186, 0x485d8561, 0x75f85050, 0x1b3c8186, 0x485d9d64,
      0x67e55048, 0x18398186, 0x6e807f67, 0x6bea4048, 0x1a8186,   0x979f796d, 0x4cc53040,
      0x3b5b8086, 0xf2f1b5be, 0x2cae0044, 0x51868186, 0xf2f1bbbb, 0x35bb0054, 0x51818186,
      0xdddac1c1, 0x51da1054, 0x67a37f86, 0x90101,    0x648080,   0x70ad7f86, 0x94f04,
      0x16858074, 0x0,        0x0,        0x0,        0xcb010051, 0xcb00fffa, 0xcb010016,
      0x5021000,  0xc008003,  0x81860080, 0x0,        0x0,        0x92351604, 0x300001f,
      0x2826450f, 0x4,        0xac31,     0x100af5e,  0x100c449,  0xa01,      0x1000d07,
      0x6143,     0x100643d,  0x10079c1,  0x8537,     0x883d,     0x1008b07,  0x1008e49,
      0x100b243,  0xb549,     0x100b837,  0xbe31,     0xc1c1,     0xc7bb,     0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x0,        0x0,        0x0,        0x808186,   0x707,
      0x39800040, 0x2ab78186, 0x141a1010, 0xf4b7080,  0x51e78186, 0x2c379113, 0x2c6d6078,
      0x43db8186, 0x2c3b1616, 0x9486080,  0x5ff88186, 0x4d5d9719, 0x26695078, 0x51e68186,
      0x4d5d1c1c, 0x33c5080,  0x64fd8186, 0x73809d1f, 0x25684078, 0x55ea8186, 0x73802222,
      0x394080,   0x5ff98186, 0x9c9fa325, 0x26683078, 0x51e68186, 0x9c9f2828, 0x33b3080,
      0x51ee8186, 0xc2bba92b, 0x2c662078, 0x43dc8186, 0xc2bf2e2e, 0x9422080,  0x3bda8186,
      0xe2da4631, 0x397f1074, 0x2abf8186, 0xe2da3434, 0xf401080,  0x1fbb8086, 0xf7f13740,
      0x397a0074, 0x16a58186, 0xf7f13a3a, 0x23520080, 0x808186,   0xffffcd3d, 0x39800040,
      0x16ae8186, 0xf7f1ca43, 0x4fa50064, 0x2ac08186, 0xe2da7649, 0x63bf1064, 0x51ed8186,
      0xc2bfa64c, 0x46942070, 0x43e08186, 0xc2bb734f, 0x69bb2068, 0x5ff98186, 0x9c9fa052,
      0x4c973070, 0x51e78186, 0x9c9f6d55, 0x6fc43068, 0x64fd8186, 0x73809a58, 0x4d984070,
      0x55ea8186, 0x7380675b, 0x72c74068, 0x5ff88186, 0x4d5d945e, 0x4c985070, 0x36c58186,
      0x9c9f826a, 0x8ae63060, 0x30be8186, 0xc2bf7c70, 0x7cdc2060, 0xd9a8086,  0xc2bb7fbb,
      0x8aee2058, 0x16a37f86, 0x5090101,  0x23640000, 0x1fad7f86, 0x5090404,  0x39850074,
      0x0,        0x0,        0x0,        0xcb010000, 0xcb00ffff, 0xcb010039, 0x5021000,
      0x20001b,   0x6c00c102, 0x2,        0x0,        0x210f0904, 0x6,        0xb090b05,
      0x4,        0x101,      0x707,      0x1307,     0x1c04,     0x1f07,     0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x0,        0x538186,
      0x90d0d,    0x1f7b0034, 0x95d7f86,  0x91010,    0x359c0044, 0x1f7b8186, 0x91616,
      0x3ead0054, 0x359c8186, 0x91919,    0x35a30064, 0x1f857f86, 0x90404,    0x530014,
      0x9648186,  0x90a0a,    0x95d0024,  0x0,        0x0,        0xcb01001f, 0xcb00fffa,
      0xcb01001f, 0x1021000,  0x223,      0x0,        0x0,        0x0};
  for (auto& word : words) {
    gen.add_word(word);
  }
  return result;
}

size_t gen_dummy_frag_ctrl(DataObjectGenerator& gen) {
  size_t result = gen.current_offset_bytes();
  gen.add_word(0x1067232);
  gen.add_word(0x54320603);
  gen.add_word(0x5d300002);
  gen.add_word(0x5d350002);
  gen.add_word(0x120f0002);
  gen.add_word(0x2);
  gen.add_word(0x0);
  gen.add_word(0x0);
  return result;
}

size_t gen_dummy_frag_ctrl_for_uploads(DataObjectGenerator& gen, int n) {
  size_t result = gen.current_offset_bytes();

  std::vector<u8> packed_frag_ctrls;

  // this is still a bit of a hack - the dummy_frag_ctrl above has 8 fragments, so we need to
  // provide fragment controls for each. The PC merc renderer will de-duplicate bone uploads over
  // all effects and fragments, so we just need to have a single fragment that asks for all bones,
  // and everything will work out.
  for (int k = 0; k < 8; k++) {
    packed_frag_ctrls.push_back(0);
    packed_frag_ctrls.push_back(0);
    packed_frag_ctrls.push_back(0);
    if (k == 0) {  // for the first frag, do all matrix uploads.
      // note that these are bogus destination addresses, but nothing uses it on PC
      packed_frag_ctrls.push_back(n);
      for (int i = 0; i < n; i++) {
        packed_frag_ctrls.push_back(i);
        packed_frag_ctrls.push_back(i);
      }
    } else {
      // remaining frags can have empty matrix upload lists.
      packed_frag_ctrls.push_back(0);
    }
  }

  std::vector<u32> u32s((packed_frag_ctrls.size() + 3) / 4);
  memcpy(u32s.data(), packed_frag_ctrls.data(), packed_frag_ctrls.size());
  for (auto x : u32s) {
    gen.add_word(x);
  }

  return result;
}

size_t gen_dummy_extra_info(DataObjectGenerator& gen) {
  size_t result = gen.current_offset_bytes();
  gen.add_word(0x1);
  gen.add_word(0x0);
  gen.add_word(0x0);
  gen.add_word(0x0);
  gen.add_word(0xb74ccccd);
  gen.add_word(0x40400000);
  gen.add_word(0x8066a1ff);
  gen.add_word(0x0);
  return result;
}

size_t generate_dummy_merc_ctrl(DataObjectGenerator& gen, const ArtGroup& ag) {
  gen.align_to_basic();
  gen.add_type_tag("merc-ctrl");
  size_t result = gen.current_offset_bytes();
  // excluding align and prejoint
  auto joints = ((ArtJointGeo*)ag.elts.at(0).get())->length - 2;
  gen.add_word(0);                                   // 4
  gen.add_ref_to_string_in_pool(ag.name + "-lod0");  // 8
  gen.add_word(0);                                   // 12
  gen.add_symbol_link("#f");                         // 16 (res-lump)
  gen.add_word(joints);                              // 20 (num-joints)
  gen.add_word(0x0);                                 // 24 (pad)
  gen.add_word(0x0);                                 // 28 (pad)
  gen.add_word(0x4188ee86);                          // 32-112 (xyz-scale)
  gen.add_word(0xc780ff80);                          // 36 (st-magic)
  gen.add_word(0x40798000);                          // 40 (st-out-a)
  gen.add_word(0x40eb4000);                          // 44 (st-out-b)
  gen.add_word(0x4780ff80);                          // 48 (st-vif-add)
  gen.add_word(0x50000);                             // 52 ((st-int-off << 16) + st-int-scale)
  gen.add_word(0x1);                                 // 56 (effect-count)
  gen.add_word(0x0);                                 // 60 (blend-target-count)
  gen.add_word(0xe00005);                            // 64 ((fragment-count << 16) + tri-count)
  gen.add_word(0x860101);                            // 68
  gen.add_word(0x86011b);                            // 72
  gen.add_word(0x0);                                 // 76
  gen.add_word(0x0);                                 // 80
  gen.add_word(0x120101);                            // 84
  gen.add_word(0x83002c);                            // 88
  gen.add_word(0x3e780184);                          // 92
  gen.add_word(0x0);                                 // 96
  gen.add_word(0x0);                                 // 100
  gen.add_word(0x0);                                 // 104
  gen.add_word(0x0);                                 // 108
  auto frag_geo_slot = gen.add_word(0);              // 112-140 (effect)
  auto frag_ctrl_slot = gen.add_word(0);             // 116 (frag-ctrl)
  gen.add_word(0x0);                                 // 120 (blend-data)
  gen.add_word(0x0);                                 // 124 (blend-ctrl)
  gen.add_word(0x50000);                             // 128
  gen.add_word(0xe00000);                            // 132
  gen.add_word(0x100011b);                           // 136
  auto extra_info_slot = gen.add_word(0);            // 140 (extra-info)
  gen.link_word_to_byte(extra_info_slot, gen_dummy_extra_info(gen));
  gen.link_word_to_byte(frag_ctrl_slot, gen_dummy_frag_ctrl_for_uploads(gen, joints + 3));
  gen.link_word_to_byte(frag_geo_slot, gen_dummy_frag_geo(gen));
  return result;
}

std::vector<u8> ArtGroup::save_object_file() const {
  DataObjectGenerator gen;
  gen.add_type_tag("art-group");
  auto ag_words = 8 + length;
  while (gen.words() < ag_words) {
    gen.add_word(0);
  }
  auto file_info_slot = info.add_to_object_file(gen);
  gen.link_word_to_byte(1, file_info_slot);      // 4 (file-info)
  gen.link_word_to_string_in_pool(name, 8 / 4);  // 8 (name)
  gen.set_word(12 / 4, length);                  // 12 (ag length)
  gen.link_word_to_symbol("#f", 16 / 4);         // 16 (res-lump)
  gen.set_word(20 / 4, 0);                       // 20 (pad)
  gen.set_word(24 / 4, 0);
  gen.set_word(28 / 4, 0);
  if (!elts.empty()) {
    if (elts.at(0)) {
      auto jgeo = (ArtJointGeo*)elts.at(0).get();
      gen.link_word_to_byte(32 / 4, jgeo->generate(gen));
    }
    if (!elts.at(1)) {
      gen.link_word_to_byte(36 / 4, generate_dummy_merc_ctrl(gen, *this));
    }

    for (size_t i = 2; i < elts.size(); i++) {
      auto ja = (ArtJointAnim*)elts.at(i).get();
      gen.link_word_to_byte((32 + i * 4) / 4, ja->generate(gen));
    }
  }

  return gen.generate_v4();
}

int ArtGroup::get_joint_idx(const std::string& name) {
  for (auto& elt : this->elts) {
    if (elt && typeid(*elt) == typeid(ArtJointGeo)) {
      auto jgeo = (ArtJointGeo*)elt.get();
      for (auto& joint : jgeo->data) {
        if (joint.name == name) {
          return joint.number;
        }
      }
    }
  }
  return -1;
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

struct GltfJoint {
  math::Matrix4f bind_pose_T_w;  // inverse bind pose
  std::string name;
  int gltf_node_index = 0;
  int parent = -1;
  std::vector<int> children;
};

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

/*!
 * Build GOAL format data for an actor. This doesn't generate the data for the .FR3.
 */
bool run_build_actor(const std::string& mdl_name,
                     const std::string& ag_out,
                     bool gen_collide_mesh) {
  std::string ag_name;
  if (fs::exists(file_util::get_jak_project_dir() / mdl_name)) {
    ag_name = fs::path(mdl_name).stem().string();
  } else {
    ASSERT_MSG(false, "Model file not found: " + mdl_name);
  }

  // Load GLTF file to check for a skeleton
  tinygltf::Model model = load_gltf_model(file_util::get_jak_project_dir() / mdl_name);
  auto all_nodes = flatten_nodes_from_all_scenes(model);
  auto skin_idx = find_single_skin(model, all_nodes);
  if (skin_idx) {
    lg::info("GLTF file contained a skin, this actor will have a real skeleton");
  }
  std::vector<anim::CompressedAnim> user_anims;

  ArtGroup ag(ag_name);
  std::vector<Joint> joints;
  MercExtractData extract_data;
  extract("test", extract_data, model, all_nodes, 0, 0, 0);
  // MercSwapData out;
  // merc_convert(out, extract_data);
  // Set up joints:
  if (skin_idx) {
    // read skeleton data out of GLTF
    auto skeleton_joints = extract_skeleton(model, *skin_idx);
    // convert to game format
    joints = convert_joints(skeleton_joints);
    // get animation from user.
    user_anims = process_anim(model, skeleton_joints);

  } else {
    auto identity = math::Matrix4f::identity();
    joints.emplace_back("align", 0, -1, identity);
    joints.emplace_back("prejoint", 1, -1, identity);
    // matrix stolen from "egg" joint from "money" art group
    auto main_pose = math::Matrix4f::identity();
    main_pose(3, 1) = -2194.1628418f;
    Joint main("main", 2, 1, main_pose);
    joints.emplace_back(main);
  }

  std::vector<CollideMesh> mesh;
  if (gen_collide_mesh) {
    mesh = gen_collide_mesh_from_model(model, all_nodes, 3);
  }

  std::shared_ptr<ArtJointGeo> jgeo = std::make_shared<ArtJointGeo>(ag.name, mesh, joints);

  ag.elts.emplace_back(jgeo);
  // dummy merc-ctrl
  ag.elts.emplace_back(nullptr);

  if (!user_anims.empty()) {
    for (auto& anim : user_anims) {
      ag.elts.emplace_back(std::make_shared<ArtJointAnim>(anim, joints));
    }
  } else {
    ag.elts.emplace_back(std::make_shared<ArtJointAnim>(ag.name, joints));
  }

  ag.length = ag.elts.size();

  auto ag_file = ag.save_object_file();
  lg::info("ag file size {} bytes", ag_file.size());
  auto save_path = fs::path(ag_out);
  file_util::create_dir_if_needed_for_file(ag_out);
  lg::info("Saving to {}", save_path.string());
  file_util::write_binary_file(file_util::get_jak_project_dir() / save_path, ag_file.data(),
                               ag_file.size());
  return true;
}
}  // namespace jak1