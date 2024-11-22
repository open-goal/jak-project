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
  // frag geo stolen from money-lod2
  static std::vector<u32> words = {
      0x51180504, 0x1000013,  0x1b150000, 0x604,      0x5225,     0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080, 0x80808080,
      0x80808080, 0x80808080, 0x80808080, 0xb4e48086, 0x757d3a0a, 0x1e324000, 0x1e328186,
      0x757d3410, 0x1c4020,   0x432e8186, 0x251313,   0x71a28044, 0x1c8186,   0x757d2e16,
      0x96ce4040, 0x719b8186, 0x151919,   0x71c08064, 0x96ce8186, 0x757d281c, 0xb4e44060,
      0x71d28186, 0x251f1f,   0x435e8080, 0xb4e48186, 0x757d4322, 0x1e324080, 0x71cc7f86,
      0xffdb4025, 0x71ad0064, 0x435c8186, 0xffea582b, 0x71bc0044, 0x43348186, 0xffdb5b31,
      0x43530024, 0x71a48086, 0xffea3755, 0x43440000, 0x71a47f86, 0xffea3d3d, 0x43440080,
      0x432e7f86, 0x254646,   0x71a20044, 0x43657f86, 0x154949,   0x43400024, 0x719b8186,
      0x154c4c,   0x71c00064, 0x71d28186, 0x254f4f,   0x435e0000, 0x71d27f86, 0x250707,
      0x435e8000, 0x43658186, 0x150d0d,   0x43408024, 0x0,        0x0,        0x0,
      0xcb01005a, 0xcb00fffa, 0xcb01005a, 0x2101e01,  0x0,        0x0,        0x306,
      0x4030000,  0x120,      0x0,        0x1cf02c14, 0x66c801d,  0x0,        0x0,
      0x34,       0x0,        0x0,        0x0,        0x8,        0x0,        0x44,
      0x80,       0x42,       0x0,
  };
  for (auto& word : words) {
    gen.add_word(word);
  }
  return result;
}

size_t gen_dummy_frag_ctrl(DataObjectGenerator& gen) {
  size_t result = gen.current_offset_bytes();
  gen.add_word(0x1063918);
  gen.add_word(0x603);
  gen.add_word(0x0);
  gen.add_word(0x0);
  return result;
}

size_t gen_dummy_frag_ctrl_for_uploads(DataObjectGenerator& gen, int n) {
  size_t result = gen.current_offset_bytes();

  std::vector<u8> packed_frag_ctrls;

  // this is still a bit of a hack - the dummy_frag_ctrl above has 4 fragments, so we need to
  // provide fragment controls for each. The PC merc renderer will de-duplicate bone uploads over
  // all effects and fragments, so we just need to have a single fragment that asks for all bones,
  // and everything will work out.
  for (int k = 0; k < 4; k++) {
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

void generate_merc_effects(DataObjectGenerator& gen, int effect_count, int joints) {
  struct EffectLocs {
    size_t frag_geo;
    size_t frag_ctrl;
    size_t extra_info;
  };
  std::vector<EffectLocs> locs;
  for (int i = 0; i < effect_count; i++) {
    EffectLocs loc{};
    loc.frag_geo = gen.add_word(0);    // 112-140 (effect)
    loc.frag_ctrl = gen.add_word(0);   // 116 (frag-ctrl)
    gen.add_word(0x0);                 // 120 (blend-data)
    gen.add_word(0x0);                 // 124 (blend-ctrl)
    gen.add_word(0x10000);             // 128
    gen.add_word(0x140000);            // 132
    gen.add_word(0x100001d);           // 136
    loc.extra_info = gen.add_word(0);  // 140 (extra-info)
    locs.push_back(loc);
  }
  for (auto& loc : locs) {
    gen.link_word_to_byte(loc.extra_info, gen_dummy_extra_info(gen));
    gen.link_word_to_byte(loc.frag_ctrl, gen_dummy_frag_ctrl_for_uploads(gen, joints + 3));
    gen.link_word_to_byte(loc.frag_geo, gen_dummy_frag_geo(gen));
  }
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
  gen.add_word(0x4181b897);                          // 32-112 (xyz-scale)
  gen.add_word(0xc780ff80);                          // 36 (st-magic)
  gen.add_word(0x40798000);                          // 40 (st-out-a)
  gen.add_word(0x40eb4000);                          // 44 (st-out-b)
  gen.add_word(0x4780ff80);                          // 48 (st-vif-add)
  gen.add_word(0x50000);                             // 52 ((st-int-off << 16) + st-int-scale)
  gen.add_word(ag.merc_effect_count);                // 56 (effect-count)
  gen.add_word(0x0);                                 // 60 (blend-target-count)
  gen.add_word((0x14 * ag.merc_effect_count << 16) +
               ag.merc_effect_count);  // 64 ((fragment-count << 16) + tri-count)
  gen.add_word(0x130101);              // 68
  gen.add_word(0x13001d);              // 72
  gen.add_word(0x0);                   // 76
  gen.add_word(0x0);                   // 80
  gen.add_word(0x10101);               // 84
  gen.add_word(0x130000);              // 88
  gen.add_word(0x3f319ca9);            // 92
  gen.add_word(0x0);                   // 96
  gen.add_word(0x0);                   // 100
  gen.add_word(0x0);                   // 104
  gen.add_word(0x0);                   // 108
  generate_merc_effects(gen, ag.merc_effect_count, joints);
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
  ag.merc_effect_count = extract_data.new_model.effects.size();
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