#include "extract_merc.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/colors.h"

#include "decompiler/level_extractor/MercData.h"
#include "decompiler/level_extractor/extract_common.h"
#include "decompiler/util/goal_data_reader.h"

namespace decompiler {

// number of slots on VU1 data memory to store matrices
constexpr int MERC_VU1_MATRIX_SLOTS = 18;
// the size of each "matrix". Includes a transformation and rotation matrix (for normals)
constexpr int MERC_MATRIX_STRIDE = 7;

// converts a vu1 address to an index of a matrix slot.
// as far as I can tell, nothing in the game uses indices, they are always addresses already
u32 vu1_addr_to_matrix_slot(u32 addr) {
  ASSERT(addr >= 6);
  ASSERT(addr < (6 + MERC_MATRIX_STRIDE * MERC_VU1_MATRIX_SLOTS));
  addr -= 6;
  ASSERT((addr % MERC_MATRIX_STRIDE) == 0);
  return addr / MERC_MATRIX_STRIDE;
}

u32 matrix_slot_to_vu1_addr(u32 slot) {
  ASSERT(slot < MERC_VU1_MATRIX_SLOTS);
  return 6 + (slot * MERC_MATRIX_STRIDE);
}

/*!
 * The GS settings of a merc draw (shader + tex)
 */
struct MercGsState {
  // the blending, clamp, etc settings. from adgif shaders
  DrawMode mode;
  // the texture to use, as a "pc combo" texture index.
  u32 pc_combo_tex_id;
  u64 as_u64() const { return (((u64)pc_combo_tex_id) << 32) | mode.as_int(); }
};

/*!
 * This is all the state that's required to understand how to draw a vertex. including both the GS
 * state, and stuff (matrices) left behind in VU memory.  The matrix slots are matrix indices into
 * the original bones matrices (all the matrices in the skeleton). An index of -1 indicates that
 * there is no known matrix in this slot.
 */
struct MercState {
  MercGsState merc_draw_mode;
  // vu1_matrix_slots[x] = y
  // where x is the slot in VU1 memory, and y is the matrix index for the bones/skeleton stuff.
  std::array<int, MERC_VU1_MATRIX_SLOTS> vu1_matrix_slots;
  MercState() { vu1_matrix_slots.fill(-1); }
};

/*!
 * Required information for a "draw", consisting of a strip of triangles (represented as indices for
 * OpenGL triangle strip) and the draw settings.
 */
struct MercDraw {
  size_t ctrl_idx;    // which merc ctrl in the level we correspond to
  size_t effect_idx;  // which effect within that control
  size_t frag_idx;    // which frag within that effect
  // note that the above triple isn't enough to uniquely identify a draw - there can be multiple
  // draws within a fragment.

  // draw settings
  MercState state;

  // opengl indices. currently into a per-effect vertex list (likely to merge into a giant buffer
  // eventually)
  std::vector<u32> indices;

  // where we would be in VU1 data memory (used in construction)
  u32 vtx_offset;  // relative to writing output zone
  u32 vtx_nloop;   // nloop that goes in the gif tag drawing us.
};

/*!
 * Merc Vertex. Not the format we'll want in the game data files, but most useful as an intermediate
 * when building up draws.
 */
struct MercUnpackedVtx {
  int kind = 0;        // 1, 2, or 3 matrix
  math::Vector3f pos;  // position
  math::Vector3f nrm;  // normal (as input to the merc math, pretty sure legnth is bogus)
  math::Vector2f st;   // texture coordinates
  math::Vector<u8, 4> rgba;

  int skel_mats[3];
  float mat_weights[3];

  u16 dst0;
  u16 dst1;
};

/*!
 * An entire merc-effect, split into draws.
 * Note that copied or multiply-placed vertices will be de-deduplicated, but not identical vertices
 * that actually appear in the input to merc.
 */
struct ConvertedMercEffect {
  size_t ctrl_idx;
  size_t effect_idx;
  // draws from all fragments.
  std::vector<MercDraw> draws;
  std::vector<MercUnpackedVtx> vertices;
};

/*!
 * Extract a merc-ctrl data structure. This is mostly just copying the GOAL data to C++ classes,
 * but does include the effect of processing the DMA data through VIF.
 */
MercCtrl extract_merc_ctrl(const LinkedObjectFile& file,
                           const DecompilerTypeSystem& dts,
                           int word_idx) {
  Ref ref;
  ref.data = &file;
  ref.seg = 0;
  ref.byte_offset = word_idx * 4;

  auto tr = typed_ref_from_basic(ref, dts);

  MercCtrl ctrl;
  ctrl.from_ref(tr, dts);  // the merc data import
  return ctrl;
}

/*!
 * Find the word indices for the merc ctrls (the type tags)
 */
std::vector<int> find_merc_ctrls(const LinkedObjectFile& file) {
  std::vector<int> result;
  for (size_t i = 0; i < file.words_by_seg.at(0).size(); i++) {
    const auto& word = file.words_by_seg[0][i];
    if (word.kind() == LinkedWord::TYPE_PTR && word.symbol_name() == "merc-ctrl") {
      result.push_back(i);
    }
  }
  return result;
}

namespace {
/*!
 * Merc models tend to have strange texture ids. I don't really understand why.
 * On login, the texture is checked against a list of textures in the bsp, and replaced with this.
 * It doesn't seem to be related to sharing textures between levels - the yakow texture uses this.
 */
u32 remap_texture(u32 original, const std::vector<level_tools::TextureRemap>& map) {
  auto masked = original & 0xffffff00;
  for (auto& t : map) {
    if (t.original_texid == masked) {
      return t.new_texid | 20;
    }
  }
  return original;
}

/*!
 * Set the alpha fields of a DrawMode (for PC renderers) based on gs alpha register
 */
void update_mode_from_alpha1(GsAlpha reg, DrawMode& mode) {
  if (reg.a_mode() == GsAlpha::BlendMode::SOURCE && reg.b_mode() == GsAlpha::BlendMode::DEST &&
      reg.c_mode() == GsAlpha::BlendMode::SOURCE && reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - Cd) * As + Cd
    // Cs * As  + (1 - As) * Cd
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);

  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - 0) * As + Cd
    // Cs * As + (1) * CD
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_SRC_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    ASSERT(reg.fix() == 128);
    // Cv = (Cs - 0) * FIX + Cd
    // if fix = 128, it works out to 1.0
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_FIX_DST);
    // src plus dest
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::DEST &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // Cv = (Cs - Cd) * FIX + Cd
    ASSERT(reg.fix() == 64);
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_FIX_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::DEST &&
             reg.b_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED) {
  }

  else {
    // unsupported blend: a 0 b 1 c 0 d 2 is this part of generic?
    lg::warn("unsupported blend: a {} b {} c {} d {}", (int)reg.a_mode(), (int)reg.b_mode(),
             (int)reg.c_mode(), (int)reg.d_mode());
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    // ASSERT(false);
  }
}

/*!
 * Convert merc shader to PC draw mode
 */
DrawMode process_draw_mode(const MercShader& info) {
  DrawMode mode;
  /*
   *       (new 'static 'gs-test
           :ate #x1
           :atst (gs-atest greater-equal)
           :aref #x26
           :zte #x1
           :ztst (gs-ztest greater-equal)
           )
   */
  mode.enable_at();
  mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  mode.set_aref(0x26);
  mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
  mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  mode.enable_zt();
  mode.enable_depth_write();
  mode.set_depth_test(GsTest::ZTest::GEQUAL);

  // check these
  mode.disable_ab();
  mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
  mode.set_tcc(info.tex0.tcc());
  mode.set_decal(info.tex0.tfx() == GsTex0::TextureFunction::DECAL);
  mode.set_filt_enable(info.tex1.mmag());

  // the alpha matters (maybe?)
  update_mode_from_alpha1(info.alpha, mode);

  // the clamp matters
  if (!(info.clamp == 0b101 || info.clamp == 0 || info.clamp == 1 || info.clamp == 0b100)) {
    ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", info.clamp));
  }

  mode.set_clamp_s_enable(info.clamp & 0b1);
  mode.set_clamp_t_enable(info.clamp & 0b100);

  return mode;
}

float u32_as_float(u32 in) {
  float out;
  memcpy(&out, &in, sizeof(float));
  return out;
}

u32 float_as_u32(float in) {
  u32 out;
  memcpy(&out, &in, sizeof(float));
  return out;
}
}  // namespace

/*!
 * Representing something that can be written to the merc output buffer. 1 qw
 */
struct MercOutputQuadword {
  enum class Kind {
    INVALID,       // uninitialized, or in the middle of another thing
    VTX_START,     // the first qw of a vertex
    SHADER_START,  // the first qw of a shader (the giftag)
    PRIM_START     // the giftag for a list of vertices
  } kind = Kind::INVALID;

  // if we're a vertex
  u32 vtx_idx = -1;  // index in the effect's vertex list.
  bool adc = false;  // if our adc flag is set. this differs between copies, so put it here.

  // if we're a primitive giftag, the number of vertices that come after us.
  u32 nloop_count = 0;
};

struct MercMemory {
  std::array<MercOutputQuadword, 1024> memory;
};

/*!
 * Add vertices from a fragment to memory. Emulates the unpacking.
 */
void handle_frag(const std::string& debug_name,
                 const MercCtrlHeader& ctrl_header,
                 const MercFragment& frag,
                 const MercFragmentControl& frag_ctrl,
                 const MercState& state,
                 std::vector<MercUnpackedVtx>& effect_vertices,
                 MercMemory& memory) {
  (void)frag_ctrl;
  (void)debug_name;
  // lg::print("handling frag: {}\n", debug_name);
  // lg::print("{}\n", frag.print());

  // we'll iterate through the lump and rgba data
  int lump_ptr = 0;                     // vertex data starts at the beginning of "lump"
  int rgba_ptr = frag.header.rgba_off;  // rgba is in u4's
  int perc_ptr = frag.header.perc_off;
  int last_mat2_perc = perc_ptr - 1;
  int perc_toggle = 0;

  u32 mat1_cnt = frag.header.mat1_cnt;
  u32 mat12_cnt = frag.header.mat2_cnt + mat1_cnt;
  u32 mat123_cnt = frag.header.mat3_cnt + mat12_cnt;

  // loop through vertices.
  int prev_mat0 = -1;
  int prev_mat1 = -1;
  int prev_mat2 = -1;
  for (size_t i = 0; i < mat123_cnt; i++) {
    u32 current_vtx_idx = effect_vertices.size();  // idx in effect vertex list.
    auto& vtx = effect_vertices.emplace_back();

    if (i < mat1_cnt) {
      vtx.kind = 1;  // 1 matrix
    } else if (i < mat12_cnt) {
      vtx.kind = 2;  // 2 matrix
    } else {
      vtx.kind = 3;
    }

    // the three quadwords in the source data
    auto v0 = frag.lump4_unpacked.at(lump_ptr);
    auto v1 = frag.lump4_unpacked.at(lump_ptr + 1);
    auto v2 = frag.lump4_unpacked.at(lump_ptr + 2);

    // ilwr.x vi08, vi01    ;; load mat0 from vertex
    u16 mat0_addr;
    memcpy(&mat0_addr, &v0.x(), 2);
    u16 mat1_addr;
    memcpy(&mat1_addr, &v0.y(), 2);

    if (vtx.kind == 1) {
      vtx.skel_mats[0] = state.vu1_matrix_slots.at(vu1_addr_to_matrix_slot(mat0_addr));
      vtx.skel_mats[1] = 0;
      vtx.skel_mats[2] = 0;
      vtx.mat_weights[0] = 1.f;
      vtx.mat_weights[1] = 0.f;
      vtx.mat_weights[2] = 0.f;
      ASSERT(vtx.skel_mats[0] >= 0);
    } else if (vtx.kind == 2) {
      u8 m0 = mat0_addr & 0x7f;
      u8 m1 = mat1_addr & 0x7f;
      if (m0 == 0x7f) {
        ASSERT(prev_mat0 != -1);
      } else {
        prev_mat0 = vu1_addr_to_matrix_slot(m0);
        prev_mat1 = vu1_addr_to_matrix_slot(m1);
      }
      vtx.skel_mats[0] = state.vu1_matrix_slots.at(prev_mat0);
      vtx.skel_mats[1] = state.vu1_matrix_slots.at(prev_mat1);
      vtx.skel_mats[2] = 0;

      if (m0 != 0x7f && i != mat1_cnt) {
        if (perc_toggle) {
          perc_ptr++;
        }
        perc_toggle = !perc_toggle;
      }

      auto perc = frag.unsigned_four_including_header.at(perc_ptr);
      last_mat2_perc = perc_ptr;

      if (!perc_toggle) {
        vtx.mat_weights[0] = perc.x() / 255.f;
        vtx.mat_weights[1] = perc.y() / 255.f;
      } else {
        vtx.mat_weights[0] = perc.z() / 255.f;
        vtx.mat_weights[1] = perc.w() / 255.f;
      }
      vtx.mat_weights[2] = 0.f;

      float sum = vtx.mat_weights[0] + vtx.mat_weights[1];
      ASSERT(std::abs(1.f - sum) < 1e-6);
    } else if (vtx.kind == 3) {
      u8 m0 = mat0_addr & 0x7f;
      u8 m1 = mat1_addr & 0x7f;

      if (i == mat12_cnt) {
        perc_ptr = last_mat2_perc;
      }

      if (m0 == 0x7f) {
        ASSERT(prev_mat0 != -1);
      } else {
        perc_ptr++;
        prev_mat0 = vu1_addr_to_matrix_slot(m0);
        prev_mat1 = vu1_addr_to_matrix_slot(m1);
        prev_mat2 = vu1_addr_to_matrix_slot(frag.unsigned_four_including_header.at(perc_ptr).w());
      }
      vtx.skel_mats[0] = state.vu1_matrix_slots.at(prev_mat0);
      vtx.skel_mats[1] = state.vu1_matrix_slots.at(prev_mat1);
      vtx.skel_mats[2] = state.vu1_matrix_slots.at(prev_mat2);

      auto perc = frag.unsigned_four_including_header.at(perc_ptr);
      vtx.mat_weights[0] = perc.x() / 255.f;
      vtx.mat_weights[1] = perc.y() / 255.f;
      vtx.mat_weights[2] = perc.z() / 255.f;

      float sum = vtx.mat_weights[0] + vtx.mat_weights[1] + vtx.mat_weights[2];
      ASSERT(std::abs(1.f - sum) < 1e-6);
    } else {
      ASSERT(false);
    }

    u16 mat1;
    memcpy(&mat1, &v0.y(), 2);
    u16 mat0;
    memcpy(&mat0, &v0.x(), 2);

    // add.zw vf08, vf08, vf17    ;; lump offset
    // vf17 = [2048, 255, -65537, xyz-add.x] (the ?? is set per fragment)
    v0.z() += -65537;
    v0.w() += frag.fp_header.x_add;

    // add.xyzw vf11, vf11, vf18  ;; lump offset
    // vf18 = [st-out-X, st-out-X, -65537, xyz-add.y] (X = a if xtop = 0, X = b otherwise)
    v1.x() += u32_as_float(ctrl_header.st_out_a);
    v1.y() += u32_as_float(ctrl_header.st_out_a);
    v1.z() += -65537;
    v1.w() += frag.fp_header.y_add;

    // add.xyzw vf14, vf14, vf19  ;; lump offset
    // vf19 = [st-magic, st-magic, -65537, xyz-add.z]
    v2.x() += u32_as_float(ctrl_header.st_magic);
    v2.y() += u32_as_float(ctrl_header.st_magic);
    v2.z() += -65537;
    v2.w() += frag.fp_header.z_add;

    vtx.pos = math::Vector3f(v0.w(), v1.w(), v2.w());
    vtx.nrm = math::Vector3f(v0.z(), v1.z(), v2.z());

    // vtx.mat1 = 0;                           // not used like this
    vtx.dst0 = float_as_u32(v1.x()) - 371;  // xtop to output buffer offset
    vtx.dst1 = float_as_u32(v1.y()) - 371;
    vtx.rgba = frag.unsigned_four_including_header.at(rgba_ptr);
    vtx.st = math::Vector2f(v2.x(), v2.y());

    // crazy flag logic to set adc
    s16 mat1_flag = mat1;
    s16 mat0_flag = mat0;
    bool dst0_adc, dst1_adc;
    if (vtx.kind == 1) {
      ASSERT(mat1_flag == -1 || mat1_flag == 0 || mat1_flag == 1);
      dst0_adc = mat1_flag <= 0;
      dst1_adc = dst0_adc && (mat1_flag != 0);
      dst0_adc = !dst0_adc;
      dst1_adc = !dst1_adc;
    } else {
      // adc logic
      //   ilw.y vi09, -6(vi01)
      s16 vi09 = mat1_flag;
      //   move.xyzw vf21, vf08
      bool vf21_has_adc = false;
      bool vf08_has_adc = false;
      if (!(vi09 > 0)) {
        vf21_has_adc = true;
      }
      //   ibgtz vi09, L47
      //
      //   addx.w vf21, vf21, vf17
      //
      // L47:
      //   ilw.x vi09, -9(vi01)
      vi09 = mat0_flag;
      //   ftoi4.xyzw vf21, vf21
      //
      //   sq.xyzw vf21, 2(vi10)
      dst0_adc = !vf21_has_adc;
      if (!(vi09 >= 0)) {
        vf21_has_adc = vf08_has_adc;
      }
      dst1_adc = !vf21_has_adc;
      //   ibgez vi09, L50
      //
      //   ftoi4.xyzw vf21, vf08
      //
      // L50:
      //   sq.xyzw vf21, 2(vi13)

      //      dst0_adc = mat1_flag <= 0;
      //      dst1_adc = dst0_adc && (mat0_flag >= 0);
      //      dst0_adc = !dst0_adc;
      //      dst1_adc = !dst1_adc;
      //      lg::print("{}\n", dst1_adc);
    }

    // write to two spots in memory
    auto& dst0_mem = memory.memory.at(vtx.dst0);
    auto& dst1_mem = memory.memory.at(vtx.dst1);

    dst0_mem.kind = MercOutputQuadword::Kind::VTX_START;
    dst0_mem.vtx_idx = current_vtx_idx;
    dst0_mem.adc = dst0_adc;
    memory.memory.at(vtx.dst0 + 1).kind = MercOutputQuadword::Kind::INVALID;
    memory.memory.at(vtx.dst0 + 2).kind = MercOutputQuadword::Kind::INVALID;

    dst1_mem.kind = MercOutputQuadword::Kind::VTX_START;
    dst1_mem.vtx_idx = current_vtx_idx;
    dst1_mem.adc = dst1_adc;
    memory.memory.at(vtx.dst1 + 1).kind = MercOutputQuadword::Kind::INVALID;
    memory.memory.at(vtx.dst1 + 2).kind = MercOutputQuadword::Kind::INVALID;

    /*
    lg::print("place vertex {} @ {} {}: {} (adc {} {}) {}\n", current_vtx_idx, vtx.dst0, vtx.dst1,
               vtx.pos.to_string_aligned(), dst0_adc, dst1_adc, mat1_flag);
               */

    lump_ptr += 3;  // advance 3 qw
    rgba_ptr++;
  }
}

/*!
 * Build OpenGL index list from a single GIF packet.
 * TODO: should check we aren't putting in x x R x x R
 */
std::vector<u32> index_list_from_packet(u32 vtx_ptr, u32 nloop, const MercMemory& memory) {
  std::vector<u32> result;
  u32 prev_vtx = UINT32_MAX;

  result.push_back(UINT32_MAX);
  while (nloop) {
    auto& vtx_mem = memory.memory.at(vtx_ptr);
    if (vtx_mem.kind == MercOutputQuadword::Kind::VTX_START) {
      bool adc = vtx_mem.adc;
      if (adc) {
        result.push_back(vtx_mem.vtx_idx);
      } else {
        result.push_back(UINT32_MAX);
        result.push_back(prev_vtx);
        result.push_back(vtx_mem.vtx_idx);
      }
      prev_vtx = vtx_mem.vtx_idx;
    } else {
      // missing vertex!
      lg::warn("MISSING VERTEX at {}", vtx_ptr);
      result.push_back(UINT32_MAX);
    }

    vtx_ptr += 3;
    nloop--;
  }

  result.push_back(UINT32_MAX);
  return result;
}

/*!
 * Dump draws to obj file.
 */
std::string debug_dump_to_obj(const std::vector<MercDraw>& draws,
                              const std::vector<MercUnpackedVtx>& vertices) {
  std::string result;
  std::vector<math::Vector4f> verts;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& draw : draws) {
    // add verts...
    int queue[2];
    int q_idx = 0;
    for (size_t ii = 2; ii < draw.indices.size(); ii++) {
      u32 v0 = draw.indices[ii - 2];
      u32 v1 = draw.indices[ii - 1];
      u32 v2 = draw.indices[ii - 0];
      if (v0 != UINT32_MAX && v1 != UINT32_MAX && v2 != UINT32_MAX) {
        faces.emplace_back(v0 + 1, v1 + 1, v2 + 1);
      }
    }
    for (auto& idx : draw.indices) {
      if (idx == UINT32_MAX) {
        q_idx = 0;
      } else {
        if (q_idx >= 2) {
          faces.emplace_back(queue[0] + 1, queue[1] + 1, idx + 1);
        }
        queue[(q_idx++) % 2] = idx;
      }
    }
  }

  for (auto& vtx : vertices) {
    result += fmt::format("v {} {} {}\n", vtx.pos.x() / 1024.f, vtx.pos.y() / 1024.f,
                          vtx.pos.z() / 1024.f);
  }

  for (auto& face : faces) {
    result += fmt::format("f {}/{} {}/{} {}/{}\n", face.x(), face.x(), face.y(), face.y(), face.z(),
                          face.z());
  }

  return result;
}

math::Vector4<u16> vtx_to_rgba_bone_debug(const MercUnpackedVtx& vtx) {
  math::Vector4<u16> result;
  result.fill(0);
  for (int i = 0; i < 3; i++) {
    if (vtx.skel_mats[i] == -1 || vtx.mat_weights[i] == 0) {
      continue;
    }
    u32 rgba_packed = colors::common_colors[vtx.skel_mats[i] % colors::COLOR_COUNT];
    result.x() += ((rgba_packed >> 16) & 0xff) * vtx.mat_weights[i];
    result.y() += ((rgba_packed >> 8) & 0xff) * vtx.mat_weights[i];
    result.z() += ((rgba_packed >> 0) & 0xff) * vtx.mat_weights[i];
  }
  return result;
}

std::string debug_dump_to_ply(const std::vector<MercDraw>& draws,
                              const std::vector<MercUnpackedVtx>& vertices) {
  std::vector<math::Vector4f> verts;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& draw : draws) {
    // add verts...
    for (size_t ii = 2; ii < draw.indices.size(); ii++) {
      u32 v0 = draw.indices[ii - 2];
      u32 v1 = draw.indices[ii - 1];
      u32 v2 = draw.indices[ii - 0];
      if (v0 != UINT32_MAX && v1 != UINT32_MAX && v2 != UINT32_MAX) {
        faces.emplace_back(v0, v1, v2);
      }
    }
  }

  std::string result = fmt::format(
      "ply\nformat ascii 1.0\nelement vertex {}\nproperty float x\nproperty float y\nproperty "
      "float z\nproperty uchar red\nproperty uchar green\nproperty uchar blue\nelement face "
      "{}\nproperty list uchar int vertex_index\nend_header\n",
      vertices.size(), faces.size());

  for (auto& vtx : vertices) {
    auto rgba = vtx_to_rgba_bone_debug(vtx);
    result += fmt::format("{} {} {} {} {} {}\n", vtx.pos.x() / 1024.f, vtx.pos.y() / 1024.f,
                          vtx.pos.z() / 1024.f, rgba[0], rgba[1], rgba[2]);
  }

  for (auto& face : faces) {
    result += fmt::format("3 {} {} {}\n", face.x(), face.y(), face.z());
  }

  return result;
}

ConvertedMercEffect convert_merc_effect(const MercEffect& input_effect,
                                        const MercCtrlHeader& ctrl_header,
                                        const TextureDB& tdb,
                                        const std::vector<level_tools::TextureRemap>& map,
                                        const std::string& debug_name,
                                        size_t ctrl_idx,
                                        size_t effect_idx,
                                        bool dump) {
  ConvertedMercEffect result;
  result.ctrl_idx = ctrl_idx;
  result.effect_idx = effect_idx;
  // full reset of state per effect.
  // we have no idea what the previous effect draw will be - it might be given to
  // mercneric.
  bool shader_set = false;       // no previous shader can reliably be known
  MercState merc_state;          // current gs settings/matrix slots
  MercMemory merc_memories[2];   // double buffered output
  int memory_buffer_toggle = 0;  // which output we're in

  for (size_t fi = 0; fi < input_effect.frag_ctrl.size(); fi++) {
    const auto& frag = input_effect.frag_geo[fi];
    const auto& frag_ctrl = input_effect.frag_ctrl[fi];

    // first, deal with matrices
    for (size_t mi = 0; mi < frag_ctrl.mat_xfer_count; mi++) {
      const auto& xfer = frag_ctrl.mat_dest_data[mi];
      merc_state.vu1_matrix_slots[vu1_addr_to_matrix_slot(xfer.matrix_dest)] = xfer.matrix_number;
    }

    if (!shader_set) {
      // if we don't have a shader set, we shouldn't have anything that reuses the last shader
      ASSERT(frag.header.strip_len == 0);
      // and we should set the shader
      ASSERT(frag.fp_header.shader_cnt > 0);
      shader_set = true;
    }

    // run the frag.
    // this will add vertices to the per-effect vertex lists and also update the merc memory
    // to point to these.

    handle_frag(debug_name, ctrl_header, frag, frag_ctrl, merc_state, result.vertices,
                merc_memories[memory_buffer_toggle]);

    // we'll add draws after this draw, but wait to actually populate the index lists until
    // we've processed all the vertices.
    size_t first_draw_to_update = result.draws.size();

    // continuation of a previous shader
    if (frag.header.strip_len) {
      // add the continued draw
      auto& new_draw = result.draws.emplace_back();
      new_draw.ctrl_idx = ctrl_idx;
      new_draw.effect_idx = effect_idx;
      new_draw.frag_idx = fi;
      new_draw.state = merc_state;
      new_draw.vtx_offset = 1;
      // just remember where it started. we might copy some vertices from a later draw in this
      // fragment back to this draw, and at this point we don't know what the adc flags would be.
      // (or their index, because we are deduplicated)
      new_draw.vtx_nloop = frag.header.strip_len;
    }

    // loop over fresh shaders
    for (size_t i = 0; i < frag.fp_header.shader_cnt; i++) {
      const auto& shader = frag.shaders.at(i);
      // update merc state from shader (will hold over to next fragment, if needed)
      merc_state.merc_draw_mode.mode = process_draw_mode(shader);
      u32 new_tex = remap_texture(shader.original_tex, map);

      // texture the texture page/texture index, and convert to a PC port texture ID
      u32 tpage = new_tex >> 20;
      u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
      u32 tex_combo = (((u32)tpage) << 16) | tidx;
      // look up the texture to make sure it's valid
      auto tex = tdb.textures.find(tex_combo);
      ASSERT(tex != tdb.textures.end());
      // remember the texture id
      merc_state.merc_draw_mode.pc_combo_tex_id = tex_combo;

      // add the draw
      auto& new_draw = result.draws.emplace_back();
      new_draw.ctrl_idx = ctrl_idx;
      new_draw.effect_idx = effect_idx;
      new_draw.frag_idx = fi;
      new_draw.state = merc_state;
      // write the shader to memory
      merc_memories[memory_buffer_toggle].memory.at(shader.output_offset).kind =
          MercOutputQuadword::Kind::SHADER_START;
      for (int mi = 1; mi < 6; mi++) {
        // fill 5qw of adgif data with invalid. make sure we don't read verts from here
        merc_memories[memory_buffer_toggle].memory.at(shader.output_offset + mi).kind =
            MercOutputQuadword::Kind::INVALID;
      }
      // write the giftag for primitives
      auto& prim_packet = merc_memories[memory_buffer_toggle].memory.at(shader.output_offset + 6);
      prim_packet.kind = MercOutputQuadword::Kind::PRIM_START;
      prim_packet.nloop_count = shader.next_strip_nloop;

      // update draw from hidden fields in adgif.
      new_draw.vtx_offset = shader.output_offset + 7;
      new_draw.vtx_nloop = shader.next_strip_nloop;
    }

    // copy from other places inside this output buffer
    u32 srcdst_ptr = frag.header.srcdest_off;
    for (u32 sci = 0; sci < frag.header.samecopy_cnt; sci++) {
      auto& cpy = frag.unsigned_four_including_header[srcdst_ptr];
      // lg::print("sci: {}\n", cpy.to_string_hex_byte());
      u32 src = cpy[0];
      auto& vert = merc_memories[memory_buffer_toggle].memory.at(src);
      u32 dst = cpy[1];
      auto& dvert = merc_memories[memory_buffer_toggle].memory.at(dst);
      if (vert.kind == MercOutputQuadword::Kind::VTX_START) {
        dvert = vert;
        if (cpy[3]) {
          // dvert.adc = true;
          dvert.adc = !dvert.adc;
        }
      } else {
        lg::warn("sc missing vert");
        dvert.kind = MercOutputQuadword::Kind::INVALID;
      }

      srcdst_ptr++;
    }

    // "cross" copy from the other output buffer
    for (u32 cci = 0; cci < frag.header.crosscopy_cnt; cci++) {
      auto& cpy = frag.unsigned_four_including_header[srcdst_ptr];
      // lg::print("cci: {}\n", cpy.to_string_hex_byte());
      u32 src = cpy[0];
      auto& vert = merc_memories[memory_buffer_toggle ^ 1].memory.at(src);
      u32 dst = cpy[1];
      auto& dvert = merc_memories[memory_buffer_toggle].memory.at(dst);
      if (vert.kind == MercOutputQuadword::Kind::VTX_START) {
        dvert = vert;
        if (cpy[3]) {
          // dvert.adc = true;
          dvert.adc = !dvert.adc;
        }
      } else {
        lg::warn("cc missing vert");
        dvert.kind = MercOutputQuadword::Kind::INVALID;
      }
      srcdst_ptr++;
    }

    // now that we've copied all vertices, create index lists.
    for (size_t i = first_draw_to_update; i < result.draws.size(); i++) {
      auto& draw = result.draws[i];
      draw.indices = index_list_from_packet(draw.vtx_offset, draw.vtx_nloop,
                                            merc_memories[memory_buffer_toggle]);
    }

    memory_buffer_toggle ^= 1;
  }

  if (dump) {
    file_util::write_text_file(
        file_util::get_file_path(
            {"debug_out/merc", fmt::format("{}_{}.ply", debug_name, effect_idx)}),
        debug_dump_to_ply(result.draws, result.vertices));
  }

  return result;
}

u8 convert_mat(int in) {
  if (in >= 0) {
    return in;
  } else {
    return 0;
  }
}

tfrag3::MercVertex convert_vertex(const MercUnpackedVtx& vtx, float xyz_scale) {
  tfrag3::MercVertex out;
  out.pos[0] = vtx.pos[0] * xyz_scale;
  out.pos[1] = vtx.pos[1] * xyz_scale;
  out.pos[2] = vtx.pos[2] * xyz_scale;
  out.pad0 = 0;
  out.normal[0] = vtx.nrm[0];
  out.normal[1] = vtx.nrm[1];
  out.normal[2] = vtx.nrm[2];
  out.pad1 = 0;
  out.weights[0] = vtx.mat_weights[0];
  out.weights[1] = vtx.mat_weights[1];
  out.weights[2] = vtx.mat_weights[2];
  out.pad2 = 0;
  out.st[0] = vtx.st[0];
  out.st[1] = vtx.st[1];
  out.rgba[0] = vtx.rgba[0];
  out.rgba[1] = vtx.rgba[1];
  out.rgba[2] = vtx.rgba[2];
  out.rgba[3] = vtx.rgba[3];
  out.mats[0] = convert_mat(vtx.skel_mats[0]);
  out.mats[1] = convert_mat(vtx.skel_mats[1]);
  out.mats[2] = convert_mat(vtx.skel_mats[2]);
  out.pad3 = 0;
  return out;
}

/*!
 * Top-level merc extraction
 */
void extract_merc(const ObjectFileData& ag_data,
                  const TextureDB& tex_db,
                  const DecompilerTypeSystem& dts,
                  const std::vector<level_tools::TextureRemap>& map,
                  tfrag3::Level& out,
                  bool dump_level) {
  if (dump_level) {
    file_util::create_dir_if_needed(file_util::get_file_path({"debug_out/merc"}));
  }
  // find all merc-ctrls in the object file
  auto ctrl_locations = find_merc_ctrls(ag_data.linked_data);

  // extract them. this does very basic unpacking of data, as done by the VIF/DMA on PS2.
  std::vector<MercCtrl> ctrls;
  for (auto location : ctrl_locations) {
    auto ctrl = extract_merc_ctrl(ag_data.linked_data, dts, location);
    ctrls.push_back(ctrl);
  }

  // extract draws. this does no regrouping yet.
  std::vector<std::vector<ConvertedMercEffect>> all_effects;
  for (size_t ci = 0; ci < ctrls.size(); ci++) {
    auto& effects_in_ctrl = all_effects.emplace_back();
    for (size_t ei = 0; ei < ctrls[ci].effects.size(); ei++) {
      effects_in_ctrl.push_back(convert_merc_effect(ctrls[ci].effects[ei], ctrls[ci].header, tex_db,
                                                    map, ctrls[ci].name, ci, ei, dump_level));
    }
  }

  // convert to PC format
  // first pass, before merging indices
  u32 first_model = out.merc_data.models.size();
  std::vector<std::vector<std::vector<std::vector<u32>>>> indices_temp;  // ctrl, effect, draw, vtx
  for (size_t ci = 0; ci < ctrls.size(); ci++) {
    indices_temp.emplace_back();
    auto& pc_ctrl = out.merc_data.models.emplace_back();
    auto& ctrl = ctrls[ci];

    pc_ctrl.name = ctrl.name;
    pc_ctrl.max_draws = 0;
    pc_ctrl.max_bones = 0;

    for (size_t ei = 0; ei < ctrls[ci].effects.size(); ei++) {
      indices_temp[ci].emplace_back();
      auto& pc_effect = pc_ctrl.effects.emplace_back();
      auto& effect = all_effects[ci][ei];
      u32 first_vertex = out.merc_data.vertices.size();
      for (auto& vtx : effect.vertices) {
        auto cvtx = convert_vertex(vtx, ctrl.header.xyz_scale);
        out.merc_data.vertices.push_back(cvtx);
        for (int i = 0; i < 3; i++) {
          pc_ctrl.max_bones = std::max(pc_ctrl.max_bones, (u32)cvtx.mats[i]);
        }
      }

      // can do two types of de-duplication: toggling back and forth shaders and matrices
      std::map<u64, u64> draw_mode_dedup;

      for (auto& draw : effect.draws) {
        pc_ctrl.max_draws++;
        indices_temp[ci][ei].emplace_back();
        // find draw to add to, or create a new one
        const auto& existing = draw_mode_dedup.find(draw.state.merc_draw_mode.as_u64());
        tfrag3::MercDraw* pc_draw = nullptr;
        u64 pc_draw_idx = -1;
        if (existing == draw_mode_dedup.end()) {
          pc_draw_idx = pc_effect.draws.size();
          draw_mode_dedup[draw.state.merc_draw_mode.as_u64()] = pc_draw_idx;
          pc_draw = &pc_effect.draws.emplace_back();
          pc_draw->mode = draw.state.merc_draw_mode.mode;

          u32 idx_in_level_texture = UINT32_MAX;
          for (u32 i = 0; i < out.textures.size(); i++) {
            if (out.textures[i].combo_id == draw.state.merc_draw_mode.pc_combo_tex_id) {
              idx_in_level_texture = i;
              break;
            }
          }

          if (idx_in_level_texture == UINT32_MAX) {
            // not added to level, add it
            auto tex_it = tex_db.textures.find(draw.state.merc_draw_mode.pc_combo_tex_id);
            if (tex_it == tex_db.textures.end()) {
              ASSERT(false);
            } else {
              idx_in_level_texture = out.textures.size();
              auto& new_tex = out.textures.emplace_back();
              new_tex.combo_id = draw.state.merc_draw_mode.pc_combo_tex_id;
              new_tex.w = tex_it->second.w;
              new_tex.h = tex_it->second.h;
              new_tex.debug_name = tex_it->second.name;
              new_tex.debug_tpage_name = tex_db.tpage_names.at(tex_it->second.page);
              new_tex.data = tex_it->second.rgba_bytes;
            }
          }

          pc_draw->tree_tex_id = idx_in_level_texture;
        } else {
          pc_draw_idx = existing->second;
          pc_draw = &pc_effect.draws.at(pc_draw_idx);
        }

        for (auto idx : draw.indices) {
          if (idx == UINT32_MAX) {
            indices_temp[ci][ei][pc_draw_idx].push_back(idx);
          } else {
            indices_temp[ci][ei][pc_draw_idx].push_back(idx + first_vertex);
          }
        }
      }
    }
  }

  // merge indices
  for (size_t ci = 0; ci < ctrls.size(); ci++) {
    auto& pc_ctrl = out.merc_data.models.at(ci + first_model);
    for (size_t ei = 0; ei < ctrls[ci].effects.size(); ei++) {
      auto& pc_effect = pc_ctrl.effects.at(ei);
      for (size_t di = 0; di < pc_effect.draws.size(); di++) {
        auto& pc_draw = pc_effect.draws.at(di);
        auto& inds = indices_temp[ci][ei][di];
        pc_draw.num_triangles = clean_up_vertex_indices(inds);
        pc_draw.first_index = out.merc_data.indices.size();
        pc_draw.index_count = inds.size();
        out.merc_data.indices.insert(out.merc_data.indices.end(), inds.begin(), inds.end());
      }
    }
  }
}
}  // namespace decompiler
