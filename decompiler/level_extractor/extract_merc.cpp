#include "extract_merc.h"

#include "common/log/log.h"
#include "common/util/BitUtils.h"
#include "common/util/FileUtil.h"
#include "common/util/colors.h"
#include "common/util/string_util.h"

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

  bool can_be_modified = false;
  int idx_in_combined_lump4 = -1;  // divided by 3

  int flump4 = -1;
  int frag = -1;
};

/*!
 * A single vertex in a single blend shape target.
 * This is used to store both the base position and offsets.
 * These integers are for the EE-format merc int data, before VIF/VU processing.
 */
struct BlercVtxIntTarget {
  math::Vector<s8, 3> pos;
  math::Vector<s8, 3> nrm;
  u8 idx = 0;  // if this is a non-base vertex (an offset), the target index

  /*!
   * Are all components zero? (if so, this vertex is not effected by this target).
   */
  bool all_zero_data() const {
    return pos == math::Vector<s8, 3>::zero() && nrm == math::Vector<s8, 3>::zero();
  }
};

/*!
 * A single vertex and all of its blend targets.
 */
struct BlercVtxInt {
  // the base position. If blend shapes are off, or weights all 0, then this position is used.
  BlercVtxIntTarget base;
  // the offsets for each target. Stored sparsely so targets that don't move the vertex
  // from the base position aren't stored.
  std::vector<BlercVtxIntTarget> targets;
  // the index of this vertex in the "lump4" EE-format merc data. before samecopy/crosscopy
  // processing
  u32 lump4_addr = 0;
  // the floating point offset specified in the fp_header that will be applied during rendering.
  math::Vector3f pos_offset;
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
  std::vector<u32> verts_per_frag;
  bool has_envmap = false;
  DrawMode envmap_mode;
  u32 envmap_texture;
  std::optional<s8> eye_slot;
  float pos_scale = 0;

  // note: these vertices are _not_ in the same order as vertices.
  // these are in the order they appeared in EE-memory. Some vertices may not use blerc.
  // the only way to map these to other vertices is to use lump4_addrs
  std::vector<BlercVtxInt> blerc_vertices_i;
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
  ctrl.from_ref(tr, dts, file.version);  // the merc data import
  return ctrl;
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
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::DEST && reg.d_mode() == GsAlpha::BlendMode::DEST) {
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_DST_DST);
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
DrawMode process_draw_mode(const MercShader& info,
                           bool enable_alpha_test,
                           bool enable_alpha_blend,
                           bool depth_write,
                           bool fge) {
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
  mode.set_at(enable_alpha_test);
  mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  mode.set_aref(0x26);
  mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
  mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  mode.enable_zt();
  mode.set_depth_write_enable(depth_write);
  mode.set_depth_test(GsTest::ZTest::GEQUAL);

  // check these
  mode.set_ab(enable_alpha_blend);
  mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
  mode.set_tcc(info.tex0.tcc());
  mode.set_decal(info.tex0.tfx() == GsTex0::TextureFunction::DECAL);
  if (info.tex0.tfx() != GsTex0::TextureFunction::DECAL) {
    ASSERT(info.tex0.tfx() == GsTex0::TextureFunction::MODULATE);
  }

  mode.set_filt_enable(info.tex1.mmag());

  // the alpha matters (maybe?)
  update_mode_from_alpha1(info.alpha, mode);

  // the clamp matters
  if (!(info.clamp == 0b101 || info.clamp == 0 || info.clamp == 1 || info.clamp == 0b100)) {
    ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", info.clamp));
  }

  mode.set_clamp_s_enable(info.clamp & 0b1);
  mode.set_clamp_t_enable(info.clamp & 0b100);

  mode.set_fog(fge);

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
                 MercMemory& memory,
                 bool can_be_modified,
                 int base_lump4,
                 int frag_idx) {
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
    vtx.can_be_modified = can_be_modified;
    vtx.idx_in_combined_lump4 = lump_ptr / 3 + base_lump4;
    vtx.frag = frag_idx;
    vtx.flump4 = lump_ptr / 3;

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

s32 find_or_add_texture_to_level(tfrag3::Level& out,
                                 const TextureDB& tex_db,
                                 const std::string& debug_name,
                                 u32 pc_combo_tex_id,
                                 const MercCtrlHeader& hdr,
                                 u8* eye_out,
                                 GameVersion version) {
  s32 idx_in_level_texture = INT32_MAX;
  for (s32 i = 0; i < (int)out.textures.size(); i++) {
    if (out.textures[i].combo_id == pc_combo_tex_id) {
      idx_in_level_texture = i;
      break;
    }
  }

  if (idx_in_level_texture == INT32_MAX) {
    // not added to level, add it
    auto tex_it = tex_db.textures.find(pc_combo_tex_id);
    if (tex_it == tex_db.textures.end()) {
      if (pc_combo_tex_id == 0 && debug_name == "yakow-lod0") {
        // this texture is missing in the real game, and it ends up using an invalid texture
        // configuration, making it completely black. Instead of that, just pick a similar-ish
        // yakow fur texture. It's not perfect, but it's better than nothing.
        for (size_t i = 0; i < out.textures.size(); i++) {
          auto& existing = out.textures[i];
          if (existing.debug_name == "yak-medfur-end") {
            idx_in_level_texture = i;
            break;
          }
        }
      } else {
        lg::error("merc failed to find texture: 0x{:x} for {}. Should be in tpage {}",
                  pc_combo_tex_id, debug_name, pc_combo_tex_id >> 16);
        idx_in_level_texture = 0;
      }
    } else {
      idx_in_level_texture = out.textures.size();
      auto& new_tex = out.textures.emplace_back();
      new_tex.combo_id = pc_combo_tex_id;
      new_tex.w = tex_it->second.w;
      new_tex.h = tex_it->second.h;
      new_tex.debug_name = tex_it->second.name;
      new_tex.debug_tpage_name = tex_db.tpage_names.at(tex_it->second.page);
      new_tex.data = tex_it->second.rgba_bytes;
    }
  }

  // check eyes
  u32 eye_tpage = version == GameVersion::Jak2 ? 0x70c : 0x1cf;
  u32 left_id = version == GameVersion::Jak2 ? 7 : 0x6f;
  u32 right_id = version == GameVersion::Jak2 ? 8 : 0x70;

  if (eye_out && (pc_combo_tex_id >> 16) == eye_tpage) {
    auto tex_it = tex_db.textures.find(pc_combo_tex_id);
    if (tex_it == tex_db.textures.end()) {
      // fmt::print("{} got dynamic merc texture (no known texture)\n", debug_name);
    } else {
      // fmt::print("{} got dynamic merc texture (will overwrite {})\n", debug_name,
      //                  tex_it->second.name);
    }
    u32 idx = pc_combo_tex_id & 0xffff;

    if (idx == left_id || idx == right_id) {
      if (!hdr.eye_ctrl) {
        fmt::print("no eye ctrl, but expected one");
        if (debug_name != "kor-break-lod0") {
          ASSERT(false);
        }
      }
      if (idx == left_id) {
        *eye_out = (hdr.eye_ctrl->eye_slot * 2);
      } else if (idx == right_id) {
        *eye_out = (hdr.eye_ctrl->eye_slot * 2) + 1;
      }
    } else {
      // fmt::print("got unknown tex id in eye page: {}\n", idx);
    }
  }

  // check anim output
  const auto& level_tex = out.textures.at(idx_in_level_texture);
  const auto& it = tex_db.animated_tex_output_to_anim_slot.find(level_tex.debug_name);
  if (it != tex_db.animated_tex_output_to_anim_slot.end()) {
    return -int(it->second) - 1;
  }
  return idx_in_level_texture;
}

ConvertedMercEffect convert_merc_effect(const MercEffect& input_effect,
                                        const MercCtrlHeader& ctrl_header,
                                        const std::vector<level_tools::TextureRemap>& map,
                                        const std::string& debug_name,
                                        size_t ctrl_idx,
                                        size_t effect_idx,
                                        bool dump,
                                        const TextureDB& tex_db,
                                        tfrag3::Level& out,
                                        GameVersion version) {
  ConvertedMercEffect result;
  result.ctrl_idx = ctrl_idx;
  result.effect_idx = effect_idx;
  result.pos_scale = ctrl_header.xyz_scale;

  if (ctrl_header.eye_ctrl) {
    result.eye_slot = ctrl_header.eye_ctrl->eye_slot;
  }
  if (input_effect.extra_info.shader) {
    result.has_envmap = true;
    result.envmap_mode =
        process_draw_mode(*input_effect.extra_info.shader, false, false, false, false);
    result.envmap_mode.set_ab(true);
    u32 new_tex = remap_texture(input_effect.extra_info.shader->original_tex, map);
    ASSERT(result.envmap_mode.get_tcc_enable());
    ASSERT(result.envmap_mode.get_alpha_blend() == DrawMode::AlphaBlend::SRC_0_DST_DST);

    // texture the texture page/texture index, and convert to a PC port texture ID
    u32 tpage = new_tex >> 20;
    u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
    u32 tex_combo = (((u32)tpage) << 16) | tidx;
    result.envmap_texture = find_or_add_texture_to_level(out, tex_db, "envmap", tex_combo,
                                                         ctrl_header, nullptr, version);
  } else if (input_effect.envmap_or_effect_usage) {
    u32 tex_combo = 0;
    switch (version) {
      case GameVersion::Jak1: {
        u32 env = 0x10000000;  // jak 1, check for jak 2.
        u32 tpage = env >> 20;
        u32 tidx = (env >> 8) & 0b1111'1111'1111;
        tex_combo = (((u32)tpage) << 16) | tidx;
      } break;
      case GameVersion::Jak2:
      case GameVersion::Jak3: {
        u32 tpage = 0x1f;
        u32 tidx = 2;
        tex_combo = (((u32)tpage) << 16) | tidx;
      } break;
      default:
        ASSERT_NOT_REACHED();
    }

    result.envmap_texture = find_or_add_texture_to_level(out, tex_db, "envmap-default", tex_combo,
                                                         ctrl_header, nullptr, version);

    DrawMode mode;
    mode.set_at(false);
    mode.enable_zt();
    mode.enable_depth_write();
    mode.set_depth_test(GsTest::ZTest::GEQUAL);

    // check these
    mode.disable_ab();
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_DST_DST);
    mode.set_tcc(true);
    mode.set_decal(false);
    mode.set_filt_enable(true);

    mode.set_clamp_s_enable(true);
    mode.set_clamp_t_enable(true);

    result.has_envmap = true;
    result.envmap_mode = mode;
    result.envmap_mode.set_ab(true);
  }

  bool use_alpha_blend = false;
  bool depth_write = true;
  // TODO check jak 3
  if (version >= GameVersion::Jak2) {
    constexpr int kWaterTexture = 4;
    constexpr int kAlphaTexture = 3;
    if (input_effect.texture_index == kAlphaTexture) {
      use_alpha_blend = true;
    }
    if (input_effect.texture_index == kWaterTexture) {
      depth_write = false;
      use_alpha_blend = true;
    }
  }

  // full reset of state per effect.
  // we have no idea what the previous effect draw will be - it might be given to
  // mercneric.
  bool shader_set = false;       // no previous shader can reliably be known
  MercState merc_state;          // current gs settings/matrix slots
  MercMemory merc_memories[2];   // double buffered output
  int memory_buffer_toggle = 0;  // which output we're in

  int combined_lump4_addr = 0;

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
    bool can_be_modified = false;
    if (fi < input_effect.blend_ctrl.size()) {
      can_be_modified = input_effect.blend_ctrl.at(fi).blend_vtx_count > 0;
    }

    if (input_effect.effect_bits & kRippleEffectBit) {
      can_be_modified = true;
    }

    if (input_effect.effect_bits & kTextureScrollEffectBit) {
      can_be_modified = true;
    }

    if (input_effect.effect_bits & kTransEffectBit) {
      use_alpha_blend = true;
    }

    handle_frag(debug_name, ctrl_header, frag, frag_ctrl, merc_state, result.vertices,
                merc_memories[memory_buffer_toggle], can_be_modified, combined_lump4_addr, fi);
    u32 vert_count = frag.lump4_unpacked.size() / 3;
    combined_lump4_addr += vert_count;
    result.verts_per_frag.push_back(vert_count);

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
      bool fog = true;
      merc_state.merc_draw_mode.mode =
          process_draw_mode(shader, result.has_envmap, use_alpha_blend, depth_write, fog);
      if (!merc_state.merc_draw_mode.mode.get_tcc_enable()) {
        ASSERT(false);
      }
      u32 new_tex = remap_texture(shader.original_tex, map);

      // texture the texture page/texture index, and convert to a PC port texture ID
      u32 tpage = new_tex >> 20;
      u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
      u32 tex_combo = (((u32)tpage) << 16) | tidx;

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

  // process blend fragments. this loop combines all fragments in this effect, and unpacks the int
  // data.
  // each blend fragment corresponds to a normal fragment.
  // the vertices also match up, but the blend fragment may be smaller, allowing them to skip
  // vertices at the end of fragments.
  // (also, there can be fewer blend frags, and the remaining frags just don't have blerc)
  int base_blend_out = 0;
  for (size_t i = 0; i < input_effect.blend_frag_count; i++) {
    // these three structures are all associated:
    auto& bc = input_effect.blend_ctrl.at(i);
    auto& bd = input_effect.blend_data.at(i);
    auto& f = input_effect.frag_geo.at(i).fp_header;

    // index in blend int data.
    int bdi = 0;

    size_t original_size = result.blerc_vertices_i.size();
    result.blerc_vertices_i.resize(original_size + bc.blend_vtx_count);
    BlercVtxInt* out_vertices = nullptr;
    if (bc.blend_vtx_count) {
      out_vertices = &result.blerc_vertices_i[original_size];
    }

    // the base position of this vertex.
    for (int vi = 0; vi < bc.blend_vtx_count; vi++) {
      auto& out_vertex = out_vertices[vi];
      out_vertex.lump4_addr = base_blend_out + vi;
      out_vertex.pos_offset = math::Vector3f(f.x_add, f.y_add, f.z_add);
      auto& vc = out_vertex.base;
      vc.nrm.x() = bd.u8_data.at(bdi++);
      vc.pos.x() = bd.u8_data.at(bdi++);

      vc.nrm.y() = bd.u8_data.at(bdi++);
      vc.pos.y() = bd.u8_data.at(bdi++);

      vc.nrm.z() = bd.u8_data.at(bdi++);
      vc.pos.z() = bd.u8_data.at(bdi++);
    }
    // align16 for DMA (transferred per group)
    bdi = align16(bdi);

    // next, add targets by
    for (size_t ti = 0; ti < bc.bt_index.size(); ti++) {
      if (bc.bt_index[ti] == 0) {
        // this fragment isn't used by this target, skip it.
        // (they also don't store the offsets for this)
        continue;
      }

      for (int vi = 0; vi < bc.blend_vtx_count; vi++) {
        BlercVtxIntTarget vc;
        vc.idx = ti;

        vc.nrm.x() = bd.u8_data.at(bdi++);
        vc.pos.x() = bd.u8_data.at(bdi++);

        vc.nrm.y() = bd.u8_data.at(bdi++);
        vc.pos.y() = bd.u8_data.at(bdi++);

        vc.nrm.z() = bd.u8_data.at(bdi++);
        vc.pos.z() = bd.u8_data.at(bdi++);

        // some vertices within a fragment may not use all the targets of that fragment.
        // detect this and skip adding the 0 offsets, so the downstream stuff can skip adding
        // this to the file.
        if (!vc.all_zero_data()) {
          out_vertices[vi].targets.push_back(vc);
        }
      }
      // for DMA
      bdi = align16(bdi);
    }
    // we should have processed all the u8 data
    ASSERT((size_t)align16(bdi) == bd.u8_data.size());

    // skip over vertices that don't have blend.
    base_blend_out += result.verts_per_frag.at(i);
  }

  if (dump) {
    auto file_path = file_util::get_file_path(
        {"debug_out/merc", fmt::format("{}_{}.ply", debug_name, effect_idx)});
    file_util::create_dir_if_needed_for_file(file_path);
    file_util::write_text_file(file_path, debug_dump_to_ply(result.draws, result.vertices));
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

struct VertexSourceInfo {
  int combined_lump4;
  int frag;
  int flump4;
};

// ND used VIF to do int->float conversion.
// This is a little tricky because you can only do an int + int, then interpret that as a float.
// This resulting conversion is linear (for some range of input):
// out = in * scale + offset

// where scale and offset are the values returned below (assuming in is small)

float magic_float_scale(u32 base_val) {
  float a, b;
  memcpy(&a, &base_val, 4);
  base_val++;
  memcpy(&b, &base_val, 4);
  return b - a;
}

float magic_float_offset(u32 base_val) {
  float a;
  memcpy(&a, &base_val, 4);
  return a;
}

/*!
 * The offset (or base position) of a vertex target (or base position), stored as a float.
 */
struct BlercVtxFloatTarget {
  math::Vector3f pos;
  math::Vector3f nrm;
  u8 idx;  // if an offset, the index of the target we belong to.
};

/*!
 * A single vertex (floating point), and all the offsets for all the targets it uses.
 * These floats are exactly the floats used by the merc2 renderer (pre-bones), so they can
 * be uploaded into mod buffers directly.
 */
struct BlercVtxFloat {
  BlercVtxFloatTarget base;
  std::vector<BlercVtxFloatTarget> targets;
  s32 dest = -1;  // the index of this vertex in the mod buffer (PC vertex ordering)
};

/*!
 * Convert a vertex offset (or base position) from int format (EE lump) to floating point.
 */
BlercVtxFloatTarget blerc_vertex_convert(const BlercVtxIntTarget& in,
                                         const math::Vector<float, 3>& pos_offset,
                                         float pos_scale,
                                         bool is_base) {
  BlercVtxFloatTarget result;
  result.idx = in.idx;

  // scale factors to apply. these integers match the row value for VIF.
  float pos_total_scale = magic_float_scale(0x4b010000) * pos_scale;
  float nrm_total_scale = magic_float_scale(0x47800000);

  if (is_base) {
    // the EE assembly was:
    // pextlb t6, r0, t6 to get u16's from the packed u8's, so these should be treated as unsigned.
    result.nrm = in.nrm.cast<u8>().cast<float>() * nrm_total_scale;
    result.pos = in.pos.cast<u8>().cast<float>() * pos_total_scale;

    // also include the floating point offset from...
    math::Vector3f post_pos_off;
    post_pos_off.fill(magic_float_offset(0x4b010000));  // the vif integer add
    post_pos_off += pos_offset;                         // the offset in the fp header
    post_pos_off *= pos_scale;                          // and scale this by xyz-scale.

    math::Vector3f nrm_off;
    nrm_off.fill(magic_float_offset(0x47800000) - 65537.f);  // 65537.f is part of MERC

    result.pos += post_pos_off;
    result.nrm += nrm_off;
  } else {
    // in the target case, the s8's were sign extended, so cast from s8 -> float directly.
    // all the offset are applied after the sum, so we don't need to include them here
    // (we include them once in the base).
    // pextlb t5, t5, r0
    // psrah t5, t5, 8
    result.pos = in.pos.cast<float>() * pos_total_scale;
    result.nrm = in.nrm.cast<float>() * nrm_total_scale;
  }
  return result;
}

/*!
 * Convert a vertex and its targets from int format to floating point. The floating point format
 * here matches the PC merc float format exactly.
 */
BlercVtxFloat blerc_vertex_convert(const BlercVtxInt& in, float pos_scale) {
  BlercVtxFloat result;
  result.base = blerc_vertex_convert(in.base, in.pos_offset, pos_scale, true);
  for (auto& t : in.targets) {
    result.targets.push_back(blerc_vertex_convert(t, in.pos_offset, pos_scale, false));
  }
  return result;
}

/*!
 * Convert floating point data for a single blerc vertex target to the format used by PC blerc code.
 * This includes padding to match the GPU vertex format and keep the vectors 16-byte aligned.
 */
tfrag3::BlercFloatData to_float_data(const math::Vector3f& pos,
                                     const math::Vector3f& nrm,
                                     float scale) {
  tfrag3::BlercFloatData result;
  result.v[0] = pos.x() * scale;
  result.v[1] = pos.y() * scale;
  result.v[2] = pos.z() * scale;
  result.v[3] = 0;
  result.v[4] = nrm.x() * scale;
  result.v[5] = nrm.y() * scale;
  result.v[6] = nrm.z() * scale;
  result.v[7] = 0;
  return result;
}

/*!
 * Pack floating point vertices to the format for PC blerc.
 * Currently this is designed with an outer loop over vertices. It's probably not the best thing,
 * but it still beats the unoptimized version by a few times.
 */
tfrag3::Blerc blerc_pack(const std::vector<BlercVtxFloat>& verts) {
  tfrag3::Blerc blerc;
  for (auto& v : verts) {
    // this check is weird, but it discards blerc vertices that don't map to any PC mod vertex.
    // why does this happen? I'm not sure. It only happens on 3 vertices in metalkor, crocadog, and
    // kor. It could be that merc has some extra vertices that are unpacked twice.
    if (v.dest >= 0) {
      // base is multiplied by 8192, then left shifted by 13, so it cancels
      blerc.float_data.push_back(to_float_data(v.base.pos, v.base.nrm, 1.f));
      for (auto& t : v.targets) {
        // target is multiplied by "weight", then left shifted by 13. So full weight is
        // 8192:
        blerc.int_data.push_back(t.idx);
        blerc.float_data.push_back(to_float_data(t.pos, t.nrm, 1.f / 8192.f));
      }
      blerc.int_data.push_back(tfrag3::Blerc::kTargetIdxTerminator);
      blerc.int_data.push_back(v.dest);
    }
  }
  return blerc;
}

void create_modifiable_vertex_data(
    const std::vector<bool>& vtx_mod_flag,
    const std::vector<VertexSourceInfo>& vtx_srcs,
    tfrag3::MercModelGroup& out,
    size_t first_out_vertex,
    size_t first_out_model,
    const std::vector<std::vector<ConvertedMercEffect>>& all_effects) {
  ASSERT(vtx_mod_flag.size() + first_out_vertex == out.vertices.size());

  // we need to be able to modify some vertices at runtime.
  // this can be detected vertex-by-vertex
  // the plan is to find MercEffects that contain modifiable vertices, and provide an alternate way
  // to draw them. In the case where no vertices should be modified, we can fall back to the normal
  // merc drawing path.

  // In this modifiable draw path, there will be a list of "fixed draws", which draw vertices that
  // cannot be modified. This set is known at build-time.
  // The "mod draws" will draw the modifiable vertices. These use the normal index buffer, but
  // index into a per-effect modifiable vertex buffer, not the giant per-FR3 merc vertex buffer.

  // some stats
  [[maybe_unused]] int num_tris = 0;  // all triangles
  [[maybe_unused]] int mod_tris = 0;  // triangles in mod draws

  // loop over models added from this art-group
  for (size_t mi = first_out_model; mi < out.models.size(); mi++) {
    auto& model = out.models.at(mi);
    // loop over "effects" within this model. the pc format merges all fragments in an effect
    // together.

    for (size_t ei = 0; ei < model.effects.size(); ei++) {
      auto& effect = model.effects[ei];

      std::vector<std::vector<u32>> inds_per_mod_draw;

      // loop over draw calls within this effect, and determine if it's fixed, modifiable, or needs
      // to be split up. For mod draws, this just figures which vertices go which the draw, using
      // the indices in the original vertex buffer.
      for (const auto& draw : effect.all_draws) {
        num_tris += draw.num_triangles;

        // first check to see what's in this draw
        bool found_mod = false;
        bool found_fixed = false;
        for (int i = 0; i < (int)draw.index_count; i++) {
          u32 idx = out.indices.at(draw.first_index + i);
          if (idx == UINT32_MAX) {
            continue;
          }
          ASSERT(idx >= first_out_vertex);
          if (vtx_mod_flag.at(idx - first_out_vertex)) {
            found_mod = true;
          } else {
            found_fixed = true;
          }
        }

        if (!found_fixed && !found_mod) {
          // nothing found at all, bad
          ASSERT_NOT_REACHED();
        } else if (found_fixed && !found_mod) {
          // only fixed. can just copy the fixed draw. This can reuse the index buffer data
          // we already added for this effect.
          effect.mod.fix_draw.push_back(draw);
        } else if (found_mod && !found_fixed) {
          // only mod. Add the entire draw
          effect.mod.mod_draw.push_back(draw);
          // remember the indices _in the main buffer_ for these vertices.
          auto& inds_out = inds_per_mod_draw.emplace_back();
          for (u32 i = 0; i < draw.index_count; i++) {
            inds_out.push_back(out.indices.at(draw.first_index + i));
          }
          mod_tris += draw.num_triangles;
        } else {
          // it's a mix and needs to be split per strip. Strips containing any mod vertices
          // go in the mod category.

          // build strips as lists of vertex indices.
          std::vector<std::vector<u32>> strips;
          strips.emplace_back();
          for (u32 i = 0; i < draw.index_count; i++) {
            u32 val = out.indices.at(draw.first_index + i);
            if (val == UINT32_MAX) {
              if (!strips.back().empty()) {
                strips.emplace_back();
              }
            } else {
              strips.back().push_back(val);
            }
          }

          // create the two draws
          tfrag3::MercDraw mod = draw;
          tfrag3::MercDraw fix = draw;
          std::vector<u32> mod_ind, fix_ind;
          // iterate over strips and add them to the right one
          for (auto& strip : strips) {
            bool strip_has_mod = false;
            for (auto ind : strip) {
              if (vtx_mod_flag.at(ind - first_out_vertex)) {
                strip_has_mod = true;
                break;
              }
            }
            if (strip_has_mod) {
              mod_ind.insert(mod_ind.end(), strip.begin(), strip.end());
              mod_ind.push_back(UINT32_MAX);
            } else {
              fix_ind.insert(fix_ind.end(), strip.begin(), strip.end());
              fix_ind.push_back(UINT32_MAX);
            }
          }

          mod.index_count = mod_ind.size();
          inds_per_mod_draw.push_back(mod_ind);
          fix.first_index = out.indices.size();
          fix.index_count = fix_ind.size();
          out.indices.insert(out.indices.end(), fix_ind.begin(), fix_ind.end());

          effect.mod.mod_draw.push_back(mod);
          effect.mod.fix_draw.push_back(fix);
        }
      }  // for draw

      // if there are no modifiable draws, we can't possible modify anything, so not worth
      // storing the fixed draws
      if (effect.mod.mod_draw.empty()) {
        effect.mod.fix_draw.clear();
      } else {
        effect.has_mod_draw = true;
        // In this second pass, we need to build the actual vertex buffer and index buffer for
        // the mod draws.

        // the renderer has some optimizations it can decide to use the default value, instead
        // of reading from the game for some subset of the mod vertices.

        // map of original vertices to slot in the mod vtx buffer.
        std::unordered_map<u32, u32> vtx_to_mod_vtx;
        // loop over mod draws
        for (size_t mdi = 0; mdi < effect.mod.mod_draw.size(); mdi++) {
          auto& draw = effect.mod.mod_draw[mdi];
          // indices into the normal vertex buffer for this draw
          auto& orig_inds = inds_per_mod_draw.at(mdi);

          // we'll be adding indices to the end of the main index buffer.
          // these never change, so we want them in the big buffer loaded with the fr3.
          u32 new_first_index = out.indices.size();

          // loop over indices into the normal vertex buffer
          for (auto vidx : orig_inds) {
            if (vidx == UINT32_MAX) {
              out.indices.push_back(UINT32_MAX);
              continue;  // strip restart
            }

            // see if we've already got a copy of this vertex in the mod buffer
            const auto& existing = vtx_to_mod_vtx.find(vidx);
            if (existing == vtx_to_mod_vtx.end()) {
              // nope, add vertex to mod buffer
              auto idx = effect.mod.vertices.size();
              // remember we did this one already
              vtx_to_mod_vtx[vidx] = idx;
              // add the vertex
              effect.mod.vertices.push_back(out.vertices.at(vidx));
              // look up where this one came from in the EE memory layout
              auto src = vtx_srcs.at(vidx - first_out_vertex);
              ASSERT(src.combined_lump4 < UINT16_MAX);
              // add the EE layout index of this vertex to the data, so the runtime
              // knows how to map from EE data to the mod vertex buffer
              effect.mod.vertex_lump4_addr.push_back(src.combined_lump4);

              // also flag that this fragment has modifiable vertices: we want to know
              // which ones are safe to skip.
              u32 frag_idx = src.frag;
              if (frag_idx >= effect.mod.fragment_mask.size()) {
                effect.mod.fragment_mask.resize(frag_idx + 1);
              }
              effect.mod.fragment_mask[frag_idx] = true;
              out.indices.push_back(idx);
            } else {
              // already added this vertex, just reuse it.
              out.indices.push_back(existing->second);
            }
          }
          draw.first_index = new_first_index;
        }

        const auto& og_effect = all_effects.at(mi - first_out_model).at(ei);

        // blerc! The blerc vertex indexing is totally different, so track which blerc vertex
        // goes with the lump4 (ee layout addr).
        std::vector<s32> which_blerc_is_at_this_lump4;
        std::vector<BlercVtxFloat> blerc_floats;

        // convert blerc from int to float, and fill out the lump4 map.
        for (size_t i = 0; i < og_effect.blerc_vertices_i.size(); i++) {
          auto& bvi = og_effect.blerc_vertices_i[i];
          if (bvi.lump4_addr >= which_blerc_is_at_this_lump4.size()) {
            which_blerc_is_at_this_lump4.resize(bvi.lump4_addr + 1, -1);
          }
          which_blerc_is_at_this_lump4[bvi.lump4_addr] = i;
          blerc_floats.push_back(blerc_vertex_convert(bvi, og_effect.pos_scale));
        }

        // the second part of blerc mapping - tell each vertex where it goes in the
        // mod vertex buffer. This way we don't really care about the order of blerc vertices,
        // and we don't have to consider lump4 at all in the renderer.

        // loop over all mod vertices
        for (u32 vi = 0; vi < effect.mod.vertices.size(); vi++) {
          // figure out its lump4.
          u16 la = effect.mod.vertex_lump4_addr[vi];
          ASSERT(la < UINT16_MAX);
          // check if there's a blerc modifier for this vertex
          if (la < which_blerc_is_at_this_lump4.size()) {
            s32 bi = which_blerc_is_at_this_lump4.at(la);
            if (bi >= 0) {
              // there is! remember this dest.
              blerc_floats[bi].dest = vi;
            }
          }
        }
        effect.mod.blerc = blerc_pack(blerc_floats);

        // this next section is a bit of a hack: the renderer loops over fragments,
        // we know ahead of time that some fragments have no modifiable vertices.
        // we'd like the renderer to just skip over this fragment, and not worry about
        // how many vertices it has. So we effectively splice out the disabled fragments indices.
        // this means that the "combined lump4" counter will skip over these fragments.
        const auto& frag_counts = og_effect.verts_per_frag;
        std::unordered_map<u32, u32> old_to_new;
        u32 old_idx = 0;
        u32 new_idx = 0;
        for (size_t fi = 0; fi < effect.mod.fragment_mask.size(); fi++) {
          if (effect.mod.fragment_mask[fi]) {
            for (u32 vi = 0; vi < frag_counts.at(fi); vi++) {
              old_to_new[old_idx] = new_idx;
              old_idx++;
              new_idx++;
            }
          } else {
            old_idx += frag_counts.at(fi);
          }
        }
        effect.mod.expect_vidx_end = new_idx;
        for (auto& v : effect.mod.vertex_lump4_addr) {
          v = old_to_new.at(v);
        }
      }
    }
  }
}

/*!
 * Top-level merc extraction
 */
void extract_merc(const ObjectFileData& ag_data,
                  const TextureDB& tex_db,
                  const DecompilerTypeSystem& dts,
                  const std::vector<level_tools::TextureRemap>& map,
                  tfrag3::Level& out,
                  bool dump_level,
                  GameVersion version) {
  if (dump_level) {
    file_util::create_dir_if_needed(file_util::get_file_path({"debug_out/merc"}));
  }
  // find all merc-ctrls in the object file
  auto ctrl_locations = find_objects_with_type(ag_data.linked_data, "merc-ctrl");

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
      effects_in_ctrl.push_back(convert_merc_effect(ctrls[ci].effects[ei], ctrls[ci].header, map,
                                                    ctrls[ci].name, ci, ei, dump_level, tex_db, out,
                                                    version));
    }
  }

  size_t first_out_vertex = out.merc_data.vertices.size();
  // convert to PC format
  // first pass, before merging indices
  u32 first_model = out.merc_data.models.size();
  std::vector<bool> vertex_modify_flags;
  std::vector<VertexSourceInfo> vertex_srcs;
  std::vector<std::vector<std::vector<std::vector<u32>>>> indices_temp;  // ctrl, effect, draw, vtx
  for (size_t ci = 0; ci < ctrls.size(); ci++) {
    indices_temp.emplace_back();
    auto& pc_ctrl = out.merc_data.models.emplace_back();
    auto& ctrl = ctrls[ci];

    pc_ctrl.name = ctrl.name;
    pc_ctrl.max_draws = 0;
    pc_ctrl.max_bones = 0;
    pc_ctrl.st_vif_add = ctrl.header.st_vif_add;
    pc_ctrl.st_magic = u32_as_float(ctrl.header.st_magic);
    pc_ctrl.xyz_scale = ctrl.header.xyz_scale;

    for (size_t ei = 0; ei < ctrls[ci].effects.size(); ei++) {
      indices_temp[ci].emplace_back();
      auto& pc_effect = pc_ctrl.effects.emplace_back();
      auto& effect = all_effects[ci][ei];
      pc_effect.has_envmap = effect.has_envmap;
      pc_effect.envmap_texture = effect.envmap_texture;
      pc_effect.envmap_mode = effect.envmap_mode;
      u32 first_vertex = out.merc_data.vertices.size();
      for (auto& vtx : effect.vertices) {
        auto cvtx = convert_vertex(vtx, ctrl.header.xyz_scale);
        vertex_modify_flags.push_back(vtx.can_be_modified);
        vertex_srcs.push_back({vtx.idx_in_combined_lump4, vtx.frag, vtx.flump4});
        out.merc_data.vertices.push_back(cvtx);
        for (int i = 0; i < 3; i++) {
          pc_ctrl.max_bones = std::max(pc_ctrl.max_bones, (u32)cvtx.mats[i]);
        }
      }

      // can do two types of de-duplication: toggling back and forth shaders and matrices
      std::map<u64, u64> draw_mode_dedup;

      for (auto& draw : effect.draws) {
        indices_temp[ci][ei].emplace_back();
        // find draw to add to, or create a new one
        const auto& existing = draw_mode_dedup.find(draw.state.merc_draw_mode.as_u64());
        tfrag3::MercDraw* pc_draw = nullptr;
        u64 pc_draw_idx = -1;
        if (existing == draw_mode_dedup.end()) {
          pc_draw_idx = pc_effect.all_draws.size();
          draw_mode_dedup[draw.state.merc_draw_mode.as_u64()] = pc_draw_idx;
          pc_draw = &pc_effect.all_draws.emplace_back();
          pc_draw->mode = draw.state.merc_draw_mode.mode;
          pc_draw->tree_tex_id = find_or_add_texture_to_level(
              out, tex_db, ctrl.name, draw.state.merc_draw_mode.pc_combo_tex_id, ctrl.header,
              &pc_draw->eye_id, version);
        } else {
          pc_draw_idx = existing->second;
          pc_draw = &pc_effect.all_draws.at(pc_draw_idx);
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
      for (size_t di = 0; di < pc_effect.all_draws.size(); di++) {
        auto& pc_draw = pc_effect.all_draws.at(di);
        auto& inds = indices_temp[ci][ei][di];
        pc_draw.num_triangles = clean_up_vertex_indices(inds);
        pc_draw.first_index = out.merc_data.indices.size();
        pc_draw.index_count = inds.size();
        out.merc_data.indices.insert(out.merc_data.indices.end(), inds.begin(), inds.end());
      }
    }
  }

  create_modifiable_vertex_data(vertex_modify_flags, vertex_srcs, out.merc_data, first_out_vertex,
                                first_model, all_effects);

  // compute max draws
  for (u32 mi = first_model; mi < out.merc_data.models.size(); mi++) {
    auto& model = out.merc_data.models[mi];
    model.max_draws = 0;
    for (auto& e : model.effects) {
      model.max_draws += e.all_draws.size();
      if (e.has_mod_draw) {
        model.max_draws += e.mod.mod_draw.size() + e.mod.fix_draw.size();
      }
    }
  }
}
}  // namespace decompiler
