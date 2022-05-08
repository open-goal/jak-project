#include "extract_merc.h"
#include "decompiler/util/goal_data_reader.h"
#include "decompiler/level_extractor/MercData.h"

namespace decompiler {

// MercV2 design
//

constexpr int MERC_VU1_MATRIX_SLOTS = 18;
constexpr int MERC_MATRIX_STRIDE = 7;

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
 * The GS settings of a merc draw
 */
struct MercGsState {
  // the blending, clamp, etc settings. from adgif shaders
  DrawMode mode;
  // the texture to use, as a "pc combo" texture index.
  u32 pc_combo_tex_id;
  u64 as_u64() const { return (((u64)pc_combo_tex_id) << 32) | mode.as_int(); }
};

/*!
 * The state of the merc renderer, including both the GS state, and stuff left behind in VU
 * memory.  The matrix slots are matrix indices (not memory location). A slot of -1 indicates
 * that there is no known matrix in this slot.
 */
struct MercState {
  MercGsState merc_draw_mode;
  // vu1_matrix_slots[x] = y
  // where x is the slot in VU1 memory, and y is the matrix index for the bones/skeleton stuff.
  std::array<int, MERC_VU1_MATRIX_SLOTS> vu1_matrix_slots;
  MercState() { vu1_matrix_slots.fill(-1); }
};

struct MercDraw {
  size_t ctrl_idx;
  size_t effect_idx;
  size_t frag_idx;
  MercState state;
  // data
};

struct ConvertedMercEffect {
  size_t ctrl_idx;
  size_t effect_idx;
  // draws from all fragments.
  std::vector<MercDraw> draws;
};

MercCtrl extract_merc_ctrl(const LinkedObjectFile& file,
                           const DecompilerTypeSystem& dts,
                           int word_idx) {
  Ref ref;
  ref.data = &file;
  ref.seg = 0;
  ref.byte_offset = word_idx * 4;

  auto tr = typed_ref_from_basic(ref, dts);

  MercCtrl ctrl;
  ctrl.from_ref(tr, dts);
  // fmt::print("{}\n", ctrl.print());
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
// todo move to common
u32 remap_texture(u32 original, const std::vector<level_tools::TextureRemap>& map) {
  auto masked = original & 0xffffff00;
  for (auto& t : map) {
    if (t.original_texid == masked) {
      return t.new_texid | 20;
    }
  }
  return original;
}

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
    fmt::print("unsupported blend: a {} b {} c {} d {}\n", (int)reg.a_mode(), (int)reg.b_mode(),
               (int)reg.c_mode(), (int)reg.d_mode());
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    // ASSERT(false);
  }
}

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
}  // namespace
ConvertedMercEffect convert_merc_effect(const MercEffect& input_effect,
                                        const TextureDB& tdb,
                                        const std::vector<level_tools::TextureRemap>& map,
                                        const std::string& debug_name,
                                        size_t ctrl_idx,
                                        size_t effect_idx) {
  ConvertedMercEffect result;
  result.ctrl_idx = ctrl_idx;
  result.effect_idx = effect_idx;
  // full reset of state per effect.
  // we have no idea what the previous effect draw will be - it might be given to
  // mercneric.
  bool shader_set = false;
  MercState merc_state;

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

    // continuation
    if (frag.header.strip_len) {
      // add the continued draw
      auto& new_draw = result.draws.emplace_back();
      new_draw.ctrl_idx = ctrl_idx;
      new_draw.effect_idx = effect_idx;
      new_draw.frag_idx = fi;
      new_draw.state = merc_state;
      // todo fill out draw data
    }

    for (size_t i = 0; i < frag.fp_header.shader_cnt; i++) {
      const auto& shader = frag.shaders.at(i);
      // update merc state from shader
      merc_state.merc_draw_mode.mode = process_draw_mode(shader);
      u32 new_tex = remap_texture(shader.original_tex, map);

      // texture the texture page/texture index, and convert to a PC port texture ID
      u32 tpage = new_tex >> 20;
      u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
      u32 tex_combo = (((u32)tpage) << 16) | tidx;
      // look up the texture to make sure it's valid
      auto tex = tdb.textures.find(tex_combo);
      ASSERT(tex != tdb.textures.end());
      if (false && shader.original_tex != new_tex) {
        fmt::print("map from 0x{:x} to 0x{:x} ({})\n", shader.original_tex, new_tex,
                   tex->second.name);
      }
      // remember the texture id
      merc_state.merc_draw_mode.pc_combo_tex_id = tex_combo;

      // add the draw
      auto& new_draw = result.draws.emplace_back();
      new_draw.ctrl_idx = ctrl_idx;
      new_draw.effect_idx = effect_idx;
      new_draw.frag_idx = fi;
      new_draw.state = merc_state;
      // todo fill out draw data
    }
  }

  return result;
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
  fmt::print("MERC extract for: {}\n", ag_data.name_in_dgo);

  // find all merc-ctrls in the object file
  auto ctrl_locations = find_merc_ctrls(ag_data.linked_data);
  fmt::print(" found {} merc ctrls\n", ctrl_locations.size());

  // extract them. this does very basic unpacking of data, as done by the VIF/DMA on PS2.
  std::vector<MercCtrl> ctrls;
  for (auto location : ctrl_locations) {
    ctrls.push_back(extract_merc_ctrl(ag_data.linked_data, dts, location));
  }

  // extract draws. this does no regrouping yet.
  std::vector<ConvertedMercEffect> all_effects;
  for (size_t ci = 0; ci < ctrls.size(); ci++) {
    for (size_t ei = 0; ei < ctrls[ci].effects.size(); ei++) {
      all_effects.push_back(
          convert_merc_effect(ctrls[ci].effects[ei], tex_db, map, ctrls[ci].name, ci, ei));
    }
  }
}
}  // namespace decompiler
