#include "extract_merc.h"
#include "decompiler/util/goal_data_reader.h"
#include "decompiler/level_extractor/MercData.h"
#include "common/util/FileUtil.h"

namespace decompiler {

// merc
// - mat1 extraction is partially working
// - if we clear merc memory in between fragments (and skip missing verts) it looks right
// - some vertices are missing that shouldn't be, like parts of the boat
// - maybe it's the samecopy/crosscopy stuff that's not implemented
// - texture info seems right

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
  std::vector<u32> indices;
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

struct MercUnpackedVtx {
  int kind = 0;  // 1, 2, or 3 matrix
  math::Vector3f pos;
  math::Vector3f nrm;
  math::Vector2f st;

  u8 mat0;
  u8 mat1;
  u16 dst0;
  u16 dst1;

  bool dst0_adc = false;
  bool dst1_adc = false;
};

std::string debug_dump_verts_only_to_obj(const std::vector<MercUnpackedVtx>& verts) {
  std::string result;
  for (auto& vert : verts) {
    result += fmt::format("v {} {} {}\n", vert.pos.x(), vert.pos.y(), vert.pos.z());
  }

  return result;
}

struct MercOutputQuadword {
  enum class Kind { INVALID, VTX_START, SHADER_START, PRIM_START } kind = Kind::INVALID;

  // if we're a vertex
  // MercUnpackedVtx vtx;  // the actual vertex
  u32 vtx_idx = -1;     // index in the effect's vertex list.
  u8 vtx_dst_idx = -1;  // which of dst0/dst1 placed us here
  bool adc = false;

  // if we're a prim start
  u32 nloop_count = 0;
  u32 prim_draw_idx = 0;
};

struct MercMemory {
  std::array<MercOutputQuadword, 1024> memory;
};

void handle_frag(const std::string& debug_name,
                 const MercCtrlHeader& ctrl_header,
                 const MercFragment& frag,
                 const MercFragmentControl& frag_ctrl,
                 std::vector<MercUnpackedVtx>& effect_vertices,
                 MercMemory& memory) {
  fmt::print("handling frag: {}\n", debug_name);
  fmt::print("{}\n", frag.print());
  int lump_ptr = 0;

  for (size_t i = 0; i < frag.header.mat1_cnt; i++) {
    u32 current_vtx_idx = effect_vertices.size();
    auto& vtx = effect_vertices.emplace_back();
    vtx.kind = 1;

    auto v0 = frag.lump4_unpacked.at(lump_ptr);
    auto v1 = frag.lump4_unpacked.at(lump_ptr + 1);
    auto v2 = frag.lump4_unpacked.at(lump_ptr + 2);

    // ilwr.x vi08, vi01    ;; load mat0 from vertex
    u16 mat0_addr;
    memcpy(&mat0_addr, &v0.x(), 2);
    vtx.mat0 = vu1_addr_to_matrix_slot(mat0_addr);
    u16 mat1;
    memcpy(&mat1, &v0.y(), 2);

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
    vtx.nrm = math::Vector3f(v0.y(), v1.y(), v1.y());
    vtx.mat1 = 0;                           // not used like this
    vtx.dst0 = float_as_u32(v1.x()) - 371;  // xtop to output buffer offset
    vtx.dst1 = float_as_u32(v1.y()) - 371;

    s16 mat1_flag = mat1;
    ASSERT(mat1_flag == -1 || mat1_flag == 0 || mat1_flag == 1);
    vtx.dst0_adc = mat1_flag <= 0;
    vtx.dst1_adc = vtx.dst0_adc && (mat1_flag != 0);
    vtx.dst0_adc = !vtx.dst0_adc;
    vtx.dst1_adc = !vtx.dst1_adc;

    auto& dst0_mem = memory.memory.at(vtx.dst0);
    auto& dst1_mem = memory.memory.at(vtx.dst1);

    dst0_mem.kind = MercOutputQuadword::Kind::VTX_START;
    dst0_mem.vtx_idx = current_vtx_idx;
    dst0_mem.vtx_dst_idx = 0;
    dst0_mem.adc = vtx.dst0_adc;
    memory.memory.at(vtx.dst0 + 1).kind = MercOutputQuadword::Kind::INVALID;
    memory.memory.at(vtx.dst0 + 2).kind = MercOutputQuadword::Kind::INVALID;

    dst1_mem.kind = MercOutputQuadword::Kind::VTX_START;
    dst1_mem.vtx_idx = current_vtx_idx;
    dst1_mem.vtx_dst_idx = 1;
    dst1_mem.adc = vtx.dst1_adc;
    memory.memory.at(vtx.dst1 + 1).kind = MercOutputQuadword::Kind::INVALID;
    memory.memory.at(vtx.dst1 + 2).kind = MercOutputQuadword::Kind::INVALID;

    fmt::print("place vertex {} @ {} {}: {} (adc {} {}) {}\n", current_vtx_idx, vtx.dst0, vtx.dst1,
               vtx.pos.to_string_aligned(), vtx.dst0_adc, vtx.dst1_adc, mat1_flag);

    lump_ptr += 3;  // advance 3 qw
  }
  fmt::print("lump_ptr is {}\n", lump_ptr);

  //  if (frag.header.mat1_cnt) {
  //
  //    // lump unpack is:
  //    // add.zw vf09, vf09, vf17
  //    // add.xyzw vf12, vf12, vf18
  //    // add.xyzw vf15, vf15, vf19
  //    // with
  //    // vf17 = [2048, 255, -65537, xyz-add.x] (the ?? is set per fragment)
  //    // vf18 = [st-out-X, st-out-X, -65537, xyz-add.y] (X = a if xtop = 0, X = b otherwise)
  //    // vf19 = [st-magic, st-magic, -65537, xyz-add.z]
  //    fmt::print("MAT1: {}\n", get_mat1_matrix_slot(frag, 0));
  //  }
}

std::vector<u32> index_list_from_packet(u32 vtx_ptr,
                                        u32 nloop,
                                        const MercMemory& memory,
                                        const std::vector<MercUnpackedVtx>& vertices) {
  std::vector<u32> result;
  // u32 prev_vtx = UINT32_MAX;

  //  result.push_back(UINT32_MAX);
  //  while (nloop) {
  //    auto& vtx_mem = memory.memory.at(vtx_ptr);
  //    if (vtx_mem.kind == MercOutputQuadword::Kind::VTX_START) {
  //      // fmt::print("found vtx: {}\n", vtx_ptr);
  //      auto& src_vtx = vertices.at(vtx_mem.vtx_idx);
  //      bool adc = vtx_mem.vtx_dst_idx == 0 ? src_vtx.dst0_adc : src_vtx.dst1_adc;
  //      // fmt::print("adc: {} (from {} {})\n", adc, src_vtx.dst0_adc, src_vtx.dst1_adc);
  //      if (adc) {
  //        fmt::print(" doing add case {}\n", vtx_mem.vtx_idx);
  //        result.push_back(vtx_mem.vtx_idx);
  //      } else {
  //        fmt::print(" doing restart case: {} {}\n", prev_vtx, vtx_mem.vtx_idx);
  //        result.push_back(UINT32_MAX);
  //        result.push_back(prev_vtx);
  //        result.push_back(vtx_mem.vtx_idx);
  //      }
  //      prev_vtx = vtx_mem.vtx_idx;
  //    } else {
  //      // missing vertex!
  //      fmt::print("MISSING VERTEX at {}\n", vtx_ptr);
  //      result.push_back(UINT32_MAX);
  //    }
  //
  //    vtx_ptr += 3;
  //    nloop--;
  //  }

  //  result.push_back(UINT32_MAX);

  struct Temp {
    u32 idx = UINT32_MAX;
    bool adc = false;
  };
  std::vector<Temp> temp_indices;
  while (nloop) {
    auto& vtx_mem = memory.memory.at(vtx_ptr);
    auto& next = temp_indices.emplace_back();

    if (vtx_mem.kind == MercOutputQuadword::Kind::VTX_START) {
      auto& src_vtx = vertices.at(vtx_mem.vtx_idx);
      bool adc = vtx_mem.vtx_dst_idx == 0 ? src_vtx.dst0_adc : src_vtx.dst1_adc;
      next.adc = vtx_mem.adc;
      next.idx = vtx_mem.vtx_idx;
      ASSERT(adc == vtx_mem.adc);
      fmt::print("read @ {}, {}: adc: {}\n", vtx_ptr, vtx_mem.vtx_idx, adc);
    } else {
      // missing vertex!
      fmt::print("MISSING VERTEX at {}\n", vtx_ptr);
    }

    vtx_ptr += 3;
    nloop--;
  }

  u32 queue[3] = {0, 0, 0};
  int q_idx = 0;
  for (auto ti : temp_indices) {
    // push vertex to queue
    queue[(q_idx++) % 3] = ti.idx;
    if (q_idx > 2 && ti.adc && queue[0] != UINT32_MAX && queue[1] != UINT32_MAX &&
        queue[2] != UINT32_MAX) {
      result.push_back(queue[q_idx % 3]);
      result.push_back(queue[(q_idx - 1) % 3]);
      result.push_back(queue[(q_idx - 2) % 3]);
    }
  }

  return result;
}

std::string debug_dump_to_obj(const std::vector<MercDraw>& draws,
                              const std::vector<MercUnpackedVtx>& vertices) {
  std::string result;
  std::vector<math::Vector4f> verts;
  // std::vector<math::Vector<float, 2>> tcs;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& draw : draws) {
    // add verts...
    int queue[2];
    int q_idx = 0;
    for (size_t i = 0; i < draw.indices.size(); i += 3) {
      faces.emplace_back(draw.indices[i] + 1, draw.indices[i + 1] + 1, draw.indices[i + 2] + 1);
    }
    //    for (size_t ii = 2; ii < draw.indices.size(); ii++) {
    //      u32 v0 = draw.indices[ii - 2];
    //      u32 v1 = draw.indices[ii - 1];
    //      u32 v2 = draw.indices[ii - 0];
    //      if (v0 != UINT32_MAX && v1 != UINT32_MAX && v2 != UINT32_MAX) {
    //        faces.emplace_back(v0 + 1, v1 + 1, v2 + 1);
    //        fmt::print("add {} {} {}\n", v0, v1, v2);
    //      } else {
    //        fmt::print("SKIP {} {} {}\n", v0, v1, v2);
    //      }
    //    }
    //    for (auto& idx : draw.indices) {
    //      if (idx == UINT32_MAX) {
    //        q_idx = 0;
    //      } else {
    //
    //        if (q_idx >= 2) {
    //          fmt::print("[{}] output: {} {} {}\n", q_idx, queue[0], queue[1], idx);
    //          faces.emplace_back(queue[0] + 1, queue[1] + 1, idx + 1);
    //        }
    //        queue[(q_idx++) % 2] = idx;
    //        fmt::print("push q[{}] = {}\n", q_idx % 2, idx);
    //      }
    //    }
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

ConvertedMercEffect convert_merc_effect(const MercEffect& input_effect,
                                        const MercCtrlHeader& ctrl_header,
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
  std::vector<MercUnpackedVtx> effect_vertices;
  MercMemory merc_memory;

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

    handle_frag(debug_name, ctrl_header, frag, frag_ctrl, effect_vertices, merc_memory);

    // continuation
    if (frag.header.strip_len) {
      // add the continued draw
      auto& new_draw = result.draws.emplace_back();
      new_draw.ctrl_idx = ctrl_idx;
      new_draw.effect_idx = effect_idx;
      new_draw.frag_idx = fi;
      new_draw.state = merc_state;
      // todo fill out draw data
      new_draw.indices =
          index_list_from_packet(1, frag.header.strip_len, merc_memory, effect_vertices);
//      file_util::write_text_file(
//          file_util::get_file_path(
//              {"debug_out/merc", fmt::format("{}_{}_{}s.obj", debug_name, effect_idx, fi)}),
//          debug_dump_to_obj(result.draws, effect_vertices));
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
      size_t draw_idx = result.draws.size();
      auto& new_draw = result.draws.emplace_back();
      new_draw.ctrl_idx = ctrl_idx;
      new_draw.effect_idx = effect_idx;
      new_draw.frag_idx = fi;
      new_draw.state = merc_state;
      // write the shader
      fmt::print("place shader: {}\n", shader.output_offset);
      merc_memory.memory.at(shader.output_offset).kind = MercOutputQuadword::Kind::SHADER_START;
      for (int mi = 1; mi < 6; mi++) {
        merc_memory.memory.at(shader.output_offset + mi).kind = MercOutputQuadword::Kind::INVALID;
      }
      // write the loop
      fmt::print("place prim: {}\n", shader.output_offset + 6);
      auto& prim_packet = merc_memory.memory.at(shader.output_offset + 6);
      prim_packet.kind = MercOutputQuadword::Kind::PRIM_START;
      prim_packet.nloop_count = shader.next_strip_nloop;

      // todo fill out draw data
      fmt::print("shader dest is {}, nloop = {}\n", shader.output_offset, shader.next_strip_nloop);
      new_draw.indices = index_list_from_packet(shader.output_offset + 7, shader.next_strip_nloop,
                                                merc_memory, effect_vertices);
      //      for (auto& vert : unpacked_frag.vertices) {
      //        fmt::print(" v: {} {}\n", vert.dst0, vert.dst1);
      //      }

    }
  }

  file_util::write_text_file(
      file_util::get_file_path(
          {"debug_out/merc", fmt::format("{}_{}.obj", debug_name, effect_idx)}),
      debug_dump_to_obj(result.draws, effect_vertices));


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
      all_effects.push_back(convert_merc_effect(ctrls[ci].effects[ei], ctrls[ci].header, tex_db,
                                                map, ctrls[ci].name, ci, ei));
    }
  }
}
}  // namespace decompiler
