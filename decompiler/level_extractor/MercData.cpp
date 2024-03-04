#include "MercData.h"

#include "common/dma/gs.h"
#include "common/util/BitUtils.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"

#include "fmt/core.h"

namespace decompiler {

void MercEyeCtrl::from_ref(TypedRef tr, const DecompilerTypeSystem& dts) {
  eye_slot = read_plain_data_field<s8>(tr, "eye-slot", dts);
}

void MercCtrlHeader::from_ref(TypedRef tr, const DecompilerTypeSystem& dts, GameVersion) {
  st_magic = read_plain_data_field<u32>(tr, "st-magic", dts);
  xyz_scale = read_plain_data_field<float>(tr, "xyz-scale", dts);
  st_out_a = read_plain_data_field<u32>(tr, "st-out-a", dts);
  st_out_b = read_plain_data_field<u32>(tr, "st-out-b", dts);
  st_vif_add = read_plain_data_field<u32>(tr, "st-vif-add", dts);
  st_int_off = read_plain_data_field<u16>(tr, "st-int-off", dts);
  st_int_scale = read_plain_data_field<u16>(tr, "st-int-scale", dts);
  effect_count = read_plain_data_field<u32>(tr, "effect-count", dts);
  blend_target_count = read_plain_data_field<u32>(tr, "blend-target-count", dts);
  fragment_count = read_plain_data_field<u16>(tr, "fragment-count", dts);
  tri_count = read_plain_data_field<u16>(tr, "tri-count", dts);
  matrix_count = read_plain_data_field<u8>(tr, "matrix-count", dts);
  shader_count = read_plain_data_field<u8>(tr, "shader-count", dts);
  transform_vertex_count = read_plain_data_field<u16>(tr, "transform-vertex-count", dts);
  dvert_count = read_plain_data_field<u16>(tr, "dvert-count", dts);
  one_mat_count = read_plain_data_field<u16>(tr, "one-mat-count", dts);
  two_mat_count = read_plain_data_field<u16>(tr, "two-mat-count", dts);
  two_mat_reuse_count = read_plain_data_field<u16>(tr, "two-mat-reuse-count", dts);
  three_mat_count = read_plain_data_field<u16>(tr, "three-mat-count", dts);
  three_mat_reuse_count = read_plain_data_field<u16>(tr, "three-mat-reuse-count", dts);
  shader_upload_count = read_plain_data_field<u8>(tr, "shader-upload-count", dts);
  matrix_upload_count = read_plain_data_field<u8>(tr, "matrix-upload-count", dts);
  same_copy_count = read_plain_data_field<u16>(tr, "same-copy-count", dts);
  cross_copy_count = read_plain_data_field<u16>(tr, "cross-copy-count", dts);
  num_verts = read_plain_data_field<u16>(tr, "num-verts", dts);
  longest_edge = read_plain_data_field<float>(tr, "longest-edge", dts);
  auto fr = get_field_ref(tr, "eye-ctrl", dts);
  const auto& word = fr.data->words_by_seg.at(fr.seg).at(fr.byte_offset / 4);
  if (word.kind() == LinkedWord::PTR) {
    eye_ctrl.emplace();
    eye_ctrl->from_ref(TypedRef(deref_label(fr), dts.ts.lookup_type("merc-eye-ctrl")), dts);
  }

  // todo masks
  envmap_tint = read_plain_data_field<u32>(tr, "envmap-tint", dts);
  needs_clip = read_plain_data_field<u8>(tr, "needs-clip", dts);
  use_isometric = read_plain_data_field<u8>(tr, "use-isometric", dts);
  use_attached_shader = read_plain_data_field<u8>(tr, "use-attached-shader", dts);
  display_triangles = read_plain_data_field<u8>(tr, "display-triangles", dts);
  death_vertex_skip = read_plain_data_field<u16>(tr, "death-vertex-skip", dts);
  death_start_vertex = read_plain_data_field<u16>(tr, "death-start-vertex", dts);
  death_effect = read_plain_data_field<u32>(tr, "death-effect", dts);
  use_translucent = read_plain_data_field<u8>(tr, "use-translucent", dts);
  display_this_fragment = read_plain_data_field<u8>(tr, "display-this-fragment", dts);
}

std::string MercCtrlHeader::print() const {
  std::string result;
  result += fmt::format("  xyz_scale: {}\n", xyz_scale);
  result += fmt::format("  st_out_a: 0x{:x}\n", st_out_a);
  result += fmt::format("  st_out_b: 0x{:x}\n", st_out_b);
  result += fmt::format("  st_vif_add: 0x{:x}\n", st_vif_add);
  result += fmt::format("  st_int_off: 0x{:x}\n", st_int_off);
  result += fmt::format("  st_int_scale: {}\n", st_int_scale);
  result += fmt::format("  effect_count: {}\n", effect_count);
  result += fmt::format("  blend_target_count: {}\n", blend_target_count);
  result += fmt::format("  fragment_count: {}\n", fragment_count);
  result += fmt::format("  tri_count: {}\n", tri_count);
  result += fmt::format("  matrix_count: {}\n", matrix_count);
  result += fmt::format("  shader_count: {}\n", shader_count);
  result += fmt::format("  transform_vertex_count: {}\n", transform_vertex_count);
  result += fmt::format("  dvert_count: {}\n", dvert_count);
  result += fmt::format("  one_mat_count: {}\n", one_mat_count);
  result += fmt::format("  two_mat_count: {}\n", two_mat_count);
  result += fmt::format("  two_mat_reuse_count: {}\n", two_mat_reuse_count);
  result += fmt::format("  three_mat_count: {}\n", three_mat_count);
  result += fmt::format("  three_mat_reuse_count: {}\n", three_mat_reuse_count);
  result += fmt::format("  shader_upload_count: {}\n", shader_upload_count);
  result += fmt::format("  matrix_upload_count: {}\n", matrix_upload_count);
  result += fmt::format("  same_copy_count: {}\n", same_copy_count);
  result += fmt::format("  cross_copy_count: {}\n", cross_copy_count);
  result += fmt::format("  num_verts: {}\n", num_verts);
  result += fmt::format("  longest_edge: {}\n", longest_edge);
  result += fmt::format("  envmap_tint: {}\n", envmap_tint);
  result += fmt::format("  needs_clip: {}\n", needs_clip);
  result += fmt::format("  use_isometric: {}\n", use_isometric);
  result += fmt::format("  use_attached_shader: {}\n", use_attached_shader);
  result += fmt::format("  display_triangles: {}\n", display_triangles);
  result += fmt::format("  death_vertex_skip: {}\n", death_vertex_skip);
  result += fmt::format("  death_start_vertex: {}\n", death_start_vertex);
  result += fmt::format("  death_effect: {}\n", death_effect);
  result += fmt::format("  use_translucent: {}\n", use_translucent);
  result += fmt::format("  display_this_fragment: {}\n", display_this_fragment);
  return result;
}

TypedRef MercFragmentControl::from_ref(TypedRef tr, const DecompilerTypeSystem& dts) {
  unsigned_four_count = read_plain_data_field<u8>(tr, "unsigned-four-count", dts);
  lump_four_count = read_plain_data_field<u8>(tr, "lump-four-count", dts);
  fp_qwc = read_plain_data_field<u8>(tr, "fp-qwc", dts);
  mat_xfer_count = read_plain_data_field<u8>(tr, "mat-xfer-count", dts);
  ASSERT(mat_xfer_count < 10);
  Ref dest_data_ref = get_field_ref(tr, "mat-dest-data", dts);
  for (u8 i = 0; i < mat_xfer_count; i++) {
    auto& entry = mat_dest_data.emplace_back();
    entry.matrix_number = deref_u8(dest_data_ref, i * 2);
    entry.matrix_dest = deref_u8(dest_data_ref, i * 2 + 1);
  }

  tr.ref.byte_offset += (4 + 2 * mat_dest_data.size());
  return tr;
}

void MercFpHeader::from_ref(TypedRef tr, const DecompilerTypeSystem& dts) {
  x_add = read_plain_data_field<float>(tr, "x-add", dts);
  y_add = read_plain_data_field<float>(tr, "y-add", dts);
  z_add = read_plain_data_field<float>(tr, "z-add", dts);

  shader_cnt = read_plain_data_field<u8>(tr, "shader-cnt", dts);
  kick_info_offset = read_plain_data_field<u8>(tr, "kick-info-offset", dts);
  kick_info_step = read_plain_data_field<u8>(tr, "kick-info-step", dts);
  hword_cnt = read_plain_data_field<u8>(tr, "hword-cnt", dts);
}

std::string MercFpHeader::print() const {
  std::string result;
  result += fmt::format("      x_add: {}\n", x_add);
  result += fmt::format("      y_add: {}\n", y_add);
  result += fmt::format("      z_add: {}\n", z_add);
  result += fmt::format("      shader_cnt: {}\n", shader_cnt);
  result += fmt::format("      kick_info_offset: {}\n", kick_info_offset);
  result += fmt::format("      kick_info_step: {}\n", kick_info_step);
  result += fmt::format("      hword_cnt: {}\n", hword_cnt);
  return result;
}

void MercByteHeader::from_ref(TypedRef tr, const DecompilerTypeSystem& dts) {
  srcdest_off = read_plain_data_field<u8>(tr, "srcdest-off", dts);
  rgba_off = read_plain_data_field<u8>(tr, "rgba-off", dts);
  lump_off = read_plain_data_field<u8>(tr, "lump-off", dts);
  fp_off = read_plain_data_field<u8>(tr, "fp-off", dts);
  mat1_cnt = read_plain_data_field<u8>(tr, "mat1-cnt", dts);
  mat2_cnt = read_plain_data_field<u8>(tr, "mat2-cnt", dts);
  mat3_cnt = read_plain_data_field<u8>(tr, "mat3-cnt", dts);
  samecopy_cnt = read_plain_data_field<u8>(tr, "samecopy-cnt", dts);
  crosscopy_cnt = read_plain_data_field<u8>(tr, "crosscopy-cnt", dts);
  strip_len = read_plain_data_field<u8>(tr, "strip-len", dts);
  mm_quadword_fp_off = read_plain_data_field<u8>(tr, "mm-quadword-fp-off", dts);
  mm_quadword_size = read_plain_data_field<u8>(tr, "mm-quadword-size", dts);
  perc_off = read_plain_data_field<u8>(tr, "perc-off", dts);

  auto ms = get_field_ref(tr, "perc-off", dts);
  bool got_end = false;
  for (int i = 0; i < MAT_SLOTS; i++) {
    ms.byte_offset++;
    mat_slot[i] = deref_u8(ms, 0);
    if (mat_slot[i] == 128) {
      got_end = true;
    } else {
      // ASSERT(!got_end);
      if (got_end) {
        // lg::print("got something after the end\n");  // todo, should investigate more
      }
    }
  }
}

std::string MercByteHeader::print() const {
  std::string result;
  result += fmt::format("      srcdest_off: {}\n", srcdest_off);
  result += fmt::format("      rgba_off: {}\n", rgba_off);
  result += fmt::format("      lump_off: {}\n", lump_off);
  result += fmt::format("      fp_off: {}\n", fp_off);
  result += fmt::format("      mat1_cnt: {}\n", mat1_cnt);
  result += fmt::format("      mat2_cnt: {}\n", mat2_cnt);
  result += fmt::format("      mat3_cnt: {}\n", mat3_cnt);
  result += fmt::format("      samecopy_cnt: {}\n", samecopy_cnt);
  result += fmt::format("      crosscopy_cnt: {}\n", crosscopy_cnt);
  result += fmt::format("      strip_len: {}\n", strip_len);
  result += fmt::format("      mm_quadword_fp_off: {}\n", mm_quadword_fp_off);
  result += fmt::format("      mm_quadword_size: {}\n", mm_quadword_size);
  result += fmt::format("      perc_off: {}\n", perc_off);
  for (int i = 0; i < MAT_SLOTS; i++) {
    result += fmt::format("      mat_slot[{}]: {}\n", i, mat_slot[i]);
  }
  return result;
}

std::string MercFragmentControl::print() const {
  std::string result;
  result += fmt::format("    unsigned_four_count: {}\n", unsigned_four_count);
  result += fmt::format("    lump_four_count: {}\n", lump_four_count);
  result += fmt::format("    fp_qwc: {}\n", fp_qwc);
  result += fmt::format("    mat_xfer_count: {}\n", mat_xfer_count);
  for (u8 i = 0; i < mat_xfer_count; i++) {
    result += fmt::format("    mat[{}] {} -> {}\n", i, mat_dest_data[i].matrix_number,
                          mat_dest_data[i].matrix_dest);
  }
  return result;
}

std::string MercShader::print() const {
  std::string result;
  result += fmt::format("        output_offset: {}\n", output_offset);
  result += fmt::format("        strip_tag: 0x{:x}\n", next_strip_nloop);
  return result;
}

MercShader make_shader(Ref& ref, bool expected_eop) {
  // adgif0
  MercShader shader;
  u8 adgif0_addr = deref_u8(ref, 8);
  ASSERT(adgif0_addr == (u8)GsRegisterAddress::TEX0_1);
  shader.output_offset = deref_u32(ref, 3);
  shader.tex0 = GsTex0(deref_u64(ref, 0));
  ref.byte_offset += 16;

  // adgif1
  u8 adgif1_addr = deref_u8(ref, 8);
  ASSERT(adgif1_addr == (u8)GsRegisterAddress::TEX1_1);
  shader.tex1 = GsTex1(deref_u64(ref, 0));
  u16 stash = deref_u32(ref, 3);
  shader.original_tex = deref_u32(ref, 2);
  shader.next_strip_nloop = stash & 0x7fff;
  ASSERT((!!(stash & 0x8000)) == expected_eop);  // set eop on last
  ref.byte_offset += 16;

  // adgif2
  u8 adgif2_addr = deref_u8(ref, 8);
  ASSERT(adgif2_addr == (u8)GsRegisterAddress::MIPTBP1_1);
  ref.byte_offset += 16;

  // adgif3
  u8 adgif3_addr = deref_u8(ref, 8);
  ASSERT(adgif3_addr == (u8)GsRegisterAddress::CLAMP_1);
  shader.clamp = deref_u64(ref, 0);
  ref.byte_offset += 16;

  // adgif4
  u8 adgif4_addr = deref_u8(ref, 8);
  ASSERT(adgif4_addr == (u8)GsRegisterAddress::ALPHA_1);
  shader.alpha = GsAlpha(deref_u64(ref, 0));
  ref.byte_offset += 16;
  return shader;
}

TypedRef MercFragment::from_ref(TypedRef tr,
                                const DecompilerTypeSystem& dts,
                                const MercFragmentControl& control,
                                const MercCtrlHeader& main_control) {
  // lg::print("frag::from_ref:\n{}\n", control.print());
  TypedRef byte_hdr(get_field_ref(tr, "header", dts), dts.ts.lookup_type("merc-byte-header"));
  header.from_ref(byte_hdr, dts);
  // lg::print("{}\n", header.print());

  // all these offsets are super confusing.
  // the DMA transfers require source and dest addresses/sized to have alignment of 16 bytes.
  // so the data is padded.
  // the transfers increase size by 4x due to VIF unpacking
  // But transferring this padding exactly would result in up to 63 bytes of wasted space.
  // so they cheat the destination pointers to be slightly overlapping so the next transfer
  // overlaps the padding of the previous.
  // as a result, the "in VU" offsets are different from "in main memory"

  // let's figure it out from bones.gc asm
  // u4
  // lbu s0, 0(gp) (fragment.control.unsigned-four-count)
  // daddiu v0, s0, 3
  // srl v0, v0, 2
  // dsll32 s0, v0, 4
  // daddu t3, t2, s0
  u32 my_u4_count = ((control.unsigned_four_count + 3) / 4) * 16;
  // lg::print("my u4: {} ({} qwc)\n", my_u4_count, my_u4_count / 16);
  for (u32 w = 0; w < my_u4_count / 4; w++) {
    u32 val = deref_u32(tr.ref, w);
    memcpy(unsigned_four_including_header.emplace_back().data(), &val, 4);
  }

  // l4
  // lbu s2, 1(gp)
  // daddiu s0, s2, 3
  // srl s0, s0, 2
  // dsll32 s2, s0, 4
  u32 my_l4_count = my_u4_count + ((control.lump_four_count + 3) / 4) * 16;
  // lg::print("my l4: {} ({} qwc)\n", my_l4_count, my_l4_count / 16);
  // end of lump should align with mm (main memory?) fp off. which
  // is used for accessing the fp data in main memory.
  ASSERT(my_l4_count / 16 == header.mm_quadword_fp_off);

  // row.x/y is st-vif-add from the merc-ctrl-header.
  // row.z = 0x47800000, row.w = 0x4b010000
  math::Vector<u32, 4> row(main_control.st_vif_add, main_control.st_vif_add, 0x47800000,
                           0x4b010000);
  for (u32 w = my_u4_count / 4; w < my_l4_count / 4; w++) {
    ASSERT((w * 4) < header.mm_quadword_fp_off * 16);
    u32 val = deref_u32(tr.ref, w);

    math::Vector<u8, 4> as_u8s;
    memcpy(as_u8s.data(), &val, 4);
    math::Vector<u32, 4> as_u32s = as_u8s.cast<u32>();
    as_u32s += row;
    memcpy(lump4_unpacked.emplace_back().data(), as_u32s.data(), 16);
  }

  // fp header
  Ref fp_ref = tr.ref;
  fp_ref.byte_offset += 16 * header.mm_quadword_fp_off;
  fp_header.from_ref(TypedRef(fp_ref, dts.ts.lookup_type("merc-fp-header")), dts);
  fp_ref.byte_offset += 16;

  // fp shaders
  for (int i = 0; i < fp_header.shader_cnt; i++) {
    bool expected_eop = (i == fp_header.shader_cnt - 1);
    shaders.push_back(make_shader(fp_ref, expected_eop));
  }

  tr.ref.byte_offset += (header.mm_quadword_size) * 16;

  // let's verify the matrix slots here.
  int used_matrix_slots = 0;
  for (auto x : header.mat_slot) {
    if (x) {
      used_matrix_slots++;
    } else {
      break;
    }
  }
  ASSERT(used_matrix_slots == control.mat_xfer_count);
  for (int i = 0; i < used_matrix_slots; i++) {
    ASSERT(header.mat_slot[i] == control.mat_dest_data.at(i).matrix_dest);
  }

  return tr;
}

std::string MercFragment::print() const {
  std::string result;
  result += fmt::format("      + BYTE-HEADER\n");
  result += header.print();
  result += fmt::format("      + FP-HEADER\n");
  result += fp_header.print();
  for (const auto& shader : shaders) {
    result += fmt::format("      + SHADER\n");
    result += shader.print();
  }
  return result;
}

void MercEffect::from_ref(TypedRef tr,
                          const DecompilerTypeSystem& dts,
                          const MercCtrlHeader& main_control) {
  effect_bits = read_plain_data_field<u8>(tr, "effect-bits", dts);
  frag_count = read_plain_data_field<u16>(tr, "frag-count", dts);
  blend_frag_count = read_plain_data_field<u16>(tr, "blend-frag-count", dts);
  tri_count = read_plain_data_field<u16>(tr, "tri-count", dts);
  dvert_count = read_plain_data_field<u16>(tr, "dvert-count", dts);
  auto* type = dynamic_cast<StructureType*>(dts.ts.lookup_type("merc-effect"));
  Field temp;
  if (type->lookup_field("envmap-usage", &temp)) {
    envmap_or_effect_usage = read_plain_data_field<u8>(tr, "envmap-usage", dts);
  } else {
    envmap_or_effect_usage = read_plain_data_field<u8>(tr, "effect-usage", dts);
  }

  if (type->lookup_field("texture-index", &temp)) {
    texture_index = read_plain_data_field<u8>(tr, "texture-index", dts);
  }

  // do frag-ctrls
  TypedRef fc(deref_label(get_field_ref(tr, "frag-ctrl", dts)),
              dts.ts.lookup_type("merc-fragment-control"));
  for (u32 i = 0; i < frag_count; i++) {
    fc = frag_ctrl.emplace_back().from_ref(fc, dts);
  }

  // do actual frags
  TypedRef f(deref_label(get_field_ref(tr, "frag-geo", dts)), dts.ts.lookup_type("merc-fragment"));
  for (u32 i = 0; i < frag_count; i++) {
    f = frag_geo.emplace_back().from_ref(f, dts, frag_ctrl.at(i), main_control);
  }

  // do blend ctrls/data
  if (blend_frag_count) {
    // each fragment has a blend-ctrl and a blend-data.

    TypedRef bc(deref_label(get_field_ref(tr, "blend-ctrl", dts)),
                dts.ts.lookup_type("merc-blend-ctrl"));
    Ref bd(deref_label(get_field_ref(tr, "blend-data", dts)));

    for (u32 i = 0; i < blend_frag_count; i++) {
      bc = blend_ctrl.emplace_back().from_ref(bc, dts, main_control.blend_target_count);
      const auto& ctrl = blend_ctrl.back();
      // the order of the data is [target][vtx]
      // Each target is 16 bytes aligned because it gets dma'd to the scratchpad separately.
      // Each vertex uses 6 bytes (1 byte for each of x,y,z,nx,ny,nz.
      int stride = align16(6 * ctrl.blend_vtx_count);
      // add an additional target for the "base" position.
      int data_size = stride * (1 + ctrl.nonzero_index_count);
      bd = blend_data.emplace_back().from_ref(bd, data_size);
    }
  }

  // do extra info
  auto fr = get_field_ref(tr, "extra-info", dts);
  const auto& word = fr.data->words_by_seg.at(fr.seg).at(fr.byte_offset / 4);
  if (word.kind() == LinkedWord::PTR) {
    TypedRef mei(deref_label(fr), dts.ts.lookup_type("merc-extra-info"));
    u8 shader_offset = read_plain_data_field<u8>(mei, "shader-offset", dts);
    if (shader_offset) {
      Ref r = mei.ref;
      r.byte_offset += 16 * shader_offset;
      extra_info.shader = make_shader(r, false);
    }
  }
}

std::string MercEffect::print() {
  std::string result;
  result += fmt::format("  effect_bits: {}\n", effect_bits);
  result += fmt::format("  frag_count: {}\n", frag_count);
  result += fmt::format("  blend_frag_count: {}\n", blend_frag_count);
  result += fmt::format("  tri_count: {}\n", tri_count);
  result += fmt::format("  dvert_count: {}\n", dvert_count);
  result += fmt::format("  envmap_or_effect_usage: {}\n", envmap_or_effect_usage);

  for (u32 i = 0; i < frag_count; i++) {
    result += fmt::format("  +FRAGMENT {}\n", i);
    result += fmt::format("    + CTRL\n");
    result += frag_ctrl[i].print();
    result += fmt::format("    + GEO\n");
    result += frag_geo[i].print();
  }
  return result;
}

void MercCtrl::from_ref(TypedRef tr, const DecompilerTypeSystem& dts, GameVersion version) {
  name = read_string_field(tr, "name", dts, false);
  num_joints = read_plain_data_field<s32>(tr, "num-joints", dts);
  auto merc_ctrl_header_ref =
      TypedRef(get_field_ref(tr, "header", dts), dts.ts.lookup_type("merc-ctrl-header"));
  header.from_ref(merc_ctrl_header_ref, dts, version);

  auto eff_ref = TypedRef(get_field_ref(tr, "effect", dts), dts.ts.lookup_type("merc-effect"));
  for (u32 i = 0; i < header.effect_count; i++) {
    effects.emplace_back().from_ref(eff_ref, dts, header);
    eff_ref.ref.byte_offset += 32;  //
  }
}

TypedRef MercBlendCtrl::from_ref(TypedRef tr,
                                 const DecompilerTypeSystem& dts,
                                 int blend_target_count) {
  blend_vtx_count = read_plain_data_field<u8>(tr, "blend-vtx-count", dts);
  nonzero_index_count = read_plain_data_field<u8>(tr, "nonzero-index-count", dts);
  tr.ref.byte_offset += 2;
  for (int i = 0; i < blend_target_count; i++) {
    bt_index.push_back(deref_u8(tr.ref, 0));
    tr.ref.byte_offset += 1;
  }
  return tr;
}

Ref MercBlendData::from_ref(Ref ref, int num_bytes) {
  for (int i = 0; i < num_bytes; i++) {
    u8_data.push_back(deref_u8(ref, 0));
    ref.byte_offset += 1;
  }
  return ref;
}

std::string MercCtrl::print() {
  std::string result;
  result += fmt::format("name: {}\n", name);
  result += fmt::format("num_joints: {}\n", num_joints);
  result += "+ HEADER\n";
  result += header.print();
  result += "\n";
  for (auto& eff : effects) {
    result += fmt::format("+ EFFECT\n{}\n", eff.print());
  }
  return result;
}

}  // namespace decompiler
