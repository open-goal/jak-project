#pragma once

#include <optional>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"

#include "decompiler/util/goal_data_reader.h"

namespace decompiler {

/*!
 * per-ctrl information. the first qw is uploaded to vu1
 */
struct MercCtrlHeader {
  float xyz_scale;
  u32 st_magic;
  u32 st_out_a;
  u32 st_out_b;
  u32 st_vif_add;
  u16 st_int_off;
  u16 st_int_scale;
  u32 effect_count;
  u32 blend_target_count;
  u16 fragment_count;
  u16 tri_count;
  u8 matrix_count;
  u8 shader_count;
  u16 transform_vertex_count;
  u16 dvert_count;
  u16 one_mat_count;
  u16 two_mat_count;
  u16 two_mat_reuse_count;
  u16 three_mat_count;
  u16 three_mat_reuse_count;
  u8 shader_upload_count;
  u8 matrix_upload_count;
  u16 same_copy_count;
  u16 cross_copy_count;
  u16 num_verts;
  float longest_edge;
  // todo (eye-ctrl               merc-eye-ctrl    :offset-assert 64)
  u32 masks[3];
  // (dummy-bytes            uint8       48 :offset 32)
  u32 envmap_tint;
  // todo  (query                  basic            :offset 36)
  u8 needs_clip;
  u8 use_isometric;
  u8 use_attached_shader;
  u8 display_triangles;
  u16 death_vertex_skip;
  u16 death_start_vertex;
  u32 death_effect;
  u8 use_translucent;
  u8 display_this_fragment;

  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts);
  std::string print() const;
};

/*!
 * the unsigned4 data of a fragment starts with this.
 */
struct MercByteHeader {
  u8 srcdest_off;
  u8 rgba_off;
  u8 lump_off;
  u8 fp_off;
  u8 mat1_cnt;
  u8 mat2_cnt;
  u8 mat3_cnt;
  u8 samecopy_cnt;
  u8 crosscopy_cnt;
  u8 strip_len;
  u8 mm_quadword_fp_off;
  u8 mm_quadword_size;
  u8 perc_off;
  static constexpr int MAT_SLOTS = 10;
  u8 mat_slot[MAT_SLOTS];
  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts);
  std::string print() const;
};
static_assert(sizeof(MercByteHeader) == 0x17);

/*!
 * the fp data of a fragment starts with this.
 */
struct MercFpHeader {
  float x_add;
  float y_add;
  float z_add;
  u8 shader_cnt;
  u8 kick_info_offset;
  u8 kick_info_step;
  u8 hword_cnt;
  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts);
  std::string print() const;
};

struct MercShader {
  GsTex0 tex0;
  GsTex1 tex1;
  // skip mip
  u64 clamp;
  GsAlpha alpha;
  u16 output_offset;
  u16 next_strip_nloop;
  u32 original_tex;
  std::string print() const;
};

/*!
 * info about a matrix to upload.
 * it maps a per-model matrix (matrix_number) to a slot in vu1 memory (matrix_dest)
 */
struct MercMatDest {
  u8 matrix_number;
  u8 matrix_dest;
};

/*!
 * per-fragment info that doesn't go to the VU
 */
struct MercFragmentControl {
  // memory layout
  u8 unsigned_four_count;
  u8 lump_four_count;
  u8 fp_qwc;

  // matrix upload info
  u8 mat_xfer_count;
  std::vector<MercMatDest> mat_dest_data;  // inline, dynamic

  TypedRef from_ref(TypedRef tr, const DecompilerTypeSystem& dts);
  std::string print() const;
};

/*!
 * the per-vu1 upload/call data for merc.
 */
struct MercFragment {
  // part1: unsigned4. it is expanded to u32 by VIF.
  MercByteHeader header;
  std::vector<math::Vector<u8, 4>>
      unsigned_four_including_header;  // repeats the data above. always 0 in vu.

  // part2: lump4. it is converted to floats by VIF.
  std::vector<math::Vector<float, 4>> lump4_unpacked;  // at lump_off qw in VU.

  // part3: fp. it contains the fp header, shaders, and ??
  MercFpHeader fp_header;
  std::vector<MercShader> shaders;
  std::vector<u8> extra_fp_data;  // ??

  TypedRef from_ref(TypedRef tr,
                    const DecompilerTypeSystem& dts,
                    const MercFragmentControl& control,
                    const MercCtrlHeader& main_control);
  std::string print() const;
};

struct MercBlendCtrl {
  u8 blend_vtx_count;
  u8 nonzero_index_count;
  std::vector<u8> bt_index;
  TypedRef from_ref(TypedRef tr, const DecompilerTypeSystem& dts, int blend_target_count);
};

struct MercExtraInfo {
  std::optional<MercShader> shader;
};

constexpr int kRippleEffectBit = 4;  // true in jak 1 and jak 2

struct MercEffect {
  //((frag-geo         merc-fragment          :offset-assert 0) ;; ?
  std::vector<MercFragment> frag_geo;
  // (frag-ctrl        merc-fragment-control  :offset-assert 4)
  std::vector<MercFragmentControl> frag_ctrl;
  // (blend-data       merc-blend-data        :offset-assert 8) ??
  std::vector<MercBlendCtrl> blend_ctrl;
  // (blend-ctrl       merc-blend-ctrl        :offset-assert 12) ??
  // (dummy0           uint8                  :offset-assert 16) ??
  u8 effect_bits;
  u16 frag_count;
  u16 blend_frag_count;
  u16 tri_count;
  u16 dvert_count;
  // (dummy1           uint8                  :offset-assert 26) ??
  u8 envmap_or_effect_usage;
  // (extra-info       merc-extra-info        :offset-assert 28) ??
  MercExtraInfo extra_info;

  u8 texture_index = -1;  // jak 2 only

  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts, const MercCtrlHeader& main_control);
  std::string print();
};

struct MercCtrl {
  std::string name;
  s32 num_joints;
  MercCtrlHeader header;
  std::vector<MercEffect> effects;

  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts);
  void debug_print_blerc();
  std::string print();
};
}  // namespace decompiler