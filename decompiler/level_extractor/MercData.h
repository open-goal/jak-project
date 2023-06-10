#pragma once

#include <optional>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"
#include "common/versions/versions.h"

#include "decompiler/util/goal_data_reader.h"

namespace decompiler {

struct MercEyeCtrl {
  s8 eye_slot;
  // there's more...
  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts);
};

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
  std::optional<MercEyeCtrl> eye_ctrl;
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

  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts, GameVersion version);
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
  u8 blend_vtx_count;  // total number of vertices

  // if a fragment is not influenced by a target, the offsets would be zero, and these offset
  // aren't stored. The format works like this:
  // if bt_index[tgt_idx] == 0:
  //   the target doesn't influence this vertex
  // else:
  //   the bt_index[tgt_idx] group of offsets is for tgt_idx.

  // All the nonzero entries of bt_index are increasing.

  // For example:
  // 0, 0, 1, 0, 2, 3
  // indicates that this blend fragment is used in targets 2, 4, and 5.
  // group 1 is the offsets for target 2, group 2 for 4, and group 3 for 5.

  // the group 0 offsets are actually the vertex base position, and should be treated as
  // unsigned. All other offsets are signed offsets.

  u8 nonzero_index_count;  // number of nonzeros in the bt_index table

  // which groups correspond to which targets (see comment above)
  // the length of this array is always the number of blend targets for the effect.
  std::vector<u8> bt_index;
  TypedRef from_ref(TypedRef tr, const DecompilerTypeSystem& dts, int blend_target_count);
};

struct MercBlendData {
  std::vector<u8> u8_data;
  Ref from_ref(Ref ref, int num_bytes);
};

struct MercExtraInfo {
  std::optional<MercShader> shader;
};

constexpr int kTextureScrollEffectBit = 1;
constexpr int kTransEffectBit = 2;   // true in 1 and 2
constexpr int kRippleEffectBit = 4;  // true in jak 1 and jak 2

struct MercEffect {
  //((frag-geo         merc-fragment          :offset-assert 0) ;; ?
  std::vector<MercFragment> frag_geo;
  // (frag-ctrl        merc-fragment-control  :offset-assert 4)
  std::vector<MercFragmentControl> frag_ctrl;
  // (blend-data       merc-blend-data        :offset-assert 8) ??
  std::vector<MercBlendData> blend_data;
  // (blend-ctrl       merc-blend-ctrl        :offset-assert 12) ??
  std::vector<MercBlendCtrl> blend_ctrl;
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

  void from_ref(TypedRef tr, const DecompilerTypeSystem& dts, GameVersion version);
  std::string print();
};
}  // namespace decompiler
