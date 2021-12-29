#include <array>

#include "extract_tie.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

#include "common/util/FileUtil.h"

namespace decompiler {

/*!
 * Get the index of the first draw node in an array. Works for node or tfrag.
 */
u16 get_first_idx(const level_tools::DrawableInlineArray* array) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);
  if (as_tie_instances) {
    return as_tie_instances->instances.at(0).id;
  } else if (as_nodes) {
    return as_nodes->draw_nodes.at(0).id;
  } else {
    assert(false);
  }
}

/*!
 * Verify node indices follow the patterns we expect. Takes start as the expected first,
 * writes the end.
 */
bool verify_node_indices_from_array(const level_tools::DrawableInlineArray* array,
                                    u16 start,
                                    u16* end) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);

  if (as_tie_instances) {
    for (auto& elt : as_tie_instances->instances) {
      if (elt.id != start) {
        fmt::print("bad inst: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else if (as_nodes) {
    for (auto& elt : as_nodes->draw_nodes) {
      if (elt.id != start) {
        fmt::print("bad node: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else {
    fmt::print("bad node array type: {}\n", array->my_type());
    return false;
  }
}

/*!
 * Verify all node indices in a tree.
 */
bool verify_node_indices(const level_tools::DrawableTreeInstanceTie* tree) {
  u16 start = get_first_idx(tree->arrays.at(0).get());
  for (auto& array : tree->arrays) {
    if (!verify_node_indices_from_array(array.get(), start, &start)) {
      return false;
    }
    start = (start + 31) & ~(31);
  }
  return true;
}

/*!
 * Extract the visibility tree.
 * This does not insert nodes for the bottom level.
 */
void extract_vis_data(const level_tools::DrawableTreeInstanceTie* tree,
                      u16 first_child,
                      tfrag3::TieTree& out) {
  out.bvh.first_leaf_node = first_child;
  out.bvh.last_leaf_node = first_child;

  if (tree->arrays.size() == 0) {
    // shouldn't hit this?
  } else if (tree->arrays.size() == 1) {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(tree->arrays.at(0).get());
    assert(array);
    out.bvh.first_root = array->instances.at(0).id;
    out.bvh.num_roots = array->instances.size();
    out.bvh.only_children = true;
  } else {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(0).get());
    assert(array);
    out.bvh.first_root = array->draw_nodes.at(0).id;
    out.bvh.num_roots = array->draw_nodes.size();
    out.bvh.only_children = false;
  }

  out.bvh.vis_nodes.resize(first_child - out.bvh.first_root);

  // may run 0 times, if there are only children.
  for (int i = 0; i < ((int)tree->arrays.size()) - 1; i++) {
    bool expecting_leaves = i == ((int)tree->arrays.size()) - 2;

    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(i).get());
    assert(array);
    u16 idx = first_child;
    for (auto& elt : array->draw_nodes) {
      auto& vis = out.bvh.vis_nodes.at(elt.id - out.bvh.first_root);
      assert(vis.num_kids == 0xff);
      for (int j = 0; j < 4; j++) {
        vis.bsphere[j] = elt.bsphere.data[j];
      }
      vis.num_kids = elt.child_count;
      vis.flags = elt.flags;
      assert(vis.flags == expecting_leaves ? 0 : 1);
      assert(vis.num_kids > 0);
      assert(vis.num_kids <= 8);
      assert(elt.children.size() == vis.num_kids);
      if (expecting_leaves) {
        for (int leaf = 0; leaf < (int)vis.num_kids; leaf++) {
          auto l = dynamic_cast<level_tools::InstanceTie*>(elt.children.at(leaf).get());
          assert(l);

          assert(idx == l->id);

          assert(l->id >= out.bvh.first_leaf_node);
          if (leaf == 0) {
            vis.child_id = l->id;
          }
          out.bvh.last_leaf_node = std::max((u16)l->id, out.bvh.last_leaf_node);
          idx++;
        }

      } else {
        u16 arr_idx = 0;
        for (int child = 0; child < (int)vis.num_kids; child++) {
          auto l = dynamic_cast<level_tools::DrawNode*>(elt.children.at(child).get());
          assert(l);
          if (child == 0) {
            arr_idx = l->id;
          } else {
            assert(arr_idx < l->id);
            arr_idx = l->id;
          }
          if (child == 0) {
            vis.child_id = l->id;
          }

          assert(l->id < out.bvh.first_leaf_node);
        }
      }
    }
  }
}

struct TieInstanceFragInfo {
  // the color index table uploaded to VU.
  // this contains indices into the shared palette.
  std::vector<u8> color_indices;

  u16 color_index_offset_in_big_palette = -1;

  math::Vector<u32, 4> lq_colors_ui(u32 qw) const {
    // note: this includes the unpack
    assert(qw >= 204);
    qw -= 204;
    qw *= 4;
    assert(qw + 4 <= color_indices.size());
    math::Vector<u32, 4> result;
    for (int i = 0; i < 4; i++) {
      result[i] = color_indices.at(qw + i);
    }
    return result;
  }
};

struct TieInstanceInfo {
  // The index of the prototype (the geometry) that is used by this instance
  // note: we're going to trust that this lines up with bucket.
  // if this assumption is wrong, we'll be drawing with the wrong model and it will be super
  // obvious.
  u16 prototype_idx = 0;

  // our bsphere's index in the BVH tree
  u16 vis_id = 0;

  // not totally sure if we'll use this (currently unused in tfrag, but probably worth if we
  // actually cull using the tree)
  math::Vector4f bsphere;

  std::array<math::Vector4f, 4> mat;

  u16 wind_index = 0;
  float unknown_wind_related_value = 0.f;  // w of the first mat vec.

  std::vector<TieInstanceFragInfo> frags;  // per-instance per-fragment info
};

struct AdgifInfo {
  u32 first_w;
  u32 second_w;
  u32 third_w;
  u32 combo_tex;
  u64 alpha_val;
  u64 clamp_val;
};

struct StrGifInfo {
  u16 address;
  u16 nloop;
  u16 mode;  // not yet fully understood, but can allow the use of other templates.
  bool eop;
};

struct TieProtoVertex {
  math::Vector<float, 3> pos;
  math::Vector<float, 3> tex;

  // NOTE: this is a double lookup.
  // first you look up the index in the _instance_ color table
  // then you look up the color in the _proto_'s interpolated color palette.
  u32 color_index_index;
};

struct TieStrip {
  AdgifInfo adgif;
  std::vector<TieProtoVertex> verts;
};

struct TieFrag {
  bool has_magic_tex0_bit = false;
  std::vector<AdgifInfo> adgifs;

  std::vector<u8> other_gif_data;
  std::vector<u8> points_data;
  std::vector<u32> point_sizes;

  u32 expected_dverts = 0;

  std::vector<TieStrip> strips;

  // this contains vertices, key is the start of the actual xyzf/st/rgbaq data for it.
  std::unordered_map<u32, TieProtoVertex> vertex_by_dest_addr;

  math::Vector<float, 4> lq_points(u32 qw) const {
    assert(qw >= 50);
    qw -= 50;
    assert((qw * 16) + 16 <= points_data.size());
    math::Vector<float, 4> result;
    memcpy(result.data(), points_data.data() + (qw * 16), 16);
    return result;
  }

  math::Vector<float, 4> lq_points_allow_past_end(u32 qw) const {
    assert(qw >= 50);
    qw -= 50;
    if ((qw * 16) + 16 <= points_data.size()) {
      math::Vector<float, 4> result;
      memcpy(result.data(), points_data.data() + (qw * 16), 16);
      return result;
    } else {
      return math::Vector4f(-1, -1, -1, -1);
    }
  }

  void sq_points(u32 qw, const math::Vector4f& data) {
    assert(qw >= 50);
    qw -= 50;
    assert((qw * 16) + 16 <= points_data.size());
    memcpy(points_data.data() + (qw * 16), data.data(), 16);
  }

  u16 ilw_other_gif(u32 qw, u32 offset) const {
    // unpacked with v8.
    int qwi = qw;
    qwi -= (adgifs.size() * 5);
    assert(qwi >= 0);
    return other_gif_data.at(qwi * 4 + offset);
  }

  struct ProgramInfo {
    std::vector<u16> adgif_offset_in_gif_buf_qw;
    std::vector<StrGifInfo> str_gifs;
    u16 skip_bp2 = 0;
    u16 skip_ips = 0;
    u16 tgt_bp1_ptr = 0;
    u16 tgt_bp2_ptr = 0;
    u16 tgt_ip1_ptr = 0;
    u16 tgt_ip2_ptr = 0;
    u16 kick_addr = 0;
    u16 clr_ptr = 0;
    u16 point_ptr = 0;
    u16 misc_x = 0;  // at 971's x.
    math::Vector4f gifbufs;
    math::Vector4f extra;
  } prog_info;
};

struct TieProtoInfo {
  std::string name;
  std::vector<TieInstanceInfo> instances;
  bool uses_generic = false;
  float stiffness = 0;
  u32 generic_flag;
  std::vector<tfrag3::TimeOfDayColor> time_of_day_colors;
  std::vector<TieFrag> frags;
};

std::array<math::Vector4f, 4> extract_tie_matrix(const u16* data) {
  std::array<math::Vector4f, 4> result;
  for (int i = 0; i < 4; i++) {
    s32 x = data[12 + i];
    x <<= 16;
    x >>= 10;
    result[3][i] = x;
  }

  for (int vec = 0; vec < 3; vec++) {
    for (int i = 0; i < 4; i++) {
      s32 x = data[vec * 4 + i];
      x <<= 16;
      x >>= 16;
      result[vec][i] = (float)x / 4096.f;
    }
  }

  return result;
}

constexpr int GEOM_IDX = 1;  // todo 0 or 1??

std::vector<TieProtoInfo> collect_instance_info(
    const level_tools::DrawableInlineArrayInstanceTie* instances,
    const std::vector<level_tools::PrototypeBucketTie>* protos) {
  std::vector<TieProtoInfo> result;
  for (auto& instance : instances->instances) {
    TieInstanceInfo info;
    info.prototype_idx = instance.bucket_index;
    info.vis_id = instance.id;
    for (int i = 0; i < 4; i++) {
      info.bsphere[i] = instance.bsphere.data[i];
    }
    info.mat = extract_tie_matrix(instance.origin.data);
    info.mat[3][0] += info.bsphere[0];
    info.mat[3][1] += info.bsphere[1];
    info.mat[3][2] += info.bsphere[2];
    info.wind_index = instance.wind_index;
    // there's a value stashed here that we can get rid of
    // it is related to wind.
    info.unknown_wind_related_value = info.mat[0][3];
    info.mat[0][3] = 0.f;

    // each fragment has its own color data (3 dmatags)

    // the number of colors (qwc) is stored in the prototype, in the color-index-qwc array of bytes.
    // at an offset of index-start[geom] + frag_idx.

    // the actual data is located at the instance's color-indices + (proto.base-qw[geom] * 16)

    // and this is only the indices.... there's yet another lookup on the VU
    auto& proto = protos->at(info.prototype_idx);
    u32 offset_bytes = proto.base_qw[GEOM_IDX] * 16;
    for (int frag_idx = 0; frag_idx < proto.frag_count[GEOM_IDX]; frag_idx++) {
      TieInstanceFragInfo frag_info;
      u32 num_color_qwc = proto.color_index_qwc.at(proto.index_start[GEOM_IDX] + frag_idx);
      for (u32 i = 0; i < num_color_qwc * 4; i++) {
        for (u32 j = 0; j < 4; j++) {
          frag_info.color_indices.push_back(
              instance.color_indices.data->words_by_seg.at(instance.color_indices.seg)
                  .at(((offset_bytes + instance.color_indices.byte_offset) / 4) + i)
                  .get_byte(j));
        }
      }
      info.frags.push_back(std::move(frag_info));
      assert(info.frags.back().color_indices.size() > 0);
      offset_bytes += num_color_qwc * 16;
    }

    if (result.size() <= info.prototype_idx) {
      result.resize(info.prototype_idx + 1);
    }
    result[info.prototype_idx].instances.push_back(info);
  }

  return result;
}

u32 remap_texture(u32 original, const std::vector<level_tools::TextureRemap>& map) {
  auto masked = original & 0xffffff00;
  for (auto& t : map) {
    if (t.original_texid == masked) {
      fmt::print("OKAY! remapped!\n");
      assert(false);
      return t.new_texid | 20;
    }
  }
  return original;
}

void update_proto_info(std::vector<TieProtoInfo>* out,
                       const std::vector<level_tools::TextureRemap>& map,
                       const TextureDB& tdb,
                       const std::vector<level_tools::PrototypeBucketTie>& protos) {
  out->resize(std::max(out->size(), protos.size()));
  for (size_t i = 0; i < protos.size(); i++) {
    const auto& proto = protos[i];
    auto& info = out->at(i);
    assert(proto.flags == 0 || proto.flags == 2);
    info.uses_generic = (proto.flags == 2);
    info.name = proto.name;
    info.stiffness = proto.stiffness;
    info.generic_flag = proto.flags & 2;

    info.time_of_day_colors.resize(proto.time_of_day.height);
    for (int k = 0; k < (int)proto.time_of_day.height; k++) {
      for (int j = 0; j < 8; j++) {
        memcpy(info.time_of_day_colors[k].rgba[j].data(), &proto.time_of_day.colors[k * 8 + j], 4);
      }
    }

    for (int frag_idx = 0; frag_idx < proto.frag_count[GEOM_IDX]; frag_idx++) {
      TieFrag frag_info;
      for (int tex_idx = 0;
           tex_idx < proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).tex_count / 5; tex_idx++) {
        AdgifInfo adgif;
        auto& gif_data = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].gif_data;
        u8 ra_tex0 = gif_data.at(16 * (tex_idx * 5 + 0) + 8);
        u64 ra_tex0_val;
        memcpy(&ra_tex0_val, &gif_data.at(16 * (tex_idx * 5 + 0)), 8);
        assert(ra_tex0 == (u8)GsRegisterAddress::TEX0_1);
        assert(ra_tex0_val == 0 || ra_tex0_val == 0x800000000);  // note: decal
        frag_info.has_magic_tex0_bit = ra_tex0_val == 0x800000000;
        memcpy(&adgif.first_w, &gif_data.at(16 * (tex_idx * 5 + 0) + 12), 4);

        u8 ra_tex1 = gif_data.at(16 * (tex_idx * 5 + 1) + 8);
        u64 ra_tex1_val;
        memcpy(&ra_tex1_val, &gif_data.at(16 * (tex_idx * 5 + 1)), 8);
        assert(ra_tex1 == (u8)GsRegisterAddress::TEX1_1);
        assert(ra_tex1_val == 0x120);  // some flag
        u32 original_tex;
        memcpy(&original_tex, &gif_data.at(16 * (tex_idx * 5 + 1) + 8), 4);
        u32 new_tex = remap_texture(original_tex, map);
        if (original_tex != new_tex) {
          fmt::print("map from 0x{:x} to 0x{:x}\n", original_tex, new_tex);
        }
        u32 tpage = new_tex >> 20;
        u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
        u32 tex_combo = (((u32)tpage) << 16) | tidx;
        auto tex = tdb.textures.find(tex_combo);
        assert(tex != tdb.textures.end());
        adgif.combo_tex = tex_combo;
        memcpy(&adgif.second_w, &gif_data.at(16 * (tex_idx * 5 + 1) + 12), 4);

        if (ra_tex0_val == 0x800000000) {
          fmt::print("texture {} in {} has weird tex setting\n", tex->second.name, proto.name);
        }

        u8 ra_mip = gif_data.at(16 * (tex_idx * 5 + 2) + 8);
        assert(ra_mip == (u8)GsRegisterAddress::MIPTBP1_1);
        memcpy(&adgif.third_w, &gif_data.at(16 * (tex_idx * 5 + 2) + 12), 4);

        // who cares about the value

        u8 ra_clamp = gif_data.at(16 * (tex_idx * 5 + 3) + 8);
        assert(ra_clamp == (u8)GsRegisterAddress::CLAMP_1);
        u64 clamp;
        memcpy(&clamp, &gif_data.at(16 * (tex_idx * 5 + 3)), 8);
        adgif.clamp_val = clamp;

        u8 ra_alpha = gif_data.at(16 * (tex_idx * 5 + 4) + 8);
        assert(ra_alpha == (u8)GsRegisterAddress::ALPHA_1);
        u64 alpha;
        memcpy(&alpha, &gif_data.at(16 * (tex_idx * 5 + 4)), 8);
        adgif.alpha_val = alpha;
        frag_info.adgifs.push_back(adgif);
      }
      frag_info.expected_dverts = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].num_dverts;
      int tex_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).tex_count;
      int other_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).gif_count;
      frag_info.other_gif_data.resize(16 * other_qwc);
      memcpy(frag_info.other_gif_data.data(),
             proto.geometry[GEOM_IDX].tie_fragments[frag_idx].gif_data.data() + (16 * tex_qwc),
             16 * other_qwc);

      const auto& pr = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].point_ref;
      int in_qw = pr.size() / 16;
      int out_qw = in_qw * 2;
      frag_info.points_data.resize(out_qw * 16);
      {
        const s16* in_ptr = (const s16*)pr.data();
        s32* out_ptr = (s32*)frag_info.points_data.data();
        for (int ii = 0; ii < out_qw * 4; ii++) {
          out_ptr[ii] = in_ptr[ii];
        }
      }

      // just for debug
      for (int g = 0; g < 4; g++) {
        frag_info.point_sizes.push_back(proto.geometry[g].tie_fragments[frag_idx].point_ref.size());
      }

      info.frags.push_back(std::move(frag_info));
    }
  }
}

// upload-palette-0: just a flusha
//   no data

// upload-palette-1: stmod 1 (add row), unpack v4 (32 qw in, 128 qw out), imm = usn, 0x346
//   colors (after time of day interpolation)
//   NOTE: adds row

// upload-model-0: stmod = 0, unpack-v4-32 imm = 0 (upload to 0?) (usn doesn't matter for v4-32)
//   adgifs, size of adgifs.

// upload-model-1:
// mscal 4
// unpack-v4-8 imm = right after adgifs, usn.
// extra gif stuff

// upload-model-2:
// unpack-v4-16 imm = 32, signed.
// points

// upload-model-3
// mscal 6
// call the models!

// upload-color-0
// 6 qw of matrix plus flag stuff
// to 198 (relative to TOP)

// upload-color-1
// to 204 unsigned (relative to TOP)

// upload-color-2/ret
// mscal 0

// MEMORY MAP of TIE
// 0 gif tags
// extra gifs
// 32 model
// 198 instance matrix
// 204 instance colors
// 242 instance matrix again
// 248 instance colors again
// 286 gifbuf
// 470 gifbuf again
// 654 ??
// 838 color palette
// 966 tie-consts
//   966 adgif
//   967 strgid
//   968 extra
//   969 gifbufs
//   970 clrbufs
//   971 misc
//   972 atestgif
//   973 atest-tra
//   974 atest-def

math::Vector4f itof0(const math::Vector4f& vec) {
  math::Vector4f result;
  for (int i = 0; i < 4; i++) {
    s32 val;
    memcpy(&val, vec.data() + i, 4);
    result[i] = val;
  }
  return result;
}

math::Vector4f itof12xyz_0w(const math::Vector4f& vec) {
  math::Vector4f result;
  for (int i = 0; i < 4; i++) {
    s32 val;
    memcpy(&val, vec.data() + i, 4);
    result[i] = val;
  }
  result.x() /= 4096.f;
  result.y() /= 4096.f;
  result.z() /= 4096.f;
  return result;
}

math::Vector4f muli64_xyz(const math::Vector4f& vec) {
  math::Vector4f result = vec;
  result.x() *= 64.f;
  result.y() *= 64.f;
  result.z() *= 64.f;
  return result;
}

void emulate_tie_prototype_program(std::vector<TieProtoInfo>& protos) {
  using math::Vector4f;

  // our convention here is to use the lower buffer for everything double buffered.

  // because double buffering was too easy, the xgkick output buffer is triple buffered!
  // the normal double buffering approach would not allow one prototype to be in setup
  // while the second is being kicked. Each prototype gets two gif bufs and the third gif buf
  // is used to xgkick whatever is left over from the previous prototype.
  float gifbuf_start = 8388894.f;   // 0x4b00011e. The 0x11e in the mantissa is 286.
  float gifbuf_middle = 8389078.f;  // 0x4b0001d6. The 0x1d6 in the mantissa is 470.
  float gifbuf_end = 8389262.f;     // 0x4b00028e. The 0x28e in the mantissa is 654.

  Vector4f vf_gifbufs(gifbuf_end, gifbuf_middle, gifbuf_end, gifbuf_middle);

  float gifbuf_sum = gifbuf_start + gifbuf_middle + gifbuf_end;
  Vector4f vf_extra(gifbuf_sum, 0, gifbuf_sum, 0);

  // u16 misc_x = 0;
  // u16 misc_y = 1;

  // First, we will emulate the program that runs after model uploads. (L1, imm = 6)
  // it runs once per fragment
  for (auto& proto : protos) {
    // loop over fragments in this proto
    for (u32 frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
      auto& frag = proto.frags[frag_idx];

      // this basically sets up some templates in memory.
      // we're going to track the memory addresses of where certain tags are placed.

      // there are 6qw gif packets that do an adgif-shader upload.
      // this vector will store the location of these adgif shaders, relative to the start
      // of the gif output buffer being used.

      // this starts off pointing to 0, which is the adgif shaders for this fragment (input data)
      u16 vi_point_ptr = 0;

      // this fiddles with the triple buffering magic for gif bufs
      // todo: figure out the trick and just use a fixed addr.
      vf_gifbufs.z() = vf_extra.z() - vf_gifbufs.x();
      vf_gifbufs.x() = vf_extra.x() - vf_gifbufs.x();

      //    L1:
      //    lq.xyz vf01, 966(vi00)                |  nop    vf01 = adgif header.
      //    ilwr.w vi04, vi_point_ptr   |  nop
      // some integers are hidden in the upper 32-bits of the adgif data.
      // the first one has the offset in the gif buffer.
      // we expect this to be 0 for the first one - we should start with adgif shaders always.
      u16 vi04 = frag.adgifs.at(0).first_w;
      assert(vi04 == 0);

      //    ilw.w vi_ind, 1(vi_point_ptr)         |  nop
      // the next hidden integer is the number of adgif shaders used in this fragment.
      // we already know this, so check it.
      u16 vi_ind = frag.adgifs.at(0).second_w;
      assert(vi_ind == frag.adgifs.size());

      //    mtir vi06, vf_gifbufs.y               |  nop
      // vi06 will be one of our gifbufs we can use.
      u16 vi06;
      memcpy(&vi06, &vf_gifbufs.y(), sizeof(u16));
      // fmt::print("vi06: {}\n", vi06);
      assert(vi06 == 470 || vi06 == 286 || vi06 == 654);  // should be one of the three gifbufs.

      //    lqi.xyzw vf02, vi_point_ptr        |  suby.xz vf_gifbufs, vf_gifbufs, vf_gifbufs
      //    lqi.xyzw vf03, vi_point_ptr        |  nop
      //    lqi.xyzw vf04, vi_point_ptr        |  nop
      //    lqi.xyzw vf05, vi_point_ptr        |  nop
      //    mtir vi05, vf_gifbufs.x            |  nop
      //    lqi.xyzw vf06, vi_point_ptr        |  subw.w vf01, vf01, vf01

      // loads the adgif data into vf02 -> vf06
      // the subw.w is to clear out the secret integer (I think the gs ignores this anyway)
      vf_gifbufs.x() -= vf_gifbufs.y();
      vf_gifbufs.z() -= vf_gifbufs.y();
      // and vi05 is our other buffer.
      u16 vi05;
      memcpy(&vi05, &vf_gifbufs.x(), sizeof(u16));
      // fmt::print("vi05: {}\n", vi05);
      // check that we understand the buffer rotation.
      if (vi06 == 470) {
        assert(vi05 == 286);
      } else if (vi06 == 286) {
        assert(vi05 == 654);
      } else {
        assert(vi05 == 470);
      }
      vi_point_ptr += 5;

      // this loop copies the adgifs to the gif buf at the appropriate address.
      // Note: the final iteration through the loop does a load that's past the end of the
      // adgif array, and vf02 is the first qw of the "extra gif data"
      u32 adgif_load_idx = 1;
    adgif_setup_loop_top:
      //    L2:
      //    iadd vi03, vi04, vi05                |  nop
      // vi04 is the adgif offset, vi05 is the buffer.
      u16 vi03 = vi04 + vi05;

      //    iadd vi04, vi04, vi06      |  nop
      // set vi04 to the offset for the adgif in the second buffer.
      vi04 += vi06;

      //    iaddi vi_ind, vi_ind, -0x1     |  nop
      vi_ind--;  // decrement remaining adgifs

      // store adgifs in one buffer.
      frag.prog_info.adgif_offset_in_gif_buf_qw.push_back(vi03 - vi05);
      // fmt::print("adgifs at offset {}\n", frag.prog_info.adgif_offset_in_gif_buf_qw.back());
      //    sqi.xyzw vf01, vi03        |  nop
      //    sqi.xyzw vf02, vi03        |  nop
      //    sqi.xyzw vf03, vi03        |  nop
      //    sqi.xyzw vf04, vi03        |  nop
      //    sqi.xyzw vf05, vi03        |  nop
      //    sqi.xyzw vf06, vi03        |  nop
      vi03 += 5;

      // and the other buffer
      //    sqi.xyzw vf01, vi04        |  nop
      //    sqi.xyzw vf02, vi04        |  nop
      //    sqi.xyzw vf03, vi04        |  nop
      //    sqi.xyzw vf04, vi04        |  nop
      //    sqi.xyzw vf05, vi04        |  nop
      //    sqi.xyzw vf06, vi04        |  nop
      vi04 += 5;

      //    ilwr.w vi04, vi_point_ptr          |  nop
      // get the offset of the next adgif
      // vi04 = frag.ilw_points(vi_point_ptr, 3);

      //    lqi.xyzw vf02, vi_point_ptr        |  nop
      //    lqi.xyzw vf03, vi_point_ptr        |  nop
      //    lqi.xyzw vf04, vi_point_ptr        |  nop
      //    lqi.xyzw vf05, vi_point_ptr        |  nop
      vi_point_ptr += 5;

      //    ibgtz vi_ind, L2             |  nop
      if (((s16)vi_ind) > 0) {
        // moved down
        vi04 = frag.adgifs.at(adgif_load_idx++).first_w;
        goto adgif_setup_loop_top;
      }
      //    lqi.xyzw vf06, vi_point_ptr        |  nop (adgif load)

      // Extra gif stuff
      // this part builds the headers for the actual drawing packets.
      // again, we do it in two parts. The extra gif data gives us offsets,
      // The extra gif stuff is unpacked immediately after adgifs. Unpacked with v8 4.
      // the above adgif loop will run off the end and vf02 will have the first byte in it's w.
      assert(frag.other_gif_data.size() > 1);
      //    mtir vi_ind, vf02.w          |  nop
      // vi_ind will contain the number of drawing packets for this fragment.
      vi_ind = frag.other_gif_data.at(3);
      u16 vf02_x = frag.other_gif_data.at(0);
      u16 vf02_y = frag.other_gif_data.at(1);
      // u16 vf02_z = frag.other_gif_data.at(2);
      u16 vf03_x = frag.other_gif_data.at(4);
      u16 vf03_y = frag.other_gif_data.at(5);
      u16 vf03_z = frag.other_gif_data.at(6);
      u16 vf03_w = frag.other_gif_data.at(7);
      u16 vf04_x = frag.other_gif_data.at(8);
      u16 vf04_y = frag.other_gif_data.at(9);
      u16 vf04_z = frag.other_gif_data.at(10);
      // u16 vf04_w = frag.other_gif_data.at(11);
      assert(vi_ind >= frag.adgifs.size());  // at least 1 draw per shader.
      assert(vi_ind < 1000);                 // check for insane value.
      // fmt::print("got: {}, other size: {}\n", vi_ind, frag.other_gif_data.size());

      //    iaddi vi_point_ptr, vi_point_ptr, -0x2     |  subw.w vf07, vf07, vf07
      vi_point_ptr -= 2;
      // vf07.w = 0

      // setup for tag building loop.

      //    ilwr.x vi07, vi_point_ptr          |  nop
      u16 vi07 = frag.ilw_other_gif(vi_point_ptr, 0);
      // vi07 is the nloop/eop.

      //    ilwr.y vi08, vi_point_ptr          |  nop
      u16 vi08 = frag.ilw_other_gif(vi_point_ptr, 1);
      // this can toggle to a different mode but I don't understand it yet.
      assert(vi08 == 0);

      //    ilwr.z vi04, vi_point_ptr          |  nop
      vi04 = frag.ilw_other_gif(vi_point_ptr, 2);
      // offset

      // fmt::print("[{}] 7: {} 8: {} 4: {}, for {}\n", vi_point_ptr, vi07, vi08, vi04, vi_ind - 1);

      //    iaddi vi_ind, vi_ind, -0x1     |  nop
      vi_ind--;

      //    iaddi vi_point_ptr, vi_point_ptr, 0x1      |  nop
      vi_point_ptr++;

      //    ibeq vi00, vi_ind, L4        |  nop
      //    lq.xyz vf07, 967(vi08)     |  nop
      u16 next_mode = vi08;

      // todo: can we rely on a strgif from a previous fragment?
      while (vi_ind) {
        StrGifInfo info;
        //    L3:
        //    iadd vi03, vi04, vi05      |  nop
        vi03 = vi04 + vi05;  // addr in one buf
        //    iadd vi04, vi04, vi06      |  nop
        vi04 = vi04 + vi06;  // addr in other buf
        //    iaddi vi_ind, vi_ind, -0x1     |  nop
        vi_ind--;  // dec remaining tag
        //    sq.xyzw vf07, 0(vi03)      |  nop
        info.address = vi03 - vi05;  // store the template. but this doesn't have size or anything.
        // fmt::print("strgif at {}, {}\n", vi03, vi04);

        //    iswr.x vi07, vi03          |  nop
        info.nloop = vi07 & 0x7fff;
        info.eop = vi07 & 0x8000;
        assert(!info.eop);  // seems like we handle this manually after the loop
        info.mode = next_mode;

        //    sq.xyzw vf07, 0(vi04)      |  nop
        //    iswr.x vi07, vi04          |  nop
        // and the same for the other tag in the other buffer

        //    ilwr.x vi07, vi_point_ptr          |  nop
        vi07 = frag.ilw_other_gif(vi_point_ptr, 0);

        //    ilwr.y vi08, vi_point_ptr          |  nop
        vi08 = frag.ilw_other_gif(vi_point_ptr, 1);

        //    ilwr.z vi04, vi_point_ptr          |  nop
        vi04 = frag.ilw_other_gif(vi_point_ptr, 2);

        //    iaddi vi_point_ptr, vi_point_ptr, 0x1      |  nop
        vi_point_ptr++;

        //    ibne vi00, vi_ind, L3        |  nop
        //    lq.xyz vf07, 967(vi08)     |  nop
        next_mode = vi08;
        // fmt::print("[{}] 7: {} 8: {} 4: {}, for {}\n", vi_point_ptr, vi07, vi08, vi04, vi_ind);
        frag.prog_info.str_gifs.push_back(info);
      }

      // and now, the final tag, which ends the drawing packet!
      //    L4:
      //    iaddiu vi07, vi07, 0x4000  |  nop
      vi07 += 0x8000;
      //    iaddiu vi07, vi07, 0x4000  |  nop
      StrGifInfo info;
      info.eop = true;  // the 0x8000 sets the eop bit.

      // compute addresses
      //    iadd vi03, vi04, vi05      |  nop
      vi03 = vi04 + vi05;
      //    iadd vi04, vi04, vi06      |  nop
      vi04 += vi06;

      // store and set nloop/eop
      //    sq.xyzw vf07, 0(vi03)      |  nop
      info.address = vi03 - vi05;
      //    iswr.x vi07, vi03          |  nop
      info.nloop = vi07 & 0x7fff;
      //    sq.xyzw vf07, 0(vi04)      |  nop
      //    iswr.x vi07, vi04          |  nop
      frag.prog_info.str_gifs.push_back(info);

      //    mtir vi06, vf04.x          |  nop
      vi06 = vf04_x;

      //    lq.xyzw vf05, 50(vi00)     |  nop
      auto vf05 = frag.lq_points(50);
      //    lq.xyzw vf15, 51(vi00)     |  nop
      auto vf15 = frag.lq_points(51);
      //    iaddiu vi05, vi00, 0x34    |  nop
      vi05 = 0x34;  // points to after the two qw's we just loaded
      //    nop                        |  nop
      //    iaddiu vi06, vi06, 0x32    |  itof0.xyzw vf05, vf05
      vi06 += 0x32;
      vf05 = itof0(vf05);

      //    lqi.xyzw vf06, vi05        |  itof12.xyz vf15, vf15
      auto vf06 = frag.lq_points(vi05);
      vi05++;
      vf15 = itof12xyz_0w(vf15);

      //    lqi.xyzw vf16, vi05        |  itof0.w vf15, vf15
      auto vf16 = frag.lq_points(vi05);
      vi05++;
      // itof0 already done by previous

      //    64.0                       |  nop :i
      //    ibeq vi06, vi05, L6        |  muli.xyz vf05, vf05, I
      vf05 = muli64_xyz(vf05);
      //    mtir vi07, vf04.y          |  itof0.xyzw vf06, vf06
      vi07 = vf04_y;
      // fmt::print("bonus points: {}\n", vi07);
      vf06 = itof0(vf06);

      //    L5:
      Vector4f vf07;
    top_of_points_loop:
      // fmt::print("{}/{}\n", vi05, vi06);
      //    lqi.xyzw vf07, vi05        |  itof12.xyz vf16, vf16
      vf07 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf16 = itof12xyz_0w(vf16);

      //    lqi.xyzw vf17, vi05        |  itof0.w vf16, vf16
      auto vf17 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      // itof done above.

      //    sq.xyzw vf15, -5(vi05)     |  nop
      frag.sq_points(vi05 - 5, vf15);

      //    ibeq vi06, vi05, L6        |  muli.xyz vf06, vf06, I
      //    sq.xyzw vf05, -6(vi05)     |  itof0.xyzw vf07, vf07
      vf06 = muli64_xyz(vf06);
      frag.sq_points(vi05 - 6, vf05);
      vf07 = itof0(vf07);
      if (vi05 == vi06) {
        goto end_of_int_to_float_loop;
      }

      //    lqi.xyzw vf05, vi05        |  itof12.xyz vf17, vf17
      vf05 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf17 = itof12xyz_0w(vf17);

      //    lqi.xyzw vf15, vi05        |  itof0.w vf17, vf17
      vf15 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      // itof doen above

      //    sq.xyzw vf16, -5(vi05)     |  nop
      frag.sq_points(vi05 - 5, vf16);
      //    ibeq vi06, vi05, L6        |  muli.xyz vf07, vf07, I
      vf07 = muli64_xyz(vf07);
      //    sq.xyzw vf06, -6(vi05)     |  itof0.xyzw vf05, vf05
      frag.sq_points(vi05 - 6, vf06);
      vf05 = itof0(vf05);
      if (vi05 == vi06) {
        goto end_of_int_to_float_loop;
      }

      //    lqi.xyzw vf06, vi05        |  itof12.xyz vf15, vf15
      vf06 = frag.lq_points_allow_past_end(vi05);
      vf15 = itof12xyz_0w(vf15);
      vi05++;

      //    lqi.xyzw vf16, vi05        |  itof0.w vf15, vf15
      vf16 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      // itof done above

      //    sq.xyzw vf17, -5(vi05)     |  nop
      frag.sq_points(vi05 - 5, vf17);

      //    ibne vi06, vi05, L5        |  muli.xyz vf05, vf05, I
      //    sq.xyzw vf07, -6(vi05)     |  itof0.xyzw vf06, vf06
      vf05 = muli64_xyz(vf05);
      frag.sq_points(vi05 - 6, vf07);
      vf06 = itof0(vf06);
      if (vi05 != vi06) {
        goto top_of_points_loop;
      }

    end_of_int_to_float_loop:
      // another points loop
      Vector4f vf10;

      //    L6:
      //    lq.xyzw vf09, -4(vi05)     |  nop
      auto vf09 = frag.lq_points_allow_past_end(vi05 - 4);
      //    lq.xyzw vf05, -3(vi05)     |  nop
      vf05 = frag.lq_points_allow_past_end(vi05 - 3);
      //    lq.xyzw vf15, -2(vi05)     |  nop
      vf15 = frag.lq_points_allow_past_end(vi05 - 2);
      //    iadd vi07, vi07, vi05      |  nop
      vi07 += vi05;
      //    iaddi vi07, vi07, -0x4     |  nop
      vi07 -= 4;
      //    iaddi vi05, vi05, -0x1     |  nop
      vi05 -= 1;
      //    iaddi vi08, vi05, -0x3     |  nop
      vi08 = vi05 - 3;
      //    ibeq vi07, vi05, L8        |  nop
      //    nop                        |  itof0.xyzw vf09, vf09
      vf09 = itof0(vf09);
      if (vi07 == vi05) {
        goto end_of_points2;
      }

      //    lqi.xyzw vf10, vi05        |  itof0.xyzw vf05, vf05
      vf10 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf05 = itof0(vf05);

      //    lqi.xyzw vf06, vi05        |  itof0.w vf15, vf15
      vf06 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf15 = itof12xyz_0w(vf15);

      //    lqi.xyzw vf16, vi05        |  itof12.xyz vf15, vf15
      vf16 = frag.lq_points_allow_past_end(vi05);
      vi05++;  // itof done above

      //    nop                        |  nop
      //    nop                        |  muli.xyz vf09, vf09, I
      vf09 = muli64_xyz(vf09);

      //    ibeq vi07, vi05, L8        |  muli.xyz vf05, vf05, I
      //    nop                        |  itof0.xyzw vf10, vf10
      vf05 = muli64_xyz(vf05);
      vf10 = itof0(vf10);
      if (vi05 == vi07) {
        goto end_of_points2;
      }

      Vector4f vf11;
    top_of_points2:
      //    L7:
      //    lqi.xyzw vf11, vi05        |  itof0.xyzw vf06, vf06
      vf11 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf06 = itof0(vf06);
      //    lqi.xyzw vf07, vi05        |  itof0.w vf16, vf16
      vf07 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf16 = itof12xyz_0w(vf16);

      //    lqi.xyzw vf17, vi05        |  itof12.xyz vf16, vf16
      vf17 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      //    sqi.xyzw vf09, vi08        |  nop
      frag.sq_points(vi08, vf09);
      vi08++;
      //    sqi.xyzw vf05, vi08        |  muli.xyz vf10, vf10, I
      frag.sq_points(vi08, vf05);
      vi08++;
      vf10 = muli64_xyz(vf10);
      //    ibeq vi07, vi05, L8        |  muli.xyz vf06, vf06, I
      vf06 = muli64_xyz(vf06);
      //    sqi.xyzw vf15, vi08        |  itof0.xyzw vf11, vf11
      frag.sq_points(vi08, vf15);
      vi08++;
      vf11 = itof0(vf11);
      if (vi07 == vi05) {
        goto end_of_points2;
      }

      //    lqi.xyzw vf09, vi05        |  itof0.xyzw vf07, vf07
      vf09 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf07 = itof0(vf07);
      //    lqi.xyzw vf05, vi05        |  itof0.w vf17, vf17
      vf05 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf17 = itof12xyz_0w(vf17);
      //    lqi.xyzw vf15, vi05        |  itof12.xyz vf17, vf17
      vf15 = frag.lq_points_allow_past_end(vi05);
      vi05++;

      //    sqi.xyzw vf10, vi08        |  nop
      frag.sq_points(vi08, vf10);
      vi08++;
      //    sqi.xyzw vf06, vi08        |  muli.xyz vf11, vf11, I
      frag.sq_points(vi08, vf06);
      vi08++;
      vf11 = muli64_xyz(vf11);
      //    ibeq vi07, vi05, L8        |  muli.xyz vf07, vf07, I
      //    sqi.xyzw vf16, vi08        |  itof0.xyzw vf09, vf09
      vf07 = muli64_xyz(vf07);
      frag.sq_points(vi08, vf16);
      vi08++;
      vf09 = itof0(vf09);
      if (vi07 == vi05) {
        goto end_of_points2;
      }

      //    lqi.xyzw vf10, vi05        |  itof0.xyzw vf05, vf05
      vf10 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf05 = itof0(vf05);
      //    lqi.xyzw vf06, vi05        |  itof0.w vf15, vf15
      vf06 = frag.lq_points_allow_past_end(vi05);
      vi05++;
      vf15 = itof12xyz_0w(vf15);
      //    lqi.xyzw vf16, vi05        |  itof12.xyz vf15, vf15
      vf16 = frag.lq_points_allow_past_end(vi05);
      vi05++;

      //    sqi.xyzw vf11, vi08        |  nop
      frag.sq_points(vi08, vf11);
      vi08++;
      //    sqi.xyzw vf07, vi08        |  muli.xyz vf09, vf09, I
      frag.sq_points(vi08, vf07);
      vi08++;
      vf09 = muli64_xyz(vf09);
      //    ibne vi07, vi05, L7        |  muli.xyz vf05, vf05, I
      //    sqi.xyzw vf17, vi08        |  itof0.xyzw vf10, vf10
      vf05 = muli64_xyz(vf05);
      frag.sq_points(vi08, vf17);
      vi08++;
      vf10 = itof0(vf10);
      if (vi07 != vi05) {
        goto top_of_points2;
      }

    end_of_points2:
      //    L8:
      //    mtir vi01, vf04.z          |  nop
      u16 vi01 = vf04_z;
      //    mtir vi05, vf02.x          |  nop
      frag.prog_info.skip_bp2 = vf02_x;

      //    mtir vi14, vf02.y          |  nop
      frag.prog_info.skip_ips = vf02_y;
      //    mtir vi04, vf03.x          |  nop
      frag.prog_info.tgt_bp1_ptr = vf03_x;
      //    mtir vi06, vf03.y          |  nop
      frag.prog_info.tgt_bp2_ptr = vf03_y;
      //    mtir vi07, vf03.z          |  nop
      frag.prog_info.tgt_ip1_ptr = vf03_z;
      //    mtir vi08, vf03.w          |  nop
      frag.prog_info.tgt_ip2_ptr = vf03_w;
      //    isw.x vi01, 971(vi00)      |  nop
      frag.prog_info.misc_x = vi01;
      //    iaddi vi15, vi00, 0x0      |  nop
      frag.prog_info.kick_addr = 0;
      //    mtir vi03, vf_clrbuf.x          |  nop
      frag.prog_info.clr_ptr = 198;  // just forcing it to one buffer for now
      //    iaddiu vi_point_ptr, vi00, 0x32    |  nop
      frag.prog_info.point_ptr = 0x32;

      //    mr32.xyzw vf_gifbufs, vf_gifbufs       |  nop
      //    mfir.y vf_extra, vi00          |  nop :e
      //    mfir.w vf_extra, vi00          |  nop
      float temp = vf_gifbufs.x();
      vf_gifbufs.x() = vf_gifbufs.y();
      vf_gifbufs.y() = vf_gifbufs.z();
      vf_gifbufs.z() = vf_gifbufs.w();
      vf_gifbufs.w() = temp;
      vf_extra.y() = 0;
      vf_extra.w() = 0;
      frag.prog_info.gifbufs = vf_gifbufs;
      frag.prog_info.extra = vf_extra;
      // todo: maybe we need more.
    }

    // assert(false);
  }
}

void debug_print_info(const std::vector<TieProtoInfo>& out) {
  for (auto& proto : out) {
    fmt::print("[{:40}]\n", proto.name);
    fmt::print("  generic: {}\n", proto.uses_generic);
    fmt::print("  use count: {}\n", proto.instances.size());
    fmt::print("  stiffness: {}\n", proto.stiffness);
  }
}

u16 float_to_u16(float f) {
  u16 result;
  memcpy(&result, &f, 2);
  return result;
}

int get_fancy_base(int draw1, int draw2) {
  int total = draw1 + draw2;
  total += 3;
  total /= 4;
  total *= 4;
  return total;
}

void emulate_tie_instance_program(std::vector<TieProtoInfo>& protos) {
  for (auto& proto : protos) {
    //    bool first_instance = true;
    //    for (auto& instance : proto.instances) {
    for (u32 frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
      auto& frag = proto.frags.at(frag_idx);
      // for these sections, see the TIE Instance VU Program Doc.
      int draw_1_count = 0;
      int draw_2_count = 0;
      int ip_1_count = 0;

      /////////////////////////////////////
      // SETUP
      /////////////////////////////////////
      // this is some basic register setup for the TIE instance
      // ad also for the pipelined Draw1 loop.
      // we omit the pipeline startup here.

      // this was set by the previous program that sets up this prototype frag
      u16 clr_ptr = frag.prog_info.clr_ptr;
      u16 tgt_bp1_ptr = frag.prog_info.tgt_bp1_ptr;
      u16 tgt_bp2_ptr = frag.prog_info.tgt_bp2_ptr;
      u16 tgt_ip1_ptr = frag.prog_info.tgt_ip1_ptr;
      u16 tgt_ip2_ptr = frag.prog_info.tgt_ip2_ptr;
      u16 skip_bp2 = frag.prog_info.skip_bp2;
      u16 kick_addr = frag.prog_info.kick_addr;
      u16 dest_ptr = 0;  // they never initialized this... seems like a bug

      // lqi.xyzw vtx_0, vi_point_ptr        |  nop
      // use hard-coded lower buffer for model data
      u16 point_ptr = 0x32;
      // lq.xyzw vf_inds, 6(vi_clr_ptr)      |  nop
      // pipeline

      // lq.xyzw vf_clr2, 3(vi_clr_ptr)      |  nop
      // lq.xyzw vf_mtx0, 0(vi_clr_ptr)      |  nop
      // lq.xyzw vf_mtx1, 1(vi_clr_ptr)      |  nop
      // lq.xyzw vf_clr1, 2(vi_clr_ptr)      |  nop
      // this is the matrix

      // mtir vi_ind, vf_inds.x              |  nop
      // pipeline

      // lqi.xyzw vf_tex0, vi_point_ptr      |  mulaw.xyzw ACC, vf_clr2, vf00
      // pipeline
      // lq.xyzw vf_morph, 4(vi_clr_ptr)     |  maddax.xyzw ACC, vf_mtx0, vtx_0
      // we're going to ignore the "morph" and use hi-res everywehere

      // ilw.x vi01, 5(vi_clr_ptr)           |  madday.xyzw ACC, vf_mtx1, vtx_0
      // the vi01 is unused here. (indicates if we're generic or not)

      // lq.xyzw vf_clr0, 838(vi_ind)        |  maddz.xyzw vf_pos02, vf_clr1, vtx_0
      // pipeline

      // lqi.xyzw vf_vtx1, vi_point_ptr      |  nop
      // pipeline

      // lq.xyzw vf_res02, 5(vi_clr_ptr)     |  nop
      // loading the flags and stuff, which we will ignore too

      // iaddi vi_clr_ptr, vi_clr_ptr, 0x7   |  nop
      // u16 clr_ptr_base = clr_ptr;
      clr_ptr += 6;  // it says 7, but we want to point to the first index data.

      // mtir vi_ind, vf_inds.y              |  addx.w vf_res13, vf_res02, vf00 <- flags crap
      // div Q, vf00.w, vf_pos02.w           |  mulaw.xyzw ACC, vf_clr2, vf00
      // lqi.xyzw vf_tex1, vi_point_ptr      |  maddax.xyzw ACC, vf_mtx0, vf_vtx1
      // mtir vi01, vf_gifbufs.x             |  madday.xyzw ACC, vf_mtx1, vf_vtx1
      u16 vi01 = float_to_u16(frag.prog_info.gifbufs.x());

      // lq.xyzw vf_mtx2, 838(vi_ind)        |  maddz.xyzw vf_pos13, vf_clr1, vf_vtx1

      // isub vi01, vi01, vi_kick_addr       |  ftoi4.w vf_res02, vf_res02
      vi01 -= kick_addr;

      // iadd vi_tgt_bp1_ptr, vi_tgt_bp1_ptr, vi01   |  ftoi4.w vf_res13, vf_res13
      tgt_bp1_ptr += vi01;
      // iadd vi_tgt_bp2_ptr, vi_tgt_bp2_ptr, vi01   |  nop
      tgt_bp2_ptr += vi01;

      // fmt::print("b tgts: {} {}\n", tgt_bp1_ptr, tgt_bp2_ptr);
      // lqi.xyzw vf_vtx2, vi_point_ptr              |  mul.xyz vf_pos02, vf_pos02, Q
      // div Q, vf00.w, vf_pos13.w                   |  mul.xyz vf_tex0, vf_tex0, Q
      // mtir vi_ind, vf_inds.z                      |  addx.w vtx_0, vtx_0, vf_gifbufs
      // lqi.xyzw vf_tex2, vi_point_ptr              |  mulaw.xyzw ACC, vf_clr2, vf00
      // iadd vi_tgt_ip1_ptr, vi_tgt_ip1_ptr, vi01   |  maddax.xyzw ACC, vf_mtx0, vf_vtx2
      // iadd vi_tgt_ip2_ptr, vi_tgt_ip2_ptr, vi01   |  madday.xyzw ACC, vf_mtx1, vf_vtx2
      tgt_ip1_ptr += vi01;
      tgt_ip2_ptr += vi01;
      // fmt::print("i tgts: {} {}\n", tgt_ip1_ptr, tgt_ip2_ptr);
      // lq.xyzw vf_mtx3, 838(vi_ind)                |  ftoi4.xyz vf_res02, vf_pos02
      // ibeq vi_tgt_bp1_ptr, vi_dest_ptr, L40       |  maddz.xyzw vf_pos02, vf_clr1, vf_vtx2
      // iadd vi_kick_addr, vi_kick_addr, vi01       |  nop
      kick_addr += vi01;
      if (tgt_bp1_ptr == dest_ptr) {
        fmt::print("DRAW FINISH 1 (no points)\n");
        goto program_end;
      }

      /////////////////////////////////////
      // DRAW 1
      /////////////////////////////////////
      {
        // Draw 1 computes and sets vertices that appear once.
        // Note that it does 3 more vertices after reaching the target pointer.
        bool reached_target = false;
        int past_target = 0;
        while (past_target < 3) {
          // there's 1 load of colors per 4x verts.
          // (lqi.xyzw vf_inds, vi_clr_ptr         |  nop)
          // these are different per instance, but index into a palette shared by all instances
          // for the i-th point, we just load the i-th color index.

          // This is reordered.
          // A "T" means it is part of transformation and we leave it out.
          // A number corresponds to the line below.

          // (4) mtir vi_dest_ptr, vtx_0.w         |  nop
          // (2) lqi.xyzw vi_vtx3, vi_point_ptr    |  (T) mul.xyz vf_pos13, vf_pos13, Q
          // (T) div Q, vf00.w, vf_pos02.w         |  (T) mul.xyz vf_tex1, vf_tex1, Q
          // (1) mtir vi_ind, vf_inds.w            |  (3) addx.w vf_vtx1, vf_vtx1, vf_gifbufs
          // (5) lqi.xyzw vi_tex3, vi_point_ptr    |  (T) mulaw.xyzw ACC, vf_clr2, vf00
          // (7) sq.xyzw vf_tex0, 0(vi_dest_ptr)   |  (T) maddax.xyzw ACC, vf_mtx0, vi_vtx3
          // (7) sq.xyzw vf_clr0, 1(vi_dest_ptr)   |  (T) madday.xyzw ACC, vf_mtx1, vi_vtx3
          // (6) lq.xyzw vi_clr3, 838(vi_ind)      |  (T) ftoi4.xyz vf_res13, vf_pos13
          // ibeq vi_tgt_bp1_ptr, vi_dest_ptr, L13 |  (T) maddz.xyzw vf_pos13, vf_clr1, vi_vtx3
          // (7) sq.xyzw vf_res02, 2(vi_dest_ptr)  |  nop

          // 01 - grab the index for this vertex color
          // we don't want to actually do the lookup here, just remember where we would have
          // looked.
          u32 clr_idx_idx = draw_1_count;

          // 02 - load the floating point vertex values
          auto vert_pos = frag.lq_points(point_ptr);
          point_ptr++;

          // 03 - do the weird gifbuf triple buffer with floats crap
          float vtx_w = vert_pos.w() + frag.prog_info.gifbufs.x();

          // 04 - now get the destination
          dest_ptr = float_to_u16(vtx_w);

          // 05 - load tex coords
          auto tex_coord = frag.lq_points(point_ptr);
          point_ptr++;

          // 06 - actually do the color load in the palette. (skip)

          // 07 - set vertex
          TieProtoVertex vertex_info;
          vertex_info.color_index_index = clr_idx_idx;
          vertex_info.pos.x() = vert_pos.x();
          vertex_info.pos.y() = vert_pos.y();
          vertex_info.pos.z() = vert_pos.z();
          vertex_info.tex.x() = tex_coord.x();
          vertex_info.tex.y() = tex_coord.y();
          vertex_info.tex.z() = tex_coord.z();

          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          assert(inserted);

          if (reached_target) {
            past_target++;
          }

          if (dest_ptr == tgt_bp1_ptr) {
            reached_target = true;
          }

          draw_1_count++;
        }
      }

      if (!skip_bp2) {
        // bp2 setup:
        // The BP2 drawing is similar to BP1, but duplicate draws vertices.
        bool reached_target = false;
        int past_target = 0;
        while (past_target < 2) {
          u32 clr_idx_idx = draw_1_count + draw_2_count;
          auto vert_pos = frag.lq_points(point_ptr);
          point_ptr++;
          float vtx_w = vert_pos.w() + frag.prog_info.gifbufs.x();
          dest_ptr = float_to_u16(vtx_w);
          auto tex_coord = frag.lq_points(point_ptr);
          // fmt::print("texw: [{}] {}\n", point_ptr, tex_coord.w());
          point_ptr++;
          float tex_w = tex_coord.w() + frag.prog_info.gifbufs.x();
          u16 dest2_ptr = float_to_u16(tex_w);

          TieProtoVertex vertex_info;
          vertex_info.color_index_index = clr_idx_idx;
          vertex_info.pos.x() = vert_pos.x();
          vertex_info.pos.y() = vert_pos.y();
          vertex_info.pos.z() = vert_pos.z();
          vertex_info.tex.x() = tex_coord.x();
          vertex_info.tex.y() = tex_coord.y();
          vertex_info.tex.z() = tex_coord.z();

          // fmt::print("double draw: {} {}\n", dest_ptr, dest2_ptr);
          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          assert(inserted);

          bool inserted2 = frag.vertex_by_dest_addr.insert({(u32)dest2_ptr, vertex_info}).second;
          assert(inserted2);

          if (reached_target) {
            past_target++;
          }

          if (dest_ptr == tgt_bp2_ptr) {
            reached_target = true;
          }

          draw_2_count++;
        }

        // setup
        // ibne vi00, vi_skip_bp2, L24      |  mul.xyz vf_pos13, vf_pos13, Q
        // lqi.xyzw vi_vtx3, vi_point_ptr   |  mul.xyz vf_tex1, vf_tex1, Q
        // div Q, vf00.w, vf_pos02.w        |  addx.w vf_vtx1, vf_vtx1, vf_gifbufs
        // mtir vi_ind, vf_inds.w           |  mulaw.xyzw ACC, vf_clr2, vf00
        // lqi.xyzw vf_inds, vi_clr_ptr     |  nop
        // sq.xyzw vf_tex0, 0(vi_dest_ptr)  |  addx.w vf_vtx2, vf_vtx2, vf_gifbufs
        // sq.xyzw vf_clr0, 1(vi_dest_ptr)  |  maddax.xyzw ACC, vf_mtx0, vi_vtx3
        // lqi.xyzw vi_tex3, vi_point_ptr   |  madday.xyzw ACC, vf_mtx1, vi_vtx3
        // lq.xyzw vi_clr3, 838(vi_ind)     |  ftoi4.xyz vf_res13, vf_pos13
        // lqi.xyzw vtx_0, vi_point_ptr     |  maddz.xyzw vf_pos13, vf_clr1, vi_vtx3
        // sq.xyzw vf_res02, 2(vi_dest_ptr) |  mul.xyz vf_pos02, vf_pos02, Q
        // mtir vi_dest_ptr, vf_vtx1.w      |  mul.xyz vf_tex2, vf_tex2, Q
        // lqi.xyzw vf_tex0, vi_point_ptr   |  mulaw.xyzw ACC, vf_clr2, vf00
        // mtir vi_ind, vf_inds.x           |  maddax.xyzw ACC, vf_mtx0, vtx_0
        // nop                              |  madday.xyzw ACC, vf_mtx1, vtx_0
        // div Q, vf00.w, vf_pos13.w        |  ftoi4.xyz vf_res02, vf_pos02
        // sq.xyzw vf_tex1, 0(vi_dest_ptr)  |  maddz.xyzw vf_pos02, vf_clr1, vtx_0
        // sq.xyzw vf_mtx2, 1(vi_dest_ptr)  |  nop
        // sq.xyzw vf_res13, 2(vi_dest_ptr) |  nop
        // mtir vi_dest_ptr, vf_vtx2.w      |  nop
        // lq.xyzw vf_clr0, 838(vi_ind)     |  addx.w vi_vtx3, vi_vtx3, vf_gifbufs
        // div Q, vf00.w, vf_pos02.w        |  mul.xyz vf_pos13, vf_pos13, Q
        // sq.xyzw vf_tex2, 0(vi_dest_ptr)  |  mul.xyz vi_tex3, vi_tex3, Q
        // sq.xyzw vf_mtx3, 1(vi_dest_ptr)  |  addx.w vi_tex3, vi_tex3, vf_gifbufs
        // sq.xyzw vf_res02, 2(vi_dest_ptr) |  nop
        // b L14                            |  ftoi4.xyz vf_res13, vf_pos13
        // mtir vi_dest_ptr, vi_vtx3.w      |  nop

        // bp2 chunk (out of 4)
        // lqi.xyzw vf_vtx1, vi_point_ptr        |  nop
        // mtir vi_ind, vf_inds.y              |  nop
        // mtir vi13, vi_tex3.w          |  mulaw.xyzw ACC, vf_clr2, vf00
        // sq.xyzw vi_tex3, 0(vi_dest_ptr)      |  addx.w vtx_0, vtx_0, vf_gifbufs
        // sq.xyzw vi_clr3, 1(vi_dest_ptr)      |  maddax.xyzw ACC, vf_mtx0, vf_vtx1
        // sq.xyzw vf_res13, 2(vi_dest_ptr)      |  madday.xyzw ACC, vf_mtx1, vf_vtx1
        // lqi.xyzw vf_tex1, vi_point_ptr        |  maddz.xyzw vf_pos13, vf_clr1, vf_vtx1
        // lq.xyzw vf_mtx2, 838(vi_ind)    |  mul.xyz vf_pos02, vf_pos02, Q
        // sq.xyzw vi_tex3, 0(vi13)      |  mul.xyz vf_tex0, vf_tex0, Q
        // sq.xyzw vi_clr3, 1(vi13)      |  addx.w vf_tex0, vf_tex0, vf_gifbufs
        // sq.xyzw vf_res13, 2(vi13)      |  nop
        // div Q, vf00.w, vf_pos13.w      |  nop
        // ibeq vi_tgt_bp2_ptr, vi_dest_ptr, L18       |  ftoi4.xyz vf_res02, vf_pos02
        // mtir vi_dest_ptr, vtx_0.w          |  nop
      }

      if (!frag.prog_info.skip_ips) {
        // Sadly TIE has no special case for highest lod.
        // this is surprising to me, but really does seem to be the case.

        // L31
        // lqi.xyzw vf_vtx1, vi_point_ptr        |  mulaw.xyzw ACC, vf_clr2, vf00
        // lqi.xyz vf_xyofs, vi_point_ptr        |  maddax.xyzw ACC, vf_mtx0, vtx_0
        // lqi.xyzw vf_tex1, vi_point_ptr        |  madday.xyzw ACC, vf_mtx1, vtx_0

        // we have an additional "xyofs" here, but otherwise similar

        // mtir vi_dest_ptr, vf_vtx2.w          |  maddz.xyzw vf_pos02, vf_clr1, vtx_0
        // as usual, using vtx.w for dest pointer.

        // mtir vi_ind, vf_inds.x        |  mulaw.xyzw ACC, vf_clr_val1, vf_morph
        // mtir vi10, vf_inds.y          |  maddz.xyzw vf_clr0, vf_clr0, vf_morph
        // mtir vi11, vf_inds.z          |  mulx.xyz vf_vtx1, vf_vtx1, vf_morph
        // inds works differently. There is a qw per vertex, containing 3 indices.
        // the formula is a pain, so I will ignore it for today.
        // ideally we can figure out the constant value of vf_morph first, to simplify all this.
        //

        // sq.xyzw vf_tex2, 0(vi_dest_ptr)      |  mul.xyz vf_res13, vf_pos13, Q
        // lq.xyzw vf_mtx2, 838(vi_ind)    |  mul.xyz vi_tex3, vi_tex3, Q
        // lq.xyzw vf_clr_val1, 838(vi10)    |  nop
        // lq.xyzw vf_clr_val2, 838(vi11)    |  nop
        // div Q, vf00.w, vf_pos02.w      |  ftoi4.xyz vf_res13, vf_res13
        // sq.xyzw vf_mtx3, 1(vi_dest_ptr)      |  add.xyzw vf_vtx1, vf_vtx1, vf_xyofs
        // lqi.xyzw vf_inds, vi_clr_ptr        |  mulay.xyzw ACC, vf_clr_val1, vf_morph
        // ibeq vi_tgt_ip1_ptr, vi_dest_ptr, L35       |  nop
        // sq.xyzw vf_res02, 2(vi_dest_ptr)      |  maddy.xyzw vf_clr_val1, vf_clr_val2, vf_morph

        int base = get_fancy_base(draw_1_count, draw_2_count);

        while (dest_ptr != tgt_ip1_ptr) {
          // todo - might be some rounding here.
          u32 clr_idx_idx = base + ip_1_count * 4 + 0;
          auto vert_pos = frag.lq_points(point_ptr);
          point_ptr++;
          auto xy_offs = frag.lq_points(point_ptr);
          point_ptr++;
          float vtx_w = vert_pos.w() + frag.prog_info.gifbufs.x();
          dest_ptr = float_to_u16(vtx_w);
          auto tex_coord = frag.lq_points(point_ptr);
          point_ptr++;

          TieProtoVertex vertex_info;
          vertex_info.color_index_index = clr_idx_idx;
          // random guess
          vert_pos = xy_offs;
          vertex_info.pos.x() = vert_pos.x();
          vertex_info.pos.y() = vert_pos.y();
          vertex_info.pos.z() = vert_pos.z();
          vertex_info.tex.x() = tex_coord.x();
          vertex_info.tex.y() = tex_coord.y();
          vertex_info.tex.z() = tex_coord.z();

          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          assert(inserted);

          ip_1_count++;
        }

        bool first_iter = true;
        while (dest_ptr != tgt_ip2_ptr) {
          // todo - might be some rounding here.
          u32 clr_idx_idx = base + ip_1_count * 4 + 0;
          auto vert_pos = frag.lq_points(point_ptr);
          point_ptr++;
          auto xy_offs = frag.lq_points(point_ptr);
          point_ptr++;
          float vtx_w = vert_pos.w() + frag.prog_info.gifbufs.x();
          dest_ptr = float_to_u16(vtx_w);
          auto tex_coord = frag.lq_points(point_ptr);
          point_ptr++;
          float tex_w = tex_coord.w() + frag.prog_info.gifbufs.x();
          u16 dest2_ptr = float_to_u16(tex_w);

          TieProtoVertex vertex_info;
          vertex_info.color_index_index = clr_idx_idx;
          // random guess
          vert_pos = xy_offs;
          vertex_info.pos.x() = vert_pos.x();
          vertex_info.pos.y() = vert_pos.y();
          vertex_info.pos.z() = vert_pos.z();
          vertex_info.tex.x() = tex_coord.x();
          vertex_info.tex.y() = tex_coord.y();
          vertex_info.tex.z() = tex_coord.z();

          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          assert(inserted);

          // first iteration of ip2 is a bit strange because how it jumps from loop to loop.
          // in some cases it uses ip2 on a point that should have used ip1 with the same addr
          // twice. I am pretty sure it's not our fault because we get exactly the right dvert.
          bool inserted2 = frag.vertex_by_dest_addr.insert({(u32)dest2_ptr, vertex_info}).second;
          if (!first_iter) {
            assert(inserted2);
          }
          first_iter = false;
          ip_1_count++;
        }
      }

      // now, let's check count:
      assert(frag.vertex_by_dest_addr.size() == frag.expected_dverts);

    program_end:;
      //      assert(false);
    }

    //    }
  }
}

// makes per-prototype meshes
void emulate_kicks(std::vector<TieProtoInfo>& protos) {
  for (auto& proto : protos) {
    for (auto& frag : proto.frags) {
      auto adgif_it = frag.prog_info.adgif_offset_in_gif_buf_qw.begin();
      auto adgif_end = frag.prog_info.adgif_offset_in_gif_buf_qw.end();
      auto str_it = frag.prog_info.str_gifs.begin();
      auto str_end = frag.prog_info.str_gifs.end();

      assert(frag.prog_info.adgif_offset_in_gif_buf_qw.at(0) == 0);
      assert(frag.prog_info.adgif_offset_in_gif_buf_qw.size() == frag.adgifs.size());
      const AdgifInfo* adgif_info = nullptr;
      int expected_next_tag = 0;

      // loop over strgifs
      while (str_it != str_end) {
        // try advance adgif
        if (adgif_it != adgif_end && (*adgif_it) == expected_next_tag) {
          int idx = adgif_it - frag.prog_info.adgif_offset_in_gif_buf_qw.begin();
          adgif_info = &frag.adgifs.at(idx);
          // fmt::print("using adgif {}\n", *adgif_it);
          expected_next_tag += 6;
          adgif_it++;
        }
        assert(adgif_info);

        // fmt::print("strip: {}\n", str_it->address);
        assert(expected_next_tag == str_it->address);
        expected_next_tag += 3 * str_it->nloop + 1;
        // here we have the right str and adgif.

        // kinda stupid, but we have to guess the base address of the gifbuf
        // 286 gifbuf
        // 470 gifbuf again
        // 654 ??
        assert(!frag.vertex_by_dest_addr.empty());
        int gifbuf_addr = frag.vertex_by_dest_addr.begin()->first;
        int base_address = 286;
        if (gifbuf_addr >= 654) {
          base_address = 654;
        } else if (gifbuf_addr >= 470) {
          base_address = 470;
        }

        // now, vertices!
        frag.strips.emplace_back();
        auto& strip = frag.strips.back();
        strip.adgif = *adgif_info;
        for (int vtx = 0; vtx < str_it->nloop; vtx++) {
          u32 vtx_addr = str_it->address + 1 + (3 * vtx) + base_address;
          strip.verts.push_back(frag.vertex_by_dest_addr.at(vtx_addr));
        }

        str_it++;
      }

      assert(adgif_it == adgif_end);
    }
  }
}

std::string debug_dump_proto_to_obj(const TieProtoInfo& proto) {
  std::vector<math::Vector<float, 3>> verts;
  std::vector<math::Vector<float, 2>> tcs;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& frag : proto.frags) {
    for (auto& strip : frag.strips) {
      // add verts...
      assert(strip.verts.size() >= 3);

      int vert_idx = 0;

      int vtx_idx_queue[3];

      int q_idx = 0;
      int startup = 0;
      while (vert_idx < (int)strip.verts.size()) {
        verts.push_back(strip.verts.at(vert_idx).pos / 65536);  // no idea
        tcs.push_back(math::Vector<float, 2>{strip.verts.at(vert_idx).tex.x(),
                                             strip.verts.at(vert_idx).tex.y()});
        vert_idx++;
        vtx_idx_queue[q_idx++] = verts.size();

        // wrap the index
        if (q_idx == 3) {
          q_idx = 0;
        }

        // bump the startup
        if (startup < 3) {
          startup++;
        }

        if (startup >= 3) {
          faces.push_back(
              math::Vector<int, 3>{vtx_idx_queue[0], vtx_idx_queue[1], vtx_idx_queue[2]});
        }
      }
    }
  }

  std::string result;
  for (auto& vert : verts) {
    result += fmt::format("v {} {} {}\n", vert.x(), vert.y(), vert.z());
  }
  for (auto& tc : tcs) {
    result += fmt::format("vt {} {}\n", tc.x(), tc.y());
  }
  for (auto& face : faces) {
    result += fmt::format("f {}/{} {}/{} {}/{}\n", face.x(), face.x(), face.y(), face.y(), face.z(),
                          face.z());
  }

  return result;
}

math::Vector<float, 3> transform_tie(const std::array<math::Vector4f, 4> mat,
                                     const math::Vector3f& pt) {
  auto temp = mat[0] * pt.x() + mat[1] * pt.y() + mat[2] * pt.z() + mat[3];

  //  math::Vector4f temp;
  //  temp.x() = pt.x();
  //  temp.y() = pt.y();
  //  temp.z() = pt.z();
  //  temp += mat[3];

  math::Vector3f result;
  result.x() = temp.x();
  result.y() = temp.y();
  result.z() = temp.z();
  return result;
}

std::string dump_full_to_obj(const std::vector<TieProtoInfo>& protos) {
  std::vector<math::Vector<float, 3>> verts;
  std::vector<math::Vector<float, 2>> tcs;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& proto : protos) {
    for (auto& inst : proto.instances) {
      auto& mat = inst.mat;
      for (auto& frag : proto.frags) {
        for (auto& strip : frag.strips) {
          // add verts...
          assert(strip.verts.size() >= 3);

          int vert_idx = 0;

          int vtx_idx_queue[3];

          int q_idx = 0;
          int startup = 0;
          while (vert_idx < (int)strip.verts.size()) {
            verts.push_back(transform_tie(mat, strip.verts.at(vert_idx).pos) / 65536);  // no idea
            tcs.push_back(math::Vector<float, 2>{strip.verts.at(vert_idx).tex.x(),
                                                 strip.verts.at(vert_idx).tex.y()});
            vert_idx++;
            vtx_idx_queue[q_idx++] = verts.size();

            // wrap the index
            if (q_idx == 3) {
              q_idx = 0;
            }

            // bump the startup
            if (startup < 3) {
              startup++;
            }

            if (startup >= 3) {
              faces.push_back(
                  math::Vector<int, 3>{vtx_idx_queue[0], vtx_idx_queue[1], vtx_idx_queue[2]});
            }
          }
        }
      }
    }
  }

  std::string result;
  for (auto& vert : verts) {
    result += fmt::format("v {} {} {}\n", vert.x(), vert.y(), vert.z());
  }
  for (auto& tc : tcs) {
    result += fmt::format("vt {} {}\n", tc.x(), tc.y());
  }
  for (auto& face : faces) {
    result += fmt::format("f {}/{} {}/{} {}/{}\n", face.x(), face.x(), face.y(), face.y(), face.z(),
                          face.z());
  }

  return result;
}

// The time of day stuff has a lot of lookups
// Each prototype has a palette. This palette is generated based on the time of day, blending
// together 8 colors from 8 times.

// Each instance is made up of fragments.
// The instance provides a color list per fragment.  These are indices into the palette.

// So, to know the color we need:
// - which prototype
// - which instance
// - which fragment
// - which color within the fragment
// and this tells us an index in the time of day palette.

struct BigPalette {
  std::vector<tfrag3::TimeOfDayColor> colors;
};

BigPalette make_big_palette(std::vector<TieProtoInfo>& protos) {
  BigPalette result;

  for (u32 proto_idx = 0; proto_idx < protos.size(); proto_idx++) {
    auto& proto = protos[proto_idx];
    u32 base_color_of_proto = result.colors.size();

    // add all colors
    for (auto& color : proto.time_of_day_colors) {
      result.colors.push_back(color);
    }

    for (u32 instance_idx = 0; instance_idx < proto.instances.size(); instance_idx++) {
      auto& instance = proto.instances[instance_idx];
      assert(proto.frags.size() == instance.frags.size());
      for (u32 frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
        auto& ifrag = instance.frags.at(frag_idx);
        ifrag.color_index_offset_in_big_palette = base_color_of_proto;
      }
    }
  }

  assert(result.colors.size() < UINT16_MAX);
  return result;
}

void update_mode_from_alpha1(u64 val, DrawMode& mode) {
  GsAlpha reg(val);
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
    assert(reg.fix() == 128);
    // Cv = (Cs - 0) * FIX + Cd
    // if fix = 128, it works out to 1.0
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_FIX_DST);
    // src plus dest
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::DEST &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // Cv = (Cs - Cd) * FIX + Cd
    assert(reg.fix() == 64);
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_FIX_DST);
  }

  else {
    fmt::print("unsupported blend: a {} b {} c {} d {}\n", (int)reg.a_mode(), (int)reg.b_mode(),
               (int)reg.c_mode(), (int)reg.d_mode());
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    //    assert(false);
  }
}

DrawMode process_draw_mode(const AdgifInfo& info, bool use_atest, bool use_decal) {
  DrawMode mode;
  mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  if (use_atest) {
    mode.enable_at();
    mode.set_aref(0x26);
    mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
    mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  } else {
    mode.disable_at();
  }
  if (use_decal) {
    mode.enable_decal();
  }
  mode.enable_depth_write();
  mode.enable_zt();                            // :zte #x1
  mode.set_depth_test(GsTest::ZTest::GEQUAL);  // :ztst (gs-ztest greater-equal))
  mode.disable_ab();
  mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);

  update_mode_from_alpha1(info.alpha_val, mode);
  if (!(info.clamp_val == 0b101 || info.clamp_val == 0 || info.clamp_val == 1 ||
        info.clamp_val == 0b100)) {
    fmt::print("clamp: 0x{:x}\n", info.clamp_val);
    assert(false);
  }

  mode.set_clamp_s_enable(info.clamp_val & 0b1);
  mode.set_clamp_t_enable(info.clamp_val & 0b100);

  return mode;
}

// we need the lev to pool textures with tfrag.
void add_vertices_and_static_draw(tfrag3::TieTree& tree,
                                  tfrag3::Level& lev,
                                  const TextureDB& tdb,
                                  const std::vector<TieProtoInfo>& protos) {
  // our current approach for static draws is just to flatten to giant mesh.

  std::unordered_map<u32, std::vector<u32>> draws_by_tex;

  std::unordered_map<u32, u32> interp_hack_colors;

  for (auto& proto : protos) {
    for (auto& inst : proto.instances) {
      for (size_t frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
        auto& frag = proto.frags[frag_idx];
        auto& ifrag = inst.frags.at(frag_idx);
        for (auto& strip : frag.strips) {
          // what texture are we using?
          u32 combo_tex = strip.adgif.combo_tex;

          // try looking it up in the existing textures
          u32 idx_in_lev_data = UINT32_MAX;
          for (u32 i = 0; i < lev.textures.size(); i++) {
            if (lev.textures[i].combo_id == combo_tex) {
              idx_in_lev_data = i;
              break;
            }
          }

          if (idx_in_lev_data == UINT32_MAX) {
            // didn't find it, have to add a new one
            auto tex_it = tdb.textures.find(combo_tex);
            if (tex_it == tdb.textures.end()) {
              bool ok_to_miss = false;  // TODO
              if (ok_to_miss) {
                // we're missing a texture, just use the first one.
                tex_it = tdb.textures.begin();
              } else {
                fmt::print(
                    "texture {} wasn't found. make sure it is loaded somehow. You may need to "
                    "include "
                    "ART.DGO or GAME.DGO in addition to the level DGOs for shared textures.\n",
                    combo_tex);
                fmt::print("tpage is {}\n", combo_tex >> 16);
                fmt::print("id is {} (0x{:x})\n", combo_tex & 0xffff, combo_tex & 0xffff);
                assert(false);
              }
            }
            idx_in_lev_data = lev.textures.size();
            lev.textures.emplace_back();
            auto& new_tex = lev.textures.back();
            new_tex.combo_id = combo_tex;
            new_tex.w = tex_it->second.w;
            new_tex.h = tex_it->second.h;
            new_tex.debug_name = tex_it->second.name;
            new_tex.debug_tpage_name = tdb.tpage_names.at(tex_it->second.page);
            new_tex.data = tex_it->second.rgba_bytes;
          }

          // determine the draw mode
          DrawMode mode =
              process_draw_mode(strip.adgif, frag.prog_info.misc_x == 0, frag.has_magic_tex0_bit);

          // okay, we now have a texture and draw mode, let's see if we can add to an existing...
          auto existing_draws_in_tex = draws_by_tex.find(idx_in_lev_data);
          tfrag3::StripDraw* draw_to_add_to = nullptr;
          if (existing_draws_in_tex != draws_by_tex.end()) {
            for (auto idx : existing_draws_in_tex->second) {
              if (tree.static_draws.at(idx).mode == mode) {
                draw_to_add_to = &tree.static_draws[idx];
              }
            }
          }

          if (!draw_to_add_to) {
            // nope, need to create a new draw
            tree.static_draws.emplace_back();
            draws_by_tex[idx_in_lev_data].push_back(tree.static_draws.size() - 1);
            draw_to_add_to = &tree.static_draws.back();
            draw_to_add_to->mode = mode;
            draw_to_add_to->tree_tex_id = idx_in_lev_data;
          }

          // now we have a draw, time to add vertices
          tfrag3::StripDraw::VisGroup vgroup;
          vgroup.vis_idx = inst.vis_id;         // associate with the tfrag for culling
          vgroup.num = strip.verts.size() + 1;  // one for the primitive restart!
          draw_to_add_to->num_triangles += strip.verts.size() - 2;
          for (auto& vert : strip.verts) {
            tfrag3::PreloadedVertex vtx;
            // todo fields
            auto tf = transform_tie(inst.mat, vert.pos);
            vtx.x = tf.x();
            vtx.y = tf.y();
            vtx.z = tf.z();
            vtx.s = vert.tex.x();
            vtx.t = vert.tex.y();
            vtx.q = vert.tex.z();
            if (vert.color_index_index == UINT32_MAX) {
              vtx.color_index = 0;
            } else {
              vtx.color_index = ifrag.color_indices.at(vert.color_index_index);
              assert(vert.color_index_index < ifrag.color_indices.size());
              vtx.color_index += ifrag.color_index_offset_in_big_palette;
            }

            size_t vert_idx = tree.vertices.size();
            tree.vertices.push_back(vtx);
            draw_to_add_to->vertex_index_stream.push_back(vert_idx);
          }
          draw_to_add_to->vertex_index_stream.push_back(UINT32_MAX);
          draw_to_add_to->vis_groups.push_back(vgroup);
        }
      }
    }
  }

  std::stable_sort(tree.static_draws.begin(), tree.static_draws.end(),
                   [](const tfrag3::StripDraw& a, const tfrag3::StripDraw& b) {
                     return a.tree_tex_id < b.tree_tex_id;
                   });
}

void extract_tie(const level_tools::DrawableTreeInstanceTie* tree,
                 const std::string& debug_name,
                 const std::vector<level_tools::TextureRemap>& tex_map,
                 const TextureDB& tex_db,
                 tfrag3::Level& out) {
  tfrag3::TieTree this_tree;

  // sanity check the vis tree (not a perfect check, but this is used in game and should be right)
  assert(tree->length == (int)tree->arrays.size());
  assert(tree->length > 0);
  auto last_array = tree->arrays.back().get();
  auto as_instance_array = dynamic_cast<level_tools::DrawableInlineArrayInstanceTie*>(last_array);
  assert(as_instance_array);
  assert(as_instance_array->length == (int)as_instance_array->instances.size());
  assert(as_instance_array->length > 0);
  u16 idx = as_instance_array->instances.front().id;
  for (auto& elt : as_instance_array->instances) {
    assert(elt.id == idx);
    idx++;
  }
  bool ok = verify_node_indices(tree);
  assert(ok);
  fmt::print("    tree has {} arrays and {} instances\n", tree->length, as_instance_array->length);

  // extract the vis tree. Note that this extracts the tree only down to the last draw node, a
  // parent of between 1 and 8 instances.
  extract_vis_data(tree, as_instance_array->instances.front().id, this_tree);

  // map of instance ID to its parent. We'll need this later.
  std::unordered_map<int, int> instance_parents;
  for (size_t node_idx = 0; node_idx < this_tree.bvh.vis_nodes.size(); node_idx++) {
    const auto& node = this_tree.bvh.vis_nodes[node_idx];
    if (node.flags == 0) {
      for (int i = 0; i < node.num_kids; i++) {
        instance_parents[node.child_id + i] = node_idx;
      }
    }
  }

  auto info = collect_instance_info(as_instance_array, &tree->prototypes.prototype_array_tie.data);
  update_proto_info(&info, tex_map, tex_db, tree->prototypes.prototype_array_tie.data);
  // debug_print_info(info);
  emulate_tie_prototype_program(info);
  emulate_tie_instance_program(info);
  emulate_kicks(info);

  auto dir = file_util::get_file_path({fmt::format("debug_out/tie-{}/", debug_name)});
  file_util::create_dir_if_needed(dir);
  for (auto& proto : info) {
    auto data = debug_dump_proto_to_obj(proto);
    file_util::write_text_file(fmt::format("{}/{}.obj", dir, proto.name), data);
    // file_util::create_dir_if_needed()
  }

  auto full = dump_full_to_obj(info);
  file_util::write_text_file(fmt::format("{}/ALL.obj", dir), full);

  auto full_palette = make_big_palette(info);
  add_vertices_and_static_draw(this_tree, out, tex_db, info);

  for (auto& draw : this_tree.static_draws) {
    for (auto& str : draw.vis_groups) {
      auto it = instance_parents.find(str.vis_idx);
      if (it == instance_parents.end()) {
        str.vis_idx = UINT32_MAX;
      } else {
        str.vis_idx = it->second;
      }
    }
  }

  this_tree.colors = full_palette.colors;
  fmt::print("TIE tree has {} draws\n", this_tree.static_draws.size());
  out.tie_trees.push_back(std::move(this_tree));
}
}  // namespace decompiler