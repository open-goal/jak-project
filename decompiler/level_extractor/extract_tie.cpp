#include "extract_tie.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

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
  out.first_leaf_node = first_child;
  out.last_leaf_node = first_child;

  if (tree->arrays.size() == 0) {
    // shouldn't hit this?
  } else if (tree->arrays.size() == 1) {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(tree->arrays.at(0).get());
    assert(array);
    out.first_root = array->instances.at(0).id;
    out.num_roots = array->instances.size();
    out.only_children = true;
  } else {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(0).get());
    assert(array);
    out.first_root = array->draw_nodes.at(0).id;
    out.num_roots = array->draw_nodes.size();
    out.only_children = false;
  }

  out.vis_nodes.resize(first_child - out.first_root);

  // may run 0 times, if there are only children.
  for (int i = 0; i < ((int)tree->arrays.size()) - 1; i++) {
    bool expecting_leaves = i == ((int)tree->arrays.size()) - 2;

    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(i).get());
    assert(array);
    u16 idx = first_child;
    for (auto& elt : array->draw_nodes) {
      auto& vis = out.vis_nodes.at(elt.id - out.first_root);
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

          assert(l->id >= out.first_leaf_node);
          if (leaf == 0) {
            vis.child_id = l->id;
          }
          out.last_leaf_node = std::max((u16)l->id, out.last_leaf_node);
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

          assert(l->id < out.first_leaf_node);
        }
      }
    }
  }
}

struct TieInstanceFragInfo {
  std::vector<u8> color_indices;
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
  u32 combo_tex;
  u64 alpha_val;
  u64 clamp_val;
};

struct TieFrag {
  bool has_magic_tex0_bit = false;
  std::vector<AdgifInfo> adgifs;

  std::vector<u8> other_gif_data;
  std::vector<u8> points_data;

  u16 ilw(u32 qw, u32 offset) const {
    u32 byte_offset = qw * 16 + offset * 4;
    assert(byte_offset + 2 <= points_data.size());
    u16 result;
    memcpy(&result, points_data.data() + byte_offset, sizeof(u16));
    return result;
  }
};

struct TieProtoInfo {
  std::string name;
  std::vector<TieInstanceInfo> instances;
  bool uses_generic = false;
  float stiffness = 0;
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

  //  if (data[15]) {
  //    fmt::print("--- {}\n", data[15]);
  //  }

  //  for (int vec = 0; vec < 4; vec++) {
  //    fmt::print("{} {}\n", vec, result[vec].to_string_aligned());
  //  }

  return result;
}

constexpr int GEOM_IDX = 1;

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
    info.wind_index = instance.wind_index;
    // there's a value stashed here that we can get rid of
    // it is related to wind.
    info.unknown_wind_related_value = info.mat[0][3];
    info.mat[0][3] = 0.f;

    // todo get colors.
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
      // fmt::print("frag: {}, qwc: {}, off: {}\n", frag_idx, num_color_qwc, offset_bytes);
      for (u32 i = 0; i < num_color_qwc / 4; i++) {
        for (u32 j = 0; j < 4; j++) {
          frag_info.color_indices.push_back(
              instance.color_indices.data->words_by_seg.at(instance.color_indices.seg)
                  .at(((offset_bytes + instance.color_indices.byte_offset) / 4) + i)
                  .get_byte(j));
        }
      }
      info.frags.push_back(std::move(frag_info));
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
        assert(ra_tex0_val == 0 || ra_tex0_val == 0x800000000);  // todo: what does this mean
        frag_info.has_magic_tex0_bit = ra_tex0_val == 0x800000000;

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

        if (ra_tex0_val == 0x800000000) {
          fmt::print("texture {} in {} has weird tex setting\n", tex->second.name, proto.name);
        }

        u8 ra_mip = gif_data.at(16 * (tex_idx * 5 + 2) + 8);
        assert(ra_mip == (u8)GsRegisterAddress::MIPTBP1_1);
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
      int tex_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).tex_count;
      int other_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).gif_count;
      frag_info.other_gif_data.resize(16 * other_qwc);
      memcpy(frag_info.other_gif_data.data(),
             proto.geometry[GEOM_IDX].tie_fragments[frag_idx].gif_data.data() + (16 * tex_qwc),
             16 * other_qwc);
      frag_info.points_data = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].point_ref;
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

void emulate_tie_program(const std::vector<TieProtoInfo>& protos) {
  using math::Vector4f;

  // our convention here is to use the lower buffer for everything double buffered.

  // because double buffering was too easy, the xgkick output buffer is triple buffered!
  // it looks like each fragment gets to use 2 of the 3 buffers.
  float gifbuf_start = 8388894.f;   // 0x4b00011e. The 0x11e in the mantissa is 286.
  float gifbuf_middle = 8389078.f;  // 0x4b0001d6. The 0x1d6 in the mantissa is 470.
  float gifbuf_end = 8389262.f;     // 0x4b00028e. The 0x28e in the mantissa is 654.

  Vector4f vf_gifbufs(gifbuf_end, gifbuf_middle, gifbuf_end, gifbuf_middle);

  float gifbuf_sum = gifbuf_start + gifbuf_middle + gifbuf_end;
  Vector4f vf_extra(gifbuf_sum, 0, gifbuf_sum, 0);

  u16 misc_x = 0;
  u16 misc_y = 1;

  for (const auto& proto : protos) {
    fmt::print("TIE for proto: {}\n", proto.name);
    fmt::print("proto has {} frags\n", proto.frags.size());

    for (u32 frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
      const auto& frag = proto.frags[frag_idx];
      fmt::print("----frag {} with {} adgifs, {} bytes of extra\n", frag_idx, frag.adgifs.size(),
                 frag.other_gif_data.size());

      // the starting address of each adgif data group.
      std::vector<u16> adgif_offset_in_gif_buf_qw;

      u16 vi_point_ptr = 0;

      // todo: figure out the trick and just use a fixed addr.
      vf_gifbufs.z() = vf_extra.z() - vf_gifbufs.x();
      vf_gifbufs.x() = vf_extra.x() - vf_gifbufs.x();

      //    L1:
      //    lq.xyz vf01, 966(vi00)                |  nop    vf01 = adgif header.
      //    ilwr.w vi_tgt_bp1_ptr, vi_point_ptr   |  nop
      u16 vi_tgt_bp1_ptr = frag.ilw(vi_point_ptr, 3);
      fmt::print("vi_tgt_bp1_ptr: {}\n", vi_tgt_bp1_ptr);

      //    ilw.w vi_ind, 1(vi_point_ptr)         |  nop
      u16 vi_ind = frag.ilw(vi_point_ptr + 1, 3);
      fmt::print("vi_ind: {}\n", vi_ind);
      assert(vi_ind == frag.adgifs.size());  // this should loop over adgifs

      //    mtir vi06, vf_gifbufs.y               |  nop
      u16 vi06;
      memcpy(&vi06, &vf_gifbufs.y(), sizeof(u16));
      fmt::print("vi06: {}\n", vi06);
      assert(vi06 == 470 || vi06 == 286 || vi06 == 470);  // should be one of the three gifbufs.

      //    lqi.xyzw vf02, vi_point_ptr        |  suby.xz vf_gifbufs, vf_gifbufs, vf_gifbufs
      //    lqi.xyzw vf03, vi_point_ptr        |  nop
      //    lqi.xyzw vf04, vi_point_ptr        |  nop
      //    lqi.xyzw vf05, vi_point_ptr        |  nop
      //    mtir vi05, vf_gifbufs.x            |  nop
      //    lqi.xyzw vf06, vi_point_ptr        |  subw.w vf01, vf01, vf01

      // loads the adgif data into vf02 -> vf06
      // sets upper 32 bits of adgif header to 0 (should have already been this????)
      vf_gifbufs.x() -= vf_gifbufs.y();
      vf_gifbufs.z() -= vf_gifbufs.y();
      u16 vi05;
      memcpy(&vi05, &vf_gifbufs.x(), sizeof(u16));
      fmt::print("vi05: {}\n", vi05);
      if (vi06 == 470) {
        assert(vi05 == 286);
      } else if (vi06 == 286) {
        assert(vi05 == 654);
      } else {
        assert(vi05 == 470);
      }
      vi_point_ptr += 5;

    adgif_setup_loop_top:
      //    L2:
      //    iadd vi03, vi_tgt_bp1_ptr, vi05                |  nop
      u16 vi03 = vi_tgt_bp1_ptr + vi05;

      //    iadd vi_tgt_bp1_ptr, vi_tgt_bp1_ptr, vi06      |  nop
      vi_tgt_bp1_ptr += vi06;

      //    iaddi vi_ind, vi_ind, -0x1     |  nop
      vi_ind--;

      // store adgifs in one buffer
      adgif_offset_in_gif_buf_qw.push_back(vi03 - vi05);
      fmt::print("adgifs at offset {}\n", adgif_offset_in_gif_buf_qw.back());
      //    sqi.xyzw vf01, vi03        |  nop
      //    sqi.xyzw vf02, vi03        |  nop
      //    sqi.xyzw vf03, vi03        |  nop
      //    sqi.xyzw vf04, vi03        |  nop
      //    sqi.xyzw vf05, vi03        |  nop
      //    sqi.xyzw vf06, vi03        |  nop
      vi03 += 5;

      // and the other buffer
      //    sqi.xyzw vf01, vi_tgt_bp1_ptr        |  nop
      //    sqi.xyzw vf02, vi_tgt_bp1_ptr        |  nop
      //    sqi.xyzw vf03, vi_tgt_bp1_ptr        |  nop
      //    sqi.xyzw vf04, vi_tgt_bp1_ptr        |  nop
      //    sqi.xyzw vf05, vi_tgt_bp1_ptr        |  nop
      //    sqi.xyzw vf06, vi_tgt_bp1_ptr        |  nop
      vi_tgt_bp1_ptr += 5;

      //    ilwr.w vi_tgt_bp1_ptr, vi_point_ptr          |  nop
      vi_tgt_bp1_ptr = frag.ilw(vi_point_ptr, 3);

      //    lqi.xyzw vf02, vi_point_ptr        |  nop
      //    lqi.xyzw vf03, vi_point_ptr        |  nop
      //    lqi.xyzw vf04, vi_point_ptr        |  nop
      //    lqi.xyzw vf05, vi_point_ptr        |  nop
      vi_point_ptr += 5;

      //    ibgtz vi_ind, L2             |  nop
      if (((s16)vi_ind) > 0) {
        goto adgif_setup_loop_top;
      }
      //    lqi.xyzw vf06, vi_point_ptr        |  nop (adgif load)


      //    mtir vi_ind, vf02.w          |  nop

      //    iaddi vi_point_ptr, vi_point_ptr, -0x2     |  subw.w vf07, vf07, vf07
      //    ilwr.x vi07, vi_point_ptr          |  nop
      //    ilwr.y vi08, vi_point_ptr          |  nop
      //    ilwr.z vi_tgt_bp1_ptr, vi_point_ptr          |  nop
      //    iaddi vi_ind, vi_ind, -0x1     |  nop
      //    iaddi vi_point_ptr, vi_point_ptr, 0x1      |  nop
      //    ibeq vi00, vi_ind, L4        |  nop
      //    lq.xyz vf07, 967(vi08)     |  nop
      //    L3:
      //    iadd vi03, vi_tgt_bp1_ptr, vi05      |  nop
      //    iadd vi_tgt_bp1_ptr, vi_tgt_bp1_ptr, vi06      |  nop
      //    iaddi vi_ind, vi_ind, -0x1     |  nop
      //    sq.xyzw vf07, 0(vi03)      |  nop
      //    iswr.x vi07, vi03          |  nop
      //    sq.xyzw vf07, 0(vi_tgt_bp1_ptr)      |  nop
      //    iswr.x vi07, vi_tgt_bp1_ptr          |  nop
      //    ilwr.x vi07, vi_point_ptr          |  nop
      //    ilwr.y vi08, vi_point_ptr          |  nop
      //    ilwr.z vi_tgt_bp1_ptr, vi_point_ptr          |  nop
      //    iaddi vi_point_ptr, vi_point_ptr, 0x1      |  nop
      //    ibne vi00, vi_ind, L3        |  nop
      //    lq.xyz vf07, 967(vi08)     |  nop
      //    L4:
      //    iaddiu vi07, vi07, 0x4000  |  nop
      //    iaddiu vi07, vi07, 0x4000  |  nop
      //    iadd vi03, vi_tgt_bp1_ptr, vi05      |  nop
      //    iadd vi_tgt_bp1_ptr, vi_tgt_bp1_ptr, vi06      |  nop
      //    sq.xyzw vf07, 0(vi03)      |  nop
      //    iswr.x vi07, vi03          |  nop
      //    sq.xyzw vf07, 0(vi_tgt_bp1_ptr)      |  nop
      //    iswr.x vi07, vi_tgt_bp1_ptr          |  nop
      //    mtir vi06, vf04.x          |  nop
      //    lq.xyzw vf05, 50(vi00)     |  nop
      //    lq.xyzw vf15, 51(vi00)     |  nop
      //    iaddiu vi05, vi00, 0x34    |  nop
      //    nop                        |  nop
      //    iaddiu vi06, vi06, 0x32    |  itof0.xyzw vf05, vf05
      //    lqi.xyzw vf06, vi05        |  itof12.xyz vf15, vf15
      //    lqi.xyzw vf16, vi05        |  itof0.w vf15, vf15
      //    64.0                       |  nop :i
      //    ibeq vi06, vi05, L6        |  muli.xyz vf05, vf05, I
      //    mtir vi07, vf04.y          |  itof0.xyzw vf06, vf06
      //    L5:
      //    lqi.xyzw vf07, vi05        |  itof12.xyz vf16, vf16
      //    lqi.xyzw vf17, vi05        |  itof0.w vf16, vf16
      //    sq.xyzw vf15, -5(vi05)     |  nop
      //    ibeq vi06, vi05, L6        |  muli.xyz vf06, vf06, I
      //    sq.xyzw vf05, -6(vi05)     |  itof0.xyzw vf07, vf07
      //    lqi.xyzw vf05, vi05        |  itof12.xyz vf17, vf17
      //    lqi.xyzw vf15, vi05        |  itof0.w vf17, vf17
      //    sq.xyzw vf16, -5(vi05)     |  nop
      //    ibeq vi06, vi05, L6        |  muli.xyz vf07, vf07, I
      //    sq.xyzw vf06, -6(vi05)     |  itof0.xyzw vf05, vf05
      //    lqi.xyzw vf06, vi05        |  itof12.xyz vf15, vf15
      //    lqi.xyzw vf16, vi05        |  itof0.w vf15, vf15
      //    sq.xyzw vf17, -5(vi05)     |  nop
      //    ibne vi06, vi05, L5        |  muli.xyz vf05, vf05, I
      //    sq.xyzw vf07, -6(vi05)     |  itof0.xyzw vf06, vf06
      //    L6:
      //    lq.xyzw vf09, -4(vi05)     |  nop
      //    lq.xyzw vf05, -3(vi05)     |  nop
      //    lq.xyzw vf15, -2(vi05)     |  nop
      //    iadd vi07, vi07, vi05      |  nop
      //    iaddi vi07, vi07, -0x4     |  nop
      //    iaddi vi05, vi05, -0x1     |  nop
      //    iaddi vi08, vi05, -0x3     |  nop
      //    ibeq vi07, vi05, L8        |  nop
      //    nop                        |  itof0.xyzw vf09, vf09
      //    lqi.xyzw vf10, vi05        |  itof0.xyzw vf05, vf05
      //    lqi.xyzw vf06, vi05        |  itof0.w vf15, vf15
      //    lqi.xyzw vf16, vi05        |  itof12.xyz vf15, vf15
      //    nop                        |  nop
      //    nop                        |  muli.xyz vf09, vf09, I
      //    ibeq vi07, vi05, L8        |  muli.xyz vf05, vf05, I
      //    nop                        |  itof0.xyzw vf10, vf10
      //    L7:
      //    lqi.xyzw vf11, vi05        |  itof0.xyzw vf06, vf06
      //    lqi.xyzw vf07, vi05        |  itof0.w vf16, vf16
      //    lqi.xyzw vf17, vi05        |  itof12.xyz vf16, vf16
      //    sqi.xyzw vf09, vi08        |  nop
      //    sqi.xyzw vf05, vi08        |  muli.xyz vf10, vf10, I
      //    ibeq vi07, vi05, L8        |  muli.xyz vf06, vf06, I
      //    sqi.xyzw vf15, vi08        |  itof0.xyzw vf11, vf11
      //    lqi.xyzw vf09, vi05        |  itof0.xyzw vf07, vf07
      //    lqi.xyzw vf05, vi05        |  itof0.w vf17, vf17
      //    lqi.xyzw vf15, vi05        |  itof12.xyz vf17, vf17
      //    sqi.xyzw vf10, vi08        |  nop
      //    sqi.xyzw vf06, vi08        |  muli.xyz vf11, vf11, I
      //    ibeq vi07, vi05, L8        |  muli.xyz vf07, vf07, I
      //    sqi.xyzw vf16, vi08        |  itof0.xyzw vf09, vf09
      //    lqi.xyzw vf10, vi05        |  itof0.xyzw vf05, vf05
      //    lqi.xyzw vf06, vi05        |  itof0.w vf15, vf15
      //    lqi.xyzw vf16, vi05        |  itof12.xyz vf15, vf15
      //    sqi.xyzw vf11, vi08        |  nop
      //    sqi.xyzw vf07, vi08        |  muli.xyz vf09, vf09, I
      //    ibne vi07, vi05, L7        |  muli.xyz vf05, vf05, I
      //    sqi.xyzw vf17, vi08        |  itof0.xyzw vf10, vf10
      //    L8:
      //    mtir vi01, vf04.z          |  nop
      //    mtir vi05, vf02.x          |  nop
      //    mtir vi14, vf02.y          |  nop
      //    mtir vi_tgt_bp1_ptr, vf03.x          |  nop
      //    mtir vi06, vf03.y          |  nop
      //    mtir vi07, vf03.z          |  nop
      //    mtir vi08, vf03.w          |  nop
      //    isw.x vi01, 971(vi00)      |  nop
      //    iaddi vi15, vi00, 0x0      |  nop
      //    mtir vi03, vf_clrbuf.x          |  nop
      //    iaddiu vi_point_ptr, vi00, 0x32    |  nop



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
    }

    assert(false);
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
  for (size_t node_idx = 0; node_idx < this_tree.vis_nodes.size(); node_idx++) {
    const auto& node = this_tree.vis_nodes[node_idx];
    if (node.flags == 0) {
      for (int i = 0; i < node.num_kids; i++) {
        instance_parents[node.child_id + i] = node_idx;
      }
    }
  }

  auto info = collect_instance_info(as_instance_array, &tree->prototypes.prototype_array_tie.data);
  update_proto_info(&info, tex_map, tex_db, tree->prototypes.prototype_array_tie.data);
  debug_print_info(info);

  emulate_tie_program(info);

  // todo handle prototypes
  // todo handle vu1

  // todo time of day
  // todo tree parents
}
}  // namespace decompiler