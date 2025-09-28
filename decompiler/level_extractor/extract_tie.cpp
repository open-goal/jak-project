#include "extract_tie.h"

#include <array>

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

// Jak 2 notes:
// - proto flags are currently ignored, but stored.

namespace decompiler {

/// <summary>
/// Get the index of the first draw node in an array. Works for node or tfrag.
/// </summary>
/// <param name="array"></param>
/// <returns></returns>
u16 get_first_idx(const level_tools::DrawableInlineArray* array) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);
  if (as_tie_instances) {
    return as_tie_instances->instances.at(0).id;
  } else if (as_nodes) {
    return as_nodes->draw_nodes.at(0).id;
  } else {
    ASSERT(false);
  }
}

/// <summary>
/// Verify node indices follow the patterns we expect. Takes start as the expected first, writes the
/// end.
/// </summary>
/// <param name="array"></param>
/// <param name="start"></param>
/// <param name="end"></param>
/// <returns></returns>
bool verify_node_indices_from_array(const level_tools::DrawableInlineArray* array,
                                    u16 start,
                                    u16* end) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);

  if (as_tie_instances) {
    for (auto& elt : as_tie_instances->instances) {
      if (elt.id != start) {
        lg::error("bad inst: exp {} got {}", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else if (as_nodes) {
    for (auto& elt : as_nodes->draw_nodes) {
      if (elt.id != start) {
        lg::error("bad node: exp {} got {}", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else {
    lg::error("bad node array type: {}", array->my_type());
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
    ASSERT(array);
    out.bvh.first_root = array->instances.at(0).id;
    out.bvh.num_roots = array->instances.size();
    out.bvh.only_children = true;
  } else {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(0).get());
    ASSERT(array);
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
    ASSERT(array);
    u16 idx = first_child;
    for (auto& elt : array->draw_nodes) {
      auto& vis = out.bvh.vis_nodes.at(elt.id - out.bvh.first_root);
      ASSERT(vis.num_kids == 0xff);
      for (int j = 0; j < 4; j++) {
        vis.bsphere[j] = elt.bsphere.data[j];
      }
      vis.num_kids = elt.child_count;
      vis.flags = elt.flags;
      vis.my_id = elt.id;
      ASSERT(vis.flags == expecting_leaves ? 0 : 1);
      ASSERT(vis.num_kids > 0);
      ASSERT(vis.num_kids <= 8);
      ASSERT(elt.children.size() == vis.num_kids);
      if (expecting_leaves) {
        for (int leaf = 0; leaf < (int)vis.num_kids; leaf++) {
          auto l = dynamic_cast<level_tools::InstanceTie*>(elt.children.at(leaf).get());
          ASSERT(l);

          ASSERT(idx == l->id);

          ASSERT(l->id >= out.bvh.first_leaf_node);
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
          ASSERT(l);
          if (child == 0) {
            arr_idx = l->id;
          } else {
            ASSERT(arr_idx < l->id);
            arr_idx = l->id;
          }
          if (child == 0) {
            vis.child_id = l->id;
          }

          ASSERT(l->id < out.bvh.first_leaf_node);
        }
      }
    }
  }
}

constexpr int GEOM_MAX = 4;  // the amount of geoms

// Each TIE prototype is broken up into "fragments". These "fragments" have some maximum size based
// on the VU memory limit, so an instance may have multiple fragments, depending on how many
// vertices are in the model.

// Each instance has different set of time of day colors per fragment in the prototype.
// this type contains the indicies of these colors.
// For the PC port we combine all colors into a single "big palette".
// this stores the indices as indices into the original game's per fragment palette.
// and an offset for where this palette is located in the big palette.
struct TieInstanceFragInfo {
  // the color index table uploaded to VU.
  // this contains indices into the shared palette.
  std::vector<u8> color_indices;

  // in the PC port format, we upload a single giant time of day color. this points to the offset
  // of the colors from this frag instance.
  u16 color_index_offset_in_big_palette = -1;
};

// Each TIE instance has one of these. This is reorganized/unpacked data from the instance-tie type.
struct TieInstanceInfo {
  // The index of the prototype (the geometry) that is used by this instance
  u16 prototype_idx = 0;

  // our bsphere's index in the BVH tree
  u16 vis_id = 0;

  // not totally sure if we'll use this (currently unused in tfrag, but probably worth if we
  // actually cull using the tree)
  math::Vector4f bsphere;

  // the transformation matrix, unpacked from the weird TIE format.
  // this can be used to transform points directly to world-space points that work
  // with the normal math camera stuff.
  std::array<math::Vector4f, 4> mat;

  // this value is stashed inside the above matrix. It tells which "wind" we should use
  // we just need to pass this along to the C++ rendering code.
  u16 wind_index = 0;

  std::vector<TieInstanceFragInfo> frags;  // per-instance per-fragment info (just colors)
};

// The 5 qw of adgif data contains draw settings, and they also snuck in some extra data.
struct AdgifInfo {
  // secret stuff they snuck in
  u32 first_w;   // VU memory offset
  u32 second_w;  // some size
  u32 third_w;   // unused, at least for not-near TIE

  // the draw settings we care about:
  u32 combo_tex;  // PC texture ID
  u64 alpha_val;  // alpha blend settings
  u64 clamp_val;  // texture clamp settings
  u32 num_mips = -1;
};

// When the prototype is uploaded, it places a bunch of strgif tags in VU memory.
// we'll need to remember where these are.
struct StrGifInfo {
  u16 address;  // vu memory address
  u16 nloop;    // the nloop field of this strgif (how much to send)
  u16 mode;     // not yet fully understood, but can allow the use of other templates
  bool eop;     // end of packet flag
};

// data per vertex in a tie prototype
struct TieProtoVertex {
  math::Vector<float, 3> pos;  // position
  math::Vector<float, 3> tex;  // texture coordinate
  math::Vector<s8, 3> nrm;     // normal

  // NOTE: this is a double lookup.
  // first you look up the index in the _instance_ color table
  // then you look up the color in the _proto_'s interpolated color palette.
  u32 color_index_index;
  math::Vector<u8, 4> envmap_tint_color;
};

// a tie fragment is made up of strips. Each strip has a single adgif info, and vertices
// the vertices make up a triangle strip
struct TieStrip {
  AdgifInfo adgif;
  std::vector<TieProtoVertex> verts;
};

// the tie fragment
// this is a per-prototype (all instances share the same TieFrags)
struct TieFrag {
  bool has_magic_tex0_bit = false;  // use decal mode (todo)
  std::vector<AdgifInfo>
      adgifs;  // the adgifs that come with this tiefrag (different strips can hve different)

  std::vector<u8> other_gif_data;  // data sent from EE asm code, sizes/offsets/metadata
  std::vector<u8> points_data;     // data sent from EE asm code, actual vertex data
  std::vector<math::Vector4<s8>>
      normal_data_packed;  // jak2 etie only, not unpacked like the others

  // number of "dverts" expected from game's metadata. we check our extraction from this.
  u32 expected_dverts = 0;

  // all the strips in this fragment
  std::vector<TieStrip> strips;

  // this contains vertices, key is the address of the actual xyzf/st/rgbaq data in VU1 memory
  // after the prototype program runs
  std::unordered_map<u32, TieProtoVertex> vertex_by_dest_addr;

  math::Vector<u8, 4> envmap_tint_color = math::Vector<u8, 4>::zero();

  // simulate a load in the points data (using vu mem addr)
  math::Vector<float, 4> lq_points(u32 qw) const {
    ASSERT(qw >= 50);
    qw -= 50;
    ASSERT((qw * 16) + 16 <= points_data.size());
    math::Vector<float, 4> result;
    memcpy(result.data(), points_data.data() + (qw * 16), 16);
    return result;
  }

  math::Vector<s8, 3> get_normal_if_present(u32 nrm_idx) const {
    if (normal_data_packed.empty()) {
      // no normals on this model
      return math::Vector<s8, 3>::zero();
    } else {
      ASSERT(nrm_idx < normal_data_packed.size());
      ASSERT(normal_data_packed.at(nrm_idx).w() == 0);
      return normal_data_packed.at(nrm_idx).xyz();
    }
  }

  // simulate a load from points, but don't die if we load past the end
  // this can happen when pipelining.
  math::Vector<float, 4> lq_points_allow_past_end(u32 qw) const {
    ASSERT(qw >= 50);
    qw -= 50;
    if ((qw * 16) + 16 <= points_data.size()) {
      math::Vector<float, 4> result;
      memcpy(result.data(), points_data.data() + (qw * 16), 16);
      return result;
    } else {
      return math::Vector4f(-1, -1, -1, -1);
    }
  }

  // store data into points. annoyingly the points have to be unpacked
  // and they are modified in place.
  void sq_points(u32 qw, const math::Vector4f& data) {
    ASSERT(qw >= 50);
    qw -= 50;
    ASSERT((qw * 16) + 16 <= points_data.size());
    memcpy(points_data.data() + (qw * 16), data.data(), 16);
  }

  // do a ilw from the other gif data.
  u16 ilw_other_gif(u32 qw, u32 offset) const {
    // unpacked with v8.
    int qwi = qw;
    qwi -= (adgifs.size() * 5);
    ASSERT(qwi >= 0);
    return other_gif_data.at(qwi * 4 + offset);
  }

  // reg values from the prototype program that are used by the instance program.
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
    // u16 clr_ptr = 0;
    u16 point_ptr = 0;
    u16 misc_x = 0;  // at 971's x.
    math::Vector4f gifbufs;
    math::Vector4f extra;
  } prog_info;
};

struct TimeOfDayColor {
  math::Vector<u8, 4> rgba[8];
};

// main instance type
// unlike the GOAL type, we store all instances info in here too.
struct TieProtoInfo {
  std::string name;
  std::vector<TieInstanceInfo> instances;
  u32 proto_flag;
  float stiffness = 0;  // wind
  std::optional<AdgifInfo> envmap_adgif;
  std::vector<TimeOfDayColor> time_of_day_colors;  // c++ type for time of day data
  std::vector<TieFrag> frags;                      // the fragments of the prototype
};

/*!
 * Convert TIE packed matrix to normal one. this was figured out from the EE asm.
 */
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

/*!
 * Confirm that the initial value of all wind vectors is 0.
 * If this is true, we don't have to actually save them to the fr3 file, we can just create
 * a bunch of 0 vectors in the TIE setup.
 */
void check_wind_vectors_zero(const std::vector<TieProtoInfo>& protos, Ref wind_ref) {
  u16 max_wind = 0;
  for (auto& proto : protos) {
    for (auto& inst : proto.instances) {
      max_wind = std::max(inst.wind_index, max_wind);
    }
  }
  u32 wind_words = max_wind;
  wind_words *= 4;
  for (size_t i = 0; i < wind_words; i++) {
    auto& word = wind_ref.data->words_by_seg.at(wind_ref.seg).at(wind_ref.byte_offset / 4 + i);
    ASSERT(word.kind() == LinkedWord::PLAIN_DATA);
    ASSERT(word.data == 0);
  }
}

// get per-instance info from the level data
std::vector<TieProtoInfo> collect_instance_info(
    const level_tools::DrawableInlineArrayInstanceTie* instances,
    const std::vector<level_tools::PrototypeBucketTie>* protos,
    int geo) {
  std::vector<TieProtoInfo> result;

  // loop over instances in level
  for (auto& instance : instances->instances) {
    // copy basic data.
    TieInstanceInfo info;
    info.prototype_idx = instance.bucket_index;
    info.vis_id = instance.id;
    for (int i = 0; i < 4; i++) {
      info.bsphere[i] = instance.bsphere.data[i];
    }
    // from ee asm
    info.mat = extract_tie_matrix(instance.origin.data);
    info.mat[3][0] += info.bsphere[0];
    info.mat[3][1] += info.bsphere[1];
    info.mat[3][2] += info.bsphere[2];
    info.wind_index = instance.wind_index;

    info.mat[0][3] = 0.f;

    // each fragment has its own color data (3 dmatags)

    // the number of colors (qwc) is stored in the prototype, in the color-index-qwc array of bytes.
    // at an offset of index-start[geom] + frag_idx.

    // the actual data is located at the instance's color-indices + (proto.base-qw[geom] * 16)

    // and this is only the indices.... there's yet another lookup on the VU
    auto& proto = protos->at(info.prototype_idx);
    u32 offset_bytes = proto.base_qw[geo] * 16;
    // loop over frags. this is only the per-instance info so only colors indices. We know the
    // location/layout of the color data from the EE asm code.
    for (int frag_idx = 0; frag_idx < proto.frag_count[geo]; frag_idx++) {
      TieInstanceFragInfo frag_info;
      // read the number of quadwords
      u32 num_color_qwc = proto.color_index_qwc.at(proto.index_start[geo] + frag_idx);
      // loop over 4-byte words
      for (u32 i = 0; i < num_color_qwc * 4; i++) {
        // loop over bytes in word
        for (u32 j = 0; j < 4; j++) {
          frag_info.color_indices.push_back(
              instance.color_indices.data->words_by_seg.at(instance.color_indices.seg)
                  .at(((offset_bytes + instance.color_indices.byte_offset) / 4) + i)
                  .get_byte(j));
        }
      }
      info.frags.push_back(std::move(frag_info));
      ASSERT(info.frags.back().color_indices.size() > 0);

      offset_bytes += num_color_qwc * 16;
    }

    if (result.size() <= info.prototype_idx) {
      result.resize(info.prototype_idx + 1);
    }
    result[info.prototype_idx].instances.push_back(info);
  }

  return result;
}

/*!
 * adgif shader texture id's can be "remapped". I think it allows textures to be shared.
 * So far we haven't seen this feature used, but we do have the texture map and we check it here.
 */
u32 remap_texture(u32 original, const std::vector<level_tools::TextureRemap>& map) {
  auto masked = original & 0xffffff00;
  for (auto& t : map) {
    if (t.original_texid == masked) {
      ASSERT_MSG(false, "OKAY! remapped!");
      return t.new_texid | 20;
    }
  }
  return original;
}

AdgifInfo process_adgif(const std::vector<u8>& gif_data,
                        int tex_idx,
                        const std::vector<level_tools::TextureRemap>& map,
                        bool* uses_magic_tex0_bit) {
  AdgifInfo adgif;
  // address for the first adgif shader qw.
  u8 ra_tex0 = gif_data.at(16 * (tex_idx * 5 + 0) + 8);
  // data for the first adgif shader qw.
  u64 ra_tex0_val;
  memcpy(&ra_tex0_val, &gif_data.at(16 * (tex_idx * 5 + 0)), 8);

  // always expecting TEX0_1
  ASSERT(ra_tex0 == (u8)GsRegisterAddress::TEX0_1);

  // the value is overwritten by the login function. We don't care about this value, it's
  // specific to the PS2's texture system.
  ASSERT(ra_tex0_val == 0 || ra_tex0_val == 0x800000000);  // note: decal
  // the original value is a flag. this means to use decal texture mode (todo)
  if (uses_magic_tex0_bit) {
    *uses_magic_tex0_bit = ra_tex0_val == 0x800000000;
  }
  // there's also a hidden value in the unused bits of the a+d data. it'll be used by the
  // VU program.
  memcpy(&adgif.first_w, &gif_data.at(16 * (tex_idx * 5 + 0) + 12), 4);

  // Second adgif. Similar to the first, except the original data value is a texture ID.
  u8 ra_tex1 = gif_data.at(16 * (tex_idx * 5 + 1) + 8);
  u64 ra_tex1_val;
  memcpy(&ra_tex1_val, &gif_data.at(16 * (tex_idx * 5 + 1)), 8);
  ASSERT(ra_tex1 == (u8)GsRegisterAddress::TEX1_1);
  ASSERT(ra_tex1_val == 0x120);  // some flag
  u32 original_tex;
  memcpy(&original_tex, &gif_data.at(16 * (tex_idx * 5 + 1) + 8), 4);
  // try remapping it
  u32 new_tex = remap_texture(original_tex, map);
  if (original_tex != new_tex) {
    lg::info("map from 0x{:x} to 0x{:x}", original_tex, new_tex);
  }
  // texture the texture page/texture index, and convert to a PC port texture ID
  u32 tpage = new_tex >> 20;
  u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
  u32 tex_combo = (((u32)tpage) << 16) | tidx;
  // remember the texture id (may be invalid, will be checked later)
  adgif.combo_tex = tex_combo;
  // and the hidden value in the unused a+d
  memcpy(&adgif.second_w, &gif_data.at(16 * (tex_idx * 5 + 1) + 12), 4);
  // todo: figure out if this matters. maybe this is decal?
  if (ra_tex0_val == 0x800000000) {
    // lg::print("texture {} in {} has weird tex setting\n", tex->second.name, proto.name);
  }

  // mipmap settings. we ignore, but get the hidden value
  u8 ra_mip = gif_data.at(16 * (tex_idx * 5 + 2) + 8);
  ASSERT(ra_mip == (u8)GsRegisterAddress::MIPTBP1_1);
  memcpy(&adgif.third_w, &gif_data.at(16 * (tex_idx * 5 + 2) + 12), 4);
  // who cares about the value

  // clamp settings. we care about these. no hidden value.
  u8 ra_clamp = gif_data.at(16 * (tex_idx * 5 + 3) + 8);
  ASSERT(ra_clamp == (u8)GsRegisterAddress::CLAMP_1);
  u64 clamp;
  memcpy(&clamp, &gif_data.at(16 * (tex_idx * 5 + 3)), 8);
  adgif.clamp_val = clamp;

  // alpha settings. we care about these, but no hidden value
  u8 ra_alpha = gif_data.at(16 * (tex_idx * 5 + 4) + 8);
  ASSERT(ra_alpha == (u8)GsRegisterAddress::ALPHA_1);
  u64 alpha;
  memcpy(&alpha, &gif_data.at(16 * (tex_idx * 5 + 4)), 8);
  adgif.alpha_val = alpha;
  return adgif;
}

struct TieCategoryInfo {
  tfrag3::TieCategory category = tfrag3::TieCategory::NORMAL;
  tfrag3::TieCategory envmap_second_draw_category = tfrag3::TieCategory::NORMAL_ENVMAP;
  bool uses_envmap = false;
};

TieCategoryInfo get_jak2_tie_category(u32 flags) {
  constexpr int kJak2ProtoEnvmap = 2;
  constexpr int kJak2ProtoTpageAlpha = 4;
  constexpr int kJak2ProtoTpageWater = 128;
  TieCategoryInfo result;
  result.uses_envmap = flags & kJak2ProtoEnvmap;

  if (flags & kJak2ProtoTpageAlpha) {
    if (result.uses_envmap) {
      result.category = tfrag3::TieCategory::TRANS_ENVMAP;
      result.envmap_second_draw_category = tfrag3::TieCategory::TRANS_ENVMAP_SECOND_DRAW;
    } else {
      result.category = tfrag3::TieCategory::TRANS;
    }
    ASSERT((flags & kJak2ProtoTpageWater) == 0);
  } else if (flags & kJak2ProtoTpageWater) {
    if (result.uses_envmap) {
      result.category = tfrag3::TieCategory::WATER_ENVMAP;
      result.envmap_second_draw_category = tfrag3::TieCategory::WATER_ENVMAP_SECOND_DRAW;
    } else {
      result.category = tfrag3::TieCategory::WATER;
    }
    ASSERT((flags & kJak2ProtoTpageAlpha) == 0);
  } else {
    if (result.uses_envmap) {
      result.category = tfrag3::TieCategory::NORMAL_ENVMAP;
      result.envmap_second_draw_category = tfrag3::TieCategory::NORMAL_ENVMAP_SECOND_DRAW;
    } else {
      result.category = tfrag3::TieCategory::NORMAL;
    }
  }
  return result;
}

u64 alpha_value_for_jak2_tie_or_etie_alpha_override(tfrag3::TieCategory category) {
  switch (category) {
    case tfrag3::TieCategory::NORMAL:
    case tfrag3::TieCategory::NORMAL_ENVMAP:
      return 0;
    case tfrag3::TieCategory::TRANS:
    case tfrag3::TieCategory::WATER:
    case tfrag3::TieCategory::TRANS_ENVMAP:
    case tfrag3::TieCategory::WATER_ENVMAP:
      return 68;
    default:
      ASSERT_NOT_REACHED();
  }
}

/*!
 * Update per-proto information.
 */
void update_proto_info(std::vector<TieProtoInfo>* out,
                       const std::vector<level_tools::TextureRemap>& map,
                       const std::vector<level_tools::PrototypeBucketTie>& protos,
                       int geo,
                       GameVersion version) {
  out->resize(std::max(out->size(), protos.size()));
  for (size_t i = 0; i < protos.size(); i++) {
    const auto& proto = protos[i];
    auto& info = out->at(i);
    info.proto_flag = proto.flags;
    // for debug, remember the name
    info.name = proto.name;
    // wind "stiffness" nonzero value means it has the wind effect
    info.stiffness = proto.stiffness;
    math::Vector<u8, 4> jak2_tint_color;
    if (proto.has_envmap_shader) {
      std::vector<u8> adgif;
      for (auto x : proto.envmap_shader) {
        adgif.push_back(x);
      }
      info.envmap_adgif = process_adgif(adgif, 0, map, nullptr);

      if (version > GameVersion::Jak1) {
        jak2_tint_color = proto.jak2_tint_color;
      }
    }

    // bool use_crazy_jak2_etie_alpha_thing = proto.has_envmap_shader;

    // the actual colors (rgba) used by time of day interpolation
    // there are "height" colors. Each color is actually 8 colors that are interpolated.
    info.time_of_day_colors.resize(proto.time_of_day.height);
    for (int k = 0; k < (int)proto.time_of_day.height; k++) {
      for (int j = 0; j < 8; j++) {
        memcpy(info.time_of_day_colors[k].rgba[j].data(), &proto.time_of_day.colors[k * 8 + j], 4);
      }
    }

    // loop over fragments in the proto. This is the actual mesh data data and drawing settings
    for (int frag_idx = 0; frag_idx < proto.frag_count[geo]; frag_idx++) {
      TieFrag frag_info;

      // loop over adgif shaders
      for (int tex_idx = 0; tex_idx < proto.geometry[geo].tie_fragments.at(frag_idx).tex_count / 5;
           tex_idx++) {
        // this adgif shader data is modified in the real game by the login methods.
        // all TIE things have pretty normal adgif shaders

        // all the useful adgif data will be saved into this AdgifInfo

        // pointer to the level data
        auto& gif_data = proto.geometry[geo].tie_fragments[frag_idx].gif_data;

        auto adgif = process_adgif(gif_data, tex_idx, map, &frag_info.has_magic_tex0_bit);

        frag_info.adgifs.push_back(adgif);
      }

      // they store a vertex count. we later use this to sanity check out mesh extraction
      frag_info.expected_dverts = proto.geometry[geo].tie_fragments[frag_idx].num_dverts;

      // each frag also has "other" data. This is some index data that the VU program uses.
      // it comes in gif_data, after tex_qwc (determined from EE program)
      int tex_qwc = proto.geometry[geo].tie_fragments.at(frag_idx).tex_count;
      int other_qwc = proto.geometry[geo].tie_fragments.at(frag_idx).gif_count;
      frag_info.other_gif_data.resize(16 * other_qwc);
      memcpy(frag_info.other_gif_data.data(),
             proto.geometry[geo].tie_fragments[frag_idx].gif_data.data() + (16 * tex_qwc),
             16 * other_qwc);

      // each frag's "point" data. These are stored as int16's, but get unpacked to 32-bit ints by
      // the VIF. (determined from EE program)
      const auto& pr = proto.geometry[geo].tie_fragments[frag_idx].point_ref;
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

      // normals (jak 2)
      const auto& normal_data = proto.geometry[geo].tie_fragments[frag_idx].normals;
      frag_info.normal_data_packed.resize(normal_data.size() / 4);
      for (size_t ni = 0; ni < normal_data.size() / 4; ni++) {
        for (int j = 0; j < 4; j++) {
          frag_info.normal_data_packed[ni][j] = normal_data[ni * 4 + j];
        }
      }

      if (proto.has_envmap_shader && version > GameVersion::Jak1) {
        frag_info.envmap_tint_color = jak2_tint_color;
      }

      // normals (jak 1)
      auto& generic = proto.geometry[geo].tie_fragments[frag_idx].generic_data;
      if (!generic.empty()) {
        // fmt::print("Generic for frag {} of {}\n", frag_idx, proto.name);

        struct GenericTieHeader {
          u8 effect;
          u8 interp_table_size;
          u8 num_bps;
          u8 num_ips;
          math::Vector<u8, 4> tint_color;
          u16 index_table_offset;
          u16 kick_table_offset;
          u16 normal_table_offset;
          u16 interp_table_offset;
        };
        static_assert(sizeof(GenericTieHeader) == 16);
        ASSERT(generic.size() >= sizeof(GenericTieHeader));
        GenericTieHeader header;
        memcpy(&header, generic.data(), sizeof(GenericTieHeader));
        frag_info.envmap_tint_color = header.tint_color;
        int normal_count = header.num_bps + header.num_ips;
        frag_info.normal_data_packed.resize(normal_count);
        for (int ni = 0; ni < normal_count; ni++) {
          for (int j = 0; j < 4; j++) {
            frag_info.normal_data_packed[ni][j] =
                generic.at(header.normal_table_offset + ni * 4 + j);
          }
        }
      }

      info.frags.push_back(std::move(frag_info));
    }
  }
}

// List of dma tags from the EE code.
// upload-palette/upload-model happen per prototype.
// (palette may happen per prototype, model per geometry, but we only use 1 geom)

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
// mscal 6  <- this runs a VU program that unpacks the model data
// call the models!

// These upload-color's happen per instance. They only happen after the upload-palette/model's
// happen for the given model.

// upload-color-0
// 6 qw of matrix plus flag stuff
// to 198 (relative to TOP)

// upload-color-1
// to 204 unsigned (relative to TOP)

// upload-color-2/ret
// mscal 0 <- this runs a VU program that generates GS data to draw the instance.

// MEMORY MAP of TIE
// these are quadword addresses.
// some things are double/triple buffered.
// we ignore this for the most part and by convention use the lower address.

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

// the vu program emulation will fill out the vertex positions/draw settings for each instance.

// helper functions for the vu programs
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
      ASSERT(vi04 == 0);

      //    ilw.w vi_ind, 1(vi_point_ptr)         |  nop
      // the next hidden integer is the number of adgif shaders used in this fragment.
      // we already know this, so check it.
      u16 vi_ind = frag.adgifs.at(0).second_w;
      ASSERT(vi_ind == frag.adgifs.size());

      //    mtir vi06, vf_gifbufs.y               |  nop
      // vi06 will be one of our gifbufs we can use.
      u16 vi06;
      memcpy(&vi06, &vf_gifbufs.y(), sizeof(u16));
      // lg::print("vi06: {}\n", vi06);
      ASSERT(vi06 == 470 || vi06 == 286 || vi06 == 654);  // should be one of the three gifbufs.

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
      // lg::print("vi05: {}\n", vi05);
      // check that we understand the buffer rotation.
      if (vi06 == 470) {
        ASSERT(vi05 == 286);
      } else if (vi06 == 286) {
        ASSERT(vi05 == 654);
      } else {
        ASSERT(vi05 == 470);
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
      // lg::print("adgifs at offset {}\n", frag.prog_info.adgif_offset_in_gif_buf_qw.back());
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
      /*
        ((skip-bp2    uint8  :offset-assert 0)
         (skip-ips    uint8  :offset-assert 1)
         (gifbuf-skip uint8  :offset-assert 2)
         (strips      uint8  :offset-assert 3)
         (target-bp1  uint8  :offset-assert 4)
         (target-bp2  uint8  :offset-assert 5)
         (target-ip1  uint8  :offset-assert 6)
         (target-ip2  uint8  :offset-assert 7)
         (target-bps  uint8  :offset-assert 8)
         (target-ips  uint8  :offset-assert 9)
         (is-generic  uint8  :offset-assert 10)
       */
      ASSERT(frag.other_gif_data.size() > 1);
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
      ASSERT(vi_ind >= frag.adgifs.size());  // at least 1 draw per shader.
      ASSERT(vi_ind < 1000);                 // check for insane value.
      // lg::print("got: {}, other size: {}\n", vi_ind, frag.other_gif_data.size());

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
      ASSERT(vi08 == 0);

      //    ilwr.z vi04, vi_point_ptr          |  nop
      vi04 = frag.ilw_other_gif(vi_point_ptr, 2);
      // offset

      // lg::print("[{}] 7: {} 8: {} 4: {}, for {}\n", vi_point_ptr, vi07, vi08, vi04, vi_ind - 1);

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
        // lg::print("strgif at {}, {}\n", vi03, vi04);

        //    iswr.x vi07, vi03          |  nop
        info.nloop = vi07 & 0x7fff;
        info.eop = vi07 & 0x8000;
        ASSERT(!info.eop);  // seems like we handle this manually after the loop
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
        // lg::print("[{}] 7: {} 8: {} 4: {}, for {}\n", vi_point_ptr, vi07, vi08, vi04, vi_ind);
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
      // lg::print("bonus points: {}\n", vi07);
      vf06 = itof0(vf06);

      //    L5:
      Vector4f vf07;
    top_of_points_loop:
      // lg::print("{}/{}\n", vi05, vi06);
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
      // frag.prog_info.clr_ptr = 198;  // just forcing it to one buffer for now
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

    // ASSERT(false);
  }
}

void debug_print_info(const std::vector<TieProtoInfo>& out) {
  for (auto& proto : out) {
    lg::debug("[{:40}]", proto.name);
    lg::debug("  flag: {}", proto.proto_flag);
    lg::debug("  use count: {}", proto.instances.size());
    lg::debug("  stiffness: {}", proto.stiffness);
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

struct NrmDebug {
  int bp1 = 0;
  int bp2 = 0;
  int ip1 = 0;
  int ip2 = 0;
};

void emulate_tie_instance_program(std::vector<TieProtoInfo>& protos, GameVersion version) {
  for (auto& proto : protos) {
    //    bool first_instance = true;
    //    for (auto& instance : proto.instances) {
    for (u32 frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
      auto& frag = proto.frags.at(frag_idx);
      // for these sections, see the TIE Instance VU Program Doc.
      int draw_1_count = 0;
      int draw_2_count = 0;
      int ip_1_count = 0;

      int normal_table_offset = 0;

      NrmDebug nd;

      /////////////////////////////////////
      // SETUP
      /////////////////////////////////////
      // this is some basic register setup for the TIE instance
      // ad also for the pipelined Draw1 loop.
      // we omit the pipeline startup here.

      // this was set by the previous program that sets up this prototype frag
      // u16 clr_ptr = frag.prog_info.clr_ptr;
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
      // clr_ptr += 6;  // it says 7, but we want to point to the first index data.

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

      // lg::print("b tgts: {} {}\n", tgt_bp1_ptr, tgt_bp2_ptr);
      // lqi.xyzw vf_vtx2, vi_point_ptr              |  mul.xyz vf_pos02, vf_pos02, Q
      // div Q, vf00.w, vf_pos13.w                   |  mul.xyz vf_tex0, vf_tex0, Q
      // mtir vi_ind, vf_inds.z                      |  addx.w vtx_0, vtx_0, vf_gifbufs
      // lqi.xyzw vf_tex2, vi_point_ptr              |  mulaw.xyzw ACC, vf_clr2, vf00
      // iadd vi_tgt_ip1_ptr, vi_tgt_ip1_ptr, vi01   |  maddax.xyzw ACC, vf_mtx0, vf_vtx2
      // iadd vi_tgt_ip2_ptr, vi_tgt_ip2_ptr, vi01   |  madday.xyzw ACC, vf_mtx1, vf_vtx2
      tgt_ip1_ptr += vi01;
      tgt_ip2_ptr += vi01;
      // lg::print("i tgts: {} {}\n", tgt_ip1_ptr, tgt_ip2_ptr);
      // lq.xyzw vf_mtx3, 838(vi_ind)                |  ftoi4.xyz vf_res02, vf_pos02
      // ibeq vi_tgt_bp1_ptr, vi_dest_ptr, L40       |  maddz.xyzw vf_pos02, vf_clr1, vf_vtx2
      // iadd vi_kick_addr, vi_kick_addr, vi01       |  nop
      kick_addr += vi01;
      if (tgt_bp1_ptr == dest_ptr) {
        lg::info("DRAW FINISH 1 (no points)");
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
          vertex_info.envmap_tint_color = frag.envmap_tint_color;
          vertex_info.nrm = frag.get_normal_if_present(normal_table_offset++);

          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          // TODO hack
          if (version != GameVersion::Jak3) {
            ASSERT(inserted);
          }
          nd.bp1++;

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
          // lg::print("texw: [{}] {}\n", point_ptr, tex_coord.w());
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
          vertex_info.envmap_tint_color = frag.envmap_tint_color;
          vertex_info.nrm = frag.get_normal_if_present(normal_table_offset++);

          // lg::print("double draw: {} {}\n", dest_ptr, dest2_ptr);
          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          ASSERT(inserted);

          bool inserted2 = frag.vertex_by_dest_addr.insert({(u32)dest2_ptr, vertex_info}).second;
          ASSERT(inserted2);

          if (reached_target) {
            past_target++;
          }

          if (dest_ptr == tgt_bp2_ptr) {
            reached_target = true;
          }

          draw_2_count++;
          nd.bp2++;
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
          vertex_info.envmap_tint_color = frag.envmap_tint_color;
          vertex_info.nrm = frag.get_normal_if_present(normal_table_offset++);

          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          ASSERT(inserted);
          nd.ip1++;
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
          vertex_info.envmap_tint_color = frag.envmap_tint_color;
          vertex_info.nrm = frag.get_normal_if_present(normal_table_offset++);

          bool inserted = frag.vertex_by_dest_addr.insert({(u32)dest_ptr, vertex_info}).second;
          ASSERT(inserted);

          // first iteration of ip2 is a bit strange because how it jumps from loop to loop.
          // in some cases it uses ip2 on a point that should have used ip1 with the same addr
          // twice. I am pretty sure it's not our fault because we get exactly the right dvert.
          bool inserted2 = frag.vertex_by_dest_addr.insert({(u32)dest2_ptr, vertex_info}).second;
          if (!first_iter) {
            ASSERT(inserted2);
          }
          nd.ip2++;
          first_iter = false;
          ip_1_count++;
        }
      }

      // now, let's check count:
      ASSERT(frag.vertex_by_dest_addr.size() == frag.expected_dverts);

    program_end:;
      if (!frag.normal_data_packed.empty()) {
        // check that we have a normal per point, if we have normals
        // in ETIE, the normal count must be a multiple of 4 due to VIF upload
        // in Jak 1, generic processes normals on the EE, so there is no multiple of 4 requirement.
        // so we allow either a round-up-to-nearest-four or exact match to pass here.
        size_t total_dvert = nd.bp1 + nd.bp2 + nd.ip1 + nd.ip2;
        size_t rounded_up_dvert = total_dvert + 3;
        rounded_up_dvert /= 4;
        rounded_up_dvert *= 4;
        ASSERT(rounded_up_dvert == frag.normal_data_packed.size() ||
               total_dvert == frag.normal_data_packed.size());
      }
      //      ASSERT(false);
    }

    //    }
  }
}

// the final step of the VU program emulation is the "xgkick" instruction.
// there is a signal xgkick per fragment and it goes through the entire gif buf, hitting
// strgifs and adgifs. We look at the memory map for each frag and figure out which strips
// go with which adgifs, then copy vertices
void emulate_kicks(std::vector<TieProtoInfo>& protos) {
  for (auto& proto : protos) {
    for (auto& frag : proto.frags) {
      // we iterate over both adgifs/stgifs. sometimes you can have multiple strgifs that use the
      // same adgif. But we never expect to see multiple adgifs in a row.
      auto adgif_it = frag.prog_info.adgif_offset_in_gif_buf_qw.begin();
      auto adgif_end = frag.prog_info.adgif_offset_in_gif_buf_qw.end();
      auto str_it = frag.prog_info.str_gifs.begin();
      auto str_end = frag.prog_info.str_gifs.end();

      // but, we should always start with an adgif (otherwise we'd use the draw settings from
      // the last model, which we don't know)
      ASSERT(frag.prog_info.adgif_offset_in_gif_buf_qw.at(0) == 0);
      // and we expect that the VU program placed all adgifs somewhere
      ASSERT(frag.prog_info.adgif_offset_in_gif_buf_qw.size() == frag.adgifs.size());

      const AdgifInfo* adgif_info = nullptr;
      int expected_next_tag = 0;

      // loop over strgifs
      while (str_it != str_end) {
        // try to see if we got a adgif here
        if (adgif_it != adgif_end && (*adgif_it) == expected_next_tag) {
          // yep
          int idx = adgif_it - frag.prog_info.adgif_offset_in_gif_buf_qw.begin();
          adgif_info = &frag.adgifs.at(idx);
          // the next strgif should come 6 qw's after
          expected_next_tag += 6;
          adgif_it++;
        }
        ASSERT(adgif_info);

        // make sure the next str is where we expect
        ASSERT(expected_next_tag == str_it->address);
        // the next tag (either str/adgif) should be located at the end of this tag's data.
        expected_next_tag += 3 * str_it->nloop + 1;
        // here we have the right str and adgif.

        // kinda stupid, but we have to guess the base address of the gifbuf we're using.
        // 286 gifbuf
        // 470 gifbuf again
        // 654 ??
        ASSERT(!frag.vertex_by_dest_addr.empty());
        int gifbuf_addr = frag.vertex_by_dest_addr.begin()->first;
        int base_address = 286;
        if (gifbuf_addr >= 654) {
          base_address = 654;
        } else if (gifbuf_addr >= 470) {
          base_address = 470;
        }

        // now, we can add the vertices!
        frag.strips.emplace_back();
        auto& strip = frag.strips.back();
        strip.adgif = *adgif_info;
        // loop over all the vertices the strgif says we'll have
        for (int vtx = 0; vtx < str_it->nloop; vtx++) {
          // compute the address of this vertex (stored after the strgif)
          u32 vtx_addr = str_it->address + 1 + (3 * vtx) + base_address;
          // and grab it from the vertex map we made earlier.
          strip.verts.push_back(frag.vertex_by_dest_addr.at(vtx_addr));
        }

        str_it++;
      }

      ASSERT(adgif_it == adgif_end);
    }
  }
}

// from here on, we are mostly converting the "info" formats to the C++ renderer format (tfrag3)

/*!
 * Just used to debug, save a proto as an .obj mesh file.
 */
std::string debug_dump_proto_to_obj(const TieProtoInfo& proto) {
  std::vector<math::Vector<float, 3>> verts;
  std::vector<math::Vector<float, 2>> tcs;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& frag : proto.frags) {
    for (auto& strip : frag.strips) {
      // add verts...
      ASSERT(strip.verts.size() >= 3);

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

/*!
 * Transform a point in a prototype to the actual point location in the game world.
 */
math::Vector<float, 3> transform_tie(const std::array<math::Vector4f, 4> mat,
                                     const math::Vector3f& pt) {
  auto temp = mat[0] * pt.x() + mat[1] * pt.y() + mat[2] * pt.z() + mat[3];
  math::Vector3f result;
  result.x() = temp.x();
  result.y() = temp.y();
  result.z() = temp.z();
  return result;
}

/*!
 * Dump the entire tie tree to an obj. Used to debug the transform_tie function. If we get this
 * right, it should fit in with .obj's produced from the tfrag debug.
 */
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
          ASSERT(strip.verts.size() >= 3);

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
  std::vector<TimeOfDayColor> colors;
};

// combine all individual time of day palettes into one giant one.
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
      ASSERT(proto.frags.size() == instance.frags.size());
      for (u32 frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
        auto& ifrag = instance.frags.at(frag_idx);
        ifrag.color_index_offset_in_big_palette = base_color_of_proto;
      }
    }
  }

  ASSERT(result.colors.size() < UINT16_MAX);
  return result;
}

tfrag3::PackedTimeOfDay pack_big_palette(const BigPalette& in) {
  tfrag3::PackedTimeOfDay out;
  out.color_count = (in.colors.size() + 3) & (~3);
  out.data.resize(out.color_count * 8 * 4);

  for (u32 color = 0; color < in.colors.size(); color++) {
    for (u32 palette = 0; palette < 8; palette++) {
      for (u32 channel = 0; channel < 4; channel++) {
        out.read(color, palette, channel) = in.colors.at(color).rgba[palette][channel];
      }
    }
  }
  return out;
}

/*!
 * Given a current draw mode, update the alpha settings from a gs-alpha register value.
 */
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
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::DEST && reg.d_mode() == GsAlpha::BlendMode::DEST) {
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_DST_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::SOURCE) {
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_SRC_SRC_SRC);
  }

  else {
    lg::error("unsupported blend: a {} b {} c {} d {}", (int)reg.a_mode(), (int)reg.b_mode(),
              (int)reg.c_mode(), (int)reg.d_mode());
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    ASSERT(false);
  }
}

/*!
 * Get the draw mode settings that are pre-set for the entire bucket and not controlled by adgif
 * shaders
 */
DrawMode get_base_draw_test_mode_jak2(bool use_tra, tfrag3::TieCategory category) {
  DrawMode mode;
  mode.enable_ab();
  switch (category) {
    case tfrag3::TieCategory::NORMAL:
    case tfrag3::TieCategory::TRANS:
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      if (use_tra) {
        mode.enable_at();
        mode.set_aref(0x26);
        mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
        mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
      } else {
        mode.disable_at();
      }
      break;
    case tfrag3::TieCategory::WATER:
    case tfrag3::TieCategory::WATER_ENVMAP:
    case tfrag3::TieCategory::WATER_ENVMAP_SECOND_DRAW:
      // (new 'static 'gs-test :ate #x1 :afail #x1 :zte #x1 :ztst (gs-ztest greater-equal))
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      mode.enable_at();
      mode.set_aref(0);
      mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
      mode.set_alpha_test(DrawMode::AlphaTest::NEVER);
      break;

    case tfrag3::TieCategory::NORMAL_ENVMAP:
    case tfrag3::TieCategory::TRANS_ENVMAP:
    case tfrag3::TieCategory::NORMAL_ENVMAP_SECOND_DRAW:
    case tfrag3::TieCategory::TRANS_ENVMAP_SECOND_DRAW:
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      mode.disable_at();
      break;

    default:
      ASSERT(false);
  }
  return mode;
}

/*!
 * Convert adgif info into a C++ renderer DrawMode.
 */
DrawMode process_draw_mode(const AdgifInfo& info,
                           bool use_tra,
                           bool use_decal,
                           GameVersion version,
                           tfrag3::TieCategory category) {
  DrawMode mode;
  if (version == GameVersion::Jak1) {
    // some of these are set up once as part of tie initialization
    mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);

    // the atest giftag is set up at the end of the VU program.
    if (use_tra) {
      mode.enable_at();
      mode.set_aref(0x26);
      mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
      mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
    } else {
      mode.disable_at();
    }
    // set up once.
    mode.enable_depth_write();
    mode.enable_zt();                            // :zte #x1
    mode.set_depth_test(GsTest::ZTest::GEQUAL);  // :ztst (gs-ztest greater-equal))
    mode.disable_ab();
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
  } else {
    mode = get_base_draw_test_mode_jak2(use_tra, category);
  }

  if (use_decal) {
    mode.enable_decal();
  }

  if (version == GameVersion::Jak1) {
    // use alpha from adgif shader, that's what we did in the past (could be wrong?)
    update_mode_from_alpha1(info.alpha_val, mode);
    if (tfrag3::is_envmap_second_draw_category(category)) {
      mode.enable_ab();
    }

    if (tfrag3::is_envmap_first_draw_category(category)) {
      // decal seems to be somewhat rarely enbaled on envmapped stuff where it's clearly wrong (edge
      // the fj temple before the room with the blue eco switch)
      mode.disable_decal();
    }
  } else {
    if (tfrag3::is_envmap_second_draw_category(category)) {
      // envmap shader gets to control its own alpha
      update_mode_from_alpha1(info.alpha_val, mode);
    } else {
      // non-envmap always get overriden (both the first draw of etie, and normal tie)
      update_mode_from_alpha1(alpha_value_for_jak2_tie_or_etie_alpha_override(category), mode);
    }
  }

  // the clamp matters
  if (!(info.clamp_val == 0b101 || info.clamp_val == 0 || info.clamp_val == 1 ||
        info.clamp_val == 0b100)) {
    ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", info.clamp_val));
  }

  mode.set_clamp_s_enable(info.clamp_val & 0b1);
  mode.set_clamp_t_enable(info.clamp_val & 0b100);

  return mode;
}

DrawMode process_envmap_draw_mode(const AdgifInfo& info,
                                  GameVersion version,
                                  tfrag3::TieCategory category) {
  // this is overwritten at log-in time.
  // (set! (-> envmap-shader tex1) (new 'static 'gs-tex1 :mmag #x1 :mmin #x1))
  // (set! (-> envmap-shader clamp) (new 'static 'gs-clamp :wms (gs-tex-wrap-mode clamp) :wmt
  // (gs-tex-wrap-mode clamp))) (set! (-> envmap-shader alpha) (new 'static 'gs-alpha :b #x2 :c #x1
  // :d #x1))
  auto mode = process_draw_mode(info, false, false, version, category);
  mode.set_filt_enable(true);
  mode.set_clamp_s_enable(true);
  mode.set_clamp_t_enable(true);
  mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_DST_DST);
  return mode;
}

TieCategoryInfo get_jak1_tie_category(u32 flags) {
  TieCategoryInfo result;
  result.uses_envmap = flags & 2;
  result.category =
      result.uses_envmap ? tfrag3::TieCategory::NORMAL_ENVMAP : tfrag3::TieCategory::NORMAL;
  result.envmap_second_draw_category = tfrag3::TieCategory::NORMAL_ENVMAP_SECOND_DRAW;
  return result;
}

s32 get_or_add_texture(u32 combo_tex, tfrag3::Level& lev, const TextureDB& tdb) {
  if (combo_tex == 0) {
    // untextured
    combo_tex = (((u32)TextureDB::kPlaceholderWhiteTexturePage) << 16) |
                TextureDB::kPlaceholderWhiteTextureId;
  }

  // try looking it up in the existing textures that we have in the C++ renderer data.
  // (this is shared with tfrag)
  s32 idx_in_lev_data = INT32_MAX;
  for (u32 i = 0; i < lev.textures.size(); i++) {
    if (lev.textures[i].combo_id == combo_tex) {
      idx_in_lev_data = i;
      break;
    }
  }

  if (idx_in_lev_data == INT32_MAX) {
    // didn't find it, have to add a new one texture.
    auto tex_it = tdb.textures.find(combo_tex);
    if (tex_it == tdb.textures.end()) {
      ASSERT_MSG(false, fmt::format(
                            "texture {} wasn't found. make sure it is loaded somehow. You may need "
                            "to "
                            "include ART.DGO or GAME.DGO in addition to the level DGOs for shared "
                            "textures. tpage is {}. id is {} (0x{:x})",
                            combo_tex, combo_tex >> 16, combo_tex & 0xffff, combo_tex & 0xffff));
    }
    // add a new texture to the level data
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
  const auto& level_tex = lev.textures.at(idx_in_lev_data);
  const auto& it = tdb.animated_tex_output_to_anim_slot.find(level_tex.debug_name);
  if (it != tdb.animated_tex_output_to_anim_slot.end()) {
    // lg::warn("TIE animated texture: {}", level_tex.debug_name);
    return -int(it->second) - 1;
  }
  return idx_in_lev_data;
}

void handle_wind_draw_for_strip(
    tfrag3::TieTree& tree,
    std::unordered_map<s32, std::vector<u32>>& wind_draws_by_tex,
    const std::vector<std::vector<std::pair<int, int>>>& packed_vert_indices,
    s32 idx_in_lev_data,
    DrawMode mode,
    const TieStrip& strip,
    const TieInstanceInfo& inst,
    const TieInstanceFragInfo& ifrag,
    u32 wind_instance_idx,
    u32 frag_idx,
    u32 strip_idx) {
  // okay, we now have a texture and draw mode, let's see if we can add to an existing...
  auto existing_draws_in_tex = wind_draws_by_tex.find(idx_in_lev_data);
  tfrag3::InstancedStripDraw* draw_to_add_to = nullptr;
  if (existing_draws_in_tex != wind_draws_by_tex.end()) {
    for (auto idx : existing_draws_in_tex->second) {
      if (tree.instanced_wind_draws.at(idx).mode == mode) {
        draw_to_add_to = &tree.instanced_wind_draws[idx];
      }
    }
  }

  if (!draw_to_add_to) {
    // nope no existing draw for these settings, need to create a new draw
    tree.instanced_wind_draws.emplace_back();
    wind_draws_by_tex[idx_in_lev_data].push_back(tree.instanced_wind_draws.size() - 1);
    draw_to_add_to = &tree.instanced_wind_draws.back();
    draw_to_add_to->mode = mode;
    draw_to_add_to->tree_tex_id = idx_in_lev_data;
  }

  // now we have a draw, time to add vertices. We make a vertex "group" which is a group
  // of vertices that the renderer can decide to not draw based on visibility data.
  tfrag3::InstancedStripDraw::InstanceGroup igroup;
  // needs to be associated with this instance.
  igroup.vis_idx = inst.vis_id;  // associate with the instance for culling
  // number of vertices. The +1 is for the primitive restart index, which tells opengl
  // that the triangle strip is done.
  igroup.num = strip.verts.size() + 1;
  // groups for instances also need the instance idx to grab the appropriate wind/matrix
  // data.
  igroup.instance_idx = wind_instance_idx;
  draw_to_add_to->num_triangles += strip.verts.size() - 2;
  // note: this is a bit wasteful to duplicate the xyz/stq.
  tfrag3::PackedTieVertices::MatrixGroup grp;
  grp.matrix_idx = -1;
  grp.start_vert = packed_vert_indices.at(frag_idx).at(strip_idx).first;
  grp.end_vert = packed_vert_indices.at(frag_idx).at(strip_idx).second;
  tree.packed_vertices.matrix_groups.push_back(grp);
  for (auto& vert : strip.verts) {
    u16 color_index = 0;
    if (vert.color_index_index == UINT32_MAX) {
      color_index = 0;
    } else {
      color_index = ifrag.color_indices.at(vert.color_index_index);
      ASSERT(vert.color_index_index < ifrag.color_indices.size());
      color_index += ifrag.color_index_offset_in_big_palette;
    }

    size_t vert_idx = tree.packed_vertices.color_indices.size();
    tree.packed_vertices.color_indices.push_back(color_index);
    draw_to_add_to->vertex_index_stream.push_back(vert_idx);
  }

  // the primitive restart index
  draw_to_add_to->vertex_index_stream.push_back(UINT32_MAX);
  draw_to_add_to->instance_groups.push_back(igroup);
}

void handle_draw_for_strip(tfrag3::TieTree& tree,
                           std::unordered_map<s32, std::vector<u32>>& static_draws_by_tex,
                           std::vector<tfrag3::StripDraw>& category_draws,
                           const std::vector<std::vector<std::pair<int, int>>>& packed_vert_indices,
                           DrawMode mode,
                           s32 idx_in_lev_data,
                           const TieStrip& strip,
                           const TieInstanceInfo& inst,
                           const TieInstanceFragInfo& ifrag,
                           u32 proto_idx,
                           u32 frag_idx,
                           u32 strip_idx,
                           u32 matrix_idx) {
  // okay, we now have a texture and draw mode, let's see if we can add to an existing...
  auto existing_draws_in_tex = static_draws_by_tex.find(idx_in_lev_data);
  tfrag3::StripDraw* draw_to_add_to = nullptr;
  if (existing_draws_in_tex != static_draws_by_tex.end()) {
    for (auto idx : existing_draws_in_tex->second) {
      if (idx < category_draws.size() && category_draws.at(idx).mode == mode &&
          category_draws.at(idx).tree_tex_id == idx_in_lev_data) {
        draw_to_add_to = &category_draws[idx];
      }
    }
  }

  if (!draw_to_add_to) {
    // nope, need to create a new draw
    category_draws.emplace_back();
    static_draws_by_tex[idx_in_lev_data].push_back(category_draws.size() - 1);
    draw_to_add_to = &category_draws.back();
    draw_to_add_to->mode = mode;
    draw_to_add_to->tree_tex_id = idx_in_lev_data;
  }

  // now we have a draw, time to add vertices
  tfrag3::StripDraw::VisGroup vgroup;
  ASSERT(inst.vis_id < UINT16_MAX);
  vgroup.vis_idx_in_pc_bvh = inst.vis_id;  // associate with the instance for culling

  // only bother with tie proto idx if we use it
  if (tree.has_per_proto_visibility_toggle) {
    ASSERT(proto_idx < UINT16_MAX);
    vgroup.tie_proto_idx = proto_idx;
  }

  vgroup.num_inds = strip.verts.size() + 1;  // one for the primitive restart!
  vgroup.num_tris = strip.verts.size() - 2;
  draw_to_add_to->num_triangles += strip.verts.size() - 2;
  tfrag3::PackedTieVertices::MatrixGroup grp;
  grp.matrix_idx = matrix_idx;
  grp.start_vert = packed_vert_indices.at(frag_idx).at(strip_idx).first;
  grp.end_vert = packed_vert_indices.at(frag_idx).at(strip_idx).second;
  grp.has_normals = false;
  for (auto i = grp.start_vert; i < grp.end_vert; i++) {
    auto& v = tree.packed_vertices.vertices.at(i);
    if (v.nx || v.ny || v.nz) {
      grp.has_normals = true;
      break;
    }
  }

  tree.packed_vertices.matrix_groups.push_back(grp);
  tfrag3::StripDraw::VertexRun run;
  run.vertex0 = tree.packed_vertices.color_indices.size();
  run.length = strip.verts.size();
  for (auto& vert : strip.verts) {
    u16 color_index = 0;
    if (vert.color_index_index == UINT32_MAX) {
      color_index = 0;
    } else {
      color_index = ifrag.color_indices.at(vert.color_index_index);
      ASSERT(vert.color_index_index < ifrag.color_indices.size());
      color_index += ifrag.color_index_offset_in_big_palette;
    }

    tree.packed_vertices.color_indices.push_back(color_index);
  }
  draw_to_add_to->runs.push_back(run);
  draw_to_add_to->vis_groups.push_back(vgroup);
}

/*!
 * Convert TieProtoInfo's to C++ renderer format
 */
void add_vertices_and_static_draw(tfrag3::TieTree& tree,
                                  tfrag3::Level& lev,
                                  const TextureDB& tdb,
                                  const std::vector<TieProtoInfo>& protos,
                                  GameVersion version) {
  // our current approach for static draws is just to flatten to giant mesh, except for wind stuff.
  // this map sorts these two types of draws by texture.
  std::unordered_map<s32, std::vector<u32>> static_draws_by_tex;
  std::unordered_map<s32, std::vector<u32>> wind_draws_by_tex;

  std::array<std::vector<tfrag3::StripDraw>, tfrag3::kNumTieCategories> draws_by_category;

  if (version > GameVersion::Jak1) {
    tree.has_per_proto_visibility_toggle = true;
  }

  // loop over all prototypes
  for (size_t proto_idx = 0; proto_idx < protos.size(); proto_idx++) {
    const auto& proto = protos[proto_idx];
    if (tree.has_per_proto_visibility_toggle) {
      tree.proto_names.push_back(proto.name);
    }

    TieCategoryInfo info;
    switch (version) {
      case GameVersion::Jak1:
        info = get_jak1_tie_category(proto.proto_flag);
        break;
      case GameVersion::Jak2:
      case GameVersion::Jak3:
        info = get_jak2_tie_category(proto.proto_flag);
        break;
      default:
        ASSERT_NOT_REACHED();
    }

    //    bool using_wind = true;  // hack, for testing
    bool using_wind = proto.stiffness != 0.f;
    if (version == GameVersion::Jak2) {
      using_wind = false;  // disable wind on jak 2 for now - not supported in GOAL or C++ yet.
    }

    bool using_envmap = info.uses_envmap;
    ASSERT(using_envmap == proto.envmap_adgif.has_value());
    DrawMode envmap_drawmode;
    if (using_envmap) {
      envmap_drawmode = process_envmap_draw_mode(proto.envmap_adgif.value(), version,
                                                 info.envmap_second_draw_category);
    }

    // create the model first
    std::vector<std::vector<std::pair<int, int>>> packed_vert_indices;
    for (size_t frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
      packed_vert_indices.emplace_back();
      auto& frag_vert_indices = packed_vert_indices.back();
      auto& frag = proto.frags[frag_idx];  // shared info for all instances of this frag
      for (auto& strip : frag.strips) {
        int start = tree.packed_vertices.vertices.size();
        for (auto& vert : strip.verts) {
          tree.packed_vertices.vertices.push_back(
              {vert.pos.x(), vert.pos.y(), vert.pos.z(), vert.tex.x(), vert.tex.y(), vert.nrm.x(),
               vert.nrm.y(), vert.nrm.z(), vert.envmap_tint_color.x(), vert.envmap_tint_color.y(),
               vert.envmap_tint_color.z(), vert.envmap_tint_color.w()});
          // TODO: check if this means anything.
          // ASSERT(vert.tex.z() == 1.);
        }
        int end = tree.packed_vertices.vertices.size();
        frag_vert_indices.emplace_back(start, end);
      }
    }

    // loop over instances of the prototypes
    for (auto& inst : proto.instances) {
      // if we're using wind, we use the instanced renderer, which requires some extra info
      // and we should remember which instance ID we are.
      // Note: this is different from the game's instance index - we don't draw everything instanced
      // so the non-instanced models don't get a C++ renderer instance ID
      u32 wind_instance_idx = tree.wind_instance_info.size();
      u32 matrix_idx = tree.packed_vertices.matrices.size();
      if (using_wind) {
        tfrag3::TieWindInstance wind_instance_info;
        wind_instance_info.wind_idx = inst.wind_index;   // which wind value to apply in the table
        wind_instance_info.stiffness = proto.stiffness;  // wind stiffness (how much we move)
        wind_instance_info.matrix = inst.mat;            // instance transformation matrix.
        tree.wind_instance_info.push_back(wind_instance_info);
      } else {
        tree.packed_vertices.matrices.push_back(inst.mat);
      }

      // loop over fragments of the prototype
      for (size_t frag_idx = 0; frag_idx < proto.frags.size(); frag_idx++) {
        auto& frag = proto.frags[frag_idx];     // shared info for all instances of this frag
        auto& ifrag = inst.frags.at(frag_idx);  // color info for this instance of the frag
        // loop over triangle strips within the fragment
        for (size_t strip_idx = 0; strip_idx < frag.strips.size(); strip_idx++) {
          auto& strip = frag.strips[strip_idx];

          s32 idx_in_lev_data = get_or_add_texture(strip.adgif.combo_tex, lev, tdb);
          // determine the draw mode
          DrawMode mode = process_draw_mode(strip.adgif, frag.prog_info.misc_x == 0,
                                            frag.has_magic_tex0_bit, version, info.category);

          if (using_wind) {
            handle_wind_draw_for_strip(tree, wind_draws_by_tex, packed_vert_indices,
                                       idx_in_lev_data, mode, strip, inst, ifrag, wind_instance_idx,
                                       frag_idx, strip_idx);
          } else {
            // also add the envmap draw
            if (info.uses_envmap) {
              // first pass: normal draw mode, envmap bucket, normal draw list
              handle_draw_for_strip(tree, static_draws_by_tex,
                                    draws_by_category.at((int)info.category), packed_vert_indices,
                                    mode, idx_in_lev_data, strip, inst, ifrag, proto_idx, frag_idx,
                                    strip_idx, matrix_idx);
              s32 envmap_tex_idx =
                  get_or_add_texture(proto.envmap_adgif.value().combo_tex, lev, tdb);

              // second pass envmap draw mode, in envmap bucket, envmap-specific draw list
              handle_draw_for_strip(tree, static_draws_by_tex,
                                    draws_by_category.at((int)info.envmap_second_draw_category),
                                    packed_vert_indices, envmap_drawmode, envmap_tex_idx, strip,
                                    inst, ifrag, proto_idx, frag_idx, strip_idx, matrix_idx);
            } else {
              handle_draw_for_strip(tree, static_draws_by_tex,
                                    draws_by_category.at((int)info.category), packed_vert_indices,
                                    mode, idx_in_lev_data, strip, inst, ifrag, proto_idx, frag_idx,
                                    strip_idx, matrix_idx);
            }
          }
        }
      }
    }
  }

  // sort draws by texture. no idea if this really matters, but will reduce the number of
  // times the renderer changes textures. it at least makes the rendererdoc debugging easier.
  for (auto& draws : draws_by_category) {
    std::stable_sort(draws.begin(), draws.end(),
                     [](const tfrag3::StripDraw& a, const tfrag3::StripDraw& b) {
                       return a.tree_tex_id < b.tree_tex_id;
                     });
  }

  ASSERT(tree.static_draws.empty());
  tree.category_draw_indices[0] = 0;
  for (int i = 0; i < tfrag3::kNumTieCategories; i++) {
    tree.static_draws.insert(tree.static_draws.end(), draws_by_category[i].begin(),
                             draws_by_category[i].end());
    tree.category_draw_indices[i + 1] = tree.static_draws.size();
  }
}

/*!
 * The groups are created per-fragment, but usually you have a few fragments per instance, so there
 * are often consecutive groups that can be merged.
 */
void merge_groups(std::vector<tfrag3::InstancedStripDraw::InstanceGroup>& grps) {
  std::vector<tfrag3::InstancedStripDraw::InstanceGroup> result;
  result.push_back(grps.at(0));
  for (size_t i = 1; i < grps.size(); i++) {
    if (grps[i].vis_idx == result.back().vis_idx &&
        grps[i].instance_idx == result.back().instance_idx) {
      result.back().num += grps[i].num;
    } else {
      result.push_back(grps[i]);
    }
  }
  std::swap(result, grps);
}

void merge_groups(std::vector<tfrag3::StripDraw::VisGroup>& grps) {
  std::vector<tfrag3::StripDraw::VisGroup> result;
  result.push_back(grps.at(0));
  for (size_t i = 1; i < grps.size(); i++) {
    if (grps[i].vis_idx_in_pc_bvh == result.back().vis_idx_in_pc_bvh &&
        grps[i].tie_proto_idx == result.back().tie_proto_idx) {
      result.back().num_tris += grps[i].num_tris;
      result.back().num_inds += grps[i].num_inds;
    } else {
      result.push_back(grps[i]);
    }
  }
  std::swap(result, grps);
}

void merge_groups(std::vector<tfrag3::PackedTieVertices::MatrixGroup>& grps) {
  std::vector<tfrag3::PackedTieVertices::MatrixGroup> result;
  result.push_back(grps.at(0));

  for (size_t i = 1; i < grps.size(); i++) {
    auto& this_group = grps[i];
    auto& maybe_merge = result.back();
    if (this_group.start_vert == maybe_merge.end_vert &&
        this_group.matrix_idx == maybe_merge.matrix_idx &&
        this_group.has_normals == maybe_merge.has_normals) {
      maybe_merge.end_vert = this_group.end_vert;
    } else {
      result.push_back(this_group);
    }
  }

  std::swap(result, grps);
}

void extract_tie(const level_tools::DrawableTreeInstanceTie* tree,
                 const std::string& debug_name,
                 const std::vector<level_tools::TextureRemap>& tex_map,
                 const TextureDB& tex_db,
                 tfrag3::Level& out,
                 bool dump_level,
                 GameVersion version) {
  for (int geo = 0; geo < GEOM_MAX; ++geo) {
    // as far as I can tell, this one has bad colors
    if (debug_name == "PRECD.DGO-2-tie" && geo == 3) {
      continue;
    }
    tfrag3::TieTree this_tree;

    // sanity check the vis tree (not a perfect check, but this is used in game and should be right)
    ASSERT(tree->length == (int)tree->arrays.size());
    ASSERT(tree->length > 0);
    auto last_array = tree->arrays.back().get();
    auto as_instance_array = dynamic_cast<level_tools::DrawableInlineArrayInstanceTie*>(last_array);
    ASSERT(as_instance_array);
    ASSERT(as_instance_array->length == (int)as_instance_array->instances.size());
    ASSERT(as_instance_array->length > 0);
    u16 idx = as_instance_array->instances.front().id;
    for (auto& elt : as_instance_array->instances) {
      ASSERT(elt.id == idx);
      idx++;
    }
    bool ok = verify_node_indices(tree);
    ASSERT(ok);

    // extract the vis tree. Note that this extracts the tree only down to the last draw node, a
    // parent of between 1 and 8 instances.
    extract_vis_data(tree, as_instance_array->instances.front().id, this_tree);

    // we use the index of the instance in the instance list as its index. But this is different
    // from its visibility index. This map goes from instance index to the parent node in the vis
    // tree. later, we can use this to remap from instance idx to the visiblity node index.
    std::unordered_map<int, int> instance_parents;
    for (size_t node_idx = 0; node_idx < this_tree.bvh.vis_nodes.size(); node_idx++) {
      const auto& node = this_tree.bvh.vis_nodes[node_idx];
      if (node.flags == 0) {
        for (int i = 0; i < node.num_kids; i++) {
          instance_parents[node.child_id + i] = node_idx;
        }
      }
    }

    // convert level format data to a nicer format
    auto info =
        collect_instance_info(as_instance_array, &tree->prototypes.prototype_array_tie.data, geo);
    update_proto_info(&info, tex_map, tree->prototypes.prototype_array_tie.data, geo, version);
    if (version < GameVersion::Jak2) {
      check_wind_vectors_zero(info, tree->prototypes.wind_vectors);
    }
    // determine draws from VU program
    emulate_tie_prototype_program(info);
    emulate_tie_instance_program(info, version);
    emulate_kicks(info);

    // debug save to .obj
    if (dump_level) {
      auto dir =
          file_util::get_file_path({fmt::format("debug_out/lod{}-tie-{}/", geo, debug_name)});
      file_util::create_dir_if_needed(dir);
      for (auto& proto : info) {
        auto data = debug_dump_proto_to_obj(proto);
        file_util::write_text_file(fmt::format("{}/{}.obj", dir, proto.name), data);
      }

      auto full = dump_full_to_obj(info);
      file_util::write_text_file(fmt::format("{}/ALL.obj", dir), full);
    }

    // create time of day data.
    auto full_palette = make_big_palette(info);

    // create draws
    add_vertices_and_static_draw(this_tree, out, tex_db, info, version);

    // remap vis indices and merge
    for (auto& draw : this_tree.static_draws) {
      for (auto& str : draw.vis_groups) {
        auto it = instance_parents.find(str.vis_idx_in_pc_bvh);
        if (it == instance_parents.end()) {
          str.vis_idx_in_pc_bvh = UINT16_MAX;
        } else {
          str.vis_idx_in_pc_bvh = it->second;
        }
      }
      merge_groups(draw.vis_groups);
    }

    for (auto& draw : this_tree.instanced_wind_draws) {
      for (auto& str : draw.instance_groups) {
        auto it = instance_parents.find(str.vis_idx);
        if (it == instance_parents.end()) {
          str.vis_idx = UINT32_MAX;
        } else {
          str.vis_idx = it->second;
        }
      }

      merge_groups(draw.instance_groups);
    }

    merge_groups(this_tree.packed_vertices.matrix_groups);

    this_tree.colors = pack_big_palette(full_palette);
    out.tie_trees[geo].push_back(std::move(this_tree));
  }
}
}  // namespace decompiler
