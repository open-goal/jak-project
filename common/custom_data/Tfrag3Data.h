#pragma once

// Data format for the tfrag3 renderer.
#include <array>

#include "common/common_types.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"
#include "common/util/Assert.h"
#include "common/util/Serializer.h"

namespace tfrag3 {

// NOTE:
// when updating any data structures in this file:
// - change the TFRAG3_VERSION
// - make sure to update the serialize function
// - if changing any large things (vertices, vis, bvh, colors, textures) update get_memory_usage
// - if adding a new category to the memory usage, update extract_level to print it.

constexpr int TFRAG3_VERSION = 38;

enum MemoryUsageCategory {
  TEXTURE,

  SPECIAL_TEXTURE,

  TIE_DEINST_VIS,
  TIE_DEINST_INDEX,
  TIE_INST_VIS,
  TIE_INST_INDEX,
  TIE_BVH,
  TIE_VERTS,
  TIE_TIME_OF_DAY,
  TIE_WIND_INSTANCE_INFO,

  TIE_CIDX,
  TIE_MATRICES,
  TIE_GRPS,

  TFRAG_VIS,
  TFRAG_INDEX,
  TFRAG_VERTS,
  TFRAG_CLUSTER,
  TFRAG_TIME_OF_DAY,
  TFRAG_BVH,

  SHRUB_TIME_OF_DAY,
  SHRUB_VERT,
  SHRUB_IND,
  SHRUB_DRAW,

  MERC_VERT,
  MERC_INDEX,
  MERC_DRAW,

  MERC_MOD_DRAW_1,
  MERC_MOD_DRAW_2,
  MERC_MOD_VERT,
  MERC_MOD_IND,
  MERC_MOD_TABLE,
  BLERC,

  COLLISION,

  NUM_CATEGORIES
};

struct MemoryUsageTracker {
  u32 data[MemoryUsageCategory::NUM_CATEGORIES];

  MemoryUsageTracker() {
    for (auto& x : data) {
      x = 0;
    }
  }

  void add(MemoryUsageCategory category, u32 size_bytes) { data[category] += size_bytes; }
};

// These vertices should be uploaded to the GPU at load time and don't change
struct PreloadedVertex {
  // the vertex position
  float x = 0, y = 0, z = 0;
  // envmap tint color, not used in == or hash.
  u8 r = 0, g = 0, b = 0, a = 0;
  // texture coordinates
  float s = 0, t = 0;

  // not used in == or hash!!
  // note that this is a 10-bit 3-element field packed into 32-bits.
  u32 nor = 0;

  // color table index
  u16 color_index = 0;

  struct hash {
    std::size_t operator()(const PreloadedVertex& x) const;
  };

  bool operator==(const PreloadedVertex& other) const {
    return x == other.x && y == other.y && z == other.z && s == other.s && t == other.t &&
           color_index == other.color_index;
  }
};
static_assert(sizeof(PreloadedVertex) == 32, "PreloadedVertex size");

struct PackedTieVertices {
  struct Vertex {
    float x, y, z;
    float s, t;
    s8 nx, ny, nz;
    u8 r, g, b, a;
  };

  struct MatrixGroup {
    s32 matrix_idx;
    u32 start_vert;
    u32 end_vert;
    bool has_normals = false;
  };

  std::vector<u16> color_indices;
  std::vector<std::array<math::Vector4f, 4>> matrices;
  std::vector<MatrixGroup> matrix_groups;  // todo pack
  std::vector<Vertex> vertices;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

struct PackedTfragVertices {
  struct Vertex {
    u16 xoff, yoff, zoff;
    u16 cluster_idx;
    s16 s, t;
    u16 color_index;
  };
  void memory_usage(MemoryUsageTracker* tracker) const;
  std::vector<Vertex> vertices;
  std::vector<math::Vector<u16, 3>> cluster_origins;
};

struct ShrubGpuVertex {
  float x, y, z;
  float s, t;
  u32 pad0;
  u16 color_index;
  u16 pad1;
  u8 rgba_base[3];
  u8 pad2;
};
static_assert(sizeof(ShrubGpuVertex) == 32, "ShrubGpuVertex size");

struct PackedShrubVertices {
  struct Vertex {
    float x, y, z;
    float s, t;
    u8 rgba[3];
  };

  struct InstanceGroup {
    s32 matrix_idx;
    u32 start_vert;
    u32 end_vert;
    u16 color_index;
  };
  std::vector<std::array<math::Vector4f, 4>> matrices;
  std::vector<InstanceGroup> instance_groups;  // todo pack
  std::vector<Vertex> vertices;
  u32 total_vertex_count;
  void memory_usage(MemoryUsageTracker* tracker) const;
  void serialize(Serializer& ser);
};

// Settings for drawing a group of triangle strips.
// This refers to a group of PreloadedVertices that are already uploaded.
// All triangles here are drawn in the same "mode" (blending, texture, etc)
// The vertex index list is chunked by visibility group.
// You can just memcpy the entire list to draw everything, or iterate through visgroups and
// check visibility.
struct StripDraw {
  DrawMode mode;        // the OpenGL draw settings.
  s32 tree_tex_id = 0;  // the texture that should be bound for the draw (negative for anim slot)

  struct {
    u32 idx_of_first_idx_in_full_buffer = 0;
  } unpacked;

  // indices can be specified as lists of runs and plain indices.
  // the runs are still drawn with indexed opengl calls, it just uses less space in the file.
  struct VertexRun {
    u32 vertex0;
    u16 length;
  };
  std::vector<VertexRun> runs;
  std::vector<u32> plain_indices;

  // to do culling, the above vertex stream is grouped.
  // by following the visgroups and checking the visibility, you can leave out invisible vertices.
  struct VisGroup {
    u32 num_inds = 0;           // number of vertex indices in this group
    u32 num_tris = 0;           // number of triangles
    u16 vis_idx_in_pc_bvh = 0;  // the visibility group they belong to (in BVH)
    u16 tie_proto_idx = 0;      // index of tie proto (tie only)
  };
  std::vector<VisGroup> vis_groups;

  // for debug counting.
  u32 num_triangles = 0;
  void serialize(Serializer& ser);
};

struct ShrubDraw {
  DrawMode mode;        // the OpenGL draw settings.
  u32 tree_tex_id = 0;  // the texture that should be bound for the draw

  u32 first_index_index;
  u32 num_indices;

  // for debug counting.
  u32 num_triangles = 0;
  void serialize(Serializer& ser);
};

struct InstancedStripDraw {
  DrawMode mode;        // the OpenGL draw settings.
  u32 tree_tex_id = 0;  // the texture that should be bound for the draw

  // the list of vertices in the draw. This includes the restart code of UINT32_MAX that OpenGL
  // will use to start a new strip.
  std::vector<u32> vertex_index_stream;

  // the vertex stream above is segmented by instance.
  struct InstanceGroup {
    u32 num = 0;           // number of vertex indices in this group
    u32 instance_idx = 0;  // the instance they belong to
    u32 vis_idx = 0;
  };
  std::vector<InstanceGroup> instance_groups;

  // for debug counting.
  u32 num_triangles = 0;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

// node in the BVH.
struct VisNode {
  math::Vector<float, 4> bsphere;  // the bounding sphere, in meters (4096 = 1 game meter). w = rad
  u16 child_id = 0xffff;           // the ID of our first child.
  u16 my_id = 0xffff;
  u8 num_kids = 0xff;  // number of children. The children are consecutive in memory
  u8 flags = 0;        // flags.  If 1, we have a DrawVisNode child, otherwise a leaf.
};

// The leaf nodes don't actually exist in the vector of VisNodes, but instead they are ID's used
// by the actual geometry.  Currently we do not include the bspheres of these, but this might be
// worth it if we have a more performant culling algorithm.
struct BVH {
  std::vector<VisNode> vis_nodes;  // bvh for frustum culling
  // additional information about the BVH
  u16 first_leaf_node = 0;
  u16 last_leaf_node = 0;
  u16 first_root = 0;
  u16 num_roots = 0;
  bool only_children = false;
  void serialize(Serializer& ser);
};

// A time-of-day color. Each stores 8 colors. At a given "time of day", they are interpolated
// to find a single color which goes into a color palette.
struct TimeOfDayColor {
  math::Vector<u8, 4> rgba[8];

  bool operator==(const TimeOfDayColor& other) const {
    for (size_t i = 0; i < 8; i++) {
      if (rgba[i] != other.rgba[i]) {
        return false;
      }
    }
    return true;
  }
};

// A single texture. Stored as RGBA8888.
struct Texture {
  u16 w, h;
  u32 combo_id = 0;
  std::vector<u32> data;
  std::string debug_name;
  std::string debug_tpage_name;
  bool load_to_pool = false;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

struct IndexTexture {
  u16 w, h;
  u32 combo_id = 0;
  std::vector<u8> index_data;
  std::vector<std::string> level_names;
  std::string name;
  std::string tpage_name;
  std::array<math::Vector4<u8>, 256> color_table;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

// Tfrag trees have several kinds:
enum class TFragmentTreeKind { NORMAL, TRANS, DIRT, ICE, LOWRES, LOWRES_TRANS, WATER, INVALID };

constexpr const char* tfrag_tree_names[] = {"normal", "trans",        "dirt",  "ice",
                                            "lowres", "lowres-trans", "water", "invalid"};

// A tfrag model
struct TfragTree {
  TFragmentTreeKind kind;        // our tfrag kind
  std::vector<StripDraw> draws;  // the actual topology and settings
  PackedTfragVertices packed_vertices;
  std::vector<TimeOfDayColor> colors;  // vertex colors (pre-interpolation)
  BVH bvh;                             // the bvh for frustum culling
  bool use_strips = true;

  struct {
    std::vector<PreloadedVertex> vertices;  // mesh vertices
    std::vector<u32> indices;
  } unpacked;
  void unpack();
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

struct TieWindInstance {
  std::array<math::Vector4f, 4> matrix;
  u16 wind_idx;
  float stiffness;
  void serialize(Serializer& ser);
};

// Tie draws are split into categories.
enum class TieCategory {
  // normal tie buckets
  NORMAL,
  TRANS,  // also called alpha
  WATER,

  // first draw (normal base draw) for envmapped stuff
  NORMAL_ENVMAP,
  TRANS_ENVMAP,
  WATER_ENVMAP,

  // second draw (shiny) for envmapped ties.
  NORMAL_ENVMAP_SECOND_DRAW,
  TRANS_ENVMAP_SECOND_DRAW,
  WATER_ENVMAP_SECOND_DRAW,
};
constexpr int kNumTieCategories = 9;

constexpr bool is_envmap_first_draw_category(tfrag3::TieCategory category) {
  switch (category) {
    case tfrag3::TieCategory::NORMAL_ENVMAP:
    case tfrag3::TieCategory::WATER_ENVMAP:
    case tfrag3::TieCategory::TRANS_ENVMAP:
      return true;
    default:
      return false;
  }
}

constexpr bool is_envmap_second_draw_category(tfrag3::TieCategory category) {
  switch (category) {
    case tfrag3::TieCategory::NORMAL_ENVMAP_SECOND_DRAW:
    case tfrag3::TieCategory::WATER_ENVMAP_SECOND_DRAW:
    case tfrag3::TieCategory::TRANS_ENVMAP_SECOND_DRAW:
      return true;
    default:
      return false;
  }
}

constexpr TieCategory get_second_draw_category(tfrag3::TieCategory category) {
  switch (category) {
    case TieCategory::NORMAL_ENVMAP:
      return TieCategory::NORMAL_ENVMAP_SECOND_DRAW;
    case TieCategory::TRANS_ENVMAP:
      return TieCategory::TRANS_ENVMAP_SECOND_DRAW;
    case TieCategory::WATER_ENVMAP:
      return TieCategory::WATER_ENVMAP_SECOND_DRAW;
    default:
      return TieCategory::NORMAL_ENVMAP;
  }
}

// A tie model
struct TieTree {
  BVH bvh;
  std::vector<StripDraw> static_draws;
  // Category n uses draws: static_draws[cdi[n]] to static_draws[cdi[n + 1]]
  std::array<u32, kNumTieCategories + 1> category_draw_indices;

  PackedTieVertices packed_vertices;
  std::vector<TimeOfDayColor> colors;  // vertex colors (pre-interpolation)

  std::vector<InstancedStripDraw> instanced_wind_draws;
  std::vector<TieWindInstance> wind_instance_info;

  // jak 2 and later can toggle on and off visibility per proto by name
  bool has_per_proto_visibility_toggle = false;
  std::vector<std::string> proto_names;

  struct {
    std::vector<PreloadedVertex> vertices;  // mesh vertices
    std::vector<u32> indices;
  } unpacked;

  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
  void unpack();
};

struct ShrubTree {
  // todo some visibility structure
  std::vector<TimeOfDayColor> time_of_day_colors;  // multiplier colors

  PackedShrubVertices packed_vertices;
  std::vector<ShrubDraw> static_draws;  // the actual topology and settings
  std::vector<u32> indices;

  struct {
    std::vector<ShrubGpuVertex> vertices;  // mesh vertices
  } unpacked;

  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
  void unpack();
};

struct CollisionMesh {
  struct Vertex {
    float x, y, z;
    u32 flags;
    s16 nx, ny, nz;
    u16 pad;
    u32 pat;
    u32 pad2;
  };
  static_assert(sizeof(Vertex) == 32);
  std::vector<Vertex> vertices;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

// MERC

struct MercVertex {
  alignas(32) float pos[3];
  float pad0;

  float normal[3];
  float pad1;

  float weights[3];
  float pad2;

  float st[2];

  u8 rgba[4];
  u8 mats[3];
  u8 pad3;
};
static_assert(sizeof(MercVertex) == 64);

struct MercDraw {
  DrawMode mode;
  s32 tree_tex_id = 0;  // the texture that should be bound for the draw (negative for anim slot)
  u8 eye_id = 0xff;     // 0xff if not eyes, (slot << 1) | (is_r)
  u32 first_index;
  u32 index_count;
  u32 num_triangles;
  void serialize(Serializer& ser);
};

struct BlercFloatData {
  // [x, y, z, pad, nx, ny, nz, pad]
  // note that this should match the layout of the merc vertex above
  alignas(32) float v[8];
};

/*!
 * Data to modify vertices based on blend shapes.
 */
struct Blerc {
  std::vector<BlercFloatData> float_data;
  std::vector<u32> int_data;
  static constexpr u32 kTargetIdxTerminator = UINT32_MAX;
  void serialize(Serializer& ser);

  // int data, per vertex:
  // [tgt0_idx, tgt1_idx, ..., terminator, dest]
  // float data, per vertex:
  // [base, tgt0, tgt1, ...]

  // final vertex position is:
  // base + sum(tgtn * weights[tgtn_idx])
};

struct MercModifiableDrawGroup {
  std::vector<MercVertex> vertices;
  std::vector<u16> vertex_lump4_addr;
  std::vector<MercDraw> fix_draw, mod_draw;
  std::vector<u8> fragment_mask;
  Blerc blerc;
  u32 expect_vidx_end = 0;

  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

struct MercEffect {
  std::vector<MercDraw> all_draws;
  MercModifiableDrawGroup mod;
  DrawMode envmap_mode;
  u32 envmap_texture;
  bool has_envmap = false;
  bool has_mod_draw = false;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

struct MercModel {
  std::string name;
  std::vector<MercEffect> effects;
  u32 max_draws;
  u32 max_bones;
  u32 st_vif_add;
  float xyz_scale;
  float st_magic;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

struct MercModelGroup {
  std::vector<MercVertex> vertices;
  std::vector<u32> indices;
  std::vector<MercModel> models;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

//

constexpr int TFRAG_GEOS = 3;
constexpr int TIE_GEOS = 4;

struct Level {
  u16 version = TFRAG3_VERSION;
  std::string level_name;
  std::vector<Texture> textures;
  std::vector<IndexTexture> index_textures;
  std::array<std::vector<TfragTree>, TFRAG_GEOS> tfrag_trees;
  std::array<std::vector<TieTree>, TIE_GEOS> tie_trees;
  std::vector<ShrubTree> shrub_trees;
  CollisionMesh collision;
  MercModelGroup merc_data;
  u16 version2 = TFRAG3_VERSION;
  void serialize(Serializer& ser);
  void memory_usage(MemoryUsageTracker* tracker) const;
};

void print_memory_usage(const tfrag3::Level& lev, int uncompressed_data_size);

}  // namespace tfrag3
