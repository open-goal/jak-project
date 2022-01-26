#pragma once

// Data format for the tfrag3 renderer.
#include <array>

#include "common/common_types.h"
#include "common/dma/gs.h"
#include "common/util/Serializer.h"
#include "common/math/Vector.h"
#include "common/util/assert.h"

namespace tfrag3 {

constexpr int TFRAG3_VERSION = 9;

// These vertices should be uploaded to the GPU at load time and don't change
struct PreloadedVertex {
  // the vertex position
  float x, y, z;
  // texture coordinates
  float s, t, q;
  // color table index
  u16 color_index;
  u16 pad[3];
};
static_assert(sizeof(PreloadedVertex) == 32, "PreloadedVertex size");

// Settings for drawing a group of triangle strips.
// This refers to a group of PreloadedVertices that are already uploaded.
// All triangles here are drawn in the same "mode" (blending, texture, etc)
// The vertex index list is chunked by visibility group.
// You can just memcpy the entire list to draw everything, or iterate through visgroups and
// check visibility.
struct StripDraw {
  DrawMode mode;        // the OpenGL draw settings.
  u32 tree_tex_id = 0;  // the texture that should be bound for the draw

  // the list of vertices in the draw. This includes the restart code of UINT32_MAX that OpenGL
  // will use to start a new strip.
  std::vector<u32> vertex_index_stream;

  // to do culling, the above vertex stream is grouped.
  // by following the visgroups and checking the visibility, you can leave out invisible vertices.
  struct VisGroup {
    u32 num = 0;                // number of vertex indices in this group
    u32 vis_idx_in_pc_bvh = 0;  // the visibility group they belong to (in BVH)
  };
  std::vector<VisGroup> vis_groups;

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
  void serialize(Serializer& ser);
};

// Tfrag trees have several kinds:
enum class TFragmentTreeKind { NORMAL, TRANS, DIRT, ICE, LOWRES, LOWRES_TRANS, INVALID };

constexpr const char* tfrag_tree_names[] = {"normal", "trans",        "dirt",   "ice",
                                            "lowres", "lowres-trans", "invalid"};

// A tfrag model
struct TfragTree {
  TFragmentTreeKind kind;                 // our tfrag kind
  std::vector<StripDraw> draws;           // the actual topology and settings
  std::vector<PreloadedVertex> vertices;  // mesh vertices
  std::vector<TimeOfDayColor> colors;     // vertex colors (pre-interpolation)
  BVH bvh;                                // the bvh for frustum culling
  void serialize(Serializer& ser);
};

struct TieWindInstance {
  std::array<math::Vector4f, 4> matrix;
  u16 wind_idx;
  float stiffness;
  void serialize(Serializer& ser);
};

// A tie model
struct TieTree {
  BVH bvh;
  std::vector<StripDraw> static_draws;    // the actual topology and settings
  std::vector<PreloadedVertex> vertices;  // mesh vertices
  std::vector<TimeOfDayColor> colors;     // vertex colors (pre-interpolation)

  std::vector<InstancedStripDraw> instanced_wind_draws;
  std::vector<TieWindInstance> instance_info;

  void serialize(Serializer& ser);
};

struct Level {
  u16 version = TFRAG3_VERSION;
  std::string level_name;
  std::vector<Texture> textures;
  std::vector<TfragTree> tfrag_trees;
  std::vector<TieTree> tie_trees;
  u16 version2 = TFRAG3_VERSION;
  void serialize(Serializer& ser);
};

}  // namespace tfrag3
