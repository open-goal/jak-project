#pragma once

// Data format for the tfrag3 renderer.

#include "common/common_types.h"
#include "common/util/assert.h"
#include "common/dma/gs.h"
#include "common/util/Serializer.h"
#include "common/math/Vector.h"

namespace tfrag3 {

constexpr int TFRAG3_VERSION = 6;

// These vertices should be uploaded to the GPU at load time and don't change
struct PreloadedVertex {
  // the vertex position
  float x, y, z;
  // texture coordinates
  float s, t, q;
  // currently unused, color table indices.
  u16 color_index;
  u16 pad[3];
};
static_assert(sizeof(PreloadedVertex) == 32, "PreloadedVertex size");

// Settings for an OpenGL draw
struct Draw {
  DrawMode mode;        // the OpenGL draw settings.
  u32 tree_tex_id = 0;  // the texture that should be bound for the draw

  // the list of vertices in the draw.
  std::vector<u32> vertex_index_stream;

  // to do culling, the above vertex stream is grouped.
  // by following the visgroups and checking the visibility of the tfrag_idx, you can leave out
  // invisible vertices.
  struct VisGroup {
    u32 num = 0;
    u32 tfrag_idx = 0;
  };
  std::vector<VisGroup> vis_groups;
  u32 num_triangles = 0;

  void serialize(Serializer& ser);
};

struct VisNode {
  math::Vector<float, 4> bsphere;  // the bounding sphere, in meters (4096 = 1 game meter). w = rad
  u16 child_id = 0xffff;           // the ID of our first child.
  u8 num_kids = 0xff;              // number of children. The children are consecutive in memory
  u8 flags = 0;                    // flags.  If 1, we have a DrawVisNode child, otherwise a Tfrag.
};

enum class TFragmentTreeKind { NORMAL, TRANS, DIRT, ICE, LOWRES, LOWRES_TRANS, INVALID };

constexpr const char* tfrag_tree_names[] = {"normal", "trans",        "dirt",   "ice",
                                            "lowres", "lowres-trans", "invalid"};

struct TimeOfDayColor {
  math::Vector<u8, 4> rgba[8];
};

struct Tree {
  TFragmentTreeKind kind;
  std::vector<Draw> draws;
  std::vector<u16> color_indices_per_vertex;
  std::vector<VisNode> vis_nodes;
  std::vector<PreloadedVertex> vertices;
  std::vector<TimeOfDayColor> colors;
  u16 first_leaf_node = 0;
  u16 last_leaf_node = 0;
  u16 first_root = 0;
  u16 num_roots = 0;
  bool only_children = false;

  void serialize(Serializer& ser);
};

struct Texture {
  u16 w, h;
  u32 combo_id = 0;
  std::vector<u32> data;
  std::string debug_name;
  std::string debug_tpage_name;

  void serialize(Serializer& ser);
};

struct Level {
  u16 version = TFRAG3_VERSION;
  std::string level_name;
  std::vector<Texture> textures;
  std::vector<Tree> trees;
  u16 version2 = TFRAG3_VERSION;
  void serialize(Serializer& ser);
};

}  // namespace tfrag3