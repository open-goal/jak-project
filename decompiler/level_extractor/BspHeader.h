#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/math/Vector.h"
#include "common/versions/versions.h"

#include "decompiler/level_extractor/common_formats.h"
#include "decompiler/util/goal_data_reader.h"

namespace decompiler {
class LinkedObjectFile;
class DecompilerTypeSystem;
}  // namespace decompiler

namespace level_tools {

struct PrintSettings {
  bool print_tfrag = false;
  bool expand_draw_node = false;
  bool expand_drawable_tree_tfrag = false;
  bool expand_drawable_tree_trans_tfrag = false;
  bool expand_drawable_tree_tie_proto = false;
  bool expand_drawable_tree_tie_proto_data = false;
  bool expand_drawable_tree_instance_tie = false;
  bool expand_drawable_tree_actor = false;
  bool expand_shrub = false;
  bool expand_collide = false;
};

struct DrawStats {
  int total_tfrag_tris = 0;
  int total_tie_prototype_tris = 0;
  int total_actors = 0;
  int total_tie_instances = 0;
  int total_tfragments = 0;

  bool debug_print_dma_data = false;

  std::string print() const;
};

/////////////////////
// Common Types
/////////////////////

// a normal vector of 4 floats.
struct Vector {
  float data[4];

  void read_from_file(Ref ref);

  std::string print(int indent = 0) const;
  std::string print_meters(int indent = 0) const;
  std::string print_decimal(int indent = 0) const;
};

// a matrix with 16-bit integers.
// typically requires some unpacking step to get meaningful values.
struct Matrix4h {
  u16 data[16];
  void read_from_file(Ref ref);
};

// A time-of-day color palette.
// this is just the raw data, doesn't have any unpacking/meaning.
struct TimeOfDayPalette {
  u32 width;
  u32 height;
  u32 pad;
  std::vector<u32> colors;
};

/////////////////////
// Drawable BVH
/////////////////////

// these types are all generic container types/base classes.
// note that the unpacker just greedily chases things and doesn't deduplicate, so comparing
// pointers for equality won't tell you if two things are the same game object or not.

// the base class for everything in the BVH tree system.
struct Drawable {
  virtual void read_from_file(TypedRef ref,
                              const decompiler::DecompilerTypeSystem& dts,
                              DrawStats* stats,
                              GameVersion version) = 0;
  virtual std::string print(const PrintSettings& settings, int indent) const = 0;
  virtual std::string my_type() const = 0;
  virtual ~Drawable() = default;
};

// a node in the BVH tree. It's just used for organizing things within a bsphere.
// these nodes are always in inline arrays and have between 0 and 8 children.
// the leaves of these nodes are some other type of drawable - the thing you actually want to draw.
struct DrawNode : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  Vector bsphere;
  u8 child_count = 0;
  u8 flags = 0;
  std::vector<std::unique_ptr<Drawable>> children;
  float distance = 0;
};

// an inline array of drawable. There are more specific types for the actual arrays.
struct DrawableInlineArray : public Drawable {};

// an inline array of draw nodes. All draw nodes at a level are stored in a drawable inline array.
struct DrawableInlineArrayNode : public DrawableInlineArray {
  s16 id;
  s16 length;
  Vector bsphere;

  std::vector<DrawNode> draw_nodes;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
};

// the generic base class for a "tree". A tree is a BVH with between 1 and 8 DrawNode roots.
// there's typically a tree per renderer.
struct DrawableTree : public Drawable {};

// how we represent drawable trees that we don't support yet - this isn't a real type in the game.
struct DrawableTreeUnknown : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  std::string type_name;
};

struct DrawableInlineArrayUnknown : public DrawableInlineArray {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  std::string type_name;
};

/////////////////////
// Actors
/////////////////////

// there's a tree for actors - but we don't do anything with it yet.

struct EntityActor {};

struct DrawableActor : public Drawable {
  s16 id;
  Vector bsphere;

  EntityActor actor;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "drawable-actor"; }
};

struct DrawableTreeActor : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  s16 length;
  // todo time of day stuff
  Vector bsphere;

  std::vector<std::unique_ptr<DrawableInlineArray>> arrays;
};

/////////////////////
// Collision
/////////////////////

struct CollideFragMesh {
  /*
  ((packed-data     uint32         :offset-assert 4)
   (pat-array       uint32         :offset-assert 8)
   (strip-data-len  uint16         :offset-assert 12)
   (poly-count      uint16         :offset-assert 14)
   (base-trans      vector :inline :offset-assert 16)
   ;; these go in the w of the vector above.
   (vertex-count    uint8          :offset 28)
   (vertex-data-qwc uint8          :offset 29)
   (total-qwc       uint8          :offset 30)
   (unused          uint8          :offset 31)
   )
   */
  void read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);
  std::string print(const PrintSettings& settings, int indent) const;

  u16 strip_data_len;
  u16 poly_count;
  // appears to be integers...
  Vector base_trans;
  u8 vertex_count;
  u8 vertex_data_qwc;
  u8 total_qwc;

  Ref packed_data;
  Ref pat_array;
};

struct CollideFragment {
  void read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);
  std::string print(const PrintSettings& settings, int indent) const;
  Vector bsphere;
  CollideFragMesh mesh;
};

struct DrawableInlineArrayCollideFragment : public DrawableInlineArray {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  std::vector<CollideFragment> collide_fragments;
  s16 id;
  s16 length;
  Vector bsphere;
};

struct DrawableTreeCollideFragment : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  DrawableInlineArrayCollideFragment last_array;
};

/////////////////////
// TFRAG
/////////////////////

struct TFragmentDebugData {
  u16 num_tris[4];
  u16 num_dverts[4];
  bool has_debug_lines;

  std::string print(int indent) const;
  void read_from_file(Ref ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);
};

// the "fragment" is just a collection of data that fits into the VU memory.
struct TFragment : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "tfragment"; }

  // 0 - 4 type tag

  s16 id;           // 4 - 6
  u16 color_index;  // 6 - 8
  TFragmentDebugData debug_data;
  // u32 color_indices;  // 12 - 16 (or colors?)
  Vector bsphere;  // 16 - 32
  // dma common/level0 // 32 - 36
  std::vector<u8> dma_common_and_level0;
  // dma base // 36 - 40
  std::vector<u8> dma_base;
  // dma level 1 // 40 - 44
  std::vector<u8> dma_level1;
  std::vector<u16> color_indices;
  u8 dma_qwc[4];
  // shader // 48 - 52
  u8 num_shaders;        // 52
  u8 num_base_colors;    // 53
  u8 num_level0_colors;  // 54
  u8 num_level1_colors;  // 55
  u8 color_offset;       // 56
  u8 color_count;        // 57
  // u8 pad0;               // 58
  // u8 pad1;               // 59
  //  generic // 60 - 64
};

// array of tfragments. This is used as part of the BVH tree
struct DrawableInlineArrayTFrag : public DrawableInlineArray {
  s16 id;
  s16 length;
  Vector bsphere;

  std::vector<TFragment> tfragments;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
};

// a top-level tfragment tree.
// it has a BVH tree of tfragments as well as a time of day palette.
struct DrawableTreeTfrag : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  s16 length;
  TimeOfDayPalette time_of_day;
  Vector bsphere;

  std::vector<std::unique_ptr<DrawableInlineArray>> arrays;
};

// various specializations of tfragment.

struct DrawableTreeTransTfrag : public DrawableTreeTfrag {
  std::string my_type() const override { return "drawable-tree-trans-tfrag"; }
};

struct DrawableTreeLowresTfrag : public DrawableTreeTfrag {
  std::string my_type() const override { return "drawable-tree-lowres-tfrag"; }
};

struct DrawableTreeDirtTfrag : public DrawableTreeTfrag {
  std::string my_type() const override { return "drawable-tree-dirt-tfrag"; }
};

struct DrawableTreeIceTfrag : public DrawableTreeTfrag {
  std::string my_type() const override { return "drawable-tree-ice-tfrag"; }
};

struct DrawableInlineArrayTransTFrag : public DrawableInlineArrayTFrag {
  std::string my_type() const override { return "drawable-inline-array-trans-tfrag"; }
};

struct DrawableInlineArrayTFragTrans : public DrawableInlineArrayTFrag {
  std::string my_type() const override { return "drawable-inline-array-tfrag-trans"; }
};

struct DrawableInlineArrayTFragWater : public DrawableInlineArrayTFrag {
  std::string my_type() const override { return "drawable-inline-array-tfrag-water"; }
};

struct DrawableTreeTfragTrans : public DrawableTreeTfrag {
  std::string my_type() const override { return "drawable-tree-tfrag-trans"; }
};

struct DrawableTreeTfragWater : public DrawableTreeTfrag {
  std::string my_type() const override { return "drawable-tree-tfrag-water"; }
};
/////////////////////
// TIE
/////////////////////

// a tie-fragment is a chunk of geometry that can be sent to the VU.
// it's similar to a tfragment, but a bit simpler.
// each prototype is made up of fragments.
struct TieFragment : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "tie-fragment"; }

  Vector bsphere;
  u16 num_tris;
  u16 num_dverts;

  u16 tex_count;
  u16 gif_count;
  u16 vertex_count;  // qwc of vertex data.

  std::vector<u8> gif_data;
  std::vector<u8> point_ref;

  std::string debug_label_name;

  std::vector<s8> normals;  // jak 2

  std::vector<u8> generic_data;  // jak 1

  // todo, lots more
};

// represents an instance of a prototype.
// each instance has its own color data, but the rest of the geometry/texture is shared between
// all instances of the same prototype.
struct InstanceTie : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "instance-tie"; }

  // (bucket-index uint16           :offset 6)
  u16 bucket_index;  // which prototype
  s16 id;
  Vector bsphere;  // where we are located
  Matrix4h origin;
  u16 flags;
  u16 wind_index;

  Ref color_indices;  // can't read this in the first pass because we don't know how long.

  // todo, lots more
};

// a prototype is basically just a collection of fragments
struct PrototypeTie : public DrawableInlineArray {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  s16 id;
  s16 length;

  Vector bsphere;
  std::vector<TieFragment> tie_fragments;
};

// a prototype bucket is a collection of 4 different prototypes (called geometries), one for each
// level of detail. All geometries share the same time of day palette.
// the bucket also refers to the fact that it collect instances during actual rendering.
// Note: collision extraction is only supported in jak 1.
struct PrototypeBucketTie {
  std::string name;  // 4 - 8
  u32 flags;         // 8 - 12
  u16 in_level;      // 12 - 14
  u16 utextures;     // 14 - 16
  PrototypeTie geometry[4];

  Vector dists;
  Vector rdists;
  u32 next[4];
  u16 count[4];

  //  u16 generic_count[4];
  //  u32 generic_next[4];
  u8 frag_count[4] = {0};
  u8 index_start[4];
  u16 base_qw[4];

  float envmap_rfade;
  float envmap_fade_far;

  float stiffness;

  std::vector<u8> color_index_qwc;

  TimeOfDayPalette time_of_day;

  bool has_envmap_shader = false;
  u8 envmap_shader[5 * 16];
  math::Vector<u8, 4> jak2_tint_color;  // jak 2 only
  // todo collide-frag
  DrawableInlineArrayCollideFragment collide_frag;
  // todo tie-colors
  // todo data

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version);
  std::string print(const PrintSettings& settings, int indent) const;
};

// an array of all the prototypes. The prototypes aren't stored in a BVH (it wouldn't make sense -
// they can be located all over the place)- there is just a plain array.
struct PrototypeArrayTie {
  u32 length;
  u32 allocated_length;
  std::string content_type;
  std::vector<PrototypeBucketTie> data;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version);
  std::string print(const PrintSettings& settings, int indent) const;
};

// the reason for this type is somewhat unknown, but it just contains the prototype array and
// wind vectors. The wind vector data is all 0's and is updated as the game runs.
struct ProxyPrototypeArrayTie {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version);
  std::string print(const PrintSettings& settings, int indent) const;

  PrototypeArrayTie prototype_array_tie;
  Ref wind_vectors;
};

// array of instances. These are part of the BVH
struct DrawableInlineArrayInstanceTie : public DrawableInlineArray {
  s16 id;
  s16 length;
  Vector bsphere;

  std::vector<InstanceTie> instances;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
};

// TIE tree
struct DrawableTreeInstanceTie : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  s16 length;
  ProxyPrototypeArrayTie prototypes;
  Vector bsphere;

  std::vector<std::unique_ptr<DrawableInlineArray>> arrays;
};

/////////////////////////////////
// SHRUB
/////////////////////////////////

namespace shrub_types {

struct Shrubbery : public level_tools::Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "shrubbery"; }

  // 4 - textures
  std::vector<u8> textures;

  std::vector<u32> header;
  u8 obj_qwc;  // 12
  u8 vtx_qwc;  // 13
  u8 col_qwc;  // 14
  u8 stq_qwc;  // 15

  std::vector<u8> obj;  // 16
  std::vector<u8> vtx;  // 20
  std::vector<u8> col;  // 24
  std::vector<u8> stq;  // 28
};

struct PrototypeShrubbery : public level_tools::DrawableInlineArray {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "prototype-shrubbery"; }
  s16 id;      // 0
  s16 length;  // 4

  level_tools::Vector bsphere;    // 16
  std::vector<Shrubbery> shrubs;  // 32
};

struct Billboard {};

struct GenericShrubFragment : public level_tools::Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "generic-shrub-fragment"; }

  //
  // (textures (inline-array adgif-shader)         :score 999 :offset 4)
  std::vector<u8> textures;
  // (vtx-cnt  uint32         :score 999 :offset 8)
  u32 vtx_cnt;
  // (cnt-qwc  uint8          :score 999 :offset 12)
  u8 cnt_qwc;
  // (vtx-qwc  uint8          :score 999 :offset 13)
  u8 vtx_qwc;
  // (col-qwc  uint8          :score 999 :offset 14)
  u8 col_qwc;
  // (stq-qwc  uint8          :score 999 :offset 15)
  u8 stq_qwc;
  // (cnt      uint32         :score 999 :offset 16)
  std::vector<u8> cnt;
  // (vtx      uint32         :score 999 :offset 20)
  std::vector<u8> vtx;
  // (col      uint32         :score 999 :offset 24)
  std::vector<u8> col;
  // (stq      uint32         :score 999 :offset 28)
  std::vector<u8> stq;
};

// really a drawable-group, but we'll make it drawable here.
struct PrototypeGenericShrub : public level_tools::Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "prototype-generic-shrub"; }
  s16 length;  // 4

  level_tools::Vector bsphere;               // 16
  std::vector<GenericShrubFragment> shrubs;  // 32
};

struct PrototypeBucketShrub {
  std::string name;  // 4
  u32 flags;         // 8
  u16 in_level;      // 12
  u16 utextures;     // 14

  // PrototypeShrubbery geometry[4];  // 16
  PrototypeGenericShrub generic_geom;  // 0
  PrototypeShrubbery shrubbery_geom;   // 1
  // todo transparent geom
  // todo billboard geom

  level_tools::Vector dists;  // 32
  // - near-plane
  // - near-stiff
  // - mid-plane
  // - far-plane
  level_tools::Vector rdists;  // 48
  // - rlength-near
  // - rlength-stiff
  // - rlength-mid
  // - stiffness

  float stiffness;  // - 60

  // the next/last/count/mod stuff is all 0's.

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version);
  std::string print(const level_tools::PrintSettings& settings, int indent) const;
};

struct PrototypeInlineArrayShrub {
  s16 id;                                  // 4
  s16 length;                              // 6
  level_tools::Vector bsphere;             // 16
  std::vector<PrototypeBucketShrub> data;  // 32

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version);
  std::string print(const level_tools::PrintSettings& settings, int indent) const;
};

struct PrototypeArrayShrubInfo {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version);
  std::string print(const level_tools::PrintSettings& settings, int indent) const;

  PrototypeInlineArrayShrub prototype_inline_array_shrub;
  Ref wind_vectors;
};

struct InstanceShrubbery : public level_tools::Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "instance-shrubbery"; }

  s16 id;                        // 4
  u16 bucket_index;              // 6
  level_tools::Vector bsphere;   // 16
  level_tools::Matrix4h origin;  // 32
  u16 wind_index;                // 62

  // --- instance-shrubbery ---
  u32 color_indices;                // 8 - unlike tie, I think this is just an integer offset
  level_tools::Vector flat_normal;  // 64 (w is flat_hwidth)
};

struct DrawableInlineArrayInstanceShrub : public level_tools::DrawableInlineArray {
  s16 id;
  s16 length;
  level_tools::Vector bsphere;

  std::vector<InstanceShrubbery> instances;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "drawable-inline-array-instance-shrub"; }
};

struct DrawableTreeInstanceShrub : public level_tools::DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats,
                      GameVersion version) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;                        // 0
  s16 length;                    // 4
  PrototypeArrayShrubInfo info;  // 8
  TimeOfDayPalette time_of_day;
  level_tools::Vector bsphere;  // 16

  std::vector<std::unique_ptr<level_tools::DrawableInlineArray>> arrays;

  // annoyingly, the shrub tree only has the top level in the array....
  // so we can't use the above "arrays" like we did in tfrag/tie.
  // luckily, it seems to be possible to figure out the location of the remaining arrays.
  // so we add this field to hold the arrays we found.
  // note that the tree format is not quite the same as tfrag/tie - the 8 child max is no longer
  // true, and the arrays are not truly by depth (the leaves aren't all the same depth)
  // this means there may be multiple arrays of instances, and we'll need to check all of them.
  std::vector<std::unique_ptr<level_tools::DrawableInlineArray>> discovered_arrays;
};

}  // namespace shrub_types

////////////////////////////////
// Main Level Type (bsp-header)
////////////////////////////////

// all the different trees are stored in a drawable-tree-array.
struct DrawableTreeArray {
  s16 id;
  s16 length;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version);

  std::string print(const PrintSettings& settings, int indent) const;

  std::vector<std::unique_ptr<DrawableTree>> trees;
};

// The "file info"
struct FileInfo {
  std::string file_type;
  std::string file_name;
  u32 major_version;
  u32 minor_version;

  std::string maya_file_name;
  std::string tool_debug;
  std::string mdb_file_name;

  void read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts);

  std::string print(int indent = 0) const;
};

struct BspHeader {
  //  (info file-info :offset 4)
  FileInfo file_info;

  // (bsphere vector :inline :offset-assert 16)
  Vector bsphere;

  //  (all-visible-list (pointer uint16) :offset-assert 32)

  //  (visible-list-length int32 :offset-assert 36)
  s32 visible_list_length = -1;

  //  (drawable-trees drawable-tree-array :offset-assert 40)
  DrawableTreeArray drawable_tree_array;

  //  (pat pointer :offset-assert 44)
  //  (pat-length int32 :offset-assert 48)
  //
  //  ;; some sort of texture remapping info
  //      (texture-remap-table (pointer uint64) :offset-assert 52)
  //  (texture-remap-table-len int32 :offset-assert 56)
  std::vector<TextureRemap> texture_remap_table;

  static constexpr int kNumTextureFlags = 10;
  u16 texture_flags[kNumTextureFlags];  // jak 2 only
  //
  //  (texture-ids (pointer texture-id) :offset-assert 60)
  //  (texture-page-count int32 :offset-assert 64)
  //
  //  (unk-zero-0 basic :offset-assert 68)
  //
  //  (name symbol :offset-assert 72)
  //  (nickname symbol :offset-assert 76)
  //  (vis-info level-vis-info 8 :offset-assert 80)
  //  (actors drawable-inline-array-actor :offset-assert 112)
  //  (cameras (array entity-camera) :offset-assert 116)
  //  (nodes (inline-array bsp-node) :offset-assert 120)
  //
  //  (level level :offset-assert 124)
  //  (current-leaf-idx uint16 :offset-assert 128)
  //  (unk-data-2 uint16 9 :offset-assert 130)
  //
  //  (boxes box8s-array :offset-assert 148)
  //  (current-bsp-back-flags uint32 :offset-assert 152)
  //  (ambients drawable-inline-array-ambient :offset-assert 156)
  //  (unk-data-4 float :offset-assert 160)
  //  (unk-data-5 float :offset-assert 164)
  //  (adgifs adgif-shader-array :offset-assert 168)
  //  (actor-birth-order (pointer uint32) :offset-assert 172)
  //  (split-box-indices (pointer uint16) :offset-assert 176)
  //  (unk-data-8 uint32 55 :offset-assert 180)

  void read_from_file(const decompiler::LinkedObjectFile& file,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats,
                      GameVersion version);

  std::string print(const PrintSettings& settings) const;
};
}  // namespace level_tools
