#pragma once

#include "common/common_types.h"
#include <string>
#include <memory>
#include <vector>

#include "decompiler/util/goal_data_reader.h"

namespace decompiler {
class LinkedObjectFile;
class DecompilerTypeSystem;
}  // namespace decompiler

namespace level_tools {

struct Vector {
  float data[4];

  void read_from_file(Ref ref);

  std::string print(int indent = 0) const;
  std::string print_meters(int indent = 0) const;
};

struct Matrix4h {
  u16 data[16];
  void read_from_file(Ref ref);
};

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

struct PrintSettings {
  bool print_tfrag = true;
  bool expand_draw_node = true;
  bool expand_drawable_tree_tfrag = true;
  bool expand_drawable_tree_trans_tfrag = true;
  bool expand_drawable_tree_tie_proto = true;
  bool expand_drawable_tree_tie_proto_data = false;
  bool expand_drawable_tree_instance_tie = false;
  bool expand_drawable_tree_actor = false;
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

struct Drawable {
  virtual void read_from_file(TypedRef ref,
                              const decompiler::DecompilerTypeSystem& dts,
                              DrawStats* stats) = 0;
  virtual std::string print(const PrintSettings& settings, int indent) const = 0;
  virtual std::string my_type() const = 0;
  virtual ~Drawable() = default;
};

struct DrawableInlineArray : public Drawable {};

struct DrawNode : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  Vector bsphere;
  u8 child_count = 0;
  u8 flags = 0;
  std::vector<std::unique_ptr<Drawable>> children;
  float distance = 0;
};

struct EntityActor {};

struct DrawableActor : public Drawable {
  s16 id;
  Vector bsphere;

  EntityActor actor;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "drawable-actor"; }
};

struct TFragmentDebugData {
  u16 num_tris[4];
  u16 num_dverts[4];
  bool has_debug_lines;

  std::string print(int indent) const;
  void read_from_file(Ref ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);
};

struct TFragment : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
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

struct TieFragment : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
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

  // todo, lots more
};

struct InstanceTie : public Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "instance-tie"; }

  // (bucket-index uint16           :offset 6)
  u16 bucket_index;
  s16 id;
  Vector bsphere;
  Matrix4h origin;
  u16 flags;
  u16 wind_index;

  Ref color_indices;  // can't read this in the first pass because we don't know how long.

  // todo, lots more
};

struct DrawableInlineArrayNode : public DrawableInlineArray {
  s16 id;
  s16 length;
  Vector bsphere;

  std::vector<DrawNode> draw_nodes;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
};

struct DrawableInlineArrayTFrag : public DrawableInlineArray {
  s16 id;
  s16 length;
  Vector bsphere;

  std::vector<TFragment> tfragments;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
};

struct DrawableInlineArrayInstanceTie : public DrawableInlineArray {
  s16 id;
  s16 length;
  Vector bsphere;

  std::vector<InstanceTie> instances;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
};

struct DrawableInlineArrayTransTFrag : public DrawableInlineArrayTFrag {
  std::string my_type() const override { return "drawable-inline-array-trans-tfrag"; }
};

struct DrawableInlineArrayUnknown : public DrawableInlineArray {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  std::string type_name;
};

struct DrawableTree : public Drawable {};

struct TimeOfDayPalette {
  u32 width;
  u32 height;
  u32 pad;
  std::vector<u32> colors;
};

struct DrawableTreeTfrag : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  s16 length;
  // todo time of day stuff
  TimeOfDayPalette time_of_day;
  Vector bsphere;

  std::vector<std::unique_ptr<DrawableInlineArray>> arrays;
};

struct DrawableTreeActor : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  s16 length;
  // todo time of day stuff
  Vector bsphere;

  std::vector<std::unique_ptr<DrawableInlineArray>> arrays;
};

struct PrototypeTie : public DrawableInlineArray {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  s16 id;
  s16 length;

  Vector bsphere;
  std::vector<TieFragment> tie_fragments;
};

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

  u16 generic_count[4];
  u32 generic_next[4];
  u8 frag_count[4] = {0};
  u8 index_start[4];
  u16 base_qw[4];

  float envmap_rfade;
  float envmap_fade_far;

  float stiffness;

  std::vector<u8> color_index_qwc;

  TimeOfDayPalette time_of_day;

  // todo envmap shader
  // todo collide-frag
  // todo tie-colors
  // todo data

  void read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);
  std::string print(const PrintSettings& settings, int indent) const;
};

struct PrototypeArrayTie {
  u32 length;
  u32 allocated_length;
  std::string content_type;
  std::vector<PrototypeBucketTie> data;

  void read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);
  std::string print(const PrintSettings& settings, int indent) const;
};

struct ProxyPrototypeArrayTie {
  void read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);
  std::string print(const PrintSettings& settings, int indent) const;

  PrototypeArrayTie prototype_array_tie;
  // todo wind vectors.
};

struct DrawableTreeInstanceTie : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;
  s16 length;
  ProxyPrototypeArrayTie prototypes;
  Vector bsphere;

  std::vector<std::unique_ptr<DrawableInlineArray>> arrays;
};

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

struct DrawableTreeUnknown : public DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      DrawStats* stats) override;
  std::string print(const PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  std::string type_name;
};

struct DrawableTreeArray {
  s16 id;
  s16 length;

  void read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts, DrawStats* stats);

  std::string print(const PrintSettings& settings, int indent) const;

  std::vector<std::unique_ptr<DrawableTree>> trees;
};

struct TextureRemap {
  u32 original_texid;
  u32 new_texid;
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
  //  ;; some osrt of texture remapping info
  //      (texture-remap-table (pointer uint64) :offset-assert 52)
  //  (texture-remap-table-len int32 :offset-assert 56)
  std::vector<TextureRemap> texture_remap_table;
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
                      DrawStats* stats);

  std::string print(const PrintSettings& settings) const;
};
}  // namespace level_tools
