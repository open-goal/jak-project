#pragma once

#include <array>
#include <string>
#include <vector>

#include "Entity.h"
#include "FileInfo.h"
#include "ambient.h"

#include "common/common_types.h"

#include "goalc/build_level/collide/common/collide_common.h"
#include "goalc/build_level/collide/jak1/collide_bvh.h"
#include "goalc/build_level/collide/jak1/collide_drawable.h"
#include "goalc/build_level/collide/jak1/collide_pack.h"
#include "goalc/build_level/common/Tfrag.h"
#include "goalc/build_level/common/Tie.h"

namespace jak1 {
struct VisibilityString {
  std::vector<u8> bytes;
};

struct DrawableTreeActor {};

struct DrawableTreeInstanceShrub {};

struct DrawableTreeArray {
  std::vector<DrawableTreeTfrag> tfrags;
  std::vector<DrawableTreeInstanceTie> ties;
  std::vector<DrawableTreeActor> actors;  // unused?
  std::vector<DrawableTreeCollideFragment> collides;
  std::vector<DrawableTreeAmbient> ambients;
  std::vector<DrawableTreeInstanceShrub> shrubs;
  size_t add_to_object_file(DataObjectGenerator& gen) const;
};

struct TexRemap {
  u32 orig_texid;
  u32 new_texid;
};

struct TextureId {};

struct VisInfo {};

struct EntityCamera {};

struct BspNode {};

struct Box8s {};

struct DrawableInlineArrayAmbient {
  std::vector<EntityAmbient> ambients;
};

struct AdgifShaderArray {
  std::vector<AdGifData> adgifs;
};

// This is a place to collect all the data that should go into the bsp-header file.
struct LevelFile {
  //  (info                   file-info                        :offset          4)
  FileInfo info;

  //  (all-visible-list       (pointer uint16)                 :offset-assert  32)
  //  (visible-list-length    int32                            :offset-assert  36)
  VisibilityString all_visibile_list;

  //  (drawable-trees         drawable-tree-array              :offset-assert  40)
  DrawableTreeArray drawable_trees;

  //  (pat                    pointer                          :offset-assert  44)
  //  (pat-length             int32                            :offset-assert  48)
  std::vector<PatSurface> pat;

  //  (texture-remap-table    (pointer uint64)                 :offset-assert  52)
  //  (texture-remap-table-len int32                           :offset-assert  56)
  std::vector<TexRemap> texture_remap_table;

  //  (texture-ids            (pointer texture-id)             :offset-assert  60)
  //  (texture-page-count     int32                            :offset-assert  64)
  std::vector<u32> texture_ids;

  //  (unk-zero-0             basic                            :offset-assert  68)
  //  "misc", seems like it can be zero and is unused.

  //  (name                   symbol                           :offset-assert  72)
  std::string name;  // full name

  //  (nickname               symbol                           :offset-assert  76)
  std::string nickname;  // 3 char name

  //  (vis-info               level-vis-info                8  :offset-assert  80) ;; note: 0 when
  std::array<VisInfo, 8> vis_infos;

  //  (actors                 drawable-inline-array-actor      :offset-assert 112)
  std::vector<EntityActor> actors;

  //  (cameras                (array entity-camera)            :offset-assert 116)
  std::vector<EntityCamera> cameras;

  //  (nodes                  (inline-array bsp-node)          :offset-assert 120)
  std::vector<BspNode> nodes;

  //  (level                  level                            :offset-assert 124)
  // zero

  //  (current-leaf-idx       uint16                           :offset-assert 128)
  // zero

  //  (unk-data-2             uint16                        9  :offset-assert 130)
  // looks like padding plus 4 floats? unused

  //  (boxes                  box8s-array                      :offset-assert 148)
  std::vector<Box8s> boxes;

  //  (current-bsp-back-flags uint32                           :offset-assert 152)
  // zero

  //  (ambients               drawable-inline-array-ambient    :offset-assert 156)
  std::vector<EntityAmbient> ambients;

  //  (unk-data-4             float                            :offset-assert 160)
  float close_subdiv = 0;

  //  (unk-data-5             float                            :offset-assert 164)
  float far_subdiv = 0;

  //  (adgifs                 adgif-shader-array               :offset-assert 168)
  AdgifShaderArray adgifs;

  //  (actor-birth-order      (pointer uint32)                 :offset-assert 172)
  std::vector<u32> actor_birth_order;

  //  (split-box-indices      (pointer uint16)                 :offset-assert 176)
  std::vector<u16> split_box_indices;

  //  (unk-data-8             uint32                        55 :offset-assert 180)

  std::vector<u8> save_object_file() const;
};
}  // namespace jak1