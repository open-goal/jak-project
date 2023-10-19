#pragma once

#include <array>
#include <string>
#include <vector>

#include "common/common_types.h"

#include "goalc/build_level/collide/common/collide_common.h"
#include "goalc/build_level/collide/jak2/collide.h"
#include "goalc/build_level/common/Tfrag.h"
#include "goalc/build_level/jak2/Entity.h"
#include "goalc/build_level/jak2/FileInfo.h"

namespace jak2 {
struct VisibilityString {
  std::vector<u8> bytes;
};

struct DrawableTreeInstanceTie {};

struct DrawableTreeActor {};

struct DrawableTreeInstanceShrub {};

struct DrawableTreeRegionPrim {};

struct DrawableTreeArray {
  std::vector<DrawableTreeTfrag> tfrags;
  std::vector<DrawableTreeInstanceTie> ties;
  std::vector<DrawableTreeActor> actors;  // unused?
  std::vector<DrawableTreeRegionPrim> regions;
  std::vector<DrawableTreeInstanceShrub> shrubs;
  size_t add_to_object_file(DataObjectGenerator& gen) const;
};

struct TextureRemap {};

struct TextureId {};

struct VisInfo {};

struct EntityCamera {};

struct BspNode {};

struct RaceMesh {};

struct LightHash {};

struct EntityNavMesh {};

struct ActorGroup {};

struct RegionTree {};

struct RegionArray {};

struct CityLevelInfo {};

struct TextureMasksArray {};

// This is a place to collect all the data that should go into the bsp-header file.
struct LevelFile {
  //  (info                   file-info                        :offset          4)
  FileInfo info;

  //  (all-visible-list       (pointer uint16)                 :offset-assert  32)
  //  (visible-list-length    int16                            :offset-assert  36)
  // (extra-vis-list-length   int16                            :offset-assert 38)
  VisibilityString all_visibile_list;

  //  (drawable-trees         drawable-tree-array              :offset-assert  40)
  DrawableTreeArray drawable_trees;

  //  (pat                    pointer                          :offset-assert  44)
  //  (pat-length             int32                            :offset-assert  48)
  std::vector<PatSurface> pat;

  //  (texture-remap-table    (pointer uint64)                 :offset-assert  52)
  //  (texture-remap-table-len int32                           :offset-assert  56)
  std::vector<TextureRemap> texture_remap_table;

  //  (texture-ids            (pointer texture-id)             :offset-assert  60)
  //  (texture-page-count     int32                            :offset-assert  64)
  std::vector<TextureId> texture_ids;

  //  (unknown-basic          basic                            :offset-assert  68)
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

  //  (texture-flags          texture-page-flag 10             :offset-assert 130)
  std::vector<u16> texture_flags;

  //  (cam-outside-bsp        uint8                            :offset        152)
  //  (cam-using-back         uint8                            :offset-assert 153)
  //  (cam-box-idx            uint16                           :offset-assert 154)
  // zero

  //  (ambients               symbol                           :offset-assert 156)
  // #t

  //  (subdivide-close        float                            :offset-assert 160)
  float close_subdiv = 0;

  //  (subdivide-far          float                            :offset-assert 164)
  float far_subdiv = 0;

  //  (race-meshes            (array entity-race-mesh)         :offset-assert 168)
  std::vector<RaceMesh> race_meshes;

  //  (actor-birth-order      (pointer uint32)                 :offset-assert 172)
  std::vector<u32> actor_birth_order;

  //  (light-hash             light-hash                       :offset-assert 176)
  LightHash light_hash;
  //  (nav-meshes             (array entity-nav-mesh)          :offset-assert 180)
  std::vector<EntityNavMesh> entity_nav_meshes;
  //  (actor-groups           (array actor-group)              :offset-assert 184)
  std::vector<ActorGroup> actor_groups;
  //  (region-trees           (array drawable-tree-region-prim) :offset-assert 188)
  std::vector<RegionTree> region_trees;
  //  (region-array           region-array                     :offset-assert 192)
  RegionArray region_array;
  //  (collide-hash           collide-hash                     :offset-assert 196)
  CollideHash collide_hash;
  //  (wind-array             uint32                           :offset        200)
  std::vector<u32> wind_array;
  //  (wind-array-length      int32                            :offset        204)
  s32 wind_array_length = 0;
  //  (city-level-info        city-level-info                  :offset        208)
  CityLevelInfo city_level_info;
  //  (vis-spheres            vector-array                     :offset        216)
  //  (vis-spheres-length     uint32                           :offset        248)

  //  (region-tree            drawable-tree-region-prim        :offset        252)
  RegionTree region_tree;
  //  (tfrag-masks            texture-masks-array              :offset-assert 256)
  //  (tfrag-closest          (pointer float)                  :offset-assert 260)
  //  (tfrag-mask-count       uint32                           :offset        260)
  TextureMasksArray tfrag_masks;
  //  (shrub-masks            texture-masks-array              :offset-assert 264)
  //  (shrub-closest          (pointer float)                  :offset-assert 268)
  //  (shrub-mask-count       uint32                           :offset        268)
  TextureMasksArray shrub_masks;
  //  (alpha-masks            texture-masks-array              :offset-assert 272)
  //  (alpha-closest          (pointer float)                  :offset-assert 276)
  //  (alpha-mask-count       uint32                           :offset        276)
  TextureMasksArray alpha_masks;
  //  (water-masks            texture-masks-array              :offset-assert 280)
  //  (water-closest          (pointer float)                  :offset-assert 284)
  //  (water-mask-count       uint32                           :offset        284)
  TextureMasksArray water_masks;
  //  (bsp-scale              vector :inline                   :offset-assert 288)
  //  (bsp-offset             vector :inline                   :offset-assert 304)

  std::vector<u8> save_object_file() const;
};
}  // namespace jak2