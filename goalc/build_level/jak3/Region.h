#pragma once

#include <cmath>
#include <string>

#include "Entity.h"

#include "common/common_types.h"
#include "common/goos/ParseHelpers.h"
#include "common/goos/Printer.h"
#include "common/math/Vector.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

#include "third-party/json.hpp"

namespace jak3 {

struct RegionFaceData {
  // (normal        vector  :inline  :offset-assert 0)
  // (normal-offset float            :offset 12)
  // (num-points    uint32           :offset-assert 16)
  // (points        vector :inline :dynamic :offset-assert 32) ;; guess
  math::Vector4f normal;  // w component is normal offset
  u32 num_points;
  std::vector<math::Vector4f> points;

  size_t generate(DataObjectGenerator& gen);
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
};

void from_json(const json& j, RegionFaceData& obj);

struct RegionFaceArray;

struct Region {
  // (id        uint32  :offset-assert 0)
  // (on-enter  pair    :offset-assert 4)
  // (on-inside pair    :offset-assert 8)
  // (on-exit   pair    :offset-assert 12)
  u32 id;
  std::optional<goos::Object*> on_enter;
  std::optional<goos::Object*> on_inside;
  std::optional<goos::Object*> on_exit;
  math::Vector4f trans;
  math::Vector4f bsphere;
  std::string tree;   // target, camera, data, water, city_vis, sample, light, entity
  std::string shape;  // sphere, face, volume
  std::optional<std::vector<RegionFaceData>> faces;

  size_t slot;
  std::optional<std::map<std::string, size_t>> actor_slots;

  size_t generate(DataObjectGenerator& gen) const;
  void generate_pairs(DataObjectGenerator& gen, const std::vector<size_t>& pair_slots);
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
  std::string print();
};

struct RegionArray {
  // (data             region :inline :dynamic :offset-assert 16)
  std::vector<Region*> data;
  std::map<int, std::vector<size_t>> pair_slots;
  std::map<int, size_t> region_slots;
  std::map<std::string, size_t> entity_actor_slots;
  std::vector<EntityActor>* entities;
  std::vector<ActorGroup>* actor_groups;

  size_t slot;

  size_t generate(DataObjectGenerator& gen);
  size_t add_to_object_file(DataObjectGenerator& gen, size_t region_array) const;
};

struct DrawableRegionPrim {
  // (deftype drawable-region-prim (drawable)
  //  ((region region  :offset 8)
  //   )
  Region* region;

  explicit DrawableRegionPrim(Region* region) { this->region = region; }

  virtual size_t generate(DataObjectGenerator& gen);
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
};

struct DrawableInlineArrayRegionPrim {
  // (deftype drawable-inline-array-region-prim (drawable-inline-array)
  //  ((data drawable-region-prim 1 :inline :offset-assert 32)
  //  )
  std::vector<DrawableRegionPrim*> data;

  size_t generate(DataObjectGenerator& gen) const;
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
};

struct DrawableTreeRegionPrim {
  // (deftype drawable-tree-region-prim (drawable-tree)
  // ((id      int16  :offset-assert 4)
  //  (length  int16  :offset 6)
  //  (name    symbol :offset 8)
  //  (bsphere vector :inline :offset-assert 16)
  //  (data2   drawable-inline-array :dynamic :offset 32 :score 1))
  //  )
  std::string name;
  math::Vector4f bsphere;
  DrawableInlineArrayRegionPrim data2;

  DrawableTreeRegionPrim() = default;
  explicit DrawableTreeRegionPrim(std::string name_) : name(std::move(name_)) {}

  size_t generate(DataObjectGenerator& gen) const;
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
};

size_t generate_drawable_tree_region_prim_array(DataObjectGenerator& gen,
                                                RegionArray& regions,
                                                const std::vector<DrawableTreeRegionPrim>& trees);

void fill_region_trees(std::vector<DrawableTreeRegionPrim>& trees,
                       std::map<int, Region>& regions,
                       RegionArray& region_arr,
                       const nlohmann::json& json,
                       u32 base_id);
void add_regions_from_json(const nlohmann::json& json,
                           DrawableTreeRegionPrim& tree,
                           std::map<int, Region>& regions,
                           u32 base_id);

struct DrawableRegionSphere : DrawableRegionPrim {
  using DrawableRegionPrim::DrawableRegionPrim;
  size_t generate(DataObjectGenerator& gen) override;
};

struct DrawableRegionFace : DrawableRegionPrim {
  // (deftype drawable-region-face (drawable-region-prim)
  //  ((data region-face-data  :offset 12)
  //   )
  RegionFaceData data;
  size_t face_data_slot;
  std::optional<math::Vector4f> bsphere_override;

  explicit DrawableRegionFace(Region* region) : DrawableRegionPrim(region) {
    if (region->faces.has_value()) {
      data = region->faces.value().at(0);
    }
  }
  size_t generate(DataObjectGenerator& gen) override;
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
};

struct RegionFaceArray {
  // (deftype region-face-array (inline-array-class)
  //  ((data drawable-region-face :inline :dynamic :offset 16)
  //   )
  std::vector<DrawableRegionFace> data;
  std::vector<RegionFaceData> faces;

  size_t generate(DataObjectGenerator& gen);
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
};

void from_json(const json& j, RegionFaceArray& obj);

struct DrawableRegionVolume : DrawableRegionPrim {
  // (deftype drawable-region-volume (drawable-region-prim)
  //  ((faces region-face-array  :offset 12)
  //   )
  RegionFaceArray faces;
  size_t face_array_slot;

  explicit DrawableRegionVolume(Region* region) : DrawableRegionPrim(region) {
    if (this->region->faces.has_value()) {
      auto face_arr = this->region->faces.value();
      for (const auto& f : face_arr) {
        auto& face = faces.data.emplace_back(region);
        face.data = f;
        // compute per-face bsphere from the face's points for volumes
        if (!f.points.empty()) {
          math::Vector4f center = {0, 0, 0, 0};
          for (auto& pt : f.points) {
            center.x() += pt.x();
            center.y() += pt.y();
            center.z() += pt.z();
          }
          auto n = static_cast<float>(f.points.size());
          center.x() /= n;
          center.y() /= n;
          center.z() /= n;
          float max_dist_sq = 0;
          for (auto& pt : f.points) {
            float dx = pt.x() - center.x();
            float dy = pt.y() - center.y();
            float dz = pt.z() - center.z();
            max_dist_sq = std::max(max_dist_sq, dx * dx + dy * dy + dz * dz);
          }
          center.w() = std::sqrt(max_dist_sq);
          face.bsphere_override = center;
        }
      }
    }
  }

  size_t generate(DataObjectGenerator& gen) override;
  size_t add_to_object_file(DataObjectGenerator& gen, size_t);
};
}  // namespace jak3