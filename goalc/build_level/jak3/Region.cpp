#include "Region.h"

#include "Entity.h"

#include "common/log/log.h"

namespace jak3 {

std::optional<std::map<std::string, size_t>> g_entity_slots;
std::optional<std::map<int, size_t>> g_actor_group_slots;

std::map<std::string, size_t> make_entity_slot_map(std::vector<EntityActor>* actors) {
  std::map<std::string, size_t> result{};
  if (actors) {
    for (const auto& e : *actors) {
      result.emplace(e.name, e.slot);
    }
  }
  return result;
}

std::map<int, size_t> make_actor_group_slot_list(std::vector<ActorGroup>* actor_groups) {
  std::map<int, size_t> result{};
  if (actor_groups) {
    for (const auto& e : *actor_groups) {
      result.emplace(e.id, e.slot);
    }
  }
  return result;
}

size_t gen_pair(DataObjectGenerator& gen, const goos::Object& pair) {
  return gen.add_pair(pair, g_entity_slots, g_actor_group_slots);
}

void Region::generate_pairs(DataObjectGenerator& gen, const std::vector<size_t>& pair_slots) {
  auto on_enter_slot = pair_slots[0];
  auto on_inside_slot = pair_slots[1];
  auto on_exit_slot = pair_slots[2];
  size_t on_enter_byte = 0;
  size_t on_inside_byte = 0;
  size_t on_exit_byte = 0;
  if (on_enter.has_value()) {
    on_enter_byte = gen_pair(gen, *on_enter.value());
  } else {
    gen.link_word_to_symbol("#f", on_enter_slot);
  }
  if (on_inside.has_value()) {
    on_inside_byte = gen_pair(gen, *on_inside.value());
  } else {
    gen.link_word_to_symbol("#f", on_inside_slot);
  }
  if (on_exit.has_value()) {
    on_exit_byte = gen_pair(gen, *on_exit.value());
  } else {
    gen.link_word_to_symbol("#f", on_exit_slot);
  }
  if (on_enter.has_value()) {
    gen.link_word_to_byte(on_enter_slot, on_enter_byte);
  }
  if (on_inside.has_value()) {
    gen.link_word_to_byte(on_inside_slot, on_inside_byte);
  }
  if (on_exit.has_value()) {
    gen.link_word_to_byte(on_exit_slot, on_exit_byte);
  }
}

size_t Region::generate(DataObjectGenerator& gen) const {
  auto region_slot = gen.add_word(id);
  auto on_enter_slot = gen.add_word(0);
  auto on_inside_slot = gen.add_word(0);
  auto on_exit_slot = gen.add_word(0);
  size_t on_enter_byte = 0;
  size_t on_inside_byte = 0;
  size_t on_exit_byte = 0;
  if (on_enter.has_value()) {
    on_enter_byte = gen_pair(gen, *on_enter.value());
  } else {
    gen.link_word_to_symbol("#f", on_enter_slot);
  }
  if (on_inside.has_value()) {
    on_inside_byte = gen_pair(gen, *on_inside.value());
  } else {
    gen.link_word_to_symbol("#f", on_inside_slot);
  }
  if (on_exit.has_value()) {
    on_exit_byte = gen_pair(gen, *on_exit.value());
  } else {
    gen.link_word_to_symbol("#f", on_exit_slot);
  }
  if (on_enter.has_value()) {
    gen.link_word_to_byte(on_enter_slot, on_enter_byte);
  }
  if (on_inside.has_value()) {
    gen.link_word_to_byte(on_inside_slot, on_inside_byte);
  }
  if (on_exit.has_value()) {
    gen.link_word_to_byte(on_exit_slot, on_exit_byte);
  }
  return region_slot;
}

size_t RegionArray::generate(DataObjectGenerator& gen) {
  g_entity_slots = make_entity_slot_map(entities);
  g_actor_group_slots = make_actor_group_slot_list(actor_groups);
  gen.align_to_basic();
  gen.add_type_tag("region-array");
  size_t result = gen.current_offset_bytes();
  gen.add_word(data.size());  // 4 (length)
  gen.add_word(data.size());  // 8 (allocated-length)
  gen.add_word(0);            // 12
  ASSERT((gen.current_offset_bytes() % 16) == 0);
  for (auto& region : data) {
    std::vector<size_t> pairs;
    region->slot = gen.add_word(region->id);
    pairs.push_back(gen.add_word(0));
    pairs.push_back(gen.add_word(0));
    pairs.push_back(gen.add_word(0));
    pair_slots.emplace(region->id, pairs);
    region_slots.emplace(region->id, region->slot);
    // region->slot = region->generate(gen);
  }
  for (auto& region : data) {
    auto pairs = pair_slots.find(region->id);
    if (pairs != pair_slots.end()) {
      region->generate_pairs(gen, pairs->second);
    }
  }
  return result;
}

size_t generate_drawable_tree_region_prim_array(DataObjectGenerator& gen,
                                                RegionArray& regions,
                                                const std::vector<DrawableTreeRegionPrim>& trees) {
  gen.align_to_basic();
  gen.add_type_tag("array");
  size_t result = gen.current_offset_bytes();
  auto length = trees.size();
  std::vector<size_t> tree_slots;
  gen.add_word(length);
  gen.add_word(length);
  gen.add_type_tag("drawable-tree-region-prim");
  tree_slots.reserve(trees.size());
  for (auto& tree : trees) {
    tree_slots.push_back(gen.add_word(0));
  }
  gen.align(4);
  regions.slot = regions.generate(gen);
  for (int i = 0; i < trees.size(); i++) {
    gen.link_word_to_byte(tree_slots[i], trees[i].generate(gen));
  }
  return result;
}

size_t DrawableTreeRegionPrim::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("drawable-tree-region-prim");
  size_t result = gen.current_offset_bytes();
  gen.add_word(1 << 16);      // 4, 6 (length)
  gen.add_symbol_link(name);  // 8
  gen.add_word(0);
  gen.add_word_float(bsphere.x());
  gen.add_word_float(bsphere.y());
  gen.add_word_float(bsphere.z());
  gen.add_word_float(bsphere.w());
  size_t arr_slot = gen.add_word(0);
  gen.align(4);
  gen.link_word_to_byte(arr_slot, data2.generate(gen));
  return result;
}

size_t DrawableInlineArrayRegionPrim::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("drawable-inline-array-region-prim");
  size_t result = gen.current_offset_bytes();
  gen.add_word(data.size() << 16);  // 4, 6 (length)
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  for (auto& prim : data) {
    prim->generate(gen);
  }
  // second pass to fill in face and volume references
  for (auto& prim : data) {
    if (dynamic_cast<DrawableRegionFace*>(prim)) {
      auto face = dynamic_cast<DrawableRegionFace*>(prim);
      gen.link_word_to_byte(face->face_data_slot, face->data.generate(gen));
    }
    if (dynamic_cast<DrawableRegionVolume*>(prim)) {
      auto vol = dynamic_cast<DrawableRegionVolume*>(prim);
      gen.link_word_to_byte(vol->face_array_slot, vol->faces.generate(gen));
    }
  }
  return result;
}

void fill_region_trees(std::vector<DrawableTreeRegionPrim>& trees,
                       std::map<int, Region>& regions,
                       RegionArray& region_arr,
                       const nlohmann::json& json,
                       u32 base_id) {
  for (const auto& [k, tree_json] : json.items()) {
    ASSERT_MSG(tree_json.is_object(), fmt::format("key \"{}\" is not an object.", k));
    if (tree_json.is_object() && !tree_json.empty()) {
      auto& tree = trees.emplace_back(k);
      tree.bsphere = vectorm4_from_json(tree_json.at("bsphere"));
      add_regions_from_json(tree_json.at("regions"), tree, regions, base_id);
      for (auto& [id, region] : regions) {
        if (region.tree == tree.name) {
          DrawableRegionPrim* prim = nullptr;
          if (region.shape == "sphere") {
            prim = new DrawableRegionSphere(&region);
            tree.data2.data.push_back(prim);
          } else if (region.shape == "face") {
            prim = new DrawableRegionFace(&region);
            tree.data2.data.push_back(prim);
          } else if (region.shape == "volume") {
            prim = new DrawableRegionVolume(&region);
            tree.data2.data.push_back(prim);
          } else {
            ASSERT_MSG(false, fmt::format("Invalid region shape \"{}\"", region.shape));
          }
        }
      }
    }
  }
  for (auto& [id, region] : regions) {
    region_arr.data.push_back(&region);
  }
}

void add_regions_from_json(const nlohmann::json& json,
                           DrawableTreeRegionPrim& tree,
                           std::map<int, Region>& regions,
                           u32 base_id) {
  std::vector<Region> result;
  auto& reader = pretty_print::get_pretty_printer_reader();
  for (const auto& region_json : json) {
    auto id = region_json.value("id", base_id + regions.size());
    auto p = regions.emplace(id, Region());
    if (p.second) {
      auto& region = p.first->second;
      result.push_back(region);
      region.id = region_json.value("id", base_id + regions.size());
      region.tree = tree.name;
      region.shape = region_json.at("shape").get<std::string>();
      region.bsphere = vectorm4_from_json(region_json.at("bsphere"));
      ASSERT_MSG(region.shape == "sphere" || region.shape == "face" || region.shape == "volume",
                 "Region must have shape 'sphere', 'face' or 'volume'.");
      if (region.shape == "sphere") {
        region.faces = std::nullopt;
      }
      if (region.shape == "face") {
        ASSERT_MSG(region_json.find("face") != region_json.end(),
                   "Face region must have 'face' key.");
        auto face = region_json.at("face").get<RegionFaceData>();
        std::vector<RegionFaceData> faces;
        faces.push_back(face);
        region.faces = std::make_optional<std::vector<RegionFaceData>>(faces);
      }
      if (region.shape == "volume") {
        ASSERT_MSG(region_json.find("volume") != region_json.end(),
                   "Volume region must have 'volume' key.");
        auto arr = region_json.at("volume").get<RegionFaceArray>();
        region.faces = std::make_optional<std::vector<RegionFaceData>>(arr.faces);
      }
      if (region_json.find("on-enter") != region_json.end()) {
        region.on_enter = std::make_optional(
            &reader.read_from_string(region_json.at("on-enter").get<std::string>(), false)
                 .as_pair()
                 ->car);
      }
      if (region_json.find("on-inside") != region_json.end()) {
        region.on_inside = std::make_optional(
            &reader.read_from_string(region_json.at("on-inside").get<std::string>(), false)
                 .as_pair()
                 ->car);
      }
      if (region_json.find("on-exit") != region_json.end()) {
        region.on_exit = std::make_optional(
            &reader.read_from_string(region_json.at("on-exit").get<std::string>(), false)
                 .as_pair()
                 ->car);
      }
      lg::print(region.print());
    } else {
      lg::warn("Duplicate region with ID {} in tree {}, skipped", p.first->first, tree.name);
    }
  }
}

std::string Region::print() {
  std::string result;
  result += fmt::format("Region {} ({}):\n", id, tree);
  result += fmt::format("  shape: {}\n", shape);
  if (on_enter.has_value()) {
    result += fmt::format("  on-enter: {}\n", on_enter.value()->print());
  } else {
    result += fmt::format("  on-enter: #f\n");
  }
  if (on_inside.has_value()) {
    result += fmt::format("  on-inside: {}\n", on_inside.value()->print());
  } else {
    result += fmt::format("  on-inside: #f\n");
  }
  if (on_exit.has_value()) {
    result += fmt::format("  on-exit: {}\n", on_exit.value()->print());
  } else {
    result += fmt::format("  on-exit: #f\n");
  }
  return result;
}

void from_json(const nlohmann::json& json, RegionFaceData& obj) {
  obj.normal = vector_from_json(json.at("normal"));
  obj.normal.w() *= METER_LENGTH;
  std::vector<math::Vector4f> points;
  for (auto& p : json.at("points")) {
    points.push_back(vectorm3_from_json(p));
  }
  obj.num_points = points.size();
  obj.points = points;
}

void from_json(const nlohmann::json& json, RegionFaceArray& obj) {
  (void)obj;
  for (const auto& face_json : json.at("faces")) {
    RegionFaceData face;
    from_json(face_json, face);
    obj.faces.push_back(face);
  }
}

size_t RegionFaceData::generate(DataObjectGenerator& gen) {
  size_t result = gen.current_offset_bytes();
  gen.add_word_float(normal.x());
  gen.add_word_float(normal.y());
  gen.add_word_float(normal.z());
  gen.add_word_float(normal.w());
  gen.add_word(num_points);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  for (auto& p : points) {
    gen.add_word_float(p.x());
    gen.add_word_float(p.y());
    gen.add_word_float(p.z());
    gen.add_word_float(p.w());
  }
  return result;
}

size_t RegionFaceArray::generate(DataObjectGenerator& gen) {
  gen.align_to_basic();
  gen.add_type_tag("region-face-array");
  size_t result = gen.current_offset_bytes();
  auto length = data.size();
  gen.add_word(length);
  gen.add_word(length);
  gen.add_word(0);
  for (auto& face : data) {
    face.generate(gen);
  }
  for (int i = 0; i < data.size(); i++) {
    auto face = data[i];
    auto face0 = face.data;
    gen.link_word_to_byte(face.face_data_slot, face0.generate(gen));
  }
  return result;
}

size_t DrawableRegionPrim::generate(DataObjectGenerator& gen) {
  return 0;
}

size_t DrawableRegionSphere::generate(DataObjectGenerator& gen) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-region-sphere");
  size_t result = gen.current_offset_bytes();
  gen.add_word(region->id);
  gen.link_word_to_word(gen.add_word(0), region->slot);
  gen.add_word(0);
  gen.add_word_float(region->bsphere.x());
  gen.add_word_float(region->bsphere.y());
  gen.add_word_float(region->bsphere.z());
  gen.add_word_float(region->bsphere.w());
  return result;
}

size_t DrawableRegionVolume::generate(DataObjectGenerator& gen
                                      /*size_t region_face_array_slot*/) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-region-volume");
  size_t result = gen.current_offset_bytes();
  gen.add_word(region->id);
  gen.link_word_to_word(gen.add_word(0), region->slot);
  face_array_slot = gen.add_word(0);
  gen.add_word_float(region->bsphere.x());
  gen.add_word_float(region->bsphere.y());
  gen.add_word_float(region->bsphere.z());
  gen.add_word_float(region->bsphere.w());
  return result;
}

size_t DrawableRegionFace::generate(DataObjectGenerator& gen) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-region-face");
  size_t result = gen.current_offset_bytes();
  gen.add_word(region->id);
  gen.link_word_to_word(gen.add_word(0), region->slot);
  face_data_slot = gen.add_word(0);
  const auto& bsphere = bsphere_override.has_value() ? bsphere_override.value() : region->bsphere;
  gen.add_word_float(bsphere.x());
  gen.add_word_float(bsphere.y());
  gen.add_word_float(bsphere.z());
  gen.add_word_float(bsphere.w());
  return result;
}
}  // namespace jak3
