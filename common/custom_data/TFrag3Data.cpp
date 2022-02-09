#include "Tfrag3Data.h"
#include "common/util/Assert.h"

namespace tfrag3 {

void StripDraw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_pod_vector(&vertex_index_stream);
  ser.from_pod_vector(&vis_groups);
  ser.from_ptr(&num_triangles);
}

void InstancedStripDraw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_pod_vector(&vertex_index_stream);
  ser.from_pod_vector(&instance_groups);
  ser.from_ptr(&num_triangles);
}

void TieWindInstance::serialize(Serializer& ser) {
  ser.from_ptr(&matrix);
  ser.from_ptr(&wind_idx);
  ser.from_ptr(&stiffness);
}

void TfragTree::serialize(Serializer& ser) {
  ser.from_ptr(&kind);

  if (ser.is_saving()) {
    ser.save<size_t>(draws.size());
  } else {
    draws.resize(ser.load<size_t>());
  }
  for (auto& draw : draws) {
    draw.serialize(ser);
  }

  ser.from_pod_vector(&vertices);
  ser.from_pod_vector(&colors);
  bvh.serialize(ser);
}

void TieTree::serialize(Serializer& ser) {
  if (ser.is_saving()) {
    ser.save<size_t>(static_draws.size());
  } else {
    static_draws.resize(ser.load<size_t>());
  }
  for (auto& draw : static_draws) {
    draw.serialize(ser);
  }

  if (ser.is_saving()) {
    ser.save<size_t>(instanced_wind_draws.size());
  } else {
    instanced_wind_draws.resize(ser.load<size_t>());
  }
  for (auto& draw : instanced_wind_draws) {
    draw.serialize(ser);
  }

  if (ser.is_saving()) {
    ser.save<size_t>(instance_info.size());
  } else {
    instance_info.resize(ser.load<size_t>());
  }
  for (auto& inst : instance_info) {
    inst.serialize(ser);
  }

  ser.from_pod_vector(&vertices);
  ser.from_pod_vector(&colors);
  bvh.serialize(ser);
}

void BVH::serialize(Serializer& ser) {
  ser.from_ptr(&first_leaf_node);
  ser.from_ptr(&last_leaf_node);
  ser.from_ptr(&first_root);
  ser.from_ptr(&num_roots);
  ser.from_ptr(&only_children);
  ser.from_pod_vector(&vis_nodes);
}

void Texture::serialize(Serializer& ser) {
  ser.from_ptr(&w);
  ser.from_ptr(&h);
  ser.from_pod_vector(&data);
  ser.from_str(&debug_name);
  ser.from_str(&debug_tpage_name);
}

void Level::serialize(Serializer& ser) {
  ser.from_ptr(&version);
  if (ser.is_loading() && version != TFRAG3_VERSION) {
    fmt::print("version mismatch when loading tfrag3 data. Got {}, expected {}\n", version,
               TFRAG3_VERSION);
    ASSERT(false);
  }

  ser.from_str(&level_name);

  if (ser.is_saving()) {
    ser.save<size_t>(textures.size());
  } else {
    textures.resize(ser.load<size_t>());
  }
  for (auto& tex : textures) {
    tex.serialize(ser);
  }

  if (ser.is_saving()) {
    ser.save<size_t>(tfrag_trees.size());
  } else {
    tfrag_trees.resize(ser.load<size_t>());
  }
  for (auto& tree : tfrag_trees) {
    tree.serialize(ser);
  }

  if (ser.is_saving()) {
    ser.save<size_t>(tie_trees.size());
  } else {
    tie_trees.resize(ser.load<size_t>());
  }
  for (auto& tree : tie_trees) {
    tree.serialize(ser);
  }

  ser.from_ptr(&version2);
  if (ser.is_loading() && version2 != TFRAG3_VERSION) {
    fmt::print("version mismatch when loading tfrag3 data (at end). Got {}, expected {}\n",
               version2, TFRAG3_VERSION);
    ASSERT(false);
  }
}

}  // namespace tfrag3
