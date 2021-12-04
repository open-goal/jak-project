#include "Tfrag3Data.h"
#include "common/util/assert.h"

namespace tfrag3 {

void Draw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_pod_vector(&vertex_index_stream);
  ser.from_pod_vector(&vis_groups);
  ser.from_ptr(&num_triangles);
}

void Tree::serialize(Serializer& ser) {
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
  ser.from_pod_vector(&color_indices_per_vertex);
  ser.from_pod_vector(&vis_nodes);
  ser.from_pod_vector(&colors);
  ser.from_ptr(&first_leaf_node);
  ser.from_ptr(&last_leaf_node);
  ser.from_ptr(&first_root);
  ser.from_ptr(&num_roots);
  ser.from_ptr(&only_children);
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
    assert(false);
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
    ser.save<size_t>(trees.size());
  } else {
    trees.resize(ser.load<size_t>());
  }
  for (auto& tree : trees) {
    tree.serialize(ser);
  }

  ser.from_ptr(&version2);
  if (ser.is_loading() && version2 != TFRAG3_VERSION) {
    fmt::print("version mismatch when loading tfrag3 data (at end). Got {}, expected {}\n",
               version2, TFRAG3_VERSION);
    assert(false);
  }
}

}  // namespace tfrag3
