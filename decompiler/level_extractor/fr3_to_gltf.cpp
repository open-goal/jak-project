#include "fr3_to_gltf.h"

#include "common/custom_data/Tfrag3Data.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

namespace {

/*!
 * Convert fr3 format indices (strip format, with UINT32_MAX as restart) to unstripped tris.
 * Also output a map to translate idx_of_first_idx_in_full_buffer to the unstripped tri buffer.
 */
void unstrip(const std::vector<u32>& stripped_indices,
             std::vector<u32>& unstripped,
             std::vector<u32>& old_to_new_start) {
  old_to_new_start.push_back(0);
  old_to_new_start.push_back(0);
  for (size_t i = 2; i < stripped_indices.size(); i++) {
    u32 a = stripped_indices[i];
    u32 b = stripped_indices[i - 1];
    u32 c = stripped_indices[i - 2];
    old_to_new_start.push_back(unstripped.size());
    if (a == UINT32_MAX || b == UINT32_MAX || c == UINT32_MAX) {
      continue;
    } else {
      unstripped.push_back(a);
      unstripped.push_back(b);
      unstripped.push_back(c);
    }
  }
  old_to_new_start.push_back(unstripped.size());
}

void unstrip_draws(const std::vector<u32>& stripped_indices,
                   std::vector<u32>& unstripped,
                   std::vector<u32>& draw_to_start,
                   std::vector<u32>& draw_to_count,
                   const std::vector<tfrag3::ShrubDraw>& draws) {
  for (auto& draw : draws) {
    draw_to_start.push_back(unstripped.size());

    for (size_t i = 2; i < draw.num_indices; i++) {
      int idx = i + draw.first_index_index;
      u32 a = stripped_indices[idx];
      u32 b = stripped_indices[idx - 1];
      u32 c = stripped_indices[idx - 2];
      if (a == UINT32_MAX || b == UINT32_MAX || c == UINT32_MAX) {
        continue;
      }
      unstripped.push_back(a);
      unstripped.push_back(b);
      unstripped.push_back(c);
    }
    draw_to_count.push_back(unstripped.size() - draw_to_start.back());
  }
}

/*!
 * Set up a buffer for the positions of the given vertices.
 * Return the index of the accessor.
 */
template <typename T>
int make_position_buffer_accessor(const std::vector<T>& vertices, tinygltf::Model& model) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 3 * vertices.size());

  // and fill it
  u8* buffer_ptr = buffer.data.data();
  for (const auto& vtx : vertices) {
    float xyz[3] = {vtx.x / 4096.f, vtx.y / 4096.f, vtx.z / 4096.f};
    memcpy(buffer_ptr, xyz, 3 * sizeof(float));
    buffer_ptr += 3 * sizeof(float);
  }

  // create a view of this buffer
  int buffer_view_idx = (int)model.bufferViews.size();
  auto& buffer_view = model.bufferViews.emplace_back();
  buffer_view.buffer = buffer_idx;
  buffer_view.byteOffset = 0;
  buffer_view.byteLength = buffer.data.size();
  buffer_view.byteStride = 0;  // tightly packed
  buffer_view.target = TINYGLTF_TARGET_ARRAY_BUFFER;

  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;
  accessor.byteOffset = 0;
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  accessor.count = vertices.size();
  accessor.type = TINYGLTF_TYPE_VEC3;

  return accessor_idx;
}

template <typename T>
int make_tex_buffer_accessor(const std::vector<T>& vertices, tinygltf::Model& model, float scale) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 2 * vertices.size());

  // and fill it
  u8* buffer_ptr = buffer.data.data();
  for (const auto& vtx : vertices) {
    float st[2] = {vtx.s * scale, vtx.t * scale};
    memcpy(buffer_ptr, st, 2 * sizeof(float));
    buffer_ptr += 2 * sizeof(float);
  }

  // create a view of this buffer
  int buffer_view_idx = (int)model.bufferViews.size();
  auto& buffer_view = model.bufferViews.emplace_back();
  buffer_view.buffer = buffer_idx;
  buffer_view.byteOffset = 0;
  buffer_view.byteLength = buffer.data.size();
  buffer_view.byteStride = 0;  // tightly packed
  buffer_view.target = TINYGLTF_TARGET_ARRAY_BUFFER;

  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;
  accessor.byteOffset = 0;
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  accessor.count = vertices.size();
  accessor.type = TINYGLTF_TYPE_VEC2;

  return accessor_idx;
}

int make_color_buffer_accessor(const std::vector<tfrag3::PreloadedVertex>& vertices,
                               tinygltf::Model& model,
                               const tfrag3::TfragTree& tfrag_tree,
                               int time_of_day) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 4 * vertices.size());
  std::vector<float> floats;

  for (size_t i = 0; i < vertices.size(); i++) {
    auto& color = tfrag_tree.colors.at(vertices[i].color_index);
    for (int j = 0; j < 3; j++) {
      floats.push_back(((float)color.rgba[time_of_day][j]) / 255.f);
    }
    floats.push_back(1.f);
  }
  memcpy(buffer.data.data(), floats.data(), sizeof(float) * floats.size());

  // create a view of this buffer
  int buffer_view_idx = (int)model.bufferViews.size();
  auto& buffer_view = model.bufferViews.emplace_back();
  buffer_view.buffer = buffer_idx;
  buffer_view.byteOffset = 0;
  buffer_view.byteLength = buffer.data.size();
  buffer_view.byteStride = 0;  // tightly packed
  buffer_view.target = TINYGLTF_TARGET_ARRAY_BUFFER;

  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;
  accessor.byteOffset = 0;
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  accessor.count = vertices.size();
  accessor.type = TINYGLTF_TYPE_VEC4;

  return accessor_idx;
}

int make_color_buffer_accessor(const std::vector<tfrag3::PreloadedVertex>& vertices,
                               tinygltf::Model& model,
                               const tfrag3::TieTree& tie_tree,
                               int time_of_day) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 4 * vertices.size());
  std::vector<float> floats;

  for (size_t i = 0; i < vertices.size(); i++) {
    auto& color = tie_tree.colors.at(vertices[i].color_index);
    for (int j = 0; j < 3; j++) {
      floats.push_back(((float)color.rgba[time_of_day][j]) / 255.f);
    }
    floats.push_back(1.f);
  }
  memcpy(buffer.data.data(), floats.data(), sizeof(float) * floats.size());

  // create a view of this buffer
  int buffer_view_idx = (int)model.bufferViews.size();
  auto& buffer_view = model.bufferViews.emplace_back();
  buffer_view.buffer = buffer_idx;
  buffer_view.byteOffset = 0;
  buffer_view.byteLength = buffer.data.size();
  buffer_view.byteStride = 0;  // tightly packed
  buffer_view.target = TINYGLTF_TARGET_ARRAY_BUFFER;

  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;
  accessor.byteOffset = 0;
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  accessor.count = vertices.size();
  accessor.type = TINYGLTF_TYPE_VEC4;

  return accessor_idx;
}

int make_color_buffer_accessor(const std::vector<tfrag3::ShrubGpuVertex>& vertices,
                               tinygltf::Model& model,
                               const tfrag3::ShrubTree& shrub_tree,
                               int time_of_day) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 4 * vertices.size());
  std::vector<float> floats;

  for (size_t i = 0; i < vertices.size(); i++) {
    auto& color = shrub_tree.time_of_day_colors.at(vertices[i].color_index);
    for (int j = 0; j < 3; j++) {
      floats.push_back(((float)color.rgba[time_of_day][j]) / 255.f);
    }
    floats.push_back(1.f);
  }
  memcpy(buffer.data.data(), floats.data(), sizeof(float) * floats.size());

  // create a view of this buffer
  int buffer_view_idx = (int)model.bufferViews.size();
  auto& buffer_view = model.bufferViews.emplace_back();
  buffer_view.buffer = buffer_idx;
  buffer_view.byteOffset = 0;
  buffer_view.byteLength = buffer.data.size();
  buffer_view.byteStride = 0;  // tightly packed
  buffer_view.target = TINYGLTF_TARGET_ARRAY_BUFFER;

  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;
  accessor.byteOffset = 0;
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  accessor.count = vertices.size();
  accessor.type = TINYGLTF_TYPE_VEC4;

  return accessor_idx;
}

/*!
 * Create a tinygltf buffer and buffer view for indices, and convert to gltf format.
 * The map can be used to go from slots in the old index buffer to new.
 */
int make_index_buffer_view(const std::vector<u32>& indices,
                           tinygltf::Model& model,
                           std::vector<u32>& map_out) {
  std::vector<u32> unstripped;
  unstrip(indices, unstripped, map_out);

  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(u32) * unstripped.size());

  // and fill it
  memcpy(buffer.data.data(), unstripped.data(), buffer.data.size());

  // create a view of this buffer
  int buffer_view_idx = (int)model.bufferViews.size();
  auto& buffer_view = model.bufferViews.emplace_back();
  buffer_view.buffer = buffer_idx;
  buffer_view.byteOffset = 0;
  buffer_view.byteLength = buffer.data.size();
  buffer_view.byteStride = 0;  // tightly packed
  buffer_view.target = TINYGLTF_TARGET_ELEMENT_ARRAY_BUFFER;
  return buffer_view_idx;
}

int make_shrub_index_buffer_view(const std::vector<u32>& indices,
                                 const std::vector<tfrag3::ShrubDraw>& draws,
                                 tinygltf::Model& model,
                                 std::vector<u32>& draw_to_start,
                                 std::vector<u32>& draw_to_count) {
  std::vector<u32> unstripped;
  unstrip_draws(indices, unstripped, draw_to_start, draw_to_count, draws);

  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(u32) * unstripped.size());

  // and fill it
  memcpy(buffer.data.data(), unstripped.data(), buffer.data.size());

  // create a view of this buffer
  int buffer_view_idx = (int)model.bufferViews.size();
  auto& buffer_view = model.bufferViews.emplace_back();
  buffer_view.buffer = buffer_idx;
  buffer_view.byteOffset = 0;
  buffer_view.byteLength = buffer.data.size();
  buffer_view.byteStride = 0;  // tightly packed
  buffer_view.target = TINYGLTF_TARGET_ELEMENT_ARRAY_BUFFER;
  return buffer_view_idx;
}

int make_index_buffer_accessor(tinygltf::Model& model,
                               const tfrag3::StripDraw& draw,
                               const std::vector<u32>& idx_map,
                               int buffer_view_idx) {
  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;
  accessor.byteOffset = sizeof(u32) * idx_map.at(draw.unpacked.idx_of_first_idx_in_full_buffer);
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_UNSIGNED_INT;
  accessor.count = draw.num_triangles * 3;
  accessor.type = TINYGLTF_TYPE_SCALAR;

  return accessor_idx;
}

int make_index_buffer_accessor(tinygltf::Model& model,
                               int draw_idx,
                               const std::vector<u32>& draw_to_start,
                               const std::vector<u32>& draw_to_count,
                               int buffer_view_idx) {
  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;

  accessor.byteOffset = sizeof(u32) * draw_to_start.at(draw_idx);
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_UNSIGNED_INT;
  accessor.count = draw_to_count.at(draw_idx);
  accessor.type = TINYGLTF_TYPE_SCALAR;

  return accessor_idx;
}

int add_material_for_tex(const tfrag3::Level& level,
                         tinygltf::Model& model,
                         int tex_idx,
                         std::unordered_map<int, int>& tex_mat_map) {
  const auto& existing = tex_mat_map.find(tex_idx);
  if (existing != tex_mat_map.end()) {
    return existing->second;
  }

  auto& tex = level.textures.at(tex_idx);
  int mat_idx = (int)model.materials.size();
  auto& mat = model.materials.emplace_back();

  mat.doubleSided = true;
  mat.pbrMetallicRoughness.baseColorFactor = {2.0, 2.0, 2.0, 2.0};
  mat.pbrMetallicRoughness.baseColorTexture.texCoord = 0;  // TEXCOORD_0, I think
  mat.pbrMetallicRoughness.baseColorTexture.index = model.textures.size();
  mat.alphaMode = "MASK";
  mat.alphaCutoff = (float)0x26 / 255.f;
  auto& gltf_texture = model.textures.emplace_back();
  gltf_texture.name = tex.debug_name;
  gltf_texture.sampler = model.samplers.size();
  auto& sampler = model.samplers.emplace_back();
  gltf_texture.source = model.images.size();
  auto& image = model.images.emplace_back();
  image.pixel_type = TINYGLTF_TEXTURE_TYPE_UNSIGNED_BYTE;
  image.width = tex.w;
  image.height = tex.h;
  image.image.resize(tex.data.size() * 4);
  image.bits = 8;
  image.component = 4;
  image.mimeType = "image/png";
  memcpy(image.image.data(), tex.data.data(), tex.data.size() * 4);

  tex_mat_map[tex_idx] = mat_idx;

  return mat_idx;
}

/*!
 * Add the given tfrag data to a node under tfrag_root.
 */
void add_tfrag(const tfrag3::Level& level,
               const tfrag3::TfragTree& tfrag_in,
               tinygltf::Model& model,
               std::unordered_map<int, int>& tex_mat_map) {
  // copy and unpack in place
  tfrag3::TfragTree tfrag = tfrag_in;
  tfrag.unpack();

  // we'll make a Node, Mesh, Primitive, then add the data to the primitive.
  int node_idx = (int)model.nodes.size();
  auto& node = model.nodes.emplace_back();
  model.scenes.at(0).nodes.push_back(node_idx);

  int mesh_idx = (int)model.meshes.size();
  auto& mesh = model.meshes.emplace_back();
  node.mesh = mesh_idx;

  int position_buffer_accessor = make_position_buffer_accessor(tfrag.unpacked.vertices, model);
  int texture_buffer_accessor = make_tex_buffer_accessor(tfrag.unpacked.vertices, model, 1.f);
  std::vector<u32> index_map;
  int index_buffer_view = make_index_buffer_view(tfrag.unpacked.indices, model, index_map);
  int color0 = make_color_buffer_accessor(tfrag.unpacked.vertices, model, tfrag, 3);

  for (auto& draw : tfrag.draws) {
    auto& prim = mesh.primitives.emplace_back();
    prim.material = add_material_for_tex(level, model, draw.tree_tex_id, tex_mat_map);
    prim.indices = make_index_buffer_accessor(model, draw, index_map, index_buffer_view);
    prim.attributes["POSITION"] = position_buffer_accessor;
    prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
    prim.attributes["COLOR_0"] = color0;
    prim.mode = TINYGLTF_MODE_TRIANGLES;
  }
}

void add_tie(const tfrag3::Level& level,
             const tfrag3::TieTree& tie_in,
             tinygltf::Model& model,
             std::unordered_map<int, int>& tex_mat_map) {
  // copy and unpack in place
  tfrag3::TieTree tie = tie_in;
  tie.unpack();

  // we'll make a Node, Mesh, Primitive, then add the data to the primitive.
  int node_idx = (int)model.nodes.size();
  auto& node = model.nodes.emplace_back();
  model.scenes.at(0).nodes.push_back(node_idx);

  int mesh_idx = (int)model.meshes.size();
  auto& mesh = model.meshes.emplace_back();
  node.mesh = mesh_idx;

  int position_buffer_accessor = make_position_buffer_accessor(tie.unpacked.vertices, model);
  int texture_buffer_accessor = make_tex_buffer_accessor(tie.unpacked.vertices, model, 1.f);
  std::vector<u32> index_map;
  int index_buffer_view = make_index_buffer_view(tie.unpacked.indices, model, index_map);
  int color0 = make_color_buffer_accessor(tie.unpacked.vertices, model, tie, 3);

  for (auto& draw : tie.static_draws) {
    auto& prim = mesh.primitives.emplace_back();
    prim.material = add_material_for_tex(level, model, draw.tree_tex_id, tex_mat_map);
    prim.indices = make_index_buffer_accessor(model, draw, index_map, index_buffer_view);
    prim.attributes["POSITION"] = position_buffer_accessor;
    prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
    prim.attributes["COLOR_0"] = color0;
    prim.mode = TINYGLTF_MODE_TRIANGLES;
  }
}

void add_shrub(const tfrag3::Level& level,
               const tfrag3::ShrubTree& shrub_in,
               tinygltf::Model& model,
               std::unordered_map<int, int>& tex_mat_map) {
  // copy and unpack in place
  tfrag3::ShrubTree shrub = shrub_in;
  shrub.unpack();

  // we'll make a Node, Mesh, Primitive, then add the data to the primitive.
  int node_idx = (int)model.nodes.size();
  auto& node = model.nodes.emplace_back();
  model.scenes.at(0).nodes.push_back(node_idx);

  int mesh_idx = (int)model.meshes.size();
  auto& mesh = model.meshes.emplace_back();
  node.mesh = mesh_idx;

  int position_buffer_accessor = make_position_buffer_accessor(shrub.unpacked.vertices, model);
  int texture_buffer_accessor =
      make_tex_buffer_accessor(shrub.unpacked.vertices, model, 1.f / 4096.f);
  std::vector<u32> draw_to_start, draw_to_count;
  int index_buffer_view = make_shrub_index_buffer_view(shrub.indices, shrub.static_draws, model,
                                                       draw_to_start, draw_to_count);
  int color0 = make_color_buffer_accessor(shrub.unpacked.vertices, model, shrub, 3);

  // for (auto& draw : shrub.static_draws) {
  for (size_t draw_idx = 0; draw_idx < shrub.static_draws.size(); draw_idx++) {
    auto& draw = shrub.static_draws[draw_idx];
    auto& prim = mesh.primitives.emplace_back();
    prim.material = add_material_for_tex(level, model, draw.tree_tex_id, tex_mat_map);
    prim.indices = make_index_buffer_accessor(model, draw_idx, draw_to_start, draw_to_count,
                                              index_buffer_view);
    prim.attributes["POSITION"] = position_buffer_accessor;
    prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
    prim.attributes["COLOR_0"] = color0;
    prim.mode = TINYGLTF_MODE_TRIANGLES;
  }
}
}  // namespace

/*!
 * Export the background geometry (tie, tfrag, shrub) to a GLTF binary format (.glb) file.
 */
void save_level_background_as_gltf(const tfrag3::Level& level, const fs::path& glb_file) {
  fmt::print("save level!!!\n");
  // burp.

  // the top level container for everything is the model.
  tinygltf::Model model;

  // a "scene" is a traditional scene graph, made up of Nodes.
  auto& scene = model.scenes.emplace_back();

  // create a node to hold all tfrag meshes, and add it to the scene as a top level node.

  // hack, add a default material.
  tinygltf::Material mat;
  mat.pbrMetallicRoughness.baseColorFactor = {1.0f, 0.9f, 0.9f, 1.0f};
  mat.doubleSided = true;
  model.materials.push_back(mat);

  std::unordered_map<int, int> tex_mat_map;

  // add all hi-lod tfrag trees
  for (const auto& tfrag : level.tfrag_trees.at(0)) {
    add_tfrag(level, tfrag, model, tex_mat_map);
  }

  for (const auto& tie : level.tie_trees.at(0)) {
    add_tie(level, tie, model, tex_mat_map);
  }

  for (const auto& shrub : level.shrub_trees) {
    add_shrub(level, shrub, model, tex_mat_map);
  }

  model.asset.generator = "opengoal";
  tinygltf::TinyGLTF gltf;
  gltf.WriteGltfSceneToFile(&model, glb_file.string(),
                            true,   // embedImages
                            true,   // embedBuffers
                            true,   // pretty print
                            true);  // write binary
}