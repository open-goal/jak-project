#include "fr3_to_gltf.h"

#include <unordered_map>

#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"

#include "decompiler/level_extractor/tfrag_tie_fixup.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

namespace {

/*!
 * Convert fr3 format indices (strip format, with UINT32_MAX as restart) to unstripped tris.
 * Assumes that this is the tfrag/tie format of stripping. Will flip tris as needed so the faces
 * in this fragment all point a consistent way. However, the entire frag may be flipped.
 */
void unstrip_tfrag_tie(const std::vector<u32>& stripped_indices,
                       const std::vector<math::Vector3f>& positions,
                       std::vector<u32>& unstripped,
                       std::vector<u32>& old_to_new_start) {
  fixup_and_unstrip_tfrag_tie(stripped_indices, positions, unstripped, old_to_new_start);
}

/*!
 * Convert shrub strips. This doesn't assume anything about the strips.
 */
void unstrip_shrub_draws(const std::vector<u32>& stripped_indices,
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
 * Convert merc strips. Doesn't assume anything about strips. Output is [model][effect][draw] format
 */
void unstrip_merc_draws(const std::vector<u32>& stripped_indices,
                        const std::vector<tfrag3::MercModel>& models,
                        std::vector<u32>& unstripped,
                        std::vector<std::vector<std::vector<u32>>>& draw_to_start,
                        std::vector<std::vector<std::vector<u32>>>& draw_to_count) {
  for (auto& model : models) {
    auto& model_dts = draw_to_start.emplace_back();
    auto& model_dtc = draw_to_count.emplace_back();
    for (auto& effect : model.effects) {
      auto& effect_dts = model_dts.emplace_back();
      auto& effect_dtc = model_dtc.emplace_back();
      for (auto& draw : effect.all_draws) {
        effect_dts.push_back(unstripped.size());

        for (size_t i = 2; i < draw.index_count; i++) {
          int idx = i + draw.first_index;
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
        effect_dtc.push_back(unstripped.size() - effect_dts.back());
      }
    }
  }
}

/*!
 * Get just the xyz positions from a preloaded vertex vector.
 */
std::vector<math::Vector3f> extract_positions(const std::vector<tfrag3::PreloadedVertex>& vtx) {
  std::vector<math::Vector3f> result;
  for (auto& v : vtx) {
    auto& o = result.emplace_back();
    o[0] = v.x;
    o[1] = v.y;
    o[2] = v.z;
  }
  return result;
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
    if constexpr (std::is_same<T, tfrag3::MercVertex>::value) {
      float xyz[3] = {vtx.pos[0] / 4096.f, vtx.pos[1] / 4096.f, vtx.pos[2] / 4096.f};
      memcpy(buffer_ptr, xyz, 3 * sizeof(float));
      buffer_ptr += 3 * sizeof(float);
    } else {
      float xyz[3] = {vtx.x / 4096.f, vtx.y / 4096.f, vtx.z / 4096.f};
      memcpy(buffer_ptr, xyz, 3 * sizeof(float));
      buffer_ptr += 3 * sizeof(float);
    }
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

/*!
 * Set up a buffer for the texture coordinates of the given vertices, multiplying by scale.
 * Return the index of the accessor.
 */
template <typename T>
int make_tex_buffer_accessor(const std::vector<T>& vertices, tinygltf::Model& model, float scale) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 2 * vertices.size());

  // and fill it
  u8* buffer_ptr = buffer.data.data();
  for (const auto& vtx : vertices) {
    if constexpr (std::is_same<T, tfrag3::MercVertex>::value) {
      float st[2] = {vtx.st[0] * scale, vtx.st[1] * scale};
      memcpy(buffer_ptr, st, 2 * sizeof(float));
      buffer_ptr += 2 * sizeof(float);
    } else {
      float st[2] = {vtx.s * scale, vtx.t * scale};
      memcpy(buffer_ptr, st, 2 * sizeof(float));
      buffer_ptr += 2 * sizeof(float);
    }
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

/*!
 * Set up a buffer of vertex colors for the given time of day index, for tfrag.
 * Uses the time of day texture to look up colors.
 */
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

/*!
 * Set up a buffer of vertex colors for the given time of day index, for tie.
 * Uses the time of day texture to look up colors.
 */
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

int make_color_buffer_accessor(const std::vector<tfrag3::MercVertex>& vertices,
                               tinygltf::Model& model) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 4 * vertices.size());
  std::vector<float> floats;

  for (size_t i = 0; i < vertices.size(); i++) {
    for (int j = 0; j < 3; j++) {
      floats.push_back(((float)vertices[i].rgba[j]) / 255.f);
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
 * Set up a buffer of vertex colors for the given time of day index, for shrub.
 * Uses the time of day texture to look up colors.
 */
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
int make_tfrag_tie_index_buffer_view(const std::vector<u32>& indices,
                                     const std::vector<math::Vector3f>& positions,
                                     tinygltf::Model& model,
                                     std::vector<u32>& map_out) {
  std::vector<u32> unstripped;
  unstrip_tfrag_tie(indices, positions, unstripped, map_out);

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

/*!
 * Create a tinygltf buffer and buffer view for indices, and convert to gltf format.
 * The map can be used to go from slots in the old index buffer to new.
 */
int make_shrub_index_buffer_view(const std::vector<u32>& indices,
                                 const std::vector<tfrag3::ShrubDraw>& draws,
                                 tinygltf::Model& model,
                                 std::vector<u32>& draw_to_start,
                                 std::vector<u32>& draw_to_count) {
  std::vector<u32> unstripped;
  unstrip_shrub_draws(indices, unstripped, draw_to_start, draw_to_count, draws);

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

int make_merc_index_buffer_view(const std::vector<u32>& indices,
                                const std::vector<tfrag3::MercModel>& models,
                                tinygltf::Model& model,
                                std::vector<std::vector<std::vector<u32>>>& draw_to_start,
                                std::vector<std::vector<std::vector<u32>>>& draw_to_count) {
  std::vector<u32> unstripped;
  unstrip_merc_draws(indices, models, unstripped, draw_to_start, draw_to_count);

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

int make_index_buffer_accessor(tinygltf::Model& model, u32 start, u32 count, int buffer_view_idx) {
  int accessor_idx = (int)model.accessors.size();
  auto& accessor = model.accessors.emplace_back();
  accessor.bufferView = buffer_view_idx;

  accessor.byteOffset = sizeof(u32) * start;
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_UNSIGNED_INT;
  accessor.count = count;
  accessor.type = TINYGLTF_TYPE_SCALAR;

  return accessor_idx;
}

int add_image_for_tex(const tfrag3::Level& level,
                      tinygltf::Model& model,
                      int tex_idx,
                      std::unordered_map<int, int>& tex_image_map) {
  const auto& existing = tex_image_map.find(tex_idx);
  if (existing != tex_image_map.end()) {
    return existing->second;
  }

  auto& tex = level.textures.at(tex_idx);
  int image_idx = (int)model.images.size();
  auto& image = model.images.emplace_back();
  image.pixel_type = TINYGLTF_TEXTURE_TYPE_UNSIGNED_BYTE;
  image.width = tex.w;
  image.height = tex.h;
  image.image.resize(tex.data.size() * 4);
  image.bits = 8;
  image.component = 4;
  image.mimeType = "image/png";
  image.name = tex.debug_name;
  memcpy(image.image.data(), tex.data.data(), tex.data.size() * 4);

  tex_image_map[tex_idx] = image_idx;
  return image_idx;
}

int add_material_for_tex(const tfrag3::Level& level,
                         tinygltf::Model& model,
                         int tex_idx,
                         std::unordered_map<int, int>& tex_image_map,
                         const DrawMode& draw_mode) {
  int mat_idx = (int)model.materials.size();
  auto& mat = model.materials.emplace_back();
  auto& tex = level.textures.at(tex_idx);

  mat.doubleSided = true;
  // the 2.0 here compensates for the ps2's weird blending where 0.5 behaves like 1.0
  mat.pbrMetallicRoughness.baseColorFactor = {2.0, 2.0, 2.0, 2.0};
  mat.pbrMetallicRoughness.baseColorTexture.texCoord = 0;  // TEXCOORD_0, I think
  mat.pbrMetallicRoughness.baseColorTexture.index = model.textures.size();
  mat.alphaMode = draw_mode.get_ab_enable() ? "BLEND" : "MASK";
  // the foreground and background renderers both use this cutoff
  mat.alphaCutoff = (float)0x26 / 255.f;
  auto& gltf_texture = model.textures.emplace_back();
  gltf_texture.name = tex.debug_name;
  gltf_texture.sampler = model.samplers.size();
  auto& sampler = model.samplers.emplace_back();
  sampler.minFilter = draw_mode.get_filt_enable() ? TINYGLTF_TEXTURE_FILTER_LINEAR
                                                  : TINYGLTF_TEXTURE_FILTER_NEAREST;
  sampler.magFilter = draw_mode.get_filt_enable() ? TINYGLTF_TEXTURE_FILTER_LINEAR
                                                  : TINYGLTF_TEXTURE_FILTER_NEAREST;
  sampler.wrapS = draw_mode.get_clamp_s_enable() ? TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE
                                                 : TINYGLTF_TEXTURE_WRAP_REPEAT;
  sampler.wrapT = draw_mode.get_clamp_t_enable() ? TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE
                                                 : TINYGLTF_TEXTURE_WRAP_REPEAT;
  sampler.name = tex.debug_name;

  gltf_texture.source = add_image_for_tex(level, model, tex_idx, tex_image_map);

  return mat_idx;
}

constexpr int kMaxColor = 1;
/*!
 * Add the given tfrag data to a node under tfrag_root.
 */
void add_tfrag(const tfrag3::Level& level,
               const tfrag3::TfragTree& tfrag_in,
               tinygltf::Model& model,
               std::unordered_map<int, int>& tex_image_map) {
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
  int index_buffer_view = make_tfrag_tie_index_buffer_view(
      tfrag.unpacked.indices, extract_positions(tfrag.unpacked.vertices), model, index_map);
  int colors[kMaxColor];

  for (int i = 0; i < kMaxColor; i++) {
    colors[i] = make_color_buffer_accessor(tfrag.unpacked.vertices, model, tfrag, i);
  }

  for (auto& draw : tfrag.draws) {
    auto& prim = mesh.primitives.emplace_back();
    prim.material = add_material_for_tex(level, model, draw.tree_tex_id, tex_image_map, draw.mode);
    prim.indices = make_index_buffer_accessor(model, draw, index_map, index_buffer_view);
    prim.attributes["POSITION"] = position_buffer_accessor;
    prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
    for (int i = 0; i < kMaxColor; i++) {
      prim.attributes[fmt::format("COLOR_{}", i)] = colors[i];
    }
    prim.mode = TINYGLTF_MODE_TRIANGLES;
  }
}

void add_tie(const tfrag3::Level& level,
             const tfrag3::TieTree& tie_in,
             tinygltf::Model& model,
             std::unordered_map<int, int>& tex_image_map) {
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
  int index_buffer_view = make_tfrag_tie_index_buffer_view(
      tie.unpacked.indices, extract_positions(tie.unpacked.vertices), model, index_map);
  int colors[kMaxColor];

  for (int i = 0; i < kMaxColor; i++) {
    colors[i] = make_color_buffer_accessor(tie.unpacked.vertices, model, tie, i);
  }

  for (auto& draw : tie.static_draws) {
    auto& prim = mesh.primitives.emplace_back();
    prim.material = add_material_for_tex(level, model, draw.tree_tex_id, tex_image_map, draw.mode);
    prim.indices = make_index_buffer_accessor(model, draw, index_map, index_buffer_view);
    prim.attributes["POSITION"] = position_buffer_accessor;
    prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
    for (int i = 0; i < kMaxColor; i++) {
      prim.attributes[fmt::format("COLOR_{}", i)] = colors[i];
    }
    prim.mode = TINYGLTF_MODE_TRIANGLES;
  }
}

void add_shrub(const tfrag3::Level& level,
               const tfrag3::ShrubTree& shrub_in,
               tinygltf::Model& model,
               std::unordered_map<int, int>& tex_image_map) {
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
  int colors[kMaxColor];
  for (int i = 0; i < kMaxColor; i++) {
    colors[i] = make_color_buffer_accessor(shrub.unpacked.vertices, model, shrub, i);
  }

  // for (auto& draw : shrub.static_draws) {
  for (size_t draw_idx = 0; draw_idx < shrub.static_draws.size(); draw_idx++) {
    auto& draw = shrub.static_draws[draw_idx];
    auto& prim = mesh.primitives.emplace_back();
    prim.material = add_material_for_tex(level, model, draw.tree_tex_id, tex_image_map, draw.mode);
    prim.indices = make_index_buffer_accessor(model, draw_to_start.at(draw_idx),
                                              draw_to_count.at(draw_idx), index_buffer_view);
    prim.attributes["POSITION"] = position_buffer_accessor;
    prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
    for (int i = 0; i < kMaxColor; i++) {
      prim.attributes[fmt::format("COLOR_{}", i)] = colors[i];
    }
    prim.mode = TINYGLTF_MODE_TRIANGLES;
  }
}

void add_merc(const tfrag3::Level& level,
              tinygltf::Model& model,
              std::unordered_map<int, int>& tex_image_map) {
  const auto& mverts = level.merc_data.vertices;

  // create position and uv buffers
  int position_buffer_accessor = make_position_buffer_accessor(mverts, model);
  int texture_buffer_accessor = make_tex_buffer_accessor(mverts, model, 1.f);

  std::vector<std::vector<std::vector<u32>>> draw_to_start, draw_to_count;
  int index_buffer_view = make_merc_index_buffer_view(
      level.merc_data.indices, level.merc_data.models, model, draw_to_start, draw_to_count);
  int colors = make_color_buffer_accessor(mverts, model);

  for (size_t model_idx = 0; model_idx < level.merc_data.models.size(); model_idx++) {
    const auto& mmodel = level.merc_data.models[model_idx];

    int node_idx = (int)model.nodes.size();
    auto& node = model.nodes.emplace_back();
    model.scenes.at(0).nodes.push_back(node_idx);
    node.name = mmodel.name;
    int mesh_idx = (int)model.meshes.size();
    auto& mesh = model.meshes.emplace_back();
    mesh.name = node.name;
    node.mesh = mesh_idx;

    for (size_t effect_idx = 0; effect_idx < mmodel.effects.size(); effect_idx++) {
      const auto& effect = mmodel.effects[effect_idx];
      for (size_t draw_idx = 0; draw_idx < effect.all_draws.size(); draw_idx++) {
        const auto& draw = effect.all_draws[draw_idx];
        auto& prim = mesh.primitives.emplace_back();
        prim.material =
            add_material_for_tex(level, model, draw.tree_tex_id, tex_image_map, draw.mode);
        prim.indices = make_index_buffer_accessor(
            model, draw_to_start[model_idx][effect_idx][draw_idx],
            draw_to_count[model_idx][effect_idx][draw_idx], index_buffer_view);
        prim.attributes["POSITION"] = position_buffer_accessor;
        prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
        prim.attributes["COLOR_0"] = colors;
        prim.mode = TINYGLTF_MODE_TRIANGLES;
      }
    }
  }
}
}  // namespace

/*!
 * Export the background geometry (tie, tfrag, shrub) to a GLTF binary format (.glb) file.
 */
void save_level_background_as_gltf(const tfrag3::Level& level, const fs::path& glb_file) {
  // the top level container for everything is the model.
  tinygltf::Model model;

  // a "scene" is a traditional scene graph, made up of Nodes.
  // sadly, attempting to nest stuff makes the blender importer unhappy, so we just dump
  // everything into the top level.
  model.scenes.emplace_back();

  // hack, add a default material.
  tinygltf::Material mat;
  mat.pbrMetallicRoughness.baseColorFactor = {1.0f, 0.9f, 0.9f, 1.0f};
  mat.doubleSided = true;
  model.materials.push_back(mat);

  std::unordered_map<int, int> tex_image_map;

  // add all hi-lod tfrag trees
  for (const auto& tfrag : level.tfrag_trees.at(0)) {
    add_tfrag(level, tfrag, model, tex_image_map);
  }

  for (const auto& tie : level.tie_trees.at(0)) {
    add_tie(level, tie, model, tex_image_map);
  }

  for (const auto& shrub : level.shrub_trees) {
    add_shrub(level, shrub, model, tex_image_map);
  }

  model.asset.generator = "opengoal";
  tinygltf::TinyGLTF gltf;
  gltf.WriteGltfSceneToFile(&model, glb_file.string(),
                            true,   // embedImages
                            true,   // embedBuffers
                            true,   // pretty print
                            true);  // write binary
}

void save_level_foreground_as_gltf(const tfrag3::Level& level, const fs::path& glb_file) {
  // the top level container for everything is the model.
  tinygltf::Model model;

  // a "scene" is a traditional scene graph, made up of Nodes.
  // sadly, attempting to nest stuff makes the blender importer unhappy, so we just dump
  // everything into the top level.
  model.scenes.emplace_back();

  // hack, add a default material.
  tinygltf::Material mat;
  mat.pbrMetallicRoughness.baseColorFactor = {1.0f, 0.9f, 0.9f, 1.0f};
  mat.doubleSided = true;
  model.materials.push_back(mat);

  std::unordered_map<int, int> tex_image_map;

  add_merc(level, model, tex_image_map);

  model.asset.generator = "opengoal";
  tinygltf::TinyGLTF gltf;
  gltf.WriteGltfSceneToFile(&model, glb_file.string(),
                            true,   // embedImages
                            true,   // embedBuffers
                            true,   // pretty print
                            true);  // write binary
}