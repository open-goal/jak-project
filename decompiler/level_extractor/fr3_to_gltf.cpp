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

void unstrip_tie_wind(std::vector<u32>& unstripped,
                      std::vector<std::vector<u32>>& draw_to_starts,
                      std::vector<std::vector<u32>>& draw_to_counts,
                      const std::vector<tfrag3::InstancedStripDraw>& draws) {
  for (auto& draw : draws) {
    auto& starts = draw_to_starts.emplace_back();
    auto& counts = draw_to_counts.emplace_back();

    int grp_offset = 0;

    for (const auto& grp : draw.instance_groups) {
      starts.push_back(unstripped.size());

      for (size_t i = grp_offset + 2; i < grp_offset + grp.num; i++) {
        u32 a = draw.vertex_index_stream.at(i);
        u32 b = draw.vertex_index_stream.at(i - 1);
        u32 c = draw.vertex_index_stream.at(i - 2);
        if (a == UINT32_MAX || b == UINT32_MAX || c == UINT32_MAX) {
          continue;
        }
        unstripped.push_back(a);
        unstripped.push_back(b);
        unstripped.push_back(c);
      }
      counts.push_back(unstripped.size() - starts.back());
      grp_offset += grp.num;
    }
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

int make_tie_wind_index_buffer_view(const std::vector<tfrag3::InstancedStripDraw>& draws,
                                    tinygltf::Model& model,
                                    std::vector<std::vector<u32>>& draw_to_starts,
                                    std::vector<std::vector<u32>>& draw_to_counts) {
  std::vector<u32> unstripped;
  unstrip_tie_wind(unstripped, draw_to_starts, draw_to_counts, draws);

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

  if (!tie.instanced_wind_draws.empty()) {
    std::vector<std::vector<u32>> draw_to_starts, draw_to_counts;
    int wind_index_buffer_view = make_tie_wind_index_buffer_view(tie.instanced_wind_draws, model,
                                                                 draw_to_starts, draw_to_counts);

    for (size_t draw_idx = 0; draw_idx < tie.instanced_wind_draws.size(); draw_idx++) {
      const auto& wind_draw = tie.instanced_wind_draws[draw_idx];
      int mat =
          add_material_for_tex(level, model, wind_draw.tree_tex_id, tex_image_map, wind_draw.mode);
      for (size_t grp_idx = 0; grp_idx < wind_draw.instance_groups.size(); grp_idx++) {
        const auto& grp = wind_draw.instance_groups[grp_idx];
        int c_node_idx = (int)model.nodes.size();
        auto& c_node = model.nodes.emplace_back();
        model.nodes[node_idx].children.push_back(c_node_idx);
        int c_mesh_idx = (int)model.meshes.size();
        auto& c_mesh = model.meshes.emplace_back();
        c_node.mesh = c_mesh_idx;
        auto& prim = c_mesh.primitives.emplace_back();

        const auto& info = tie.wind_instance_info.at(grp.instance_idx);
        for (int i = 0; i < 4; i++) {
          float scale = i == 3 ? (1.f / 4096.f) : 1.f;
          for (int j = 0; j < 4; j++) {
            c_node.matrix.push_back(scale * info.matrix[i][j]);
          }
        }

        prim.material = mat;
        prim.indices = make_index_buffer_accessor(model, draw_to_starts.at(draw_idx).at(grp_idx),
                                                  draw_to_counts.at(draw_idx).at(grp_idx),
                                                  wind_index_buffer_view);
        prim.attributes["POSITION"] = position_buffer_accessor;
        prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
        for (int i = 0; i < kMaxColor; i++) {
          prim.attributes[fmt::format("COLOR_{}", i)] = colors[i];
        }
        prim.mode = TINYGLTF_MODE_TRIANGLES;
      }
    }
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

int make_weights_accessor(const std::vector<tfrag3::MercVertex>& vertices, tinygltf::Model& model) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 4 * vertices.size());

  // and fill it
  u8* buffer_ptr = buffer.data.data();
  for (const auto& vtx : vertices) {
    float weights[4] = {vtx.weights[0], vtx.weights[1], vtx.weights[2], 0};
    memcpy(buffer_ptr, weights, 4 * sizeof(float));
    buffer_ptr += 4 * sizeof(float);
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
  accessor.type = TINYGLTF_TYPE_VEC4;
  return accessor_idx;
}

int make_bones_accessor(const std::vector<tfrag3::MercVertex>& vertices, tinygltf::Model& model) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 4 * vertices.size());

  // and fill it
  u8* buffer_ptr = buffer.data.data();
  for (const auto& vtx : vertices) {
    s32 indices[4];
    for (int i = 0; i < 3; i++) {

      indices[i] = vtx.mats[i] ? vtx.mats[i]- 1 : 0;
    }
    indices[3] = 0;
    memcpy(buffer_ptr, indices, 4 * sizeof(s32));
    buffer_ptr += 4 * sizeof(s32);
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
  accessor.componentType = TINYGLTF_COMPONENT_TYPE_UNSIGNED_INT;  // blender doesn't support INT...
  accessor.count = vertices.size();
  accessor.type = TINYGLTF_TYPE_VEC4;
  return accessor_idx;
}

struct Bone {
  int number;
  std::string name;
  std::string parent;
  float inv_matrix[16];
  float matrix[16];
};

int make_inv_matrix_bind_poses(const std::vector<Bone>& bones, tinygltf::Model& model) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 16 * bones.size());

  // and fill it
  u8* buffer_ptr = buffer.data.data();
  for (int m = 0; m < bones.size(); m++) {
    for (int i = 0; i < 4; i++) {
      for (int j = 0; j < 4; j++) {
        memcpy(buffer.data.data() + sizeof(float) * (i * 4 + j + m * 16),
               &bones[m].matrix[i * 4 + j], 4);
      }
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
  accessor.count = bones.size();
  accessor.type = TINYGLTF_TYPE_MAT4;
  return accessor_idx;
}

std::vector<Bone> get_jak_bones() {
  return {
      {
          0,
          "align",
          "NONE",
          {1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000,
           0.0000, 0.0000, 0.0000, 0.0000, 1.0000},
          {1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000,
           0.0000, 0.0000, 0.0000, 0.0000, 1.0000},
      },
      {
          1,
          "prejoint",
          "NONE",
          {1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000,
           0.0000, 0.0000, 0.0000, 0.0000, 1.0000},
          {1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0000,
           0.0000, 0.0000, 0.0000, 0.0000, 1.0000},
      },
      {
          2,
          "main",
          "prejoint",
          {1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, 0.0000, 1.4419, 0.0000, 1.0000},
          {0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142,
           0.0000, 0.0000, -1.0299, 0.0000, 1.0000},
      },
      {
          3,
          "upper_body",
          "main",
          {1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, 0.0000, 1.6870, 0.0000, 1.0000},
          {0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142,
           0.0000, 0.0000, -1.2050, 0.0000, 1.0000},
      },
      {
          4,
          "chest",
          "upper_body",
          {1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, 0.0000, 2.0720, 0.0000, 1.0000},
          {0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142,
           0.0000, 0.0000, -1.4800, 0.0000, 1.0000},
      },
      {
          5,
          "neckA",
          "chest",
          {1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, 0.0000, 2.1979, 0.0559, 1.0000},
          {0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142,
           0.0000, 0.0000, -1.5700, -0.0399, 1.0000},
      },
      {
          6,
          "neckB",
          "neckA",
          {1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, 0.0000, 2.3099, 0.0559, 1.0000},
          {0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142,
           0.0000, 0.0000, -1.6500, -0.0399, 1.0000},
      },
      {
          7,
          "MhairA",
          "neckB",
          {1.3946, -0.1220, 0.0000, 0.0000, 0.1178, 1.3471, -0.3623, 0.0000, 0.0315, 0.3609, 1.3522,
           0.0000, 0.0000, 2.6320, 0.0560, 1.0000},
          {0.7115, 0.0601, 0.0161, 0.0000, -0.0622, 0.6873, 0.1841, 0.0000, -0.0000, -0.1848,
           0.6899, 0.0000, 0.1638, -1.7986, -0.5233, 1.0000},
      },
      {
          8,
          "MhairB",
          "MhairA",
          {1.3530, -0.3540, 0.0629, 0.0000, 0.3378, 1.1675, -0.6946, 0.0000, 0.1232, 0.6865, 1.2138,
           0.0000, 0.0271, 2.9418, -0.0273, 1.0000},
          {0.6903, 0.1723, 0.0628, 0.0000, -0.1806, 0.5957, 0.3502, 0.0000, 0.0321, -0.3544, 0.6193,
           0.0000, 0.5136, -1.7668, -1.0152, 1.0000},
      },
      {
          9,
          "Learbase",
          "neckB",
          {0.9725, 0.0000, 1.0070, 0.0000, 0.9463, 0.4788, -0.9138, 0.0000, -0.3444, 1.3155, 0.3326,
           0.0000, 0.1610, 2.6222, 0.1400, 1.0000},
          {0.4961, 0.4828, -0.1757, 0.0000, -0.0000, 0.2443, 0.6712, 0.0000, 0.5138, -0.4662,
           0.1697, 0.0000, -0.1518, -0.6530, -1.7555, 1.0000},
      },
      {
          10,
          "Learmid",
          "Learbase",
          {0.9725, 0.0000, 1.0070, 0.0000, 0.9463, 0.4788, -0.9138, 0.0000, -0.3444, 1.3155, 0.3326,
           0.0000, 0.3975, 2.7419, -0.0884, 1.0000},
          {0.4961, 0.4828, -0.1757, 0.0000, -0.0000, 0.2443, 0.6712, 0.0000, 0.5138, -0.4662,
           0.1697, 0.0000, -0.1518, -0.9030, -1.7555, 1.0000},
      },
      {
          11,
          "Rearbase",
          "neckB",
          {0.9725, -0.0000, -1.0070, 0.0000, 0.9463, -0.4788, 0.9138, 0.0000, -0.3444, -1.3155,
           -0.3326, 0.0000, -0.1610, 2.6222, 0.1400, 1.0000},
          {0.4961, 0.4828, -0.1757, 0.0000, 0.0000, -0.2443, -0.6712, 0.0000, -0.5138, 0.4662,
           -0.1697, 0.0000, 0.1518, 0.6530, 1.7555, 1.0000},
      },
      {
          12,
          "Rearmid",
          "Rearbase",
          {0.9725, -0.0000, -1.0070, 0.0000, 0.9463, -0.4788, 0.9138, 0.0000, -0.3444, -1.3155,
           -0.3326, 0.0000, -0.3975, 2.7419, -0.0884, 1.0000},
          {0.4961, 0.4828, -0.1757, 0.0000, 0.0000, -0.2443, -0.6712, 0.0000, -0.5138, 0.4662,
           -0.1697, 0.0000, 0.1518, 0.9030, 1.7555, 1.0000},
      },
      {
          13,
          "Lshould",
          "chest",
          {-0.0000, -1.3999, 0.0000, 0.0000, 1.3999, -0.0000, -0.0000, 0.0000, 0.0000, 0.0000,
           1.3999, 0.0000, 0.2240, 2.0369, -0.0700, 1.0000},
          {-0.0000, 0.7142, 0.0000, 0.0000, -0.7142, -0.0000, 0.0000, 0.0000, 0.0000, -0.0000,
           0.7142, 0.0000, 1.4550, -0.1599, 0.0500, 1.0000},
      },
      {
          14,
          "Larm",
          "Lshould",
          {0.0068, 0.0000, -1.3999, 0.0000, 1.3999, -0.0000, 0.0068, 0.0000, -0.0000, -1.3999,
           -0.0000, 0.0000, 0.3639, 2.0369, -0.0699, 1.0000},
          {0.0034, 0.7142, -0.0000, 0.0000, 0.0000, -0.0000, -0.7142, 0.0000, -0.7142, 0.0034,
           -0.0000, 0.0000, -0.0512, -0.2597, 1.4550, 1.0000},
      },
      {
          15,
          "Lforarm",
          "Larm",
          {0.1138, 0.0000, -1.3953, 0.0000, 1.3953, -0.0000, 0.1138, 0.0000, -0.0000, -1.3999,
           -0.0000, 0.0000, 0.8539, 2.0369, -0.0676, 1.0000},
          {0.0580, 0.7119, -0.0000, 0.0000, 0.0000, -0.0000, -0.7142, 0.0000, -0.7119, 0.0580,
           -0.0000, 0.0000, -0.0977, -0.6040, 1.4550, 1.0000},
      },
      {
          16,
          "sk_lhand",
          "Lforarm",
          {0.1134, -0.1220, -1.3900, 0.0000, 1.3953, 0.0000, 0.1138, 0.0000, -0.0099, -1.3946,
           0.1216, 0.0000, 1.2726, 2.0370, -0.0334, 1.0000},
          {0.0578, 0.7119, -0.0050, 0.0000, -0.0622, -0.0000, -0.7115, 0.0000, -0.7092, 0.0580,
           0.0620, 0.0000, 0.0294, -0.9040, 1.4579, 1.0000},
      },
      {
          17,
          "handLStrapTopTop",
          "sk_lhand",
          {-0.3472, 1.2027, -0.6267, 0.0000, 1.3530, 0.3518, -0.0743, 0.0000, 0.0936, -0.6241,
           -1.2496, 0.0000, 1.3214, 2.0560, -0.0921, 1.0000},
          {-0.1771, 0.6903, 0.0477, 0.0000, 0.6136, 0.1795, -0.3184, 0.0000, -0.3197, -0.0379,
           -0.6375, 0.0000, -1.0570, -1.2848, 0.5328, 1.0000},
      },
      {
          18,
          "handLStrapTopMid",
          "handLStrapTopTop",
          {-0.3472, 1.2027, -0.6267, 0.0000, 1.3311, 0.1783, -0.3952, 0.0000, -0.2597, -0.6939,
           -1.1878, 0.0000, 1.4906, 2.1000, -0.1014, 1.0000},
          {-0.1771, 0.6791, -0.1325, 0.0000, 0.6136, 0.0909, -0.3540, 0.0000, -0.3197, -0.2016,
           -0.6060, 0.0000, -1.0570, -1.2239, 0.8795, 1.0000},
      },
      {
          19,
          "Rshould",
          "chest",
          {0.0000, -1.3999, 0.0000, 0.0000, 1.3999, 0.0000, -0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, -0.2240, 2.0369, -0.0700, 1.0000},
          {0.0000, 0.7142, 0.0000, 0.0000, -0.7142, 0.0000, 0.0000, 0.0000, -0.0000, -0.0000,
           0.7142, 0.0000, 1.4550, 0.1599, 0.0500, 1.0000},
      },
      {
          20,
          "Rarm",
          "Rshould",
          {-0.0000, 1.3999, 0.0000, 0.0000, 1.3999, 0.0000, -0.0066, 0.0000, -0.0066, 0.0000,
           -1.3999, 0.0000, -0.3639, 2.0370, -0.0700, 1.0000},
          {-0.0000, 0.7142, -0.0034, 0.0000, 0.7142, 0.0000, 0.0000, 0.0000, 0.0000, -0.0034,
           -0.7142, 0.0000, -1.4550, 0.2597, -0.0513, 1.0000},
      },
      {
          21,
          "Rforarm",
          "Rarm",
          {-0.0000, 1.3999, 0.0000, 0.0000, 1.3953, 0.0000, -0.1139, 0.0000, -0.1139, 0.0000,
           -1.3953, 0.0000, -0.8540, 2.0370, -0.0676, 1.0000},
          {-0.0000, 0.7119, -0.0581, 0.0000, 0.7142, 0.0000, 0.0000, 0.0000, 0.0000, -0.0581,
           -0.7119, 0.0000, -1.4550, 0.6040, -0.0978, 1.0000},
      },
      {
          22,
          "sk_rhand",
          "Rforarm",
          {-0.0099, 1.3946, -0.1215, 0.0000, 1.3953, 0.0000, -0.1139, 0.0000, -0.1135, -0.1219,
           -1.3900, 0.0000, -1.2726, 2.0370, -0.0334, 1.0000},
          {-0.0050, 0.7119, -0.0579, 0.0000, 0.7115, 0.0000, -0.0622, 0.0000, -0.0620, -0.0581,
           -0.7092, 0.0000, -1.4579, 0.9040, 0.0292, 1.0000},
      },
      {
          23,
          "handRStrapTopTop",
          "sk_rhand",
          {-0.3471, -1.2027, 0.6267, 0.0000, 1.3530, -0.3518, 0.0742, 0.0000, 0.0937, 0.6241,
           1.2496, 0.0000, -1.3214, 2.0560, -0.0921, 1.0000},
          {-0.1771, 0.6903, 0.0478, 0.0000, -0.6136, -0.1795, 0.3184, 0.0000, 0.3197, 0.0378,
           0.6375, 0.0000, 1.0570, 1.2848, -0.5326, 1.0000},
      },
      {
          24,
          "handRStrapTopMid",
          "handRStrapTopTop",
          {-0.3471, -1.2027, 0.6267, 0.0000, 1.3311, -0.1783, 0.3951, 0.0000, -0.2596, 0.6939,
           1.1878, 0.0000, -1.4906, 2.1000, -0.1014, 1.0000},
          {-0.1771, 0.6791, -0.1324, 0.0000, -0.6136, -0.0910, 0.3540, 0.0000, 0.3197, 0.2015,
           0.6060, 0.0000, 1.0570, 1.2239, -0.8794, 1.0000},
      },
      {
          25,
          "hips",
          "main",
          {0.0000, -1.3999, 0.0000, 0.0000, 1.3999, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, 0.0000, 1.4419, 0.0000, 1.0000},
          {0.0000, 0.7142, 0.0000, 0.0000, -0.7142, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142,
           0.0000, 1.0299, -0.0000, 0.0000, 1.0000},
      },
      {
          26,
          "Lthigh",
          "hips",
          {1.3946, 0.1220, -0.0000, 0.0000, 0.1219, -1.3941, 0.0366, 0.0000, 0.0031, -0.0365,
           -1.3995, 0.0000, 0.1400, 1.3019, 0.0000, 1.0000},
          {0.7115, 0.0622, 0.0016, 0.0000, 0.0622, -0.7113, -0.0186, 0.0000, -0.0000, 0.0186,
           -0.7140, 0.0000, -0.1806, 0.9174, 0.0240, 1.0000},
      },
      {
          27,
          "Lknee",
          "Lthigh",
          {0.0138, -0.1578, 1.3910, 0.0000, 0.1212, -1.3857, -0.1584, 0.0000, 1.3946, 0.1220,
           -0.0000, 0.0000, 0.1918, 0.7094, 0.0155, 1.0000},
          {0.0070, 0.0618, 0.7115, 0.0000, -0.0805, -0.7069, 0.0622, 0.0000, 0.7096, -0.0808,
           -0.0000, 0.0000, 0.0447, 0.4909, -0.1806, 1.0000},
      },
      {
          28,
          "Lankle",
          "Lknee",
          {0.3957, -1.2013, -0.6000, 0.0000, -0.0081, -0.6277, 1.2513, 0.0000, -1.3428, -0.3502,
           -0.1844, 0.0000, 0.2418, 0.1378, -0.0497, 1.0000},
          {0.2018, -0.0041, -0.6851, 0.0000, -0.6129, -0.3202, -0.1786, 0.0000, -0.3061, 0.6384,
           -0.0940, 0.0000, 0.0204, 0.0769, 0.1856, 1.0000},
      },
      {
          29,
          "ankleLStrap",
          "Lknee",
          {-0.0766, -0.3638, -1.3497, 0.0000, 1.3064, -0.4994, 0.0604, 0.0000, -0.4972, -1.2562,
           0.3668, 0.0000, 0.2794, 0.4114, -0.1170, 1.0000},
          {-0.0390, 0.6665, -0.2536, 0.0000, -0.1856, -0.2548, -0.6409, 0.0000, -0.6886, 0.0308,
           0.1871, 0.0000, 0.0066, -0.0778, 0.3565, 1.0000},
      },
      {
          30,
          "Rthigh",
          "hips",
          {1.3946, -0.1220, -0.0000, 0.0000, 0.1219, 1.3941, -0.0366, 0.0000, 0.0031, 0.0365,
           1.3995, 0.0000, -0.1400, 1.3019, 0.0000, 1.0000},
          {0.7115, 0.0622, 0.0016, 0.0000, -0.0622, 0.7113, 0.0186, 0.0000, -0.0000, -0.0187,
           0.7140, 0.0000, 0.1806, -0.9174, -0.0240, 1.0000},
      },
      {
          31,
          "Rknee",
          "Rthigh",
          {-1.3946, 0.1220, 0.0000, 0.0000, 0.1212, 1.3857, 0.1584, 0.0000, 0.0138, 0.1578, -1.3910,
           0.0000, -0.1918, 0.7094, 0.0155, 1.0000},
          {-0.7115, 0.0618, 0.0070, 0.0000, 0.0622, 0.7069, 0.0805, 0.0000, 0.0000, 0.0808, -0.7096,
           0.0000, -0.1806, -0.4909, -0.0447, 1.0000},
      },
      {
          32,
          "Rankle",
          "Rknee",
          {1.3414, -0.3788, -0.1298, 0.0000, 0.0544, 0.6222, -1.2529, 0.0000, 0.3967, 1.1955,
           0.6110, 0.0000, -0.2418, 0.1378, -0.0497, 1.0000},
          {0.6844, 0.0277, 0.2024, 0.0000, -0.1932, 0.3174, 0.6099, 0.0000, -0.0662, -0.6392,
           0.3117, 0.0000, 0.1888, -0.0688, -0.0196, 1.0000},
      },
      {
          33,
          "ankleRStrap",
          "Rknee",
          {-0.0765, 0.3638, 1.3497, 0.0000, -1.3064, -0.4994, 0.0605, 0.0000, 0.4972, -1.2562,
           0.3668, 0.0000, -0.2794, 0.4114, -0.1170, 1.0000},
          {-0.0390, -0.6665, 0.2536, 0.0000, 0.1856, -0.2548, -0.6409, 0.0000, 0.6886, 0.0308,
           0.1871, 0.0000, -0.0066, -0.0778, 0.3565, 1.0000},
      },
      {
          34,
          "shirtLthigh",
          "hips",
          {1.3946, 0.1220, -0.0000, 0.0000, 0.1219, -1.3941, 0.0366, 0.0000, 0.0031, -0.0365,
           -1.3995, 0.0000, 0.1400, 1.3019, 0.0000, 1.0000},
          {0.7115, 0.0622, 0.0016, 0.0000, 0.0622, -0.7113, -0.0186, 0.0000, -0.0000, 0.0186,
           -0.7140, 0.0000, -0.1806, 0.9174, 0.0240, 1.0000},
      },
      {
          35,
          "shirtRthigh",
          "hips",
          {1.3946, -0.1220, -0.0000, 0.0000, 0.1219, 1.3941, -0.0366, 0.0000, 0.0031, 0.0365,
           1.3995, 0.0000, -0.1400, 1.3019, 0.0000, 1.0000},
          {0.7115, 0.0622, 0.0016, 0.0000, -0.0622, 0.7113, 0.0186, 0.0000, -0.0000, -0.0187,
           0.7140, 0.0000, 0.1806, -0.9174, -0.0240, 1.0000},
      },
      {
          36,
          "mouth",
          "neckB",
          {1.3999, 0.0000, 0.0000, 0.0000, 0.0000, -0.2431, 1.3787, 0.0000, 0.0000, -1.3787,
           -0.2431, 0.0000, 0.0000, 2.3376, 0.1719, 1.0000},
          {0.7142, 0.0000, 0.0000, 0.0000, 0.0000, -0.1240, -0.7034, 0.0000, 0.0000, 0.7034,
           -0.1240, 0.0000, 0.0000, 0.1690, 1.6657, 1.0000},
      },
      {
          37,
          "browL",
          "neckB",
          {-0.2431, 1.3787, 0.0000, 0.0000, -1.3787, -0.2431, 0.0000, 0.0000, 0.0000, 0.0000,
           1.3999, 0.0000, 0.1750, 2.5620, 0.3150, 1.0000},
          {-0.1240, -0.7034, 0.0000, 0.0000, 0.7034, -0.1240, 0.0000, 0.0000, 0.0000, 0.0000,
           0.7142, 0.0000, -1.7804, 0.4408, -0.2250, 1.0000},
      },
      {
          38,
          "browR",
          "neckB",
          {0.2431, 1.3787, 0.0000, 0.0000, 1.3787, -0.2431, 0.0000, 0.0000, 0.0000, -0.0000,
           -1.3999, 0.0000, -0.1750, 2.5620, 0.3150, 1.0000},
          {0.1240, 0.7034, 0.0000, 0.0000, 0.7034, -0.1240, -0.0000, 0.0000, -0.0000, 0.0000,
           -0.7142, 0.0000, -1.7804, 0.4408, 0.2250, 1.0000},
      },
      {
          39,
          "lthumA",
          "sk_lhand",
          {0.2024, -1.2882, -0.5093, 0.0000, -0.0474, -0.5208, 1.2986, 0.0000, -1.3844, -0.1705,
           -0.1189, 0.0000, 1.3384, 2.0206, 0.0416, 1.0000},
          {0.1032, -0.0241, -0.7063, 0.0000, -0.6572, -0.2657, -0.0869, 0.0000, -0.2598, 0.6625,
           -0.0606, 0.0000, 1.2007, 0.5418, 1.1237, 1.0000},
      },
      {
          40,
          "lthumB",
          "lthumA",
          {0.2024, -1.2882, -0.5093, 0.0000, 1.1752, -0.1127, 0.7523, 0.0000, -0.7333, -0.5363,
           1.0651, 0.0000, 1.3365, 1.9998, 0.0935, 1.0000},
          {0.1032, 0.5996, -0.3741, 0.0000, -0.6572, -0.0575, -0.2736, 0.0000, -0.2598, 0.3838,
           0.5434, 0.0000, 1.2007, -0.7222, 0.9964, 1.0000},
      },
      {
          41,
          "lindA",
          "sk_lhand",
          {0.0019, -1.3997, -0.0243, 0.0000, 1.3999, 0.0021, -0.0081, 0.0000, 0.0082, -0.0243,
           1.3997, 0.0000, 1.4751, 2.0516, 0.0538, 1.0000},
          {0.0010, 0.7142, 0.0041, 0.0000, -0.7141, 0.0010, -0.0124, 0.0000, -0.0124, -0.0041,
           0.7141, 0.0000, 1.4643, -1.0556, -0.0191, 1.0000},
      },
      {
          42,
          "lindB",
          "lindA",
          {-0.0082, 0.0243, -1.3997, 0.0000, 1.0737, -0.8981, -0.0219, 0.0000, -0.8983, -1.0736,
           -0.0134, 0.0000, 1.6011, 2.0518, 0.0531, 1.0000},
          {-0.0041, 0.5478, -0.4583, 0.0000, 0.0124, -0.4582, -0.5477, 0.0000, -0.7141, -0.0111,
           -0.0068, 0.0000, 0.0191, 0.0636, 1.8581, 1.0000},
      },
      {
          43,
          "lmidA",
          "sk_lhand",
          {-0.0000, -1.3999, -0.0000, 0.0000, 1.3999, -0.0000, -0.0081, 0.0000, 0.0081, -0.0000,
           1.3999, 0.0000, 1.4791, 2.0533, 0.0050, 1.0000},
          {-0.0000, 0.7142, 0.0041, 0.0000, -0.7142, -0.0000, -0.0000, 0.0000, -0.0000, -0.0041,
           0.7142, 0.0000, 1.4666, -1.0564, -0.0097, 1.0000},
      },
      {
          44,
          "lmidB",
          "lmidA",
          {-0.0081, 0.0000, -1.3999, 0.0000, 1.0724, -0.8999, -0.0062, 0.0000, -0.8998, -1.0724,
           0.0052, 0.0000, 1.6051, 2.0533, 0.0043, 1.0000},
          {-0.0041, 0.5471, -0.4591, 0.0000, 0.0000, -0.4591, -0.5471, 0.0000, -0.7142, -0.0031,
           0.0026, 0.0000, 0.0097, 0.0645, 1.8604, 1.0000},
      },
      {
          45,
          "lringA",
          "sk_lhand",
          {0.0243, -1.3997, 0.0019, 0.0000, 1.3997, 0.0243, 0.0048, 0.0000, -0.0049, 0.0019, 1.3999,
           0.0000, 1.4761, 2.0541, -0.0443, 1.0000},
          {0.0124, 0.7141, -0.0025, 0.0000, -0.7141, 0.0124, 0.0009, 0.0000, 0.0010, 0.0024, 0.7142,
           0.0000, 1.4487, -1.0796, 0.0333, 1.0000},
      },
      {
          46,
          "lringB",
          "lringA",
          {0.0049, -0.0019, -1.3999, 0.0000, 1.0879, -0.8811, 0.0050, 0.0000, -0.8811, -1.0879,
           -0.0016, 0.0000, 1.5811, 2.0560, -0.0439, 1.0000},
          {0.0025, 0.5550, -0.4495, 0.0000, -0.0009, -0.4495, -0.5550, 0.0000, -0.7142, 0.0025,
           -0.0008, 0.0000, -0.0333, 0.0467, 1.8520, 1.0000},
      },
      {
          47,
          "lpinkA",
          "sk_lhand",
          {-0.0000, -1.3999, -0.0000, 0.0000, 1.3999, -0.0000, -0.0081, 0.0000, 0.0081, -0.0000,
           1.3999, 0.0000, 1.4727, 2.0548, -0.0881, 1.0000},
          {-0.0000, 0.7142, 0.0041, 0.0000, -0.7142, -0.0000, -0.0000, 0.0000, -0.0000, -0.0041,
           0.7142, 0.0000, 1.4677, -1.0523, 0.0568, 1.0000},
      },
      {
          48,
          "lpinkB",
          "lpinkA",
          {-0.0081, 0.0000, -1.3999, 0.0000, 1.0724, -0.8999, -0.0062, 0.0000, -0.8998, -1.0724,
           0.0052, 0.0000, 1.5637, 2.0548, -0.0887, 1.0000},
          {-0.0041, 0.5471, -0.4591, 0.0000, 0.0000, -0.4591, -0.5471, 0.0000, -0.7142, -0.0031,
           0.0026, 0.0000, -0.0568, 0.0875, 1.8425, 1.0000},
      },
      {
          49,
          "handLStrapBotTop",
          "sk_lhand",
          {-0.2236, 1.3720, -0.1656, 0.0000, 1.3554, 0.1850, -0.2972, 0.0000, -0.2694, -0.2078,
           -1.3580, 0.0000, 1.3214, 2.0560, -0.0921, 1.0000},
          {-0.1141, 0.6915, -0.1374, 0.0000, 0.7000, 0.0944, -0.1060, 0.0000, -0.0845, -0.1516,
           -0.6928, 0.0000, -1.2962, -1.1220, 0.3358, 1.0000},
      },
      {
          50,
          "handLStrapBotMid",
          "handLStrapBotTop",
          {-0.1046, 1.3829, -0.1909, 0.0000, 1.3793, 0.1319, 0.1997, 0.0000, 0.2153, -0.1731,
           -1.3724, 0.0000, 1.4976, 2.0800, -0.1308, 1.0000},
          {-0.0534, 0.7037, 0.1098, 0.0000, 0.7055, 0.0673, -0.0883, 0.0000, -0.0974, 0.1019,
           -0.7002, 0.0000, -1.4004, -1.1807, -0.0723, 1.0000},
      },
      {
          51,
          "rthumA",
          "sk_rhand",
          {0.2024, 1.2882, 0.5093, 0.0000, -0.0475, 0.5209, -1.2985, 0.0000, -1.3844, 0.1705,
           0.1190, 0.0000, -1.3384, 2.0206, 0.0416, 1.0000},
          {0.1032, -0.0242, -0.7063, 0.0000, 0.6572, 0.2657, 0.0869, 0.0000, 0.2598, -0.6625,
           0.0607, 0.0000, -1.2006, -0.5419, -1.1237, 1.0000},
      },
      {
          52,
          "rthumB",
          "rthumA",
          {0.2024, 1.2882, 0.5093, 0.0000, -1.2227, 0.4081, -0.5461, 0.0000, -0.6510, -0.3658,
           1.1841, 0.0000, -1.3365, 1.9998, 0.0935, 1.0000},
          {0.1032, -0.6238, -0.3321, 0.0000, 0.6572, 0.2082, -0.1866, 0.0000, 0.2598, -0.2786,
           0.6041, 0.0000, -1.2006, -1.2241, -0.1271, 1.0000},
      },
      {
          53,
          "rindA",
          "sk_rhand",
          {-0.0162, -0.0081, 1.3998, 0.0000, -1.3999, 0.0038, -0.0162, 0.0000, -0.0037, -1.3999,
           -0.0081, 0.0000, -1.4751, 2.0516, 0.0538, 1.0000},
          {-0.0082, -0.7142, -0.0019, 0.0000, -0.0041, 0.0019, -0.7142, 0.0000, 0.7142, -0.0082,
           -0.0041, 0.0000, -0.0422, -1.0571, 1.4628, 1.0000},
      },
      {
          54,
          "rindB",
          "rindA",
          {-0.0162, -0.0081, 1.3998, 0.0000, -1.0747, -0.8969, -0.0176, 0.0000, 0.8969, -1.0748,
           0.0041, 0.0000, -1.6011, 2.0519, 0.0524, 1.0000},
          {-0.0082, -0.5483, 0.4576, 0.0000, -0.0041, -0.4576, -0.5484, 0.0000, 0.7142, -0.0090,
           0.0021, 0.0000, -0.0422, 0.0615, 1.8579, 1.0000},
      },
      {
          55,
          "rmidA",
          "sk_rhand",
          {-0.0135, 0.0137, 1.3998, 0.0000, -1.3997, -0.0254, -0.0132, 0.0000, 0.0253, -1.3997,
           0.0139, 0.0000, -1.4791, 2.0533, 0.0050, 1.0000},
          {-0.0068, -0.7141, 0.0129, 0.0000, 0.0069, -0.0130, -0.7141, 0.0000, 0.7142, -0.0067,
           0.0071, 0.0000, -0.0281, -1.0295, 1.4854, 1.0000},
      },
      {
          56,
          "rmidB",
          "rmidA",
          {-0.0135, 0.0137, 1.3998, 0.0000, -1.0559, -0.9192, -0.0011, 0.0000, 0.9191, -1.0558,
           0.0192, 0.0000, -1.6050, 2.0510, 0.0038, 1.0000},
          {-0.0068, -0.5387, 0.4689, 0.0000, 0.0069, -0.4689, -0.5386, 0.0000, 0.7142, -0.0006,
           0.0098, 0.0000, -0.0281, 0.0972, 1.8575, 1.0000},
      },
      {
          57,
          "rringA",
          "sk_rhand",
          {0.0079, -0.0111, 1.3999, 0.0000, -1.3997, 0.0276, 0.0082, 0.0000, -0.0277, -1.3996,
           -0.0109, 0.0000, -1.4761, 2.0541, -0.0443, 1.0000},
          {0.0040, -0.7141, -0.0141, 0.0000, -0.0056, 0.0141, -0.7141, 0.0000, 0.7142, 0.0041,
           -0.0056, 0.0000, 0.0493, -1.0829, 1.4458, 1.0000},
      },
      {
          58,
          "rringB",
          "rringA",
          {0.0079, -0.0111, 1.3999, 0.0000, -1.0900, -0.8785, -0.0007, 0.0000, 0.8784, -1.0899,
           -0.0136, 0.0000, -1.5811, 2.0562, -0.0437, 1.0000},
          {0.0040, -0.5561, 0.4482, 0.0000, -0.0056, -0.4482, -0.5561, 0.0000, 0.7142, -0.0003,
           -0.0069, 0.0000, 0.0493, 0.0422, 1.8518, 1.0000},
      },
      {
          59,
          "rpinkA",
          "sk_rhand",
          {-0.0026, 0.0024, 1.3999, 0.0000, -1.3999, 0.0035, -0.0026, 0.0000, -0.0035, -1.3999,
           0.0024, 0.0000, -1.4727, 2.0548, -0.0881, 1.0000},
          {-0.0013, -0.7142, -0.0018, 0.0000, 0.0012, 0.0018, -0.7142, 0.0000, 0.7142, -0.0013,
           0.0012, 0.0000, 0.0583, -1.0558, 1.4651, 1.0000},
      },
      {
          60,
          "rpinkB",
          "rpinkA",
          {-0.0026, 0.0024, 1.3999, 0.0000, -1.0747, -0.8971, -0.0004, 0.0000, 0.8971, -1.0747,
           0.0035, 0.0000, -1.5637, 2.0550, -0.0883, 1.0000},
          {-0.0013, -0.5483, 0.4577, 0.0000, 0.0012, -0.4577, -0.5483, 0.0000, 0.7142, -0.0002,
           0.0018, 0.0000, 0.0583, 0.0832, 1.8428, 1.0000},
      },
      {
          61,
          "handRStrapBotTop",
          "sk_rhand",
          {0.2236, 1.3720, -0.1655, 0.0000, 0.0172, -0.1705, -1.3894, 0.0000, -1.3819, 0.2199,
           -0.0441, 0.0000, -1.3214, 2.0560, -0.0921, 1.0000},
          {0.1141, 0.0088, -0.7050, 0.0000, 0.7000, -0.0869, 0.1122, 0.0000, -0.0844, -0.7089,
           -0.0225, 0.0000, -1.2962, 0.1251, -1.1644, 1.0000},
      },
      {
          62,
          "handRStrapBotMid",
          "handRStrapBotTop",
          {0.3409, 1.3507, -0.1390, 0.0000, -1.3355, 0.3076, -0.2857, 0.0000, -0.2450, 0.2022,
           1.3634, 0.0000, -1.4977, 2.0800, -0.1307, 1.0000},
          {0.1739, -0.6814, -0.1250, 0.0000, 0.6891, 0.1569, 0.1031, 0.0000, -0.0709, -0.1457,
           0.6956, 0.0000, -1.1822, -1.3661, -0.3109, 1.0000},
      },
      {
          63,
          "LshoulderPad",
          "chest",
          {0.2910, -1.3694, 0.0000, 0.0000, 1.3694, 0.2910, 0.0000, 0.0000, 0.0000, 0.0000, 1.3999,
           0.0000, 0.2240, 2.0369, -0.0699, 1.0000},
          {0.1485, 0.6986, 0.0000, 0.0000, -0.6986, 0.1485, 0.0000, 0.0000, 0.0000, 0.0000, 0.7142,
           0.0000, 1.3899, -0.4590, 0.0500, 1.0000},
      },
      {
          64,
          "collarL",
          "chest",
          {-0.5158, 0.3520, -1.2529, 0.0000, 0.9879, -0.7714, -0.6235, 0.0000, -0.8472, -1.1139,
           0.0358, 0.0000, 0.1513, 2.3275, 0.0245, 1.0000},
          {-0.2632, 0.5040, -0.4322, 0.0000, 0.1796, -0.3936, -0.5683, 0.0000, -0.6392, -0.3181,
           0.0182, 0.0000, -0.3624, 0.8476, 1.3878, 1.0000},
      },
      {
          65,
          "collarR",
          "chest",
          {-0.7126, -0.0601, 1.2035, 0.0000, -0.9879, -0.7714, -0.6235, 0.0000, 0.6899, -1.1667,
           0.3502, 0.0000, -0.1513, 2.3275, 0.0245, 1.0000},
          {-0.3635, -0.5040, 0.3520, 0.0000, -0.0306, -0.3935, -0.5952, 0.0000, 0.6140, -0.3181,
           0.1786, 0.0000, 0.0012, 0.8475, 1.4344, 1.0000},
      },
      {
          66,
          "packStrapTop",
          "upper_body",
          {-0.3293, 0.2550, 1.3366, 0.0000, -0.7706, -1.1683, 0.0330, 0.0000, 1.1214, -0.7279,
           0.4152, 0.0000, 0.0033, 2.0020, -0.3729, 1.0000},
          {-0.1680, -0.3931, 0.5721, 0.0000, 0.1301, -0.5960, -0.3714, 0.0000, 0.6819, 0.0168,
           0.2118, 0.0000, -0.0055, 1.2009, 0.8206, 1.0000},
      },
      {
          67,
          "packStrapMid",
          "packStrapTop",
          {1.1622, -0.7701, -0.1266, 0.0000, -0.7549, -1.0515, -0.5332, 0.0000, 0.1982, 0.5109,
           -1.2882, 0.0000, -0.0661, 1.8965, -0.3699, 1.0000},
          {0.5929, -0.3851, 0.1011, 0.0000, -0.3929, -0.5364, 0.2606, 0.0000, -0.0646, -0.2720,
           -0.6572, 0.0000, 0.7605, 0.8913, -0.7309, 1.0000},
      },
      {
          68,
          "Lball",
          "Lankle",
          {0.3475, -1.3554, 0.0431, 0.0000, -0.1893, -0.0044, 1.3871, 0.0000, -1.3428, -0.3502,
           -0.1844, 0.0000, 0.2400, -0.0024, 0.2300, 1.0000},
          {0.1773, -0.0966, -0.6851, 0.0000, -0.6915, -0.0022, -0.1786, 0.0000, 0.0220, 0.7077,
           -0.0940, 0.0000, -0.0493, -0.1396, 0.1856, 1.0000},
      },
      {
          69,
          "LbigToe",
          "Lball",
          {-0.0354, -1.3995, -0.0095, 0.0000, -0.2644, -0.0026, 1.3747, 0.0000, -1.3743, 0.0366,
           -0.2643, 0.0000, 0.1432, 0.0382, 0.2431, 1.0000},
          {-0.0180, -0.1349, -0.7011, 0.0000, -0.7140, -0.0013, 0.0186, 0.0000, -0.0048, 0.7014,
           -0.1348, 0.0000, 0.0310, -0.1511, 0.1325, 1.0000},
      },
      {
          70,
          "Ltoes",
          "Lball",
          {1.3921, -0.0362, 0.1435, 0.0000, -0.1437, -0.0058, 1.3925, 0.0000, -0.0354, -1.3995,
           -0.0095, 0.0000, 0.3541, 0.0330, 0.2205, 1.0000},
          {0.7102, -0.0733, -0.0180, 0.0000, -0.0185, -0.0029, -0.7140, 0.0000, 0.0732, 0.7105,
           -0.0048, 0.0000, -0.2670, -0.1306, 0.0310, 1.0000},
      },
      {
          71,
          "footLStrap",
          "Lankle",
          {-0.3626, 0.4878, 1.2611, 0.0000, 0.8884, 1.0702, -0.1585, 0.0000, -1.0193, 0.7592,
           -0.5868, 0.0000, 0.3946, 0.0528, 0.0735, 1.0000},
          {-0.1850, 0.4532, -0.5200, 0.0000, 0.2488, 0.5460, 0.3873, 0.0000, 0.6434, -0.0808,
           -0.2994, 0.0000, 0.0125, -0.2017, 0.2067, 1.0000},
      },
      {
          72,
          "Rball",
          "Rankle",
          {0.6854, -0.1995, -1.2042, 0.0000, -1.1685, 0.2922, -0.7135, 0.0000, 0.3530, 1.3545,
           -0.0234, 0.0000, -0.2540, -0.0012, 0.2303, 1.0000},
          {0.3497, -0.5961, 0.1801, 0.0000, -0.1018, 0.1491, 0.6910, 0.0000, -0.6144, -0.3640,
           -0.0119, 0.0000, 0.2302, -0.0673, 0.0494, 1.0000},
      },
      {
          73,
          "RbigToe",
          "Rball",
          {1.3834, 0.0334, -0.2122, 0.0000, 0.2125, -0.0100, 1.3837, 0.0000, 0.0315, -1.3995,
           -0.0149, 0.0000, -0.1577, 0.0390, 0.2473, 1.0000},
          {0.7058, 0.1084, 0.0160, 0.0000, 0.0170, -0.0051, -0.7140, 0.0000, -0.1083, 0.7059,
           -0.0076, 0.0000, 0.1374, -0.1573, 0.0323, 1.0000},
      },
      {
          74,
          "Rtoes",
          "Rball",
          {1.3966, 0.0324, -0.0908, 0.0000, 0.0911, -0.0128, 1.3969, 0.0000, 0.0315, -1.3995,
           -0.0149, 0.0000, -0.3675, 0.0346, 0.2167, 1.0000},
          {0.7125, 0.0465, 0.0160, 0.0000, 0.0165, -0.0065, -0.7140, 0.0000, -0.0463, 0.7127,
           -0.0076, 0.0000, 0.2714, -0.1371, 0.0323, 1.0000},
      },
      {
          75,
          "footRStrap",
          "Rankle",
          {-0.5195, -0.1765, 1.2879, 0.0000, 0.8588, -1.0878, 0.1973, 0.0000, 0.9759, 0.8633,
           0.5120, 0.0000, -0.4011, 0.0565, 0.0676, 1.0000},
          {-0.2650, 0.4382, 0.4979, 0.0000, -0.0900, -0.5550, 0.4405, 0.0000, 0.6571, 0.1006,
           0.2612, 0.0000, -0.1457, 0.2003, 0.1571, 1.0000},
      },
      {
          76,
          "pantsRthigh",
          "hips",
          {1.3946, -0.1220, -0.0000, 0.0000, 0.1219, 1.3941, -0.0366, 0.0000, 0.0031, 0.0365,
           1.3995, 0.0000, -0.1400, 1.3019, 0.0000, 1.0000},
          {0.7115, 0.0622, 0.0016, 0.0000, -0.0622, 0.7113, 0.0186, 0.0000, -0.0000, -0.0187,
           0.7140, 0.0000, 0.1806, -0.9174, -0.0240, 1.0000},
      },
      {
          77,
          "pantsRknee",
          "pantsRthigh",
          {-1.3946, 0.1220, 0.0000, 0.0000, 0.1212, 1.3857, 0.1584, 0.0000, 0.0138, 0.1578, -1.3910,
           0.0000, -0.1918, 0.7094, 0.0155, 1.0000},
          {-0.7115, 0.0618, 0.0070, 0.0000, 0.0622, 0.7069, 0.0805, 0.0000, 0.0000, 0.0808, -0.7096,
           0.0000, -0.1806, -0.4909, -0.0447, 1.0000},
      },
      {
          78,
          "pantsLthigh",
          "hips",
          {1.3946, 0.1220, -0.0000, 0.0000, 0.1219, -1.3941, 0.0366, 0.0000, 0.0031, -0.0365,
           -1.3995, 0.0000, 0.1400, 1.3019, 0.0000, 1.0000},
          {0.7115, 0.0622, 0.0016, 0.0000, 0.0622, -0.7113, -0.0186, 0.0000, -0.0000, 0.0186,
           -0.7140, 0.0000, -0.1806, 0.9174, 0.0240, 1.0000},
      },
      {
          79,
          "pantsLknee",
          "pantsLthigh",
          {0.0138, -0.1578, 1.3910, 0.0000, 0.1212, -1.3857, -0.1584, 0.0000, 1.3946, 0.1220,
           -0.0000, 0.0000, 0.1918, 0.7094, 0.0155, 1.0000},
          {0.0070, 0.0618, 0.7115, 0.0000, -0.0805, -0.7069, 0.0622, 0.0000, 0.7096, -0.0808,
           -0.0000, 0.0000, 0.0447, 0.4909, -0.1806, 1.0000},
      },
      {
          80,
          "belt",
          "main",
          {-0.1749, -0.0692, -1.3873, 0.0000, 1.3820, -0.1485, -0.1668, 0.0000, -0.1389, -1.3903,
           0.0869, 0.0000, 0.0773, 1.5848, -0.0864, 1.0000},
          {-0.0892, 0.7051, -0.0708, 0.0000, -0.0353, -0.0757, -0.7093, 0.0000, -0.7078, -0.0851,
           0.0443, 0.0000, 0.0016, 0.0581, 1.1335, 1.0000},
      },
      {
          81,
          "eyeL",
          "neckB",
          {0.0000, 1.4000, 0.0000, 0.0000, 0.3623, -0.0000, 1.3522, 0.0000, 1.3522, -0.0000,
           -0.3623, 0.0000, 0.0840, 2.4852, 0.2290, 1.0000},
          {0.0000, 0.1848, 0.6899, 0.0000, 0.7142, -0.0000, -0.0000, 0.0000, -0.0000, 0.6899,
           -0.1848, 0.0000, -1.7751, -0.1735, -0.0156, 1.0000},
      },
      {
          82,
          "eyeR",
          "neckB",
          {0.0000, 1.4000, 0.0000, 0.0000, -0.3623, 0.0000, 1.3522, 0.0000, 1.3522, -0.0000, 0.3623,
           0.0000, -0.0840, 2.4852, 0.2290, 1.0000},
          {0.0000, -0.1848, 0.6899, 0.0000, 0.7142, 0.0000, -0.0000, 0.0000, 0.0000, 0.6899, 0.1848,
           0.0000, -1.7751, -0.1735, 0.0156, 1.0000},
      },

  };
}

void matrix_multiply(float* result, const float* a, const float* b) {
  for (int i = 0; i < 16; i++) {
    result[i] = 0;
  }
  for (int rx = 0; rx < 4; rx++) {
    for (int cx = 0; cx < 4; cx++) {
      for (int yi = 0; yi < 4; yi++) {
        result[rx + 4 * yi] += a[rx + 4 * cx] * b[cx + 4 * yi];
      }
    }
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

  auto joints_accessor = make_bones_accessor(mverts, model);
  auto weights_accessor = make_weights_accessor(mverts, model);

  for (size_t model_idx = 0; model_idx < level.merc_data.models.size(); model_idx++) {
    const auto& mmodel = level.merc_data.models[model_idx];
    printf("model is %s\n", mmodel.name.c_str());
    if (mmodel.name != "eichar-lod0") {
      continue;
    }

    int node_idx = (int)model.nodes.size();
    auto& node = model.nodes.emplace_back();
    model.scenes.at(0).nodes.push_back(node_idx);
    node.name = mmodel.name;
    int mesh_idx = (int)model.meshes.size();
    auto& mesh = model.meshes.emplace_back();
    mesh.name = node.name;
    node.mesh = mesh_idx;

    if (mmodel.name == "eichar-lod0") {
      node.skin = model.skins.size();
      auto& skin = model.skins.emplace_back();
      auto game_bones = get_jak_bones();
      int n_bones = game_bones.size();
      std::vector<std::vector<int>> children(n_bones);
      std::map<std::string, int> name_to_idx;
      for (const auto& bone : game_bones) {
        name_to_idx[bone.name] = bone.number;
      }
      for (const auto& bone : game_bones) {
        if (bone.parent == "NONE") {
        } else {
          children.at(name_to_idx.at(bone.parent)).push_back(bone.number);
        }
      }
      ASSERT(name_to_idx.size() == n_bones);
      skin.skeleton = model.nodes.size();
      for (int i = 0; i < n_bones; i++) {
        const auto& gbone = game_bones[i];
        skin.joints.push_back(skin.skeleton + i);
        auto& snode = model.nodes.emplace_back();
        snode.name = gbone.name;
        // matrix is w_T_default
        // inv matrix is default_T_w
        // so defaultp_T_w * w_T_child
        // printf("%s is child of %s\n", gbone.name.c_str(), gbone.parent.c_str());
        float matrix[16];

        //        float debug[16];
        //        matrix_multiply(debug, gbone.matrix, gbone.inv_matrix);
        //        for (int r = 0; r < 4; r++) {
        //          for (int c = 0; c < 4; c++) {
        //            printf("%f  ", debug[r * 4 + c]);
        //          }
        //          printf("\n");
        //        }
        //        printf("\n\n");

        if (gbone.parent == "NONE") {
          memcpy(matrix, gbone.matrix, 16 * 4);
        } else {
          auto* pmatrix = game_bones.at(name_to_idx.at(gbone.parent)).inv_matrix;
          auto* cmatrix = gbone.matrix;
          // parent_T_w
          matrix_multiply(matrix, game_bones.at(name_to_idx.at(gbone.parent)).matrix,
                          gbone.inv_matrix);
          // printf("%f %f %f %f\n", pmatrix[12], pmatrix[13], pmatrix[14], pmatrix[15]);
          // printf("%f %f %f %f\n", cmatrix[12], cmatrix[13], cmatrix[14], cmatrix[15]);

          // printf("%f %f %f %f\n", matrix[12], matrix[13], matrix[14], matrix[15]);
        }

        for (int r = 0; r < 4; r++) {
          for (int c = 0; c < 4; c++) {
            snode.matrix.push_back(matrix[r * 4 + c]);
          }
        }
        for (auto child : children.at(i)) {
          snode.children.push_back(skin.skeleton + child);
        }
      }
      ASSERT(skin.skeleton + n_bones == model.nodes.size());
      for (int i = 0; i < n_bones; i++) {
        const auto& node = model.nodes[i + skin.skeleton];
        const auto& matrix = node.matrix;
        ASSERT(matrix.size() == 16);
      }
      skin.inverseBindMatrices = make_inv_matrix_bind_poses(game_bones, model);
    }

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
        prim.attributes["JOINTS_0"] = joints_accessor;
        prim.attributes["WEIGHTS_0"] = weights_accessor;
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