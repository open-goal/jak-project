#include "fr3_to_gltf.h"

#include <algorithm>
#include <unordered_map>

#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"
#include "common/math/geometry.h"

#include "decompiler/level_extractor/tfrag_tie_fixup.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

namespace {

/*!
 * Remove 4096 meter scaling from a transformation matrix.
 */
math::Matrix4f unscale_translation(const math::Matrix4f& in) {
  auto out = in;
  for (int i = 0; i < 3; i++) {
    out(i, 3) /= 4096.;
  }
  return out;
}

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
 * Convert merc strips. Doesn't assume anything about strips. Output is [effect][draw] format (done
 * for each model)
 */
void unstrip_merc_draws(const std::vector<u32>& stripped_indices,
                        const tfrag3::MercModel& model,
                        std::vector<u32>& unstripped,
                        std::vector<std::vector<u32>>& draw_to_start,
                        std::vector<std::vector<u32>>& draw_to_count) {
  for (auto& effect : model.effects) {
    auto& effect_dts = draw_to_start.emplace_back();
    auto& effect_dtc = draw_to_count.emplace_back();
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
    for (int j = 0; j < 3; j++) {
      floats.push_back(((float)tfrag_tree.colors.read(vertices[i].color_index, time_of_day, j)) /
                       255.f);
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
    for (int j = 0; j < 3; j++) {
      floats.push_back(((float)tie_tree.colors.read(vertices[i].color_index, time_of_day, j)) /
                       255.f);
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
    for (int j = 0; j < 3; j++) {
      floats.push_back(
          ((float)shrub_tree.time_of_day_colors.read(vertices[i].color_index, time_of_day, j)) /
          255.f);
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
                                const tfrag3::MercModel& mmodel,
                                tinygltf::Model& model,
                                std::vector<std::vector<u32>>& draw_to_start,
                                std::vector<std::vector<u32>>& draw_to_count) {
  std::vector<u32> unstripped;
  unstrip_merc_draws(indices, mmodel, unstripped, draw_to_start, draw_to_count);

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
  if (tex_idx < 0) {
    // anim textures, just use default material
    return 0;
  }
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
      indices[i] = vtx.mats[i] ? vtx.mats[i] - 1 : 0;
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

int make_inv_matrix_bind_poses(const std::vector<level_tools::Joint>& joints,
                               tinygltf::Model& model) {
  // first create a buffer:
  int buffer_idx = (int)model.buffers.size();
  auto& buffer = model.buffers.emplace_back();
  buffer.data.resize(sizeof(float) * 16 * joints.size());

  // and fill it
  for (int m = 0; m < (int)joints.size(); m++) {
    auto matrix = unscale_translation(joints[m].bind_pose_T_w);
    for (int i = 0; i < 4; i++) {
      for (int j = 0; j < 4; j++) {
        memcpy(buffer.data.data() + sizeof(float) * (i * 4 + j + m * 16), &matrix(j, i), 4);
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
  accessor.count = joints.size();
  accessor.type = TINYGLTF_TYPE_MAT4;
  return accessor_idx;
}

level_tools::UncompressedJointAnim decompress_anim(const level_tools::ArtJointAnim& art_anim) {
  constexpr float kQuatScale = 0.000030517578125f;
  constexpr float kScaleScale = 0.000244140625f;
  constexpr float kTransScale = 4.f / 4096.f;

  auto read_f32 = [](const u8*& ptr) -> float {
    float v;
    memcpy(&v, ptr, 4);
    ptr += 4;
    return v;
  };
  auto read_s16 = [](const u8*& ptr) -> float {
    s16 v;
    memcpy(&v, ptr, 2);
    ptr += 2;
    return v;
  };

  const auto& ctrl = art_anim.frames;
  const auto& fixed = ctrl.fixed;
  const auto& hdr = fixed.hdr;
  int num_joints = (int)hdr.num_joints;
  int total_frames = (int)ctrl.num_frames;

  level_tools::UncompressedJointAnim out;
  out.name = art_anim.name;
  out.framerate = art_anim.speed > 0.f ? art_anim.speed * 60.f : 30.f;
  out.frames = total_frames;
  out.joints.resize(2 + num_joints);

  auto d64 = (const u8*)fixed.data64.data();
  auto d32 = (const u8*)fixed.data32.data();
  auto d16 = (const u8*)fixed.data16.data();

  if (fixed.mat[0])
    d64 += 64;
  if (fixed.mat[1])
    d64 += 64;

  for (int tqi = 0; tqi < num_joints; tqi++) {
    int ctrl_idx = tqi / 8;
    int ctrl_shift = 4 * (tqi % 8);
    int c = 0xf & (hdr.control_bits[ctrl_idx] >> ctrl_shift);
    auto& joint = out.joints[2 + tqi];

    if (!(c & 0b0001)) {
      math::Vector3f t;
      if (c & 0b1000) {
        t.x() = read_f32(d64) / 4096.f;
        t.y() = read_f32(d64) / 4096.f;
        t.z() = read_f32(d32) / 4096.f;
      } else {
        t.x() = read_s16(d32) * kTransScale;
        t.y() = read_s16(d32) * kTransScale;
        t.z() = read_s16(d16) * kTransScale;
      }
      joint.trans_frames.push_back(t);
    }

    if (!(c & 0b0010)) {
      math::Vector4f q;
      q.x() = read_s16(d64) * kQuatScale;
      q.y() = read_s16(d64) * kQuatScale;
      q.z() = read_s16(d64) * kQuatScale;
      q.w() = read_s16(d64) * kQuatScale;
      joint.quat_frames.push_back(q);
    }

    if (!(c & 0b0100)) {
      math::Vector3f s;
      s.x() = read_s16(d32) * kScaleScale;
      s.y() = read_s16(d32) * kScaleScale;
      s.z() = read_s16(d16) * kScaleScale;
      joint.scale_frames.push_back(s);
    }
  }

  for (int fi = 0; fi < total_frames; fi++) {
    const auto& frame = ctrl.frame[fi];
    const u8* data64 = (const u8*)frame.data64.data();
    const u8* data32 = (const u8*)frame.data32.data();
    const u8* data16 = (const u8*)frame.data16.data();

    if (!fixed.mat[0])
      data64 += sizeof(math::Matrix4f);
    if (!fixed.mat[1])
      data64 += sizeof(math::Matrix4f);

    for (int tqi = 0; tqi < num_joints; tqi++) {
      int ctrl_idx = tqi / 8;
      int ctrl_shift = 4 * (tqi % 8);
      int c = 0xf & (hdr.control_bits[ctrl_idx] >> ctrl_shift);
      auto& joint = out.joints[2 + tqi];

      if (c & 0b0001) {
        math::Vector3f t;
        if (c & 0b1000) {
          t.x() = read_f32(data64) / 4096.f;
          t.y() = read_f32(data64) / 4096.f;
          t.z() = read_f32(data32) / 4096.f;
        } else {
          t.x() = read_s16(data32) * kTransScale;
          t.y() = read_s16(data32) * kTransScale;
          t.z() = read_s16(data16) * kTransScale;
        }
        joint.trans_frames.push_back(t);
      }

      if (c & 0b0010) {
        math::Vector4f q;
        q.x() = read_s16(data64) * kQuatScale;
        q.y() = read_s16(data64) * kQuatScale;
        q.z() = read_s16(data64) * kQuatScale;
        q.w() = read_s16(data64) * kQuatScale;
        joint.quat_frames.push_back(q);
      }

      if (c & 0b0100) {
        math::Vector3f s;
        s.x() = read_s16(data32) * kScaleScale;
        s.y() = read_s16(data32) * kScaleScale;
        s.z() = read_s16(data16) * kScaleScale;
        joint.scale_frames.push_back(s);
      }
    }
  }

  for (int ji = 2; ji < (int)out.joints.size(); ji++) {
    auto& joint = out.joints[ji];
    while ((int)joint.trans_frames.size() < total_frames) {
      if (joint.trans_frames.empty())
        joint.trans_frames.emplace_back(0.f, 0.f, 0.f);
      else
        joint.trans_frames.push_back(joint.trans_frames[0]);
    }
    while ((int)joint.quat_frames.size() < total_frames) {
      if (joint.quat_frames.empty())
        joint.quat_frames.emplace_back(0.f, 0.f, 0.f, 1.f);
      else
        joint.quat_frames.push_back(joint.quat_frames[0]);
    }
    while ((int)joint.scale_frames.size() < total_frames) {
      if (joint.scale_frames.empty())
        joint.scale_frames.emplace_back(1.f, 1.f, 1.f);
      else
        joint.scale_frames.push_back(joint.scale_frames[0]);
    }
  }

  return out;
}

int make_anim_float_accessor(const std::vector<float>& values, tinygltf::Model& model) {
  int buf_idx = (int)model.buffers.size();
  auto& buf = model.buffers.emplace_back();
  buf.data.resize(values.size() * sizeof(float));
  memcpy(buf.data.data(), values.data(), buf.data.size());

  int bv_idx = (int)model.bufferViews.size();
  auto& bv = model.bufferViews.emplace_back();
  bv.buffer = buf_idx;
  bv.byteOffset = 0;
  bv.byteLength = buf.data.size();

  int acc_idx = (int)model.accessors.size();
  auto& acc = model.accessors.emplace_back();
  acc.bufferView = bv_idx;
  acc.byteOffset = 0;
  acc.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  acc.count = (int)values.size();
  acc.type = TINYGLTF_TYPE_SCALAR;
  if (!values.empty()) {
    float mn = values[0], mx = values[0];
    for (float v : values) {
      mn = std::min(mn, v);
      mx = std::max(mx, v);
    }
    acc.minValues = {(double)mn};
    acc.maxValues = {(double)mx};
  }
  return acc_idx;
}

int make_anim_vec3_accessor(const std::vector<math::Vector3f>& values, tinygltf::Model& model) {
  static_assert(sizeof(math::Vector3f) == 3 * sizeof(float));
  int buf_idx = (int)model.buffers.size();
  auto& buf = model.buffers.emplace_back();
  buf.data.resize(values.size() * sizeof(math::Vector3f));
  memcpy(buf.data.data(), values.data(), buf.data.size());

  int bv_idx = (int)model.bufferViews.size();
  auto& bv = model.bufferViews.emplace_back();
  bv.buffer = buf_idx;
  bv.byteOffset = 0;
  bv.byteLength = buf.data.size();

  int acc_idx = (int)model.accessors.size();
  auto& acc = model.accessors.emplace_back();
  acc.bufferView = bv_idx;
  acc.byteOffset = 0;
  acc.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  acc.count = (int)values.size();
  acc.type = TINYGLTF_TYPE_VEC3;
  return acc_idx;
}

int make_anim_vec4_accessor(const std::vector<math::Vector4f>& values, tinygltf::Model& model) {
  static_assert(sizeof(math::Vector4f) == 4 * sizeof(float));
  int buf_idx = (int)model.buffers.size();
  auto& buf = model.buffers.emplace_back();
  buf.data.resize(values.size() * sizeof(math::Vector4f));
  memcpy(buf.data.data(), values.data(), buf.data.size());

  int bv_idx = (int)model.bufferViews.size();
  auto& bv = model.bufferViews.emplace_back();
  bv.buffer = buf_idx;
  bv.byteOffset = 0;
  bv.byteLength = buf.data.size();

  int acc_idx = (int)model.accessors.size();
  auto& acc = model.accessors.emplace_back();
  acc.bufferView = bv_idx;
  acc.byteOffset = 0;
  acc.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
  acc.count = (int)values.size();
  acc.type = TINYGLTF_TYPE_VEC4;
  return acc_idx;
}

void add_animation_to_gltf(const level_tools::UncompressedJointAnim& anim,
                           const tinygltf::Skin& skin,
                           tinygltf::Model& model) {
  if (anim.frames == 0 || anim.joints.size() <= 2)
    return;

  auto& gltf_anim = model.animations.emplace_back();
  gltf_anim.name = anim.name;

  std::vector<float> times(anim.frames);
  for (int i = 0; i < anim.frames; i++)
    times[i] = i / anim.framerate;
  int time_acc = make_anim_float_accessor(times, model);

  int n_anim_joints = (int)anim.joints.size();
  int n_skin_joints = (int)skin.joints.size();
  for (int ji = 2; ji < n_anim_joints && ji < n_skin_joints; ji++) {
    const auto& joint = anim.joints[ji];
    int target_node = skin.joints[ji];

    auto add_channel = [&](int val_acc, const std::string& path) {
      int si = (int)gltf_anim.samplers.size();
      auto& sampler = gltf_anim.samplers.emplace_back();
      sampler.input = time_acc;
      sampler.output = val_acc;
      sampler.interpolation = "LINEAR";
      auto& channel = gltf_anim.channels.emplace_back();
      channel.sampler = si;
      channel.target_node = target_node;
      channel.target_path = path;
    };

    if ((int)joint.trans_frames.size() == anim.frames)
      add_channel(make_anim_vec3_accessor(joint.trans_frames, model), "translation");
    if ((int)joint.quat_frames.size() == anim.frames)
      add_channel(make_anim_vec4_accessor(joint.quat_frames, model), "rotation");
    if ((int)joint.scale_frames.size() == anim.frames)
      add_channel(make_anim_vec3_accessor(joint.scale_frames, model), "scale");
  }
}

void add_merc(const tfrag3::Level& level,
              const std::map<std::string, level_tools::ArtData>& art_data,
              const tfrag3::MercModel& mmodel,
              tinygltf::Model& model,
              std::unordered_map<int, int>& tex_image_map) {
  const auto& mverts = level.merc_data.vertices;

  // create position and uv buffers
  int position_buffer_accessor = make_position_buffer_accessor(mverts, model);
  int texture_buffer_accessor = make_tex_buffer_accessor(mverts, model, 1.f);

  std::vector<std::vector<u32>> draw_to_start, draw_to_count;
  int index_buffer_view = make_merc_index_buffer_view(level.merc_data.indices, mmodel, model,
                                                      draw_to_start, draw_to_count);
  int colors = make_color_buffer_accessor(mverts, model);

  auto joints_accessor = make_bones_accessor(mverts, model);
  auto weights_accessor = make_weights_accessor(mverts, model);

  const auto& art = art_data.find(mmodel.name);
  int node_idx = (int)model.nodes.size();
  auto& node = model.nodes.emplace_back();
  model.scenes.at(0).nodes.push_back(node_idx);
  node.name = mmodel.name;
  int mesh_idx = (int)model.meshes.size();
  auto& mesh = model.meshes.emplace_back();
  mesh.name = node.name;
  node.mesh = mesh_idx;

  if (art != art_data.end() && !art->second.joint_group.empty()) {
    node.skin = model.skins.size();
    auto& skin = model.skins.emplace_back();
    const auto& game_bones = art->second.joint_group;
    int n_bones = game_bones.size();
    std::vector<std::vector<int>> children(n_bones);
    for (size_t i = 0; i < game_bones.size(); i++) {
      if (game_bones[i].parent_idx >= 0) {
        children.at(game_bones[i].parent_idx).push_back(i);
      }
    }
    skin.skeleton = model.nodes.size();
    for (int i = 0; i < n_bones; i++) {
      const auto& gbone = game_bones[i];
      skin.joints.push_back(skin.skeleton + i);
      auto& snode = model.nodes.emplace_back();
      snode.name = gbone.name;

      // bind pose is bind_T_w
      // for glb we want bind_parent_T_bind_child
      // so bindp_T_w * inverse(bindc_T_w)
      math::Matrix4f matrix;
      if (gbone.parent_idx >= 0) {
        matrix = unscale_translation(game_bones.at(gbone.parent_idx).bind_pose_T_w) *
                 inverse(unscale_translation(gbone.bind_pose_T_w));

      } else {
        // I think this value is ignored anyway.
        for (int r = 0; r < 4; r++) {
          for (int c = 0; c < 4; c++) {
            matrix(r, c) = (r == c) ? 1 : 0;
          }
        }
      }

      for (int r = 0; r < 4; r++) {
        for (int c = 0; c < 4; c++) {
          snode.matrix.push_back(matrix(c, r));
        }
      }
      for (auto child : children.at(i)) {
        snode.children.push_back(skin.skeleton + child);
      }
    }
    ASSERT(skin.skeleton + n_bones == (int)model.nodes.size());
    skin.inverseBindMatrices = make_inv_matrix_bind_poses(game_bones, model);
  }

  if (art != art_data.end() && !art->second.anims.empty() && node.skin >= 0 &&
      node.skin < model.skins.size()) {
    const auto& skin = model.skins[node.skin];
    for (const auto& ja : art->second.anims) {
      auto uncompressed = decompress_anim(ja);
      add_animation_to_gltf(uncompressed, skin, model);
    }
  }

  for (size_t effect_idx = 0; effect_idx < mmodel.effects.size(); effect_idx++) {
    const auto& effect = mmodel.effects[effect_idx];
    for (size_t draw_idx = 0; draw_idx < effect.all_draws.size(); draw_idx++) {
      const auto& draw = effect.all_draws[draw_idx];
      auto& prim = mesh.primitives.emplace_back();
      prim.material =
          add_material_for_tex(level, model, draw.tree_tex_id, tex_image_map, draw.mode);
      prim.indices =
          make_index_buffer_accessor(model, draw_to_start[effect_idx][draw_idx],
                                     draw_to_count[effect_idx][draw_idx], index_buffer_view);
      prim.attributes["POSITION"] = position_buffer_accessor;
      prim.attributes["TEXCOORD_0"] = texture_buffer_accessor;
      prim.attributes["COLOR_0"] = colors;
      prim.attributes["JOINTS_0"] = joints_accessor;
      prim.attributes["WEIGHTS_0"] = weights_accessor;
      prim.mode = TINYGLTF_MODE_TRIANGLES;
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

void save_level_foreground_as_gltf(const tfrag3::Level& level,
                                   const std::map<std::string, level_tools::ArtData>& art_data,
                                   const fs::path& glb_path) {
  for (size_t model_idx = 0; model_idx < level.merc_data.models.size(); model_idx++) {
    const auto& mmodel = level.merc_data.models[model_idx];

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

    add_merc(level, art_data, mmodel, model, tex_image_map);

    model.asset.generator = "opengoal";

    auto glb_file = glb_path / fmt::format("{}.glb", mmodel.name);
    file_util::create_dir_if_needed_for_file(glb_file);

    tinygltf::TinyGLTF gltf;
    gltf.WriteGltfSceneToFile(&model, glb_file.string(),
                              true,   // embedImages
                              true,   // embedBuffers
                              true,   // pretty print
                              true);  // write binary
  }
}