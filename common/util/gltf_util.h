#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/common_types.h"
#include "common/custom_data/Tfrag3Data.h"
#include "common/log/log.h"
#include "common/math/Vector.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

namespace gltf_util {

/*!
 * Convert a GLTF index buffer to std::vector<u32>
 */
template <typename T>
std::vector<u32> index_list_to_u32(const u8* data, u32 num_verts, u32 offset, u32 stride) {
  std::vector<u32> result;
  result.reserve(num_verts);
  for (u32 i = 0; i < num_verts; i++) {
    T val;
    memcpy(&val, data, sizeof(T));
    result.push_back(offset + val);
    data += stride;
  }
  return result;
}

struct JointsAndWeights {
  math::Vector<u8, 3> joints = math::Vector<u8, 3>::zero();
  math::Vector<float, 3> weights = math::Vector<float, 3>::zero();
};

std::vector<math::Vector3f> extract_vec3f(const u8* data, u32 count, u32 stride);
std::vector<math::Vector2f> extract_vec2f(const u8* data, u32 count, u32 stride);
std::vector<math::Vector<u8, 4>> extract_color_from_vec4_u16(const u8* data, u32 count, u32 stride);
std::vector<u32> gltf_index_buffer(const tinygltf::Model& model, int indices_idx, u32 index_offset);
std::vector<math::Matrix4f> extract_mat4(const tinygltf::Model& model, int accessor_idx);
std::vector<JointsAndWeights> extract_and_flatten_joints_and_weights(
    const tinygltf::Model& model,
    const tinygltf::Primitive& prim);

struct ExtractedVertices {
  std::vector<tfrag3::PreloadedVertex> vtx;
  std::vector<math::Vector<u8, 4>> vtx_colors;
  std::vector<math::Vector3f> normals;
};

ExtractedVertices gltf_vertices(const tinygltf::Model& model,
                                const std::map<std::string, int>& attributes,
                                const math::Matrix4f& w_T_local,
                                bool get_colors,
                                bool get_normals,
                                const std::string& debug_name);
DrawMode make_default_draw_mode();

struct TexturePool {
  std::unordered_map<std::string, int> textures_by_name;
  std::vector<tfrag3::Texture> textures_by_idx;
};

int texture_pool_add_texture(TexturePool* pool, const tinygltf::Image& tex);
int texture_pool_debug_checker(TexturePool* pool);

struct NodeWithTransform {
  int node_idx;
  math::Matrix4f w_T_node;
};

void dedup_vertices(const std::vector<tfrag3::PreloadedVertex>& vertices_in,
                    std::vector<tfrag3::PreloadedVertex>& vertices_out,
                    std::vector<u32>& old_to_new_out);

std::vector<NodeWithTransform> flatten_nodes_from_all_scenes(const tinygltf::Model& model);

DrawMode draw_mode_from_sampler(const tinygltf::Sampler& sampler);

/*!
 * Find the index of the skin for this model. Returns nullopt if there is no skin, the index of the
 * skin if there is a single skin used, or fatal error if there are multiple skins.
 */
std::optional<int> find_single_skin(const tinygltf::Model& model,
                                    const std::vector<NodeWithTransform>& all_nodes);

template <typename T, int n>
std::vector<math::Vector<T, n>> extract_vec(const tinygltf::Model& model,
                                            int accessor_idx,
                                            int format) {
  const auto& accessor = model.accessors[accessor_idx];
  const auto& buffer_view = model.bufferViews[accessor.bufferView];
  const auto& buffer = model.buffers[buffer_view.buffer];
  const u8* data_ptr = buffer.data.data() + buffer_view.byteOffset + accessor.byteOffset;
  const auto stride = accessor.ByteStride(buffer_view);
  const auto count = accessor.count;

  // ASSERT(buffer_view.target == TINYGLTF_TARGET_ARRAY_BUFFER);  // ??
  if (accessor.componentType != format) {
    lg::die("mismatched format, wanted {} but got {}", format, accessor.componentType);
  }
  ASSERT(accessor.componentType == format);
  switch (n) {
    case 3:
      ASSERT(accessor.type == TINYGLTF_TYPE_VEC3);
      break;
    case 4:
      ASSERT(accessor.type == TINYGLTF_TYPE_VEC4);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  std::vector<math::Vector<T, n>> result(accessor.count);
  for (size_t x = 0; x < count; x++) {
    for (int i = 0; i < n; i++) {
      memcpy(&result[x][i], data_ptr + sizeof(T) * i, sizeof(T));
    }
    data_ptr += stride;
  }
  return result;
}

std::vector<float> extract_floats(const tinygltf::Model& model, int accessor_idx);
math::Matrix4f matrix_from_trs(const math::Vector3f& trans,
                               const math::Vector4f& quat,
                               const math::Vector3f& scale);
}  // namespace gltf_util