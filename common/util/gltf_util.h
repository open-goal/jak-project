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
  std::map<std::pair<int, int>, int> envmap_textures_by_gltf_id;
};

int texture_pool_add_texture(TexturePool* pool, const tinygltf::Image& tex, int alpha_shift = 1);
int texture_pool_add_envmap_control_texture(TexturePool* pool,
                                            const tinygltf::Model& model,
                                            int rgb_image_id,
                                            int mr_image_id,
                                            bool wrap_w,
                                            bool wrap_h);
int texture_pool_debug_checker(TexturePool* pool);

struct NodeWithTransform {
  int node_idx;
  math::Matrix4f w_T_node;
};

struct TieFullVertex {
  tfrag3::PackedTieVertices::Vertex vertex;
  u16 color_index = 0;
  struct hash {
    std::size_t operator()(const TieFullVertex& x) const;
  };

  bool operator==(const TieFullVertex& other) const {
    return vertex == other.vertex && color_index == other.color_index;
  }
};

template <typename T>
void dedup_vertices(const std::vector<T>& vertices_in,
                    std::vector<T>& vertices_out,
                    std::vector<u32>& old_to_new_out) {
  ASSERT(vertices_out.empty());
  ASSERT(old_to_new_out.empty());
  old_to_new_out.resize(vertices_in.size(), -1);

  std::unordered_map<T, u32, typename T::hash> vtx_to_new;

  for (size_t in_idx = 0; in_idx < vertices_in.size(); in_idx++) {
    auto& vtx = vertices_in[in_idx];
    const auto& lookup = vtx_to_new.find(vtx);
    if (lookup == vtx_to_new.end()) {
      // first time seeing this one
      size_t new_idx = vertices_out.size();
      vertices_out.push_back(vtx);
      old_to_new_out[in_idx] = new_idx;
      vtx_to_new[vtx] = new_idx;
    } else {
      old_to_new_out[in_idx] = lookup->second;
    }
  }
}

std::vector<NodeWithTransform> flatten_nodes_from_all_scenes(const tinygltf::Model& model);

void setup_alpha_from_material(const tinygltf::Material& material, DrawMode* mode);
void setup_draw_mode_from_sampler(const tinygltf::Sampler& sampler, DrawMode* mode);

struct EnvmapSettings {
  int texture_idx = -1;
};

EnvmapSettings envmap_settings_from_gltf(const tinygltf::Material& mat);
bool material_has_envmap(const tinygltf::Material& mat);
bool envmap_is_valid(const tinygltf::Material& mat);

/*!
 * Find the index of the skin for this model. Returns nullopt if there is no skin, the index of the
 * skin if there is a single skin used, or fatal error if there are multiple skins.
 */
std::optional<int> find_single_skin(const tinygltf::Model& model,
                                    const std::vector<NodeWithTransform>& all_nodes);
int get_joint_count(const tinygltf::Model& model, int skin_idx);

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

tfrag3::PackedTimeOfDay pack_time_of_day(const std::vector<math::Vector<u8, 4>>& color_palette);

}  // namespace gltf_util