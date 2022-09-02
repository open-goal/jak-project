#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "common/common_types.h"
#include "common/custom_data/Tfrag3Data.h"
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

std::vector<math::Vector3f> extract_vec3f(const u8* data, u32 count, u32 stride);
std::vector<math::Vector2f> extract_vec2f(const u8* data, u32 count, u32 stride);
std::vector<math::Vector<u8, 4>> extract_color_from_vec4_u16(const u8* data, u32 count, u32 stride);
std::vector<u32> gltf_index_buffer(const tinygltf::Model& model, int indices_idx, u32 index_offset);

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

}  // namespace gltf_util