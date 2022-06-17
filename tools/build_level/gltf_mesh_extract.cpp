/*!
 * Mesh extraction for GLTF meshes.
 */

#include "gltf_mesh_extract.h"
#include "tools/build_level/color_quantization.h"
#include "third-party/tiny_gltf/tiny_gltf.h"
#include "common/log/log.h"
#include "common/util/Timer.h"

namespace gltf_mesh_extract {

namespace {

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

/*!
 * Convert a GLTF position buffer or similar to std::vector<Vec3f>
 */
std::vector<math::Vector3f> extract_vec3f(const u8* data, u32 count, u32 stride) {
  std::vector<math::Vector3f> result;
  result.reserve(count);
  for (u32 i = 0; i < count; i++) {
    memcpy(&result.emplace_back(), data, sizeof(math::Vector3f));
    data += stride;
  }
  return result;
}

/*!
 * Convert a GLTF color buffer to u8 colors.
 */
std::vector<math::Vector<u8, 4>> extract_color_from_vec4_u16(const u8* data,
                                                             u32 count,
                                                             u32 stride) {
  std::vector<math::Vector<u8, 4>> result;
  result.reserve(count);
  for (u32 i = 0; i < count; i++) {
    math::Vector<u16, 4> temp;
    memcpy(&temp, data, sizeof(math::Vector<u16, 4>));
    data += stride;
    result.emplace_back(temp.x() >> 8, temp.y() >> 8, temp.z() >> 8, temp.w() >> 8);
  }
  return result;
}

/*!
 * Convert a GLTF index buffer
 */
std::vector<u32> gltf_index_buffer(const tinygltf::Model& model,
                                   int indices_idx,
                                   u32 index_offset) {
  const auto& indices_accessor = model.accessors[indices_idx];
  const auto& buffer_view = model.bufferViews[indices_accessor.bufferView];
  const auto& buffer = model.buffers[buffer_view.buffer];
  const auto data_ptr = buffer.data.data() + buffer_view.byteOffset + indices_accessor.byteOffset;
  const auto stride = indices_accessor.ByteStride(buffer_view);
  const auto count = indices_accessor.count;

  switch (indices_accessor.componentType) {
    case TINYGLTF_COMPONENT_TYPE_BYTE:
      return index_list_to_u32<s8>(data_ptr, count, index_offset, stride);
    case TINYGLTF_COMPONENT_TYPE_UNSIGNED_BYTE:
      return index_list_to_u32<u8>(data_ptr, count, index_offset, stride);
    case TINYGLTF_COMPONENT_TYPE_SHORT:
      return index_list_to_u32<s16>(data_ptr, count, index_offset, stride);
    case TINYGLTF_COMPONENT_TYPE_UNSIGNED_SHORT:
      return index_list_to_u32<u16>(data_ptr, count, index_offset, stride);
    case TINYGLTF_COMPONENT_TYPE_INT:
      return index_list_to_u32<s32>(data_ptr, count, index_offset, stride);
    case TINYGLTF_COMPONENT_TYPE_UNSIGNED_INT:
      return index_list_to_u32<u32>(data_ptr, count, index_offset, stride);
    default:
      ASSERT_MSG(false, "unsupported component type");
  }
}

struct ExtractedVertices {
  std::vector<tfrag3::PreloadedVertex> vtx;
  std::vector<math::Vector<u8, 4>> vtx_colors;
};

ExtractedVertices gltf_vertices(const tinygltf::Model& model,
                                const std::map<std::string, int>& attributes,
                                const math::Matrix4f& w_T_local,
                                bool get_colors) {
  std::vector<tfrag3::PreloadedVertex> result;
  std::vector<math::Vector<u8, 4>> vtx_colors;

  {
    const auto& position_attrib = attributes.find("POSITION");
    ASSERT_MSG(position_attrib != attributes.end(), "Did not find position attribute.");

    const auto attrib_accessor = model.accessors[position_attrib->second];
    const auto& buffer_view = model.bufferViews[attrib_accessor.bufferView];
    const auto& buffer = model.buffers[buffer_view.buffer];
    const auto data_ptr = buffer.data.data() + buffer_view.byteOffset + attrib_accessor.byteOffset;
    const auto byte_stride = attrib_accessor.ByteStride(buffer_view);
    const auto count = attrib_accessor.count;

    ASSERT_MSG(attrib_accessor.type == TINYGLTF_TYPE_VEC3, "POSITION wasn't vec3");
    ASSERT_MSG(attrib_accessor.componentType == TINYGLTF_COMPONENT_TYPE_FLOAT,
               "POSITION wasn't float");
    // for (auto& attrib : attributes) {
    // fmt::print("attrib: {}\n", attrib.first);
    //}
    auto mesh_verts = extract_vec3f(data_ptr, count, byte_stride);
    result.reserve(mesh_verts.size());
    for (auto& vert : mesh_verts) {
      auto& new_vert = result.emplace_back();
      math::Vector4f v_in(vert.x(), vert.y(), vert.z(), 1);
      math::Vector4f v_w = w_T_local * v_in;
      new_vert.x = v_w.x() * 4096;
      new_vert.y = v_w.y() * 4096;
      new_vert.z = v_w.z() * 4096;
    }
  }

  if (get_colors) {
    const auto& color_attrib = attributes.find("COLOR_0");
    ASSERT_MSG(color_attrib != attributes.end(), "Did not find color attribute.");

    const auto attrib_accessor = model.accessors[color_attrib->second];
    const auto& buffer_view = model.bufferViews[attrib_accessor.bufferView];
    const auto& buffer = model.buffers[buffer_view.buffer];
    const auto data_ptr = buffer.data.data() + buffer_view.byteOffset + attrib_accessor.byteOffset;
    const auto byte_stride = attrib_accessor.ByteStride(buffer_view);
    const auto count = attrib_accessor.count;

    ASSERT_MSG(attrib_accessor.type == TINYGLTF_TYPE_VEC4, "COLOR_0 wasn't vec4");
    ASSERT_MSG(attrib_accessor.componentType == TINYGLTF_COMPONENT_TYPE_UNSIGNED_SHORT,
               fmt::format("COLOR_0 wasn't float, got {} instead", attrib_accessor.componentType));
    auto colors = extract_color_from_vec4_u16(data_ptr, count, byte_stride);
    vtx_colors.insert(vtx_colors.end(), colors.begin(), colors.end());
  }

  for (auto& v : result) {
    v.color_index = 0;
    v.s = 0;
    v.t = 0;
    v.q_unused = 0;
    v.pad[0] = 0;
    v.pad[1] = 0;
    v.pad[2] = 0;
  }
  // TODO: other properties
  return {result, vtx_colors};
}

DrawMode make_default_draw_mode() {
  DrawMode mode;
  mode.set_depth_write_enable(true);
  mode.set_depth_test(GsTest::ZTest::GEQUAL);
  mode.set_alpha_blend(DrawMode::AlphaBlend::DISABLED);
  mode.set_aref(0);
  mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
  mode.set_clamp_s_enable(false);
  mode.set_clamp_t_enable(false);
  mode.disable_filt();  // for checkerboard...
  mode.enable_tcc();    // ?
  mode.disable_at();
  mode.enable_zt();
  mode.disable_ab();
  mode.disable_decal();
  mode.enable_fog();
  return mode;
}

int texture_pool_debug_checker(TexturePool* pool) {
  const auto& existing = pool->textures_by_name.find("DEBUG_CHECKERBOARD");
  if (existing == pool->textures_by_name.end()) {
    size_t idx = pool->textures_by_idx.size();
    pool->textures_by_name["DEBUG_CHECKERBOARD"] = idx;
    auto& tex = pool->textures_by_idx.emplace_back();
    tex.w = 16;
    tex.h = 16;
    tex.debug_name = "DEBUG_CHECKERBOARD";
    tex.debug_tpage_name = "DEBUG";
    tex.load_to_pool = false;
    tex.combo_id = 0;  // doesn't matter, not a pool tex
    tex.data.resize(16 * 16);
    u32 c0 = 0xa0303030;
    u32 c1 = 0xa0e0e0e0;
    for (int i = 0; i < 16; i++) {
      for (int j = 0; j < 16; j++) {
        tex.data[i * 16 + j] = (((i / 4) & 1) ^ ((j / 4) & 1)) ? c1 : c0;
      }
    }
    return idx;
  } else {
    return existing->second;
  }
}
}  // namespace

math::Matrix4f affine_translation(const math::Vector3f& translation) {
  math::Matrix4f result = math::Matrix4f::identity();
  result(0, 3) = translation[0];
  result(1, 3) = translation[1];
  result(2, 3) = translation[2];
  result(3, 3) = 1;
  return result;
}

math::Matrix4f affine_scale(const math::Vector3f& scale) {
  math::Matrix4f result = math::Matrix4f::zero();
  result(0, 0) = scale[0];
  result(1, 1) = scale[1];
  result(2, 2) = scale[2];
  result(3, 3) = 1;
  return result;
}

math::Matrix4f affine_rot_qxyzw(const math::Vector4f& quat) {
  math::Matrix4f result = math::Matrix4f::zero();
  result(3, 3) = 1;
  result(0, 0) = 1.0 - 2.0 * (quat.y() * quat.y() + quat.z() * quat.z());
  result(0, 1) = 2.0 * (quat.x() * quat.y() - quat.z() * quat.w());
  result(0, 2) = 2.0 * (quat.x() * quat.z() + quat.y() * quat.w());
  result(1, 0) = 2.0 * (quat.x() * quat.y() + quat.z() * quat.w());
  result(1, 1) = 1.0 - 2.0 * (quat.x() * quat.x() + quat.z() * quat.z());
  result(1, 2) = 2.0 * (quat.y() * quat.z() - quat.x() * quat.w());
  result(2, 0) = 2.0 * (quat.x() * quat.z() - quat.y() * quat.w());
  result(2, 1) = 2.0 * (quat.y() * quat.z() + quat.x() * quat.w());
  result(2, 2) = 1.0 - 2.0 * (quat.x() * quat.x() + quat.y() * quat.y());
  return result;
}

math::Vector3f vector3f_from_gltf(const std::vector<double>& in) {
  ASSERT(in.size() == 3);
  return math::Vector3f{in[0], in[1], in[2]};
}

math::Vector4f vector4f_from_gltf(const std::vector<double>& in) {
  ASSERT(in.size() == 4);
  return math::Vector4f{in[0], in[1], in[2], in[3]};
}

math::Matrix4f matrix_from_node(const tinygltf::Node& node) {
  if (!node.matrix.empty()) {
    math::Matrix4f result;
    for (int i = 0; i < 16; i++) {
      result.data()[i] = node.matrix[i];
    }
    return result;
  } else {
    // from trs
    math::Matrix4f t, r, s;
    if (!node.translation.empty()) {
      t = affine_translation(vector3f_from_gltf(node.translation));
    } else {
      t = math::Matrix4f::identity();
    }

    if (!node.rotation.empty()) {
      r = affine_rot_qxyzw(vector4f_from_gltf(node.rotation));
    } else {
      r = math::Matrix4f::identity();
    }

    if (!node.scale.empty()) {
      s = affine_scale(vector3f_from_gltf(node.scale));
    } else {
      s = math::Matrix4f::identity();
    }

    return t * r * s;
  }
}

struct NodeWithTransform {
  int node_idx;
  math::Matrix4f w_T_node;
};

/*!
 * Recursively walk the tree of nodes, flatten, and compute w_T_node for each.
 */
void node_find_helper(const tinygltf::Model& model,
                      const math::Matrix4f& w_T_parent,
                      int node_idx,
                      std::vector<NodeWithTransform>* out) {
  const auto& node = model.nodes.at(node_idx);
  math::Matrix4f w_T_node = w_T_parent * matrix_from_node(node);
  out->push_back({node_idx, w_T_node});
  for (auto& child : node.children) {
    node_find_helper(model, w_T_node, child, out);
  }
}

std::vector<NodeWithTransform> flatten_nodes_from_all_scenes(const tinygltf::Model& model) {
  std::vector<NodeWithTransform> out;
  for (auto& scene : model.scenes) {
    for (auto& nidx : scene.nodes) {
      math::Matrix4f identity = math::Matrix4f::identity();
      node_find_helper(model, identity, nidx, &out);
    }
  }
  return out;
}

void dedup_vertices(const std::vector<tfrag3::PreloadedVertex>& vertices_in,
                    std::vector<tfrag3::PreloadedVertex>& vertices_out,
                    std::vector<u32>& old_to_new_out) {
  ASSERT(vertices_out.empty());
  ASSERT(old_to_new_out.empty());
  old_to_new_out.resize(vertices_in.size(), -1);

  std::unordered_map<tfrag3::PreloadedVertex, u32, tfrag3::PreloadedVertex::hash> vtx_to_new;

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

void dedup_vertices(Output& data) {
  Timer timer;
  size_t original_size = data.vertices.size();
  std::vector<tfrag3::PreloadedVertex> new_verts;
  std::vector<u32> old_to_new;

  dedup_vertices(data.vertices, new_verts, old_to_new);
  data.vertices = std::move(new_verts);

  for (auto& draw : data.strip_draws) {
    ASSERT(draw.runs.empty());  // not supported yet
    for (auto& idx : draw.plain_indices) {
      idx = old_to_new.at(idx);
    }
  }

  lg::info("Deduplication took {:.2f} ms, {} -> {} ({:.2f} %)", timer.getMs(), original_size,
           data.vertices.size(), 100.f * data.vertices.size() / original_size);
}

void extract(const Input& in, Output& out) {
  lg::info("Reading gltf mesh: {}", in.filename);
  Timer read_timer;
  tinygltf::TinyGLTF loader;
  tinygltf::Model model;
  std::string err, warn;
  bool res = loader.LoadBinaryFromFile(&model, &err, &warn, in.filename);
  ASSERT_MSG(warn.empty(), warn.c_str());
  ASSERT_MSG(err.empty(), err.c_str());
  ASSERT_MSG(res, "Failed to load GLTF file!");

  std::vector<math::Vector<u8, 4>> all_vtx_colors;

  ASSERT(out.vertices.empty());

  int mesh_count = 0;
  int prim_count = 0;
  auto all_nodes = flatten_nodes_from_all_scenes(model);
  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_index_buffer(model, prim.indices, out.vertices.size());
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts = gltf_vertices(model, prim.attributes, n.w_T_node, in.get_colors);
        out.vertices.insert(out.vertices.end(), verts.vtx.begin(), verts.vtx.end());
        if (in.get_colors) {
          all_vtx_colors.insert(all_vtx_colors.end(), verts.vtx_colors.begin(),
                                verts.vtx_colors.end());
        }

        // TODO: just putting it all in one material
        auto& draw = out.strip_draws.emplace_back();
        draw.mode = make_default_draw_mode();
        draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);
        draw.num_triangles = prim_indices.size() / 3;
        auto& grp = draw.vis_groups.emplace_back();
        grp.num_inds = prim_indices.size();
        grp.num_tris = draw.num_triangles;
        grp.vis_idx_in_pc_bvh = UINT32_MAX;
        draw.plain_indices = std::move(prim_indices);
      }
    }
  }

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.vertices.size());
  lg::info("Took {:.2f} ms", read_timer.getMs());

  if (in.get_colors) {
    Timer quantize_timer;
    auto quantized = quantize_colors_octree(all_vtx_colors, 1024);
    for (size_t i = 0; i < out.vertices.size(); i++) {
      out.vertices[i].color_index = quantized.vtx_to_color[i];
    }
    out.color_palette = std::move(quantized.final_colors);
    lg::info("Color palette generation took {:.2f} ms", quantize_timer.getMs());
  }

  dedup_vertices(out);
}
}  // namespace gltf_mesh_extract
