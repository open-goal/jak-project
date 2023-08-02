/*!
 * Mesh extraction for GLTF meshes.
 */

#include "gltf_mesh_extract.h"

#include <optional>

#include "common/log/log.h"
#include "common/math/geometry.h"
#include "common/util/Timer.h"

#include "goalc/build_level/color_quantization.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

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

std::vector<math::Vector2f> extract_vec2f(const u8* data, u32 count, u32 stride) {
  std::vector<math::Vector2f> result;
  result.reserve(count);
  for (u32 i = 0; i < count; i++) {
    memcpy(&result.emplace_back(), data, sizeof(math::Vector2f));
    data += stride;
  }
  return result;
}

/*!
 * Convert a GLTF color buffer (u16 format) to u8 colors.
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
  std::vector<math::Vector3f> normals;
};

/*!
 * Extract positions, colors, and normals from a mesh.
 */
ExtractedVertices gltf_vertices(const tinygltf::Model& model,
                                const std::map<std::string, int>& attributes,
                                const math::Matrix4f& w_T_local,
                                bool get_colors,
                                bool get_normals,
                                const std::string& debug_name) {
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
    // lg::print("attrib: {}\n", attrib.first);
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
    if (color_attrib == attributes.end()) {
      lg::error("Mesh {} didn't have any colors, using white", debug_name);
      for (size_t i = 0; i < result.size(); i++) {
        vtx_colors.emplace_back(0x80, 0x80, 0x80, 0xff);
      }
    } else {
      const auto attrib_accessor = model.accessors[color_attrib->second];
      const auto& buffer_view = model.bufferViews[attrib_accessor.bufferView];
      const auto& buffer = model.buffers[buffer_view.buffer];
      const auto data_ptr =
          buffer.data.data() + buffer_view.byteOffset + attrib_accessor.byteOffset;
      const auto byte_stride = attrib_accessor.ByteStride(buffer_view);
      const auto count = attrib_accessor.count;

      ASSERT_MSG(attrib_accessor.type == TINYGLTF_TYPE_VEC4, "COLOR_0 wasn't vec4");
      ASSERT_MSG(
          attrib_accessor.componentType == TINYGLTF_COMPONENT_TYPE_UNSIGNED_SHORT,
          fmt::format("COLOR_0 wasn't float, got {} instead", attrib_accessor.componentType));
      auto colors = extract_color_from_vec4_u16(data_ptr, count, byte_stride);
      vtx_colors.insert(vtx_colors.end(), colors.begin(), colors.end());
    }

    // ASSERT_MSG(color_attrib != attributes.end(), "Did not find color attribute.");
  }

  bool got_texture = false;
  {
    const auto& texcoord_attrib = attributes.find("TEXCOORD_0");
    if (texcoord_attrib != attributes.end()) {
      const auto attrib_accessor = model.accessors[texcoord_attrib->second];
      const auto& buffer_view = model.bufferViews[attrib_accessor.bufferView];
      const auto& buffer = model.buffers[buffer_view.buffer];
      const auto data_ptr =
          buffer.data.data() + buffer_view.byteOffset + attrib_accessor.byteOffset;
      const auto byte_stride = attrib_accessor.ByteStride(buffer_view);
      const auto count = attrib_accessor.count;

      ASSERT_MSG(attrib_accessor.type == TINYGLTF_TYPE_VEC2, "TEXCOORD wasn't vec2");
      ASSERT_MSG(attrib_accessor.componentType == TINYGLTF_COMPONENT_TYPE_FLOAT,
                 "TEXCOORD wasn't float");
      auto mesh_verts = extract_vec2f(data_ptr, count, byte_stride);
      ASSERT(mesh_verts.size() == result.size());
      got_texture = true;
      for (size_t i = 0; i < mesh_verts.size(); i++) {
        result[i].s = mesh_verts[i].x();
        result[i].t = mesh_verts[i].y();
      }
    } else {
      if (!get_normals) {
        // don't warn if we're just getting collision
        lg::warn("No texcoord attribute for mesh: {}", debug_name);
      }
    }
  }

  std::vector<math::Vector3f> normals;
  if (get_normals) {
    const auto& normal_attrib = attributes.find("NORMAL");
    if (normal_attrib != attributes.end()) {
      const auto attrib_accessor = model.accessors[normal_attrib->second];
      const auto& buffer_view = model.bufferViews[attrib_accessor.bufferView];
      const auto& buffer = model.buffers[buffer_view.buffer];
      const auto data_ptr =
          buffer.data.data() + buffer_view.byteOffset + attrib_accessor.byteOffset;
      const auto byte_stride = attrib_accessor.ByteStride(buffer_view);
      const auto count = attrib_accessor.count;

      ASSERT_MSG(attrib_accessor.type == TINYGLTF_TYPE_VEC3, "NORMAL wasn't vec3");
      ASSERT_MSG(attrib_accessor.componentType == TINYGLTF_COMPONENT_TYPE_FLOAT,
                 "NORMAL wasn't float");
      normals = extract_vec3f(data_ptr, count, byte_stride);
      for (auto& nrm : normals) {
        math::Vector4f nrm4(nrm.x(), nrm.y(), nrm.z(), 0.f);
        nrm = (w_T_local * nrm4).xyz();
      }
      ASSERT(normals.size() == result.size());
    } else {
      lg::error("No NORMAL attribute for mesh: {}", debug_name);
    }
  }

  for (auto& v : result) {
    v.color_index = 0;
    if (!got_texture) {
      v.s = 0;
      v.t = 0;
    }
  }
  // TODO: other properties
  return {result, vtx_colors, normals};
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

int texture_pool_add_texture(TexturePool* pool, const tinygltf::Image& tex) {
  const auto& existing = pool->textures_by_name.find(tex.name);
  if (existing != pool->textures_by_name.end()) {
    lg::info("Reusing image: {}", tex.name);
    return existing->second;
  } else {
    lg::info("adding new texture: {}, size {} kB", tex.name, tex.width * tex.height * 4 / 1024);
  }

  ASSERT(tex.bits == 8);
  ASSERT(tex.component == 4);
  ASSERT(tex.pixel_type == TINYGLTF_TEXTURE_TYPE_UNSIGNED_BYTE);

  size_t idx = pool->textures_by_idx.size();
  pool->textures_by_name[tex.name] = idx;
  auto& tt = pool->textures_by_idx.emplace_back();
  tt.w = tex.width;
  tt.h = tex.height;
  tt.debug_name = tex.name;
  tt.debug_tpage_name = "custom-level";
  tt.load_to_pool = false;
  tt.combo_id = 0;  // doesn't matter, not a pool tex
  tt.data.resize(tt.w * tt.h);
  ASSERT(tex.image.size() >= tt.data.size());
  memcpy(tt.data.data(), tex.image.data(), tt.data.size() * 4);
  return idx;
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

void dedup_vertices(TfragOutput& data) {
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

DrawMode draw_mode_from_sampler(const tinygltf::Sampler& sampler) {
  DrawMode mode = make_default_draw_mode();
  if (sampler.magFilter == TINYGLTF_TEXTURE_FILTER_NEAREST) {
    ASSERT(sampler.minFilter == TINYGLTF_TEXTURE_FILTER_NEAREST);
    mode.set_filt_enable(false);
  } else {
    ASSERT(sampler.minFilter != TINYGLTF_TEXTURE_FILTER_NEAREST);
    mode.set_filt_enable(true);
  }

  switch (sampler.wrapS) {
    case TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE:
      mode.set_clamp_s_enable(true);
      break;
    case TINYGLTF_TEXTURE_WRAP_REPEAT:
      mode.set_clamp_s_enable(false);
      break;
    default:
      ASSERT(false);
  }

  switch (sampler.wrapT) {
    case TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE:
      mode.set_clamp_t_enable(true);
      break;
    case TINYGLTF_TEXTURE_WRAP_REPEAT:
      mode.set_clamp_t_enable(false);
      break;
    default:
      ASSERT(false);
  }

  return mode;
}

void extract(const Input& in,
             TfragOutput& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes) {
  std::vector<math::Vector<u8, 4>> all_vtx_colors;
  ASSERT(out.vertices.empty());
  std::map<int, tfrag3::StripDraw> draw_by_material;
  int mesh_count = 0;
  int prim_count = 0;

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.extras.Has("set_invisible") && node.extras.Get("set_invisible").Get<int>()) {
      continue;
    }
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
        if (prim.material >= 0 && model.materials[prim.material].extras.Has("set_invisible") &&
            model.materials[prim.material].extras.Get("set_invisible").Get<int>()) {
          continue;
        }
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_index_buffer(model, prim.indices, out.vertices.size());
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts =
            gltf_vertices(model, prim.attributes, n.w_T_node, in.get_colors, false, mesh.name);
        out.vertices.insert(out.vertices.end(), verts.vtx.begin(), verts.vtx.end());
        if (in.get_colors) {
          all_vtx_colors.insert(all_vtx_colors.end(), verts.vtx_colors.begin(),
                                verts.vtx_colors.end());
          ASSERT(all_vtx_colors.size() == out.vertices.size());
        }

        // TODO: just putting it all in one material
        auto& draw = draw_by_material[prim.material];
        draw.mode = make_default_draw_mode();                        // todo rm
        draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);  // todo rm
        draw.num_triangles += prim_indices.size() / 3;
        if (draw.vis_groups.empty()) {
          auto& grp = draw.vis_groups.emplace_back();
          grp.num_inds += prim_indices.size();
          grp.num_tris += draw.num_triangles;
          grp.vis_idx_in_pc_bvh = UINT16_MAX;
        } else {
          auto& grp = draw.vis_groups.back();
          grp.num_inds += prim_indices.size();
          grp.num_tris += draw.num_triangles;
          grp.vis_idx_in_pc_bvh = UINT16_MAX;
        }

        draw.plain_indices.insert(draw.plain_indices.end(), prim_indices.begin(),
                                  prim_indices.end());
      }
    }
  }

  for (const auto& [mat_idx, d_] : draw_by_material) {
    out.strip_draws.push_back(d_);
    auto& draw = out.strip_draws.back();
    draw.mode = make_default_draw_mode();

    if (mat_idx == -1) {
      lg::warn("Draw had a material index of -1, using default texture.");
      draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);
      continue;
    }
    const auto& mat = model.materials[mat_idx];
    int tex_idx = mat.pbrMetallicRoughness.baseColorTexture.index;
    if (tex_idx == -1) {
      lg::warn("Material {} has no texture, using default texture.", mat.name);
      draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);
      continue;
    }

    const auto& tex = model.textures[tex_idx];
    ASSERT(tex.sampler >= 0);
    ASSERT(tex.source >= 0);
    draw.mode = draw_mode_from_sampler(model.samplers.at(tex.sampler));

    const auto& img = model.images[tex.source];
    draw.tree_tex_id = texture_pool_add_texture(in.tex_pool, img);
  }
  lg::info("total of {} unique materials", out.strip_draws.size());

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.vertices.size());

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

std::optional<std::vector<CollideFace>> subdivide_face_if_needed(CollideFace face_in) {
  math::Vector3f v_min = face_in.v[0];
  v_min.min_in_place(face_in.v[1]);
  v_min.min_in_place(face_in.v[2]);
  v_min -= 16.f;
  bool needs_subdiv = false;
  for (auto& vert : face_in.v) {
    if ((vert - v_min).squared_length() > 154.f * 154.f * 4096.f * 4096.f) {
      needs_subdiv = true;
      break;
    }
  }

  if (needs_subdiv) {
    math::Vector3f a = (face_in.v[0] + face_in.v[1]) * 0.5f;
    math::Vector3f b = (face_in.v[1] + face_in.v[2]) * 0.5f;
    math::Vector3f c = (face_in.v[2] + face_in.v[0]) * 0.5f;
    math::Vector3f v0 = face_in.v[0];
    math::Vector3f v1 = face_in.v[1];
    math::Vector3f v2 = face_in.v[2];
    CollideFace fs[4];
    fs[0].v[0] = v0;
    fs[0].v[1] = a;
    fs[0].v[2] = c;
    fs[0].bsphere = math::bsphere_of_triangle(face_in.v);

    fs[1].v[0] = a;
    fs[1].v[1] = v1;
    fs[1].v[2] = b;
    fs[1].bsphere = math::bsphere_of_triangle(fs[1].v);
    fs[1].pat = face_in.pat;

    fs[2].v[0] = a;
    fs[2].v[1] = b;
    fs[2].v[2] = c;
    fs[2].bsphere = math::bsphere_of_triangle(fs[2].v);
    fs[2].pat = face_in.pat;

    fs[3].v[0] = b;
    fs[3].v[1] = v2;
    fs[3].v[2] = c;
    fs[3].bsphere = math::bsphere_of_triangle(fs[3].v);
    fs[3].pat = face_in.pat;

    std::vector<CollideFace> result;
    for (auto f : fs) {
      auto next_faces = subdivide_face_if_needed(f);
      if (next_faces) {
        result.insert(result.end(), next_faces->begin(), next_faces->end());
      } else {
        result.push_back(f);
      }
    }
    return result;
  } else {
    return std::nullopt;
  }
}

struct PatResult {
  bool set = false;
  bool ignore = false;
  PatSurface pat;
};

PatResult custom_props_to_pat(const tinygltf::Value& val, const std::string& /*debug_name*/) {
  PatResult result;
  if (!val.IsObject() || !val.Has("set_collision") || !val.Get("set_collision").Get<int>()) {
    // unset.
    result.set = false;
    return result;
  }

  result.set = true;

  if (val.Get("ignore").Get<int>()) {
    result.ignore = true;
    return result;
  }
  result.ignore = false;

  int mat = val.Get("collide_material").Get<int>();
  ASSERT(mat < (int)PatSurface::Material::MAX_MATERIAL);
  result.pat.set_material(PatSurface::Material(mat));

  int evt = val.Get("collide_event").Get<int>();
  ASSERT(evt < (int)PatSurface::Event::MAX_EVENT);
  result.pat.set_event(PatSurface::Event(evt));

  if (val.Get("nolineofsight").Get<int>()) {
    result.pat.set_nolineofsight(true);
  }

  if (val.Get("noedge").Get<int>()) {
    result.pat.set_noedge(true);
  }

  if (val.Has("collide_mode")) {
    int mode = val.Get("collide_mode").Get<int>();
    ASSERT(mode < (int)PatSurface::Mode::MAX_MODE);
    result.pat.set_mode(PatSurface::Mode(mode));
  }

  if (val.Get("nocamera").Get<int>()) {
    result.pat.set_nocamera(true);
  }

  if (val.Get("noentity").Get<int>()) {
    result.pat.set_noentity(true);
  }

  return result;
}

void extract(const Input& in,
             CollideOutput& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes) {
  [[maybe_unused]] int mesh_count = 0;
  [[maybe_unused]] int prim_count = 0;
  int suspicious_faces = 0;

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    PatResult mesh_default_collide = custom_props_to_pat(node.extras, node.name);
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
        // get material
        const auto& mat_idx = prim.material;
        PatResult pat = mesh_default_collide;
        if (mat_idx != -1) {
          const auto& mat = model.materials[mat_idx];
          auto mat_pat = custom_props_to_pat(mat.extras, mat.name);
          if (mat_pat.set) {
            pat = mat_pat;
          }
        }

        if (pat.set && pat.ignore) {
          continue;  // skip, no collide here
        }
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_index_buffer(model, prim.indices, 0);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts = gltf_vertices(model, prim.attributes, n.w_T_node, false, true, mesh.name);

        for (size_t iidx = 0; iidx < prim_indices.size(); iidx += 3) {
          CollideFace face;

          // get the positions
          for (int j = 0; j < 3; j++) {
            auto& vtx = verts.vtx.at(prim_indices.at(iidx + j));
            face.v[j].x() = vtx.x;
            face.v[j].y() = vtx.y;
            face.v[j].z() = vtx.z;
          }

          // now face normal
          math::Vector3f face_normal =
              (face.v[2] - face.v[0]).cross(face.v[1] - face.v[0]).normalized();

          float dots[3];
          for (int j = 0; j < 3; j++) {
            dots[j] = face_normal.dot(verts.normals.at(prim_indices.at(iidx + j)).normalized());
          }

          if (dots[0] > 1e-3 && dots[1] > 1e-3 && dots[2] > 1e-3) {
            suspicious_faces++;
            auto temp = face.v[2];
            face.v[2] = face.v[1];
            face.v[1] = temp;
          }

          face.bsphere = math::bsphere_of_triangle(face.v);
          face.bsphere.w() += 1e-1 * 5;
          for (int j = 0; j < 3; j++) {
            float output_dist = face.bsphere.w() - (face.bsphere.xyz() - face.v[j]).length();
            if (output_dist < 0) {
              lg::print("{}\n", output_dist);
              lg::print("BAD:\n{}\n{}\n{}\n", face.v[0].to_string_aligned(),
                        face.v[1].to_string_aligned(), face.v[2].to_string_aligned());
              lg::print("bsphere: {}\n", face.bsphere.to_string_aligned());
            }
          }
          face.pat = pat.pat;
          out.faces.push_back(face);
        }
      }
    }
  }

  std::vector<CollideFace> fixed_faces;
  int fix_count = 0;
  for (auto& face : out.faces) {
    auto try_fix = subdivide_face_if_needed(face);
    if (try_fix) {
      fix_count++;
      fixed_faces.insert(fixed_faces.end(), try_fix->begin(), try_fix->end());
    } else {
      fixed_faces.push_back(face);
    }
  }

  if (in.double_sided_collide) {
    size_t os = fixed_faces.size();
    for (size_t i = 0; i < os; i++) {
      auto f0 = fixed_faces.at(i);
      std::swap(f0.v[0], f0.v[1]);
      fixed_faces.push_back(f0);
    }
  }

  out.faces = std::move(fixed_faces);

  if (in.auto_wall_enable) {
    lg::info("automatically detecting walls with angle {}", in.auto_wall_angle);
    int wall_count = 0;
    float wall_cos = std::cos(in.auto_wall_angle * 2.f * 3.14159 / 360.f);
    for (auto& face : out.faces) {
      math::Vector3f face_normal =
          (face.v[1] - face.v[0]).cross(face.v[2] - face.v[0]).normalized();
      if (face_normal[1] < wall_cos) {
        face.pat.set_mode(PatSurface::Mode::WALL);
        wall_count++;
      }
    }
    lg::info("automatic wall: {}/{} converted to walls", wall_count, out.faces.size());
  }

  lg::info("{} out of {} faces appeared to have wrong orientation and were flipped",
           suspicious_faces, out.faces.size());
  lg::info("{} faces were too big and were subdivided", fix_count);
  // lg::info("Collision extract{} {}", mesh_count, prim_count);
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
  auto all_nodes = flatten_nodes_from_all_scenes(model);
  extract(in, out.tfrag, model, all_nodes);
  extract(in, out.collide, model, all_nodes);
  lg::info("GLTF total took {:.2f} ms", read_timer.getMs());
}
}  // namespace gltf_mesh_extract
