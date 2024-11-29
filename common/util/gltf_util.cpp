#include "gltf_util.h"

#include "image_resize.h"

#include "common/log/log.h"

namespace gltf_util {

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
 * Convert a GLTF color buffer (float format) to u8 colors.
 */
std::vector<math::Vector<u8, 4>> extract_color_from_vec4_float(const u8* data,
                                                               u32 count,
                                                               u32 stride) {
  std::vector<math::Vector<u8, 4>> result;
  result.reserve(count);
  for (u32 i = 0; i < count; i++) {
    math::Vector<float, 4> temp;
    memcpy(&temp, data, sizeof(math::Vector<float, 4>));
    data += stride;
    result.emplace_back(temp.x() * 255, temp.y() * 255, temp.z() * 255, temp.w() * 255);
  }
  return result;
}

std::vector<math::Vector<u8, 4>> extract_color_from_vec3_float(const u8* data,
                                                               u32 count,
                                                               u32 stride) {
  std::vector<math::Vector<u8, 4>> result;
  result.reserve(count);
  for (u32 i = 0; i < count; i++) {
    math::Vector<float, 3> temp;
    memcpy(&temp, data, sizeof(math::Vector<float, 3>));
    data += stride;
    result.emplace_back(temp.x() * 255, temp.y() * 255, temp.z() * 255, 255);
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

std::vector<math::Vector<u8, 4>> extract_color_from_vec4_u8(const u8* data, u32 count, u32 stride) {
  std::vector<math::Vector<u8, 4>> result;
  result.reserve(count);
  for (u32 i = 0; i < count; i++) {
    result.push_back(math::Vector<u8, 4>(data[0], data[1], data[2], data[3]));
    data += stride;
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

std::vector<math::Matrix4f> extract_mat4(const tinygltf::Model& model, int accessor_idx) {
  const auto& accessor = model.accessors[accessor_idx];
  const auto& buffer_view = model.bufferViews[accessor.bufferView];
  const auto& buffer = model.buffers[buffer_view.buffer];
  const u8* data_ptr = buffer.data.data() + buffer_view.byteOffset + accessor.byteOffset;
  const auto stride = accessor.ByteStride(buffer_view);
  const auto count = accessor.count;

  // ASSERT(buffer_view.target == TINYGLTF_TARGET_ARRAY_BUFFER);  // ??
  ASSERT(accessor.componentType == TINYGLTF_COMPONENT_TYPE_FLOAT);
  ASSERT(accessor.type == TINYGLTF_TYPE_MAT4);

  std::vector<math::Matrix4f> result(accessor.count);
  for (size_t x = 0; x < count; x++) {
    for (int i = 0; i < 4; i++) {
      for (int j = 0; j < 4; j++) {
        memcpy(&result[x](j, i), data_ptr + sizeof(float) * (i * 4 + j), sizeof(float));
      }
    }
    data_ptr += stride;
  }
  return result;
}

JointsAndWeights convert_per_vertex_data(const math::Vector4f& weights,
                                         const math::Vector<u8, 4>& joints) {
  int discard_idx = -1;
  float discard_weight = 100;
  for (int i = 0; i < 4; i++) {
    if (weights[i] < discard_weight) {
      discard_idx = i;
      discard_weight = weights[i];
    }
  }

  JointsAndWeights ret;
  int dst = 0;
  float sum = 0;
  for (int src = 0; src < 4; src++) {
    if (src == discard_idx) {
      continue;
    }
    // this +1 is to account for align not existing in the gltf.
    ret.joints[dst] = joints[src] + 2;
    ret.weights[dst] = weights[src];
    sum += ret.weights[dst];
    dst++;
  }

  ret.weights /= sum;
  return ret;
}

std::vector<JointsAndWeights> extract_and_flatten_joints_and_weights(
    const tinygltf::Model& model,
    const tinygltf::Primitive& prim) {
  auto weights =
      extract_vec<float, 4>(model, prim.attributes.at("WEIGHTS_0"), TINYGLTF_COMPONENT_TYPE_FLOAT);
  auto joints = extract_vec<u8, 4>(model, prim.attributes.at("JOINTS_0"),
                                   TINYGLTF_COMPONENT_TYPE_UNSIGNED_BYTE);
  std::vector<JointsAndWeights> ret;
  ASSERT(weights.size() == joints.size());
  for (size_t i = 0; i < weights.size(); i++) {
    ret.push_back(convert_per_vertex_data(weights[i], joints[i]));
  }
  return ret;
}

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
      std::vector<math::Vector<u8, 4>> colors;

      switch (attrib_accessor.type) {
        case TINYGLTF_TYPE_VEC4:
          switch (attrib_accessor.componentType) {
            case TINYGLTF_COMPONENT_TYPE_FLOAT:
              colors = extract_color_from_vec4_float(data_ptr, count, byte_stride);
              break;
            case TINYGLTF_COMPONENT_TYPE_UNSIGNED_SHORT:
              colors = extract_color_from_vec4_u16(data_ptr, count, byte_stride);
              break;
            case TINYGLTF_COMPONENT_TYPE_UNSIGNED_BYTE:
              colors = extract_color_from_vec4_u8(data_ptr, count, byte_stride);
              break;
            default:
              lg::die("Unknown type for COLOR_0: {}", attrib_accessor.componentType);
          }
          break;
        case TINYGLTF_TYPE_VEC3:
          switch (attrib_accessor.componentType) {
            case TINYGLTF_COMPONENT_TYPE_FLOAT:
              colors = extract_color_from_vec3_float(data_ptr, count, byte_stride);
              break;
            default:
              lg::die("unkonwn component type for vec3 color {}", attrib_accessor.componentType);
          }
          break;
        default:
          lg::die("unknown attribute type for color {}", attrib_accessor.type);
      }

      vtx_colors.insert(vtx_colors.end(), colors.begin(), colors.end());
    }
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
        // we found that normals aren't normalized if an object is scaled in blender.
        nrm = (w_T_local * nrm4).xyz().normalized();
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

int texture_pool_add_texture(TexturePool* pool, const tinygltf::Image& tex, int alpha_shift) {
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

  // adjust alpha colors for PS2/PC difference
  for (auto& color : tt.data) {
    u32 alpha = color >> 24;
    alpha >>= alpha_shift;
    color &= 0xff'ff'ff;
    color |= (alpha << 24);
  }
  return idx;
}

int texture_pool_add_envmap_control_texture(TexturePool* pool,
                                            const tinygltf::Model& model,
                                            int rgb_image_id,
                                            int mr_image_id,
                                            bool wrap_w,
                                            bool wrap_h) {
  const auto& existing = pool->envmap_textures_by_gltf_id.find({rgb_image_id, mr_image_id});
  if (existing != pool->envmap_textures_by_gltf_id.end()) {
    lg::info("Reusing envmap textures");
    return existing->second;
  }
  const auto& rgb_tex = model.images.at(rgb_image_id);
  const auto& mr_tex = model.images.at(mr_image_id);
  lg::info("new envmap texture {} {}", rgb_tex.name, mr_tex.name);
  ASSERT(rgb_tex.bits == 8);
  ASSERT(rgb_tex.component == 4);
  ASSERT(rgb_tex.pixel_type == TINYGLTF_TEXTURE_TYPE_UNSIGNED_BYTE);

  ASSERT(mr_tex.bits == 8);
  ASSERT(mr_tex.pixel_type == TINYGLTF_TEXTURE_TYPE_UNSIGNED_BYTE);
  ASSERT(mr_tex.component == 4);

  std::vector<u8> resized_mr_tex;
  const u8* mr_src;
  if (rgb_tex.width == mr_tex.width && rgb_tex.height == mr_tex.height) {
    mr_src = mr_tex.image.data();
  } else {
    resized_mr_tex.resize(rgb_tex.width * rgb_tex.height * 4);
    resize_rgba_image(resized_mr_tex.data(), rgb_tex.width, rgb_tex.height, mr_tex.image.data(),
                      mr_tex.width, mr_tex.height, wrap_w, wrap_h);
    mr_src = resized_mr_tex.data();
  }

  size_t idx = pool->textures_by_idx.size();
  pool->envmap_textures_by_gltf_id[{rgb_image_id, mr_image_id}] = idx;
  auto& tt = pool->textures_by_idx.emplace_back();
  tt.w = rgb_tex.width;
  tt.h = rgb_tex.height;
  tt.debug_name = rgb_tex.name;
  tt.debug_tpage_name = "custom-level";
  tt.load_to_pool = false;
  tt.combo_id = 0;  // doesn't matter, not a pool tex
  tt.data.resize(tt.w * tt.h);
  ASSERT(rgb_tex.image.size() >= tt.data.size());
  memcpy(tt.data.data(), rgb_tex.image.data(), tt.data.size() * 4);

  // adjust alpha from metallic channel
  for (size_t i = 0; i < tt.data.size(); i++) {
    u32 rgb = tt.data[i];
    u32 metal = mr_src[4 * i + 2] / 4;
    rgb &= 0xff'ff'ff;
    rgb |= (metal << 24);
    tt.data[i] = rgb;
  }

  return idx;
}

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

math::Matrix4f matrix_from_trs(const math::Vector3f& trans,
                               const math::Vector4f& quat,
                               const math::Vector3f& scale) {
  math::Matrix4f t = affine_translation(trans);
  math::Matrix4f r = affine_rot_qxyzw(quat);
  math::Matrix4f s = affine_scale(scale);
  return t * r * s;
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

void setup_alpha_from_material(const tinygltf::Material& material, DrawMode* mode) {
  if (material.alphaMode == "OPAQUE") {
    mode->disable_ab();
  } else if (material.alphaMode == "MASK") {
    mode->enable_at();
    mode->set_alpha_test(DrawMode::AlphaTest::GEQUAL);
    mode->set_alpha_fail(GsTest::AlphaFail::KEEP);
    mode->set_aref(material.alphaCutoff * 127);
  } else if (material.alphaMode == "BLEND") {
    mode->enable_ab();
    mode->set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    mode->set_depth_write_enable(false);
  } else {
    lg::die("Unknown GLTF alphaMode {}", material.alphaMode);
  }
}

void setup_draw_mode_from_sampler(const tinygltf::Sampler& sampler, DrawMode* mode) {
  if (sampler.magFilter == TINYGLTF_TEXTURE_FILTER_NEAREST) {
    ASSERT(sampler.minFilter == TINYGLTF_TEXTURE_FILTER_NEAREST_MIPMAP_NEAREST);
    mode->set_filt_enable(false);
  } else {
    ASSERT(sampler.minFilter != TINYGLTF_TEXTURE_FILTER_NEAREST);
    mode->set_filt_enable(true);
  }

  switch (sampler.wrapS) {
    case TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE:
      mode->set_clamp_s_enable(true);
      break;
    case TINYGLTF_TEXTURE_WRAP_REPEAT:
      mode->set_clamp_s_enable(false);
      break;
    default:
      ASSERT(false);
  }

  switch (sampler.wrapT) {
    case TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE:
      mode->set_clamp_t_enable(true);
      break;
    case TINYGLTF_TEXTURE_WRAP_REPEAT:
      mode->set_clamp_t_enable(false);
      break;
    default:
      ASSERT(false);
  }
}

EnvmapSettings envmap_settings_from_gltf(const tinygltf::Material& mat) {
  EnvmapSettings settings;

  ASSERT(mat.extensions.contains("KHR_materials_specular"));
  const auto& specular_extension = mat.extensions.at("KHR_materials_specular");
  ASSERT(specular_extension.Has("specularColorTexture"));

  auto& texture = specular_extension.Get("specularColorTexture");
  ASSERT(texture.Has("index"));
  settings.texture_idx = texture.Get("index").Get<int>();
  return settings;
}

bool material_has_envmap(const tinygltf::Material& mat) {
  return mat.extensions.contains("KHR_materials_specular");
}

bool envmap_is_valid(const tinygltf::Material& mat) {
  if (material_has_envmap(mat) && mat.pbrMetallicRoughness.metallicRoughnessTexture.index < 0) {
    lg::warn(fmt::format(
        "Material \"{}\" has specular property set, but is missing a metallic roughness texture, "
        "ignoring envmap!",
        mat.name));
    return false;
  }
  return true;
}

std::optional<int> find_single_skin(const tinygltf::Model& model,
                                    const std::vector<NodeWithTransform>& all_nodes) {
  std::optional<int> skin_index;
  for (const auto& n : all_nodes) {
    const auto& node = model.nodes.at(n.node_idx);
    if (node.skin >= 0) {
      if (skin_index && *skin_index != node.skin) {
        lg::die("GLTF contains multiple skins, but only one skin per actor is supported.");
      }
      skin_index = node.skin;
    }
  }
  return skin_index;
}

int get_joint_count(const tinygltf::Model& model, int skin_idx) {
  const auto& skin = model.skins.at(skin_idx);
  return skin.joints.size();
}

std::vector<float> extract_floats(const tinygltf::Model& model, int accessor_idx) {
  const auto& accessor = model.accessors[accessor_idx];
  const auto& buffer_view = model.bufferViews[accessor.bufferView];
  const auto& buffer = model.buffers[buffer_view.buffer];
  const u8* data_ptr = buffer.data.data() + buffer_view.byteOffset + accessor.byteOffset;
  const auto stride = accessor.ByteStride(buffer_view);
  const auto count = accessor.count;

  // ASSERT(buffer_view.target == TINYGLTF_TARGET_ARRAY_BUFFER);  // ??
  if (accessor.componentType != TINYGLTF_COMPONENT_TYPE_FLOAT) {
    lg::die("mismatched format, wanted {} but got {}", TINYGLTF_COMPONENT_TYPE_FLOAT,
            accessor.componentType);
  }
  ASSERT(accessor.type == TINYGLTF_TYPE_SCALAR);

  std::vector<float> result(accessor.count);
  for (size_t x = 0; x < count; x++) {
    memcpy(&result[x], data_ptr, sizeof(float));
    data_ptr += stride;
  }
  return result;
}

std::size_t TieFullVertex::hash::operator()(const TieFullVertex& x) const {
  return tfrag3::PackedTieVertices::Vertex::hash()(x.vertex) ^ std::hash<u16>()(x.color_index);
}

tfrag3::PackedTimeOfDay pack_time_of_day(const std::vector<math::Vector<u8, 4>>& color_palette) {
  tfrag3::PackedTimeOfDay colors;
  colors.color_count = (color_palette.size() + 3) & (~3);
  colors.data.resize(colors.color_count * 8 * 4);
  for (u32 color_index = 0; color_index < color_palette.size(); color_index++) {
    for (u32 palette = 0; palette < 8; palette++) {
      for (u32 channel = 0; channel < 4; channel++) {
        colors.read(color_index, palette, channel) = color_palette[color_index][channel];
      }
    }
  }
  return colors;
}

}  // namespace gltf_util