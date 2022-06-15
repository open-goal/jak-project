#include "gltf_mesh_extract.h"

#include "third-party/tiny_gltf/tiny_gltf.h"
#include "common/log/log.h"

namespace gltf_mesh_extract {

namespace {
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

std::vector<math::Vector3f> extract_vec3f(const u8* data, u32 count, u32 stride) {
  std::vector<math::Vector3f> result;
  result.reserve(count);
  for (u32 i = 0; i < count; i++) {
    memcpy(&result.emplace_back(), data, sizeof(math::Vector3f));
    data += stride;
  }
  return result;
}

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

struct QuantizedColors {
  std::vector<math::Vector<u8, 4>> final_colors;
  std::vector<u32> vtx_to_color;
};

QuantizedColors quantize_colors_dumb(const std::vector<math::Vector<u8, 4>>& in) {
  QuantizedColors result;
  std::unordered_map<u64, u32> color_to_slot;
  for (auto& vtx : in) {
    u64 key;
    memcpy(&key, vtx.data(), sizeof(u64));
    const auto& existing = color_to_slot.find(key);
    if (existing == color_to_slot.end()) {
      auto cidx = result.final_colors.size();
      result.vtx_to_color.push_back(cidx);
      color_to_slot[key] = cidx;
      result.final_colors.push_back(vtx);
    } else {
      result.vtx_to_color.push_back(existing->second);
    }
  }
  fmt::print("quantize_colors_dumb: {} -> {}\n", in.size(), result.final_colors.size());
  ASSERT(result.final_colors.size() < 8192);
  return result;
}

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
                                const std::map<std::string, int>& attributes) {
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
    for (auto& attrib : attributes) {
      fmt::print("attrib: {}\n", attrib.first);
    }
    auto mesh_verts = extract_vec3f(data_ptr, count, byte_stride);
    result.reserve(mesh_verts.size());
    for (auto& vert : mesh_verts) {
      auto& new_vert = result.emplace_back();
      new_vert.x = vert.x() * 4096;
      new_vert.y = vert.y() * 4096;
      new_vert.z = vert.z() * 4096;
    }
  }

  {
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
    v.q = 0;
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

void extract(const Input& in, Output& out) {
  lg::info("Reading gltf mesh: {}", in.filename);
  tinygltf::TinyGLTF loader;
  tinygltf::Model model;
  std::string err, warn;
  bool res = loader.LoadBinaryFromFile(&model, &err, &warn, in.filename);
  ASSERT_MSG(warn.empty(), warn.c_str());
  ASSERT_MSG(err.empty(), err.c_str());
  ASSERT_MSG(res, "Failed to load GLTF file!");

  std::vector<math::Vector<u8, 4>> all_vtx_colors;
  ASSERT(out.vertices.empty());
  // iterate through all meshes
  for (const auto& mesh : model.meshes) {
    // mesh is made of primitives
    for (const auto& prim : mesh.primitives) {
      // extract index buffer
      std::vector<u32> prim_indices = gltf_index_buffer(model, prim.indices, out.vertices.size());
      ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
      // extract vertices
      auto verts = gltf_vertices(model, prim.attributes);
      out.vertices.insert(out.vertices.end(), verts.vtx.begin(), verts.vtx.end());
      all_vtx_colors.insert(all_vtx_colors.end(), verts.vtx_colors.begin(), verts.vtx_colors.end());

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

  auto quantized = quantize_colors_dumb(all_vtx_colors);
  for (size_t i = 0; i < out.vertices.size(); i++) {
    out.vertices[i].color_index = quantized.vtx_to_color[i];
  }
  out.color_palette = std::move(quantized.final_colors);
}
}  // namespace gltf_mesh_extract
