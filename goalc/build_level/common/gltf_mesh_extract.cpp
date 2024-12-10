/*!
 * Mesh extraction for GLTF meshes.
 */

#include "gltf_mesh_extract.h"

#include <optional>

#include "color_quantization.h"

#include "common/log/log.h"
#include "common/math/geometry.h"
#include "common/util/Timer.h"
#include "common/util/gltf_util.h"
#include <common/util/image_resize.h>

using namespace gltf_util;
constexpr int kColorTreeDepth = 13;
namespace gltf_mesh_extract {

void dedup_tfrag_vertices(TfragOutput& data) {
  Timer timer;
  size_t original_size = data.tfrag_vertices.size();
  std::vector<tfrag3::PreloadedVertex> new_verts;
  std::vector<u32> old_to_new;

  gltf_util::dedup_vertices(data.tfrag_vertices, new_verts, old_to_new);
  data.tfrag_vertices = std::move(new_verts);

  // TODO: properly split vertices between trees...
  for (auto drawlist : {&data.normal_strip_draws, &data.trans_strip_draws}) {
    for (auto& draw : *drawlist) {
      ASSERT(draw.runs.empty());  // not supported yet
      for (auto& idx : draw.plain_indices) {
        idx = old_to_new.at(idx);
      }
    }
  }

  lg::info("Deduplication took {:.2f} ms, {} -> {} ({:.2f} %)", timer.getMs(), original_size,
           data.tfrag_vertices.size(), 100.f * data.tfrag_vertices.size() / original_size);
}

void dedup_tie_vertices(TieOutput& data) {
  Timer timer;
  size_t original_size = data.vertices.size();

  std::vector<TieFullVertex> old_verts;
  old_verts.reserve(data.vertices.size());
  for (size_t i = 0; i < data.vertices.size(); i++) {
    auto& x = old_verts.emplace_back();
    x.color_index = data.color_indices[i];
    x.vertex = data.vertices[i];
  }

  std::vector<TieFullVertex> new_verts;
  std::vector<u32> old_to_new;

  gltf_util::dedup_vertices(old_verts, new_verts, old_to_new);
  data.vertices.clear();
  data.color_indices.clear();
  data.vertices.reserve(new_verts.size());
  data.color_indices.reserve(new_verts.size());
  for (auto& x : new_verts) {
    data.vertices.push_back(x.vertex);
    data.color_indices.push_back(x.color_index);
  }

  // TODO: properly split vertices between trees...
  for (auto drawlist : {&data.base_draws, &data.envmap_draws}) {
    for (auto& draw : *drawlist) {
      ASSERT(draw.runs.empty());  // not supported yet
      for (auto& idx : draw.plain_indices) {
        idx = old_to_new.at(idx);
      }
    }
  }

  lg::info("Deduplication took {:.2f} ms, {} -> {} ({:.2f} %)", timer.getMs(), original_size,
           data.vertices.size(), 100.f * data.vertices.size() / original_size);
}

bool prim_needs_tie(const tinygltf::Model& model, const tinygltf::Primitive& prim) {
  if (prim.material >= 0) {
    auto mat = model.materials.at(prim.material);
    return mat.extensions.contains("KHR_materials_specular");
  }
  return false;
}

void extract(const Input& in,
             TfragOutput& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes) {
  std::vector<math::Vector<u8, 4>> all_vtx_colors;
  ASSERT(out.tfrag_vertices.empty());

  struct MaterialInfo {
    tfrag3::StripDraw draw;
    bool needs_tie = false;
  };
  std::map<int, MaterialInfo> info_by_material;
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

        if (prim_needs_tie(model, prim)) {
          continue;
        }
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices =
            gltf_index_buffer(model, prim.indices, out.tfrag_vertices.size());
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts = gltf_vertices(model, prim.attributes, n.w_T_node, true, false, mesh.name);
        out.tfrag_vertices.insert(out.tfrag_vertices.end(), verts.vtx.begin(), verts.vtx.end());
        all_vtx_colors.insert(all_vtx_colors.end(), verts.vtx_colors.begin(),
                              verts.vtx_colors.end());
        ASSERT(all_vtx_colors.size() == out.tfrag_vertices.size());

        auto& info = info_by_material[prim.material];
        info.draw.mode = make_default_draw_mode();                        // todo rm
        info.draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);  // todo rm
        info.draw.num_triangles += prim_indices.size() / 3;
        if (info.draw.vis_groups.empty()) {
          auto& grp = info.draw.vis_groups.emplace_back();
          grp.num_inds += prim_indices.size();
          grp.num_tris += info.draw.num_triangles;
          grp.vis_idx_in_pc_bvh = UINT16_MAX;
        } else {
          auto& grp = info.draw.vis_groups.back();
          grp.num_inds += prim_indices.size();
          grp.num_tris += info.draw.num_triangles;
          grp.vis_idx_in_pc_bvh = UINT16_MAX;
        }

        info.draw.plain_indices.insert(info.draw.plain_indices.end(), prim_indices.begin(),
                                       prim_indices.end());
      }
    }
  }

  for (const auto& [mat_idx, d_] : info_by_material) {
    // out.strip_draws.push_back(d_);
    // auto& draw = out.strip_draws.back();
    tfrag3::StripDraw draw = d_.draw;
    draw.mode = make_default_draw_mode();

    if (mat_idx == -1) {
      lg::warn("Draw had a material index of -1, using default texture.");
      draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);
      out.normal_strip_draws.push_back(draw);
      continue;
    }

    const auto& mat = model.materials[mat_idx];
    setup_alpha_from_material(mat, &draw.mode);
    int tex_idx = mat.pbrMetallicRoughness.baseColorTexture.index;
    if (tex_idx == -1) {
      lg::warn("Material {} has no texture, using default texture.", mat.name);
      draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);
      if (draw.mode.get_ab_enable()) {
        out.trans_strip_draws.push_back(draw);
      } else {
        out.normal_strip_draws.push_back(draw);
      }
      continue;
    }

    const auto& tex = model.textures[tex_idx];
    ASSERT(tex.sampler >= 0);
    ASSERT(tex.source >= 0);
    setup_draw_mode_from_sampler(model.samplers.at(tex.sampler), &draw.mode);

    const auto& img = model.images[tex.source];
    draw.tree_tex_id = texture_pool_add_texture(in.tex_pool, img);

    if (draw.mode.get_ab_enable()) {
      out.trans_strip_draws.push_back(draw);
    } else {
      out.normal_strip_draws.push_back(draw);
    }
  }
  lg::info("total of {} normal, {} transparent unique materials", out.normal_strip_draws.size(),
           out.trans_strip_draws.size());

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.tfrag_vertices.size());

  Timer quantize_timer;
  auto quantized = quantize_colors_kd_tree(all_vtx_colors, kColorTreeDepth);
  for (size_t i = 0; i < out.tfrag_vertices.size(); i++) {
    out.tfrag_vertices[i].color_index = quantized.vtx_to_color[i];
  }
  out.color_palette = std::move(quantized.final_colors);
  lg::info("Color palette generation took {:.2f} ms", quantize_timer.getMs());

  dedup_tfrag_vertices(out);
}

s8 normal_to_s8(float in) {
  s32 in_s32 = in * 127.f;
  ASSERT(in_s32 <= INT8_MAX);
  ASSERT(in_s32 >= INT8_MIN);
  return in_s32;
}

void add_to_packed_verts(std::vector<tfrag3::PackedTieVertices::Vertex>* out,
                         const std::vector<tfrag3::PreloadedVertex>& vtx,
                         const std::vector<math::Vector3f>& normals) {
  ASSERT(vtx.size() == normals.size());
  for (size_t i = 0; i < normals.size(); i++) {
    auto& x = out->emplace_back();
    // currently not supported.
    x.r = 255;
    x.g = 255;
    x.b = 255;
    x.a = 255;

    x.x = vtx[i].x;
    x.y = vtx[i].y;
    x.z = vtx[i].z;

    x.s = vtx[i].s;
    x.t = vtx[i].t;

    x.nx = normal_to_s8(normals[i].x());
    x.ny = normal_to_s8(normals[i].y());
    x.nz = normal_to_s8(normals[i].z());
  }
}

void extract(const Input& in,
             TieOutput& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes) {
  std::vector<math::Vector<u8, 4>> all_vtx_colors;

  struct MaterialInfo {
    tfrag3::StripDraw draw;
    bool needs_tie = false;
  };
  std::map<int, MaterialInfo> info_by_material;
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

        if (!prim_needs_tie(model, prim)) {
          continue;
        }
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_index_buffer(model, prim.indices, out.vertices.size());
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts = gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        add_to_packed_verts(&out.vertices, verts.vtx, verts.normals);
        all_vtx_colors.insert(all_vtx_colors.end(), verts.vtx_colors.begin(),
                              verts.vtx_colors.end());
        ASSERT(all_vtx_colors.size() == out.vertices.size());

        auto& info = info_by_material[prim.material];
        info.draw.mode = make_default_draw_mode();                        // todo rm
        info.draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);  // todo rm
        info.draw.num_triangles += prim_indices.size() / 3;
        if (info.draw.vis_groups.empty()) {
          auto& grp = info.draw.vis_groups.emplace_back();
          grp.num_inds += prim_indices.size();
          grp.num_tris += info.draw.num_triangles;
          grp.vis_idx_in_pc_bvh = UINT16_MAX;
        } else {
          auto& grp = info.draw.vis_groups.back();
          grp.num_inds += prim_indices.size();
          grp.num_tris += info.draw.num_triangles;
          grp.vis_idx_in_pc_bvh = UINT16_MAX;
        }

        info.draw.plain_indices.insert(info.draw.plain_indices.end(), prim_indices.begin(),
                                       prim_indices.end());
      }
    }
  }

  for (const auto& [mat_idx, d_] : info_by_material) {
    // out.strip_draws.push_back(d_);
    // auto& draw = out.strip_draws.back();
    tfrag3::StripDraw draw = d_.draw;
    draw.mode = make_default_draw_mode();

    if (mat_idx == -1) {
      lg::warn("Draw had a material index of -1, using default texture.");
      draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);
      out.base_draws.push_back(draw);
      continue;
    }

    const auto& mat = model.materials[mat_idx];
    setup_alpha_from_material(mat, &draw.mode);
    int base_tex_idx = mat.pbrMetallicRoughness.baseColorTexture.index;
    if (base_tex_idx == -1) {
      lg::warn("Material {} has no texture, using default texture.", mat.name);
      draw.tree_tex_id = texture_pool_debug_checker(in.tex_pool);
      out.base_draws.push_back(draw);
      continue;
    }
    int roughness_tex_idx = mat.pbrMetallicRoughness.metallicRoughnessTexture.index;
    ASSERT(roughness_tex_idx >= 0);
    const auto& base_tex = model.textures[base_tex_idx];
    ASSERT(base_tex.sampler >= 0);
    ASSERT(base_tex.source >= 0);
    setup_draw_mode_from_sampler(model.samplers.at(base_tex.sampler), &draw.mode);
    const auto& roughness_tex = model.textures.at(roughness_tex_idx);
    ASSERT(roughness_tex.sampler >= 0);
    ASSERT(roughness_tex.source >= 0);

    // draw.tree_tex_id = texture_pool_add_texture(in.tex_pool, model.images[base_tex.source]);
    draw.tree_tex_id = texture_pool_add_envmap_control_texture(
        in.tex_pool, model, base_tex.source, roughness_tex.source, !draw.mode.get_clamp_s_enable(),
        !draw.mode.get_clamp_t_enable());
    out.base_draws.push_back(draw);

    // now, setup envmap draw:
    auto envmap_settings = envmap_settings_from_gltf(mat);
    const auto& envmap_tex = model.textures[envmap_settings.texture_idx];
    ASSERT(envmap_tex.sampler >= 0);
    ASSERT(envmap_tex.source >= 0);
    draw.mode = make_default_draw_mode();
    setup_draw_mode_from_sampler(model.samplers.at(envmap_tex.sampler), &draw.mode);
    draw.tree_tex_id = texture_pool_add_texture(in.tex_pool, model.images[envmap_tex.source]);
    draw.mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_DST_DST);
    draw.mode.enable_ab();

    out.envmap_draws.push_back(draw);
  }
  lg::info("total of {} normal TIE draws, {} envmap", out.base_draws.size(),
           out.envmap_draws.size());

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.vertices.size());

  Timer quantize_timer;
  auto quantized = quantize_colors_kd_tree(all_vtx_colors, kColorTreeDepth);
  for (size_t i = 0; i < out.vertices.size(); i++) {
    out.color_indices.push_back(quantized.vtx_to_color[i]);
  }
  out.color_palette = std::move(quantized.final_colors);
  lg::info("Color palette generation took {:.2f} ms", quantize_timer.getMs());

  dedup_tie_vertices(out);
}

std::optional<std::vector<jak1::CollideFace>> subdivide_face_if_needed(jak1::CollideFace face_in) {
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
    jak1::CollideFace fs[4];
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

    std::vector<jak1::CollideFace> result;
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
  ASSERT(mat < (int)jak1::PatSurface::Material::MAX_MATERIAL);
  result.pat.set_material(jak1::PatSurface::Material(mat));

  int evt = val.Get("collide_event").Get<int>();
  ASSERT(evt < (int)jak1::PatSurface::Event::MAX_EVENT);
  result.pat.set_event(jak1::PatSurface::Event(evt));

  if (val.Get("nolineofsight").Get<int>()) {
    result.pat.set_nolineofsight(true);
  }

  if (val.Get("noedge").Get<int>()) {
    result.pat.set_noedge(true);
  }

  if (val.Has("collide_mode")) {
    int mode = val.Get("collide_mode").Get<int>();
    ASSERT(mode < (int)jak1::PatSurface::Mode::MAX_MODE);
    result.pat.set_mode(jak1::PatSurface::Mode(mode));
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
          jak1::CollideFace face;

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

  std::vector<jak1::CollideFace> fixed_faces;
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
        face.pat.set_mode(jak1::PatSurface::Mode::WALL);
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
  extract(in, out.tie, model, all_nodes);
  lg::info("GLTF total took {:.2f} ms", read_timer.getMs());
}
}  // namespace gltf_mesh_extract
