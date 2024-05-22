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

using namespace gltf_util;
namespace gltf_mesh_extract {

void dedup_vertices(TfragOutput& data) {
  Timer timer;
  size_t original_size = data.vertices.size();
  std::vector<tfrag3::PreloadedVertex> new_verts;
  std::vector<u32> old_to_new;

  gltf_util::dedup_vertices(data.vertices, new_verts, old_to_new);
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
  lg::info("GLTF total took {:.2f} ms", read_timer.getMs());
}
}  // namespace gltf_mesh_extract
