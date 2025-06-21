#include "MercExtract.h"

#include "common/log/log.h"
#include "common/util/gltf_util.h"

#include "goalc/build_level/common/gltf_mesh_extract.h"

void extract(const std::string& name,
             gltf_util::MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<gltf_util::NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset) {
  ASSERT(out.new_vertices.empty());

  std::map<int, tfrag3::MercDraw> draw_by_material;
  int mesh_count = 0;
  int prim_count = 0;
  int joints = 3;
  auto skin_idx = find_single_skin(model, all_nodes);
  if (skin_idx) {
    joints += gltf_util::get_joint_count(model, *skin_idx);
  }

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.extras.Has("set_invisible") && node.extras.Get("set_invisible").Get<int>()) {
      continue;
    }
    // gltf_mesh_extract::PatResult mesh_default_collide =
    //     gltf_mesh_extract::custom_props_to_pat(node.extras, node.name);
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
        // get material
        // const auto& mat_idx = prim.material;
        // gltf_mesh_extract::PatResult pat = mesh_default_collide;
        // if (mat_idx != -1) {
        //   const auto& mat = model.materials[mat_idx];
        //   auto mat_pat = gltf_mesh_extract::custom_props_to_pat(mat.extras, mat.name);
        //   if (mat_pat.set) {
        //     pat = mat_pat;
        //   }
        // }
        // if (pat.set && pat.ignore) {
        //   continue;  // skip, no collide here
        // }
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_util::gltf_index_buffer(
            model, prim.indices, out.new_vertices.size() + vertex_offset);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts =
            gltf_util::gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        out.new_vertices.insert(out.new_vertices.end(), verts.vtx.begin(), verts.vtx.end());
        out.new_colors.insert(out.new_colors.end(), verts.vtx_colors.begin(),
                              verts.vtx_colors.end());
        out.normals.insert(out.normals.end(), verts.normals.begin(), verts.normals.end());
        ASSERT(out.new_colors.size() == out.new_vertices.size());

        if (prim.attributes.count("JOINTS_0") && prim.attributes.count("WEIGHTS_0")) {
          auto joints_and_weights = gltf_util::extract_and_flatten_joints_and_weights(model, prim);
          ASSERT(joints_and_weights.size() == verts.vtx.size());
          out.joints_and_weights.insert(out.joints_and_weights.end(), joints_and_weights.begin(),
                                        joints_and_weights.end());
        } else {
          // add fake data for vertices without this data
          gltf_util::JointsAndWeights dummy;
          dummy.joints[0] = 3;
          dummy.weights[0] = 1.f;
          for (size_t i = 0; i < out.new_vertices.size(); i++) {
            out.joints_and_weights.push_back(dummy);
          }
        }

        // real draw details will be filled out in the next loop.
        auto& draw = draw_by_material[prim.material];
        draw.mode = gltf_util::make_default_draw_mode();  // todo rm
        draw.tree_tex_id = 0;                             // todo rm
        draw.num_triangles += prim_indices.size() / 3;
        draw.no_strip = true;
        draw.index_count = prim_indices.size();
        draw.first_index = index_offset + out.new_indices.size();
        out.new_indices.insert(out.new_indices.end(), prim_indices.begin(), prim_indices.end());
      }
    }
  }

  tfrag3::MercEffect e;
  tfrag3::MercEffect envmap_eff;
  envmap_eff.has_envmap = false;
  out.new_model.name = name;
  out.new_model.max_bones = joints;
  out.new_model.max_draws = 0;

  for (const auto& [mat_idx, d_] : draw_by_material) {
    const auto& mat = model.materials[mat_idx];
    if (mat_idx < 0 || !gltf_util::material_has_envmap(model.materials[mat_idx]) ||
        !gltf_util::envmap_is_valid(model.materials[mat_idx])) {
      gltf_util::process_normal_merc_draw(model, out, tex_offset, e, mat_idx, d_);
    } else {
      envmap_eff.has_envmap = true;
      gltf_util::process_envmap_merc_draw(model, out, tex_offset, envmap_eff, mat_idx, d_);
    }
  }

  // in case a model only has envmap draws, we don't push the normal merc effect
  if (!e.all_draws.empty()) {
    out.new_model.effects.push_back(e);
  }
  if (envmap_eff.has_envmap) {
    out.new_model.effects.push_back(envmap_eff);
  }

  for (auto& effect : out.new_model.effects) {
    out.new_model.max_draws += effect.all_draws.size();
  }

  lg::info("total of {} unique materials ({} normal, {} envmap)", out.new_model.max_draws,
           e.all_draws.size(), envmap_eff.all_draws.size());
  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.new_vertices.size());
}

void merc_convert(gltf_util::MercSwapData& out, const gltf_util::MercExtractData& in) {
  // easy
  out.new_model = in.new_model;
  out.new_indices = in.new_indices;
  out.new_textures = in.tex_pool.textures_by_idx;

  // convert vertices
  for (size_t i = 0; i < in.new_vertices.size(); i++) {
    const auto& y = in.new_vertices[i];
    auto& x = out.new_vertices.emplace_back();
    x.pos[0] = y.x;
    x.pos[1] = y.y;
    x.pos[2] = y.z;
    x.normal[0] = in.normals.at(i).x();
    x.normal[1] = in.normals.at(i).y();
    x.normal[2] = in.normals.at(i).z();
    x.weights[0] = in.joints_and_weights.at(i).weights[0];
    x.weights[1] = in.joints_and_weights.at(i).weights[1];
    x.weights[2] = in.joints_and_weights.at(i).weights[2];
    x.st[0] = y.s;
    x.st[1] = y.t;
    x.rgba[0] = in.new_colors[i][0];
    x.rgba[1] = in.new_colors[i][1];
    x.rgba[2] = in.new_colors[i][2];
    x.rgba[3] = in.new_colors[i][3];
    x.mats[0] = in.joints_and_weights.at(i).joints[0];
    x.mats[1] = in.joints_and_weights.at(i).joints[1];
    x.mats[2] = in.joints_and_weights.at(i).joints[2];
  }
}

gltf_util::MercSwapData load_merc_model(u32 current_idx_count,
                                        u32 current_vtx_count,
                                        u32 current_tex_count,
                                        const std::string& path,
                                        const std::string& name) {
  gltf_util::MercSwapData result;
  lg::info("Reading gltf mesh: {}", path);
  tinygltf::TinyGLTF loader;
  tinygltf::Model model;
  std::string err, warn;
  bool res = loader.LoadBinaryFromFile(&model, &err, &warn, path);
  ASSERT_MSG(warn.empty(), warn.c_str());
  ASSERT_MSG(err.empty(), err.c_str());
  ASSERT_MSG(res, "Failed to load GLTF file!");
  auto all_nodes = gltf_util::flatten_nodes_from_all_scenes(model);

  gltf_util::MercExtractData extract_data;
  extract(name, extract_data, model, all_nodes, current_idx_count, current_vtx_count,
          current_tex_count);
  merc_convert(result, extract_data);

  return result;
}

std::vector<jak1::CollideMesh> gen_collide_mesh_from_model_jak1(
    const tinygltf::Model& model,
    const std::vector<gltf_util::NodeWithTransform>& all_nodes,
    int joint_idx) {
  // data for a single primitive
  struct PrimWork {
    std::string mesh_name;
    std::vector<math::Vector4f> verts;
    std::vector<u32> indices;
    gltf_mesh_extract::PatResult pat;
  };
  std::vector<std::vector<PrimWork>> mesh_data;
  int mesh_count = 0;
  // int prim_count = 0;
  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    gltf_mesh_extract::PatResult mesh_default_collide =
        gltf_mesh_extract::custom_props_to_pat(node.extras, node.name);
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      std::vector<PrimWork> prims;
      for (const auto& prim : mesh.primitives) {
        // get material
        const auto& mat_idx = prim.material;
        gltf_mesh_extract::PatResult pat = mesh_default_collide;
        if (mat_idx != -1) {
          const auto& mat = model.materials[mat_idx];
          auto mat_pat = gltf_mesh_extract::custom_props_to_pat(mat.extras, mat.name);
          if (mat_pat.set) {
            pat = mat_pat;
          }
        }
        if (pat.set && pat.ignore) {
          continue;  // skip, no collide here
        }
        auto& prim_data = prims.emplace_back();
        prim_data.mesh_name = mesh.name;
        prim_data.pat = pat;
        // prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_util::gltf_index_buffer(model, prim.indices, 0);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES,
                   fmt::format("Mesh {}: Unsupported triangle mode {}", mesh.name, prim.mode));
        // extract vertices
        auto verts =
            gltf_util::gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        ASSERT_MSG(verts.vtx.size() <= 255,
                   fmt::format("primitive of mesh {} has too many vertices (max 255, actual {})",
                               mesh.name, verts.vtx.size()));
        prim_data.verts.reserve(verts.vtx.size());
        for (auto& vert : verts.vtx) {
          prim_data.verts.emplace_back(vert.x, vert.y, vert.z, 1.0);
        }
        prim_data.indices = prim_indices;
        mesh_data.push_back(prims);
      }
    }
  }

  std::vector<jak1::CollideMesh> cmeshes;
  cmeshes.reserve(mesh_count);
  // we extracted all of the prim data for each mesh, now combine
  for (size_t p = 0; p < mesh_data.size(); p++) {
    auto& prims = mesh_data.at(p);
    std::vector<math::Vector4f> verts;
    std::vector<jak1::CollideMeshTri> tris;
    int vert_count = 0;
    for (auto& prim : prims) {
      if (prim.pat.ignore) {
        continue;
      }
      vert_count += verts.size();
      verts.insert(verts.end(), prim.verts.begin(), prim.verts.end());
      for (size_t i = 0; i < prim.indices.size(); i += 3) {
        auto& tri = tris.emplace_back();
        tri.vert_idx[0] = prim.indices.at(i);
        tri.vert_idx[1] = prim.indices.at(i + 1);
        tri.vert_idx[2] = prim.indices.at(i + 2);
        tri.pat = prim.pat.pat;
      }
    }
    ASSERT_MSG(vert_count <= 255, fmt::format("Mesh {} has too many vertices (max 255, actual {})",
                                              prims.at(p).mesh_name, vert_count));
    auto& cmesh = cmeshes.emplace_back();
    // TODO joint idx as a custom property in blender?
    cmesh.joint_id = joint_idx;
    cmesh.vertices.reserve(255);
    cmesh.vertices.insert(cmesh.vertices.begin(), verts.begin(), verts.end());
    cmesh.num_verts = cmesh.vertices.size();
    cmesh.num_tris = tris.size();
    cmesh.tris.reserve(cmesh.num_tris);
    for (size_t j = 0; j < cmesh.num_tris; j++) {
      cmesh.tris.push_back(tris.at(j));
    }
  }
  return cmeshes;
}

std::vector<jak2::CollideMesh> gen_collide_mesh_from_model_jak2(
    const tinygltf::Model& model,
    const std::vector<gltf_util::NodeWithTransform>& all_nodes,
    int joint_idx) {
  // data for a single primitive
  struct PrimWork {
    std::string mesh_name;
    std::vector<math::Vector4f> verts;
    std::vector<u32> indices;
    gltf_mesh_extract::PatResult pat;
  };
  std::vector<std::vector<PrimWork>> mesh_data;
  int mesh_count = 0;
  // int prim_count = 0;
  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    gltf_mesh_extract::PatResult mesh_default_collide =
        gltf_mesh_extract::custom_props_to_pat(node.extras, node.name);
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      std::vector<PrimWork> prims;
      for (const auto& prim : mesh.primitives) {
        // get material
        const auto& mat_idx = prim.material;
        gltf_mesh_extract::PatResult pat = mesh_default_collide;
        if (mat_idx != -1) {
          const auto& mat = model.materials[mat_idx];
          auto mat_pat = gltf_mesh_extract::custom_props_to_pat(mat.extras, mat.name);
          if (mat_pat.set) {
            pat = mat_pat;
          }
        }
        if (pat.set && pat.ignore) {
          continue;  // skip, no collide here
        }
        auto& prim_data = prims.emplace_back();
        prim_data.mesh_name = mesh.name;
        prim_data.pat = pat;
        // prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_util::gltf_index_buffer(model, prim.indices, 0);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES,
                   fmt::format("Mesh {}: Unsupported triangle mode {}", mesh.name, prim.mode));
        // extract vertices
        auto verts =
            gltf_util::gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        ASSERT_MSG(verts.vtx.size() <= 255,
                   fmt::format("primitive of mesh {} has too many vertices (max 255, actual {})",
                               mesh.name, verts.vtx.size()));
        prim_data.verts.reserve(verts.vtx.size());
        for (auto& vert : verts.vtx) {
          prim_data.verts.emplace_back(vert.x, vert.y, vert.z, 1.0);
        }
        prim_data.indices = prim_indices;
        mesh_data.push_back(prims);
      }
    }
  }

  std::vector<jak2::CollideMesh> cmeshes;
  cmeshes.reserve(mesh_count);
  // we extracted all of the prim data for each mesh, now combine
  for (size_t p = 0; p < mesh_data.size(); p++) {
    auto& prims = mesh_data.at(p);
    std::vector<math::Vector4f> verts;
    std::vector<jak2::CollideMeshTri> tris;
    int vert_count = 0;
    for (auto& prim : prims) {
      if (prim.pat.ignore) {
        continue;
      }
      vert_count += verts.size();
      verts.insert(verts.end(), prim.verts.begin(), prim.verts.end());
      for (size_t i = 0; i < prim.indices.size(); i += 3) {
        auto& tri = tris.emplace_back();
        tri.vert_idx[0] = prim.indices.at(i);
        tri.vert_idx[1] = prim.indices.at(i + 1);
        tri.vert_idx[2] = prim.indices.at(i + 2);
        tri.pat = jak2_pat(prim.pat.pat);
      }
    }
    ASSERT_MSG(vert_count <= 255, fmt::format("Mesh {} has too many vertices (max 255, actual {})",
                                              prims.at(p).mesh_name, vert_count));
    auto& cmesh = cmeshes.emplace_back();
    // TODO joint idx as a custom property in blender?
    cmesh.joint_id = joint_idx;
    cmesh.vertices.reserve(255);
    cmesh.vertices.insert(cmesh.vertices.begin(), verts.begin(), verts.end());
    cmesh.num_verts = cmesh.vertices.size();
    cmesh.num_tris = tris.size();
    cmesh.tris.reserve(cmesh.num_tris);
    for (size_t j = 0; j < cmesh.num_tris; j++) {
      cmesh.tris.push_back(tris.at(j));
    }
  }
  return cmeshes;
}

std::vector<jak3::CollideMesh> gen_collide_mesh_from_model_jak3(
    const tinygltf::Model& model,
    const std::vector<gltf_util::NodeWithTransform>& all_nodes,
    int joint_idx) {
  // data for a single primitive
  struct PrimWork {
    std::string mesh_name;
    std::vector<math::Vector4f> verts;
    std::vector<u32> indices;
    gltf_mesh_extract::PatResult pat;
  };
  std::vector<std::vector<PrimWork>> mesh_data;
  int mesh_count = 0;
  // int prim_count = 0;
  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    gltf_mesh_extract::PatResult mesh_default_collide =
        gltf_mesh_extract::custom_props_to_pat(node.extras, node.name);
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      std::vector<PrimWork> prims;
      for (const auto& prim : mesh.primitives) {
        // get material
        const auto& mat_idx = prim.material;
        gltf_mesh_extract::PatResult pat = mesh_default_collide;
        if (mat_idx != -1) {
          const auto& mat = model.materials[mat_idx];
          auto mat_pat = gltf_mesh_extract::custom_props_to_pat(mat.extras, mat.name);
          if (mat_pat.set) {
            pat = mat_pat;
          }
        }
        if (pat.set && pat.ignore) {
          continue;  // skip, no collide here
        }
        auto& prim_data = prims.emplace_back();
        prim_data.mesh_name = mesh.name;
        prim_data.pat = pat;
        // prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_util::gltf_index_buffer(model, prim.indices, 0);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES,
                   fmt::format("Mesh {}: Unsupported triangle mode {}", mesh.name, prim.mode));
        // extract vertices
        auto verts =
            gltf_util::gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        ASSERT_MSG(verts.vtx.size() <= 255,
                   fmt::format("primitive of mesh {} has too many vertices (max 255, actual {})",
                               mesh.name, verts.vtx.size()));
        prim_data.verts.reserve(verts.vtx.size());
        for (auto& vert : verts.vtx) {
          prim_data.verts.emplace_back(vert.x, vert.y, vert.z, 1.0);
        }
        prim_data.indices = prim_indices;
        mesh_data.push_back(prims);
      }
    }
  }

  std::vector<jak3::CollideMesh> cmeshes;
  cmeshes.reserve(mesh_count);
  // we extracted all of the prim data for each mesh, now combine
  for (size_t p = 0; p < mesh_data.size(); p++) {
    auto& prims = mesh_data.at(p);
    std::vector<math::Vector4f> verts;
    std::vector<jak3::CollideMeshTri> tris;
    int vert_count = 0;
    for (auto& prim : prims) {
      if (prim.pat.ignore) {
        continue;
      }
      vert_count += verts.size();
      verts.insert(verts.end(), prim.verts.begin(), prim.verts.end());
      for (size_t i = 0; i < prim.indices.size(); i += 3) {
        auto& tri = tris.emplace_back();
        tri.vert_idx[0] = prim.indices.at(i);
        tri.vert_idx[1] = prim.indices.at(i + 1);
        tri.vert_idx[2] = prim.indices.at(i + 2);
        tri.pat = jak3_pat(prim.pat.pat);
      }
    }
    ASSERT_MSG(vert_count <= 255, fmt::format("Mesh {} has too many vertices (max 255, actual {})",
                                              prims.at(p).mesh_name, vert_count));
    auto& cmesh = cmeshes.emplace_back();
    // TODO joint idx as a custom property in blender?
    cmesh.joint_id = joint_idx;
    cmesh.vertices.reserve(255);
    cmesh.vertices.insert(cmesh.vertices.begin(), verts.begin(), verts.end());
    cmesh.num_verts = cmesh.vertices.size();
    cmesh.num_tris = tris.size();
    cmesh.tris.reserve(cmesh.num_tris);
    for (size_t j = 0; j < cmesh.num_tris; j++) {
      cmesh.tris.push_back(tris.at(j));
    }
  }
  return cmeshes;
}