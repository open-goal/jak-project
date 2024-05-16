#include "extract_level.h"

#include <set>
#include <thread>

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/SimpleThreadGroup.h"
#include "common/util/compress.h"
#include "common/util/string_util.h"

#include "decompiler/level_extractor/BspHeader.h"
#include "decompiler/level_extractor/extract_actors.h"
#include "decompiler/level_extractor/extract_collide_frags.h"
#include "decompiler/level_extractor/extract_hfrag.h"
#include "decompiler/level_extractor/extract_joint_group.h"
#include "decompiler/level_extractor/extract_merc.h"
#include "decompiler/level_extractor/extract_shrub.h"
#include "decompiler/level_extractor/extract_tfrag.h"
#include "decompiler/level_extractor/extract_tie.h"
#include "decompiler/level_extractor/fr3_to_gltf.h"
#include "goalc/build_actor/jak1/build_actor.h"

namespace decompiler {

/*!
 * Look through files in a DGO and find the bsp-header file (the level)
 */
std::optional<ObjectFileRecord> get_bsp_file(const std::vector<ObjectFileRecord>& records,
                                             const std::string& dgo_name) {
  std::optional<ObjectFileRecord> result;
  if (str_util::ends_with(dgo_name, ".DGO")) {
    // only DGOs are valid levels, and the last file is the bsp file
    result = records.at(records.size() - 1);
  }
  return result;
}

/*!
 * Make sure a file is a valid bsp-header.
 */
bool is_valid_bsp(const decompiler::LinkedObjectFile& file) {
  if (file.segments != 1) {
    lg::error("Got {} segments, but expected 1", file.segments);
    return false;
  }

  auto& first_word = file.words_by_seg.at(0).at(0);
  if (first_word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    lg::error("Expected the first word to be a type pointer, but it wasn't.");
    return false;
  }

  if (first_word.symbol_name() != "bsp-header") {
    lg::error("Expected to get a bsp-header, but got {} instead.", first_word.symbol_name());
    return false;
  }

  return true;
}

tfrag3::Texture make_texture(u32 id,
                             const TextureDB::TextureData& tex,
                             const std::string& tpage_name,
                             bool pool_load) {
  tfrag3::Texture new_tex;
  new_tex.combo_id = id;
  new_tex.w = tex.w;
  new_tex.h = tex.h;
  new_tex.debug_tpage_name = tpage_name;
  new_tex.debug_name = tex.name;
  new_tex.data = tex.rgba_bytes;
  new_tex.combo_id = id;
  new_tex.load_to_pool = pool_load;
  return new_tex;
}

void add_all_textures_from_level(tfrag3::Level& lev,
                                 const std::string& level_name,
                                 const TextureDB& tex_db) {
  const auto& level_it = tex_db.texture_ids_per_level.find(level_name);
  if (level_it != tex_db.texture_ids_per_level.end()) {
    for (auto id : level_it->second) {
      const auto& tex = tex_db.textures.at(id);
      lev.textures.push_back(make_texture(id, tex, tex_db.tpage_names.at(tex.page), true));
    }
  }
}

void confirm_textures_identical(const TextureDB& tex_db) {
  std::unordered_map<std::string, std::vector<u32>> tex_dupl;
  for (auto& tex : tex_db.textures) {
    auto name = tex_db.tpage_names.at(tex.second.page) + tex.second.name;
    auto it = tex_dupl.find(name);
    if (it == tex_dupl.end()) {
      tex_dupl.insert({name, tex.second.rgba_bytes});
    } else {
      bool ok = it->second == tex.second.rgba_bytes;
      if (!ok) {
        ASSERT_MSG(false, fmt::format("BAD duplicate: {} {} vs {}", name,
                                      tex.second.rgba_bytes.size(), it->second.size()));
      }
    }
  }
}

void extract_art_groups_from_level(const ObjectFileDB& db,
                                   const TextureDB& tex_db,
                                   const std::vector<level_tools::TextureRemap>& tex_remap,
                                   const std::string& dgo_name,
                                   tfrag3::Level& level_data,
                                   std::map<std::string, level_tools::ArtData>& art_group_data) {
  if (db.obj_files_by_dgo.count(dgo_name)) {
    const auto& files = db.obj_files_by_dgo.at(dgo_name);
    for (const auto& file : files) {
      if (file.name.length() > 3 && !file.name.compare(file.name.length() - 3, 3, "-ag")) {
        const auto& ag_file = db.lookup_record(file);
        extract_merc(ag_file, tex_db, db.dts, tex_remap, level_data, false, db.version());
        extract_joint_group(ag_file, db.dts, db.version(), art_group_data);
      }
    }
  }
}

std::vector<level_tools::TextureRemap> extract_tex_remap(const ObjectFileDB& db,
                                                         const std::string& dgo_name) {
  auto bsp_rec = get_bsp_file(db.obj_files_by_dgo.at(dgo_name), dgo_name);
  if (!bsp_rec) {
    lg::warn("Skipping extract for {} because the BSP file was not found", dgo_name);
    return {};
  }
  std::string level_name = bsp_rec->name;

  lg::info("Processing level {} ({})", dgo_name, level_name);
  const auto& bsp_file = db.lookup_record(*bsp_rec);
  bool ok = is_valid_bsp(bsp_file.linked_data);
  ASSERT(ok);

  level_tools::BspHeader bsp_header;
  bsp_header.read_from_file(bsp_file.linked_data, db.dts, db.version(), true);

  return bsp_header.texture_remap_table;
}

level_tools::BspHeader extract_bsp_from_level(const ObjectFileDB& db,
                                              const TextureDB& tex_db,
                                              const std::string& dgo_name,
                                              const Config& config,
                                              tfrag3::Level& level_data) {
  auto hacks = config.hacks;
  auto bsp_rec = get_bsp_file(db.obj_files_by_dgo.at(dgo_name), dgo_name);
  if (!bsp_rec) {
    lg::warn("Skipping extract for {} because the BSP file was not found", dgo_name);
    return {};
  }

  lg::info("Processing {}...", dgo_name);
  const auto& bsp_file = db.lookup_record(*bsp_rec);
  bool ok = is_valid_bsp(bsp_file.linked_data);
  ASSERT(ok);

  level_tools::BspHeader bsp_header;
  bsp_header.read_from_file(bsp_file.linked_data, db.dts, db.version());
  ASSERT((int)bsp_header.drawable_tree_array.trees.size() == bsp_header.drawable_tree_array.length);

  // grrr.....
  if (db.version() == GameVersion::Jak1 && dgo_name == "TIT.DGO" && bsp_header.name == "intro") {
    bsp_header.name = "title";
  } else if (db.version() == GameVersion::Jak1 && dgo_name == "DEM.DGO" &&
             bsp_header.name == "intro") {
    bsp_header.name = "demo";
  }

  /*
  level_tools::PrintSettings settings;
  settings.expand_collide = true;
  lg::print("{}\n", bsp_header.print(settings));
   */

  const std::set<std::string> tfrag_trees = {
      "drawable-tree-tfrag",        "drawable-tree-trans-tfrag",       "drawable-tree-tfrag-trans",
      "drawable-tree-dirt-tfrag",   "drawable-tree-tfrag-water",       "drawable-tree-ice-tfrag",
      "drawable-tree-lowres-tfrag", "drawable-tree-lowres-trans-tfrag"};
  int i = 0;

  std::vector<const level_tools::DrawableTreeInstanceTie*> all_ties;
  for (auto& draw_tree : bsp_header.drawable_tree_array.trees) {
    auto as_tie_tree = dynamic_cast<level_tools::DrawableTreeInstanceTie*>(draw_tree.get());
    if (as_tie_tree) {
      all_ties.push_back(as_tie_tree);
    }
  }

  bool got_collide = false;
  for (auto& draw_tree : bsp_header.drawable_tree_array.trees) {
    if (tfrag_trees.count(draw_tree->my_type())) {
      auto as_tfrag_tree = dynamic_cast<level_tools::DrawableTreeTfrag*>(draw_tree.get());
      ASSERT(as_tfrag_tree);
      std::vector<std::pair<int, int>> expected_missing_textures;
      auto it = hacks.missing_textures_by_level.find(bsp_header.name);
      if (it != hacks.missing_textures_by_level.end()) {
        expected_missing_textures = it->second;
      }
      bool atest_disable_flag = false;
      if (db.version() == GameVersion::Jak2) {
        if (bsp_header.texture_flags[0] & 1) {
          atest_disable_flag = true;
        }
      }
      extract_tfrag(as_tfrag_tree, fmt::format("{}-{}", dgo_name, i++),
                    bsp_header.texture_remap_table, tex_db, expected_missing_textures, level_data,
                    false, bsp_header.name, atest_disable_flag);
    } else if (draw_tree->my_type() == "drawable-tree-instance-tie") {
      auto as_tie_tree = dynamic_cast<level_tools::DrawableTreeInstanceTie*>(draw_tree.get());
      ASSERT(as_tie_tree);
      extract_tie(as_tie_tree, fmt::format("{}-{}-tie", dgo_name, i++),
                  bsp_header.texture_remap_table, tex_db, level_data, false, db.version());
    } else if (draw_tree->my_type() == "drawable-tree-instance-shrub") {
      auto as_shrub_tree =
          dynamic_cast<level_tools::shrub_types::DrawableTreeInstanceShrub*>(draw_tree.get());
      ASSERT(as_shrub_tree);
      extract_shrub(as_shrub_tree, fmt::format("{}-{}-shrub", dgo_name, i++),
                    bsp_header.texture_remap_table, tex_db, {}, level_data, false, db.version());
    } else if (draw_tree->my_type() == "drawable-tree-collide-fragment" &&
               config.extract_collision) {
      auto as_collide_frags =
          dynamic_cast<level_tools::DrawableTreeCollideFragment*>(draw_tree.get());
      ASSERT(as_collide_frags);
      ASSERT(!got_collide);
      got_collide = true;
      extract_collide_frags(as_collide_frags, all_ties, config,
                            fmt::format("{}-{}-collide", dgo_name, i++), level_data);
    } else {
      lg::print("  unsupported tree {}\n", draw_tree->my_type());
    }
  }

  if (bsp_header.collide_hash.num_items) {
    ASSERT(!got_collide);
    extract_collide_frags(bsp_header.collide_hash, all_ties, config,
                          fmt::format("{}-{}-collide", dgo_name, i++), db.dts, level_data);
  }
  if (bsp_header.hfrag) {
    extract_hfrag(bsp_header, tex_db, &level_data);
  }
  level_data.level_name = bsp_header.name;

  return bsp_header;
}

struct MercExtractData {
  gltf_util::TexturePool tex_pool;
  std::vector<u32> new_indices;
  std::vector<tfrag3::PreloadedVertex> new_vertices;
  std::vector<math::Vector<u8, 4>> new_colors;
  std::vector<math::Vector3f> normals;

  tfrag3::MercModel new_model;
};

// Data produced by loading a replacement model
struct MercSwapData {
  std::vector<u32> new_indices;
  std::vector<tfrag3::MercVertex> new_vertices;
  std::vector<tfrag3::Texture> new_textures;
  tfrag3::MercModel new_model;
};

void extract(const std::string& name,
             MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<gltf_util::NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset) {
  ASSERT(out.new_vertices.empty());
  std::map<int, tfrag3::MercDraw> draw_by_material;
  int mesh_count = 0;
  int prim_count = 0;

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
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

        // TODO: just putting it all in one material
        auto& draw = draw_by_material[prim.material];
        draw.mode = gltf_util::make_default_draw_mode();  // todo rm
        draw.tree_tex_id = 0;                             // todo rm
        draw.num_triangles += prim_indices.size() / 3;
        //        if (draw.vis_groups.empty()) {
        //          auto& grp = draw.vis_groups.emplace_back();
        //          grp.num_inds += prim_indices.size();
        //          grp.num_tris += draw.num_triangles;
        //          grp.vis_idx_in_pc_bvh = UINT32_MAX;
        //        } else {
        //          auto& grp = draw.vis_groups.back();
        //          grp.num_inds += prim_indices.size();
        //          grp.num_tris += draw.num_triangles;
        //          grp.vis_idx_in_pc_bvh = UINT32_MAX;
        //        }
        draw.index_count = prim_indices.size();
        draw.first_index = index_offset + out.new_indices.size();

        out.new_indices.insert(out.new_indices.end(), prim_indices.begin(), prim_indices.end());
      }
    }
  }

  tfrag3::MercEffect e;
  out.new_model.name = name;
  out.new_model.max_bones = 120;  // idk
  out.new_model.max_draws = 200;
  for (const auto& [mat_idx, d_] : draw_by_material) {
    e.all_draws.push_back(d_);
    auto& draw = e.all_draws.back();
    draw.mode = gltf_util::make_default_draw_mode();

    if (mat_idx == -1) {
      lg::warn("Draw had a material index of -1, using default texture.");
      draw.tree_tex_id = 0;
      continue;
    }
    const auto& mat = model.materials[mat_idx];
    int tex_idx = mat.pbrMetallicRoughness.baseColorTexture.index;
    if (tex_idx == -1) {
      lg::warn("Material {} has no texture, using default texture.", mat.name);
      draw.tree_tex_id = 0;
      continue;
    }

    const auto& tex = model.textures[tex_idx];
    ASSERT(tex.sampler >= 0);
    ASSERT(tex.source >= 0);
    draw.mode = gltf_util::draw_mode_from_sampler(model.samplers.at(tex.sampler));

    const auto& img = model.images[tex.source];
    draw.tree_tex_id = tex_offset + texture_pool_add_texture(&out.tex_pool, img);
  }
  lg::info("total of {} unique materials", e.all_draws.size());
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.new_vertices.size());
}

const tfrag3::MercVertex& find_closest(const std::vector<tfrag3::MercVertex>& old,
                                       float x,
                                       float y,
                                       float z) {
  float best_dist = 1e10;
  int best_idx = 0;

  for (int i = 0; i < old.size(); i++) {
    auto& v = old[i];
    float dx = v.pos[0] - x;
    float dy = v.pos[1] - y;
    float dz = v.pos[2] - z;
    float dist = (dx * dx) + (dy * dy) + (dz * dz);
    if (dist < best_dist) {
      best_dist = dist;
      best_idx = i;
    }
  }

  return old[best_idx];
}

void merc_convert(MercSwapData& out, const MercExtractData& in
                  /*const std::vector<tfrag3::MercVertex>& old_verts*/) {
  /*
   *   std::vector<u32> new_indices;
  std::vector<tfrag3::MercVertex> new_vertices;
  std::vector<decompiler::TextureDB::TextureData> new_textures;
  tfrag3::MercModel new_model;
   */

  // easy
  out.new_model = in.new_model;
  out.new_indices = in.new_indices;
  out.new_textures = in.tex_pool.textures_by_idx;

  // convert vertices
  for (size_t i = 0; i < in.new_vertices.size(); i++) {
    const auto& y = in.new_vertices[i];
    // const auto& copy_from = find_closest(old_verts, y.x, y.y, y.z);
    auto& x = out.new_vertices.emplace_back();
    x.pos[0] = y.x;
    x.pos[1] = y.y;
    x.pos[2] = y.z;
    x.normal[0] = in.normals.at(i).x();
    x.normal[1] = in.normals.at(i).y();
    x.normal[2] = in.normals.at(i).z();
    x.weights[0] = 1.0f;  // copy_from.weights[0];
    x.weights[1] = 0.0f;  // copy_from.weights[1];
    x.weights[2] = 0.0f;  // copy_from.weights[2];
    x.st[0] = y.s;
    x.st[1] = y.t;
    x.rgba[0] = in.new_colors[i][0];
    x.rgba[1] = in.new_colors[i][1];
    x.rgba[2] = in.new_colors[i][2];
    x.rgba[3] = in.new_colors[i][3];
    x.mats[0] = 4;  // copy_from.mats[0];
    x.mats[1] = 0;  // copy_from.mats[1];
    x.mats[2] = 0;  // copy_from.mats[2];
  }
}

MercSwapData load_merc_model(u32 current_idx_count,
                             u32 current_vtx_count,
                             u32 current_tex_count,
                             const std::string& path,
                             const std::string& name) {
  MercSwapData result;
  lg::info("Reading gltf mesh: {}", path);
  tinygltf::TinyGLTF loader;
  tinygltf::Model model;
  std::string err, warn;
  bool res = loader.LoadBinaryFromFile(&model, &err, &warn, path);
  ASSERT_MSG(warn.empty(), warn.c_str());
  ASSERT_MSG(err.empty(), err.c_str());
  ASSERT_MSG(res, "Failed to load GLTF file!");
  auto all_nodes = gltf_util::flatten_nodes_from_all_scenes(model);

  MercExtractData extract_data;
  extract(name, extract_data, model, all_nodes, current_idx_count, current_vtx_count,
          current_tex_count);
  merc_convert(result, extract_data);

  return result;
}

/*!
 * Extract stuff found in GAME.CGO.
 * Even though GAME.CGO isn't technically a level, the decompiler/loader treat it like one,
 * but the bsp stuff is just empty. It will contain only textures/art groups.
 */
void extract_common(const ObjectFileDB& db,
                    const TextureDB& tex_db,
                    const std::string& dgo_name,
                    const fs::path& output_folder,
                    const Config& config) {
  if (db.obj_files_by_dgo.count(dgo_name) == 0) {
    lg::warn("Skipping common extract for {} because the DGO was not part of the input", dgo_name);
    return;
  }

  if (tex_db.textures.size() == 0) {
    lg::warn("Skipping common extract because there were no textures in the input");
    return;
  }

  confirm_textures_identical(tex_db);

  tfrag3::Level tfrag_level;
  auto merc_data = load_merc_model(0, 0, 0,
                                   fs::path(file_util::get_jak_project_dir() / "custom_assets" /
                                            "jak1" / "models" / "test-actor.glb")
                                       .string(),
                                   "test-actor-lod0");
  for (auto& idx : merc_data.new_indices) {
    tfrag_level.merc_data.indices.push_back(idx);
  }
  for (auto& vert : merc_data.new_vertices) {
    tfrag_level.merc_data.vertices.push_back(vert);
  }
  tfrag_level.merc_data.models.push_back(merc_data.new_model);
  std::map<std::string, level_tools::ArtData> art_group_data;
  add_all_textures_from_level(tfrag_level, dgo_name, tex_db);
  extract_art_groups_from_level(db, tex_db, {}, dgo_name, tfrag_level, art_group_data);
  for (auto& tex : merc_data.new_textures) {
    tfrag_level.textures.push_back(tex);
  }

  add_all_textures_from_level(tfrag_level, "ARTSPOOL", tex_db);
  extract_art_groups_from_level(db, tex_db, {}, "ARTSPOOL", tfrag_level, art_group_data);

  std::set<std::string> textures_we_have;

  // put _all_ index textures in common.
  for (const auto& [id, tex] : tex_db.index_textures_by_combo_id) {
    tfrag_level.index_textures.push_back(tex);
  }

  for (const auto& t : tfrag_level.textures) {
    textures_we_have.insert(t.debug_name);
  }

  for (const auto& [id, normal_texture] : tex_db.textures) {
    if (config.common_tpages.count(normal_texture.page) &&
        !textures_we_have.count(normal_texture.name)) {
      textures_we_have.insert(normal_texture.name);
      tfrag_level.textures.push_back(
          make_texture(id, normal_texture, tex_db.tpage_names.at(normal_texture.page), true));
    }
  }

  // add animated textures that are missing.
  for (const auto& [id, normal_texture] : tex_db.textures) {
    if (config.animated_textures.count(normal_texture.name) &&
        !textures_we_have.count(normal_texture.name)) {
      textures_we_have.insert(normal_texture.name);
      tfrag_level.textures.push_back(
          make_texture(id, normal_texture, tex_db.tpage_names.at(normal_texture.page), false));
    }
  }

  Serializer ser;
  tfrag_level.serialize(ser);
  auto compressed =
      compression::compress_zstd(ser.get_save_result().first, ser.get_save_result().second);

  lg::info("stats for {}", dgo_name);
  print_memory_usage(tfrag_level, ser.get_save_result().second);
  lg::info("compressed: {} -> {} ({:.2f}%)", ser.get_save_result().second, compressed.size(),
           100.f * compressed.size() / ser.get_save_result().second);
  file_util::write_binary_file(
      output_folder / fmt::format("{}.fr3", dgo_name.substr(0, dgo_name.length() - 4)),
      compressed.data(), compressed.size());

  if (config.rip_levels) {
    auto file_path = file_util::get_jak_project_dir() / "glb_out" / "common.glb";
    file_util::create_dir_if_needed_for_file(file_path);
    save_level_foreground_as_gltf(tfrag_level, art_group_data, file_path);
  }
}

void extract_from_level(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::string& dgo_name,
                        const Config& config,
                        const fs::path& output_folder,
                        const fs::path& entities_folder) {
  if (db.obj_files_by_dgo.count(dgo_name) == 0) {
    lg::warn("Skipping extract for {} because the DGO was not part of the input", dgo_name);
    return;
  }
  tfrag3::Level level_data;
  std::map<std::string, level_tools::ArtData> art_group_data;
  add_all_textures_from_level(level_data, dgo_name, tex_db);

  // the bsp header file data
  auto bsp_header = extract_bsp_from_level(db, tex_db, dgo_name, config, level_data);
  extract_art_groups_from_level(db, tex_db, bsp_header.texture_remap_table, dgo_name, level_data,
                                art_group_data);

  Serializer ser;
  level_data.serialize(ser);
  auto compressed =
      compression::compress_zstd(ser.get_save_result().first, ser.get_save_result().second);
  lg::info("stats for {}", level_data.level_name);
  print_memory_usage(level_data, ser.get_save_result().second);
  lg::info("compressed: {} -> {} ({:.2f}%)", ser.get_save_result().second, compressed.size(),
           100.f * compressed.size() / ser.get_save_result().second);
  file_util::write_binary_file(output_folder / fmt::format("{}.fr3", level_data.level_name),
                               compressed.data(), compressed.size());

  if (config.rip_levels) {
    auto back_file_path = file_util::get_jak_project_dir() / "glb_out" /
                          fmt::format("{}_background.glb", level_data.level_name);
    file_util::create_dir_if_needed_for_file(back_file_path);
    save_level_background_as_gltf(level_data, back_file_path);
    auto fore_file_path = file_util::get_jak_project_dir() / "glb_out" /
                          fmt::format("{}_foreground.glb", level_data.level_name);
    file_util::create_dir_if_needed_for_file(fore_file_path);
    save_level_foreground_as_gltf(level_data, art_group_data, fore_file_path);
  }
  file_util::write_text_file(entities_folder / fmt::format("{}_actors.json", level_data.level_name),
                             extract_actors_to_json(bsp_header.actors));
}

void extract_all_levels(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::vector<std::string>& dgo_names,
                        const std::string& common_name,
                        const Config& config,
                        const fs::path& output_path) {
  extract_common(db, tex_db, common_name, output_path, config);
  auto entities_dir = file_util::get_jak_project_dir() / "decompiler_out" /
                      game_version_names[config.game_version] / "entities";
  file_util::create_dir_if_needed(entities_dir);
  SimpleThreadGroup threads;
  threads.run(
      [&](int idx) {
        extract_from_level(db, tex_db, dgo_names[idx], config, output_path, entities_dir);
      },
      dgo_names.size());
  threads.join();
}

}  // namespace decompiler
