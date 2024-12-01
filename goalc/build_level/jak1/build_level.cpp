#include "build_level.h"

#include "common/util/gltf_util.h"

#include "decompiler/extractor/extractor_util.h"
#include "decompiler/level_extractor/BspHeader.h"
#include "decompiler/level_extractor/extract_collide_frags.h"
#include "decompiler/level_extractor/extract_level.h"
#include "decompiler/level_extractor/extract_merc.h"
#include "goalc/build_level/collide/jak1/collide_bvh.h"
#include "goalc/build_level/collide/jak1/collide_pack.h"
#include "goalc/build_level/common/Tfrag.h"
#include "goalc/build_level/jak1/Entity.h"
#include "goalc/build_level/jak1/FileInfo.h"
#include "goalc/build_level/jak1/LevelFile.h"

namespace jak1 {
bool run_build_level(const std::string& input_file,
                     const std::string& bsp_output_file,
                     const std::string& output_prefix) {
  auto level_json = parse_commented_json(
      file_util::read_text_file(file_util::get_file_path({input_file})), input_file);
  LevelFile file{};                 // GOAL level file
  tfrag3::Level pc_level;           // PC level file
  gltf_util::TexturePool tex_pool;  // pc level texture pool

  // process input mesh from blender
  gltf_mesh_extract::Input mesh_extract_in;
  mesh_extract_in.filename =
      file_util::get_file_path({level_json.at("gltf_file").get<std::string>()});
  mesh_extract_in.auto_wall_enable = level_json.value("automatic_wall_detection", true);
  mesh_extract_in.double_sided_collide = level_json.at("double_sided_collide").get<bool>();
  mesh_extract_in.auto_wall_angle = level_json.value("automatic_wall_angle", 30.0);
  mesh_extract_in.tex_pool = &tex_pool;
  gltf_mesh_extract::Output mesh_extract_out;
  gltf_mesh_extract::extract(mesh_extract_in, mesh_extract_out);

  // add stuff to the GOAL level structure
  file.info = make_file_info_for_level(fs::path(input_file).filename().string());
  // all vis
  // drawable trees
  // pat
  // texture remap
  // texture ids
  // unk zero
  // name
  file.name = level_json.at("long_name").get<std::string>();
  // nick
  file.nickname = level_json.at("nickname").get<std::string>();
  // vis infos
  // actors
  std::vector<EntityActor> actors;
  auto dts = decompiler::DecompilerTypeSystem(GameVersion::Jak1);
  dts.parse_type_defs({"decompiler", "config", "jak1", "all-types.gc"});
  add_actors_from_json(level_json.at("actors"), actors, level_json.value("base_id", 1234), dts);
  std::sort(actors.begin(), actors.end(), [](auto& a, auto& b) { return a.aid < b.aid; });
  auto duplicates = std::adjacent_find(actors.begin(), actors.end(),
                                       [](auto& a, auto& b) { return a.aid == b.aid; });
  ASSERT_MSG(duplicates == actors.end(),
             fmt::format("Actor IDs must be unique. Found at least two actors with ID {}",
                         duplicates->aid));
  file.actors = std::move(actors);
  // ambients
  std::vector<EntityAmbient> ambients;
  jak1::add_ambients_from_json(level_json.at("ambients"), ambients,
                               level_json.value("base_id", 12345), dts);
  file.ambients = std::move(ambients);
  auto& ambient_drawable_tree = file.drawable_trees.ambients.emplace_back();
  (void)ambient_drawable_tree;
  // cameras
  // nodes
  // boxes
  // ambients
  // subdivs
  // adgifs
  // actor birth
  for (size_t i = 0; i < file.actors.size(); i++) {
    file.actor_birth_order.push_back(i);
  }
  // split box

  // add stuff to the PC level structure
  pc_level.level_name = file.name;

  // TFRAG
  file.drawable_trees.tfrags.emplace_back("drawable-tree-tfrag", "drawable-inline-array-tfrag");
  file.drawable_trees.tfrags.emplace_back("drawable-tree-trans-tfrag",
                                          "drawable-inline-array-trans-tfrag");

  tfrag_from_gltf(mesh_extract_out.tfrag, pc_level.tfrag_trees[0]);

  // TIE
  if (!mesh_extract_out.tie.base_draws.empty()) {
    file.drawable_trees.ties.emplace_back();
    tie_from_gltf(mesh_extract_out.tie, pc_level.tie_trees[0]);
  }

  // TEXTURE
  pc_level.textures = std::move(tex_pool.textures_by_idx);

  // COLLIDE
  if (mesh_extract_out.collide.faces.empty()) {
    lg::error("No collision geometry was found");
  } else {
    auto& collide_drawable_tree = file.drawable_trees.collides.emplace_back();
    collide_drawable_tree.bvh = collide::construct_collide_bvh(mesh_extract_out.collide.faces);
    collide_drawable_tree.packed_frags = pack_collide_frags(collide_drawable_tree.bvh.frags.frags);
    // for collision renderer
    for (auto& face : mesh_extract_out.collide.faces) {
      math::Vector4f verts[3];
      for (int i = 0; i < 3; i++) {
        verts[i].x() = face.v[i].x();
        verts[i].y() = face.v[i].y();
        verts[i].z() = face.v[i].z();
        verts[i].w() = 1.f;
      }
      tfrag3::CollisionMesh::Vertex out_verts[3];
      decompiler::set_vertices_for_tri(out_verts, verts);
      for (auto& out : out_verts) {
        out.pat = face.pat.val;
        pc_level.collision.vertices.push_back(out);
      }
    }
  }

  auto sky_name = level_json.value("sky", "none");
  auto texture_remap = level_json.value("tex_remap", "none");
  auto tpages = level_json.value("tpages", std::vector<u32>({}));

  // Add textures and models
  // TODO remove hardcoded config settings
  if ((level_json.contains("art_groups") && !level_json.at("art_groups").empty()) ||
      (level_json.contains("textures") && !level_json.at("textures").empty())) {
    lg::info("Looking for ISO path...");
    const auto iso_folder = file_util::get_iso_dir_for_game(GameVersion::Jak1);
    lg::info("Found ISO path: {}", iso_folder.string());

    if (iso_folder.empty() || !fs::exists(iso_folder)) {
      lg::warn("Could not locate ISO path!");
      return false;
    }

    // Look for iso build info if it's available, otherwise default to ntsc_v1
    const auto version_info = get_version_info_or_default(iso_folder);

    decompiler::Config config;
    try {
      config = decompiler::read_config_file(
          file_util::get_jak_project_dir() / "decompiler/config/jak1/jak1_config.jsonc",
          version_info.decomp_config_version,
          R"({"decompile_code": false, "find_functions": false, "levels_extract": true, "allowed_objects": [], "save_texture_pngs": false})");
    } catch (const std::exception& e) {
      lg::error("Failed to parse config: {}", e.what());
      return false;
    }

    std::vector<fs::path> dgos, objs;
    for (const auto& dgo_name : config.dgo_names) {
      dgos.push_back(iso_folder / dgo_name);
    }

    for (const auto& obj_name : config.object_file_names) {
      objs.push_back(iso_folder / obj_name);
    }

    decompiler::ObjectFileDB db(dgos, fs::path(config.obj_file_name_map_file), objs, {}, {}, {},
                                config);

    // need to process link data for tpages
    db.process_link_data(config);

    decompiler::TextureDB tex_db;
    auto textures_out = file_util::get_jak_project_dir() / "decompiler_out/jak1/textures";
    file_util::create_dir_if_needed(textures_out);
    db.process_tpages(tex_db, textures_out, config, "");

    std::vector<std::string> processed_art_groups;

    // find all art groups used by the custom level in other dgos and extract sky and texture remap
    // if desired
    auto should_process_art_groups =
        (level_json.contains("art_groups") && !level_json.at("art_groups").empty()) ||
        (sky_name != "none" || texture_remap != "none");
    if (should_process_art_groups) {
      for (auto& dgo : config.dgo_names) {
        // remove "DGO/" prefix
        const auto& dgo_name = dgo.substr(4);
        const auto& files = db.obj_files_by_dgo.at(dgo_name);
        auto art_groups =
            find_art_groups(processed_art_groups,
                            level_json.value("art_groups", std::vector<std::string>{}), files);
        std::vector<level_tools::TextureRemap> tex_remap{};
        if (auto bsp = get_bsp_file(files, dgo_name)) {
          const auto& link_data = db.lookup_record(bsp.value()).linked_data;
          if (is_valid_bsp(link_data)) {
            level_tools::BspHeader level_file;
            level_file.read_from_file(link_data, dts, GameVersion::Jak1, true);
            auto bsp_name = bsp.value().name.substr(0, bsp.value().name.size() - 4);
            tex_remap = level_file.texture_remap_table;
            auto is_sky_bsp = bsp_name == sky_name;
            auto is_tex_remap_bsp = bsp_name == texture_remap;
            auto sky_and_tex_remap_same = sky_name == texture_remap;
            if (is_tex_remap_bsp) {
              lg::info("custom level: copying texture remap data from {}", texture_remap);
              // copy texture remap data from bsp
              file.texture_remap_table.resize(tex_remap.size());
              memcpy(file.texture_remap_table.data(), level_file.texture_remap_table.data(),
                     tex_remap.size() * sizeof(level_tools::TextureRemap));
            }
            if (is_sky_bsp) {
              // copy adgif data from bsp
              lg::info("custom level: copying adgifs from {}", sky_name);
              auto& adgifs = file.adgifs.adgifs;
              adgifs.resize(level_file.adgifs.adgifs.size());
              memcpy(adgifs.data(), level_file.adgifs.adgifs.data(),
                     level_file.adgifs.adgifs.size() * sizeof(AdGifData));
            }
            if (sky_and_tex_remap_same && is_sky_bsp && is_tex_remap_bsp && tpages.empty()) {
              // if tpages json is empty and sky and tex remap are the same level, auto fill
              file.texture_ids.resize(level_file.texture_page_count);
              memcpy(file.texture_ids.data(), level_file.texture_ids.data(),
                     sizeof(u32) * level_file.texture_page_count);
              std::vector<u32> tex_ids;
              tex_ids.reserve(level_file.texture_page_count);
              for (auto& id : level_file.texture_ids) {
                tex_ids.push_back(id >> 20);
              }
              lg::info("custom level: login tpages automatically set to [{}]",
                       fmt::join(tex_ids, ", "));
            }
          }
        }
        for (const auto& ag : art_groups) {
          if (ag.name.length() > 3 && !ag.name.compare(ag.name.length() - 3, 3, "-ag")) {
            const auto& ag_file = db.lookup_record(ag);
            lg::info("custom level: extracting art group {}", ag_file.name_in_dgo);
            decompiler::MercSwapInfo info;
            decompiler::extract_merc(ag_file, tex_db, db.dts, tex_remap, pc_level, false,
                                     db.version(), info);
          }
        }
      }
    }

    // add textures
    if (level_json.contains("textures") && !level_json.at("textures").empty()) {
      std::map<std::string, std::vector<std::string>> processed_textures;
      auto tex_json = level_json.value("textures", std::vector<std::vector<std::string>>{});
      std::map<std::string, std::vector<std::string>> wanted_texs;
      for (auto& arr : tex_json) {
        auto tpage_name = arr[0];
        // we only want a select few textures
        if (arr.size() > 1) {
          for (size_t i = 1; i < arr.size(); i++) {
            wanted_texs.insert({tpage_name, {arr.begin() + 1, arr.end()}});
          }
        } else {
          // we want all textures from this tpage
          auto it = std::find_if(tex_db.tpage_names.begin(), tex_db.tpage_names.end(),
                                 [tpage_name](const std::pair<u32, std::string>& t) {
                                   return t.second == tpage_name;
                                 });
          if (it != tex_db.tpage_names.end()) {
            lg::info("custom level: adding all textures from tpage {}:", tpage_name);
            std::vector<std::string> tex_names;
            for (auto& [id, tex] : tex_db.textures) {
              if (tex_db.tpage_names.at(tex.page) == tpage_name) {
                lg::info("custom level: adding texture {} (tpage {})", tex.name, tex.page);
                tex_names.push_back(tex.name);
                pc_level.textures.push_back(make_texture(id, tex, tpage_name, true));
                processed_textures[tpage_name].push_back(tex.name);
              }
            }
            wanted_texs.insert({tpage_name, tex_names});
          }
        }
      }

      // first check the texture is not already in the level
      for (auto& level_tex : pc_level.textures) {
        auto tpage = level_tex.debug_tpage_name;
        auto name = level_tex.debug_name;
        auto it = std::find_if(
            wanted_texs.begin(), wanted_texs.end(),
            [tpage, name](const std::pair<std::string, std::vector<std::string>>& elt) {
              return elt.first == tpage &&
                     std::find(elt.second.begin(), elt.second.end(), name) != elt.second.end();
            });
        if (it != wanted_texs.end()) {
          processed_textures[level_tex.debug_tpage_name].push_back(level_tex.debug_name);
        }
      }

      // then add
      for (auto& [id, tex] : tex_db.textures) {
        auto db_tpage_name = tex_db.tpage_names.at(tex.page);
        for (auto& [wanted_tpage_name, wanted_tex_list] : wanted_texs) {
          auto processed = processed_textures[db_tpage_name];
          if (std::find(processed.begin(), processed.end(), tex.name) != processed.end()) {
            // lg::info("custom level: ignoring duplicate texture {} from {}", tex.name, tex.page);
            continue;
          }
          for (auto& wanted_tex : wanted_tex_list)
            if (db_tpage_name == wanted_tpage_name && tex.name == wanted_tex) {
              lg::info("custom level: adding texture {} from tpage {} ({})", tex.name, tex.page,
                       db_tpage_name);
              pc_level.textures.push_back(make_texture(id, tex, db_tpage_name, true));
              processed_textures[db_tpage_name].push_back(tex.name);
            }
        }
      }
    }
  }

  // add custom models to fr3
  if (level_json.contains("custom_models") && !level_json.at("custom_models").empty()) {
    auto models = level_json.at("custom_models").get<std::vector<std::string>>();
    for (auto& name : models) {
      add_model_to_level(GameVersion::Jak1, name, pc_level);
    }
  }

  // Save the GOAL level
  auto result = file.save_object_file();
  lg::print("Level bsp file size {} bytes\n", result.size());
  auto save_path = file_util::get_jak_project_dir() / bsp_output_file;
  file_util::create_dir_if_needed_for_file(save_path);
  lg::print("Saving to {}\n", save_path.string());
  file_util::write_binary_file(save_path, result.data(), result.size());

  // Save the PC level
  save_pc_data(file.name, pc_level,
               file_util::get_jak_project_dir() / "out" / output_prefix / "fr3");

  return true;
}
}  // namespace jak1
