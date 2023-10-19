#include "build_level.h"

#include "decompiler/extractor/extractor_util.h"
#include "decompiler/level_extractor/extract_merc.h"
#include "goalc/build_level/collide/jak2/collide.h"
#include "goalc/build_level/common/Tfrag.h"
#include "goalc/build_level/jak2/Entity.h"
#include "goalc/build_level/jak2/FileInfo.h"
#include "goalc/build_level/jak2/LevelFile.h"

namespace jak2 {
bool run_build_level(const std::string& input_file,
                     const std::string& bsp_output_file,
                     const std::string& output_prefix) {
  auto level_json = parse_commented_json(
      file_util::read_text_file(file_util::get_file_path({input_file})), input_file);
  LevelFile file;          // GOAL level file
  tfrag3::Level pc_level;  // PC level file
  TexturePool tex_pool;    // pc level texture pool

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
  add_actors_from_json(level_json.at("actors"), actors, level_json.value("base_id", 1234));
  file.actors = std::move(actors);
  // cameras
  // nodes
  // regions
  // subdivs
  // actor birth
  for (size_t i = 0; i < file.actors.size(); i++) {
    file.actor_birth_order.push_back(i);
  }

  // add stuff to the PC level structure
  pc_level.level_name = file.name;

  // TFRAG
  auto& tfrag_drawable_tree = file.drawable_trees.tfrags.emplace_back();
  tfrag_from_gltf(mesh_extract_out.tfrag, tfrag_drawable_tree,
                  pc_level.tfrag_trees[0].emplace_back());
  pc_level.textures = std::move(tex_pool.textures_by_idx);

  // COLLIDE
  if (mesh_extract_out.collide.faces.empty()) {
    lg::error("No collision geometry was found");
  } else {
    file.collide_hash = construct_collide_hash(mesh_extract_out.collide.faces);
  }

  // Save the GOAL level
  auto result = file.save_object_file();
  lg::print("Level bsp file size {} bytes\n", result.size());
  auto save_path = file_util::get_jak_project_dir() / bsp_output_file;
  file_util::create_dir_if_needed_for_file(save_path);
  lg::print("Saving to {}\n", save_path.string());
  file_util::write_binary_file(save_path, result.data(), result.size());

  // Add textures and models
  // TODO remove hardcoded config settings
  if ((level_json.contains("art_groups") && !level_json.at("art_groups").empty()) ||
      (level_json.contains("textures") && !level_json.at("textures").empty())) {
    fs::path iso_folder = "";
    lg::info("Looking for ISO path...");
    // TODO - add to file_util
    for (const auto& entry :
         fs::directory_iterator(file_util::get_jak_project_dir() / "iso_data")) {
      // TODO - hard-coded to jak 2
      if (entry.is_directory() &&
          entry.path().filename().string().find("jak2") != std::string::npos) {
        lg::info("Found ISO path: {}", entry.path().string());
        iso_folder = entry.path();
      }
    }

    if (iso_folder.empty() || !fs::exists(iso_folder)) {
      lg::warn("Could not locate ISO path!");
      return false;
    }

    // Look for iso build info if it's available, otherwise default to ntsc_v1
    const auto version_info = get_version_info_or_default(iso_folder);

    decompiler::Config config;
    try {
      config = decompiler::read_config_file(
          file_util::get_jak_project_dir() / "decompiler/config/jak2/jak2_config.jsonc",
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

    decompiler::ObjectFileDB db(dgos, fs::path(config.obj_file_name_map_file), objs, {}, {},
                                config);

    // need to process link data for tpages
    db.process_link_data(config);

    decompiler::TextureDB tex_db;
    auto textures_out = file_util::get_jak_project_dir() / "decompiler_out/jak2/textures";
    file_util::create_dir_if_needed(textures_out);
    db.process_tpages(tex_db, textures_out, config);

    // find all art groups used by the custom level in other dgos
    if (level_json.contains("art_groups") && !level_json.at("art_groups").empty()) {
      for (auto& dgo : config.dgo_names) {
        std::vector<std::string> processed_art_groups;
        // remove "DGO/" prefix
        const auto& dgo_name = dgo.substr(4);
        const auto& files = db.obj_files_by_dgo.at(dgo_name);
        auto art_groups =
            find_art_groups(processed_art_groups,
                            level_json.at("art_groups").get<std::vector<std::string>>(), files);
        auto tex_remap = decompiler::extract_tex_remap(db, dgo_name);
        for (const auto& ag : art_groups) {
          if (ag.name.length() > 3 && !ag.name.compare(ag.name.length() - 3, 3, "-ag")) {
            const auto& ag_file = db.lookup_record(ag);
            lg::print("custom level: extracting art group {}\n", ag_file.name_in_dgo);
            decompiler::extract_merc(ag_file, tex_db, db.dts, tex_remap, pc_level, false,
                                     db.version());
          }
        }
      }
    }

    // add textures
    if (level_json.contains("textures") && !level_json.at("textures").empty()) {
      std::vector<std::string> processed_textures;
      std::vector<std::string> wanted_texs =
          level_json.at("textures").get<std::vector<std::string>>();
      // first check the texture is not already in the level
      for (auto& level_tex : pc_level.textures) {
        if (std::find(wanted_texs.begin(), wanted_texs.end(), level_tex.debug_name) !=
            wanted_texs.end()) {
          processed_textures.push_back(level_tex.debug_name);
        }
      }

      // then add
      for (auto& [id, tex] : tex_db.textures) {
        for (auto& tex0 : wanted_texs) {
          if (std::find(processed_textures.begin(), processed_textures.end(), tex.name) !=
              processed_textures.end()) {
            continue;
          }
          if (tex.name == tex0) {
            lg::info("custom level: adding texture {} from tpage {} ({})", tex.name, tex.page,
                     tex_db.tpage_names.at(tex.page));
            pc_level.textures.push_back(
                make_texture(id, tex, tex_db.tpage_names.at(tex.page), true));
            processed_textures.push_back(tex.name);
          }
        }
      }
    }
  }

  // Save the PC level
  save_pc_data(file.nickname, pc_level,
               file_util::get_jak_project_dir() / "out" / output_prefix / "fr3");

  return true;
}
}  // namespace jak2