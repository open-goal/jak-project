#include "build_level.h"

#include "common/custom_data/Tfrag3Data.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/compress.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"

#include "decompiler/level_extractor/extract_merc.h"
#include "goalc/build_level/Entity.h"
#include "goalc/build_level/FileInfo.h"
#include "goalc/build_level/LevelFile.h"
#include "goalc/build_level/Tfrag.h"
#include "goalc/build_level/collide_bvh.h"
#include "goalc/build_level/collide_pack.h"
#include "goalc/build_level/gltf_mesh_extract.h"

#include "third-party/fmt/core.h"

void save_pc_data(const std::string& nickname,
                  tfrag3::Level& data,
                  const fs::path& fr3_output_dir) {
  Serializer ser;
  data.serialize(ser);
  auto compressed =
      compression::compress_zstd(ser.get_save_result().first, ser.get_save_result().second);
  lg::print("stats for {}\n", data.level_name);
  print_memory_usage(data, ser.get_save_result().second);
  lg::print("compressed: {} -> {} ({:.2f}%)\n", ser.get_save_result().second, compressed.size(),
            100.f * compressed.size() / ser.get_save_result().second);
  file_util::write_binary_file(fr3_output_dir / fmt::format("{}.fr3", str_util::to_upper(nickname)),
                               compressed.data(), compressed.size());
}

std::vector<std::string> get_build_level_deps(const std::string& input_file) {
  auto level_json = parse_commented_json(
      file_util::read_text_file(file_util::get_file_path({input_file})), input_file);
  return {level_json.at("gltf_file").get<std::string>()};
}

// Find all art groups the custom level needs in a list of object files,
// skipping any that we already found in other dgos before
std::vector<decompiler::ObjectFileRecord> find_art_groups(
    std::vector<std::string>& processed_ags,
    const std::vector<std::string>& custom_level_ag,
    const std::vector<decompiler::ObjectFileRecord>& dgo_files) {
  std::vector<decompiler::ObjectFileRecord> art_groups;
  for (const auto& file : dgo_files) {
    // skip any art groups we already added from other dgos
    if (std::find(processed_ags.begin(), processed_ags.end(), file.name) != processed_ags.end()) {
      continue;
    }
    if (std::find(custom_level_ag.begin(), custom_level_ag.end(), file.name) !=
        custom_level_ag.end()) {
      art_groups.push_back(file);
      processed_ags.push_back(file.name);
    }
  }
  return art_groups;
}

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
  // ambients
  std::vector<EntityAmbient> ambients;
  add_ambients_from_json(level_json.at("ambients"), ambients, level_json.value("base_id", 12345));
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
  auto& tfrag_drawable_tree = file.drawable_trees.tfrags.emplace_back();
  tfrag_from_gltf(mesh_extract_out.tfrag, tfrag_drawable_tree,
                  pc_level.tfrag_trees[0].emplace_back());
  pc_level.textures = std::move(tex_pool.textures_by_idx);

  // COLLIDE
  if (mesh_extract_out.collide.faces.empty()) {
    lg::error("No collision geometry was found");
  } else {
    auto& collide_drawable_tree = file.drawable_trees.collides.emplace_back();
    collide_drawable_tree.bvh = collide::construct_collide_bvh(mesh_extract_out.collide.faces);
    collide_drawable_tree.packed_frags = pack_collide_frags(collide_drawable_tree.bvh.frags.frags);
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
  if (level_json.contains("art_groups") && !level_json.at("art_groups").empty()) {
    decompiler::Config config;
    try {
      config = decompiler::read_config_file(
          file_util::get_jak_project_dir() / "decompiler/config/jak1/jak1_config.jsonc", "ntsc_v1",
          R"({"decompile_code": false, "find_functions": false, "levels_extract": true, "allowed_objects": []})");
    } catch (const std::exception& e) {
      lg::error("Failed to parse config: {}", e.what());
      return false;
    }

    fs::path in_folder;
    lg::info("Looking for ISO path...");
    for (const auto& entry :
         fs::directory_iterator(file_util::get_jak_project_dir() / "iso_data")) {
      if (entry.is_directory() &&
          entry.path().filename().string().find("jak1") != std::string::npos) {
        lg::info("Found ISO path: {}", entry.path().string());
        in_folder = entry.path();
      }
    }
    if (!fs::exists(in_folder)) {
      lg::error("Could not find ISO path!");
      return false;
    }
    std::vector<fs::path> dgos, objs;
    for (const auto& dgo_name : config.dgo_names) {
      dgos.push_back(in_folder / dgo_name);
    }

    for (const auto& obj_name : config.object_file_names) {
      objs.push_back(in_folder / obj_name);
    }

    decompiler::ObjectFileDB db(dgos, fs::path(config.obj_file_name_map_file), objs, {}, {},
                                config);

    // need to process link data for tpages
    db.process_link_data(config);

    decompiler::TextureDB tex_db;
    auto textures_out = file_util::get_jak_project_dir() / "decompiler_out/jak1/textures";
    file_util::create_dir_if_needed(textures_out);
    db.process_tpages(tex_db, textures_out, config);

    std::vector<std::string> processed_art_groups;

    // find all art groups used by the custom level in other dgos
    for (auto& dgo : config.dgo_names) {
      // remove "DGO/" prefix
      const auto& dgo_name = dgo.substr(4);
      const auto& files = db.obj_files_by_dgo.at(dgo_name);
      auto art_groups = find_art_groups(
          processed_art_groups, level_json.at("art_groups").get<std::vector<std::string>>(), files);
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

  // Save the PC level
  save_pc_data(file.nickname, pc_level,
               file_util::get_jak_project_dir() / "out" / output_prefix / "fr3");

  return true;
}
