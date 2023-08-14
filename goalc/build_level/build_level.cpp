#include "common/custom_data/Tfrag3Data.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/compress.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"

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

  // Save the PC level
  save_pc_data(file.nickname, pc_level,
               file_util::get_jak_project_dir() / "out" / output_prefix / "fr3");

  return true;
}
