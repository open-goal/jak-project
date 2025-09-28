#include "build_level.h"

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
  file_util::write_binary_file(fr3_output_dir / fmt::format("{}.fr3", nickname), compressed.data(),
                               compressed.size());
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

void add_model_to_level(GameVersion version, const std::string& name, tfrag3::Level& lvl) {
  lg::info("custom level: adding custom model {}", name);
  auto glb = name + ".glb";
  auto merc_data = load_merc_model(
      lvl.merc_data.indices.size(), lvl.merc_data.vertices.size(), lvl.textures.size(),
      fs::path(file_util::get_jak_project_dir() / "custom_assets" / game_version_names[version] /
               "models" / "custom_levels" / glb)
          .string(),
      name + "-lod0");
  for (auto& idx : merc_data.new_indices) {
    lvl.merc_data.indices.push_back(idx);
  }
  for (auto& vert : merc_data.new_vertices) {
    lvl.merc_data.vertices.push_back(vert);
  }

  lvl.merc_data.models.push_back(merc_data.new_model);
  lvl.textures.insert(lvl.textures.end(), merc_data.new_textures.begin(),
                      merc_data.new_textures.end());
}