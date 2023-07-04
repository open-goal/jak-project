#include "subtitles_v2.h"

#include <common/util/FileUtil.h>

#include "subtitles_v1.h"

#include "third-party/fmt/core.h"

void to_json(json& j, const SubtitleLineMetadata& obj) {
  json_serialize(frame_start);
  json_serialize(frame_end);
  json_serialize(offscreen);
  json_serialize(speaker);
  json_serialize(merge);
}

void from_json(const json& j, SubtitleLineMetadata& obj) {
  json_deserialize_if_exists(frame_start);
  json_deserialize_if_exists(frame_end);
  json_deserialize_if_exists(offscreen);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(merge);
}

void to_json(json& j, const SubtitleSceneMetadata& obj) {
  json_serialize(lines);
}

void from_json(const json& j, SubtitleSceneMetadata& obj) {
  json_deserialize_if_exists(lines);
}

void to_json(json& j, const SubtitleMetadataFile& obj) {
  json_serialize(cutscenes);
  json_serialize(other);
}

void from_json(const json& j, SubtitleMetadataFile& obj) {
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(other);
}

void to_json(json& j, const SubtitleFile& obj) {
  json_serialize(speakers);
  json_serialize(cutscenes);
  json_serialize(other);
}

void from_json(const json& j, SubtitleFile& obj) {
  json_deserialize_if_exists(speakers);
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(other);
}

// matches enum in `subtitle2.gc` with "none" (first) and "max" (last) removed
const std::unordered_map<std::string, int> jak2_speaker_name_to_enum_val = {
    {"none", 0},
    {"computer", 1},
    {"jak", 2},
    {"darkjak", 3},
    {"daxter", 4},
    {"samos", 5},
    {"keira", 6},
    {"keira-before-class-3", 7},
    {"kid", 8},
    {"kor", 9},
    {"metalkor", 10},
    {"baron", 11},
    {"errol", 12},
    {"torn", 13},
    {"tess", 14},
    {"guard", 15},
    {"guard-a", 16},
    {"guard-b", 17},
    {"krew", 18},
    {"sig", 19},
    {"brutter", 20},
    {"vin", 21},
    {"youngsamos", 22},
    {"youngsamos-before-rescue", 23},
    {"pecker", 24},
    {"onin", 25},
    {"ashelin", 26},
    {"jinx", 27},
    {"mog", 28},
    {"grim", 29},
    {"agent", 30},
    {"citizen-male", 31},
    {"citizen-female", 32},
    {"oracle", 33},
    {"precursor", 34}};

std::pair<SubtitleMetadataFile, SubtitleFile> read_json_files_v2(
    const GameSubtitleDefinitionFile& file_info) {
  SubtitleMetadataFile meta_file;
  SubtitleFile lines_file;
  try {
    // If we have a base file defined, load that and merge it
    if (file_info.meta_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.meta_base_path.value()),
                               "subtitle_meta_base_path");
      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("other").update(data.at("other"));
      meta_file = base_data;

    } else {
      meta_file = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
    }
    if (file_info.lines_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.lines_base_path.value()),
                               "subtitle_line_base_path");

      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("other").update(data.at("other"));
      base_data.at("speakers").update(data.at("speakers"));
      auto test = base_data.dump();
      lines_file = base_data;
    } else {
      lines_file = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
    }
  } catch (std::exception& e) {
    lg::error("Unable to parse subtitle json entry, couldn't successfully load files - {}",
              e.what());
    throw;
  }
  return {meta_file, lines_file};
}

void GameSubtitleDB::init_banks_from_file(const GameSubtitleDefinitionFile& file_info) {
  // Init Settings
  std::shared_ptr<GameSubtitleBank> bank;
  if (!bank_exists(file_info.language_id)) {
    // database has no lang yet
    bank = add_bank(std::make_shared<GameSubtitleBank>(file_info.language_id));
  } else {
    bank = bank_by_id(file_info.language_id);
  }
  bank->m_text_version = get_text_version_from_name(file_info.text_version);
  bank->m_file_path = file_info.lines_path;
  const auto font = get_font_bank(file_info.text_version);
  bank->m_game_version = m_game_version;
  // Parse the file
  if (m_subtitle_version == SubtitleFormat::V1) {
    const auto [meta_file, lines_file] = read_json_files_v1(file_info);
    bank->add_scenes_from_files(meta_file, lines_file);
  } else {
    const auto [meta_file, lines_file] = read_json_files_v2(file_info);
    bank->add_scenes_from_files(meta_file, lines_file);
  }
}

GameSubtitleSceneInfo GameSubtitleBank::new_scene_from_meta(
    const std::string& scene_name,
    const SubtitleSceneMetadata& scene_meta,
    const std::unordered_map<std::string, std::vector<std::string>>& relevant_lines) {
  GameSubtitleSceneInfo new_scene;
  new_scene.m_hint_id = scene_meta.m_hint_id;
  int line_idx = 0;
  int lines_added = 0;
  for (const auto& line_meta : scene_meta.lines) {
    // Caveat from v1, v2 doesn't have a clear-screen concept
    if (relevant_lines.find(scene_name) != relevant_lines.end() &&
        relevant_lines.at(scene_name).size() > line_idx &&
        relevant_lines.at(scene_name).at(line_idx).empty()) {
      new_scene.m_lines.push_back({relevant_lines.at(scene_name).at(line_idx), line_meta});
      lines_added++;
    } else if (m_speakers.find(line_meta.speaker) == m_speakers.end() ||
               relevant_lines.find(scene_name) == relevant_lines.end() ||
               relevant_lines.at(scene_name).size() < line_idx) {
      lg::warn(
          "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
          "be resolved {}!",
          m_lang_id, scene_name, line_meta.speaker);
    } else {
      new_scene.m_lines.push_back({relevant_lines.at(scene_name).at(line_idx), line_meta});
      lines_added++;
    }
    line_idx++;
  }
  // Verify we added the amount of lines we expected to
  if (lines_added != int(scene_meta.lines.size())) {
    throw std::runtime_error(
        fmt::format("Cutscene: '{}' has a mismatch in metadata lines vs text lines. Expected {} "
                    "only added {} lines",
                    scene_name, scene_meta.lines.size(), lines_added));
  }
  return new_scene;
}

void GameSubtitleBank::add_scenes_from_files(const SubtitleMetadataFile& meta_file,
                                             const SubtitleFile& lines_file) {
  // Set speaker map
  m_speakers = lines_file.speakers;
  // Iterate through the metadata file as blank lines are now omitted from the lines file now
  for (const auto& [scene_name, scene_meta] : meta_file.cutscenes) {
    m_scenes.emplace(scene_name, new_scene_from_meta(scene_name, scene_meta, lines_file.cutscenes));
  }
  for (const auto& [scene_name, scene_meta] : meta_file.other) {
    auto new_scene = new_scene_from_meta(scene_name, scene_meta, lines_file.other);
    new_scene.is_cutscene = false;
    m_scenes.emplace(scene_name, new_scene);
  }
}

u16 GameSubtitleBank::speaker_enum_value_from_name(const std::string& speaker_id) {
  if (m_game_version != GameVersion::Jak2) {
    throw std::runtime_error(
        "Speaker lookup is not yet implemented for anything other than jak 2 for the v2 subtitle "
        "format!");
  }
  if (jak2_speaker_name_to_enum_val.find(speaker_id) == jak2_speaker_name_to_enum_val.end()) {
    throw std::runtime_error(fmt::format(
        "'{}' speaker could not be found in the enum value mapping, update it!", speaker_id));
  }
  return u16(jak2_speaker_name_to_enum_val.at(speaker_id));
}

bool GameSubtitleBank::is_valid_speaker_id(const std::string& speaker_id) {
  if (m_game_version != GameVersion::Jak2) {
    throw std::runtime_error(
        "Speaker lookup is not yet implemented for anything other than jak 2 for the v2 subtitle "
        "format!");
  }
  if (jak2_speaker_name_to_enum_val.find(speaker_id) == jak2_speaker_name_to_enum_val.end()) {
    throw std::runtime_error(fmt::format(
        "'{}' speaker_id is not valid or is not yet wired up end-to-end, update it!", speaker_id));
  }
  return true;
}

SubtitleMetadataFile dump_bank_meta_v2(std::shared_ptr<GameSubtitleBank> bank) {
  auto meta_file = SubtitleMetadataFile();
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    SubtitleSceneMetadata scene_meta;
    for (const auto& line : scene_info.m_lines) {
      scene_meta.lines.push_back(line.metadata);
    }
    if (scene_info.is_cutscene) {
      meta_file.cutscenes[scene_name] = scene_meta;
    } else {
      meta_file.other[scene_name] = scene_meta;
    }
  }
  return meta_file;
}

SubtitleFile dump_bank_lines_v2(std::shared_ptr<GameSubtitleBank> bank) {
  SubtitleFile file;
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    for (const auto& line : scene_info.m_lines) {
      if (line.text.empty()) {
        continue;
      }
      // TODO - correct?
      file.speakers[line.metadata.speaker] = line.metadata.speaker;
    }
  }
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    for (const auto& scene_line : scene_info.m_lines) {
      if (scene_info.is_cutscene) {
        file.cutscenes[scene_name].push_back(scene_line.text);
      } else {
        file.other[scene_name].push_back(scene_line.text);
      }
    }
  }
  return file;
}

bool GameSubtitleDB::write_subtitle_db_to_files(const GameVersion game_version) {
  try {
    for (const auto& [language_id, bank] : m_banks) {
      json meta_file;
      if (m_subtitle_version == SubtitleFormat::V1) {
        meta_file = dump_bank_meta_v1(bank);

      } else {
        meta_file = dump_bank_meta_v2(bank);
      }
      std::string dump_path =
          (file_util::get_jak_project_dir() / "game" / "assets" /
           version_to_game_name(game_version) / "subtitle" /
           fmt::format("subtitle_meta_{}.json", lookup_locale_code(game_version, language_id)))
              .string();
      file_util::write_text_file(dump_path, meta_file.dump(2));
      // Now dump the actual subtitle lines
      json lines_file;
      if (m_subtitle_version == SubtitleFormat::V1) {
        lines_file = dump_bank_lines_v1(bank);
      } else {
        lines_file = dump_bank_lines_v2(bank);
      }
      dump_path =
          (file_util::get_jak_project_dir() / "game" / "assets" /
           version_to_game_name(game_version) / "subtitle" /
           fmt::format("subtitle_lines_{}.json", lookup_locale_code(game_version, language_id)))
              .string();
      file_util::write_text_file(dump_path, lines_file.dump(2));
    }
  } catch (std::exception& ex) {
    lg::error(ex.what());
    return false;
  }
  return true;
}

GameSubtitleDB load_subtitle_project(const GameSubtitleDB::SubtitleFormat format_version,
                                     const GameVersion game_version) {
  // Load the subtitle files
  GameSubtitleDB db;
  db.m_game_version = game_version;
  db.m_subtitle_version = format_version;
  try {
    std::vector<GameSubtitleDefinitionFile> files;
    std::string subtitle_project = (file_util::get_jak_project_dir() / "game" / "assets" /
                                    version_to_game_name(game_version) / "game_subtitle.gp")
                                       .string();
    open_subtitle_project("subtitle", subtitle_project, files);
    for (auto& file : files) {
      db.init_banks_from_file(file);
    }
  } catch (std::runtime_error& e) {
    lg::error("error loading subtitle project: {}", e.what());
  }

  return db;
}
