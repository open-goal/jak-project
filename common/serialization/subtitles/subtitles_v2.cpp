#include "subtitles_v2.h"

#include <common/util/FileUtil.h>

#include "third-party/fmt/core.h"

void to_json(json& j, const SubtitleLineMetadataV2& obj) {
  json_serialize(frame_start);
  json_serialize(frame_end);
  json_serialize(offscreen);
  json_serialize(speaker);
  json_serialize(merge);
}

void from_json(const json& j, SubtitleLineMetadataV2& obj) {
  json_deserialize_if_exists(frame_start);
  json_deserialize_if_exists(frame_end);
  json_deserialize_if_exists(offscreen);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(merge);
}

void to_json(json& j, const SubtitleCutsceneMetadataV2& obj) {
  json_serialize(is_cutscene);
  json_serialize(lines);
}

void from_json(const json& j, SubtitleCutsceneMetadataV2& obj) {
  json_deserialize_if_exists(is_cutscene);
  json_deserialize_if_exists(lines);
}

void to_json(json& j, const SubtitleMetadataFileV2& obj) {
  json_serialize(scenes);
}

void from_json(const json& j, SubtitleMetadataFileV2& obj) {
  json_deserialize_if_exists(scenes);
}

void to_json(json& j, const SubtitleFileV2& obj) {
  json_serialize(speakers);
  json_serialize(scenes);
}

void from_json(const json& j, SubtitleFileV2& obj) {
  json_deserialize_if_exists(speakers);
  json_deserialize_if_exists(scenes);
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

void GameSubtitleDBV2::init_banks_from_file(const GameSubtitleDefinitionFile& file_info) {
  // TODO - some validation
  // Init Settings
  std::shared_ptr<GameSubtitleBankV2> bank;
  if (!bank_exists(file_info.language_id)) {
    // database has no lang yet
    bank = add_bank(std::make_shared<GameSubtitleBankV2>(file_info.language_id));
  } else {
    bank = bank_by_id(file_info.language_id);
  }
  bank->m_text_version = get_text_version_from_name(file_info.text_version);
  bank->m_file_path = file_info.lines_path;
  const auto font = get_font_bank(file_info.text_version);
  bank->add_scenes_from_file(file_info);
  // TODO - pass through game version, or just infer it from the game text version
  bank->m_game_version = GameVersion::Jak2;
}

void GameSubtitleBankV2::add_scenes_from_file(const GameSubtitleDefinitionFile& file_info) {
  // Parse the file
  SubtitleMetadataFileV2 meta_file;
  SubtitleFileV2 lines_file;
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
      base_data.at("scenes").update(data.at("scenes"));
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
      base_data.at("scenes").update(data.at("scenes"));
      base_data.at("speakers").update(data.at("speakers"));
      auto test = base_data.dump();
      lines_file = base_data;
      // Set speaker map
      m_speakers = lines_file.speakers;
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
  // Iterate through the metadata file as blank lines are now omitted from the lines file now
  for (const auto& [scene_name, scene_meta] : meta_file.scenes) {
    GameSubtitleSceneInfoV2 scene;
    scene.is_cutscene = scene_meta.is_cutscene;
    // Iterate the lines, grab the actual text from the lines file if it's not a clear screen entry
    int line_idx = 0;
    int lines_added = 0;
    for (const auto& line_meta : scene_meta.lines) {
      if (m_speakers.find(line_meta.speaker) == m_speakers.end() ||
          lines_file.scenes.find(scene_name) == lines_file.scenes.end() ||
          int(lines_file.scenes.at(scene_name).size()) < line_idx) {
        lg::warn(
            "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
            "be resolved {}!",
            file_info.language_id, scene_name, line_meta.speaker);
      } else {
        scene.m_lines.push_back({lines_file.scenes.at(scene_name).at(line_idx), line_meta});
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
    m_scenes.emplace(scene_name, scene);
  }
}

u16 GameSubtitleBankV2::speaker_enum_value_from_name(const std::string& speaker_id) {
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

bool GameSubtitleBankV2::is_valid_speaker_id(const std::string& speaker_id) {
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

SubtitleMetadataFileV2 dump_bank_as_meta_json(std::shared_ptr<GameSubtitleBankV2> bank) {
  auto meta_file = SubtitleMetadataFileV2();
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    SubtitleCutsceneMetadataV2 scene_meta;
    scene_meta.is_cutscene = scene_info.is_cutscene;
    for (const auto& line : scene_info.m_lines) {
      auto line_meta = SubtitleLineMetadataV2();
      line_meta.frame_start = line.metadata.frame_start;
      line_meta.offscreen = line.metadata.offscreen;
      line_meta.speaker = line.metadata.speaker;
      scene_meta.lines.push_back(line_meta);
    }
    meta_file.scenes[scene_name] = scene_meta;
  }
  return meta_file;
}

SubtitleFileV2 dump_bank_as_json(std::shared_ptr<GameSubtitleBankV2> bank) {
  SubtitleFileV2 file;
  // Figure out speakers
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    for (const auto& line : scene_info.m_lines) {
      if (line.text.empty()) {
        continue;
      }
      // TODO - correct?
      file.speakers[line.metadata.speaker] = line.metadata.speaker;
    }
  }
  // Cutscenes
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    for (const auto& scene_line : scene_info.m_lines) {
      file.scenes[scene_name].push_back(scene_line.text);
    }
  }
  return file;
}

bool GameSubtitleDBV2::write_subtitle_db_to_files(const GameVersion game_version) {
  try {
    for (const auto& [language_id, bank] : m_banks) {
      auto meta_file = dump_bank_as_meta_json(bank);
      std::string dump_path =
          (file_util::get_jak_project_dir() / "game" / "assets" /
           version_to_game_name(game_version) / "subtitle" /
           fmt::format("subtitle_meta_{}.json", lookup_locale_code(game_version, language_id)))
              .string();
      json data = meta_file;
      file_util::write_text_file(dump_path, data.dump(2));
      // Now dump the actual subtitles
      auto subtitle_file = dump_bank_as_json(bank);
      dump_path =
          (file_util::get_jak_project_dir() / "game" / "assets" /
           version_to_game_name(game_version) / "subtitle" /
           fmt::format("subtitle_lines_{}.json", lookup_locale_code(game_version, language_id)))
              .string();
      data = subtitle_file;
      file_util::write_text_file(dump_path, data.dump(2));
    }
  } catch (std::exception& ex) {
    lg::error(ex.what());
    return false;
  }
  return true;
}

GameSubtitleDBV2 load_subtitle_project_v2(GameVersion game_version) {
  // Load the subtitle files
  GameSubtitleDBV2 db;
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
