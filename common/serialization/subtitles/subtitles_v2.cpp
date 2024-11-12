#include "subtitles_v2.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "subtitles_v1.h"

#include "fmt/core.h"

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

// matches enum in `subtitle2.gc` with "none" (first) and "max" (last and removed)
const std::unordered_map<std::string, u16> jak2_speaker_name_to_enum_val = {
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
    {"precursor", 34},
    {"metalkor-before-consite", 35},
    {"metalkor-intro", 36}};

// matches enum in `subtitle3-h.gc` with "none" (first) and "max" (last and removed)
const std::unordered_map<std::string, u16> jak3_speaker_name_to_enum_val = {
    {"none", 0},
    {"jak", 1},
    {"darkjak", 2},
    {"daxter", 3},
    {"pecker", 4},
    {"ashelin", 5},
    {"veger", 6},
    {"samos", 7},
    {"damas", 8},
    {"kleiver", 9},
    {"seem", 10},
    {"errol", 11},
    {"errol-hologram", 12},
    {"sig", 13},
    {"torn", 14},
    {"tess", 15},
    {"guard", 16},
    {"guard-a", 17},
    {"guard-b", 18},
    {"keira", 19},
    {"vin", 20},
    {"onin", 21},
    {"jinx", 22},
    {"wastelander-male", 23},
    {"wastelander-female", 24},
    {"citizen-male", 25},
    {"citizen-female", 26},
    {"marauder", 27},
    {"oracle", 28},
    {"precursor", 29},
    {"ottsel-leader", 30},
    {"ottsel-surfer", 31},
    {"ottsel-dummy", 32},
    {"ottsel-veger", 33},
    {"ottsel-tess", 34},
    {"computer", 35},
    {"krew", 36},
    {"baron", 37},
    {"scherr", 38},
    {"arey", 39},
    {"baldwin", 40},
    {"schimpf", 41},
    {"martinsen", 42},
    {"phillips", 43},
    {"yates", 44},
};

GameSubtitlePackage read_json_files_v2(const GameSubtitleDefinitionFile& file_info) {
  GameSubtitlePackage package;
  SubtitleFile lang_lines;
  try {
    // If we have a base file defined, load that and merge it
    if (file_info.meta_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.meta_base_path.value()),
                               "subtitle_meta_base_path");
      package.base_meta = base_data;
      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("other").update(data.at("other"));
      package.combined_meta = base_data;
    } else {
      package.combined_meta = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
    }
    if (file_info.lines_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.lines_base_path.value()),
                               "subtitle_line_base_path");
      package.base_lines = base_data;
      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
      lang_lines = data;
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("other").update(data.at("other"));
      base_data.at("speakers").update(data.at("speakers"));
      package.combined_lines = base_data;
    } else {
      package.combined_lines = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
      lang_lines = package.combined_lines;
    }
    for (const auto& [scene_name, scene_info] : lang_lines.cutscenes) {
      package.scenes_defined_in_lang.insert(scene_name);
    }
    for (const auto& [scene_name, scene_info] : lang_lines.other) {
      package.scenes_defined_in_lang.insert(scene_name);
    }
  } catch (std::exception& e) {
    lg::error("Unable to parse subtitle json entry, couldn't successfully load files - {}",
              e.what());
    throw;
  }
  return package;
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
  bank->m_file_base_path = file_info.lines_base_path;
  try {
    if (m_subtitle_version == SubtitleFormat::V1) {
      const auto package = read_json_files_v1(file_info);
      bank->m_speakers = package.combined_lines.speakers;
      bank->add_scenes_from_files(package);
    } else {
      const auto package = read_json_files_v2(file_info);
      bank->m_speakers = package.combined_lines.speakers;
      bank->m_speakers.emplace("none", "none");
      bank->add_scenes_from_files(package);
    }
  } catch (std::exception& e) {
    throw;
  }
}

GameSubtitleSceneInfo GameSubtitleBank::new_scene_from_meta(
    const std::string& scene_name,
    const SubtitleSceneMetadata& scene_meta,
    const std::unordered_map<std::string, std::vector<std::string>>& relevant_lines) {
  GameSubtitleSceneInfo new_scene;
  new_scene.m_name = scene_name;
  new_scene.m_hint_id = scene_meta.m_hint_id;
  new_scene.only_defined_in_base = false;
  new_scene.is_cutscene = false;
  int line_idx = 0;
  int lines_added = 0;
  for (const auto& line_meta : scene_meta.lines) {
    // In V1, there was a concept of a "clear" line, you don't have to specify these in the "lines"
    // file as they are just blank lines.
    //
    // In V2, there are no longer "clear" lines, but there are lines that are "merged" which
    // essentially inherit the text from the base game (since Jak 2+ actually has subtitles!)
    //
    // In either case, we acknowledge that there is a line, but there is no text to retrieve at that
    // index.
    if (line_meta.merge || (relevant_lines.find(scene_name) != relevant_lines.end() &&
                            (int)relevant_lines.at(scene_name).size() > line_idx &&
                            relevant_lines.at(scene_name).at(line_idx).empty())) {
      new_scene.m_lines.push_back({"", line_meta});
      lines_added++;
    } else if (m_speakers.find(line_meta.speaker) == m_speakers.end() ||
               relevant_lines.find(scene_name) == relevant_lines.end() ||
               line_idx >= (int)relevant_lines.at(scene_name).size()) {
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

void GameSubtitleBank::add_scenes_from_files(const GameSubtitlePackage& package) {
  // Save the base and lang specific file info separately for later context
  for (const auto& [scene_name, scene_meta] : package.base_meta.cutscenes) {
    auto new_scene = new_scene_from_meta(scene_name, scene_meta, package.base_lines.cutscenes);
    new_scene.is_cutscene = true;
    m_base_scenes.emplace(scene_name, new_scene);
  }
  for (const auto& [scene_name, scene_meta] : package.base_meta.other) {
    auto new_scene = new_scene_from_meta(scene_name, scene_meta, package.base_lines.other);
    m_base_scenes.emplace(scene_name, new_scene);
  }
  // Iterate through the metadata file as blank lines are now omitted from the lines file now
  for (const auto& [scene_name, scene_meta] : package.combined_meta.cutscenes) {
    auto new_scene = new_scene_from_meta(scene_name, scene_meta, package.combined_lines.cutscenes);
    new_scene.is_cutscene = true;
    // Check if the only place lines were defined was in the base file
    if (package.scenes_defined_in_lang.find(scene_name) == package.scenes_defined_in_lang.end()) {
      new_scene.only_defined_in_base = true;
    }
    m_scenes.emplace(scene_name, new_scene);
  }
  for (const auto& [scene_name, scene_meta] : package.combined_meta.other) {
    auto new_scene = new_scene_from_meta(scene_name, scene_meta, package.combined_lines.other);
    // Check if the only place lines were defined was in the base file
    if (package.scenes_defined_in_lang.find(scene_name) == package.scenes_defined_in_lang.end()) {
      new_scene.only_defined_in_base = true;
    }
    m_scenes.emplace(scene_name, new_scene);
  }
}

std::vector<std::string> GameSubtitleBank::speaker_names_ordered_by_enum_value() {
  // Create a temporary vector of pairs (key, value)
  std::vector<std::pair<std::string, u16>> temp_vec;
  switch (m_text_version) {
    case GameTextVersion::JAK2:
      temp_vec = {jak2_speaker_name_to_enum_val.begin(), jak2_speaker_name_to_enum_val.end()};
      break;
    case GameTextVersion::JAK3:
      temp_vec = {jak3_speaker_name_to_enum_val.begin(), jak3_speaker_name_to_enum_val.end()};
      break;
    default:
      throw std::runtime_error(fmt::format("GameSubtitleBank: invalid game text version {} ({})",
                                           (int)m_text_version,
                                           get_text_version_name(m_text_version)));
  }
  // Sort the temporary vector based on the enum value in ascending order
  std::sort(temp_vec.begin(), temp_vec.end(),
            [](const auto& a, const auto& b) { return a.second < b.second; });
  // Extract the sorted keys into a new vector
  std::vector<std::string> sorted_names;
  sorted_names.reserve(temp_vec.size());
  for (const auto& pair : temp_vec) {
    if (pair.second == 0) {
      // we write #f for invalid entries, including the "none" at the start
      sorted_names.push_back("#f");
    } else {
      sorted_names.push_back(m_speakers.at(pair.first));
    }
  }
  return sorted_names;
}

u16 GameSubtitleBank::speaker_enum_value_from_name(const std::string& speaker_id) {
  std::unordered_map<std::string, u16> enum_map;
  switch (m_text_version) {
    case GameTextVersion::JAK2:
      enum_map = jak2_speaker_name_to_enum_val;
      break;
    case GameTextVersion::JAK3:
      enum_map = jak3_speaker_name_to_enum_val;
      break;
    default:
      throw std::runtime_error(fmt::format("GameSubtitleBank: invalid game text version {}",
                                           get_text_version_name(m_text_version)));
  }
  if (enum_map.find(speaker_id) == enum_map.end()) {
    throw std::runtime_error(
        fmt::format("'{}' speaker could not be found in the enum value mapping, update it or fix "
                    "the invalid speaker!",
                    speaker_id));
  }
  return u16(enum_map.at(speaker_id));
}

SubtitleMetadataFile dump_bank_meta_v2(const GameVersion game_version,
                                       std::shared_ptr<GameSubtitleBank> bank) {
  const auto dump_with_duplicates =
      dump_language_with_duplicates_from_base(game_version, bank->m_lang_id);
  (void)dump_with_duplicates;
  auto meta_file = SubtitleMetadataFile();
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    // Avoid dumping duplicates
    if (bank->m_base_scenes.find(scene_name) != bank->m_base_scenes.end() &&
        scene_info.same_metadata_as_other(bank->m_base_scenes.at(scene_name))) {
      continue;
    }
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

SubtitleFile dump_bank_lines_v2(const GameVersion game_version,
                                std::shared_ptr<GameSubtitleBank> bank) {
  const auto dump_with_duplicates =
      dump_language_with_duplicates_from_base(game_version, bank->m_lang_id);
  SubtitleFile file;
  file.speakers = bank->m_speakers;
  if (file.speakers.find("none") != file.speakers.end()) {
    file.speakers.erase("none");
  }
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    // Avoid dumping duplicates if needed
    if (!dump_with_duplicates &&
        bank->m_base_scenes.find(scene_name) != bank->m_base_scenes.end() &&
        scene_info.same_lines_as_other(bank->m_base_scenes.at(scene_name))) {
      continue;
    }
    for (const auto& scene_line : scene_info.m_lines) {
      // Skip merged lines
      if (scene_line.metadata.merge) {
        continue;
      }
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
        meta_file = dump_bank_meta_v1(game_version, bank);

      } else {
        meta_file = dump_bank_meta_v2(game_version, bank);
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
        lines_file = dump_bank_lines_v1(game_version, bank);
      } else {
        lines_file = dump_bank_lines_v2(game_version, bank);
      }
      dump_path =
          (file_util::get_jak_project_dir() / "game" / "assets" /
           version_to_game_name(game_version) / "subtitle" /
           fmt::format("subtitle_lines_{}.json", lookup_locale_code(game_version, language_id)))
              .string();
      file_util::write_text_file(dump_path, lines_file.dump(2));
    }
  } catch (std::exception& ex) {
    lg::error("{}", ex.what());
    return false;
  }
  return true;
}

GameSubtitleDB load_subtitle_project(const GameSubtitleDB::SubtitleFormat format_version,
                                     const GameVersion game_version) {
  // Load the subtitle files
  GameSubtitleDB db;
  db.m_subtitle_version = format_version;
  try {
    std::vector<GameSubtitleDefinitionFile> files;
    std::string subtitle_project = (file_util::get_jak_project_dir() / "game" / "assets" /
                                    version_to_game_name(game_version) / "game_subtitle.gp")
                                       .string();
    if (format_version == GameSubtitleDB::SubtitleFormat::V1) {
      open_subtitle_project("subtitle", subtitle_project, files);
    } else {
      open_subtitle_project("subtitle-v2", subtitle_project, files);
    }
    for (auto& file : files) {
      db.init_banks_from_file(file);
    }
  } catch (std::runtime_error& e) {
    // TODO - these run in gk, all exceptions must go...not reliable
    lg::error("error loading subtitle project: {}", e.what());
    db.m_load_error = e.what();
  }
  return db;
}
