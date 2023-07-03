#include "subtitles_v1.h"

#include <common/util/FileUtil.h>

#include "third-party/fmt/core.h"

void to_json(json& j, const SubtitleCutsceneLineMetadataV1& obj) {
  j = json{{"frame_start", obj.frame_start},
           {"offscreen", obj.offscreen},
           {"speaker", obj.speaker},
           {"clear", obj.clear}};
}
void from_json(const json& j, SubtitleCutsceneLineMetadataV1& obj) {
  json_deserialize_if_exists(frame_start);
  json_deserialize_if_exists(offscreen);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(clear);
}

void to_json(json& j, const SubtitleHintLineMetadataV1& obj) {
  j = json{{"frame_start", obj.frame_start}, {"speaker", obj.speaker}, {"clear", obj.clear}};
}
void from_json(const json& j, SubtitleHintLineMetadataV1& obj) {
  json_deserialize_if_exists(frame_start);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(clear);
}
void to_json(json& j, const SubtitleHintMetadataV1& obj) {
  j = json{{"id", obj.id}, {"lines", obj.lines}};
}
void from_json(const json& j, SubtitleHintMetadataV1& obj) {
  json_deserialize_if_exists(id);
  json_deserialize_if_exists(lines);
}

void to_json(json& j, const SubtitleMetadataFileV1& obj) {
  j = json{{"cutscenes", obj.cutscenes}, {"hints", obj.hints}};
}

void from_json(const json& j, SubtitleMetadataFileV1& obj) {
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(hints);
}
void to_json(json& j, const SubtitleFileV1& obj) {
  j = json{{"speakers", obj.speakers}, {"cutscenes", obj.cutscenes}, {"hints", obj.hints}};
}
void from_json(const json& j, SubtitleFileV1& obj) {
  json_deserialize_if_exists(speakers);
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(hints);
}

void GameSubtitleDBV1::init_banks_from_file(const GameSubtitleDefinitionFile& file_info) {
  // TODO - some validation
  // Init Settings
  std::shared_ptr<GameSubtitleBankV1> bank;
  if (!bank_exists(file_info.language_id)) {
    // database has no lang yet
    bank = add_bank(std::make_shared<GameSubtitleBankV1>(file_info.language_id));
  } else {
    bank = bank_by_id(file_info.language_id);
  }
  bank->m_text_version = get_text_version_from_name(file_info.text_version);
  bank->m_file_path = file_info.lines_path;
  const auto font = get_font_bank(file_info.text_version);
  bank->add_scenes_from_file(file_info);
}

void GameSubtitleBankV1::add_scenes_from_file(const GameSubtitleDefinitionFile& file_info) {
  // Parse the file
  SubtitleMetadataFileV1 meta_file;
  SubtitleFileV1 lines_file;
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
      base_data.at("hints").update(data.at("hints"));
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
      base_data.at("hints").update(data.at("hints"));
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
  // Iterate through the metadata file as blank lines are now omitted from the lines file now
  // Cutscenes First
  for (const auto& [cutscene_name, cutscene_lines] : meta_file.cutscenes) {
    GameSubtitleSceneInfoV1 scene(GameSubtitleSceneInfoV1::SceneKind::Movie);
    scene.m_name = cutscene_name;
    // Iterate the lines, grab the actual text from the lines file if it's not a clear screen entry
    int line_idx = 0;
    int lines_added = 0;
    for (const auto& line_meta : cutscene_lines) {
      if (line_meta.clear) {
        scene.m_lines.push_back({"", line_meta});
        lines_added++;
      } else {
        if (lines_file.speakers.find(line_meta.speaker) == lines_file.speakers.end() ||
            lines_file.cutscenes.find(cutscene_name) == lines_file.cutscenes.end() ||
            int(lines_file.cutscenes.at(cutscene_name).size()) < line_idx) {
          lg::warn(
              "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
              "be resolved {}!",
              file_info.language_id, cutscene_name, line_meta.speaker);
        } else {
          scene.m_lines.push_back({lines_file.cutscenes.at(cutscene_name).at(line_idx), line_meta});
          lines_added++;
        }
        line_idx++;
      }
    }
    // Verify we added the amount of lines we expected to
    if (lines_added != int(cutscene_lines.size())) {
      throw std::runtime_error(
          fmt::format("Cutscene: '{}' has a mismatch in metadata lines vs text lines. Expected {} "
                      "only added {} lines",
                      cutscene_name, cutscene_lines.size(), lines_added));
    }
    m_scenes.emplace(cutscene_name, scene);
  }
  // Now hints
  for (const auto& [hint_name, hint_info] : meta_file.hints) {
    // TODO - check why this change was needed
    GameSubtitleSceneInfoV1 scene(GameSubtitleSceneInfoV1::SceneKind::HintNamed);
    if (hint_info.id == "0") {
      scene.m_kind = GameSubtitleSceneInfoV1::SceneKind::HintNamed;
    } else {
      scene.m_id = std::stoi(hint_info.id, nullptr, 16);
    }
    scene.m_name = hint_name;
    // Iterate the lines, grab the actual text from the lines file if it's not a clear screen entry
    int line_idx = 0;
    int lines_added = 0;
    for (const auto& line_meta : hint_info.lines) {
      if (line_meta.clear) {
        SubtitleCutsceneLineMetadataV1 meta;
        meta.clear = true;
        scene.m_lines.push_back({"", meta});
        lines_added++;
      } else {
        if (lines_file.speakers.find(line_meta.speaker) == lines_file.speakers.end() ||
            lines_file.hints.find(hint_name) == lines_file.hints.end() ||
            int(lines_file.hints.at(hint_name).size()) < line_idx) {
          lg::warn(
              "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
              "be resolved {}!",
              file_info.language_id, hint_name, line_meta.speaker);
        } else {
          // NOTE - the convert_utf8_to_game function is really really slow (about 80-90% of the
          // time loading the subtitle files)
          // TODO - improve that as a follow up sometime in the future
          SubtitleCutsceneLineMetadataV1 meta;
          meta.clear = false;
          meta.frame_start = line_meta.frame_start;
          meta.offscreen = true;
          meta.speaker = line_meta.speaker;
          scene.m_lines.push_back({lines_file.hints.at(hint_name).at(line_idx), meta});
          lines_added++;
        }
        line_idx++;
      }
    }
    // Verify we added the amount of lines we expected to
    if (lines_added != int(hint_info.lines.size())) {
      throw std::runtime_error(
          fmt::format("Hint: '{}' has a mismatch in metadata lines vs text lines. Expected {} "
                      "only added {} lines",
                      hint_name, hint_info.lines.size(), lines_added));
    }
    m_scenes.emplace(hint_name, scene);
  }
}

SubtitleMetadataFileV1 dump_bank_as_meta_json(std::shared_ptr<GameSubtitleBankV1> bank) {
  auto meta_file = SubtitleMetadataFileV1();
  auto font = get_font_bank(bank->m_text_version);
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == GameSubtitleSceneInfoV1::SceneKind::Movie) {
      std::vector<SubtitleCutsceneLineMetadataV1> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleCutsceneLineMetadataV1();
        line_meta.frame_start = line.metadata.frame_start;
        if (line.text.empty()) {
          line_meta.clear = true;
        } else {
          auto line_speaker = font->convert_game_to_utf8(line.metadata.speaker.c_str());
          line_meta.offscreen = line.metadata.offscreen;
          line_meta.speaker = line_speaker;
        }
        lines.push_back(line_meta);
      }
      meta_file.cutscenes[scene_name] = lines;
    } else if (scene_info.m_kind == GameSubtitleSceneInfoV1::SceneKind::Hint ||
               scene_info.m_kind == GameSubtitleSceneInfoV1::SceneKind::HintNamed) {
      SubtitleHintMetadataV1 hint;
      hint.id = fmt::format("{:x}", scene_info.m_id);
      std::vector<SubtitleHintLineMetadataV1> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleHintLineMetadataV1();
        line_meta.frame_start = line.metadata.frame_start;
        if (line.text.empty()) {
          line_meta.clear = true;
        } else {
          auto line_speaker = font->convert_game_to_utf8(line.metadata.speaker.c_str());
          line_meta.speaker = line_speaker;
        }
        lines.push_back(line_meta);
      }
      hint.lines = lines;
      meta_file.hints[scene_name] = hint;
    }
  }
  return meta_file;
}

SubtitleFileV1 dump_bank_as_json(std::shared_ptr<GameSubtitleBankV1> bank) {
  SubtitleFileV1 file;
  auto font = get_font_bank(bank->m_text_version);
  // Figure out speakers
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    for (const auto& line : scene_info.m_lines) {
      if (line.text.empty()) {
        continue;
      }
      auto line_speaker = font->convert_game_to_utf8(line.metadata.speaker.c_str());
      file.speakers[line_speaker] = line_speaker;
    }
  }
  // Hints
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == GameSubtitleSceneInfoV1::SceneKind::Hint ||
        scene_info.m_kind == GameSubtitleSceneInfoV1::SceneKind::HintNamed) {
      file.hints[scene_name] = {};
      for (const auto& scene_line : scene_info.m_lines) {
        if (scene_line.text.empty()) {
          continue;
        }
        auto line_utf8 = font->convert_game_to_utf8(scene_line.text.c_str());
        file.hints[scene_name].push_back(line_utf8);
      }
    }
  }

  // Cutscenes
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == GameSubtitleSceneInfoV1::SceneKind::Movie) {
      file.cutscenes[scene_name] = {};
      for (const auto& scene_line : scene_info.m_lines) {
        if (scene_line.text.empty()) {
          continue;
        }
        auto line_utf8 = font->convert_game_to_utf8(scene_line.text.c_str());
        file.cutscenes[scene_name].push_back(line_utf8);
      }
    }
  }

  return file;
}

bool GameSubtitleDBV1::write_subtitle_db_to_files(const GameVersion game_version) {
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

GameSubtitleDBV1 load_subtitle_project_v1(GameVersion game_version) {
  // Load the subtitle files
  GameSubtitleDBV1 db;
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
