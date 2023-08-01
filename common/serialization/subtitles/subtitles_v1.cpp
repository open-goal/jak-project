#include "subtitles_v1.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "subtitles_v2.h"

#include "third-party/fmt/core.h"

void to_json(json& j, const SubtitleCutsceneLineMetadataV1& obj) {
  json_serialize(frame_start);
  json_serialize(offscreen);
  json_serialize(speaker);
  json_serialize(clear);
}
void from_json(const json& j, SubtitleCutsceneLineMetadataV1& obj) {
  json_deserialize_if_exists(frame_start);
  json_deserialize_if_exists(offscreen);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(clear);
}

void to_json(json& j, const SubtitleHintLineMetadataV1& obj) {
  json_serialize(frame_start);
  json_serialize(speaker);
  json_serialize(clear);
}
void from_json(const json& j, SubtitleHintLineMetadataV1& obj) {
  json_deserialize_if_exists(frame_start);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(clear);
}
void to_json(json& j, const SubtitleHintMetadataV1& obj) {
  json_serialize(id);
  json_serialize(lines);
}
void from_json(const json& j, SubtitleHintMetadataV1& obj) {
  json_deserialize_if_exists(id);
  json_deserialize_if_exists(lines);
}

void to_json(json& j, const SubtitleMetadataFileV1& obj) {
  json_serialize(cutscenes);
  json_serialize(hints);
}

void from_json(const json& j, SubtitleMetadataFileV1& obj) {
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(hints);
}
void to_json(json& j, const SubtitleFileV1& obj) {
  json_serialize(speakers);
  json_serialize(cutscenes);
  json_serialize(hints);
}
void from_json(const json& j, SubtitleFileV1& obj) {
  json_deserialize_if_exists(speakers);
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(hints);
}

std::pair<SubtitleMetadataFile, SubtitleFile> convert_v1_to_v2(
    const GameSubtitleDefinitionFile& file_info,
    const SubtitleMetadataFileV1& v1_meta_file,
    const SubtitleFileV1& v1_lines_file) {
  // Convert the old format into the new
  SubtitleMetadataFile meta_file;
  SubtitleFile lines_file;
  lines_file.speakers = v1_lines_file.speakers;
  for (const auto& [cutscene_name, cutscene_lines] : v1_meta_file.cutscenes) {
    std::vector<std::string> scene_lines;
    SubtitleSceneMetadata new_scene_meta;
    // Iterate the lines, grab the actual text from the lines file if it's not a clear screen entry
    int line_idx = 0;
    int lines_added = 0;
    for (const auto& line_meta : cutscene_lines) {
      SubtitleLineMetadata new_meta;
      new_meta.frame_start = line_meta.frame_start;
      new_meta.offscreen = line_meta.offscreen;
      new_meta.speaker = line_meta.speaker;
      new_meta.frame_end = 0;  // v1 doesn't use frame_end
      new_meta.merge = false;  // or merge
      new_scene_meta.lines.push_back(new_meta);
      if (line_meta.clear) {
        scene_lines.push_back("");
        lines_added++;
      } else {
        if (v1_lines_file.speakers.find(line_meta.speaker) == v1_lines_file.speakers.end() ||
            v1_lines_file.cutscenes.find(cutscene_name) == v1_lines_file.cutscenes.end() ||
            int(v1_lines_file.cutscenes.at(cutscene_name).size()) < line_idx) {
          lg::warn(
              "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
              "be resolved {}!",
              file_info.language_id, cutscene_name, line_meta.speaker);
        } else {
          scene_lines.push_back(v1_lines_file.cutscenes.at(cutscene_name).at(line_idx));
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
    meta_file.cutscenes.emplace(cutscene_name, new_scene_meta);
    lines_file.cutscenes.emplace(cutscene_name, scene_lines);
  }
  // Now hints
  for (const auto& [hint_name, hint_info] : v1_meta_file.hints) {
    std::vector<std::string> scene_lines;
    SubtitleSceneMetadata new_scene_meta;
    if (hint_info.id != "0") {
      new_scene_meta.m_hint_id = std::stoi(hint_info.id, nullptr, 16);
    }
    // Iterate the lines, grab the actual text from the lines file if it's not a clear screen entry
    int line_idx = 0;
    int lines_added = 0;
    for (const auto& line_meta : hint_info.lines) {
      SubtitleLineMetadata new_meta;
      new_meta.frame_start = line_meta.frame_start;
      new_meta.frame_end = 0;  // unused by v1
      new_meta.offscreen = true;
      new_meta.speaker = line_meta.speaker;
      new_scene_meta.lines.push_back(new_meta);
      if (line_meta.clear) {
        scene_lines.push_back("");
        lines_added++;
      } else {
        if (v1_lines_file.speakers.find(line_meta.speaker) == v1_lines_file.speakers.end() ||
            v1_lines_file.hints.find(hint_name) == v1_lines_file.hints.end() ||
            line_idx >= (int)v1_lines_file.hints.at(hint_name).size()) {
          lg::warn(
              "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
              "be resolved {}!",
              file_info.language_id, hint_name, line_meta.speaker);
        } else {
          scene_lines.push_back(v1_lines_file.hints.at(hint_name).at(line_idx));
          lines_added++;
        }
        line_idx++;
      }
    }
    // Verify we added the amount of lines we expected to
    if (lines_added != (int)hint_info.lines.size()) {
      throw std::runtime_error(
          fmt::format("Hint: '{}' has a mismatch in metadata lines vs text lines. Expected {} "
                      "only added {} lines",
                      hint_name, hint_info.lines.size(), lines_added));
    }
    meta_file.other.emplace(hint_name, new_scene_meta);
    lines_file.other.emplace(hint_name, scene_lines);
  }
  return {meta_file, lines_file};
}

GameSubtitlePackage read_json_files_v1(const GameSubtitleDefinitionFile& file_info) {
  // Parse the files
  SubtitleMetadataFileV1 v1_meta_base_file;
  SubtitleMetadataFileV1 v1_meta_combined_file;
  SubtitleFileV1 v1_lines_base_file;
  SubtitleFileV1 v1_lines_lang_file;
  SubtitleFileV1 v1_lines_combined_file;
  try {
    // If we have a base file defined, load that and merge it
    if (file_info.meta_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.meta_base_path.value()),
                               "subtitle_meta_base_path");
      v1_meta_base_file = base_data;
      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("hints").update(data.at("hints"));
      v1_meta_combined_file = base_data;
    } else {
      v1_meta_combined_file = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
    }
    if (file_info.lines_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.lines_base_path.value()),
                               "subtitle_line_base_path");
      v1_lines_base_file = base_data;
      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
      v1_lines_lang_file = data;
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("hints").update(data.at("hints"));
      base_data.at("speakers").update(data.at("speakers"));
      v1_lines_combined_file = base_data;
    } else {
      v1_lines_combined_file = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
      v1_lines_lang_file = v1_lines_combined_file;
    }
    GameSubtitlePackage package;
    std::tie(package.base_meta, package.base_lines) =
        convert_v1_to_v2(file_info, v1_meta_base_file, v1_lines_base_file);
    std::tie(package.combined_meta, package.combined_lines) =
        convert_v1_to_v2(file_info, v1_meta_combined_file, v1_lines_combined_file);
    for (const auto& [scene_name, scene_info] : v1_lines_lang_file.cutscenes) {
      package.scenes_defined_in_lang.insert(scene_name);
    }
    for (const auto& [scene_name, scene_info] : v1_lines_lang_file.hints) {
      package.scenes_defined_in_lang.insert(scene_name);
    }
    return package;
  } catch (std::exception& e) {
    lg::error("Unable to parse subtitle json entry, couldn't successfully load files - {}",
              e.what());
    throw;
  }
}

SubtitleMetadataFileV1 dump_bank_meta_v1(const GameVersion /*game_version*/,
                                         std::shared_ptr<GameSubtitleBank> bank) {
  auto meta_file = SubtitleMetadataFileV1();
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    // Avoid dumping duplicates
    if (bank->m_base_scenes.find(scene_name) != bank->m_base_scenes.end() &&
        scene_info.same_metadata_as_other(bank->m_base_scenes.at(scene_name))) {
      continue;
    }
    if (scene_info.is_cutscene) {
      std::vector<SubtitleCutsceneLineMetadataV1> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleCutsceneLineMetadataV1();
        line_meta.frame_start = line.metadata.frame_start;
        if (line.text.empty()) {
          line_meta.clear = true;
        } else {
          line_meta.offscreen = line.metadata.offscreen;
          line_meta.speaker = line.metadata.speaker;
        }
        lines.push_back(line_meta);
      }
      meta_file.cutscenes[scene_name] = lines;
    } else {
      SubtitleHintMetadataV1 hint;
      hint.id = fmt::format("{:x}", scene_info.m_hint_id);
      std::vector<SubtitleHintLineMetadataV1> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleHintLineMetadataV1();
        line_meta.frame_start = line.metadata.frame_start;
        if (line.text.empty()) {
          line_meta.clear = true;
        } else {
          line_meta.speaker = line.metadata.speaker;
        }
        lines.push_back(line_meta);
      }
      hint.lines = lines;
      meta_file.hints[scene_name] = hint;
    }
  }
  return meta_file;
}

SubtitleFileV1 dump_bank_lines_v1(const GameVersion game_version,
                                  std::shared_ptr<GameSubtitleBank> bank) {
  const auto dump_with_duplicates =
      dump_language_with_duplicates_from_base(game_version, bank->m_lang_id);
  SubtitleFileV1 file;
  file.speakers = bank->m_speakers;
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    // Avoid dumping duplicates if needed
    if (!dump_with_duplicates &&
        bank->m_base_scenes.find(scene_name) != bank->m_base_scenes.end() &&
        scene_info.same_lines_as_other(bank->m_base_scenes.at(scene_name))) {
      continue;
    }
    if (scene_info.is_cutscene) {
      file.cutscenes[scene_name] = {};
      for (const auto& scene_line : scene_info.m_lines) {
        if (scene_line.text.empty()) {
          continue;
        }
        file.cutscenes[scene_name].push_back(scene_line.text);
      }
    } else {
      file.hints[scene_name] = {};
      for (const auto& scene_line : scene_info.m_lines) {
        if (scene_line.text.empty()) {
          continue;
        }
        file.hints[scene_name].push_back(scene_line.text);
      }
    }
  }

  return file;
}
