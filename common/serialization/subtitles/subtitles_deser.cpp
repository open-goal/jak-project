#include "subtitles_deser.h"

#include <algorithm>
#include <regex>
#include <vector>

#include "common/util/FileUtil.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/ranges.h"
#include "third-party/json.hpp"

SubtitleMetadataFile dump_bank_as_meta_json(std::shared_ptr<GameSubtitleBank> bank) {
  auto meta_file = SubtitleMetadataFile();
  auto font = get_font_bank(bank->m_text_version);
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == SubtitleSceneKind::Movie) {
      std::vector<SubtitleCutsceneLineMetadata> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleCutsceneLineMetadata();
        line_meta.frame = line.frame;
        if (line.line.empty()) {
          line_meta.clear = true;
        } else {
          auto line_speaker = font->convert_game_to_utf8(line.speaker.c_str());
          line_meta.offscreen = line.offscreen;
          line_meta.speaker = line_speaker;
        }
        lines.push_back(line_meta);
      }
      meta_file.cutscenes[scene_name] = lines;
    } else if (scene_info.m_kind == SubtitleSceneKind::Hint ||
               scene_info.m_kind == SubtitleSceneKind::HintNamed) {
      SubtitleHintMetadata hint;
      hint.id = fmt::format("{:x}", scene_info.m_id);
      std::vector<SubtitleHintLineMetadata> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleHintLineMetadata();
        line_meta.frame = line.frame;
        if (line.line.empty()) {
          line_meta.clear = true;
        } else {
          auto line_speaker = font->convert_game_to_utf8(line.speaker.c_str());
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

SubtitleFile dump_bank_as_json(std::shared_ptr<GameSubtitleBank> bank) {
  SubtitleFile file;
  auto font = get_font_bank(bank->m_text_version);
  // Figure out speakers
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    for (const auto& line : scene_info.m_lines) {
      if (line.line.empty()) {
        continue;
      }
      auto line_speaker = font->convert_game_to_utf8(line.speaker.c_str());
      file.speakers[line_speaker] = line_speaker;
    }
  }
  // Hints
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == SubtitleSceneKind::Hint ||
        scene_info.m_kind == SubtitleSceneKind::HintNamed) {
      file.hints[scene_name] = {};
      for (const auto& scene_line : scene_info.m_lines) {
        if (scene_line.line.empty()) {
          continue;
        }
        auto line_utf8 = font->convert_game_to_utf8(scene_line.line.c_str());
        file.hints[scene_name].push_back(line_utf8);
      }
    }
  }

  // Cutscenes
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == SubtitleSceneKind::Movie) {
      file.cutscenes[scene_name] = {};
      for (const auto& scene_line : scene_info.m_lines) {
        if (scene_line.line.empty()) {
          continue;
        }
        auto line_utf8 = font->convert_game_to_utf8(scene_line.line.c_str());
        file.cutscenes[scene_name].push_back(line_utf8);
      }
    }
  }

  return file;
}

const std::vector<std::string> locale_lookup = {
    "en-US", "fr-FR", "de-DE", "es-ES", "it-IT", "jp-JP", "en-GB", "pt-PT", "fi-FI",
    "sv-SE", "da-DK", "no-NO", "nl-NL", "pt-BR", "hu-HU", "ca-ES", "is-IS"};

bool write_subtitle_db_to_files(const GameSubtitleDB& db, const GameVersion game_version) {
  try {
    for (const auto& [language_id, bank] : db.m_banks) {
      auto meta_file = dump_bank_as_meta_json(bank);
      std::string dump_path = (file_util::get_jak_project_dir() / "game" / "assets" /
                               version_to_game_name(game_version) / "subtitle" /
                               fmt::format("subtitle_meta_{}.json", locale_lookup.at(language_id)))
                                  .string();
      json data = meta_file;
      file_util::write_text_file(dump_path, data.dump(2));
      // Now dump the actual subtitles
      auto subtitle_file = dump_bank_as_json(bank);
      dump_path = (file_util::get_jak_project_dir() / "game" / "assets" /
                   version_to_game_name(game_version) / "subtitle" /
                   fmt::format("subtitle_lines_{}.json", locale_lookup.at(language_id)))
                      .string();
      data = subtitle_file;
      file_util::write_text_file(dump_path, data.dump(2));
    }
    // Write the subtitle group info out
    nlohmann::json json(db.m_subtitle_groups->m_groups);
    json[db.m_subtitle_groups->group_order_key] =
        nlohmann::json(db.m_subtitle_groups->m_group_order);
    std::string file_path =
        (file_util::get_jak_project_dir() / "game" / "assets" / version_to_game_name(game_version) /
         "subtitle" / "subtitle-groups.json")
            .string();
    file_util::write_text_file(file_path, json.dump(2));
  } catch (std::exception& ex) {
    lg::error(ex.what());
    return false;
  }
  return true;
}
