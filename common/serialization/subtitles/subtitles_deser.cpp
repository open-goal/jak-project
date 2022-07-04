#include "subtitles_deser.h"

#include <algorithm>
#include <regex>
#include <vector>

#include "common/util/FileUtil.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/ranges.h"
#include "third-party/json.hpp"

bool write_subtitle_db_to_files(const GameSubtitleDB& db) {
  // Write the subtitles out
  std::vector<int> completed_banks = {};
  for (const auto& [id, bank] : db.m_banks) {
    // If we've done the bank before, skip it
    auto it = find(completed_banks.begin(), completed_banks.end(), bank->m_lang_id);
    if (it != completed_banks.end()) {
      continue;
    }
    // Check to see if this bank is shared by any other, if so do it at the same time
    // and skip it
    // This is basically just to deal with US/UK english in a not so hacky way
    std::vector<int> banks = {};
    for (const auto& [_id, _bank] : db.m_banks) {
      if (_bank->file_path == bank->file_path) {
        banks.push_back(_bank->m_lang_id);
        completed_banks.push_back(_bank->m_lang_id);
      }
    }

    std::string file_contents = "";
    file_contents += fmt::format("(language-id {})\n", fmt::join(banks, " "));
    auto file_ver = parse_text_only_version(bank->file_path);
    auto font = get_font_bank(file_ver);
    file_contents += fmt::format("(text-version {})\n", get_text_version_name(file_ver));

    for (const auto& group_name : db.m_subtitle_groups->m_group_order) {
      bool last_was_single = false;
      file_contents +=
          fmt::format("\n;; -----------------\n;; {}\n;; -----------------\n", group_name);
      std::vector<GameSubtitleSceneInfo> all_scenes;
      for (const auto& [scene_name, scene] : bank->scenes()) {
        all_scenes.push_back(scene);
      }
      std::sort(all_scenes.begin(), all_scenes.end(),
                [](const GameSubtitleSceneInfo& a, const GameSubtitleSceneInfo& b) {
                  if (a.kind() != b.kind()) {
                    return a.kind() < b.kind();
                  }
                  if (a.kind() == SubtitleSceneKind::Movie) {
                    return a.name() < b.name();
                  } else if (a.kind() == SubtitleSceneKind::HintNamed) {
                    if (a.id() == b.id()) {
                      return a.name() < b.name();
                    } else {
                      return a.id() < b.id();
                    }
                  } else if (a.kind() == SubtitleSceneKind::Hint) {
                    return a.id() < b.id();
                  }
                  return false;
                });
      for (const auto& scene : all_scenes) {
        if (scene.m_sorting_group != group_name) {
          continue;
        }

        if (last_was_single && scene.lines().size() == 1) {
          file_contents += fmt::format("(\"{}\"", scene.name());
        } else {
          file_contents += fmt::format("\n(\"{}\"", scene.name());
        }
        if (scene.kind() == SubtitleSceneKind::Hint) {
          file_contents += " :hint #x0";
        } else if (scene.kind() == SubtitleSceneKind::HintNamed) {
          file_contents += fmt::format(" :hint #x{0:x}", scene.id());
        }
        // more compact formatting for single-line entries
        if (scene.lines().size() == 1) {
          const auto& line = scene.lines().at(0);
          if (line.line.empty()) {
            file_contents += fmt::format(" ({})", line.frame);
          } else {
            file_contents += fmt::format(" ({}", line.frame);
            if (line.offscreen && scene.kind() == SubtitleSceneKind::Movie) {
              file_contents += " :offscreen";
            }
            file_contents +=
                fmt::format(" \"{}\"", font->convert_game_to_utf8(line.speaker.c_str()));
            file_contents += fmt::format(" \"{}\")", font->convert_game_to_utf8(line.line.c_str()));
          }
          file_contents += ")\n";
          last_was_single = true;
        } else {
          file_contents += "\n";
          for (auto& line : scene.lines()) {
            // Clear screen entries
            if (line.line.empty()) {
              file_contents += fmt::format("  ({})\n", line.frame);
            } else {
              file_contents += fmt::format("  ({}", line.frame);
              if (line.offscreen && scene.kind() == SubtitleSceneKind::Movie) {
                file_contents += " :offscreen";
              }
              file_contents +=
                  fmt::format(" \"{}\"", font->convert_game_to_utf8(line.speaker.c_str()));
              file_contents +=
                  fmt::format(" \"{}\")\n", font->convert_game_to_utf8(line.line.c_str()));
            }
          }
          file_contents += "  )\n";
          last_was_single = false;
        }
      }
    }

    // Commit it to the file
    std::string full_path =
        (file_util::get_jak_project_dir() / std::filesystem::path(bank->file_path)).string();
    file_util::write_text_file(full_path, file_contents);
  }

  // Write the subtitle group info out
  nlohmann::json json(db.m_subtitle_groups->m_groups);
  json[db.m_subtitle_groups->group_order_key] = nlohmann::json(db.m_subtitle_groups->m_group_order);
  std::string file_path = (file_util::get_jak_project_dir() / "game" / "assets" / "jak1" /
                           "subtitle" / "subtitle-groups.json")
                              .string();
  file_util::write_text_file(file_path, json.dump(2));

  return true;
}
