#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <utility>

#include "common/goos/Object.h"
#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FontUtils.h"
#include "common/util/json_util.h"
#include "common/versions/versions.h"

struct GameSubtitleDefinitionFile {
  int language_id = -1;
  std::string text_version = "jak1-v2";
  std::string lines_path = "";
  std::optional<std::string> lines_base_path = std::nullopt;
  std::string meta_path = "";
  std::optional<std::string> meta_base_path = std::nullopt;
};

/*!
 * The subtitle bank contains subtitles for all scenes in a language.
 */
template <typename T>
class GameSubtitleBank {
 public:
  GameSubtitleBank(const int lang_id) : m_lang_id(lang_id) {}

  int m_lang_id;
  GameVersion m_game_version;
  GameTextVersion m_text_version;
  std::string m_file_path;
  std::map<std::string, T> m_scenes;

  bool scene_exists(const std::string& name) const { return m_scenes.find(name) != m_scenes.end(); }
  T& scene_by_name(const std::string& name) { return m_scenes.at(name); }

  virtual void add_scenes_from_file(const GameSubtitleDefinitionFile& file_info) = 0;
};

void open_subtitle_project(const std::string& project_kind,
                           const std::string& file_path,
                           std::vector<GameSubtitleDefinitionFile>& inputs);
std::string lookup_locale_code(const GameVersion game_version, const int language_id);
