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

struct SubtitleCutsceneLineMetadata {
  int frame_start;
  bool offscreen;
  std::string speaker;
};
void to_json(json& j, const SubtitleCutsceneLineMetadata& obj);
void from_json(const json& j, SubtitleCutsceneLineMetadata& obj);

struct SubtitleHintLineMetadata {
  int frame;
  std::string speaker;
  bool clear;
};
void to_json(json& j, const SubtitleHintLineMetadata& obj);
void from_json(const json& j, SubtitleHintLineMetadata& obj);

struct SubtitleHintMetadata {
  std::string id;  // hex
  std::vector<SubtitleHintLineMetadata> lines;
};
void to_json(json& j, const SubtitleHintMetadata& obj);
void from_json(const json& j, SubtitleHintMetadata& obj);

struct SubtitleMetadataFile {
  std::unordered_map<std::string, std::vector<SubtitleCutsceneLineMetadata>> cutscenes;
  std::unordered_map<std::string, SubtitleHintMetadata> hints;
};
void to_json(json& j, const SubtitleMetadataFile& obj);
void from_json(const json& j, SubtitleMetadataFile& obj);

struct SubtitleFile {
  std::unordered_map<std::string, std::string> speakers;
  std::unordered_map<std::string, std::vector<std::string>> cutscenes;
  std::unordered_map<std::string, std::vector<std::string>> hints;
};
void to_json(json& j, const SubtitleFile& obj);
void from_json(const json& j, SubtitleFile& obj);

struct GameSubtitleDefinitionFile {
  int language_id = -1;
  // TODO - GameTextVersion text_version = GameTextVersion::JAK2;
  std::string text_version = "jak1-v2";
  std::string lines_path = "";
  std::optional<std::string> lines_base_path = std::nullopt;
  std::string meta_path = "";
  std::optional<std::string> meta_base_path = std::nullopt;
};

/*!
 * The subtitle bank contains subtitles for all scenes in a language.
 */
class GameSubtitleBank {
 public:
  GameSubtitleBank(int lang_id) : m_lang_id(lang_id) {}

  int lang() const { return m_lang_id; }
  const std::map<std::string, GameSubtitleSceneInfo>& scenes() const { return m_scenes; }

  bool scene_exists(const std::string& name) const { return m_scenes.find(name) != m_scenes.end(); }
  GameSubtitleSceneInfo& scene_by_name(const std::string& name) { return m_scenes.at(name); }
  void add_scene(GameSubtitleSceneInfo& scene) {
    ASSERT(!scene_exists(scene.name()));
    m_scenes.insert({scene.name(), scene});
  }

  int m_lang_id;
  std::string m_text_version;
  std::string m_file_path;
  std::map<std::string, GameSubtitleSceneInfo> m_scenes;
};

/*!
 * The subtitles database contains a subtitles bank for each language.
 * Each subtitles bank contains a series of subtitle scene infos.
 */
class GameSubtitleDB {
 public:
  const std::map<int, std::shared_ptr<GameSubtitleBank>>& banks() const { return m_banks; }

  bool bank_exists(int id) const { return m_banks.find(id) != m_banks.end(); }

  std::shared_ptr<GameSubtitleBank> add_bank(std::shared_ptr<GameSubtitleBank> bank) {
    ASSERT(!bank_exists(bank->lang()));
    m_banks[bank->lang()] = bank;
    return bank;
  }
  std::shared_ptr<GameSubtitleBank> bank_by_id(int id) {
    if (!bank_exists(id)) {
      return nullptr;
    }
    return m_banks.at(id);
  }

  std::map<int, std::shared_ptr<GameSubtitleBank>> m_banks;
  GameVersion m_game_version;
};

void parse_subtitle(GameSubtitleDB& db, const GameSubtitleDefinitionFile& file_info);
void open_subtitle_project(const std::string& kind,
                           const std::string& filename,
                           std::vector<GameSubtitleDefinitionFile>& inputs);
GameSubtitleDB load_subtitle_project(GameVersion game_version);
