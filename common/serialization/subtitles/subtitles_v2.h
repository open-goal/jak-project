#pragma once

#include "common/serialization/subtitles/subtitles.h"

struct SubtitleLineMetadata {
  int frame_start;
  int frame_end;
  bool offscreen;
  std::string speaker;
  bool merge;
};
void to_json(json& j, const SubtitleLineMetadata& obj);
void from_json(const json& j, SubtitleLineMetadata& obj);

struct SubtitleCutsceneMetadata {
  std::vector<SubtitleLineMetadata> lines;
};
void to_json(json& j, const SubtitleCutsceneMetadata& obj);
void from_json(const json& j, SubtitleCutsceneMetadata& obj);

struct SubtitleMetadataFile {
  std::unordered_map<std::string, SubtitleCutsceneMetadata> cutscenes;
  std::unordered_map<std::string, SubtitleCutsceneMetadata> other;
};
void to_json(json& j, const SubtitleMetadataFile& obj);
void from_json(const json& j, SubtitleMetadataFile& obj);

struct SubtitleFile {
  std::unordered_map<std::string, std::string> speakers;
  std::unordered_map<std::string, std::vector<std::string>> cutscenes;
  std::unordered_map<std::string, std::vector<std::string>> other;
};
void to_json(json& j, const SubtitleFile& obj);
void from_json(const json& j, SubtitleFile& obj);

struct SubtitleLine {
  std::string text;
  SubtitleLineMetadata metadata;

  bool operator<(const SubtitleLine& other) const {
    return (metadata.frame_start < other.metadata.frame_start);
  }
};

struct GameSubtitleSceneInfo {
  std::string m_name;
  std::vector<SubtitleLine> m_lines;
  bool is_cutscene;
  int m_hint_id;

  // TODO
  /*void add_line(int frame, std::string text, std::string speaker, bool offscreen) {
    m_lines.push_back({text, {frame, offscreen, speaker, false}});
    std::sort(m_lines.begin(), m_lines.end());
  }*/
};

/*!
 * The subtitle bank contains subtitles for all scenes in a language.
 */
class GameSubtitleBank {
 public:
  GameSubtitleBank(const int lang_id) : m_lang_id(lang_id) {}

  int m_lang_id;
  GameVersion m_game_version;
  GameTextVersion m_text_version;
  std::string m_file_path;

  std::map<std::string, GameSubtitleSceneInfo> m_scenes;
  bool scene_exists(const std::string& name) const { return m_scenes.find(name) != m_scenes.end(); }
  GameSubtitleSceneInfo new_scene_from_meta(
      const std::string& scene_name,
      const SubtitleCutsceneMetadata& scene_meta,
      const std::unordered_map<std::string, std::vector<std::string>>& relevant_lines);
  GameSubtitleSceneInfo& scene_by_name(const std::string& name) { return m_scenes.at(name); }
  void add_scenes_from_files(const SubtitleMetadataFile& meta_file, const SubtitleFile& lines_file);

  std::unordered_map<std::string, std::string> m_speakers;
  u16 speaker_enum_value_from_name(const std::string& speaker_id);
  bool is_valid_speaker_id(const std::string& speaker_id);
};

/*!
 * The subtitles database contains a subtitles bank for each language.
 * Each subtitles bank contains a series of subtitle scene infos.
 */
class GameSubtitleDB {
 public:
  enum class SubtitleFormat { V1, V2 };
  std::map<int, std::shared_ptr<GameSubtitleBank>> m_banks;
  GameVersion m_game_version;
  SubtitleFormat m_subtitle_version = SubtitleFormat::V2;

  bool bank_exists(int id) const { return m_banks.find(id) != m_banks.end(); }
  std::shared_ptr<GameSubtitleBank> add_bank(std::shared_ptr<GameSubtitleBank> bank) {
    ASSERT(!bank_exists(bank->m_lang_id));
    m_banks[bank->m_lang_id] = bank;
    return bank;
  }
  std::shared_ptr<GameSubtitleBank> bank_by_id(int id) {
    if (!bank_exists(id)) {
      return nullptr;
    }
    return m_banks.at(id);
  }

  void init_banks_from_file(const GameSubtitleDefinitionFile& file_info);
  bool write_subtitle_db_to_files(const GameVersion game_version);
};

GameSubtitleDB load_subtitle_project(const GameSubtitleDB::SubtitleFormat format_version,
                                     GameVersion game_version);
