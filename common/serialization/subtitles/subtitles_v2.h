#pragma once

#include "common/serialization/subtitles/subtitles.h"

struct SubtitleLineMetadataV2 {
  int frame_start;
  int frame_end;
  bool offscreen;
  std::string speaker;
  bool merge;
};
void to_json(json& j, const SubtitleLineMetadataV2& obj);
void from_json(const json& j, SubtitleLineMetadataV2& obj);

struct SubtitleCutsceneMetadataV2 {
  bool is_cutscene;
  std::vector<SubtitleLineMetadataV2> lines;
};
void to_json(json& j, const SubtitleCutsceneMetadataV2& obj);
void from_json(const json& j, SubtitleCutsceneMetadataV2& obj);

struct SubtitleMetadataFileV2 {
  std::unordered_map<std::string, SubtitleCutsceneMetadataV2> scenes;
};
void to_json(json& j, const SubtitleMetadataFileV2& obj);
void from_json(const json& j, SubtitleMetadataFileV2& obj);

struct SubtitleFileV2 {
  std::unordered_map<std::string, std::string> speakers;
  std::unordered_map<std::string, std::vector<std::string>> scenes;
};
void to_json(json& j, const SubtitleFileV2& obj);
void from_json(const json& j, SubtitleFileV2& obj);

struct SubtitleLineV2 {
  std::string text;
  SubtitleLineMetadataV2 metadata;

  bool operator<(const SubtitleLineV2& other) const {
    return (metadata.frame_start < other.metadata.frame_start);
  }
};

struct GameSubtitleSceneInfoV2 {
  bool is_cutscene;
  std::vector<SubtitleLineV2> m_lines;
};

class GameSubtitleBankV2 : public GameSubtitleBank<GameSubtitleSceneInfoV2> {
 public:
  using GameSubtitleBank<GameSubtitleSceneInfoV2>::GameSubtitleBank;
  void add_scenes_from_file(const GameSubtitleDefinitionFile& file_info) override;

  std::unordered_map<std::string, std::string> m_speakers;

  u16 speaker_enum_value_from_name(const std::string& speaker_id);
  bool is_valid_speaker_id(const std::string& speaker_id);
};

/*!
 * The subtitles database contains a subtitles bank for each language.
 * Each subtitles bank contains a series of subtitle scene infos.
 */
class GameSubtitleDBV2 {
 public:
  std::map<int, std::shared_ptr<GameSubtitleBankV2>> m_banks;
  GameVersion m_game_version;

  bool bank_exists(int id) const { return m_banks.find(id) != m_banks.end(); }
  std::shared_ptr<GameSubtitleBankV2> add_bank(std::shared_ptr<GameSubtitleBankV2> bank) {
    ASSERT(!bank_exists(bank->m_lang_id));
    m_banks[bank->m_lang_id] = bank;
    return bank;
  }
  std::shared_ptr<GameSubtitleBankV2> bank_by_id(int id) {
    if (!bank_exists(id)) {
      return nullptr;
    }
    return m_banks.at(id);
  }

  void init_banks_from_file(const GameSubtitleDefinitionFile& file_info);
  bool write_subtitle_db_to_files(const GameVersion game_version);
};

GameSubtitleDBV2 load_subtitle_project_v2(GameVersion game_version);
