#pragma once

#include "common/serialization/subtitles/subtitles.h"

struct SubtitleCutsceneLineMetadataV1 {
  int frame_start;
  bool offscreen;
  std::string speaker;
  bool clear;
};
void to_json(json& j, const SubtitleCutsceneLineMetadataV1& obj);
void from_json(const json& j, SubtitleCutsceneLineMetadataV1& obj);

struct SubtitleHintLineMetadataV1 {
  int frame_start;
  std::string speaker;
  bool clear;
};
void to_json(json& j, const SubtitleHintLineMetadataV1& obj);
void from_json(const json& j, SubtitleHintLineMetadataV1& obj);

struct SubtitleHintMetadataV1 {
  std::string id;  // hex
  std::vector<SubtitleHintLineMetadataV1> lines;
};
void to_json(json& j, const SubtitleHintMetadataV1& obj);
void from_json(const json& j, SubtitleHintMetadataV1& obj);

struct SubtitleMetadataFileV1 {
  std::unordered_map<std::string, std::vector<SubtitleCutsceneLineMetadataV1>> cutscenes;
  std::unordered_map<std::string, SubtitleHintMetadataV1> hints;
};
void to_json(json& j, const SubtitleMetadataFileV1& obj);
void from_json(const json& j, SubtitleMetadataFileV1& obj);

struct SubtitleFileV1 {
  std::unordered_map<std::string, std::string> speakers;
  std::unordered_map<std::string, std::vector<std::string>> cutscenes;
  std::unordered_map<std::string, std::vector<std::string>> hints;
};
void to_json(json& j, const SubtitleFileV1& obj);
void from_json(const json& j, SubtitleFileV1& obj);

struct SubtitleLineV1 {
  std::string text;
  SubtitleCutsceneLineMetadataV1 metadata;

  bool operator<(const SubtitleLineV1& other) const {
    return (metadata.frame_start < other.metadata.frame_start);
  }
};

struct GameSubtitleSceneInfoV1 {
  enum class SceneKind { Invalid = -1, Movie = 0, Hint = 1, HintNamed = 2 };

  GameSubtitleSceneInfoV1(SceneKind kind) : m_kind(kind) {}

  int m_id;
  SceneKind m_kind;
  std::vector<SubtitleLineV1> m_lines;
  std::string m_name;

  void add_line(int frame, std::string text, std::string speaker, bool offscreen) {
    m_lines.push_back({text, {frame, offscreen, speaker, false}});
    std::sort(m_lines.begin(), m_lines.end());
  }
};

class GameSubtitleBankV1 : public GameSubtitleBank<GameSubtitleSceneInfoV1> {
 public:
  using GameSubtitleBank<GameSubtitleSceneInfoV1>::GameSubtitleBank;

  void add_scenes_from_file(const GameSubtitleDefinitionFile& file_info) override;
};

/*!
 * The subtitles database contains a subtitles bank for each language.
 * Each subtitles bank contains a series of subtitle scene infos.
 */
// TODO - it would be nice to get rid of this, but that would break all current translations.
// Consider doing so once everything for jak 1 has been 100% translated.
class GameSubtitleDBV1 {
 public:
  std::map<int, std::shared_ptr<GameSubtitleBankV1>> m_banks;
  GameVersion m_game_version;

  bool bank_exists(int id) const { return m_banks.find(id) != m_banks.end(); }
  std::shared_ptr<GameSubtitleBankV1> add_bank(std::shared_ptr<GameSubtitleBankV1> bank) {
    ASSERT(!bank_exists(bank->m_lang_id));
    m_banks[bank->m_lang_id] = bank;
    return bank;
  }
  std::shared_ptr<GameSubtitleBankV1> bank_by_id(int id) {
    if (!bank_exists(id)) {
      return nullptr;
    }
    return m_banks.at(id);
  }

  void init_banks_from_file(const GameSubtitleDefinitionFile& file_info);
  bool write_subtitle_db_to_files(const GameVersion game_version);
};

GameSubtitleDBV1 load_subtitle_project_v1(GameVersion game_version);
