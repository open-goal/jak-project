#pragma once

#include "common/serialization/subtitles/subtitles.h"
#include "common/util/Assert.h"
#include "common/util/FontUtils.h"
#include "common/util/json_util.h"

struct SubtitleLineMetadata {
  int frame_start;
  int frame_end;
  bool offscreen;
  std::string speaker;
  bool merge;

  bool operator==(const SubtitleLineMetadata& other) const {
    if (frame_start != other.frame_start || frame_end != other.frame_end ||
        offscreen != other.offscreen || speaker != other.speaker || merge != other.merge) {
      return false;
    }
    return true;
  }
  bool operator!=(const SubtitleLineMetadata& other) const { return !(*this == other); }
};
void to_json(json& j, const SubtitleLineMetadata& obj);
void from_json(const json& j, SubtitleLineMetadata& obj);

struct SubtitleSceneMetadata {
  std::vector<SubtitleLineMetadata> lines;
  int m_hint_id = 0;  // used only for jak1, intentionally ignored in serialization/deserialization
};
void to_json(json& j, const SubtitleSceneMetadata& obj);
void from_json(const json& j, SubtitleSceneMetadata& obj);

struct SubtitleMetadataFile {
  std::unordered_map<std::string, SubtitleSceneMetadata> cutscenes;
  std::unordered_map<std::string, SubtitleSceneMetadata> other;
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

// Returns the individual components of each subtitle "package":
// - the base file
// - the combination of the two
// This allows for more context when dumping the files back out and you want
// to skip duplicate info
struct GameSubtitlePackage {
  SubtitleMetadataFile base_meta;
  SubtitleMetadataFile combined_meta;
  SubtitleFile base_lines;
  SubtitleFile combined_lines;
  std::unordered_set<std::string> scenes_defined_in_lang;
};

struct GameSubtitleSceneInfo {
  std::string m_name;
  std::vector<SubtitleLine> m_lines;
  bool is_cutscene;
  bool only_defined_in_base = false;
  int m_hint_id;  // used only for jak1

  void add_line(const std::string& text,
                const int frame_start,
                const int frame_end,
                const bool offscreen,
                const std::string& speaker,
                const bool merge) {
    m_lines.push_back({text, {frame_start, frame_end, offscreen, speaker, merge}});
    std::sort(m_lines.begin(), m_lines.end());
  }
  bool same_lines_as_other(const GameSubtitleSceneInfo& other) const {
    if (m_lines.size() != other.m_lines.size()) {
      return false;
    }
    // Check each line
    for (size_t i = 0; i < m_lines.size(); i++) {
      if (m_lines.at(i).text != other.m_lines.at(i).text) {
        return false;
      }
    }
    return true;
  }
  bool same_metadata_as_other(const GameSubtitleSceneInfo& other) const {
    if (m_name != other.m_name || m_lines.size() != other.m_lines.size() ||
        is_cutscene != other.is_cutscene || m_hint_id != other.m_hint_id) {
      return false;
    }
    // Check each line's metadata
    for (size_t i = 0; i < m_lines.size(); i++) {
      if (m_lines.at(i).metadata != other.m_lines.at(i).metadata) {
        return false;
      }
    }
    return true;
  }
};

/*!
 * The subtitle bank contains subtitles for all scenes in a language.
 */
class GameSubtitleBank {
 public:
  GameSubtitleBank(const int lang_id) : m_lang_id(lang_id) {}

  int m_lang_id;
  GameTextVersion m_text_version;
  std::string m_file_path;
  std::optional<std::string> m_file_base_path;

  std::map<std::string, GameSubtitleSceneInfo> m_base_scenes;
  std::map<std::string, GameSubtitleSceneInfo> m_lang_scenes;
  std::map<std::string, GameSubtitleSceneInfo> m_scenes;
  bool scene_exists(const std::string& name) const { return m_scenes.find(name) != m_scenes.end(); }
  GameSubtitleSceneInfo new_scene_from_meta(
      const std::string& scene_name,
      const SubtitleSceneMetadata& scene_meta,
      const std::unordered_map<std::string, std::vector<std::string>>& relevant_lines);
  GameSubtitleSceneInfo& scene_by_name(const std::string& name) { return m_scenes.at(name); }
  void add_scenes_from_files(const GameSubtitlePackage& package);

  std::unordered_map<std::string, std::string> m_speakers;
  std::vector<std::string> speaker_names_ordered_by_enum_value();
  u16 speaker_enum_value_from_name(const std::string& speaker_id);
};

/*!
 * The subtitles database contains a subtitles bank for each language.
 * Each subtitles bank contains a series of subtitle scene infos.
 */
class GameSubtitleDB {
 public:
  enum class SubtitleFormat { V1, V2 };
  std::optional<std::string> m_load_error = {};
  std::map<int, std::shared_ptr<GameSubtitleBank>> m_banks;
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
