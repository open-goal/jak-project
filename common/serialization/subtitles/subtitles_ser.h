#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>

#include "common/goos/Object.h"
#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FontUtils.h"
#include "common/versions/versions.h"

/*!
 * The text bank contains all lines (accessed with an ID) for a language.
 */
class GameTextBank {
 public:
  GameTextBank(int lang_id) : m_lang_id(lang_id) {}

  int lang() const { return m_lang_id; }
  const std::map<int, std::string>& lines() const { return m_lines; }

  bool line_exists(int id) const { return m_lines.find(id) != m_lines.end(); }
  std::string line(int id) { return m_lines.at(id); }
  void set_line(int id, std::string line) { m_lines[id] = line; }

 private:
  int m_lang_id;
  std::map<int, std::string> m_lines;
};

/*!
 * The text database contains a text bank for each language for each text group.
 * Each text bank contains a list of text lines. Very simple.
 */
class GameTextDB {
 public:
  const std::unordered_map<std::string, std::map<int, std::shared_ptr<GameTextBank>>>& groups()
      const {
    return m_banks;
  }
  const std::map<int, std::shared_ptr<GameTextBank>>& banks(std::string group) const {
    return m_banks.at(group);
  }

  bool bank_exists(std::string group, int id) const {
    if (m_banks.find(group) == m_banks.end())
      return false;
    return m_banks.at(group).find(id) != m_banks.at(group).end();
  }

  std::shared_ptr<GameTextBank> add_bank(std::string group, std::shared_ptr<GameTextBank> bank) {
    ASSERT(!bank_exists(group, bank->lang()));
    m_banks[group][bank->lang()] = bank;
    return bank;
  }
  std::shared_ptr<GameTextBank> bank_by_id(std::string group, int id) {
    if (!bank_exists(group, id)) {
      return nullptr;
    }
    return m_banks.at(group).at(id);
  }

 private:
  std::unordered_map<std::string, std::map<int, std::shared_ptr<GameTextBank>>> m_banks;
};

/*!
 * The subtitle scene info (accessed through the scene name) contains all lines and their timestamps
 * and other settings.
 */
enum class SubtitleSceneKind { Invalid = -1, Movie = 0, Hint = 1, HintNamed = 2 };
class GameSubtitleSceneInfo {
 public:
  struct SubtitleLine {
    SubtitleLine(int frame, std::string line, std::string speaker, bool offscreen)
        : frame(frame), line(line), speaker(speaker), offscreen(offscreen) {}

    int frame;
    std::string line;
    std::string speaker;
    bool offscreen;

    bool operator<(const SubtitleLine& other) const { return (frame < other.frame); }
  };

  GameSubtitleSceneInfo(SubtitleSceneKind kind) : m_kind(kind) {}

  const std::string& name() const { return m_name; }
  const std::vector<SubtitleLine>& lines() const { return m_lines; }
  int id() const { return m_id; }
  SubtitleSceneKind kind() const { return m_kind; }

  void clear_lines() { m_lines.clear(); }
  void set_name(const std::string& new_name) { m_name = new_name; }
  void set_id(int new_id) { m_id = new_id; }
  void from_other_scene(GameSubtitleSceneInfo& scene) {
    m_name = scene.name();
    m_lines = scene.lines();
    m_kind = scene.kind();
    m_id = scene.id();
  }

  void add_line(int frame, std::string line, std::string speaker, bool offscreen) {
    m_lines.emplace_back(SubtitleLine(frame, line, speaker, offscreen));
    std::sort(m_lines.begin(), m_lines.end());
  }

  std::string m_name;
  int m_id;
  std::vector<SubtitleLine> m_lines;
  SubtitleSceneKind m_kind;
  std::string m_sorting_group;
  int m_sorting_group_idx;
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
  std::string file_path;

  std::map<std::string, GameSubtitleSceneInfo> m_scenes;
};

class GameSubtitleGroups {
 public:
  std::vector<std::string> m_group_order;
  std::map<std::string, std::vector<std::string>> m_groups;

  void hydrate_from_asset_file();
  std::string find_group(const std::string& scene_name);
  int find_group_index(const std::string& group_name);
  void remove_scene(const std::string& group_name, const std::string& scene_name);
  void add_scene(const std::string& group_name, const std::string& scene_name);

  std::string group_order_key = "_groups";
  std::string uncategorized_group = "uncategorized";
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
  std::unique_ptr<GameSubtitleGroups> m_subtitle_groups;
};

// TODO add docstrings

void parse_text(const goos::Object& data, GameTextDB& db);
void parse_subtitle(const goos::Object& data, GameSubtitleDB& db, const std::string& file_path);

GameTextVersion parse_text_only_version(const std::string& filename);
GameTextVersion parse_text_only_version(const goos::Object& data);

void open_text_project(const std::string& kind,
                       const std::string& filename,
                       std::vector<std::string>& inputs);
GameSubtitleDB load_subtitle_project(GameVersion game_version);
