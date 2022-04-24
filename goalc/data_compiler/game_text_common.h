#pragma once
#include "common/util/FontUtils.h"
#include "common/util/Assert.h"
#include <string>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <memory>

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
class GameSubtitleSceneInfo {
 public:
  GameSubtitleSceneInfo() {}
  GameSubtitleSceneInfo(std::string name) : m_name(name) {}

  struct SubtitleLine {
    SubtitleLine(int frame, std::string line, std::string speaker, bool offscreen)
        : frame(frame), line(line), speaker(speaker), offscreen(offscreen) {}

    int frame;
    std::string line;
    std::string speaker;
    bool offscreen;
  };

  const std::string& name() const { return m_name; }
  const std::vector<SubtitleLine>& lines() const { return m_lines; }
  void clear_lines() { m_lines.clear(); }
  void from_other_scene(GameSubtitleSceneInfo& scene) {
    m_name = scene.name();
    m_lines = scene.lines();
  }

  void add_line(int frame, std::string line, std::string speaker, bool offscreen) {
    m_lines.emplace_back(frame, line, speaker, offscreen);
  }

 private:
  std::string m_name;
  std::vector<SubtitleLine> m_lines;
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
    m_scenes[scene.name()] = scene;
  }

 private:
  int m_lang_id;

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

 private:
  std::map<int, std::shared_ptr<GameSubtitleBank>> m_banks;
};

void compile_game_text(const std::vector<std::string>& filenames,
                       GameTextVersion text_ver,
                       GameTextDB& db);
void compile_game_subtitle(const std::vector<std::string>& filenames,
                           GameTextVersion text_ver,
                           GameSubtitleDB& db);

void open_text_project(const std::string& kind,
                       const std::string& filename,
                       std::unordered_map<GameTextVersion, std::vector<std::string>>& inputs);
