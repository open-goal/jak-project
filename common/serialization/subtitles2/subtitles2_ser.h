#pragma once

#include <string>
#include <vector>

#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/util/FontUtils.h"
#include "common/util/json_util.h"
#include "common/versions/versions.h"

const std::vector<std::string> get_speaker_names(GameVersion version);

struct Subtitle2Line {
  Subtitle2Line() {}
  Subtitle2Line(float start,
                float end,
                const std::string& text,
                const std::string& speaker,
                bool offscreen,
                bool merge)
      : start(start), end(end), text(text), speaker(speaker), offscreen(offscreen), merge(merge) {}

  float start, end;

  std::string text;

  // name in enum. saved as int later.
  std::string speaker;

  bool offscreen, merge;

  bool operator<(const Subtitle2Line& other) const {
    return (start < other.start) || (start == other.start && end < other.end);
  }
};
void to_json(json& j, const Subtitle2Line& obj);
void from_json(const json& j, Subtitle2Line& obj);

struct Subtitle2Scene {
  bool scene = false;

  std::vector<Subtitle2Line> lines;
};
void to_json(json& j, const Subtitle2Scene& obj);
void from_json(const json& j, Subtitle2Scene& obj);

struct GameSubtitle2Bank {
  GameSubtitle2Bank(int lang) : lang(lang) {}

  int lang;

  GameTextVersion text_version = GameTextVersion::JAK2;
  std::string file_path;

  std::map<std::string, std::string> speakers;
  std::map<std::string, Subtitle2Scene> scenes;

  bool scene_exists(const std::string& name) const { return scenes.find(name) != scenes.end(); }
  void add_scene(const std::string& name, Subtitle2Scene& scene) {
    ASSERT(!scene_exists(name));
    scenes.insert({name, scene});
  }
};
void to_json(json& j, const GameSubtitle2Bank& obj);
void from_json(const json& j, GameSubtitle2Bank& obj);

class GameSubtitle2DB {
 public:
  GameSubtitle2DB(GameVersion version) : m_version(version) {}

  const std::map<int, std::shared_ptr<GameSubtitle2Bank>>& banks() const { return m_banks; }

  bool bank_exists(int id) const { return m_banks.find(id) != m_banks.end(); }

  std::shared_ptr<GameSubtitle2Bank> add_bank(std::shared_ptr<GameSubtitle2Bank> bank) {
    ASSERT(!bank_exists(bank->lang));
    m_banks[bank->lang] = bank;
    return bank;
  }
  std::shared_ptr<GameSubtitle2Bank> bank_by_id(int id) {
    if (!bank_exists(id)) {
      return nullptr;
    }
    return m_banks.at(id);
  }

  std::map<int, std::shared_ptr<GameSubtitle2Bank>> m_banks;
  std::unique_ptr<GameSubtitle2Bank> m_subtitle_groups;

  GameVersion version() const { return m_version; }

 private:
  GameVersion m_version;
};

struct GameSubtitle2DefinitionFile {
  std::string file_path = "";
  int language_id = -1;
  GameTextVersion text_version = GameTextVersion::JAK2;
};

void parse_subtitle2_json(GameSubtitle2DB& db, const GameSubtitle2DefinitionFile& file_info);
void open_subtitle2_project(const std::string& kind,
                            const std::string& filename,
                            std::vector<GameSubtitle2DefinitionFile>& inputs);
GameSubtitle2DB load_subtitle2_project(GameVersion game_version);
