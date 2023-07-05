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

struct GameTextDefinitionFile {
  enum class Format { GOAL, JSON };
  Format format;
  std::string file_path = "";
  int language_id = -1;
  std::string text_version = "jak1-v2";
  std::optional<std::string> group_name = std::nullopt;
};

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

void parse_text_goal(const goos::Object& data,
                     GameTextDB& db,
                     const GameTextDefinitionFile& /*file_info*/);
void parse_text_json(const nlohmann::json& json,
                     GameTextDB& db,
                     const GameTextDefinitionFile& file_info);
GameTextVersion parse_text_only_version(const std::string& filename);
GameTextVersion parse_text_only_version(const goos::Object& data);

void open_text_project(const std::string& kind,
                       const std::string& filename,
                       std::vector<GameTextDefinitionFile>& inputs);
