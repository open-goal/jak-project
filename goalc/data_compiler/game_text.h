#pragma once
#include "common/util/FontUtils.h"
#include "common/util/Assert.h"
#include <string>
#include <map>
#include <unordered_map>
#include <unordered_set>

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
  const std::unordered_map<std::string, std::map<int, GameTextBank*>>& groups() const {
    return m_banks;
  }
  const std::map<int, GameTextBank*>& banks(std::string group) const { return m_banks.at(group); }

  bool bank_exists(std::string group, int id) const {
    if (m_banks.find(group) == m_banks.end())
      return false;
    return m_banks.at(group).find(id) != m_banks.at(group).end();
  }

  GameTextBank* new_bank(std::string group, int id) {
    ASSERT(!bank_exists(group, id));
    m_banks[group][id] = new GameTextBank(id);
    return m_banks.at(group).at(id);
  }
  void add_bank(std::string group, GameTextBank* bank) {
    ASSERT(!bank_exists(group, bank->lang()));
    m_banks[group][bank->lang()] = bank;
  }
  GameTextBank* bank_by_id(std::string group, int id) {
    if (!bank_exists(group, id)) {
      return nullptr;
    }
    return m_banks.at(group).at(id);
  }

 private:
  std::unordered_map<std::string, std::map<int, GameTextBank*>> m_banks;
};

void compile_game_text(const std::vector<std::string>& filenames,
                       GameTextVersion text_ver,
                       GameTextDB& db);
