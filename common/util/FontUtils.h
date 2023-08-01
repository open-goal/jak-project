#pragma once

/*!
 * @file FontUtils.h
 *
 * Code for handling text and strings in Jak 1's "large font" format.
 *
 * MAKE SURE THIS FILE IS ENCODED IN UTF-8!!! The various strings here depend on it.
 * Always verify the encoding if string detection suddenly goes awry.
 */

#include <map>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "common/common_types.h"

// version of the game text file's text encoding. Not real, but we need to differentiate them
// somehow, since the encoding changes.
enum class GameTextVersion {
  JAK1_V1 = 10,  // jak 1 (ntsc-u v1)
  JAK1_V2 = 11,  // jak 1 (pal+)
  JAK2 = 20,     // jak 2
  JAK3 = 30,     // jak 3
  JAKX = 40      // jak x
};

extern const std::unordered_map<std::string, GameTextVersion> sTextVerEnumMap;

const std::string& get_text_version_name(GameTextVersion version);
GameTextVersion get_text_version_from_name(const std::string& name);

/*!
 * What bytes a set of characters (UTF-8) correspond to. You can convert to and fro.
 */
struct EncodeInfo {
  std::string chars;
  std::vector<u8> bytes;
};
/*!
 * Replace an unconventional string of characters with/from something more readable.
 * For example, turns Ñ into N + ~ + a bunch of modifiers.
 */
struct ReplaceInfo {
  std::string from;
  std::string to;
};

/*!
 * All the information to convert UTF-8 text into game text.
 */
class GameTextFontBank {
  GameTextVersion m_version;  // the version of the game text. we determine this ourselves.
  std::vector<EncodeInfo>* m_encode_info;
  std::vector<ReplaceInfo>* m_replace_info;
  std::unordered_set<char>* m_passthrus;

  const EncodeInfo* find_encode_to_utf8(const char* in) const;
  const EncodeInfo* find_encode_to_game(const std::string& in, int off = 0) const;
  const ReplaceInfo* find_replace_to_utf8(const std::string& in, int off = 0) const;
  const ReplaceInfo* find_replace_to_game(const std::string& in, int off = 0) const;

  std::string replace_to_utf8(std::string& str) const;
  std::string replace_to_game(std::string& str) const;
  std::string encode_utf8_to_game(std::string& str) const;

 public:
  GameTextFontBank(GameTextVersion version,
                   std::vector<EncodeInfo>* encode_info,
                   std::vector<ReplaceInfo>* replace_info,
                   std::unordered_set<char>* passthrus);

  const std::vector<EncodeInfo>* encode_info() const { return m_encode_info; }
  const std::vector<ReplaceInfo>* replace_info() const { return m_replace_info; }
  const std::unordered_set<char>* passthrus() const { return m_passthrus; }

  GameTextVersion version() const { return m_version; }

  // TODO - methods would help make this code a lot better for different game versions
  // hacking it for now
  bool valid_char_range(const char in) const;

  std::string convert_utf8_to_game(std::string str, bool escape = false) const;
  std::string convert_game_to_utf8(const char* in) const;
};

extern GameTextFontBank g_font_bank_jak1_v1;
extern GameTextFontBank g_font_bank_jak1_v2;
extern GameTextFontBank g_font_bank_jak2;
extern std::map<GameTextVersion, GameTextFontBank*> g_font_banks;

const GameTextFontBank* get_font_bank(GameTextVersion version);
const GameTextFontBank* get_font_bank(const std::string& name);
bool font_bank_exists(GameTextVersion version);
