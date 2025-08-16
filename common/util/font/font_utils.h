#pragma once

#include <map>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "common/common_types.h"
#include "common/util/Trie.h"
#include "common/util/font/font_utils_korean.h"
#include "common/util/json_util.h"
#include "common/versions/versions.h"

/*!
 * What bytes a set of characters (UTF-8) correspond to. You can convert to and fro.
 */
struct EncodeInfo {
  std::string utf8;
  std::string game_bytes;
};

/*!
 * Replace an unconventional string of characters with/from something more readable.
 * For example, turns Ñ into N + ~ + a bunch of modifiers.
 */
struct ReplaceInfo {
  std::string game_encoding;
  std::string utf8_string;
  // for some replacements, we want to replace with something different when going from utf8 than
  // when we originally matched it this is mostly applicable for when we have to max a string with
  // hex chars that have been converted into `\c00` format but we want to insert the actual raw hex
  // bytes back.
  std::string utf8_alternative = "";
};

// version of the game text file's text encoding. Not real, but we need to differentiate them
// somehow, since the encoding changes.
enum class GameTextVersion {
  JAK1_V1 = 10,  // jak 1 (ntsc-u v1)
  JAK1_V2 = 11,  // jak 1 (pal+)
  JAK2 = 20,     // jak 2
  JAK3 = 30,     // jak 3
  JAKX = 40      // jak x
};

/*!
 * All the information to convert UTF-8 text into game text.
 */
class GameTextFontBank {
  GameTextVersion m_version;  // the version of the game text. we determine this ourselves.

  Trie<EncodeInfo> m_encode_to_utf8_trie;
  Trie<EncodeInfo> m_encode_to_game_trie;
  Trie<ReplaceInfo> m_replace_to_utf8_trie;
  Trie<ReplaceInfo> m_replace_to_game_trie;

  std::unordered_set<char>* m_passthrus;
  // jamo=>6 orientations with their drawing info
  std::optional<std::unordered_map<std::string, KoreanLookupOrientations>> m_korean_db =
      std::nullopt;

  std::string replace_to_utf8(const std::string& str) const;
  std::string replace_to_game(const std::string& str) const;
  std::string encode_utf8_to_game(const std::string& str) const;
  std::string encode_game_to_utf8(const std::string& str) const;

 public:
  GameTextFontBank(GameTextVersion version,
                   std::vector<EncodeInfo>* encode_info,
                   std::vector<ReplaceInfo>* replace_info,
                   std::unordered_set<char>* passthrus);

  const std::unordered_set<char>* passthrus() const { return m_passthrus; }

  GameTextVersion version() const { return m_version; }

  // TODO - methods would help make this code a lot better for different game versions
  // hacking it for now
  bool valid_char_range(const char& in) const;
  bool is_language_id_korean(const int language_id) const;

  std::string convert_utf8_to_game(const std::string& str) const;
  std::string convert_game_to_utf8(const char* in) const;

  std::string convert_utf8_to_game_korean(const std::string& str);
  std::string convert_korean_game_to_utf8(const char* in) const;
};

extern const std::unordered_map<std::string, GameTextVersion> sTextVerEnumMap;
const std::string& get_text_version_name(GameTextVersion version);
GameTextVersion get_text_version_from_name(const std::string& name);

/*!
 * ========================
 * GAME TEXT FONT BANK LIST
 * ========================
 * The list of available font banks and a couple of helper functions.
 */
extern std::map<GameTextVersion, GameTextFontBank*> g_font_banks;
GameTextFontBank* get_font_bank(GameTextVersion version);
GameTextFontBank* get_font_bank_from_game_version(GameVersion version);
GameTextFontBank* get_font_bank(const std::string& name);
bool font_bank_exists(GameTextVersion version);
