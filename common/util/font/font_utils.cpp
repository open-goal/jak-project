/*!
 * @file FontUtils.cpp
 *
 * Code for handling text and strings in Jak 1's "large font" format.
 *
 * MAKE SURE THIS FILE IS ENCODED IN UTF-8!!! The various strings here depend on it.
 * Always verify the encoding if string detection suddenly goes awry.
 */

#include "font_utils.h"

#include <algorithm>
#include <stdexcept>
#include <string_view>

#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/util/font/dbs/font_db_jak1.h"
#include "common/util/font/dbs/font_db_jak2.h"
#include "common/util/font/dbs/font_db_jak3.h"
#include "common/util/font/font_utils_korean.h"
#include "common/util/string_util.h"
#include "common/versions/versions.h"

#include "fmt/format.h"

void from_json(const json& j, KoreanLookupEntry& obj) {
  json_deserialize_if_exists(defaultGlyph);
  json_deserialize_if_exists(alternatives);
}

std::map<GameTextVersion, GameTextFontBank*> g_font_banks = {
    {GameTextVersion::JAK1_V1, &g_font_bank_jak1_v1},
    {GameTextVersion::JAK1_V2, &g_font_bank_jak1_v2},
    {GameTextVersion::JAK2, &g_font_bank_jak2},
    {GameTextVersion::JAK3, &g_font_bank_jak3}};

const std::unordered_map<std::string, GameTextVersion> sTextVerEnumMap = {
    {"jak1-v1", GameTextVersion::JAK1_V1},
    {"jak1-v2", GameTextVersion::JAK1_V2},
    {"jak2", GameTextVersion::JAK2},
    {"jak3", GameTextVersion::JAK3}};

const std::string& get_text_version_name(GameTextVersion version) {
  for (auto& [name, ver] : sTextVerEnumMap) {
    if (ver == version) {
      return name;
    }
  }
  throw std::runtime_error(fmt::format("invalid text version {}", fmt::underlying(version)));
}

GameTextVersion get_text_version_from_name(const std::string& name) {
  return sTextVerEnumMap.at(name);
}

GameTextFontBank::GameTextFontBank(GameTextVersion version,
                                   std::vector<EncodeInfo>* encode_info,
                                   std::vector<ReplaceInfo>* replace_info,
                                   std::unordered_set<char>* passthrus)
    : m_version(version), m_passthrus(passthrus) {
  // Insert the encode and replacement info into a Trie, much faster lookups that way
  for (const auto& encoding : *encode_info) {
    m_encode_to_utf8_trie.insert(encoding.game_bytes, encoding);
    m_encode_to_game_trie.insert(encoding.utf8, encoding);
  }
  for (const auto& replacement : *replace_info) {
    m_replace_to_utf8_trie.insert(replacement.game_encoding, replacement);
    m_replace_to_game_trie.insert(replacement.utf8_string, replacement);
  }
}

bool GameTextFontBank::is_language_id_korean(const int language_id) const {
  if (m_version == GameTextVersion::JAK2 && language_id == 6) {
    return true;
  } else if (m_version == GameTextVersion::JAK3 && language_id == 7) {
    return true;
  }
  return false;
}

GameTextFontBank* get_font_bank(GameTextVersion version) {
  return g_font_banks.at(version);
}

GameTextFontBank* get_font_bank_from_game_version(GameVersion version) {
  if (version == GameVersion::Jak1) {
    // Jak 1 has been patched to use V2
    return get_font_bank(GameTextVersion::JAK1_V2);
  } else if (version == GameVersion::Jak2) {
    auto font_bank = get_font_bank(GameTextVersion::JAK2);
    return font_bank;
  } else if (version == GameVersion::Jak3) {
    return get_font_bank(GameTextVersion::JAK3);
  } else {
    ASSERT_MSG(false, "Unsupported game for get_font_bank_from_game_version");
  }
}

GameTextFontBank* get_font_bank(const std::string& name) {
  if (auto it = sTextVerEnumMap.find(name); it == sTextVerEnumMap.end()) {
    throw std::runtime_error(fmt::format("unknown text version {}", name));
  } else {
    return get_font_bank(it->second);
  }
}

bool font_bank_exists(GameTextVersion version) {
  return g_font_banks.find(version) != g_font_banks.cend();
}

std::string GameTextFontBank::replace_to_game(const std::string& str) const {
  std::string newstr;
  newstr.reserve(str.size());
  for (int i = 0; i < str.length();) {
    const ReplaceInfo* remap = m_replace_to_game_trie.find_longest_prefix(str, i);
    if (!remap) {
      newstr.push_back(str[i]);
      i += 1;
    } else {
      if (!remap->utf8_alternative.empty()) {
        newstr.append(remap->utf8_alternative);
      } else {
        newstr.append(remap->game_encoding);
      }
      i += remap->utf8_string.size();
    }
  }
  return newstr;
}

std::string GameTextFontBank::encode_utf8_to_game(const std::string& str) const {
  std::string newstr;
  newstr.reserve(str.size());
  for (int i = 0; i < str.length();) {
    auto match = m_encode_to_game_trie.find_longest_prefix(str, i);
    if (!match) {
      newstr.push_back(str[i]);
      i += 1;
    } else {
      for (auto b : match->game_bytes) {
        newstr.push_back(b);
      }
      i += match->utf8.size();
    }
  }
  return newstr;
}

/*!
 * Turn a normal readable string into a string readable in the in-game font encoding and converts
 * \cXX escape sequences
 */
std::string GameTextFontBank::convert_utf8_to_game(const std::string& str) const {
  return encode_utf8_to_game(replace_to_game(str));
}

std::string GameTextFontBank::replace_to_utf8(const std::string& str) const {
  std::string result;
  result.reserve(str.size());
  for (size_t i = 0; i < str.size();) {
    const ReplaceInfo* remap = m_replace_to_utf8_trie.find_longest_prefix(str, i);
    if (!remap) {
      result.push_back(str[i]);
      i += 1;
    } else {
      result.append(remap->utf8_string);
      i += remap->game_encoding.size();
    }
  }
  return result;
}

bool GameTextFontBank::valid_char_range(const char& in) const {
  if (m_version == GameTextVersion::JAK1_V1 || m_version == GameTextVersion::JAK1_V2) {
    return ((in >= '0' && in <= '9') || (in >= 'A' && in <= 'Z') ||
            m_passthrus->find(in) != m_passthrus->end()) &&
           in != '\\';
  } else if (m_version == GameTextVersion::JAK2 || m_version == GameTextVersion::JAK3 ||
             m_version == GameTextVersion::JAKX) {
    return ((in >= '0' && in <= '9') || (in >= 'A' && in <= 'Z') || (in >= 'a' && in <= 'z') ||
            m_passthrus->find(in) != m_passthrus->end()) &&
           in != '\\';
  }
  return false;
}

std::string GameTextFontBank::encode_game_to_utf8(const std::string& str) const {
  std::string newstr;
  newstr.reserve(str.size());
  for (size_t i = 0; i < str.size();) {
    auto encoding = m_encode_to_utf8_trie.find_longest_prefix(str, i);
    if (!encoding) {
      // No match: copy valid characters as-is, or escape unknown bytes
      unsigned char c = static_cast<unsigned char>(str[i]);
      if (valid_char_range(c) || c == '\n' || c == '\t' || c == '\\' || c == '"') {
        newstr.push_back(c);
      } else {
        newstr += fmt::format("\\c{:02x}", c);
      }
      ++i;
    } else {
      // Found a match: append its UTF-8 sequence
      newstr.append(encoding->utf8);
      i += encoding->game_bytes.size();  // advance past matched game bytes
    }
  }
  return newstr;
}

std::string GameTextFontBank::convert_game_to_utf8(const char* in) const {
  // Encode and apply replacement ONCE
  std::string decoded = replace_to_utf8(encode_game_to_utf8(in));

  // Escape special characters while writing directly into result
  std::string result;
  result.reserve(decoded.size());
  for (size_t i = 0; i < decoded.size(); ++i) {
    char c = decoded[i];
    if (c == '\n') {
      result += "\\n";
    } else if (c == '\t') {
      result += "\\t";
    } else if (c == '\\') {
      if (i < decoded.size() - 1 && decoded[i + 1] == 'c') {
        result.push_back(c);  // preserve \cXX
      } else {
        result += "\\\\";
      }
    } else if (c == '"') {
      result += "\\\"";
    } else {
      result.push_back(c);
    }
  }

  return result;
}

std::string GameTextFontBank::convert_utf8_to_game_korean(const std::string& str) {
  ASSERT_MSG(m_version == GameTextVersion::JAK2 || m_version == GameTextVersion::JAK3,
             "Korean is not supported for any game other than Jak 2 and Jak 3 right now");
  if (!m_korean_db.has_value()) {
    const auto db_file_path =
        file_util::get_file_path({"game/assets/fonts/jak2_jak3_korean_db.json"});
    if (file_util::file_exists(db_file_path)) {
      auto raw_data = file_util::read_text_file(db_file_path);
      auto json_data = parse_commented_json(raw_data, "jak2_jak3_korean_db.json");
      std::unordered_map<std::string, KoreanLookupOrientations> temp_db;
      json_data.get_to(temp_db);
      m_korean_db = temp_db;
    }
  }

  std::string output;
  output.reserve(str.size());
  std::string non_korean_buffer = "";
  size_t i = 0;
  while (i < str.size()) {
    char32_t cp = str_util::next_utf8_char(str, i);
    if (font_util_korean::is_korean_syllable(cp)) {
      // flush any non-korean buffer
      if (!non_korean_buffer.empty()) {
        output += 0x3;
        // encode / remap it
        output += encode_utf8_to_game(replace_to_game(non_korean_buffer));
        non_korean_buffer = "";
      }
      // write out the korean character
      output += font_util_korean::game_encode_korean_syllable(str, cp, m_korean_db.value());
    } else {
      non_korean_buffer += str_util::utf8_encode(cp);
    }
  }
  // flush any non-korean buffer
  if (!non_korean_buffer.empty()) {
    output += 0x3;
    // encode / remap it
    output += encode_utf8_to_game(replace_to_game(non_korean_buffer));
    non_korean_buffer = "";
  }
  return output;
}

std::string GameTextFontBank::convert_korean_game_to_utf8(const char* in) const {
  ASSERT_MSG(m_version == GameTextVersion::JAK2 || m_version == GameTextVersion::JAK3,
             "Korean is not supported for any game other than Jak 2 and Jak 3 right now");
  // Korean strings are fully bitstrings, in other words, it's just a bunch of bytes
  // Some info on the layout:
  // - Every korean syllable block starts with a `4`
  //   - The following byte indicates how many glyphs are drawn for that syllable block
  // - Each jamo that makes up the syllable block follows as a single byte
  //   - Unless the jamo is part of the "extra" texture page, in which case it's preceeded by a `5`.
  //   There are very few jamo that are and they are only applicable for the final consonant
  // - The korean strings can contain non-korean characters.  These are preceeded by a `3`
  //   - For example a space would be `3 20`
  //   - It might be more accurate to say that a 3 signifies "consume characters as normal until
  //   something else is encountered (ie. flags or more complex font encodings)"
  std::string result;
  std::string_view str(in);

  u64 index = 0;
  u8 curr_byte = 0;
  bool in_syllable_block = false;
  std::string jamo_buffer = "";
  std::string non_korean_buffer = "";
  int num_syllable_glyphs = 0;
  while (index < str.length()) {
    curr_byte = str.at(index);
    // new syllable block
    if (curr_byte == 4) {
      in_syllable_block = true;
      if (index + 1 < str.length()) {
        num_syllable_glyphs = str.at(index + 1);
        index++;
      }
      index++;
      // flush any non-korean characters
      if (!non_korean_buffer.empty()) {
        // handle remap
        std::string remapped_str = replace_to_utf8(encode_game_to_utf8(non_korean_buffer));
        result += remapped_str;
        non_korean_buffer = "";
      }
      continue;
    }
    if (in_syllable_block) {
      // extra page
      std::string glyph_key;
      u8 hex_byte = curr_byte;
      if (curr_byte == 5 && index + 1 < str.length()) {
        hex_byte = str.at(index + 1);
        glyph_key = fmt::format("extra_0x{:02x}", hex_byte);
        index++;
      } else {
        glyph_key = fmt::format("0x{:02x}", hex_byte);
      }
      const auto jamo_list = jamo_glyph_mappings_jak2.find(glyph_key);
      ASSERT_MSG(jamo_list != jamo_glyph_mappings_jak2.end(),
                 fmt::format("{} not found in jamo glyph lookup table", glyph_key));
      for (const auto& jamo : jamo_list->second) {
        jamo_buffer += jamo;
      }
      num_syllable_glyphs--;
      if (num_syllable_glyphs == 0) {
        in_syllable_block = false;
        result += font_util_korean::compose_korean_containing_text(jamo_buffer);
        jamo_buffer = "";
      }
    } else {
      if (curr_byte != 0x3) {
        non_korean_buffer.push_back(curr_byte);
      }
    }
    index++;
  }
  // flush any non-korean characters
  if (!non_korean_buffer.empty()) {
    // handle remap
    std::string remapped_str = replace_to_utf8(encode_game_to_utf8(non_korean_buffer));
    result += remapped_str;
    non_korean_buffer = "";
  }
  return result;
}
