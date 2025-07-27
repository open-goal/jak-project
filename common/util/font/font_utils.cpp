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
    : m_version(version),
      m_encode_info(encode_info),
      m_replace_info(replace_info),
      m_passthrus(passthrus) {
  std::sort(
      m_encode_info->begin(), m_encode_info->end(),
      [](const EncodeInfo& a, const EncodeInfo& b) { return a.bytes.size() > b.bytes.size(); });
  std::sort(
      m_replace_info->begin(), m_replace_info->end(),
      [](const ReplaceInfo& a, const ReplaceInfo& b) { return a.from.size() > b.from.size(); });
}

/*!
 * Finds a remap info that best matches the byte sequence (is the longest match).
 */
const EncodeInfo* GameTextFontBank::find_encode_to_utf8(const char* in) const {
  const EncodeInfo* best_info = nullptr;
  for (auto& info : *m_encode_info) {
    if (info.bytes.size() == 0)
      continue;

    bool found = true;
    for (int i = 0; found && i < (int)info.bytes.size(); ++i) {
      if (uint8_t(in[i]) != info.bytes.at(i)) {
        found = false;
      }
    }

    if (found && (!best_info || info.chars.length() > best_info->chars.length())) {
      best_info = &info;
    }
  }
  return best_info;
}

/*!
 * Finds a remap info that best matches the character sequence (is the longest match).
 */
const EncodeInfo* GameTextFontBank::find_encode_to_game(const std::string& in, int off) const {
  const EncodeInfo* best_info = nullptr;
  for (auto& info : *m_encode_info) {
    if (info.chars.length() == 0)
      continue;

    bool found = true;
    for (int i = 0; found && i < (int)info.chars.length() && i + off < (int)in.size(); ++i) {
      if (in.at(i + off) != info.chars.at(i)) {
        found = false;
      }
    }

    if (found && (!best_info || info.chars.length() > best_info->chars.length())) {
      best_info = &info;
    }
  }
  return best_info;
}

/*!
 * Finds a remap info that best matches the character sequence (is the longest match).
 */
const ReplaceInfo* GameTextFontBank::find_replace_to_utf8(const std::string& in, int off) const {
  const ReplaceInfo* best_info = nullptr;
  for (auto& info : *m_replace_info) {
    if (info.from.empty() || in.size() - off < info.from.size())
      continue;

    bool found = memcmp(in.data() + off, info.from.data(), info.from.size()) == 0;
    if (found && (!best_info || info.from.length() > best_info->from.length())) {
      best_info = &info;
    }
  }
  return best_info;
}

/*!
 * Finds a remap info that best matches the character sequence (is the longest match).
 */
const ReplaceInfo* GameTextFontBank::find_replace_to_game(const std::string& in, int off) const {
  const ReplaceInfo* best_info = nullptr;
  for (auto& info : *m_replace_info) {
    if (info.to.empty() || in.size() - off < info.to.size())
      continue;

    bool found = memcmp(in.data() + off, info.to.data(), info.to.size()) == 0;
    if (found && (!best_info || info.to.length() > best_info->to.length())) {
      best_info = &info;
    }
  }
  return best_info;
}

/*!
 * Try to replace specific substrings with better variants.
 * These are for hiding confusing text transforms.
 */
std::string GameTextFontBank::replace_to_utf8(std::string& str) const {
  std::string newstr;

  for (int i = 0; i < (int)str.length();) {
    auto remap = find_replace_to_utf8(str, i);
    if (!remap) {
      newstr.push_back(str.at(i));
      i += 1;
    } else {
      for (auto b : remap->to) {
        newstr.push_back(b);
      }
      i += remap->from.length();
    }
  }

  str = newstr;
  return str;
}

std::string GameTextFontBank::replace_to_game(std::string& str) const {
  std::string newstr;

  for (int i = 0; i < (int)str.length();) {
    auto remap = find_replace_to_game(str, i);
    if (!remap) {
      newstr.push_back(str.at(i));
      i += 1;
    } else {
      for (auto b : remap->from) {
        newstr.push_back(b);
      }
      i += remap->to.length();
    }
  }

  str = newstr;
  return str;
}

std::string GameTextFontBank::encode_utf8_to_game(std::string& str) const {
  std::string newstr;

  for (int i = 0; i < (int)str.length();) {
    auto remap = find_encode_to_game(str, i);
    if (!remap) {
      newstr.push_back(str.at(i));
      i += 1;
    } else {
      for (auto b : remap->bytes) {
        newstr.push_back(b);
      }
      i += remap->chars.length();
    }
  }

  str = newstr;
  return str;
}

/*!
 * Turn a normal readable string into a string readable in the in-game font encoding and converts
 * \cXX escape sequences
 */
// NOTE - the convert_utf8_to_game function is really really slow (about 80-90% of the
// time loading the text files)
// TODO - improve that as a follow up sometime in the future
std::string GameTextFontBank::convert_utf8_to_game(std::string str, bool escape) const {
  std::string newstr;

  if (escape) {
    for (size_t i = 0; i < str.size(); ++i) {
      auto c = str.at(i);
      if (c == '"') {
        newstr.push_back('"');
        i += 1;
      } else if (c == '\\') {
        if (i + 1 >= str.size()) {
          throw std::runtime_error("incomplete string escape code");
        }
        auto p = str.at(i + 1);
        if (p == 'c') {
          if (i + 3 >= str.size()) {
            throw std::runtime_error("incomplete string escape code");
          }
          auto first = str.at(i + 2);
          auto second = str.at(i + 3);
          if (!str_util::hex_char(first) || !str_util::hex_char(second)) {
            throw std::runtime_error("invalid character escape hex number");
          }
          char hex_num[3] = {first, second, '\0'};
          std::size_t end = 0;
          auto value = std::stoul(hex_num, &end, 16);
          if (end != 2) {
            throw std::runtime_error("invalid character escape");
          }
          ASSERT(value < 256);
          newstr.push_back(char(value));
          i += 3;
        } else if (p == '"' || p == '\\') {
          newstr.push_back(p);
          i += 1;
        } else {
          throw std::runtime_error(
              fmt::format("unknown string escape code '{}' (0x{:x})", p, u32(p)));
        }
      } else {
        newstr.push_back(c);
      }
    }
  } else {
    newstr = str;
  }

  replace_to_game(newstr);
  encode_utf8_to_game(newstr);
  return newstr;
}

bool GameTextFontBank::valid_char_range(const char in) const {
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

bool GameTextFontBank::is_language_id_korean(const int language_id) const {
  if (m_version == GameTextVersion::JAK2 && language_id == 6) {
    return true;
  } else if (m_version == GameTextVersion::JAK3 && language_id == 7) {
    return true;
  }
  return false;
}

/*!
 * Convert a string from the game-text font encoding to something normal.
 * Unprintable characters become escape sequences, including tab and newline.
 */
std::string GameTextFontBank::convert_game_to_utf8(const char* in) const {
  std::string temp;
  std::string result;
  while (*in) {
    auto remap = find_encode_to_utf8(in);
    if (remap != nullptr) {
      temp.append(remap->chars);
      in += remap->bytes.size() - 1;
    } else if (valid_char_range(*in) || *in == '\n' || *in == '\t' || *in == '\\' || *in == '\"') {
      temp.push_back(*in);
    } else {
      temp += fmt::format("\\c{:02x}", uint8_t(*in));
    }
    in++;
  }
  replace_to_utf8(temp);
  for (size_t i = 0; i < temp.length(); ++i) {
    auto c = temp.at(i);
    if (c == '\n') {
      result += "\\n";
    } else if (c == '\t') {
      result += "\\t";
    } else if (c == '\\') {
      if (i < temp.length() - 1 && temp.at(i + 1) == 'c') {
        result.push_back(c);
      } else {
        result += "\\\\";
      }
    } else if (c == '"') {
      result += "\\\"";
    } else {
      result.push_back(c);
    }
  }
  return replace_to_utf8(result);
}

std::string GameTextFontBank::convert_utf8_to_game_korean(std::string str) {
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
  std::string non_korean_buffer = "";
  size_t i = 0;
  while (i < str.size()) {
    char32_t cp = str_util::next_utf8_char(str, i);
    if (font_util_korean::is_korean_syllable(cp)) {
      // flush any non-korean buffer
      if (!non_korean_buffer.empty()) {
        output += 0x3;
        // encode / remap it
        replace_to_game(non_korean_buffer);
        encode_utf8_to_game(non_korean_buffer);
        output += non_korean_buffer;
        non_korean_buffer = "";
      }
      // write out the korean character
      output += font_util_korean::game_encode_korean_syllable(str, cp, m_korean_db.value());
    } else {
      non_korean_buffer += ((char)(cp));
    }
  }
  // flush any non-korean buffer
  if (!non_korean_buffer.empty()) {
    output += 0x3;
    // encode / remap it
    replace_to_game(non_korean_buffer);
    encode_utf8_to_game(non_korean_buffer);
    output += non_korean_buffer;
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
  std::string temp_substring = "";
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
      if (!temp_substring.empty()) {
        // handle remap
        std::string remapped_str;
        auto temp_substring_ptr = temp_substring.c_str();
        while (*temp_substring_ptr) {
          auto remap = find_encode_to_utf8(temp_substring_ptr);
          if (remap != nullptr) {
            remapped_str.append(remap->chars);
            temp_substring_ptr += remap->bytes.size() - 1;
          } else {
            remapped_str.push_back(*temp_substring_ptr);
          }
          temp_substring_ptr++;
        }
        replace_to_utf8(remapped_str);
        result += remapped_str;
        temp_substring = "";
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
        temp_substring += jamo;
      }
      num_syllable_glyphs--;
      if (num_syllable_glyphs == 0) {
        in_syllable_block = false;
        result += font_util_korean::compose_korean_containing_text(temp_substring);
        temp_substring = "";
      }
    } else {
      if (curr_byte != 0x3) {
        temp_substring.push_back(curr_byte);
      }
    }
    index++;
  }
  // flush any non-korean characters
  if (!temp_substring.empty()) {
    // handle remap
    std::string remapped_str;
    auto temp_substring_ptr = temp_substring.c_str();
    while (*temp_substring_ptr) {
      auto remap = find_encode_to_utf8(temp_substring_ptr);
      if (remap != nullptr) {
        remapped_str.append(remap->chars);
        temp_substring_ptr += remap->bytes.size() - 1;
      } else {
        remapped_str.push_back(*temp_substring_ptr);
      }
      temp_substring_ptr++;
    }
    replace_to_utf8(remapped_str);
    result += remapped_str;
    temp_substring = "";
  }
  return result;
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
