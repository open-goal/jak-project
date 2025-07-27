/*
    MIT License

    Copyright (c) 2017 Jonghwan Hyeon

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

    https://github.com/jonghwanhyeon/hangul-jamo/blob/main/LICENSE

    Code converted to C++ and deals with UTF-8 input
*/

#include "font_utils_korean.h"

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/string_util.h"

#include "fmt/format.h"

//
// Reference: http://www.unicode.org/versions/Unicode8.0.0/ch03.pdf#G24646
//

const int BASE_OF_SYLLABLES = 0xAC00;

const int BASE_OF_LEADING_CONSONANTS = 0x1100;
const int BASE_OF_VOWELS = 0x1161;
// One less than the beginning of the range of trailing consonants (0x11A8)
const int BASE_OF_TRAILING_CONSONANTS = 0x11A7;

const int NUMBER_OF_LEADING_CONSONANTS = 19;
const int NUMBER_OF_VOWELS = 21;
// One more than the number of trailing consonants
const int NUMBER_OF_TRAILING_CONSONANTS = 28;

const int NUMBER_OF_SYLLABLES_FOR_EACH_LEADING_CONSONANT =
    NUMBER_OF_VOWELS * NUMBER_OF_TRAILING_CONSONANTS;
const int NUMBER_OF_SYLLABLES =
    NUMBER_OF_LEADING_CONSONANTS * NUMBER_OF_SYLLABLES_FOR_EACH_LEADING_CONSONANT;

// TODO - fix all of these, these generated wrong
const std::vector<std::string> LEADING_CONSONANTS = {"ㄱ", "ㄲ", "ㄴ", "ㄷ", "ㄸ", "ㄹ", "ㅁ",
                                                     "ㅂ", "ㅃ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅉ",
                                                     "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ"};

const std::unordered_map<std::string, int> INDEX_BY_LEADING_CONSONANT = []() {
  std::unordered_map<std::string, int> m;
  for (size_t i = 0; i < LEADING_CONSONANTS.size(); ++i) {
    m[LEADING_CONSONANTS[i]] = static_cast<int>(i);
  }
  return m;
}();

const std::vector<std::string> VOWELS = {"ㅏ", "ㅐ", "ㅑ", "ㅒ", "ㅓ", "ㅔ", "ㅕ",
                                         "ㅖ", "ㅗ", "ㅘ", "ㅙ", "ㅚ", "ㅛ", "ㅜ",
                                         "ㅝ", "ㅞ", "ㅟ", "ㅠ", "ㅡ", "ㅢ", "ㅣ"};

const std::unordered_map<std::string, int> INDEX_BY_VOWEL = []() {
  std::unordered_map<std::string, int> m;
  for (size_t i = 0; i < VOWELS.size(); ++i) {
    m[VOWELS[i]] = static_cast<int>(i);
  }
  return m;
}();

const std::vector<std::string> TRAILING_CONSONANTS = {
    "",   "ᆨ",  "ᆩ", "ᆪ", "ᆫ",  "ᆬ", "ᆭ", "ᆮ", "ᆯ", "ᆯ", "ᆱ", "ㄼ", "ㄽ", "ㄾ",
    "ㄿ", "ㅀ", "ᆷ", "ᆸ", "ㅄ", "ᆺ", "ᆻ", "ᆼ", "ᆽ", "ᆾ", "ᆿ", "ᇀ",  "ᇁ",  "ᇂ"};

const std::unordered_map<std::string, int> INDEX_BY_TRAILING_CONSONANT = []() {
  std::unordered_map<std::string, int> m;
  for (size_t i = 0; i < TRAILING_CONSONANTS.size(); ++i) {
    m[TRAILING_CONSONANTS[i]] = static_cast<int>(i);
  }
  return m;
}();

bool font_util_korean::is_korean_syllable(char32_t syllable) {
  int index_of_syllable = syllable - BASE_OF_SYLLABLES;
  return 0 <= index_of_syllable && index_of_syllable < NUMBER_OF_SYLLABLES;
}

inline bool is_jamo_character(const std::string& character) {
  return (std::find(LEADING_CONSONANTS.begin(), LEADING_CONSONANTS.end(), character) !=
          LEADING_CONSONANTS.end()) ||
         (std::find(VOWELS.begin(), VOWELS.end(), character) != VOWELS.end()) ||
         (std::find(TRAILING_CONSONANTS.begin(), TRAILING_CONSONANTS.end(), character) !=
          TRAILING_CONSONANTS.end());
}

inline std::string codepoint_to_utf8(char32_t cp) {
  std::string result;
  if (cp <= 0x7F) {
    result += static_cast<char>(cp);
  } else if (cp <= 0x7FF) {
    result += static_cast<char>(0xC0 | ((cp >> 6) & 0x1F));
    result += static_cast<char>(0x80 | (cp & 0x3F));
  } else if (cp <= 0xFFFF) {
    result += static_cast<char>(0xE0 | ((cp >> 12) & 0x0F));
    result += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    result += static_cast<char>(0x80 | (cp & 0x3F));
  } else if (cp <= 0x10FFFF) {
    result += static_cast<char>(0xF0 | ((cp >> 18) & 0x07));
    result += static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
    result += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    result += static_cast<char>(0x80 | (cp & 0x3F));
  }
  return result;
}

inline std::string compose_jamo_characters(const std::string& leading_consonant,
                                           const std::string& vowel,
                                           const std::optional<std::string>& trailing_consonant) {
  const int index_of_leading_consonant = INDEX_BY_LEADING_CONSONANT.at(leading_consonant) *
                                         NUMBER_OF_SYLLABLES_FOR_EACH_LEADING_CONSONANT;
  const int index_of_vowel = INDEX_BY_VOWEL.at(vowel) * NUMBER_OF_TRAILING_CONSONANTS;
  const int index_of_leading_consonant_and_vowel = index_of_leading_consonant + index_of_vowel;

  int index_of_trailing = trailing_consonant.has_value()
                              ? INDEX_BY_TRAILING_CONSONANT.at(trailing_consonant.value())
                              : 0;
  int index_of_syllable = index_of_leading_consonant_and_vowel + index_of_trailing;

  return codepoint_to_utf8(BASE_OF_SYLLABLES + index_of_syllable);
}

std::vector<char32_t> utf8_to_codepoints(const std::string& text) {
  std::vector<char32_t> codepoints;
  size_t i = 0;
  while (i < text.size()) {
    char32_t cp = 0;
    unsigned char c = text[i];
    if (c < 0x80) {
      cp = c;
      ++i;
    } else if ((c >> 5) == 0x6) {
      cp = ((c & 0x1F) << 6) | (text[i + 1] & 0x3F);
      i += 2;
    } else if ((c >> 4) == 0xE) {
      cp = ((c & 0x0F) << 12) | ((text[i + 1] & 0x3F) << 6) | (text[i + 2] & 0x3F);
      i += 3;
    } else if ((c >> 3) == 0x1E) {
      cp = ((c & 0x07) << 18) | ((text[i + 1] & 0x3F) << 12) | ((text[i + 2] & 0x3F) << 6) |
           (text[i + 3] & 0x3F);
      i += 4;
    } else {
      ++i;
    }
    codepoints.push_back(cp);
  }
  return codepoints;
}

bool is_leading(char32_t c) {
  // Replace with actual lookup (e.g. return LEADING_CONSONANTS.count(c) > 0;)
  return (c >= 0x1100 && c <= 0x1112);
}

bool is_vowel(char32_t c) {
  return (c >= 0x1161 && c <= 0x1175);
}

bool is_trailing(char32_t c) {
  return (c >= 0x11A8 && c <= 0x11C2);
}

// Compose jamo into a syllable block
char32_t compose_jamo(char32_t lead, char32_t vowel, std::optional<char32_t> trail = std::nullopt) {
  char32_t l_index = lead - 0x1100;
  char32_t v_index = vowel - 0x1161;
  char32_t t_index = trail ? (*trail - 0x11A7) : 0;
  return 0xAC00 + (l_index * 21 * 28) + (v_index * 28) + t_index;
}

std::string font_util_korean::compose_korean_containing_text(const std::string& text) {
  std::string output;
  std::vector<char32_t> cps = utf8_to_codepoints(text);

  size_t i = 0;
  while (i < cps.size()) {
    char32_t first = cps[i];
    char32_t second = (i + 1 < cps.size()) ? cps[i + 1] : 0;
    char32_t third = (i + 2 < cps.size()) ? cps[i + 2] : 0;
    char32_t fourth = (i + 3 < cps.size()) ? cps[i + 3] : 0;
    if (is_leading(first) && is_vowel(second) && is_leading(third) && is_vowel(fourth)) {
      char32_t syllable = compose_jamo(first, second);
      output += codepoint_to_utf8(syllable);
      i += 2;  // consume 2 codepoints
    } else if (is_leading(first) && is_vowel(second) && is_trailing(third)) {
      char32_t syllable = compose_jamo(first, second, third);
      output += codepoint_to_utf8(syllable);
      i += 3;  // consume 3 codepoints
    } else if (is_leading(first) && is_vowel(second)) {
      char32_t syllable = compose_jamo(first, second);
      output += codepoint_to_utf8(syllable);
      i += 2;  // consume 2 codepoints
    } else {
      output += codepoint_to_utf8(first);
      i += 1;
    }
  }

  return output;
}

// std::string font_util_korean::decompose_korean_containing_text(const std::string& text) {
//   std::string output;
//   size_t i = 0;
//   while (i < text.size()) {
//     char32_t cp = next_utf8_char(text, i);

//     if (is_syllable(cp)) {
//       // Decompose Hangul syllable block into jamo
//       char32_t SIndex = cp - 0xAC00;
//       char32_t L = 0x1100 + SIndex / (21 * 28);
//       char32_t V = 0x1161 + (SIndex % (21 * 28)) / 28;
//       char32_t T = 0x11A7 + (SIndex % 28);

//       output += codepoint_to_utf8(L);
//       output += codepoint_to_utf8(V);
//       if (T != 0x11A7) {  // has final consonant
//         output += codepoint_to_utf8(T);
//       }
//     } else {
//       // For other characters, re-encode as-is
//       output += codepoint_to_utf8(cp);
//     }
//   }
//   return output;
// }

bool is_median_vowel_vertical(char32_t vowel) {
  if ((vowel >= 0x1161 && vowel <= 0x1168) || vowel == 0x1175) {
    return true;
  }
  return false;
}

bool is_median_vowel_horizontal(char32_t vowel) {
  if (vowel == 0x1169 || (vowel >= 0x116D && vowel <= 0x116E) ||
      (vowel >= 0x1172 && vowel <= 0x1173)) {
    return true;
  }
  return false;
}

bool is_median_vowel_combined(char32_t vowel) {
  if (vowel >= 0x1161 && vowel <= 0x1175 && !is_median_vowel_vertical(vowel) &&
      !is_median_vowel_horizontal(vowel)) {
    return true;
  }
  return false;
}

std::string glyph_hex_string_to_int(const std::string& str) {
  try {
    std::string result;
    if (str_util::starts_with(str, "extra_")) {
      // handle glyphs on the secondary texture page
      result += 0x5;
      std::string temp = str;
      str_util::replace(temp, "extra_", "");
      result += std::stoi(temp, nullptr, 0);
    } else {
      result += std::stoi(str, nullptr, 0);
    }
    return result;
  } catch (std::exception& e) {
    lg::error("Unable to convert hex_string_to_int: {}", str);
    throw e;
  }
}

std::string font_util_korean::game_encode_korean_syllable(
    const std::string& context,
    const char32_t cp,
    const std::unordered_map<std::string, KoreanLookupOrientations> db) {
  std::string output;
  // Decompose Hangul syllable block into jamo
  char32_t syllable_index = cp - BASE_OF_SYLLABLES;
  char32_t initial = BASE_OF_LEADING_CONSONANTS + syllable_index / (21 * 28);
  char32_t median = BASE_OF_VOWELS + (syllable_index % (21 * 28)) / 28;
  char32_t final = BASE_OF_TRAILING_CONSONANTS + (syllable_index % 28);

  bool final_present = final != BASE_OF_TRAILING_CONSONANTS;
  int orientation = -1;
  // Figure out which orientation we are dealing with
  if (!final_present) {
    // orientations 0 - 2
    if (is_median_vowel_vertical(median)) {
      orientation = 0;
    } else if (is_median_vowel_horizontal(median)) {
      orientation = 1;
    } else if (is_median_vowel_combined(median)) {
      orientation = 2;
    }
  } else {
    // orientations 3 - 5
    if (is_median_vowel_vertical(median)) {
      orientation = 3;
    } else if (is_median_vowel_horizontal(median)) {
      orientation = 4;
    } else if (is_median_vowel_combined(median)) {
      orientation = 5;
    }
  }
  ASSERT_MSG(orientation != -1,
             fmt::format("Unable to deduce drawing orientation for korean syllable block in '{}'",
                         context));

  // now that we know the orientation, we can consult our lookup database to get the glyphs to
  // use for all the involved jamo
  //
  // convert each jamo to utf-8 bytes since that is what our DB is encoded with.
  // - first see if the jamo has an alternative drawing glyph, if not, use the default
  //
  // the order the glyphs are drawn in does not matter, as they all overlap anyway.
  std::vector<std::string> glyphs_to_draw = {};
  const auto initial_utf8 = codepoint_to_utf8(initial);
  const auto median_utf8 = codepoint_to_utf8(median);
  if (final == BASE_OF_TRAILING_CONSONANTS) {  // no final consonant
    const auto initial_alt_lookup_key = fmt::format("<G>,{}", median_utf8);
    const auto& initial_alts = db.at(initial_utf8).at(orientation).alternatives;
    if (initial_alts.contains(initial_alt_lookup_key)) {
      glyphs_to_draw.push_back(glyph_hex_string_to_int(initial_alts.at(initial_alt_lookup_key)));
    } else {
      glyphs_to_draw.push_back(
          glyph_hex_string_to_int(db.at(initial_utf8).at(orientation).defaultGlyph));
    }
    const auto median_alt_lookup_key = fmt::format("{},<G>", initial_utf8);
    const auto& median_alts = db.at(median_utf8).at(orientation).alternatives;
    if (median_alts.contains(median_alt_lookup_key)) {
      glyphs_to_draw.push_back(glyph_hex_string_to_int(median_alts.at(median_alt_lookup_key)));
    } else {
      glyphs_to_draw.push_back(
          glyph_hex_string_to_int(db.at(median_utf8).at(orientation).defaultGlyph));
    }
  } else {
    const auto final_utf8 = codepoint_to_utf8(final);
    const auto initial_alt_lookup_key = fmt::format("<G>,{},{}", median_utf8, final_utf8);
    const auto& initial_alts = db.at(initial_utf8).at(orientation).alternatives;
    if (initial_alts.contains(initial_alt_lookup_key)) {
      glyphs_to_draw.push_back(glyph_hex_string_to_int(initial_alts.at(initial_alt_lookup_key)));
    } else {
      glyphs_to_draw.push_back(
          glyph_hex_string_to_int(db.at(initial_utf8).at(orientation).defaultGlyph));
    }
    const auto median_alt_lookup_key = fmt::format("{},<G>,{}", initial_utf8, final_utf8);
    const auto& median_alts = db.at(median_utf8).at(orientation).alternatives;
    if (median_alts.contains(median_alt_lookup_key)) {
      glyphs_to_draw.push_back(glyph_hex_string_to_int(median_alts.at(median_alt_lookup_key)));
    } else {
      glyphs_to_draw.push_back(
          glyph_hex_string_to_int(db.at(median_utf8).at(orientation).defaultGlyph));
    }
    const auto final_alt_lookup_key = fmt::format("{},{},<G>", initial_utf8, median_utf8);
    const auto& final_alts = db.at(final_utf8).at(orientation).alternatives;
    if (final_alts.contains(final_alt_lookup_key)) {
      glyphs_to_draw.push_back(glyph_hex_string_to_int(final_alts.at(final_alt_lookup_key)));
    } else {
      glyphs_to_draw.push_back(
          glyph_hex_string_to_int(db.at(final_utf8).at(orientation).defaultGlyph));
    }
  }

  // Get rid of any duplicates in the glyphs
  std::vector<std::string> final_glyphs_to_draw = {};
  for (const auto& glyph : glyphs_to_draw) {
    if (std::find(final_glyphs_to_draw.begin(), final_glyphs_to_draw.end(), glyph) ==
        final_glyphs_to_draw.end()) {
      final_glyphs_to_draw.push_back(glyph);
    }
  }

  output += 0x4;
  output += final_glyphs_to_draw.size();
  for (const auto& glyph : final_glyphs_to_draw) {
    output += glyph;
  }
  return output;
}
