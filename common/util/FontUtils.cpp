/*!
 * @file FontUtils.cpp
 *
 * Code for handling text and strings in Jak 1's "large font" format.
 *
 * MAKE SURE THIS FILE IS ENCODED IN UTF-8!!! The various strings here depend on it.
 * Always verify the encoding if string detection suddenly goes awry.
 */

#include "FontUtils.h"

#include <algorithm>
#include <stdexcept>
#include <array>
#include <unordered_set>

#include "string_util.h"

#include "common/util/Assert.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/format.h"

const std::unordered_map<std::string, GameTextVersion> sTextVerEnumMap = {
    {"jak1-v1", GameTextVersion::JAK1_V1},
    {"jak1-v2", GameTextVersion::JAK1_V2},
    {"jak2", GameTextVersion::JAK2}};

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

static constexpr u32 utf8_hangul_syllable_base = 0xEAB080;
static constexpr int utf8_hangul_lead_consonant_amount = 19;
static constexpr int utf8_hangul_vowel_amount = 21;
static constexpr int utf8_hangul_trail_consonant_amount = 28;

// TABLE FORMAT
// 0 - TWO-JAMO SEQ 1
// 1 - THREE-JAMO SEQ 1
// 2 - TWO-JAMO SEQ 2
// 3 - THREE-JAMO SEQ 2

// special sequences:
// 0x389 - [Xㅜㄴ]

static std::vector<std::unordered_set<int>> s_unsorted_hangul_lead_consonant = {
    /* ㄱ */ {0x306},
    /* ㄲ */ {0x307},
    /* ㄴ */ {0x308, 0x31f},
    /* ㄷ */ {0x309, 0x370},
    /* ㄸ */ {0x30A},
    /* ㄹ */ {0x30b, 0x372},
    /* ㅁ */ {0x30C, 0x373},
    /* ㅂ */ {0x30d},
    /* ㅃ */ {0x30e},
    /* ㅅ */ {0x320},
    /* ㅆ */ {},
    /* ㅇ */ {0x30F, 0x378},
    /* ㅈ */ {0x310, 0x379},
    /* ㅉ */ {0x311},
    /* ㅊ */ {},
    /* ㅋ */ {0x312},
    /* ㅌ */ {0x313},
    /* ㅍ */ {0x314},
    /* ㅎ */ {0x32E},
};

static std::vector<std::unordered_set<int>> s_unsorted_hangul_vowel = {
    /* ㅏ */ {0x31A, 0x315},
    /* ㅐ */ {},
    /* ㅑ */ {0x316, 0x31b},
    /* ㅒ */ {},
    /* ㅓ */ {0x31D, 0x318},
    /* ㅔ */ {},
    /* ㅕ */ {0x31E, 0x319},
    /* ㅖ */ {},
    /* ㅗ */ {0x382, 0x37F},
    /* ㅘ */ {},
    /* ㅙ */ {},
    /* ㅚ */ {},
    /* ㅛ */ {0x383},
    /* ㅜ */ {},
    /* ㅝ */ {},
    /* ㅞ */ {},
    /* ㅟ */ {},
    /* ㅠ */ {},
    /* ㅡ */ {0x384},
    /* ㅢ */ {},
    /* ㅣ */ {0x31C, 0x317},
};

static std::vector<std::unordered_set<int>> s_unsorted_hangul_trail_consonant = {
    /*    */ {},
    /* ㄱ */ {0x3C5, 0x3FE},
    /* ㄲ */ {},
    /* ㄳ */ {},
    /* ㄴ */ {0x3C8, 0x3FF},
    /* ㄵ */ {},
    /* ㄶ */ {},
    /* ㄷ */ {},
    /* ㄹ */ {0x3CC, 0x186},
    /* ㄺ */ {},
    /* ㄻ */ {},
    /* ㄼ */ {},
    /* ㄽ */ {},
    /* ㄾ */ {},
    /* ㄿ */ {},
    /* ㅀ */ {},
    /* ㅁ */ {0x187},
    /* ㅂ */ {0x188},
    /* ㅄ */ {},
    /* ㅅ */ {0x3D5, 0x189},
    /* ㅆ */ {0x3D6, 0x18A},
    /* ㅇ */ {0x3D7, 0x3EE, 0x18B},
    /* ㅈ */ {},
    /* ㅊ */ {},
    /* ㅋ */ {},
    /* ㅌ */ {},
    /* ㅍ */ {},
    /* ㅎ */ {},
};

static std::vector<std::unordered_set<int>> s_unsorted_hangul_other = {
    /*    */ {},
    /* ㄱ */ {0x3C5},
    /* ㄲ */ {},
    /* ㄳ */ {},
    /* ㄴ */ {0x3C8, 0x3C8},
    /* ㄵ */ {},
    /* ㄶ */ {},
    /* ㄷ */ {},
    /* ㄹ */ {0x3CC, 0x186},
    /* ㄺ */ {},
    /* ㄻ */ {},
    /* ㄼ */ {},
    /* ㄽ */ {},
    /* ㄾ */ {},
    /* ㄿ */ {},
    /* ㅀ */ {},
    /* ㅁ */ {0x187},
    /* ㅂ */ {0x188},
    /* ㅄ */ {},
    /* ㅅ */ {0x3D5, 0x189},
    /* ㅆ */ {0x3D6, 0x18A},
    /* ㅇ */ {0x3D7, 0x3EE, 0x18B},
    /* ㅈ */ {},
    /* ㅊ */ {},
    /* ㅋ */ {},
    /* ㅌ */ {},
    /* ㅍ */ {},
    /* ㅎ */ {},
};

static std::vector<std::vector<int>> s_hangul_lead_consonant = {
    /* ㄱ */ {0x306, 0x306, -9999, 0x306},
    /* ㄲ */ {-9999, -9999},
    /* ㄴ */ {-9999, -9999},
    /* ㄷ */ {-9999, -9999, 0x370, -9999},
    /* ㄸ */ {-9999, -9999},
    /* ㄹ */ {-9999, -9999, 0x372, -9999},
    /* ㅁ */ {-9999, 0x30C, 0x373, -9999},
    /* ㅂ */ {-9999, -9999},
    /* ㅃ */ {-9999, -9999},
    /* ㅅ */ {0x320, -9999},
    /* ㅆ */ {-9999, -9999},
    /* ㅇ */ {-9999, 0x30F, 0x30F, 0x30F, -9999, 0x378},
    /* ㅈ */ {0x310, 0x310, -9999, 0x379},
    /* ㅉ */ {-9999, -9999},
    /* ㅊ */ {-9999, -9999},
    /* ㅋ */ {-9999, -9999},
    /* ㅌ */ {-9999, -9999},
    /* ㅍ */ {-9999, -9999},
    /* ㅎ */ {0x32E, -9999},
};

static std::vector<std::vector<int>> s_hangul_vowel = {
    /* ㅏ */ {0x31A, 0x315, -9999, 0x31A},
    /* ㅐ */ {-9999, -9999},
    /* ㅑ */ {-9999, -9999},
    /* ㅒ */ {-9999, -9999},
    /* ㅓ */ {0x31D, 0x31D, -9999, 0x318},
    /* ㅔ */ {-9999, -9999},
    /* ㅕ */ {0x31E, 0x319},
    /* ㅖ */ {-9999, -9999},
    /* ㅗ */ {-9999, -9999, 0x382, 0x37F},
    /* ㅘ */ {-9999, -9999},
    /* ㅙ */ {-9999, -9999},
    /* ㅚ */ {-9999, -9999},
    /* ㅛ */ {-9999, -9999, 0x383, -9999},
    /* ㅜ */ {-9999, -9999},
    /* ㅝ */ {-9999, -9999},
    /* ㅞ */ {-9999, -9999},
    /* ㅟ */ {-9999, -9999},
    /* ㅠ */ {-9999, -9999},
    /* ㅡ */ {-9999, -9999, 0x384, -9999},
    /* ㅢ */ {-9999, -9999},
    /* ㅣ */ {0x31C, 0x317},
};

static std::vector<std::vector<int>> s_hangul_trail_consonant = {
    /*    */ {-9999},
    /* ㄱ */ {0x3C5},
    /* ㄲ */ {-9999},
    /* ㄳ */ {-9999},
    /* ㄴ */ {0x3C8, 0x3C8},
    /* ㄵ */ {-9999},
    /* ㄶ */ {-9999},
    /* ㄷ */ {-9999},
    /* ㄹ */ {0x3CC},
    /* ㄺ */ {-9999},
    /* ㄻ */ {-9999},
    /* ㄼ */ {-9999},
    /* ㄽ */ {-9999},
    /* ㄾ */ {-9999},
    /* ㄿ */ {-9999},
    /* ㅀ */ {-9999},
    /* ㅁ */ {-9999},
    /* ㅂ */ {-9999},
    /* ㅄ */ {-9999},
    /* ㅅ */ {0x3D5},
    /* ㅆ */ {0x3D6},
    /* ㅇ */ {0x3D7, 0x3EE},
    /* ㅈ */ {-9999},
    /* ㅊ */ {-9999},
    /* ㅋ */ {-9999},
    /* ㅌ */ {-9999},
    /* ㅍ */ {-9999},
    /* ㅎ */ {-9999},
};


// temp
//static std::vector<std::vector<int>> s_hangul_lead_consonant = {
//    /* ㄱ */ {0x306, 0x31F, 0x333, 0x35e},
//    /* ㄲ */ {0x307, -9999, 0x334},
//    /* ㄴ */ {0x308, -9999, 0x335, 0x34a},
//    /* ㄷ */ {0x309, 0x326, 0x336, 0x351},
//    /* ㄸ */ {0x30A, 0x327, 0x337, 0x352},
//    /* ㄹ */ {0x30B, 0x328, 0x338, 0x353},
//    /* ㅁ */ {0x30C, -9999, 0x339},
//    /* ㅂ */ {0x30D, -9999, 0x33a},
//    /* ㅃ */ {0x30E, -9999, 0x33b},
//    /* ㅅ */ {-9999, 0x320, 0x34b},
//    /* ㅆ */ {-9999, 0x321, 0x34c},
//    /* ㅇ */ {0x30F, -9999, 0x33c},
//    /* ㅈ */ {0x310, -9999, 0x33d},
//    /* ㅉ */ {0x311, -9999, 0x33e},
//    /* ㅊ */ {0x32D, 0x358},
//    /* ㅋ */ {0x312, -9999, 0x33f, 0x35f},
//    /* ㅌ */ {0x313, 0x329, 0x340, 0x354},
//    /* ㅍ */ {0x314, 0x32A, 0x341, 0x355},
//    /* ㅎ */ {0x32E, 0x359},
//};
//static std::vector<std::vector<int>> s_hangul_vowel_trio = {
//    /* ㅏ */ {0x315, -9999},
//    /* ㅐ */ {0x342, -9999},
//    /* ㅑ */ {0x316, -9999},
//    /* ㅒ */ {0x343, -9999},
//    /* ㅓ */ {0x318, 0x322, 0x32F},
//    /* ㅔ */ {0x344, 0x34d, 0x35a},
//    /* ㅕ */ {0x319, 0x323, 0x32B, 0x330},
//    /* ㅖ */ {0x345, 0x34e, 0x356, 0x35b},
//    /* ㅗ */ {0x360, 0x362},
//    /* ㅘ */ {-9999, -9999},
//    /* ㅙ */ {-9999, -9999},
//    /* ㅚ */ {-9999, -9999},
//    /* ㅛ */ {0x361, 0x363},
//    /* ㅜ */ {-9999, -9999},
//    /* ㅝ */ {-9999, -9999},
//    /* ㅞ */ {-9999, -9999},
//    /* ㅟ */ {-9999, -9999},
//    /* ㅠ */ {-9999, -9999},
//    /* ㅡ */ {-9999, -9999},
//    /* ㅢ */ {-9999, -9999},
//    /* ㅣ */ {0x317, -9999},
//};
//static std::vector<std::vector<int>> s_hangul_vowel_duo = {
//    /* ㅏ */ {0x31A, -9999},
//    /* ㅐ */ {0x346, -9999},
//    /* ㅑ */ {0x31B, -9999},
//    /* ㅒ */ {0x347, -9999},
//    /* ㅓ */ {0x31D, 0x324, 0x331},
//    /* ㅔ */ {0x348, 0x34f, 0x35c},
//    /* ㅕ */ {0x31E, 0x325, 0x32C, 0x332},
//    /* ㅖ */ {0x349, 0x350, 0x357, 0x35d},
//    /* ㅗ */ {-9999, -9999},
//    /* ㅘ */ {-9999, -9999},
//    /* ㅙ */ {-9999, -9999},
//    /* ㅚ */ {-9999, -9999},
//    /* ㅛ */ {-9999, -9999},
//    /* ㅜ */ {-9999, -9999},
//    /* ㅝ */ {-9999, -9999},
//    /* ㅞ */ {-9999, -9999},
//    /* ㅟ */ {-9999, -9999},
//    /* ㅠ */ {-9999, -9999},
//    /* ㅡ */ {-9999, -9999},
//    /* ㅢ */ {-9999, -9999},
//    /* ㅣ */ {0x31C, -9999},
//};

static std::vector<u64> seqs = {};

std::string convert_korean_text_from_game(const char* in) {
  std::string out;
  while (*in) {
    if (*in == 3) {
      ++in;
      while (*in && *in != 3 && *in != 4) {
        out.push_back(*in);
        ++in;
      }
    } else {
      ++in;
      int len = *in;
      ++in;
      int i = 0;
      bool save_seq = true;
      u64 seq = 0;
      while (*in && *in != 3 && *in != 4) {
        if (*in == 5) {
          ++in;
          if (save_seq) {
            seq |= (0x100ULL | u8(*in)) << (i * 16);
          }
          out.push_back(0x1);
        } else {
          if (save_seq) {
            seq |= (0x300ULL | u8(*in)) << (i * 16);
          }
          out.push_back(0x3);
        }
        out.push_back(*in);
        ++in;
        ++i;
      }
      if (save_seq && std::find(seqs.begin(), seqs.end(), seq) == seqs.end()) {
        seqs.push_back(seq);
        std::sort(seqs.begin(), seqs.end());
      }
    }
  }
  return out;
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

GameTextFontBank::~GameTextFontBank() {
  if (!seqs.empty()) {
    printf("\nall seqs:\n");
    std::vector<std::vector<u16>> all_jamo_by_pos(4);
    std::vector<u16> all_jamo;
    for (auto seq : seqs) {
      for (int i = 0; i < 4; ++i) {
        u16 v = (seq >> (i * 16)) & 0xffff;
        if (v == 0) {
          break;
        }
        auto& jamo_seq = all_jamo_by_pos.at(i);
        if (std::find(jamo_seq.begin(), jamo_seq.end(), v) == jamo_seq.end()) {
          jamo_seq.push_back(v);
        }
        if (std::find(all_jamo.begin(), all_jamo.end(), v) == all_jamo.end()) {
          all_jamo.push_back(v);
        }
        // printf("0x%x ", v);
      }
      // printf("\n");
    }

    std::sort(all_jamo_by_pos.at(0).begin(), all_jamo_by_pos.at(0).end());
    std::sort(all_jamo_by_pos.at(1).begin(), all_jamo_by_pos.at(1).end());
    std::sort(all_jamo_by_pos.at(2).begin(), all_jamo_by_pos.at(2).end());
    std::sort(all_jamo_by_pos.at(3).begin(), all_jamo_by_pos.at(3).end());
    std::sort(all_jamo.begin(), all_jamo.end());
    printf("\nall first jamo:\n");
    for (auto v : all_jamo_by_pos.at(0)) {
      printf("0x%x\n", v);
    }
    printf("\nall second jamo:\n");
    for (auto v : all_jamo_by_pos.at(1)) {
      printf("0x%x\n", v);
    }
    printf("\nall third jamo:\n");
    for (auto v : all_jamo_by_pos.at(2)) {
      printf("0x%x\n", v);
    }
    printf("\nall fourth jamo:\n");
    for (auto v : all_jamo_by_pos.at(3)) {
      printf("0x%x\n", v);
    }
    printf("\nall jamo:\n");
    for (auto v : all_jamo) {
      printf("%x ", v);
      if (std::find(all_jamo_by_pos.at(0).begin(), all_jamo_by_pos.at(0).end(), v) !=
          all_jamo_by_pos.at(0).end()) {
        printf("pos 1\n");
      } else if (std::find(all_jamo_by_pos.at(1).begin(), all_jamo_by_pos.at(1).end(), v) !=
          all_jamo_by_pos.at(1).end()) {
        printf("pos 2\n");
      } else if (std::find(all_jamo_by_pos.at(2).begin(), all_jamo_by_pos.at(2).end(), v) !=
          all_jamo_by_pos.at(2).end()) {
        printf("pos 3\n");
      } else if (std::find(all_jamo_by_pos.at(3).begin(), all_jamo_by_pos.at(3).end(), v) !=
          all_jamo_by_pos.at(3).end()) {
        printf("pos 4\n");
      }
    }
    seqs.clear();
  }
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
  } else if (m_version == GameTextVersion::JAK2) {
    return ((in >= '0' && in <= '9') || (in >= 'A' && in <= 'Z') || (in >= 'a' && in <= 'z') ||
            m_passthrus->find(in) != m_passthrus->end()) &&
           in != '\\';
  }
  return false;
}

/*!
 * Convert a string from the game-text font encoding to something normal.
 * Unprintable characters become escape sequences, including tab and newline.
 */
std::string GameTextFontBank::convert_game_to_utf8(const char* in, bool korean) const {
  std::string temp;
  std::string result;

  if (korean) {
    result = convert_korean_text_from_game(in);
    in = result.c_str();
  }

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
  result.clear();

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

static std::vector<EncodeInfo> s_encode_info_null = {};
static std::vector<ReplaceInfo> s_replace_info_null = {};

/*!
 * ===========================
 * GAME TEXT FONT BANK - JAK 1
 * ===========================
 * This font is used in:
 * - Jak & Daxter: The Precursor Legacy (Black Label)
 */

static std::unordered_set<char> s_passthrus_jak1 = {'~', ' ', ',', '.', '-', '+', '(', ')',
                                                    '!', ':', '?', '=', '%', '*', '/', '#',
                                                    ';', '<', '>', '@', '[', '_'};

static std::vector<EncodeInfo> s_encode_info_jak1 = {
    // random
    {"ˇ", {0x10}},      // caron
    {"`", {0x11}},      // grave accent
    {"'", {0x12}},      // apostrophe
    {"^", {0x13}},      // circumflex
    {"<TIL>", {0x14}},  // tilde
    {"¨", {0x15}},      // umlaut
    {"º", {0x16}},      // numero/overring
    {"¡", {0x17}},      // inverted exclamation mark
    {"¿", {0x18}},      // inverted question mark

    {"海", {0x1a}},  // umi
    {"Æ", {0x1b}},   // aesc
    {"界", {0x1c}},  // kai
    {"Ç", {0x1d}},   // c-cedilla
    {"学", {0x1e}},  // gaku
    {"ß", {0x1f}},   // eszett

    {"ワ", {0x24}},  // wa

    {"ヲ", {0x26}},  // wo
    {"ン", {0x27}},  // -n

    {"岩", {0x5c}},  // iwa
    {"旧", {0x5d}},  // kyuu
    {"空", {0x5e}},  // sora
    //{"掘", {0x5f}},  // horu

    {"ヮ", {0x60}},  // -wa
    {"撃", {0x61}},  // utsu
    {"賢", {0x62}},  // kashikoi
    {"湖", {0x63}},  // mizuumi
    {"口", {0x64}},  // kuchi
    {"行", {0x65}},  // iku
    {"合", {0x66}},  // ai
    {"士", {0x67}},  // shi
    {"寺", {0x68}},  // tera
    {"山", {0x69}},  // yama
    {"者", {0x6a}},  // mono
    {"所", {0x6b}},  // tokoro
    {"書", {0x6c}},  // kaku
    {"小", {0x6d}},  // shou
    {"沼", {0x6e}},  // numa
    {"上", {0x6f}},  // ue
    {"城", {0x70}},  // shiro
    {"場", {0x71}},  // ba
    {"出", {0x72}},  // shutsu
    {"闇", {0x73}},  // yami
    {"遺", {0x74}},  // nokosu
    {"黄", {0x75}},  // ki
    {"屋", {0x76}},  // ya
    {"下", {0x77}},  // shita
    {"家", {0x78}},  // ie
    {"火", {0x79}},  // hi
    {"花", {0x7a}},  // hana
    {"レ", {0x7b}},  // re
    {"Œ", {0x7c}},   // oe
    {"ロ", {0x7d}},  // ro

    {"青", {0x7f}},  // ao

    {"・", {0x90}},  // nakaguro
    {"゛", {0x91}},  // dakuten
    {"゜", {0x92}},  // handakuten
    {"ー", {0x93}},  // chouompu
    {"『", {0x94}},  // nijuukagikakko left
    {"』", {0x95}},  // nijuukagikakko right
    // hiragana
    {"ぁ", {0x96}},  // -a
    {"あ", {0x97}},  // a
    {"ぃ", {0x98}},  // -i
    {"い", {0x99}},  // i
    {"ぅ", {0x9a}},  // -u
    {"う", {0x9b}},  // u
    {"ぇ", {0x9c}},  // -e
    {"え", {0x9d}},  // e
    {"ぉ", {0x9e}},  // -o
    {"お", {0x9f}},  // o
    {"か", {0xa0}},  // ka
    {"き", {0xa1}},  // ki
    {"く", {0xa2}},  // ku
    {"け", {0xa3}},  // ke
    {"こ", {0xa4}},  // ko
    {"さ", {0xa5}},  // sa
    {"し", {0xa6}},  // shi
    {"す", {0xa7}},  // su
    {"せ", {0xa8}},  // se
    {"そ", {0xa9}},  // so
    {"た", {0xaa}},  // ta
    {"ち", {0xab}},  // chi
    {"っ", {0xac}},  // sokuon
    {"つ", {0xad}},  // tsu
    {"て", {0xae}},  // te
    {"と", {0xaf}},  // to
    {"な", {0xb0}},  // na
    {"に", {0xb1}},  // ni
    {"ぬ", {0xb2}},  // nu
    {"ね", {0xb3}},  // ne
    {"の", {0xb4}},  // no
    {"は", {0xb5}},  // ha
    {"ひ", {0xb6}},  // hi
    {"ふ", {0xb7}},  // hu
    {"へ", {0xb8}},  // he
    {"ほ", {0xb9}},  // ho
    {"ま", {0xba}},  // ma
    {"み", {0xbb}},  // mi
    {"む", {0xbc}},  // mu
    {"め", {0xbd}},  // me
    {"も", {0xbe}},  // mo
    {"ゃ", {0xbf}},  // youon ya
    {"や", {0xc0}},  // ya
    {"ゅ", {0xc1}},  // youon yu
    {"ゆ", {0xc2}},  // yu
    {"ょ", {0xc3}},  // youon yo
    {"よ", {0xc4}},  // yo
    {"ら", {0xc5}},  // ra
    {"り", {0xc6}},  // ri
    {"る", {0xc7}},  // ru
    {"れ", {0xc8}},  // re
    {"ろ", {0xc9}},  // ro
    {"ゎ", {0xca}},  // -wa
    {"わ", {0xcb}},  // wa
    {"を", {0xcc}},  // wo
    {"ん", {0xcd}},  // -n
    // katakana
    {"ァ", {0xce}},  // -a
    {"ア", {0xcf}},  // a
    {"ィ", {0xd0}},  // -i
    {"イ", {0xd1}},  // i
    {"ゥ", {0xd2}},  // -u
    {"ウ", {0xd3}},  // u
    {"ェ", {0xd4}},  // -e
    {"エ", {0xd5}},  // e
    {"ォ", {0xd6}},  // -o
    {"オ", {0xd7}},  // o
    {"カ", {0xd8}},  // ka
    {"キ", {0xd9}},  // ki
    {"ク", {0xda}},  // ku
    {"ケ", {0xdb}},  // ke
    {"コ", {0xdc}},  // ko
    {"サ", {0xdd}},  // sa
    {"シ", {0xde}},  // shi
    {"ス", {0xdf}},  // su
    {"セ", {0xe0}},  // se
    {"ソ", {0xe1}},  // so
    {"タ", {0xe2}},  // ta
    {"チ", {0xe3}},  // chi
    {"ッ", {0xe4}},  // sokuon
    {"ツ", {0xe5}},  // tsu
    {"テ", {0xe6}},  // te
    {"ト", {0xe7}},  // to
    {"ナ", {0xe8}},  // na
    {"ニ", {0xe9}},  // ni
    {"ヌ", {0xea}},  // nu
    {"ネ", {0xeb}},  // ne
    {"ノ", {0xec}},  // no
    {"ハ", {0xed}},  // ha
    {"ヒ", {0xee}},  // hi
    {"フ", {0xef}},  // hu
    {"ヘ", {0xf0}},  // he
    {"ホ", {0xf1}},  // ho
    {"マ", {0xf2}},  // ma
    {"ミ", {0xf3}},  // mi
    {"ム", {0xf4}},  // mu
    {"メ", {0xf5}},  // me
    {"モ", {0xf6}},  // mo
    {"ャ", {0xf7}},  // youon ya
    {"ヤ", {0xf8}},  // ya
    {"ュ", {0xf9}},  // youon yu
    {"ユ", {0xfa}},  // yu
    {"ョ", {0xfb}},  // youon yo
    {"ヨ", {0xfc}},  // yo
    {"ラ", {0xfd}},  // ra
    {"リ", {0xfe}},  // ri
    {"ル", {0xff}},  // ru
    // kanji 2
    {"宝", {1, 0x01}},  // takara

    {"石", {1, 0x10}},  // ishi
    {"赤", {1, 0x11}},  // aka
    {"跡", {1, 0x12}},  // ato
    {"川", {1, 0x13}},  // kawa
    {"戦", {1, 0x14}},  // ikusa
    {"村", {1, 0x15}},  // mura
    {"隊", {1, 0x16}},  // tai
    {"台", {1, 0x17}},  // utena
    {"長", {1, 0x18}},  // osa
    {"鳥", {1, 0x19}},  // tori
    {"艇", {1, 0x1a}},  // tei
    {"洞", {1, 0x1b}},  // hora
    {"道", {1, 0x1c}},  // michi
    {"発", {1, 0x1d}},  // hatsu
    {"飛", {1, 0x1e}},  // tobu
    {"噴", {1, 0x1f}},  // fuku

    {"池", {1, 0xa0}},  // ike
    {"中", {1, 0xa1}},  // naka
    {"塔", {1, 0xa2}},  // tou
    {"島", {1, 0xa3}},  // shima
    {"部", {1, 0xa4}},  // bu
    {"砲", {1, 0xa5}},  // hou
    {"産", {1, 0xa6}},  // san
    {"眷", {1, 0xa7}},  // kaerimiru
    {"力", {1, 0xa8}},  // chikara
    {"緑", {1, 0xa9}},  // midori
    {"岸", {1, 0xaa}},  // kishi
    {"像", {1, 0xab}},  // zou
    {"谷", {1, 0xac}},  // tani
    {"心", {1, 0xad}},  // kokoro
    {"森", {1, 0xae}},  // mori
    {"水", {1, 0xaf}},  // mizu
    {"船", {1, 0xb0}},  // fune
    {"™", {1, 0xb1}},   // trademark
};

static std::vector<ReplaceInfo> s_replace_info_jak1 = {
    // other
    {"A~Y~-21H~-5Vº~Z", "Å"},
    {"N~Y~-6Hº~Z~+10H", "Nº"},
    {"O~Y~-16H~-1V/~Z", "Ø"},
    {"A~Y~-6H~+3V,~Z", "Ą"},
    {"E~Y~-6H~+2V,~Z", "Ę"},
    {"L~Y~-16H~+0V/~Z", "Ł"},
    {"Z~Y~-21H~-5Vº~Z", "Ż"},

    // tildes
    {"N~Y~-22H~-4V<TIL>~Z", "Ñ"},
    {"A~Y~-21H~-5V<TIL>~Z", "Ã"},  // custom
    {"O~Y~-22H~-4V<TIL>~Z", "Õ"},  // custom

    // acute accents
    {"A~Y~-21H~-5V'~Z", "Á"},
    {"E~Y~-22H~-5V'~Z", "É"},
    {"I~Y~-19H~-5V'~Z", "Í"},
    {"O~Y~-22H~-4V'~Z", "Ó"},
    {"U~Y~-24H~-3V'~Z", "Ú"},
    {"C~Y~-21H~-5V'~Z", "Ć"},
    {"N~Y~-21H~-5V'~Z", "Ń"},
    {"S~Y~-21H~-5V'~Z", "Ś"},
    {"Z~Y~-21H~-5V'~Z", "Ź"},

    // double acute accents
    {"O~Y~-28H~-4V'~-9H'~Z", "Ő"},   // custom
    {"U~Y~-27H~-4V'~-12H'~Z", "Ű"},  // custom

    // circumflex
    {"A~Y~-20H~-4V^~Z", "Â"},  // custom
    {"E~Y~-20H~-5V^~Z", "Ê"},
    {"I~Y~-19H~-5V^~Z", "Î"},
    {"O~Y~-20H~-4V^~Z", "Ô"},  // custom
    {"U~Y~-24H~-3V^~Z", "Û"},

    // grave accents
    {"A~Y~-21H~-5V`~Z", "À"},
    {"E~Y~-22H~-5V`~Z", "È"},
    {"I~Y~-19H~-5V`~Z", "Ì"},
    {"O~Y~-22H~-4V`~Z", "Ò"},  // custom
    {"U~Y~-24H~-3V`~Z", "Ù"},

    // umlaut
    {"A~Y~-21H~-5V¨~Z", "Ä"},
    {"E~Y~-20H~-5V¨~Z", "Ë"},
    {"I~Y~-19H~-5V¨~Z", "Ï"},  // custom
    {"O~Y~-22H~-4V¨~Z", "Ö"},
    {"O~Y~-22H~-3V¨~Z", "ö"},  // dumb
    {"U~Y~-22H~-3V¨~Z", "Ü"},

    // dakuten katakana
    {"~Yウ~Z゛", "ヴ"},
    {"~Yカ~Z゛", "ガ"},
    {"~Yキ~Z゛", "ギ"},
    {"~Yク~Z゛", "グ"},
    {"~Yケ~Z゛", "ゲ"},
    {"~Yコ~Z゛", "ゴ"},
    {"~Yサ~Z゛", "ザ"},
    {"~Yシ~Z゛", "ジ"},
    {"~Yス~Z゛", "ズ"},
    {"~Yセ~Z゛", "ゼ"},
    {"~Yソ~Z゛", "ゾ"},
    {"~Yタ~Z゛", "ダ"},
    {"~Yチ~Z゛", "ヂ"},
    {"~Yツ~Z゛", "ヅ"},
    {"~Yテ~Z゛", "デ"},
    {"~Yト~Z゛", "ド"},
    {"~Yハ~Z゛", "バ"},
    {"~Yヒ~Z゛", "ビ"},
    {"~Yフ~Z゛", "ブ"},
    {"~Yヘ~Z゛", "ベ"},
    {"~Yホ~Z゛", "ボ"},
    // handakuten katakana
    {"~Yハ~Z゜", "パ"},
    {"~Yヒ~Z゜", "ピ"},
    {"~Yフ~Z゜", "プ"},
    {"~Yヘ~Z゜", "ペ"},
    {"~Yホ~Z゜", "ポ"},
    // dakuten hiragana
    {"~Yか~Z゛", "が"},
    {"~Yき~Z゛", "ぎ"},
    {"~Yく~Z゛", "ぐ"},
    {"~Yけ~Z゛", "げ"},
    {"~Yこ~Z゛", "ご"},
    {"~Yさ~Z゛", "ざ"},
    {"~Yし~Z゛", "じ"},
    {"~Yす~Z゛", "ず"},
    {"~Yせ~Z゛", "ぜ"},
    {"~Yそ~Z゛", "ぞ"},
    {"~Yた~Z゛", "だ"},
    {"~Yち~Z゛", "ぢ"},
    {"~Yつ~Z゛", "づ"},
    {"~Yて~Z゛", "で"},
    {"~Yと~Z゛", "ど"},
    {"~Yは~Z゛", "ば"},
    {"~Yひ~Z゛", "び"},
    {"~Yふ~Z゛", "ぶ"},
    {"~Yへ~Z゛", "べ"},
    {"~Yほ~Z゛", "ぼ"},
    // handakuten hiragana
    {"~Yは~Z゜", "ぱ"},
    {"~Yひ~Z゜", "ぴ"},
    {"~Yふ~Z゜", "ぷ"},
    {"~Yへ~Z゜", "ぺ"},
    {"~Yほ~Z゜", "ぽ"},
    // japanese punctuation
    {",~+8H", "、"},
    {"~+8H ", "　"},

    // (hack) special case kanji
    {"~~", "世"},

    // playstation buttons
    {"~Y~22L<~Z~Y~27L*~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_X>"},
    {"~Y~22L<~Z~Y~26L;~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_TRIANGLE>"},
    {"~Y~22L<~Z~Y~25L@~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_CIRCLE>"},
    {"~Y~22L<~Z~Y~24L#~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_SQUARE>"},  // custom
};

GameTextFontBank g_font_bank_jak1_v1(GameTextVersion::JAK1_V1,
                                     &s_encode_info_jak1,
                                     &s_replace_info_jak1,
                                     &s_passthrus_jak1);

/*!
 * ================================
 * GAME TEXT FONT BANK - JAK 1 (v2)
 * ================================
 * This font is used in:
 * - Jak & Daxter: The Precursor Legacy (PAL)
 * - ジャックＸダクスター　～　旧世界の遺産
 * - Jak & Daxter: The Precursor Legacy (NTSC-U v2)
 *
 * It is the same as v1, but _ has been fixed and no longer overlaps 掘
 */

static std::vector<EncodeInfo> s_encode_info_jak1_v2 = {
    // random
    {"_", {0x03}},      // large space
    {"ˇ", {0x10}},      // caron
    {"`", {0x11}},      // grave accent
    {"'", {0x12}},      // apostrophe
    {"^", {0x13}},      // circumflex
    {"<TIL>", {0x14}},  // tilde
    {"¨", {0x15}},      // umlaut
    {"º", {0x16}},      // numero/overring
    {"¡", {0x17}},      // inverted exclamation mark
    {"¿", {0x18}},      // inverted question mark

    {"海", {0x1a}},  // umi
    {"Æ", {0x1b}},   // aesc
    {"界", {0x1c}},  // kai
    {"Ç", {0x1d}},   // c-cedilla
    {"学", {0x1e}},  // gaku
    {"ß", {0x1f}},   // eszett

    {"ワ", {0x24}},  // wa

    {"ヲ", {0x26}},  // wo
    {"ン", {0x27}},  // -n

    {"岩", {0x5c}},  // iwa
    {"旧", {0x5d}},  // kyuu
    {"空", {0x5e}},  // sora
    {"掘", {0x5f}},  // horu

    {"ヮ", {0x60}},  // -wa
    {"撃", {0x61}},  // utsu
    {"賢", {0x62}},  // kashikoi
    {"湖", {0x63}},  // mizuumi
    {"口", {0x64}},  // kuchi
    {"行", {0x65}},  // iku
    {"合", {0x66}},  // ai
    {"士", {0x67}},  // shi
    {"寺", {0x68}},  // tera
    {"山", {0x69}},  // yama
    {"者", {0x6a}},  // mono
    {"所", {0x6b}},  // tokoro
    {"書", {0x6c}},  // kaku
    {"小", {0x6d}},  // shou
    {"沼", {0x6e}},  // numa
    {"上", {0x6f}},  // ue
    {"城", {0x70}},  // shiro
    {"場", {0x71}},  // ba
    {"出", {0x72}},  // shutsu
    {"闇", {0x73}},  // yami
    {"遺", {0x74}},  // nokosu
    {"黄", {0x75}},  // ki
    {"屋", {0x76}},  // ya
    {"下", {0x77}},  // shita
    {"家", {0x78}},  // ie
    {"火", {0x79}},  // hi
    {"花", {0x7a}},  // hana
    {"レ", {0x7b}},  // re
    {"Œ", {0x7c}},   // oe
    {"ロ", {0x7d}},  // ro

    {"青", {0x7f}},  // ao

    {"・", {0x90}},  // nakaguro
    {"゛", {0x91}},  // dakuten
    {"゜", {0x92}},  // handakuten
    {"ー", {0x93}},  // chouompu
    {"『", {0x94}},  // nijuukagikakko left
    {"』", {0x95}},  // nijuukagikakko right
    // hiragana
    {"ぁ", {0x96}},  // -a
    {"あ", {0x97}},  // a
    {"ぃ", {0x98}},  // -i
    {"い", {0x99}},  // i
    {"ぅ", {0x9a}},  // -u
    {"う", {0x9b}},  // u
    {"ぇ", {0x9c}},  // -e
    {"え", {0x9d}},  // e
    {"ぉ", {0x9e}},  // -o
    {"お", {0x9f}},  // o
    {"か", {0xa0}},  // ka
    {"き", {0xa1}},  // ki
    {"く", {0xa2}},  // ku
    {"け", {0xa3}},  // ke
    {"こ", {0xa4}},  // ko
    {"さ", {0xa5}},  // sa
    {"し", {0xa6}},  // shi
    {"す", {0xa7}},  // su
    {"せ", {0xa8}},  // se
    {"そ", {0xa9}},  // so
    {"た", {0xaa}},  // ta
    {"ち", {0xab}},  // chi
    {"っ", {0xac}},  // sokuon
    {"つ", {0xad}},  // tsu
    {"て", {0xae}},  // te
    {"と", {0xaf}},  // to
    {"な", {0xb0}},  // na
    {"に", {0xb1}},  // ni
    {"ぬ", {0xb2}},  // nu
    {"ね", {0xb3}},  // ne
    {"の", {0xb4}},  // no
    {"は", {0xb5}},  // ha
    {"ひ", {0xb6}},  // hi
    {"ふ", {0xb7}},  // hu
    {"へ", {0xb8}},  // he
    {"ほ", {0xb9}},  // ho
    {"ま", {0xba}},  // ma
    {"み", {0xbb}},  // mi
    {"む", {0xbc}},  // mu
    {"め", {0xbd}},  // me
    {"も", {0xbe}},  // mo
    {"ゃ", {0xbf}},  // youon ya
    {"や", {0xc0}},  // ya
    {"ゅ", {0xc1}},  // youon yu
    {"ゆ", {0xc2}},  // yu
    {"ょ", {0xc3}},  // youon yo
    {"よ", {0xc4}},  // yo
    {"ら", {0xc5}},  // ra
    {"り", {0xc6}},  // ri
    {"る", {0xc7}},  // ru
    {"れ", {0xc8}},  // re
    {"ろ", {0xc9}},  // ro
    {"ゎ", {0xca}},  // -wa
    {"わ", {0xcb}},  // wa
    {"を", {0xcc}},  // wo
    {"ん", {0xcd}},  // -n
    // katakana
    {"ァ", {0xce}},  // -a
    {"ア", {0xcf}},  // a
    {"ィ", {0xd0}},  // -i
    {"イ", {0xd1}},  // i
    {"ゥ", {0xd2}},  // -u
    {"ウ", {0xd3}},  // u
    {"ェ", {0xd4}},  // -e
    {"エ", {0xd5}},  // e
    {"ォ", {0xd6}},  // -o
    {"オ", {0xd7}},  // o
    {"カ", {0xd8}},  // ka
    {"キ", {0xd9}},  // ki
    {"ク", {0xda}},  // ku
    {"ケ", {0xdb}},  // ke
    {"コ", {0xdc}},  // ko
    {"サ", {0xdd}},  // sa
    {"シ", {0xde}},  // shi
    {"ス", {0xdf}},  // su
    {"セ", {0xe0}},  // se
    {"ソ", {0xe1}},  // so
    {"タ", {0xe2}},  // ta
    {"チ", {0xe3}},  // chi
    {"ッ", {0xe4}},  // sokuon
    {"ツ", {0xe5}},  // tsu
    {"テ", {0xe6}},  // te
    {"ト", {0xe7}},  // to
    {"ナ", {0xe8}},  // na
    {"ニ", {0xe9}},  // ni
    {"ヌ", {0xea}},  // nu
    {"ネ", {0xeb}},  // ne
    {"ノ", {0xec}},  // no
    {"ハ", {0xed}},  // ha
    {"ヒ", {0xee}},  // hi
    {"フ", {0xef}},  // hu
    {"ヘ", {0xf0}},  // he
    {"ホ", {0xf1}},  // ho
    {"マ", {0xf2}},  // ma
    {"ミ", {0xf3}},  // mi
    {"ム", {0xf4}},  // mu
    {"メ", {0xf5}},  // me
    {"モ", {0xf6}},  // mo
    {"ャ", {0xf7}},  // youon ya
    {"ヤ", {0xf8}},  // ya
    {"ュ", {0xf9}},  // youon yu
    {"ユ", {0xfa}},  // yu
    {"ョ", {0xfb}},  // youon yo
    {"ヨ", {0xfc}},  // yo
    {"ラ", {0xfd}},  // ra
    {"リ", {0xfe}},  // ri
    {"ル", {0xff}},  // ru
    // kanji 2
    {"宝", {1, 0x01}},  // takara

    {"石", {1, 0x10}},  // ishi
    {"赤", {1, 0x11}},  // aka
    {"跡", {1, 0x12}},  // ato
    {"川", {1, 0x13}},  // kawa
    {"戦", {1, 0x14}},  // ikusa
    {"村", {1, 0x15}},  // mura
    {"隊", {1, 0x16}},  // tai
    {"台", {1, 0x17}},  // utena
    {"長", {1, 0x18}},  // osa
    {"鳥", {1, 0x19}},  // tori
    {"艇", {1, 0x1a}},  // tei
    {"洞", {1, 0x1b}},  // hora
    {"道", {1, 0x1c}},  // michi
    {"発", {1, 0x1d}},  // hatsu
    {"飛", {1, 0x1e}},  // tobu
    {"噴", {1, 0x1f}},  // fuku

    {"池", {1, 0xa0}},  // ike
    {"中", {1, 0xa1}},  // naka
    {"塔", {1, 0xa2}},  // tou
    {"島", {1, 0xa3}},  // shima
    {"部", {1, 0xa4}},  // bu
    {"砲", {1, 0xa5}},  // hou
    {"産", {1, 0xa6}},  // san
    {"眷", {1, 0xa7}},  // kaerimiru
    {"力", {1, 0xa8}},  // chikara
    {"緑", {1, 0xa9}},  // midori
    {"岸", {1, 0xaa}},  // kishi
    {"像", {1, 0xab}},  // zou
    {"谷", {1, 0xac}},  // tani
    {"心", {1, 0xad}},  // kokoro
    {"森", {1, 0xae}},  // mori
    {"水", {1, 0xaf}},  // mizu
    {"船", {1, 0xb0}},  // fune
    {"™", {1, 0xb1}},   // trademark
};

GameTextFontBank g_font_bank_jak1_v2(GameTextVersion::JAK1_V2,
                                     &s_encode_info_jak1_v2,
                                     &s_replace_info_jak1,
                                     &s_passthrus_jak1);

/*!
 * ================================
 * GAME TEXT FONT BANK - JAK 2
 * ================================
 * This font is used in:
 * - Jak II (NTSC-U, NTSC-K)
 * - Jak II - Renegade
 * - ジャックＸダクスター２
 */

static std::unordered_set<char> s_passthrus_jak2 = {'~', ' ', ',', '.', '-', '+', '(', ')',
                                                    '!', ':', '?', '=', '%', '*', '/', '#',
                                                    ';', '<', '>', '@', '[', '_', ']'};

static std::vector<ReplaceInfo> s_replace_info_jak2 = {
    // other
    {"A~Y~-21H~-5Vº~Z", "Å"},
    {"N~Y~-6Hº~Z~+10H", "Nº"},
    {"~+4Vç~-4V", ",c"},

    // tildes
    {"N~Y~-22H~-4V<TIL>~Z", "Ñ"},
    {"n~Y~-24H~-4V<TIL>~Z", "ñ"},
    {"A~Y~-21H~-5V<TIL>~Z", "Ã"},
    {"O~Y~-22H~-4V<TIL>~Z", "Õ"},

    // acute accents
    {"A~Y~-21H~-5V'~Z", "Á"},
    {"A~Y~-26H~-8V'~Z", "<Á_V2>"},  // unfortunate...
    {"a~Y~-25H~-5V'~Z", "á"},
    {"E~Y~-23H~-9V'~Z", "É"},
    {"e~Y~-26H~-5V'~Z", "é"},
    {"I~Y~-19H~-5V'~Z", "Í"},
    {"i~Y~-19H~-8V'~Z", "í"},
    {"O~Y~-22H~-4V'~Z", "Ó"},
    {"o~Y~-26H~-4V'~Z", "ó"},
    {"U~Y~-24H~-3V'~Z", "Ú"},
    {"u~Y~-24H~-3V'~Z", "ú"},

    // circumflex
    {"A~Y~-20H~-4V^~Z", "Â"},
    {"a~Y~-24H~-5V^~Z", "â"},
    {"E~Y~-20H~-5V^~Z", "Ê"},
    {"e~Y~-25H~-4V^~Zt", "ê"},
    {"I~Y~-19H~-5V^~Z", "Î"},
    {"i~Y~-19H~-8V^~Z", "î"},
    {"O~Y~-20H~-4V^~Z", "Ô"},
    {"o~Y~-25H~-4V^~Z", "ô"},
    {"U~Y~-24H~-3V^~Z", "Û"},
    {"u~Y~-23H~-3V^~Z", "û"},

    // grave accents
    {"A~Y~-26H~-8V`~Z", "À"},
    {"a~Y~-25H~-5V`~Z", "à"},
    {"E~Y~-23H~-9V`~Z", "È"},
    {"e~Y~-26H~-5V`~Z", "è"},
    {"I~Y~-19H~-5V`~Z", "Ì"},
    {"i~Y~-19H~-8V`~Z", "ì"},
    {"O~Y~-22H~-4V`~Z", "Ò"},
    {"o~Y~-26H~-4V`~Z", "ò"},
    {"U~Y~-24H~-3V`~Z", "Ù"},
    {"u~Y~-24H~-3V`~Z", "ù"},

    // umlaut
    {"A~Y~-26H~-8V¨~Z", "Ä"},
    {"a~Y~-25H~-5V¨~Z", "ä"},
    {"E~Y~-20H~-5V¨~Z", "Ë"},
    {"I~Y~-19H~-5V¨~Z", "Ï"},
    {"O~Y~-26H~-8V¨~Z", "Ö"},
    {"o~Y~-26H~-4V¨~Z", "ö"},
    {"U~Y~-25H~-8V¨~Z", "Ü"},
    {"u~Y~-24H~-3V¨~Z", "ü"},

    // dakuten katakana
    {"~Yウ~Z゛", "ヴ"},
    {"~Yカ~Z゛", "ガ"},
    {"~Yキ~Z゛", "ギ"},
    {"~Yク~Z゛", "グ"},
    {"~Yケ~Z゛", "ゲ"},
    {"~Yコ~Z゛", "ゴ"},
    {"~Yサ~Z゛", "ザ"},
    {"~Yシ~Z゛", "ジ"},
    {"~Yス~Z゛", "ズ"},
    {"~Yセ~Z゛", "ゼ"},
    {"~Yソ~Z゛", "ゾ"},
    {"~Yタ~Z゛", "ダ"},
    {"~Yチ~Z゛", "ヂ"},
    {"~Yツ~Z゛", "ヅ"},
    {"~Yテ~Z゛", "デ"},
    {"~Yト~Z゛", "ド"},
    {"~Yハ~Z゛", "バ"},
    {"~Yヒ~Z゛", "ビ"},
    {"~Yフ~Z゛", "ブ"},
    {"~Yヘ~Z゛", "ベ"},
    {"~Yホ~Z゛", "ボ"},
    // handakuten katakana
    {"~Yハ~Z゜", "パ"},
    {"~Yヒ~Z゜", "ピ"},
    {"~Yフ~Z゜", "プ"},
    {"~Yヘ~Z゜", "ペ"},
    {"~Yホ~Z゜", "ポ"},
    // dakuten hiragana
    {"~Yか~Z゛", "が"},
    {"~Yき~Z゛", "ぎ"},
    {"~Yく~Z゛", "ぐ"},
    {"~Yけ~Z゛", "げ"},
    {"~Yこ~Z゛", "ご"},
    {"~Yさ~Z゛", "ざ"},
    {"~Yし~Z゛", "じ"},
    {"~Yす~Z゛", "ず"},
    {"~Yせ~Z゛", "ぜ"},
    {"~Yそ~Z゛", "ぞ"},
    {"~Yた~Z゛", "だ"},
    {"~Yち~Z゛", "ぢ"},
    {"~Yつ~Z゛", "づ"},
    {"~Yて~Z゛", "で"},
    {"~Yと~Z゛", "ど"},
    {"~Yは~Z゛", "ば"},
    {"~Yひ~Z゛", "び"},
    {"~Yふ~Z゛", "ぶ"},
    {"~Yへ~Z゛", "べ"},
    {"~Yほ~Z゛", "ぼ"},
    // handakuten hiragana
    {"~Yは~Z゜", "ぱ"},
    {"~Yひ~Z゜", "ぴ"},
    {"~Yふ~Z゜", "ぷ"},
    {"~Yへ~Z゜", "ぺ"},
    {"~Yほ~Z゜", "ぽ"},
    // japanese punctuation
    {",~+8H", "、"},
    {"~+8H ", "　"},

    // playstation buttons
    // - face
    {"~Y~22L<~Z~Y~27L*~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_X>"},
    {"~Y~22L<~Z~Y~26L;~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_TRIANGLE>"},
    {"~Y~22L<~Z~Y~25L@~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_CIRCLE>"},
    {"~Y~22L<~Z~Y~24L#~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<PAD_SQUARE>"},
    // - dpad
    {"~Y~22L<PAD_PART_DPAD_L>~Z~3L~+17H~-13V<PAD_PART_DPAD_U>~Z~22L~+17H~+14V<PAD_PART_DPAD_D>~Z~"
     "22L~+32H<PAD_PART_DPAD_R>~Z~+56H",
     "<PAD_DPAD_UP>"},
    {"~Y~22L<PAD_PART_DPAD_L>~Z~3L~+17H~-13V<PAD_PART_DPAD_U>~Z~3L~+17H~+14V<PAD_PART_DPAD_D>~Z~"
     "22L~+32H<PAD_PART_DPAD_R>~Z~+56H",
     "<PAD_DPAD_DOWN>"},
    {"~Y~22L<PAD_PART_DPAD_L>~Z~22L~+17H~-13V<PAD_PART_DPAD_U>~Z~22L~+17H~+14V<PAD_PART_DPAD_D>~Z~"
     "22L~+32H<PAD_PART_DPAD_R>~Z~+56H",
     "<PAD_DPAD_ANY>"},
    // - shoulder
    {"~Y~22L~-2H~-12V<PAD_PART_SHOULDER_TOP_LEFT><PAD_PART_SHOULDER_TOP_RIGHT>~Z~22L~-2H~+17V<PAD_"
     "PART_SHOULDER_BOTTOM_LEFT><PAD_PART_SHOULDER_BOTTOM_RIGHT>~Z~1L~+4H~+3V<PAD_PART_L1_NAME>~Z~+"
     "38H",
     "<PAD_L1>"},
    {"~Y~22L~-2H~-12V<PAD_PART_SHOULDER_TOP_LEFT><PAD_PART_SHOULDER_TOP_RIGHT>~Z~22L~-2H~+17V<PAD_"
     "PART_SHOULDER_BOTTOM_LEFT><PAD_PART_SHOULDER_BOTTOM_RIGHT>~Z~1L~+6H~+3V<PAD_PART_R1_NAME>~Z~+"
     "38H",
     "<PAD_R1>"},
    {"~Y~22L~-2H~-6V<PAD_PART_TRIGGER_TOP_LEFT><PAD_PART_TRIGGER_TOP_RIGHT>~Z~22L~-2H~+16V<PAD_"
     "PART_TRIGGER_BOTTOM_LEFT><PAD_PART_TRIGGER_BOTTOM_RIGHT>~Z~1L~+5H~-2V<PAD_PART_R2_NAME>~Z~+"
     "38H",
     "<PAD_R2>"},
    {"~Y~22L~-2H~-6V<PAD_PART_TRIGGER_TOP_LEFT><PAD_PART_TRIGGER_TOP_RIGHT>~Z~22L~-2H~+16V<PAD_"
     "PART_TRIGGER_BOTTOM_LEFT><PAD_PART_TRIGGER_BOTTOM_RIGHT>~Z~1L~+5H~-2V<PAD_PART_L2_NAME>~Z~+"
     "38H",
     "<PAD_L2>"},
    // - analog
    {"~1L~+8H~Y<PAD_PART_STICK>~Z~6L~-16H<PAD_PART_STICK_LEFT>~Z~+16h~6L<PAD_PART_STICK_RIGHT>~Z~"
     "6L~-15V<PAD_PART_STICK_DOWN>~Z~+13V~6L<PAD_PART_STICK_UP>~Z~-10H~+9V~6L<PAD_PART_STICK_UP_"
     "LEFT>~Z~+10H~+9V~6L<PAD_PART_STICK_UP_RIGHT>~Z~-10H~-11V~6L<PAD_PART_STICK_DOWN_LEFT>~Z~+10H~"
     "-11V~6L<PAD_PART_STICK_DOWN_RIGHT>~Z~+32H",
     "<PAD_ANALOG_ANY>"},
    {"~Y~1L~+8H<PAD_PART_STICK>~Z~6L~-8H<PAD_PART_STICK_LEFT>~Z~+24H~6L<PAD_PART_STICK_RIGHT>~Z~+"
     "40H",
     "<PAD_ANALOG_LEFT_RIGHT>"},
    {"~Y~1L<PAD_PART_STICK>~Z~6L~-15V<PAD_PART_STICK_DOWN>~Z~+13V~6L<PAD_PART_STICK_UP>~Z~+26H",
     "<PAD_ANALOG_UP_DOWN>"},

    // icons
    {"~Y~6L<~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<ICON_MISSION_COMPLETE>"},
    {"~Y~3L<~Z~Y~1L>~Z~Y~23L[~Z~+26H", "<ICON_MISSION_TODO>"},

    // flags
    {"~Y~6L<FLAG_PART_VERT_STRIPE_LARGE>~Z~+15H~1L<FLAG_PART_VERT_STRIPE_LARGE>~Z~+30H~3L<FLAG_"
     "PART_VERT_STRIPE_LARGE>~Z~+45H",
     "<FLAG_ITALIAN>"},
    {"~Y~5L<FLAG_PART_FILL>~Z~3L<FLAG_PART_TOP_BOTTOM_STRIPE>~]~-1H~Y~5L<FLAG_PART_FILL>~Z~3L<FLAG_"
     "PART_TOP_BOTTOM_STRIPE>~Z~+26H",
     "<FLAG_SPAIN>"},
    {"~Y~39L~~~Z~3L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~5L<FLAG_PART_HORZ_STRIPE_BOTTOM>~]~-1H~Y~39L~~~"
     "Z~3L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~5L<FLAG_PART_HORZ_STRIPE_BOTTOM>~Z~+26H",
     "<FLAG_GERMAN>"},
    {"~Y~7L<FLAG_PART_VERT_STRIPE_LARGE>~Z~+15H~1L<FLAG_PART_VERT_STRIPE_LARGE>~Z~+30H~3L<FLAG_"
     "PART_VERT_STRIPE_LARGE>~Z~+47H",
     "<FLAG_FRANCE>"},
    {"~Y~1L<FLAG_PART_FILL>~Z~3L<FLAG_PART_UK_CROSS_LEFT>~Z~7L<FLAG_PART_UK_FILL_LEFT>~]~-1H~Y~1L<"
     "FLAG_PART_FILL>~Z~3L<FLAG_PART_UK_CROSS_RIGHT>~Z~7L<FLAG_PART_UK_FILL_RIGHT>~Z~+26H",
     "<FLAG_UK>"},
    {"~Y~1L<FLAG_PART_FILL>~Z~3L<FLAG_PART_USA_STRIPES_LEFT>~Z~7L<FLAG_PART_USA_STARS>~]~-1H~Y~1L<"
     "FLAG_PART_FILL>~Z~3L<FLAG_PART_USA_STRIPES_RIGHT>~Z~+26H",
     "<FLAG_USA>"},
    {"~Y~1L<FLAG_PART_FILL>~Z~39L<FLAG_PART_KOREA_TRIGRAMS_LEFT>~]~-1H~Y~1L<FLAG_PART_FILL>~Z~39L<"
     "FLAG_PART_KOREA_TRIGRAMS_RIGHT>~Z~-11H~7L<FLAG_PART_KOREA_CIRCLE_FILL>~Z~-11H~3L<FLAG_PART_"
     "KOREA_CIRCLE_TOP>~Z~+26H",
     "<FLAG_KOREA>"},
    {"~Y~1L<FLAG_PART_FILL>~]~-1H~Y~1L<FLAG_PART_FILL>~Z~-11H~3L<FLAG_PART_JAPAN_SUN>~Z~+26H",
     "<FLAG_JAPAN>"},

    // weird stuff
    // - descenders
    {"~+7Vp~-7V", "p"},
    {"~+7Vy~-7V", "y"},
    {"~+7Vg~-7V", "g"},
    {"~+7Vq~-7V", "q"},
    {"~+1Vj~-1V", "j"},

    {"\\\\",
     "~%"},  // this is 2 slashes, duplicated because we use an escape sequence when decompiling

    // - symbols and ligatures
    {"~-4H~-3V<SOMETHING>~+3V~-4H",
     "<SUPERSCRIPT_QUOTE>"},  // used for the 4<__> place in spanish.  the 5th uses the same
                              // character but looks different...?
    {"~Y~-6Hº~Z~+10H", "°"},

    // Color / Emphasis
    {"~[~1L", "<COLOR_WHITE>"},
    {"~[~32L", "<COLOR_DEFAULT>"}};

static std::vector<EncodeInfo> s_encode_info_jak2 = {
    {"ˇ", {0x10}},      // caron
    {"`", {0x11}},      // grave accent
    {"'", {0x12}},      // apostrophe
    {"^", {0x13}},      // circumflex
    {"<TIL>", {0x14}},  // tilde
    {"¨", {0x15}},      // umlaut
    {"º", {0x16}},      // numero/overring
    {"¡", {0x17}},      // inverted exclamation mark
    {"¿", {0x18}},      // inverted question mark
    {"<SOMETHING>", {0x19}},
    {"ç", {0x1d}},  // c-cedilla
    {"Ç", {0x1e}},  // c-cedilla
    {"ß", {0x1f}},  // eszett

    {"œ", {0x5e}},  // ligature o+e

    {"<FLAG_PART_HORZ_STRIPE_MIDDLE>", {0x7f}},
    {"<FLAG_PART_HORZ_STRIPE_BOTTOM>", {0x80}},
    {"<FLAG_PART_VERT_STRIPE_LARGE>", {0x81}},
    {"<FLAG_PART_VERT_STRIPE_RIGHT>", {0x82}},
    {"<FLAG_PART_VERT_STRIPE_LEFT>", {0x83}},
    {"<FLAG_PART_VERT_STRIPE_MIDDLE>", {0x84}},
    {"<FLAG_PART_FILL>", {0x85}},
    {"<FLAG_PART_JAPAN_SUN>", {0x86}},
    {"<FLAG_PART_KOREA_TRIGRAMS_LEFT>", {0x87}},
    {"<FLAG_PART_KOREA_TRIGRAMS_RIGHT>", {0x88}},
    {"<FLAG_PART_KOREA_CIRCLE_TOP>", {0x89}},
    {"<FLAG_PART_KOREA_CIRCLE_FILL>", {0x8a}},
    {"<FLAG_PART_TOP_BOTTOM_STRIPE>", {0x8b}},
    {"<FLAG_PART_UK_CROSS_LEFT>", {0x8c}},
    {"<FLAG_PART_UK_CROSS_RIGHT>", {0x8d}},
    {"<FLAG_PART_UK_FILL_LEFT>", {0x8e}},
    {"<FLAG_PART_UK_FILL_RIGHT>", {0x8f}},
    {"<FLAG_PART_USA_STRIPES_RIGHT>", {0x90}},
    {"<PAD_PART_STICK>", {0x91}},
    {"<PAD_PART_SELECT>", {0x92}},
    {"<PAD_PART_TRIGGER_BACK>", {0x93}},
    {"<PAD_PART_R1_NAME>", {0x94}},
    {"<PAD_PART_L1_NAME>", {0x95}},
    {"<PAD_PART_R2_NAME>", {0x96}},
    {"<PAD_PART_L2_NAME>", {0x97}},
    {"<PAD_PART_STICK_UP>", {0x98}},
    {"<PAD_PART_STICK_UP_RIGHT>", {0x99}},
    {"<FLAG_PART_USA_STRIPES_LEFT>", {0x9a}},
    {"<FLAG_PART_USA_STARS>", {0x9b}},
    {"<PAD_PART_STICK_DOWN>", {0x9c}},
    {"<PAD_PART_STICK_DOWN_LEFT>", {0x9d}},
    {"<PAD_PART_STICK_LEFT>", {0x9e}},
    {"<PAD_PART_STICK_UP_LEFT>", {0x9f}},
    {"<PAD_PART_DPAD_D>", {0xa0}},
    {"<PAD_PART_DPAD_L>", {0xa1}},
    {"<PAD_PART_DPAD_U>", {0xa2}},
    {"<PAD_PART_DPAD_R>", {0xa3}},
    {"<PAD_PART_STICK_RIGHT>", {0xa4}},
    {"<PAD_PART_STICK_DOWN_RIGHT>", {0xa5}},
    {"<PAD_PART_SHOULDER_TOP_LEFT>", {0xa6}},
    {"<PAD_PART_SHOULDER_TOP_RIGHT>", {0xa7}},
    {"<PAD_PART_TRIGGER_TOP_LEFT>", {0xa8}},
    {"<PAD_PART_TRIGGER_TOP_RIGHT>", {0xa9}},
    {"<PAD_PART_TRIGGER_SHIM1>", {0xaa}},
    {"<PAD_PART_TRIGGER_SHIM2>", {0xab}},
    {"<PAD_PART_SHOULDER_SHIM2>", {0xac}},

    {"<PAD_PART_SHOULDER_BOTTOM_LEFT>", {0xb0}},
    {"<PAD_PART_SHOULDER_BOTTOM_RIGHT>", {0xb1}},
    {"<PAD_PART_TRIGGER_BOTTOM_LEFT>", {0xb2}},
    {"<PAD_PART_TRIGGER_BOTTOM_RIGHT>", {0xb3}},
    // {"入", {1, 0x00}},
    // {"年", {1, 0x01}},
    // punctuation
    {"・", {1, 0x10}},
    {"゛", {1, 0x11}},
    {"゜", {1, 0x12}},
    {"ー", {1, 0x13}},
    {"『", {1, 0x14}},
    {"』", {1, 0x15}},
    // hiragana
    {"ぁ", {1, 0x16}},  // -a
    {"あ", {1, 0x17}},  // a
    {"ぃ", {1, 0x18}},  // -i
    {"い", {1, 0x19}},  // i
    {"ぅ", {1, 0x1a}},  // -u
    {"う", {1, 0x1b}},  // u
    {"ぇ", {1, 0x1c}},  // -e
    {"え", {1, 0x1d}},  // e
    {"ぉ", {1, 0x1e}},  // -o
    {"お", {1, 0x1f}},  // o
    {"か", {1, 0x20}},  // ka
    {"き", {1, 0x21}},  // ki
    {"く", {1, 0x22}},  // ku
    {"け", {1, 0x23}},  // ke
    {"こ", {1, 0x24}},  // ko
    {"さ", {1, 0x25}},  // sa
    {"し", {1, 0x26}},  // shi
    {"す", {1, 0x27}},  // su
    {"せ", {1, 0x28}},  // se
    {"そ", {1, 0x29}},  // so
    {"た", {1, 0x2a}},  // ta
    {"ち", {1, 0x2b}},  // chi
    {"っ", {1, 0x2c}},  // sokuon
    {"つ", {1, 0x2d}},  // tsu
    {"て", {1, 0x2e}},  // te
    {"と", {1, 0x2f}},  // to
    {"な", {1, 0x30}},  // na
    {"に", {1, 0x31}},  // ni
    {"ぬ", {1, 0x32}},  // nu
    {"ね", {1, 0x33}},  // ne
    {"の", {1, 0x34}},  // no
    {"は", {1, 0x35}},  // ha
    {"ひ", {1, 0x36}},  // hi
    {"ふ", {1, 0x37}},  // fu
    {"へ", {1, 0x38}},  // he
    {"ほ", {1, 0x39}},  // ho
    {"ま", {1, 0x3a}},  // ma
    {"み", {1, 0x3b}},  // mi
    {"む", {1, 0x3c}},  // mu
    {"め", {1, 0x3d}},  // me
    {"も", {1, 0x3e}},  // mo
    {"ゃ", {1, 0x3f}},  // youon ya
    {"や", {1, 0x40}},  // ya
    {"ゅ", {1, 0x41}},  // youon yu
    {"ゆ", {1, 0x42}},  // yu
    {"ょ", {1, 0x43}},  // youon yo
    {"よ", {1, 0x44}},  // yo
    {"ら", {1, 0x45}},  // ra
    {"り", {1, 0x46}},  // ri
    {"る", {1, 0x47}},  // ru
    {"れ", {1, 0x48}},  // re
    {"ろ", {1, 0x49}},  // ro
    {"ゎ", {1, 0x4a}},  // -wa
    {"わ", {1, 0x4b}},  // wa
    {"を", {1, 0x4c}},  // wo
    {"ん", {1, 0x4d}},  // -n
    // katakana
    {"ァ", {1, 0x4e}},  // -a
    {"ア", {1, 0x4f}},  // a
    {"ィ", {1, 0x50}},  // -i
    {"イ", {1, 0x51}},  // i
    {"ゥ", {1, 0x52}},  // -u
    {"ウ", {1, 0x53}},  // u
    {"ェ", {1, 0x54}},  // -e
    {"エ", {1, 0x55}},  // e
    {"ォ", {1, 0x56}},  // -o
    {"オ", {1, 0x57}},  // o
    {"カ", {1, 0x58}},  // ka
    {"キ", {1, 0x59}},  // ki
    {"ク", {1, 0x5a}},  // ku
    {"ケ", {1, 0x5b}},  // ke
    {"コ", {1, 0x5c}},  // ko
    {"サ", {1, 0x5d}},  // sa
    {"シ", {1, 0x5e}},  // shi
    {"ス", {1, 0x5f}},  // su
    {"セ", {1, 0x60}},  // se
    {"ソ", {1, 0x61}},  // so
    {"タ", {1, 0x62}},  // ta
    {"チ", {1, 0x63}},  // chi
    {"ッ", {1, 0x64}},  // sokuon
    {"ツ", {1, 0x65}},  // tsu
    {"テ", {1, 0x66}},  // te
    {"ト", {1, 0x67}},  // to
    {"ナ", {1, 0x68}},  // na
    {"ニ", {1, 0x69}},  // ni
    {"ヌ", {1, 0x6a}},  // nu
    {"ネ", {1, 0x6b}},  // ne
    {"ノ", {1, 0x6c}},  // no
    {"ハ", {1, 0x6d}},  // ha
    {"ヒ", {1, 0x6e}},  // hi
    {"フ", {1, 0x6f}},  // fu
    {"ヘ", {1, 0x70}},  // he
    {"ホ", {1, 0x71}},  // ho
    {"マ", {1, 0x72}},  // ma
    {"ミ", {1, 0x73}},  // mi
    {"ム", {1, 0x74}},  // mu
    {"メ", {1, 0x75}},  // me
    {"モ", {1, 0x76}},  // mo
    {"ャ", {1, 0x77}},  // youon ya
    {"ヤ", {1, 0x78}},  // ya
    {"ュ", {1, 0x79}},  // youon yu
    {"ユ", {1, 0x7a}},  // yu
    {"ョ", {1, 0x7b}},  // youon yo
    {"ヨ", {1, 0x7c}},  // yo
    {"ラ", {1, 0x7d}},  // ra
    {"リ", {1, 0x7e}},  // ri
    {"ル", {1, 0x7f}},  // ru
    {"レ", {1, 0x80}},  // re
    {"ロ", {1, 0x81}},  // ro
    {"ヮ", {1, 0x82}},  // -wa
    {"ワ", {1, 0x83}},  // wa
    {"ヲ", {1, 0x84}},  // wo
    {"ン", {1, 0x85}},  // -n

    {"位", {1, 0x8c}},
    {"遺", {1, 0x8d}},
    {"院", {1, 0x8e}},
    {"映", {1, 0x8f}},
    {"衛", {1, 0x90}},
    {"応", {1, 0x91}},
    {"下", {1, 0x92}},
    {"画", {1, 0x93}},
    {"解", {1, 0x94}},
    {"開", {1, 0x95}},
    {"外", {1, 0x96}},
    {"害", {1, 0x97}},
    {"蓋", {1, 0x98}},
    {"完", {1, 0x99}},
    {"換", {1, 0x9a}},
    {"監", {1, 0x9b}},
    {"間", {1, 0x9c}},
    {"器", {1, 0x9d}},
    {"記", {1, 0x9e}},
    {"逆", {1, 0x9f}},
    {"救", {1, 0xa0}},
    {"金", {1, 0xa1}},
    {"空", {1, 0xa2}},
    {"掘", {1, 0xa3}},
    {"警", {1, 0xa4}},
    {"迎", {1, 0xa5}},
    {"撃", {1, 0xa6}},
    {"建", {1, 0xa7}},
    {"源", {1, 0xa8}},
    {"現", {1, 0xa9}},
    {"言", {1, 0xaa}},
    {"限", {1, 0xab}},
    {"個", {1, 0xac}},
    {"庫", {1, 0xad}},
    {"後", {1, 0xae}},
    {"語", {1, 0xaf}},
    {"護", {1, 0xb0}},
    {"交", {1, 0xb1}},
    {"功", {1, 0xb2}},
    {"向", {1, 0xb3}},
    {"工", {1, 0xb4}},
    {"攻", {1, 0xb5}},
    {"溝", {1, 0xb6}},
    {"行", {1, 0xb7}},
    {"鉱", {1, 0xb8}},
    {"降", {1, 0xb9}},
    {"合", {1, 0xba}},
    {"告", {1, 0xbb}},
    {"獄", {1, 0xbc}},
    {"彩", {1, 0xbd}},
    {"作", {1, 0xbe}},
    {"山", {1, 0xbf}},
    {"使", {1, 0xc0}},
    {"始", {1, 0xc1}},
    {"試", {1, 0xc2}},
    {"字", {1, 0xc3}},
    {"寺", {1, 0xc4}},
    {"時", {1, 0xc5}},
    {"示", {1, 0xc6}},
    {"自", {1, 0xc7}},
    {"式", {1, 0xc8}},
    {"矢", {1, 0xc9}},
    {"射", {1, 0xca}},
    {"者", {1, 0xcb}},
    {"守", {1, 0xcc}},
    {"手", {1, 0xcd}},
    {"終", {1, 0xce}},
    {"週", {1, 0xcf}},
    {"出", {1, 0xd0}},
    {"所", {1, 0xd1}},
    {"書", {1, 0xd2}},
    {"勝", {1, 0xd3}},
    {"章", {1, 0xd4}},
    {"上", {1, 0xd5}},
    {"乗", {1, 0xd6}},
    {"場", {1, 0xd7}},
    {"森", {1, 0xd8}},
    {"進", {1, 0xd9}},
    {"人", {1, 0xda}},
    {"水", {1, 0xdb}},
    {"数", {1, 0xdc}},
    {"制", {1, 0xdd}},
    {"性", {1, 0xde}},
    {"成", {1, 0xdf}},
    {"聖", {1, 0xe0}},
    {"石", {1, 0xe1}},
    {"跡", {1, 0xe2}},
    {"先", {1, 0xe3}},
    {"戦", {1, 0xe4}},
    {"船", {1, 0xe5}},
    {"選", {1, 0xe6}},
    {"走", {1, 0xe7}},
    {"送", {1, 0xe8}},
    {"像", {1, 0xe9}},
    {"造", {1, 0xea}},
    {"続", {1, 0xeb}},
    {"対", {1, 0xec}},
    {"袋", {1, 0xed}},
    {"台", {1, 0xee}},
    {"弾", {1, 0xef}},
    {"地", {1, 0xf0}},
    {"中", {1, 0xf1}},
    {"敵", {1, 0xf2}},
    {"転", {1, 0xf3}},
    {"電", {1, 0xf4}},
    {"塔", {1, 0xf5}},
    {"頭", {1, 0xf6}},
    {"動", {1, 0xf7}},
    {"内", {1, 0xf8}},
    {"日", {1, 0xf9}},
    {"入", {1, 0xfa}},
    {"年", {1, 0xfb}},
    {"能", {1, 0xfc}},
    {"廃", {1, 0xfd}},
    {"排", {1, 0xfe}},
    {"敗", {1, 0xff}},

    {"発", {2, 0x10}},
    {"反", {2, 0x11}},
    {"必", {2, 0x12}},
    {"表", {2, 0x13}},
    {"武", {2, 0x14}},
    {"壁", {2, 0x15}},
    {"墓", {2, 0x16}},
    {"放", {2, 0x17}},
    {"方", {2, 0x18}},
    {"砲", {2, 0x19}},
    {"妨", {2, 0x1a}},
    {"北", {2, 0x1b}},
    {"本", {2, 0x1c}},
    {"幕", {2, 0x1d}},
    {"無", {2, 0x1e}},
    {"迷", {2, 0x1f}},
    {"面", {2, 0x20}},
    {"戻", {2, 0x21}},
    {"紋", {2, 0x22}},
    {"薬", {2, 0x23}},
    {"輸", {2, 0x24}},
    {"勇", {2, 0x25}},
    {"友", {2, 0x26}},
    {"遊", {2, 0x27}},
    {"容", {2, 0x28}},
    {"要", {2, 0x29}},
    {"利", {2, 0x2a}},
    {"了", {2, 0x2b}},
    {"量", {2, 0x2c}},
    {"力", {2, 0x2d}},
    {"練", {2, 0x2e}},
    {"連", {2, 0x2f}},
    {"録", {2, 0x30}},
    {"話", {2, 0x31}},
    {"墟", {2, 0x32}},
    {"脱", {2, 0x33}},
    // {"成", {2, 0x34}},
    {"旗", {2, 0x35}},
    {"破", {2, 0x36}},
    {"壊", {2, 0x37}},
    {"全", {2, 0x38}},
    {"滅", {2, 0x39}},
    {"機", {2, 0x3a}},
    {"仲", {2, 0x3b}},
    {"渓", {2, 0x3c}},
    {"谷", {2, 0x3d}},
    {"優", {2, 0x3e}},
    {"探", {2, 0x3f}},
    {"部", {2, 0x40}},
    {"索", {2, 0x41}},
    // {"乗", {2, 0x42}},
    {"前", {2, 0x43}},
    {"右", {2, 0x44}},
    {"左", {2, 0x45}},
    {"会", {2, 0x46}},
    {"高", {2, 0x47}},
    {"低", {2, 0x48}},
    {"押", {2, 0x49}},
    {"切", {2, 0x4a}},
    {"替", {2, 0x4b}},
    // {"対", {2, 0x4c}},
    {"秒", {2, 0x4d}},
    {"箱", {2, 0x4e}},
    {"泳", {2, 0x4f}},
    {"～", {2, 0x50}},

    {"闇", {2, 0x56}},
    {"以", {2, 0x57}},
    {"屋", {2, 0x58}},
    {"俺", {2, 0x59}},
    {"化", {2, 0x5a}},
    {"界", {2, 0x5b}},
    {"感", {2, 0x5c}},
    {"気", {2, 0x5d}},
    {"却", {2, 0x5e}},
    {"曲", {2, 0x5f}},
    {"継", {2, 0x60}},
    {"権", {2, 0x61}},
    {"見", {2, 0x62}},
    {"古", {2, 0x63}},
    {"好", {2, 0x64}},
    // {"高", {2, 0x65}},
    {"才", {2, 0x66}},
    {"士", {2, 0x67}},
    {"子", {2, 0x68}},
    {"次", {2, 0x69}},
    {"主", {2, 0x6a}},
    {"種", {2, 0x6b}},
    {"讐", {2, 0x6c}},
    {"女", {2, 0x6d}},
    {"小", {2, 0x6e}},
    {"焼", {2, 0x6f}},
    {"証", {2, 0x70}},
    {"神", {2, 0x71}},
    {"身", {2, 0x72}},
    {"寸", {2, 0x73}},
    {"世", {2, 0x74}},
    {"想", {2, 0x75}},
    {"退", {2, 0x76}},
    {"第", {2, 0x77}},
    {"着", {2, 0x78}},
    {"天", {2, 0x79}},
    {"倒", {2, 0x7a}},
    {"到", {2, 0x7b}},
    {"突", {2, 0x7c}},
    {"爆", {2, 0x7d}},
    {"番", {2, 0x7e}},
    {"負", {2, 0x7f}},
    {"復", {2, 0x80}},
    {"物", {2, 0x81}},
    {"眠", {2, 0x82}},
    {"予", {2, 0x83}},
    {"用", {2, 0x84}},
    {"落", {2, 0x85}},
    {"緑", {2, 0x86}},

    {"封", {2, 0x88}},
    {"印", {2, 0x89}},
    {"扉", {2, 0x8a}},
    {"最", {2, 0x8b}},
    {"刻", {2, 0x8c}},
    {"足", {2, 0x8d}},

    //{"<H300>", {3, 0x00}},
    //{"<H301>", {3, 0x01}},
    //{"<H302>", {3, 0x02}},
    //{"<H303>", {3, 0x03}},
    //{"<H304>", {3, 0x04}},
    //{"<H305>", {3, 0x05}},
    {"<H186>", {1, 0x86}},
    {"<H187>", {1, 0x87}},
    {"<H188>", {1, 0x88}},
    {"<H189>", {1, 0x89}},
    {"<H18a>", {1, 0x8a}},
    {"<H306>", {3, 0x06}},
    {"<H307>", {3, 0x07}},
    {"<H308>", {3, 0x08}},
    {"<H309>", {3, 0x09}},
    {"<H30a>", {3, 0x0a}},
    {"<H30b>", {3, 0x0b}},
    {"<H30c>", {3, 0x0c}},
    {"<H30d>", {3, 0x0d}},
    {"<H30e>", {3, 0x0e}},
    {"<H30f>", {3, 0x0f}},
    {"<H310>", {3, 0x10}},
    {"<H311>", {3, 0x11}},
    {"<H312>", {3, 0x12}},
    {"<H313>", {3, 0x13}},
    {"<H314>", {3, 0x14}},
    {"<H315>", {3, 0x15}},
    {"<H316>", {3, 0x16}},
    {"<H317>", {3, 0x17}},
    {"<H318>", {3, 0x18}},
    {"<H319>", {3, 0x19}},
    {"<H31a>", {3, 0x1a}},
    {"<H31b>", {3, 0x1b}},
    {"<H31c>", {3, 0x1c}},
    {"<H31d>", {3, 0x1d}},
    {"<H31e>", {3, 0x1e}},
    {"<H31f>", {3, 0x1f}},
    {"<H320>", {3, 0x20}},
    {"<H321>", {3, 0x21}},
    {"<H322>", {3, 0x22}},
    {"<H323>", {3, 0x23}},
    {"<H324>", {3, 0x24}},
    {"<H325>", {3, 0x25}},
    {"<H326>", {3, 0x26}},
    {"<H327>", {3, 0x27}},
    {"<H328>", {3, 0x28}},
    {"<H329>", {3, 0x29}},
    {"<H32a>", {3, 0x2a}},
    {"<H32b>", {3, 0x2b}},
    {"<H32c>", {3, 0x2c}},
    {"<H32d>", {3, 0x2d}},
    {"<H32e>", {3, 0x2e}},
    {"<H32f>", {3, 0x2f}},
    {"<H330>", {3, 0x30}},
    {"<H331>", {3, 0x31}},
    {"<H332>", {3, 0x32}},
    {"<H333>", {3, 0x33}},
    {"<H334>", {3, 0x34}},
    {"<H335>", {3, 0x35}},
    {"<H336>", {3, 0x36}},
    {"<H337>", {3, 0x37}},
    {"<H338>", {3, 0x38}},
    {"<H339>", {3, 0x39}},
    {"<H33a>", {3, 0x3a}},
    {"<H33b>", {3, 0x3b}},
    {"<H33c>", {3, 0x3c}},
    {"<H33d>", {3, 0x3d}},
    {"<H33e>", {3, 0x3e}},
    {"<H33f>", {3, 0x3f}},
    {"<H340>", {3, 0x40}},
    {"<H341>", {3, 0x41}},
    {"<H342>", {3, 0x42}},
    {"<H343>", {3, 0x43}},
    {"<H344>", {3, 0x44}},
    {"<H345>", {3, 0x45}},
    {"<H346>", {3, 0x46}},
    {"<H347>", {3, 0x47}},
    {"<H348>", {3, 0x48}},
    {"<H349>", {3, 0x49}},
    {"<H34a>", {3, 0x4a}},
    {"<H34b>", {3, 0x4b}},
    {"<H34c>", {3, 0x4c}},
    {"<H34d>", {3, 0x4d}},
    {"<H34e>", {3, 0x4e}},
    {"<H34f>", {3, 0x4f}},
    {"<H350>", {3, 0x50}},
    {"<H351>", {3, 0x51}},
    {"<H352>", {3, 0x52}},
    {"<H353>", {3, 0x53}},
    {"<H354>", {3, 0x54}},
    {"<H355>", {3, 0x55}},
    {"<H356>", {3, 0x56}},
    {"<H357>", {3, 0x57}},
    {"<H358>", {3, 0x58}},
    {"<H359>", {3, 0x59}},
    {"<H35a>", {3, 0x5a}},
    {"<H35b>", {3, 0x5b}},
    {"<H35c>", {3, 0x5c}},
    {"<H35d>", {3, 0x5d}},
    {"<H35e>", {3, 0x5e}},
    {"<H35f>", {3, 0x5f}},
    {"<H360>", {3, 0x60}},
    {"<H361>", {3, 0x61}},
    {"<H362>", {3, 0x62}},
    {"<H363>", {3, 0x63}},
    {"<H364>", {3, 0x64}},
    {"<H365>", {3, 0x65}},
    {"<H366>", {3, 0x66}},
    {"<H367>", {3, 0x67}},
    {"<H368>", {3, 0x68}},
    {"<H369>", {3, 0x69}},
    {"<H36a>", {3, 0x6a}},
    {"<H36b>", {3, 0x6b}},
    {"<H36c>", {3, 0x6c}},
    {"<H36d>", {3, 0x6d}},
    {"<H36e>", {3, 0x6e}},
    {"<H36f>", {3, 0x6f}},
    {"<H370>", {3, 0x70}},
    {"<H371>", {3, 0x71}},
    {"<H372>", {3, 0x72}},
    {"<H373>", {3, 0x73}},
    {"<H374>", {3, 0x74}},
    {"<H375>", {3, 0x75}},
    {"<H376>", {3, 0x76}},
    {"<H377>", {3, 0x77}},
    {"<H378>", {3, 0x78}},
    {"<H379>", {3, 0x79}},
    {"<H37a>", {3, 0x7a}},
    {"<H37b>", {3, 0x7b}},
    {"<H37c>", {3, 0x7c}},
    {"<H37d>", {3, 0x7d}},
    {"<H37e>", {3, 0x7e}},
    {"<H37f>", {3, 0x7f}},
    {"<H380>", {3, 0x80}},
    {"<H381>", {3, 0x81}},
    {"<H382>", {3, 0x82}},
    {"<H383>", {3, 0x83}},
    {"<H384>", {3, 0x84}},
    {"<H385>", {3, 0x85}},
    {"<H386>", {3, 0x86}},
    {"<H387>", {3, 0x87}},
    {"<H388>", {3, 0x88}},
    {"<H389>", {3, 0x89}},
    {"<H38a>", {3, 0x8a}},
    {"<H38b>", {3, 0x8b}},
    {"<H38c>", {3, 0x8c}},
    {"<H38d>", {3, 0x8d}},
    {"<H38e>", {3, 0x8e}},
    {"<H38f>", {3, 0x8f}},
    {"<H390>", {3, 0x90}},
    {"<H391>", {3, 0x91}},
    {"<H392>", {3, 0x92}},
    {"<H393>", {3, 0x93}},
    {"<H394>", {3, 0x94}},
    {"<H395>", {3, 0x95}},
    {"<H396>", {3, 0x96}},
    {"<H397>", {3, 0x97}},
    {"<H398>", {3, 0x98}},
    {"<H399>", {3, 0x99}},
    {"<H39a>", {3, 0x9a}},
    {"<H39b>", {3, 0x9b}},
    {"<H39c>", {3, 0x9c}},
    {"<H39d>", {3, 0x9d}},
    {"<H39e>", {3, 0x9e}},
    {"<H39f>", {3, 0x9f}},
    {"<H3a0>", {3, 0xa0}},
    {"<H3a1>", {3, 0xa1}},
    {"<H3a2>", {3, 0xa2}},
    {"<H3a3>", {3, 0xa3}},
    {"<H3a4>", {3, 0xa4}},
    {"<H3a5>", {3, 0xa5}},
    {"<H3a6>", {3, 0xa6}},
    {"<H3a7>", {3, 0xa7}},
    {"<H3a8>", {3, 0xa8}},
    {"<H3a9>", {3, 0xa9}},
    {"<H3aa>", {3, 0xaa}},
    {"<H3ab>", {3, 0xab}},
    {"<H3ac>", {3, 0xac}},
    {"<H3ad>", {3, 0xad}},
    {"<H3ae>", {3, 0xae}},
    {"<H3af>", {3, 0xaf}},
    {"<H3b0>", {3, 0xb0}},
    {"<H3b1>", {3, 0xb1}},
    {"<H3b2>", {3, 0xb2}},
    {"<H3b3>", {3, 0xb3}},
    {"<H3b4>", {3, 0xb4}},
    {"<H3b5>", {3, 0xb5}},
    {"<H3b6>", {3, 0xb6}},
    {"<H3b7>", {3, 0xb7}},
    {"<H3b8>", {3, 0xb8}},
    {"<H3b9>", {3, 0xb9}},
    {"<H3ba>", {3, 0xba}},
    {"<H3bb>", {3, 0xbb}},
    {"<H3bc>", {3, 0xbc}},
    {"<H3bd>", {3, 0xbd}},
    {"<H3be>", {3, 0xbe}},
    {"<H3bf>", {3, 0xbf}},
    {"<H3c0>", {3, 0xc0}},
    {"<H3c1>", {3, 0xc1}},
    {"<H3c2>", {3, 0xc2}},
    {"<H3c3>", {3, 0xc3}},
    {"<H3c4>", {3, 0xc4}},
    {"<H3c5>", {3, 0xc5}},
    {"<H3c6>", {3, 0xc6}},
    {"<H3c7>", {3, 0xc7}},
    {"<H3c8>", {3, 0xc8}},
    {"<H3c9>", {3, 0xc9}},
    {"<H3ca>", {3, 0xca}},
    {"<H3cb>", {3, 0xcb}},
    {"<H3cc>", {3, 0xcc}},
    {"<H3cd>", {3, 0xcd}},
    {"<H3ce>", {3, 0xce}},
    {"<H3cf>", {3, 0xcf}},
    {"<H3d0>", {3, 0xd0}},
    {"<H3d1>", {3, 0xd1}},
    {"<H3d2>", {3, 0xd2}},
    {"<H3d3>", {3, 0xd3}},
    {"<H3d4>", {3, 0xd4}},
    {"<H3d5>", {3, 0xd5}},
    {"<H3d6>", {3, 0xd6}},
    {"<H3d7>", {3, 0xd7}},
    {"<H3d8>", {3, 0xd8}},
    {"<H3d9>", {3, 0xd9}},
    {"<H3da>", {3, 0xda}},
    {"<H3db>", {3, 0xdb}},
    {"<H3dc>", {3, 0xdc}},
    {"<H3dd>", {3, 0xdd}},
    {"<H3de>", {3, 0xde}},
    {"<H3df>", {3, 0xdf}},
    {"<H3e0>", {3, 0xe0}},
    {"<H3e1>", {3, 0xe1}},
    {"<H3e2>", {3, 0xe2}},
    {"<H3e3>", {3, 0xe3}},
    {"<H3e4>", {3, 0xe4}},
    {"<H3e5>", {3, 0xe5}},
    {"<H3e6>", {3, 0xe6}},
    {"<H3e7>", {3, 0xe7}},
    {"<H3e8>", {3, 0xe8}},
    {"<H3e9>", {3, 0xe9}},
    {"<H3ea>", {3, 0xea}},
    {"<H3eb>", {3, 0xeb}},
    {"<H3ec>", {3, 0xec}},
    {"<H3ed>", {3, 0xed}},
    {"<H3ee>", {3, 0xee}},
    {"<H3ef>", {3, 0xef}},
    {"<H3f0>", {3, 0xf0}},
    {"<H3f1>", {3, 0xf1}},
    {"<H3f2>", {3, 0xf2}},
    {"<H3f3>", {3, 0xf3}},
    {"<H3f4>", {3, 0xf4}},
    {"<H3f5>", {3, 0xf5}},
    {"<H3f6>", {3, 0xf6}},
    {"<H3f7>", {3, 0xf7}},
    {"<H3f8>", {3, 0xf8}},
    {"<H3f9>", {3, 0xf9}},
    {"<H3fa>", {3, 0xfa}},
    {"<H3fb>", {3, 0xfb}},
    {"<H3fc>", {3, 0xfc}},
    {"<H3fd>", {3, 0xfd}},
    {"<H3fe>", {3, 0xfe}},
    {"<H3ff>", {3, 0xff}},
};

GameTextFontBank g_font_bank_jak2(GameTextVersion::JAK2,
                                  &s_encode_info_jak2,
                                  &s_replace_info_jak2,
                                  &s_passthrus_jak2);

/*!
 * ========================
 * GAME TEXT FONT BANK LIST
 * ========================
 * The list of available font banks and a couple of helper functions.
 */

std::map<GameTextVersion, GameTextFontBank*> g_font_banks = {
    {GameTextVersion::JAK1_V1, &g_font_bank_jak1_v1},
    {GameTextVersion::JAK1_V2, &g_font_bank_jak1_v2},
    {GameTextVersion::JAK2, &g_font_bank_jak2}};

const GameTextFontBank* get_font_bank(GameTextVersion version) {
  return g_font_banks.at(version);
}

const GameTextFontBank* get_font_bank(const std::string& name) {
  if (auto it = sTextVerEnumMap.find(name); it == sTextVerEnumMap.end()) {
    throw std::runtime_error(fmt::format("unknown text version {}", name));
  } else {
    return get_font_bank(it->second);
  }
}

bool font_bank_exists(GameTextVersion version) {
  return g_font_banks.find(version) != g_font_banks.cend();
}
