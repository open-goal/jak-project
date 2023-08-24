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
 * - Jak 2 - NTSC - v1
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

    {"<K300>", {3, 0x00}},
    {"<K301>", {3, 0x01}},
    {"<K302>", {3, 0x02}},
    {"<K303>", {3, 0x03}},
    {"<K304>", {3, 0x04}},
    {"<K305>", {3, 0x05}},
    {"<K306>", {3, 0x06}},
    {"<K307>", {3, 0x07}},
    {"<K308>", {3, 0x08}},
    {"<K309>", {3, 0x09}},
    {"<K30a>", {3, 0x0a}},
    {"<K30b>", {3, 0x0b}},
    {"<K30c>", {3, 0x0c}},
    {"<K30d>", {3, 0x0d}},
    {"<K30e>", {3, 0x0e}},
    {"<K30f>", {3, 0x0f}},
    {"<K310>", {3, 0x10}},
    {"<K311>", {3, 0x11}},
    {"<K312>", {3, 0x12}},
    {"<K313>", {3, 0x13}},
    {"<K314>", {3, 0x14}},
    {"<K315>", {3, 0x15}},
    {"<K316>", {3, 0x16}},
    {"<K317>", {3, 0x17}},
    {"<K318>", {3, 0x18}},
    {"<K319>", {3, 0x19}},
    {"<K31a>", {3, 0x1a}},
    {"<K31b>", {3, 0x1b}},
    {"<K31c>", {3, 0x1c}},
    {"<K31d>", {3, 0x1d}},
    {"<K31e>", {3, 0x1e}},
    {"<K31f>", {3, 0x1f}},
    {"<K320>", {3, 0x20}},
    {"<K321>", {3, 0x21}},
    {"<K322>", {3, 0x22}},
    {"<K323>", {3, 0x23}},
    {"<K324>", {3, 0x24}},
    {"<K325>", {3, 0x25}},
    {"<K326>", {3, 0x26}},
    {"<K327>", {3, 0x27}},
    {"<K328>", {3, 0x28}},
    {"<K329>", {3, 0x29}},
    {"<K32a>", {3, 0x2a}},
    {"<K32b>", {3, 0x2b}},
    {"<K32c>", {3, 0x2c}},
    {"<K32d>", {3, 0x2d}},
    {"<K32e>", {3, 0x2e}},
    {"<K32f>", {3, 0x2f}},
    {"<K330>", {3, 0x30}},
    {"<K331>", {3, 0x31}},
    {"<K332>", {3, 0x32}},
    {"<K333>", {3, 0x33}},
    {"<K334>", {3, 0x34}},
    {"<K335>", {3, 0x35}},
    {"<K336>", {3, 0x36}},
    {"<K337>", {3, 0x37}},
    {"<K338>", {3, 0x38}},
    {"<K339>", {3, 0x39}},
    {"<K33a>", {3, 0x3a}},
    {"<K33b>", {3, 0x3b}},
    {"<K33c>", {3, 0x3c}},
    {"<K33d>", {3, 0x3d}},
    {"<K33e>", {3, 0x3e}},
    {"<K33f>", {3, 0x3f}},
    {"<K340>", {3, 0x40}},
    {"<K341>", {3, 0x41}},
    {"<K342>", {3, 0x42}},
    {"<K343>", {3, 0x43}},
    {"<K344>", {3, 0x44}},
    {"<K345>", {3, 0x45}},
    {"<K346>", {3, 0x46}},
    {"<K347>", {3, 0x47}},
    {"<K348>", {3, 0x48}},
    {"<K349>", {3, 0x49}},
    {"<K34a>", {3, 0x4a}},
    {"<K34b>", {3, 0x4b}},
    {"<K34c>", {3, 0x4c}},
    {"<K34d>", {3, 0x4d}},
    {"<K34e>", {3, 0x4e}},
    {"<K34f>", {3, 0x4f}},
    {"<K350>", {3, 0x50}},
    {"<K351>", {3, 0x51}},
    {"<K352>", {3, 0x52}},
    {"<K353>", {3, 0x53}},
    {"<K354>", {3, 0x54}},
    {"<K355>", {3, 0x55}},
    {"<K356>", {3, 0x56}},
    {"<K357>", {3, 0x57}},
    {"<K358>", {3, 0x58}},
    {"<K359>", {3, 0x59}},
    {"<K35a>", {3, 0x5a}},
    {"<K35b>", {3, 0x5b}},
    {"<K35c>", {3, 0x5c}},
    {"<K35d>", {3, 0x5d}},
    {"<K35e>", {3, 0x5e}},
    {"<K35f>", {3, 0x5f}},
    {"<K360>", {3, 0x60}},
    {"<K361>", {3, 0x61}},
    {"<K362>", {3, 0x62}},
    {"<K363>", {3, 0x63}},
    {"<K364>", {3, 0x64}},
    {"<K365>", {3, 0x65}},
    {"<K366>", {3, 0x66}},
    {"<K367>", {3, 0x67}},
    {"<K368>", {3, 0x68}},
    {"<K369>", {3, 0x69}},
    {"<K36a>", {3, 0x6a}},
    {"<K36b>", {3, 0x6b}},
    {"<K36c>", {3, 0x6c}},
    {"<K36d>", {3, 0x6d}},
    {"<K36e>", {3, 0x6e}},
    {"<K36f>", {3, 0x6f}},
    {"<K370>", {3, 0x70}},
    {"<K371>", {3, 0x71}},
    {"<K372>", {3, 0x72}},
    {"<K373>", {3, 0x73}},
    {"<K374>", {3, 0x74}},
    {"<K375>", {3, 0x75}},
    {"<K376>", {3, 0x76}},
    {"<K377>", {3, 0x77}},
    {"<K378>", {3, 0x78}},
    {"<K379>", {3, 0x79}},
    {"<K37a>", {3, 0x7a}},
    {"<K37b>", {3, 0x7b}},
    {"<K37c>", {3, 0x7c}},
    {"<K37d>", {3, 0x7d}},
    {"<K37e>", {3, 0x7e}},
    {"<K37f>", {3, 0x7f}},
    {"<K380>", {3, 0x80}},
    {"<K381>", {3, 0x81}},
    {"<K382>", {3, 0x82}},
    {"<K383>", {3, 0x83}},
    {"<K384>", {3, 0x84}},
    {"<K385>", {3, 0x85}},
    {"<K386>", {3, 0x86}},
    {"<K387>", {3, 0x87}},
    {"<K388>", {3, 0x88}},
    {"<K389>", {3, 0x89}},
    {"<K38a>", {3, 0x8a}},
    {"<K38b>", {3, 0x8b}},
    {"<K38c>", {3, 0x8c}},
    {"<K38d>", {3, 0x8d}},
    {"<K38e>", {3, 0x8e}},
    {"<K38f>", {3, 0x8f}},
    {"<K390>", {3, 0x90}},
    {"<K391>", {3, 0x91}},
    {"<K392>", {3, 0x92}},
    {"<K393>", {3, 0x93}},
    {"<K394>", {3, 0x94}},
    {"<K395>", {3, 0x95}},
    {"<K396>", {3, 0x96}},
    {"<K397>", {3, 0x97}},
    {"<K398>", {3, 0x98}},
    {"<K399>", {3, 0x99}},
    {"<K39a>", {3, 0x9a}},
    {"<K39b>", {3, 0x9b}},
    {"<K39c>", {3, 0x9c}},
    {"<K39d>", {3, 0x9d}},
    {"<K39e>", {3, 0x9e}},
    {"<K39f>", {3, 0x9f}},
    {"<K3a0>", {3, 0xa0}},
    {"<K3a1>", {3, 0xa1}},
    {"<K3a2>", {3, 0xa2}},
    {"<K3a3>", {3, 0xa3}},
    {"<K3a4>", {3, 0xa4}},
    {"<K3a5>", {3, 0xa5}},
    {"<K3a6>", {3, 0xa6}},
    {"<K3a7>", {3, 0xa7}},
    {"<K3a8>", {3, 0xa8}},
    {"<K3a9>", {3, 0xa9}},
    {"<K3aa>", {3, 0xaa}},
    {"<K3ab>", {3, 0xab}},
    {"<K3ac>", {3, 0xac}},
    {"<K3ad>", {3, 0xad}},
    {"<K3ae>", {3, 0xae}},
    {"<K3af>", {3, 0xaf}},
    {"<K3b0>", {3, 0xb0}},
    {"<K3b1>", {3, 0xb1}},
    {"<K3b2>", {3, 0xb2}},
    {"<K3b3>", {3, 0xb3}},
    {"<K3b4>", {3, 0xb4}},
    {"<K3b5>", {3, 0xb5}},
    {"<K3b6>", {3, 0xb6}},
    {"<K3b7>", {3, 0xb7}},
    {"<K3b8>", {3, 0xb8}},
    {"<K3b9>", {3, 0xb9}},
    {"<K3ba>", {3, 0xba}},
    {"<K3bb>", {3, 0xbb}},
    {"<K3bc>", {3, 0xbc}},
    {"<K3bd>", {3, 0xbd}},
    {"<K3be>", {3, 0xbe}},
    {"<K3bf>", {3, 0xbf}},
    {"<K3c0>", {3, 0xc0}},
    {"<K3c1>", {3, 0xc1}},
    {"<K3c2>", {3, 0xc2}},
    {"<K3c3>", {3, 0xc3}},
    {"<K3c4>", {3, 0xc4}},
    {"<K3c5>", {3, 0xc5}},
    {"<K3c6>", {3, 0xc6}},
    {"<K3c7>", {3, 0xc7}},
    {"<K3c8>", {3, 0xc8}},
    {"<K3c9>", {3, 0xc9}},
    {"<K3ca>", {3, 0xca}},
    {"<K3cb>", {3, 0xcb}},
    {"<K3cc>", {3, 0xcc}},
    {"<K3cd>", {3, 0xcd}},
    {"<K3ce>", {3, 0xce}},
    {"<K3cf>", {3, 0xcf}},
    {"<K3d0>", {3, 0xd0}},
    {"<K3d1>", {3, 0xd1}},
    {"<K3d2>", {3, 0xd2}},
    {"<K3d3>", {3, 0xd3}},
    {"<K3d4>", {3, 0xd4}},
    {"<K3d5>", {3, 0xd5}},
    {"<K3d6>", {3, 0xd6}},
    {"<K3d7>", {3, 0xd7}},
    {"<K3d8>", {3, 0xd8}},
    {"<K3d9>", {3, 0xd9}},
    {"<K3da>", {3, 0xda}},
    {"<K3db>", {3, 0xdb}},
    {"<K3dc>", {3, 0xdc}},
    {"<K3dd>", {3, 0xdd}},
    {"<K3de>", {3, 0xde}},
    {"<K3df>", {3, 0xdf}},
    {"<K3e0>", {3, 0xe0}},
    {"<K3e1>", {3, 0xe1}},
    {"<K3e2>", {3, 0xe2}},
    {"<K3e3>", {3, 0xe3}},
    {"<K3e4>", {3, 0xe4}},
    {"<K3e5>", {3, 0xe5}},
    {"<K3e6>", {3, 0xe6}},
    {"<K3e7>", {3, 0xe7}},
    {"<K3e8>", {3, 0xe8}},
    {"<K3e9>", {3, 0xe9}},
    {"<K3ea>", {3, 0xea}},
    {"<K3eb>", {3, 0xeb}},
    {"<K3ec>", {3, 0xec}},
    {"<K3ed>", {3, 0xed}},
    {"<K3ee>", {3, 0xee}},
    {"<K3ef>", {3, 0xef}},
    {"<K3f0>", {3, 0xf0}},
    {"<K3f1>", {3, 0xf1}},
    {"<K3f2>", {3, 0xf2}},
    {"<K3f3>", {3, 0xf3}},
    {"<K3f4>", {3, 0xf4}},
    {"<K3f5>", {3, 0xf5}},
    {"<K3f6>", {3, 0xf6}},
    {"<K3f7>", {3, 0xf7}},
    {"<K3f8>", {3, 0xf8}},
    {"<K3f9>", {3, 0xf9}},
    {"<K3fa>", {3, 0xfa}},
    {"<K3fb>", {3, 0xfb}},
    {"<K3fc>", {3, 0xfc}},
    {"<K3fd>", {3, 0xfd}},
    {"<K3fe>", {3, 0xfe}},
    {"<K3ff>", {3, 0xff}},
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
