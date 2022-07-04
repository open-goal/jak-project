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

#include "common/util/Assert.h"

#include "third-party/fmt/core.h"

namespace {

/*!
 * Is this a valid character for a hex number?
 */
bool hex_char(char c) {
  return !((c < '0' || c > '9') && (c < 'a' || c > 'f') && (c < 'A' || c > 'F'));
}

}  // namespace

const std::unordered_map<std::string, GameTextVersion> sTextVerEnumMap = {
    {"jak1-v1", GameTextVersion::JAK1_V1},
    {"jak1-v2", GameTextVersion::JAK1_V2}};

const std::string& get_text_version_name(GameTextVersion version) {
  for (auto& [name, ver] : sTextVerEnumMap) {
    if (ver == version) {
      return name;
    }
  }
  throw std::runtime_error(fmt::format("invalid text version {}", version));
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
  (void)m_version;
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
 * Try to replace specific substrings with better variants.
 * These are for hiding confusing text transforms.
 */
std::string GameTextFontBank::replace_to_utf8(std::string& str) const {
  for (auto& info : *m_replace_info) {
    auto pos = str.find(info.from);
    while (pos != std::string::npos) {
      str.replace(pos, info.from.size(), info.to);
      pos = str.find(info.from, pos + info.to.size());
    }
  }
  return str;
}
std::string GameTextFontBank::replace_to_game(std::string& str) const {
  for (auto& info : *m_replace_info) {
    auto pos = str.find(info.to);
    while (pos != std::string::npos) {
      str.replace(pos, info.to.size(), info.from);
      pos = str.find(info.to, pos + info.from.size());
    }
  }
  return str;
}

std::string GameTextFontBank::encode_utf8_to_game(std::string& str) const {
  std::string new_str;

  for (int i = 0; i < (int)str.length();) {
    auto remap = find_encode_to_game(str, i);
    if (!remap) {
      new_str.push_back(str.at(i));
      i += 1;
    } else {
      for (auto b : remap->bytes) {
        new_str.push_back(b);
      }
      i += remap->chars.length();
    }
  }

  str = new_str;
  return str;
}

/*!
 * Turn a normal readable string into a string readable in the Jak 1 font encoding.
 */
std::string GameTextFontBank::convert_utf8_to_game(std::string str) const {
  replace_to_game(str);
  encode_utf8_to_game(str);
  return str;
}

/*!
 * Turn a normal readable string into a string readable in the in-game font encoding and converts
 * \cXX escape sequences
 */
std::string GameTextFontBank::convert_utf8_to_game_with_escape(const std::string& str) const {
  std::string newstr;

  for (int i = 0; i < str.size(); ++i) {
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
        if (!hex_char(first) || !hex_char(second)) {
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
      } else if (p == '"') {
        newstr.push_back(p);
        i += 1;
      } else {
        throw std::runtime_error("unknown string escape code");
      }
    } else {
      newstr.push_back(c);
    }
  }

  replace_to_game(newstr);
  encode_utf8_to_game(newstr);
  return newstr;
}

/*!
 * Convert a string from the game-text font encoding to something normal.
 * Unprintable characters become escape sequences, including tab and newline.
 */
std::string GameTextFontBank::convert_game_to_utf8(const char* in) const {
  std::string result;
  while (*in) {
    auto remap = find_encode_to_utf8(in);
    if (remap != nullptr) {
      result.append(remap->chars);
      in += remap->bytes.size() - 1;
    } else if (((*in >= '0' && *in <= '9') || (*in >= 'A' && *in <= 'Z') ||
                m_passthrus->find(*in) != m_passthrus->end()) &&
               *in != '\\') {
      result.push_back(*in);
    } else if (*in == '\n') {
      result += "\\n";
    } else if (*in == '\t') {
      result += "\\t";
    } else if (*in == '\\') {
      result += "\\\\";
    } else if (*in == '"') {
      result += "\\\"";
    } else {
      result += fmt::format("\\c{:02x}", uint8_t(*in));
    }
    in++;
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

static std::unordered_set<char> s_passthrus = {'~', ' ', ',', '.', '-', '+', '(', ')',
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

GameTextFontBank g_font_bank_jak1(GameTextVersion::JAK1_V1,
                                  &s_encode_info_jak1,
                                  &s_replace_info_jak1,
                                  &s_passthrus);

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
                                     &s_passthrus);

GameTextFontBank g_font_bank_jak2(GameTextVersion::JAK2,
                                  &s_encode_info_null,
                                  &s_replace_info_null,
                                  &s_passthrus);

/*!
 * ========================
 * GAME TEXT FONT BANK LIST
 * ========================
 * The list of available font banks and a couple of helper functions.
 */

std::map<GameTextVersion, GameTextFontBank*> g_font_banks = {
    {GameTextVersion::JAK1_V1, &g_font_bank_jak1},
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
