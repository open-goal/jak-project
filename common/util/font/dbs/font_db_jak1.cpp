#include "font_db_jak1.h"

std::unordered_set<char> passthrus_jak1 = {'~', ' ', ',', '.', '-', '+', '(', ')', '!', ':', '?',
                                           '=', '%', '*', '/', '#', ';', '<', '>', '@', '[', '_'};

std::vector<EncodeInfo> encode_info_jak1 = {
    // random
    {"ˇ", "\x10"},      // caron
    {"`", "\x11"},      // grave accent
    {"'", "\x12"},      // apostrophe
    {"^", "\x13"},      // circumflex
    {"<TIL>", "\x14"},  // tilde
    {"¨", "\x15"},      // umlaut
    {"º", "\x16"},      // numero/overring
    {"¡", "\x17"},      // inverted exclamation mark
    {"¿", "\x18"},      // inverted question mark

    {"海", "\x1a"},  // umi
    {"Æ", "\x1b"},   // aesc
    {"界", "\x1c"},  // kai
    {"Ç", "\x1d"},   // c-cedilla
    {"学", "\x1e"},  // gaku
    {"ß", "\x1f"},   // eszett

    {"ワ", "\x24"},  // wa

    {"ヲ", "\x26"},  // wo
    {"ン", "\x27"},  // -n

    {"岩", "\x5c"},  // iwa
    {"旧", "\x5d"},  // kyuu
    {"空", "\x5e"},  // sora
    //{"掘", "\x5f"},  // horu

    {"ヮ", "\x60"},  // -wa
    {"撃", "\x61"},  // utsu
    {"賢", "\x62"},  // kashikoi
    {"湖", "\x63"},  // mizuumi
    {"口", "\x64"},  // kuchi
    {"行", "\x65"},  // iku
    {"合", "\x66"},  // ai
    {"士", "\x67"},  // shi
    {"寺", "\x68"},  // tera
    {"山", "\x69"},  // yama
    {"者", "\x6a"},  // mono
    {"所", "\x6b"},  // tokoro
    {"書", "\x6c"},  // kaku
    {"小", "\x6d"},  // shou
    {"沼", "\x6e"},  // numa
    {"上", "\x6f"},  // ue
    {"城", "\x70"},  // shiro
    {"場", "\x71"},  // ba
    {"出", "\x72"},  // shutsu
    {"闇", "\x73"},  // yami
    {"遺", "\x74"},  // nokosu
    {"黄", "\x75"},  // ki
    {"屋", "\x76"},  // ya
    {"下", "\x77"},  // shita
    {"家", "\x78"},  // ie
    {"火", "\x79"},  // hi
    {"花", "\x7a"},  // hana
    {"レ", "\x7b"},  // re
    {"Œ", "\x7c"},   // oe
    {"ロ", "\x7d"},  // ro

    {"青", "\x7f"},  // ao

    {"・", "\x90"},  // nakaguro
    {"゛", "\x91"},  // dakuten
    {"゜", "\x92"},  // handakuten
    {"ー", "\x93"},  // chouompu
    {"『", "\x94"},  // nijuukagikakko left
    {"』", "\x95"},  // nijuukagikakko right
    // hiragana
    {"ぁ", "\x96"},  // -a
    {"あ", "\x97"},  // a
    {"ぃ", "\x98"},  // -i
    {"い", "\x99"},  // i
    {"ぅ", "\x9a"},  // -u
    {"う", "\x9b"},  // u
    {"ぇ", "\x9c"},  // -e
    {"え", "\x9d"},  // e
    {"ぉ", "\x9e"},  // -o
    {"お", "\x9f"},  // o
    {"か", "\xa0"},  // ka
    {"き", "\xa1"},  // ki
    {"く", "\xa2"},  // ku
    {"け", "\xa3"},  // ke
    {"こ", "\xa4"},  // ko
    {"さ", "\xa5"},  // sa
    {"し", "\xa6"},  // shi
    {"す", "\xa7"},  // su
    {"せ", "\xa8"},  // se
    {"そ", "\xa9"},  // so
    {"た", "\xaa"},  // ta
    {"ち", "\xab"},  // chi
    {"っ", "\xac"},  // sokuon
    {"つ", "\xad"},  // tsu
    {"て", "\xae"},  // te
    {"と", "\xaf"},  // to
    {"な", "\xb0"},  // na
    {"に", "\xb1"},  // ni
    {"ぬ", "\xb2"},  // nu
    {"ね", "\xb3"},  // ne
    {"の", "\xb4"},  // no
    {"は", "\xb5"},  // ha
    {"ひ", "\xb6"},  // hi
    {"ふ", "\xb7"},  // hu
    {"へ", "\xb8"},  // he
    {"ほ", "\xb9"},  // ho
    {"ま", "\xba"},  // ma
    {"み", "\xbb"},  // mi
    {"む", "\xbc"},  // mu
    {"め", "\xbd"},  // me
    {"も", "\xbe"},  // mo
    {"ゃ", "\xbf"},  // youon ya
    {"や", "\xc0"},  // ya
    {"ゅ", "\xc1"},  // youon yu
    {"ゆ", "\xc2"},  // yu
    {"ょ", "\xc3"},  // youon yo
    {"よ", "\xc4"},  // yo
    {"ら", "\xc5"},  // ra
    {"り", "\xc6"},  // ri
    {"る", "\xc7"},  // ru
    {"れ", "\xc8"},  // re
    {"ろ", "\xc9"},  // ro
    {"ゎ", "\xca"},  // -wa
    {"わ", "\xcb"},  // wa
    {"を", "\xcc"},  // wo
    {"ん", "\xcd"},  // -n
    // katakana
    {"ァ", "\xce"},  // -a
    {"ア", "\xcf"},  // a
    {"ィ", "\xd0"},  // -i
    {"イ", "\xd1"},  // i
    {"ゥ", "\xd2"},  // -u
    {"ウ", "\xd3"},  // u
    {"ェ", "\xd4"},  // -e
    {"エ", "\xd5"},  // e
    {"ォ", "\xd6"},  // -o
    {"オ", "\xd7"},  // o
    {"カ", "\xd8"},  // ka
    {"キ", "\xd9"},  // ki
    {"ク", "\xda"},  // ku
    {"ケ", "\xdb"},  // ke
    {"コ", "\xdc"},  // ko
    {"サ", "\xdd"},  // sa
    {"シ", "\xde"},  // shi
    {"ス", "\xdf"},  // su
    {"セ", "\xe0"},  // se
    {"ソ", "\xe1"},  // so
    {"タ", "\xe2"},  // ta
    {"チ", "\xe3"},  // chi
    {"ッ", "\xe4"},  // sokuon
    {"ツ", "\xe5"},  // tsu
    {"テ", "\xe6"},  // te
    {"ト", "\xe7"},  // to
    {"ナ", "\xe8"},  // na
    {"ニ", "\xe9"},  // ni
    {"ヌ", "\xea"},  // nu
    {"ネ", "\xeb"},  // ne
    {"ノ", "\xec"},  // no
    {"ハ", "\xed"},  // ha
    {"ヒ", "\xee"},  // hi
    {"フ", "\xef"},  // hu
    {"ヘ", "\xf0"},  // he
    {"ホ", "\xf1"},  // ho
    {"マ", "\xf2"},  // ma
    {"ミ", "\xf3"},  // mi
    {"ム", "\xf4"},  // mu
    {"メ", "\xf5"},  // me
    {"モ", "\xf6"},  // mo
    {"ャ", "\xf7"},  // youon ya
    {"ヤ", "\xf8"},  // ya
    {"ュ", "\xf9"},  // youon yu
    {"ユ", "\xfa"},  // yu
    {"ョ", "\xfb"},  // youon yo
    {"ヨ", "\xfc"},  // yo
    {"ラ", "\xfd"},  // ra
    {"リ", "\xfe"},  // ri
    {"ル", "\xff"},  // ru
    // kanji 2
    {"宝", "\x1\x01"},  // takara

    {"石", "\x1\x10"},  // ishi
    {"赤", "\x1\x11"},  // aka
    {"跡", "\x1\x12"},  // ato
    {"川", "\x1\x13"},  // kawa
    {"戦", "\x1\x14"},  // ikusa
    {"村", "\x1\x15"},  // mura
    {"隊", "\x1\x16"},  // tai
    {"台", "\x1\x17"},  // utena
    {"長", "\x1\x18"},  // osa
    {"鳥", "\x1\x19"},  // tori
    {"艇", "\x1\x1a"},  // tei
    {"洞", "\x1\x1b"},  // hora
    {"道", "\x1\x1c"},  // michi
    {"発", "\x1\x1d"},  // hatsu
    {"飛", "\x1\x1e"},  // tobu
    {"噴", "\x1\x1f"},  // fuku

    {"池", "\x1\xa0"},  // ike
    {"中", "\x1\xa1"},  // naka
    {"塔", "\x1\xa2"},  // tou
    {"島", "\x1\xa3"},  // shima
    {"部", "\x1\xa4"},  // bu
    {"砲", "\x1\xa5"},  // hou
    {"産", "\x1\xa6"},  // san
    {"眷", "\x1\xa7"},  // kaerimiru
    {"力", "\x1\xa8"},  // chikara
    {"緑", "\x1\xa9"},  // midori
    {"岸", "\x1\xaa"},  // kishi
    {"像", "\x1\xab"},  // zou
    {"谷", "\x1\xac"},  // tani
    {"心", "\x1\xad"},  // kokoro
    {"森", "\x1\xae"},  // mori
    {"水", "\x1\xaf"},  // mizu
    {"船", "\x1\xb0"},  // fune
    {"™", "\x1\xb1"},   // trademark
};

std::vector<ReplaceInfo> replace_info_jak1 = {
    // other
    {"A~Y~-21H~-5Vº~Z", "Å"},
    {"N~Y~-6Hº~Z~+10H", "Nº"},
    {"O~Y~-16H~-1V/~Z", "Ø"},
    {"A~Y~-6H~+3V,~Z", "Ą"},
    {"E~Y~-6H~+2V,~Z", "Ę"},
    {"L~Y~-16H~+0V/~Z", "Ł"},
    {"Z~Y~-21H~-5Vº~Z", "Ż"},
    {"E~Y~-20H~-5Vº~Z", "Ė"},
    {"C~Y~-20H~-4Vˇ~Z", "Č"},
    {"D~Y~-20H~-4Vˇ~Z", "Ď"},
    {"S~Y~-22H~-4Vˇ~Z", "Š"},
    {"Z~Y~-22H~-4Vˇ~Z", "Ž"},
    {"U~Y~-13H~+2V,~Z", "Ų"},
    {"U~Y~-18H~-10V-~Z", "Ū"},
    {"D~Y~-25H~-1V-~Z", "Đ"},
    {"I~Y~-8H~+1V,~Z", "Į"},
    // czech specific
    {"U~Y~-23H~-5Vº~Z", "Ů"},

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
    // czech specific
    {"Y~Y~-25H~-4V'~Z", "Ý"},

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

    // caron - Ǎ ǎ Ě ě Ǧ ǧ Ǐ ǐ Ǒ ǒ Ǔ ǔ Y̌ y̌
    {"A~Y~-20H~-4Vˇ~Z", "Ǎ"},
    {"E~Y~-20H~-5Vˇ~Z", "Ě"},
    {"G~Y~-20H~-5Vˇ~Z", "Ǧ"},
    {"I~Y~-19H~-5Vˇ~Z", "Ǐ"},
    {"O~Y~-20H~-4Vˇ~Z", "Ǒ"},
    {"U~Y~-24H~-3Vˇ~Z", "Ǔ"},
    {"Y~Y~-24H~-3Vˇ~Z", "Y̌"},
    // czech specific - Č Ň Ř Š Ž Ť
    {"C~Y~-25H~-9Vˇ~Z", "Č"},
    {"N~Y~-23H~-5Vˇ~Z", "Ň"},
    {"R~Y~-24H~-5Vˇ~Z", "Ř"},
    {"S~Y~-24H~-5Vˇ~Z", "Š"},
    {"T~Y~-23H~-5Vˇ~Z", "Ť"},
    {"Z~Y~-23H~-5Vˇ~Z", "Ž"},

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

std::vector<EncodeInfo> encode_info_jak1_v2 = {
    // random
    {"_", "\x03"},      // large space
    {"ˇ", "\x10"},      // caron
    {"`", "\x11"},      // grave accent
    {"'", "\x12"},      // apostrophe
    {"^", "\x13"},      // circumflex
    {"<TIL>", "\x14"},  // tilde
    {"¨", "\x15"},      // umlaut
    {"º", "\x16"},      // numero/overring
    {"¡", "\x17"},      // inverted exclamation mark
    {"¿", "\x18"},      // inverted question mark

    {"海", "\x1a"},  // umi
    {"Æ", "\x1b"},   // aesc
    {"界", "\x1c"},  // kai
    {"Ç", "\x1d"},   // c-cedilla
    {"学", "\x1e"},  // gaku
    {"ß", "\x1f"},   // eszett

    {"ワ", "\x24"},  // wa

    {"ヲ", "\x26"},  // wo
    {"ン", "\x27"},  // -n

    {"岩", "\x5c"},  // iwa
    {"旧", "\x5d"},  // kyuu
    {"空", "\x5e"},  // sora
    {"掘", "\x5f"},  // horu

    {"ヮ", "\x60"},  // -wa
    {"撃", "\x61"},  // utsu
    {"賢", "\x62"},  // kashikoi
    {"湖", "\x63"},  // mizuumi
    {"口", "\x64"},  // kuchi
    {"行", "\x65"},  // iku
    {"合", "\x66"},  // ai
    {"士", "\x67"},  // shi
    {"寺", "\x68"},  // tera
    {"山", "\x69"},  // yama
    {"者", "\x6a"},  // mono
    {"所", "\x6b"},  // tokoro
    {"書", "\x6c"},  // kaku
    {"小", "\x6d"},  // shou
    {"沼", "\x6e"},  // numa
    {"上", "\x6f"},  // ue
    {"城", "\x70"},  // shiro
    {"場", "\x71"},  // ba
    {"出", "\x72"},  // shutsu
    {"闇", "\x73"},  // yami
    {"遺", "\x74"},  // nokosu
    {"黄", "\x75"},  // ki
    {"屋", "\x76"},  // ya
    {"下", "\x77"},  // shita
    {"家", "\x78"},  // ie
    {"火", "\x79"},  // hi
    {"花", "\x7a"},  // hana
    {"レ", "\x7b"},  // re
    {"Œ", "\x7c"},   // oe
    {"ロ", "\x7d"},  // ro

    {"青", "\x7f"},  // ao

    {"・", "\x90"},  // nakaguro
    {"゛", "\x91"},  // dakuten
    {"゜", "\x92"},  // handakuten
    {"ー", "\x93"},  // chouompu
    {"『", "\x94"},  // nijuukagikakko left
    {"』", "\x95"},  // nijuukagikakko right
    // hiragana
    {"ぁ", "\x96"},  // -a
    {"あ", "\x97"},  // a
    {"ぃ", "\x98"},  // -i
    {"い", "\x99"},  // i
    {"ぅ", "\x9a"},  // -u
    {"う", "\x9b"},  // u
    {"ぇ", "\x9c"},  // -e
    {"え", "\x9d"},  // e
    {"ぉ", "\x9e"},  // -o
    {"お", "\x9f"},  // o
    {"か", "\xa0"},  // ka
    {"き", "\xa1"},  // ki
    {"く", "\xa2"},  // ku
    {"け", "\xa3"},  // ke
    {"こ", "\xa4"},  // ko
    {"さ", "\xa5"},  // sa
    {"し", "\xa6"},  // shi
    {"す", "\xa7"},  // su
    {"せ", "\xa8"},  // se
    {"そ", "\xa9"},  // so
    {"た", "\xaa"},  // ta
    {"ち", "\xab"},  // chi
    {"っ", "\xac"},  // sokuon
    {"つ", "\xad"},  // tsu
    {"て", "\xae"},  // te
    {"と", "\xaf"},  // to
    {"な", "\xb0"},  // na
    {"に", "\xb1"},  // ni
    {"ぬ", "\xb2"},  // nu
    {"ね", "\xb3"},  // ne
    {"の", "\xb4"},  // no
    {"は", "\xb5"},  // ha
    {"ひ", "\xb6"},  // hi
    {"ふ", "\xb7"},  // hu
    {"へ", "\xb8"},  // he
    {"ほ", "\xb9"},  // ho
    {"ま", "\xba"},  // ma
    {"み", "\xbb"},  // mi
    {"む", "\xbc"},  // mu
    {"め", "\xbd"},  // me
    {"も", "\xbe"},  // mo
    {"ゃ", "\xbf"},  // youon ya
    {"や", "\xc0"},  // ya
    {"ゅ", "\xc1"},  // youon yu
    {"ゆ", "\xc2"},  // yu
    {"ょ", "\xc3"},  // youon yo
    {"よ", "\xc4"},  // yo
    {"ら", "\xc5"},  // ra
    {"り", "\xc6"},  // ri
    {"る", "\xc7"},  // ru
    {"れ", "\xc8"},  // re
    {"ろ", "\xc9"},  // ro
    {"ゎ", "\xca"},  // -wa
    {"わ", "\xcb"},  // wa
    {"を", "\xcc"},  // wo
    {"ん", "\xcd"},  // -n
    // katakana
    {"ァ", "\xce"},  // -a
    {"ア", "\xcf"},  // a
    {"ィ", "\xd0"},  // -i
    {"イ", "\xd1"},  // i
    {"ゥ", "\xd2"},  // -u
    {"ウ", "\xd3"},  // u
    {"ェ", "\xd4"},  // -e
    {"エ", "\xd5"},  // e
    {"ォ", "\xd6"},  // -o
    {"オ", "\xd7"},  // o
    {"カ", "\xd8"},  // ka
    {"キ", "\xd9"},  // ki
    {"ク", "\xda"},  // ku
    {"ケ", "\xdb"},  // ke
    {"コ", "\xdc"},  // ko
    {"サ", "\xdd"},  // sa
    {"シ", "\xde"},  // shi
    {"ス", "\xdf"},  // su
    {"セ", "\xe0"},  // se
    {"ソ", "\xe1"},  // so
    {"タ", "\xe2"},  // ta
    {"チ", "\xe3"},  // chi
    {"ッ", "\xe4"},  // sokuon
    {"ツ", "\xe5"},  // tsu
    {"テ", "\xe6"},  // te
    {"ト", "\xe7"},  // to
    {"ナ", "\xe8"},  // na
    {"ニ", "\xe9"},  // ni
    {"ヌ", "\xea"},  // nu
    {"ネ", "\xeb"},  // ne
    {"ノ", "\xec"},  // no
    {"ハ", "\xed"},  // ha
    {"ヒ", "\xee"},  // hi
    {"フ", "\xef"},  // hu
    {"ヘ", "\xf0"},  // he
    {"ホ", "\xf1"},  // ho
    {"マ", "\xf2"},  // ma
    {"ミ", "\xf3"},  // mi
    {"ム", "\xf4"},  // mu
    {"メ", "\xf5"},  // me
    {"モ", "\xf6"},  // mo
    {"ャ", "\xf7"},  // youon ya
    {"ヤ", "\xf8"},  // ya
    {"ュ", "\xf9"},  // youon yu
    {"ユ", "\xfa"},  // yu
    {"ョ", "\xfb"},  // youon yo
    {"ヨ", "\xfc"},  // yo
    {"ラ", "\xfd"},  // ra
    {"リ", "\xfe"},  // ri
    {"ル", "\xff"},  // ru
    // kanji 2
    {"宝", "\x1\x01"},  // takara

    {"石", "\x1\x10"},  // ishi
    {"赤", "\x1\x11"},  // aka
    {"跡", "\x1\x12"},  // ato
    {"川", "\x1\x13"},  // kawa
    {"戦", "\x1\x14"},  // ikusa
    {"村", "\x1\x15"},  // mura
    {"隊", "\x1\x16"},  // tai
    {"台", "\x1\x17"},  // utena
    {"長", "\x1\x18"},  // osa
    {"鳥", "\x1\x19"},  // tori
    {"艇", "\x1\x1a"},  // tei
    {"洞", "\x1\x1b"},  // hora
    {"道", "\x1\x1c"},  // michi
    {"発", "\x1\x1d"},  // hatsu
    {"飛", "\x1\x1e"},  // tobu
    {"噴", "\x1\x1f"},  // fuku

    {"池", "\x1\xa0"},  // ike
    {"中", "\x1\xa1"},  // naka
    {"塔", "\x1\xa2"},  // tou
    {"島", "\x1\xa3"},  // shima
    {"部", "\x1\xa4"},  // bu
    {"砲", "\x1\xa5"},  // hou
    {"産", "\x1\xa6"},  // san
    {"眷", "\x1\xa7"},  // kaerimiru
    {"力", "\x1\xa8"},  // chikara
    {"緑", "\x1\xa9"},  // midori
    {"岸", "\x1\xaa"},  // kishi
    {"像", "\x1\xab"},  // zou
    {"谷", "\x1\xac"},  // tani
    {"心", "\x1\xad"},  // kokoro
    {"森", "\x1\xae"},  // mori
    {"水", "\x1\xaf"},  // mizu
    {"船", "\x1\xb0"},  // fune
    {"™", "\x1\xb1"},   // trademark
};

GameTextFontBank g_font_bank_jak1_v1(GameTextVersion::JAK1_V1,
                                     &encode_info_jak1,
                                     &replace_info_jak1,
                                     &passthrus_jak1);

GameTextFontBank g_font_bank_jak1_v2(GameTextVersion::JAK1_V2,
                                     &encode_info_jak1_v2,
                                     &replace_info_jak1,
                                     &passthrus_jak1);
