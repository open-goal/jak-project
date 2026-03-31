#include "font_db_jakx.h"

std::unordered_set<char> passthrus_jakx = {'~', ' ', ',', '.', '-', '+', '(', ')',
                                           '!', ':', '?', '=', '%', '*', '/', '#',
                                           ';', '<', '>', '@', '[', '_', ']'};

std::vector<ReplaceInfo> replace_info_jakx = {
    // other
    {"A~Y~-21H~-5Vº~Z", "Å"},
    {"N~Y~-6Hº~Z~+10H", "Nº"},
    // portuguese
    {"~+4Vç~-4V", "ç"},

    {"O~Y~-25H~-1V/~Z", "Ø"},
    {"o~Y~-23H~+4V/~Z", "ø"},
    {"A~Y~-13H~+8V,~Z", "Ą"},
    {"a~Y~-8H~+6V,~Z", "ą"},
    {"E~Y~-6H~+8V,~Z", "Ę"},
    {"e~Y~-10H~+7V,~Z", "ę"},
    {"L~Y~-21H~+0V/~Z", "Ł"},
    {"l~Y~-16H~+0V/~Z", "ł"},  // TODO - this one is ugly, font character addition (small slash)
    {"Z~Y~-25H~-11Vº~Z", "Ż"},
    {"z~Y~-23H~-5Vº~Z", "ż"},
    {"a~Y~-25H~-5Vº~Z", "å"},
    {"S~Y~-21H~-5V'~Z", "Ś"},
    {"s~Y~-25H~-5V'~Z", "ś"},
    {"n~Y~-25H~-5V'~Z", "ń"},
    {"c~Y~-25H~-5V'~Z", "ć"},
    {"o~Y~-25H~-4V<TIL>~Z", "õ"},
    {"a~Y~-25H~-4V<TIL>~Z", "ã"},
    {"O~Y~-28H~-4V'~-9H'~Z", "Ő"},
    {"U~Y~-27H~-4V'~-12H'~Z", "Ű"},
    {"o~Y~-28H~-4V'~-9H'~Z", "ő"},
    {"u~Y~-28H~-4V'~-9H'~Z", "ű"},
    {"E~Y~-22H~-11Vº~Z", "Ė"},
    {"e~Y~-25H~-5Vº~Z", "ė"},
    {"C~Y~-27H~-10Vˇ~Z", "Č"},
    {"c~Y~-25H~-5Vˇ~Z", "č"},
    {"D~Y~-27H~-10Vˇ~Z", "Ď"},
    {"S~Y~-24H~-10Vˇ~Z", "Š"},
    {"s~Y~-22H~-4Vˇ~Z", "š"},
    {"Z~Y~-25H~-10Vˇ~Z", "Ž"},
    {"z~Y~-23H~-4Vˇ~Z", "ž"},
    {"U~Y~-15H~+5V,~Z", "Ų"},
    {"u~Y~-15H~+5V,~Z", "ų"},
    {"U~Y~-20H~-18V-~Z", "Ū"},
    {"u~Y~-18H~-15V-~Z", "ū"},
    {"D~Y~-28H~-1V-~Z", "Đ"},
    {"d~Y~-13H~-10V-~Z", "đ"},
    {"I~Y~-8H~+4V,~Z", "Į"},
    {"i~Y~-8H~+4V,~Z", "į"},
    // czech specific
    {"U~Y~-24H~-7Vº~Z", "Ů"},
    {"u~Y~-23H~-5Vº~Z", "ů"},
    {"t~Y~-7H~-21V,~Z", "ť"},

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
    {"I~Y~-19H~-8V'~Z", "Í"},
    {"i~Y~-19H~-8V'~Z", "í"},
    {"O~Y~-22H~-4V'~Z", "Ó"},
    {"o~Y~-26H~-4V'~Z", "ó"},
    {"U~Y~-25H~-8V'~Z", "Ú"},
    {"u~Y~-24H~-3V'~Z", "ú"},
    {"Z~Y~-24H~-3V'~Z", "Ź"},
    {"z~Y~-24H~-3V'~Z", "ź"},
    // czech specific
    {"Y~Y~-26H~-5V'~Z", "Ý"},
    {"~+7Vy~-7V~Y~-24H~-3V'~Z", "ý"},

    // circumflex
    {"A~Y~-20H~-4V^~Z", "Â"},
    {"a~Y~-24H~-5V^~Z", "â"},
    {"E~Y~-20H~-5V^~Z", "Ê"},
    {"e~Y~-25H~-4V^~Z", "ê"},
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
    {"e~Y~-25H~-5V¨~Z", "ë"},
    {"I~Y~-19H~-5V¨~Z", "Ï"},
    {"i~Y~-26H~-4V¨~Z", "ï"},
    {"O~Y~-26H~-8V¨~Z", "Ö"},
    {"o~Y~-26H~-4V¨~Z", "ö"},
    {"U~Y~-25H~-8V¨~Z", "Ü"},
    {"u~Y~-24H~-3V¨~Z", "ü"},

    // caron - Ǎ ǎ Ě ě Ǧ ǧ Ǐ ǐ Ǒ ǒ Ǔ ǔ Y̌ y̌
    {"A~Y~-25H~-9Vˇ~Z", "Ǎ"},
    {"a~Y~-24H~-5Vˇ~Z", "ǎ"},
    {"E~Y~-22H~-8Vˇ~Z", "Ě"},
    {"e~Y~-25H~-4Vˇ~Z", "ě"},
    {"G~Y~-24H~-8Vˇ~Z", "Ǧ"},
    {"~+7Vg~-7V~Y~-25H~-4Vˇ~Z", "ǧ"},
    {"I~Y~-19H~-8Vˇ~Z", "Ǐ"},
    {"i~Y~-19H~-8Vˇ~Z", "ǐ"},
    {"O~Y~-25H~-7Vˇ~Z", "Ǒ"},
    {"o~Y~-25H~-4Vˇ~Z", "ǒ"},
    {"U~Y~-25H~-6Vˇ~Z", "Ǔ"},
    {"u~Y~-24H~-3Vˇ~Z", "ǔ"},
    {"Y~Y~-25H~-5Vˇ~Z", "Y̌"},
    {"~+7Vy~-7V~Y~-25H~-3Vˇ~Z", "y̌"},
    // czech specific - Č č Ň ň Ř ř Š š Ž ž Ť
    {"C~Y~-25H~-9Vˇ~Z", "Č"},
    {"c~Y~-24H~-5Vˇ~Z", "č"},
    {"N~Y~-25H~-9Vˇ~Z", "Ň"},
    {"n~Y~-24H~-5Vˇ~Z", "ň"},
    {"R~Y~-25H~-9Vˇ~Z", "Ř"},
    {"r~Y~-22H~-5Vˇ~Z", "ř"},
    {"S~Y~-25H~-9Vˇ~Z", "Š"},
    {"s~Y~-22H~-5Vˇ~Z", "š"},
    {"T~Y~-24H~-7Vˇ~Z", "Ť"},
    {"Z~Y~-25H~-9Vˇ~Z", "Ž"},
    {"z~Y~-24H~-5Vˇ~Z", "ž"},

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
    {"~Y~1L<FLAG_PART_FILL>~Z~7L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~7L<FLAG_PART_VERT_STRIPE_RIGHT>~]"
     "~-1H~Y~1L<FLAG_PART_FILL>~Z~7L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~+26H",
     "<FLAG_FINLAND>"},
    {"~Y~7L<FLAG_PART_FILL>~Z~5L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~5L<FLAG_PART_VERT_STRIPE_RIGHT>~]"
     "~-1H~Y~7L<FLAG_PART_FILL>~Z~5L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~+26H",
     "<FLAG_SWEDEN>"},
    {"~Y~3L<FLAG_PART_FILL>~Z~1L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~1L<FLAG_PART_VERT_STRIPE_RIGHT>~]"
     "~-1H~Y~3L<FLAG_PART_FILL>~Z~1L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~+26H",
     "<FLAG_DENMARK>"},
    {"~Y~1L<FLAG_PART_FILL>~Z~3L<FLAG_PART_TOP_BOTTOM_STRIPE>~]~-1H~Y~1L<FLAG_PART_FILL>~Z~3L<FLAG_"
     "PART_TOP_BOTTOM_STRIPE>~Z~-19H~1L<FLAG_PART_VERT_STRIPE_MIDDLE>~Z~-23H~7L<FLAG_PART_VERT_"
     "STRIPE_RIGHT>~Z~-23H~7L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~7L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~"
     "+26H",
     "<FLAG_NORWAY>"},
    {"~Y~1L<FLAG_PART_FILL>~Z~7L<FLAG_PART_TOP_BOTTOM_STRIPE>~]~-1H~Y~1L<FLAG_PART_FILL>~Z~7L<FLAG_"
     "PART_TOP_BOTTOM_STRIPE>~Z~-19H~1L<FLAG_PART_VERT_STRIPE_MIDDLE>~Z~-23H~3L<FLAG_PART_VERT_"
     "STRIPE_RIGHT>~Z~-23H~3L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~3L<FLAG_PART_HORZ_STRIPE_MIDDLE>~Z~"
     "+26H",
     "<FLAG_ICELAND>"},

    // weird stuff
    // - descenders
    {"~+7Vp~-7V", "p"},
    {"~+7Vy~-7V", "y"},
    {"~+7Vg~-7V", "g"},
    {"~+7Vq~-7V", "q"},
    {"~+1Vj~-1V", "j"},

    // - cryllic
    {"~+2Vд~-2V", "д"},
    {"~+4Vу~-4V", "у"},
    {"~+4Vр~-4V", "р"},
    {"~+3Vф~-3V", "ф"},
    {"~+2Vц~-2V", "ц"},
    {"~+2VД~-2V", "Д"},
    {"~+2Vщ~-2V", "щ"},

    {"\\\\",
     "~%"},  // this is 2 slashes, duplicated because we use an escape sequence when decompiling

    // - symbols and ligatures
    {"~-4H~-3V<SOMETHING>~+3V~-4H",
     "<SUPERSCRIPT_QUOTE>"},  // used for the 4<__> place in spanish.  the 5th uses the same
                              // character but looks different...?
    {"~Y~-6Hº~Z~+10H", "°"},

    // Color / Emphasis
    {"~[~0L", "<COLOR_DEFAULT>"},
    {"~[~1L", "<COLOR_WHITE>"},
    {"~[~2L", "<COLOR_TRANSPARENT>"},
    {"~[~3L", "<COLOR_RED>"},
    {"~[~4L", "<COLOR_ORANGE>"},
    {"~[~5L", "<COLOR_YELLOW>"},
    {"~[~6L", "<COLOR_GREEN>"},
    {"~[~7L", "<COLOR_BLUE>"},
    {"~[~8L", "<COLOR_CYAN>"},
    {"~[~9L", "<COLOR_PINK>"},
    {"~[~10L", "<COLOR_MENU-SELECTED>"},
    {"~[~11L", "<COLOR_MENU-SELECTED-PARENT>"},
    {"~[~12L", "<COLOR_MENU>"},
    {"~[~13L", "<COLOR_MENU-PARENT>"},
    {"~[~14L", "<COLOR_MENU-FUNC-BAD>"},
    {"~[~15L", "<COLOR_MENU-FLAG-ON>"},
    {"~[~16L", "<COLOR_MENU-FLAG-ON-PARENT>"},
    {"~[~17L", "<COLOR_MENU-FLAG-OFF>"},
    {"~[~18L", "<COLOR_MENU-FLAG-OFF-PARENT>"},
    {"~[~19L", "<COLOR_MENU-INVALID>"},
    {"~[~20L", "<COLOR_FLAT-YELLOW>"},
    {"~[~21L", "<COLOR_COLOR-21>"},
    {"~[~22L", "<COLOR_PAD-BACK>"},
    {"~[~23L", "<COLOR_PAD-SHINE>"},
    {"~[~24L", "<COLOR_PAD-SQUARE>"},
    {"~[~25L", "<COLOR_PAD-CIRCLE>"},
    {"~[~26L", "<COLOR_PAD-TRIANGLE>"},
    {"~[~27L", "<COLOR_PAD-CROSS>"},
    {"~[~28L", "<COLOR_PROGRESS-OLD-BLUE>"},
    {"~[~29L", "<COLOR_PROGRESS-OLD-YELLOW>"},
    {"~[~30L", "<COLOR_PROGRESS-OLD-SELECTED>"},
    {"~[~31L", "<COLOR_PROGRESS-OLD-PERCENT>"},
    {"~[~32L", "<COLOR_PROGRESS>"},
    {"~[~33L", "<COLOR_PROGRESS-SELECTED>"},
    {"~[~34L", "<COLOR_PROGRESS-FORCE-SELECTED>"},
    {"~[~35L", "<COLOR_PROGRESS-OPTION-OFF>"},
    {"~[~36L", "<COLOR_COLOR-36>"},
    {"~[~37L", "<COLOR_CREDITS-STAFF-TITLE-1>"},
    {"~[~38L", "<COLOR_CREDITS-STAFF-TITLE-2>"},
    {"~[~39L", "<COLOR_COLOR-39>"}};

// TODO - cryllic

std::vector<EncodeInfo> encode_info_jakx = {
    {"ˇ", "\x10"},      // caron
    {"`", "\x11"},      // grave accent
    {"'", "\x12"},      // apostrophe
    {"^", "\x13"},      // circumflex
    {"<TIL>", "\x14"},  // tilde
    {"¨", "\x15"},      // umlaut
    {"º", "\x16"},      // numero/overring
    {"¡", "\x17"},      // inverted exclamation mark
    {"¿", "\x18"},      // inverted question mark
    {"<SOMETHING>", "\x19"},
    {"ç", "\x1d"},  // c-cedilla
    {"Ç", "\x1e"},  // c-cedilla
    {"ß", "\x1f"},  // eszett

    {"œ", "\x5e"},  // ligature o+e

    {"<FLAG_PART_HORZ_STRIPE_MIDDLE>", "\x7f"},
    {"<FLAG_PART_HORZ_STRIPE_BOTTOM>", "\x80"},
    {"<FLAG_PART_VERT_STRIPE_LARGE>", "\x81"},
    {"<FLAG_PART_VERT_STRIPE_RIGHT>", "\x82"},
    {"<FLAG_PART_VERT_STRIPE_LEFT>", "\x83"},
    {"<FLAG_PART_VERT_STRIPE_MIDDLE>", "\x84"},
    {"<FLAG_PART_FILL>", "\x85"},
    {"<FLAG_PART_JAPAN_SUN>", "\x86"},
    {"<FLAG_PART_KOREA_TRIGRAMS_LEFT>", "\x87"},
    {"<FLAG_PART_KOREA_TRIGRAMS_RIGHT>", "\x88"},
    {"<FLAG_PART_KOREA_CIRCLE_TOP>", "\x89"},
    {"<FLAG_PART_KOREA_CIRCLE_FILL>", "\x8a"},
    {"<FLAG_PART_TOP_BOTTOM_STRIPE>", "\x8b"},
    {"<FLAG_PART_UK_CROSS_LEFT>", "\x8c"},
    {"<FLAG_PART_UK_CROSS_RIGHT>", "\x8d"},
    {"<FLAG_PART_UK_FILL_LEFT>", "\x8e"},
    {"<FLAG_PART_UK_FILL_RIGHT>", "\x8f"},
    {"<FLAG_PART_USA_STRIPES_RIGHT>", "\x90"},
    {"<PAD_PART_STICK>", "\x91"},
    {"<PAD_PART_SELECT>", "\x92"},
    {"<PAD_PART_TRIGGER_BACK>", "\x93"},
    {"<PAD_PART_R1_NAME>", "\x94"},
    {"<PAD_PART_L1_NAME>", "\x95"},
    {"<PAD_PART_R2_NAME>", "\x96"},
    {"<PAD_PART_L2_NAME>", "\x97"},
    {"<PAD_PART_STICK_UP>", "\x98"},
    {"<PAD_PART_STICK_UP_RIGHT>", "\x99"},
    {"<FLAG_PART_USA_STRIPES_LEFT>", "\x9a"},
    {"<FLAG_PART_USA_STARS>", "\x9b"},
    {"<PAD_PART_STICK_DOWN>", "\x9c"},
    {"<PAD_PART_STICK_DOWN_LEFT>", "\x9d"},
    {"<PAD_PART_STICK_LEFT>", "\x9e"},
    {"<PAD_PART_STICK_UP_LEFT>", "\x9f"},
    {"<PAD_PART_DPAD_D>", "\xa0"},
    {"<PAD_PART_DPAD_L>", "\xa1"},
    {"<PAD_PART_DPAD_U>", "\xa2"},
    {"<PAD_PART_DPAD_R>", "\xa3"},
    {"<PAD_PART_STICK_RIGHT>", "\xa4"},
    {"<PAD_PART_STICK_DOWN_RIGHT>", "\xa5"},
    {"<PAD_PART_SHOULDER_TOP_LEFT>", "\xa6"},
    {"<PAD_PART_SHOULDER_TOP_RIGHT>", "\xa7"},
    {"<PAD_PART_TRIGGER_TOP_LEFT>", "\xa8"},
    {"<PAD_PART_TRIGGER_TOP_RIGHT>", "\xa9"},
    {"<PAD_PART_TRIGGER_SHIM1>", "\xaa"},
    {"<PAD_PART_TRIGGER_SHIM2>", "\xab"},
    {"<PAD_PART_SHOULDER_SHIM2>", "\xac"},

    {"<PAD_PART_SHOULDER_BOTTOM_LEFT>", "\xb0"},
    {"<PAD_PART_SHOULDER_BOTTOM_RIGHT>", "\xb1"},
    {"<PAD_PART_TRIGGER_BOTTOM_LEFT>", "\xb2"},
    {"<PAD_PART_TRIGGER_BOTTOM_RIGHT>", "\xb3"},

    // cryllic
    // - uppercase
    {"А", "\xba"},
    {"Б", "\xbb"},
    {"В", "\xbc"},
    {"Г", "\xbd"},
    {"Д", "\xbe"},
    {"Е", "\xbf"},
    // "Ё" - if ever used in subtitles, likely uses the accents at the end
    {"Ж", "\xc0"},
    {"З", "\xc1"},
    {"И", "\xc2"},
    // blank (c3)
    // c4
    // "Й" - if ever used in subtitles, likely uses the accents at the end
    {"К", "\xc4"},
    {"Л", "\xc5"},
    {"М", "\xc6"},
    {"Н", "\xc7"},
    {"О", "\xc8"},
    {"П", "\xc9"},
    {"Р", "\xca"},
    {"С", "\xcb"},
    {"Т", "\xcc"},
    // blank (cd)
    // ce
    {"У", "\xce"},
    {"Ф", "\xcf"},
    {"Х", "\xd0"},
    {"Ц", "\xd1"},
    {"Ч", "\xd2"},
    {"Ш", "\xd3"},
    {"Щ", "\xd4"},
    {"Ъ", "\xd5"},
    {"Ы", "\xd6"},
    {"Ь", "\xd7"},
    // d8
    {"Э", "\xd8"},
    {"Ю", "\xd9"},
    {"Я", "\xda"},
    // - lowercase
    {"а", "\xdb"},
    {"б", "\xdc"},
    {"в", "\xdd"},
    {"г", "\xde"},
    {"д", "\xdf"},
    {"е", "\xe0"},
    {"ё", "\xe1"},
    // e2
    {"ж", "\xe2"},
    {"з", "\xe3"},
    {"и", "\xe4"},
    {"й", "\xe5"},
    {"к", "\xe6"},
    {"л", "\xe7"},
    {"м", "\xe8"},
    {"н", "\xe9"},
    {"о", "\xea"},
    {"п", "\xeb"},
    // ec
    {"р", "\xec"},
    {"с", "\xed"},
    {"т", "\xee"},
    {"у", "\xef"},
    {"ф", "\xf0"},
    {"х", "\xf1"},
    {"ц", "\xf2"},
    {"ч", "\xf3"},
    {"ш", "\xf4"},
    {"щ", "\xf5"},
    // f6
    {"ъ", "\xf6"},
    {"ы", "\xf7"},
    {"ь", "\xf8"},
    {"э", "\xf9"},
    {"ю", "\xfa"},
    {"я", "\xfb"},
    // - extra accents
    {"RU-TODO-1",
     "\xfc"},  // NOTE: they never used this in the game text, maybe in subtitles though
    {"RU-TODO-2",
     "\xfd"},  // NOTE: they never used this in the game text, maybe in subtitles though

    // {"入", "\x1\x00"}},
    // {"年", "\x1\x01"}},
    // punctuation
    {"・", "\x1\x10"},
    {"゛", "\x1\x11"},
    {"゜", "\x1\x12"},
    {"ー", "\x1\x13"},
    {"『", "\x1\x14"},
    {"』", "\x1\x15"},
    // hiragana
    {"ぁ", "\x1\x16"},  // -a
    {"あ", "\x1\x17"},  // a
    {"ぃ", "\x1\x18"},  // -i
    {"い", "\x1\x19"},  // i
    {"ぅ", "\x1\x1a"},  // -u
    {"う", "\x1\x1b"},  // u
    {"ぇ", "\x1\x1c"},  // -e
    {"え", "\x1\x1d"},  // e
    {"ぉ", "\x1\x1e"},  // -o
    {"お", "\x1\x1f"},  // o
    {"か", "\x1\x20"},  // ka
    {"き", "\x1\x21"},  // ki
    {"く", "\x1\x22"},  // ku
    {"け", "\x1\x23"},  // ke
    {"こ", "\x1\x24"},  // ko
    {"さ", "\x1\x25"},  // sa
    {"し", "\x1\x26"},  // shi
    {"す", "\x1\x27"},  // su
    {"せ", "\x1\x28"},  // se
    {"そ", "\x1\x29"},  // so
    {"た", "\x1\x2a"},  // ta
    {"ち", "\x1\x2b"},  // chi
    {"っ", "\x1\x2c"},  // sokuon
    {"つ", "\x1\x2d"},  // tsu
    {"て", "\x1\x2e"},  // te
    {"と", "\x1\x2f"},  // to
    {"な", "\x1\x30"},  // na
    {"に", "\x1\x31"},  // ni
    {"ぬ", "\x1\x32"},  // nu
    {"ね", "\x1\x33"},  // ne
    {"の", "\x1\x34"},  // no
    {"は", "\x1\x35"},  // ha
    {"ひ", "\x1\x36"},  // hi
    {"ふ", "\x1\x37"},  // fu
    {"へ", "\x1\x38"},  // he
    {"ほ", "\x1\x39"},  // ho
    {"ま", "\x1\x3a"},  // ma
    {"み", "\x1\x3b"},  // mi
    {"む", "\x1\x3c"},  // mu
    {"め", "\x1\x3d"},  // me
    {"も", "\x1\x3e"},  // mo
    {"ゃ", "\x1\x3f"},  // youon ya
    {"や", "\x1\x40"},  // ya
    {"ゅ", "\x1\x41"},  // youon yu
    {"ゆ", "\x1\x42"},  // yu
    {"ょ", "\x1\x43"},  // youon yo
    {"よ", "\x1\x44"},  // yo
    {"ら", "\x1\x45"},  // ra
    {"り", "\x1\x46"},  // ri
    {"る", "\x1\x47"},  // ru
    {"れ", "\x1\x48"},  // re
    {"ろ", "\x1\x49"},  // ro
    {"ゎ", "\x1\x4a"},  // -wa
    {"わ", "\x1\x4b"},  // wa
    {"を", "\x1\x4c"},  // wo
    {"ん", "\x1\x4d"},  // -n
    // katakana
    {"ァ", "\x1\x4e"},  // -a
    {"ア", "\x1\x4f"},  // a
    {"ィ", "\x1\x50"},  // -i
    {"イ", "\x1\x51"},  // i
    {"ゥ", "\x1\x52"},  // -u
    {"ウ", "\x1\x53"},  // u
    {"ェ", "\x1\x54"},  // -e
    {"エ", "\x1\x55"},  // e
    {"ォ", "\x1\x56"},  // -o
    {"オ", "\x1\x57"},  // o
    {"カ", "\x1\x58"},  // ka
    {"キ", "\x1\x59"},  // ki
    {"ク", "\x1\x5a"},  // ku
    {"ケ", "\x1\x5b"},  // ke
    {"コ", "\x1\x5c"},  // ko
    {"サ", "\x1\x5d"},  // sa
    {"シ", "\x1\x5e"},  // shi
    {"ス", "\x1\x5f"},  // su
    {"セ", "\x1\x60"},  // se
    {"ソ", "\x1\x61"},  // so
    {"タ", "\x1\x62"},  // ta
    {"チ", "\x1\x63"},  // chi
    {"ッ", "\x1\x64"},  // sokuon
    {"ツ", "\x1\x65"},  // tsu
    {"テ", "\x1\x66"},  // te
    {"ト", "\x1\x67"},  // to
    {"ナ", "\x1\x68"},  // na
    {"ニ", "\x1\x69"},  // ni
    {"ヌ", "\x1\x6a"},  // nu
    {"ネ", "\x1\x6b"},  // ne
    {"ノ", "\x1\x6c"},  // no
    {"ハ", "\x1\x6d"},  // ha
    {"ヒ", "\x1\x6e"},  // hi
    {"フ", "\x1\x6f"},  // fu
    {"ヘ", "\x1\x70"},  // he
    {"ホ", "\x1\x71"},  // ho
    {"マ", "\x1\x72"},  // ma
    {"ミ", "\x1\x73"},  // mi
    {"ム", "\x1\x74"},  // mu
    {"メ", "\x1\x75"},  // me
    {"モ", "\x1\x76"},  // mo
    {"ャ", "\x1\x77"},  // youon ya
    {"ヤ", "\x1\x78"},  // ya
    {"ュ", "\x1\x79"},  // youon yu
    {"ユ", "\x1\x7a"},  // yu
    {"ョ", "\x1\x7b"},  // youon yo
    {"ヨ", "\x1\x7c"},  // yo
    {"ラ", "\x1\x7d"},  // ra
    {"リ", "\x1\x7e"},  // ri
    {"ル", "\x1\x7f"},  // ru
    {"レ", "\x1\x80"},  // re
    {"ロ", "\x1\x81"},  // ro
    {"ヮ", "\x1\x82"},  // -wa
    {"ワ", "\x1\x83"},  // wa
    {"ヲ", "\x1\x84"},  // wo
    {"ン", "\x1\x85"},  // -n

    {"位", "\x1\x8c"},
    {"遺", "\x1\x8d"},
    {"院", "\x1\x8e"},
    {"映", "\x1\x8f"},
    {"衛", "\x1\x90"},
    {"応", "\x1\x91"},
    {"下", "\x1\x92"},
    {"画", "\x1\x93"},
    {"解", "\x1\x94"},
    {"開", "\x1\x95"},
    {"外", "\x1\x96"},
    {"害", "\x1\x97"},
    {"蓋", "\x1\x98"},
    {"完", "\x1\x99"},
    {"換", "\x1\x9a"},
    {"監", "\x1\x9b"},
    {"間", "\x1\x9c"},
    {"器", "\x1\x9d"},
    {"記", "\x1\x9e"},
    {"逆", "\x1\x9f"},
    {"救", "\x1\xa0"},
    {"金", "\x1\xa1"},
    {"空", "\x1\xa2"},
    {"掘", "\x1\xa3"},
    {"警", "\x1\xa4"},
    {"迎", "\x1\xa5"},
    {"撃", "\x1\xa6"},
    {"建", "\x1\xa7"},
    {"源", "\x1\xa8"},
    {"現", "\x1\xa9"},
    {"言", "\x1\xaa"},
    {"限", "\x1\xab"},
    {"個", "\x1\xac"},
    {"庫", "\x1\xad"},
    {"後", "\x1\xae"},
    {"語", "\x1\xaf"},
    {"護", "\x1\xb0"},
    {"交", "\x1\xb1"},
    {"功", "\x1\xb2"},
    {"向", "\x1\xb3"},
    {"工", "\x1\xb4"},
    {"攻", "\x1\xb5"},
    {"溝", "\x1\xb6"},
    {"行", "\x1\xb7"},
    {"鉱", "\x1\xb8"},
    {"降", "\x1\xb9"},
    {"合", "\x1\xba"},
    {"告", "\x1\xbb"},
    {"獄", "\x1\xbc"},
    {"彩", "\x1\xbd"},
    {"作", "\x1\xbe"},
    {"山", "\x1\xbf"},
    {"使", "\x1\xc0"},
    {"始", "\x1\xc1"},
    {"試", "\x1\xc2"},
    {"字", "\x1\xc3"},
    {"寺", "\x1\xc4"},
    {"時", "\x1\xc5"},
    {"示", "\x1\xc6"},
    {"自", "\x1\xc7"},
    {"式", "\x1\xc8"},
    {"矢", "\x1\xc9"},
    {"射", "\x1\xca"},
    {"者", "\x1\xcb"},
    {"守", "\x1\xcc"},
    {"手", "\x1\xcd"},
    {"終", "\x1\xce"},
    {"週", "\x1\xcf"},
    {"出", "\x1\xd0"},
    {"所", "\x1\xd1"},
    {"書", "\x1\xd2"},
    {"勝", "\x1\xd3"},
    {"章", "\x1\xd4"},
    {"上", "\x1\xd5"},
    {"乗", "\x1\xd6"},
    {"場", "\x1\xd7"},
    {"森", "\x1\xd8"},
    {"進", "\x1\xd9"},
    {"人", "\x1\xda"},
    {"水", "\x1\xdb"},
    {"数", "\x1\xdc"},
    {"制", "\x1\xdd"},
    {"性", "\x1\xde"},
    {"成", "\x1\xdf"},
    {"聖", "\x1\xe0"},
    {"石", "\x1\xe1"},
    {"跡", "\x1\xe2"},
    {"先", "\x1\xe3"},
    {"戦", "\x1\xe4"},
    {"船", "\x1\xe5"},
    {"選", "\x1\xe6"},
    {"走", "\x1\xe7"},
    {"送", "\x1\xe8"},
    {"像", "\x1\xe9"},
    {"造", "\x1\xea"},
    {"続", "\x1\xeb"},
    {"対", "\x1\xec"},
    {"袋", "\x1\xed"},
    {"台", "\x1\xee"},
    {"弾", "\x1\xef"},
    {"地", "\x1\xf0"},
    {"中", "\x1\xf1"},
    {"敵", "\x1\xf2"},
    {"転", "\x1\xf3"},
    {"電", "\x1\xf4"},
    {"塔", "\x1\xf5"},
    {"頭", "\x1\xf6"},
    {"動", "\x1\xf7"},
    {"内", "\x1\xf8"},
    {"日", "\x1\xf9"},
    {"入", "\x1\xfa"},
    {"年", "\x1\xfb"},
    {"能", "\x1\xfc"},
    {"廃", "\x1\xfd"},
    {"排", "\x1\xfe"},
    {"敗", "\x1\xff"},

    {"発", "\x2\x10"},
    {"反", "\x2\x11"},
    {"必", "\x2\x12"},
    {"表", "\x2\x13"},
    {"武", "\x2\x14"},
    {"壁", "\x2\x15"},
    {"墓", "\x2\x16"},
    {"放", "\x2\x17"},
    {"方", "\x2\x18"},
    {"砲", "\x2\x19"},
    {"妨", "\x2\x1a"},
    {"北", "\x2\x1b"},
    {"本", "\x2\x1c"},
    {"幕", "\x2\x1d"},
    {"無", "\x2\x1e"},
    {"迷", "\x2\x1f"},
    {"面", "\x2\x20"},
    {"戻", "\x2\x21"},
    {"紋", "\x2\x22"},
    {"薬", "\x2\x23"},
    {"輸", "\x2\x24"},
    {"勇", "\x2\x25"},
    {"友", "\x2\x26"},
    {"遊", "\x2\x27"},
    {"容", "\x2\x28"},
    {"要", "\x2\x29"},
    {"利", "\x2\x2a"},
    {"了", "\x2\x2b"},
    {"量", "\x2\x2c"},
    {"力", "\x2\x2d"},
    {"練", "\x2\x2e"},
    {"連", "\x2\x2f"},
    {"録", "\x2\x30"},
    {"話", "\x2\x31"},
    {"墟", "\x2\x32"},
    {"脱", "\x2\x33"},
    // {"成", "\x2\x34"},
    {"旗", "\x2\x35"},
    {"破", "\x2\x36"},
    {"壊", "\x2\x37"},
    {"全", "\x2\x38"},
    {"滅", "\x2\x39"},
    {"機", "\x2\x3a"},
    {"仲", "\x2\x3b"},
    {"渓", "\x2\x3c"},
    {"谷", "\x2\x3d"},
    {"優", "\x2\x3e"},
    {"探", "\x2\x3f"},
    {"部", "\x2\x40"},
    {"索", "\x2\x41"},
    // {"乗", "\x2\x42"},
    {"前", "\x2\x43"},
    {"右", "\x2\x44"},
    {"左", "\x2\x45"},
    {"会", "\x2\x46"},
    {"高", "\x2\x47"},
    {"低", "\x2\x48"},
    {"押", "\x2\x49"},
    {"切", "\x2\x4a"},
    {"替", "\x2\x4b"},
    // {"対", "\x2\x4c"},
    {"秒", "\x2\x4d"},
    {"箱", "\x2\x4e"},
    {"泳", "\x2\x4f"},
    {"～", "\x2\x50"},

    {"闇", "\x2\x56"},
    {"以", "\x2\x57"},
    {"屋", "\x2\x58"},
    {"俺", "\x2\x59"},
    {"化", "\x2\x5a"},
    {"界", "\x2\x5b"},
    {"感", "\x2\x5c"},
    {"気", "\x2\x5d"},
    {"却", "\x2\x5e"},
    {"曲", "\x2\x5f"},
    {"継", "\x2\x60"},
    {"権", "\x2\x61"},
    {"見", "\x2\x62"},
    {"古", "\x2\x63"},
    {"好", "\x2\x64"},
    // {"高", "\x2\x65"},
    {"才", "\x2\x66"},
    {"士", "\x2\x67"},
    {"子", "\x2\x68"},
    {"次", "\x2\x69"},
    {"主", "\x2\x6a"},
    {"種", "\x2\x6b"},
    {"讐", "\x2\x6c"},
    {"女", "\x2\x6d"},
    {"小", "\x2\x6e"},
    {"焼", "\x2\x6f"},
    {"証", "\x2\x70"},
    {"神", "\x2\x71"},
    {"身", "\x2\x72"},
    {"寸", "\x2\x73"},
    {"世", "\x2\x74"},
    {"想", "\x2\x75"},
    {"退", "\x2\x76"},
    {"第", "\x2\x77"},
    {"着", "\x2\x78"},
    {"天", "\x2\x79"},
    {"倒", "\x2\x7a"},
    {"到", "\x2\x7b"},
    {"突", "\x2\x7c"},
    {"爆", "\x2\x7d"},
    {"番", "\x2\x7e"},
    {"負", "\x2\x7f"},
    {"復", "\x2\x80"},
    {"物", "\x2\x81"},
    {"眠", "\x2\x82"},
    {"予", "\x2\x83"},
    {"用", "\x2\x84"},
    {"落", "\x2\x85"},
    {"緑", "\x2\x86"},

    {"封", "\x2\x88"},
    {"印", "\x2\x89"},
    {"扉", "\x2\x8a"},
    {"最", "\x2\x8b"},
    {"刻", "\x2\x8c"},
    {"足", "\x2\x8d"},
};

GameTextFontBank g_font_bank_jakx(GameTextVersion::JAKX,
                                  &encode_info_jakx,
                                  &replace_info_jakx,
                                  &passthrus_jakx);
