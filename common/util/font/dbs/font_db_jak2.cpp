#include "font_db_jak2.h"

std::unordered_set<char> passthrus_jak2 = {'~', ' ', ',', '.', '-', '+', '(', ')',
                                           '!', ':', '?', '=', '%', '*', '/', '#',
                                           ';', '<', '>', '@', '[', '_', ']'};

std::vector<ReplaceInfo> replace_info_jak2 = {
    // other
    {"A~Y~-21H~-5Vº~Z", "Å"},
    {"N~Y~-6Hº~Z~+10H", "Nº"},
    {"~+4Vç~-4V", ",c"},

    // added for translations TODO - check these for jak 2
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
    {"o~Y~-24H~-4V<TIL>~Z", "õ"},
    {"a~Y~-24H~-4V<TIL>~Z", "ã"},
    {"O~Y~-28H~-4V'~-9H'~Z", "Ő"},
    {"U~Y~-27H~-4V'~-12H'~Z", "Ű"},
    {"o~Y~-28H~-4V'~-9H'~Z", "ő"},
    {"u~Y~-28H~-4V'~-9H'~Z", "ű"},
    {"E~Y~-22H~-11Vº~Z", "Ė"},
    {"e~Y~-25H~-5Vº~Z", "ė"},
    {"C~Y~-27H~-10Vˇ~Z", "Č"},
    {"c~Y~-25H~-5Vˇ~Z", "č"},
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
    {"I~Y~-19H~-5V'~Z", "Í"},
    {"i~Y~-19H~-8V'~Z", "í"},
    {"O~Y~-22H~-4V'~Z", "Ó"},
    {"o~Y~-26H~-4V'~Z", "ó"},
    {"U~Y~-24H~-3V'~Z", "Ú"},
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

std::vector<EncodeInfo> encode_info_jak2 = {
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

    // TODO - there is a bug if original text has one of these hex values, it will get replaced with
    // these ie. korean or japanese
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
};

std::unordered_map<std::string, std::vector<std::string>> jamo_glyph_mappings_jak2 =
    {
        {"0x06", {"ᄀ"}},      {"0x07", {"ᄁ"}},      {"0x08", {"ᄂ"}},      {"0x09", {"ᄃ"}},
        {"0x0a", {"ᄄ"}},      {"0x0b", {"ᄅ"}},      {"0x0c", {"ᄆ"}},      {"0x0d", {"ᄇ"}},
        {"0x0e", {"ᄈ"}},      {"0x0f", {"ᄋ"}},      {"0x10", {"ᄌ"}},      {"0x11", {"ᄍ"}},
        {"0x12", {"ᄏ"}},      {"0x13", {"ᄐ"}},      {"0x14", {"ᄑ"}},      {"0x15", {"ᅡ"}},
        {"0x16", {"ᅣ"}},       {"0x17", {"ᅵ"}},       {"0x18", {"ᅥ"}},       {"0x19", {"ᅧ"}},
        {"0x1a", {"ᅡ"}},       {"0x1b", {"ᅣ"}},       {"0x1c", {"ᅵ"}},       {"0x1d", {"ᅥ"}},
        {"0x1e", {"ᅧ"}},       {"0x1f", {"ᄂ"}},      {"0x20", {"ᄉ"}},      {"0x21", {"ᄊ"}},
        {"0x22", {"ᅥ"}},       {"0x23", {"ᅧ"}},       {"0x24", {"ᅥ"}},       {"0x25", {"ᅧ"}},
        {"0x26", {"ᄃ"}},      {"0x27", {"ᄄ"}},      {"0x28", {"ᄅ"}},      {"0x29", {"ᄐ"}},
        {"0x2a", {"ᄑ"}},      {"0x2b", {"ᅧ"}},       {"0x2c", {"ᅧ"}},       {"0x2d", {"ᄎ"}},
        {"0x2e", {"ᄒ"}},      {"0x2f", {"ᅥ"}},       {"0x30", {"ᅧ"}},       {"0x31", {"ᅥ"}},
        {"0x32", {"ᅧ"}},       {"0x33", {"ᄀ"}},      {"0x34", {"ᄁ"}},      {"0x35", {"ᄂ"}},
        {"0x36", {"ᄃ"}},      {"0x37", {"ᄄ"}},      {"0x38", {"ᄅ"}},      {"0x39", {"ᄆ"}},
        {"0x3a", {"ᄇ"}},      {"0x3b", {"ᄈ"}},      {"0x3c", {"ᄋ"}},      {"0x3d", {"ᄌ"}},
        {"0x3e", {"ᄍ"}},      {"0x3f", {"ᄏ"}},      {"0x40", {"ᄐ"}},      {"0x41", {"ᄑ"}},
        {"0x42", {"ᅢ"}},       {"0x43", {"ᅤ"}},       {"0x44", {"ᅦ"}},       {"0x45", {"ᅨ"}},
        {"0x46", {"ᅢ"}},       {"0x47", {"ᅤ"}},       {"0x48", {"ᅦ"}},       {"0x49", {"ᅨ"}},
        {"0x4a", {"ᄂ"}},      {"0x4b", {"ᄉ"}},      {"0x4c", {"ᄊ"}},      {"0x4d", {"ᅦ"}},
        {"0x4e", {"ᅨ"}},       {"0x4f", {"ᅦ"}},       {"0x50", {"ᅨ"}},       {"0x51", {"ᄃ"}},
        {"0x52", {"ᄄ"}},      {"0x53", {"ᄅ"}},      {"0x54", {"ᄐ"}},      {"0x55", {"ᄑ"}},
        {"0x56", {"ᅨ"}},       {"0x57", {"ᅨ"}},       {"0x58", {"ᄎ"}},      {"0x59", {"ᄒ"}},
        {"0x5a", {"ᅦ"}},       {"0x5b", {"ᅨ"}},       {"0x5c", {"ᅦ"}},       {"0x5d", {"ᅨ"}},
        {"0x5e", {"ᄀ"}},      {"0x5f", {"ᄏ"}},      {"0x60", {"ᅩ"}},       {"0x61", {"ᅭ"}},
        {"0x62", {"ᅩ"}},       {"0x63", {"ᅭ"}},       {"0x64", {"ᄁ", "ᅫ"}}, {"0x65", {"ᄁ", "ᅩ"}},
        {"0x66", {"ᄁ", "ᅩ"}}, {"0x67", {"ᄁ", "ᅭ"}}, {"0x68", {"ᄁ"}},      {"0x69", {"ᄂ"}},
        {"0x6a", {"ᅳ"}},       {"0x6b", {"ᅮ"}},       {"0x6c", {"ᅲ"}},       {"0x6d", {"ᅳ"}},
        {"0x6e", {"ᅮ"}},       {"0x6f", {"ᅲ"}},       {"0x70", {"ᄃ"}},      {"0x71", {"ᄄ"}},
        {"0x72", {"ᄅ"}},      {"0x73", {"ᄆ"}},      {"0x74", {"ᄇ"}},      {"0x75", {"ᄈ"}},
        {"0x76", {"ᄉ"}},      {"0x77", {"ᄊ"}},      {"0x78", {"ᄋ"}},      {"0x79", {"ᄌ"}},
        {"0x7a", {"ᄍ"}},      {"0x7b", {"ᄎ"}},      {"0x7c", {"ᄐ"}},      {"0x7d", {"ᄑ"}},
        {"0x7e", {"ᄒ"}},      {"0x7f", {"ᅩ"}},       {"0x80", {"ᅭ"}},       {"0x81", {"ᅳ"}},
        {"0x82", {"ᅩ"}},       {"0x83", {"ᅭ"}},       {"0x84", {"ᅳ"}},       {"0x85", {"ᅮ"}},
        {"0x86", {"ᅲ"}},       {"0x87", {"ᅮ"}},       {"0x88", {"ᅲ"}},       {"0x89", {"ᅮ", "ᆫ"}},
        {"0x8a", {"ᅲ", "ᆫ"}},  {"0x8b", {"ᄀ"}},      {"0x8c", {"ᄏ"}},      {"0x8d", {"ᅩ"}},
        {"0x8e", {"ᅩ"}},       {"0x8f", {"ᄁ", "ᅩ"}}, {"0x90", {"ᄁ", "ᅩ"}}, {"0x91", {"ᄁ"}},
        {"0x92", {"ᄂ"}},      {"0x93", {"ᄃ"}},      {"0x94", {"ᄄ"}},      {"0x95", {"ᄅ"}},
        {"0x96", {"ᄆ"}},      {"0x97", {"ᄇ"}},      {"0x98", {"ᄈ"}},      {"0x99", {"ᄉ"}},
        {"0x9a", {"ᄊ"}},      {"0x9b", {"ᄋ"}},      {"0x9c", {"ᄌ"}},      {"0x9d", {"ᄍ"}},
        {"0x9e", {"ᄎ"}},      {"0x9f", {"ᄐ"}},      {"0xa0", {"ᄑ"}},      {"0xa1", {"ᄒ"}},
        {"0xa2", {"ᅩ"}},       {"0xa3", {"ᅳ"}},       {"0xa4", {"ᅩ"}},       {"0xa5", {"ᅳ"}},
        {"0xa6", {"ᅡ"}},       {"0xa7", {"ᅵ"}},       {"0xa8", {"ᅡ"}},       {"0xa9", {"ᅵ"}},
        {"0xaa", {"ᅯ"}},       {"0xab", {"ᅱ"}},       {"0xac", {"ᅯ"}},       {"0xad", {"ᅱ"}},
        {"0xae", {"ᄀ"}},      {"0xaf", {"ᄏ"}},      {"0xb0", {"ᅫ"}},       {"0xb1", {"ᅫ"}},
        {"0xb2", {"ᄆ"}},      {"0xb3", {"ᄁ"}},      {"0xb4", {"ᄂ"}},      {"0xb5", {"ᄃ"}},
        {"0xb6", {"ᄄ"}},      {"0xb7", {"ᄅ"}},      {"0xb8", {"ᄇ"}},      {"0xb9", {"ᄉ"}},
        {"0xba", {"ᄊ"}},      {"0xbb", {"ᄋ"}},      {"0xbc", {"ᄌ"}},      {"0xbd", {"ᄍ"}},
        {"0xbe", {"ᄎ"}},      {"0xbf", {"ᄐ"}},      {"0xc0", {"ᄒ"}},      {"0xc1", {"ᅫ"}},
        {"0xc2", {"ᅫ"}},       {"0xc3", {"ᅰ"}},       {"0xc4", {"ᅰ"}},       {"0xc5", {"ᆨ"}},
        {"0xc6", {"ᆩ"}},       {"0xc7", {"ᆪ"}},       {"0xc8", {"ᆫ"}},       {"0xc9", {"ᆬ"}},
        {"0xca", {"ᆭ"}},       {"0xcb", {"ᆮ"}},       {"0xcc", {"ᆯ"}},       {"0xcd", {"ᆰ"}},
        {"0xce", {"ᆱ"}},       {"0xcf", {"ᆲ"}},       {"0xd0", {"ᆴ"}},       {"0xd1", {"ᆶ"}},
        {"0xd2", {"ᆷ"}},       {"0xd3", {"ᆸ"}},       {"0xd4", {"ᆹ"}},       {"0xd5", {"ᆺ"}},
        {"0xd6", {"ᆻ"}},       {"0xd7", {"ᆼ"}},       {"0xd8", {"ᆽ"}},       {"0xd9", {"ᆾ"}},
        {"0xda", {"ᆿ"}},       {"0xdb", {"ᇀ"}},       {"0xdc", {"ᇁ"}},       {"0xdd", {"ᇂ"}},
        {"0xde", {"ᆨ"}},       {"0xdf", {"ᆩ"}},       {"0xe0", {"ᆪ"}},       {"0xe1", {"ᆫ"}},
        {"0xe2", {"ᆭ"}},       {"0xe3", {"ᆮ"}},       {"0xe4", {"ᆯ"}},       {"0xe5", {"ᆰ"}},
        {"0xe6", {"ᆱ"}},       {"0xe7", {"ᆳ"}},       {"0xe8", {"ᆴ"}},       {"0xe9", {"ᆵ"}},
        {"0xea", {"ᆶ"}},       {"0xeb", {"ᆷ"}},       {"0xec", {"ᆸ"}},       {"0xed", {"ᆺ"}},
        {"0xee", {"ᆼ"}},       {"0xef", {"ᆽ"}},       {"0xf0", {"ᆾ"}},       {"0xf1", {"ᆿ"}},
        {"0xf2", {"ᇀ"}},       {"0xf3", {"ᇁ"}},       {"0xf4", {"ᇂ"}},       {"0xf5", {"ᆨ"}},
        {"0xf6", {"ᆫ"}},       {"0xf7", {"ᆯ"}},       {"0xf8", {"ᆱ"}},       {"0xf9", {"ᆷ"}},
        {"0xfa", {"ᆸ"}},       {"0xfb", {"ᆺ"}},       {"0xfc", {"ᆻ"}},       {"0xfd", {"ᆼ"}},
        {"0xfe", {"ᆨ"}},       {"0xff", {"ᆫ"}},       {"extra_0x86", {"ᆯ"}}, {"extra_0x87", {"ᆷ"}},
        {"extra_0x88", {"ᆸ"}}, {"extra_0x89", {"ᆺ"}}, {"extra_0x8a", {"ᆻ"}}, {"extra_0x8b", {"ᆼ"}},
};

GameTextFontBank g_font_bank_jak2(GameTextVersion::JAK2,
                                  &encode_info_jak2,
                                  &replace_info_jak2,
                                  &passthrus_jak2);