import glob
import json
import re

import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--fix", action="store_true")
parser.set_defaults(fix=False)
args = parser.parse_args()

# TODO - trim strings

# fmt: off
JAK1_ALLOWED_CHARACTERS = [
    "_", # NOTE - not an actual underscore, adds a long space!
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "'", "!", "(", ")", "+", "-", ",", ".", "/", ":", "=", "<", ">", "*", "%", "?", "\"",
    "`", "ˇ", "¨", "º", "¡", "¿", "Æ", "Ç", "ß", "™", "、", " ", "Å", "Ø", "Ą", "Ę", "Ł", "Ż",
    "Ñ", "Ã", "Õ", "Á", "É", "Í", "Ó", "Ú", "Ć", "Ń", "Ś", "Ź", "Ő", "Ű", "Â", "Ê", "Î", "Ô", "Û", "À", "È", "Ì", "Ò", "Ù", "Ä", "Ë", "Ï", "Ö", "ö", "Ü", "Ė","Č","Š","Ž","Ų","Ū","Į",
    "海", "界", "学", "ワ", "ヲ", "ン", "岩", "旧", "空", "ヮ", "撃", "賢", "湖", "口", "行", "合", "士", "寺", "山", "者", "所", "書", "小", "沼", "上", "城", "場", "出", "闇", "遺", "黄", "屋", "下", "家", "火", "花", "レ", "ロ", "青", "・", "゛", "゜", "ー", "『", "』", "宝", "石", "赤", "跡", "川", "戦", "村", "隊", "台", "長", "鳥", "艇", "洞", "道", "発", "飛", "噴", "池", "中", "塔", "島", "部", "砲", "産", "眷", "力", "緑", "岸", "像", "谷", "心", "森", "水", "船", "世",
    "ぁ", "あ", "ぃ", "い", "ぅ", "う", "ぇ", "え", "ぉ", "お", "か", "き", "く", "け", "こ", "さ", "し", "す", "せ", "そ", "た", "ち", "っ", "つ", "て", "と", "な", "に", "ぬ", "ね", "の", "は", "ひ", "ふ", "へ", "ほ", "ま", "み", "む", "め", "も", "ゃ", "や", "ゅ", "ゆ", "ょ", "よ", "ら", "り", "る", "れ", "ろ", "ゎ", "わ", "を", "ん",
    "が", "ぎ", "ぐ", "げ", "ご", "ざ", "じ", "ず", "ぜ", "ぞ", "だ", "ぢ", "づ", "で", "ど", "ば", "び", "ぶ", "べ", "ぼ",
    "ぱ", "ぴ", "ぷ", "ぺ", "ぽ",
    "ァ", "ア", "ィ", "イ", "ゥ", "ウ", "ェ", "エ", "ォ", "オ", "カ", "キ", "ク", "ケ", "コ", "サ", "シ", "ス", "セ", "ソ", "タ", "チ", "ッ", "ツ", "テ", "ト", "ナ", "ニ", "ヌ", "ネ", "ノ", "ハ", "ヒ", "フ", "ヘ", "ホ", "マ", "ミ", "ム", "メ", "モ", "ャ", "ヤ", "ュ", "ユ", "ョ", "ヨ", "ラ", "リ", "ル",
    "ヴ", "ガ", "ギ", "グ", "ゲ", "ゴ", "ザ", "ジ", "ズ", "ゼ", "ゾ", "ダ", "ヂ", "ヅ", "デ", "ド", "バ", "ビ", "ブ", "ベ", "ボ",
    "パ", "ピ", "プ", "ペ", "ポ",
    "~", "Œ"
]

JAK1_ALLOWED_CODES = [
    "<TIL>",
    "<PAD_X>", "<PAD_TRIANGLE>", "<PAD_CIRCLE>", "<PAD_SQUARE>"
]

JAK1_AUTO_REPLACEMENTS = {
    "ª": "º",
    "\n": "",
    "’": "'",
    "·": "-",
    "–": "-",
    "​": "",
    "„": ",,",
    "”": "\"",
    "　": " ",
    "！": "!",
    "（": "(",
    "）": ")",
    "。": ".",
    "×": "x",
    "？": "?"
}

# TODO - check for korean text
JAK2_ALLOWED_CHARACTERS = [
    "_", # NOTE - not an actual underscore, adds a long space!
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "'", "!", "(", ")", "+", "-", ",", ".", "/", ":", "=", "<", ">", "*", "%", "?", "\"",
    "`", "ˇ", "¨", "º", "¡", "¿", "Æ", "Ç", "ß", "™", "、", " ", "Å", "Ø", "Ą", "Ę", "Ł", "Ż",
    "æ", "ø", "œ",
    "Ñ", "Ã", "Õ", "Á", "É", "Í", "Ó", "Ú", "Ć", "Ń", "Ś", "Ź", "ź", "Ő", "Ű", "Â", "Ê", "Î", "Ô", "Û", "À", "È", "Ì", "Ò", "Ù", "Ä", "Ë", "Ï", "ï", "Ö", "ö", "Ü", "Ė","Č","Š","Ž","Ų","Ū","Į",
    "ñ", "á", "é", "í", "ó", "ú", "â", "ê", "î", "ô", "û", "à", "è", "ì", "ò", "ù", "ä", "ö", "ü", "ś", "å", "õ", "ã", "ę", "ż", "ć", "ą", "ł", "ń", "ű", "ő", "ė","č","š","ž","ų","ū","į",
    "・", "゛", "゜", "ー", "『", "』",
    "海", "界", "学", "ワ", "ヲ", "ン", "岩", "旧", "空", "ヮ", "撃", "賢", "湖", "口", "行", "合", "士", "寺", "山", "者", "所", "書", "小", "沼", "上", "城", "場", "出", "闇", "遺", "黄", "屋", "下", "家", "火", "花", "レ", "ロ", "青", "宝", "石", "赤", "跡", "川", "戦", "村", "隊", "台", "長", "鳥", "艇", "洞", "道", "発", "飛", "噴", "池", "中", "塔", "島", "部", "砲", "産", "眷", "力", "緑", "岸", "像", "谷", "心", "森", "水", "船", "世",
    "位", "遺", "院", "映", "衛", "応", "下", "画", "解", "開", "外", "害", "蓋", "完", "換", "監", "間", "器", "記", "逆", "救", "金", "空", "掘", "警", "迎", "撃", "建", "源", "現", "言", "限", "個", "庫", "後", "語", "護", "交", "功", "向", "工", "攻", "溝", "行", "鉱", "降", "合", "告", "獄", "彩", "作", "山", "使", "始", "試", "字", "寺", "時", "示", "自", "式", "矢", "射", "者", "守", "手", "終", "週", "出", "所", "書", "勝", "章", "上", "乗", "場", "森", "進", "人", "水", "数", "制", "性", "成", "聖", "石", "跡", "先", "戦", "船", "選", "走", "送", "像", "造", "続", "対", "袋", "台", "弾", "地", "中", "敵", "転", "電", "塔", "頭", "動", "内", "日", "入", "年", "能", "廃", "排", "敗",
    "発", "反", "必", "表", "武", "壁", "墓", "放", "方", "砲", "妨", "北", "本", "幕", "無", "迷", "面", "戻", "紋", "薬", "輸", "勇", "友", "遊", "容", "要", "利", "了", "量", "力", "練", "連", "録", "話", "墟", "脱", "旗", "破", "壊", "全", "滅", "機", "仲", "渓", "谷", "優", "探", "部", "索", "前", "右", "左", "会", "高", "低", "押", "切", "替", "秒", "箱", "泳", "～",
    "闇", "以", "屋", "俺", "化", "界", "感", "気", "却", "曲", "継", "権", "見", "古", "好", "才", "士", "子", "次", "主", "種", "讐", "女", "小", "焼", "証", "神", "身", "寸", "世", "想", "退", "第", "着", "天", "倒", "到", "突", "爆", "番", "負", "復", "物", "眠", "予", "用", "落", "緑", "封", "印", "扉", "最", "刻", "足",
    "ぁ", "あ", "ぃ", "い", "ぅ", "う", "ぇ", "え", "ぉ", "お", "か", "き", "く", "け", "こ", "さ", "し", "す", "せ", "そ", "た", "ち", "っ", "つ", "て", "と", "な", "に", "ぬ", "ね", "の", "は", "ひ", "ふ", "へ", "ほ", "ま", "み", "む", "め", "も", "ゃ", "や", "ゅ", "ゆ", "ょ", "よ", "ら", "り", "る", "れ", "ろ", "ゎ", "わ", "を", "ん",
    "が", "ぎ", "ぐ", "げ", "ご", "ざ", "じ", "ず", "ぜ", "ぞ", "だ", "ぢ", "づ", "で", "ど", "ば", "び", "ぶ", "べ", "ぼ",
    "ぱ", "ぴ", "ぷ", "ぺ", "ぽ",
    "ァ", "ア", "ィ", "イ", "ゥ", "ウ", "ェ", "エ", "ォ", "オ", "カ", "キ", "ク", "ケ", "コ", "サ", "シ", "ス", "セ", "ソ", "タ", "チ", "ッ", "ツ", "テ", "ト", "ナ", "ニ", "ヌ", "ネ", "ノ", "ハ", "ヒ", "フ", "ヘ", "ホ", "マ", "ミ", "ム", "メ", "モ", "ャ", "ヤ", "ュ", "ユ", "ョ", "ヨ", "ラ", "リ", "ル",
    "ヴ", "ガ", "ギ", "グ", "ゲ", "ゴ", "ザ", "ジ", "ズ", "ゼ", "ゾ", "ダ", "ヂ", "ヅ", "デ", "ド", "バ", "ビ", "ブ", "ベ", "ボ",
    "パ", "ピ", "プ", "ペ", "ポ",
    "~", "Œ", "°", "ç"
]

JAK2_ALLOWED_CODES = [
    "<TIL>", "<SUPERSCRIPT_QUOTE>",
    "<PAD_X>", "<PAD_TRIANGLE>", "<PAD_CIRCLE>", "<PAD_SQUARE>", "<PAD_DPAD_UP>", "<PAD_DPAD_DOWN>", "<PAD_DPAD_ANY>", "<PAD_L1>", "<PAD_R1>", "<PAD_R2>", "<PAD_L2>", "<PAD_ANALOG_ANY>", "<PAD_ANALOG_LEFT_RIGHT>", "<PAD_ANALOG_UP_DOWN>", "<ICON_MISSION_COMPLETE>", "<ICON_MISSION_TODO>", "<FLAG_ITALIAN>", "<FLAG_SPAIN>", "<FLAG_GERMAN>", "<FLAG_FRANCE>", "<FLAG_UK>", "<FLAG_USA>", "<FLAG_KOREA>", "<FLAG_JAPAN>", "<FLAG_FINLAND>", "<FLAG_SWEDEN>", "<FLAG_DENMARK>", "<FLAG_NORWAY>", "<FLAG_ICELAND>"
]

JAK2_AUTO_REPLACEMENTS = {
    "ª": "º",
    "\n": "",
    "’": "'",
    "·": "-",
    "–": "-",
    "​": "",
    "„": ",,",
    "”": "\"",
    "　": " ",
    "！": "!",
    "（": "(",
    "）": ")",
    "〜": "~",
    "。": ".",
    "×": "x",
    "？": "?"
}
# fmt: on

invalid_characters_found = False

# TODO - reduce duplication


def jak1_is_allowed_code(pos, text):
    # Find any occurences of allowed codes in the string
    # if the position overlaps with these occurrences, it's allowed
    for code in JAK1_ALLOWED_CODES:
        for match in re.finditer(code, text):
            if pos >= match.start() and pos <= match.end():
                return match.end()
    return -1


def jak1_char_allowed(char):
    return char in JAK1_ALLOWED_CHARACTERS


def jak1_fix_character(char):
    # First let's try upper-casing it, if that's allowed, let's use that instead
    upper_case = char.upper()
    if jak1_char_allowed(upper_case):
        return upper_case
    if char in JAK1_AUTO_REPLACEMENTS:
        return JAK1_AUTO_REPLACEMENTS[char]
    return char


def jak1_replace_character(string, position, new_character):
    string_list = list(string)
    string_list[position] = new_character
    new_string = "".join(string_list)
    return new_string


def lint_jak1_characters(text):
    invalid_characters_found = False
    pos = 0
    while pos < len(text):
        character = text[pos]
        if not jak1_char_allowed(character):
            # Check to see if it's an allowed code
            code_end_pos = jak1_is_allowed_code(pos, text)
            if code_end_pos == -1:
                # If we are fixing instances, attempt to do so
                char_fixed = False
                if args.fix:
                    new_char = jak1_fix_character(character)
                    if new_char != character:
                        text = jak1_replace_character(text, pos, new_char)
                        char_fixed = True
                if not char_fixed:
                    print(
                        "Character '{}' not allowed - Found in {}".format(
                            character, text
                        )
                    )
                    # text = jak1_replace_character(text, pos, "?")
                    invalid_characters_found = True
                pos = pos + 1
            else:
                # advance to the end of the code and continue checking
                pos = code_end_pos
        else:
            pos = pos + 1
    return invalid_characters_found, text


# Iterate through the translations making sure there are no characters that are not allowed
text_files = glob.glob("./game/assets/jak1/text/*.json")

for text_file in text_files:
    print("Checking {}".format(text_file))
    with open(text_file, encoding="utf-8") as f:
        file_data = json.load(f)
    for id, text in file_data.items():
        invalid_chars_exist, new_text = lint_jak1_characters(text)
        if args.fix:
            file_data[id] = new_text
        if invalid_chars_exist:
            invalid_characters_found = True
    if args.fix:
        # save the modified file back out
        with open(text_file, "w", encoding="utf-8") as f:
            json.dump(file_data, f, indent=2, ensure_ascii=False)
            f.write("\n")

subtitle_files = glob.glob("./game/assets/jak1/subtitle/*lines*.json")

for subtitle_file in subtitle_files:
    print("Checking {}...".format(subtitle_file))
    with open(subtitle_file, encoding="utf-8") as f:
        file_data = json.load(f)
    # Check Speakers
    for id, text in file_data["speakers"].items():
        invalid_chars_exist, new_text = lint_jak1_characters(text)
        if args.fix and new_text != text:
            file_data["speakers"][id] = new_text
        if invalid_chars_exist:
            invalid_characters_found = True
    # Check Lines
    for id, lines in file_data["cutscenes"].items():
        for i, line in enumerate(lines):
            invalid_chars_exist, new_text = lint_jak1_characters(line)
            if args.fix and new_text != line:
                lines[i] = new_text
            if invalid_chars_exist:
                invalid_characters_found = True
    for id, lines in file_data["hints"].items():
        for i, line in enumerate(lines):
            invalid_chars_exist, new_text = lint_jak1_characters(line)
            if args.fix and new_text != line:
                lines[i] = new_text
            if invalid_chars_exist:
                invalid_characters_found = True
    if args.fix:
        # save the modified file back out
        with open(subtitle_file, "w", encoding="utf-8") as f:
            json.dump(file_data, f, indent=2, ensure_ascii=False)
            f.write("\n")


def jak2_is_allowed_code(pos, text):
    # Find any occurences of allowed codes in the string
    # if the position overlaps with these occurrences, it's allowed
    for code in JAK2_ALLOWED_CODES:
        for match in re.finditer(code, text):
            if pos >= match.start() and pos <= match.end():
                return match.end()
    return -1


def jak2_char_allowed(char):
    return char in JAK2_ALLOWED_CHARACTERS


def jak2_fix_character(char):
    if char in JAK2_AUTO_REPLACEMENTS:
        return JAK2_AUTO_REPLACEMENTS[char]
    return char


def jak2_replace_character(string, position, new_character):
    string_list = list(string)
    string_list[position] = new_character
    new_string = "".join(string_list)
    return new_string


def lint_jak2_characters(text):
    invalid_characters_found = False
    pos = 0
    while pos < len(text):
        character = text[pos]
        if not jak2_char_allowed(character):
            # Check to see if it's an allowed code
            code_end_pos = jak2_is_allowed_code(pos, text)
            if code_end_pos == -1:
                # If we are fixing instances, attempt to do so
                char_fixed = False
                if args.fix:
                    new_char = jak2_fix_character(character)
                    if new_char != character:
                        text = jak2_replace_character(text, pos, new_char)
                        char_fixed = True
                if not char_fixed:
                    print(
                        "Character '{}' not allowed - Found in {}".format(
                            character, text
                        )
                    )
                    # text = jak2_replace_character(text, pos, "?")
                    invalid_characters_found = True
                pos = pos + 1
            else:
                # advance to the end of the code and continue checking
                pos = code_end_pos
        else:
            pos = pos + 1
    return invalid_characters_found, text


# Iterate through the translations making sure there are no characters that are not allowed
text_files = glob.glob("./game/assets/jak2/text/*.json")

for text_file in text_files:
    print("Checking {}".format(text_file))
    with open(text_file, encoding="utf-8") as f:
        file_data = json.load(f)
    for id, text in file_data.items():
        invalid_chars_exist, new_text = lint_jak2_characters(text)
        if args.fix:
            file_data[id] = new_text
        if invalid_chars_exist:
            invalid_characters_found = True
    if args.fix:
        # save the modified file back out
        with open(text_file, "w", encoding="utf-8") as f:
            json.dump(file_data, f, indent=2, ensure_ascii=False)
            f.write("\n")

# subtitle_files = glob.glob("./game/assets/jak2/subtitle/*lines*.json")

# for subtitle_file in subtitle_files:
#     print("Checking {}...".format(subtitle_file))
#     with open(subtitle_file, encoding="utf-8") as f:
#         file_data = json.load(f)
#     # Check Speakers
#     for id, text in file_data["speakers"].items():
#         invalid_chars_exist, new_text = lint_jak2_characters(text)
#         if args.fix and new_text != text:
#             file_data["speakers"][id] = new_text
#         if invalid_chars_exist:
#             invalid_characters_found = True
#     # Check Lines
#     for id, lines in file_data["cutscenes"].items():
#         for i, line in enumerate(lines):
#             invalid_chars_exist, new_text = lint_jak2_characters(line)
#             if args.fix and new_text != line:
#                 lines[i] = new_text
#             if invalid_chars_exist:
#                 invalid_characters_found = True
#     for id, lines in file_data["hints"].items():
#         for i, line in enumerate(lines):
#             invalid_chars_exist, new_text = lint_jak2_characters(line)
#             if args.fix and new_text != line:
#                 lines[i] = new_text
#             if invalid_chars_exist:
#                 invalid_characters_found = True
#     if args.fix:
#         # save the modified file back out
#         with open(subtitle_file, "w", encoding="utf-8") as f:
#             json.dump(file_data, f, indent=2, ensure_ascii=False)
#             f.write("\n")

if invalid_characters_found:
    print("Invalid characters were found, see above")
    exit(1)
else:
    print("No invalid characters found!")
