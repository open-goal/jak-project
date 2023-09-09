import glob
import json
import re

import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--fix", action="store_true")
parser.set_defaults(fix=False)
args = parser.parse_args()

# TODO - trim strings
# TODO - jak 2 files

# fmt: off
JAK1_ALLOWED_CHARACTERS = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "'", "!", "(", ")", "+", "-", ",", ".", "/", ":", "=", "<", ">", "*", "%", "?", "\"",
    "`", "ˇ", "¨", "º", "¡", "¿", "Æ", "Ç", "ß", "™", "、", " ", "Å", "Ø", "Ą", "Ę", "Ł", "Ż",
    "Ñ", "Ã", "Õ", "Á", "É", "Í", "Ó", "Ú", "Ć", "Ń", "Ś", "Ź", "Ő", "Ű", "Â", "Ê", "Î", "Ô", "Û", "À", "È", "Ì", "Ò", "Ù", "Ä", "Ë", "Ï", "Ö", "ö", "Ü",
    "海", "界", "学", "ワ", "ヲ", "ン", "岩", "旧", "空", "ヮ", "撃", "賢", "湖", "口", "行", "合", "士", "寺", "山", "者", "所", "書", "小", "沼", "上", "城", "場", "出", "闇", "遺", "黄", "屋", "下", "家", "火", "花", "レ", "Œ", "ロ", "青", "・", "゛", "゜", "ー", "『", "』", "宝", "石", "赤", "跡", "川", "戦", "村", "隊", "台", "長", "鳥", "艇", "洞", "道", "発", "飛", "噴", "池", "中", "塔", "島", "部", "砲", "産", "眷", "力", "緑", "岸", "像", "谷", "心", "森", "水", "船", "世",
    "ぁ", "あ", "ぃ", "い", "ぅ", "う", "ぇ", "え", "ぉ", "お", "か", "き", "く", "け", "こ", "さ", "し", "す", "せ", "そ", "た", "ち", "っ", "つ", "て", "と", "な", "に", "ぬ", "ね", "の", "は", "ひ", "ふ", "へ", "ほ", "ま", "み", "む", "め", "も", "ゃ", "や", "ゅ", "ゆ", "ょ", "よ", "ら", "り", "る", "れ", "ろ", "ゎ", "わ", "を", "ん",
    "が", "ぎ", "ぐ", "げ", "ご", "ざ", "じ", "ず", "ぜ", "ぞ", "だ", "ぢ", "づ", "で", "ど", "ば", "び", "ぶ", "べ", "ぼ",
    "ぱ", "ぴ", "ぷ", "ぺ", "ぽ",
    "ァ", "ア", "ィ", "イ", "ゥ", "ウ", "ェ", "エ", "ォ", "オ", "カ", "キ", "ク", "ケ", "コ", "サ", "シ", "ス", "セ", "ソ", "タ", "チ", "ッ", "ツ", "テ", "ト", "ナ", "ニ", "ヌ", "ネ", "ノ", "ハ", "ヒ", "フ", "ヘ", "ホ", "マ", "ミ", "ム", "メ", "モ", "ャ", "ヤ", "ュ", "ユ", "ョ", "ヨ", "ラ", "リ", "ル",
    "ヴ", "ガ", "ギ", "グ", "ゲ", "ゴ", "ザ", "ジ", "ズ", "ゼ", "ゾ", "ダ", "ヂ", "ヅ", "デ", "ド", "バ", "ビ", "ブ", "ベ", "ボ",
    "パ", "ピ", "プ", "ペ", "ポ",
    "~"
]

JAK1_ALLOWED_CODES = [
    "<TIL>",
    "<PAD_X>", "<PAD_TRIANGLE>", "<PAD_CIRCLE>", "<PAD_SQUARE>"
]

JAK1_AUTO_REPLACEMENTS = {
    "ª": "º",
    "\n": "",
    "’": "'",
}
# fmt: on

invalid_characters_found = False


def is_allowed_code(pos, text):
    # Find any occurences of allowed codes in the string
    # if the position overlaps with these occurrences, it's allowed
    for code in JAK1_ALLOWED_CODES:
        for match in re.finditer(code, text):
            if pos >= match.start() and pos <= match.end():
                return match.end()
    return -1


def char_allowed(char):
    return char in JAK1_ALLOWED_CHARACTERS


def fix_character(char):
    # First let's try upper-casing it, if that's allowed, let's use that instead
    upper_case = char.upper()
    if char_allowed(upper_case):
        return upper_case
    if char in JAK1_AUTO_REPLACEMENTS:
        return JAK1_AUTO_REPLACEMENTS[char]
    return char


def replace_character(string, position, new_character):
    string_list = list(string)
    string_list[position] = new_character
    new_string = "".join(string_list)
    return new_string


def lint_jak1_characters(text):
    invalid_characters_found = False
    pos = 0
    while pos < len(text):
        character = text[pos]
        if not char_allowed(character):
            # Check to see if it's an allowed code
            code_end_pos = is_allowed_code(pos, text)
            if code_end_pos == -1:
                # If we are fixing instances, attempt to do so
                char_fixed = False
                if args.fix:
                    new_char = fix_character(character)
                    if new_char != character:
                        text = replace_character(text, pos, new_char)
                        char_fixed = True
                if not char_fixed:
                    print(
                        "Character '{}' not allowed - Found in {}".format(
                            character, text
                        )
                    )
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

if invalid_characters_found:
    print("Invalid characters were found, see above")
    exit(1)
else:
    print("No invalid characters found!")
