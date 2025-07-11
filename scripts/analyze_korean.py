# The idea is to parse through all of the game's korean strings and
# determine the glyph combinations that are used to construct the syllable blocks

# In terms of font setting, korean syllable blocks have 6 main configurations
# not all jamos are relevant for all these configurations, which reduces the complexity
# For example, the first 3 configurations involve no third jamo.

# 1 (2) - left -> right. For example 가
# 2 (2) - top -> middle. For example 고
# 3 (2) - left + right + middle.  For example 과
#
# 4 (3) - left + right + bottom (no combined middle). For example 갈
# 5 (3) - top down. For example 골
# 6 (3) - left + right + bottom with middle. For example 괄

# Thinking about the characters this way eliminates a ton of permutations
# and allows us to use existing glyphs to fill in the gaps

# The hope is that by analyzing all of the korean text, we can atleast find where every glyph is used
# and then any glyphs left over we can do manually.

# read in the `game_text.txt` file and extract all the korean strings
from itertools import product
from pprint import pprint
import json

with open(
    "../decompiler_out/jak2/assets/game_text.txt", mode="r", encoding="utf-8"
) as f:
    game_text_lines = f.readlines()

korean_lines = {}
i = 0
while i < len(game_text_lines):
    curr_line = game_text_lines[i].strip()
    if curr_line.startswith("(#x"):
        id = curr_line.split("(#x")[1]
        korean_lines[id] = game_text_lines[i + 7].strip().replace("\\c", ",0x")[2:-1]
    i = i + 1

# also parse subtitles
with open(
    "../decompiler_out/jak2/assets/subtitles_raw.txt", mode="r", encoding="utf-8"
) as f:
    subtitle_text_lines = f.readlines()
for line in subtitle_text_lines:
    parts = line.split("::")
    text = parts[2].replace('"', "").replace(" ", ",")
    # pad with 0s for single hex digits
    for c in [
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "a",
        "b",
        "c",
        "d",
        "e",
        "f",
    ]:
        text = text.replace(f"0x{c},", f"0x0{c},")
    key = f"{parts[0]}_{parts[1]}"
    korean_lines[key] = text[:-1]

print(f"Analyzing {len(korean_lines)} lines of korean text")

# we will fill up this structure, which will allow us to recreate / fill in the gaps in their korean encoding
jamo_combinations = {
    # jamo : 6 orientations
    # if the orientation is irrelvevant, null instead of a list
    # the reason we use lists is because multiple glyphs may be used to represent the same jamo
    # even in the same orientation, depending on the surrounding characters
    #
    # we will store these surrounding "context" characters to deduce a pattern at the end
    # ie. 0x06 is always used for ᄀ unless if it becomes before ᅦ then it uses 0x33
    #
    # Choseong (initial)
    "ᄀ": [[], [], [], [], [], []],
    "ᄁ": [[], [], [], [], [], []],
    "ᄂ": [[], [], [], [], [], []],
    "ᄃ": [[], [], [], [], [], []],
    "ᄄ": [[], [], [], [], [], []],
    "ᄅ": [[], [], [], [], [], []],
    "ᄆ": [[], [], [], [], [], []],
    "ᄇ": [[], [], [], [], [], []],
    "ᄈ": [[], [], [], [], [], []],
    "ᄉ": [[], [], [], [], [], []],
    "ᄊ": [[], [], [], [], [], []],
    "ᄋ": [[], [], [], [], [], []],
    "ᄌ": [[], [], [], [], [], []],
    "ᄍ": [[], [], [], [], [], []],
    "ᄎ": [[], [], [], [], [], []],
    "ᄏ": [[], [], [], [], [], []],
    "ᄐ": [[], [], [], [], [], []],
    "ᄑ": [[], [], [], [], [], []],
    "ᄒ": [[], [], [], [], [], []],
    # Jungseong (middle)
    # - verticals
    "ᅵ": [[], None, None, [], None, None],
    "ᅡ": [[], None, None, [], None, None],
    "ᅢ": [[], None, None, [], None, None],
    "ᅣ": [[], None, None, [], None, None],
    "ᅤ": [[], None, None, [], None, None],
    "ᅥ": [[], None, None, [], None, None],
    "ᅦ": [[], None, None, [], None, None],
    "ᅧ": [[], None, None, [], None, None],
    "ᅨ": [[], None, None, [], None, None],
    # - horizontals
    "ᅩ": [None, [], None, None, [], None],
    "ᅭ": [None, [], None, None, [], None],
    "ᅮ": [None, [], None, None, [], None],
    "ᅲ": [None, [], None, None, [], None],
    "ᅳ": [None, [], None, None, [], None],
    # - combinations
    "ᅪ": [None, None, [], None, None, []],
    "ᅫ": [None, None, [], None, None, []],
    "ᅬ": [None, None, [], None, None, []],
    "ᅯ": [None, None, [], None, None, []],
    "ᅰ": [None, None, [], None, None, []],
    "ᅱ": [None, None, [], None, None, []],
    "ᅴ": [None, None, [], None, None, []],
    # Jongseong (final)
    "ᆨ": [None, None, None, [], [], []],
    "ᆩ": [None, None, None, [], [], []],
    "ᆪ": [None, None, None, [], [], []],
    "ᆫ": [None, None, None, [], [], []],
    "ᆬ": [None, None, None, [], [], []],
    "ᆭ": [None, None, None, [], [], []],
    "ᆮ": [None, None, None, [], [], []],
    "ᆯ": [None, None, None, [], [], []],
    "ᆰ": [None, None, None, [], [], []],
    "ᆱ": [None, None, None, [], [], []],
    "ᆲ": [None, None, None, [], [], []],
    "ᆳ": [None, None, None, [], [], []],
    "ᆴ": [None, None, None, [], [], []],
    "ᆵ": [None, None, None, [], [], []],
    "ᆶ": [None, None, None, [], [], []],
    "ᆷ": [None, None, None, [], [], []],
    "ᆸ": [None, None, None, [], [], []],
    "ᆹ": [None, None, None, [], [], []],
    "ᆺ": [None, None, None, [], [], []],
    "ᆻ": [None, None, None, [], [], []],
    "ᆼ": [None, None, None, [], [], []],
    "ᆽ": [None, None, None, [], [], []],
    "ᆾ": [None, None, None, [], [], []],
    "ᆿ": [None, None, None, [], [], []],
    "ᇀ": [None, None, None, [], [], []],
    "ᇁ": [None, None, None, [], [], []],
    "ᇂ": [None, None, None, [], [], []],
}
# we will be reading byte strings from the game, these correspond to the font glyphs
# we can deduce all of these
jamo_glyph_mappings = {
    "0x06": "ᄀ",
    "0x07": "ᄁ",
    "0x08": "ᄂ",
    "0x09": "ᄃ",
    "0x0a": "ᄄ",
    "0x0b": "ᄅ",
    "0x0c": "ᄆ",
    "0x0d": "ᄇ",
    "0x0e": "ᄈ",
    "0x0f": "ᄋ",
    "0x10": "ᄌ",
    "0x11": "ᄍ",
    "0x12": "ᄏ",
    "0x13": "ᄐ",
    "0x14": "ᄑ",
    "0x15": "ᅡ",
    "0x16": "ᅣ",
    "0x17": "ᅵ",
    "0x18": "ᅥ",
    "0x19": "ᅧ",
    "0x1a": "ᅡ",
    "0x1b": "ᅣ",
    "0x1c": "ᅵ",
    "0x1d": "ᅥ",
    "0x1e": "ᅧ",
    "0x1f": "ᄂ",
    "0x20": "ᄉ",
    "0x21": "ᄊ",
    "0x22": "ᅥ",
    "0x23": "ᅧ",
    "0x24": "ᅥ",
    "0x25": "ᅧ",
    "0x26": "ᄃ",
    "0x27": "ᄄ",
    "0x28": "ᄅ",
    "0x29": "ᄐ",
    "0x2a": "ᄑ",
    "0x2b": "ᅧ",
    "0x2c": "ᅧ",
    "0x2d": "ᄎ",
    "0x2e": "ᄒ",
    "0x2f": "ᅥ",
    "0x30": "ᅧ",
    "0x31": "ᅥ",
    "0x32": "ᅧ",
    "0x33": "ᄀ",
    "0x34": "ᄁ",
    "0x35": "ᄂ",
    "0x36": "ᄃ",
    "0x37": "ᄄ",
    "0x38": "ᄅ",
    "0x39": "ᄆ",
    "0x3a": "ᄇ",
    "0x3b": "ᄈ",
    "0x3c": "ᄋ",
    "0x3d": "ᄌ",
    "0x3e": "ᄍ",
    "0x3f": "ᄏ",
    "0x40": "ᄐ",
    "0x41": "ᄑ",
    "0x42": "ᅢ",
    "0x43": "ᅤ",
    "0x44": "ᅦ",
    "0x45": "ᅨ",
    "0x46": "ᅢ",
    "0x47": "ᅤ",
    "0x48": "ᅦ",
    "0x49": "ᅨ",
    "0x4a": "ᄂ",
    "0x4b": "ᄉ",
    "0x4c": "ᄊ",
    "0x4d": "ᅦ",
    "0x4e": "ᅨ",
    "0x4f": "ᅦ",
    "0x50": "ᅨ",
    "0x51": "ᄃ",
    "0x52": "ᄄ",
    "0x53": "ᄅ",
    "0x54": "ᄐ",
    "0x55": "ᄑ",
    "0x56": "ᅨ",
    "0x57": "ᅨ",
    "0x58": "ᄎ",
    "0x59": "ᄒ",
    "0x5a": "ᅦ",
    "0x5b": "ᅨ",
    "0x5c": "ᅦ",
    "0x5d": "ᅨ",
    "0x5e": "ᄀ",
    "0x5f": "ᄏ",
    "0x60": "ᅩ",
    "0x61": "ᅭ",
    "0x62": "ᅩ",
    "0x63": "ᅭ",
    "0x64": ["ᄁ", "ᅫ"],
    "0x65": ["ᄁ", "ᅩ"],
    "0x66": ["ᄁ", "ᅩ"],
    "0x67": ["ᄁ", "ᅭ"],
    "0x68": "ᄁ",
    "0x69": "ᄂ",
    "0x6a": "ᅳ",
    "0x6b": "ᅮ",
    "0x6c": "ᅲ",
    "0x6d": "ᅳ",
    "0x6e": "ᅮ",
    "0x6f": "ᅲ",
    "0x70": "ᄃ",
    "0x71": "ᄄ",
    "0x72": "ᄅ",
    "0x73": "ᄆ",
    "0x74": "ᄇ",
    "0x75": "ᄈ",
    "0x76": "ᄉ",
    "0x77": "ᄊ",
    "0x78": "ᄋ",
    "0x79": "ᄌ",
    "0x7a": "ᄍ",
    "0x7b": "ᄎ",
    "0x7c": "ᄐ",
    "0x7d": "ᄑ",
    "0x7e": "ᄒ",
    "0x7f": "ᅩ",
    "0x80": "ᅭ",
    "0x81": "ᅳ",
    "0x82": "ᅩ",
    "0x83": "ᅭ",
    "0x84": "ᅳ",
    "0x85": "ᅮ",
    "0x86": "ᅲ",
    "0x87": "ᅮ",
    "0x88": "ᅲ",
    "0x89": ["ᅮ", "ᆫ"],
    "0x8a": ["ᅲ", "ᆫ"],
    "0x8b": "ᄀ",
    "0x8c": "ᄏ",
    "0x8d": "ᅩ",
    "0x8e": "ᅩ",
    "0x8f": ["ᄁ", "ᅩ"],
    "0x90": ["ᄁ", "ᅩ"],
    "0x91": "ᄁ",
    "0x92": "ᄂ",
    "0x93": "ᄃ",
    "0x94": "ᄄ",
    "0x95": "ᄅ",
    "0x96": "ᄆ",
    "0x97": "ᄇ",
    "0x98": "ᄈ",
    "0x99": "ᄉ",
    "0x9a": "ᄊ",
    "0x9b": "ᄋ",
    "0x9c": "ᄌ",
    "0x9d": "ᄍ",
    "0x9e": "ᄎ",
    "0x9f": "ᄐ",
    "0xa0": "ᄑ",
    "0xa1": "ᄒ",
    "0xa2": "ᅩ",
    "0xa3": "ᅳ",
    "0xa4": "ᅩ",
    "0xa5": "ᅳ",
    "0xa6": "ᅡ",
    "0xa7": "ᅵ",
    "0xa8": "ᅡ",
    "0xa9": "ᅵ",
    "0xaa": "ᅯ",
    "0xab": "ᅱ",
    "0xac": "ᅯ",
    "0xad": "ᅱ",
    "0xae": "ᄀ",
    "0xaf": "ᄏ",
    "0xb0": "ᅫ",
    "0xb1": "ᅫ",
    "0xb2": "ᄆ",
    "0xb3": "ᄁ",
    "0xb4": "ᄂ",
    "0xb5": "ᄃ",
    "0xb6": "ᄄ",
    "0xb7": "ᄅ",
    "0xb8": "ᄇ",
    "0xb9": "ᄉ",
    "0xba": "ᄊ",
    "0xbb": "ᄋ",
    "0xbc": "ᄌ",
    "0xbd": "ᄍ",
    "0xbe": "ᄎ",
    "0xbf": "ᄐ",
    "0xc0": "ᄒ",
    "0xc1": "ᅫ",
    "0xc2": "ᅫ",
    "0xc3": "ᅰ",
    "0xc4": "ᅰ",
    "0xc5": "ᆨ",
    "0xc6": "ᆩ",
    "0xc7": "ᆪ",
    "0xc8": "ᆫ",
    "0xc9": "ᆬ",
    "0xca": "ᆭ",
    "0xcb": "ᆮ",
    "0xcc": "ᆯ",
    "0xcd": "ᆰ",
    "0xce": "ᆱ",
    "0xcf": "ᆲ",
    "0xd0": "ᆴ",
    "0xd1": "ᆶ",
    "0xd2": "ᆷ",
    "0xd3": "ᆸ",
    "0xd4": "ᆹ",
    "0xd5": "ᆺ",
    "0xd6": "ᆻ",
    "0xd7": "ᆼ",
    "0xd8": "ᆽ",
    "0xd9": "ᆾ",
    "0xda": "ᆿ",
    "0xdb": "ᇀ",
    "0xdc": "ᇁ",
    "0xdd": "ᇂ",
    "0xde": "ᆨ",
    "0xdf": "ᆩ",
    "0xe0": "ᆪ",
    "0xe1": "ᆫ",
    "0xe2": "ᆭ",
    "0xe3": "ᆮ",
    "0xe4": "ᆯ",
    "0xe5": "ᆰ",
    "0xe6": "ᆱ",
    "0xe7": "ᆳ",
    "0xe8": "ᆴ",
    "0xe9": "ᆵ",
    "0xea": "ᆶ",
    "0xeb": "ᆷ",
    "0xec": "ᆸ",
    "0xed": "ᆺ",
    "0xee": "ᆼ",
    "0xef": "ᆽ",
    "0xf0": "ᆾ",
    "0xf1": "ᆿ",
    "0xf2": "ᇀ",
    "0xf3": "ᇁ",
    "0xf4": "ᇂ",
    "0xf5": "ᆨ",
    "0xf6": "ᆫ",
    "0xf7": "ᆯ",
    "0xf8": "ᆱ",
    "0xf9": "ᆷ",
    "0xfa": "ᆸ",
    "0xfb": "ᆺ",
    "0xfc": "ᆻ",
    "0xfd": "ᆼ",
    "0xfe": "ᆨ",
    "0xff": "ᆫ",
    "extra_0x86": "ᆯ",
    "extra_0x87": "ᆷ",
    "extra_0x88": "ᆸ",
    "extra_0x89": "ᆺ",
    "extra_0x8a": "ᆻ",
    "extra_0x8b": "ᆼ",
}
jamo_groupings = {
    "initial": [
        "ᄀ",
        "ᄁ",
        "ᄂ",
        "ᄃ",
        "ᄄ",
        "ᄅ",
        "ᄆ",
        "ᄇ",
        "ᄈ",
        "ᄉ",
        "ᄊ",
        "ᄋ",
        "ᄌ",
        "ᄍ",
        "ᄎ",
        "ᄏ",
        "ᄐ",
        "ᄑ",
        "ᄒ",
    ],
    "median": [
        "ᅡ",
        "ᅢ",
        "ᅣ",
        "ᅤ",
        "ᅥ",
        "ᅦ",
        "ᅧ",
        "ᅨ",
        "ᅩ",
        "ᅪ",
        "ᅫ",
        "ᅬ",
        "ᅭ",
        "ᅮ",
        "ᅯ",
        "ᅰ",
        "ᅱ",
        "ᅲ",
        "ᅳ",
        "ᅴ",
        "ᅵ",
    ],
    "final": [
        "ᆨ",
        "ᆩ",
        "ᆪ",
        "ᆫ",
        "ᆬ",
        "ᆭ",
        "ᆮ",
        "ᆯ",
        "ᆰ",
        "ᆱ",
        "ᆲ",
        "ᆳ",
        "ᆴ",
        "ᆵ",
        "ᆶ",
        "ᆷ",
        "ᆸ",
        "ᆹ",
        "ᆺ",
        "ᆻ",
        "ᆼ",
        "ᆽ",
        "ᆾ",
        "ᆿ",
        "ᇀ",
        "ᇁ",
        "ᇂ",
    ],
}
median_jamo_groupings = {
    "right": ["ᅡ", "ᅢ", "ᅣ", "ᅤ", "ᅥ", "ᅦ", "ᅧ", "ᅨ", "ᅵ"],
    "bottom": ["ᅩ", "ᅭ", "ᅮ", "ᅲ", "ᅳ"],
    "combined": ["ᅪ", "ᅫ", "ᅬ", "ᅯ", "ᅰ", "ᅱ", "ᅴ"],
}
median_combos = {
    "ᅪ": ["ᅩ", "ᅡ"],
    "ᅫ": ["ᅩ", "ᅢ"],
    "ᅬ": ["ᅩ", "ᅵ"],
    "ᅯ": ["ᅮ", "ᅥ"],
    "ᅰ": ["ᅮ", "ᅦ"],
    "ᅱ": ["ᅮ", "ᅵ"],
    "ᅴ": ["ᅳ", "ᅵ"],
}


def derive_syllable_block_info(glyph_list):
    jamos = []
    # iterate the glyphs, convert them into their mappings
    for glyph in glyph_list:
        if glyph not in jamo_glyph_mappings:
            print(f"{glyph} not in mapping dictionary, fix it")
            exit(1)
        mapping = jamo_glyph_mappings[glyph]
        # TODO - ugly for figuring out what glyphs by jamo!
        if isinstance(mapping, list):
            # there are a few select glyphs that are multiple jamos
            for jamo in mapping:
                jamos.append([jamo, glyph])
        else:
            jamos.append([mapping, glyph])
    # Associate each jamo with it's initial/median/final grouping
    jamo_info = []
    found_medians = []
    for jamo_and_glyph in jamos:
        jamo = jamo_and_glyph[0]
        glyph = jamo_and_glyph[1]
        for [grouping, jamos_in_group] in jamo_groupings.items():
            if jamo in jamos_in_group:
                jamo_grouping = grouping
                break
        if jamo_grouping == "median":
            found_medians.append([jamo, glyph])
        jamo_info.append({"jamo": jamo, "grouping": jamo_grouping, "glyph": glyph})
    if len(found_medians) > 2:
        print(f"found more than 2 median vowels in {jamo_info}")
        exit(1)
    # Consolidate median vowels, as jak typically typically draws them as a combination of two
    # glyphs
    if len(found_medians) > 1:
        combined_median = None
        combined_glyphs = None
        for [vowel, vowel_parts] in median_combos.items():
            if (
                found_medians[0][0] in vowel_parts
                and found_medians[1][0] in vowel_parts
            ):
                combined_median = vowel
                combined_glyphs = [found_medians[0][1], found_medians[1][1]]
                break
        if combined_median == None:
            print(f"unable to combine median in {jamo_info}")
            exit(1)
        new_jamo_info = []
        skip_rest = False
        for info in jamo_info:
            if info["grouping"] != "median":
                new_jamo_info.append(info)
            elif not skip_rest:
                new_jamo_info.append(
                    {
                        "jamo": combined_median,
                        "grouping": "median",
                        "glyph": combined_glyphs,
                    }
                )
                skip_rest = True
        jamo_info = new_jamo_info
    # Now we can consolidate median vowels and determine the orientation
    if len(jamo_info) == 2:
        for [grouping, jamos_in_group] in median_jamo_groupings.items():
            if jamo_info[1]["jamo"] in jamos_in_group:
                median_group = grouping
                break
        if median_group == "right":
            writing_orientation = 0
        elif median_group == "bottom":
            writing_orientation = 1
        elif median_group == "combined":
            writing_orientation = 2
        else:
            print(f"couldnt figure out median group for {jamo_info}")
            exit(1)
    elif len(jamo_info) == 3:
        for [grouping, jamos_in_group] in median_jamo_groupings.items():
            if jamo_info[1]["jamo"] in jamos_in_group:
                median_group = grouping
                break
        if median_group == "right":
            writing_orientation = 3
        elif median_group == "bottom":
            writing_orientation = 4
        elif median_group == "combined":
            writing_orientation = 5
        else:
            print(f"couldnt figure out median group for {jamo_info}")
            exit(1)
    else:
        print(f"unhandled jamo configuration {jamo_info}")
        exit(1)
    return {"writingOrientation": writing_orientation, "jamos": jamo_info}


# finally start going through the real text to figure out the mappings
total_syllable_blocks = 0
for [id, game_text_line] in korean_lines.items():
    # print()
    # print(game_text_line)
    # split the bytes into characters, sound the alarm if we see a `0x05`
    # NOTE - hopefully this is not a hack (seems like the font textures dont start until 0x6...how conveniant!)
    game_text_line = game_text_line.replace("0x05,", "extra_")
    text_bytes = game_text_line.split(",")
    syllable_blocks = []
    i = 0
    while i < len(text_bytes):
        curr_byte = text_bytes[i]
        if curr_byte == "0x04":
            total_syllable_blocks = total_syllable_blocks + 1
            expected_num_glyphs = int(text_bytes[i + 1], 16)
            syllable_blocks.append(
                {
                    "numGlyphs": expected_num_glyphs,
                    "rawGlyphs": text_bytes[i + 2 : i + 2 + expected_num_glyphs],
                }
            )
            i = i + 2 + expected_num_glyphs
            continue
        i = i + 1
    # now we will inspect the choice of glyphs (which are individual jamo or jamo combinations)
    # to determine the jamo and the writing orientation
    for block in syllable_blocks:
        jamo_info = derive_syllable_block_info(block["rawGlyphs"])
        block["jamos"] = jamo_info["jamos"]
        block["writingOrientation"] = jamo_info["writingOrientation"]

    # pprint(syllable_blocks)

    # The (almost) final step, store this information in our big jamo combination
    # "database"
    #
    # We now effectively have an encoding, and we can process that to further refine it and
    # see what we have to do manually
    for block in syllable_blocks:
        writing_orientation = block["writingOrientation"]
        for jamo in block["jamos"]:
            jamo_entry = jamo_combinations[jamo["jamo"]]
            if jamo_entry[writing_orientation] == None:
                print(f"something is very wrong with {block}")
                exit(1)
            new_entry = {"glyph": jamo["glyph"], "context": block["jamos"]}
            if new_entry not in jamo_entry[writing_orientation]:
                jamo_entry[writing_orientation].append(new_entry)

# Print some stats before finalizing the result
empty_cells = 0
glyph_list = set(jamo_glyph_mappings.keys())
for [jamo, orientations] in jamo_combinations.items():
    for orientation in orientations:
        if orientation is not None:
            if len(orientation) == 0:
                empty_cells = empty_cells + 1
            for entry in orientation:
                if isinstance(entry["glyph"], list):
                    for glyph in entry["glyph"]:
                        glyph_list.discard(glyph)
                else:
                    glyph_list.discard(entry["glyph"])

print()
print(f"Analyzed {total_syllable_blocks} syllable blocks")
print(f"{empty_cells} empty jamo cells\n")
print(f"Did not see {len(glyph_list)} out of {len(jamo_glyph_mappings.keys())} glyphs:")

# with open("./jamo-db-before.json", mode="w", encoding="utf-8") as f:
#     f.write(json.dumps(jamo_combinations, indent=2))

def format_alternative(curr_jamo, curr_glyph, full_glyph_context):
    # Make a string key that represents the unicode jamos with a <GLYPH> placeholder to represent
    # the jamo we are dealing with
    # And the value is the glyph itself that gets used to draw this combination of jamos
    key_parts = []
    for glyph in full_glyph_context:
        if curr_jamo == glyph["jamo"]:
            key_parts.append("<G>")
        else:
            key_parts.append(glyph["jamo"])
    formatted_curr_glyph = curr_glyph
    if isinstance(curr_glyph, list):
        formatted_curr_glyph = ",".join(curr_glyph)
    return [",".join(key_parts), formatted_curr_glyph]


# Enumerate through the db, and consolidate duplicates / find the most common
# jamo for each position
for [jamo, orientations] in jamo_combinations.items():
    for [index, orientation] in enumerate(orientations):
        if orientation is not None:
            result = {"defaultGlyph": "", "alternatives": {}}
            glyph_frequencies = {}
            alternatives = {}
            if len(orientation) == 0:
                empty_cells = empty_cells + 1
                continue
            for entry in orientation:
                glyph_key = entry["glyph"]
                if isinstance(entry["glyph"], list):
                    glyph_key = ",".join(entry["glyph"])
                if glyph_key not in glyph_frequencies:
                    glyph_frequencies[glyph_key] = 0
                glyph_frequencies[glyph_key] = glyph_frequencies[glyph_key] + 1

                if glyph_key not in alternatives:
                    alternatives[glyph_key] = []
                alternatives[glyph_key].append(
                    format_alternative(jamo, entry["glyph"], entry["context"])
                )
            # Consolidate
            most_common_glyph = ""
            most_common_glyph_times = -1
            for [glyph, freq] in glyph_frequencies.items():
                if freq > most_common_glyph_times:
                    most_common_glyph_times = freq
                    most_common_glyph = glyph
            result["defaultGlyph"] = most_common_glyph
            # TODO - handle if this is multiple glyphs
            del alternatives[most_common_glyph]
            # Flatten alternatives
            for [glyph, alternatives] in alternatives.items():
                for alternative in alternatives:
                    result["alternatives"][alternative[0]] = alternative[1]
            # Overwrite the db value
            jamo_combinations[jamo][index] = result

# These are found MANUALLY by iterating through all combinations and finding alternatives
# for jamo combinations to be legible
manual_encoding_additions = {
    # Choseong (initial)
    "ᄀ": [
        [],
        [],
        ["0xb3:<G>,ᅫ"],
        ["0x33:<G>,ᅤ,*", "0x33:<G>,ᅦ,*", "0x33:<G>,ᅨ,*"],
        [],
        ["0xae:<G>,ᅴ,*"],
    ],
    "ᄁ": [
        ["!0x34", "0x07:<G>,ᅵ"],
        ["0x67:<G>,ᅭ"],
        [],
        ["!0x34"],
        ["!0x65", "0x68:<G>,ᅭ,*", "0x68:<G>,ᅮ,*", "0x68:<G>,ᅲ,*", "0x68:<G>,ᅳ,*"],
        [
            "!0x91",
            "0x8f:<G>,ᅪ,*",
            "0x64:<G>,ᅫ,*",
            "0xb3:<G>,ᅰ,*",
            "0x8f:<G>,ᅬ,*",
        ],
    ],
    "ᄂ": [
        ["0x1f:<G>,ᅡ", "0x1f:<G>,ᅣ", "0x4a:<G>,ᅨ", "0x35:<G>,ᅤ"],
        [],
        ["0xb4:<G>,ᅫ", "0xb4:<G>,ᅰ"],
        [
            "0x35:<G>,ᅢ,*",
            "0x35:<G>,ᅤ,*",
            "0x35:<G>,ᅦ,*",
            "0x1f:<G>,ᅧ,*",
            "0x1f:<G>,ᅥ,*",
            "0x4a:<G>,ᅨ,*",
        ],
        [],
        ["0xb4:<G>,ᅪ,*", "0xb4:<G>,ᅫ,*", "0xb4:<G>,ᅰ,*"],
    ],
    "ᄃ": [
        ["0x26:<G>,ᅡ", "0x36:<G>,ᅤ", "0x26:<G>,ᅣ", "0x36:<G>,ᅧ", "0x51:<G>,ᅨ"],
        [],
        ["0xb5:<G>,ᅪ", "0xb5:<G>,ᅰ"],
        [
            "0x36:<G>,ᅢ,*",
            "0x36:<G>,ᅤ,*",
            "0x51:<G>,ᅦ,*",
            "0x26:<G>,ᅧ,*",
            "0x51:<G>,ᅨ,*",
        ],
        [],
        ["0xb5:<G>,ᅪ,*", "0xb5:<G>,ᅫ,*", "0xb5:<G>,ᅰ,*"],
    ],
    "ᄄ": [
        [
            "0x27:<G>,ᅡ",
            "0x52:<G>,ᅢ",
            "0x52:<G>,ᅤ",
            "0x27:<G>,ᅣ",
            "0x27:<G>,ᅧ",
            "0x52:<G>,ᅨ",
            "0x27:<G>,ᅵ",
        ],
        [],
        ["0xb6:<G>,ᅪ", "0xb6:<G>,ᅫ", "0xb6:<G>,ᅰ"],
        [
            "!0x0a",
            "0x52:<G>,ᅢ,*",
            "0x52:<G>,ᅣ,*",
            "0x52:<G>,ᅤ,*",
            "0x52:<G>,ᅦ,*",
            "0x52:<G>,ᅧ,*",
            "0x52:<G>,ᅨ,*",
        ],
        [],
        ["0xb6:<G>,ᅪ,*", "0xb6:<G>,ᅫ,*", "0xb6:<G>,ᅰ,*"],
    ],
    "ᄅ": [
        ["0x0b:<G>,ᅣ", "0x38:<G>,ᅤ"],
        [],
        ["0xb7:<G>,ᅪ", "0xb7:<G>,ᅫ", "0xb7:<G>,ᅰ"],
        [
            "0x0b:<G>,ᅵ,*",
            "0x0b:<G>,ᅡ,*",
            "0x38:<G>,ᅢ,*",
            "0x38:<G>,ᅤ,*",
            "0x53:<G>,ᅦ,*",
            "0x53:<G>,ᅨ,*",
        ],
        [],
        ["0x95", "0xb7:<G>,ᅪ,*", "0xb7:<G>,ᅫ,*", "0xb7:<G>,ᅰ,*"],
    ],
    "ᄆ": [
        ["0x39:<G>,ᅤ", "0x39:<G>,ᅨ"],
        [],
        ["0xb2:<G>,ᅪ", "0xb2:<G>,ᅫ", "0xb2:<G>,ᅰ"],
        ["0x39:<G>,ᅢ,*", "0x39:<G>,ᅤ,*", "0x39:<G>,ᅦ,*", "0x39:<G>,ᅨ,*"],
        [],
        ["0xb2:<G>,ᅪ,*", "0xb2:<G>,ᅫ,*", "0xb2:<G>,ᅰ,*"],
    ],
    "ᄇ": [
        ["0x3a:<G>,ᅤ", "0x3a:<G>,ᅨ"],
        [],
        ["0xb8:<G>,ᅪ", "0xb8:<G>,ᅫ", "0xb8:<G>,ᅰ"],
        ["0x3a:<G>,ᅢ,*", "0x3a:<G>,ᅤ,*", "0x3a:<G>,ᅦ,*", "0x3a:<G>,ᅨ,*"],
        [],
        ["0xb8:<G>,ᅪ,*", "0xb8:<G>,ᅫ,*", "0xb8:<G>,ᅰ,*"],
    ],
    "ᄈ": [
        ["!0x3b", "0x0e:<G>,ᅥ", "0x0e:<G>,ᅵ"],
        [],
        ["0x98"],
        [
            "0x3b:<G>,ᅢ,*",
            "0x3b:<G>,ᅣ,*",
            "0x3b:<G>,ᅤ,*",
            "0x3b:<G>,ᅦ,*",
            "0x3b:<G>,ᅧ,*",
            "0x3b:<G>,ᅨ,*",
        ],
        [],
        ["0x98"],
    ],
    "ᄉ": [
        ["0x4b:<G>,ᅤ", "0x4b:<G>,ᅨ"],
        [],
        ["0xb9:<G>,ᅪ", "0xb9:<G>,ᅰ"],
        ["0x4b:<G>,ᅢ,*", "0x4b:<G>,ᅤ,*", "0x4b:<G>,ᅦ,*", "0x4b:<G>,ᅨ,*"],
        [],
        ["0xb9:<G>,ᅪ,*", "0xb9:<G>,ᅫ,*", "0xb9:<G>,ᅰ,*"],
    ],
    "ᄊ": [
        [
            "0x4c:<G>,ᅢ",
            "0x4c:<G>,ᅤ",
            "0x4c:<G>,ᅥ",
            "0x4c:<G>,ᅧ",
            "0x4c:<G>,ᅨ",
        ],
        [],
        ["0xba:<G>,ᅪ", "0xba:<G>,ᅫ", "0xba:<G>,ᅰ"],
        [
            "0x4c:<G>,ᅢ,*",
            "0x4c:<G>,ᅣ,*",
            "0x4c:<G>,ᅤ,*",
            "0x4c:<G>,ᅦ,*",
            "0x4c:<G>,ᅧ,*",
            "0x4c:<G>,ᅨ,*",
        ],
        [],
        ["0x9a", "0xba:<G>,ᅪ,*", "0xba:<G>,ᅫ,*", "0xba:<G>,ᅰ,*"],
    ],
    "ᄋ": [
        [],
        [],
        ["0xbb:<G>,ᅪ"],
        ["0x3c:<G>,ᅢ,*", "0x3c:<G>,ᅤ,*", "0x3c:<G>,ᅦ,*", "0x3c:<G>,ᅨ,*"],
        [],
        ["0xbb:<G>,ᅪ,*", "0xbb:<G>,ᅫ,*", "0xbb:<G>,ᅰ,*"],
    ],
    "ᄌ": [
        ["0x3d:<G>,ᅨ"],
        [],
        ["0xbc:<G>,ᅪ", "0xbc:<G>,ᅫ", "0xbc:<G>,ᅰ"],
        ["0x3d:<G>,ᅢ,*", "0x3d:<G>,ᅤ,*", "0x3d:<G>,ᅦ,*", "0x3d:<G>,ᅨ,*"],
        [],
        ["0xbc:<G>,ᅪ,*", "0xbc:<G>,ᅫ,*", "0xbc:<G>,ᅰ,*"],
    ],
    "ᄍ": [
        ["!0x3e", "0x11:<G>,ᅵ"],
        [],
        ["0xbd:<G>,ᅪ", "0xbd:<G>,ᅫ", "0xbd:<G>,ᅰ"],
        [
            "0x3e:<G>,ᅢ,*",
            "0x3e:<G>,ᅣ,*",
            "0x3e:<G>,ᅤ,*",
            "0x3e:<G>,ᅦ,*",
            "0x3e:<G>,ᅧ,*",
            "0x3e:<G>,ᅨ,*",
            "0x3e:<G>,ᅵ,*",
        ],
        [],
        ["0xbd:<G>,ᅪ,*", "0xbd:<G>,ᅫ,*", "0xbd:<G>,ᅰ,*"],
    ],
    "ᄎ": [
        ["0x58:<G>,ᅤ", "0x58:<G>,ᅨ"],
        [],
        ["0xbe:<G>,ᅪ", "0xbe:<G>,ᅫ", "0xbe:<G>,ᅰ"],
        ["0x58:<G>,ᅢ,*", "0x58:<G>,ᅤ,*", "0x58:<G>,ᅦ,*", "0x58:<G>,ᅨ,*"],
        [],
        ["0x9e", "0xbe:<G>,ᅪ,*", "0xbe:<G>,ᅫ,*", "0xbe:<G>,ᅰ,*"],
    ],
    "ᄏ": [
        ["0x3f:<G>,ᅤ", "0x3f:<G>,ᅨ"],
        [],
        ["0xaf:<G>,ᅪ", "0xaf:<G>,ᅫ", "0xaf:<G>,ᅰ"],
        ["0x3f:<G>,ᅢ,*", "0x3f:<G>,ᅤ,*", "0x3f:<G>,ᅦ,*", "0x3f:<G>,ᅨ,*"],
        [],
        ["0xaf:<G>,ᅪ,*", "0xaf:<G>,ᅫ,*", "0xaf:<G>,ᅰ,*"],
    ],
    "ᄐ": [
        ["0x13:<G>,ᅣ", "0x40:<G>,ᅤ", "0x54:<G>,ᅨ"],
        [],
        ["0xbf:<G>,ᅪ", "0xbf:<G>,ᅫ", "0xbf:<G>,ᅰ"],
        [
            "0x40:<G>,ᅢ,*",
            "0x40:<G>,ᅤ,*",
            "0x54:<G>,ᅦ,*",
            "0x40:<G>,ᅧ,*",
            "0x54:<G>,ᅨ,*",
        ],
        [],
        ["0x9f", "0xbf:<G>,ᅪ,*", "0xbf:<G>,ᅫ,*", "0xbf:<G>,ᅰ,*"],
    ],
    "ᄑ": [
        [
            "0x2a:<G>,ᅡ",
            "0x41:<G>,ᅤ",
            "0x2a:<G>,ᅣ",
            "0x2a:<G>,ᅧ",
        ],
        [],
        ["0xa0"],
        [
            "0x41:<G>,ᅢ,*",
            "0x41:<G>,ᅣ,*",
            "0x41:<G>,ᅤ,*",
            "0x2a:<G>,ᅥ,*",
            "0x55:<G>,ᅦ,*",
            "0x41:<G>,ᅧ,*",
            "0x55:<G>,ᅨ,*",
        ],
        [],
        ["0xa0"],
    ],
    "ᄒ": [
        ["0x59:<G>,ᅤ"],
        [],
        ["0xc0:<G>,ᅪ", "0xc0:<G>,ᅫ", "0xc0:<G>,ᅰ"],
        [
            "0x59:<G>,ᅡ,*",
            "0x59:<G>,ᅢ,*",
            "0x59:<G>,ᅣ,*",
            "0x59:<G>,ᅤ,*",
            "0x59:<G>,ᅦ,*",
            "0x59:<G>,ᅧ,*",
            "0x59:<G>,ᅨ,*",
        ],
        [],
        ["0xc0:<G>,ᅪ,*", "0xc0:<G>,ᅫ,*", "0xc0:<G>,ᅰ,*"],
    ],
    # Jungseong (middle)
    "ᅡ": [[], None, None, ["0x1a:*,<G>,ᆫ"], None, None],
    "ᅢ": [[], None, None, ["0x46:*,<G>,ᆫ"], None, None],
    "ᅣ": [
        [],
        None,
        None,
        [],
        None,
        None,
    ],
    "ᅤ": [[], None, None, ["0x43", "0x47:*,<G>,ᆫ"], None, None],
    "ᅥ": [
        [],
        None,
        None,
        ["0x2f:ᄎ,<G>,*", "0x2f:ᄒ,<G>,*", "0x1d:*[^ᄎ;ᄐ;ᄒ],<G>,ᆫ"],
        None,
        None,
    ],
    "ᅦ": [[], None, None, ["0x5a:ᄒ,<G>,*", "0x48:*,<G>,ᆫ"], None, None],
    "ᅧ": [
        [],
        None,
        None,
        ["0x25:ᄂ,<G>,*", "0x1e:ᄎ,<G>,*", "0x30:ᄒ,<G>,*", "0x1e:*,<G>,ᆫ"],
        None,
        None,
    ],
    "ᅨ": [
        [
            "0x50:ᄂ,<G>",
            "0x50:ᄃ,<G>",
            "0x5d:ᄄ,<G>",
            "0x50:ᄅ,<G>",
            "0x50:ᄆ,<G>",
            "0x50:ᄈ,<G>",
        ],
        None,
        None,
        ["0x49:*,<G>,ᆫ"],
        None,
        None,
    ],
    "ᅩ": [
        None,
        [
            "0x62:ᄂ,<G>",
            "0x62:ᄄ,<G>",
            "0x62:ᄅ,<G>",
            "0x62:ᄋ,<G>",
            "0x62:ᄐ,<G>",
            "0x62:ᄒ,<G>",
        ],
        None,
        None,
        ["!0x82", "0x82:*,<G>,*"],
        None,
    ],
    "ᅪ": [
        None,
        None,
        ["0x8e,0xa8:ᄁ,<G>"],
        None,
        None,
        [
            "0x8e,0xa6:ᄀ,<G>,*",
            "0x8e,0xa8:ᄀ,<G>,ᆫ",
            "0x8f,0xa6:ᄁ,<G>,*",
            "0x8f,0xa8:ᄁ,<G>,ᆫ",
        ],
    ],
    "ᅫ": [
        None,
        None,
        [],
        None,
        None,
        [
            "!0xc1",
            "0xc2:*,<G>,ᆫ",
            "0x8e,0x42:ᄀ,<G>,*",
            "0x8e,0x46:ᄀ,<G>,ᆫ",
            "0x64:ᄁ,<G>,*",
            "0x64:ᄁ,<G>,ᆫ",
        ],
    ],
    "ᅬ": [
        None,
        None,
        ["0x8e,0xa9:ᄁ,<G>"],
        None,
        None,
        [
            "0xa2,0xa9:*,<G>,ᆫ",
            "0x8e,0xa7:ᄀ,<G>,*",
            "0x8e,0xa9:ᄀ,<G>,ᆫ",
            "0x8f,0xa7:ᄁ,<G>,*",
            "0x8f,0xa9:ᄁ,<G>,ᆫ",
        ],
    ],
    "ᅭ": [
        None,
        [
            "0x61:ᄀ,<G>",
            "0x67:ᄁ,<G>",
            "0x63:ᄂ,<G>",
            "0x63:ᄃ,<G>",
            "0x63:ᄄ,<G>",
            "0x63:ᄅ,<G>",
            "0x63:ᄈ,<G>",
            "0x63:ᄊ,<G>",
            "0x63:ᄋ,<G>",
            "0x63:ᄏ,<G>",
            "0x63:ᄐ,<G>",
            "0x63:ᄒ,<G>",
        ],
        None,
        None,
        ["!0x83"],
        None,
    ],
    "ᅮ": [None, ["0x6e:ᄒ,<G>"], None, None, ["0x85:*,<G>,*", "0x89:*,<G>,ᆫ"], None],
    "ᅯ": [None, None, [], None, None, ["0xaa:*,<G>,*", "0xac:*,<G>,ᆫ"]],
    "ᅰ": [None, None, [], None, None, ["0xc4:*,<G>,ᆫ"]],
    "ᅱ": [None, None, [], None, None, ["0xad:*,<G>,ᆫ"]],
    "ᅲ": [
        None,
        [
            "0x6f:ᄂ,<G>",
            "0x6f:ᄄ,<G>",
            "0x6f:ᄅ,<G>",
            "0x6f:ᄆ,<G>",
            "0x6f:ᄇ,<G>",
            "0x6f:ᄈ,<G>",
            "0x6f:ᄉ,<G>",
            "0x6f:ᄊ,<G>",
            "0x6f:ᄋ,<G>",
            "0x6f:ᄌ,<G>",
            "0x6f:ᄍ,<G>",
            "0x6f:ᄎ,<G>",
            "0x6f:ᄏ,<G>",
            "0x6f:ᄐ,<G>",
            "0x6f:ᄑ,<G>",
            "0x6f:ᄒ,<G>",
        ],
        None,
        None,
        ["0x8a:*,<G>,ᆫ"],
        None,
    ],
    "ᅳ": [None, ["0x6d:ᄐ,<G>", "0x6d:ᄒ,<G>"], None, None, ["0x84:*,<G>,*"], None],
    "ᅴ": [None, None, [], None, None, ["!0xa3,0xa7", "0xa3,0xa9:*,<G>,ᆫ"]],
    "ᅵ": [[], None, None, ["0x1c:*,<G>,ᆫ"], None, None],
    # Jongseong (final)
    "ᆨ": [None, None, None, [], [], ["!0xfe"]],
    "ᆩ": [None, None, None, [], [], ["0xdf"]],
    "ᆪ": [None, None, None, ["0xc7"], [], ["0xe0"]],
    "ᆫ": [None, None, None, [], ["0xe1:*,*,<G>"], ["!0xff"]],
    "ᆬ": [None, None, None, [], ["0xc9"], ["0xc9"]],
    "ᆭ": [None, None, None, [], ["0xe2"], ["0xe2"]],
    "ᆮ": [None, None, None, [], [], ["0xe3"]],
    "ᆯ": [None, None, None, [], [], ["!extra_0x86:*,*,<G>"]],
    "ᆰ": [None, None, None, [], [], ["0xe5"]],
    "ᆱ": [None, None, None, [], [], ["0xf8"]],
    "ᆲ": [None, None, None, [], ["0xcf"], ["0xcf"]],
    "ᆳ": [None, None, None, ["0xe7"], ["0xe7"], ["0xe7"]],
    "ᆴ": [None, None, None, ["0xd0"], ["0xe8"], ["0xe8"]],
    "ᆵ": [None, None, None, ["0xe9"], ["0xe9"], ["0xe9"]],
    "ᆶ": [None, None, None, [], [], ["0xea"]],
    "ᆷ": [None, None, None, [], [], ["extra_0x87", "extra_0x87:*,*,<G>"]],
    "ᆸ": [None, None, None, [], [], ["extra_0x88:*,*,<G>"]],
    "ᆹ": [None, None, None, [], ["0xd4"], ["0xd4"]],
    "ᆺ": [None, None, None, [], [], ["extra_0x89:*,*,<G>"]],
    "ᆻ": [None, None, None, [], ["0xfc"], ["extra_0x8a:*,*,<G>"]],
    "ᆼ": [None, None, None, [], [], ["extra_0x8b:*,*,<G>"]],
    "ᆽ": [None, None, None, [], [], ["0xef"]],
    "ᆾ": [None, None, None, [], ["0xf0"], ["0xf0"]],
    "ᆿ": [None, None, None, ["0xda"], ["0xf1"], ["0xf1"]],
    "ᇀ": [None, None, None, [], [], ["0xf2"]],
    "ᇁ": [None, None, None, [], [], ["0xf3"]],
    "ᇂ": [None, None, None, [], [], ["0xf4"]],
}

# Print the results
with open("./jamo-db.json", mode="w", encoding="utf-8") as f:
    f.write(json.dumps(jamo_combinations, indent=2))

# Fill in the rest of the encoding table with manually identified alternatives / additions
# Most of these are additions, but some override the original encoding (because it looked terrible)
#
# These are provided in a specific formats:
# - 0x01:<G>,ᅤ -- means use 0x01 glyph for drawing the jamo (in position shown, in this case, only before ᅤ
# - 0x01:<G>,* -- means use 0x01 no matter what comes after
# - 0x01:<G>,*[^ᄎ;ᄐ] -- means use 0x01 no matter what comes after (except ᄐ and ᄎ)
# - 0x01,0x02:<G>,* -- sometimes a jamo requires multiple glyphs
# - !0x01 -- means to override the default glyph, this is used very rarely
# - 0x01 -- means to set the default -- some jamos never were used at all.  If a default is already set, this should throw an error
#
# Note that all of these changes will override any existing alternatives, so order matters even in these lists
# - 0x01:<G>,* and
# - 0x02:<G>,ᄎ
# will replace the ᄎ involving entry that uses 0x01 because of ordering!
jamo_replacements = {
    0: jamo_groupings["initial"],
    1: jamo_groupings["median"],
    2: jamo_groupings["final"],
}

added_defaults = 0
replaced_defaults = 0
for [jamo, orientations] in jamo_combinations.items():
    for [index, orientation] in enumerate(orientations):
        if orientation is None:
            continue
        new_alternative_list = manual_encoding_additions[jamo][index]
        if new_alternative_list is None or len(new_alternative_list) == 0:
            continue
        # enumerate new list and collect info / generate list of new alternatives
        new_default = None
        force_default = False
        alternative_list = []
        for item in new_alternative_list:
            # - !0x01 -- means to override the default glyph, this is used very rarely
            # - 0x01 -- means to set the default -- some jamos never were used at all.  If a default is already set, this should throw an error
            if ":" not in item:
                if item.startswith("!"):
                    force_default = True
                    new_default = item[1:]
                else:
                    new_default = item
                continue
            parts = item.split(":")
            glyph = parts[0]
            pattern = parts[1]
            # order matters, so we have to append to a list for now
            # 0x01:<G>,ᅤ
            if "*" not in pattern:
                alternative_list.append({"glyph": glyph, "jamo_combination": pattern})
            # 0x01:<G>,*
            else:
                # 0x01:<G>,*[^ᄎ;ᄐ]
                # exclusion lists
                exclusion_list = set()
                # we generate all alternatives in order from left-to-right
                tokens = pattern.split(",")
                # cleanup each item first
                new_tokens = []
                for token in tokens:
                    if token.startswith("*[^"):
                        for exclude_jamo in token[3:-1].split(","):
                            exclusion_list.add(exclude_jamo)
                        new_tokens.append("*")
                    else:
                        new_tokens.append(token)
                wildcard_indices = [i for i, token in enumerate(tokens) if token == "*"]
                filtered_replacements = {
                    idx: [val for val in lst if val not in exclusion_list]
                    for idx, lst in jamo_replacements.items()
                }
                replacement_lists = [filtered_replacements[i] for i in wildcard_indices]
                for combo in product(*replacement_lists):
                    generated = tokens.copy()
                    for idx, val in zip(wildcard_indices, combo):
                        generated[idx] = val
                    alternative_list.append(
                        {"glyph": glyph, "jamo_combination": ",".join(generated)}
                    )
        # Ok, now we have our big list of new alternatives
        # we go through them one by one, adding them to the existing list
        # since alternatives is a map we don't have to concern ourselves with worrying about duplicates
        # the last one wins
        #
        # We also must set the new default if applicable
        new_orientation = orientation
        if new_default is not None:
            if isinstance(new_orientation, list) and len(new_orientation) == 0:
                new_orientation = {"defaultGlyph": new_default, "alternatives": {}}
                added_defaults = added_defaults + 1
            elif force_default:
                new_orientation["defaultGlyph"] = new_default
                replaced_defaults = replaced_defaults + 1
            else:
                print(
                    f"Trying to replace the default {new_orientation["defaultGlyph"]} with {new_default} improperly"
                )
                exit(1)
        # Alternatives
        if len(alternative_list) > 0:
            for new_alt in alternative_list:
                new_orientation["alternatives"][new_alt["jamo_combination"]] = new_alt[
                    "glyph"
                ]
        # Finally, update the DB
        jamo_combinations[jamo][index] = new_orientation

# Print some Stats again!
empty_cells = 0
new_glyph_list = set(jamo_glyph_mappings.keys())
for [jamo, orientations] in jamo_combinations.items():
    for orientation in orientations:
        if isinstance(orientation, dict):
            new_glyph_list.discard(orientation["defaultGlyph"])
            for combo, glyph in orientation["alternatives"].items():
                new_glyph_list.discard(glyph)
        elif orientation is not None:
            print(f"{jamo} - {orientation}")
            empty_cells = empty_cells + 1

print()
print(f"Added {added_defaults} defaults")
print(f"Replaced {replaced_defaults} defaults")
print(f"{empty_cells} empty jamo cells\n")
print(
    f"Still did not see {len(new_glyph_list)} out of {len(jamo_glyph_mappings.keys())} glyphs:"
)
print(
    f"Used an additional {len(new_glyph_list.difference(glyph_list))} glyphs, never seen in the original game!"
)
print("Never Used Glyphs:")
print(new_glyph_list)

# Print the results
with open("./jamo-db.json", mode="w", encoding="utf-8") as f:
    f.write(json.dumps(jamo_combinations, indent=None))

# pprint(jamo_combinations)

# Export some CSV results so that we can fill in the rest of the encoding using excel (easier to keep track of
# what's missing)
# This CSV table will only include the most common for each as:
# - we already have the alternatives, we aren't going to check those
# - we will add a new alternative, only if the common glyphs don't match (and we don't already have one, which i can manually check)
# Use the lists so we have a consistent ordering
csv_lines = []
for jamo in jamo_groupings["initial"]:
    cells_in_line = []
    for orientation in jamo_combinations[jamo]:
        if orientation is None:
            cells_in_line.append("N/A")
        elif isinstance(orientation, list) and len(orientation) == 0:
            cells_in_line.append("")
        else:
            alternative_entries = []
            for [context, alternative_glyph] in orientation["alternatives"].items():
                alternative_entries.append(
                    f"- {alternative_glyph} for {context.replace("<GLYPH>", "<G>")}"
                )
            alternatives = "\n".join(alternative_entries)
            if len(alternatives) > 0:
                cells_in_line.append(
                    f"\"{orientation['defaultGlyph'].replace(",", " ")}\n{alternatives.replace(",", ";")}\""
                )
            else:
                cells_in_line.append(
                    f"\"{orientation['defaultGlyph'].replace(",", " ")}\""
                )
    csv_lines.append(",".join(cells_in_line) + "\n")
for jamo in jamo_groupings["median"]:
    cells_in_line = []
    for orientation in jamo_combinations[jamo]:
        if orientation is None:
            cells_in_line.append("N/A")
        elif isinstance(orientation, list) and len(orientation) == 0:
            cells_in_line.append("")
        else:
            alternative_entries = []
            for [context, alternative_glyph] in orientation["alternatives"].items():
                alternative_entries.append(
                    f"- {alternative_glyph} for {context.replace("<GLYPH>", "<G>")}"
                )
            alternatives = "\n".join(alternative_entries)
            if len(alternatives) > 0:
                cells_in_line.append(
                    f"\"{orientation['defaultGlyph'].replace(",", " ")}\n{alternatives.replace(",", ";")}\""
                )
            else:
                cells_in_line.append(
                    f"\"{orientation['defaultGlyph'].replace(",", " ")}\""
                )
    csv_lines.append(",".join(cells_in_line) + "\n")
for jamo in jamo_groupings["final"]:
    cells_in_line = []
    for orientation in jamo_combinations[jamo]:
        if orientation is None:
            cells_in_line.append("N/A")
        elif isinstance(orientation, list) and len(orientation) == 0:
            cells_in_line.append("")
        else:
            alternative_entries = []
            for [context, alternative_glyph] in orientation["alternatives"].items():
                alternative_entries.append(
                    f"- {alternative_glyph} for {context.replace("<GLYPH>", "<G>")}"
                )
            alternatives = "\n".join(alternative_entries)
            if len(alternatives) > 0:
                cells_in_line.append(
                    f"\"{orientation['defaultGlyph'].replace(",", " ")}\n{alternatives.replace(",", ";")}\""
                )
            else:
                cells_in_line.append(
                    f"\"{orientation['defaultGlyph'].replace(",", " ")}\""
                )
    csv_lines.append(",".join(cells_in_line) + "\n")
# with open("./jamo-db.csv", mode="w", encoding="utf-8") as f:
#     f.writelines(csv_lines)
