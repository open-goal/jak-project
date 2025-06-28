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
    "../decompiler_out/jak2/assets/subtitles.txt", mode="r", encoding="utf-8"
) as f:
    subtitle_text_lines = f.readlines()
for line in subtitle_text_lines:
    parts = line.split("::")
    text = parts[2].replace("\"","").replace(" ",",")
    # pad with 0s for single hex digits
    for c in ["1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"]:
        text = text.replace(f"0x{c},", f"0x0{c},")
    key = f"{parts[0]}_{parts[1]}"
    korean_lines[key] = text[:-1]

# These are extra lines not found from the original game, beacuse the original game text
# does not cover all possible korean permutations
#
# These are manually verified legible glyph combinations to fill in these voids
manual_lines = [

]

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
        jamo_info.append(
            {
                "jamo": jamo,
                "grouping": jamo_grouping,
                "glyph": glyph
            }
        )
    if len(found_medians) > 2:
        print(f"found more than 2 median vowels in {jamo_info}")
        exit(1)
    # Consolidate median vowels, as jak typically typically draws them as a combination of two
    # glyphs
    if len(found_medians) > 1:
        combined_median = None
        combined_glyphs = None
        for [vowel, vowel_parts] in median_combos.items():
            if found_medians[0][0] in vowel_parts and found_medians[1][0] in vowel_parts:
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
                new_jamo_info.append({"jamo": combined_median, "grouping": "median", "glyph": combined_glyphs})
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
    print()
    print(game_text_line)
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

    pprint(syllable_blocks)

    # The (almost) final step, store this information in our big jamo combination
    # "database"
    # 
    # We now effectively have an encoding, and we can process that to further refine it and
    # see what we have to do manually
    for block in syllable_blocks:
        writing_orientation = block['writingOrientation']
        for jamo in block['jamos']:
            jamo_entry = jamo_combinations[jamo['jamo']]
            if jamo_entry[writing_orientation] == None:
                print(f"something is very wrong with {block}")
                exit(1)
            new_entry = {
                'glyph': jamo['glyph'],
                'context': block['jamos'] 
            }
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
                if isinstance(entry['glyph'], list):
                    for glyph in entry['glyph']:
                        glyph_list.discard(glyph)
                else:
                    glyph_list.discard(entry['glyph'])

print()
print(f"Analyzed {total_syllable_blocks} syllable blocks")
print(f"{empty_cells} empty jamo cells\n")
print(f"Did not see {len(glyph_list)} out of {len(jamo_glyph_mappings.keys())} glyphs:")
print(glyph_list)

with open('./jamo-db-before.json', mode="w", encoding="utf-8") as f:
    f.write(json.dumps(jamo_combinations, indent=2))

def format_alternative(curr_glyph, full_glyph_context):
    # Make a string key that represents the unicode jamos with a <GLYPH> placeholder to represent
    # the jamo we are dealing with
    # And the value is the glyph itself that gets used to draw this combination of jamos
    key_parts = []
    for glyph in full_glyph_context:
        if curr_glyph == glyph['glyph']:
            key_parts.append("<GLYPH>")
        else:
            key_parts.append(glyph['jamo'])
    return [",".join(key_parts), curr_glyph]

# Enumerate through the db, and consolidate duplicates / find the most common
# jamo for each position
for [jamo, orientations] in jamo_combinations.items():
    for [index, orientation] in enumerate(orientations):
        if orientation is not None:
            result = {
                "defaultGlyph": "",
                "alternatives": {}
            }
            glyph_frequencies = {}
            alternatives = {}
            if len(orientation) == 0:
                empty_cells = empty_cells + 1
                continue
            for entry in orientation:
                glyph_key = entry['glyph']
                if isinstance(entry['glyph'], list):
                    glyph_key = ",".join(entry['glyph'])
                if glyph_key not in glyph_frequencies:
                    glyph_frequencies[glyph_key] = 0
                glyph_frequencies[glyph_key] = glyph_frequencies[glyph_key] + 1

                if glyph_key not in alternatives:
                    alternatives[glyph_key] = []
                alternatives[glyph_key].append(format_alternative(entry['glyph'], entry['context']))
            # Consolidate
            most_common_glyph = ''
            most_common_glyph_times = -1
            for [glyph, freq] in glyph_frequencies.items():
                if freq > most_common_glyph_times:
                    most_common_glyph_times = freq
                    most_common_glyph = glyph
            result['defaultGlyph'] = most_common_glyph
            # TODO - handle if this is multiple glyphs
            del alternatives[most_common_glyph]
            # Flatten alternatives
            for [glyph, alternatives] in alternatives.items():
                for alternative in alternatives:
                    result['alternatives'][alternative[0]] = alternative[1]
            # Overwrite the db value
            jamo_combinations[jamo][index] = result

# Print the results
with open('./jamo-db.json', mode="w", encoding="utf-8") as f:
    f.write(json.dumps(jamo_combinations, indent=2))

pprint(jamo_combinations)

# Export some CSV results so that we can fill in the rest of the encoding using excel (easier to keep track of 
# what's missing)
# This CSV table will only include the most common for each as:
# - we already have the alternatives, we aren't going to check those
# - we will add a new alternative, only if the common glyphs don't match (and we don't already have one, which i can manually check)
# Use the lists so we have a consistent ordering
csv_lines = []
for jamo in jamo_groupings['initial']:
    cells_in_line = []
    for orientation in jamo_combinations[jamo]:
        if orientation is None:
            cells_in_line.append("N/A")
        elif isinstance(orientation, list) and len(orientation) == 0:
            cells_in_line.append("")
        else:
            cells_in_line.append(orientation['defaultGlyph'].replace(",", " "))
    csv_lines.append(",".join(cells_in_line) + "\n")
for jamo in jamo_groupings['median']:
    cells_in_line = []
    for orientation in jamo_combinations[jamo]:
        if orientation is None:
            cells_in_line.append("N/A")
        elif isinstance(orientation, list) and len(orientation) == 0:
            cells_in_line.append("")
        else:
            cells_in_line.append(orientation['defaultGlyph'].replace(",", " "))
    csv_lines.append(",".join(cells_in_line) + "\n")
for jamo in jamo_groupings['final']:
    cells_in_line = []
    for orientation in jamo_combinations[jamo]:
        if orientation is None:
            cells_in_line.append("N/A")
        elif isinstance(orientation, list) and len(orientation) == 0:
            cells_in_line.append("")
        else:
            cells_in_line.append(orientation['defaultGlyph'].replace(",", " "))
    csv_lines.append(",".join(cells_in_line) + "\n")
with open('./jamo-db.csv', mode="w", encoding="utf-8") as f:
    f.writelines(csv_lines)

# - fill in empty table cells
# - mark unused glyphs in the image

# - finally, write C++ code that converts from utf-8 to the korean encoding or vise versa using the finished json data table