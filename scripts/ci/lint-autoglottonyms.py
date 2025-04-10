# autoglottonyms are the way the native speakers refer to their language
# they should be left alone and not translated, ever accidentally change your language and then struggle to find it
# in the list because you don't know how to say "English" in, lets say, french ("anglais").
#
# yeah thats why you leave em alone, this script makes sure that remains true.

import argparse
import glob
import json

parser = argparse.ArgumentParser()
parser.add_argument("--fix", action="store_true")
parser.set_defaults(fix=False)
args = parser.parse_args()

JAK1_AUTOGLOT_IDS = [
    "1110",
    "1111",
    "1112",
    "1113",
    "1114",
    "1115",
    "1116",
    "1117",
    "1118",
    "1119",
    "111a",
    "111b",
    "111c",
    "111d",
    "111e",
    "111f",
]
JAK2_AUTOGLOT_IDS = [
    "133a",
    "133c",
    "133d",
    "133e",
    "133f",
    "1340",
    "1341",
    "1342",
    "1343",
    "1344",
    "1345",
    "1346",
    "1347",
    "1348",
    "1349",
]

def check_text_files(game_name, ids_to_check):
    problem_found = False

    english_file = "./game/assets/{}/text/game_custom_text_en-US.json".format(game_name)
    with open(english_file, encoding="utf-8") as f:
        english_data = json.load(f)

    # Iterate through the translations making sure there are no characters that are not allowed
    text_files = glob.glob("./game/assets/{}/text/*.json".format(game_name))

    for text_file in text_files:
        print("Checking {}".format(text_file))
        with open(text_file, encoding="utf-8") as f:
            file_data = json.load(f)
        for id in ids_to_check:
            if id in file_data and english_data[id] != file_data[id]:
                print("{} doesn't match english file".format(id))
                problem_found = True
                if args.fix:
                    file_data[id] = english_data[id]
        if args.fix:
            # save the modified file back out
            with open(text_file, "w", encoding="utf-8") as f:
                json.dump(file_data, f, indent=2, ensure_ascii=False)
                f.write("\n")
    return problem_found

not_ok_jak1 = check_text_files("jak1", JAK1_AUTOGLOT_IDS)
not_ok_jak2 = check_text_files("jak2", JAK2_AUTOGLOT_IDS)

if not_ok_jak1 or not_ok_jak2:
    print("Autoglottonyms were changed, stop!")
    exit(1)
else:
    print("Autoglottonyms were left alone!")
