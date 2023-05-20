import json


speaker_names = [
    "VIS MAN",
    "DAXTER",
    "KEIRA",
    "GAMMAL MAN",
    "KVINNA",
    "ORAKEL",
    "BONDE," "FLUT FLUT",
    "???",
    "BILLY",
    "FÅGEL SKÅDARE",
    "BLÅ VIS MAN",
    "DAXTER",
    "BONDE",
    "FISKARE",
    "FLUT-FLUT",
    "SPELARE",
    "GEOLOG",
    "GOL",
    "GORDY",
    "JAK",
    "JAK'S FARBROR",
    "KEIRA",
    "MAIA",
    "BORGMÄSTARE",
    "GRUVARBETARE",
    "GAMMAL MAN",
    "ORAKEL",
    "RÖD VIS MAN",
    "SAMOS",
    "SKULPTÖR",
    "KRIGARE",
    "WILLARD",
    "KVINNA",
    "GUL VIS MAN",
]


def starts_with_speaker(line):
    if line.strip() == "":
        return True
    for speaker in speaker_names:
        if line.startswith(speaker):
            return True

# with open("swedish_subs.txt", "r", encoding="utf-8") as f:
#     lines = f.readlines()
#     i = 0
#     new_lines = []
#     while i < len(lines):
#         print("[{}/{}] Lines Processed".format(i, len(lines)))
#         line = lines[i]
#         if line.strip() == "":
#             new_lines.append(line)
#             i = i + 1
#             continue
#         # If the next line doesn't start with a speaker, then we need to combine it with the current line
#         if i < len(lines) - 1 and not starts_with_speaker(lines[i+1]):
#             combined_line = "{} {}\n".format(line.strip(), lines[i+1].strip())
#             new_lines.append(combined_line.lstrip())
#             i = i + 1
#         else:
#             new_lines.append(line.lstrip())
#         i = i + 1
#     with open("swedish_subs_remove_newlines.txt", "w", encoding="utf-8") as f:
#         f.writelines(new_lines)

cutscenes_and_hints = []

with open("english_base.gd", "r", encoding="utf-8") as f:
    lines = f.readlines()
    for line in lines:
        if line.startswith("(\""):
            name = line.split("(\"")[1].split("\"")[0]
            if name == "MSH-AM08":
                continue # this has no dialogue
            if "hint" in line:
                cutscenes_and_hints.append({
                    "name": name,
                    "hint": True
                })
            else:
                cutscenes_and_hints.append({
                    "name": name,
                    "hint": False
                })

subtitle_file = {
    "cutscenes": {},
    "hints": {},
}

print(len(cutscenes_and_hints))

line_groups = []

with open("swedish_subs_remove_newlines.txt", "r", encoding="utf-8") as f:
    lines = f.readlines()
    i = 0
    curr_group = []
    while i < len(lines):
        line = lines[i]
        if line.strip() == "":
            line_groups.append(curr_group)
            curr_group = []
            i = i + 1
            continue
        curr_group.append(line)
        i = i + 1
    line_groups.append(curr_group)

print(len(line_groups))

with open("subtitle_lines_sv-SE.json", "r", encoding="utf-8") as f:
  reference_file = json.load(f)

for idx, group in enumerate(line_groups):
    name = cutscenes_and_hints[idx]["name"]
    is_hint = cutscenes_and_hints[idx]["hint"]
    if is_hint:
        expected_line_count = len(reference_file["hints"][name])
    else:
        expected_line_count = len(reference_file["cutscenes"][name])
    if len(group) != expected_line_count:
        print("ERROR: Expected {} lines for {}, got {} -- {}\n\n".format(expected_line_count, name, len(group), group))
        exit(1)
