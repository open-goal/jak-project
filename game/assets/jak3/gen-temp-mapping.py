import json

file1_path = "../jak2/text/game_custom_text_en-US.json"
file2_path = "./text/game_custom_text_en-US.json"
output_path = "temp-text-id-mapping.json"

with open(file1_path, "r", encoding="utf-8") as f:
    a = json.load(f)

with open(file2_path, "r", encoding="utf-8") as f:
    b = json.load(f)

# invert: value -> key
rev = {v: k for k, v in a.items()}

out = {k2: rev[v2] for k2, v2 in b.items() if v2 in rev}

with open(output_path, "w", encoding="utf-8") as f:
    json.dump(out, f, ensure_ascii=False, indent=2)
