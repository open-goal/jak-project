# Jak 2 shares many of the same custom menu strings as jak 1 obviously
# this is to copy them over so they are already translated.

import json
import os


with open("temp-text-id-mapping.json", "r", encoding="utf-8") as f:
    mapping = json.load(f)

with open("./text/game_custom_text_en-US.json", "r", encoding="utf-8") as f:
    default_jak2_english = json.load(f)

def create_text_file(locale):
    if not os.path.exists(f"./text/game_custom_text_{locale}.json"):
        with open(f"./text/game_custom_text_{locale}.json", "w", encoding="utf-8") as f:
            json.dump(default_jak2_english, f, indent=2, ensure_ascii=False)

    with open(f"./text/game_custom_text_{locale}.json", "r", encoding="utf-8") as f:
        jak2_file = json.load(f)

    with open(
        f"../jak1/text/game_custom_text_{locale}.json", "r", encoding="utf-8"
    ) as f:
        jak1_file = json.load(f)

    for jak2_id, jak1_id in mapping.items():
        jak2_file[jak2_id] = jak1_file[jak1_id].title()

    with open(f"./text/game_custom_text_{locale}.json", "w", encoding="utf-8") as f:
        json.dump(jak2_file, f, indent=2, ensure_ascii=False)


create_text_file("ca-ES")
create_text_file("da-DK")
create_text_file("de-DE")
create_text_file("es-ES")
create_text_file("fi-FI")
create_text_file("fr-FR")
create_text_file("hu-HU")
create_text_file("is-IS")
create_text_file("it-IT")
create_text_file("ja-JP")
create_text_file("nl-NL")
create_text_file("no-NO")
create_text_file("pl-PL")
create_text_file("pt-PT")
create_text_file("pt-BR")
create_text_file("sv-SE")
