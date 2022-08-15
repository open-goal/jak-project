from tokenize import group
import yaml

with open("./localization/jak1/text/meta.yml", 'r', encoding="utf-8") as f:
  meta = yaml.safe_load(f)

for gs_file_name, info in meta.items():
  # Build up the file
  language_ids = set()
  output_lines = [
    "(group-name \"{}\")\n".format(info["group-name"]),
  ]
  translations = []
  for language in info["languages"]:
    for lang_code, lang_info in language.items():
      language_ids.add(str(lang_info["id"]))
      file_name = "./localization/jak1/text/text.{}.yml".format(lang_code)
      with open(file_name, 'r', encoding="utf-8") as f:
        translations.append(yaml.safe_load(f))
  output_lines.append("(language-id {})\n".format(" ".join(language_ids)))
  output_lines.append("(text-version {})\n".format(info["version"]))

  # Actually build up translated strings
  # TODO - there are some potential edge-cases here around multi-language files
  # what should happen if the string isn't defined in both?
  # For now, just assuming everything is translated in both as this is only a uk english concern
  string_dict = {}
  for translation_file in translations:
    for group_name, string_map in translation_file.items():
      for string_id, string_val in string_map.items():
        if group_name not in string_dict:
          string_dict[group_name] = {}
        if string_id not in string_dict[group_name]:
          string_dict[group_name][string_id] = ["  \"{}\"".format(string_val)]
        else:
          string_dict[group_name][string_id].append("  \"{}\"".format(string_val))
  # Create final output
  for group_name, strings in string_dict.items():
    output_lines.append("\n;; {}\n".format(group_name))
    for string_id, translated_vals in strings.items():
      if len(translated_vals) == 1:
        output_lines.append("(#x{} {})\n".format(string_id, translated_vals[0].lstrip()))
      else:
        output_lines.append("(#x{}\n{})\n".format(string_id, "\n".join(translated_vals)))

  output_path = "./game/assets/jak1/text/{}.gs".format(gs_file_name)
  with open(output_path, "w", encoding="utf-8") as f:
    f.writelines(output_lines)
