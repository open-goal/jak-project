;; "project file" for subtitles make tool.

(subtitle-v2
  (file-json
    :language-id 0
    :text-version "jak2"
    :lines "game/assets/jak2/subtitle/subtitle_lines_en-US.json"
    :meta "game/assets/jak2/subtitle/subtitle_meta_en-US.json")
  (file-json
    :language-id 5
    :text-version "jak2"
    :lines "game/assets/jak2/subtitle/subtitle_lines_jp-JP.json"
    :lines-base "game/assets/jak2/subtitle/subtitle_lines_en-US.json"
    :meta "game/assets/jak2/subtitle/subtitle_meta_jp-JP.json"
    :meta-base "game/assets/jak2/subtitle/subtitle_meta_en-US.json"))
