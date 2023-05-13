;; "project file" for subtitles make tool.
;; it's very simple... a list of (action args...)
;; There is one supported action:
;; - file (A path to a GOAL data file)
;;   - the same arguments are provided within the file itself

(subtitle
  (file "game/assets/jak1/subtitle/game_subtitle_en.gd")
  (file "game/assets/jak1/subtitle/game_subtitle_fr.gd")
  (file "game/assets/jak1/subtitle/game_subtitle_en-uk.gd")
  (file "game/assets/jak1/subtitle/game_subtitle_de.gd")
  (file "game/assets/jak1/subtitle/game_subtitle_es.gd")
  (file "game/assets/jak1/subtitle/game_subtitle_ptbr.gd")
  (file "game/assets/jak1/subtitle/game_subtitle_it.gd")
  ;; (file-json
  ;;   :language-id 0
  ;;   :text-version 'jak1-v2
  ;;   :lines "game/assets/jak1/subtitle/subtitle_lines_en-US.json"
  ;;   :meta "game/assets/jak1/subtitle/subtitle_meta_en-US.json")
  ;; (file-json
  ;;   :language-id 1
  ;;   :text-version 'jak1-v2
  ;;   :lines "game/assets/jak1/subtitle/subtitle_lines_fr-FR.json"
  ;;   :lines-base "game/assets/jak1/subtitle/subtitle_lines_en-US.json"
  ;;   :meta-base "game/assets/jak1/subtitle/subtitle_meta_en-US.json"
  ;;   :meta "game/assets/jak1/subtitle/subtitle_meta_fr-FR.json")
  ;; (file-json
  ;;   :language-id 2
  ;;   :text-version 'jak1-v2
  ;;   :lines "game/assets/jak1/subtitle/subtitle_lines_de-DE.json"
  ;;   :lines-base "game/assets/jak1/subtitle/subtitle_lines_en-US.json"
  ;;   :meta-base "game/assets/jak1/subtitle/subtitle_meta_en-US.json"
  ;;   :meta "game/assets/jak1/subtitle/subtitle_meta_de-DE.json")
  ;; (file-json
  ;;   :language-id 3
  ;;   :text-version 'jak1-v2
  ;;   :lines "game/assets/jak1/subtitle/subtitle_lines_es-ES.json"
  ;;   :lines-base "game/assets/jak1/subtitle/subtitle_lines_en-US.json"
  ;;   :meta-base "game/assets/jak1/subtitle/subtitle_meta_en-US.json"
  ;;   :meta "game/assets/jak1/subtitle/subtitle_meta_es-ES.json")
  ;; (file-json
  ;;   :language-id 4
  ;;   :text-version 'jak1-v2
  ;;   :lines "game/assets/jak1/subtitle/subtitle_lines_it-IT.json"
  ;;   :lines-base "game/assets/jak1/subtitle/subtitle_lines_en-US.json"
  ;;   :meta-base "game/assets/jak1/subtitle/subtitle_meta_en-US.json"
  ;;   :meta "game/assets/jak1/subtitle/subtitle_meta_it-IT.json")
  ;; ;; TODO japanese
  ;; (file-json
  ;;   :language-id 6
  ;;   :text-version 'jak1-v2
  ;;   :lines "game/assets/jak1/subtitle/subtitle_lines_en-GB.json"
  ;;   :meta-base "game/assets/jak1/subtitle/subtitle_meta_en-GB.json"
  ;;   :meta "game/assets/jak1/subtitle/subtitle_meta_en-GB.json")
  ;; (file-json
  ;;   :language-id 13
  ;;   :text-version 'jak1-v2
  ;;   :lines "game/assets/jak1/subtitle/subtitle_lines_pt-BR.json"
  ;;   :meta-base "game/assets/jak1/subtitle/subtitle_meta_en-GB.json"
  ;;   :meta "game/assets/jak1/subtitle/subtitle_meta_pt-BR.json")
  ;; ;; TODO - the rest
  )


