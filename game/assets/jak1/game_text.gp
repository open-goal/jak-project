;; "project file" for text make tool.
;; it's very simple... a list of (action args...)
;; There are two actions:
;; - file-json (A path to a json file)
;;   - args: (language_id filename text_version group_name)
;; - file (A path to a GOAL data file)
;;   - the same arguments are provided within the file itself

(text
  ;; NOTE : we compile using the fixed v2 encoding because it's what we use.
  (file "$DECOMP/assets/game_text.txt") ;; this is the decompiler-generated file!
  ;; add custom files down here
  (file-json 0 "game/assets/jak1/text/game_text_en-US.json" jak1-v2 "common")
  (file-json 1 "game/assets/jak1/text/game_text_fr-FR.json" jak1-v2 "common")
  (file-json 2 "game/assets/jak1/text/game_text_de-DE.json" jak1-v2 "common")
  (file-json 3 "game/assets/jak1/text/game_text_es-ES.json" jak1-v2 "common")
  (file-json 4 "game/assets/jak1/text/game_text_it-IT.json" jak1-v2 "common")
  (file-json 5 "game/assets/jak1/text/game_text_ja-JP.json" jak1-v2 "common")
  (file-json 6 "game/assets/jak1/text/game_text_en-GB.json" jak1-v2 "common")
  (file-json 7 "game/assets/jak1/text/game_text_pt-PT.json" jak1-v2 "common")
  (file-json 8 "game/assets/jak1/text/game_text_fi-FI.json" jak1-v2 "common")
  (file-json 9 "game/assets/jak1/text/game_text_sv-SE.json" jak1-v2 "common")
  (file-json 10 "game/assets/jak1/text/game_text_da-DK.json" jak1-v2 "common")
  (file-json 11 "game/assets/jak1/text/game_text_no-NO.json" jak1-v2 "common")
  (file-json 12 "game/assets/jak1/text/game_text_nl-NL.json" jak1-v2 "common")
  (file-json 13 "game/assets/jak1/text/game_text_pt-BR.json" jak1-v2 "common")
  (file-json 14 "game/assets/jak1/text/game_text_hu-HU.json" jak1-v2 "common")
)
