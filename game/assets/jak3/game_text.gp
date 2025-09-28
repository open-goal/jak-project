;; "project file" for text make tool.
;; it's very simple... a list of (action args...)
;; There are two actions:
;; - file-json (A path to a json file)
;;   - args: (language_id text_version group_name '(filename list))
;; - file (A path to a GOAL data file)
;;   - the same arguments are provided within the file itself

(text
  ;; NOTE : we compile using the fixed v2 encoding because it's what we use.
  (file "$DECOMP/assets/game_text.txt") ;; this is the decompiler-generated file!
  ;; add custom files down here
  (file-json 0 jak3 "common" '("game/assets/jak3/text/game_custom_text_en-US.json"))
  (file-json 6 jak3 "common" '("game/assets/jak3/text/game_custom_text_ja-JP.json"))
  (file-json 11 jak3 "common" '("game/assets/jak3/text/game_custom_text_en-GB.json"))
  )


