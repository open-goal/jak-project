;; "project file" for text make tool.
;; it's very simple... a list of (version file)
;; eventually should also include output filename
;; you can find the game-text-version parsing in .cpp and an enum in goal-lib.gc

(text
  ;; NOTE : we compile using the fixed v2 encoding because it's what we use.
  (jak1-v2 "assets/game_text.txt") ;; this is the decompiler-generated file!
  ;; "patch" files so we can fix some errors and perhaps maintain consistency
  (jak1-v2 "game/assets/jak1/text/text_patch_ja.gs")
  ;; add custom files down here
  (jak1-v2 "game/assets/jak1/text/game_text_en.gs")
  (jak1-v2 "game/assets/jak1/text/game_text_ja.gs")
  )


