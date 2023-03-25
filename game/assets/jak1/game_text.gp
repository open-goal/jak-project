;; "project file" for text make tool.
;; it's very simple... a list of (action args...)
;; currently the only action available is 'file'
;; and it takes 1 argument: input filename.

(text
  ;; NOTE : we compile using the fixed v2 encoding because it's what we use.
  (file "$DECOMP/assets/game_text.txt") ;; this is the decompiler-generated file!
  ;; add custom files down here
  (file "game/assets/jak1/text/game_text_en.gs")
  (file "game/assets/jak1/text/game_text_fr.gs")
  (file "game/assets/jak1/text/game_text_de.gs")
  (file "game/assets/jak1/text/game_text_ja.gs")
  (file "game/assets/jak1/text/game_text_ptbr.gs")
  (file "game/assets/jak1/text/game_text_es.gs")
  (file "game/assets/jak1/text/game_text_it.gs")
  (file "game/assets/jak1/text/game_text_hu.gs")
  )


