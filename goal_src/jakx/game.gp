;;-*-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jak X Project File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see goal_src/jak1/game.gp for more detailed explanation

;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from ISO
;;;;;;;;;;;;;;;;;;;;;;;

(cond
  ;; extractor can override everything by providing *use-iso-data-path*
  (*use-iso-data-path*
   (map-path! "$ISO" (string-append *iso-data* "/")))
  ;; if the user's repl-config has a game version folder, use that
  ((> (string-length (get-game-version-folder)) 0)
   (map-path! "$ISO" (string-append "iso_data/" (get-game-version-folder) "/")))
  ;; otherwise, default to jakx
  (#t
   (map-path! "$ISO" "iso_data/jakx/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from decompiler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
  ;; if the user's repl-config has a game version folder, use that
  ((> (string-length (get-game-version-folder)) 0)
   (map-path! "$DECOMP" (string-append "decompiler_out/" (get-game-version-folder) "/")))
  ;; otherwise, default to jakx
  (#t
   (map-path! "$DECOMP" "decompiler_out/jakx/")))

;;;;;;;;;;;;;;;;;;;;;;;
;; Output
;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: the game itself will load from out/jakx/iso and out/jakx/fr3.
(map-path! "$OUT" "out/jakx/")

;; tell the compiler to put its outputs in out/jakx/
(set-output-prefix "jakx/")

;; use defmacro to define goos macros.
(define defmacro defsmacro)
(define defun desfun)

;;;;;;;;;;;;;;;;;;;;;;;
;; Build Groups
;;;;;;;;;;;;;;;;;;;;;;;

(define *all-cgos* '())
(define *all-str* '())
(define *all-vis* '())
(define *all-mus* '())
(define *all-sbk* '())
(define *all-vag* '())
(define *all-gc* '())

(define *file-entry-map* (make-string-hash-table))

;; Load required macros
(load-file "goal_src/jakx/lib/project-lib.gp")
(set-gsrc-folder! "goal_src/jakx")

;;;;;;;;;;;;;;;;;
;; GOAL Kernel
;;;;;;;;;;;;;;;;;

(cgo-file "kernel.gd" '())

;;;;;;;;;;;;;;;;;;;;;
;; misc files
;;;;;;;;;;;;;;;;;;;;;

;; the VAGDIR file
(defstep :in "$ISO/VAG/VAGDIR.AYB"
  :tool 'copy
  :out '("$OUT/iso/VAGDIR.AYB"))

;;;;;;;;;;;;;;;;;;;;;
;; DGOs
;;;;;;;;;;;;;;;;;;;;;

;; (defstep :in "$DECOMP/textures/tpage-dir.txt"
;;   :tool 'tpage-dir
;;   :out '("$OUT/obj/dir-tpages.go")
;;   )
;; (hash-table-set! *file-entry-map* "dir-tpages.go" #f)

(cgo-file "game.gd" '("$OUT/obj/gcommon.o" "$OUT/obj/gstate.o" "$OUT/obj/gstring.o" "$OUT/obj/gkernel.o"))

;; note: some of these dependencies are slightly wrong because cgo-file doesn't really handle
;; the case of a .o appearing in multiple dgos. But, if we depend on the last item in both lists, it
;; works out.

(define common-dep '("$OUT/obj/minimap.o"))

;; generated via `scripts/gsrc/skeleton_creation/generate_dgo_proj.py
;; (cgo-file "clfx.gd" common-dep)
;; (cgo-file "swx.gd" common-dep)
;; (cgo-file "cyc.gd" common-dep)
;; (cgo-file "tpc.gd" common-dep)
;; (cgo-file "dromett.gd" common-dep)
;; (cgo-file "ximlev.gd" common-dep)
;; (cgo-file "pecvl.gd" common-dep)
;; (cgo-file "dkb.gd" common-dep)
;; (cgo-file "kcrsclct.gd" common-dep)
;; (cgo-file "spa.gd" common-dep)
;; (cgo-file "dockfoot.gd" common-dep)
;; (cgo-file "sewerw.gd" common-dep)
;; (cgo-file "thacred.gd" common-dep)
;; (cgo-file "ashcred.gd" common-dep)
;; (cgo-file "krass.gd" common-dep)
;; (cgo-file "jgc.gd" common-dep)
;; (cgo-file "leopl.gd" common-dep)
;; (cgo-file "csy.gd" common-dep)
;; (cgo-file "icefoot.gd" common-dep)
;; (cgo-file "spx.gd" common-dep)
;; (cgo-file "peakw.gd" common-dep)
;; (cgo-file "hva.gd" common-dep)
;; (cgo-file "icebfoot.gd" common-dep)
;; (cgo-file "jungtrn.gd" common-dep)
;; (cgo-file "cyy.gd" common-dep)
;; (cgo-file "daxlev.gd" common-dep)
;; (cgo-file "havtours.gd" common-dep)
;; (cgo-file "icy.gd" common-dep)
;; (cgo-file "thbvl.gd" common-dep)
;; (cgo-file "dromdocs.gd" common-dep)
;; (cgo-file "dke.gd" common-dep)
;; (cgo-file "clifhunt.gd" common-dep)
;; (cgo-file "sewfoot.gd" common-dep)
;; (cgo-file "pkx.gd" common-dep)
;; (cgo-file "cnspfoot.gd" common-dep)
;; (cgo-file "dkc.gd" common-dep)
;; (cgo-file "temptbox.gd" common-dep)
;; (cgo-file "jgy.gd" common-dep)
;; (cgo-file "krastbox.gd" common-dep)
;; (cgo-file "krasfoot.gd" common-dep)
;; (cgo-file "dsrx.gd" common-dep)
;; (cgo-file "cyd.gd" common-dep)
;; (cgo-file "desisles.gd" common-dep)
;; (cgo-file "razcred.gd" common-dep)
;; (cgo-file "tpa.gd" common-dep)
;; (cgo-file "falcl.gd" common-dep)
;; (cgo-file "cheel.gd" common-dep)
;; (cgo-file "krattbox.gd" common-dep)
;; (cgo-file "jungtbox.gd" common-dep)
;; (cgo-file "sigvl3.gd" common-dep)
;; (cgo-file "ur8cred.gd" common-dep)
;; (cgo-file "havenw.gd" common-dep)
;; (cgo-file "ratlev.gd" common-dep)
;; (cgo-file "jungfoot.gd" common-dep)
;; (cgo-file "templew.gd" common-dep)
;; (cgo-file "klevl2.gd" common-dep)
;; (cgo-file "spc.gd" common-dep)
;; (cgo-file "kcrosctf.gd" common-dep)
;; (cgo-file "cliffart.gd" common-dep)
;; (cgo-file "swc.gd" common-dep)
;; (cgo-file "canyonw.gd" common-dep)
;; (cgo-file "jaklev.gd" common-dep)
;; (cgo-file "jakcred.gd" common-dep)
;; (cgo-file "kratourw.gd" common-dep)
;; (cgo-file "eightb.gd" common-dep)
;; (cgo-file "swd.gd" common-dep)
;; (cgo-file "snowtt.gd" common-dep)
;; (cgo-file "hvtrtt.gd" common-dep)
;; (cgo-file "coliseus.gd" common-dep)
;; (cgo-file "pkd.gd" common-dep)
;; (cgo-file "colart.gd" common-dep)
;; (cgo-file "possl.gd" common-dep)
;; (cgo-file "krastrn.gd" common-dep)
;; (cgo-file "ashvl.gd" common-dep)
;; (cgo-file "dromew.gd" common-dep)
;; (cgo-file "hvswtbox.gd" common-dep)
;; (cgo-file "sigvl.gd" common-dep)
;; (cgo-file "sparfoot.gd" common-dep)
;; (cgo-file "sptmtt.gd" common-dep)
;; (cgo-file "dkx.gd" common-dep)
;; (cgo-file "hvd.gd" common-dep)
;; (cgo-file "snw.gd" common-dep)
;; (cgo-file "brdroom.gd" common-dep)
;; (cgo-file "icc.gd" common-dep)
;; (cgo-file "sewers.gd" common-dep)
;; (cgo-file "garage.gd" common-dep)
;; (cgo-file "ashvl2.gd" common-dep)
;; (cgo-file "jkclev.gd" common-dep)
;; (cgo-file "jakvl.gd" common-dep)
;; (cgo-file "dra.gd" common-dep)
;; (cgo-file "kaelev.gd" common-dep)
;; (cgo-file "thbcred.gd" common-dep)
;; (cgo-file "thbvl2.gd" common-dep)
;; (cgo-file "pkb.gd" common-dep)
;; (cgo-file "spatours.gd" common-dep)
;; (cgo-file "dkkrtbox.gd" common-dep)
;; (cgo-file "klecred.gd" common-dep)
;; (cgo-file "dockstt.gd" common-dep)
;; (cgo-file "snowtrn2.gd" common-dep)
;; (cgo-file "icebergs.gd" common-dep)
;; (cgo-file "cnsptbox.gd" common-dep)
;; (cgo-file "spe.gd" common-dep)
;; (cgo-file "swe.gd" common-dep)
;; (cgo-file "drd.gd" common-dep)
;; (cgo-file "clf.gd" common-dep)
;; (cgo-file "thavl.gd" common-dep)
;; (cgo-file "atl.gd" common-dep)
;; (cgo-file "tpy.gd" common-dep)
;; (cgo-file "cye.gd" common-dep)
;; (cgo-file "snakl.gd" common-dep)
;; (cgo-file "ashlev.gd" common-dep)
;; (cgo-file "krx.gd" common-dep)
;; (cgo-file "jgx.gd" common-dep)
;; (cgo-file "hvy.gd" common-dep)
;; (cgo-file "csx.gd" common-dep)
;; (cgo-file "hve.gd" common-dep)
;; (cgo-file "jkalev.gd" common-dep)
;; (cgo-file "canspars.gd" common-dep)
;; (cgo-file "raycred.gd" common-dep)
;; (cgo-file "dsi.gd" common-dep)
;; (cgo-file "icettbox.gd" common-dep)
;; (cgo-file "spy.gd" common-dep)
;; (cgo-file "icetrn.gd" common-dep)
;; (cgo-file "torvl3.gd" common-dep)
;; (cgo-file "icetourw.gd" common-dep)
;; (cgo-file "sbwlctf.gd" common-dep)
;; (cgo-file "jungles.gd" common-dep)
;; (cgo-file "pka.gd" common-dep)
;; (cgo-file "dsr.gd" common-dep)
;; (cgo-file "tpe.gd" common-dep)
;; (cgo-file "ur8vl.gd" common-dep)
;; (cgo-file "ica.gd" common-dep)
;; (cgo-file "icew.gd" common-dep)
;; (cgo-file "dromtbox.gd" common-dep)
;; (cgo-file "credits.gd" common-dep)
;; (cgo-file "sigvl2.gd" common-dep)
;; (cgo-file "spartems.gd" common-dep)
;; (cgo-file "dromex.gd" common-dep)
;; (cgo-file "havnfoot.gd" common-dep)
;; (cgo-file "icebtbox.gd" common-dep)
;; (cgo-file "havtfoot.gd" common-dep)
;; (cgo-file "menumap.gd" common-dep)
;; (cgo-file "icetbox.gd" common-dep)
;; (cgo-file "snobart.gd" common-dep)
;; (cgo-file "eight.gd" common-dep)
;; (cgo-file "wombl.gd" common-dep)
;; (cgo-file "desart.gd" common-dep)
;; (cgo-file "dethrace.gd" common-dep)
;; (cgo-file "cliftrn.gd" common-dep)
;; (cgo-file "spb.gd" common-dep)
;; (cgo-file "desrapt.gd" common-dep)
;; (cgo-file "kielev.gd" common-dep)
;; (cgo-file "deshunt.gd" common-dep)
;; (cgo-file "wolfl.gd" common-dep)
;; (cgo-file "havntbox.gd" common-dep)
;; (cgo-file "icx.gd" common-dep)
;; (cgo-file "razlev.gd" common-dep)
;; (cgo-file "icett.gd" common-dep)
;; (cgo-file "icd.gd" common-dep)
;; (cgo-file "keicred.gd" common-dep)
;; (cgo-file "hvc.gd" common-dep)
;; (cgo-file "fmvlev.gd" common-dep)
;; (cgo-file "hsy.gd" common-dep)
;; (cgo-file "peclev.gd" common-dep)
;; (cgo-file "foxl.gd" common-dep)
;; (cgo-file "pkc.gd" common-dep)
;; (cgo-file "dromdocw.gd" common-dep)
;; (cgo-file "drx.gd" common-dep)
;; (cgo-file "krastt.gd" common-dep)
;; (cgo-file "desactf.gd" common-dep)
;; (cgo-file "raceweap.gd" common-dep)
;; (cgo-file "klevl.gd" common-dep)
;; (cgo-file "spargusw.gd" common-dep)
;; (cgo-file "ur8vl2.gd" common-dep)
;; (cgo-file "daxcred.gd" common-dep)
;; (cgo-file "tpb.gd" common-dep)
;; (cgo-file "desarens.gd" common-dep)
;; (cgo-file "jgf.gd" common-dep)
;; (cgo-file "col.gd" common-dep)
;; (cgo-file "hvjgtbox.gd" common-dep)
;; (cgo-file "gtbvl.gd" common-dep)
;; (cgo-file "colirev.gd" common-dep)
;; (cgo-file "temples.gd" common-dep)
;; (cgo-file "jgg.gd" common-dep)
;; (cgo-file "peakfoot.gd" common-dep)
;; (cgo-file "thcvl.gd" common-dep)
;; (cgo-file "sty.gd" common-dep)
;; (cgo-file "drdx.gd" common-dep)
;; (cgo-file "torlev.gd" common-dep)
;; (cgo-file "turtl.gd" common-dep)
;; (cgo-file "colictf.gd" common-dep)
;; (cgo-file "havtourw.gd" common-dep)
;; (cgo-file "desclct.gd" common-dep)
;; (cgo-file "jge.gd" common-dep)
;; (cgo-file "deshunt2.gd" common-dep)
;; (cgo-file "thcvl2.gd" common-dep)
;; (cgo-file "swf.gd" common-dep)
;; (cgo-file "krc.gd" common-dep)
;; (cgo-file "drone.gd" common-dep)
;; (cgo-file "dockss.gd" common-dep)
;; (cgo-file "disleart.gd" common-dep)
;; (cgo-file "dromfoot.gd" common-dep)
;; (cgo-file "dockkraw.gd" common-dep)
;; (cgo-file "jgb.gd" common-dep)
;; (cgo-file "hsx.gd" common-dep)
;; (cgo-file "bobcl.gd" common-dep)
;; (cgo-file "atollctf.gd" common-dep)
;; (cgo-file "krb.gd" common-dep)
;; (cgo-file "clifctf.gd" common-dep)
;; (cgo-file "rayvl2.gd" common-dep)
;; (cgo-file "hvjgtt.gd" common-dep)
;; (cgo-file "thavl2.gd" common-dep)
;; (cgo-file "colx.gd" common-dep)
;; (cgo-file "jga.gd" common-dep)
;; (cgo-file "cya.gd" common-dep)
;; (cgo-file "hvb.gd" common-dep)
;; (cgo-file "sptrtt.gd" common-dep)
;; (cgo-file "siglev.gd" common-dep)
;; (cgo-file "templett.gd" common-dep)
;; (cgo-file "thccred.gd" common-dep)
;; (cgo-file "iceptbox.gd" common-dep)
;; (cgo-file "coliclct.gd" common-dep)
;; (cgo-file "icbgtt.gd" common-dep)
;; (cgo-file "torvl2.gd" common-dep)
;; (cgo-file "peaks.gd" common-dep)
;; (cgo-file "thalev.gd" common-dep)
;; (cgo-file "hvx.gd" common-dep)
;; (cgo-file "thavl3.gd" common-dep)
;; (cgo-file "peaktbox.gd" common-dep)
;; (cgo-file "spatourw.gd" common-dep)
;; (cgo-file "sewtbox.gd" common-dep)
;; (cgo-file "canfoot.gd" common-dep)
;; (cgo-file "cnsptt.gd" common-dep)
;; (cgo-file "havtt.gd" common-dep)
;; (cgo-file "cliffss.gd" common-dep)
;; (cgo-file "ibx.gd" common-dep)
;; (cgo-file "gtbcred.gd" common-dep)
;; (cgo-file "atoplow.gd" common-dep)
;; (cgo-file "dkky.gd" common-dep)
;; (cgo-file "rth.gd" common-dep)
;; (cgo-file "s2a.gd" common-dep)
;; (cgo-file "junglett.gd" common-dep)
;; (cgo-file "jkblev.gd" common-dep)
;; (cgo-file "krtrtt.gd" common-dep)
;; (cgo-file "snox.gd" common-dep)
;; (cgo-file "dkkrtt.gd" common-dep)
;; (cgo-file "docktbox.gd" common-dep)
;; (cgo-file "dkkx.gd" common-dep)
;; (cgo-file "snobowls.gd" common-dep)
;; (cgo-file "sparguss.gd" common-dep)
;; (cgo-file "dockkras.gd" common-dep)
;; (cgo-file "sptmfoot.gd" common-dep)
;; (cgo-file "mongl.gd" common-dep)
;; (cgo-file "icepassw.gd" common-dep)
;; (cgo-file "razvl.gd" common-dep)
;; (cgo-file "swa.gd" common-dep)
;; (cgo-file "spd.gd" common-dep)
;; (cgo-file "peccred.gd" common-dep)
;; (cgo-file "kcr.gd" common-dep)
;; (cgo-file "drc.gd" common-dep)
;; (cgo-file "thbvl3.gd" common-dep)
;; (cgo-file "cars.gd" common-dep)
;; (cgo-file "sigcred.gd" common-dep)
;; (cgo-file "tempfoot.gd" common-dep)
;; (cgo-file "desrev.gd" common-dep)
;; (cgo-file "hjngfoot.gd" common-dep)
;; (cgo-file "dry.gd" common-dep)
;; (cgo-file "sprgstbx.gd" common-dep)
;; (cgo-file "ur8lev.gd" common-dep)
;; (cgo-file "ashvl3.gd" common-dep)
;; (cgo-file "cyx.gd" common-dep)
;; (cgo-file "cyb.gd" common-dep)
;; (cgo-file "kratfoot.gd" common-dep)
;; (cgo-file "cansparw.gd" common-dep)
;; (cgo-file "stx.gd" common-dep)
;; (cgo-file "hvswtt.gd" common-dep)
;; (cgo-file "havseww.gd" common-dep)
;; (cgo-file "torcred.gd" common-dep)
;; (cgo-file "klelev.gd" common-dep)
;; (cgo-file "gtblev.gd" common-dep)
;; (cgo-file "daxtl.gd" common-dep)
;; (cgo-file "sewertt.gd" common-dep)
;; (cgo-file "dsx.gd" common-dep)
;; (cgo-file "brdroomf.gd" common-dep)
;; (cgo-file "havjungs.gd" common-dep)
;; (cgo-file "spartemw.gd" common-dep)
;; (cgo-file "bearl.gd" common-dep)
;; (cgo-file "sno.gd" common-dep)
;; (cgo-file "s3a.gd" common-dep)
;; (cgo-file "kcrx.gd" common-dep)
;; (cgo-file "hjy.gd" common-dep)
;; (cgo-file "cougl.gd" common-dep)
;; (cgo-file "peaktt.gd" common-dep)
;; (cgo-file "dkkrfoot.gd" common-dep)
;; (cgo-file "spartt.gd" common-dep)
;; (cgo-file "icpstt.gd" common-dep)
;; (cgo-file "kcrsplow.gd" common-dep)
;; (cgo-file "iby.gd" common-dep)
;; (cgo-file "snwx.gd" common-dep)
;; (cgo-file "raylev.gd" common-dep)
;; (cgo-file "havsews.gd" common-dep)
;; (cgo-file "drdktbox.gd" common-dep)
;; (cgo-file "ipx.gd" common-dep)
;; (cgo-file "rayvl.gd" common-dep)
;; (cgo-file "torvl.gd" common-dep)
;; (cgo-file "dkd.gd" common-dep)
;; (cgo-file "rustyh.gd" common-dep)
;; (cgo-file "ipy.gd" common-dep)
;; (cgo-file "swb.gd" common-dep)
;; (cgo-file "icepasss.gd" common-dep)
;; (cgo-file "atx.gd" common-dep)
;; (cgo-file "krasw.gd" common-dep)
;; (cgo-file "spattbox.gd" common-dep)
;; (cgo-file "drdkfoot.gd" common-dep)
;; (cgo-file "ictrtt.gd" common-dep)
;; (cgo-file "pantl.gd" common-dep)
;; (cgo-file "spatfoot.gd" common-dep)
;; (cgo-file "kievl.gd" common-dep)
;; (cgo-file "drdy.gd" common-dep)
;; (cgo-file "osmlev.gd" common-dep)
;; (cgo-file "kcrosart.gd" common-dep)
;; (cgo-file "canyons.gd" common-dep)
;; (cgo-file "tigel.gd" common-dep)
;; (cgo-file "drdktt.gd" common-dep)
;; (cgo-file "thblev.gd" common-dep)
;; (cgo-file "dislectf.gd" common-dep)
;; (cgo-file "canyontt.gd" common-dep)
;; (cgo-file "thcvl3.gd" common-dep)
;; (cgo-file "junglew.gd" common-dep)
;; (cgo-file "tarlev.gd" common-dep)
;; (cgo-file "havens.gd" common-dep)
;; (cgo-file "kra.gd" common-dep)
;; (cgo-file "tpd.gd" common-dep)
;; (cgo-file "jgd.gd" common-dep)
;; (cgo-file "cantbox.gd" common-dep)
;; (cgo-file "gilal.gd" common-dep)
;; (cgo-file "icetours.gd" common-dep)
;; (cgo-file "snows.gd" common-dep)
;; (cgo-file "icb.gd" common-dep)
;; (cgo-file "dromes.gd" common-dep)
;; (cgo-file "snowfoot.gd" common-dep)
;; (cgo-file "kcrosss.gd" common-dep)
;; (cgo-file "snowtbox.gd" common-dep)
;; (cgo-file "drb.gd" common-dep)
;; (cgo-file "ices.gd" common-dep)
;; (cgo-file "dka.gd" common-dep)
;; (cgo-file "tpx.gd" common-dep)
;; (cgo-file "ur8vl3.gd" common-dep)
;; (cgo-file "sptmtbox.gd" common-dep)
;; (cgo-file "icetfoot.gd" common-dep)
;; (cgo-file "toadl.gd" common-dep)
;; (cgo-file "havttbox.gd" common-dep)
;; (cgo-file "havjungw.gd" common-dep)
;; (cgo-file "thclev.gd" common-dep)
;; (cgo-file "atollart.gd" common-dep)
;; (cgo-file "hjx.gd" common-dep)
;; (cgo-file "kratours.gd" common-dep)
;; (cgo-file "menu2.gd" common-dep)
;; (cgo-file "hvswfoot.gd" common-dep)
;; (cgo-file "atolls.gd" common-dep)
;; (cgo-file "garageb.gd" common-dep)
;; (cgo-file "icepfoot.gd" common-dep)
;; (cgo-file "docksw.gd" common-dep)
;; (cgo-file "icebergw.gd" common-dep)
;; (cgo-file "pke.gd" common-dep)
;; (cgo-file "kry.gd" common-dep)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Custom Level
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the build system to build the level geometry
;; this path is relative to the custom_levels/jakx folder
;; it should point to the .jsonc file that specifies the level.
;; (build-custom-level "test-zone")
;; ;; the DGO file
;; (goal-src "levels/test-zone/test-zone-obs.gc" "process-focusable")
;; (custom-level-cgo "TSZ.DGO" "test-zone/testzone.gd")

;; ;; generate the art group for a custom actor.
;; ;; requires a .glb model file in custom_assets/jakx/models/custom_levels
;; ;; to also generate a collide-mesh, add :gen-mesh #t
;; (build-actor "test-actor" :gen-mesh #t)

;;;;;;;;;;;;;;;;;;;;;
;; ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;

;; TODO

;;;;;;;;;;;;;;;;;;;;;
;; MUSIC
;;;;;;;;;;;;;;;;;;;;;

;; TODO

;; Jak X has no MUS files
(defstep :in "$ISO/RES/TWEAKVAL.MUS"
  :tool 'copy
  :out '("$OUT/iso/TWEAKVAL.MUS"))

;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;

;; (defstep :in "game/assets/jakx/game_text.gp"
;;   :tool 'text
;;   :out '("$OUT/iso/0COMMON.TXT"
;;          "$OUT/iso/1COMMON.TXT"
;;          "$OUT/iso/2COMMON.TXT"
;;          "$OUT/iso/3COMMON.TXT"
;;          "$OUT/iso/4COMMON.TXT"
;;          "$OUT/iso/5COMMON.TXT"
;;          "$OUT/iso/6COMMON.TXT"
;;          "$OUT/iso/7COMMON.TXT")
;;   )

;; (defstep :in "game/assets/jakx/game_subtitle.gp"
;;   :tool 'subtitle-v2
;;   :out '("$OUT/iso/0SUBTIX.TXT")
;;   )

;;;;;;;;;;;;;;;;;;;;;
;; ISO Group
;;;;;;;;;;;;;;;;;;;;;
;; the iso group is a group of files built by the "(mi)" command.

;; TODO - ensure this is complete

(group-list "iso"
 `(
  ;;  "$OUT/iso/0COMMON.TXT"
  ;;  "$OUT/iso/1COMMON.TXT"
  ;;  "$OUT/iso/2COMMON.TXT"
  ;;  "$OUT/iso/3COMMON.TXT"
  ;;  "$OUT/iso/4COMMON.TXT"
  ;;  "$OUT/iso/5COMMON.TXT"
  ;;  "$OUT/iso/6COMMON.TXT"
  ;;  "$OUT/iso/7COMMON.TXT"
  ;;  "$OUT/iso/0SUBTIX.TXT"
  ;;  "$OUT/iso/VAGDIR.AYB"
  ;;  "$OUT/iso/TWEAKVAL.MUS"
  ;;  ,@(reverse *all-vis*)
  ;;  ,@(reverse *all-str*)
  ;;  ,@(reverse *all-sbk*)
  ;;  ,@(reverse *all-vag*)
   ,@(reverse *all-cgos*))
 )

(group-list "text"
 `("$OUT/iso/0COMMON.TXT"
   "$OUT/iso/1COMMON.TXT"
   "$OUT/iso/2COMMON.TXT"
   "$OUT/iso/3COMMON.TXT"
   "$OUT/iso/4COMMON.TXT"
   "$OUT/iso/5COMMON.TXT"
   "$OUT/iso/6COMMON.TXT"
   "$OUT/iso/7COMMON.TXT"
   "$OUT/iso/0SUBTIX.TXT"
   )
 )

;; used for the type consistency test.
(group-list "all-code"
  `(,@(reverse *all-gc*))
  )

(group "engine"
       "$OUT/iso/0COMMON.TXT"
       "$OUT/iso/0SUBTIX.TXT"
       "$OUT/iso/KERNEL.CGO"
       "$OUT/iso/GAME.CGO"
       "$OUT/iso/VAGDIR.AYB"
       "$OUT/iso/VAGWAD.ENG"
       )
