;;-*-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jak 3 Project File
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
  ;; otherwise, default to jak3
  (#t
   (map-path! "$ISO" "iso_data/jak3/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from decompiler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
  ;; if the user's repl-config has a game version folder, use that
  ((> (string-length (get-game-version-folder)) 0)
   (map-path! "$DECOMP" (string-append "decompiler_out/" (get-game-version-folder) "/")))
  ;; otherwise, default to jak3
  (#t
   (map-path! "$DECOMP" "decompiler_out/jak3/")))

;;;;;;;;;;;;;;;;;;;;;;;
;; Output
;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: the game itself will load from out/jak3/iso and out/jak3/fr3.
(map-path! "$OUT" "out/jak3/")

;; tell the compiler to put its outputs in out/jak3/
(set-output-prefix "jak3/")

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
(load-file "goal_src/jak3/lib/project-lib.gp")
(set-gsrc-folder! "goal_src/jak3")

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

(defstep :in "$DECOMP/textures/tpage-dir.txt"
  :tool 'tpage-dir
  :out '("$OUT/obj/dir-tpages.go")
  )
(hash-table-set! *file-entry-map* "dir-tpages.go" #f)

(cgo-file "game.gd" '("$OUT/obj/gcommon.o" "$OUT/obj/gstate.o" "$OUT/obj/gstring.o" "$OUT/obj/gkernel.o"))

;; note: some of these dependencies are slightly wrong because cgo-file doesn't really handle
;; the case of a .o appearing in multiple dgos. But, if we depend on the last item in both lists, it
;; works out.

(define common-dep '("$OUT/obj/cty-guard-turret-button.o" "$OUT/obj/default-menu-pc.o"))
;; city
(cgo-file "cwi.gd" common-dep)
(cgo-file "lwidea.gd" common-dep)
(cgo-file "lwideb.gd" common-dep)
(cgo-file "lwidec.gd" common-dep)
(cgo-file "ctykora.gd" common-dep)
(cgo-file "cta.gd" common-dep)
(cgo-file "ctb.gd" common-dep)
(cgo-file "ctc.gd" common-dep)
(cgo-file "cia.gd" common-dep)
(cgo-file "cib.gd" common-dep)
(cgo-file "cpo.gd" common-dep)
(cgo-file "ljakdax.gd" common-dep)
(cgo-file "cpa.gd" common-dep)
(cgo-file "cga.gd" common-dep)
(cgo-file "cgb.gd" common-dep)
(cgo-file "cgc.gd" common-dep)
(cgo-file "sta.gd" common-dep)
(cgo-file "cma.gd" common-dep)
(cgo-file "cmb.gd" common-dep)
(cgo-file "ctyasha.gd" common-dep)
(cgo-file "cfa.gd" common-dep)
(cgo-file "cfb.gd" common-dep)
(cgo-file "hiphog.gd" common-dep)
(cgo-file "hideout.gd" common-dep)
(cgo-file "gga.gd" common-dep)
(cgo-file "onintent.gd" common-dep)
(cgo-file "vin.gd" common-dep)
(cgo-file "garage.gd" common-dep)
(cgo-file "kiosk.gd" common-dep)
(cgo-file "oracle.gd" common-dep)
(cgo-file "stadblmp.gd" common-dep)
;; city borrow
(cgo-file "lbbush.gd" common-dep)
(cgo-file "lmeetbrt.gd" common-dep)
(cgo-file "lpower.gd" common-dep)
(cgo-file "lshuttle.gd" common-dep)
(cgo-file "lkiddoge.gd" common-dep)
(cgo-file "lbombbot.gd" common-dep)
(cgo-file "lerlchal.gd" common-dep)
(cgo-file "lprotect.gd" common-dep)
(cgo-file "lpackage.gd" common-dep)
(cgo-file "lportrun.gd" common-dep)
(cgo-file "lhelldog.gd" common-dep)
(cgo-file "lsack.gd" common-dep)
(cgo-file "lprtrace.gd" common-dep)
;; other borrow
(cgo-file "ltentout.gd" common-dep)
(cgo-file "ltentob.gd" common-dep)
(cgo-file "lkeirift.gd" common-dep)
(cgo-file "lgarcsta.gd" common-dep)
(cgo-file "lwhack.gd" common-dep)
;; title
(cgo-file "title.gd" common-dep)
;; intro
(cgo-file "vi1.gd" common-dep)
(cgo-file "introcst.gd" common-dep)
(cgo-file "lintcstb.gd" common-dep)
(cgo-file "lcitylow.gd" common-dep)
;; stadium
(cgo-file "ska.gd" common-dep)
(cgo-file "stb.gd" common-dep)
(cgo-file "stc.gd" common-dep)
(cgo-file "std.gd" common-dep)
(cgo-file "lracelit.gd" common-dep)
(cgo-file "lracebf.gd" common-dep)
(cgo-file "lracecf.gd" common-dep)
(cgo-file "lracedf.gd" common-dep)
(cgo-file "lracebb.gd" common-dep)
(cgo-file "lracecb.gd" common-dep)
(cgo-file "lracedb.gd" common-dep)
(cgo-file "lwidesta.gd" common-dep)
;; fortress
(cgo-file "pri.gd" common-dep)
(cgo-file "ldjakbrn.gd" common-dep)
(cgo-file "lprsncst.gd" common-dep)
(cgo-file "fea.gd" common-dep)
(cgo-file "feb.gd" common-dep)
(cgo-file "fda.gd" common-dep)
(cgo-file "fdb.gd" common-dep)
(cgo-file "fordumpc.gd" common-dep)
(cgo-file "fordumpd.gd" common-dep)
(cgo-file "fra.gd" common-dep)
(cgo-file "frb.gd" common-dep)
;; ruins
(cgo-file "rui.gd" common-dep)
(cgo-file "sag.gd" common-dep)
;; atoll
(cgo-file "ato.gd" common-dep)
(cgo-file "ate.gd" common-dep)
;; sewer
(cgo-file "sew.gd" common-dep)
(cgo-file "seb.gd" common-dep)
(cgo-file "swe.gd" common-dep)
(cgo-file "swb.gd" common-dep)
;; mountain
(cgo-file "mtn.gd" common-dep)
(cgo-file "mtx.gd" common-dep)
(cgo-file "mcn.gd" common-dep)
;; tomb
(cgo-file "toa.gd" common-dep)
(cgo-file "tob.gd" common-dep)
(cgo-file "toc.gd" common-dep)
(cgo-file "tod.gd" common-dep)
(cgo-file "toe.gd" common-dep)
(cgo-file "tbo.gd" common-dep)
(cgo-file "tombext.gd" common-dep)
;; drill
(cgo-file "dri.gd" common-dep)
(cgo-file "drb.gd" common-dep)
(cgo-file "dmi.gd" common-dep)
(cgo-file "drillmtn.gd" common-dep)
;; palace
(cgo-file "pas.gd" common-dep)
(cgo-file "pac.gd" common-dep)
(cgo-file "par.gd" common-dep)
(cgo-file "thr.gd" common-dep)
(cgo-file "palboss.gd" common-dep)
(cgo-file "pae.gd" common-dep)
(cgo-file "palout.gd" common-dep)
(cgo-file "lbrnermk.gd" common-dep)
;; strip
(cgo-file "str.gd" common-dep)
;; castle
(cgo-file "cap.gd" common-dep)
(cgo-file "cas.gd" common-dep)
(cgo-file "cab.gd" common-dep)
(cgo-file "casext.gd" common-dep)
(cgo-file "cascity.gd" common-dep)
;; dig
(cgo-file "dg1.gd" common-dep)
(cgo-file "d3a.gd" common-dep)
(cgo-file "d3b.gd" common-dep)
;; forest
(cgo-file "for.gd" common-dep)
(cgo-file "fob.gd" common-dep)
;; under
(cgo-file "und.gd" common-dep)
(cgo-file "unb.gd" common-dep)
;; consite
(cgo-file "coa.gd" common-dep)
(cgo-file "cob.gd" common-dep)
;; nest
(cgo-file "nes.gd" common-dep)
(cgo-file "neb.gd" common-dep)
(cgo-file "nestt.gd" common-dep)
;; outro
(cgo-file "outrocst.gd" common-dep)
(cgo-file "loutcstb.gd" common-dep)
(cgo-file "lthrnout.gd" common-dep)
(cgo-file "lhipout.gd" common-dep)
(cgo-file "portwall.gd" common-dep)
;; demo
(cgo-file "demo.gd" common-dep)
;; test
(cgo-file "halfpipe.gd" common-dep)
;; scene borrow packages
(cgo-file "lerltess.gd" common-dep)
(cgo-file "lsamergd.gd" common-dep)
(cgo-file "lysamsam.gd" common-dep)
(cgo-file "lsmysbrt.gd" common-dep)
(cgo-file "ltrnysam.gd" common-dep)
(cgo-file "lashthrn.gd" common-dep)
(cgo-file "lcguard.gd" common-dep)
(cgo-file "ljkdxash.gd" common-dep)
(cgo-file "lerrol.gd" common-dep)
(cgo-file "lashgrd.gd" common-dep)
(cgo-file "ltess.gd" common-dep)
(cgo-file "ltrnkrkd.gd" common-dep)
(cgo-file "ltrntess.gd" common-dep)
(cgo-file "lguard.gd" common-dep)
(cgo-file "lerbrngd.gd" common-dep)
(cgo-file "lyskdcd.gd" common-dep)

;; test levels from the ps3 version
(when USE_PS3_LEVELS
  (cgo-file "skatepar.gd" common-dep)
  (cgo-file "4aaron.gd" common-dep)
  (cgo-file "4pal01.gd" common-dep)
  (cgo-file "bsbs.gd" common-dep)
  (cgo-file "chartest.gd" common-dep)
  (cgo-file "ctyfence.gd" common-dep)
  (cgo-file "dptest.gd" common-dep)
  (cgo-file "eitest.gd" common-dep)
  (cgo-file "island1.gd" common-dep)
  (cgo-file "miketest.gd" common-dep)
  (cgo-file "stadocc.gd" common-dep)
  (cgo-file "tatetest.gd" common-dep)
  (cgo-file "teststdc.gd" common-dep)
  (cgo-file "teststdd.gd" common-dep)
  (cgo-file "tobytest.gd" common-dep)
  (cgo-file "wasall.gd" common-dep)
  )

;;;;;;;;;;;;;;;;;;;;;
;; ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;

;; (copy-strs "")

;;;;;;;;;;;;;;;;;;;;;
;; MUSIC
;;;;;;;;;;;;;;;;;;;;;

(copy-vag-files "COM" "ENG" "FRE" "GER" "INT" "ITA" "SPA")

(copy-sbk-files
  "BBUSH1" "BBUSH2" "BLOW1" "BLOW2" "BLOW3H" "BLOWBAR1"
  "BLOWBAR2" "BLOWBAR3" "BOMBBOT1" "CITYALTH" "CITYBB1" "CITYBBF"
  "CITYBBH" "CITYCARH" "CITYFFH" "CITYGRID" "CITYHQ1" "CITYHQ2"
  "CITYHQ3" "CITYKGF" "CITYMHF" "CITYPEDH" "CITYPROT" "CMBTASK1"
  "CMBTASK2" "CMBTASK3" "COMMON" "COMMONJ" "CTYGNBH" "CTYINDH"
  "CTYPORTH" "CTYPRTDH" "CTYSLMAH" "CTYSLMBH" "CTYSLMCH" "CTYSNIP1"
  "DARKECO1" "DESBATL1" "DESBATL2" "DESBOS1" "DESBOS2" "DESBST1"
  "DESERT1" "DESERT2" "DESERT3" "DESERT4" "DESERT5" "DESERT6"
  "DESJUMP1" "DESRESC1" "EMPTY0" "EMPTY1" "EMPTY2" "FACTOR10"
  "FACTORY1" "FACTORY2" "FACTORY3" "FACTORY4" "FACTORY5" "FACTORY6"
  "FACTORY7" "FACTORY8" "FACTORY9" "FACVEH" "FNLBOSS1" "FNLBOSS2"
  "FNLBOSS3" "FOREST1" "FOREST2" "FOREST3" "FOREST4" "FOREST5"
  "FOREST6" "FOREST7" "FOREST8" "FOREST9" "GENBAMB" "GENBTSK1"
  "GUNGAME1" "GUNGAME2" "HANG1" "HANG2" "HANG3" "HIJACK1"
  "INTRO1" "INTRO2" "MECH" "MENU1" "MHCITY1H" "MHCITY2H"
  "MINE1" "MINE10" "MINE2" "MINE3" "MINE4" "MINE5"
  "MINE6" "MINE7" "MINE8" "MINE9" "MODEBORD" "MODEDARK"
  "MODEFLUT" "MODEGUB1" "MODEGUB2" "MODEGUB3" "MODEGUD1" "MODEGUD2"
  "MODEGUD3" "MODEGUR1" "MODEGUR2" "MODEGUR3" "MODEGUY1" "MODEGUY2"
  "MODEGUY3" "MODEIDAX" "MODELIT" "MODEMECH" "NEST1" "NEST2"
  "NEST3" "NEST4" "ONIN1" "PORTATK1" "PORTFITE" "POWERGD1"
  "PRECUR1" "PRECUR2" "PRECUR3" "PRECUR4" "PRECUR5" "RUINTSK1"
  "RUINTSK2" "RUINTSK3" "RUINTSK4" "RUINTSK5" "SEWER1" "SEWER2"
  "SEWER3" "SEWER4" "SEWER5" "SEWER6" "SEWER7" "SEWER8"
  "SEWER9" "STRONG1" "STRONG2" "TEMPLE1" "TEMPLE3" "TEMPLE4"
  "TEMPLE5" "TEMPLE6" "TOWER1" "TOWER2" "TOWER3" "VINROOMH"
  "VOLCANO1" "VOLCANO2" "VOLCANO3" "VOLCANO4" "WASALL1" "WASCITY1"
  "WASCITY2" "WASCITY3" "WASCITY4" "WASDEF1" "WASDEF2" "WASDEF3"
  "WASGAME1" "WASGUN1" "WASGUN2" "WASKANG1" "WASMIR" "WASPALA1"
  "WASRHINO" "WASSCORP" "WASSNAKE" "WASSTAD1" "WASSTAD2" "WASSTAD3"
  "WASSTAD4" "WASSTAD5" "WASSTAD6" "WASTOAD" "WASTURT")

;; Jak 3 has no MUS files
;; (copy-mus-files "" "TWEAKVAL")

;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;

(defstep :in "game/assets/jak3/game_text.gp"
  :tool 'text
  :out '("$OUT/iso/0COMMON.TXT"
         "$OUT/iso/1COMMON.TXT"
         "$OUT/iso/2COMMON.TXT"
         "$OUT/iso/3COMMON.TXT"
         "$OUT/iso/4COMMON.TXT"
         "$OUT/iso/5COMMON.TXT"
         "$OUT/iso/6COMMON.TXT"
         "$OUT/iso/7COMMON.TXT")
  )

(defstep :in "game/assets/jak3/game_subtitle.gp"
  :tool 'subtitle-v2
  :out '("$OUT/iso/0SUBTI2.TXT")
  )

;;;;;;;;;;;;;;;;;;;;;
;; ISO Group
;;;;;;;;;;;;;;;;;;;;;
;; the iso group is a group of files built by the "(mi)" command.

(group-list "iso"
 `("$OUT/iso/0COMMON.TXT"
   "$OUT/iso/1COMMON.TXT"
   "$OUT/iso/2COMMON.TXT"
   "$OUT/iso/3COMMON.TXT"
   "$OUT/iso/4COMMON.TXT"
   "$OUT/iso/5COMMON.TXT"
   "$OUT/iso/6COMMON.TXT"
   "$OUT/iso/7COMMON.TXT"
   "$OUT/iso/0SUBTI2.TXT"
   "$OUT/iso/VAGDIR.AYB"
   ,@(reverse *all-vis*)
   ,@(reverse *all-str*)
   ,@(reverse *all-sbk*)
   ,@(reverse *all-vag*)
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
   "$OUT/iso/0SUBTI2.TXT"
   )
 )

;; used for the type consistency test.
(group-list "all-code"
  `(,@(reverse *all-gc*))
  )

(group "engine"
       "$OUT/iso/0COMMON.TXT"
       "$OUT/iso/0SUBTI2.TXT"
       "$OUT/iso/KERNEL.CGO"
       "$OUT/iso/GAME.CGO"
       "$OUT/iso/VAGDIR.AYB"
       "$OUT/iso/VAGWAD.ENG"
       )

