;;-*-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jak 2 Project File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see goal_src/jak1/game.gp for more detailed explanation

;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from ISO
;;;;;;;;;;;;;;;;;;;;;;;

(cond
  ;; extractor can override everything by providing *use-iso-data-path*
  (*use-iso-data-path*
    (map-path! "$ISO" (string-append *iso-data* "/")))
  (#t
    (map-path! "$ISO" "iso_data/jak2/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from decompiler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(map-path! "$DECOMP" "decompiler_out/jak2/")

;;;;;;;;;;;;;;;;;;;;;;;
;; Output
;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: the game itself will load from out/jak2/iso and out/jak2/fr3.
(map-path! "$OUT" "out/jak2/")

;; tell the compiler to put its outputs in out/jak2/
(set-output-prefix "jak2/")

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
(load-file "goal_src/jak2/lib/project-lib.gp")
(set-gsrc-folder! "goal_src/jak2")

;;;;;;;;;;;;;;;;;
;; GOAL Kernel
;;;;;;;;;;;;;;;;;

(cgo-file "kernel.gd" ())

;;;;;;;;;;;;;;;;;;;;;
;; DGOs
;;;;;;;;;;;;;;;;;;;;;

(defstep :in "$DECOMP/textures/tpage-dir.txt"
  :tool 'tpage-dir
  :out '("$OUT/obj/dir-tpages.go")
  )
(hash-table-set! *file-entry-map* "dir-tpages.go" #f)

(cgo-file "engine.gd" ("$OUT/obj/gcommon.o" "$OUT/obj/gstate.o" "$OUT/obj/gstring.o" "$OUT/obj/gkernel.o"))
(cgo-file "game.gd" ("$OUT/obj/gcommon.o" "$OUT/obj/gstate.o" "$OUT/obj/gstring.o" "$OUT/obj/gkernel.o"))

;; TODO - can't use a variable because :deps downstream doesn't take a proper list (can't pass it in quoted)
;; (define common-dep ("$OUT/obj/los-control.o"))

(cgo-file "pri.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cwi.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lwidea.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cta.gd" ("$OUT/obj/los-control.o"))
(cgo-file "vi1.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cascity.gd" ("$OUT/obj/los-control.o"))
(cgo-file "casext.gd" ("$OUT/obj/los-control.o"))
(cgo-file "d3b.gd" ("$OUT/obj/los-control.o"))
(cgo-file "drb.gd" ("$OUT/obj/los-control.o"))
(cgo-file "dri.gd" ("$OUT/obj/los-control.o"))
(cgo-file "fob.gd" ("$OUT/obj/los-control.o"))
(cgo-file "fordumpd.gd" ("$OUT/obj/los-control.o"))
(cgo-file "garage.gd" ("$OUT/obj/los-control.o"))
(cgo-file "halfpipe.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lashgrd.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lashthrn.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lcguard.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lcitylow.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ldjakbrn.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lerbrngd.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lerltess.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lerrol.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lgarcsta.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lguard.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lhipout.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lintcstb.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "ljakdax.gd" ("$OUT/obj/los-control.o"))
(cgo-file "for.gd" ("$OUT/obj/los-control.o"))
(cgo-file "hideout.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ctb.gd" ("$OUT/obj/los-control.o"))
(cgo-file "kiosk.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "ljkdxash.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lkeirift.gd" ("$OUT/obj/los-control.o"))
(cgo-file "loutcstb.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lprsncst.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lracebb.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lracebf.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lracecb.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lracecf.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lracedb.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lracedf.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lracelit.gd" ("$OUT/obj/los-control.o"))
(cgo-file "feb.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lsamergd.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lsmysbrt.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ltentob.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "ltentout.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "ltess.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lthrnout.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "ltrnkrkd.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "ltrntess.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ltrnysam.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lwideb.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lwidec.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lwidesta.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lysamsam.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lyskdcd.gd" ("$OUT/obj/los-control.o"))
(cgo-file "mtx.gd" ("$OUT/obj/los-control.o"))
(cgo-file "portwall.gd" ("$OUT/obj/los-control.o"))
(cgo-file "sag.gd" ("$OUT/obj/los-control.o"))
(cgo-file "sew.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lbrnermk.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ctc.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "stadblmp.gd" ("$OUT/obj/los-control.o"))
(cgo-file "swe.gd" ("$OUT/obj/los-control.o"))
(cgo-file "mtn.gd" ("$OUT/obj/los-control.o"))
(cgo-file "introcst.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ate.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cfa.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cab.gd" ("$OUT/obj/los-control.o"))
(cgo-file "tbo.gd" ("$OUT/obj/los-control.o"))
(cgo-file "tob.gd" ("$OUT/obj/los-control.o"))
(cgo-file "toc.gd" ("$OUT/obj/los-control.o"))
(cgo-file "tombext.gd" ("$OUT/obj/los-control.o"))
(cgo-file "und.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ato.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lpower.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cib.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lshuttle.gd" ("$OUT/obj/los-control.o"))
(cgo-file "fordumpc.gd" ("$OUT/obj/los-control.o"))
(cgo-file "thr.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cob.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lbombbot.gd" ("$OUT/obj/los-control.o"))
(cgo-file "demo.gd" ("$OUT/obj/los-control.o"))
(cgo-file "outrocst.gd" ("$OUT/obj/los-control.o"))
(cgo-file "par.gd" ("$OUT/obj/los-control.o"))
(cgo-file "fda.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lwhack.gd" ("$OUT/obj/los-control.o"))
(cgo-file "coa.gd" ("$OUT/obj/los-control.o"))
(cgo-file "toe.gd" ("$OUT/obj/los-control.o"))
(cgo-file "palboss.gd" ("$OUT/obj/los-control.o"))
(cgo-file "frb.gd" ("$OUT/obj/los-control.o"))
(cgo-file "title.gd" ("$OUT/obj/los-control.o"))
(cgo-file "drillmtn.gd" ("$OUT/obj/los-control.o"))
(cgo-file "pac.gd" ("$OUT/obj/los-control.o"))
(cgo-file "onintent.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cgc.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cma.gd" ("$OUT/obj/los-control.o"))
(cgo-file "ska.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cia.gd" ("$OUT/obj/los-control.o"))
(cgo-file "toa.gd" ("$OUT/obj/los-control.o"))
(cgo-file "pas.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lbbush.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lpackage.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lportrun.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cgb.gd" ("$OUT/obj/los-control.o"))
;; (cgo-file "lhelldog.gd" ("$OUT/obj/los-control.o"))
(cgo-file "gga.gd" ("$OUT/obj/los-control.o"))
(cgo-file "mcn.gd" ("$OUT/obj/los-control.o"))
(cgo-file "vin.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cga.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cpa.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cpo.gd" ("$OUT/obj/los-control.o"))
(cgo-file "cap.gd" ("$OUT/obj/los-control.o"))
(cgo-file "lsack.gd" ("$OUT/obj/los-control.o"))
(cgo-file "hiphog.gd" ("$OUT/obj/los-control.o"))

;;;;;;;;;;;;;;;;;;;;;
;; ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;

;; intro cutscenes
(copy-strs "INSHUT" "INVORTEX" "INCSQUAR" "INPRISON")

;; jak ambient
(copy-strs "JAA1" "JAA2" "JAA3" "JAA4" "JAA5" "JAA6" "JAA7")
(copy-strs "AT1RES" "ATSINTRO" "ATSTANK" "ATSA" "ATSB" "ATSC" "ATSD" "ATSE" "ATSARA" "ATSARB")
(copy-strs "DRTEXPLO")
(copy-strs "DRW1")
(copy-strs "OUPORT")
(copy-strs "OUNEST")
(copy-strs "CIWAMINT" "CIWAMRES")
(copy-strs "RHW1" "RHW2")
(copy-strs "PAOWRB" "PABRES")
(copy-strs "RUB1" "RUBW1" "RUBW2" "RUBW3" "RUBW4" "RUBW5" "RUBW6" "RUDPA1" "RUPC1" "RUPC2" "RUPC3" "RUGTHRES" "RUSVICTO" "RUTVICTO")
(copy-strs "ECVICTOR")
(copy-strs "DESCREEN" "TIDINTRO")
(copy-strs "TOBINTRO" "TOBRES")
(copy-strs "UNBD1" "UNBD2" "UNBD3" "UNBD4" "UNFSRES" "UNCONE" "UNCTWO" "UNCTHREE" "UNGSORES")

;;;;;;;;;;;;;;;;;;;;;
;; MUSIC
;;;;;;;;;;;;;;;;;;;;;

(copy-vag-files "ENG")

(copy-sbk-files
  "ASHTAN1" "ASHTAN2" "ATOLL1" "ATOLL2" "ATOLL3" "ATOLL4"
  "BBUSH1" "BOARD" "BOMBBOT1" "CASBOSS1" "CASBOSS2" "CASBOSS3"
  "CASTLE1" "CASTLE2" "CASTLE3" "COMMON" "COMMONJ" "CONSITE1"
  "CONSITE2" "CONSITE3" "CTYFARM1" "CTYWIDE1" "CTYWIDE2" "CTYWIDE3"
  "CTYWIDE4" "CTYWIDE5" "DEMO1" "DIG1" "DIG2" "DIG3" "DIG4" "DIG5"
  "DIG6" "DIG7" "DIG8" "DRILL1" "DRILL2" "DRILL3" "DRILL4" "DRILL5"
  "DRILL6" "DRILL7" "DRILL8" "EMPTY0" "EMPTY1" "EMPTY2" "ERLCHAL1"
  "ESCKID1" "FORDUMP1" "FORDUMP2" "FOREST1" "FOREST2" "FOREST3"
  "FOREST4" "FOREST5" "FOREXIT1" "FOREXIT2" "FORRESC1" "FORRESC2"
  "GUN" "GUNGAME1" "HELLDOG1" "HIDEOUT1" "HIPHOG1" "INTRO1" "INTRO2"
  "INTRO3" "MECH" "MECHWAT" "MEETBRT1" "MENU1" "MOUNT1" "MOUNT2"
  "MOUNT3" "NEST1" "NEST2" "NEST3" "NEST4" "NEST5" "NEST6" "ONIN1"
  "ONIN2" "ORACLE1" "OUTRO1" "PALCAB1" "PALCAB2" "PALCAB3" "PALENT1"
  "PALENT2" "PALENT3" "PALROOF1" "PALROOF2" "PALROOF3" "PORTRUN1"
  "PROTECT1" "RUINS1" "RUINS2" "RUINS3" "SACK1" "SEWER1" "SEWER2"
  "SEWER3" "SEWER4" "SEWER5" "SEWER6" "SKATE1" "STADIUM1" "STRIP1"
  "STRIP2" "STRIP3" "TOMB1" "TOMB2" "TOMB3" "TOMB4" "TOMB5" "TOMB6"
  "TOMB7" "TOMB8" "TOMB9" "UNDER1" "UNDER2" "UNDER3" "UNDER4" "UNDER5" "VINROOM1")

(copy-mus-files
  "ATOLL" "BATTLE" "CITY1" "CREDITS" "DANGER" "DANGER1" "DANGER2"
  "DANGER3" "DANGER4" "DANGER6" "DANGER7" "DANGER9" "DANGER10"
  "DANGER11" "DIG" "FOREST" "FORTRESS" "MOUNTAIN" "PALCAB" "RACE"
  "RUINS" "SEWER" "STRIP" "TOMB" "TWEAKVAL")

;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;

(defstep :in "game/assets/jak2/game_text.gp"
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

;;;;;;;;;;;;;;;;;;;;;
;; ISO Group
;;;;;;;;;;;;;;;;;;;;;
;; the iso group is a group of files built by the "(mi)" command.

(group-list "iso"
 `("$OUT/iso/0COMMON.TXT"
   ,@(reverse *all-vis*)
   ,@(reverse *all-str*)
   ,@(reverse *all-sbk*)
   ,@(reverse *all-mus*)
   ,@(reverse *all-vag*)
   ,@(reverse *all-cgos*))
 )

(group-list "text"
 `("$OUT/iso/0COMMON.TXT")
 )

;; used for the type consistency test.
(group-list "all-code"
  `(,@(reverse *all-gc*))
  )

(group "engine"
       "$OUT/iso/KERNEL.CGO"
       "$OUT/iso/GAME.CGO"
       )
