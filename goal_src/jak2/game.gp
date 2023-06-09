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
  ;; if the user's repl-config has a game version folder, use that
  ((> (string-length (get-game-version-folder)) 0)
   (map-path! "$ISO" (string-append "iso_data/" (get-game-version-folder) "/")))
  ;; otherwise, default to jak2
  (#t
   (map-path! "$ISO" "iso_data/jak2/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from decompiler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
  ;; if the user's repl-config has a game version folder, use that
  ((> (string-length (get-game-version-folder)) 0)
   (map-path! "$DECOMP" (string-append "decompiler_out/" (get-game-version-folder) "/")))
  ;; otherwise, default to jak2
  (#t
   (map-path! "$DECOMP" "decompiler_out/jak2/")))

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
(cgo-file "cwi.gd" common-dep)
(cgo-file "lmeetbrt.gd" common-dep)
(cgo-file "cta.gd" common-dep)
(cgo-file "palout.gd" common-dep)
(cgo-file "std.gd" common-dep)
(cgo-file "for.gd" common-dep)
(cgo-file "hideout.gd" common-dep)
(cgo-file "ctb.gd" common-dep)
(cgo-file "kiosk.gd" common-dep)
(cgo-file "dg1.gd" common-dep)
(cgo-file "feb.gd" common-dep)
(cgo-file "dmi.gd" common-dep)
(cgo-file "oracle.gd" common-dep)
(cgo-file "lbrnermk.gd" common-dep)
(cgo-file "ctc.gd" common-dep)
(cgo-file "fra.gd" common-dep)
(cgo-file "mtn.gd" common-dep)
(cgo-file "introcst.gd" common-dep)
(cgo-file "ate.gd" common-dep)
(cgo-file "cfb.gd" common-dep)
(cgo-file "cab.gd" common-dep)
(cgo-file "str.gd" common-dep)
(cgo-file "ato.gd" common-dep)
(cgo-file "seb.gd" common-dep)
(cgo-file "lpower.gd" common-dep)
(cgo-file "cib.gd" common-dep)
(cgo-file "lshuttle.gd" common-dep)
(cgo-file "fordumpc.gd" common-dep)
(cgo-file "thr.gd" common-dep)
(cgo-file "pri.gd" common-dep)
(cgo-file "lkiddoge.gd" common-dep)
(cgo-file "nestt.gd" common-dep)
(cgo-file "neb.gd" common-dep)
(cgo-file "cob.gd" common-dep)
(cgo-file "lbombbot.gd" common-dep)
(cgo-file "demo.gd" common-dep)
(cgo-file "lerlchal.gd" common-dep)
(cgo-file "outrocst.gd" common-dep)
(cgo-file "par.gd" common-dep)
(cgo-file "fda.gd" common-dep)
(cgo-file "lwhack.gd" common-dep)
(cgo-file "cas.gd" common-dep)
(cgo-file "coa.gd" common-dep)
(cgo-file "toe.gd" common-dep)
(cgo-file "palboss.gd" common-dep)
(cgo-file "frb.gd" common-dep)
(cgo-file "pae.gd" common-dep)
(cgo-file "title.gd" common-dep)
(cgo-file "drillmtn.gd" common-dep)
(cgo-file "pac.gd" common-dep)
(cgo-file "lprotect.gd" common-dep)
(cgo-file "fea.gd" common-dep)
(cgo-file "onintent.gd" common-dep)
(cgo-file "sta.gd" common-dep)
(cgo-file "cgc.gd" common-dep)
(cgo-file "cma.gd" common-dep)
(cgo-file "fdb.gd" common-dep)
(cgo-file "ska.gd" common-dep)
(cgo-file "cia.gd" common-dep)
(cgo-file "toa.gd" common-dep)
(cgo-file "pas.gd" common-dep)
;; (cgo-file "lbbush.gd" common-dep) - moved
;; (cgo-file "lpackage.gd" common-dep) - moved
(cgo-file "lportrun.gd" common-dep)
(cgo-file "cgb.gd" common-dep)
(cgo-file "lhelldog.gd" common-dep)
(cgo-file "gga.gd" common-dep)
(cgo-file "mcn.gd" common-dep)
(cgo-file "vin.gd" common-dep)
(cgo-file "cga.gd" common-dep)
(cgo-file "cpa.gd" common-dep)
(cgo-file "unb.gd" common-dep)
(cgo-file "cpo.gd" common-dep)
(cgo-file "cap.gd" common-dep)
(cgo-file "lbbush.gd" common-dep)
(cgo-file "lpackage.gd" common-dep)
(cgo-file "ctykora.gd" common-dep)
(cgo-file "rui.gd" common-dep)
(cgo-file "lsack.gd" common-dep)
(cgo-file "ctyasha.gd" common-dep)
(cgo-file "hiphog.gd" common-dep)
(cgo-file "tod.gd" common-dep)
(cgo-file "lerltess.gd" common-dep)
(cgo-file "tob.gd" common-dep)
(cgo-file "vi1.gd" common-dep)
(cgo-file "lracecb.gd" common-dep)
(cgo-file "lhipout.gd" common-dep)
(cgo-file "garage.gd" common-dep)
(cgo-file "casext.gd" common-dep)
(cgo-file "stadblmp.gd" common-dep)
(cgo-file "sag.gd" common-dep)
(cgo-file "lintcstb.gd" common-dep)
(cgo-file "lcitylow.gd" common-dep)
(cgo-file "lracelit.gd" common-dep)
(cgo-file "fordumpd.gd" common-dep)
(cgo-file "swe.gd" common-dep)
(cgo-file "sew.gd" common-dep)
(cgo-file "lracedb.gd" common-dep)
(cgo-file "lsamergd.gd" common-dep)
(cgo-file "ljakdax.gd" common-dep)
(cgo-file "lysamsam.gd" common-dep)
(cgo-file "lwidesta.gd" common-dep)
(cgo-file "lsmysbrt.gd" common-dep)
(cgo-file "lwideb.gd" common-dep)
(cgo-file "ldjakbrn.gd" common-dep)
(cgo-file "ltrnysam.gd" common-dep)
(cgo-file "und.gd" common-dep)
(cgo-file "swb.gd" common-dep)
(cgo-file "dri.gd" common-dep)
(cgo-file "cascity.gd" common-dep)
(cgo-file "lashthrn.gd" common-dep)
(cgo-file "lcguard.gd" common-dep)
(cgo-file "tombext.gd" common-dep)
(cgo-file "mtx.gd" common-dep)
(cgo-file "lracedf.gd" common-dep)
(cgo-file "ljkdxash.gd" common-dep)
(cgo-file "lerrol.gd" common-dep)
(cgo-file "d3b.gd" common-dep)
(cgo-file "lwidea.gd" common-dep)
(cgo-file "fob.gd" common-dep)
(cgo-file "lkeirift.gd" common-dep)
(cgo-file "d3a.gd" common-dep)
(cgo-file "lashgrd.gd" common-dep)
(cgo-file "ltess.gd" common-dep)
(cgo-file "portwall.gd" common-dep)
(cgo-file "nes.gd" common-dep)
(cgo-file "lwidec.gd" common-dep)
(cgo-file "cfa.gd" common-dep)
(cgo-file "lprtrace.gd" common-dep)
(cgo-file "tbo.gd" common-dep)
(cgo-file "loutcstb.gd" common-dep)
(cgo-file "ltrnkrkd.gd" common-dep)
(cgo-file "toc.gd" common-dep)
(cgo-file "ltrntess.gd" common-dep)
(cgo-file "lguard.gd" common-dep)
(cgo-file "lerbrngd.gd" common-dep)
(cgo-file "lracecf.gd" common-dep)
(cgo-file "lprsncst.gd" common-dep)
(cgo-file "drb.gd" common-dep)
(cgo-file "lyskdcd.gd" common-dep)
(cgo-file "lthrnout.gd" common-dep)
(cgo-file "stc.gd" common-dep)
(cgo-file "halfpipe.gd" common-dep)
(cgo-file "cmb.gd" common-dep)
(cgo-file "stb.gd" common-dep)
(cgo-file "lracebf.gd" common-dep)
(cgo-file "ltentout.gd" common-dep)
(cgo-file "lgarcsta.gd" common-dep)
(cgo-file "lracebb.gd" common-dep)
(cgo-file "ltentob.gd" common-dep)

;;;;;;;;;;;;;;;;;;;;;
;; ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;

(copy-strs "AT1INT" "AT1RES" "AT2INTRO" "AT3INTRO" "ATSA"
  "ATSARA" "ATSARB" "ATSB" "ATSC" "ATSD" "ATSE" "ATSINTRO"
  "ATSTANK" "BACONSIT" "BASQUID" "BAWIDOW" "CAATIN" "CAATOUT"
  "CAIIINTR" "CAIIRES" "CAKBFINT" "CAKBFRES" "CASEXPLO" "CIADOFF"
  "CIATICAS" "CIATINES" "CIATOUT" "CIC1RIA" "CIC1RIB" "CIC1RRES"
  "CIC2RINT" "CIC2RRES" "CIC3RINT" "CIC3RRES" "CIDGVINT" "CIDSINTR"
  "CIDSRES" "CIECINTR" "CIECRES" "CIEKINTR" "CIGDGUN" "CIGHOVER"
  "CIGYGUN" "CIHKINTR" "CIHKRESO" "CIIDINTR" "CIIHCINT" "CIIHCRES"
  "CIITINTR" "CIITRES" "CIKCINTR" "CIKCRES" "CIKDINTR" "CIMBINTR"
  "CIMBRES" "CIOINTRO" "CIOL0" "CIOL1" "CIOL2" "CIOL3" "CIPHOVER"
  "CIPOGINT" "CIPOGRES" "CIPSINTR" "CISBBINT" "CISLINTR" "CISOPINT"
  "CISUINTR" "CIWAMINT" "CIWAMRES" "COFBRES" "CRINTRO" "CRVICTOR"
  "DAMOLE" "DEDINTRO" "DESCREEN" "DIDEXPLO" "DIFTINTR" "DIFTRES"
  "DIKDSINT" "DRBSBREA" "DRCBREAK" "DRDCTINT" "DRDSINTR" "DRKMHINT"
  "DRTEXPLO" "DRW1" "DRW2" "ECINTRO" "ECVICTOR" "FO2INTRO" "FOBUARA"
  "FOBUARB" "FOCMHINT" "FOFA" "FOFB" "FOHCMHIN" "FOPSIA" "FOPSIB"
  "FOPSRES" "FOSFIA" "FOSFRES" "GRMANIMS" "INCSQUAR" "INPRISON"
  "INSHUT" "INVORTEX" "JAA1" "JAA2" "JAA3" "JAA4" "JAA5" "JAA6"
  "JAA7" "JABOARD" "JACARRY" "JAD1" "JAD2" "JAD3" "JAD4" "JAD5"
  "JADARK" "JADON" "JADUMMY" "JAFLUT" "JAGUN" "JAICE" "JAINDAX"
  "JAMECH" "JAPEGASU" "JAPIDAX" "JAPILOT" "JAPOLE" "JARACER" "JASWIM"
  "JATUBE" "JATURRET" "KEANIM" "KEGARAGE" "KILTRNKR" "KILYSKDC"
  "KINESTB" "KITOMBD" "KRDRES" "MOFINTRO" "MOGRES" "MOLRES" "MOSRES"
  "MTAR1" "MTPBRA" "MTSPRA" "MTSPRB" "MTSPRC" "NEATIN" "NEATOUT"
  "NEBBRES" "NEKBFIB" "NEKBFMID" "ONGAME" "OUHIPHOG" "OUNEST"
  "OUPALACE" "OUPORT" "PABRES" "PAOWRB" "PAOWRES" "PASIRES"
  "PRMINIMA" "RHW1" "RHW2" "RUB1" "RUBW1" "RUBW2" "RUBW3"
  "RUBW4" "RUBW5" "RUBW6" "RUDPA1" "RUDPB1" "RUDPC1" "RUGTHRES"
  "RUPC1" "RUPC2" "RUPC3" "RUSINTRO" "RUSVICTO" "RUTINTRO"
  "RUTVICTO" "SALSAMER" "SCBOOK" "SE1INTRO" "SE1RES" "SE2INTRO"
  "SEBUSINT" "SEBUSRES" "SEC1" "SEDRES" "SEHOSEHE" "SESGRUNT"
  "SEW1" "SEW2" "TELHIPHO" "TELTRNTE" "TELWHACK" "TESCENE" "TIDINTRO"
  "TOBBA" "TOBBB" "TOBINTRO" "TOBOPEN" "TOBRES" "TOBSTART" "TOFTINTR"
  "TOSC0" "TOSC1" "TOSC2" "TOSSCARE" "TOTURRET" "TOUPOLES" "TOUSTART"
  "TOUWATER" "UNBD1" "UNBD2" "UNBD3" "UNBD4" "UNCONE" "UNCTHREE" "UNCTWO"
  "UNFSRES" "UNGSORES" "VIRESCUE" "VIRINTRO" "WOMAP" "YOFOREST" "YOLTRNYS"
  "YOLYSAMS" "YOLYSKDC" "YOONINTE" "YOTOMBD")

;;;;;;;;;;;;;;;;;;;;;
;; MUSIC
;;;;;;;;;;;;;;;;;;;;;

(copy-vag-files "ENG" "FRE" "GER" "ITA" "JAP" "KOR" "SPA")

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

(defstep :in "game/assets/jak2/game_subtitle.gp"
  :tool 'subtitle2
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
   "$OUT/iso/TWEAKVAL.MUS"
   "$OUT/iso/VAGDIR.AYB"
   ,@(reverse *all-vis*)
   ,@(reverse *all-str*)
   ,@(reverse *all-sbk*)
   ,@(reverse *all-mus*)
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

