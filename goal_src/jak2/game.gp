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
;; (define common-dep ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cwi.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lmeetbrt.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cta.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "palout.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "std.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "for.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "hideout.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ctb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "kiosk.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "dg1.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "feb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "dmi.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "oracle.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lbrnermk.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ctc.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "fra.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "mtn.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "introcst.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ate.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cfb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cab.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "str.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ato.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "seb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lpower.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cib.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lshuttle.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "fordumpc.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "thr.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "pri.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lkiddoge.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "nestt.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "neb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cob.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lbombbot.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "demo.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lerlchal.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "outrocst.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "par.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "fda.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lwhack.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cas.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "coa.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "toe.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "palboss.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "frb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "pae.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "title.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "drillmtn.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "pac.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lprotect.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "fea.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "onintent.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "sta.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cgc.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cma.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "fdb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ska.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cia.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "toa.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "pas.gd" ("$OUT/obj/cty-guard-turret-button.o"))
;; (cgo-file "lbbush.gd" ("$OUT/obj/cty-guard-turret-button.o")) - moved
;; (cgo-file "lpackage.gd" ("$OUT/obj/cty-guard-turret-button.o")) - moved
(cgo-file "lportrun.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cgb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lhelldog.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "gga.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "mcn.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "vin.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cga.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cpa.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "unb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cpo.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cap.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lbbush.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lpackage.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ctykora.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "rui.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lsack.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ctyasha.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "hiphog.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "tod.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lerltess.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "tob.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "vi1.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lracecb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lhipout.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "garage.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "casext.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "stadblmp.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "sag.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lintcstb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lcitylow.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lracelit.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "fordumpd.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "swe.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "sew.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lracedb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lsamergd.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ljakdax.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lysamsam.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lwidesta.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lsmysbrt.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lwideb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ldjakbrn.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ltrnysam.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "und.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "swb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "dri.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cascity.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lashthrn.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lcguard.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "tombext.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "mtx.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lracedf.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ljkdxash.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lerrol.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "d3b.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lwidea.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "fob.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lkeirift.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "d3a.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lashgrd.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ltess.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "portwall.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "nes.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lwidec.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cfa.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lprtrace.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "tbo.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "loutcstb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ltrnkrkd.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "toc.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ltrntess.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lguard.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lerbrngd.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lracecf.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lprsncst.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "drb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lyskdcd.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lthrnout.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "stc.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "halfpipe.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "cmb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "stb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lracebf.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ltentout.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lgarcsta.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "lracebb.gd" ("$OUT/obj/cty-guard-turret-button.o"))
(cgo-file "ltentob.gd" ("$OUT/obj/cty-guard-turret-button.o"))

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
