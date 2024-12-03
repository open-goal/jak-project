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

(define common-dep '("$OUT/obj/default-menu-pc.o"))

;; wascity
(cgo-file "waschase.gd" common-dep)
(cgo-file "wasdefen.gd" common-dep)
(cgo-file "waspala.gd" common-dep)
(cgo-file "wasseem.gd" common-dep)
(cgo-file "wca.gd" common-dep)
(cgo-file "wcb.gd" common-dep)
(cgo-file "wcaseem.gd" common-dep)
(cgo-file "wascast.gd" common-dep)
(cgo-file "cwi.gd" common-dep) ;; ctywide
(cgo-file "wasleapr.gd" common-dep)
(cgo-file "wasall.gd" common-dep)
(cgo-file "desresc.gd" common-dep)
(cgo-file "wsd.gd" common-dep) ;; wasdoors (garage)
(cgo-file "waspgame.gd" common-dep)
(cgo-file "wwd.gd" common-dep) ;; waswide
; ;; arena
(cgo-file "wasstada.gd" common-dep)
(cgo-file "wasstadb.gd" common-dep)
(cgo-file "wasstadc.gd" common-dep)
(cgo-file "arenacst.gd" common-dep)
; ;; desert
(cgo-file "desa.gd" common-dep)
(cgo-file "desb.gd" common-dep)
(cgo-file "desbattl.gd" common-dep)
(cgo-file "desbcst.gd" common-dep)
(cgo-file "desc.gd" common-dep)
(cgo-file "deschase.gd" common-dep)
(cgo-file "desd.gd" common-dep)
(cgo-file "dese.gd" common-dep)
(cgo-file "deserrol.gd" common-dep)
(cgo-file "desf.gd" common-dep)
(cgo-file "desg.gd" common-dep)
(cgo-file "desh.gd" common-dep)
(cgo-file "deshover.gd" common-dep)
(cgo-file "deshunt.gd" common-dep)
(cgo-file "desinter.gd" common-dep)
(cgo-file "desjump.gd" common-dep)
(cgo-file "desliz.gd" common-dep)
(cgo-file "desoasis.gd" common-dep)
(cgo-file "desrace1.gd" common-dep)
(cgo-file "desrace2.gd" common-dep)
(cgo-file "desrally.gd" common-dep)
(cgo-file "desrescc.gd" common-dep)
(cgo-file "desrescg.gd" common-dep)
(cgo-file "destrack.gd" common-dep)
(cgo-file "dst.gd" common-dep)
(cgo-file "desw.gd" common-dep)
(cgo-file "desboss1.gd" common-dep)
(cgo-file "desboss2.gd" common-dep)
(cgo-file "oasiscst.gd" common-dep)
(cgo-file "warpcast.gd" common-dep) ;; air train
; ;; nest
(cgo-file "nsa.gd" common-dep)
(cgo-file "nsb.gd" common-dep)
; ;; temple
(cgo-file "tema.gd" common-dep)
(cgo-file "temb.gd" common-dep)
(cgo-file "temc.gd" common-dep)
(cgo-file "temd.gd" common-dep)
(cgo-file "temp.gd" common-dep)
(cgo-file "templee.gd" common-dep)
(cgo-file "temx.gd" common-dep)
; ;; hang
(cgo-file "hga.gd" common-dep)
(cgo-file "hgb.gd" common-dep)
; ;; volcano
(cgo-file "voca.gd" common-dep)
(cgo-file "vocx.gd" common-dep)
;; mine
(cgo-file "mia.gd" common-dep)
(cgo-file "mib.gd" common-dep)
(cgo-file "mic.gd" common-dep)
(cgo-file "mined.gd" common-dep)
(cgo-file "minee.gd" common-dep)
; ;; city
(cgo-file "cfa.gd" common-dep) ;; ctyfarm
(cgo-file "cfb.gd" common-dep)
(cgo-file "cgb.gd" common-dep) ;; ctygen
(cgo-file "cia.gd" common-dep) ;; ctyind
(cgo-file "cib.gd" common-dep)
(cgo-file "cpo.gd" common-dep) ;; ctyport
(cgo-file "cta.gd" common-dep)
(cgo-file "ctb.gd" common-dep)
(cgo-file "ctc.gd" common-dep)
(cgo-file "ctypepa.gd" common-dep) ;; citizens
(cgo-file "ctypepb.gd" common-dep) ;; predator
(cgo-file "ctypepc.gd" common-dep) ;; empty
(cgo-file "ctypesa.gd" common-dep) ;; guards
(cgo-file "ctypesb.gd" common-dep) ;; metal heads
(cgo-file "ctypesc.gd" common-dep) ;; kg
(cgo-file "ctycara.gd" common-dep) ;; cars
(cgo-file "ctycarb.gd" common-dep) ;; bikes
(cgo-file "ctycarc.gd" common-dep) ;; hellcat
(cgo-file "ctycarkg.gd" common-dep) ;; empty
(cgo-file "onintent.gd" common-dep)
(cgo-file "hhg.gd" common-dep) ;; hiphog
(cgo-file "gga.gd" common-dep) ;; gungame
(cgo-file "gungame1.gd" common-dep)
(cgo-file "gungame2.gd" common-dep)
(cgo-file "powergd.gd" common-dep)
(cgo-file "freehq.gd" common-dep)
(cgo-file "vin.gd" common-dep)
(cgo-file "freecast.gd" common-dep)
(cgo-file "citycast.gd" common-dep)
(cgo-file "gridcst.gd" common-dep) ;; city-destroy-grid-res
(cgo-file "slumbset.gd" common-dep) ;; sewer-met-hum-intro
; ;; sewer
(cgo-file "sea.gd" common-dep)
(cgo-file "seb.gd" common-dep)
(cgo-file "sec.gd" common-dep)
(cgo-file "sed.gd" common-dep)
(cgo-file "see.gd" common-dep)
(cgo-file "sef.gd" common-dep)
(cgo-file "seg.gd" common-dep)
(cgo-file "seh.gd" common-dep)
(cgo-file "sei.gd" common-dep)
(cgo-file "sej.gd" common-dep)
(cgo-file "sek.gd" common-dep)
(cgo-file "sel.gd" common-dep)
(cgo-file "sem.gd" common-dep)
(cgo-file "sen.gd" common-dep)
(cgo-file "seo.gd" common-dep)
; ;; mhcity
(cgo-file "mhca.gd" common-dep)
(cgo-file "mhcb.gd" common-dep)
(cgo-file "mhctycst.gd" common-dep)
;; forest
(cgo-file "frsta.gd" common-dep)
(cgo-file "frstb.gd" common-dep)
(cgo-file "frstx.gd" common-dep)
;; factory
(cgo-file "lfacrm1.gd" common-dep)
(cgo-file "factorya.gd" common-dep)
(cgo-file "facb.gd" common-dep)
(cgo-file "facc.gd" common-dep)
(cgo-file "facd.gd" common-dep)
; ;; tower
(cgo-file "towb.gd" common-dep)
(cgo-file "towera.gd" common-dep)
(cgo-file "towerc.gd" common-dep)
(cgo-file "towercst.gd" common-dep)
; ;; stadium
(cgo-file "sta.gd" common-dep)
(cgo-file "staa.gd" common-dep)
(cgo-file "stb.gd" common-dep)
; ;; rubble
(cgo-file "rbct.gd" common-dep)
(cgo-file "ruba.gd" common-dep)
(cgo-file "ruba2.gd" common-dep)
(cgo-file "rubb.gd" common-dep)
(cgo-file "rubc.gd" common-dep)
; ;; comb
(cgo-file "comba.gd" common-dep)
(cgo-file "combb.gd" common-dep)
(cgo-file "combc.gd" common-dep)
(cgo-file "combd.gd" common-dep)
(cgo-file "combe.gd" common-dep)
(cgo-file "combn.gd" common-dep)
(cgo-file "combx.gd" common-dep)
(cgo-file "raila.gd" common-dep)
(cgo-file "railb.gd" common-dep)
(cgo-file "railb2.gd" common-dep)
(cgo-file "railc.gd" common-dep)
(cgo-file "railcst.gd" common-dep)
(cgo-file "raild.gd" common-dep)
(cgo-file "raile.gd" common-dep)
(cgo-file "railf.gd" common-dep)
(cgo-file "railx.gd" common-dep)
; ;; precursor
(cgo-file "lmech.gd" common-dep)
(cgo-file "preca.gd" common-dep)
(cgo-file "precb.gd" common-dep)
(cgo-file "precc.gd" common-dep)
(cgo-file "precd.gd" common-dep)
; ;; title/intro
(cgo-file "win.gd" common-dep) ;; wasintro
(cgo-file "title.gd" common-dep)
(cgo-file "inttitle.gd" common-dep)
(cgo-file "intpalrf.gd" common-dep) ;; intro-palace-roof
(cgo-file "ipf.gd" common-dep) ;; intro-palace-fall
(cgo-file "introcst.gd" common-dep)
; ;; outro
(cgo-file "outcast3.gd" common-dep)
(cgo-file "outrocst.gd" common-dep)
; ;; museum
(cgo-file "museum.gd" common-dep)
(cgo-file "museum2.gd" common-dep)
(cgo-file "museum3.gd" common-dep)
(cgo-file "museum3b.gd" common-dep)
(cgo-file "museum4.gd" common-dep)
(cgo-file "museum4b.gd" common-dep)
;; test
(cgo-file "halfpipe.gd" common-dep)
; ;; borrow
(cgo-file "lashelin.gd" common-dep)
(cgo-file "lbbring1.gd" common-dep)
(cgo-file "lbbring2.gd" common-dep)
(cgo-file "lbbring3.gd" common-dep)
(cgo-file "lbbring4.gd" common-dep)
(cgo-file "lbbring5.gd" common-dep)
(cgo-file "lbbring6.gd" common-dep)
(cgo-file "lbbsdrp1.gd" common-dep)
(cgo-file "lbbsdrp2.gd" common-dep)
(cgo-file "lbbsdrp3.gd" common-dep)
(cgo-file "lbbspid.gd" common-dep)
(cgo-file "lbbspirt.gd" common-dep)
(cgo-file "lbbsprt2.gd" common-dep)
(cgo-file "lbbsprt3.gd" common-dep)
(cgo-file "lbbtcha1.gd" common-dep)
(cgo-file "lbbtcha2.gd" common-dep)
(cgo-file "lbbtcha3.gd" common-dep)
(cgo-file "lbiped.gd" common-dep)
(cgo-file "lblowcst.gd" common-dep)
(cgo-file "lblowtkg.gd" common-dep)
(cgo-file "lblowtmh.gd" common-dep)
(cgo-file "lbombbot.gd" common-dep)
(cgo-file "lcitysml.gd" common-dep)
(cgo-file "lctyass.gd" common-dep)
(cgo-file "lctyblow.gd" common-dep)
(cgo-file "lctydest.gd" common-dep)
(cgo-file "lctyhijk.gd" common-dep)
(cgo-file "lctypalt.gd" common-dep)
(cgo-file "lctypatk.gd" common-dep)
(cgo-file "lctyprot.gd" common-dep)
(cgo-file "lctysnpr.gd" common-dep)
(cgo-file "ldamklev.gd" common-dep)
(cgo-file "ldampeck.gd" common-dep)
(cgo-file "ldampksm.gd" common-dep)
(cgo-file "ldamsig.gd" common-dep)
(cgo-file "ldax.gd" common-dep)
(cgo-file "ldesgcst.gd" common-dep)
(cgo-file "ldmpckgn.gd" common-dep)
(cgo-file "lerrol.gd" common-dep)
(cgo-file "lfacb.gd" common-dep)
(cgo-file "lfaccar.gd" common-dep)
(cgo-file "lfaccity.gd" common-dep)
(cgo-file "lfaco.gd" common-dep)
(cgo-file "lfacrm2.gd" common-dep)
(cgo-file "lfactory.gd" common-dep)
(cgo-file "lform.gd" common-dep)
(cgo-file "lforp.gd" common-dep)
(cgo-file "lforring.gd" common-dep)
(cgo-file "lfreeout.gd" common-dep)
(cgo-file "lgunnorm.gd" common-dep)
(cgo-file "lgunrnc.gd" common-dep)
(cgo-file "ljak.gd" common-dep)
(cgo-file "ljakc.gd" common-dep)
(cgo-file "ljakcklv.gd" common-dep)
(cgo-file "ljakklev.gd" common-dep)
(cgo-file "ljakndax.gd" common-dep)
(cgo-file "ljaksig.gd" common-dep)
(cgo-file "ljinx.gd" common-dep)
(cgo-file "ljkcdmkl.gd" common-dep)
(cgo-file "ljkdmpk.gd" common-dep)
(cgo-file "ljkdxvin.gd" common-dep)
(cgo-file "ljkfeet.gd" common-dep)
(cgo-file "ljndklev.gd" common-dep)
(cgo-file "lkeira.gd" common-dep)
(cgo-file "lkleever.gd" common-dep)
(cgo-file "lmhca.gd" common-dep)
(cgo-file "lmhcb.gd" common-dep)
(cgo-file "lnstcst.gd" common-dep)
(cgo-file "lnstoa.gd" common-dep)
(cgo-file "lnstobb.gd" common-dep)
(cgo-file "lnstobc.gd" common-dep)
(cgo-file "loninsim.gd" common-dep)
(cgo-file "loutro.gd" common-dep)
(cgo-file "loutro2.gd" common-dep)
(cgo-file "loutro3.gd" common-dep)
(cgo-file "lpatk.gd" common-dep)
(cgo-file "lpatkcs.gd" common-dep)
(cgo-file "lprecc.gd" common-dep)
(cgo-file "lprenme.gd" common-dep)
(cgo-file "lptrl.gd" common-dep)
(cgo-file "lsamos.gd" common-dep)
(cgo-file "lseemwca.gd" common-dep)
(cgo-file "lsig.gd" common-dep)
(cgo-file "lsigjakc.gd" common-dep)
(cgo-file "lsigklv.gd" common-dep)
(cgo-file "lsnkwhls.gd" common-dep)
(cgo-file "ltnfxhip.gd" common-dep)
(cgo-file "ltnjxhip.gd" common-dep)
(cgo-file "ltorn.gd" common-dep)
(cgo-file "ltornjnx.gd" common-dep)
(cgo-file "ltornsam.gd" common-dep)
(cgo-file "ltowa.gd" common-dep)
(cgo-file "ltowb.gd" common-dep)
(cgo-file "ltowcity.gd" common-dep)
(cgo-file "ltrtwhls.gd" common-dep)
(cgo-file "lvincst.gd" common-dep)
(cgo-file "lwasbbv.gd" common-dep)
(cgo-file "lwassig.gd" common-dep)
(cgo-file "lwlandm.gd" common-dep)
(cgo-file "lwstdpck.gd" common-dep)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Custom Level
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the build system to build the level geometry
;; this path is relative to the custom_levels/jak3 folder
;; it should point to the .jsonc file that specifies the level.
(build-custom-level "test-zone")
;; the DGO file
(custom-level-cgo "TSZ.DGO" "test-zone/testzone.gd")

;;;;;;;;;;;;;;;;;;;;;
;; ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;

(copy-strs
  "ARF1INTR"  "CIBBRES"   "CIPFINTR"  "DAD16"     "DEAR1RES"  "DEHRES"
  "ARF1RES"   "CIBTINTR"  "CIPGINTR"  "DAD17"     "DEAR2INT"  "DEJGOTA"
  "ARF2INTR"  "CIDGRES"   "CIPGRES"   "DAD18"     "DEATIN"    "DEJGOTB"
  "ARF2RES"   "CIFCEINT"  "CIPHINTR"  "DAD20"     "DEATOUT"   "DEJGOTC"
  "ARF3INTR"  "CIGC1RES"  "CIPHRES"   "DAD21"     "DEBBINTR"  "DEJMINTR"
  "ARF3RES"   "CIGC2INT"  "CISFINTR"  "DAD24"     "DECLINTR"  "DELC2"
  "AROUTRO"   "CIGC2RES"  "COETEMPL"  "DAD31"     "DECRINTR"  "DELC3"
  "ART1INTR"  "CIGCINTR"  "COEXIT"    "DAD35"     "DECWIN"    "DELCATCH"
  "CAGSHIEL"  "CIGDPUNC"  "DAD06"     "DAD38"     "DEFBIA"    "DEODRB"
  "CATRES"    "CIGTCINT"  "DAD07"     "DAD57"     "DEFBINTR"  "DEODRES"
  "CAWRRES"   "CIHVRES"   "DAD10"     "DAD58"     "DEFBRB"    "DERINTRO"
  "CIATIDES"  "CIPAIB"    "DAD12"     "DAD61"     "DEFBRES"   "DERRA"
  "CIATOUT"   "CIPAINTR"  "DAD13"     "DAMOLE"    "DEGRES"    "DESCREEN"
  "CIBBINTR"  "CIPARES"   "DAD14"     "DEAR1INT"  "DEHINTRO"  "DPBTURRE"
  "FABINTRO"  "INDROP"    "JABOARD"   "JAEGOLD"   "JAPGLIDE"  "KRWB3"
  "FABRES"    "INFFHQ"    "JACARRY"   "JAELIGHT"  "JAPGUN"    "MIBINTRO"
  "FAI1INTR"  "INLOST"    "JAD1"      "JAENORMA"  "JAPHCAR"   "MIBRES"
  "FAI2INTR"  "INPALACE"  "JAD2"      "JAEXTERN"  "JAPIDAX"   "MIERES"
  "FAI3INTR"  "INRESCUE"  "JAD3"      "JAFLDAX"   "JAPILOT"   "MIGEXIT"
  "FAI4INTR"  "INTIRED"   "JAD4"      "JAFLUT"    "JAPOLE"    "MITINTRO"
  "FASBIB"    "INTRAINI"  "JAD5"      "JAGUN"     "JAPWCAR"   "MITRES"
  "FASBINTR"  "JAA1"      "JADARK"    "JAICE"     "JARACER"   "MU2ANIMS"
  "FASBRES"   "JAA2"      "JADON"     "JAINDAX"   "JASWIM"    "MU3ANIMS"
  "FORB"      "JAA3"      "JADUMMY"   "JAKANGA"   "JATENTAC"  "MU4ANIMS"
  "FORCRES"   "JAA4"      "JAEBOARD"  "JALADDER"  "JATUBE"    "MUANIMS"
  "FOTOMRES"  "JAA5"      "JAEDARK"   "JALIGHT"   "JATURRET"  "NEDBARRI"
  "FOTOWER"   "JAA6"      "JAEGC"     "JAMECH"    "KRWB1"     "NEEINTRO"
  "GRMANIMS"  "JAA7"      "JAEGNORM"  "JAPEGASU"  "KRWB2"     "NEHINTRO"
  "NEHRES"    "PRHC"      "SEMEXIT"   "TEORES"    "WADRES"    "NSB1BREA"
  "PRHFINAL"  "SEMHINTR"  "TETINTRO"  "WAGINTRO"  "NSB2BREA"  "PRMINIMA"
  "SEPEXIT"   "TETRA"     "WAGRES"    "PARAINTR"  "PRTRES"    "SESGRUNT"
  "TETRB"     "WALRINTR"  "PARARA"    "RATA1INT"  "SEWATERS"  "TEWDESER"
  "WALRRES"   "PARARES"   "RATA2INT"  "TECINTRO"  "TODINTRO"  "WAPGINTR"
  "PARPINTR"  "SCBOOK"    "TECRES"    "TODRB"     "WAPGRES"   "PEFLY"
  "SEEINTRO"  "TEDINTRO"  "TODRES"    "WOMAP"     "POAINTRO"  "SEGENTRA"
  "TEDRES"    "VODRES"    "PRDSERES"  "SEHKENTR"  "TEEANIMS"  "VOI1INTR"
  "PRDSLOSE"  "SEHKRES"   "TEELAANI"  "VOI1RES"   "PRDSRES"   "SEKENTRA"
  "TEELODS"   "VOI2INTR"  "PRHA"      "SEKEXIT"   "TEJGLGLI"  "VOI2RES"
  "PRHB"      "SEKMINTR"  "TEOINTRO"  "WACINTRO")



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
(defstep :in "$ISO/RES/TWEAKVAL.MUS"
  :tool 'copy
  :out '("$OUT/iso/TWEAKVAL.MUS"))
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
  :out '("$OUT/iso/0SUBTI3.TXT")
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
   "$OUT/iso/0SUBTI3.TXT"
   "$OUT/iso/VAGDIR.AYB"
   "$OUT/iso/TWEAKVAL.MUS"
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
   "$OUT/iso/0SUBTI3.TXT"
   )
 )

;; used for the type consistency test.
(group-list "all-code"
  `(,@(reverse *all-gc*))
  )

(group "engine"
       "$OUT/iso/0COMMON.TXT"
       "$OUT/iso/0SUBTI3.TXT"
       "$OUT/iso/KERNEL.CGO"
       "$OUT/iso/GAME.CGO"
       "$OUT/iso/VAGDIR.AYB"
       "$OUT/iso/VAGWAD.ENG"
       )

