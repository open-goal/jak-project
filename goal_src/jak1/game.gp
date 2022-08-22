;;-*-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jak 1 Project File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file sets up the OpenGOAL build system for Jak 1.
;; This file is treated as a GOOS program. There is a single special form `defstep` that
;; allows you to define a build step.

;; Then, you can use the `make` command to build a target. Like real make, it will only rebuild things if
;; the inputs change.

;; Each defstep takes the following arguments:
;; in - an input file. The step automatically depends on this.
;; tool - the tool (goalc, copy, dgo, group, tpage-dir)
;; out - a list of outputs (unlike make, we support multiple outputs without hacks!)
;; dep - a list of outputs from other rules that are required for this.

;; Before the build order is determined, the tool gets to look at its input file and tell the build system
;; about other deps. For example, in a "dgo" rule, you don't have to say that you depend on all of your input
;; files, the DGO tool provides that information to the build system.

;; It is an error to provide two steps to make the same file, even if they are identical.
;; It is an error to not provide a step to make a required file.
;; It is an error to have a circular dependency and this will crash the compiler due to stack overflow.

;; our input/output directories aren't fixed ahead of time and some users may override them.
;; the map-path! command can be used to define path constants that can be used in in/out/dep and
;; inside certain tool files, like text project files.

;; The substitution is only used at the beginning of paths, and I hope to keep the number
;; of these to a minimum - it's quite confusing to have all these non-fixed paths everywhere.

;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from ISO
;;;;;;;;;;;;;;;;;;;;;;;

(cond
  ;; extractor can override everything by providing *use-iso-data-path*
  (*use-iso-data-path*
    (map-path! "$ISO" (string-append *iso-data* "/")))
  ;; user-specific places to put $ISO
  ((user? dass)
    (map-path! "$ISO" "iso_data/jak1_us2/"))
  ;; for normal people, just use jak1.
  (#t
    (map-path! "$ISO" "iso_data/jak1/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs from decompiler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
  ;; user-specific places to put $ISO
  ((user? dass)
    (map-path! "$DECOMP" "decompiler_out/jak1_us2/"))
  (#t
    (map-path! "$DECOMP" "decompiler_out/jak1/")))

;;;;;;;;;;;;;;;;;;;;;;;
;; Output
;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: the game itself will load from out/jak1/iso and out/jak1/fr3.
(map-path! "$OUT" "out/jak1/")

;; tell the compiler to put its outputs in out/jak1/
(set-output-prefix "jak1/")

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

;;;;;;;;;;;;;;;;;;;;;;;
;; Build system macros
;;;;;;;;;;;;;;;;;;;;;;;
(defun gc-file->o-file (filename)
  "Get the name of the object file for the given GOAL (*.gc) source file."
  (string-append "$OUT/obj/" (stem filename) ".o")
  )

(defmacro goal-src (src-file &rest deps)
  "Add a GOAL source file with the given dependencies"
  `(let ((output-file ,(gc-file->o-file src-file)))
    (set! *all-gc* (cons output-file *all-gc*))
    (defstep :in ,(string-append "goal_src/jak1/" src-file)
     ;; use goal compiler
     :tool 'goalc
     ;; will output the obj file
     :out (list output-file)
     ;; dependencies are the obj files
     :dep '(,@(apply gc-file->o-file deps))
     )
    )
  )

(defun make-src-sequence-elt (current previous prefix)
  "Helper for goal-src-sequence"
  `(let ((output-file ,(gc-file->o-file current)))
    (set! *all-gc* (cons output-file *all-gc*))
    (defstep :in ,(string-append "goal_src/jak1/" prefix current)
     :tool 'goalc
     :out (list output-file)
     :dep '(,(gc-file->o-file previous))
     )
    )
  )

(defmacro goal-src-sequence (prefix &key (deps '()) &rest sequence)
  "Add a sequence of GOAL files (each depending on the previous) in the given directory,
   with all depending on the given deps."
  (let* ((first-thing `(goal-src ,(string-append prefix (first sequence)) ,@deps))
         (result (cons first-thing '()))
         (iter result))

    (let ((prev (first sequence))
          (in-iter (rest sequence)))

      (while (not (null? in-iter))
        ;; (fmt #t "{} dep on {}\n" (first in-iter) prev)
        (let ((next (make-src-sequence-elt (first in-iter) prev prefix)))
          (set-cdr! iter (cons next '()))
          (set! iter (cdr iter))
          )

        (set! prev (car in-iter))
        (set! in-iter (cdr in-iter))
        )
      )

    `(begin ,@result)
    )
  )

(defun custom-level-cgo (output-name desc-file-name)
  "Add a CGO with the given output name (in $OUT/iso) and input name (in custom_levels/)"
  (let ((out-name (string-append "$OUT/iso/" output-name)))
    (defstep :in (string-append "custom_levels/" desc-file-name)
      :tool 'dgo
      :out `(,out-name)
      )
    (set! *all-cgos* (cons out-name *all-cgos*))
    )
  )

(defun cgo (output-name desc-file-name)
  "Add a CGO with the given output name (in $OUT/iso) and input name (in goal_src/jak1/dgos)"
  (let ((out-name (string-append "$OUT/iso/" output-name)))
    (defstep :in (string-append "goal_src/jak1/dgos/" desc-file-name)
      :tool 'dgo
      :out `(,out-name)
      )
    (set! *all-cgos* (cons out-name *all-cgos*))
    )
  )

(defun tpage-name (id)
  "Get the name of the tpage obj file with the given id"
  (fmt #f "tpage-{}.go" id)
  )



(defmacro copy-texture (tpage-id)
  "Copy a texture from the game, using the given tpage ID"
  (let* ((path (string-append "$DECOMP/raw_obj/" (tpage-name tpage-id))))
    `(defstep :in ,path
              :tool 'copy
              :out '(,(string-append "$OUT/obj/" (tpage-name tpage-id))))))

(defmacro copy-textures (&rest ids)
  `(begin
    ,@(apply (lambda (x) `(copy-texture ,x)) ids)
    )
  )

(defmacro copy-go (name)
  (let* ((path (string-append "$DECOMP/raw_obj/" name ".go")))
    `(defstep :in ,path
              :tool 'copy
              :out '(,(string-append "$OUT/obj/" name ".go")))))

(defmacro copy-gos (&rest gos)
  `(begin
    ,@(apply (lambda (x) `(copy-go ,x)) gos)
    )
  )

(defmacro build-custom-level (name)
  (let* ((path (string-append "custom_levels/" name "/" name ".jsonc")))
    `(defstep :in ,path
              :tool 'build-level
              :out '(,(string-append "$OUT/obj/" name ".go")))))


(defun copy-iso-file (name subdir ext)
  (let* ((path (string-append "$ISO/" subdir name ext))
         (out-name (string-append "$OUT/iso/" name ext)))
    (defstep :in path
             :tool 'copy
             :out `(,out-name))
    out-name))

(defmacro copy-strs (&rest strs)
  `(begin ,@(apply (lambda (x) `(set! *all-str* (cons (copy-iso-file ,x "STR/" ".STR") *all-str*))) strs)))

(defmacro copy-vis-files (&rest files)
  `(begin ,@(apply (lambda (x) `(set! *all-vis* (cons (copy-iso-file ,x "VIS/" ".VIS") *all-vis*))) files)))

;; Files not yet added in here:
;; - TESTTONE.SBK
(defmacro copy-sbk-files (&rest files)
  `(begin ,@(apply (lambda (x) `(set! *all-sbk* (cons (copy-iso-file ,x "SBK/" ".SBK") *all-sbk*))) files)))

(defmacro copy-mus-files (&rest files)
  `(begin ,@(apply (lambda (x) `(set! *all-mus* (cons (copy-iso-file ,x "MUS/" ".MUS") *all-mus*))) files)))

(defmacro copy-vag-files (&rest files)
  `(begin ,@(apply (lambda (x) `(set! *all-vag* (cons (copy-iso-file "VAGWAD" "VAG/" (string-append "." ,x)) *all-vag*))) files)))

(defmacro group (name &rest stuff)
  `(defstep :in ""
     :tool 'group
     :out '(,(string-append "GROUP:" name))
     :dep '(,@stuff))
  )

(defun group-list (name stuff)
  (defstep :in ""
     :tool 'group
     :out `(,(string-append "GROUP:" name))
     :dep stuff)
  )

;;;;;;;;;;;;;;;;;;;;;;
;; CGO's
;;;;;;;;;;;;;;;;;;;;;;
(cgo "KERNEL.CGO" "kernel.gd")
(cgo "ENGINE.CGO" "engine.gd")
(cgo "GAME.CGO" "game.gd")

;;;;;;;;;;;;;;;;;
;; GOAL Kernel
;;;;;;;;;;;;;;;;;

;; These are set up with proper dependencies

(goal-src "kernel/gcommon.gc")
(goal-src "kernel/gstring-h.gc")
(goal-src "kernel/gkernel-h.gc"
  "gcommon"
  "gstring-h")
(goal-src "kernel/gkernel.gc"
  "gkernel-h")
(goal-src "kernel/pskernel.gc"
  "gcommon"
  "gkernel-h")
(goal-src "kernel/gstring.gc"
  "gcommon"
  "gstring-h")
(goal-src "kernel/dgo-h.gc")
(goal-src "kernel/gstate.gc"
  "gkernel")


;;;;;;;;;;;;;;;;;;;;;;;;
;; Weird special things
;;;;;;;;;;;;;;;;;;;;;;;;

;; The tpage directory
(defstep :in "$DECOMP/textures/tpage-dir.txt"
  :tool 'tpage-dir
  :out '("$OUT/obj/dir-tpages.go")
  )

;; the count file.
(defstep :in "$DECOMP/assets/game_count.txt"
  :tool 'game-cnt
  :out '("$OUT/obj/game-cnt.go")
  )

;; the TWEAKVAL file
(defstep :in "$ISO/MUS/TWEAKVAL.MUS"
  :tool 'copy
  :out '("$OUT/iso/TWEAKVAL.MUS"))

;; the VAGDIR file
(defstep :in "$ISO/VAG/VAGDIR.AYB"
  :tool 'copy
  :out '("$OUT/iso/VAGDIR.AYB"))

;; the save icon file
(defstep :in "$ISO/DRIVERS/SAVEGAME.ICO"
  :tool 'copy
  :out '("$OUT/iso/SAVEGAME.ICO"))

;; the loading screen file
(defstep :in "$ISO/DRIVERS/SCREEN1.USA"
  :tool 'copy
  :out '("$OUT/iso/SCREEN1.USA"))

;;;;;;;;;;;;;;;;;;;;;
;; Textures (Common)
;;;;;;;;;;;;;;;;;;;;;

(copy-textures 463 2 880 256 1278 1032 62 1532)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Streaming anim (common)
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(copy-strs
  ;; power cell animations
  "FUCVICTO"
  "FUCV2"
  "FUCV3"
  "FUCV4"
  "FUCV5"
  "FUCV6"
  "FUCV7"
  "FUCV8"
  "FUCRV1"
  "FUCFV1"
  ;; jak's ambient
  "EIA1"
  "EIA2"
  "EIA3"
  "EIA4"
  ;; jak death
  "DE0181"
  "DE0182"
  "DE0184"
  "DE0186"
  "DE0187"
  "DE0191"
  "DE0193"
  "DE0195"
  "DE0197"
  "DE0199"
  "DE0202"

  ;; intro camera
  "NDINTRO"
  "LOINTRO"
  "LOLOOP"
  "LOI2"
  )


;;;;;;;;;;;;;;;;;;;;;
;; Art (Common)
;;;;;;;;;;;;;;;;;;;;;

(copy-gos
 "fuel-cell-ag"
 "money-ag"
 "buzzer-ag"
 "ecovalve-ag"
 "crate-ag"
 "speaker-ag"
 "fuelcell-naked-ag"
 "eichar-ag"
 "sidekick-ag"
 "deathcam-ag"
 )

;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;

(defstep :in "game/assets/game_text.gp"
  :tool 'text
  :out '("$OUT/iso/0COMMON.TXT"
         "$OUT/iso/1COMMON.TXT"
         "$OUT/iso/2COMMON.TXT"
         "$OUT/iso/3COMMON.TXT"
         "$OUT/iso/4COMMON.TXT"
         "$OUT/iso/5COMMON.TXT"
         "$OUT/iso/6COMMON.TXT")
  )

(defstep :in "game/assets/game_subtitle.gp"
  :tool 'subtitle
  :out '("$OUT/iso/0SUBTIT.TXT"
         "$OUT/iso/3SUBTIT.TXT"
         "$OUT/iso/6SUBTIT.TXT")
  )



;;;;;;;;;;;;;;;;;;;;;
;; kernel Group
;;;;;;;;;;;;;;;;;;;;;
;; the kernel group is a group of files required to boot the game kernel

(group "kernel" "$OUT/iso/KERNEL.CGO")


;;;;;;;;;;;;;;;;;;;;;
;; engine Group
;;;;;;;;;;;;;;;;;;;;;
;; the engine group is a group of files required to boot the game engine with no levels

(group "engine"
       "$OUT/iso/0COMMON.TXT"
       "$OUT/iso/0SUBTIT.TXT"
       "$OUT/iso/KERNEL.CGO"
       "$OUT/iso/GAME.CGO"
       )

;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Level Objects
;;;;;;;;;;;;;;;;;;;;;;;;

;; as we find objects that exist in multiple levels, put them here

;; early versions of the game - including the black label release - do not have all files.
(if *jak1-full-game*
    (copy-sbk-files "COMMON" "COMMONJ" "EMPTY1" "EMPTY2")
    (copy-sbk-files "COMMON" "EMPTY1" "EMPTY2")
    )

(copy-gos
 "sharkey-ag"
 "orb-cache-top-ag"
 "warp-gate-switch-ag"
 "warpgate-ag"
 "babak-ag"
 "oracle-ag"
 "village-cam-ag"

 "eichar-racer+0-ag"
 "ef-plane-ag"
 "racer-ag"

 "eichar-flut+0-ag"
 "flut-saddle-ag"
  )


;;;;;;;;;;;;;;;;;;;;;
;; Common Level Code
;;;;;;;;;;;;;;;;;;;;;

(goal-src-sequence
  "levels/"
   :deps ;; no idea what these depend on, make it depend on the whole engine
   ("$OUT/obj/ticky.o")

   "village_common/villagep-obs.gc"
   "village_common/oracle.gc"

   "common/blocking-plane.gc"
   "common/launcherdoor.gc"
   "common/battlecontroller.gc"

   "racer_common/target-racer-h.gc"
   "racer_common/racer-part.gc"
   "racer_common/racer.gc"
   "racer_common/target-racer.gc"
   "racer_common/racer-states.gc"
   "racer_common/collide-reaction-racer.gc"

   "flut_common/flut-part.gc"
   "flut_common/flutflut.gc"
   "flut_common/target-flut.gc"
   )


;;;;;;;;;;;;;;;;;;;;;
;; Beach
;;;;;;;;;;;;;;;;;;;;;

(cgo "BEA.DGO"
  "bea.gd"
  )

(copy-vis-files "BEA")
(copy-sbk-files "BEACH")
(copy-mus-files "BEACH")

(goal-src-sequence
  "levels/beach/"
  :deps ("$OUT/obj/ticky.o")
  "air-h.gc"
  "air.gc"
  "wobbler.gc"
  "twister.gc"
  "beach-obs.gc"
  "bird-lady.gc"
  "bird-lady-beach.gc"
  "mayor.gc"
  "sculptor.gc"
  "pelican.gc"
  "lurkerworm.gc"
  "lurkercrab.gc"
  "lurkerpuppy.gc"
  "beach-rocks.gc"
  "seagull.gc"
  "beach-part.gc"
  )

(copy-textures 212 214 213 215)

(copy-gos
  "barrel-ag"
  "beachcam-ag"
  "bird-lady-ag"
  "bird-lady-beach-ag"
  "bladeassm-ag"
  "ecoventrock-ag"
  "flutflut-ag"
  "flutflutegg-ag"
  "grottopole-ag"
  "harvester-ag"
  "kickrock-ag"
  "lrocklrg-ag"
  "lurkercrab-ag"
  "lurkerpuppy-ag"
  "lurkerworm-ag"
  "mayor-ag"
  "pelican-ag"
  "sculptor-ag"
  "sculptor-muse-ag"
  "seagull-ag"
  "windmill-one-ag"
  "beach-vis"
  )

;; pelican
(copy-strs "PESEXT")
;; beachcam
(copy-strs "BECANNON")
;; sculptor
(copy-strs "SCINTROD" "SCR1" "SCRESOLU")
;; lrocklrg
(copy-strs "LRFALLIN")
;; mayor
(copy-strs "MAINTROD" "MARBEAMS" "MARDONAT" "MAZBEAMS" "MAZDONAT")
;; bird-lady
(copy-strs "BILINTRO" "BILR1" "BILR2" "BILBRESO")


;;;;;;;;;;;;;;;;;;;;;
;; Jungle
;;;;;;;;;;;;;;;;;;;;;

(cgo "JUN.DGO"
  "jun.gd")

(copy-vis-files "JUN")
(copy-sbk-files "JUNGLE")
(copy-mus-files "JUNGLE" "FISHGAME")

(goal-src-sequence
 "levels/jungle/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o")

 "jungle-elevator.gc"
 "bouncer.gc"
 "hopper.gc"
 "junglesnake.gc"
 "darkvine.gc"
 "jungle-obs.gc"
 "jungle-mirrors.gc"
 "junglefish.gc"
 "fisher.gc"
 "jungle-part.gc"
 )

(copy-textures 385 531 386 388 765)

(copy-gos
  "eichar-fish+0-ag"
  "accordian-ag"
  "bounceytarp-ag"
  "catch-fisha-ag"
  "catch-fishb-ag"
  "catch-fishc-ag"
  "darkvine-ag"
  "fish-net-ag"
  "fisher-ag"
  "hopper-ag"
  "junglecam-ag"
  "junglefish-ag"
  "junglesnake-ag"
  "launcherdoor-ag"
  "logtrap-ag"
  "lurkerm-piston-ag"
  "lurkerm-tall-sail-ag"
  "maindoor-ag"
  "medres-firecanyon-ag"
  "periscope-ag"
  "plat-button-ag"
  "plat-eco-ag"
  "precurbridge-ag"
  "reflector-mirror-ag"
  "ropebridge-52-ag"
  "ropebridge-70-ag"
  "sidedoor-ag"
  "towertop-ag"
  "water-anim-jungle-ag"
  "jungle-vis"
  )

;; fisher
(copy-strs "FIINTROD" "FIR1" "FIACCEPT" "FIREJECT" "FIRESOLU")


;;;;;;;;;;;;;;;;;;;;;
;; Village 1
;;;;;;;;;;;;;;;;;;;;;

;; the definition for the DGO file.
(cgo "VI1.DGO"
     "vi1.gd"
     )

;; the VIS file
(copy-vis-files "VI1")
(copy-sbk-files "VILLAGE1")
(copy-mus-files "VILLAGE1")

;; the code
(goal-src-sequence
 "levels/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o")

 "village1/farmer.gc"
 "village1/explorer.gc"
 "village1/assistant.gc"
 "village1/sage.gc"
 "village1/yakow.gc"
 "village1/village-obs.gc"
 "village1/fishermans-boat.gc"
 "village1/village1-part.gc"
 "village1/village1-part2.gc"
 "village1/sequence-a-village1.gc"
 )

;; the textures
(copy-textures 398 400 399 401 1470)

;; the art
(copy-gos
 "assistant-ag"
 "evilplant-ag"
 "explorer-ag"
 "farmer-ag"
 "fishermans-boat-ag"
 "hutlamp-ag"
 "mayorgears-ag"
 "medres-beach-ag"
 "medres-beach1-ag"
 "medres-beach2-ag"
 "medres-beach3-ag"
 "medres-jungle-ag"
 "medres-jungle1-ag"
 "medres-jungle2-ag"
 "medres-misty-ag"
 "medres-training-ag"
 "medres-village11-ag"
 "medres-village12-ag"
 "medres-village13-ag"
 "reflector-middle-ag"
 "revcycle-ag"
 "revcycleprop-ag"
 "ropebridge-32-ag"
 "sage-ag"
 "sagesail-ag"
 "villa-starfish-ag"
 "village1cam-ag"
 "water-anim-village1-ag"
 "windmill-sail-ag"
 "windspinner-ag"
 "yakow-ag"
 "village1-vis"
 )

;; farmer
(copy-strs "FAINTROD" "FAR1" "FAR2" "FARESOLU")
;; explorer
(copy-strs "EXINTROD" "EXR1" "EXR2" "EXRESOLU")
;; oracle
(copy-strs "ORI1" "ORLE1" "ORRE1" "ORR1")
;; assistant
(copy-strs "ASIBESWI" "ASR1BESW" "ASIRBIKE" "ASR1RBIK" "ASR1GENE")
;; sage
(copy-strs "SAISA" "SAISD1" "SAISD2" "SAISE" "SAR1ECOR" "SAIMCANN" "SAR1MCAN" "SAR1GENE" "SAR2GENE")
;; fishermans-boat
(copy-strs "FIBRTMIS")

;;;;;;;;;;;;;;;;;;;;;
;; Jungle temple
;;;;;;;;;;;;;;;;;;;;;

(cgo "JUB.DGO" "jub.gd")

(copy-vis-files "JUB")
(copy-sbk-files "JUNGLEB")
(copy-mus-files "JUNGLEB")

(goal-src-sequence
 "levels/jungleb/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o")

 "jungleb-obs.gc"
 "plat-flip.gc"
 "aphid.gc"
 "plant-boss.gc"
 )

(copy-textures 485 510 507 966)

(copy-gos
  "plant-boss-main+0-ag"
  "aphid-lurker-ag"
  "eggtop-ag"
  "jng-iris-door-ag"
  "plant-boss-ag"
  "plat-flip-ag"
  "plat-jungleb-ag"
  "jungleb-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; misty island
;;;;;;;;;;;;;;;;;;;;;

(cgo "MIS.DGO" "mis.gd")

(copy-vis-files "MIS")
(copy-sbk-files "MISTY")
(copy-mus-files "MISTY")

(goal-src-sequence
  "levels/misty/"
  :deps ("$OUT/obj/evilbro.o")
  "mistycannon.gc"
  "babak-with-cannon.gc"
  "misty-obs.gc"
  "misty-warehouse.gc"
  "misty-conveyor.gc"
  "mud.gc"
  "muse.gc"
  "bonelurker.gc"
  "quicksandlurker.gc"
  "misty-teetertotter.gc"
  "balloonlurker.gc"
  "misty-part.gc"
  "sidekick-human.gc"
  )

(copy-textures 516 521 518 520)

(copy-gos
  "mistycannon-ag"
  "sack-ag"
  "balloonlurker-ag"
  "boatpaddle-ag"
  "bonelurker-ag"
  "breakaway-left-ag"
  "breakaway-mid-ag"
  "breakaway-right-ag"
  "darkecocan-ag"
  "keg-ag"
  "keg-conveyor-ag"
  "keg-conveyor-paddle-ag"
  "mis-bone-bridge-ag"
  "mis-bone-platform-ag"
  "mistycam-ag"
  "muse-ag"
  "quicksandlurker-ag"
  "ropebridge-36-ag"
  "rounddoor-ag"
  "sidekick-human-ag"
  "silostep-ag"
  "teetertotter-ag"
  "water-anim-misty-ag"
  "wheel-ag"
  "windturbine-ag"
  "misty-vis"
  )

;; fishermans-boat
(copy-strs "FIBRTVIL" "FIBRT1AL")
;; muse
(copy-strs "MUVICTOR")
;; sidekick-human
(copy-strs "SIHISA" "SIHISB" "SIHISC")
;; mistycam
(copy-strs "MICANNON")

;;;;;;;;;;;;;;;;;;;;;
;; swamp
;;;;;;;;;;;;;;;;;;;;;

(cgo "SWA.DGO" "swa.gd")

(copy-vis-files "SWA")
(copy-sbk-files "SWAMP")
(copy-mus-files "SWAMP")

(goal-src-sequence
 "levels/swamp/"
 :deps ("$OUT/obj/ticky.o")
 "swamp-obs.gc"
 "swamp-bat.gc"
 "swamp-rat.gc"
 "swamp-rat-nest.gc"
 "kermit.gc"
 "swamp-part.gc"
 "billy.gc"
 )

(copy-textures 358 659 629 630)

(copy-gos
  "eichar-pole+0-ag"
  "balance-plat-ag"
  "billy-ag"
  "billy-sidekick-ag"
  "farthy-snack-ag"
  "kermit-ag"
  "swamp-bat-ag"
  "swamp-rat-ag"
  "swamp-rat-nest-ag"
  "swamp-rock-ag"
  "swamp-spike-ag"
  "swampcam-ag"
  "tar-plat-ag"
  "swamp-vis"
  )

;; billy
(copy-strs "BIINTROD" "BIR1" "BIACCEPT" "BIREJECT" "BIRESOLU")

;;;;;;;;;;;;;;;;;;;;;
;; LPC
;;;;;;;;;;;;;;;;;;;;;

(cgo "SUN.DGO" "sun.gd")

(copy-vis-files "SUN")
(copy-sbk-files "SUNKEN")
(copy-mus-files "SUNKEN")

(goal-src-sequence
  "levels/sunken/"
  :deps ("$OUT/obj/ticky.o")
  "sunken-part.gc"
  "sunken-part2.gc"
  "sunken-part3.gc"
  "sunken-part4.gc"
  "sunken-part5.gc"
  "target-tube.gc"
  "sunken-obs.gc"
  "shover.gc"
  "square-platform.gc"
  "sun-iris-door.gc"
  "orbit-plat.gc"
  "wedge-plats.gc"
  "wall-plat.gc"
  "qbert-plat.gc"
  "steam-cap.gc"
  "sun-exit-chamber.gc"
  "floating-launcher.gc"
  "sunken-water.gc"
  "whirlpool.gc"
  "sunken-pipegame.gc"
  "bully.gc"
  "double-lurker.gc"
  "helix-water.gc"
  "puffer.gc"
  "sunken-fish.gc"
  )

(copy-textures 661 663 714 662 766)

(copy-gos
  "eichar-tube+0-ag"
  "bully-ag"
  "double-lurker-ag"
  "double-lurker-top-ag"
  "exit-chamber-ag"
  "generic-button-ag"
  "orbit-plat-ag"
  "orbit-plat-bottom-ag"
  "plat-sunken-ag"
  "puffer-ag"
  "qbert-plat-ag"
  "qbert-plat-on-ag"
  "seaweed-ag"
  "shover-ag"
  "side-to-side-plat-ag"
  "square-platform-ag"
  "steam-cap-ag"
  "sun-iris-door-ag"
  "sunkencam-ag"
  "sunkenfisha-ag"
  "wall-plat-ag"
  "water-anim-sunken-ag"
  "water-anim-sunken-dark-eco-ag"
  "wedge-plat-ag"
  "wedge-plat-outer-ag"
  "whirlpool-ag"
  "sunken-vis"
  )


;;;;;;;;;;;;;;;;;;;;;
;; sunken city b
;;;;;;;;;;;;;;;;;;;;;

(cgo "SUB.DGO" "sub.gd")

(copy-vis-files "SUB")

(copy-textures 163 164 166 162 764)

(copy-gos
  "blue-eco-charger-ag"
  "blue-eco-charger-orb-ag"
  "floating-launcher-ag"
  "helix-button-ag"
  "helix-slide-door-ag"
  "sunkenb-vis"
  )


;;;;;;;;;;;;;;;;;;;;;
;; snow mountain
;;;;;;;;;;;;;;;;;;;;;

(cgo "SNO.DGO" "sno.gd")

(copy-vis-files "SNO")
(copy-sbk-files "SNOW")
(copy-mus-files "SNOW")

(goal-src-sequence
 "levels/snow/"
 :deps ("$OUT/obj/ticky.o")
 "target-snowball.gc"
 "target-ice.gc"
 "ice-cube.gc"
 "snow-ball.gc"
 "snow-obs.gc"
 "snow-flutflut-obs.gc"
 "snow-bumper.gc"
 "snow-ram-h.gc"
 "snow-ram-boss.gc"
 "snow-ram.gc"
 "snow-part.gc"
 "yeti.gc"
 "snow-bunny.gc"
 )

(copy-textures 710 842 711 712)

(copy-gos
  "eichar-ice+0-ag"
  "flutflut-plat-large-ag"
  "flutflut-plat-med-ag"
  "flutflut-plat-small-ag"
  "ice-cube-ag"
  "ice-cube-break-ag"
  "ram-ag"
  "ram-boss-ag"
  "snow-ball-ag"
  "snow-bridge-36-ag"
  "snow-bumper-ag"
  "snow-bunny-ag"
  "snow-button-ag"
  "snow-eggtop-ag"
  "snow-fort-gate-ag"
  "snow-gears-ag"
  "snow-log-ag"
  "snow-spatula-ag"
  "snow-switch-ag"
  "snowcam-ag"
  "snowpusher-ag"
  "yeti-ag"
  "snow-vis"
  )

;; ram-boss
(copy-strs "SNRBICFC" "SNRBIPFC" "SNRBSBFC")

;;;;;;;;;;;;;;;;;;;;;
;; Fire Canyon
;;;;;;;;;;;;;;;;;;;;;

(cgo "FIC.DGO"
     "fic.gd"
     )

(copy-vis-files "FIC")
(copy-sbk-files "FIRECANY")
(copy-mus-files "FIRECANY")

(copy-textures 1119) ;; might be common/zoomer hud?? also in misty, lavatube, ogre and racerpkg

(goal-src-sequence
 "levels/firecanyon/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o")

 "firecanyon-part.gc"
 "assistant-firecanyon.gc"
 "firecanyon-obs.gc"

 )

(copy-textures 815 822 854 1123)

(copy-gos
  "assistant-firecanyon-ag"
  "balloon-ag"
  "crate-darkeco-cluster-ag"
  "spike-ag"
  "firecanyon-vis")

;; assistant firecanyon
(copy-strs "ASFRESOL")

;;;;;;;;;;;;;;;;;;;;;
;; ogre boss
;;;;;;;;;;;;;;;;;;;;;

(cgo "OGR.DGO" "ogr.gd")

(copy-vis-files "OGR")
(copy-sbk-files "OGRE")
(copy-mus-files "OGRE" "OGREBOSS")

(goal-src-sequence
 "levels/ogre/"
 :deps ("$OUT/obj/ticky.o")
 "ogre-part.gc"
 "ogreboss.gc"
 "ogre-obs.gc"
 "flying-lurker.gc"
 )

(copy-textures 875 967 884 1117)

(copy-gos
  "flying-lurker-ag"
  "medres-snow-ag"
  "ogre-bridge-ag"
  "ogre-bridgeend-ag"
  "ogre-isle-ag"
  "ogre-step-ag"
  "ogreboss-ag"
  "ogrecam-ag"
  "plunger-lurker-ag"
  "shortcut-boulder-ag"
  "tntbarrel-ag"
  "water-anim-ogre-ag"
  "ogre-vis"
  )

;; flying-lurker
(copy-strs "FLLINTRO" "PLLBLOWU")


;;;;;;;;;;;;;;;;;;;;;
;; Village 2
;;;;;;;;;;;;;;;;;;;;;

(cgo "VI2.DGO" "vi2.gd")

(copy-vis-files "VI2")
(copy-sbk-files "VILLAGE2")
(copy-mus-files "VILLAGE2")

(goal-src-sequence
 "levels/village2/"
 :deps ("$OUT/obj/ticky.o")
 "village2-part.gc"
 "village2-obs.gc"
 "village2-part2.gc"
 "gambler.gc"
 "warrior.gc"
 "geologist.gc"
 "swamp-blimp.gc"
 "sage-bluehut.gc"
 "flutflut-bluehut.gc"
 "assistant-village2.gc"
 "sunken-elevator.gc"
 )

(copy-textures 919 922 920 921 1476)

(copy-gos
  "allpontoons-ag"
  "assistant-village2-ag"
  "ceilingflag-ag"
  "exit-chamber-dummy-ag"
  "fireboulder-ag"
  "flutflut-bluehut-ag"
  "gambler-ag"
  "geologist-ag"
  "jaws-ag"
  "medres-rolling-ag"
  "medres-rolling1-ag"
  "medres-village2-ag"
  "ogreboss-village2-ag"
  "pontoonfive-ag"
  "pontoonten-ag"
  "precursor-arm-ag"
  "sage-bluehut-ag"
  "sunken-elevator-ag"
  "swamp-blimp-ag"
  "swamp-rope-ag"
  "swamp-tetherrock-ag"
  "swamp-tetherrock-explode-ag"
  "village2cam-ag"
  "warrior-ag"
  "water-anim-village2-ag"
  "village2-vis"
  )

;; assistant village2
(copy-strs "AS2INTRO" "AS2IROOM" "AS2R1ROO" "AS2IROBB" "AS2R1ROB" "AS2IFLUT" "AS2R1FLU" "AS2RESOL")
;; sage bluehut
(copy-strs "SABICDUS" "SABR1CDU" "SABIPARM" "SABR1PAR")
;; geologist
(copy-strs "GEINTROD" "GERMOLES" "GEZMOLES" "GERMONEY" "GEZMONEY")
;; gambler
(copy-strs "GAI1" "GARRACE" "GARMONEY" "GAZRACE" "GAZMONEY")
;; warrior
(copy-strs "WAINTROD" "WAR1" "WARESOLU")
;; oracle
(copy-strs "ORI2" "ORLE2" "ORRE2" "ORR2")

;;;;;;;;;;;;;;;;;;;;;
;; rolling hills
;;;;;;;;;;;;;;;;;;;;;

(cgo "ROL.DGO" "rol.gd")

(copy-vis-files "ROL")
(copy-sbk-files "ROLLING")
(copy-mus-files "ROLLING")

(goal-src-sequence
 "levels/rolling/"
 :deps ("$OUT/obj/ticky.o")
 "rolling-obs.gc"
 "rolling-lightning-mole.gc"
 "rolling-robber.gc"
 "rolling-race-ring.gc"
 )

(copy-textures 923 926 924 925 1353)

(copy-gos
  "dark-plant-ag"
  "happy-plant-ag"
  "lightning-mole-ag"
  "pusher-ag"
  "race-ring-ag"
  "robber-ag"
  "rolling-start-ag"
  "rollingcam-ag"
  "water-anim-rolling-ag"
  "rolling-vis"
  )

;; happy-plant
(copy-strs "HAPOPEN")
;; race-ring
(copy-strs "RARANIM" "RARSANIM")

;;;;;;;;;;;;;;;;;;;;;
;; Village 3
;;;;;;;;;;;;;;;;;;;;;

;; the definition for the DGO file.
(cgo "VI3.DGO" "vi3.gd")

(copy-vis-files "VI3")
(copy-sbk-files "VILLAGE3")
(copy-mus-files "VILLAGE3")

;; the code
(goal-src-sequence
 "levels/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o")
 "village3/village3-part.gc"
 "village3/village3-obs.gc"
 "village3/minecart.gc"
 "village3/miners.gc"
 "village3/assistant-village3.gc"
 "village3/sage-village3.gc"
 )

(copy-textures 1208 1210 1209 1194)

(copy-gos
  "assistant-village3-ag"
  "cavegem-ag"
  "evilbro-village3-ag"
  "evilsis-village3-ag"
  "gondola-ag"
  "gondolacables-ag"
  "lavaspoutdrip-ag"
  "medres-finalboss-ag"
  "medres-ogre-ag"
  "medres-ogre2-ag"
  "medres-ogre3-ag"
  "minecartsteel-ag"
  "minershort-ag"
  "minertall-ag"
  "pistons-ag"
  "sage-village3-ag"
  "vil3-bridge-36-ag"
  "water-anim-village3-ag"
  "village3-vis"
  )

;; sage-villagec
(copy-strs "SA3INTRO" "SA3IDECO" "SA3R1DEC" "SA3IRAMS" "SA3R1RAM")
;; assistant-villagec
(copy-strs "AS3REMIN")
;; oracle
(copy-strs "ORI3" "ORLE3" "ORRE3" "ORR3")
;; gondola
(copy-strs "GORUP" "GORDOWN")
;; minershort
(copy-strs "MIIORBS" "MIR1ORBS" "MIR2ORBS" "MIZ1ORBS" "MIZ2ORBS" "MIIGNAWE" "MIR1GNAW" "MIISWITC" "MIR1SWIT")

;;;;;;;;;;;;;;;;;;;;;
;; Training
;;;;;;;;;;;;;;;;;;;;;

;; the definition of the DGO package for the level
(cgo "TRA.DGO"
     "tra.gd")

(copy-vis-files "TRA")
(copy-sbk-files "TRAINING")

;; The code
(goal-src-sequence
  "levels/training/"
  :deps ("$OUT/obj/ticky.o") ;; makes us depend on the whole engine

  "training-obs.gc"
  "training-part.gc"
  )

;; the textures
(copy-textures 1309 1311 1310 1308 775)

(copy-gos
  "scarecrow-a-ag"
  "scarecrow-b-ag"
  "trainingcam-ag"
  "water-anim-training-ag"
  "training-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; Spider Cave
;;;;;;;;;;;;;;;;;;;;;

(cgo "MAI.DGO" "mai.gd")
(copy-vis-files "MAI")
(copy-sbk-files "MAINCAVE")
(copy-mus-files "MAINCAVE")

(goal-src-sequence
 "levels/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o"
  )
 "maincave/cavecrystal-light.gc"
 "darkcave/darkcave-obs.gc"
 "maincave/maincave-obs.gc"
 "maincave/maincave-part.gc"
 "maincave/spiderwebs.gc"
 "maincave/dark-crystal.gc"
 "maincave/baby-spider.gc"
 "maincave/mother-spider-h.gc"
 "maincave/mother-spider-egg.gc"
 "maincave/mother-spider-proj.gc"
 "maincave/mother-spider.gc"
 "maincave/gnawer.gc"
 "maincave/driller-lurker.gc"
 )

(copy-textures 1313 1315 1314 1312 767)

(copy-gos
  "baby-spider-ag"
  "cavetrapdoor-ag"
  "dark-crystal-ag"
  "driller-lurker-ag"
  "gnawer-ag"
  "launcherdoor-maincave-ag"
  "maincavecam-ag"
  "mother-spider-ag"
  "plat-ag"
  "spider-egg-ag"
  "spiderwebs-ag"
  "water-anim-maincave-ag"
  "water-anim-maincave-water-ag"
  "maincave-vis"
  )

(copy-strs "MAGFCELL")

;;;;;;;;;;;;;;;;;;;;;
;; dark cave
;;;;;;;;;;;;;;;;;;;;;

(cgo "DAR.DGO" "dar.gd")
(copy-vis-files "DAR")
(copy-sbk-files "DARKCAVE")

(copy-textures 1306 1307 1305 1304 1352)

(copy-gos
  "caveelevator-ag"
  "cavecrystal-ag"
  "cavespatula-darkcave-ag"
  "water-anim-darkcave-ag"
  "darkcave-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; robo cave
;;;;;;;;;;;;;;;;;;;;;

(cgo "ROB.DGO" "rob.gd")
(copy-vis-files "ROB")
(copy-sbk-files "ROBOCAVE")

(goal-src-sequence
 "levels/robocave/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
  ("$OUT/obj/ticky.o")
 "cave-trap.gc"
 "spider-egg.gc"
 "robocave-part.gc"
 )

(copy-textures 1318 1319 1317 1316)

(copy-gos
  "cavecrusher-ag"
  "cavespatulatwo-ag"
  "water-anim-robocave-ag"
  "robocave-vis"
  )


;;;;;;;;;;;;;;;;;;;;;
;; lavatube
;;;;;;;;;;;;;;;;;;;;;

(cgo "LAV.DGO" "lav.gd")

(copy-vis-files "LAV")
(copy-sbk-files "LAVATUBE")
(copy-mus-files "LAVATUBE")

(goal-src-sequence
  "levels/lavatube/"
  :deps ("$OUT/obj/ticky.o")

  "lavatube-obs.gc"
  "lavatube-energy.gc"
  "lavatube-part.gc"
  "assistant-lavatube.gc"
  )

(copy-textures 1338 1340 1339 1337)

(copy-gos
  "assistant-lavatube-start-ag"
  "chainmine-ag"
  "darkecobarrel-ag"
  "energyarm-ag"
  "energyball-ag"
  "energybase-ag"
  "energydoor-ag"
  "energyhub-ag"
  "lavaballoon-ag"
  "lavabase-ag"
  "lavafall-ag"
  "lavafallsewera-ag"
  "lavafallsewerb-ag"
  "lavashortcut-ag"
  "lavayellowtarp-ag"
  "water-anim-lavatube-ag"
  "lavatube-vis"
  )

;; assistant-lavatube
(copy-strs "ASLSRESO" "ASLERESO")

;;;;;;;;;;;;;;;;;;;;;
;; citadel
;;;;;;;;;;;;;;;;;;;;;

(cgo "CIT.DGO" "cit.gd")

(copy-vis-files "CIT")
(copy-sbk-files "CITADEL")
(copy-mus-files "CITADEL")

(goal-src-sequence
  "levels/citadel/"
  :deps ("$OUT/obj/battlecontroller.o" "$OUT/obj/snow-bunny.o")

  "citadel-part.gc"
  "citadel-obs.gc"
  "citb-plat.gc"
  "citadel-sages.gc"
  "citb-bunny.gc"
  "citb-drop-plat.gc"
  "assistant-citadel.gc"
  )

(copy-textures 1415 1417 1416 1414)

(copy-gos
  "assistant-lavatube-end-ag"
  "bluesage-ag"
  "citadelcam-ag"
  "citb-arm-ag"
  "citb-arm-shoulder-ag"
  "citb-bunny-ag"
  "citb-button-ag"
  "citb-chain-plat-ag"
  "citb-chains-ag"
  "citb-coil-ag"
  "citb-disc-ag"
  "citb-donut-ag"
  "citb-drop-plat-ag"
  "citb-exit-plat-ag"
  "citb-firehose-ag"
  "citb-generator-ag"
  "citb-hose-ag"
  "citb-iris-door-ag"
  "citb-launcher-ag"
  "citb-robotboss-ag"
  "citb-rotatebox-ag"
  "citb-sagecage-ag"
  "citb-stopbox-ag"
  "evilbro-citadel-ag"
  "evilsis-citadel-ag"
  "green-sagecage-ag"
  "plat-citb-ag"
  "plat-eco-citb-ag"
  "redsage-ag"
  "yellowsage-ag"
  "citadel-vis"
  )

;; green-sagecage
(copy-strs "GRSINTRO" "GRSRESOL" "GRSOPREB")
;; sage-cage
(copy-strs "YERESOLU" "RERESOLU" "BLRESOLU")

;;;;;;;;;;;;;;;;;;;;;
;; Final Boss
;;;;;;;;;;;;;;;;;;;;;

(cgo "FIN.DGO" "fin.gd")

(copy-vis-files "FIN")
(copy-sbk-files "FINALBOS")
(copy-mus-files "FINALBOS" "CREDITS")

(goal-src-sequence
  "levels/finalboss/"
  :deps ("$OUT/obj/assistant-citadel.o")

  "robotboss-h.gc"
  "robotboss-part.gc"
  "sage-finalboss-part.gc"
  "light-eco.gc"
  "robotboss-weapon.gc"
  "robotboss-misc.gc"
  "green-eco-lurker.gc"
  "robotboss.gc"
  "final-door.gc"
  "sage-finalboss.gc"
  )

(copy-textures 1419 1420 634 1418 545)

(copy-gos
  "darkecobomb-ag"
  "ecoclaw-ag"
  "finalbosscam-ag"
  "green-eco-lurker-ag"
  "greenshot-ag"
  "jak-white-ag"
  "light-eco-ag"
  "plat-eco-finalboss-ag"
  "power-left-ag"
  "power-right-ag"
  "powercellalt-ag"
  "redring-ag"
  "robotboss-ag"
  "robotboss-blueeco-ag"
  "robotboss-cinematic-ag"
  "robotboss-redeco-ag"
  "robotboss-yelloweco-ag"
  "silodoor-ag"
  "water-anim-finalboss-ag"
  "finalboss-vis"
  )

;; finalboss
(copy-strs "FIWECO")
;; green-sagecage
(copy-strs "GRSDSACR" "GRSOBBA" "GRSOBBB" "GRSOBBEC" "GRSOBBNC" "GRSOBFIN")

;;;;;;;;;;;;;;;;;;;;;
;; intro only
;;;;;;;;;;;;;;;;;;;;;

(cgo "INT.DGO" "int.gd")

(copy-vis-files "INT")

(goal-src-sequence
 "levels/intro/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o")

 "evilbro.gc"
 )

(copy-textures 1455 1457 1456 1454)

(copy-gos
  "evilbro-ag"
  "evilsis-ag"
  "intro-vis"
  )

;; evilbro
(copy-strs "EVMEND")

;;;;;;;;;;;;;;;;;;;;;
;; demo
;;;;;;;;;;;;;;;;;;;;;

(cgo "DEM.DGO" "dem.gd")

(copy-vis-files "DEM")

(goal-src-sequence
 "levels/demo/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o")

 "static-screen.gc"
 "demo-obs.gc"
 )

(copy-textures 1485 1486 1487 1599 1600 1601 1602 1603 1604 1605 1606 1607 1480 1479)

(copy-gos
  "demo-vis")

;;;;;;;;;;;;;;;;;;;;;
;; title
;;;;;;;;;;;;;;;;;;;;;

(cgo "TIT.DGO" "tit.gd")

(copy-vis-files "TIT")

(goal-src-sequence
 "levels/title/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("$OUT/obj/ticky.o"
  "$OUT/obj/progress-pc.o"
  )

 "title-obs.gc"
 )

(copy-textures 1609 416 415 397 1499)

(copy-gos
  "logo-ag"
  "logo-black-ag"
  "logo-cam-ag"
  "logo-volumes-ag"
  "ndi-ag"
  "ndi-cam-ag"
  "ndi-volumes-ag"
  "title-vis")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Custom Level
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the build system to build the level geometry
;; this path is relative to the custom_levels/ folder
;; it should point to the .jsonc file that specifies the level.
(build-custom-level "test-zone")
;; the DGO file
(custom-level-cgo "TSZ.DGO" "test-zone/testzone.gd")

;;;;;;;;;;;;;;;;;;;;;
;; Game Engine Code
;;;;;;;;;;;;;;;;;;;;;

;; We don't know the actual dependencies, but the build
;; order is a possibly ordering, and the goal-src-sequence
;; will force these to always build in this order.

(goal-src-sequence
 ;; prefix
 "engine/"

 :deps
 ("$OUT/obj/gcommon.o"
  "$OUT/obj/gstate.o"
  "$OUT/obj/gstring.o"
  "$OUT/obj/gkernel.o"
  )

 ;; sources
 "util/types-h.gc"
 "ps2/vu1-macros.gc"
 "math/math.gc"
 "math/vector-h.gc"
 "physics/gravity-h.gc"
 "geometry/bounding-box-h.gc"
 "math/matrix-h.gc"
 "math/quaternion-h.gc"
 "math/euler-h.gc"
 "math/transform-h.gc"
 "geometry/geometry-h.gc"
 "math/trigonometry-h.gc"
 "math/transformq-h.gc"
 "geometry/bounding-box.gc"
 "math/matrix.gc"
 "math/transform.gc"
 "math/quaternion.gc"
 "math/euler.gc"
 "geometry/geometry.gc"
 "math/trigonometry.gc"
 "sound/gsound-h.gc"
 "ps2/timer-h.gc"
 "ps2/timer.gc"
 "ps2/vif-h.gc"
 "dma/dma-h.gc"
 "gfx/hw/video-h.gc"
 "gfx/vu1-user-h.gc"
 "dma/dma.gc"
 "dma/dma-buffer.gc"
 "dma/dma-bucket.gc"
 "dma/dma-disasm.gc"
 )


(goal-src "engine/ps2/pad.gc" "pckernel-h")

(goal-src-sequence
 ;; prefix
 "engine/"

 :deps
 ("$OUT/obj/pad.o"
  "$OUT/obj/dma-disasm.o")
 "gfx/hw/gs.gc"
 "gfx/hw/display-h.gc"
 "math/vector.gc"
 "load/file-io.gc"
 "load/loader-h.gc"
 "gfx/texture/texture-h.gc"
 "level/level-h.gc"
 "gfx/math-camera-h.gc"
 "gfx/math-camera.gc"
 "gfx/font-h.gc"
 "load/decomp-h.gc"
 )

(goal-src "engine/gfx/hw/display.gc" "decomp-h" "pckernel-h")

(goal-src-sequence
 ;; prefix
 "engine/"

 :deps
 ("$OUT/obj/display.o"
  "$OUT/obj/decomp-h.o")
 
 "engine/connect.gc"
 "ui/text-h.gc"
 "game/settings-h.gc"
 )

(goal-src "pc/util/knuth-rand.gc" "settings-h")

(goal-src-sequence
 ;; prefix
 "engine/"

 :deps
 ("$OUT/obj/settings-h.o")

 "util/capture.gc"
 "debug/memory-usage-h.gc"
 "gfx/texture/texture.gc"
 "game/main-h.gc"
 "anim/mspace-h.gc"
 "draw/drawable-h.gc"
 "draw/drawable-group-h.gc"
 "draw/drawable-inline-array-h.gc"
 "draw/draw-node-h.gc"
 "draw/drawable-tree-h.gc"
 "draw/drawable-actor-h.gc"
 "draw/drawable-ambient-h.gc"
 "game/task/game-task-h.gc"
 "game/task/hint-control-h.gc"
 "gfx/generic/generic-h.gc"
 "gfx/lights-h.gc"
 "gfx/ocean/ocean-h.gc"
 "gfx/ocean/ocean-trans-tables.gc"
 "gfx/ocean/ocean-tables.gc"
 "gfx/ocean/ocean-frames.gc"
 "gfx/sky/sky-h.gc"
 "gfx/mood/mood-h.gc"
 "gfx/mood/time-of-day-h.gc"
 "data/art-h.gc"
 "gfx/generic/generic-vu1-h.gc"
 "gfx/merc/merc-h.gc"
 "gfx/merc/generic-merc-h.gc"
 "gfx/tie/generic-tie-h.gc"
 "gfx/generic/generic-work-h.gc"
 "gfx/shadow/shadow-cpu-h.gc"
 "gfx/shadow/shadow-vu1-h.gc"
 "ps2/memcard-h.gc"
 "game/game-info-h.gc"
 "gfx/background/wind-h.gc"
 "gfx/background/prototype-h.gc"
 "anim/joint-h.gc"
 "gfx/foreground/bones-h.gc"
 "engine/engines.gc"
 "entity/res-h.gc"
 "entity/res.gc"
 "gfx/lights.gc"
 "physics/dynamics-h.gc"
 "collide/surface-h.gc"
 "collide/pat-h.gc"
 "game/fact-h.gc"
 "anim/aligner-h.gc"
 "game/game-h.gc"
 "common-obs/generic-obs-h.gc"
 "camera/pov-camera-h.gc"
 "util/sync-info-h.gc"
 "util/smush-control-h.gc"
 "physics/trajectory-h.gc"
 "debug/debug-h.gc"
 "anim/joint-mod-h.gc"
 "collide/collide-func-h.gc"
 "collide/collide-mesh-h.gc"
 "collide/collide-shape-h.gc"
 "collide/collide-target-h.gc"
 "collide/collide-touch-h.gc"
 "collide/collide-edge-grab-h.gc"
 "common-obs/process-drawable-h.gc"
 "game/effect-control-h.gc"
 "collide/collide-frag-h.gc"
 "game/projectiles-h.gc"
 "target/target-h.gc"
 "gfx/depth-cue-h.gc"
 "debug/stats-h.gc"
 "level/bsp-h.gc"
 "collide/collide-cache-h.gc"
 "collide/collide-h.gc"
 "gfx/shrub/shrubbery-h.gc"
 "gfx/tie/tie-h.gc"
 "gfx/tfrag/tfrag-h.gc"
 "gfx/background/background-h.gc"
 "gfx/background/subdivide-h.gc"
 "entity/entity-h.gc"
 "gfx/sprite/sprite-h.gc"
 "gfx/shadow/shadow-h.gc"
 "gfx/foreground/eye-h.gc"
 "gfx/sprite/sparticle/sparticle-launcher-h.gc"
 "gfx/sprite/sparticle/sparticle-h.gc"
 "entity/actor-link-h.gc"
 "camera/camera-h.gc"
 "camera/cam-debug-h.gc"
 "camera/cam-interface-h.gc"
 "camera/cam-update-h.gc"
 "debug/assert-h.gc"
 "ui/hud-h.gc"
 "ui/progress/progress-h.gc"
 "ps2/rpc-h.gc"
 "geometry/path-h.gc"
 "nav/navigate-h.gc"
 "load/load-dgo.gc"
 "load/ramdisk.gc"
 "sound/gsound.gc"
 "math/transformq.gc"
 "collide/collide-func.gc"
 "anim/joint.gc"
 "geometry/cylinder.gc"
 "gfx/background/wind.gc"
 "level/bsp.gc"
 "gfx/background/subdivide.gc"
 "gfx/sprite/sprite.gc"
 "gfx/sprite/sprite-distort.gc"
 "debug/debug-sphere.gc"
 "debug/debug.gc"
 "gfx/merc/merc-vu1.gc"
 "gfx/merc/merc-blend-shape.gc"
 "gfx/merc/merc.gc"
 "gfx/foreground/ripple.gc"
 "gfx/foreground/bones.gc"
 "gfx/generic/generic-vu0.gc"
 "gfx/generic/generic.gc"
 "gfx/generic/generic-vu1.gc"
 "gfx/generic/generic-effect.gc"
 "gfx/generic/generic-merc.gc"
 "gfx/generic/generic-tie.gc"
 "gfx/shadow/shadow-cpu.gc"
 "gfx/shadow/shadow-vu1.gc"
 "gfx/depth-cue.gc"
 "gfx/font.gc"
 "load/decomp.gc"
 "gfx/background/background.gc"
 "draw/draw-node.gc"
 "gfx/shrub/shrubbery.gc"
 "gfx/shrub/shrub-work.gc"
 "gfx/tfrag/tfrag-near.gc"
 "gfx/tfrag/tfrag.gc"
 "gfx/tfrag/tfrag-methods.gc"
 "gfx/tfrag/tfrag-work.gc"
 "gfx/tie/tie.gc"
 "gfx/tie/tie-near.gc"
 "gfx/tie/tie-work.gc"
 "gfx/tie/tie-methods.gc"
 "util/sync-info.gc"
 "physics/trajectory.gc"
 "gfx/sprite/sparticle/sparticle-launcher.gc"
 "gfx/sprite/sparticle/sparticle.gc"
 "entity/entity-table.gc"
 "load/loader.gc"
 "game/task/task-control-h.gc"
 "game/game-info.gc"
 "game/game-save.gc"
 "game/settings.gc"
 "gfx/mood/mood-tables.gc"
 "gfx/mood/mood.gc"
 "gfx/mood/weather-part.gc"
 "gfx/mood/time-of-day.gc"
 "gfx/sky/sky-utils.gc"
 "gfx/sky/sky.gc"
 "gfx/sky/sky-tng.gc"
 "level/load-boundary-h.gc"
 "level/load-boundary.gc"
 "level/load-boundary-data.gc"
 "level/level-info.gc"
 "level/level.gc"
 "ui/text.gc"
 "collide/collide-probe.gc"
 "collide/collide-frag.gc"
 "collide/collide-mesh.gc"
 "collide/collide-touch.gc"
 "collide/collide-edge-grab.gc"
 "collide/collide-shape.gc"
 "collide/collide-shape-rider.gc"
 "collide/collide.gc"
 "collide/collide-planes.gc"
 "gfx/merc/merc-death.gc"
 "common-obs/water-h.gc"
 "camera/camera.gc"
 "camera/cam-interface.gc"
 "camera/cam-master.gc"
 "camera/cam-states.gc"
 "camera/cam-states-dbg.gc"
 "camera/cam-combiner.gc"
 "camera/cam-update.gc"
 "geometry/vol-h.gc"
 "camera/cam-layout.gc"
 "camera/cam-debug.gc"
 "camera/cam-start.gc"
 "common-obs/process-drawable.gc"
 "game/task/hint-control.gc"
 "entity/ambient.gc"
 "debug/assert.gc"
 "common-obs/generic-obs.gc"
 "target/target-util.gc"
 "target/target-part.gc"
 "target/collide-reaction-target.gc"
 "target/logic-target.gc"
 "target/sidekick.gc"
 "common-obs/voicebox.gc"
 "target/target-handler.gc"
 "target/target.gc"
 "target/target2.gc"
 "target/target-death.gc"
 "debug/menu.gc"
 "draw/drawable.gc"
 "draw/drawable-group.gc"
 "draw/drawable-inline-array.gc"
 "draw/drawable-tree.gc"
 "gfx/background/prototype.gc"
 "collide/main-collide.gc"
 "gfx/hw/video.gc"
 )

(goal-src "engine/game/main.gc" "pckernel" "video")

(goal-src-sequence
 ;; prefix
 "engine/"

 :deps
 ("$OUT/obj/main.o"
  "$OUT/obj/video.o")

 "collide/collide-cache.gc"
 "entity/relocate.gc"
 "debug/memory-usage.gc"
 "entity/entity.gc"
 "geometry/path.gc"
 "geometry/vol.gc"
 "nav/navigate.gc"
 "anim/aligner.gc"
 "game/effect-control.gc"
 "common-obs/water.gc"
 "common-obs/collectables-part.gc"
 "common-obs/collectables.gc"
 "game/task/task-control.gc"
 "common-obs/process-taskable.gc"
 "camera/pov-camera.gc"
 "game/powerups.gc"
 "common-obs/crates.gc"
 "ui/hud.gc"
 "ui/hud-classes.gc"
 "ui/progress/progress-static.gc"
 "ui/progress/progress-part.gc"
 "ui/progress/progress-draw.gc"
 "ui/progress/progress.gc"
 "ui/credits.gc"
 "game/projectiles.gc"
 "gfx/ocean/ocean.gc"
 "gfx/ocean/ocean-vu0.gc"
 "gfx/ocean/ocean-texture.gc"
 "gfx/ocean/ocean-mid.gc"
 "gfx/ocean/ocean-transition.gc"
 "gfx/ocean/ocean-near.gc"
 "gfx/shadow/shadow.gc"
 "gfx/foreground/eye.gc"
 "util/glist-h.gc"
 "util/glist.gc"
 "debug/anim-tester.gc"
 "debug/viewer.gc"
 "debug/part-tester.gc"

 "gfx/texture/texture-upload.gc"
 "common-obs/rigid-body-h.gc"
 "common-obs/water-anim.gc"
 "common-obs/dark-eco-pool.gc"
 "common-obs/rigid-body.gc"
 "common-obs/nav-enemy-h.gc"
 "common-obs/nav-enemy.gc"
 "common-obs/baseplat.gc"
 "common-obs/basebutton.gc"
 "common-obs/tippy.gc"
 "anim/joint-exploder.gc"
 "common-obs/babak.gc"
 "common-obs/sharkey.gc"
 "common-obs/orb-cache.gc"
 "common-obs/plat.gc"
 "common-obs/plat-button.gc"
 "common-obs/plat-eco.gc"
 "common-obs/ropebridge.gc"
 "common-obs/ticky.gc"
 )

(copy-mus-files "DANGER")

(copy-vag-files "ENG" "FRE" "GER" "ITA" "SPA" "JAP")

;;;;;;;;;;;;;;;;;;;;;
;; ISO Group
;;;;;;;;;;;;;;;;;;;;;
;; the iso group is a group of files built by the "(mi)" command.

(group-list "iso"
 `("$OUT/iso/0COMMON.TXT"
   "$OUT/iso/0SUBTIT.TXT"
   "$OUT/iso/TWEAKVAL.MUS"
   "$OUT/iso/VAGDIR.AYB"
   ,@(reverse *all-vis*)
   ,@(reverse *all-str*)
   ,@(reverse *all-sbk*)
   ,@(reverse *all-mus*)
   ,@(reverse *all-vag*)
   ,@(reverse *all-cgos*))
 )


;;(fmt #t "found {} spools\n" (count *all-str*))
(group-list "spools" *all-str*)


(group-list "text"
 `("$OUT/iso/0COMMON.TXT"
   "$OUT/iso/0SUBTIT.TXT"
   )
 )

;; Custom or Modified Code
(goal-src "pc/features/autosplit-h.gc")
(goal-src "pc/features/autosplit.gc" "autosplit-h" "task-control-h")
(goal-src "pc/pckernel-h.gc" "dma-buffer")
(goal-src "pc/util/pc-anim-util.gc" "target-h")
(goal-src "pc/pckernel.gc" "pc-anim-util" "settings" "video" "target-h" "autosplit-h")
(goal-src "pc/subtitle.gc" "text" "pckernel" "hint-control" "loader-h" "gsound" "ambient")
(goal-src "pc/progress-pc.gc" "progress" "pckernel")
(goal-src "pc/util/anim-tester-x.gc" "pckernel" "gstring" "joint" "process-drawable" "art-h" "effect-control")
(goal-src "pc/hud-classes-pc.gc" "pckernel" "hud" "battlecontroller" "generic-obs")

;; the debug menu is modified to include PC specific options:
(goal-src "engine/debug/default-menu.gc" "anim-tester-x" "part-tester")

(group-list "all-code"
  `(,@(reverse *all-gc*))
  )

