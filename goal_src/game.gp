;;-*-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jak 1 Project File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file sets up the OpenGOAL build system for Jak 1.

;;;;;;;;;;;;;;;;;;;;;;;
;; Build system macros
;;;;;;;;;;;;;;;;;;;;;;;

;; use defmacro to define goos macros.
(define defmacro defsmacro)
(define defun desfun)

(defun gc-file->o-file (filename)
  "Get the name of the object file for the given GOAL (*.gc) source file."
  (string-append "out/obj/" (stem filename) ".o")
  )

(defmacro goal-src (src-file &rest deps)
  "Add a GOAL source file with the given dependencies"
  `(defstep :in ,(string-append "goal_src/" src-file)
     ;; use goal compiler
     :tool 'goalc
     ;; will output the obj file
     :out '(,(gc-file->o-file src-file))
     ;; dependencies are the obj files
     :dep '(,@(apply gc-file->o-file deps))
     )
  )

(defun make-src-sequence-elt (current previous prefix)
  "Helper for goal-src-sequence"
  `(defstep :in ,(string-append "goal_src/" prefix current)
     :tool 'goalc
     :out '(,(gc-file->o-file current))
     :dep '(#|"iso/KERNEL.CGO"|#
           ,(gc-file->o-file previous))
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

(defmacro cgo (output-name desc-file-name)
  "Add a CGO with the given output name (in out/iso) and input name (in goal_src/dgos)"
  `(defstep :in ,(string-append "goal_src/dgos/" desc-file-name)
     :tool 'dgo
     :out '(,(string-append "out/iso/" output-name))
     )
  )

(defun tpage-name (id)
  (fmt #f "tpage-{}.go" id)
  )

(defmacro copy-texture (tpage-id)
  (let* ((folder (get-environment-variable "OPENGOAL_DECOMP_DIR" :default ""))
         (path (string-append "decompiler_out/" folder "raw_obj/" (tpage-name tpage-id))))
    `(defstep :in ,path
              :tool 'copy
              :out '(,(string-append "out/obj/" (tpage-name tpage-id))))))

(defmacro copy-textures (&rest ids)
  `(begin
    ,@(apply (lambda (x) `(copy-texture ,x)) ids)
    )
  )

(defmacro copy-go (name)
  (let* ((folder (get-environment-variable "OPENGOAL_DECOMP_DIR" :default ""))
         (path (string-append "decompiler_out/" folder "raw_obj/" name ".go")))
    `(defstep :in ,path
              :tool 'copy
              :out '(,(string-append "out/obj/" name ".go")))))

(defmacro copy-gos (&rest gos)
  `(begin
    ,@(apply (lambda (x) `(copy-go ,x)) gos)
    )
  )

(defmacro group (name &rest stuff)
  `(defstep :in ""
     :tool 'group
     :out '(,(string-append "GROUP:" name))
     :dep '(,@stuff))
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
(defstep :in "assets/tpage-dir.txt"
  :tool 'tpage-dir
  :out '("out/obj/dir-tpages.go")
  )

;; the count file.
(defstep :in "assets/game_count.txt"
  :tool 'game-cnt
  :out '("out/obj/game-cnt.go")
  )


;;;;;;;;;;;;;;;;;;;;;
;; Textures (Common)
;;;;;;;;;;;;;;;;;;;;;
                 
(copy-textures 463 2 880 256 1278 1032 62 1532)


;;;;;;;;;;;;;;;;;;;;;
;; Art (Common)
;;;;;;;;;;;;;;;;;;;;;

(copy-gos
 "fuel-cell-ag"
 "money-ag"
 "buzzer-ag"
 "ecovalve-ag-ART-GAME"
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

(defstep :in "assets/game_text.txt"
  :tool 'text
  :out '("out/iso/0COMMON.TXT"
         "out/iso/1COMMON.TXT"
         "out/iso/2COMMON.TXT"
         "out/iso/3COMMON.TXT"
         "out/iso/4COMMON.TXT"
         "out/iso/5COMMON.TXT"
         "out/iso/6COMMON.TXT")
  )


;;;;;;;;;;;;;;;;;;;;;
;; ISO Group
;;;;;;;;;;;;;;;;;;;;;
;; the iso group is a group of files required to boot.

(group "iso"
       "out/iso/0COMMON.TXT"
       "out/iso/KERNEL.CGO"
       "out/iso/GAME.CGO"
       "out/iso/VI1.DGO"
       "out/iso/VI2.DGO"
       "out/iso/VI3.DGO"
       "out/iso/TRA.DGO"
       "out/iso/FIN.DGO"
       "out/iso/FIC.DGO"
       "out/iso/JUN.DGO"
       "out/iso/MAI.DGO"
       "out/iso/BEA.DGO"
       "out/iso/CIT.DGO"
       "out/iso/SUN.DGO"
       )


;;;;;;;;;;;;;;;;;;;;;
;; engine Group
;;;;;;;;;;;;;;;;;;;;;
;; the engine group is a group of files loaded as the game engine with no levels

(group "engine"
       "out/iso/0COMMON.TXT"
       "out/iso/KERNEL.CGO"
       "out/iso/GAME.CGO"
       )


;;;;;;;;;;;;;;;;;;;;;
;; hub1 Group
;;;;;;;;;;;;;;;;;;;;;
;; the hub1 group is a group of files required to play the first hub (village1, jungle, beach, misty, training, firecanyon)

(group "hub1"
       "out/iso/0COMMON.TXT"
       "out/iso/KERNEL.CGO"
       "out/iso/GAME.CGO"
       "out/iso/VI1.DGO"
       "out/iso/TRA.DGO"
       "out/iso/FIC.DGO"
       "out/iso/JUN.DGO"
       "out/iso/BEA.DGO"
       )

;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Level Objects
;;;;;;;;;;;;;;;;;;;;;;;;

;; as we find objects that exist in multiple levels, put them here

(copy-gos
 "warpgate-ag"
 "sharkey-ag-BEA-TRA-VI2"
 "eichar-racer+0-ag"

 "babak-ag"
  )


;;;;;;;;;;;;;;;;;;;;;
;; Common Level Code
;;;;;;;;;;;;;;;;;;;;;

(goal-src-sequence
  "levels/"
   :deps ;; no idea what these depend on, make it depend on the whole engine
   ("out/obj/default-menu.o")

   "village_common/villagep-obs.gc"
   "village_common/oracle.gc"

   "common/blocking-plane.gc"
   "common/launcherdoor.gc"
   "common/mistycannon.gc"
   "common/babak-with-cannon.gc"
   "common/snow-bunny.gc"
   "common/battlecontroller.gc"

   "racer_common/target-racer-h-FIC-LAV-MIS-OGR-ROL.gc"
   "racer_common/racer-part.gc"
   "racer_common/racer.gc"
   "racer_common/target-racer-FIC-LAV-MIS-OGR-ROL.gc"
   "racer_common/racer-states-FIC-LAV-MIS-OGR-ROL.gc"
   "racer_common/collide-reaction-racer.gc"

   )


;;;;;;;;;;;;;;;;;;;;;
;; Village 1
;;;;;;;;;;;;;;;;;;;;;

;; the definition for the DGO file.
(cgo "VI1.DGO"
     "vi1.gd"
     )

;; the code
(goal-src-sequence
 "levels/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("out/obj/default-menu.o")

 "village1/farmer.gc"
 "village1/explorer.gc"
 "village1/assistant.gc"
 "village1/sage.gc"
 "village1/yakow.gc"
 "village1/village-obs-VI1.gc"
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
 "oracle-ag-VI1"
 "orb-cache-top-ag-VI1"
 "reflector-middle-ag"
 "revcycle-ag"
 "revcycleprop-ag"
 "ropebridge-32-ag"
 "sage-ag"
 "sagesail-ag"
 "sharkey-ag-VI1"
 "villa-starfish-ag"
 "village-cam-ag-VI1"
 "village1cam-ag"
 "warp-gate-switch-ag-VI1-VI3"
 "water-anim-village1-ag"
 "windmill-sail-ag"
 "windspinner-ag"
 "yakow-ag"
 "village1-vis"
 )

;;;;;;;;;;;;;;;;;;;;;
;; Jungle
;;;;;;;;;;;;;;;;;;;;;

(cgo "JUN.DGO"
  "jun.gd")

(goal-src-sequence
 "levels/jungle/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("out/obj/default-menu.o")

 "jungle-elevator.gc"
 "bouncer.gc"
 "hopper.gc"
 "junglesnake.gc"
 "darkvine.gc"
 "jungle-obs.gc"
 "jungle-mirrors.gc"
 "junglefish.gc"
 "fisher-JUN.gc"
 "jungle-part.gc"
 )

(copy-gos
  "eichar-fish+0-ag-JUN"
  "accordian-ag"
  "bounceytarp-ag"
  "catch-fisha-ag"
  "catch-fishb-ag"
  "catch-fishc-ag"
  "darkvine-ag-JUN"
  "ecovalve-ag-JUB-JUN"
  "fish-net-ag"
  "fisher-ag"
  "hopper-ag"
  "junglecam-ag"
  "junglefish-ag"
  "junglesnake-ag"
  "launcherdoor-ag-JUN"
  "logtrap-ag"
  "lurkerm-piston-ag"
  "lurkerm-tall-sail-ag"
  "maindoor-ag"
  "medres-firecanyon-ag"
  "orb-cache-top-ag-JUN"
  "periscope-ag"
  "plat-button-ag"
  "plat-eco-ag-JUN"
  "precurbridge-ag"
  "reflector-mirror-ag"
  "ropebridge-52-ag"
  "ropebridge-70-ag"
  "sharkey-ag-JUN-MIS"
  "sidedoor-ag"
  "towertop-ag"
  "water-anim-jungle-ag"
  "jungle-vis"
  )

(copy-textures 385 531 386 388 765)


;;;;;;;;;;;;;;;;;;;;;
;; Beach
;;;;;;;;;;;;;;;;;;;;;

(cgo "BEA.DGO"
  "bea.gd"
  )

(goal-src-sequence
  "levels/beach/"
  :deps ("out/obj/default-menu.o")
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
  "barrel-ag-BEA"
  "beachcam-ag"
  "bird-lady-ag"
  "bird-lady-beach-ag"
  "bladeassm-ag"
  "ecovalve-ag-BEA"
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
  "mistycannon-ag"
  "orb-cache-top-ag-BEA"
  "pelican-ag"
  "sack-ag-BEA"
  "sculptor-ag"
  "sculptor-muse-ag"
  "seagull-ag"
  "windmill-one-ag"
  "beach-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; Fire Canyon
;;;;;;;;;;;;;;;;;;;;;

(cgo "FIC.DGO"
     "fic.gd"
     )

(copy-textures 1119) ;; might be common/zoomer hud??

(goal-src-sequence
 "levels/firecanyon/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("out/obj/default-menu.o")

 "firecanyon-part.gc"
 "assistant-firecanyon.gc"
 "firecanyon-obs.gc"

 )

(copy-textures 815 822 854 1123)

(copy-gos
  "assistant-firecanyon-ag"
  "balloon-ag"
  "crate-darkeco-cluster-ag-FIC"
  "ecovalve-ag-FIC-OGR"
  "ef-plane-ag-FIC-LAV-OGR-ROL-SNO-SWA"
  "racer-ag-FIC-ROL"
  "spike-ag"
  "firecanyon-vis")

;;;;;;;;;;;;;;;;;;;;;
;; Training
;;;;;;;;;;;;;;;;;;;;;

;; the definition of the DGO package for the level
(cgo "TRA.DGO"
     "tra.gd")

;; The code
(goal-src-sequence
  "levels/training/"
  :deps ("out/obj/default-menu.o") ;; makes us depend on the whole engine

  "training-obs.gc"
  "training-part.gc"
  )

;; the textures
(copy-textures 1309 1311 1310 1308 775)

(copy-gos
  "ecovalve-ag-TRA"
  "jng-iris-door-ag-TRA"
  "plat-eco-ag-TRA"
  "pontoonfive-ag-TRA"
  "scarecrow-a-ag"
  "scarecrow-b-ag"
  "trainingcam-ag"
  "warp-gate-switch-ag-TRA"
  "water-anim-training-ag"
  "training-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; Village 2
;;;;;;;;;;;;;;;;;;;;;

(cgo "VI2.DGO" "vi2.gd")

(goal-src-sequence
 "levels/village2/"
 :deps ("out/obj/default-menu.o")
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
  "barrel-ag-VI2"
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
  "oracle-ag-VI2"
  "orb-cache-top-ag-VI2"
  "pontoonfive-ag-VI2"
  "pontoonten-ag"
  "precursor-arm-ag"
  "sage-bluehut-ag"
  "sunken-elevator-ag"
  "swamp-blimp-ag"
  "swamp-rope-ag"
  "swamp-tetherrock-ag"
  "swamp-tetherrock-explode-ag"
  "swampcam-ag-VI2"
  "village-cam-ag-VI2"
  "village2cam-ag"
  "warp-gate-switch-ag-VI2"
  "warrior-ag"
  "water-anim-village2-ag"
  "village2-vis"
  )


;;;;;;;;;;;;;;;;;;;;;
;; LPC
;;;;;;;;;;;;;;;;;;;;;

(cgo "SUN.DGO" "sun.gd")

(goal-src-sequence
  "levels/sunken/"
  :deps ("out/obj/default-menu.o")
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
  "eichar-tube+0-ag-SUN"
  "bully-ag"
  "double-lurker-ag"
  "double-lurker-top-ag"
  "exit-chamber-ag"
  "generic-button-ag"
  "launcherdoor-ag-SUN"
  "orb-cache-top-ag-SUN"
  "orbit-plat-ag"
  "orbit-plat-bottom-ag"
  "plat-sunken-ag"
  "puffer-ag"
  "qbert-plat-ag"
  "qbert-plat-on-ag"
  "seaweed-ag"
  "shover-ag-SUN"
  "side-to-side-plat-ag"
  "square-platform-ag"
  "steam-cap-ag-SUN"
  "sun-iris-door-ag"
  "sunkencam-ag-SUN"
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
;; Village 3
;;;;;;;;;;;;;;;;;;;;;

;; the definition for the DGO file.
(cgo "VI3.DGO" "vi3.gd")

;; the code
(goal-src-sequence
 "levels/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("out/obj/default-menu.o")
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
  "oracle-ag-VI3"
  "pistons-ag"
  "sage-village3-ag"
  "vil3-bridge-36-ag"
  "village-cam-ag-VI3"
  "water-anim-village3-ag"
  "village3-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; Spider Cave
;;;;;;;;;;;;;;;;;;;;;

(cgo "MAI.DGO" "mai.gd")

(goal-src-sequence
 "levels/"
 :deps ;; no idea what these depend on, make it depend on the whole engine
 ("out/obj/default-menu.o"
  ;;"out/obj/darkcave-obs.o"
  )
 "maincave/cavecrystal-light.gc"
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
  "baby-spider-ag-MAI"
  "cavetrapdoor-ag-MAI"
  "dark-crystal-ag"
  "driller-lurker-ag"
  "ecovalve-ag-MAI"
  "gnawer-ag"
  "launcherdoor-maincave-ag"
  "maincavecam-ag"
  "mother-spider-ag"
  "plat-ag-MAI"
  "spider-egg-ag-DAR-MAI"
  "spiderwebs-ag"
  "water-anim-maincave-ag"
  "water-anim-maincave-water-ag"
  "maincave-vis"
  )

; (goal-src-sequence
;  "levels/"
;  :deps ;; no idea what these depend on, make it depend on the whole engine
;  ("out/obj/default-menu.o" "out/obj/cavecrystal-light.o")
;  "darkcave/darkcave-obs.gc"
;  )

;;;;;;;;;;;;;;;;;;;;;
;; citadel
;;;;;;;;;;;;;;;;;;;;;

(cgo "CIT.DGO" "cit.gd")

(goal-src-sequence
  "levels/citadel/"
  :deps ("out/obj/default-menu.o")

  "citadel-part.gc"
  "citadel-obs.gc"
  "citb-plat.gc"
  "citadel-sages.gc"
  "citb-bunny.gc"
  "citb-drop-plat-CIT.gc"
  "assistant-citadel.gc"
  )

(copy-textures 1415 1417 1416 1414)

(copy-gos
  "babak-ag-CIT"
  "ecovalve-ag-CIT"
  "orb-cache-top-ag-CIT"
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
  "warp-gate-switch-ag-CIT"
  "yellowsage-ag"
  "citadel-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; Final Boss
;;;;;;;;;;;;;;;;;;;;;

(cgo "FIN.DGO" "fin.gd")

(goal-src-sequence
  "levels/finalboss/"
  :deps ("out/obj/default-menu.o")

  "robotboss-h.gc"
  "robotboss-part.gc"
  "sage-finalboss-part.gc"
  "light-eco.gc"
  "robotboss-weapon.gc"
  "robotboss-misc.gc"
  "green-eco-lurker.gc"
  "robotboss.gc"
  "final-door.gc"
  "sage-finalboss-FIN.gc"
  )

(copy-textures 1419 1420 634 1418 545)

(copy-gos
  "darkecobomb-ag"
  "ecoclaw-ag"
  "ecovalve-ag-FIN"
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
 ("out/obj/gcommon.o"
  "out/obj/gstate.o"
  "out/obj/gstring.o"
  "out/obj/gkernel.o"
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
 "gfx/hw/vu1-user-h.gc"
 "dma/dma.gc"
 "dma/dma-buffer.gc"
 "dma/dma-bucket.gc"
 "dma/dma-disasm.gc"
 "ps2/pad.gc"
 "gfx/hw/gs.gc"
 "gfx/hw/display-h.gc"
 "math/vector.gc"
 "load/file-io.gc"
 "load/loader-h.gc"
 "gfx/texture-h.gc"
 "level/level-h.gc"
 "camera/math-camera-h.gc"
 "camera/math-camera.gc"
 "gfx/font-h.gc"
 "gfx/decomp-h.gc"
 "gfx/hw/display.gc"
 "engine/connect.gc"
 "ui/text-h.gc"
 "game/settings-h.gc"
 "gfx/capture.gc"
 "debug/memory-usage-h.gc"
 "gfx/texture.gc"
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
 "gfx/mood-h.gc"
 "gfx/time-of-day-h.gc"
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
 "gfx/wind-h.gc"
 "gfx/tie/prototype-h.gc"
 "anim/joint-h.gc"
 "anim/bones-h.gc"
 "engine/engines.gc"
 "data/res-h.gc"
 "data/res.gc"
 "gfx/lights.gc"
 "physics/dynamics-h.gc"
 "target/surface-h.gc"
 "target/pat-h.gc"
 "game/fact-h.gc"
 "anim/aligner-h.gc"
 "game/game-h.gc"
 "game/generic-obs-h.gc"
 "camera/pov-camera-h.gc"
 "util/sync-info-h.gc"
 "util/smush-control-h.gc"
 "physics/trajectory-h.gc"
 "debug/debug-h.gc"
 "target/joint-mod-h.gc"
 "collide/collide-func-h.gc"
 "collide/collide-mesh-h.gc"
 "collide/collide-shape-h.gc"
 "collide/collide-target-h.gc"
 "collide/collide-touch-h.gc"
 "collide/collide-edge-grab-h.gc"
 "draw/process-drawable-h.gc"
 "game/effect-control-h.gc"
 "collide/collide-frag-h.gc"
 "game/projectiles-h.gc"
 "target/target-h.gc"
 "gfx/depth-cue-h.gc"
 "debug/stats-h.gc"
 "gfx/vis/bsp-h.gc"
 "collide/collide-cache-h.gc"
 "collide/collide-h.gc"
 "gfx/shrub/shrubbery-h.gc"
 "gfx/tie/tie-h.gc"
 "gfx/tfrag/tfrag-h.gc"
 "gfx/background-h.gc"
 "gfx/tfrag/subdivide-h.gc"
 "entity/entity-h.gc"
 "gfx/sprite/sprite-h.gc"
 "gfx/shadow/shadow-h.gc"
 "gfx/eye-h.gc"
 "sparticle/sparticle-launcher-h.gc"
 "sparticle/sparticle-h.gc"
 "entity/actor-link-h.gc"
 "camera/camera-h.gc"
 "camera/cam-debug-h.gc"
 "camera/cam-interface-h.gc"
 "camera/cam-update-h.gc"
 "debug/assert-h.gc"
 "ui/hud-h.gc"
 "ui/progress-h.gc"
 "ps2/rpc-h.gc"
 "nav/path-h.gc"
 "nav/navigate-h.gc"
 "load/load-dgo.gc"
 "load/ramdisk.gc"
 "sound/gsound.gc"
 "math/transformq.gc"
 "collide/collide-func.gc"
 "anim/joint.gc"
 "geometry/cylinder.gc"
 "gfx/wind.gc"
 "gfx/vis/bsp.gc"
 "gfx/tfrag/subdivide.gc"
 "gfx/sprite/sprite.gc"
 "gfx/sprite/sprite-distort.gc"
 "debug/debug-sphere.gc"
 "debug/debug.gc"
 "gfx/merc/merc-vu1.gc"
 "gfx/merc/merc-blend-shape.gc"
 "gfx/merc/merc.gc"
 "gfx/ripple.gc"
 "anim/bones.gc"
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
 "gfx/background.gc"
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
 "sparticle/sparticle-launcher.gc"
 "sparticle/sparticle.gc"
 "entity/entity-table.gc"
 "load/loader.gc"
 "game/task/task-control-h.gc"
 "game/game-info.gc"
 "game/game-save.gc"
 "game/settings.gc"
 "ambient/mood-tables.gc"
 "ambient/mood.gc"
 "ambient/weather-part.gc"
 "gfx/time-of-day.gc"
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
 "gfx/water/water-h.gc"
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
 "draw/process-drawable.gc"
 "game/task/hint-control.gc"
 "ambient/ambient.gc"
 "debug/assert.gc"
 "game/generic-obs.gc"
 "target/target-util.gc"
 "target/target-part.gc"
 "collide/collide-reaction-target.gc"
 "target/logic-target.gc"
 "target/sidekick.gc"
 "game/voicebox.gc"
 "target/target-handler.gc"
 "target/target.gc"
 "target/target2.gc"
 "target/target-death.gc"
 "debug/menu.gc"
 "draw/drawable.gc"
 "draw/drawable-group.gc"
 "draw/drawable-inline-array.gc"
 "draw/drawable-tree.gc"
 "gfx/tie/prototype.gc"
 "collide/main-collide.gc"
 "game/video.gc"
 "game/main.gc"
 "collide/collide-cache.gc"
 "entity/relocate.gc"
 "debug/memory-usage.gc"
 "entity/entity.gc"
 "nav/path.gc"
 "geometry/vol.gc"
 "nav/navigate.gc"
 "anim/aligner.gc"
 "game/effect-control.gc"
 "gfx/water/water.gc"
 "game/collectables-part.gc"
 "game/collectables.gc"
 "game/task/task-control.gc"
 "game/task/process-taskable.gc"
 "camera/pov-camera.gc"
 "game/powerups.gc"
 "game/crates.gc"
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
 "gfx/eye.gc"
 "util/glist-h.gc"
 "util/glist.gc"
 "debug/anim-tester.gc"
 "debug/viewer.gc"
 "debug/part-tester.gc"
 "debug/default-menu.gc"
 )

(goal-src-sequence
 "levels/common/"
 :deps ("out/obj/default-menu.o")
 "texture-upload.gc"
 "rigid-body-h.gc"
 "water-anim.gc"
 "dark-eco-pool.gc"
 "rigid-body.gc"
 "nav-enemy-h.gc"
 "nav-enemy.gc"
 "baseplat.gc"
 "basebutton.gc"
 "tippy.gc"
 "joint-exploder.gc"
 "babak.gc"
 "sharkey.gc"
 "orb-cache.gc"
 "plat.gc"
 "plat-button.gc"
 "plat-eco.gc"
 "ropebridge.gc"
 "ticky.gc"
 )


