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
    (defstep :in ,(string-append "goal_src/jak2/" src-file)
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
    (defstep :in ,(string-append "goal_src/jak2/" prefix current)
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

(defun cgo (output-name desc-file-name)
  "Add a CGO with the given output name (in $OUT/iso) and input name (in goal_src/jak2/dgos)"
  (let ((out-name (string-append "$OUT/iso/" output-name)))
    (defstep :in (string-append "goal_src/jak2/dgos/" desc-file-name)
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


(defun copy-iso-file (name subdir ext)
  (let* ((path (string-append "$ISO/" subdir name ext))
         (out-name (string-append "$OUT/iso/" name ext)))
    (defstep :in path
             :tool 'copy
             :out `(,out-name))
    out-name))

(defmacro copy-strs (&rest strs)
  `(begin ,@(apply (lambda (x) `(set! *all-str* (cons (copy-iso-file ,x "STR/" ".STR") *all-str*))) strs)))

(defmacro copy-sbk-files (&rest files)
  `(begin ,@(apply (lambda (x) `(set! *all-sbk* (cons (copy-iso-file ,x "SBK/" ".SBK") *all-sbk*))) files)))

(defmacro copy-mus-files (&rest files)
  `(begin ,@(apply (lambda (x) `(set! *all-mus* (cons (copy-iso-file ,x "MUS/" ".MUS") *all-mus*))) files)))

(defmacro copy-vag-files (&rest files)
  `(begin ,@(apply (lambda (x) `(set! *all-vag* (cons (copy-iso-file "VAGWAD" "VAG/" (string-append "." ,x)) *all-vag*))) files)))

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

(cgo "KERNEL.CGO" "kernel.gd")

;;;;;;;;;;;;;
;; engine
;;;;;;;;;;;;;

(goal-src-sequence
  "engine/"
  :deps
  ("$OUT/obj/gcommon.o"
    "$OUT/obj/gstate.o"
    "$OUT/obj/gstring.o"
    "$OUT/obj/gkernel.o"
    )
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
  "math/trigonometry.gc"
  "sound/gsound-h.gc"
  "ps2/timer-h.gc"
  "ps2/vif-h.gc"
  "dma/dma-h.gc"
  "gfx/hw/video-h.gc"
  "gfx/vu1-user-h.gc"
  "util/profile-h.gc"
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
  "geometry/geometry.gc"
  "ps2/timer.gc"
  "math/vector.gc"
  "load/file-io.gc"
  "load/loader-h.gc"
  "gfx/texture/texture-h.gc"
  "gfx/texture/texture-anim-h.gc"
  "gfx/lights-h.gc"
  "gfx/mood/mood-h.gc"
  "level/level-h.gc"
  "util/capture-h.gc"
  "gfx/math-camera-h.gc"
  "gfx/math-camera.gc"
  "gfx/font-h.gc"
  "load/decomp-h.gc"
  "util/profile.gc"
  "gfx/hw/display.gc"
  "engine/connect.gc"
  "ui/text-id-h.gc"
  "ui/text-h.gc"
  "camera/camera-defs-h.gc"
)

(goal-src-sequence
  "levels/"
  :deps ("$OUT/obj/camera-defs-h.o")
  "city/common/trail-h.gc"
  )

(goal-src-sequence
  "engine/"
  :deps
  ("$OUT/obj/trail-h.o")
  "ui/minimap-h.gc"
  "ui/bigmap-h.gc"
  "game/settings-h.gc"
  "util/capture.gc"
  "debug/memory-usage-h.gc"
  "gfx/blit-displays-h.gc"
  "gfx/texture/texture.gc"
  "game/main-h.gc"
  "anim/mspace-h.gc"
  "draw/drawable-h.gc"
  "draw/drawable-group-h.gc"
  "draw/drawable-inline-array-h.gc"
  "draw/draw-node-h.gc"
  "draw/drawable-tree-h.gc"
  "draw/drawable-actor-h.gc"
  "level/region-h.gc"
  "ai/traffic-h.gc"
  "game/task/game-task-h.gc"
  "game/task/task-control-h.gc"
  "gfx/generic/generic-h.gc"
  "gfx/sky/sky-h.gc"
  "gfx/ocean/ocean-h.gc"
  "gfx/ocean/ocean-trans-tables.gc"
  "gfx/ocean/ocean-tables.gc"
  "gfx/ocean/ocean-frames.gc"
  "gfx/mood/time-of-day-h.gc"
  "data/art-h.gc"
  "gfx/generic/generic-vu1-h.gc"
  "gfx/merc/merc-h.gc"
  "gfx/merc/generic-merc-h.gc"
  "gfx/tie/generic-tie-h.gc"
  "gfx/generic/generic-work-h.gc"
  "gfx/foreground/shadow-cpu-h.gc"
  "gfx/foreground/shadow-vu1-h.gc"
  "ps2/memcard-h.gc"
  "game/game-info-h.gc"
  "ui/gui-h.gc"
  "ambient/ambient-h.gc"
  "sound/speech-h.gc"
  "gfx/background/wind-h.gc"
  "gfx/background/prototype-h.gc"
  "anim/joint-h.gc"
  "gfx/foreground/bones-h.gc"
  "gfx/foreground/foreground-h.gc"
  "engine/engines.gc"
  "gfx/lightning-h.gc"
  "entity/res-h.gc"
  "entity/res.gc"
  "gfx/lights.gc"
  "physics/dynamics-h.gc"
  "target/surface-h.gc"
  "collide/pat-h.gc"
  "game/fact-h.gc"
  "anim/aligner-h.gc"
  "game/penetrate-h.gc"
  "game/game-h.gc"
  "util/script-h.gc"
  "scene/scene-h.gc"
  "util/sync-info-h.gc"
  "camera/pov-camera-h.gc"
  "util/smush-control-h.gc"
  "debug/debug-h.gc"
  "anim/joint-mod-h.gc"
  "collide/collide-func-h.gc"
  "collide/collide-mesh-h.gc"
  "collide/collide-shape-h.gc"
  "common_objs/generic-obs-h.gc"
  "physics/trajectory-h.gc"
  "collide/collide-target-h.gc"
  "collide/collide-touch-h.gc"
  "collide/collide-edge-grab-h.gc"
  "process-drawable/process-drawable-h.gc"
  "process-drawable/process-focusable.gc"
  "process-drawable/process-taskable-h.gc"
  "process-drawable/focus.gc"
  "game/effect-control-h.gc"
  "collide/collide-frag-h.gc"
  "spatial-hash/collide-hash-h.gc"
  "physics/chain-physics-h.gc"
  "common_objs/projectile-h.gc"
  "collide/find-nearest-h.gc"
  "target/target-h.gc"
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
  "gfx/sprite/simple-sprite-h.gc"
  "gfx/foreground/eye-h.gc"
  "gfx/sprite/particles/sparticle-launcher-h.gc"
  "gfx/sprite/particles/sparticle-h.gc"
  "entity/actor-link-h.gc"
  "camera/camera-h.gc"
  "camera/cam-debug-h.gc"
  "camera/cam-interface-h.gc"
  "camera/cam-update-h.gc"
  "ui/hud-h.gc"
  "ui/progress/progress-h.gc"
  "ps2/rpc-h.gc"
  "geometry/path-h.gc"
  "nav/nav-mesh-h.gc"
  "nav/nav-control-h.gc"
  "spatial-hash/spatial-hash-h.gc"
  "spatial-hash/actor-hash-h.gc"
  "load/load-dgo.gc"
  "load/ramdisk.gc"
  "sound/gsound.gc"
  "math/transformq.gc"
  "collide/collide-func.gc"
  "anim/joint.gc"
  "anim/joint-mod.gc"
  "physics/chain-physics.gc"
  "geometry/cylinder.gc"
  "gfx/background/wind-work.gc"
  "gfx/background/wind.gc"
  "level/bsp.gc"
  "gfx/background/subdivide.gc"
  "gfx/sprite/sprite.gc"
  "gfx/sprite/sprite-distort.gc"
  "gfx/sprite/sprite-glow.gc"
  "debug/debug-sphere.gc"
  "debug/debug.gc"
  "debug/history.gc"
  "gfx/merc/merc-vu1.gc"
  "gfx/merc/emerc-vu1.gc"
  "gfx/merc/merc-blend-shape.gc"
  "gfx/merc/merc.gc"
  "gfx/merc/emerc.gc"
  "gfx/foreground/ripple.gc"
  "gfx/foreground/bones.gc"
  "gfx/foreground/debug-foreground.gc"
  "gfx/foreground/foreground.gc"
  "gfx/generic/generic-vu0.gc"
  "gfx/generic/generic-vu1.gc"
  "gfx/generic/generic-effect.gc"
  "gfx/generic/generic-merc.gc"
  "gfx/generic/generic-tie.gc"
  "gfx/foreground/shadow-cpu.gc"
  "gfx/foreground/shadow-vu1.gc"
  "gfx/warp.gc"
  "gfx/texture/texture-anim.gc"
  "gfx/texture/texture-anim-funcs.gc"
  "gfx/texture/texture-anim-tables.gc"
  "gfx/blit-displays.gc"
  "data/font-data.gc"
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
  "gfx/tie/etie-vu1.gc"
  "gfx/tie/etie-near-vu1.gc"
  "gfx/tie/tie-near.gc"
  "gfx/tie/tie-work.gc"
  "gfx/tie/tie-methods.gc"
  "util/sync-info.gc"
  "physics/trajectory.gc"
  "gfx/sprite/particles/sparticle-launcher.gc"
  "gfx/sprite/particles/sparticle.gc"
  "entity/entity-table.gc"
  "load/loader.gc"
  "game/game-info.gc"
  "game/task/game-task.gc"
  "game/game-save.gc"
  "game/settings.gc"
  "gfx/mood/mood-tables.gc"
  "gfx/mood/mood-tables2.gc"
  "gfx/mood/mood.gc"
  "gfx/mood/mood-funcs.gc"
  "gfx/mood/mood-funcs2.gc"
  "gfx/mood/weather-part.gc"
  "gfx/mood/time-of-day.gc"
  "gfx/sky/sky-data.gc"
  "gfx/sky/sky-tng.gc"
  "load/load-state.gc"
  "level/level-info.gc"
  "level/level.gc"
  "ui/text.gc"
  "spatial-hash/collide-hash.gc"
  "collide/collide-probe.gc"
  "collide/collide-frag.gc"
  "collide/collide-mesh.gc"
  "collide/collide-touch.gc"
  "collide/collide-edge-grab.gc"
  "collide/collide-shape.gc"
  "collide/collide-shape-rider.gc"
  "collide/collide.gc"
  ;; "collide/collide-planes.gc"
  "spatial-hash/spatial-hash.gc"
  "spatial-hash/actor-hash.gc"
  "gfx/merc/merc-death.gc"
  "common_objs/water-flow.gc"
  "common_objs/water-h.gc"
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
  "process-drawable/process-drawable.gc"
  "ambient/ambient.gc"
  "sound/speech.gc"
  "level/region.gc"
  "anim/fma-sphere.gc"
  "util/script.gc"
  "common_objs/generic-obs.gc"
  "gfx/lightning.gc"
  "target/mech_suit/carry-h.gc"
  "game/pilot-h.gc"
  "target/gun/gun-h.gc"
  "target/board/board-h.gc"
  "target/darkjak-h.gc"
  "target/target-util.gc"
  "target/target-part.gc"
  "target/gun/gun-part.gc"
  "target/collide-reaction-target.gc"
  "target/logic-target.gc"
  "target/sidekick.gc"
  "game/effect-control.gc"
  "common_objs/voicebox.gc"
  "common_objs/collectables-part.gc"
  "debug/debug-part.gc"
  "collide/find-nearest.gc"
  "game/task/task-arrow.gc"
  "common_objs/projectile.gc"
  "target/target-handler.gc"
  "target/target-anim.gc"
  "target/target.gc"
  "target/target2.gc"
  "target/target-swim.gc"
  "target/target-carry.gc"
  "target/target-darkjak.gc"
  "target/target-death.gc"
  "target/target-gun.gc"
  "target/gun/gun-util.gc"
  "target/gun/gun-blue-shot.gc"
  "target/gun/gun-yellow-shot.gc"
  "target/gun/gun-red-shot.gc"
  "target/gun/gun-dark-shot.gc"
  "target/gun/gun-states.gc"
  "target/board/board-util.gc"
  "target/board/target-board.gc"
  "target/board/board-part.gc"
  "target/board/board-states.gc"
  "target/mech_suit/mech-h.gc"
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
  "collide/collide-debug.gc"
  "entity/relocate.gc"
  "debug/memory-usage.gc"
  "entity/entity.gc"
  "geometry/path.gc"
  "geometry/vol.gc"
  "nav/nav-mesh.gc"
  "nav/nav-control.gc"
  "anim/aligner.gc"
  "common_objs/water.gc"
  "common_objs/collectables.gc"
  "game/task/task-control.gc"
  "scene/scene.gc"
  "camera/pov-camera.gc"
  "common_objs/powerups.gc"
  "common_objs/crates.gc"
  "ui/hud.gc"
  "ui/hud-classes.gc"
  "ui/progress/progress-static.gc"
  "ui/progress/progress.gc"
  "ui/progress/progress-draw.gc"
  "gfx/ocean/ocean.gc"
  "gfx/ocean/ocean-vu0.gc"
  "gfx/ocean/ocean-texture.gc"
  "gfx/ocean/ocean-mid.gc"
  "gfx/ocean/ocean-transition.gc"
  "gfx/ocean/ocean-near.gc"
  "ui/minimap.gc"
  "ui/bigmap-data.gc"
  "ui/bigmap.gc"
  "gfx/foreground/eye.gc"
  "util/glist-h.gc"
  "util/glist.gc"
  "debug/anim-tester.gc"
  "debug/viewer.gc"
  "debug/part-tester.gc"
  "debug/editable-h.gc"
  "debug/editable.gc"
  "debug/editable-player.gc"
  "debug/nav/mysql-nav-graph.gc"
  "debug/nav/nav-graph-editor.gc"
  "debug/sampler.gc"
  "debug/default-menu.gc"
  "gfx/texture/texture-upload.gc"
  "gfx/texture/texture-finish.gc"
  "collide/los-control-h.gc"
  "common_objs/water-anim.gc"
  "common_objs/blocking-plane.gc"
  "game/idle-control.gc"
  "common_objs/dark-eco-pool.gc"
  "ai/enemy-h.gc"
  "nav/nav-enemy-h.gc"
  "physics/rigid-body-h.gc"
  "ai/enemy.gc"
  "nav/nav-enemy.gc"
  "common_objs/base-plat.gc"
  "common_objs/plat.gc"
  "common_objs/basebutton.gc"
  "common_objs/conveyor.gc"
  "common_objs/elevator.gc"
  "physics/rigid-body.gc"
  "physics/rigid-body-queue.gc"
  "common_objs/rigid-body-plat.gc"
  "anim/joint-exploder.gc"
  "process-drawable/simple-focus.gc"
  "process-drawable/simple-nav-sphere.gc"
  "process-drawable/process-taskable.gc"
  "collide/los-control.gc"
  )

(goal-src-sequence
  "levels/common/"
  :deps
  ("$OUT/obj/los-control-h.o")

  "airlock.gc"
  "enemy/bouncer.gc"
  "scene-actor.gc"
  "scene-looper.gc"
  "warp-gate.gc"
  "guard-projectile.gc"
  "metalhead-projectile.gc"
  "grunt.gc"
  "flitter.gc"
  "battle.gc"
  "elec-gate.gc"
  "cty-guard-turret-button.gc"
  "enemy/guards/crimson-guard-level.gc"
  "enemy/guards/guard-conversation.gc"
  "enemy/guards/transport-level.gc"

  )

(cgo "ENGINE.CGO" "engine.gd")

(cgo "GAME.CGO" "game.gd")

(defstep :in "$DECOMP/textures/tpage-dir.txt"
  :tool 'tpage-dir
  :out '("$OUT/obj/dir-tpages.go")
  )

(copy-textures 11 31 1804 12 917 918 1106 1141 1658 2841 2932 3076)

(copy-gos
  "collectables-ag"
  "ctywide-arrow-ag"
  "crate-ag"
  "talk-box-ag"
  "scenecamera-ag"
  "eco-canister-ag"
  "hud-ring-ag"
  "jakb-ag"
  "daxter-ag"
  "board-ag"
  "gun-ag"
  "jak-gun+0-ag"
  "jak-board+0-ag"
  "jak-dark+0-ag"
  "jak-swim+0-ag"
  "blocking-plane-ag"
  )

(copy-sbk-files "ASHTAN1"
"ASHTAN2"
"ATOLL1"
"ATOLL2"
"ATOLL3"
"ATOLL4"
"BBUSH1"
"BOARD"
"BOMBBOT1"
"CASBOSS1"
"CASBOSS2"
"CASBOSS3"
"CASTLE1"
"CASTLE2"
"CASTLE3"
"COMMON"
"COMMONJ"
"CONSITE1"
"CONSITE2"
"CONSITE3"
"CTYFARM1"
"CTYWIDE1"
"CTYWIDE2"
"CTYWIDE3"
"CTYWIDE4"
"CTYWIDE5"
"DEMO1"
"DIG1"
"DIG2"
"DIG3"
"DIG4"
"DIG5"
"DIG6"
"DIG7"
"DIG8"
"DRILL1"
"DRILL2"
"DRILL3"
"DRILL4"
"DRILL5"
"DRILL6"
"DRILL7"
"DRILL8"
"EMPTY0"
"EMPTY1"
"EMPTY2"
"ERLCHAL1"
"ESCKID1"
"FORDUMP1"
"FORDUMP2"
"FOREST1"
"FOREST2"
"FOREST3"
"FOREST4"
"FOREST5"
"FOREXIT1"
"FOREXIT2"
"FORRESC1"
"FORRESC2"
"GUN"
"GUNGAME1"
"HELLDOG1"
"HIDEOUT1"
"HIPHOG1"
"INTRO1"
"INTRO2"
"INTRO3"
"MECH"
"MECHWAT"
"MEETBRT1"
"MENU1"
"MOUNT1"
"MOUNT2"
"MOUNT3"
"NEST1"
"NEST2"
"NEST3"
"NEST4"
"NEST5"
"NEST6"
"ONIN1"
"ONIN2"
"ORACLE1"
"OUTRO1"
"PALCAB1"
"PALCAB2"
"PALCAB3"
"PALENT1"
"PALENT2"
"PALENT3"
"PALROOF1"
"PALROOF2"
"PALROOF3"
"PORTRUN1"
"PROTECT1"
"RUINS1"
"RUINS2"
"RUINS3"
"SACK1"
"SEWER1"
"SEWER2"
"SEWER3"
"SEWER4"
"SEWER5"
"SEWER6"
"SKATE1"
"STADIUM1"
"STRIP1"
"STRIP2"
"STRIP3"
"TOMB1"
"TOMB2"
"TOMB3"
"TOMB4"
"TOMB5"
"TOMB6"
"TOMB7"
"TOMB8"
"TOMB9"
"UNDER1"
"UNDER2"
"UNDER3"
"UNDER4"
"UNDER5"
"VINROOM1"
)

(copy-mus-files "ATOLL"
"BATTLE"
"CITY1"
"CREDITS"
"DANGER"
"DANGER1"
"DANGER2"
"DANGER3"
"DANGER4"
"DANGER6"
"DANGER7"
"DANGER9"
"DANGER10"
"DANGER11"
"DIG"
"FOREST"
"FORTRESS"
"MOUNTAIN"
"PALCAB"
"RACE"
"RUINS"
"SEWER"
"STRIP"
"TOMB"
"TWEAKVAL")

;; intro cutscenes
(copy-strs "INSHUT" "INVORTEX" "INCSQUAR" "INPRISON")

;; jak ambient
(copy-strs "JAA1" "JAA2" "JAA3" "JAA4" "JAA5" "JAA6")

(copy-vag-files "ENG")

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
;; COMMON CITY STUFF
;;;;;;;;;;;;;;;;;;;;;

(copy-gos
  "fort-entry-gate-ag"
  "crimson-guard-ag"
  "com-airlock-outer-ag"
  )

;;;;;;;;;;;;;;;;;;;;;
;; COMMON
;;;;;;;;;;;;;;;;;;;;;

(copy-gos
  "daxter-highres-ag")

;;;;;;;;;;;;;;;;;;;;;
;; PRISON
;;;;;;;;;;;;;;;;;;;;;

(cgo "PRI.DGO" "pri.gd")

(goal-src-sequence
 "levels/"
 :deps ("$OUT/obj/los-control.o")
 "fortress/prison/intro-texture.gc"
 "fortress/prison/prison-part.gc"
 "fortress/prison/prison-obs.gc"
 )

(copy-textures 1578 1950 1579 2647)

(copy-gos
  "prsn-torture-ag"
  "prsn-chair-shackle-ag"
  "prsn-hang-cell-ag"
  "warp-gate-b-ag"
  "prsn-vent-fan-ag"
  "prsn-cell-door-ag"
  "prison-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; CITY WIDE
;;;;;;;;;;;;;;;;;;;;;

(cgo "CWI.DGO" "cwi.gd")

(goal-src-sequence
  "levels/"
  :deps ("$OUT/obj/los-control.o")
  "city/common/nav-graph-h.gc"
  "city/common/traffic-engine-h.gc"
  "city/common/vehicle-h.gc"
  "city/common/citizen-h.gc"
  "city/common/height-map-h.gc"
  "city/common/ctywide-obs-h.gc"
  "city/common/height-map.gc"
  "city/common/traffic-height-map.gc"
  "city/common/nav-graph.gc"
  "city/common/vehicle-rider.gc"
  "city/common/vehicle-control.gc"
  "city/common/vehicle-part.gc"
  "city/common/vehicle-effects.gc"
  "city/common/vehicle.gc"
  "city/common/vehicle-util.gc"
  "city/common/vehicle-physics.gc"
  "city/common/vehicle-states.gc"
  "city/common/vehicle-guard.gc"
  "city/common/transport.gc"
  "city/common/bike.gc"
  "city/common/car.gc"
  "city/common/test-bike.gc"
  "city/common/test-car.gc"
  "city/common/citizen.gc"
  "city/common/civilian.gc"
  "city/common/guard.gc"
  "city/common/citizen-norm.gc"
  "city/common/citizen-fat.gc"
  "city/common/citizen-chick.gc"
  "city/common/citizen-enemy.gc"
  "city/common/metalhead-predator.gc"
  "city/common/metalhead-grunt.gc"
  "city/common/metalhead-flitter.gc"
  "city/common/traffic-engine.gc"
  "city/common/trail-graph.gc"
  "city/common/trail.gc"
  "city/common/traffic-manager.gc"
  "city/common/ctywide-texture.gc"
  "city/common/ctywide-part.gc"
  "city/common/ctywide-obs.gc"
  "city/common/ctywide-tasks.gc"
  "city/common/ctywide-scenes.gc"
  "city/common/ctywide-speech.gc"
  "city/common/ctyport-obs.gc"
  "city/common/target-pilot.gc"
  "city/common/pilot-states.gc"
  "city/common/searchlight.gc"
  )

(copy-textures 1264 1266 1265 1674 1118 1657)

(copy-gos
  "jak-pilot+0-ag"
  "baron-statue-ag"
  "cty-guard-turret-ag"
  "vehicle-explosion-ag"
  "barons-ship-lores-ag"
  "propa-ag"
  "vehicle-turret-ag"
  "lurker-pipe-lid-ag"
  "searchlight-ag"
  "burning-bush-ag"
  "stadium-barrier-ag"
  "security-wall-ag"
  "ctywide-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; L CITY WIDE A
;;;;;;;;;;;;;;;;;;;;;

(cgo "LWIDEA.DGO" "lwidea.gd")

(copy-textures 2929 2930)

(copy-gos
  "citizen-fat-ag"
  "citizen-norm-ag"
  "citizen-chick-ag"
  "hellcat-ag"
  "carc-ag"
  "cara-ag"
  "carb-ag"
  "citizen-norm-rider-ag"
  "crimson-bike-ag"
  "bikec-ag"
  "bikeb-ag"
  "bikea-ag"
  "lwidea")

;;;;;;;;;;;;;;;;;;;;;
;; CITY SLUM A
;;;;;;;;;;;;;;;;;;;;;

(cgo "CTA.DGO" "cta.gd")

(goal-src-sequence
  "levels/city/slums/"
  :deps ("$OUT/obj/los-control.o")
  "ctysluma-part.gc"
  "neon-baron-part.gc"
  )

(copy-textures 974 973 1680 1021 1646)

(copy-gos
  "cty-fruit-stand-ag"
  "hide-door-a-ag"
  "ctysluma-vis"
)

;;;;;;;;;;;;;;;;;;;;;
;; VILLAGE 1
;;;;;;;;;;;;;;;;;;;;;

(cgo "VI1.DGO" "vi1.gd")
(copy-textures 3034 3037 3035 3038 3036 2761 3516)

(copy-gos
  "darkjak-highres-ag"
  "metalkor-torso-ag"
  "rift-ring-ag"
  "vil-break-support-ag"
  "intro-flamer-ag"
  "rift-rider-donut-ag"
  "vil-windmill-sail-ag"
  "vil-windspinner-ag"
  "vil-sagesail-ag"
  "particleman-ag"
  "village1-vis"
)

;; The below project code was auto-generated via:
;; - python .\scripts\gsrc\skeleton_creation\generate_dgo_project_code.py --game jak2 --dep los-control
;; all dgos already manually created aboved were skipped, look at the script's code to change this behaviour

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; ART
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "ART.DGO" "art.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "engine/gfx/texture/texture-upload.gc"
;;   "engine/gfx/texture/texture-finish.gc"
;;   )

;; (copy-textures tpages 11 31 1804 12 917 918 1106 1141 1658 2841 2932 3076)

;; (copy-gos
;;   "collectables-ag"
;;   "ctywide-arrow-ag"
;;   "crate-ag"
;;   "talk-box-ag"
;;   "scenecamera-ag"
;;   "eco-canister-ag"
;;   "hud-ring-ag"
;;   "jakb-ag"
;;   "daxter-ag"
;;   "board-ag"
;;   "gun-ag"
;;   "jak-gun+0-ag"
;;   "jak-board+0-ag"
;;   "jak-dark+0-ag"
;;   "jak-swim+0-ag"
;;   "blocking-plane-ag"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; ATE
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "ATE.DGO" "ate.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/common/ai/ai-task-h.gc"
  "levels/common/ai/bot-h.gc"
  "levels/common/ai/bot.gc"
  "levels/common/ai/bot-states.gc"
  "characters/sig/sig-h.gc"
  "characters/sig/sig-task.gc"
  "characters/sig/sig-shot.gc"
  "characters/sig/sig-plasma.gc"
  "characters/sig/sig.gc"
  "characters/sig/sig-states.gc"
  "characters/ashelin/ash-h.gc"
  "characters/ashelin/ash-task.gc"
  "characters/ashelin/ash-shot.gc"
  "characters/ashelin/ash.gc"
  "characters/ashelin/ash-states.gc"
  "levels/common/enemy/spyder.gc"
  "levels/atoll/sig0-course.gc"
  "levels/atoll/ash1-course.gc"
  "levels/atoll/atoll-tank.gc"
  "levels/atoll/atoll-scenes.gc"
  "levels/atoll/sniper.gc"
  "levels/common/enemy/amphibian/amphibian.gc"
  "levels/atoll/juicer.gc"
  "levels/common/enemy/metalhead_brown/metalmonk.gc"
  )

(copy-textures 1606 1607 2461 3237 3412)

(copy-gos
  "sig-ag"
  "jak-highres-ag"
  "atoll-tank-ag"
  "sig-highres-ag"
  "juicer-ag"
  "ashelin-highres-ag"
  "atoll-hellcat-ag"
  "ashelin-ag"
  "metalmonk-ag"
  "amphibian-ag"
  "spyder-ag"
  "com-airlock-inner-ag"
  "palmpilot-ag"
  "atollext-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; ATO
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "ATO.DGO" "ato.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/common/entities/gun-buoy.gc"
  "levels/atoll/atoll-part.gc"
  "levels/atoll/atoll-obs.gc"
  "levels/common/enemy/fodder/fodder.gc"
  )

(copy-textures 856 858 857 855 1083)

(copy-gos
  "fodder-ag"
  "liftcat-ag"
  "atoll-windmill-ag"
  "turbine-ag"
  "slider-ag"
  "gun-buoy-ag"
  "atoll-hatch-ag"
  "atoll-valve-ag"
  "piston-ag"
  "atollrotpipe-ag"
  "atoll-mar-symbol-ag"
  "atoll-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CAB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CAB.DGO" "cab.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/castle/boss/casboss-texture.gc"
  "levels/castle/boss/castle-baron.gc"
  "levels/castle/boss/castle-scenes.gc"
  "levels/castle/boss/casboss-part.gc"
  )

(copy-textures 2996 2997 3435 3458 3429)

(copy-gos
  "krew-highres-ag"
  "krew-lowres-ag"
  "cboss-tractor-ag"
  "krew-clone-ag"
  "cboss-bomb-ag"
  "cboss-elevator-ag"
  "casboss-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CAP
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CAP.DGO" "cap.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/castle/pad/caspad-obs.gc"
  "levels/castle/pad/caspad-part.gc"
  "levels/castle/pad/castle-tasks.gc"
  )

(copy-textures 2537 2539 2538 3498)

(copy-gos
  "air-train-ag"
  "cpad-elevator-ag"
  "caspad-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CAS
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "CAS.DGO" "cas.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;
;;   "levels/landing_pad/roboguard-level.gc"
;;   "levels/landing_pad/castle-texture.gc"
;;   "levels/landing_pad/castle-obs.gc"
;;   "levels/landing_pad/castle-part.gc"
;;   )

;; (copy-textures 2865 2867 2866 3124 2970)

;; (copy-gos
;;   "roboguard-ag"
;;   "crimson-guard-ag"
;;   "cas-rot-bridge-ag"
;;   "cas-electric-fence-ag"
;;   "cas-rot-blade-ag"
;;   "switch-ag"
;;   "cas-conveyor-switch-ag"
;;   "cas-robot-door-ag"
;;   "cas-trapdoor-ag"
;;   "cas-chain-plat-ag"
;;   "cas-elevator-ag"
;;   "cas-button-ag"
;;   "cas-flag-a-ag"
;;   "cas-flag-b-ag"
;;   "castle-vis"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CASCITY
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "CASCITY.DGO" "cascity.gd")

;; (copy-textures 3221 3222 3425)

;; (copy-gos
;;   "searchlight-ag"
;;   "pal-windmill-ag"
;;   "cascity"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CASEXT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "CASEXT.DGO" "casext.gd")

;; (copy-textures 2597 3424 3315)

;; (copy-gos
;;   "daxter-highres-ag"
;;   "jak-highres-ag"
;;   "ashelin-highres-ag"
;;   "hellcat-ag"
;;   "heart-of-mar-ag"
;;   "gun-upgrade-a-ag"
;;   "particleman-ag"
;;   "casext"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CFA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CFA.DGO" "cfa.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/farm/ctyfarm-obs.gc"
  "levels/city/farm/ctyfarma-part.gc"
  ;; "levels/city/farm/ctyfarmb-part.gc"
  ;; "levels/city/farm/yakow.gc"
  )

(copy-textures 2444 2445 2645 2644 2744 2791 2441)

(copy-gos
  "farm-chilirots-ag"
  "farm-cabbage-ag"
  "farm-marrow-ag"
  "farm-beetree-ag"
  "farm-small-cabbage-ag"
  "farm-sprinkler-barrels-ag"
  "ctyfarma-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CFB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CFB.DGO" "cfb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/city/farm/ctyfarm-obs.gc"
  ;; "levels/city/farm/ctyfarma-part.gc"
  "levels/city/farm/ctyfarmb-part.gc"
  "levels/city/farm/yakow.gc"
  )

(copy-textures 3005 3009 3007 3008 3006 3004 2442)

(copy-gos
  "yakow-ag"
  ;; "farm-chilirots-ag"
  ;; "farm-cabbage-ag"
  ;; "farm-marrow-ag"
  ;; "farm-beetree-ag"
  ;; "farm-small-cabbage-ag"
  ;; "farm-sprinkler-barrels-ag"
  "ctyfarmb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CGA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CGA.DGO" "cga.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/generic/ctygena-part.gc"
  "levels/city/generic/neon-praxis-part.gc"
  )

(copy-textures 1137 1138 1117 1637)

(copy-gos
  "ctyn-lamp-ag"
  "ctygena-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CGB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CGB.DGO" "cgb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/generic/ctygenb-part.gc"
  )

(copy-textures 955 957 956 958 1019 1638)

(copy-gos
  "ctygenb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CGC
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CGC.DGO" "cgc.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/generic/ctygenc-part.gc"
  )

(copy-textures 1605 2285 1612 1639)

(copy-gos
  ;; "ctyn-lamp-ag"
  "ctygenc-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CIA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CIA.DGO" "cia.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/industrial/ctyinda-part.gc"
  "levels/city/industrial/ctyinda-obs.gc"
  )

(copy-textures 1561 1562 1361 1640)

(copy-gos
  "door-ctyinda-ag"
  "ctyinda-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CITY INDUSTRIAL B
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CIB.DGO" "cib.gd")

(goal-src-sequence
  "levels/city/industrial/"
  :deps ("$OUT/obj/los-control.o")
  "ctyindb-part.gc"
  "ctyindb-obs.gc"
  )

(copy-textures 1565 1577 1601 1614 1642)

(copy-gos
  "ctyindb-vis"
)

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CMA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CMA.DGO" "cma.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/market/ctymark-obs.gc"
  "levels/city/market/ctymarka-part.gc"
  ;; "levels/city/market/ctymarkb-part.gc"
  )

(copy-textures 2739 2743 2740 2741 2742 1615 1643)

(copy-gos
  ;; "market-sack-b-ag"
  "market-sack-a-ag"
  "market-crate-ag"
  ;; "market-basket-b-ag"
  "market-basket-a-ag"
  ;; "cty-fruit-stand-ag"
  "ctymarka-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CMB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CMB.DGO" "cmb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/city/market/ctymark-obs.gc"
  ;; "levels/city/market/ctymarka-part.gc"
  "levels/city/market/ctymarkb-part.gc"
  )

(copy-textures 2734 2738 2735 2737 2736 1616 1644)

(copy-gos
  "market-sack-b-ag"
  ;; "market-sack-a-ag"
  ;; "market-crate-ag"
  "market-basket-b-ag"
  ;; "market-basket-a-ag"
  ;; "cty-fruit-stand-ag"
  "ctymarkb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; COA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "COA.DGO" "coa.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/consite/consite-texture.gc"
  "levels/consite/consite-part.gc"
  "levels/consite/consite-scenes.gc"
  "levels/consite/consite-obs.gc"
  )

(copy-textures 3030 3031 3029 3141)

(copy-gos
  "jak-pole+0-ag"
  "baron-consite+0-ag"
  "consite-break-scaffold-a-ag"
  "consite-break-scaffold-ag"
  "baron-highres-ag"
  "crimson-guard-highres-ag"
  "consite-bomb-elevator-ag"
  "crimson-guard-lowres-ag"
  "consite-silo-doors-ag"
  "precursor-stone-ag"
  "bomb-trigger-ag"
  "consite-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; COB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "COB.DGO" "cob.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/consite/consiteb-part.gc"
  )

(copy-textures 3032 3033 3142 3180 3239)

(copy-gos
  "kor-transform-ag"
  "kor-break-ag"
  "kor-break-ray-ag"
  "consiteb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CPA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CPA.DGO" "cpa.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/palace/ctypal-part.gc"
  "levels/city/palace/ctypal-obs.gc"
  )

(copy-textures 2526 2527 3423 2528 1636)

(copy-gos
  "ctypal-baron-statue-broken-ag"
  ;; "com-airlock-inner-ag"
  "water-anim-ctypal-ag"
  "palace-door-ag"
  "ctypal-broke-wall-ag"
  "ctypal-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CPO
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "CPO.DGO" "cpo.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/city/port/ctyport-part.gc"
;;   )

;; (copy-textures 1557 1560 1558 1325 1645)

;; (copy-gos
;;   "barge-ag"
;;   "mecha-daxter-ag"
;;   "farthy-ag"
;;   "air-train-ag"
;;   "hip-door-a-ag"
;;   "ctyport-vis"
;;   )

;;;;;;;;;;;;;;;;;;;;;
;; CTB
;;;;;;;;;;;;;;;;;;;;;

(cgo "CTB.DGO" "ctb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/slums/ctyslumb-part.gc"
  )

(copy-textures 977 1308 978 1022 1647)

(copy-gos
  ;; "com-airlock-inner-ag"
  "ctyslumb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CTC
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CTC.DGO" "ctc.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/slums/ctyslumc-part.gc"
  )

(copy-textures 1268 1303 1269 1023 1648)

(copy-gos
  "door-ag"
  "ctyslumc-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CTYASHA
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "CTYASHA.DGO" "ctyasha.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/ai/ai-task-h.gc"
;;   "levels/common/ai/bot-h.gc"
;;   "levels/common/ai/bot.gc"
;;   "levels/common/ai/bot-states.gc"
;;   "characters/ashelin/ash-h.gc"
;;   "characters/ashelin/ash-task.gc"
;;   "characters/ashelin/ash-shot.gc"
;;   "characters/ashelin/ash.gc"
;;   "characters/ashelin/ash-states.gc"
;;   "levels/atoll/juicer.gc"
;;   "levels/city/market/east/ashelin/ctyasha-obs.gc"
;;   "levels/city/market/east/ashelin/ash4-course.gc"
;;   )

;; (copy-textures 1496 1409 3240 3416)

;; (copy-gos
;;   "daxter-highres-ag"
;;   "grunt-ag"
;;   "jak-highres-ag"
;;   "juicer-ag"
;;   "ashelin-highres-ag"
;;   "ashelin-ag"
;;   "crimson-guard-highres-ag"
;;   "tanker-ag"
;;   "crimson-guard-lowres-ag"
;;   "tanker-roof-break-ag"
;;   "tanker-crash-ag"
;;   "tanker-barrels-ag"
;;   "tanker-container-ag"
;;   "tanker-one-barrel-ag"
;;   "ctyasha"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; CTYKORA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "CTYKORA.DGO" "ctykora.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/common/ai/ai-task-h.gc"
  ;; "levels/common/ai/bot-h.gc"
  ;; "levels/common/ai/bot.gc"
  ;; "levels/common/ai/bot-states.gc"
  ;; "levels/undefined/hal-h.gc"
  ;; "levels/undefined/hal-task.gc"
  ;; "levels/undefined/hal.gc"
  "levels/city/slums/kor/kid-h.gc"
  "levels/city/slums/kor/kor-h.gc"
  "levels/city/slums/kor/hal3-course.gc"
  "levels/city/slums/kor/kid-task.gc"
  "levels/city/slums/kor/kid.gc"
  "levels/city/slums/kor/kid-states.gc"
  "levels/city/slums/kor/kor-task.gc"
  "levels/city/slums/kor/kor.gc"
  "levels/city/slums/kor/kor-states.gc"
  "levels/city/slums/kor/kid3-course.gc"
  "levels/city/slums/kor/kor3-course.gc"
  )

(copy-textures 1451 2727 3224)

(copy-gos
  "jak-dark-on+0-ag"
  ;; "daxter-highres-ag"
  ;; "jak-highres-ag"
  ;; "kor-highres-ag"
  ;; "atoll-hellcat-ag"
  "kor-ag"
  ;; "kid-highres-ag"
  ;; "crimson-guard-highres-ag"
  ;; "kid-ag"
  ;; "crimson-guard-lowres-ag"
  ;; "transport-ag"
  "ctykora"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; D3A
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "D3A.DGO" "d3a.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/common/enemy/hover/hover-formation-h.gc"
  "levels/common/enemy/hover/hover-nav-control-h.gc"
  "levels/common/enemy/hover/hover-enemy-h.gc"
  "levels/common/enemy/hover/hover-nav-network.gc"
  "levels/common/enemy/hover/hover-nav-control.gc"
  "levels/common/enemy/hover/hover-enemy.gc"
  "levels/common/enemy/hover/hover-enemy-battle.gc"
  "levels/common/enemy/hover/hover-formation.gc"
  "levels/common/enemy/hover/hover-nav-edit.gc"
  "levels/common/enemy/hover/wasp-part.gc"
  "levels/common/enemy/hover/wasp.gc"
  "levels/common/enemy/hover/crimson-guard-hover.gc"
  "levels/common/enemy/hover/flamer.gc"
  "levels/dig/dig-digger.gc"
  "levels/dig/dig-obs.gc"
  "levels/dig/dig2-obs.gc"
  "levels/dig/dig3-obs.gc"
  "levels/common/enemy/baby_spider/tomb-baby-spider.gc"
  "levels/common/enemy/metalhead_slinger/grenadier.gc"
  "levels/dig/dig-texture.gc"
  "levels/dig/dig-scenes.gc"
  "levels/dig/dig-part.gc"
  )

(copy-textures 2290 2292 2291 2782 3361)

(copy-gos
  "grenadier-ag"
  "flitter-ag"
  "dig-balloon-lurker-ag"
  "dig-spikey-sphere-ag"
  "dig-totem-ag"
  "dig-wheel-step-ag"
  "warp-gate-ag"
  "dig-spikey-step-ag"
  "dig-stomp-block-ag"
  "dig-totem-fish-ag"
  "seal-of-mar-base-ag"
  "dig-tipping-rock-ag"
  "dig-sinking-plat-ag"
  "dig-log-ag"
  "dig-spikey-sphere-door-ag"
  "dig-button-ag"
  "dig3a-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; D3B
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "D3B.DGO" "d3b.gd")

(copy-textures 2406 2407 3548)

(copy-gos
  "dig3b-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; DEMO
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "DEMO.DGO" "demo.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/demo/demo-obs.gc"
;;   )

;; (copy-textures 1872 1876 1868)

;; (copy-gos
;;   "daxter-highres-ag"
;;   "jak-logo-ag"
;;   "jak-stand-ag"
;;   "demo"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; DG1
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "DG1.DGO" "dg1.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/dig/dig1-obs.gc"
  )

(copy-textures 2282 2284 2283 2419 2844)

(copy-gos
  "dig-digger-ag"
  "dig-bomb-crate-ag"
  "dig-conveyor-ag"
  "dig-clasp-ag"
  "dig-tether-ag"
  "dig-jump-pad-ag"
  "dig-breakable-door-ag"
  "dig-clasp-b-ag"
  "dig-bomb-crate-cylinder-ag"
  "dig1-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; DMI
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "DMI.DGO" "dmi.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/common/enemy/hover/hover-formation-h.gc"
  ;; "levels/common/enemy/hover/hover-nav-control-h.gc"
  ;; "levels/common/enemy/hover/hover-enemy-h.gc"
  ;; "levels/common/enemy/hover/hover-nav-network.gc"
  ;; "levels/common/enemy/hover/hover-nav-control.gc"
  ;; "levels/common/enemy/hover/hover-enemy.gc"
  ;; "levels/common/enemy/hover/hover-enemy-battle.gc"
  ;; "levels/common/enemy/hover/hover-formation.gc"
  ;; "levels/common/enemy/hover/hover-nav-edit.gc"
  ;; "levels/common/enemy/hover/wasp-part.gc"
  ;; "levels/common/enemy/hover/wasp.gc"
  ;; "levels/common/enemy/hover/crimson-guard-hover.gc"
  ;; "levels/common/enemy/hover/flamer.gc"
  "engine/target/target-turret-shot.gc"
  "engine/target/target-turret.gc"
  "levels/drill_platform/drill-turret.gc"
  "levels/drill_platform/drillmid-obs.gc"
  "levels/drill_platform/drill-part.gc"
  "levels/drill_platform/drill-part2.gc"
  "levels/drill_platform/drill-mech-master.gc"
  "levels/drill_platform/drill-obs.gc"
  "levels/drill_platform/drill-panel.gc"
  "levels/drill_platform/drill-obs2.gc"
  "levels/drill_platform/drill-spool.gc"
  "levels/drill_platform/drill-baron.gc"
  "levels/drill_platform/drill-scenes.gc"
  "levels/common/enemy/metalhead_bearer/centurion.gc"
  "levels/drill_platform/ginsu.gc"
  )

(copy-textures 2300 2302 2301 2493 2303 2565 2846)

(copy-gos
  ;; "jak-pole+0-ag"
  "jak-turret+0-ag"
  "mech-ag"
  ;; "warp-gate-ag"
  "drill-falling-door-ag"
  "drillmid-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; DRB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "DRB.DGO" "drb.gd")

;; (copy-textures 2298 2299 2492)

;; (copy-gos
;;   "drill-elevator-ag"
;;   "drill-flip-step-ag"
;;   "drill-elevator-base-ag"
;;   "drill-falling-door-ag"
;;   "drill-drop-plat-ag"
;;   "drill-switch-ag"
;;   "drillb-vis"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; DRI
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "DRI.DGO" "dri.gd")

(copy-textures 1204 1205 1239 1203 1206)

(copy-gos
  "centurion-ag"
  ;; "drill-turret-ext-ag"
  ;; "mech-ag"
  ;; "port-turret-ag"
  "ginsu-ag"
  "drill-elevator-ag"
  "drill-plat-falling-ag"
  "drill-bridge-shot-ag"
  ;; "warp-gate-ag"
  "drill-elevator-base-ag"
  ;; "drill-wall-ag"
  ;; "drill-metalhead-eggs-c-ag"
  ;; "drill-metalhead-eggs-b-ag"
  ;; "drill-metalhead-eggs-a-ag"
  ;; "drill-turret-int-ag"
  "drill-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; DRILLMTN
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "DRILLMTN.DGO" "drillmtn.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "engine/target/mech_suit/mech-part.gc"
  "engine/target/mech_suit/mech.gc"
  "engine/target/mech_suit/target-mech.gc"
  "engine/target/mech_suit/mech-states.gc"
  "engine/target/mech_suit/grunt-mech.gc"
  )

(copy-textures 1877 2535 2624)

(copy-gos
  "jak-mech+0-ag"
  "grunt-mech-anims+0-ag"
  "drill-top-break-ag"
  "drill-barons-ship-ag"
  "grunt-ag"
  "drill-control-panel-ag"
  "drill-crane-ag"
  "crimson-guard-hover-ag"
  "wasp-ag"
  "drill-turret-ext-ag"
  ;; "mech-ag"
  "drill-drill-ag"
  "port-turret-ag"
  "drill-flip-step-ag"
  "drill-lift-ag"
  "drill-elevator-doors-ag"
  "drill-wall-ag"
  "drill-metalhead-eggs-c-ag"
  "drill-metalhead-eggs-b-ag"
  "drill-metalhead-eggs-a-ag"
  "drill-switch-ag"
  "drill-turret-int-ag"
  "drill-moving-staircase-ag"
  "drillmtn"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FDA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FDA.DGO" "fda.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/fortress/ammo_dump/fordumpa-part.gc"
  "levels/fortress/ammo_dump/fordumpa-obs.gc"
  "levels/fortress/ammo_dump/fordumpa-texture.gc"
  "levels/fortress/ammo_dump/fort-robotank-turret.gc"
  "levels/fortress/ammo_dump/fort-robotank.gc"
  "levels/fortress/fort-turret.gc"
  )

(copy-textures 1589 1588 1590 2490 2909)

(copy-gos
  ;; "jak-pole+0-ag"
  "fort-fence-ag"
  "fort-robotank-ag"
  "fort-turret-ag"
  "fort-robotank-top-ag"
  "fort-elec-switch-ag"
  "fort-roboscreen-ag"
  "fort-robotank-sight-ag"
  "fordumpa-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FDB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FDB.DGO" "fdb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/fortress/ammo_dump/fordumpb-obs.gc"
  "levels/fortress/ammo_dump/fordumpb-part.gc"
  "levels/fortress/fortress-obs.gc"
  "levels/common/entities/fort-floor-spike.gc"
  )

(copy-textures 1592 1621 1593)

(copy-gos
  "fort-plat-orbit-ag"
  ;; "crimson-guard-ag"
  "fort-floor-spike-c-ag"
  "fort-floor-spike-b-ag"
  "fort-trap-door-ag"
  "fort-plat-shuttle-ag"
  "fort-conveyor-ag"
  "fordumpb-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; FEA
;;;;;;;;;;;;;;;;;;;;;

(cgo "FEA.DGO" "fea.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/fortress/exit/forexita-part.gc"
  "levels/fortress/exit/forexita-obs.gc"
  )

(copy-textures 1580 1583 1581)

(copy-gos
  ;; "jak-pole+0-ag"
  "fort-lift-plat-ag"
  "forexita-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FEB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FEB.DGO" "feb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "engine/target/target-tube.gc"
  ;; "levels/fortress/fortress-obs.gc"
  "levels/fortress/exit/forexitb-part.gc"
  )

(copy-textures 1584 1587 1585 1586 1712)

(copy-gos
  "jak-tube+0-ag"
  "water-anim-fortress-ag"
  ;; "fort-trap-door-ag"
  "forexitb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FOB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FOB.DGO" "fob.gd")

(copy-textures 2280 2574 2281 3262 3417)

(copy-gos
  "youngsamos-forestb+0-ag"
  ;; "daxter-highres-ag"
  "youngsamos-highres-ag"
  ;; "jak-highres-ag"
  ;; "crimson-guard-hover-ag"
  ;; "transport-ag"
  ;; "life-seed-ag"
  "forestb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FOR
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FOR.DGO" "for.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/forest/forest-part.gc"
  "levels/forest/forest-obs.gc"
  "levels/forest/pegasus.gc"
  "levels/forest/wren.gc"
  "levels/forest/fish.gc"
  "levels/forest/predator-h.gc"
  "levels/forest/predator-graph.gc"
  "levels/forest/predator.gc"
  "levels/forest/forest-scenes.gc"
  )

(copy-textures 1414 1416 1415 1413 2335 1411 2845)

(copy-gos
  "jak-pegasus+0-ag"
  "pegasus-ag"
  "predator-ag"
  "wren-ag"
  "minnow-ag"
  "forest-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FORDUMPC
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FORDUMPC.DGO" "fordumpc.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/fortress/ammo_dump/fordumpc-part.gc"
  "levels/fortress/ammo_dump/fordumpc-obs.gc"
  "levels/fortress/ammo_dump/fortress-scenes.gc"
  )

(copy-textures 1570 1573 1571 2917 2847)

(copy-gos
  "fort-missile-target-ag"
  "fort-missile-ag"
  "fort-dump-bomb-a-ag"
  "fordumpc"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FORDUMPD
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FORDUMPD.DGO" "fordumpd.gd")

(copy-textures 1574)

(copy-gos
  "fordumpd"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FRA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FRA.DGO" "fra.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/common/enemy/hover/hover-formation-h.gc"
  ;; "levels/common/enemy/hover/hover-nav-control-h.gc"
  ;; "levels/common/enemy/hover/hover-enemy-h.gc"
  ;; "levels/common/enemy/hover/hover-nav-network.gc"
  ;; "levels/common/enemy/hover/hover-nav-control.gc"
  ;; "levels/common/enemy/hover/hover-enemy.gc"
  ;; "levels/common/enemy/hover/hover-enemy-battle.gc"
  ;; "levels/common/enemy/hover/hover-formation.gc"
  ;; "levels/common/enemy/hover/hover-nav-edit.gc"
  ;; "levels/common/enemy/hover/wasp-part.gc"
  ;; "levels/common/enemy/hover/wasp.gc"
  ;; "levels/common/enemy/hover/crimson-guard-hover.gc"
  ;; "levels/common/enemy/hover/flamer.gc"
  "levels/fortress/rescue/forresca-part.gc"
  "levels/fortress/rescue/forresca-obs.gc"
  )

(copy-textures 3190 3192 3191)

(copy-gos
  ;; "crimson-guard-hover-ag"
  "cty-guard-turret-button-ag"
  "fort-led-ag"
  "forresca-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; FRB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "FRB.DGO" "frb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/fortress/rescue/forrescb-part.gc"
  "levels/fortress/rescue/forrescb-obs.gc"
  "levels/common/entities/spydroid.gc"
  )

(copy-textures 3172 3175 3173 3174)

(copy-gos
  "spydroid-ag"
  "fort-elec-belt-ag"
  "forrescb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; GARAGE
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "GARAGE.DGO" "garage.gd")

;; (copy-textures 1369 1430 1786 1787 3025)

;; (copy-gos
;;   "keira-garage+0-ag"
;;   "keira-highres-ag"
;;   "rift-rider-ag"
;;   "gar-curtain-ag"
;;   "gar-trophy-ca-ag"
;;   "gar-trophy-cb-ag"
;;   "gar-trophy-cc-ag"
;;   "garage-bikec-ag"
;;   "gar-weld-project-ag"
;;   "garage-bikeb-ag"
;;   "particleman-ag"
;;   "spotlight-ag"
;;   "garage"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; GGA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "GGA.DGO" "gga.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/gungame/gun-dummy.gc"
  "levels/gungame/gungame-data.gc"
  "levels/gungame/gungame-obs.gc"
  "levels/gungame/gungame-part.gc"
  )

(copy-textures 1733 1735 1734 2405)

(copy-gos
  "gun-dummy-yellow-gun-ag"
  "gun-cit-d-ag"
  "gun-cit-a-ag"
  "gun-cit-b-ag"
  "gun-dummy-gold-ag"
  "gun-dummy-a-ag"
  "gun-dummy-c-ag"
  "gun-dummy-b-ag"
  "gun-cit-c-ag"
  "gun-dummy-big-ag"
  "hip-door-a-ag"
  "yellow-barrel-ag"
  "dark-barrel-ag"
  "gungame-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; HALFPIPE
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "HALFPIPE.DGO" "halfpipe.gd")

;; (copy-textures 43 623 42)

;; (copy-gos
;;   "jak-pole+0-ag"
;;   "grunt-ag"
;;   "halfpipe"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; HIDEOUT
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "HIDEOUT.DGO" "hideout.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/hideout/hideout-scenes.gc"
  "levels/hideout/hideout-part.gc"
  "levels/hideout/hideout-obs.gc"
  )

(copy-textures 932 933 2404 999)

(copy-gos
  ;; "daxter-highres-ag"
  ;; "jak-highres-ag"
  "hide-light-ag"
  "hide-bike-ag"
  "hide-door-b-ag"
  "gun-barrel-ag"
  "hide-faucet-ag"
  ;; "particleman-ag"
  "hideout"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; HIPHOG
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "HIPHOG.DGO" "hiphog.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/hiphog/hiphog-obs.gc"
  "levels/hiphog/hiphog-part.gc"
  "levels/hiphog/hiphog-scenes.gc"
  )

(copy-textures 929 930 3042 998)

(copy-gos
  ;; "daxter-highres-ag"
  ;; "jak-highres-ag"
  "hip-whack-a-metal-ag"
  "hip-trophy-g-ag"
  "hip-trophy-d-ag"
  "hip-trophy-c-ag"
  "hip-trophy-a-ag"
  "hip-trophy-f-ag"
  "hip-trophy-i-ag"
  "hip-trophy-m-ag"
  ;; "hip-door-a-ag"
  "hip-trophy-n-ag"
  "hip-trophy-j-ag"
  "hip-mirror-ag"
  "hiphog"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; INTROCST
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "INTROCST.DGO" "introcst.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/intro/intro-ocean.gc"
  "levels/intro/intro-scenes.gc"
  "levels/intro/intro-obs.gc"
  "levels/intro/vortex-data.gc"
  "levels/intro/vortex.gc"
  )

(copy-textures 2568 2570 3264)

(copy-gos
  ;; "daxter-highres-ag"
  "jakone-highres-ag"
  "errol-highres-ag"
  ;; "crimson-guard-lowres-ag"
  "time-map-ag"
  "rift-rider-heart-of-mar-ag"
  "rift-rider-dash-ag"
  "meteor-ag"
  "rift-rider-break-b-ag"
  "vortex-light-ag"
  "rift-rider-chunk-a-ag"
  "rift-rider-break-c-ag"
  ;; "particleman-ag"
  "rift-rider-bar-ag"
  "introcst"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; KIOSK
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "KIOSK.DGO" "kiosk.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/market/west/brutter_kiosk/kiosk-part.gc"
  )

(copy-textures 2192 2212 2225 2911 2334)

(copy-gos
  ;; "daxter-highres-ag"
  "brutter-highres-ag"
  ;; "jak-highres-ag"
  "kiosk-fish-sign-ag"
  "kiosk-banner-ag"
  ;; "particleman-ag"
  "kiosk"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LASHGRD
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LASHGRD.DGO" "lashgrd.gd")

;; (copy-textures 2707 3265)

;; (copy-gos
;;   "ashelin-highres-ag"
;;   "crimson-guard-highres-ag"
;;   "crimson-guard-lowres-ag"
;;   "lashgrd"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LASHTHRN
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LASHTHRN.DGO" "lashthrn.gd")

;; (copy-textures 2953 3266)

;; (copy-gos
;;   "ashelin-highres-ag"
;;   "palmpilot-ag"
;;   "lashthrn"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LBBUSH
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "LBBUSH.DGO" "lbbush.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/side_missions/ctywide-bbush.gc"
  )

(copy-textures 3336 3465)

(copy-gos
  "hovering-mine-ag"
  "homing-beacon-ag"
  "lbbush"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LBOMBBOT
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "LBOMBBOT.DGO" "lbombbot.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/common/enemy/bombots/bombbot-h.gc"
  "levels/common/enemy/bombots/bombbot-path.gc"
  "levels/common/enemy/bombots/bombbot.gc"
  )

(copy-textures 2751)

(copy-gos
  "bombbot-ag"
  "lbombbot"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LBRNERMK
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LBRNERMK.DGO" "lbrnermk.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "characters/baron_errol_metalkor/metalkor-texture.gc"
;;   )

;; (copy-textures 2558 2801 3267)

;; (copy-gos
;;   "errol-highres-ag"
;;   "baron-highres-ag"
;;   "metalkor-head-ag"
;;   "lbrnermk"
;;   )

;;;;;;;;;;;;;;;;;;;;;
;; LCGUARD
;;;;;;;;;;;;;;;;;;;;;

(cgo "LCGUARD.DGO" "lcguard.gd")

(copy-textures 2691)

(copy-gos
  "lcguard"
  )

;;;;;;;;;;;;;;;;;;;;;
;; LCITYLOW
;;;;;;;;;;;;;;;;;;;;;

(cgo "LCITYLOW.DGO" "lcitylow.gd")

(copy-textures 2755 2756 3388)

(copy-gos
  "lcitylow"
  )

;;;;;;;;;;;;;;;;;;;;;
;; LDJAKBRN
;;;;;;;;;;;;;;;;;;;;;

(cgo "LDJAKBRN.DGO" "ldjakbrn.gd")

(copy-textures 2662 2657 3094)

(copy-gos
  "jak-highres-prison-ag"
  ;; "baron-highres-ag"
  "prsn-daxter-plat-ag"
  "jak-clothes-ag"
  "ldjakbrn"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LERBRNGD
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LERBRNGD.DGO" "lerbrngd.gd")

;; (copy-textures 2703 3268)

;; (copy-gos
;;   "samos-highres-ag"
;;   "baron-highres-ag"
;;   "crimson-guard-highres-ag"
;;   "crimson-guard-lowres-ag"
;;   "lerbrngd"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LERLCHAL
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "LERLCHAL.DGO" "lerlchal.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/common/races/race-h.gc"
  "levels/common/races/race-mesh.gc"
  "levels/common/races/race-part.gc"
  "levels/common/races/race-obs.gc"
  "levels/common/races/vehicle-racer.gc"
  "levels/common/races/race-info.gc"
  "levels/common/races/race-manager.gc"
  "levels/common/races/race-hud.gc"
  "levels/common/races/pilot-recorder.gc"
  "levels/city/port/race/errol-chal-part.gc"
  "levels/city/port/race/errol-chal.gc"
  )

(copy-textures 3159 3337)

(copy-gos
  "errol-racer-ag"
  "lerlchal"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LERLTESS
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LERLTESS.DGO" "lerltess.gd")

;; (copy-textures 2949 3269)

;; (copy-gos
;;   "tess-lhiphog+0-ag"
;;   "krew-highres-ag"
;;   "errol-highres-ag"
;;   "tess-highres-ag"
;;   "lerltess"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LERROL
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LERROL.DGO" "lerrol.gd")

;; (copy-textures 3270)

;; (copy-gos
;;   "errol-highres-ag"
;;   "lerrol"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LGARCSTA
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LGARCSTA.DGO" "lgarcsta.gd")

;; (copy-textures 2699 3512 3271)

;; (copy-gos
;;   "krew-highres-ag"
;;   "samos-highres-ag"
;;   "tess-highres-ag"
;;   "ashelin-highres-ag"
;;   "gold-key-ag"
;;   "lgarcsta"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LGUARD
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LGUARD.DGO" "lguard.gd")

;; (copy-textures 1753 2883 3272)

;; (copy-gos
;;   "krew-highres-ag"
;;   "sig-highres-ag"
;;   "crimson-guard-highres-ag"
;;   "gun-upgrade-a-ag"
;;   "palmpilot-ag"
;;   "hip-mug-ag"
;;   "computerpaper-ag"
;;   "lguard"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LHELLDOG
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LHELLDOG.DGO" "lhelldog.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/enemy/hellcat/helldog.gc"
;;   )

;; (copy-textures 2324 3273)

;; (copy-gos
;;   "helldog-ag"
;;   "lhelldog"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LHIPOUT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LHIPOUT.DGO" "lhipout.gd")

;; (copy-textures 3440 3199 3200 3486 3242 3544)

;; (copy-gos
;;   "pecker-highres-ag"
;;   "crocadog-highres-ag"
;;   "sig-highres-ag"
;;   "metalkor-chopped-ag"
;;   "gold-key-ag"
;;   "kid-medallion-ag"
;;   "hip-bottle-c-ag"
;;   "hip-mug-ag"
;;   "particleman-ag"
;;   "lhipout"
;;   )

;;;;;;;;;;;;;;;;;;;;;
;; LINTCSTB
;;;;;;;;;;;;;;;;;;;;;

(cgo "LINTCSTB.DGO" "lintcstb.gd")

(copy-textures 2752 2754 3243)

(copy-gos
  "rift-rider-break-ag"
  "rift-rider-ag"
  "lintcstb"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LJAKDAX
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LJAKDAX.DGO" "ljakdax.gd")

;; (copy-textures 1879)

;; (copy-gos
;;   "daxter-highres-ag"
;;   "jak-highres-ag"
;;   "ljakdax"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LJKDXASH
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LJKDXASH.DGO" "ljkdxash.gd")

;; (copy-textures 3188 3244)

;; (copy-gos
;;   "daxter-highres-ag"
;;   "jak-highres-ag"
;;   "ashelin-highres-ag"
;;   "ljkdxash"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LKEIRIFT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LKEIRIFT.DGO" "lkeirift.gd")

;; (copy-textures 3023 3024)

;; (copy-gos
;;   "keira-anim+0-ag"
;;   "keira-highres-ag"
;;   "rift-rider-ag"
;;   "lkeirift"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LKIDDOGE
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "LKIDDOGE.DGO" "lkiddoge.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/kid_escort/kidesc-h.gc"
  "levels/city/kid_escort/crocesc-h.gc"
  "levels/city/kid_escort/hal4-course.gc"
  "levels/city/kid_escort/kidesc-task.gc"
  "levels/city/kid_escort/kidesc.gc"
  "levels/city/kid_escort/kidesc-states.gc"
  "levels/city/kid_escort/crocesc-task.gc"
  "levels/city/kid_escort/crocesc.gc"
  "levels/city/kid_escort/crocesc-states.gc"
  "levels/city/kid_escort/kidesc4-course.gc"
  "levels/city/kid_escort/crocesc4-course.gc"
  )

(copy-textures 2209)

(copy-gos
  "crocadog-escort-ag"
  "kid-escort-ag"
  "lkiddoge"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LMEETBRT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LMEETBRT.DGO" "lmeetbrt.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/city/market/west/brutter_kiosk/meet-brutter.gc"
;;   )

;; (copy-textures 2438)

;; (copy-gos
;;   "babak-ag"
;;   "paddy-wagon-ag"
;;   "lmeetbrt"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LOUTCSTB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LOUTCSTB.DGO" "loutcstb.gd")

;; (copy-textures 3186 3185 3187 3245)

;; (copy-gos
;;   "youngsamos-highres-ag"
;;   "brutter-highres-ag"
;;   "kid-highres-ag"
;;   "rift-ring-ag"
;;   "brutter-balloon-norift-ag"
;;   "metalkor-chopped-ag"
;;   "rift-rider-ag"
;;   "precursor-stone-ag"
;;   "particleman-ag"
;;   "loutcstb"
;;   )

;;;;;;;;;;;;;;;;;;;;;
;; LPACKAGE
;;;;;;;;;;;;;;;;;;;;;

(cgo "LPACKAGE.DGO" "lpackage.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/misc/delivery/delivery-task.gc"
  )

(copy-textures 2459)

(copy-gos
  "krew-package-ag"
  "lpackage"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LPORTRUN
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LPORTRUN.DGO" "lportrun.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/city/port/mines/portrun.gc"
;;   )

;; (copy-textures 2817 2373)

;; (copy-gos
;;   "ctyport-cargo-ag"
;;   "ctyport-spy-ag"
;;   "mine-b-ag"
;;   "ctyport-mine-ag"
;;   "lportrun"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LPOWER
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LPOWER.DGO" "lpower.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/power_station/power_switches/ctypower.gc"
;;   )

;; (copy-textures 3348)

;; (copy-gos
;;   "cty-guard-turret-button-ag"
;;   "lpower"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LPROTECT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LPROTECT.DGO" "lprotect.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/entities/gun-buoy.gc"
;;   "levels/forest/lifeseed/protect.gc"
;;   )

;; (copy-textures 2869)

;; (copy-gos
;;   "gun-buoy-ag"
;;   "transport-ag"
;;   "seal-of-mar-ag"
;;   "seal-of-mar-top-ag"
;;   "lprotect"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LPRSNCST
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LPRSNCST.DGO" "lprsncst.gd")

;; (copy-textures 2658 3246)

;; (copy-gos
;;   "youngsamos-highres-ag"
;;   "jak-highres-ag"
;;   "samos-highres-ag"
;;   "tess-highres-ag"
;;   "lprsncst"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LPRTRACE
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LPRTRACE.DGO" "lprtrace.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/races/race-h.gc"
;;   "levels/common/races/race-mesh.gc"
;;   "levels/common/races/race-part.gc"
;;   "levels/common/races/race-obs.gc"
;;   "levels/common/races/vehicle-racer.gc"
;;   "levels/common/races/race-info.gc"
;;   "levels/common/races/race-manager.gc"
;;   "levels/common/races/race-hud.gc"
;;   "levels/common/races/pilot-recorder.gc"
;;   "levels/city/port/race/errol-chal-part.gc"
;;   "levels/city/port/race/errol-chal.gc"
;;   )

;; (copy-textures 3427)

;; (copy-gos
;;   "lprtrace"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LRACEBB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LRACEBB.DGO" "lracebb.gd")

;; (copy-gos
;;   "lracebb"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LRACEBF
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LRACEBF.DGO" "lracebf.gd")

;; (copy-gos
;;   "lracebf"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LRACECB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LRACECB.DGO" "lracecb.gd")

;; (copy-textures 3432)

;; (copy-gos
;;   "lracecb"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LRACECF
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LRACECF.DGO" "lracecf.gd")

;; (copy-textures 3430)

;; (copy-gos
;;   "lracecf"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LRACEDB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LRACEDB.DGO" "lracedb.gd")

;; (copy-gos
;;   "lracedb"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LRACEDF
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LRACEDF.DGO" "lracedf.gd")

;; (copy-gos
;;   "lracedf"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LRACELIT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LRACELIT.DGO" "lracelit.gd")

;; (copy-textures 3162 3374)

;; (copy-gos
;;   "stadium-racer-ag"
;;   "race-start-light-ag"
;;   "race-start-light-banner-ag"
;;   "lracelit"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LSACK
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "LSACK.DGO" "lsack.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/misc/collection_task/collection-task.gc"
  )

(copy-textures 2605)

(copy-gos
  "krew-moneybag-ag"
  "lsack"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LSAMERGD
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LSAMERGD.DGO" "lsamergd.gd")

;; (copy-textures 2701 3248)

;; (copy-gos
;;   "samos-lsamergd+0-ag"
;;   "samos-highres-ag"
;;   "errol-highres-ag"
;;   "crimson-guard-highres-ag"
;;   "crimson-guard-lowres-ag"
;;   "lsamergd"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LSHUTTLE
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LSHUTTLE.DGO" "lshuttle.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "characters/underground_fighters/shuttle.gc"
;;   )

;; (copy-textures 3214)

;; (copy-gos
;;   "citizen-rebel-ag"
;;   "krew-package-ag"
;;   "lshuttle"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LSMYSBRT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LSMYSBRT.DGO" "lsmysbrt.gd")

;; (copy-textures 2705 3249)

;; (copy-gos
;;   "youngsamos-highres-ag"
;;   "brutter-highres-ag"
;;   "samos-highres-ag"
;;   "lsmysbrt"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LTENTOB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LTENTOB.DGO" "ltentob.gd")

;; (copy-textures 3073 3112 3250)

;; (copy-gos
;;   "kor-highres-ag"
;;   "seal-of-mar-ag"
;;   "ltentob"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LTENTOUT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LTENTOUT.DGO" "ltentout.gd")

;; (copy-textures 2618 3251)

;; (copy-gos
;;   "youngsamos-onintent+0-ag"
;;   "youngsamos-highres-ag"
;;   "ltentout"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LTESS
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LTESS.DGO" "ltess.gd")

;; (copy-textures 1862 1756 2881 3241)

;; (copy-gos
;;   "tess-lhiphog+0-ag"
;;   "krew-highres-ag"
;;   "sig-highres-ag"
;;   "tess-highres-ag"
;;   "hip-bottle-c-ag"
;;   "hip-bottle-b-ag"
;;   "ltess"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LTHRNOUT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LTHRNOUT.DGO" "lthrnout.gd")

;; (copy-textures 3194 3252)

;; (copy-gos
;;   "torn-highres-ag"
;;   "ashelin-highres-ag"
;;   "palmpilot-ag"
;;   "lthrnout"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LTRNKRKD
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LTRNKRKD.DGO" "ltrnkrkd.gd")

;; (copy-textures 1773 3253)

;; (copy-gos
;;   "kid-ltrnkrkd+0-ag"
;;   "kor-highres-ag"
;;   "torn-highres-ag"
;;   "kid-highres-ag"
;;   "ltrnkrkd"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LTRNTESS
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LTRNTESS.DGO" "ltrntess.gd")

;; (copy-textures 1764 3254)

;; (copy-gos
;;   "tess-ltrntess+0-ag"
;;   "daxter-highres-ag"
;;   "jak-highres-ag"
;;   "torn-highres-ag"
;;   "tess-highres-ag"
;;   "particleman-ag"
;;   "ltrntess"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LTRNYSAM
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "LTRNYSAM.DGO" "ltrnysam.gd")

(copy-textures 1774 3255)

(copy-gos
  "youngsamos-ltrnysam+0-ag"
  ; "youngsamos-highres-ag"
  ; "torn-highres-ag"
  "ltrnysam"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LWHACK
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "LWHACK.DGO" "lwhack.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/hiphog/whack.gc"
  )

(copy-textures 2889 3256)

(copy-gos
  "daxter-mole+0-ag"
  "tess-lwhack+0-ag"
  "tess-highres-ag"
  ;; "time-map-ag"
  "grunt-fma-ag"
  "hip-mole-ag"
  "big-bopper-ag"
  ;; "particleman-ag"
  "lwhack"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LWIDEB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LWIDEB.DGO" "lwideb.gd")

;; (copy-textures 2972)

;; (copy-gos
;;   "grunt-ag"
;;   "citizen-norm-ag"
;;   "predator-ag"
;;   "flitter-ag"
;;   "cara-ag"
;;   "citizen-norm-rider-ag"
;;   "crimson-bike-ag"
;;   "bikea-ag"
;;   "lwideb"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LWIDEC
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LWIDEC.DGO" "lwidec.gd")

;; (copy-textures 3039 3041)

;; (copy-gos
;;   "roboguard-ag"
;;   "citizen-norm-ag"
;;   "citizen-chick-ag"
;;   "hellcat-ag"
;;   "carc-ag"
;;   "cara-ag"
;;   "carb-ag"
;;   "crimson-bike-ag"
;;   "bikec-ag"
;;   "bikeb-ag"
;;   "bikea-ag"
;;   "lwidec"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LWIDESTA
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LWIDESTA.DGO" "lwidesta.gd")

;; (copy-textures 2964 3257)

;; (copy-gos
;;   "errol-highres-ag"
;;   "baron-highres-ag"
;;   "crimson-guard-highres-ag"
;;   "stdm-baron-box-ag"
;;   "crimson-guard-lowres-ag"
;;   "stdm-barrels-ag"
;;   "palmpilot-ag"
;;   "particleman-ag"
;;   "lwidesta"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LYSAMSAM
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LYSAMSAM.DGO" "lysamsam.gd")

;; (copy-textures 3260)

;; (copy-gos
;;   "youngsamos-lysamsam+0-ag"
;;   "youngsamos-highres-ag"
;;   "samos-highres-ag"
;;   "lysamsam"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; LYSKDCD
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "LYSKDCD.DGO" "lyskdcd.gd")

;; (copy-textures 1772 3261)

;; (copy-gos
;;   "youngsamos-lyskdcd+0-ag"
;;   "kid-lyskdcd+0-ag"
;;   "daxter-highres-ag"
;;   "youngsamos-highres-ag"
;;   "jak-highres-ag"
;;   "crocadog-highres-ag"
;;   "kid-highres-ag"
;;   "particleman-ag"
;;   "lyskdcd"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; MCN
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "MCN.DGO" "mcn.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/temple/canyon/mincan-obs.gc"
  "levels/temple/canyon/canyon-scenes.gc"
  )

(copy-textures 3341 3343 3342 3344 3471)

(copy-gos
  "ctypal-baron-statue-break-ag"
  "mincan-cogs-ag"
  "ctypal-break-wall-ag"
  "mincan-lighthouse-ag"
  "water-anim-mincan-ag"
  "mincan-lens-ag"
  "shard-ag"
  "gear-device-gear-ag"
  "mtn-lens-ag"
  "mincan-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; MTN
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "MTN.DGO" "mtn.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/temple/rhino.gc"
  "levels/temple/rhino-wall.gc"
  "levels/temple/mountain-ocean.gc"
  "levels/temple/mountain-obs.gc"
  "levels/temple/mountain-obs2.gc"
  "levels/temple/mountain-scenes.gc"
  "levels/temple/mountain-part.gc"
  "levels/common/enemy/hopper.gc"
  )

(copy-textures 1254 1256 1255 1253 1257 3470 1275)

(copy-gos
  "plat-buried-ag"
  "plat-return-ag"
  "water-anim-mountain-ag"
  "mtn-gate-ag"
  "mtn-lens-base-ag"
  "mtn-plat-long-ag"
  "iris-door-ag"
  "mtn-plat-elevator-ag"
  "mtn-plat-shoot-ag"
  "mtn-lens-floor-ag"
  "mtn-plat-updown-ag"
  "pal-windmill-ag"
  "mountain-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; MTX
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "MTX.DGO" "mtx.gd")

(copy-textures 1719 1721 1720 1722 3506)

(copy-gos
  "mtn-gear-device-ag"
  "mtn-step-plat-rocks-ag"
  "rhino-ag"
  "mtn-plat-buried-rocks-ag"
  "rhino-wall-ag"
  "hopper-ag"
  "water-anim-mountain-dark-eco-ag"
  "mtn-aval-rocks-ag"
  "mtn-dice-ag"
  "seal-of-mar-ag"
  "mtn-dice-button-ag"
  "mtn-plat-eject-ag"
  "mtn-button-ag"
  "mtnext-vis"
  )

(copy-strs "RHW1" "RHW2")

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; NEB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "NEB.DGO" "neb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/nest/boss/nest-texture.gc"
  "levels/nest/boss/metalkor-part.gc"
  "levels/nest/boss/metalkor-setup.gc"
  "levels/nest/boss/metalkor-states.gc"
  "levels/nest/boss/metalkor-extras.gc"
  "levels/nest/boss/nestb-scenes.gc"
  "levels/nest/boss/nestb-part.gc"
  )

(copy-textures 2985 2986 3122 3121 2987 3092 3378)

(copy-gos
  "kid-nestb+0-ag"
  "metalkor-ag"
  "metalkor-lowtorso-ag"
  "metalkor-highres-ag"
  "metalkor-legs-ag"
  "metalkor-explode-ag"
  "kid-highres-ag"
  "nest-gun-parts-ag"
  "kid-ag"
  "nest-break-precipice-ag"
  "metalkor-bomb-ag"
  "metalkor-wings-ag"
  "metalkor-egg-ag"
  "rift-ring-in-game-ag"
  "nest-unbroken-rocks-ag"
  "metalkor-distort-ag"
  "metalkor-rays-ag"
  "nest-gun-elevator-ag"
  "metalkor-fma-spinner-ag"
  "nestb-tail-bound-ag"
  "rift-occlude-ag"
  "nestb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; NES
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "NES.DGO" "nes.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/common/entities/gun-buoy.gc"
  "levels/nest/nest-obs.gc"
  "levels/nest/mantis.gc"
  "levels/nest/mammoth.gc"
  "levels/nest/flying-spider.gc"
  "levels/nest/nest-ocean.gc"
  "levels/nest/nest-scenes.gc"
  "levels/nest/nest-part.gc"
  )

(copy-textures 2988 2990 2989 2991 3349)

(copy-gos
  "mammoth-ag"
  "flying-spider-ag"
  "mantis-ag"
  "nest-break-rocks-ag"
  "water-anim-nest-dark-eco-ag"
  ;; "gun-buoy-ag"
  "transport-ag"
  "switch-ag"
  ;; "air-train-ag"
  ;; "piston-ag"
  "nest-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; NESTT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "NESTT.DGO" "nestt.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/entities/gun-buoy.gc"
;;   "levels/nest/nest-obs.gc"
;;   "levels/nest/mantis.gc"
;;   "levels/nest/mammoth.gc"
;;   "levels/nest/flying-spider.gc"
;;   "levels/nest/nest-ocean.gc"
;;   "levels/nest/nest-scenes.gc"
;;   "levels/nest/nest-part.gc"
;;   )

;; (copy-textures 2988 2990 2989 2991 3349)

;; (copy-gos
;;   "mammoth-ag"
;;   "flying-spider-ag"
;;   "mantis-ag"
;;   "nest-break-rocks-ag"
;;   "water-anim-nest-dark-eco-ag"
;;   "gun-buoy-ag"
;;   "transport-ag"
;;   "switch-ag"
;;   "air-train-ag"
;;   "piston-ag"
;;   "nestt"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; ONINTENT
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "ONINTENT.DGO" "onintent.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/onin_tent/onintent-part.gc"
  "levels/city/onin_tent/onintent-scenes.gc"
  "levels/city/onin_tent/onin-game.gc"
  )

(copy-textures 1031 1183 2676 1035)

(copy-gos
  "onin-game+0-ag"
  ;; "daxter-highres-ag"
  "pecker-highres-ag"
  ;; "jak-highres-ag"
  "onin-highres-ag"
  "life-seed-ag"
  "onin-brain-ag"
  "son-of-particleman-ag"
  ;; "particleman-ag"
  "onintent"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; ORACLE
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "ORACLE.DGO" "oracle.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/city/oracle/oracle-texture.gc"
  "levels/city/oracle/oracle-part.gc"
  "levels/city/oracle/oracle-scenes.gc"
  "levels/city/oracle/oracle-training.gc"
  )

(copy-textures 2553 2863 2557)

(copy-gos
  ;; "daxter-highres-ag"
  ;; "darkjak-highres-ag"
  "oracle-roof-banner-b-ag"
  "oracle-roof-banner-ag"
  ;; "door-ag"
  "oracle-wall-banner-ag"
  ;; "particleman-ag"
  "oracle"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; OUTROCST
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "OUTROCST.DGO" "outrocst.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "engine/ui/credits.gc"
  "levels/outro/outro-scenes.gc"
  )

(copy-textures 3182 3183 3263 3513)

(copy-gos
  "0credits-tx"
  "1credits-tx"
  "2credits-tx"
  "3credits-tx"
  "4credits-tx"
  "5credits-tx"
  "6credits-tx"
  "7credits-tx"
  "keira-highres-ag"
  "samos-highres-ag"
  ;; "tess-highres-ag"
  ;; "onin-highres-ag"
  "rift-break-ring-ag"
  "precursor-ag"
  "outrocst"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; PAC
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "PAC.DGO" "pac.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/palace/cable/palcab-part.gc"
  "levels/palace/cable/palcab-obs.gc"
  ;; "levels/city/common/searchlight.gc"
  "levels/common/entities/sew-gunturret.gc"
  "levels/palace/pal-obs.gc"
  )

(copy-textures 2354 2357 2355 2356 3371 2619)

(copy-gos
  ;; "jak-pole+0-ag"
  "pal-gun-turret-ag"
  "pal-electric-fan-ag"
  "pal-cable-nut-ag"
  "pal-flip-step-ag"
  "pal-rot-gun-ag"
  "pal-falling-plat-ag"
  ;; "searchlight-ag"
  ;; "pal-windmill-ag"
  "palcab-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; PAE
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "PAE.DGO" "pae.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/common/entities/sew-gunturret.gc"
  "levels/palace/explore/palent-part.gc"
  ;; "levels/palace/pal-obs.gc"
  )

(copy-textures 3169 3171 3170 3168)

(copy-gos
  ;; "jak-pole+0-ag"
  "palent-turret-ag"
  "pal-breakable-window-ag"
  "pal-grind-ring-ag"
  "pal-grind-ring-center-ag"
  "pal-throne-door-ag"
  ;; "pal-falling-plat-ag"
  "pal-ent-door-ag"
  "pal-ent-glass-ag"
  "palent-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; PALBOSS
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "PALBOSS.DGO" "palboss.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/palace/boss/squid-part.gc"
;;   "levels/palace/boss/squid-setup.gc"
;;   "levels/palace/boss/squid-extras.gc"
;;   "levels/palace/boss/squid-states.gc"
;;   )

;; (copy-textures 2679 2680 2797 3419)

;; (copy-gos
;;   "baron-squid+0-ag"
;;   "daxter-highres-ag"
;;   "jak-highres-ag"
;;   "squid-ag"
;;   "squid-break-ag"
;;   "baron-highres-ag"
;;   "baron-ag"
;;   "squid-collision-ag"
;;   "palboss"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; PALOUT
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "PALOUT.DGO" "palout.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/palace/outside/palace-ocean.gc"
;;   )

;; (copy-textures 2572 2573)

;; (copy-gos
;;   "palout"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; PAR
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "PAR.DGO" "par.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/palace/roof/palroof-part.gc"
  "levels/palace/roof/palroof-obs.gc"
  "levels/palace/roof/palboss-texture.gc"
  "levels/palace/roof/palboss-part.gc"
  "levels/palace/roof/palboss-scenes.gc"
  )

(copy-textures 2360 2363 2361 2362 2521 2746)

(copy-gos
  "pal-prong-ag"
  ;; "pal-flip-step-ag"
  "pal-lowrez-throne-ag"
  "palroof-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; PAS
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "PAS.DGO" "pas.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/common/entities/com-elevator.gc"
  "levels/palace/shaft/palshaft-part.gc"
  )

(copy-textures 2371 2372 2692)

(copy-gos
  ;; "com-airlock-inner-ag"
  "com-elevator-ag"
  "palshaft-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; PORTWALL
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "PORTWALL.DGO" "portwall.gd")

;; (copy-textures 3181 3323)

;; (copy-gos
;;   "mecha-daxter-ag"
;;   "portwall"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; RUI
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "RUI.DGO" "rui.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/ruins/ruins-ocean.gc"
  "levels/ruins/ruins-scenes.gc"
  "levels/ruins/mechtest-obs.gc"
  "levels/ruins/ruins-part.gc"
  "levels/ruins/breakable-wall.gc"
  "levels/ruins/pillar-collapse.gc"
  "levels/ruins/ruins-obs.gc"
  "levels/ruins/rapid-gunner.gc"
  )

(copy-textures 851 853 852 2205 1000)

(copy-gos
  "ruins-breakable-wall-ag"
  "ruins-drop-plat-ag"
  "rapid-gunner-ag"
  "ruins-pillar-collapse-ag"
  "ruins-bridge-ag"
  "shield-gunner-ag"
  "precipice-b-ag"
  "ruins-precipice-ag"
  "precipice-a-ag"
  "pushblock-ag"
  "throwblock-ag"
  "zipline-ag"
  "flag-ag"
  "sinking-plat-ag"
  "awning-ag"
  "beam-ag"
  "ruins-vis"
  )

(copy-strs "RUB1" "RUBW1" "RUBW2" "RUBW3" "RUBW4" "RUBW5" "RUBW6" "RUDPA1" "RUPC1" "RUPC2" "RUTVICTO")

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; SAG
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "SAG.DGO" "sag.gd")

(copy-textures 881 1131 882 880)

(copy-gos
  ;; "daxter-highres-ag"
  ;; "jak-highres-ag"
  "torn-highres-ag"
  "ruins-tower-a-ag"
  "ruins-tower-c-ag"
  "ruins-tower-e-ag"
  "ruins-tower-b-ag"
  "ruins-tower-f-ag"
  "ruins-tower-d-ag"
  ;; "life-seed-ag"
  "ruins-tower-rp-ag"
  "sagehut-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; SEB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "SEB.DGO" "seb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/undefined/hal-h.gc"
  "levels/undefined/hal-task.gc"
  "levels/undefined/hal.gc"
  "levels/undefined/ruf-h.gc"
  "levels/undefined/ruf-task.gc"
  "levels/undefined/ruf.gc"
  "levels/undefined/ruf-states.gc"
  "levels/sewer/escort/jinx-h.gc"
  "levels/sewer/escort/jinx-shot.gc"
  "levels/sewer/escort/jinx-bomb.gc"
  "levels/sewer/escort/jinx.gc"
  "levels/sewer/escort/jinx-states.gc"
  "levels/sewer/escort/mog-h.gc"
  "levels/sewer/escort/mog.gc"
  "levels/sewer/escort/grim-h.gc"
  "levels/sewer/escort/grim.gc"
  "levels/sewer/sewer-part.gc"
  "levels/sewer/sewer-ocean.gc"
  "levels/sewer/sewer-obs.gc"
  "levels/sewer/sewer-obs2.gc"
  "levels/sewer/hosehead.gc"
  "levels/sewer/hosehead-fake.gc"
  "levels/sewer/gator.gc"
  "levels/sewer/hal2-course.gc"
  "levels/sewer/mog2-course.gc"
  "levels/sewer/jinx2-course.gc"
  "levels/sewer/grim2-course.gc"
  "levels/sewer/sewer-scenes.gc"
  )

(copy-textures 1133 1135 1134 3383)

(copy-gos
  "sew-elevator-ag"
  "sewerb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; SEW
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "SEW.DGO" "sew.gd")

(copy-textures 1059 1060 1066 2270 3479)

(copy-gos
  "sew-catwalk-ag"
  "sew-multi-blade-ag"
  "sew-gunturret-ag"
  "mar-statue-ag"
  "sew-twist-blade-ag"
  "sew-tri-blade-ag"
  "sew-single-blade-ag"
  "mine-a-ag"
  "sew-valve-ag"
  "mine-b-ag"
  "sew-arm-blade-ag"
  "gold-key-ag"
  "light-switch-ag"
  "grill-ag"
  "sewer-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; SKA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "SKA.DGO" "ska.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/stadium/jetboard/skatea-part.gc"
  "levels/stadium/jetboard/skatea-obs.gc"
  )

(copy-textures 2992 3131 2995 3573)

(copy-gos
  "skate-training-ramp-ag"
  "skatea-jump-pad-ag"
  "skatea-floating-ring-ag"
  "skate-gate-ag"
  "skatea-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; STA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "STA.DGO" "sta.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  ;; "levels/common/enemy/hopper.gc"
  "levels/stadium/stadiumb-texture.gc"
  "levels/stadium/stadium-part.gc"
  "levels/stadium/stadiumb-part.gc"
  "levels/stadium/stadium-obs.gc"
  "levels/stadium/stadium-scenes.gc"
  )

(copy-textures 2423 2426 2424 2575 1641)

(copy-gos
  ;; "daxter-highres-ag"
  ;; "jak-highres-ag"
  "water-anim-stadium-ag"
  "gar-door-ag"
  "stadium-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; STADBLMP
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "STADBLMP.DGO" "stadblmp.gd")

;; (copy-textures 2956 2957 2958 3280 3457 3418)

;; (copy-gos
;;   "youngsamos-highres-ag"
;;   "brutter-highres-ag"
;;   "grunt-ag"
;;   "samos-highres-ag"
;;   "samos-ag"
;;   "keira-ag"
;;   "brutter-balloon-ag"
;;   "babak-ag"
;;   "flitter-ag"
;;   "hopper-ag"
;;   "brutter-low-ag"
;;   "stadblmp"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; STB
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "STB.DGO" "stb.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/races/race-h.gc"
;;   "levels/common/races/race-mesh.gc"
;;   "levels/common/races/race-part.gc"
;;   "levels/common/races/race-obs.gc"
;;   "levels/common/races/vehicle-racer.gc"
;;   "levels/common/races/race-info.gc"
;;   "levels/common/races/race-manager.gc"
;;   "levels/common/races/race-hud.gc"
;;   "levels/common/races/pilot-recorder.gc"
;;   "levels/stadium/racebike.gc"
;;   "levels/stadium/stadium-race-obs.gc"
;;   )

;; (copy-textures 3451 3454 3452 3453 3109)

;; (copy-gos
;;   "jak-pidax+0-ag"
;;   "race-bike-c-ag"
;;   "race-bike-b-ag"
;;   "race-bike-a-ag"
;;   "stdmb-race-hatch-ag"
;;   "stad-force-field-ag"
;;   "stadiumb-vis"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; STC
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "STC.DGO" "stc.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/races/race-h.gc"
;;   "levels/common/races/race-mesh.gc"
;;   "levels/common/races/race-part.gc"
;;   "levels/common/races/race-obs.gc"
;;   "levels/common/races/vehicle-racer.gc"
;;   "levels/common/races/race-info.gc"
;;   "levels/common/races/race-manager.gc"
;;   "levels/common/races/race-hud.gc"
;;   "levels/common/races/pilot-recorder.gc"
;;   "levels/stadium/racebike.gc"
;;   "levels/stadium/stadium-race-obs.gc"
;;   )

;; (copy-textures 3442 3445 3443 3444 3351)

;; (copy-gos
;;   "jak-pidax+0-ag"
;;   "race-bike-c-ag"
;;   "race-bike-b-ag"
;;   "race-bike-a-ag"
;;   "stdmb-race-hatch-ag"
;;   "stad-c-force-field-ag"
;;   "stadiumc-vis"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; STD
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "STD.DGO" "std.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/races/race-h.gc"
;;   "levels/common/races/race-mesh.gc"
;;   "levels/common/races/race-part.gc"
;;   "levels/common/races/race-obs.gc"
;;   "levels/common/races/vehicle-racer.gc"
;;   "levels/common/races/race-info.gc"
;;   "levels/common/races/race-manager.gc"
;;   "levels/common/races/race-hud.gc"
;;   "levels/common/races/pilot-recorder.gc"
;;   "levels/stadium/racebike.gc"
;;   "levels/stadium/stadium-race-obs.gc"
;;   )

;; (copy-textures 3447 3450 3448 3449 3352)

;; (copy-gos
;;   "jak-pidax+0-ag"
;;   "race-bike-c-ag"
;;   "race-bike-b-ag"
;;   "race-bike-a-ag"
;;   "stdmb-race-hatch-ag"
;;   "stad-d-force-field-ag"
;;   "stadiumd-vis"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; STR
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "STR.DGO" "str.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/strip/strip-scenes.gc"
  "levels/strip/strip-part.gc"
  "levels/strip/strip-ocean.gc"
  "levels/strip/strip-rescue.gc"
  "levels/strip/strip-drop.gc"
  "levels/strip/strip-obs.gc"
  "levels/strip/chaincrate.gc"
  )

(copy-textures 787 789 788 786 1272 2854)

(copy-gos
  "flamer-ag"
  "water-anim-strip-dark-eco-ag"
  "cranecrate-ag"
  "strip-game-crate-ag"
  "drill-plat-ag"
  "curtainsaw-ag"
  "fencespikes-ag"
  "lgconveyor-ag"
  "pitspikes-ag"
  "grunt-egg-b-ag"
  "grunt-egg-a-ag"
  "grunt-egg-d-ag"
  "crane-ag"
  "strip-chain-crate-ag"
  "grunt-egg-c-ag"
  "cable-ag"
  "cntrlrm-door-ag"
  "plasmitebomb-ag"
  "strip-conveyor-ag"
  "cntrlrm-button-ag"
  "strip-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; SWB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "SWB.DGO" "swb.gd")

(copy-textures 1372 1373 1458)

(copy-gos
  "sewescb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; SWE
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "SWE.DGO" "swe.gd")

(copy-textures 1370 1387 1371 2293 3478 3415)

(copy-gos
  "jinx-ag"
  "hosehead-ag"
  "sew-wall-ag"
  "sew-mar-statue-explode-ag"
  "heart-of-mar-ag"
  "jinx-bomb-ag"
  "sewesc-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; TBO
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "TBO.DGO" "tbo.gd")

(copy-textures 1628 1629 2229 2247)

(copy-gos
  "baron-widow+0-ag"
  "tomb-boss-catwalk-ag"
  "tomb-boss-bridge-ag"
  "widow-ag"
  "tomb-boss-pillar-ag"
  ;; "spydroid-ag"
  "baron-pod-ag"
  "tomb-boss-firepot-ag"
  "baron-ag"
  "tomb-boss-debris-ag"
  "heart-mar-ag"
  "widow-bomb-ag"
  ;; "tomb-wing-door-ag"
  ;; "particleman-ag"
  "tombboss-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; THR
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "THR.DGO" "thr.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/palace/throne_room/throne-part.gc"
  "levels/palace/throne_room/palace-scenes.gc"
  )

(copy-textures 2731 2733 3372)

(copy-gos
  ;; "daxter-highres-ag"
  ;; "jak-highres-ag"
  "throne-throne-ag"
  ;; "pal-throne-door-ag"
  ;; "particleman-ag"
  "throne-vis"
  )

;;;;;;;;;;;;;;;;;;;;;
;; TITLE
;;;;;;;;;;;;;;;;;;;;;

(cgo "TITLE.DGO" "title.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/title/title-obs.gc"
  )

(copy-textures 3091 3090)

(copy-gos
  "jak-logo-ag"
  "jak-stand-ag"
  "title"
  )

(copy-strs "DESCREEN" "TIDINTRO")

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; TOA
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "TOA.DGO" "toa.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/mars_tomb/tomb-part.gc"
  "levels/mars_tomb/tomb-obs.gc"
  "levels/mars_tomb/tomb-water.gc"
  "levels/mars_tomb/tomb-beetle.gc"
  "levels/mars_tomb/widow-part.gc"
  "levels/mars_tomb/widow-baron.gc"
  "levels/mars_tomb/widow-extras.gc"
  "levels/mars_tomb/widow-more-extras.gc"
  "levels/mars_tomb/widow.gc"
  "levels/mars_tomb/widow2.gc"
  "levels/mars_tomb/monster-frog.gc"
  "levels/mars_tomb/tomb-scenes.gc"
  )

(copy-textures 1594 1595 1831 1467)

(copy-gos
  "water-anim-tomb-ag"
  "monster-frog-ag"
  "tomb-boss-door-ag"
  "tomb-mar-door-ag"
  "tomb-simon-button-ag"
  "tomb-wing-door-ag"
  "tomb-plat-return-ag"
  "tomba-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; TOB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "TOB.DGO" "tob.gd")

(copy-textures 1596 1597)

(copy-gos
  ;; "jak-pole+0-ag"
  "tomb-stair-block-ag"
  "tomb-baby-spider-ag"
  "water-anim-tomb-dark-eco-ag"
  "tomb-elevator-ag"
  "tomb-plat-wall-ag"
  "tomb-button-ag"
  "tomb-stair-block-spikes-ag"
  "tomb-boulder-door-ag"
  "tomb-move-swing-pole-ag"
  "tombb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; TOC
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "TOC.DGO" "toc.gd")

(copy-textures 1598 1599 1832)

(copy-gos
  ;; "water-anim-tomb-ag"
  ;; "tomb-baby-spider-ag"
  "tomb-beetle-ag"
  "tomb-door-ag"
  "tomb-vibe-ag"
  ;; "tomb-simon-button-ag"
  "tomb-plat-simon-ag"
  ;; "tomb-wing-door-ag"
  ;; "tomb-button-ag"
  "tomb-beetle-door-ag"
  "tombc-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; TOD
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "TOD.DGO" "tod.gd")

;; (goal-src-sequence
;;   ""
;;   :deps ("$OUT/obj/los-control.o")
;;   "levels/common/entities/com-elevator.gc"
;;   )

(copy-textures 1626 1627 3275)

(copy-gos
  "youngsamos-tombd+0-ag"
  ;; "kid-tombd+0-ag"
  ;; "daxter-highres-ag"
  ;; "youngsamos-highres-ag"
  ;; "jak-highres-ag"
  ;; "crocadog-highres-ag"
  ;; "kor-highres-ag"
  ;; "kid-highres-ag"
  ;; "tomb-mar-door-ag"
  ;; "com-elevator-ag"
  "tombd-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; TOE
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "TOE.DGO" "toe.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/mars_tomb/left/chase/target-indax.gc"
  "levels/mars_tomb/left/chase/tomb-boulder.gc"
  )

(copy-textures 2374 2375 2376)

(copy-gos
  "jak-indax+0-ag"
  "tomb-boulder-ag"
  ;; "tomb-baby-spider-ag"
  "tomb-plat-pillar-ag"
  "spider-eyes-ag"
  "tomb-bounce-web-ag"
  "tombe-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; TOMBEXT
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "TOMBEXT.DGO" "tombext.gd")

(copy-textures 2525)

(copy-gos
  ;; "daxter-highres-ag"
  "tomb-boss-explode-ag"
  ;; "baron-highres-ag"
  "tombext"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; UNB
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "UNB.DGO" "unb.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/underport/under-shoot-block.gc"
  "levels/underport/underb-master.gc"
  "levels/underport/under-obs.gc"
  "levels/underport/under-sig-obs.gc"
  "levels/underport/under-laser.gc"
  "levels/underport/pipe-grunt.gc"
  "levels/underport/jellyfish.gc"
  "levels/underport/under-part.gc"
  "levels/underport/centipede.gc"
  "levels/underport/sig5-cent1-path0.gc"
  "levels/underport/sig5-cent2-path0.gc"
  "levels/underport/sig5-course.gc"
  "levels/underport/under-scenes.gc"
  "levels/underport/sig-recorder.gc"
  )

(copy-textures 3051 3053 3054 3413)

(copy-gos
  "under-mine-ag"
  "jellyfish-ag"
  "under-shoot-block-ag"
  "under-break-floor-ag"
  "centipede-ag"
  "under-int-door-ag"
  "under-break-wall-b-ag"
  "under-break-wall-ag"
  "under-rise-plat-ag"
  "under-lift-ag"
  "under-buoy-base-ag"
  "under-buoy-chain-ag"
  "under-buoy-plat-ag"
  "water-anim-under-ag"
  "under-plat-shoot-ag"
  "under-plat-wall-ag"
  "under-plat-long-ag"
  "under-warp-ag"
  "under-laser-ag"
  "under-laser-shadow-ag"
  "underb-vis"
  )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; UND
;; ;;;;;;;;;;;;;;;;;;;;;

;; (cgo "UND.DGO" "und.gd")

;; (copy-textures 3045 3048 3049 3047 3276)

;; (copy-gos
;;   "daxter-highres-ag"
;;   "jak-highres-ag"
;;   "sig-highres-ag"
;;   "centipede-fma-ag"
;;   "under-break-bridge-b-ag"
;;   "under-break-bridge-ag"
;;   "com-airlock-inner-ag"
;;   "under-break-door-ag"
;;   "grunt-fma-ag"
;;   "under-break-ceiling-ag"
;;   "under-seaweed-c-ag"
;;   "under-seaweed-b-ag"
;;   "under-seaweed-d-ag"
;;   "under-seaweed-a-ag"
;;   "particleman-ag"
;;   "under-vis"
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; VIN
;; ;;;;;;;;;;;;;;;;;;;;;

(cgo "VIN.DGO" "vin.gd")

(goal-src-sequence
  ""
  :deps ("$OUT/obj/los-control.o")
  "levels/power_station/vinroom-part.gc"
  "levels/power_station/vinroom-scenes.gc"
  "levels/power_station/vinroom-obs.gc"
  )

(copy-textures 778 779 3278 979)

(copy-gos
  "kid-tombd+0-ag"
  ;; "daxter-highres-ag"
  "vin-ag"
  ;; "jak-highres-ag"
  "crocadog-highres-ag"
  "kor-highres-ag"
  ;; "kid-highres-ag"
  "ecowell-a-ag"
  "ecowell-b-ag"
  "ecowell-c-ag"
  "ecowell-d-ag"
  "plasmitebox-ag"
  ;; "warp-gate-ag"
  "vin-door-ag"
  "vin-turbine-ag"
  "pow-mov-plat-ag"
  ;; "palmpilot-ag"
  ;; "particleman-ag"
  "vinroom-vis"
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

;; Custom or Modified Code
(goal-src "pc/pckernel-h.gc" "dma-buffer")
(goal-src "pc/pckernel.gc" "video")

;; used for the type consistency test.
(group-list "all-code"
  `(,@(reverse *all-gc*))
  )

(group "engine"
       "$OUT/iso/KERNEL.CGO"
       "$OUT/iso/GAME.CGO"
       )
