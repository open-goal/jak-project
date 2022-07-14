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
"ps2/pad.gc"
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
"collide/collide-planes.gc"
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
"game/main.gc"
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

  )

(cgo "ENGINE.CGO" "engine.gd")


;;;;;;;;;;;;;;;;;;;;;
;; ISO Group
;;;;;;;;;;;;;;;;;;;;;
;; the iso group is a group of files built by the "(mi)" command.

(group-list "iso"
 `(,@(reverse *all-vis*)
   ,@(reverse *all-str*)
   ,@(reverse *all-sbk*)
   ,@(reverse *all-mus*)
   ,@(reverse *all-vag*)
   ,@(reverse *all-cgos*))
 )

;; used for the type consistency test.
(group-list "all-code"
  `(,@(reverse *all-gc*))
  )
