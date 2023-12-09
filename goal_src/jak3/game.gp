
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

;; NOTE: the game itself will load from out/jak2/iso and out/jak2/fr3.
(map-path! "$OUT" "out/jak3/")

;; tell the compiler to put its outputs in out/jak2/
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
;; ISO Group
;;;;;;;;;;;;;;;;;;;;;
;; the iso group is a group of files built by the "(mi)" command.

(group-list "iso"
 `(
   ; "$OUT/iso/0COMMON.TXT"
   ; "$OUT/iso/1COMMON.TXT"
   ; "$OUT/iso/2COMMON.TXT"
   ; "$OUT/iso/3COMMON.TXT"
   ; "$OUT/iso/4COMMON.TXT"
   ; "$OUT/iso/5COMMON.TXT"
   ; "$OUT/iso/6COMMON.TXT"
   ; "$OUT/iso/7COMMON.TXT"
   ; "$OUT/iso/0SUBTI2.TXT"
   ; "$OUT/iso/TWEAKVAL.MUS"
   ; "$OUT/iso/VAGDIR.AYB"
   ,@(reverse *all-vis*)
   ,@(reverse *all-str*)
   ,@(reverse *all-sbk*)
   ,@(reverse *all-mus*)
   ,@(reverse *all-vag*)
   ,@(reverse *all-cgos*)
   )
 )
