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