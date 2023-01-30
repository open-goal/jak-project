;;-*-Lisp-*-

;; TODO extract most of this into a common lib that isn't so game dependent

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

;; TODO - deps should probably just treated as a proper list to refactor duplication 
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

(defun reverse-list (list)
  (let ((new-list '())
        (curr-elt list))
    (while (not (null? curr-elt))
      (set! new-list (cons (car curr-elt) new-list))
      (set! curr-elt (cdr curr-elt)))
    new-list))

(defmacro cgo-file (dgo-file-name deps)
  ;; First read in the DGO file, it has pretty much everything we need
  (let ((dgo-data (car (read-data-file (string-append "goal_src/jak2/dgos/" dgo-file-name)))))
    ;; Get the name of the DGO
    (let ((dgo-name (car dgo-data))
          (files (cdr dgo-data))
          (gsrc-seq-args '())
          (textures '())
          (gos '()))
      ;; create the dgo step
      (cgo dgo-name dgo-file-name)
      ;; Now we iterate through the list of files, skipping ones we've already processed
      ;; and creating steps for the ones that are new!
      (while (not (null? files))
        (let ((file-name (car (car files)))
              (obj-name (car (cdr (car files)))))
          ;; Check to see if we've already handled this file
          (when (not (car (hash-table-try-ref *file-entry-map* file-name)))
            ;; Depending on the type of file, generate the appropriate steps
            (cond
              ((string-ends-with? file-name ".o")
               ;; build up a list of all gsrc files needing to be compiled
               (let ((gsrc-path (get-gsrc-path obj-name)))
                (set! gsrc-seq-args (cons gsrc-path gsrc-seq-args))))
              ((string-starts-with? obj-name "tpage-")
               ;; copy textures
               (let ((tpage-id (car (cdr (string-split obj-name "-")))))
                (set! textures (cons tpage-id textures))))
              ((string-ends-with? file-name ".go")
               ;; copy art files
               (set! gos (cons (stem file-name) gos))))
            ;; Update the map so this file isn't processed again
            (hash-table-set! *file-entry-map* file-name #f)))
        (set! files (cdr files)))
      ;; TODO - need an `append`!, reverse lists by re-cons'ing them for now
      (set! gsrc-seq-args (reverse-list gsrc-seq-args))
      (set! textures (reverse-list textures))
      (set! gos (reverse-list gos))
      `(begin
        ;; macros can't return nothing, so these macros assume they will be given a non-empty list
        (when (not (null? '(,@gsrc-seq-args)))
          (goal-src-sequence "" :deps ,deps ,@gsrc-seq-args))
        (when (not (null? '(,@textures)))
          (copy-textures ,@textures))
        (when (not (null? '(,@gos)))
          (copy-gos ,@gos)))
      )
    ))
