;; Emacs configuration for interacting with OpenGOAL files and compiler.
;; This sets up:
;; - some reasonable defaults for highlighting parens
;; - syntax highlighting
;; - indentation
;; - REPL integration
;; - shortcuts to compile files/s-expressions in the editor

;; To use this config, you can add something like this to your emacs config:
;; the path to the compiler:
;; (defconst opengoal-compiler-path "~/jak-project/build/goalc/goalc")
;; the path to this file (or you can copy this file to your emacs config)
;; (add-to-list 'load-path "~/jak-project/scripts/emacs")
;; load this file!
;; (load "goal-config")

;; Other recommended tools:
;; - projectile: search through all .gc files
;; - doom emacs: make emacs look fancy and have good defaults. It is very slow with default settings.



(defun lispy-parens ()
  "Set up parens in a reasonable way."
  ;; based on https://www.emacswiki.org/emacs/ShowParenMode
  ;; remove the stupid delay when highlighting parens. Emacs is slow enough without intentional delays
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1)
  ;; the default color is hard to see - make it the most painful color possible.
  (custom-set-faces
    '(show-paren-match ((t (:foreground "white" :background "red")))))
  )

;; very simple syntax highlighting (sometimes fails)
;; these are added to lisp-mode's highlighting.
(font-lock-add-keywords 'lisp-mode
                        '(("set!" . font-lock-keyword-face)
                          ("local-vars" . font-lock-keyword-face)
                          ("until" . font-lock-keyword-face)
                          ("car" . font-lock-keyword-face)
                          ("cdr" . font-lock-keyword-face)
                          ("->" . font-lock-keyword-face)
                          ("else" . font-lock-constant-face)
                          ("nop!" . font-lock-builtin-face)
                          ;;("and" . font-lock-keyword-face)
                          ;;("or" . font-lock-keyword-face)
                          ("format" . font-lock-builtin-face)
                          ("defun-debug" . font-lock-keyword-face)
                          ("defbehavior" . font-lock-keyword-face)
                          ("defenum" . font-lock-keyword-face)
                          ("begin" . font-lock-keyword-face)
                          ("zero?" . font-lock-builtin-face)
                          ("#f" . font-lock-constant-face)
                          ("#t" . font-lock-constant-face)))

;; more advanced syntax highlighting. These still fail sometimes, but look nicer.
(defconst scheme-font-lock-keywords-1
          (eval-when-compile
            (list
              ;;
              ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
              ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
              (list (concat "(\\(define\\*?\\("
                            ;; Function names.
                            "\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|"
                            ;; Macro names, as variable names.  A bit dubious, this.
                            "\\(-syntax\\|-macro\\)\\|"
                            ;; Class names.
                            "-class"
                            ;; Guile modules.
                            "\\|-module"
                            "\\)\\)\\>"
                            ;; Any whitespace and declared object.
                            ;; The "(*" is for curried definitions, e.g.,
                            ;;  (define ((sum a) b) (+ a b))
                            "[ \t]*(*"
                            "\\(\\sw+\\)?")
                    '(1 font-lock-keyword-face)
                    '(6 (cond ((match-beginning 3) font-lock-function-name-face)
                              ((match-beginning 5) font-lock-variable-name-face)
                              (t font-lock-type-face))
                      nil t))
              ))
          "Subdued expressions to highlight in Scheme modes.")
(font-lock-add-keywords 'lisp-mode scheme-font-lock-keywords-1)

;; make gc files use lisp-mode
(add-to-list 'auto-mode-alist '("\\.gc\\'" . lisp-mode))
;; run setup-goal when we enter lisp mode.
(add-hook 'lisp-mode-hook 'setup-goal)

(defun setup-goal ()
  "Check if we are in a GOAL file. If so, change settings for GOAL."
  (when (and (stringp buffer-file-name)
             (string-match "\\.gc\\'" buffer-file-name))

    ;; set up indentation for GOAL keywords
    (put 'with-pp      'common-lisp-indent-function 0)
    (put 'while        'common-lisp-indent-function 1)
    (put 'goal-src     'common-lisp-indent-function 1)
    (put 'engine-src   'common-lisp-indent-function 1)
    (put 'rlet         'common-lisp-indent-function 1)
    (put 'until        'common-lisp-indent-function 1)
    (put 'countdown    'common-lisp-indent-function 1)
    (put 'defun-debug  'common-lisp-indent-function 2)
    (put 'defenum      'common-lisp-indent-function 2)
    (put 'defbehavior  'common-lisp-indent-function 3)

    ;; indent for common lisp, this makes if's look nicer
    (custom-set-variables '(lisp-indent-function 'common-lisp-indent-function))
    (autoload 'common-lisp-indent-function "cl-indent" "Common Lisp indent.")
    ;; use spaces, not tabs
    (setq-default indent-tabs-mode nil)
    ;; set up parens
    (lispy-parens)
    ;; set up keyboard shortcuts
    (local-set-key (kbd "C-x C-t") 'opengoal-send-to-repl)
    (local-set-key (kbd "C-x C-g") 'opengoal-ml-file)
    )
  )

;; this will make some but not all colors work.
;; I think only very simple "terminal color" settings will work in emacs, but
;; OpenGOAL uses fancy xterm color codes in places.
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; OpenGOAL compiler interaction
(defun run-opengoal ()
  "Switch to the OpenGOAL compiler.  Will open a new one if there is none."
  ;; check if we have it running already
  (interactive)
  (let ((buffer (comint-check-proc "OpenGOAL")))
    ;; create or reuse a buffer and make it visible
    (pop-to-buffer
      (if (or buffer (not (derived-mode-p 'opengoal-mode))
              (comint-check-proc (current-buffer)))
        (get-buffer-create (or buffer "*OpenGOAL*"))
        (current-buffer)))

    (unless buffer
      ;; start the compiler
      (ansi-color-for-comint-mode-on)
      (apply 'make-comint-in-buffer "OpenGOAL" buffer
             opengoal-compiler-path '()))))

(defun opengoal-local-path (filename)
  "Convert a file path to one that works in the compiler."
  ;; this is a bit of a hack.
  (string-match "jak-project/*" filename) ;; windows?
  (substring filename (+ (match-beginning 0) 12)))

(defun opengoal-compile-file (filename mode)
  "Send a command to the compiler to compile a file."
  ;; make a command like (m "my-file.gc")
  (let ((cmd (format "(%s \"%s\")\n" mode (opengoal-local-path filename))))
    ;; switch to OpenGOAL buffer
    (run-opengoal)
    ;; send it!
    (comint-send-string "*OpenGOAL*" cmd)))

(defun opengoal-compile-current-file (mode)
  "Check if the current file is saved. If so, compile it!"
  (cond
    ((buffer-modified-p)
     (message "Cannot compile current file: save your changes first!"))
    (t
      (opengoal-compile-file (buffer-file-name) mode))))

(defun opengoal-m-file ()
  "Make the current file."
  (interactive)
  (opengoal-compile-current-file "m"))

(defun opengoal-md-file ()
  "Make and current file and print x86 disassembly."
  (interactive)
  (opengoal-compile-current-file "md"))

(defun opengoal-ml-file ()
  "Make and load the current file."
  (interactive)
  (opengoal-compile-current-file "ml"))

(defun opengoal-send-to-repl ()
  (interactive)
  ;; this is a bit of a hack to work around newline stuff.
  ;; This grabs the text, removes line comments and newlines, and sends it to the REPL.
  (save-excursion
    (beginning-of-defun)
    (let ((start-pos (point)))
      (end-of-sexp)
      (let* ((end-pos (point))
             (text (buffer-substring start-pos end-pos))
             (text-no-comment (replace-regexp-in-string ";;.*$" " " text nil 'literal))
             (flat-string (concat (replace-regexp-in-string "\n" " " text-no-comment nil 'literal) "\n")))
        (comint-send-string "*OpenGOAL*" flat-string)))))
