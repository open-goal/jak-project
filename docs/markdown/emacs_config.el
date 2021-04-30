;; make gc files use lisp-mode
(add-to-list 'auto-mode-alist '("\\.gc\\'" . lisp-mode))
;; run setup-goal when we enter lisp mode
(add-hook 'lisp-mode-hook 'setup-goal)

(defun setup-goal ()
  ;; if we are in a gc file, change indent settings for GOAL
  (when (and (stringp buffer-file-name)
             (string-match "\\.gc\\'" buffer-file-name))
    (put 'with-pp      'common-lisp-indent-function 0)
    (put 'while        'common-lisp-indent-function 1)
    (put 'rlet         'common-lisp-indent-function 1)
    (put 'until        'common-lisp-indent-function 1)
    (put 'countdown    'common-lisp-indent-function 1)
    (put 'defun-debug  'common-lisp-indent-function 2)
    (put 'defenum      'common-lisp-indent-function 2)

    ;; indent for common lisp, this makes if's look nicer
    (custom-set-variables '(lisp-indent-function 'common-lisp-indent-function))
    (autoload 'common-lisp-indent-function "cl-indent" "Common Lisp indent.")
    ;; use spaces, not tabs
    (setq-default indent-tabs-mode nil)
    )
  )
