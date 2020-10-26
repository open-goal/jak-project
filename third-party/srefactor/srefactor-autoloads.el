;;; srefactor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "srefactor" "srefactor.el" (0 0 0 0))
;;; Generated autoloads from srefactor.el

(autoload 'srefactor-refactor-at-point "srefactor" "\
Offer contextual menu with actions based on current tag in scope.

Each menu item added returns a token for what type of refactoring
to perform." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srefactor" '("srefactor-")))

;;;***

;;;### (autoloads nil "srefactor-lisp" "srefactor-lisp.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from srefactor-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srefactor-lisp" '("comment-" "cur-buf" "first-token" "format-type" "ignore-num" "lexemes" "next-token" "orig-format-type" "recursive-p" "second-token" "srefactor-" "tok")))

;;;***

;;;### (autoloads nil "srefactor-ui" "srefactor-ui.el" (0 0 0 0))
;;; Generated autoloads from srefactor-ui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srefactor-ui" '("srefactor-ui-")))

;;;***

;;;### (autoloads nil nil ("srefactor-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; srefactor-autoloads.el ends here
