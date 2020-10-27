;;; Srefactor --- A refactoring tool based on Semantic parser framework
;;
;; Filename: srefactor-lisp.el
;; Description: A refactoring tool based on Semantic parser framework
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Tu, Do Hoang
;; Created: Wed Feb 11 21:25:51 2015 (+0700)
;; Version: 0.3
;; Package-Requires: ((emacs "24.3+"))
;; Last-Updated: Wed Feb 11 21:25:51 2015 (+0700)
;;           By: Tu, Do Hoang
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords: emacs-lisp, languages, tools
;; Compatibility: GNU Emacs: 24.3+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Semantic is a package that provides a framework for writing
;; parsers. Parsing is a process of analyzing source code based on
;; programming language syntax. This package relies on Semantic for
;; analyzing source code and uses its results to perform smart code
;; refactoring that based on code structure of the analyzed language,
;; instead of plain text structure.
;;
;; This package provides the following features for Emacs Lisp:
;;
;; - `srefactor-lisp-format-buffer': Format whole buffer.
;; - `srefactor-lisp-format-defun': Format the current defun point is in.
;; - `srefactor-lisp-one-line': Transform all sub-sexpressions current sexpression at
;; point into one line separated each one by a space.
;;
;; - `srefactor-lisp-format-sexp': Transform all sub-sexpressions current sexpression
;; at point into multiple lines separated. If the head symbol belongs to the
;; list `srefactor-lisp-symbol-to-skip', then the first N next symbol/sexpressions
;; (where N is the nummber associated with the head symbol as stated in the
;;  list) are skipped before a newline is inserted.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'semantic/bovine/el)

(defcustom srefactor-newline-threshold 40
  "If a token is about to be inserted, if the current posistion
  exceeds this threshold characters, insert the token in the next
  line isntead. Note that this does not account for indentation
  but the total number of characters in a line."
  :group 'srefactor)

(defcustom srefactor-lisp-symbol-to-skip '(("progn" . 0)
                                           ("cond" . 0)
                                           ("save-excursion" . 0)
                                           ("unwind-protect" . 0)
                                           ("with-temp-buffer" . 0)
                                           ;; ("condition-case" . 1)
                                           ;; ("with-current-buffer" . 1)
                                           ;; ("with-open-file" . 1)
                                           ;; ("let" . 1)
                                           ;; ("let*" . 1)
                                           ;; ("if" . 1)
                                           ;; ("while" . 1)
                                           ;; ("dolist" . 1)
                                           ;; ("do" . 1)
                                           ;; ("when" . 1)
                                           ;; ("buffer-substring-no-properties" . 1)
                                           ;; ("unless" . 1)
                                           ;; ("not" . 1)
                                           ;; ("null" . 1)
                                           ;; ("null?" . 1)
                                           ;; ("concat" . 1)
                                           ;; ("or" . 1)
                                           ;; ("and" . 1)
                                           ;; ("catch" . 1)
                                           ;; ("mapcar" . 1)
                                           ;; ("mapcan" . 1)
                                           ;; ("mapc" . 1)
                                           ;; ("+" . 1)
                                           ;; ("-" . 1)
                                           ;; ("*" . 1)
                                           ;; ("/" . 1)
                                           ;; ("error" . 1)
                                           ;; ("goto-char" . 1)
                                           ;; ("insert" . 1)
                                           ;; ("car" . 1)
                                           ;; ("cdr" . 1)
                                           ;; ("lambda" . 1)
                                           ;; ("1+" . 1)
                                           ;; ("1-" . 1)
                                           ("defmethod" . 1)
                                           ("deftype" . 2)
                                           ("cons" . 2)
                                           ("kill-region" . 2)
                                           ("equal" . 2)
                                           ("member" . 2)
                                           ("eq?" . 2)
                                           ("eq" . 2)
                                           ("get" . 2)
                                           ("assoc" . 2)
                                           ("defun" . 2)
                                           ("defclass" . 2)
                                           ("defstruct" . 2)
                                           ("defmacro" . 2)
                                           ("defsubst" . 2)
                                           ("defface" . 2)
                                           ("defalias" . 2)
                                           ("defcustom" . 2)
                                           ("declare" . 2)
                                           ("defvar" . 2)
                                           ("defparameter" . 2)
                                           ("defconst" . 2)
                                           ("string-match" . 2)
                                           ("defcustom" . 2)
                                           ("setq" . 2)
                                           ("setq-default" . 2)
                                           ("member" . 2)
                                           ("setf" . 2)
                                           (">" . 2)
                                           ("<" . 2)
                                           ("<=" . 2)
                                           (">=" . 2)
                                           ("/=" . 2)
                                           ("=" . 2)
                                           ("some" . 2)
                                           ("define-key" . 3)
                                           ("modify-syntax-entry" . 3))
  "A list of pairs of a symbol and a number that denotes how many
  sexp to skip before inserting the first newline. "
  :group 'srefactor)

(defcustom srefactor-clojure-symbol-to-skip '(("fn" . 1)
                                              ("ns" . 1)
                                              (":require" . 1)
                                              (":import" . 1)
                                              ("def" . 2)
                                              ("struct-map" . 1)
                                              ("defmacro" . 1)
                                              ("binding" . 1)
                                              ("with-bindings" . 1)
                                              ("doseq" . 1)
                                              ("catch" . 2)
                                              ("defn" . 2))
  "A list of pairs of a symbol and a number that denotes how many
  sexp to skip before inserting a newline. This will be merged
  with `srefactor-lisp-symbol-to-skip'. Symbols in this list
  overrides symbols in `srefactor-lisp-symbol-to-skip'."
  :group 'srefactor)

;; Internal variables of parser state
(defvar token nil)
(defvar token-type nil)
(defvar token-str nil)
(defvar ignore-num nil)
(defvar tok-start nil)
(defvar next-token nil)
(defvar next-token-start nil)
(defvar next-token-end nil)
(defvar next-token-type nil)
(defvar next-token-str nil)
(defvar tok-end nil)
(defvar cur-buf nil)
(defvar first-token nil)
(defvar first-token-name nil)
(defvar second-token nil)
(defvar lexemes nil)
(defvar comment-token nil)
(defvar comment-content nil)
(defvar token-real-line nil)
(defvar next-token-real-line nil)
(defvar comment-real-line-start nil)
(defvar comment-real-line-end nil)
(defvar comment-token-start nil)
(defvar comment-token-end nil)
(defvar format-type nil)
(defvar recursive-p nil)
(defvar orig-format-type nil)


(defun srefactor--appropriate-major-mode (major-mode)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (emacs-lisp-mode))
   ((eq major-mode 'scheme-mode)
    (scheme-mode))
   ((eq major-mode 'common-lisp-mode)
    (common-lisp-mode))
   ((and (fboundp 'clojure-mode)
         (eq major-mode 'clojure-mode))
    (clojure-mode))
   (t (emacs-lisp-mode))))

(defun srefactor--define-skip-list-for-mode (major-mode)
  (cond
   ((and (fboundp 'clojure-mode)
         (eq major-mode 'clojure-mode))
    (cl-remove-duplicates (append srefactor-lisp-symbol-to-skip srefactor-clojure-symbol-to-skip)
                          :test (lambda (a b)
                                  (equal (car a) (car b)))))
   (t srefactor-lisp-symbol-to-skip)))

(defun srefactor-lisp-format-buffer ()
  "Format current buffer."
  (interactive)
  (let ((cur-pos (point))
        (buf-content (buffer-substring-no-properties (point-min)
                                                     (point-max)))
        (cur-major-mode major-mode)
        (orig-skip-list srefactor-lisp-symbol-to-skip)
        (cur-indent-mode indent-tabs-mode))
    (setq buf-content (with-temp-buffer
                        (semantic-default-elisp-setup)
                        (emacs-lisp-mode)
                        (setq indent-tabs-mode cur-indent-mode)
                        (setq srefactor-lisp-symbol-to-skip (srefactor--define-skip-list-for-mode cur-major-mode))
                        (semantic-lex-init)
                        (insert buf-content)
                        (goto-char (point-max))
                        (while (beginning-of-defun-raw)
                          (let ((beg (point))
                                (end (save-excursion
                                       (forward-sexp)
                                       (point))))
                            (srefactor--lisp-format-one-or-multi-lines
                             beg end beg 'multi-line nil t)
                            (goto-char beg)))
                        (srefactor--appropriate-major-mode cur-major-mode)
                        (indent-region (point-min)
                                       (point-max))
                        (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                        (buffer-substring-no-properties (point-min)
                                                        (point-max))))
    (kill-region (point-min) (point-max))
    (insert buf-content)
    (goto-char cur-pos)))

(defun srefactor-lisp-format-defun ()
  "Format current defun point is in."
  (interactive)
  (let* ((orig-point (point))
         (beg (save-excursion
                (forward-char 1)
                (beginning-of-defun-raw)
                (point)))
         (end (save-excursion
                (goto-char beg)
                (forward-sexp)
                (point)))
         (orig-skip-list srefactor-lisp-symbol-to-skip)
         (cur-indent-mode indent-tabs-mode)
         (cur-major-mode major-mode)
         (content (buffer-substring-no-properties beg end)))
    (progn
      (setq content (with-temp-buffer
                      (semantic-default-elisp-setup)
                      (emacs-lisp-mode)
                      (setq indent-tabs-mode cur-indent-mode)
                      (setq srefactor-lisp-symbol-to-skip (srefactor--define-skip-list-for-mode cur-major-mode))
                      (semantic-lex-init)
                      (insert content)
                      (srefactor--lisp-format-one-or-multi-lines (point-min)
                                                                 (point-max)
                                                                 (point-min)'multi-line
                                                                 nil
                                                                 t)
                      (srefactor--appropriate-major-mode cur-major-mode)
                      (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                      (indent-region (point-min)
                                     (point-max))
                      (buffer-substring-no-properties (point-min)
                                                      (point-max))))
      (kill-region beg end)
      (insert content)
      (goto-char orig-point))))

(defun srefactor-lisp-format-sexp ()
  "Transform all sub-sexpressions current sexpression at point
into multiple lines separatedly. If the head symbol belongs to the
list `srefactor-lisp-symbol-to-skip', then the first N next
symbol/sexpressions (where N is the nummber associated with the
head symbol as stated in the list) are skipped before a newline
is inserted."
  (interactive)
  (let* ((orig-point (point))
         (beg (save-excursion
                (unless (looking-at "[({[]")
                  (backward-up-list))
                (point)))
         (end (save-excursion
                (goto-char beg)
                (forward-sexp)
                (point)))
         (orig-skip-list srefactor-lisp-symbol-to-skip)
         (cur-indent-mode indent-tabs-mode)
         (cur-major-mode major-mode)
         (content (buffer-substring-no-properties beg end)))
    (progn
      (setq content (with-temp-buffer
                      (semantic-default-elisp-setup)
                      (emacs-lisp-mode)
                      (setq indent-tabs-mode cur-indent-mode)
                      (setq srefactor-lisp-symbol-to-skip (srefactor--define-skip-list-for-mode cur-major-mode))
                      (semantic-lex-init)
                      (insert content)
                      (srefactor--lisp-format-one-or-multi-lines (point-min)
                                                                 (point-max)
                                                                 (point-min)'multi-line
                                                                 nil
                                                                 t)
                      (srefactor--appropriate-major-mode cur-major-mode)
                      (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                      (buffer-substring-no-properties (point-min)
                                                      (point-max))))
      (kill-region beg end)
      (insert content)
      (goto-char beg)
      (forward-sexp)
      (setq end (point))
      (indent-region beg end)
      (goto-char orig-point))))

(defun srefactor-lisp-one-line (recursive-p)
  "Transform all sub-sexpressions current sexpression at point
into one line separated each one by a space."
  (interactive "P")
  (let* ((orig-point (point))
         (beg (save-excursion
                (unless (looking-at "[({[]")
                  (backward-up-list))
                (point)))
         (end (save-excursion
                (goto-char beg)
                (forward-sexp)
                (point)))
         (orig-skip-list srefactor-lisp-symbol-to-skip)
         (cur-indent-mode indent-tabs-mode)
         (cur-major-mode major-mode)
         (content (buffer-substring-no-properties beg end)))
    (progn
      (setq content (with-temp-buffer
                      (semantic-default-elisp-setup)
                      (emacs-lisp-mode)
                      (setq indent-tabs-mode cur-indent-mode)
                      (setq srefactor-lisp-symbol-to-skip (srefactor--define-skip-list-for-mode cur-major-mode))
                      (semantic-lex-init)
                      (insert content)
                      (srefactor--lisp-format-one-or-multi-lines (point-min)
                                                                 (point-max)
                                                                 (point-min)'one-line
                                                                 nil
                                                                 recursive-p)
                      (srefactor--appropriate-major-mode cur-major-mode)
                      (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                      (indent-region (point-min)
                                     (point-max))
                      (buffer-substring-no-properties (point-min)
                                                      (point-max))))
      (kill-region beg end)
      (insert content)
      (goto-char orig-point))))

(defun srefactor--lisp-format-one-or-multi-lines (beg end orig-point format-type &optional
                                                      newline-betwen-semantic-lists recursive-p)
  "Turn the current sexpression into one line/multi-line depends
on the value of FORMAT-TYPE. If FORMAT-TYPE is 'one-line,
transforms all sub-sexpressions of the same level into one
line. If FORMAT-TYPE is 'multi-line, transforms all
sub-sexpressions of the same level into multiple lines.

Return the position of last closing sexp."
  (let* ((lexemes (semantic-emacs-lisp-lexer beg end 1))
         (cur-buf (current-buffer))
         (first-token (cadr lexemes))
         (first-token-name (srefactor--lisp-token-text first-token))
         (second-token (caddr lexemes))
         (tmp-buf (generate-new-buffer (make-temp-name "")))
         (orig-format-type format-type)
         token-str
         ignore-pair
         ignore-num
         token)
    (unwind-protect
        (progn
          (unless (assoc 'semantic-list lexemes)
            (setq format-type 'one-line))
          (if (or (eq (car first-token) 'semantic-list)
                  (assoc first-token-name srefactor-lisp-symbol-to-skip))
              (setq newline-betwen-semantic-lists t))
          (setq ignore-pair (assoc first-token-name srefactor-lisp-symbol-to-skip))
          (setq ignore-num (cdr ignore-pair))
          (while lexemes
            (let* (token-type tok-start tok-end next-token next-token-start
                              next-token-type next-token-str)
              (srefactor--lisp-forward-token)
              (with-current-buffer tmp-buf
                (insert token-str)
                (srefactor--lisp-comment-formatter)
                (cond
                 ((and (eq token-type 'number)
                       (member next-token-str '("+" "-" "*" "/")))
                  (srefactor--lisp-number-formatter))
                 ((or (eq token-type 'punctuation)
                      (eq token-type 'open-paren)
                      (eq token-type 'close-paren)
                      (eq next-token-type 'close-paren))
                  (srefactor--lisp-punctuation-formatter))
                 ((eq token-type 'symbol)
                  (srefactor--lisp-symbol-formatter))
                 ((eq format-type 'one-line)
                  (srefactor--lisp-oneline-formatter))
                 ((eq format-type 'multi-line)
                  (srefactor--lisp-multiline-formatter))))))
          (kill-region beg end)
          (setq beg (point))
          (insert (with-current-buffer tmp-buf
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))
          (setq end (point))
          ;; descend into sub-sexpressions
          (setq lexemes (semantic-emacs-lisp-lexer beg end 1))
          (when recursive-p
            (srefactor--lisp-visit-semantic-list-lex (nreverse lexemes))))
      (kill-buffer tmp-buf))))

(defun srefactor--lisp-number-formatter ()
  "Make use of dynamic scope of its parent
function `srefactor--lisp-format-one-or-multi-lines'"
  (goto-char (semantic-lex-token-end token))
  (insert next-token-str)
  (srefactor--lisp-comment-formatter)
  (insert " ")
  (setq first-token (semantic-lex-token 'symbol
                                        (semantic-lex-token-start token)
                                        (1+ (semantic-lex-token-end token))))
  (setq first-token-name (concat token-str next-token-str))
  (setq second-token (cadr lexemes))
  (srefactor--lisp-forward-token))

(defun srefactor--lisp-punctuation-formatter ()
  "Make use of dynamic scope of its parent
function `srefactor--lisp-format-one-or-multi-lines'"
  (let ((orig-token token)
        token
        token-str)
    (while (srefactor--lisp-token-in-punctuation-p (srefactor--lisp-forward-token))
      (insert token-str)
      (srefactor--lisp-comment-formatter))
    (when (eq first-token-name (srefactor--lisp-token-text orig-token))
      (srefactor--lisp-forward-first-second-token))
    (when token
      (push token lexemes))))

(defun srefactor--lisp-symbol-formatter ()
  "Insert additional text based on symbol appearance. Make use of
dynamic scope of its parent function `srefactor--lisp-format-one-or-multi-lines'"
  (cond
   ((and (not (equal token-str first-token-name))
         (eq orig-format-type 'multi-line)
         (string-match ":.*" token-str))
    (insert " ")
    (srefactor--lisp-forward-token)
    (while (member token-type '(punctuation open-paren semantic-list))
      (insert token-str)
      (srefactor--lisp-forward-token))
    (insert token-str)
    (cond
     ((or (equal next-token-str "}"))
      (insert next-token-str "\n" " ")
      (srefactor--lisp-comment-formatter)
      (srefactor--lisp-forward-token))
     ((not (or (srefactor--lisp-token-in-punctuation-p next-token)
               (null next-token)))
      (insert "\n"))
     (t)))
   ;; TODO / NOTE - commas are omitted from the symbol formatter it seems, this breaks quasiquotes with the format of `,@`
   ;; this isn't the best workaround, but it is the smallest. Alternatives include a two-pass format or a larger refactor here.
   ;; Hopefully this doesn't result in new edge-cases! be warned!
   ((member token-str '("@" "~@" "?")) "") 
   ((string-equal token-str ".") (insert " "))
   ((eq format-type 'one-line)
    (srefactor--lisp-oneline-formatter))
   ((eq format-type 'multi-line)
    (srefactor--lisp-multiline-formatter))))

(defun srefactor--lisp-forward-first-second-token ()
  (setq first-token token)
  (setq first-token-name (srefactor--lisp-token-text first-token))
  (setq second-token (car lexemes)))

(defun srefactor--lisp-forward-token ()
  (setq token (pop lexemes))
  (when token
    (setq token-type (semantic-lex-token-class token))
    (setq tok-start (semantic-lex-token-start token))
    (setq tok-end (semantic-lex-token-end token))
    (setq token-str (srefactor--lisp-token-text token))
    (setq next-token (car lexemes))
    (setq next-token-type (semantic-lex-token-class next-token))
    (setq next-token-start (semantic-lex-token-start next-token))
    (setq next-token-end (semantic-lex-token-end next-token))
    (setq next-token-str (if next-token
                             (srefactor--lisp-token-text next-token)
                           ""))
    token))

(defun srefactor--lisp-comment-formatter ()
  (let (comment-token comment-token-start comment-token-end
                      comment-content next-token-real-line token-real-line
                      comment-real-line-start comment-real-line-end)
    (when (and tok-end next-token-start)
      (setq comment-token (with-current-buffer cur-buf ;; asdf
                            (condition-case nil
                                (car (semantic-comment-lexer tok-end next-token-start))
                              (error nil))))
      (when comment-token
        (setq comment-content (with-current-buffer cur-buf
                                ;; set values inside the buffer to avoid global variable
                                (setq comment-token-start (semantic-lex-token-start comment-token))
                                (setq comment-token-end (semantic-lex-token-end comment-token))
                                (setq comment-real-line-start (line-number-at-pos comment-token-start))
                                (setq comment-real-line-end (line-number-at-pos comment-token-end))
                                (setq token-real-line (line-number-at-pos tok-end))
                                (setq next-token-real-line (line-number-at-pos next-token-start))
                                (buffer-substring-no-properties comment-token-start
                                                                comment-token-end)))
        (cond
         ;; if comment token is next to a string, chances are it is below the
         ;; docstring. Add a newlien in between.
         ((eq token-type 'string)
          (insert "\n" comment-content))
         ((= token-real-line comment-real-line-start)
          (insert " " comment-content))
         ((not (= token-real-line comment-real-line-start))
          (insert "\n" comment-content))
         (t))
        ;; If the current/next token is a punctuation (open/close paren,
        ;; punctuation) add a newline no matter what; otherwise it destroys the
        ;; layout of sexp because nonewline is inserted after the current/next
        ;; token and it will be in the same line with the just inserted comment
        ;; and be part of it, which is dangerous
        (when (or (srefactor--lisp-token-in-punctuation-p token)
                  (srefactor--lisp-token-in-punctuation-p next-token)
                  (string-match "[]}]" token-str))
          (insert "\n"))))))

(defun srefactor--lisp-oneline-formatter ()
  (unless (srefactor--lisp-token-in-punctuation-p token)
    (let ((distance (- (point)
                       (line-beginning-position))))
      (if (or (eq orig-format-type 'one-line)
              (<= distance srefactor-newline-threshold))
          (insert " ")
        (insert "\n")))))

(defun srefactor--lisp-multiline-formatter ()
  (cond
   (ignore-num (when (and (equal first-token-name token-str))
                 (insert " ")
                 (when (and ignore-num
                            (= ignore-num 0))
                   (setq ignore-num (1- ignore-num))))
               (while (> ignore-num 0)
                 (if (srefactor--lisp-token-paren-p token)
                     (progn
                       (delete-char -1)
                       (push token lexemes)
                       (setq ignore-num 0))
                   (srefactor--lisp-forward-token)
                   (insert token-str)
                   (srefactor--lisp-comment-formatter)
                   (if (srefactor--lisp-token-in-punctuation-p token)
                       (srefactor--lisp-forward-first-second-token)
                     (setq ignore-num (1- ignore-num))
                     (insert " "))))
               (delete-char -1)
               (if (srefactor--lisp-token-paren-p (car lexemes))
                   (srefactor--lisp-punctuation-formatter)
                 (insert "\n"))
               (setq ignore-num nil))
   ((and (equal first-token-name token-str)
         (not (eq next-token-type 'semantic-list)))
    (insert " "))
   ((and (eq next-token-type 'semantic-list)
         (eq token-type 'symbol)
         (equal first-token-name token-str))
    (insert " "))
   ((eq token-type 'semantic-list)
    (insert "\n"))
   ((or (null ignore-num)
        (= ignore-num 0))
    (insert "\n"))
   (t (insert "\n"))))

(defun srefactor--lisp-token-name-in-skip-list-p (token-name)
  (member token-name srefactor-lisp-symbol-to-skip))

(defun srefactor--lisp-token-in-punctuation-p (token)
  (member (semantic-lex-token-class token) '(open-paren charquote close-paren punctuation)))

(defun srefactor--lisp-token-paren-p (token)
  (member (semantic-lex-token-class token) '(open-paren close-paren)))

(defun srefactor--lisp-token-text (token)
  (if token
      (with-current-buffer cur-buf
        (buffer-substring-no-properties (semantic-lex-token-start token)
                                        (semantic-lex-token-end token)))
    ""))

(defun srefactor--lisp-visit-semantic-list-lex (lexemes)
  "Visit and format all sub-sexpressions (semantic list) in LEXEMES."
  (dolist (token lexemes)
    (let ((tok-start (semantic-lex-token-start token))
          (tok-end (semantic-lex-token-end token))
          tok-str)
      (when (and (eq (car token) 'semantic-list)
                 (> (- tok-end tok-start) 2))
        (goto-char (semantic-lex-token-start token))
        (srefactor--lisp-format-one-or-multi-lines tok-start
                                                   tok-end
                                                   tok-start
                                                   format-type
                                                   (assoc tok-str srefactor-lisp-symbol-to-skip)
                                                   recursive-p)))))

(defun srefactor--lisp-comment-debug-messages ()
  (message "comment-token: %s" comment-token)
  (message "comment-start: %s" comment-token-start)
  (message "comment-end: %s" comment-token-end)
  (message "comment-content: %s" comment-content)
  (message "comment-content: %s" comment-content)
  (message "token-real-line: %s" token-real-line)
  (message "next-token-real-line: %s" next-token-real-line)
  (message "comment-real-line-start: %s" comment-real-line-start)
  (message "comment-real-line-end %s" comment-real-line-end))

(defun srefactor--lisp-debug-messages ()
  (message "token: %s" token)
  (message "token-type: %s" token-type)
  (message "token-str: %s" token-str)
  (when ignore-num
    (message "ignore-num: %s" ignore-num))
  (message "next-token: %s" next-token)
  (message "next-token-type: %s" next-token-type)
  (message "next-token-str: %s" next-token-str))

(provide 'srefactor-lisp)
