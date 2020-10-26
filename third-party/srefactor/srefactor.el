;;; srefactor.el --- A refactoring tool based on Semantic parser framework
;;
;; Filename: srefactor.el
;; Description: A refactoring tool based on Semantic parser framework
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Tu, Do Hoang
;; Created: Wed Feb 11 21:25:51 2015 (+0700)
;; Version: 0.3
;; Package-Requires: ((emacs "24.4"))
;; Last-Updated: Wed Feb 11 21:25:51 2015 (+0700)
;;           By: Tu, Do Hoang
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords: c, languages, tools
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
;; To use this package, user only needs to use this single command:
;; `srefactor-refactor-at-point'
;;
;; This package includes the following features:
;;
;; - Context-sensitive menu: when user runs the command, a menu
;; appears and offer refactoring choices based on current scope of
;; semantic tag. For example, if the cursor is inside a class, the
;; menu lists choices such as generate function implementations for
;; the class, generate class getters/setters... Each menu item also
;; includes its own set of options, such as perform a refactoring
;; option in current file or other file.
;;
;; - Generate class implementation: From the header file, all function
;; prototypes of a class can be generated into corresponding empty
;; function implementation in a source file. The generated function
;; implementations also include all of their (nested) parents as
;; prefix in the names, if any. If the class is a template, then the
;; generated functions also includes all templates declarations and in
;; the parent prefix properly.
;;
;; - Generate function implementation: Since all function
;; implementations can be generated a class, this feature should be
;; present.
;;
;; - Generate function prototype: When the cursor is in a function
;; implementation, a function prototype can be generated and placed in
;; a selected file. When the prototype is moved into, its prefix is
;; stripped.
;;
;; - Convert function to function pointer: Any function can be
;; converted to a function pointer with typedef. The converted
;; function pointer can also be placed as a parameter of a function.
;; In this case, all the parameter names of the function pointer is
;; stripped.
;;
;; - Move semantic units: any meaningful tags recognized by Semantic
;; (class, function, variable, namespace...) can be moved relative to
;; other tags in current file or any other file.
;;
;; - Extract function: select a region and turn it into a function,
;; with relevant variables turned into function parameters and
;; preserve full type information.
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

(require 'cl-lib)
(require 'cc-mode)
(require 'semantic)
(require 'semantic/tag-ls)
(require 'semantic/bovine/c)
(require 'semantic/format)
(require 'semantic/doc)
(require 'srecode/semantic)
(require 'srefactor-ui)

(if (not (version< emacs-version "24.4"))
    (require 'subr-x)
  (defun string-empty-p (string)
    "Check whether STRING is empty."
    (string= string ""))

  (defun string-trim-left (string)
    "Remove leading whitespace from STRING."
    (if (string-match "\\`[ \t\n\r]+" string)
        (replace-match "" t t string)
      string))

  (defun string-trim-right (string)
    "Remove trailing whitespace from STRING."
    (if (string-match "[ \t\n\r]+\\'" string)
        (replace-match "" t t string)
      string)))

(when (version< emacs-version "25")
  (defalias 'semantic-documentation-comment-preceding-tag 'semantic-documentation-comment-preceeding-tag))

(defvar srefactor--current-local-var nil
  "Current local variable at point")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom srefactor--getter-prefix "get_"
  "Prefix for inserting getter."
  :group 'srefactor)

(defcustom srefactor--setter-prefix "set_"
  "Prefix for inserting getter."
  :group 'srefactor)

(defcustom srefactor--getter-setter-removal-prefix ""
  "Prefix for removing getter and setter."
  :group 'srefactor)

(defcustom srefactor--getter-setter-capitalize-p nil
  "Whether getter and setter should be capitalized."
  :group 'srefactor
  :type 'boolean)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer Options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar srefactor-use-srecode-p nil
  "Use experimental SRecode tag insertion ")

;; MACROS
(defmacro srefactor--is-proto (type)
  `(eq ,type 'gen-func-proto))

(defmacro srefactor--add-menu-item (label operation-type file-options)
  `(add-to-list 'menu-item-list (list ,label
                                      ',operation-type
                                      ,file-options)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands - only one currently
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun srefactor-refactor-at-point ()
  "Offer contextual menu with actions based on current tag in scope.

Each menu item added returns a token for what type of refactoring
to perform."
  (interactive)
  (let* ((semanticdb-find-default-throttle '(file))
         (refresh (semantic-parse-changes-default))
         (srefactor--file-options (srefactor-ui--return-option-list 'file))
         (tag (srefactor--copy-tag))
         (menu (make-instance 'srefactor-ui-menu :name "menu"))
         menu-item-list)
    (setq srefactor--current-local-var (srefactor--menu-add-rename-local-p))
    (when (srefactor--menu-add-function-implementation-p tag)
      (srefactor--add-menu-item "Generate Function Implementation (Other file)"
                                gen-func-impl
                                srefactor--file-options))
    (when (srefactor--menu-add-function-proto-p tag)
      (srefactor--add-menu-item  "Generate Function Prototype (Other file)"
                                 gen-func-proto
                                 srefactor--file-options))
    (when (srefactor--menu-add-function-pointer-p tag)
      (srefactor--add-menu-item "Generate Function Pointer (Current file)"
                                gen-func-ptr
                                srefactor--file-options))
    (when (srefactor--menu-add-getters-setters-p tag)
      (srefactor--add-menu-item  "Generate Getters and Setters (Current file)"
                                 gen-getters-setters
                                 srefactor--file-options))
    (when (srefactor--menu-add-getter-setter-p tag)
      (srefactor--add-menu-item  "Generate Setter (Current file)"
                                 gen-setter
                                 srefactor--file-options)
      (srefactor--add-menu-item  "Generate Getter (Current file)"
                                 gen-getter
                                 srefactor--file-options)
      (srefactor--add-menu-item  "Generate Getter and Setter (Current file)"
                                 gen-getter-setter
                                 srefactor--file-options))
    (when srefactor--current-local-var
      (setq tag srefactor--current-local-var)
      (srefactor--add-menu-item  "Rename local variable (Current file)"
                                 rename-local-var
                                 '("(Current file)")))
    (when (srefactor--menu-add-move-p)
      (srefactor--add-menu-item  "Move (Current file)"
                                 move
                                 srefactor--file-options))
    (when (region-active-p)
      (srefactor--add-menu-item  "Extract function (Current file)"
                                 extract-function
                                 nil))
    (oset menu :items menu-item-list)
    (oset menu :action #'srefactor-ui--refactor-action)
    (oset menu :context tag)
    (oset menu :shortcut-p t)
    (srefactor-ui-create-menu menu)))

(defun srefactor--tag-filter (predicate tag-classes-or-names tags)
  "Filter TAGS based on PREDICATE that satisfies TAG-CLASSES-OR-NAMES.

TAG-CLASSES-OR-NAMES can be a list of Semantic tag classes, or a
list of Semantic tag names, but not both.

Based on the type of list passed above, either use
`semantic-tag-class' or `semantic-tag-name' as PREDICATE."
  (let (l)
    (dolist (tag tags l)
      (when (member (funcall predicate tag) tag-classes-or-names)
        (setq l (cons tag l))))))

(defun srefactor--c-tag-start-with-comment (tag)
  (save-excursion
    (goto-char (semantic-tag-start tag))
    (if (and (search-backward-regexp "/\\*" nil t)
             (semantic-documentation-comment-preceding-tag tag)
             (looking-at "^[ ]*\\/\\*"))
        (progn
          (beginning-of-line)
          (point))
      (semantic-tag-start tag))))

(defun srefactor--copy-tag ()
  "Take the current tag, and place it in the tag ring."
  (semantic-fetch-tags)
  (let ((ft (semantic-current-tag)))
    (when ft
      (ring-insert senator-tag-ring ft)
      (semantic-tag-set-bounds ft
                               (srefactor--c-tag-start-with-comment ft)
                               (semantic-tag-end ft))
      (kill-ring-save (semantic-tag-start ft)
                      (semantic-tag-end ft))
      (when (called-interactively-p 'interactive)
        (message "Use C-y to yank text.  \
Use `senator-yank-tag' for prototype insert.")))
    ft))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level functions that select action to make
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--refactor-based-on-tag-class (operation &optional file-option)
  "Refactor based on current tag in context.

OPERATION is a refactoring type user selected from the menu.
FILE-OPTION is a file destination associated with OPERATION."
  (let* ((refactor-tag (srefactor--copy-tag))
         (class (semantic-tag-class refactor-tag)))
    (cond
     ((eq class 'function)
      (cond
       ((eq operation 'extract-function)
        (srefactor--extract-region 'function))
       ((eq operation 'rename-local-var)
        (let* ((local-var (srefactor--tag-at-point))
               (function-tag (semantic-current-tag))
               (search-start (semantic-tag-start function-tag))
               (search-end (semantic-tag-end function-tag))
               prompt)
          (unwind-protect
              (condition-case nil
                  (let ((tag-occurrences (srefactor--collect-tag-occurrences local-var
                                                                             search-start
                                                                             search-end)))
                    (srefactor--highlight-tag local-var tag-occurrences refactor-tag 'match)
                    (setq prompt (format "Replace (%s) with: " (semantic-tag-name local-var)))
                    (srefactor--rename-local-var local-var
                                                 tag-occurrences
                                                 refactor-tag
                                                 (read-from-minibuffer prompt)))
                (error nil))
            (remove-overlays))))
       (t
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))))
     ((eq class 'type)
      (cond
       ((eq operation 'gen-getters-setters)
        (srefactor-insert-class-getters-setters refactor-tag file-option)
        (message "Getter and setter generated."))
       ((eq operation 'move)
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))
       (t (srefactor--refactor-type (srefactor--contextual-open-file
                                     (srefactor--select-file file-option))
                                    refactor-tag))))
     ((eq class 'variable)
      (cond
       ((eq operation 'gen-getter-setter)
        (let ((buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
          (srefactor--variable-insert-getter-setter t t refactor-tag buffer))
        (message "Getter and setter generated."))
       ((eq operation 'gen-getter)
        (let ((buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
          (srefactor--variable-insert-getter-setter t nil refactor-tag buffer))
        (message "Getter generated."))
       ((eq operation 'gen-setter)
        (let ((buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
          (srefactor--variable-insert-getter-setter nil t refactor-tag buffer))
        (message "Setter generated."))
       ((eq operation 'move)
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))
       (t nil)))
     ((eq class 'package)
      (message "FIXME: 'package refactoring is not yet implemented."))
     ((eq class 'include)
      (message "FIXME: 'include refactoring is not yet implemented."))
     ((eq class 'label)
      (message "FIXME: 'label refactoring is not yet implemented."))
     (t))))

(defun srefactor--select-file (option)
  "Return a file based on OPTION selected by a user."
  (let ((projectile-func-list '(projectile-get-other-files
                                projectile-current-project-files
                                projectile-project-root
                                projectile-find-file))
        other-files file l)
    (when (and (featurep 'projectile)
             (cl-reduce (lambda (acc f)
                          (and (fboundp f) acc))
                        projectile-func-list
                        :initial-value t))
        (cond
         ((string-equal option "(Other file)")
          (condition-case nil
              (progn
                (setq other-files (projectile-get-other-files (buffer-file-name)
                                                              (projectile-current-project-files)
                                                              nil))
                (setq l (length other-files))
                (setq file (concat (projectile-project-root)
                                   (cond ((> l 1)
                                          (completing-read "Select a file: "
                                                           other-files))
                                         ((= l 1)
                                          (car other-files))
                                         (t (projectile-find-file))))))
            (error nil)))
         ((and (string-equal option "(Project file)")
               (featurep 'projectile))
          (setq file (concat (projectile-project-root)
                             (completing-read "Select a file: "
                                              (projectile-current-project-files)))))
         ))

    (when (string-equal option "(Current file)")
      (setq file (buffer-file-name (current-buffer))))

    (when (string-equal option "(File)")
      (setq file (with-current-buffer (call-interactively 'find-file-other-window)
                   (buffer-file-name (current-buffer)))))
    file))

(defun srefactor--tag-persistent-action ()
  "Move to a tag when executed."
  (back-to-indentation)
  (when srefactor-ui--current-active-tag-overlay
    (delete-overlay srefactor-ui--current-active-tag-overlay))
  (let (link tag)
    (save-excursion
      (if (search-forward ":" (line-end-position) t)
          (setq link (get-pos-property (point) 'button))
        (setq link (get-pos-property (point) 'button))))
    (when (and link
               (listp (widget-value link))
               (semantic-tag-p (car (widget-value link))))
      (with-selected-window srefactor-ui--current-active-window
        (setq tag (car (widget-value link)))
        (when (car (widget-value link))
          (semantic-go-to-tag tag)
          (let ((o (make-overlay (semantic-tag-start tag)
                                 (semantic-tag-end tag))))
            (setq srefactor-ui--current-active-tag-overlay o)
            (overlay-put o 'face 'region)))))))

(defun srefactor--refactor-tag (buffer refactor-tag func-type &optional ask-place-p)
  "Refactor a tag.

BUFFER is a buffer from opening selected file chosen from the menu.

REFACTOR-TAG is selected tag to be refactored.

FUNC-TYPE is a refactoring action to be performed.

ASK-PLACE-P, if true, asks user to select a tag in BUFFER to insert next to it."
  (let (dest-tag
        (tag-list (nreverse (srefactor--fetch-candidates))))
    (setq srefactor-ui--func-type func-type)
    (with-current-buffer buffer
      (if (and ask-place-p tag-list)
          (progn
            (oset srefactor-ui--current-active-menu :items tag-list)
            (oset srefactor-ui--current-active-menu :action #'srefactor-ui--tag-action)
            (oset srefactor-ui--current-active-menu :shortcut-p nil)
            (oset srefactor-ui--current-active-menu :persistent-action 'srefactor--tag-persistent-action)
            (oset srefactor-ui--current-active-menu :post-handler
                  (lambda ()
                    (let ((tag (context srefactor-ui--current-active-menu))
                          tag-string)
                      (with-temp-buffer
                        (setq major-mode 'c++-mode)
                        (setq tag-string (semantic-format-tag-summarize tag nil nil)))
                      (search-forward-regexp (regexp-quote tag-string) (point-max) t)
                      (back-to-indentation))))
            (oset srefactor-ui--current-active-menu :keymap
                  (lambda ()
                    (cl-flet ((next (key)
                                    (define-key srefactor-ui-menu-mode-map key
                                      (lambda ()
                                        (interactive)
                                        (widget-forward 1)
                                        (srefactor--tag-persistent-action))))
                              (previous (key)
                                        (define-key srefactor-ui-menu-mode-map key
                                          (lambda ()
                                            (interactive)
                                            (widget-backward 1)
                                            (srefactor--tag-persistent-action)))))
                      (next "n")
                      (next "j")
                      (previous "p")
                      (previous "k"))))
            (srefactor-ui-create-menu srefactor-ui--current-active-menu))
        (srefactor--insert-tag refactor-tag nil func-type)))))

(defun srefactor--refactor-type (dest-buffer refactor-tag)
  "Generate function implementations for all functions in a
class, including functions in nested classes.

DEST-BUFFER is the destination buffer to insert generated code.
REFACTOR-TAG is a Semantic tag that holds information of a C++ class."
  (let* ((members (semantic-tag-type-members refactor-tag))
         (dest-buffer-tags (with-current-buffer dest-buffer
                             (semantic-fetch-tags)))
         (diff (set-difference members
                               dest-buffer-tags
                               :test #'semantic-equivalent-tag-p))
         )
    (dolist (tag diff)
      (cond
       ((and (eq (semantic-tag-class tag) 'function)
             (semantic-tag-prototype-p tag))
        (srefactor--refactor-tag dest-buffer tag 'gen-func-impl))
       ((eq (semantic-tag-class tag) 'type)
        (srefactor--refactor-type dest-buffer tag))
       (t)))))

(defun srefactor--insert-tag (refactor-tag dest-tag insert-type &optional pos)
  "Insert a Semantic TAG to current buffer.

REFACTOR-TAG is selected Semantic tag to be refactored.

DEST-TAG is destination tag for refactored tag to be inserted
next to it. If nil, insert at the end of file.

POS is specific relative position to be inserted. POS is one of
the option \"Before|Inside|After\" that appears when a
destination tag can have its own members, such as a class or a
namespace.
"
  (let* ((parent-is-func-p (eq (semantic-tag-class (semantic-tag-calculate-parent dest-tag))
                               'function))
         (class (semantic-tag-class refactor-tag))
         beg end)

    ;; if  refactor-tag dest-tag is nil, just insert at end of file
    (if dest-tag
        (progn
          (semantic-go-to-tag dest-tag)

          (if parent-is-func-p
              (srefactor--insert-function-as-parameter refactor-tag)

            ;; Handle selected position
            (cond
             ((string-equal pos "(Before)")
              (open-line 1))
             ((string-equal pos "(Inside)")
              (search-forward "{")
              (newline 1))
             (t (goto-char (semantic-tag-end dest-tag))
                (forward-line 1)))

            ;; handle insert type
            (cond
             ((eq insert-type 'gen-func-ptr)
              (srefactor--insert-function-pointer refactor-tag)
              (newline-and-indent)
              (recenter))
             ((or (eq insert-type 'gen-func-impl) (eq insert-type 'gen-func-proto))
              (if (region-active-p)
                  (mapc (lambda (f-t)
                          (srefactor--insert-function f-t insert-type))
                        (semantic-parse-region (region-beginning) (region-end)))
                (srefactor--insert-function refactor-tag insert-type)))
             ((srefactor--tag-pointer refactor-tag)
              (semantic-insert-foreign-tag (srefactor--function-pointer-to-function refactor-tag)))
             ((eq insert-type 'move)
              (with-current-buffer (semantic-tag-buffer refactor-tag)
                (save-excursion
                  (goto-char (semantic-tag-start refactor-tag))
                  (delete-region (semantic-tag-start refactor-tag)
                                 (semantic-tag-end refactor-tag))
                  (delete-blank-lines)))
              (if (and (or (srefactor--tag-struct-p dest-tag)
                           (srefactor--tag-struct-p
                            (srefactor--calculate-parent-tag dest-tag)))
                       (eq class 'function)
                       (eq major-mode 'c-mode))
                  (progn
                    (insert (srefactor--function-to-function-pointer refactor-tag))
                    (insert ";"))
                (delete-trailing-whitespace)
                (if (eq class 'function)
                    (srefactor--insert-function refactor-tag (if (semantic-tag-prototype-p refactor-tag)
                                                                 'gen-func-proto
                                                               'gen-func-proto))
                  (setq beg (point))
                  (yank)
                  (insert "\n")
                  (setq end (point))
                  (indent-region beg end))))
             (t (senator-yank-tag)))))
      (goto-char (point-max))
      (cond
       ((eq insert-type 'gen-func-ptr)
        (srefactor--insert-function-pointer refactor-tag))
       ((eq insert-type 'gen-func-impl)
        (srefactor--insert-function refactor-tag 'gen-func-impl))
       ((eq insert-type 'gen-func-proto)
        (srefactor--insert-function refactor-tag 'gen-func-proto))
       ((semantic-tag-get-attribute refactor-tag :function-pointer)
        (semantic-insert-foreign-tag (srefactor--function-pointer-to-function refactor-tag)))
       (t (senator-yank-tag))))

    ;; indent after inserting refactor-tag
    (indent-according-to-mode)
    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - IO
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--contextual-open-file (other-file)
  "If the current buffer is also the selected file, don't open
the file in another window but use the current buffer and window
instead.

OTHER-FILE is the selected file from the menu."
  (if other-file
      (cond
       ((srefactor--switch-to-window other-file)
        (current-buffer))
       ((equal other-file (buffer-file-name (current-buffer)))
        (find-file other-file))
       (t (find-file-other-window other-file)
          (current-buffer)))
    ;; use ff-find-other-file when no file is chosen,
    ;; it means that user selected (Other file) option, but
    ;; does not install Projectile so he cannot use its function to
    ;; return the filename of other file. In this case, he simply gets
    ;; nil, which mean it's the job for `ff-find-other-file'. This needs
    ;; fixing in the future
    (ff-find-other-file t t)

    ;; `ff-find-other-file' does not return a buffer but switching to
    ;; the opened buffer instantly. We must return a buffer from this
    ;; function otherwise things go wrong
    (current-buffer)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that insert actual text or modify text
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; CLASS
;;
(defun srefactor-insert-class-getters-setters (tag file-option)
  "Insert getter-setter of a class TAG into file specified in FILE-OPTION."
  (semantic-fetch-tags-fast)
  (let ((tag (semantic-current-tag))
        (buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
    (when (eq (semantic-tag-class tag) 'type)
      (when (eq (semantic-tag-class tag) 'type)
        (let* ((members (srefactor--tag-filter 'semantic-tag-class
                                               '(variable label)
                                               (semantic-tag-type-members tag)))
               (variables (srefactor--tag-filter 'semantic-tag-class '(variable) members))
               (tag-start (semantic-tag-start tag)))
          (dolist (v variables)
            (when (srefactor--tag-private-p v)
              (srefactor--variable-insert-getter-setter t t v buffer)))
          (recenter))))))

(defun srefactor--insert-getter (tag &optional newline-before newline-after prototype-p)
  "Insert getter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (let ((tag-type (srefactor--tag-type-string tag))
        (tag-buffer (semantic-tag-buffer tag))
        (tag-parent-string "")
        tag-name beg)
    (setq beg (point))
    (unless (eq tag-buffer (current-buffer))
      (setq tag-parent-string (srefactor--tag-parents-string tag)))
    (when newline-before
      (newline newline-before))
    (when (and (or (listp (semantic-tag-type tag))
                   (semantic-tag-get-attribute tag :pointer))
               (not (semantic-tag-get-attribute tag :constant-flag)))
      (insert "const "))
    (insert tag-type)
    (setq tag-name (replace-regexp-in-string srefactor--getter-setter-removal-prefix
                                             ""
                                             (semantic-tag-name tag)))
    (insert (concat " "
                    tag-parent-string
                    srefactor--getter-prefix
                    (if srefactor--getter-setter-capitalize-p
                        (capitalize tag-name)
                      tag-name)))
    (insert "() const")
    (if prototype-p
        (insert ";")
      (insert " {")
      (srefactor--indent-and-newline 1)
      (insert (concat "return"
                      " "
                      (semantic-tag-name tag) ";"))
      (srefactor--indent-and-newline 1)
      (insert "}")
      (indent-according-to-mode)
      (when newline-after
        (newline newline-after)))
    (indent-region beg (point))))

(defun srefactor--insert-setter (tag newline-before newline-after &optional prototype-p)
  "Insert setter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (when newline-before
    (newline newline-before))
  (let ((tag-type (srefactor--tag-type-string tag))
        (tag-type (srefactor--tag-type-string tag))
        (tag-pointer (srefactor--tag-pointer tag))
        (tag-name (semantic-tag-name tag))
        (tag-type-string (srefactor--tag-type-string tag))
        (tag-buffer (semantic-tag-buffer tag))
        tag-parent-string modified-tag-name beg)
    (setq beg (point))
    (unless (eq tag-buffer (current-buffer))
      (setq tag-parent-string (srefactor--tag-parents-string tag)))
    (insert "void")
    (setq modified-tag-name (replace-regexp-in-string srefactor--getter-setter-removal-prefix
                                                      ""
                                                      (semantic-tag-name tag)))
    (insert (concat " "
                    tag-parent-string
                    srefactor--setter-prefix
                    (if srefactor--getter-setter-capitalize-p
                        (capitalize modified-tag-name)
                      modified-tag-name)))
    (insert (concat (insert "(")
                    (unless (semantic-tag-variable-constant-p tag)
                      "const ")
                    tag-type
                    (when (and (listp tag-type)
                               ;; (srefactor--tag-reference tag)
                               (not tag-pointer))
                      "&")
                    " "
                    tag-name
                    ")"))
    (if prototype-p
        (insert ";")
      (insert " {")
      (srefactor--indent-and-newline 1)
      (insert (concat "this->" tag-name " = " tag-name ";"))
      (srefactor--indent-and-newline 1)
      (insert "}")
      (indent-according-to-mode)
      (when newline-after
        (newline newline-after)))
    (indent-region beg (point))))

(defun srefactor--jump-or-insert-public-label (tag)
  "Check if TAG is a class or struct.
If so, check if any public label exists, jump to it.
Otherwise, insert one."
  (when (eq (semantic-tag-class tag) 'type)
    (goto-char (semantic-tag-start tag))
    (let* (label-pos
           (members (srefactor--tag-filter 'semantic-tag-class
                                           '(variable label)
                                           (semantic-tag-type-members tag)))
           (public-label (car (srefactor--tag-filter 'semantic-tag-name
                                                     '("public")
                                                     members))))
      (if public-label
          (progn
            (if (semantic-overlay-start (semantic-tag-overlay public-label))
                (progn
                  (goto-char (semantic-tag-end public-label))
                  (setq label-pos (semantic-tag-start public-label)))
              (search-forward "public:")
              (setq label-pos (point))))
        (goto-char (semantic-tag-end tag))
        (search-backward "}")
        (open-line 1)
        (insert "public:")
        (setq label-pos (point)))
      label-pos)))

(defun srefactor--variable-insert-getter-setter (insert-getter-p insert-setter-p tag buffer)
  "Insert getter if INSERT-GETTER-P is t, insert setter if INSERT-SETTER-P is t.
TAG is the current variable at point.
BUFFER is the destination buffer from file user selects from contextual menu."
  (with-current-buffer buffer
    (unless (srefactor--jump-or-insert-public-label (save-excursion
                                                      (goto-char (semantic-tag-start tag))
                                                      (semantic-current-tag-parent)))
      (goto-char (point-max)))
    (unless (eq buffer (semantic-tag-buffer tag))
      (with-current-buffer (semantic-tag-buffer tag)
        (srefactor--jump-or-insert-public-label (save-excursion
                                                  (goto-char (semantic-tag-start tag))
                                                  (semantic-current-tag-parent)))
        (when insert-getter-p (srefactor--insert-getter tag 1 1 t))
        (when insert-setter-p (srefactor--insert-setter tag 1 1 t))))
    (when insert-getter-p (srefactor--insert-getter tag 1 1))
    (when insert-setter-p (srefactor--insert-setter tag 1 1))))

;;
;; FUNCTION
;;
(defun srefactor--insert-with-srecode (func-tag)
  "Insert a tag using srecode"
  (let* ((copy (semantic-tag-copy func-tag))
         ;; (parent (semantic-tag-calculate-parent func-tag))
         ;; TODO - below srefactor fcn should be a part of semantic or srecode.
         (parentstring1 (srefactor--tag-parents-string func-tag))
         (parentstring (substring parentstring1 0 (- (length parentstring1) 2)))
         (endofinsert nil))
    ;; Copied this line from original
    (semantic-tag-put-attribute func-tag :typemodifiers nil)
    (semantic-tag-put-attribute func-tag :parent parentstring)
    ;; Insert the tag
    (require 'srecode/semantic)
    ;; TODO - does it need any special dictionary entries?
    (setq endofinsert
          (srecode-semantic-insert-tag
           func-tag
           nil ;; Style
           (lambda (localtag)
             (srefactor--insert-initial-content-based-on-return-type
              (if (or (srefactor--tag-function-constructor copy)
                      (srefactor--tag-function-destructor copy))
                  ""
                (semantic-tag-type copy)))
             ) ;; Callbck for function body.
           ;; Dictionary entries go here.
           ))
    (goto-char endofinsert)
    (insert "\n\n")))

(defun srefactor--insert-function (func-tag type)
  "Insert function implementations for FUNC-TAG at point, a tag that is a function.
`type' is the operation to be done, not the type of the tag."
  (newline)
  (left-char)
  (if srefactor-use-srecode-p
      ;; Try using SRecode as the mechanism for inserting a tag.
      (srefactor--insert-with-srecode func-tag)
    ;; official routine
    ;; add 2 newlines before insert the function
    ;; (newline-and-indent)
    (unless (srefactor--is-proto type)
      (newline-and-indent))

    (let ((func-tag-name (srefactor--tag-name func-tag))
          (parent (srefactor--calculate-parent-tag func-tag)))
      ;; insert const if return a const value
      (when (semantic-tag-get-attribute func-tag :constant-flag)
        (insert "const "))

      (when (srefactor--tag-function-modifiers func-tag)
        (semantic-tag-put-attribute func-tag :typemodifiers nil))
      (save-excursion
        (when (and (eq major-mode 'c++-mode)
                   parent)
          (insert (srefactor--tag-templates-declaration-string parent)))
        (insert (srefactor--tag-function-string func-tag))

        ;; insert const modifer for method
        (when (semantic-tag-get-attribute func-tag :methodconst-flag)
          (insert " const"))

        (when (srefactor--is-proto type)
          (insert ";\n")))
      (unless (eq major-mode 'c-mode)
        (search-forward-regexp (regexp-quote func-tag-name) (line-end-position) t)
        (search-backward-regexp (regexp-quote func-tag-name) (line-beginning-position) t)

        (when (srefactor--tag-function-destructor func-tag)
          (forward-char -1))

        ;; insert tag parent if any
        (unless (or (srefactor--tag-friend-p func-tag)
                    (eq type 'gen-func-proto)
                    ;; check if parent exists for a tag
                    (null (srefactor--calculate-parent-tag func-tag)))
          (insert (srefactor--tag-parents-string func-tag)))

        (when (srefactor--tag-function-constructor func-tag)
          (let ((variables (srefactor--tag-filter #'semantic-tag-class
                                                  '(variable)
                                                  (semantic-tag-type-members parent))))
            (setq variables
                  (remove-if-not (lambda (v)
                                   (string-match "const" (srefactor--tag-type-string v)))
                                 variables))
            (when variables
              (goto-char (line-end-position))
              (insert ":")
              (mapc (lambda (v)
                      (when (string-match "const" (srefactor--tag-type-string v))
                        (insert (semantic-tag-name v))
                        (insert "()")))
                    variables)))))))

  ;; post content insertion based on context
  (unless (srefactor--is-proto type)
    (end-of-line)
    (insert " {")
    (newline 1)
    (save-excursion
      (srefactor--insert-initial-content-based-on-return-type
       (if (or (srefactor--tag-function-constructor func-tag)
               (srefactor--tag-function-destructor func-tag))
           ""
         (semantic-tag-type func-tag)))
      (insert "}")
      (indent-according-to-mode))
    (goto-char (line-end-position))))

(defun srefactor--insert-function-pointer (tag)
  "Insert function pointer definition for TAG."
  (insert (concat "typedef "
                  (srefactor--tag-type-string tag)
                  " "
                  "("
                  (srefactor--tag-parents-string tag)
                  "*"
                  (semantic-tag-name tag)
                  ")"
                  "("))
  (let ((param-str (mapconcat
                    (lambda (tag)
                      (let ((ptr-level (srefactor--tag-pointer tag))
                            (ref-level (srefactor--tag-reference tag)))
                        (srefactor--tag-type-string tag)))
                    (semantic-tag-function-arguments tag)
                    ", ")))
    (insert param-str)
    (insert ");")))

(defun srefactor--insert-function-as-parameter (tag)
  "Insert TAG that is a function as a function parameter.
This means, the function is converted into a function pointer."
  (insert (srefactor--function-to-function-pointer tag))
  (insert ", "))

(defun srefactor--insert-new-function-from-region ()
  "Extract function from region."
  (semantic-force-refresh)
  (push-mark (region-beginning))
  (let ((reg-diff (- (region-end) (region-beginning)))
        (region (buffer-substring-no-properties (region-beginning) (region-end)))
        (tag (semantic-current-tag))
        (local-vars (semantic-get-all-local-variables))
        l orig p1 p2 name has-error)
    (unwind-protect
        (condition-case e
            (progn
              (setq orig (point))
              (setq region (with-temp-buffer
                             (let (p1 p2)
                               (insert (concat "void" " " "new_function"))
                               (insert "()")
                               (insert " {")
                               (newline 1)
                               (setq p1 (point))
                               (insert region)
                               (setq p2 (point))
                               (newline 1)
                               (insert "}")
                               (c-beginning-of-defun-1)
                               (search-forward "(" (point-max) t)
                               (dolist (v local-vars l)
                                 (when (srefactor--var-in-region-p v p1 p2)
                                   (push v l)))
                               (insert (srefactor--tag-function-parameters-string l))
                               (buffer-substring-no-properties (point-min) (point-max)))))
              (beginning-of-defun-raw)
              (recenter-top-bottom)
              (setq p1 (point))
              (insert region)
              (open-line 2)
              (setq p2 (point))
              (re-search-backward "new_function" nil t)
              (forward-char 1)
              (srefactor--mark-symbol-at-point)
              (setq name (read-from-minibuffer "Enter function name: "))
              (when (re-search-backward "new_function" nil t)
                (replace-match name))
              (indent-region (progn
                               (c-beginning-of-defun)
                               (point))
                             (progn
                               (c-end-of-defun)
                               (point))))
          (error "malform"
                 (setq has-error t)
                 (message "%s" "The selected region is malformed."))))
    (when has-error
      (unless (and (null p1) (null p2))
        (delete-region p1 p2))
      (kill-line 2)
      (goto-char orig)
      (pop-mark))
    (goto-char (car mark-ring))
    (delete-region (car mark-ring) (+ (car mark-ring) reg-diff))
    (setq p1 (point))
    (insert name)
    (insert "(")
    (dolist (v l)
      (insert (concat (semantic-tag-name v) ", ")))
    (insert ");")
    (indent-region p1 (point))
    (when (re-search-backward ", " nil t)
      (replace-match ""))
    (pop-mark)))

(defun srefactor--insert-initial-content-based-on-return-type (tag-type)
  "Insert initial content of function implementations.

TAG-TYPE is the return type such as int, long, float, double..."
  (cond
   ((listp tag-type)
    (insert (semantic-tag-name tag-type) " b;" )
    (indent-according-to-mode)
    (newline 2)
    (insert "return b;")
    (indent-according-to-mode))
   ((or (string-match "int" tag-type)
        (string-match "short" tag-type)
        (string-match "long" tag-type))
    (insert "return 0;"))
   ((or (string-match "double" tag-type)
        (string-match "float" tag-type))
    (insert "return 0.0;"))
   ((string-match "bool" tag-type)
    (insert "return true;"))
   ((string-match "char" tag-type)
    (insert "return 'a';"))
   (t))
  (srefactor--indent-and-newline 1))

;; TODO: work on this in next release
(defun srefactor--insert-new-macro-from-region ()
  "Assume region is marked."
  (let* ((region (buffer-substring (region-beginning) (region-end)))
         (beg (region-beginning))
         (end (region-end))
         (multiline-p (> (count-lines beg end) 1))
         (name (read-from-minibuffer "Enter a macro name: ")))
    (filter-buffer-substring beg end t)
    (insert (concat name "()"))
    (goto-char (semantic-tag-start (semantic-current-tag)))
    (search-backward-regexp "^$")
    (newline 1)
    (open-line 1)
    ;; (setq mark-active nil)
    (setq beg (point))
    (insert (concat "#define " name (if multiline-p "\n" " ")))
    (insert region)
    (forward-line 2)
    (setq end (point))
    (goto-char beg)
    (set-mark-command nil )
    (goto-char end)
    (setq deactivate-mark nil)
    (recenter)
    (when multiline-p
      (call-interactively 'c-backslash-region))
    (setq end (point))
    (indent-region beg end)
    (setq mark-active nil)))

;;
;; VARIABLE
;;
(defun srefactor--rename-local-var (tag tag-occurrences function-tag new-name)
  "Rename the variable instances in TAG-OCCURRENCES in FUNCTION-TAG to NEW-NAME."
  (save-excursion
    (goto-char (semantic-tag-start function-tag))
    (let* ((distance (- (length new-name)
                        (length (semantic-tag-name tag))))
           (var-list (loop for v in tag-occurrences
                           for i from 0 upto (1- (length tag-occurrences))
                           collect (if (consp v)
                                       (cons (+ (car v) (* 14 i)) (cdr v))
                                     (+ v (* distance i))))))
      (mapc (lambda (c)
              (goto-char c)
              (search-forward-regexp (srefactor--local-var-regexp tag)
                                     (semantic-tag-end function-tag)
                                     t)
              (replace-match new-name t t nil 1))
            var-list)
      (message (format "Renamed %d occurrences of %s to %s" (length var-list) (semantic-tag-name tag) new-name)))))

;;
;; GENERAL
;;

(defun srefactor--indent-and-newline (&optional number)
  "Indent than insert a NUMBER of newline."
  (indent-according-to-mode)
  (newline (if number number 1)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that operate on a Semantic tag and return information
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--get-all-parents (tag)
  "Return a list of parent tags of a TAG.
The closer to the end of the list, the higher the parents."
  (let* ((tag-buffer (semantic-tag-buffer tag))
         (parents (cdr (nreverse
                        (semantic-find-tag-by-overlay (semantic-tag-start tag)
                                                      (if tag-buffer
                                                          tag-buffer
                                                        (current-buffer)))))))
    parents))

(defun srefactor--tag-parents-string (tag)
  "Return parent prefix string of a TAG.

It is used for prepending to function or variable name defined
outside of a scope."
  (let* ((parents (srefactor--get-all-parents tag))
         (parents-at-point (semantic-find-tag-by-overlay))
         (parents-str-lst (mapcar (lambda (tag)
                                    (concat (semantic-tag-name tag)
                                            (srefactor--tag-templates-parameters-string tag)))
                                  parents))
         (parents-at-point-str-lst (mapcar (lambda (tag)
                                             (concat (semantic-tag-name tag)
                                                     (srefactor--tag-templates-parameters-string tag)))
                                           parents-at-point))
         (diff (set-difference parents-str-lst
                               parents-at-point-str-lst
                               :test #'string-equal)))
    (concat (mapconcat #'identity (nreverse diff) "::") "::")))

(defun srefactor--tag-function-parameters-string (members)
  "Return function parameter string of a function.

MEMBERS is a list of tags that are parameters of a function.  The
parameters are retrieved by the function `semantic-tag-function-arguments'.

The returned string is formatted as \"param1, param2, param3,...\"."
  (string-trim-right
   (mapconcat (lambda (m)
                (concat (srefactor--tag-type-string m)
                        " "
                        (semantic-tag-name m)
                        ))
              members
              ", ")))

(defun srefactor--tag-function-string (tag)
  "Return a complete string representation of a TAG that is a function."
  (let ((return-type (srefactor--tag-type-string tag))
        (members (semantic-tag-function-arguments tag))
        (is-constructor (srefactor--tag-function-constructor tag))
        (is-destructor (srefactor--tag-function-destructor tag)))
    (string-trim-left (concat (unless (or is-destructor is-constructor)
                                (concat return-type " "))
                              (when is-destructor "~")
                              (srefactor--tag-name tag)
                              "("
                              (srefactor--tag-function-parameters-string members)
                              ")"))))

(defun srefactor--tag-template-string-list (tag)
  "Return a list of templates as a list of strings from a TAG."
  (let ((templates (semantic-c-tag-template tag)))
    (unless templates
      (setq templates (semantic-c-tag-template (srefactor--calculate-parent-tag tag))))
    (when templates
      (mapcar #'car templates))))

(defun srefactor--calculate-parent-tag (tag)
  "An alternative version of `semantic-tag-calculate-parent'.

It is the same except does not check if a TAG is in current
buffer.  If such check is performed, even if a TAG has parent, nil
is returned."
  (let ((tag-buffer (semantic-tag-buffer tag)))
    (with-current-buffer (if tag-buffer
                             tag-buffer
                           (current-buffer))
      (save-excursion
        (goto-char (semantic-tag-start tag))
        (semantic-current-tag-parent)))))

(defun srefactor--tag-templates-parameters-string (tag)
  "Return a string with all template parameters from a TAG.

The returned string is formatted as \"<class T1, class T2, ...>\"."
  (let ((tmpl-list (srefactor--tag-template-string-list tag)))
    (if tmpl-list
        (concat "<"
                (mapconcat #'identity tmpl-list ", ")
                ">")
      ""))
  )

(defun srefactor--tag-templates-declaration-string (tag)
  "Return a string with all template declarations from a TAG.

The returned string is formatted as:

\"template <class T1, class T2>\"
\"template <class T3>\"
\"....\"."
  (let* ((parent (condition-case nil
                     (srefactor--calculate-parent-tag tag)
                   (error nil)))
         (tmpl-list (srefactor--tag-template-string-list tag)))
    (if tmpl-list
        (concat (if parent
                    (srefactor--tag-templates-declaration-string parent)
                  "")
                (concat "template <"
                        (mapconcat (lambda (T)
                                     (concat "class " T))
                                   tmpl-list
                                   ", ")
                        ">"
                        "\n"))
      "")))

(defun srefactor--function-pointer-to-function (tag)
  "Convert a function pointer from a function TAG."
  (let* ((new-tag (semantic-tag-copy tag))
         (args (semantic-tag-function-arguments new-tag))
         (i 1))
    (mapc (lambda (arg)
            (semantic-tag-set-name arg (concat "a" (number-to-string i)))
            (setq i (+ i 1)))
          args)
    (semantic-tag-set-name new-tag (semantic-tag-name new-tag))
    (semantic--tag-put-property new-tag :foreign-flag t)
    (semantic-tag-put-attribute new-tag :function-pointer nil)
    new-tag))

(defun srefactor--function-to-function-pointer (tag)
  "Convert a function to function pointer from a TAG"
  (let* ((type-string (srefactor--tag-type-string tag))
         (tag-name (concat "(*" (semantic-tag-name tag) ")"))
         (args (semantic-tag-function-arguments tag)))
    (concat type-string
            " "
            tag-name
            " "
            "("
            (mapconcat (lambda (arg)
                         (srefactor--tag-type-string arg))
                       args
                       ", ")
            ")")))

(defun srefactor--tag-function-modifiers (tag)
  "Return `:typemodifiers' attribute of a TAG."
  (semantic-tag-get-attribute tag :typemodifiers))

(defun srefactor--tag-function-destructor (tag)
  "Return `:destructor-flag' attribute of a TAG, that is either t or nil."
  (semantic-tag-get-attribute tag :destructor-flag))

(defun srefactor--tag-function-constructor (tag)
  "Return `:constructor-flag' attribute of a TAG, that is either t or nil."
  (semantic-tag-get-attribute tag :constructor-flag))

(defun srefactor--local-var-regexp (tag)
  "Return regexp for seraching local variable TAG."
  (format (concat "\\(\\_\<%s\\)[ ]*\\([^[:alnum:]_"
                  ;; (unless (srefactor--tag-lambda-p tag) "(")
                  "]\\)")
          (regexp-quote (semantic-tag-name tag))))

(defun srefactor--tag-pointer (tag)
  "Return `:pointer' attribute of a TAG."
  (semantic-tag-get-attribute tag :pointer))

(defun srefactor--tag-typedef (tag)
  "Return `:typedef' attribute of a TAG."
  (semantic-tag-get-attribute tag :typedef))

(defun srefactor--tag-reference (tag)
  "Return `:reference' attribute of a TAG.

If it does not exist, perform additional check to make sure it
does not, since the actual text in buffer has it but for some
complicated language construct, Semantic cannot retrieve it."
  (let ((reference (semantic-tag-get-attribute tag :reference))
        (tag-buffer (semantic-tag-buffer tag))
        (tag-start (semantic-tag-start tag))
        (tag-end (semantic-tag-end tag))
        ref-start ref-end
        statement-beg)
    (if reference
        reference
      (save-excursion
        (with-current-buffer (if tag-buffer
                                 tag-buffer
                               ;; only tag in current buffer does not
                               ;; carry buffer information
                               (current-buffer))
          (goto-char tag-end)
          (setq statement-beg (save-excursion
                                (c-beginning-of-statement-1)
                                (point)))
          (goto-char statement-beg)
          (setq ref-start (re-search-forward "&"
                                             tag-end
                                             t))
          (goto-char statement-beg)
          (setq ref-end (re-search-forward "[&]+"
                                           tag-end
                                           t))
          (when (and ref-end ref-start)
            (1+ (- ref-end ref-start))))))))

(defun srefactor--tag-name (tag)
  "Return TAG name and handle edge cases."
  (let ((tag-name (semantic-tag-name tag)))
    (with-current-buffer (semantic-tag-buffer tag)
      (if (not (string-empty-p tag-name))
          (if (semantic-tag-get-attribute tag :operator-flag)
              (concat "operator " tag-name)
            tag-name)
        ""))))

(defun srefactor--tag-type-string (tag)
  "Return a complete return type of a TAG as string."
  (let* ((ptr-level (srefactor--tag-pointer tag))
         (ref-level (srefactor--tag-reference tag))
         (ptr-string (if ptr-level
                         (make-string ptr-level ?\*)
                       ""))
         (ref-string (if ref-level
                         (make-string ref-level ?\&)
                       ""))
         (tag-type (semantic-tag-type tag))
         (const-p (semantic-tag-variable-constant-p tag))
         (template-specifier (when (semantic-tag-p tag-type)
                               (semantic-c-tag-template-specifier tag-type))))
    (cond
     ((semantic-tag-function-constructor-p tag)
      "")
     (template-specifier
      (replace-regexp-in-string ",>" ">"
                                (concat (when (semantic-tag-variable-constant-p tag)
                                          "const ")
                                        (when (srefactor--tag-struct-p tag)
                                          "struct ")
                                        (car (semantic-tag-type tag))
                                        "<"
                                        (srefactor--tag-type-string-inner-template-list template-specifier)
                                        ">"
                                        (cond
                                         (ptr-level
                                          ptr-string)
                                         (ref-level
                                          ref-string)
                                         (t "")))))
     (t
      (if (listp tag-type)
          (concat (when const-p
                    "const ")
                  (when (srefactor--tag-struct-p tag)
                    "struct ")
                  (car tag-type)
                  (cond
                   (ref-level
                    ref-string)
                   (ptr-level
                    ptr-string)))
        tag-type)))))

(defun srefactor--tag-type-string-inner-template-list (tmpl-spec-list)
  (mapconcat (lambda (tmpl)
               (let* ((templates (semantic-c-tag-template-specifier tmpl)))
                 (concat (if (listp tmpl)
                             (car tmpl)
                           tmpl)
                         (if (and (not (null templates)) (listp templates))
                             (concat "<"  (srefactor--tag-type-string-inner-template-list templates)) ",")
                         (when templates "> "))))
             tmpl-spec-list
             ""))

(defun srefactor--extract-region (extract-type)
  "Extract region based on type.

EXTRACT-TYPE can be 'function or 'macro."
  (if (region-active-p)
      (unwind-protect
          (progn
            ;; (narrow-to-region (region-beginning) (region-end))
            ;; (when (semantic-parse-region (region-beginning) (region-end))
            ;;   (error "Please select a region that is not a declaration or an implementation."))
            (save-excursion
              (narrow-to-region (region-beginning) (region-end))
              (c-beginning-of-defun)
              (c-end-of-defun))
            (widen)
            (cond
             ((eq extract-type 'function)
              (srefactor--insert-new-function-from-region))
             ((eq extract-type 'macro)
              (srefactor--insert-new-macro-from-region))
             (t)))
        (widen))
    (error "No active region.")))

(defun srefactor--mark-symbol-at-point ()
  "Activate mark for a symbol at point."
  (interactive)
  (forward-sexp -1)
  (set-mark-command nil)
  (forward-sexp 1)
  (setq deactivate-mark nil))

(defun srefactor--fetch-candidates ()
  "Return a list of candidates in current buffer.

Each candidate is a list '(DISPLAY TAG OPTIONS).  This is a
wrapper for `srefactor--fetch-candidates-helper'.  See
`srefactor--fetch-candidates-helper' for more details."
  (srefactor--fetch-candidates-helper (semantic-fetch-tags) 0 nil))

(defun srefactor--fetch-candidates-helper (tags depth &optional class)
  "Return a list of lists '(DISPLAY TAG OPTIONS).

This function is intended to be used with `srefactor-ui-create-menu' to
be displayed as a list of menu items.

DISPLAY is the string to bepresented to user, TAG is a semantic
tag and OPTIONS is a list of possible choices for each menu item.

 TAGS are collection of Semantic tags in current buffer.
 DEPTH is current recursion depth.
 CLASS is the parent class."
  (let ((spaces (make-string (* depth 3) ?\s))
        (srefactor--tag-options (srefactor-ui--return-option-list 'tag))
        (dashes (make-string 1 ?\-))
        (class class)
        cur-type display tag-list)
    (cl-dolist (tag tags)
      (when (listp tag)
        (cl-case (setq cur-type (semantic-tag-class tag))
          ((function type)
           (let ((type-p (eq cur-type 'type)))
             (unless (and (> depth 0) (not type-p))
               (setq class nil))
             (setq display (concat (if (null class)
                                       spaces
                                     (format "%s|%s%s" spaces dashes "►"))
                                   (semantic-format-tag-summarize tag nil t)
                                   (if (eq cur-type 'type)
                                       " (Inside)")))
             (and type-p
                  (setq class (car tag)))
             ;; Recurse to children
             (push (list display tag (if (eq cur-type 'type)
                                         srefactor--tag-options
                                       nil)) tag-list)
             (setq tag-list (append (srefactor--fetch-candidates-helper (semantic-tag-components tag)
                                                                        (1+ depth)
                                                                        class)
                                    tag-list))))

          ((package include label variable)
           (let* ((parent-tag (semantic-tag-calculate-parent tag))
                  (display (concat (if parent-tag
                                       (format "%s|%s%s" spaces dashes "►")
                                     spaces)
                                   (semantic-format-tag-summarize tag nil t))))
             (push (list display tag nil) tag-list)))
          ;; Catch-all
          (t))))
    tag-list))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - Predicates
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--menu-add-function-proto-p (tag)
  "Check whether to add generate function prototype menu item for a TAG."
  (let ((class (semantic-tag-class tag)))
    (and (eq class 'function)
         (not (semantic-tag-prototype-p tag))
         (and (not (srefactor--tag-function-constructor tag))
              (not (srefactor--tag-function-destructor tag)))
         (not (region-active-p))
         (null srefactor--current-local-var) )))

(defun srefactor--menu-add-function-implementation-p (tag)
  "Check whether to add generate function implementation menu item for a TAG."
  (let ((class (semantic-tag-class tag)))
    (and (or (eq class 'type)
             (and (eq class 'function)
                  (semantic-tag-prototype-p tag)))
         (not (region-active-p))
         (null srefactor--current-local-var))))

(defun srefactor--menu-add-rename-local-p ()
  "Check whether to add rename menu item."
  (let* ((local-var (srefactor--tag-at-point))
         (cur-tag (semantic-current-tag))
         cur-tag-start cur-tag-end tag-name)
    (when (and local-var
               (eq (semantic-tag-class cur-tag) 'function)
               (not (equal (car (nreverse (semantic-ctxt-current-symbol)))
                           (semantic-tag-name cur-tag)))
               (not (semantic-tag-prototype-p cur-tag))
               (not (region-active-p)))
      local-var)))

(defun srefactor--menu-add-function-pointer-p (tag)
  "Check whether to add generate function pointer menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'function)
       (not (semantic-tag-get-attribute tag :pointer))
       (and (not (srefactor--tag-function-constructor tag))
            (not (srefactor--tag-function-destructor tag)))
       (not (region-active-p))
       (null srefactor--current-local-var)))

(defun srefactor--menu-add-getters-setters-p (tag)
  "Check whether to add generate getters and setters menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'type)
       (srefactor--tag-filter 'semantic-tag-class '(variable) (semantic-tag-type-members tag))
       (not (region-active-p))))

(defun srefactor--menu-add-getter-setter-p (tag)
  "Check whether to add generate getter and setter menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'variable)
       (eq (semantic-tag-class (semantic-current-tag-parent)) 'type)
       (not (region-active-p))))

(defun srefactor--menu-add-move-p ()
  "Check whether to add move menu."
  (and (semantic-current-tag)
       (not (region-active-p))))

(defun srefactor--tag-at-point ()
  "Retrieve current variable tag at piont."
  (let* ((ctxt (semantic-analyze-current-context (point)))
         (pf (when ctxt
               ;; The CTXT is an EIEIO object.  The below
               ;; method will attempt to pick the most interesting
               ;; tag associated with the current context.
               (semantic-analyze-interesting-tag ctxt))))
    pf))

(defun srefactor--activate-region (beg end)
  "Activate a region from BEG to END."
  (interactive)
  (goto-char beg)
  (set-mark-command nil)
  (goto-char end)
  (setq deactivate-mark nil))

(defun srefactor--menu-for-region-p ()
  "Check whether to add exclusive menu item for a region."
  (region-active-p))

(defun srefactor--var-in-region-p (tag beg end)
  "Check if a local variable TAG is in a region from BEG to END."
  (save-excursion
    (goto-char beg)
    (search-forward-regexp (srefactor--local-var-regexp tag)
                           end t)))

(defun srefactor--tag-struct-p (tag)
  "Check if TAG is a C struct."
  (condition-case nil
      (let* ((type-tag (semantic-tag-type tag))
             (typedef-tag (srefactor--tag-typedef tag))
             type-type-tag struct-p)
        (when typedef-tag
          (setq struct-p (semantic-tag-type typedef-tag)))
        (unless struct-p
          (setq type-type-tag (semantic-tag-type type-tag))
          (setq struct-p (and (stringp type-type-tag)
                              (string-equal type-type-tag "struct"))))
        struct-p)
    (error nil)))

(defun srefactor--tag-private-p (tag)
  "Check whether a TAG is a private variable."
  (let* ((members (srefactor--tag-filter 'semantic-tag-class
                                         '(variable label)
                                         (semantic-tag-type-members (semantic-tag-calculate-parent tag))))
         (labels (srefactor--tag-filter 'semantic-tag-class
                                        '(label)
                                        members))
         (public-label (car (srefactor--tag-filter 'semantic-tag-name
                                                   '("public")
                                                   labels)))
         (private-label (car (srefactor--tag-filter 'semantic-tag-name
                                                    '("private")
                                                    labels)))
         (tag-start (when tag (semantic-tag-start tag)))
         (private-pos (when private-label (semantic-tag-start private-label)))
         (public-pos (when public-label (semantic-tag-start public-label))))
    (when (and private-label public-label)
      (or (and private-label (> tag-start private-pos)
               public-label (< tag-start public-pos))
          (and public-label (> tag-start public-pos)
               private-label (> tag-start private-pos)
               (> private-pos public-pos))))))

(defun srefactor--tag-auto-p (tag)
  "Check whether a TAG is an auto variable."
  (let ((type (semantic-tag-type tag)))
    (and (listp type)
         (string-equal "auto" (car type)))))

(defun srefactor--tag-lambda-p (tag)
  "Check whether TAG is a lambda function."
  (condition-case nil
      (save-excursion
        (goto-char (semantic-tag-start tag))
        (and (srefactor--tag-auto-p tag)
             (search-forward-regexp "=[ ]*\\[.*\\][ ]*(.*)[ ]*"  (semantic-tag-end tag) t)))
    (error nil)))

(defun srefactor--tag-friend-p (tag)
  "Check whether a TAG is a friend to everyone."
  (condition-case nil
      (let ((tag-start (semantic-tag-start tag))
            (tag-end (semantic-tag-end tag))
            (tag-buffer (semantic-tag-buffer tag)))
        (with-current-buffer tag-buffer
          (save-excursion
            (goto-char tag-start)
            (search-forward-regexp "friend" tag-end t))))
    (error nil)))

(defun srefactor--unknown-symbol-at-point-p ()
  "Check whether a symbol at point is an unknown variable."
  (unless (and (semantic-ctxt-current-symbol)
               (srefactor--tag-at-point))
    t))

(defun srefactor--introduce-variable-at-point ()
  (save-excursion
    ;;
    (let ((var (save-excursion
                 (c-end-of-statement)
                 (semantic-ctxt-current-assignment)))
          var-string)
      (unless var
        (setq var (semantic-ctxt-current-symbol)))
      (setq var-string (read-from-minibuffer "New variable: " var))
      (goto-char (semantic-tag-end (car (last (semantic-get-all-local-variables)))))
      (newline-and-indent)
      (insert (concat var-string ";")))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - Utilities
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--collect-tag-occurrences (tag beg end &optional with-content)
  "Collect all TAG occurrences.
PARENT-TAG is the tag that contains TAG, such as a function or a class or a namespace."
  (save-excursion
    (let ((local-var-regexp (srefactor--local-var-regexp tag))
          p positions)
      (goto-char beg)
      (while (re-search-forward local-var-regexp end t)
        (setq p (match-beginning 0))
        ;; must compare tag to avoid tags with the same name but are
        ;; different types and/or different scopes
        (save-excursion
          (goto-char p)
          (when (or (semantic-equivalent-tag-p tag (srefactor--tag-at-point))
                    (semantic-equivalent-tag-p tag (semantic-current-tag)))
            (push (if with-content
                      (cons p (buffer-substring-no-properties (line-beginning-position)
                                                              (line-end-position)))
                    p)
                  positions))))
      (nreverse positions))))

(defun srefactor--highlight-tag (tag tag-occurrences &optional scope-tag face)
  "Highlight tag in TAG-OCCURRENCES in SCOPE-TAG with FACE."
  (let (beg end)
    (mapc (lambda (p)
            (save-excursion
              (goto-char p)
              (let ((overlay (make-overlay p (progn
                                               (forward-sexp 1)
                                               (point)))))
                (overlay-put overlay 'face 'match))))
          tag-occurrences)))

(defun srefactor--switch-to-window (file-path)
  "Switch to window that contains FILE-PATH string."
  (catch 'found
    (dolist (w (window-list))
      (when (equal file-path (buffer-file-name (window-buffer w)))
        (select-window w)
        (throw 'found "Found window.")))))

(provide 'srefactor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
