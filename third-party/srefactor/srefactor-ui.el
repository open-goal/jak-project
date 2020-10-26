;;; srefactor-ui.el --- A refactoring tool based on Semantic parser framework
;;
;; Filename: srefactor-ui.el
;; Description: A refactoring tool based on Semantic parser framework
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com
;; Maintainer: Tu, Do Hoang
;; Created: Wed Feb 11 21:25:51 2015 (+0700)
;; Version: 0.1
;; Package-Requires: ()
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
;; This package provides a UI to interact with users of Srefactor
;; package.
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
(with-no-warnings
  (require 'cl))
(require 'recentf)
(require 'eieio)
(require 'semantic/format)
(autoload 'srefactor--refactor-based-on-tag-class "srefactor")
(autoload 'srefactor--insert-tag "srefactor")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar srefactor-ui--current-active-window nil
  "Store the current active window where the menu is invoked.")

(defvar srefactor-ui--current-active-region-start nil
  "Store the start of an active region of current window if any.")

(defvar srefactor-ui--current-active-region-end nil
  "Store the end of an active region of current window if any.")

(defvar srefactor-ui--current-active-menu nil
  "Current menu object biing used.")

(defvar srefactor-ui--func-type nil
  "What type of refactoring to perform.")

(defvar srefactor-ui--current-active-tag-overlay nil
  "Overlay of tag in srefactor-ui--current-active-window.")

(defcustom srefactor-ui-menu-show-help t
  "Turn on/off help message."
  :group 'srefactor-ui
  :type 'boolean)

(defsubst srefactor-ui--menu-label (e)
  (car e))

(defsubst srefactor-ui--menu-value-item (e)
  (cdr e))

(defsubst srefactor-ui--digit-shortcut-command-name (n)
  "Return a command name to open the Nth most recent file.
See also the command `recentf-open-most-recent-file'."
  (intern (format "srefactor-ui--refactor-based-on-tag-class-%d" n)))

(defsubst srefactor-ui--make-menu-element (menu-item menu-value)
  "Create a new menu-element.
A menu element is a pair (MENU-ITEM . MENU-VALUE), where MENU-ITEM is
the menu item string displayed.  MENU-VALUE is the file to be open
when the corresponding MENU-ITEM is selected."
  (cons menu-item menu-value))

(defclass srefactor-ui-menu ()
  ((name
    :initarg :name
    :initform "*Srefactor Menu*"
    :accessor name
    :type string
    :documentation
    "Name of the menu to be displayed in the modeline.")
   (items
    :initarg :items
    :initform nil
    :accessor items
    :type list
    :documentation
    "Item list to be displayed in a menu. Item is a list
          '(DISPLAY REAL OPTIONS).")
   (action
    :initarg :action
    :initform nil
    :accessor action
    :documentation
    "An action to run when a menu item is selected.")
   (context
    :initarg :context
    :initform nil
    :accessor context
    :documentation
    "Current Semantic tag in scope, used as context to
     select appropriate refactor actions.")
   (shortcut-p
    :initarg :shortcut-p
    :initform nil
    :accessor shortcut-p
    :type boolean
    :documentation
    "If t, first 9 actions can be executed by digit keys 1-9.")
   (persistent-action
    :initarg :persistent-action
    :initform nil
    :accessor persistent-action
    :documentation
    "An action to execute without exiting the menu.")
   (keymap
    :initarg :keymap
    :initform nil
    :accessor keymap
    :documentation
    "A function that set define keys in srefactor-ui-menu-mode-map.")
   (post-handler
    :initarg :post-handler
    :initform nil
    :accessor post-handler
    :documentation
    "A function to be executed after the menu is created."))
  "Class srefactor-ui-menu ")

(defmacro srefactor-ui--menu (name &rest forms)
  "Show a dialog buffer with NAME, setup with FORMS."
  (declare (indent 1) (debug t))
  `(with-current-buffer (get-buffer-create ,name)
     ;; Cleanup buffer
     (let ((inhibit-read-only t)
           (ol (overlay-lists)))
       (mapc 'delete-overlay (car ol))
       (mapc 'delete-overlay (cdr ol))
       (erase-buffer))
     (srefactor-ui-menu-mode)
     ,@forms
     (widget-setup)
     (switch-to-buffer (current-buffer))
     (hl-line-mode 1)))

(defun srefactor-ui-create-menu (menu)
  (interactive)
  (unless (items menu)
    (error "No available action."))
  (setq srefactor-ui--current-active-window (car (window-list)))
  (setq srefactor-ui--current-active-menu menu)
  (if (region-active-p)
      (progn
        (setq srefactor-ui--current-active-region-start (region-beginning))
        (setq srefactor-ui--current-active-region-end (region-end)))
    (setq srefactor-ui--current-active-region-start nil)
    (setq srefactor-ui--current-active-region-end nil))
  (condition-case nil
      (with-selected-window (select-window (split-window-below))
        (srefactor-ui--menu
            (or (name srefactor-ui--current-active-menu)
                (format "*%s*" "*Srefactor Menu*"))
          (let ((major-mode 'c++-mode))
            (widget-insert (if (context srefactor-ui--current-active-menu)
                               (concat (semantic-format-tag-summarize (context srefactor-ui--current-active-menu) nil t) "\n")
                             "")
                           (if srefactor-ui-menu-show-help
                               (concat  (if (shortcut-p srefactor-ui--current-active-menu)
                                            (concat "Press "
                                                    (propertize "1-9" 'face  'font-lock-preprocessor-face)
                                                    " or click on an action to execute.\n")
                                          "Click on an action to execute.\n")
                                        "Press "
                                        (propertize "o" 'face  'bold)
                                        " or "
                                        (propertize "O" 'face  'bold)
                                        " to switch to next/previous option."
                                        "\n"
                                        "Click on "
                                        (propertize "[Cancel]" 'face 'bold)
                                        " or press "
                                        (propertize "q" 'face 'bold)
                                        " to quit.\n")
                             "")))
          (apply 'widget-create
                 `(group
                   :indent 2
                   :format "\n%v\n"
                   ,@(srefactor-ui--generate-items
                      (items srefactor-ui--current-active-menu)
                      (action srefactor-ui--current-active-menu)
                      (shortcut-p srefactor-ui--current-active-menu))))
          (widget-create
           'push-button
           :notify 'srefactor-ui--menu-quit
           (propertize  "Cancel" 'face 'bold))
          (recentf-dialog-goto-first 'link)
          (when (post-handler menu)
            (funcall (post-handler menu)))
          (when (keymap menu)
            (funcall (keymap menu))))
        (fit-window-to-buffer (car (window-list))
                              (/ (* (frame-height) 50)
                                 100)
                              (/ (* (frame-height) 10)
                                 100))
        (when (and (fboundp 'evil-mode)
                   evil-mode)
          (evil-local-mode)))
    (error (srefactor-ui--clean-up-menu-window)
           (message "Error when creating menu."))))

(defun srefactor-ui--return-option-list (type)
  (let (options)
    (cond
     ((eq type 'file)
      (push "(Current file)" options)
      (push "(Other file)" options)
      (when (featurep 'projectile)
        (push "(Project file)" options))
      (push "(File)" options))
     ((eq type 'tag)
      '("(Before)" "(Inside)" "(After)"))
     (t))))

(defun srefactor-ui--generate-items (commands action &optional add-shortcut)
  "Return a list of widgets to display FILES in a dialog buffer."
  (mapcar (lambda (w)
            (srefactor-ui--create-menu-widget w action))
          (if add-shortcut
              (srefactor-ui--show-digit-shortcut (mapcar 'srefactor-ui--make-default-menu-element
                                                         commands))
            (mapcar 'srefactor-ui--make-default-menu-element
                    commands))))

(defun srefactor-ui--show-digit-shortcut (l)
  "Filter the list of menu-elements L to show digit shortcuts."
  (let ((i 0))
    (dolist (e l)
      (setq i (1+ i))
      (setcar e (format (if (< i 10)
                            "[%s] %s"
                          " %s  %s")
                        (if (< i 10 )
                            (propertize (number-to-string (% i 10))
                                        'face 'font-lock-preprocessor-face
                                        'mouse-face 'italic)
                          " ")
                        (srefactor-ui--menu-label e))))
    l))

(defun srefactor-ui--make-default-menu-element (command)
  (srefactor-ui--make-menu-element (srefactor-ui--menu-label command)
                                   (srefactor-ui--menu-value-item command)))

(defun srefactor-ui--create-menu-widget (menu-element action)
  "Return a widget to display MENU-ELEMENT in a dialog buffer."
  `(link :tag ,(srefactor-ui--menu-label menu-element)
         :button-prefix ""
         :button-suffix ""
         :button-face nil
         :format "%[%t\n%]"
         :help-echo ""
         :action ,action
         ,(srefactor-ui--menu-value-item menu-element)))

(defun srefactor-ui--clean-up-menu-window (&optional kill-buffer)
  (interactive)
  (when kill-buffer
    (kill-buffer (current-buffer)))
  (delete-window (car (window-list)))
  (select-window srefactor-ui--current-active-window)
  (when (and srefactor-ui--current-active-region-start
             srefactor-ui--current-active-region-end)
    (goto-char srefactor-ui--current-active-region-start)
    (set-mark-command nil)
    (goto-char srefactor-ui--current-active-region-end)
    (setq deactivate-mark nil))
  (when srefactor-ui--current-active-tag-overlay
    (delete-overlay srefactor-ui--current-active-tag-overlay)))

(defun srefactor-ui--refactor-action (widget &rest _ignore)
  "Open the file stored in WIDGET's value when notified.
-IGNORE other arguments."
  (interactive)
  (srefactor-ui--clean-up-menu-window t)
  (srefactor--refactor-based-on-tag-class (car (widget-value widget))
                                          (srefactor-ui--get-current-menu-option (widget-get widget :tag))))

(defun srefactor-ui--tag-action (widget &rest _ignore)
  (interactive)
  (srefactor-ui--clean-up-menu-window t)
  (srefactor--insert-tag (context srefactor-ui--current-active-menu)
                         (car (widget-value widget))
                         srefactor-ui--func-type
                         (srefactor-ui--get-current-menu-option (widget-get widget :tag))))

(defun srefactor-ui--menu-quit (&rest ignored)
  (interactive)
  (srefactor-ui--clean-up-menu-window t))

(defvar srefactor-ui--shortcuts-keymap
  (let ((km (make-sparse-keymap)))
    (dolist (k '(9 8 7 6 5 4 3 2 1))
      (let ((cmd (srefactor-ui--digit-shortcut-command-name k)))
        ;; Define a shortcut command.
        (defalias cmd
          `(lambda ()
             (interactive)
             (unless (search-forward (number-to-string ,k) nil t)
                 (search-backward (number-to-string ,k)) nil t)
             (srefactor-ui--refactor-action (get-char-property (point) 'button))))
        ;; Bind it to a digit key.
        (define-key km (vector (+ k ?0)) cmd)))
    km)
  "Digit shortcuts keymap.")

(defun srefactor-ui--previous-page-target-window ()
  (interactive)
  (let ((menu-window (car (window-list))))
    (select-window srefactor-ui--current-active-window)
    (condition-case nil
        (scroll-down)
      (error nil))
    (select-window menu-window)))

(defun srefactor-ui--next-page-target-window ()
  (interactive)
  (let ((menu-window (car (window-list))))
    (select-window srefactor-ui--current-active-window)
    (condition-case nil
        (scroll-up)
      (error nil))
    (select-window menu-window)))

(defun srefactor-ui--cycle-option (direction current-option options)
  (let* ((options options)
         (pos (position current-option options :test #'string-equal))
         (l (length options)))
    (if (eq direction 'next)
        (if (< pos (1- l))
            (nth (1+ pos) options)
          (car options))
      (if (> pos 0)
          (nth (1- pos) options)
        (nth (1- l) options)))))

(defun srefactor-ui--get-current-menu-option (menu-string)
  (condition-case nil
      (progn
        (string-match "(\\(.*\\))" menu-string)
        (match-string 0 menu-string))
    (error nil)))

(defun srefactor-ui--cycle (direction)
  (let* ((pos (point))
         (link (get-char-property pos 'button))
         (current-opt (srefactor-ui--get-current-menu-option (widget-get link :tag)))
         (options (cadr (widget-value-value-get link)))
         (check (unless current-opt (throw 'option-not-available "No option is available for this tag.")))
         (next-opt (srefactor-ui--cycle-option direction current-opt options))
         (next-tag (replace-regexp-in-string "(\\(.*\\))" "" (widget-get link :tag))))
    (when link
      (widget-put link :tag (concat next-tag next-opt))
      (widget-delete (get-char-property pos 'button))
      (widget-create link)
      (forward-line -1)
      (widget-forward 1))))

(defvar srefactor-ui-menu-mode-map
  (let ((km (copy-keymap srefactor-ui--shortcuts-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km "q" 'srefactor-ui--menu-quit)
    (define-key km "n" 'widget-forward)
    (define-key km "p" 'widget-backward)
    (define-key km "j" 'widget-forward)
    (define-key km "k" 'widget-backward)
    (define-key km (kbd "TAB") (lambda ()
                                 (interactive)
                                 (when (persistent-action srefactor-ui--current-active-menu)
                                   (funcall (persistent-action srefactor-ui--current-active-menu)))))
    (define-key km "o" (lambda ()
                         (interactive)
                         (message "%s"
                                  (catch 'option-not-available
                                    (srefactor-ui--cycle 'next)))))
    (define-key km "O" (lambda ()
                         (interactive)
                         (message "%s"
                                  (catch 'option-not-available
                                    (srefactor-ui--cycle 'prev)))))
    (define-key km (kbd "M-<next>") 'srefactor-ui--next-page-target-window)
    (define-key km (kbd "M-<prior>") 'srefactor-ui--previous-page-target-window)
    (when (featurep 'evil)
      (define-key km (kbd "/") 'evil-search-forward)
      (define-key km (kbd "?") 'evil-search-backward))
    (define-key km (kbd "C-g") 'srefactor-ui--menu-quit)
    (define-key km [follow-link] "\C-m")
    km)
  "Keymap used in recentf dialogs.")

(define-derived-mode srefactor-ui-menu-mode nil "srefactor-ui-menu"
  "Major mode of recentf dialogs.
 "
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(provide 'srefactor-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor-ui.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
