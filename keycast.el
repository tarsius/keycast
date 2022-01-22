;;; keycast.el --- Show current command and its key in the mode line  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/keycast

;; Package-Requires: ((emacs "25.3"))
;; Package-Version: 1.1.3

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides Keycast mode.  Once enabled, that mode shows
;; the current command and its key or mouse binding in the mode line,
;; and updates them whenever another command is invoked.

;;; Code:

(require 'format-spec)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(define-obsolete-variable-alias 'keycast-insert-after
  'keycast-mode-line-insert-after "Keycast 2.0.0")
(define-obsolete-variable-alias 'keycast-remove-tail-elements
  'keycast-mode-line-remove-tail-elements "Keycast 2.0.0")
(define-obsolete-variable-alias 'keycast-window-predicate
  'keycast-mode-line-window-predicate "Keycast 2.0.0")

;;; Options

(defgroup keycast nil
  "Show the current command and its key binding in the mode line."
  :group 'applications)

(defcustom keycast-mode-line-insert-after 'mode-line-buffer-identification
  "The position in `mode-line-format' where `keycast-mode-line' is inserted.

Enabling `keycast-mode' inserts the element `keycast-mode-line'
into `mode-line-format' after the element specified here."
  :group 'keycast
  :type '(cons (choice :tag "Insert after"
                       (const mode-line-buffer-identification)
                       (const moody-mode-line-buffer-identification)
                       variable
                       sexp)
               (boolean :tag "Remove following elements")))

(defcustom keycast-mode-line-remove-tail-elements t
  "Whether enabling `keycast-mode' removes elements to the right.

When this is non-nil, then enabling `keycast-mode' not only
inserts `keycast-mode-line' into `mode-line-format' but also
removes all elements to the right of where that was inserted."
  :group 'keycast
  :type 'boolean)

(defcustom keycast-mode-line-window-predicate 'keycast-active-frame-bottom-right-p
  "Whether to display the binding in the mode line of the selected window.

This predicate is used while updating the mode line of a window
to determine whether the current command and its key binding
should be displayed in its mode line.  The function is called
with no argument and acts on `selected-window'.

`moody-window-active-p'
  Return non-nil if the temporarily selected window is the
  active window, i.e. if it is the selected window as far
  as the user is concerned.  Load the `moody' library to be
  able to use this.

`powerline-selected-window-active'
  This function behaves like `moody-window-active-p', but
  is defined in the `powerline' library.  Load that library
  to be able to use this.

`keycast-bottom-right-window-p'
  Return non-nil if the temporarily selected window is the
  right-most bottom window of its frame.

`keycast-active-frame-bottom-right-p'
  Return non-nil if the temporarily selected window is the
  right-most bottom window of the frame that is active as
  far as the user is concerned.  If neither the `moody' nor
  the `powerline' library is loaded, then behave the same
  as `keycast-bottom-right-window-p'."
  :group 'keycast
  :type 'function)

(defcustom keycast-mode-line-format "%10s%k%c%r"
  "The format spec used by `keycast-mode-line'.

%s Some spaces, intended to be used like so: %10s.
%k The key using the `keycast-key' face and padding.
%K The key with no styling and without any padding.
%c The command using the `keycast-command' face.
%C The command with no styling.
%r The times the command was repeated.
%R The times the command was repeated using the `shadow' face."
  :package-version '(keycast . "1.0.3")
  :group 'keycast
  :type 'integer)

(defcustom keycast-tab-bar-location 'tab-bar-format-align-right
  "The location in `tab-bar-format' where `keycast-tab-bar' is inserted.

Enabling `keycast-tab-bar-mode' inserts the element
`keycast-tab-bar' into `tab-tab-bar-format' at the location
specified here.

If the value is `beginning' or `end', then insert as the first or
last element.  If the value is `replace', then insert as the only
element until the mode is disabled again.

Otherwise the value has to be a function that should be a member
of the format list.  `keycast-tab-bar' is inserted after that
function if it is a member or at the end of the list if not, in
which case a warning is shown.

As a special case it the value is `tab-bar-format-align-right'
but that isn't a member yet, then insert that followed by
`keycast-tab-bar', without showing a warning."
  :package-version '(keycast . "2.0.0")
  :group 'keycast
  :type
  '(choice
    (const :tag "Insert after tab-bar-format-add-tab" tab-bar-format-add-tab)
    (const :tag "Insert after tab-bar-format-align-right" tab-bar-format-align-right)
    (const :tag "Insert after tab-bar-format-global" tab-bar-format-global)
    (function :tag "Insert after function")
    (const :tag "Insert as first element" beginning)
    (const :tag "Insert as last element" end)
    (const :tag "Replace all other elements" replace)))

(defcustom keycast-tab-bar-format "%k%c%r"
  "The format spec used by `keycast-tab-bar'.

%s Some spaces, intended to be used like so: %10s.
%k The key using the `keycast-key' face and padding.
%K The key with no styling and without any padding.
%c The command using the `keycast-command' face.
%C The command with no styling.
%r The times the command was repeated.
%R The times the command was repeated using the `shadow' face."
  :group 'keycast
  :type 'integer)

(defcustom keycast-tab-bar-minimal-width 40
  "The minimal width of `keycast-tab-bar'."
  :package-version '(keycast . "2.0.0")
  :group 'keycast
  :type 'integer)

(defcustom keycast-log-format "%-20K%C%R\n"
  "The format spec used by `keycast-log-mode'.

%s `keycast-separator-width' spaces.
%k The key using the `keycast-key' face and padding.
%K The key with no styling and without any padding.
%c The command using the `keycast-command' face.
%C The command with no styling.
%r The times the command was repeated.
%R The times the command was repeated using the `shadow' face."
  :package-version '(keycast . "2.0.0")
  :group 'keycast
  :type 'string)

(defcustom keycast-log-frame-alist
  '((minibuffer . nil)
    (no-other-frame . t)
    (no-other-window . t)
    (no-delete-other-windows . t)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (desktop-dont-save . t)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil))
  "Alist of frame parameters used by `keycast-log-mode's frame."
  :package-version '(keycast . "2.0.0")
  :group 'keycast
  :type 'alist)

(defcustom keycast-log-newest-first t
  "Whether `keycast-log-mode' inserts events at beginning of buffer."
  :package-version '(keycast . "2.0.0")
  :group 'keycast
  :type 'boolean)

(defcustom keycast-log-buffer-name " *keycast*"
  "The name of the buffer used by `keycast-log-mode'."
  :package-version '(keycast . "2.0.0")
  :group 'keycast
  :type 'string)

(defcustom keycast-substitute-alist nil
  "Alist used to substituted events and/or commands for display.

Occasionally it might be necessary to pretend you pressed another
key than the one you actually pressed (because watchers don't
care about your weird key bindings), or to hide certain commands
\(such as `self-insert-command').  This option allows doing that
and more.

Each element has the form (MATCH EVENT COMMAND).  MATCH is an
event or a command.  When a command is invoked then this package
looks for a MATCH for that.  If there is a match, then that the
respective EVENT and COMMAND are used.  If not, then it looks
for a MATCH for that instead.

If either EVENT or COMMAND is nil, then neither the event nor
the command is shown (regardless of the value of the other).
Otherwise if EVENT is t then the actual event is shown, else
it has to be a string to be shown instead.  Likewise COMMAND
can be t to show the actual COMMAND or a symbol to be shown
instead."
  :group 'keycast
  :type '(repeat
          (list (choice :format "%{Actual event/command%}: %[Value Menu%] %v"
                        (string  :tag "Event")
                        (symbol  :tag "Command")
                        (const   :tag "Lambda" t))
                (choice :format "%{Display event%}:        %[Value Menu%] %v"
                        (const   :tag "Omit binding" nil)
                        (const   :tag "Use actual event" t)
                        (string  :tag "Substitute event"))
                (choice :format "%{Display command%}:      %[Value Menu%] %v"
                        (const   :tag "Omit binding" nil)
                        (const   :tag "Use actual command" t)
                        (symbol  :tag "Substitute command")))))

(defface keycast-key
  '((t (:weight bold
        :background "#d5cfbf"
        :foreground "#000000"
	:box (:line-width -3 :style released-button))))
  "When Keycast mode is enabled, face used for the key in the mode line."
  :group 'keycast)

(defface keycast-command '((t (:weight bold)))
  "When Keycast mode is enabled, face used for the command in the mode line."
  :group 'keycast)

;;; Common

(defvar keycast-mode)
(defvar keycast-tab-bar-mode)
(defvar keycast-log-mode)

(defun keycast--mode-active-p (&optional line)
  (or keycast-mode keycast-tab-bar-mode
      (and (not line) keycast-log-mode)))

(defvar keycast--this-command nil)
(defvar keycast--this-command-keys nil)
(defvar keycast--command-repetitions 0)
(defvar keycast--reading-passwd nil)

(defun keycast--update ()
  (if (eq last-command this-command)
      (cl-incf keycast--command-repetitions)
    (setq keycast--command-repetitions 0))
  ;; Remember these values because the mode line update won't actually
  ;; happen until we return to the command loop and by that time these
  ;; values have been reset to nil.
  (setq keycast--this-command-keys (this-single-command-keys))
  (setq keycast--this-command
        (cond ((symbolp this-command) this-command)
              ((eq (car-safe this-command) 'lambda) "<lambda>")
              (t (format "<%s>" (type-of this-command)))))
  (when (and keycast-log-mode
             (not keycast--reading-passwd))
    (keycast-log-update-buffer))
  (when (keycast--mode-active-p 'line)
    (force-mode-line-update (minibufferp))))

(defun keycast--format (format)
  (and (not keycast--reading-passwd)
       (let* ((key (ignore-errors
                     (key-description keycast--this-command-keys)))
              (cmd keycast--this-command)
              (elt (or (assoc cmd keycast-substitute-alist)
                       (assoc key keycast-substitute-alist))))
         (when elt
           (pcase-let ((`(,_ ,k ,c) elt))
             (unless (eq k t) (setq key k))
             (unless (eq c t) (setq cmd c))))
         (and key cmd
              (let ((k (if (and (bound-and-true-p mode-line-compact)
                                (eq format keycast-mode-line-format))
                           key
                         (let ((pad (max 2 (- 5 (length key)))))
                           (concat (make-string (ceiling pad 2) ?\s) key
                                   (make-string (floor   pad 2) ?\s)))))
                    (c (format " %s" cmd))
                    (r (if (> keycast--command-repetitions 0)
                           (format " x%s" (1+ keycast--command-repetitions))
                         "")))
                (format-spec
                 format
                 `((?s . "")
                   (?k . ,(propertize k 'face 'keycast-key))
                   (?K . ,key)
                   (?c . ,(propertize c 'face 'keycast-command))
                   (?C . ,c)
                   (?r . ,r)
                   (?R . ,(propertize r 'face 'shadow)))))))))

(defun keycast--read-passwd (fn prompt &optional confirm default)
  (let ((keycast--reading-passwd t))
    (funcall fn prompt confirm default)))

(advice-add 'read-passwd :around #'keycast--read-passwd)

(defun keycast-bottom-right-window-p ()
  (and (window-at-side-p nil 'right)
       (window-at-side-p nil 'bottom)))

(defun keycast-active-frame-bottom-right-p ()
  (and (keycast-bottom-right-window-p)
       (keycast--active-frame-p)))

(defun keycast--active-frame-p ()
  (cond ((boundp 'moody--active-window)
         (eq (window-frame) (window-frame moody--active-window)))
        ((boundp 'powerline-selected-window)
         (eq (window-frame) (window-frame powerline-selected-window)))
        (t t)))

;;; Mode-Line

(defvar keycast--removed-tail nil)

;;;###autoload
(define-minor-mode keycast-mode
  "Show current command and its key binding in the mode line."
  :global t
  (if keycast-mode
      (let ((cons (keycast--tree-member keycast-mode-line-insert-after mode-line-format)))
        (unless cons
          (setq keycast-mode nil)
          (user-error
           "Cannot turn on %s.  %s not found in %s.  Try customizing %s."
           'keycast-mode keycast-mode-line-insert-after 'mode-line-format
           'keycast-mode-line-insert-after))
        (cond (keycast-mode-line-remove-tail-elements
               (setq keycast--removed-tail (cdr cons))
               (setcdr cons (list 'keycast-mode-line)))
              (t
               (setcdr cons (cons 'keycast-mode-line (cdr cons)))))
        (add-hook 'pre-command-hook 'keycast--update t))
    (let ((cons (keycast--tree-member 'keycast-mode-line mode-line-format)))
      (cond (keycast--removed-tail
             (setcar cons (car keycast--removed-tail))
             (setcdr cons (cdr keycast--removed-tail)))
            (t
             (setcar cons (cadr cons))
             (setcdr cons (cddr cons)))))
    (setq keycast--removed-tail nil)
    (unless (keycast--mode-active-p)
      (remove-hook 'pre-command-hook 'keycast--update))))

(defun keycast--tree-member (elt tree)
  (or (member elt tree)
      (catch 'found
        (dolist (sub tree)
          (when-let ((found (and (listp sub)
                                 (keycast--tree-member elt sub))))
            (throw 'found found))))))

(defvar keycast-mode-line
  '(:eval
    (and (funcall keycast-mode-line-window-predicate)
         (keycast--format keycast-mode-line-format))))

(put 'keycast-mode-line 'risky-local-variable t)
(make-variable-buffer-local 'keycast-mode-line)

;;; Tab-Bar

(eval-when-compile
  (defvar tab-bar-format))

(defvar keycast--temporary-tab-bar nil)
(defvar keycast--previous-tab-bar-format nil)

;;;###autoload
(define-minor-mode keycast-tab-bar-mode
  "Show current command and its key binding in the tab bar."
  :global t
  (when (< emacs-major-version 28)
    (user-error "`keycast-tab-bar-mode' requires Emacs 28.1"))
  (cond
   (keycast-tab-bar-mode
    (unless tab-bar-mode
      (setq keycast--temporary-tab-bar t)
      (tab-bar-mode 1))
    (cl-case keycast-tab-bar-location
      (replace
       (setq keycast--previous-tab-bar-format tab-bar-format)
       (setq tab-bar-format (list 'keycast-tab-bar)))
      (beginning
       (setq tab-bar-format (cons 'keycast-tab-bar tab-bar-format)))
      (end
       (setq tab-bar-format (nconc tab-bar-format (list 'keycast-tab-bar))))
      (t
       (let ((mem (memq keycast-tab-bar-location tab-bar-format)))
         (if mem
             (setcdr mem (cons 'keycast-tab-bar (cdr mem)))
           (setq tab-bar-format
                 (nconc tab-bar-format
                        (if (eq keycast-tab-bar-location
                                'tab-bar-format-align-right)
                            (list 'tab-bar-format-align-right
                                  'keycast-tab-bar)
                          (message "%s not found in %s; adding at end instead"
                                   keycast-tab-bar-location 'tab-bar-format)
                          (list 'keycast-tab-bar))))))))
    (add-hook 'pre-command-hook 'keycast--update t))
   (t
    (when keycast--temporary-tab-bar
      (setq keycast--temporary-tab-bar nil)
      (tab-bar-mode -1))
    (cond (keycast--previous-tab-bar-format
           (setq tab-bar-format keycast--previous-tab-bar-format)
           (setq keycast--previous-tab-bar-format nil))
          (t
           (setq tab-bar-format (delq 'keycast-tab-bar tab-bar-format))))
    (unless (keycast--mode-active-p)
      (remove-hook 'pre-command-hook 'keycast--update)))))

(defun keycast-tab-bar ()
  "Produce key binding information for the tab bar."
  (and keycast-tab-bar-mode
       (keycast--active-frame-p)
       (when-let ((output (keycast--format keycast-tab-bar-format)))
         (concat output
                 (make-string (max 0 (- keycast-tab-bar-minimal-width
                                        (length output)))
                              ?\s)))))

;;; Log-Buffer

;;;###autoload
(define-minor-mode keycast-log-mode
  "Log invoked commands and their key bindings in a buffer."
  :global t
  (cond
   (keycast-log-mode
    (add-hook 'pre-command-hook 'keycast--update t)
    (keycast-log-update-buffer))
   ((not (keycast--mode-active-p))
    (remove-hook 'pre-command-hook 'keycast--update))))

(defun keycast-log-update-buffer ()
  (let ((buf (get-buffer keycast-log-buffer-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create keycast-log-buffer-name))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (setq mode-line-format nil)))
    (unless (get-buffer-window buf t)
      (display-buffer-pop-up-frame
       buf `((pop-up-frame-parameters . ,keycast-log-frame-alist))))
    (when-let ((output (keycast--format keycast-log-format)))
      (with-current-buffer buf
        (goto-char (if keycast-log-newest-first (point-min) (point-max)))
        (let ((inhibit-read-only t))
          (when (and (> keycast--command-repetitions 0)
                     (string-match-p "%[rR]" keycast-log-format))
            (unless keycast-log-newest-first
              (backward-char))
            (ignore-errors
              (delete-region (line-beginning-position)
                             (1+ (line-end-position)))))
          (insert output))
        (goto-char (if keycast-log-newest-first (point-min) (point-max)))))))

(defun keycast-log-erase-buffer ()
  "Erase the contents of `keycast-log-mode's buffer."
  (interactive)
  (let ((buf (get-buffer keycast-log-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer))))))

;;; _
(provide 'keycast)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keycast.el ends here
