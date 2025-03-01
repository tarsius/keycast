;;; keycast.el --- Show current command and its binding  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.keycast@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/keycast
;; Keywords: multimedia

;; Package-Version: 1.4.2
;; Package-Requires: ((emacs "28.1") (compat "30.0.2.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides four modes that display the current command and
;; its key or mouse binding.
;;
;; - `keycast-mode-line-mode' shows the current binding at the bottom of
;;   the selected window, in its mode line.
;;
;; - `keycast-header-line-mode' shows the current binding at the top of
;;   the selected window, in its header line.
;;
;; - `keycast-tab-bar-mode' shows the current binding at the top of
;;   the selected frame, in its tab bar.
;;
;; - `keycast-log-mode' displays a list of recent bindings in a dedicated
;;   frame.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'format-spec)

(eval-when-compile (require 'subr-x))

;;; Options
;;;; Common

(defgroup keycast nil
  "Show the current command and its key binding in the mode line."
  :group 'applications)

(defcustom keycast-show-minibuffer-exit-command 'log-only
  "Whether to show the command that exited the minibuffer.

If `log-only', the default, then show the command that exited the
minibuffer in the log buffer, but in places where just the latest
command is shown, show the command that used the minibuffer.

This is the default because if you use, e.g., `find-file' once,
then it is more reasonable to show

  RET      minibuffer-complete-and-exit
  x        self-insert-command x5
  C-x C-f  find-file

than this would be

  C-x C-f  find-file
  x        self-insert-command x5
  C-x C-f  find-file

Otherwise this has to be a boolean and its value applies to all
Keycast modes.

`execute-extended-command' is not affected by this option.  For
that \"M-x the-selected-command\" is displayed unconditionally."
  :group 'keycast
  :type '(choice (const :tag "Exiting command in log only" log-only)
                 (const :tag "Exiting command everywhere" t)
                 (const :tag "Using command everywhere" nil)))

;;;; Mode-Line

(defcustom keycast-mode-line-insert-after 'mode-line-buffer-identification
  "The position in `mode-line-format' where `keycast-mode-line' is inserted.

Enabling `keycast-mode-line-mode' inserts the element
`keycast-mode-line' into `mode-line-format' after the
element specified here."
  :group 'keycast
  :type '(choice :tag "Insert after"
                 (const mode-line-buffer-identification)
                 (const moody-mode-line-buffer-identification)
                 variable
                 sexp))

(defcustom keycast-mode-line-remove-tail-elements t
  "Whether enabling `keycast-mode-line-mode' removes elements to the right.

When this is non-nil, then enabling `keycast-mode-line-mode' not
only inserts `keycast-mode-line' into `mode-line-format' but also
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
  active window, i.e., if it is the selected window as far
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
  :type 'string)

;;;; Header-Line

(defcustom keycast-header-line-insert-after ""
  "The position in `header-line-format' where `keycast-header-line' is inserted.

Enabling `keycast-header-line-mode' inserts the element
`keycast-header-line' into `header-line-format' after the
element specified here."
  :group 'keycast
  :type '(choice :tag "Insert after"
                 string
                 variable
                 sexp))

(defcustom keycast-header-line-remove-tail-elements t
  "Whether enabling `keycast-header-line-mode' removes elements to the right.

When this is non-nil, then enabling `keycast-header-linemode' not
only inserts `keycast-header-line' into `header-line-format' but
also removes all elements to the right of where that was inserted."
  :group 'keycast
  :type 'boolean)

(defcustom keycast-header-line-format "%k%c%r "
  "The format spec used by `keycast-header-line'.

%s Some spaces, intended to be used like so: %10s.
%k The key using the `keycast-key' face and padding.
%K The key with no styling and without any padding.
%c The command using the `keycast-command' face.
%C The command with no styling.
%r The times the command was repeated.
%R The times the command was repeated using the `shadow' face."
  :package-version '(keycast . "1.0.3")
  :group 'keycast
  :type 'string)

;;;; Tab-Bar

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
  :type 'string)

(defcustom keycast-tab-bar-minimal-width 40
  "The minimal width of `keycast-tab-bar'."
  :package-version '(keycast . "2.0.0")
  :group 'keycast
  :type 'integer)

;;;; Log-Buffer

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

;;;; Common

(defcustom keycast-substitute-alist
  '((keycast-log-erase-buffer nil nil)
    (transient-update         nil nil))
  "Alist used to substituted events and/or commands for display.

Occasionally it might be necessary to pretend you pressed another
key than the one you actually pressed (because watchers don't
care about your weird key bindings), or to hide certain commands
\(such as `self-insert-command').  This option allows doing that
and more.

Each element has the form (MATCH EVENT COMMAND).  MATCH is an
event or a command.  When a command is invoked then this package
looks for a MATCH for that.  If and only if there is no match for
that, then it looks for a MATCH for its key binding.  If there is
a match of either kind, then the respective EVENT and COMMAND are
used.

If either EVENT or COMMAND is nil, then neither the event nor the
command is shown (regardless of the value of the other).

Otherwise if EVENT is t then the actual event is shown, else it
has to be a string to be shown instead.  Likewise COMMAND can be
t to show the actual COMMAND, or a symbol to be shown instead."
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

;;;; Faces

(defface keycast-key
  '((t ( :inherit fixed-pitch
         :weight bold
         :background "#d5cfbf"
         :foreground "#000000"
         :box (:line-width -3 :style released-button))))
  "When Keycast mode is enabled, face used for the key in the mode line."
  :group 'keycast)

(defface keycast-command '((t (:weight bold)))
  "When Keycast mode is enabled, face used for the command in the mode line."
  :group 'keycast)

;;; Common

(defvar keycast-mode-line-mode)
(defvar keycast-header-line-mode)
(defvar keycast-tab-bar-mode)
(defvar keycast-log-mode)

(defun keycast--mode-active-p (&optional line)
  (or keycast-mode-line-mode
      keycast-header-line-mode
      keycast-tab-bar-mode
      (and (not line) keycast-log-mode)))

(defvar keycast--this-command-desc nil)
(defvar keycast--this-command-keys nil)
(defvar keycast--minibuffer-exited nil)
(defvar keycast--command-repetitions 0)
(defvar keycast--reading-passwd nil)

(defvar keycast--prefix-argument-commands
  '(universal-argument
    universal-argument-more
    negative-argument
    digit-argument))

(defun keycast--minibuffer-exit ()
  (setq keycast--minibuffer-exited
        (cons (this-single-command-keys) this-command))
  ;; If the command that used the minibuffer is immediately
  ;; repeated, then don't treat that as a repetition because
  ;; at least the command that exited the minibuffer was used
  ;; in between (but `last-command' doesn't account for that).
  (setq keycast--command-repetitions -2))

(defun keycast--update ()
  (let ((key (this-single-command-keys))
        (cmd this-command))
    (cond
     ((memq this-original-command keycast--prefix-argument-commands)
      ;; These commands set `this-command' to `last-command', to
      ;; prevent `last-command' being the prefix command, for the
      ;; command that uses the prefix argument.
      (setq cmd this-original-command))
     ((and (not cmd)
           (length> key 0)
           (eq (aref key (1- (length key))) ?\C-g))
      ;; If a valid but incomplete prefix sequence is followed by
      ;; an unbound key, then Emacs calls the `undefined' command
      ;; but does not set `this-command', which is nil instead.
      (setq cmd 'undefined)))
    (cond
     ;; Special handling for `execute-extended-command'.
     ((eq this-original-command 'execute-extended-command)
      ;; Instead of "M-x t h e - c o m m a n d RET", just use
      ;; "M-x", because we follow that up with the command anyway.
      (setq key [?\M-x]))
     ;; If a command uses the minibuffer then `post-command-hook'
     ;; gets called twice on behalf of that command:
     ((equal key [])
      ;; 1. When the minibuffer is entered.  The key is void in that
      ;;    case, which allows us to detect that it is the first call,
      ;;    but also means we have to get the actual key like this.
      (setq key (this-single-command-raw-keys)))
     (keycast--minibuffer-exited
      ;; 2. When the minibuffer is exited (unless it is aborted).
      (when (eq keycast-show-minibuffer-exit-command t)
        (setq key (car keycast--minibuffer-exited))
        (setq cmd (cdr keycast--minibuffer-exited)))))
    (setq keycast--this-command-keys key)
    (setq keycast--this-command-desc (keycast--format-command cmd))
    (if (or (eq last-command cmd)
            (< keycast--command-repetitions 0))
        (cl-incf keycast--command-repetitions)
      (setq keycast--command-repetitions 0)))
  (when keycast-mode-line-mode
    (keycast--maybe-edit-local-format
     'mode-line-format
     'keycast-mode-line
     'keycast--mode-line-modified-buffers))
  (when keycast-header-line-mode
    (keycast--maybe-edit-local-format
     'header-line-format
     'keycast-header-line
     'keycast--header-line-modified-buffers))
  (when (and keycast-log-mode
             (not keycast--reading-passwd))
    (if (and keycast--minibuffer-exited
             (eq keycast-show-minibuffer-exit-command 'log-only)
             (not (eq this-original-command 'execute-extended-command)))
        (let ((keycast--this-command-keys (car keycast--minibuffer-exited))
              (keycast--this-command-desc (keycast--format-command
                                           (cdr keycast--minibuffer-exited))))
          (keycast-log-update-buffer))
      (keycast-log-update-buffer)))
  (setq keycast--minibuffer-exited nil)
  (when (keycast--mode-active-p 'line)
    (force-mode-line-update (minibufferp))))

(defun keycast--format-command (cmd)
  (cond ((symbolp cmd) cmd)
        ((eq (car-safe cmd) 'lambda) "<lambda>")
        (t (format "<%s>" (type-of cmd)))))

(defun keycast--maybe-edit-local-format (format item record)
  (let ((value (buffer-local-value format (current-buffer))))
    (unless (or (eq value (default-value format))
                (keycast--tree-member item value))
      (set record (cons (current-buffer) (symbol-value record)))
      (set format (if (keycast--format-atom-p value)
                      (list "" item value)
                    (nconc (list "" item) value))))))

(defun keycast--format-atom-p (format)
  (or (stringp format)
      (and (not (stringp (car-safe format)))
           (not (listp (car-safe format))))))

(defun keycast--format (format)
  (and (not keycast--reading-passwd)
       (let* ((key (ignore-errors
                     (key-description keycast--this-command-keys)))
              (cmd keycast--this-command-desc)
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

(define-advice read-passwd
    (:around (fn prompt &optional confirm default) keycast)
  "Suppress echoing keys while reading passwords."
  (let ((keycast--reading-passwd t))
    (funcall fn prompt confirm default)))

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

(defun keycast--tree-member (elt tree &optional delete)
  ;; Also known as auto-compile--tree-member.
  (and (listp tree)
       (if-let* ((pos (cl-position elt tree))
                 (mem (nthcdr pos tree)))
           (cond ((not delete) mem)
                 ((cdr mem)
                  (setcar mem (cadr mem))
                  (setcdr mem (cddr mem))
                  nil)
                 ((nbutlast tree) nil))
         (catch 'found
           (dolist (sub tree)
             (when-let ((found (keycast--tree-member elt sub delete)))
               (throw 'found found)))))))

;;; Mode-Line

(defvar keycast--mode-line-removed-tail nil)
(defvar keycast--temporary-mode-line nil)
(defvar keycast--mode-line-modified-buffers nil)
(defvar keycast--mode-line-enabled nil)

;;;###autoload
(define-minor-mode keycast-mode-line-mode
  "Show current command and its key binding in the mode line."
  :global t
  (cond
   (keycast-mode-line-mode
    (when keycast--mode-line-enabled
      (keycast-mode-line-mode -1)
      (setq keycast-mode-line-mode t))
    (let ((format (default-value 'mode-line-format)))
      (cond ((not format)
             (setq keycast--temporary-mode-line t)
             (setq-default mode-line-format (list "")))
            ((keycast--format-atom-p format)
             (setq-default mode-line-format (list "" format)))))
    (let ((cons (keycast--tree-member keycast-mode-line-insert-after
                                      (default-value 'mode-line-format))))
      (unless cons
        (setq keycast-mode-line-mode nil)
        (user-error
         "Cannot turn on %s.  %s not found in %s.  Try customizing %s."
         'keycast-mode-line-mode keycast-mode-line-insert-after
         'mode-line-format 'keycast-mode-line-insert-after))
      (cond (keycast-mode-line-remove-tail-elements
             (setq keycast--mode-line-removed-tail (cdr cons))
             (setcdr cons (list 'keycast-mode-line)))
            (t
             (setcdr cons (cl-pushnew 'keycast-mode-line (cdr cons)))))
      (add-hook 'post-command-hook #'keycast--update t)
      (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t))
    (setq keycast--mode-line-enabled t))
   (t
    (let ((cons (keycast--tree-member 'keycast-mode-line
                                      (default-value 'mode-line-format))))
      (cond (keycast--temporary-mode-line
             (setq-default mode-line-format nil)
             (setq keycast--temporary-mode-line nil))
            (keycast--mode-line-removed-tail
             (setcar cons (car keycast--mode-line-removed-tail))
             (setcdr cons (cdr keycast--mode-line-removed-tail))
             (setq keycast--mode-line-removed-tail nil))
            ((keycast--tree-member 'keycast-mode-line
                                   (default-value 'mode-line-format)
                                   'delete))))
    (while-let ((buf (pop keycast--mode-line-modified-buffers)))
      (when (and (buffer-live-p buf)
                 (condition-case nil
                     (listp (buffer-local-value 'mode-line-format buf))
                   (void-variable nil)))
        (with-current-buffer buf
          (keycast--tree-member 'keycast-mode-line mode-line-format 'delete))))
    (unless (keycast--mode-active-p)
      (remove-hook 'post-command-hook #'keycast--update)
      (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit))
    (setq keycast--mode-line-enabled nil))))

(defvar keycast-mode-line
  '(:eval
    (and (funcall keycast-mode-line-window-predicate)
         (keycast--format keycast-mode-line-format))))

(put 'keycast-mode-line 'risky-local-variable t)
(make-variable-buffer-local 'keycast-mode-line)

;;; Header-Line

(defvar keycast--temporary-header-line nil)
(defvar keycast--header-line-removed-tail nil)
(defvar keycast--header-line-modified-buffers nil)

;;;###autoload
(define-minor-mode keycast-header-line-mode
  "Show current command and its key binding in the header line."
  :global t
  (cond
   (keycast-header-line-mode
    (cond ((not (default-value 'header-line-format))
           (setq keycast--temporary-header-line t)
           (setq-default header-line-format (list "")))
          ((keycast--format-atom-p header-line-format)
           (setq-default header-line-format (list "" header-line-format))))
    (let ((cons (keycast--tree-member keycast-header-line-insert-after
                                      (default-value 'header-line-format))))
      (unless cons
        (setq keycast-header-line-mode nil)
        (user-error
         "Cannot turn on %s.  %s not found in %s.  Try customizing %s."
         'keycast-header-line-mode keycast-header-line-insert-after
         'header-line-format 'keycast-header-line-insert-after))
      (cond (keycast-header-line-remove-tail-elements
             (setq keycast--header-line-removed-tail (cdr cons))
             (setcdr cons (list 'keycast-header-line)))
            (t
             (setcdr cons (cl-pushnew 'keycast-header-line (cdr cons)))))
      (add-hook 'post-command-hook #'keycast--update t)
      (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t)))
   (t
    (let ((cons (keycast--tree-member 'keycast-header-line
                                      (default-value 'header-line-format))))
      (cond (keycast--temporary-header-line
             (setq keycast--temporary-header-line nil)
             (setq-default header-line-format nil))
            (keycast--header-line-removed-tail
             (setcar cons (car keycast--header-line-removed-tail))
             (setcdr cons (cdr keycast--header-line-removed-tail)))
            ((keycast--tree-member 'keycast-header-line
                                   (default-value 'header-line-format)
                                   'delete))))
    (setq keycast--header-line-removed-tail nil)
    (dolist (buf keycast--header-line-modified-buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (unless (stringp header-line-format)
            (setq header-line-format
                  (delq 'keycast-header-line header-line-format))))))
    (unless (keycast--mode-active-p)
      (remove-hook 'post-command-hook #'keycast--update)
      (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)))))

(defvar keycast-header-line
  '(:eval
    (and (funcall keycast-mode-line-window-predicate)
         (keycast--format keycast-header-line-format))))

(put 'keycast-header-line 'risky-local-variable t)
(make-variable-buffer-local 'keycast-header-line)

;;; Tab-Bar

(eval-when-compile
  (defvar tab-bar-format)
  (defvar tab-bar-mode))
(declare-function tab-bar-mode "tab-bar" (&optional arg))

(defvar keycast--temporary-tab-bar nil)
(defvar keycast--previous-tab-bar-format nil)

;;;###autoload
(define-minor-mode keycast-tab-bar-mode
  "Show current command and its key binding in the tab bar."
  :global t
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
             (setcdr mem (cl-pushnew 'keycast-tab-bar (cdr mem)))
           (setq tab-bar-format
                 (nconc tab-bar-format
                        (if (eq keycast-tab-bar-location
                                'tab-bar-format-align-right)
                            (list 'tab-bar-format-align-right
                                  'keycast-tab-bar)
                          (message "%s not found in %s; adding at end instead"
                                   keycast-tab-bar-location 'tab-bar-format)
                          (list 'keycast-tab-bar))))))))
    (add-hook 'post-command-hook #'keycast--update t)
    (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t))
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
      (remove-hook 'post-command-hook #'keycast--update)
      (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)))))

(defun keycast-tab-bar ()
  "Produce key binding information for the tab bar."
  (and keycast-tab-bar-mode
       (keycast--active-frame-p)
       (and-let* ((output (keycast--format keycast-tab-bar-format)))
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
    (add-hook 'post-command-hook #'keycast--update t)
    (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t)
    (keycast-log--set-focus-properties t)
    (keycast-log-update-buffer))
   ((not (keycast--mode-active-p))
    (remove-hook 'post-command-hook #'keycast--update)
    (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)
    (keycast-log--set-focus-properties nil))))

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

(defun keycast-log--set-focus-properties (value)
  (when-let* ((buffer (get-buffer keycast-log-buffer-name))
              (window (get-buffer-window buffer t))
              (frame (window-frame window)))
    (when (cdr (assq 'no-accept-focus keycast-log-frame-alist))
      (set-frame-parameter frame 'no-accept-focus value))
    (when (cdr (assq 'no-accept-on-map keycast-log-frame-alist))
      (set-frame-parameter frame 'no-focus-on-map value))))

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
