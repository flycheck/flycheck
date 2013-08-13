;;; flycheck.el --- On-the-fly syntax checking (Flymake done right) -*- lexical-binding: t; -*-

;; Copyright (c) 2012, 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck
;; Keywords: convenience languages tools
;; Version: 0.14-cvs
;; Package-Requires: ((s "1.6.0") (dash "1.2") (cl-lib "0.1") (emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Modern on-the-fly syntax checking for GNU Emacs (aka "flymake done right")

;; Provide `flycheck-mode' which enables on-the-fly syntax checking for a large
;; number of different modes and languages (see `flycheck-checkers' for a
;; complete list).
;;
;; Support for new modes and languages can be added by defining a new syntax
;; checker (see `flycheck-define-checker').

;;; Code:

(eval-when-compile
  (require 'jka-compr)                  ; For JKA workarounds in
                                        ; `flycheck-temp-file-system'
  (require 'compile)                    ; Compile Mode integration
  (require 'sh-script))                 ; `sh-shell' for sh checker predicates

(require 's)
(require 'dash)
(require 'rx)             ; For Regexp fanciness in `flycheck-define-checker'
(require 'cl-lib)         ; For `cl-defstruct'
(require 'help-mode)      ; For `define-button-type'
(require 'find-func)      ; For `find-function-space-re', etc.
(require 'package)        ; For `package-buffer-info' and `package-version-join'


;;;; Compatibility
(eval-and-compile
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val)))

  (unless (fboundp 'user-error)
    ;; Provide `user-error' for Emacs 24.2
    (defalias 'user-error 'error)
    ;; And make the debugger ignore our Flycheck user errors in Emacs 24.2
    (add-to-list 'debug-ignored-errors
                 (rx string-start "No more Flycheck errors" string-end))
    (add-to-list 'debug-ignored-errors
                 (rx string-start "Flycheck mode disabled" string-end))
    (add-to-list 'debug-ignored-errors
                 (rx string-start "Configured syntax checker "
                     symbol-start (one-or-more not-newline) symbol-end
                     " cannot be used" string-end))))


;;;; Customization
(defgroup flycheck nil
  "On-the-fly syntax checking (aka \"flymake done right\")."
  :prefix "flycheck-"
  :group 'tools
  :link '(url-link :tag "Online manual" "http://flycheck.lunaryorn.com")
  :link '(url-link :tag "Github" "https://github.com/lunaryorn/flycheck")
  :link '(custom-manual "(flycheck)Top")
  :link '(info-link "(flycheck)Usage"))

(defgroup flycheck-config-files nil
  "Configuration files for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck
  :link '(info-link "(flycheck)Configuration"))

(defgroup flycheck-options nil
  "Options for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck
  :link '(info-link "(flycheck)Configuration"))

(defgroup flycheck-faces nil
  "Faces used by on-the-fly syntax checking."
  :prefix "flycheck-"
  :group 'flycheck
  :link '(info-link "(flycheck)Configuration"))

(defcustom flycheck-checkers
  '(bash
    c/c++-clang
    c/c++-cppcheck
    coffee-coffeelint
    css-csslint
    elixir
    emacs-lisp
    emacs-lisp-checkdoc
    erlang
    go-gofmt
    go-build
    go-test
    haml
    haskell-hdevtools
    haskell-ghc
    haskell-hlint
    html-tidy
    javascript-jshint
    json-jsonlint
    less
    lua
    perl
    php
    php-phpcs
    puppet-parser
    puppet-lint
    python-flake8
    python-pylint
    rst
    ruby-rubocop
    ruby
    ruby-jruby
    rust
    sass
    scala
    scss
    sh-dash
    sh-bash
    tex-chktex
    tex-lacheck
    xml-xmlstarlet
    zsh)
  "Syntax checkers available for automatic selection.

A list of Flycheck syntax checkers to choose from when syntax
checking a buffer.  Flycheck will automatically select a suitable
syntax checker from this list, unless `flycheck-checker' is set,
either directly or with `flycheck-select-checker'.

Syntax checkers in this list must be defined with
`flycheck-define-checker'."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker"))
  :risky t)

(defvar-local flycheck-checker nil
  "Syntax checker to use for the current buffer.

If unset or nil, automatically select a suitable syntax checker
from `flycheck-checkers' on every syntax check.

If set to a syntax checker only use this syntax checker and never
select one from `flycheck-checkers' automatically.  If the syntax
checker is unusable in the current buffer an error is signaled.

A syntax checker assigned to this variable must be defined with
`flycheck-define-checker'.

Use the command `flycheck-select-checker' to select a syntax
checker for the current buffer, or set this variable as file
local variable to always use a specific syntax checker for a
file.")
(put 'flycheck-checker 'safe-local-variable 'flycheck-registered-checker-p)

(defcustom flycheck-locate-config-file-functions
  '(flycheck-locate-config-file-absolute-path
    flycheck-locate-config-file-projectile
    flycheck-locate-config-file-ancestor-directories
    flycheck-locate-config-file-home)
  "Functions to locate syntax checker configuration files.

Each function in this hook must accept two arguments: The value
of the configuration file variable, and the syntax checker
symbol.  It must return either a string with an absolute path to
the configuration file, or nil, if it cannot locate the
configuration file.

The functions in this hook are called in order of appearance, until a
function returns non-nil.  The configuration file returned by that
function is then given to the syntax checker if it exists.

This variable is an abnormal hook."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-process-error-functions '(flycheck-add-overlay)
  "Functions to process errors.

Each function in this hook must accept a single argument: A
Flycheck error to process.

All functions in this hook are called in order of appearance,
until a function returns non-nil.  Thus, a function in this hook
may return nil, to allow for further processing of the error, or
t, to indicate that the error was fully processed and inhibit any
further processing.

The functions are called for each newly parsed error immediately
after the corresponding syntax checker finished.  At this stage,
the overlays from the previous syntax checks are still present,
and there may be further syntax checkers in the chain.

This variable is an abnormal hook."
  :group 'flycheck
  :type 'hook
  :package-version '(flycheck . "0.13")
  :risky t)

(defcustom flycheck-display-errors-function #'flycheck-display-error-messages
  "Function to display error messages.

If set to a function, call the function with the list of errors
to display as single argument.  Each error is an instance of the
`flycheck-error' struct.

If set to nil, do not display errors at all."
  :group 'flycheck
  :type '(choice (const :tag "Display error messages"
                        flycheck-display-error-messages)
                 (const :tag "Display errors in error list"
                        flycheck-display-errors-in-list)
                 (function :tag "Error display function"))
  :package-version '(flycheck . "0.13")
  :risky t)

(defcustom flycheck-indication-mode 'left-fringe
  "The indication mode for Flycheck errors and warnings.

Controls how Flycheck indicates errors in buffers.  May either be
`left-fringe', `right-fringe', or nil.

If set to `left-fringe' or `right-fringe', indicate errors and
warnings via icons in the left and right fringe respectively.

If set to nil, do not indicate errors and warnings, but just
highlight them according to `flycheck-highlighting-mode'."
  :group 'flycheck
  :type '(choice (const :tag "Indicate in the left fringe" left-fringe)
                 (const :tag "Indicate in the right fringe" right-fringe)
                 (const :tag "Do not indicate" nil))
  :safe #'symbolp)

(defcustom flycheck-highlighting-mode 'sexps
  "The highlighting mode for Flycheck errors and warnings.

The highlighting mode controls how Flycheck highlights errors in
buffers.  The following modes are known:

`columns'
     highlights the error column.  If the error does not have a
     column, highlight the whole line.

`sexps'
     highlights the expression at the error column, if there is
     any, otherwise behave like `columns'.

`lines'
     highlights the whole line.

nil
     does not highlight errors at all.  Errors will still be
     indicated according to `flycheck-indication-mode'.

Note that this does not affect error navigation.  When navigating
errors with `next-error' and `previous-error' Flycheck always
jumps to the error column regardless of the highlighting mode."
  :group 'flycheck
  :type '(choice (const :tag "Highlight columns only" columns)
                 (const :tag "Highlight expressions" sexps)
                 (const :tag "Highlight whole lines" lines)
                 (const :tag "Do not highlight errors" nil))
  :package-version '(flycheck . "0.12")
  :safe #'symbolp)

(defcustom flycheck-check-syntax-automatically '(save
                                                 idle-change
                                                 new-line
                                                 mode-enabled)
  "When Flycheck should check syntax automatically.

This variable is a list of events that may trigger syntax checks.
The following events are known:

`save'
     checks syntax automatically each time the buffer is saved.

`idle-change'
     checks syntax automatically some time after the last change
     to the buffer occurred.

`new-line'
     checks syntax automatically each time a new line is inserted
     into the buffer.

`mode-enabled'
     checks syntax automatically when `flycheck-mode' is enabled.

For instance, set this variable to '(mode-enabled save) to only
check syntax automatically when saving a buffer, but never when
modifying its contents.

If nil, never check syntax automatically.  Use `flycheck-buffer'
to start a syntax check manually."
  :group 'flycheck
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After the buffer was changed and idle" idle-change)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After `flycheck-mode' was enabled" mode-enabled))
  :package-version '(flycheck . "0.12")
  :safe #'symbolp)

(defcustom flycheck-idle-change-delay 0.5
  "How many seconds to wait before checking syntax automatically.

After the buffer was changed, Flycheck will wait as many seconds
as the value of this variable before starting a syntax check.  If
the buffer is modified during this time, Flycheck will wait
again.

This variable has no effect, if `idle-change' is not contained in
`flycheck-check-syntax-automatically'."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "0.13")
  :safe #'numberp)

(defcustom flycheck-google-max-messages 5
  "How many messages to google at once.

If set to an integer, `flycheck-google-messages' will signal an
error if there are more Flycheck messages at point than the value
of this variable.

If set to nil, `flycheck-google-messages' will always google all
messages at point."
  :group 'flycheck
  :type '(choice (const :tag "Always google all messages" nil)
                 (integer :tag "Maximum messages to google"))
  :package-version '(flycheck . "0.10")
  :safe #'numberp)

(defcustom flycheck-mode-hook nil
  "Hooks to run after `flycheck-mode'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-after-syntax-check-hook nil
  "Functions to run after each syntax check.

This hook is run after a syntax check was finished.

At this point, *all* chained checkers were run, and all errors
were parsed, highlighted and reported.  The variable
`flycheck-current-errors' contains all errors from all syntax
checkers run during the syntax check, so you can use the various
error analysis functions.

Note that this hook does *not* run after each individual syntax
checker in the syntax checker chain, but only after the *last
checker*.

This variable is a normal hook."
  :group 'flycheck
  :type 'hook)

(defcustom flycheck-before-syntax-check-hook nil
  "Functions to run before each syntax check.

This hook is run right before a syntax check starts.

Error information from the previous syntax check is *not*
cleared before this hook runs.

Note that this hook does *not* run before each individual syntax
checker in the syntax checker chain, but only before the *first
checker*.

This variable is a normal hook."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-syntax-check-failed-hook nil
  "Functions to run if a syntax check failed.

This hook is run whenever an error occurs during Flycheck's
internal processing.  No information about the error is given to
this hook.

You should use this hook to conduct additional cleanup actions
when Flycheck failed.

This variable is a normal hook."
  :group 'flycheck
  :type 'hook
  :risky t)

(defface flycheck-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Flycheck face for errors."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-warning
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (t
     :underline t :inherit warning))
  "Flycheck face for warnings."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-fringe-error
  '((t :inherit error))
  "Flycheck face for fringe error indicators."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-face)

(defface flycheck-fringe-warning
  '((t :inherit warning))
  "Flycheck face for fringe warning indicators."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-face)


;;;; Minor mode definition
;;;###autoload
(defconst flycheck-mode-line-lighter " FlyC"
  "The standard lighter for flycheck mode.")

(defvar-local flycheck-mode-line nil
  "The mode line lighter of variable `flycheck-mode'.")

(defvar flycheck-mode-map
  (let ((map (make-sparse-keymap))
        (pmap (make-sparse-keymap)))
    (define-key pmap "c" 'flycheck-buffer)
    (define-key pmap "C" 'flycheck-clear)
    (define-key pmap (kbd "C-c") 'flycheck-compile)
    (define-key pmap "n" 'flycheck-next-error)
    (define-key pmap "p" 'flycheck-previous-error)
    (define-key pmap "l" 'flycheck-list-errors)
    (define-key pmap (kbd "C-w") 'flycheck-copy-messages-as-kill)
    (define-key pmap "/" 'flycheck-google-messages)
    (define-key pmap "s" 'flycheck-select-checker)
    (define-key pmap "?" 'flycheck-describe-checker)
    (define-key pmap "i" 'flycheck-info)
    (define-key pmap "V" 'flycheck-version)
    (define-key map (kbd "C-c !") pmap)
    map)
  "Keymap of `flycheck-mode'.")

(easy-menu-change
 '("Tools") "Syntax Checking"
 '(["Check current buffer" flycheck-buffer t]
   ["Clear errors in buffer" flycheck-clear t]
   ["Compile current buffer" flycheck-compile t]
   "---"
   ["Go to next error" flycheck-next-error t]
   ["Go to next error" flycheck-previous-error t]
   ["Show all errors" flycheck-list-errors t]
   ["Google messages at point" flycheck-google-messages t]
   "---"
   ["Select syntax checker" flycheck-select-checker t]
   "---"
   ["Describe syntax checker" flycheck-describe-checker t]
   ["Read the Flycheck manual" flycheck-info t])
 "Spell Checking")

(easy-menu-change '("Tools") "--" nil "Spell Checking")

(defun flycheck-teardown ()
  "Teardown flyheck.

Completely clear the whole flycheck state.  Remove overlays, kill
running checks, and empty all variables used by flycheck."
  (flycheck-clean-deferred-check)
  (flycheck-clear)
  (flycheck-stop-checker)
  (flycheck-cancel-error-show-error-timer)
  (flycheck-safe-delete-temporaries)
  (flycheck-clear-checker))

(defvar-local flycheck-previous-next-error-function nil
  "Remember the previous `next-error-function'.")

(defconst flycheck-hooks-alist
  '(
    ;; Handle events that may start automatic syntax checks
    (after-save-hook                  . flycheck-handle-save)
    (after-change-functions           . flycheck-handle-change)
    ;; Handle events that may triggered pending deferred checks
    (window-configuration-change-hook . flycheck-perform-deferred-syntax-check)
    (post-command-hook                . flycheck-perform-deferred-syntax-check)
    ;; Tear down Flycheck if the buffer or Emacs are killed, or if the major
    ;; mode changes, to clean up temporary files and directories.
    (kill-buffer-hook                 . flycheck-teardown)
    (kill-emacs-hook                  . flycheck-teardown)
    (change-major-mode-hook           . flycheck-teardown)
    ;; Show or hide error popups after commands
    (post-command-hook                . flycheck-show-error-at-point-soon)
    (post-command-hook                . flycheck-hide-error-buffer)
    ;; Immediately show error popups when navigating to an error
    (next-error-hook                  . flycheck-show-error-at-point))
  "Hooks which Flycheck needs to hook in.

The `car' of each pair is a hook variable, the `cdr' a function
to be added or removed from the hook variable if Flycheck mode is
enabled and disabled respectively.")

;;;###autoload
(define-minor-mode flycheck-mode
  "Minor mode for on-the-fly syntax checking.

When called interactively, toggle `flycheck-mode'.  With prefix
ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `flycheck-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
Otherwise behave as if called interactively.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

\\{flycheck-mode-map}"
  :init-value nil
  :keymap flycheck-mode-map
  :lighter flycheck-mode-line
  :group 'flycheck
  :require 'flycheck
  :after-hook (flycheck-buffer-automatically 'mode-enabled :force-deferred)
  (cond
   (flycheck-mode
    (flycheck-clear)

    (--each flycheck-hooks-alist
      (add-hook (car it) (cdr it) nil t))

    (setq flycheck-previous-next-error-function next-error-function)
    (setq next-error-function 'flycheck-next-error-function))
   (t
    (setq next-error-function flycheck-previous-next-error-function)

    (--each flycheck-hooks-alist
      (remove-hook (car it) (cdr it) t))

    (flycheck-teardown))))


;;; Version information
(defun flycheck-library-version ()
  "Get the version in the Flycheck library header."
  (-when-let* ((definition (symbol-function 'flycheck-mode))
               (source-file (find-lisp-object-file-name 'flycheck-mode
                                                        definition)))
    (with-temp-buffer
      (insert-file-contents source-file)
      (let ((info (package-buffer-info)))
        (package-version-join
         (if (fboundp 'package-desc-version)
             (package-desc-version info)
           (version-to-list (aref (package-buffer-info) 3))))))))

(defun flycheck-package-version ()
  "Get the package version of Flycheck.

This is the version number of the installed Flycheck package."
  (when (boundp 'package-alist)
    (-when-let (info (assq 'flycheck package-alist))
      (package-version-join
       (if (fboundp 'package-desc-version)
           (package-desc-version (cadr info))
         (aref (cdr info) 0))))))

(defun flycheck-version (&optional show-version)
  "Get the Flycheck version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list (not (or executing-kbd-macro noninteractive))))
  (let* ((lib-version (flycheck-library-version))
         (pkg-version (flycheck-package-version))
         (version (cond
                   ((and lib-version pkg-version
                         (not (string= lib-version pkg-version)))
                    (format "%s (package: %s)" lib-version pkg-version))
                   ((or pkg-version lib-version)
                    (format "%s" (or pkg-version lib-version))))))
    (when show-version
      (unless version
        (error "Could not find out Flycheck version"))
      (message "Flycheck version: %s" version))
    version))


;;; Global syntax checking
(defun flycheck-may-enable-mode ()
  "Determine whether Flycheck mode may be enabled.

Flycheck mode is not enabled under any of the following
conditions

The current buffer is a temporary buffer as determined by
`flycheck-temporary-buffer-p'.

The current buffer refers to a remote file, as determined by
`file-remote-p'.

No suitable syntax checker exists for the current buffer.

Return t if Flycheck mode may be enabled, and nil otherwise."
  (and (not (flycheck-temporary-buffer-p))
       (not (and (buffer-file-name) (file-remote-p (buffer-file-name) 'method)))
       (flycheck-get-checker-for-buffer)))

(defun flycheck-mode-on-safe ()
  "Enable `flycheck-mode' if it is safe to do so.

`flycheck-mode' is only enabled if `flycheck-may-enable-mode'
returns t."
  (when (flycheck-may-enable-mode)
    (flycheck-mode)))

;;;###autoload
(define-globalized-minor-mode global-flycheck-mode flycheck-mode
  flycheck-mode-on-safe
  :init-value nil
  :group 'flycheck
  :require 'flycheck)


;;; Manual syntax checking
(defun flycheck-clear ()
  "Clear all errors in the current buffer."
  (interactive)
  (flycheck-delete-all-overlays)
  (flycheck-clear-errors)
  (flycheck-hide-error-buffer))

(defun flycheck-buffer ()
  "Check syntax in the current buffer."
  (interactive)
  (flycheck-clean-deferred-check)
  (if flycheck-mode
      (unless (flycheck-running-p)
        ;; Clear error list and mark all overlays for deletion.  We do not
        ;; delete all overlays immediately to avoid excessive re-displays and
        ;; flickering, if the same errors gets highlighted again after the check
        ;; completed.
        (run-hooks 'flycheck-before-syntax-check-hook)
        (flycheck-clear-errors)
        (flycheck-mark-all-overlays-for-deletion)
        (condition-case err
            (let ((checker (flycheck-get-checker-for-buffer)))
              (if checker
                  (flycheck-start-checker checker)
                (flycheck-clear)
                (flycheck-report-status "-")))
          (error
           (flycheck-report-error)
           (signal (car err) (cdr err)))))
    (user-error "Flycheck mode disabled")))

(defun flycheck-compile-name (_name)
  "Get a name for a Flycheck compilation buffer.

_NAME is ignored."
  (format "*Flycheck %s*" (buffer-file-name)))

(defun flycheck-compile ()
  "Run syntax checker as compiler."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Cannot compile buffers without backing file"))
  (let ((checker (flycheck-get-checker-for-buffer)))
    (if checker
        (let* ((command (flycheck-checker-shell-command checker))
               (buffer (compilation-start command nil
                                          #'flycheck-compile-name)))
          (with-current-buffer buffer
            (set (make-local-variable 'compilation-error-regexp-alist)
                 (flycheck-checker-compilation-error-regexp-alist checker))))
      (user-error "No suitable checker available"))))


;;;; Deferred syntax checking
(defvar-local flycheck-deferred-syntax-check nil
  "If non-nil, a deferred syntax check is pending.")

(defun flycheck-must-defer-check ()
  "Determine whether the syntax check has to be deferred.

A check has to be deferred if the buffer is not visible, or if the buffer is
currently being reverted.

Return t if the check is to be deferred, or nil otherwise."
  (or (not (get-buffer-window))
      ;; We must defer checks while a buffer is being reverted, to avoid race
      ;; conditions while the buffer contents are being restored.
      revert-buffer-in-progress-p))

(defun flycheck-deferred-check-p ()
  "Determine whether the current buffer has a deferred check.

Return t if so, or nil otherwise."
  flycheck-deferred-syntax-check)

(defun flycheck-buffer-deferred ()
  "Defer syntax check for the current buffer."
  (setq flycheck-deferred-syntax-check t))

(defun flycheck-clean-deferred-check ()
  "Clean an deferred syntax checking state."
  (setq flycheck-deferred-syntax-check nil))

(defun flycheck-perform-deferred-syntax-check ()
  "Perform any deferred syntax checks."
  (when (flycheck-deferred-check-p)
    (flycheck-clean-deferred-check)
    (flycheck-buffer-automatically)))


;;;; Automatic syntax checking
(defun flycheck-may-check-automatically (&optional condition)
  "Determine whether the buffer may be checked under CONDITION.

Read-only buffers may never be checked automatically.

If CONDITION is non-nil, determine whether syntax may checked
automatically according to
`flycheck-check-syntax-automatically'."
  (and (not buffer-read-only)
       (or (not condition)
           (memq condition flycheck-check-syntax-automatically))))

(defun flycheck-buffer-automatically (&optional condition force-deferred)
  "Automatically check syntax at CONDITION.

Syntax is not checked if `flycheck-may-check-automatically'
returns nil for CONDITION.

The syntax check is deferred if FORCE-DEFERRED is non-nil, or if
`flycheck-must-defer-check' returns t."
  (when (and flycheck-mode (flycheck-may-check-automatically condition))
    (if (or force-deferred (flycheck-must-defer-check))
        (flycheck-buffer-deferred)
      (with-demoted-errors
        (flycheck-buffer)))))

(defvar-local flycheck-idle-change-timer nil
  "Timer to mark the idle time since the last change.")

(defun flycheck-clear-idle-change-timer ()
  "Clear the idle change timer."
  (when flycheck-idle-change-timer
    (cancel-timer flycheck-idle-change-timer)
    (setq flycheck-idle-change-timer nil)))

(defun flycheck-handle-change (beg end _len)
  "Handle a buffer change between BEG and END.

BEG and END mark the beginning and end of the change text.  _LEN
is ignored.

Start a syntax check if a new line has been inserted into the
buffer."
  ;; Save and restore the match data, as recommended in (elisp)Change Hooks
  (save-match-data
    (when flycheck-mode
      ;; The buffer was changed, thus clear the idle timer
      (flycheck-clear-idle-change-timer)
      (if (s-contains? "\n" (buffer-substring beg end))
          (flycheck-buffer-automatically 'new-line :force-deferred)
        (setq flycheck-idle-change-timer
              (run-at-time flycheck-idle-change-delay nil #'flycheck-handle-idle-change))))))

(defun flycheck-handle-idle-change ()
  "Handle an expired idle time since the last change."
  (flycheck-clear-idle-change-timer)
  (flycheck-buffer-automatically 'idle-change :force-deferred))

(defun flycheck-handle-save ()
  "Handle a save of the buffer."
  (flycheck-buffer-automatically 'save))


;;;; Mode line reporting
(defun flycheck-report-status (status)
  "Report Flycheck STATUS."
  (let ((mode-line flycheck-mode-line-lighter))
    (setq mode-line (concat mode-line status))
    (setq flycheck-mode-line mode-line)
    (force-mode-line-update)))

(defun flycheck-report-error ()
  "Report a Flycheck error status.

Clears all Flycheck errors first."
  (flycheck-clear)
  (run-hooks 'flycheck-syntax-check-failed-hook)
  (flycheck-report-status "!"))

(defun flycheck-report-error-count (errors)
  "Report ERRORS in the current buffer.

Report a proper flycheck status."
  (if errors
      (let ((no-err-warnings (flycheck-count-errors errors)))
        (flycheck-report-status
         (format ":%s/%s" (car no-err-warnings) (cdr no-err-warnings))))
    (flycheck-report-status "")))


;;;; Utility functions
(defun flycheck-sexp-to-string (sexp)
  "Convert SEXP to a string.

Like `prin1-to-string' but ensure that the returned string
is loadable."
  (let ((print-quoted t)
        (print-length nil)
        (print-level nil))
    (prin1-to-string sexp)))

(defun flycheck-string-to-number-safe (string)
  "Safely convert STRING to a number.

If STRING is of string type, and a numeric string (see
`s-numeric?'), convert STRING to a number and return it.
Otherwise return nil."
  (when (and (stringp string) (s-numeric? string))
    (string-to-number string)))

(defun flycheck-string-list-p (obj)
  "Determine if OBJ is a list of strings."
  (and (listp obj) (-all? #'stringp obj)))

(defvar-local flycheck-temp-files nil
  "A list of temporary files created by Flycheck.")

(defvar-local flycheck-temp-directories nil
  "A list of all temporary directories created by Flycheckg.")

(defun flycheck-temp-dir-system (prefix)
  "Create a unique temporary directory from PREFIX.

Add the directory to `flycheck-temp-directories'.

Return the path of the directory"
  (let* ((tempdir (make-temp-file prefix :directory)))
    (add-to-list 'flycheck-temp-directories tempdir)
    tempdir))

(defun flycheck-temp-file-system (filename prefix)
  "Create a temporary file named after FILENAME with PREFIX.

If FILENAME is nil, this function creates a temporary file with
PREFIX and a random suffix.  The path of the file is added to
`flycheck-temp-files'.

If FILENAME is non-nil, this function creates a temporary
directory with PREFIX and a random suffix using
`flycheck-temp-dir-system', and creates a file with the same name
as FILENAME in this directory.  The path of the file is *not*
added to `flycheck-temp-files', because the directory is already
tracked as temp file.

Return the path of the file."
  ;; HACK: Prevent re-compression to work around a supposed bug in Emacs.
  ;; `make-temp-file' calls `write-region' to set the contents of the new
  ;; temporary file, which in turn calls `jka-compr-write-region' for compressed
  ;; files. If `jka-compr-really-do-compress' is non-nil this function uses END
  ;; even though START is a string, hence breaking the `write-region' API that
  ;; we rely on.  Report upstream!
  (let ((jka-compr-really-do-compress nil)
        tempfile)
    (if filename
        (let ((directory (flycheck-temp-dir-system prefix)))
          (setq tempfile (expand-file-name (file-name-nondirectory filename)
                                           directory)))
      (setq tempfile (make-temp-file prefix)))
    (add-to-list 'flycheck-temp-files tempfile)
    tempfile))

(defun flycheck-temp-file-inplace (filename prefix)
  "Create an in-place copy of FILENAME with PREFIX added.

Add the path of the file to `flycheck-temp-files'.

If FILENAME is nil, fall back to `flycheck-temp-file-system'.

Return the path of the file."
  (if filename
      (let* ((directory (file-name-directory filename))
             (name (file-name-nondirectory filename))
             (tempname (format "%s-%s" prefix name))
             (tempfile (expand-file-name tempname directory)))
        (add-to-list 'flycheck-temp-files tempfile)
        tempfile)
    (flycheck-temp-file-system filename prefix)))

(defun flycheck-canonical-file-name (filename)
  "Turn FILENAME into canonical form.

Return FILENAME expanded and fully resolved, in a canonical form
without double slashes and without trailing slash, i.e. in a form
suitable for comparison of file names."
  (directory-file-name (file-truename filename)))

(defun flycheck-same-files-p (file1 file2)
  "Determine whether two files FILE1 and FILE2 are the same."
  (string= (flycheck-canonical-file-name file1)
           (flycheck-canonical-file-name file2)))

(defun flycheck-save-buffer-to-file (file-name)
  "Save the contents of the current buffer to FILE-NAME."
  (make-directory (file-name-directory file-name) t)
  (write-region nil nil file-name nil 0))

(defun flycheck-save-buffer-to-temp (temp-file-fn prefix)
  "Save buffer to temp file returned by TEMP-FILE-FN with PREFIX.

Return the name of the temporary file."
  (let ((filename (funcall temp-file-fn (buffer-file-name) prefix)))
    ;; Do not flush short-lived temporary files onto disk
    (let ((write-region-inhibit-fsync t))
      (flycheck-save-buffer-to-file filename))
    filename))

(defun flycheck-option-with-value-argument (option value)
  "Create arguments specifying OPTION with VALUE.

OPTION is a string denoting the option to pass, VALUE a string
containing the value for this option.

If OPTION ends with a equal sign =, OPTION and VALUE are
concatenated to a single string, which is then wrapped in a list
and returned.

Otherwise `(list OPTION VALUE)' is returned."
  (if (s-ends-with? "=" option)
      (list (concat option value))
    (list option value)))

(defun flycheck-prepend-with-option (option items &optional prepend-fn)
  "Prepend OPTION to each item in ITEMS, using PREPEND-FN.

Prepend OPTION to each item in ITEMS.

ITEMS is a list of strings to pass to the syntax checker.  OPTION
is the option, as string.  PREPEND-FN is a function called to
prepend OPTION to each item in ITEMS.  If nil or omitted, use
`list'.

Return a flattened list where OPTION is prepended to each item in
ITEMS."
  (unless (stringp option)
    (error "Option %S is not a string" option))
  (unless prepend-fn
    (setq prepend-fn #'list))
  (-flatten (--map (funcall prepend-fn option it) items)))

(defun flycheck-temporary-buffer-p ()
  "Determine whether the current buffer is a temporary buffer.

Buffers whose names start with a space are considered temporary
buffers."
  (s-starts-with? " " (buffer-name)))

(defun flycheck-safe-delete-files (files)
  "Safely delete FILES."
  (--each files (ignore-errors (delete-file it))))

(defun flycheck-safe-delete-directories (directories)
  "Safely delete DIRECTORIES."
  (--each directories (ignore-errors (delete-directory it :recursive))))

(defun flycheck-safe-delete-temporaries ()
  "Safely delete all temp files and directories of Flycheck.

Safely delete all files listed in `flycheck-temp-files' and all
directories in `flycheck-temp-directories', and set both
variables to nil."
  (flycheck-safe-delete-directories flycheck-temp-directories)
  (flycheck-safe-delete-files flycheck-temp-files)
  (setq flycheck-temp-directories nil
        flycheck-temp-files nil))

(eval-and-compile
  (defun flycheck-rx-message (form)
    "Translate the `(message)' FORM into a regular expression."
    (let ((body (or (cdr form) '((one-or-more not-newline)))))
      (rx-submatch-n `(group-n 4 ,@body))))

  (defun flycheck-rx-file-name (form)
    "Translate the `(file-name)' FORM into a regular expression."
    (let ((body (or (cdr form) '((minimal-match
                                  (one-or-more not-newline))))))
      (rx-submatch-n `(group-n 1 ,@body))))

  (defun flycheck-rx-to-string (form &optional no-group)
    "Like `rx-to-string' for FORM, but with special keywords:

`line'
     matches the line number.

`column'
     matches the column number.

`(file-name SEXP ...)'
     matches the file name.  SEXP constitutes the body of the message.  If no
     SEXP is given, use a default body  of `(minimal-match
     (one-or-more not-newline))'.

`(message SEXP ...)'
     matches the message. SEXP constitutes the body of the message.  If no SEXP
     is given, use a default body of `(one-or-more not-newline)'.

NO-GROUP is passed to `rx-to-string'."
    (let ((rx-constituents
           (append
            `((line . ,(rx (group-n 2 (one-or-more digit))))
              (column . ,(rx (group-n 3 (one-or-more digit))))
              (file-name flycheck-rx-file-name 0 nil)
              (message flycheck-rx-message 0 nil))
            rx-constituents nil)))
      (rx-to-string form no-group))))

(defun flycheck-current-load-file ()
  "Get the source file currently being loaded.

Always return the name of the corresponding source file, never
any byte-compiled file.

Return nil, if the currently loaded file cannot be determined."
  ;; If we're loading from a file, just use the file being loaded.  Otherwise
  ;; use the current buffer file name, in case of `eval-buffer' and the like.
  (-when-let (filename (if load-in-progress load-file-name (buffer-file-name)))
    (if (s-ends-with? ".elc" filename)
        (s-chop-suffix "c" filename)
      filename)))


;;;; Minibuffer tools
(defvar read-flycheck-checker-history nil
  "History of `read-flycheck-checker'.")

(defun read-flycheck-checker (prompt)
  "Read a flycheck checker from minibuffer with PROMPT.

Return the checker as symbol, or nil if no checker was
chosen."
  (let* ((input (completing-read prompt obarray
                                 #'flycheck-valid-checker-p t
                                 nil 'read-flycheck-checker-history)))
    (if (string= input "") nil (intern input))))


;;;; Obsolete checker declarations
;;;###autoload
(defmacro flycheck-declare-checker (symbol docstring &rest properties)
  "Declare SYMBOL as syntax checker with DOCSTRING and PROPERTIES.

DOCSTRING provides documentation for the new checker.  Use
`flycheck-checker-documentation' to access the documentation
string of a checker, and `flycheck-describe-checker' to get help
about a checker.

The following PROPERTIES are understood:

`:command'
     A list with the executable (in `car') and the arguments (in
     `cdr') of the syntax checker.  The executable is checked for
     existence with `executable-find' before executing the
     checker.  The arguments are substituted with
     `flycheck-substitute-argument' before execution, see the
     documentation of this function for a list of special tags
     allowed in arguments.

`:error-patterns'
     A list of error patterns to parse the output of the checker.
     Each pattern is a list (REGEXP LEVEL).  REGEXP is a regular
     expression that matches an error.  This regular expression
     may contain match groups extracting specific information
     about the error.  The 1st group is the file name, the 2nd
     group the line number, the 3rd group the column number and
     the 4th group the error message.  A group is ignored if it
     did not match or the match returned an empty string.  LEVEL
     is either warning or error and determines the severity of
     the error message parsed with the pattern.

`:error-parser'
     A function symbol to parse errors with.  The function must
     accept three arguments OUTPUT CHECKER BUFFER, where OUTPUT
     is the output as string and CHECKER the checker symbol that
     was used to check BUFFER.  The function must return a list
     of `flycheck-error' objects parsed from OUTPUT.

`:modes'
     A major mode symbol or a list thereof.  If present the
     checker is only used in these modes.

`:predicate'
     A function symbol or lambda expression.  If present the
     syntax checker is only used if this function returns non-nil
     when called in the buffer to check.

     May also be a form, but this usage is obsolete.

`:next-checkers'
     A list where each element is either a checker
     symbol to run after this checker or a cons cell (PREDICATE
     . CHECKER).  In the latter case, CHECKER is the checker symbol to
     run, and the PREDICATE symbol specifies when to run the checker:
     If PREDICATE is `no-errors' run the next checker only if this
     checker returned no errors at all.  If PREDICATE is
     `warnings-only', run the next checker only if this checker
     returned only warnings.  Only the first usable and
     registered (see `flycheck-registered-checker-p') is run.

A checker must have a `:command' property, at least one of
`:error-patterns' or `:error-parser', and at least one of
`:predicate' and `:modes'.  If `:predicate' and `:modes' are
present, both must match for the checker to be used."
  (declare (indent 1)
           (doc-string 2))
  `(flycheck-set-checker-properties ',symbol
     :command ,(plist-get properties :command)
     :error-patterns ,(plist-get properties :error-patterns)
     :error-parser (or ,(plist-get properties :error-parser)
                       'flycheck-parse-with-patterns)
     :modes ,(plist-get properties :modes)
     :predicate ,(plist-get properties :predicate)
     :next-checkers ,(plist-get properties :next-checkers)
     :documentation ,docstring))

(make-obsolete 'flycheck-declare-checker 'flycheck-define-checker
               "0.14")

(defun flycheck-clear-checker-properties (symbol)
  "Remove all Flycheck checker properties from SYMBOL."
  (--each '(checker command error-parser error-patterns modes predicate
                    next-checkers documentation file)
    (put symbol (intern (concat ":flycheck-" (symbol-name it))) nil)))

(defun flycheck-set-checker-properties (symbol &rest properties)
  "Set Flycheck checker PROPERTIES on SYMBOL."
  (declare (indent 1))
  (flycheck-clear-checker-properties symbol) ; Clear old properties first
  (--each '(command error-parser modes next-checkers documentation)
    (let ((name (symbol-name it)))
      (put symbol (intern (concat ":flycheck-" name))
           (plist-get properties (intern (concat ":" name))))))
  (put symbol :flycheck-error-patterns
       (--map (cons (car it) (cadr it)) (plist-get properties :error-patterns)))
  (-when-let (predicate (plist-get properties :predicate))
    (unless (functionp predicate)
      (setq predicate #'(lambda () (eval predicate))))
    (put symbol :flycheck-predicate predicate))
  (put symbol :flycheck-file (flycheck-current-load-file))
  (flycheck-verify-checker symbol)
  (put symbol :flycheck-checker t))

(defun flycheck-error-pattern-p (pattern)
  "Check whether PATTERN is a valid error pattern."
  (and
   (consp pattern)
   (stringp (car pattern))
   (memq (cdr pattern) '(warning error))))

(defun flycheck-error-patterns-list-p (patterns)
  "Check whether PATTERNS is a list of valid error patterns."
  (-all? 'flycheck-error-pattern-p patterns))

(defun flycheck-verify-checker (checker)
  "Verify CHECKER.

Ensure that all required properties are present, and signal an
error if not."
  (let ((command (get checker :flycheck-command))
        (patterns (get checker :flycheck-error-patterns))
        (parser (get checker :flycheck-error-parser))
        (modes (get checker :flycheck-modes))
        (predicate (get checker :flycheck-predicate))
        (next-checkers (get checker :flycheck-next-checkers))
        (doc (get checker :flycheck-documentation)))
    (unless (and doc (stringp doc))
      (error "Checker %s must have documentation" checker))
    (unless command
      (error "Checker %s must have a :command" checker))
    (unless (stringp (car command))
      (error "Checker %s must have an executable in :command" checker))
    (unless (flycheck-command-arguments-list-p command)
      (error "Checker %s has invalid :command arguments" checker))
    (when (and (eq parser 'flycheck-parse-with-patterns) (null patterns))
      (error "Checker %s must have :error-patterns" checker))
    (unless (or (null patterns) (flycheck-error-patterns-list-p patterns))
      (error "Checker %s has invalid :error-patterns" checker))
    (unless (or (null parser) (fboundp parser))
      (error "Function definition of error parser %s of checker %s is void"
             parser checker))
    (unless (or modes predicate)
      (error "Checker %s must have :modes or :predicate" checker))
    (unless (or (symbolp modes) (-all? #'symbolp modes))
      (error "Modes of checker %s must be a major mode or a list thereof"
             checker))
    (unless (or (null predicate) (functionp predicate))
      (error "Predicate of checker %s must be a function" checker))
    (unless (or
             (null next-checkers)
             (and (listp next-checkers)
                  (--all? (or (symbolp it)
                              (and (listp it)
                                   (memq (car it) '(no-errors warnings-only))
                                   (symbolp (cdr it))))
                          next-checkers)))
      (error "Checker %s has invalid next checkers" checker))))


;;;; Checker definitions
(eval-and-compile
  (defun flycheck-command-argument-p (arg)
    "Check whether ARG is a valid command argument."
    (pcase arg
      ((pred stringp) t)
      ((or `source `source-inplace `source-original `temporary-directory) t)
      (`(config-file ,option-name ,config-file-var)
       (and (stringp option-name)
            (symbolp config-file-var)))
      (`(,(or `option `option-list) ,option-name ,option-var)
       (and (stringp option-name)
            (symbolp option-var)))
      (`(,(or `option `option-list) ,option-name ,option-var ,prepender-or-filter)
       (and (stringp option-name)
            (-all? #'symbolp (list option-var prepender-or-filter))))
      (`(option-list ,option-name ,option-var ,prepender ,filter)
       (and (stringp option-name)
            (-all? #'symbolp (list option-var prepender filter))))
      (`(eval ,_) t)
      (_ nil)))

  (defun flycheck-command-arguments-list-p (arguments)
    "Check whether ARGUMENTS is a list of valid arguments."
    (-all? 'flycheck-command-argument-p arguments)))

(defmacro flycheck-define-checker (symbol doc-string &rest properties)
  "Define SYMBOL as syntax checker with DOC-STRING and PROPERTIES.

DOCSTRING provides documentation for the new syntax checker, as
shown with `flycheck-describe-checker'.

The following PROPERTIES constitute a syntax checker:

`:command (COMMAND ARG ...)'
     An unquoted list describing the syntax checker command to
     execute.  `COMMAND' and `ARG' are subject to substitution of
     special symbols and forms by `flycheck-substitute-argument',
     which provide the file to check, options and configuration
     file paths.  The `COMMAND' is checked with `executable-find'
     before execution.

`:error-patterns ((LEVEL SEXP ...) ...)'
     An unquoted list of error patterns to parse the output of
     the syntax checker `:command'.  LEVEL is either `error' or
     `warning' and denotes the severity of errors matched by the
     pattern.  The LEVEL is followed by one or more RX `SEXP's
     which parse the error and extract line, column, file name
     and error message.  See `rx' for general information about
     RX, and `flycheck-rx-to-string' for special RX forms
     provided by Flycheck.

`:error-parser FUNCTION'
`:error-parser (lambda (output checker buffer) BODY ...)'
     A function to parse errors with, either as unquoted symbol,
     or `lambda' form.  The function must accept three arguments
     OUTPUT CHECKER BUFFER, where OUTPUT is the syntax checker
     output as string, CHECKER the syntax checker that was used,
     and BUFFER a buffer object representing the checked buffer.
     The function must return a list of `flycheck-error' objects
     parsed from OUTPUT.  See Info node `(flycheck)Error API' for
     more information about `flycheck-error', and Info
     node `(flycheck)Error parsers' for a list of existing
     parsers.

`:modes MODE'
`:modes (MODE ...)'
     An unquoted major mode symbol or a list thereof.  If
     present, the syntax checker is only used if the major mode
     of the buffer to check is equal (as in `eq') to any given
     MODE.

`:predicate FUNCTION'
`:predicate (lambda () BODY ...)'
     A function to determine whether to use the syntax checker in
     the current buffer, either as unquoted function symbol or
     `lambda' form.  The syntax checker is only used if this
     function returns non-nil when called in the buffer to check.
     If `:modes' is given, the function is only called in
     matching modes.

`:next-checkers (ITEM ...)'
     An unquoted list defining the syntax checker to run after
     this syntax checker.  Each ITEM is either a syntax checker
     symbol, or a cons cell `(PREDICATE . CHECKER)'.  In the
     former case, the syntax checker is always considered.  In
     the later case, CHECKER is only considered if the PREDICATE
     matches.  PREDICATE is either `no-errors' or
     `warnings-only'.  In the former case, CHECKER is only
     considered if this checker reported no errors or warnings at
     all, in the latter case, CHECKER is only considered if this
     checker reported only warnings, but no errors.  The first
     registered and available syntax checker with matching
     predicate is executed after this checker.

A syntax checker must have a `:command', and at least one of
`:error-patterns' or `:error-parser', and at least one of
`:modes' and `:predicate'.  If `:predicate' and `:modes' are
given, both must match for the syntax checker to be used.
`:next-checkers' is entirely optional.

Signal a (compile-time) error if any property has an invalid
value."
  (declare (indent 1)
           (doc-string 2))
  (let ((command (plist-get properties :command))
        (parser (plist-get properties :error-parser))
        (patterns (plist-get properties :error-patterns))
        (modes (plist-get properties :modes))
        (predicate (plist-get properties :predicate))
        (next-checkers (plist-get properties :next-checkers)))
    (when (null command)
      (error "Missing :command"))
    (unless (flycheck-command-arguments-list-p command)
      (error "Invalid command %S " command))
    (when (and (null parser) (null patterns))
      (error "Missing :error-pattern or :error-parser"))
    (unless (or (null parser) (functionp parser))
      (error "%S is not a function" parser))
    (unless (--all? (memq (car it) '(warning error)) patterns)
      (error "Patterns %S have invalid levels" patterns))
    (unless (or modes predicate)
      (error "Missing :modes or :predicate"))
    (unless (or (symbolp modes) (-all? #'symbolp modes))
      (error "Invalid :modes %s, must be a symbol or a list thereof" modes))
    (unless (or (null predicate) (functionp predicate))
      (error "%S is not a function" predicate))
    (unless (or
             (null next-checkers)
             (and (listp next-checkers)
                  (--all? (or (symbolp it)
                              (and (listp it)
                                   (memq (car it) '(no-errors warnings-only))
                                   (symbolp (cdr it))))
                          next-checkers)))
      (error "Invalid next checkers %S" next-checkers))
    `(progn
       (put ',symbol :flycheck-documentation ,doc-string)
       (put ',symbol :flycheck-command ',command)
       (put ',symbol :flycheck-error-parser
            #',(or parser 'flycheck-parse-with-patterns))
       (put ',symbol :flycheck-error-patterns
            ',(--map (cons (flycheck-rx-to-string `(and ,@(cdr it)) :no-group)
                           (car it))
                     (plist-get properties :error-patterns)))
       (put ',symbol :flycheck-modes
            ',(if (and modes (symbolp modes)) (list modes) modes))
       (put ',symbol :flycheck-predicate #',predicate)
       (put ',symbol :flycheck-next-checkers ',next-checkers)
       (put ',symbol :flycheck-file (flycheck-current-load-file))
       (put ',symbol :flycheck-checker t))))

;;;###autoload
(defmacro flycheck-def-config-file-var (symbol checker &optional file-name
                                               &rest custom-args)
  "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable (see `defcustom`)
providing a configuration file for CHECKER.  The CHECKER argument
is used for documentation purposes only.  If given use FILE-NAME
as initial value.

The variable is declared with `defcustom', and declared
buffer-local.  CUSTOM-ARGS are forwarded to `defcustom'

Use this together with the `config-file' cell in syntax checker
commands."
  (declare (indent 3))
  `(progn
     (put ',checker :flycheck-config-file-var ',symbol)
     (defcustom ,symbol ,file-name
       ,(format "Configuration file for `%s'.

Locate the configuration file using the functions from
`flycheck-locate-config-file-functions'.  If the file is found
pass it to the syntax checker as configuration file.  Otherwise
invoke the syntax checker without a configuration file.

Use this variable as file-local variable if you need a specific
configuration file a buffer." checker)
       :type '(choice (const :tag "No configuration file" nil)
                      (string :tag "File name or path"))
       :group 'flycheck-config-files
       ,@custom-args)
     (make-variable-buffer-local ',symbol)))

;;;###autoload
(defmacro flycheck-def-option-var (symbol init-value checker docstring
                                          &rest custom-args)
  "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

INIT-VALUE is the initial value for the new variable.  DOCSTRING
is its docstring.

The variable is declared with `defcustom', and declared
buffer-local.  CUSTOM-ARGS are forwarded to `defcustom'.

Use this together with the `option' cell in syntax checker
commands."
  (declare (indent 3)
           (doc-string 4))
  `(progn
     (let ((options (flycheck-checker-option-vars ',checker)))
       (put ',checker :flycheck-option-vars (-uniq (cons ',symbol options))))
     (defcustom ,symbol ,init-value
       ,(format "%s

This variable is an option for the syntax checker `%s'." docstring checker)
       :group 'flycheck-options
       ,@custom-args)
     (make-variable-buffer-local ',symbol)))


;;;; Checker API
(defun flycheck-defined-checkers ()
  "Find all defined syntax checkers.

The returned list is sorted alphapetically by the symbol name of
the syntax checkers."
  (let (defined-checkers)
    (mapatoms (lambda (symbol)
                (when (flycheck-valid-checker-p symbol)
                  (push symbol defined-checkers))))
    (sort defined-checkers #'string<)))

(defun flycheck-registered-checker-p (checker)
  "Determine whether CHECKER is registered.

A checker is registered if it is contained in `flycheck-checkers'."
  (memq checker flycheck-checkers))

(defun flycheck-valid-checker-p (checker)
  "Check whether a CHECKER is valid.

A valid checker is a symbol define as syntax checker with
`flycheck-define-checker'."
  (get checker :flycheck-checker))

(defun flycheck-checker-modes (checker)
  "Get the modes of CHECKER."
  (let ((modes (get checker :flycheck-modes)))
    (if (and modes (symbolp modes))
        (list modes)
      modes)))

(defun flycheck-checker-predicate (checker)
  "Get the predicate function of CHECKER."
  (get checker :flycheck-predicate))

(defun flycheck-checker-next-checkers (checker)
  "Get the next checkers for CHECKER."
  (get checker :flycheck-next-checkers))

(defun flycheck-checker-command (checker)
  "Get the raw command of CHECKER.

The command list returned by this function is not substituted,
and hence still contains special tags and symbols.  Use
`flycheck-checker-substituted-command' to get an executable
command list with no special tags and symbols."
  (get checker :flycheck-command))

(defun flycheck-checker-error-patterns (checker)
  "Get the error patterns of CHECKER.

Each pattern has the form `(REGEXP . LEVEL)' where REGEXP is the
regular expression, and LEVEL the corresponding level symbol."
  (get checker :flycheck-error-patterns))

(defun flycheck-checker-error-parser (checker)
  "Get the error parser of CHECKER."
  (get checker :flycheck-error-parser))

(defun flycheck-checker-pattern-to-error-regexp (pattern)
  "Convert PATTERN into an error regexp for compile.el.

Return a list representing PATTERN, suitable as element in
`compilation-error-regexp-alist'."
  (let* ((regexp (car pattern))
         (level (cdr pattern))
         (level-no (pcase level
                     (`error 2)
                     (`warning 1))))
    (list regexp 1 2 3 level-no)))

(defun flycheck-checker-compilation-error-regexp-alist (checker)
  "Convert error patterns of CHECKER for use with compile.el.

Return an alist of all error patterns of CHECKER, suitable for
use with `compilation-error-regexp-alist'."
  (-map #'flycheck-checker-pattern-to-error-regexp
        (flycheck-checker-error-patterns checker)))

(defun flycheck-checker-documentation (checker)
  "Get the documentation of CHECKER."
  (documentation-property checker :flycheck-documentation))

(defun flycheck-checker-file (checker)
  "Get the file CHECKER was defined in.

Return nil if the file cannot be determined."
  (get checker :flycheck-file))

(defun flycheck-checker-config-file-var (checker)
  "Get the associated configuration file variable of CHECKER.

Return nil if CHECKER has no associated configuration file
variable."
  (get checker :flycheck-config-file-var))

(defun flycheck-checker-option-vars (checker)
  "Get the associated option variables of CHECKER.

Return a (possibly empty) list of variable symbols."
  (get checker :flycheck-option-vars))

(defun flycheck-locate-config-file (filename checker)
  "Locate the configuration file FILENAME for CHECKER.

Locate the configuration file using
`flycheck-locate-config-file-functions'.

Return the absolute path of the configuration file, or nil if no
configuration file was found."
  (-when-let (filepath (run-hook-with-args-until-success
                        'flycheck-locate-config-file-functions
                        filename checker))
    (when (file-exists-p filepath)
      filepath)))

(defun flycheck-substitute-argument (arg checker)
  "Substitute ARG for CHECKER.

ARG may be one of the following forms:

STRING
     Return ARG unchanged.

`source', `source-inplace'
     Create a temporary file to check and return its path.  With
     `source-inplace' create the temporary file in the same
     directory as the original file.  With `source', try to
     retain the non-directory component of the buffer's file name
     in the temporary file.

`source-original'
     Return the path of the actual file to check, or an empty
     string if the buffer has no file name.  Note that the
     contents of the file may not be up to date with the contents
     of the buffer to check.  Do not use this as primary input to
     a checker!

`temporary-directory'
     Create a unique temporary directory and return its path.

`(config-file OPTION VARIABLE)'
     Search the configuration file bound to VARIABLE with
     `flycheck-find-config-file' and return a list of arguments
     that pass this configuration file to the syntax checker, or
     nil if the configuration file was not found.

     If OPTION ends with a = character, the returned list
     contains a single element only, being the concatenation of
     OPTION and the path of the configuration file.  Otherwise
     the list has two items, the first being OPTION, the second
     the path of the configuration file.

`(option OPTION VARIABLE [FILTER])'
     Retrieve the value of VARIABLE and return a list of
     arguments that pass this value as value for OPTION to the
     syntax checker.

     FILTER is an optional function to be applied to the value of
     VARIABLE.  This function must return nil or a string.  In
     the former case, return nil.  In the latter case, return a
     list of arguments as described above.  If OPTION ends with a
     =, process it like in a `config-file' cell (see above).

`(option-list OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE, which must be a list,
     and prepend OPTION before each item in this list, using
     PREPEND-FN.

     PREPEND-FN is called with the OPTION and each item of the
     list as second argument, and should return OPTION prepended
     before the item, either as string or as list.

     FILTER is an optional function to be applied to each item in
     the list.  Items for which FILTER returns nil are dropped.
     If the list is non-nil after the application of FILTER,
     return a list `(OPTION ITEM1 OPTION ITEM2 ...)'.  Otherwise
     return nil.

`(eval FORM)
     Return the result of evaluating FORM in the buffer to be
     checked.  FORM must either return a string or a list of
     strings, or nil to indicate that nothing should be
     substituted for CELL.  For all other return types, signal an
     error

     _No_ further substitutions are performed, neither in FORM
     before it is evaluated, nor in the result of evaluating
     FORM.

In all other cases, signal an error.

Note that substitution is *not* recursive.  No symbols or cells
are substituted within the body of cells!"
  (pcase arg
    ((pred stringp) arg)
    (`source
     (flycheck-save-buffer-to-temp #'flycheck-temp-file-system "flycheck"))
    (`source-inplace
     (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace "flycheck"))
    (`source-original (or (buffer-file-name) ""))
    (`temporary-directory (flycheck-temp-dir-system "flycheck"))
    (`(config-file ,option-name ,file-name-var)
     (-when-let* ((value (symbol-value file-name-var))
                  (file-name (flycheck-locate-config-file value checker)))
       (flycheck-option-with-value-argument option-name file-name)))
    (`(option ,option-name ,variable)
     (-when-let (value (symbol-value variable))
       (unless (stringp value)
         (error "Value %S of %S for option %s is not a string"
                value variable option-name))
       (flycheck-option-with-value-argument option-name value)))
    (`(option ,option-name ,variable ,filter)
     (-when-let (value (funcall filter (symbol-value variable)))
       (unless (stringp value)
         (error "Value %S of %S (filter: %S) for option %s is not a string"
                value variable filter option-name))
       (flycheck-option-with-value-argument option-name value)))
    (`(option-list ,option-name ,variable)
     (-when-let (value (symbol-value variable))
       (unless (and (listp value) (-all? #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value)))
    (`(option-list ,option-name ,variable ,prepend-fn)
     (-when-let (value (symbol-value variable))
       (unless (and (listp value) (-all? #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(option-list ,option-name ,variable ,prepend-fn ,filter)
     (-when-let (value (-keep filter (symbol-value variable)))
       (unless (and (listp value) (-all? #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(eval ,form)
     (let ((result (eval form)))
       (if (or (null result)
               (stringp result)
               (and (listp result) (-all? #'stringp result)))
           result
         (error "Invalid result from evaluation of %S: %S" form result))))
    (_ (error "Unsupported argument %S" arg))))

(defun flycheck-checker-substituted-command (checker)
  "Get the substituted command of a CHECKER.

Substitute each argument in the command of CHECKER using
`flycheck-substitute-argument'.  This replaces any special
symbols in the command."
  (-flatten (--keep (flycheck-substitute-argument it checker)
                    (flycheck-checker-command checker))))

(defun flycheck-substitute-shell-argument (arg checker)
  "Substitute ARG for CHECKER.

Like `flycheck-substitute-argument', but return a single string
suitable for use as argument to a shell command, and do not
substitute with temporary files.  `source' and `source-inplace'
are substituted with the buffer file name."
  (if (memq arg '(source source-inplace source-original))
      (shell-quote-argument (buffer-file-name))
    (let ((args (flycheck-substitute-argument arg checker)))
      (if (stringp args)
          (shell-quote-argument args)
        (s-join " " (-map #'shell-quote-argument args))))))

(defun flycheck-checker-shell-command (checker)
  "Get a shell command for CHECKER.

Substitutions are performed like in
`flycheck-checker-substituted-command', but with
`flycheck-substitute-shell-argument'.

Return the command of CHECKER as single string, suitable for
shell execution."
  (s-join " " (--keep (flycheck-substitute-shell-argument it checker)
                      (flycheck-checker-command checker))))

(defun flycheck-checker-executable (checker)
  "Get the executable of CHECKER.

The executable is the `car' of the checker command as returned by
`flycheck-checker-command'."
  (let* ((command (flycheck-checker-command checker))
         (executable (flycheck-substitute-argument (car command) checker)))
    (if (listp executable) (car executable) executable)))

(defun flycheck-check-modes (checker)
  "Check the allowed modes of CHECKER.

Check the current `major-mode' against the modes allowed for
CHECKER.  Return t if the modes match or nil otherwise."
  (let ((modes (flycheck-checker-modes checker)))
    (or (not modes) (memq major-mode modes))))

(defun flycheck-check-predicate (checker)
  "Check the predicate of CHECKER.

Check the predicate of CHECKER.  Return t if CHECKER has no
predicate function, otherwise return the result of calling the
predicate function."
  (let ((predicate (flycheck-checker-predicate checker)))
    (or (not predicate) (funcall predicate))))

(defun flycheck-check-executable (checker)
  "Check the executable of the CHECKER."
  (when (executable-find (flycheck-checker-executable checker)) t))

(defun flycheck-may-use-checker (checker)
  "Determine whether a CHECKER may be used.

Return t if CHECKER may be used for the current buffer and nil
otherwise."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is no defined flycheck syntax checker (see `flycheck-define-checker')"
           checker))
  (and (flycheck-check-modes checker)
       (flycheck-check-predicate checker)
       (flycheck-check-executable checker)))

(defun flycheck-may-use-next-checker (next-checker)
  "Determine whether NEXT-CHECKER may be used."
  (when (symbolp next-checker)
    (setq next-checker (cons t next-checker)))
  (let ((predicate (car next-checker))
        (next-checker (cdr next-checker)))
    (and (or (eq predicate t)
             (and (eq predicate 'no-errors)
                  (not (flycheck-has-current-errors-p)))
             (and (eq predicate 'warnings-only)
                  (not (flycheck-has-current-errors-p 'error))))
         (flycheck-registered-checker-p next-checker)
         (flycheck-may-use-checker next-checker))))


;;;; Configuration file functions
(defun flycheck-locate-config-file-absolute-path (filepath _checker)
  "Locate a configuration file by a FILEPATH.

If FILEPATH is a contains a path separator, expand it against the
default directory and return it.  Otherwise return nil.

_CHECKER is ignored."
  (when (file-name-directory filepath)
    (expand-file-name filepath)))

(defun flycheck-locate-config-file-projectile (filename _checker)
  "Locate a configuration FILENAME in a projectile project.

If the Projectile library (see URL
`https://github.com/bbatsov/projectile') is available and the
current buffer is within a Projectile project, search FILENAME in
the root directory of the project.  If the file is found, return
its absolute path.

Otherwise return nil.

_CHECKER is ignored."
  (when (fboundp 'projectile-project-root)
    (condition-case nil
        (let* ((root-directory (projectile-project-root))
               (filepath (expand-file-name filename root-directory)))
          (when (file-exists-p filepath)
            filepath))
      (error nil))))

(defun flycheck-locate-config-file-ancestor-directories (filename _checker)
  "Locate a configuration FILENAME in ancestor directories.

If the current buffer has a file name, search FILENAME in the
directory of the current buffer and all ancestors thereof (see
`locate-dominating-file').  If the file is found, return its
absolute path.  Otherwise return nil.

_CHECKER is ignored."
  (-when-let* ((basefile (buffer-file-name))
               (directory (locate-dominating-file basefile filename)))
    (expand-file-name filename directory)))

(defun flycheck-locate-config-file-home (filename _checker)
  "Locate a configuration FILENAME in the home directory.

Return the absolute path, if FILENAME exists in the user's home
directory, or nil otherwise."
  (let ((path (expand-file-name filename "~")))
    (when (file-exists-p path)
      path)))

(--each '(flycheck-locate-config-file-absolute-path
          flycheck-locate-config-file-projectile
          flycheck-locate-config-file-ancestor-directories
          flycheck-locate-config-file-home)
  (custom-add-frequent-value 'flycheck-locate-config-file-functions it))


;;;; Generic option filters
(defun flycheck-option-int (value)
  "Convert an integral option VALUE to a string.

If VALUE is nil, return nil.  Otherwise return VALUE converted to
a string."
  (when value
    (number-to-string value)))

(defun flycheck-option-comma-separated-list (value &optional separator filter)
  "Convert VALUE into a list separated by SEPARATOR.

SEPARATOR is a string to separate items in VALUE, defaulting to
\",\".  FILTER is an optional function, which takes a single
argument and returns either a string or nil.

If VALUE is a list, apply FILTER to each item in VALUE, remove
all nil items, and return a single string of all remaining items
separated by SEPARATOR.

Otherwise, apply FILTER to VALUE and return the result.  FILTER is ignored."
  (let ((filter (or filter #'identity))
        (separator (or separator ",")))
    (if (listp value)
        (-when-let (value (-keep filter value))
          (s-join separator value))
      (funcall filter value))))


;;;; Checker selection
(defvar-local flycheck-last-checker nil
  "The last checker used for the current buffer.")

(defun flycheck-clear-checker ()
  "Clear configured and remembered checkers in the current buffer."
  (setq flycheck-last-checker nil))

(defun flycheck-try-last-checker-for-buffer ()
  "Try the last checker for the current buffer.

Return the checker if it may be used, or nil otherwise."
  ;; We should not use the last checker if it was removed from the list of
  ;; allowed checkers in the meantime
  (when (and (flycheck-registered-checker-p flycheck-last-checker)
             (flycheck-may-use-checker flycheck-last-checker))
    flycheck-last-checker))

(defun flycheck-get-new-checker-for-buffer ()
  "Find a new checker for the current buffer.

If a checker is found set `flycheck-last-checker' to re-use this
checker for the next check.

Return the checker if there is any, or nil otherwise."
  (-when-let (checker (-first #'flycheck-may-use-checker flycheck-checkers))
    (setq flycheck-last-checker checker)))

(defun flycheck-get-checker-for-buffer ()
  "Find the checker for the current buffer.

Return checker if there is a checker for the current buffer, or
nil otherwise."
  (if flycheck-checker
      (if (flycheck-may-use-checker flycheck-checker)
          flycheck-checker
        (user-error "Configured syntax checker %s cannot be used"
                    flycheck-checker))
    (or (flycheck-try-last-checker-for-buffer)
        (flycheck-get-new-checker-for-buffer))))

(defun flycheck-get-next-checker-for-buffer (checker)
  "Get the checker to run after CHECKER for the current buffer."
  (-when-let (next-checkers (flycheck-checker-next-checkers checker))
    (let ((next-checker (-first #'flycheck-may-use-next-checker next-checkers)))
      (if (symbolp next-checker)
          next-checker
        (cdr next-checker)))))

(defun flycheck-select-checker (checker)
  "Select CHECKER for the current buffer.

CHECKER is a syntax checker symbol (see `flycheck-checkers') or
nil.  It does not need to be registered in `flycheck-checkers'.
If nil deselect the current syntax checker (if any) and use
automatic checker selection via `flycheck-checkers'.

If called interactively prompt for CHECKER.  If no syntax checker
is entered deselect the current syntax checker.  With prefix arg
immediately deselect the current syntax checker without any
prompt.

Set `flycheck-checker' to CHECKER and automatically start a new
syntax check if the syntax checker changed."
  (interactive
   (if current-prefix-arg
       (list nil)
     (list (read-flycheck-checker "Select checker: "))))
  (when (not (eq checker flycheck-checker))
    (setq flycheck-checker checker)
    (when flycheck-mode
      (flycheck-buffer))))


;;;; Documentation
;;;###autoload
(defun flycheck-info ()
  "Open the Flycheck manual."
  (interactive)
  (info "flycheck"))

(define-button-type 'help-flycheck-checker-def
  :supertype 'help-xref
  'help-function 'flycheck-goto-checker-definition
  'help-echo (purecopy "mouse-2, RET: find Flycheck checker definition"))

(defconst flycheck-find-checker-regexp
  ;; We use `eval-and-compile' and `rx-to-string' here instead of simply `rx',
  ;; because we need to dynamically add the regexp from `find-function-space-re'
  (eval-when-compile
    (rx-to-string
     `(and line-start (zero-or-more (syntax whitespace))
           "("
           symbol-start
           (or "flycheck-declare-checker" "flycheck-define-checker")
           symbol-end
           (regexp ,find-function-space-re)
           symbol-start
           "%s"
           symbol-end
           (or (syntax whitespace) line-end))))
  "Regular expression to find a checker definition.")

(add-to-list 'find-function-regexp-alist
             '(flycheck-checker . flycheck-find-checker-regexp))

(defun flycheck-goto-checker-definition (checker file)
  "Go to to the definition of CHECKER in FILE."
  (let ((location (find-function-search-for-symbol
                   checker 'flycheck-checker file)))
    (pop-to-buffer (car location))
    (if (cdr location)
        (goto-char (cdr location))
      (message "Unable to find checker location in file"))))

(defun flycheck-checker-at-point ()
  "Return the Flycheck checker found at or before point.

Return 0 if there is no checker."
  (let ((symbol (variable-at-point :any-symbol)))
    (if (and (symbolp symbol) (flycheck-valid-checker-p symbol))
        symbol
      0)))

(defun flycheck-describe-checker (checker)
  "Display the documentation of CHECKER.

CHECKER is a checker symbol.

Pop up a help buffer with the documentation of CHECKER."
  (interactive
   (let* ((checker (flycheck-checker-at-point))
          (enable-recursive-minibuffers t)
          (prompt (if (symbolp checker)
                      (format "Describe syntax checker (default %s): " checker)
                    "Describe syntax checker: "))
          (reply (read-flycheck-checker prompt)))
     (list (or reply checker))))
  (if (or (null checker) (not (flycheck-valid-checker-p checker)))
      (message "You didn't specify a Flycheck syntax checker.")
    (help-setup-xref (list #'flycheck-describe-checker checker)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (let ((executable (flycheck-checker-executable checker))
              (filename (flycheck-checker-file checker))
              (modes (flycheck-checker-modes checker))
              (config-file-var (flycheck-checker-config-file-var checker))
              (option-vars (-sort #'string<
                                  (flycheck-checker-option-vars checker))))
          (princ (format "%s is a Flycheck syntax checker" checker))
          (when filename
            (princ (format " in `%s'" (file-name-nondirectory filename)))
            (with-current-buffer standard-output
              (save-excursion
                (re-search-backward "`\\([^`']+\\)'" nil t)
                (help-xref-button 1 'help-flycheck-checker-def checker filename))))
          (princ ".\n\n")
          (princ (format "  This syntax checker executes \"%s\"" executable))
          (if config-file-var
              (princ (format ", using a configuration file from `%s'.\n"
                             config-file-var))
            (princ ".\n"))
          (if modes
           (princ (format "  It checks syntax in the major mode(s) %s. "
                          (s-join ", " (--map (format "`%s'" it) modes)))))
          (with-current-buffer (help-buffer)
            (save-excursion
              (goto-char (point-min))
              (forward-paragraph)
              (fill-region-as-paragraph (point) (point-max))))
          (princ "\n")
          (when option-vars
            (princ "\n  This syntax checker can be configured with these options:\n\n")
            (--each option-vars
              (princ (format "     * `%s'\n" it)))))
        (princ (format "\nDocumentation:\n%s"
                       (flycheck-checker-documentation checker)))))))


;;;; Checker error API
(cl-defstruct (flycheck-error
               (:constructor flycheck-error-new)
               (:constructor flycheck-error-new-at (line column
                                                         &optional level message
                                                         &key checker filename
                                                         (buffer (current-buffer)))))
  "Structure representing an error reported by a syntax checker.
Slots:

`buffer'
     The buffer the reported was reported for, as buffer object.

`checker'
     The syntax checker which reported this error, as symbol.

`filename'
     The file name the error refers to, as string.

`line'
     The line number the error refers to, as number.

`column'
     The column number the error refers to, as number.

`level'
     The error level, as either `warning' or `error'."
  buffer checker filename line column message level)

(defmacro flycheck-error-with-buffer (err &rest forms)
  "Switch to the buffer of ERR and evaluate FORMS.

If the buffer of ERR is not live, FORMS are not evaluated."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (flycheck-error-buffer ,err))
     (with-current-buffer (flycheck-error-buffer ,err)
       ,@forms)))

(defun flycheck-error-line-region (err)
  "Get the line region of ERR.

ERR is a Flycheck error whose region to get.

Return a cons cell `(BEG . END)' where BEG is the first
non-whitespace character on the line ERR refers to, and END the
end of the line."
  (flycheck-error-with-buffer err
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (- (flycheck-error-line err) 1))
        (back-to-indentation)
        (let ((beg (point))
              (end (line-end-position)))
          (when (= beg end)
            ;; The current line is empty, so include the previous line break
            ;; character(s) to have any region at all
            (forward-line -1)
            (setq beg (line-end-position)))
          (cons beg end))))))

(defun flycheck-error-column-region (err)
  "Get the error column region of ERR.

ERR is a Flycheck error whose region to get.

Return a cons cell `(BEG . END)' where BEG is the character
before the error column, and END the actual error column, or nil
if ERR has no column."
  (-when-let (column (flycheck-error-column err))
    (save-excursion
      (save-restriction
        (widen)
        (let ((line (flycheck-error-line err)))
          (goto-char (point-min))
          (forward-line (- line 1))
          (cond
           ((> line (line-number-at-pos))
            ;; If the line is beyond the end of the file, return the very last
            ;; column in the file
            (cons (- (point-max) 1) (point-max)))
           ((= (line-beginning-position) (line-end-position))
              ;; The line is empty, so there is no column to highlight on this
              ;; line.  Thus, return the last column of the previous line
            (let ((end (line-beginning-position)))
              (forward-line -1)
              (cons (line-end-position) end)))
           (:else
            ;; The end is either the column offset of the line, or
            ;; the end of the line, if the column offset points beyond the end
            ;; of the line.
            (let ((end (min (+ (line-beginning-position) column)
                           (+ (line-end-position) 1))))
              (cons (- end 1) end)))))))))

(defun flycheck-error-sexp-region (err)
  "Get the sexp region of ERR.

ERR is a Flycheck error whose region to get.

Return a cons cell `(BEG . END)' where BEG is the beginning of
the symbol at the error column, and END the end of the symbol.
If ERR has no error column, or if there is no symbol at this
column, return nil."
  (-when-let (column (car (flycheck-error-column-region err)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char column)
        (bounds-of-thing-at-point 'sexp)))))

(defun flycheck-error-region-for-mode (err mode)
  "Get the region of ERR for the highlighting MODE.

ERR is a Flycheck error.  MODE may be one of the following symbols:

`columns'
     Return the column region of ERR, or the line region if ERR
     has no column.

`sexps'
     Return the sexp region of ERR, or the result of `columns',
     if there is no symbol at the error column.

`lines'
     Return the line region.

Otherwise signal an error."
  (pcase mode
    (`columns (or (flycheck-error-column-region err)
                  (flycheck-error-line-region err)))
    (`sexps (or (flycheck-error-sexp-region err)
                (flycheck-error-region-for-mode err 'columns)))
    (`lines (flycheck-error-line-region err))
    (_ (error "Invalid mode %S" mode))))

(defun flycheck-error-pos (err)
  "Get the buffer position of ERR.

ERR is a Flycheck error whose position to get.

The error position is the error column, or the first
non-whitespace character of the error line, if ERR has no error column."
  (car (or (flycheck-error-column-region err)
           (flycheck-error-line-region err))))

(defun flycheck-error-format (err)
  "Format ERR as human-readable string.

Return a string that represents the given ERR.  This string does
_not_ include the file name."
  (let ((line (flycheck-error-line err))
        (column (flycheck-error-column err))
        (level (symbol-name (flycheck-error-level err)))
        (checker (symbol-name (flycheck-error-checker err)))
        (message (flycheck-error-message err)))
    (if column
        (s-lex-format "${line}:${column}:${level}: ${message} (${checker})")
      (s-lex-format "${line}:${level}: ${message} (${checker})"))))


;;;; General error parsing
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let* ((parser (flycheck-checker-error-parser checker))
         (errors (funcall parser output checker buffer)))
    (--each errors
      (setf (flycheck-error-buffer it) buffer)
      (setf (flycheck-error-checker it) checker))
    (-map #'flycheck-sanitize-error errors)))

(defun flycheck-fix-error-filename (err buffer-files)
  "Fix the file name of ERR from BUFFER-FILES.

If the file name of ERR is in BUFFER-FILES, replace it with the
return value of the function `buffer-file-name'."
  (flycheck-error-with-buffer err
    (-when-let (filename (flycheck-error-filename err))
      (when (--any? (flycheck-same-files-p filename it) buffer-files)
        (setf (flycheck-error-filename err) (buffer-file-name)))))
  err)

(defun flycheck-fix-error-filenames (errors buffer-files)
  "Fix the file names of all ERRORS from BUFFER-FILES.

See `flycheck-fix-error-filename' for details."
  (--map (flycheck-fix-error-filename it buffer-files) errors))

(defun flycheck-sanitize-error (err)
  "Sanitize ERR.

Make the error filename absolute, and clean up whitespace in the
error message."
  (flycheck-error-with-buffer err
    (let ((filename (flycheck-error-filename err))
          (message (flycheck-error-message err)))
      (when message
        (setf (flycheck-error-message err) (s-trim message)))
      (when filename
        (setf (flycheck-error-filename err) (expand-file-name filename)))))
  err)


;;;; Error parsing with regular expressions
(defun flycheck-match-string-non-empty (group match &optional trim-first)
  "Get a non-empty string from a GROUP in MATCH.

If the string returned by GROUP is empty, return nil instead.

If TRIM-FIRST is t trim leading and trailing white space in the matched
string."
  (let ((matched-string (nth group match)))
    (save-match-data
      (when matched-string
        (when trim-first
          (setq matched-string (s-trim matched-string)))
        (when (not (s-blank? matched-string))
          matched-string)))))

(defun flycheck-match-int (group match)
  "Get an integer from a GROUP in MATCH.

Return nil if the group did not match a number."
  (flycheck-string-to-number-safe
   (flycheck-match-string-non-empty group match t)))

(defun flycheck-get-regexp (patterns)
  "Create a single regular expression from PATTERNS."
  (rx-to-string `(or ,@(--map `(regexp ,(car it)) patterns)) :no-group))

(defun flycheck-tokenize-output-with-patterns (output patterns)
  "Tokenize OUTPUT with PATTERNS.

Split the output into error tokens, using all regular expressions
from the error PATTERNS.  An error token is simply a string
containing a single error from OUTPUT.  Such a token can then be
parsed into a structured error by applying the PATTERNS again,
see `flycheck-parse-errors-with-patterns'.

Return a list of error tokens."
  (let ((regexp (flycheck-get-regexp patterns))
        (errors nil)
        (last-match 0))
    (while (string-match regexp output last-match)
      (!cons (match-string 0 output) errors)
      (setq last-match (match-end 0)))
    errors))

(defun flycheck-try-parse-error-with-pattern (err pattern)
  "Try to parse a single ERR with a PATTERN.

Return the parsed error if PATTERN matched ERR, or nil
otherwise."
  (let* ((regexp (car pattern))
         (level (cdr pattern))
         (match (s-match regexp err)))
    (when match
      (flycheck-error-new
       :filename (flycheck-match-string-non-empty 1 match)
       :line (flycheck-match-int 2 match)
       :column (flycheck-match-int 3 match)
       :message (flycheck-match-string-non-empty 4 match t)
       :level level))))

(defun flycheck-parse-error-with-patterns (err patterns)
  "Parse a single ERR with error PATTERNS.

Apply each pattern in PATTERNS to ERR, in the given order, and
return the first parsed error."
  ;; Try to parse patterns in the order of declaration to make sure that the
  ;; first match wins.
  (car (--keep (flycheck-try-parse-error-with-pattern err it) patterns)))

(defun flycheck-parse-errors-with-patterns (errors patterns)
  "Parse ERRORS with PATTERNS.

ERRORS is a list of strings where each string is an unparsed
error message, typically from `flycheck-split-output'.  PATTERNS
is a list of error patterns to parse ERRORS with.

Return a list of parsed errors."
  (--map (flycheck-parse-error-with-patterns it patterns) errors))

(defun flycheck-parse-with-patterns (output checker _buffer)
  "Parse OUTPUT from CHECKER with error patterns.

Uses the error patterns of CHECKER to tokenize the output and
tries to parse each error token with all patterns, in the order
of declaration.  Hence an error is never matched twice by two
different patterns.  The pattern declared first always wins.

_BUFFER is ignored.

Return a list of parsed errors and warnings (as `flycheck-error'
objects)."
  (let ((patterns (flycheck-checker-error-patterns checker)))
    (-> output
      (flycheck-tokenize-output-with-patterns patterns)
      (flycheck-parse-errors-with-patterns patterns))))


;;;; Error parsers
(defun flycheck-parse-xml-region (beg end)
  "Parse the xml region between BEG and END.

Wrapper around `xml-parse-region' which transforms the return
value of this function into one compatible to
`libxml-parse-xml-region' by simply returning the first element
from the node list."
  (car (xml-parse-region beg end)))

(defvar flycheck-xml-parser
  (if (fboundp 'libxml-parse-xml-region)
      'libxml-parse-xml-region 'flycheck-parse-xml-region)
  "Parse an xml string from a region.

Use libxml if Emacs is built with libxml support.  Otherwise fall
back to `xml-parse-region'.")

(defun flycheck-parse-xml-string (xml)
  "Parse an XML string.

Return the document tree parsed from XML in the form (ROOT ATTRS
BODY...).  ROOT is a symbol identifying the name of the root
element.  ATTRS is an alist of the attributes of the root node.
BODY is zero or more body elements, either as strings (in case of
text nodes) or as XML nodes, in the same for as the root node."
  (with-temp-buffer
    (insert xml)
    (funcall flycheck-xml-parser (point-min) (point-max))))

(defun flycheck-parse-checkstyle-file-node (node)
  "Parse a single file NODE in a Checkstyle document.

Return a list of all errors contained in the NODE, or nil if NODE
is not a file node."
  (let ((filename (cdr (assq 'name (cadr node)))))
    (->> (cddr node)
      (--filter (and (listp it) (eq (car it) 'error)))
      (--map
       (let* ((attrs (cadr it))
              (line (flycheck-string-to-number-safe
                     (cdr (assq 'line attrs))))
              (column (flycheck-string-to-number-safe
                       (cdr (assq 'column attrs))))
              (severity (cdr (assq 'severity attrs)))
              (message (cdr (assq 'message attrs))))
         (flycheck-error-new
          :filename filename
          :line line
          :column (when (and column (> column 0)) column)
          :message message
          :level (if (string= severity "error") 'error 'warning)))))))

(eval-and-compile
  ;; Parser must be defined during compilation, to allow syntax checkers parse
  ;; the verification
  (defun flycheck-parse-checkstyle (output _checker _buffer)
    "Parse Checkstyle errors from OUTPUT.

Parse Checkstyle-like XML output.  Use this error parser for
checkers that have an option to output errors in this format.

_CHECKER and _BUFFER are ignored.

See URL `http://checkstyle.sourceforge.net/' for information
about Checkstyle."
    (-when-let (root (flycheck-parse-xml-string output))
      (unless (eq (car root) 'checkstyle)
        (error "Unexpected root element %s" (car root)))
      ;; cddr gets us the body of the node without its name and its attributes
      (->> (cddr root)
        (--filter (and (listp it) (eq (car it) 'file)))
        (-map #'flycheck-parse-checkstyle-file-node)
        -flatten))))

(defun flycheck-parse-cppcheck-error-node (node)
  "Parse a single error NODE from Cppcheck XML.

Return a list of all Flycheck errors this node represents."
  (let ((attrs (cadr node)))
    (->> (cddr node)
      (--filter (and (listp it) (eq (car it) 'location)))
      (--map (let ((locattrs (cadr it)))
               (flycheck-error-new
                :filename (cdr (assq 'file locattrs))
                :line (flycheck-string-to-number-safe
                       (cdr (assq 'line locattrs)))
                :message (cdr (assq 'verbose attrs))
                :level (if (string= (cdr (assq 'severity attrs)) "error")
                           'error 'warning)))))))

(eval-and-compile
  (defun flycheck-parse-cppcheck (output _checker _buffer)
    "Parse Cppcheck errors from OUTPUT.

Parse Cppcheck XML v2 output.

_BUFFER and _ERROR are ignored.

See URL `http://cppcheck.sourceforge.net/' for more information
about Cppcheck."
    (-when-let* ((root (flycheck-parse-xml-string output))
                 (errors (--first (eq (car it) 'errors) (cddr root))))
      (unless (eq (car root) 'results)
        (error "Unexpected root element %s" (car root)))
      (->> errors
        ;; Filter error nodes
        (--filter (and (listp it) (eq (car it) 'error)))
        (-map #'flycheck-parse-cppcheck-error-node)
        -flatten))))


;;;; Error analysis
(defvar-local flycheck-current-errors nil
  "A list of all errors and warnings in the current buffer.")

(defun flycheck-clear-errors ()
  "Remove all error information from the current buffer."
  (setq flycheck-current-errors nil)
  (flycheck-report-status ""))

(defun flycheck-relevant-error-p (err)
  "Determine whether ERR is relevant for the current buffer.

Return t if ERR may be shown for the current buffer, or nil
otherwise."
  (flycheck-error-with-buffer err
    (let ((file-name (flycheck-error-filename err)))
      (and
       (or (not file-name) (flycheck-same-files-p file-name (buffer-file-name)))
       (not (s-blank? (flycheck-error-message err)))
       (flycheck-error-line err)))))

(defun flycheck-relevant-errors (errors)
  "Filter the relevant errors from ERRORS.

Return a list of all errors that are relevant for their
corresponding buffer."
  (-filter #'flycheck-relevant-error-p errors))

(defun flycheck-error-<= (err1 err2)
  "Determine whether ERR1 goes before ERR2.

Compare by line numbers and then by column numbers."
  (let ((line1 (flycheck-error-line err1))
        (line2 (flycheck-error-line err2)))
    (if (= line1 line2)
        (let ((col1 (flycheck-error-column err1))
              (col2 (flycheck-error-column err2)))
          (or (not col1)                ; Sort errors for the whole line first
              (and col2 (<= col1 col2))))
      (< line1 line2))))

(defun flycheck-sort-errors (errors)
  "Sort ERRORS by line and column numbers.

ERRORS is modified by side effects."
  (sort errors 'flycheck-error-<=))

(defun flycheck-count-errors (errors)
  "Count the number of warnings and errors in ERRORS.

Return a cons cell whose `car' is the number of errors and whose
`car' is the number of warnings."
  (let* ((groups (-group-by 'flycheck-error-level errors))
         (errors (cdr (assq 'error groups)))
         (warnings (cdr (assq 'warning groups))))
    (cons (length errors) (length warnings))))

(defun flycheck-has-errors-p (errors &optional level)
  "Determine if there are any ERRORS with LEVEL.

If LEVEL is omitted check if ERRORS is not nil."
  (if level
      (--any? (eq (flycheck-error-level it) level) errors)
    (when errors t)))

(defun flycheck-has-current-errors-p (&optional level)
  "Determine if the current buffer has errors with LEVEL.

If LEVEL is omitted if the current buffer has any errors at all."
  (flycheck-has-errors-p flycheck-current-errors level))


;;;; Error overlay management
(when (fboundp 'define-fringe-bitmap)
  ;; define-fringe-bitmap is not available if Emacs is built without GUI
  ;; support, see https://github.com/lunaryorn/flycheck/issues/57
  (define-fringe-bitmap 'flycheck-fringe-exclamation-mark
    [24 60 60 24 24 0 0 24 24] nil nil 'center))

(defconst flycheck-fringe-exclamation-mark
  (if (get 'exclamation-mark 'fringe)
      'exclamation-mark
    'flycheck-fringe-exclamation-mark)
  "The symbol to use as exclamation mark bitmap.

Defaults to the built-in exclamation mark if available or to the
flycheck exclamation mark otherwise.")

(put 'flycheck-error-overlay 'flycheck-overlay t)
(put 'flycheck-error-overlay 'face 'flycheck-error)
(put 'flycheck-error-overlay 'priority 110)
(put 'flycheck-error-overlay 'help-echo "Unknown error.")
(put 'flycheck-error-overlay 'flycheck-fringe-face 'flycheck-fringe-error)
(put 'flycheck-error-overlay 'flycheck-fringe-bitmap
     flycheck-fringe-exclamation-mark)

(put 'flycheck-warning-overlay 'flycheck-overlay t)
(put 'flycheck-warning-overlay 'face 'flycheck-warning)
(put 'flycheck-warning-overlay 'priority 100)
(put 'flycheck-warning-overlay 'help-echo "Unknown warning.")
(put 'flycheck-warning-overlay 'flycheck-fringe-face 'flycheck-fringe-warning)
(put 'flycheck-warning-overlay 'flycheck-fringe-bitmap 'question-mark)

(defun flycheck-make-fringe-icon (category)
  "Create the fringe icon for CATEGORY.

Return a propertized string that shows a fringe bitmap according
to CATEGORY and the side specified `flycheck-indication-mode'.
Use this string as `before-string' of an overlay to actually show
the icon.

If `flycheck-indication-mode' is neither `left-fringe' nor
`right-fringe', returned nil."
  (when (memq flycheck-indication-mode '(left-fringe right-fringe))
    (let ((bitmap (get category 'flycheck-fringe-bitmap))
          (face (get category 'flycheck-fringe-face)))
      (propertize "!" 'display (list flycheck-indication-mode bitmap face)))))

(defun flycheck-add-overlay (err)
  "Add overlay for ERR.

Return the created overlay."
  (flycheck-error-with-buffer err
    (pcase-let* ((mode flycheck-highlighting-mode)
                 ;; Default to lines highlighting.  If MODE is nil, we want the
                 ;; line for the sake of indication and error messages.  We erase
                 ;; the highlighting later on
                 (`(,beg . ,end) (flycheck-error-region-for-mode
                                  err (or mode 'lines)))
                 (overlay (make-overlay beg end))
                 (level (flycheck-error-level err))
                 (category (pcase level
                             (`warning 'flycheck-warning-overlay)
                             (`error 'flycheck-error-overlay)
                             (_ (error "Invalid error level %S" level)))))
      (overlay-put overlay 'flycheck-error err)
      ;; TODO: Consider hooks to re-check if overlay contents change
      (overlay-put overlay 'category category)
      (unless mode
        ;; Erase the highlighting from the overlay if requested by the user
        (overlay-put overlay 'face nil))
      (-when-let (icon (flycheck-make-fringe-icon category))
        (overlay-put overlay 'before-string icon))
      (overlay-put overlay 'help-echo (flycheck-error-message err))
      overlay)))

(defun flycheck-filter-overlays (overlays)
  "Get all Flycheck overlays from OVERLAYS."
  (--filter (overlay-get it 'flycheck-overlay) overlays))

(defun flycheck-overlays-at (pos)
  "Get all Flycheck overlays at POS."
  (flycheck-filter-overlays (overlays-at pos)))

(defun flycheck-overlays-in (beg end)
  "Get all Flycheck overlays between BEG and END."
  (flycheck-filter-overlays (overlays-in beg end)))

(defun flycheck-overlay-errors-at (pos)
  "Return a list of all flycheck errors overlayed at POS."
  (--map (overlay-get it 'flycheck-error) (flycheck-overlays-at pos)))

(defvar-local flycheck-overlays-to-delete nil
  "Overlays mark for deletion after all syntax checks completed.")
(put 'flycheck-overlays-to-delete 'permanent-local t)

(defun flycheck-delete-all-overlays ()
  "Remove all flycheck overlays in the current buffer."
  (flycheck-delete-marked-overlays)
  (save-restriction
    (widen)
    (-each (flycheck-overlays-in (point-min) (point-max)) #'delete-overlay)))

(defun flycheck-mark-all-overlays-for-deletion ()
  "Mark all current overlays for deletion."
  (setq flycheck-overlays-to-delete
        (append (flycheck-overlays-in (point-min) (point-max))
                flycheck-overlays-to-delete)))

(defun flycheck-delete-marked-overlays ()
  "Delete all overlays marked for deletion."
  (-each flycheck-overlays-to-delete #'delete-overlay)
  (setq flycheck-overlays-to-delete nil))


;;;; Error navigation
(defun flycheck-navigatable-position-p (pos)
  "Determine whether POS can be navigated to."
  (and (>= pos (point-min))
       (<= pos (point-max))
       (and (flycheck-overlays-at pos))))

(defun flycheck-next-error-function (n reset)
  "Visit the N-th error from the current point.

Intended for use with `next-error-function'."
  ;; TODO: Horribly inefficient, possibly improve by considering less errors.
  (let* ((n (or n 1))
         (current-pos (if reset (point-min) (point)))
         (before-and-after (->> flycheck-current-errors
                             (-map 'flycheck-error-pos)
                             (-uniq)
                             (-filter #'flycheck-navigatable-position-p)
                             (--remove (= current-pos it))
                             (--split-with (>= current-pos it))))
         (before (nreverse (car before-and-after)))
         (after (cadr before-and-after))
         (error-pos (nth (- (abs n) 1) (if (< n 0) before after))))
    (if error-pos
        (goto-char error-pos)
      (user-error "No more Flycheck errors"))))

(defun flycheck-next-error (&optional n reset)
  "Visit the N-th error from the current point.

If RESET is given and non-nil, re-start from the beginning of the buffer.

N specifies how many errors to move forwards.  If negative, move backwards."
  (interactive "P")
  (flycheck-next-error-function n reset))

(defun flycheck-previous-error (&optional n)
  "Visit the N-th previous error.

If given, N specifies the number of errors to move backwards.  If
N is negative, move forwards instead."
  (interactive "P")
  (flycheck-next-error (- (or n 1))))

(defun flycheck-first-error (&optional n)
  "Visit the N-th error from beginning of the buffer.

If given, N specifies the number of errors to move forward from
the beginning of the buffer."
  (interactive "P")
  (flycheck-next-error n :reset))


;;;; Error list
(defconst flycheck-error-list-buffer "*Flycheck errors*"
  "The name of the buffer to show error lists.")

(defun flycheck-error-list-buffer ()
  "Get the buffer to show error lists.

If the buffer does not exist yet, create it.

Return the buffer object."
  (get-buffer-create flycheck-error-list-buffer))

(defconst flycheck-list-error-regex-alist
  '(("^\\(?1:.+?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)?error:" 1 2 3 2)
    ("^\\(?1:.+?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)?warning:" 1 2 3 1))
  "Compilation mode expressions for Flycheck error listings.")

(defvar flycheck-error-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'flycheck-error-list-refresh)
    map)
  "Keymap for Flycheck error list buffers.")

(define-derived-mode flycheck-error-list-mode compilation-mode "Flycheck errors"
  "A major mode for a Flycheck error listing."
  (setq-local compilation-error-regexp-alist flycheck-list-error-regex-alist))

(defvar-local flycheck-error-list-source-buffer nil
  "The current source buffer of the error list.")

(defun flycheck-error-list-buffer-label (buffer)
  "Create a list label for BUFFER relative to DIRECTORY.

BUFFER is a buffer object.

The label is either the file name of BUFFER, or the buffer name.
The file name is made relative to the current
`default-directory'.

Return the label as string."
  (-if-let (filename (buffer-file-name buffer))
    (file-relative-name filename)
    (format "#<buffer %s>" (buffer-name buffer))))

(defun flycheck-error-list-error-label (err)
  "Create a list label for ERR.

ERR is a Flycheck error.

The label is either the file name of ERR, the file name of the
buffer of ERR, or the buffer name.  The file name is made
relative to the current `default-directory'.

Return the label as string."
  (-if-let (filename (flycheck-error-filename err))
    (file-relative-name filename)
    (flycheck-error-list-buffer-label (flycheck-error-buffer err))))

(defun flycheck-error-list-insert-header (buffer)
  "Insert an error header for BUFFER into the current buffer.

Insert an error list header for BUFFER into the current buffer.
The header contains the filename of BUFFER relative to the
current `default-directory', or the buffer name, if BUFFER has no
file name."
  (insert (format "\n\n\C-l\n*** %s: Syntax and style errors (Flycheck v%s)\n"
                  (flycheck-error-list-buffer-label buffer)
                  (or (flycheck-version) "Unknown"))))

(defun flycheck-error-list-insert-errors (errors)
  "Insert a list of all ERRORS into the current buffer.

Insert a human-readable listing of ERRORS into the current
buffer.  The file names of ERRORS are printed relative to the
current `default-directory'."
  (--each errors
    (insert (flycheck-error-list-error-label it))
    (insert ":")
    (insert (flycheck-error-format it))
    (insert "\n")))

(defun flycheck-error-list-add-errors (errors &optional source-buffer)
  "Add ERRORS from SOURCE-BUFFER to the Flycheck error list.

ERRORS is a list of Flycheck errors to show.  SOURCE-BUFFER is
the buffer from which these errors originate, defaulting to the
current buffer.

The name of SOURCE-BUFFER is shown in the header inserted for
ERRORS.  It is also added to `flycheck-error-list-source-buffer'
and thus used by `flycheck-error-list-refresh'."
  (let* ((source-buffer (or source-buffer (current-buffer)))
         ;; Remember the default directory of the source buffer
         (source-directory (buffer-local-value 'default-directory source-buffer)))
    (with-current-buffer (flycheck-error-list-buffer)
      (flycheck-error-list-mode)
      (setq default-directory source-directory ; Change to the right directory
                                        ; to resolve error references properly
            flycheck-error-list-source-buffer source-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (flycheck-error-list-insert-header source-buffer)
        (flycheck-error-list-insert-errors errors)))))

(defun flycheck-error-list-goto-current-errors ()
  "Move the point to the beginning of the current errors."
  (goto-char (point-max))
  (re-search-backward "\C-l" nil :no-error)
  (beginning-of-line)
  (forward-line 1))

(defun flycheck-error-list-recenter ()
  "Recenter the current error list buffer.

Move the point to the beginning of the current errors, and
recenter the display, if the error list is currently shown."
  (-if-let (window (get-buffer-window))
    (with-selected-window window
      ;; We must move the point with the right window selected, for otherwise
      ;; the point location is mysteriously wrong when recentering :|
      (flycheck-error-list-goto-current-errors)
      (recenter 0))
    ;; If there is no window, just update the location of the point
    (flycheck-error-list-goto-current-errors)))

(defun flycheck-error-list-refresh ()
  "Refresh the current error list.

Add all errors currently reported for the current
`flycheck-error-list-source-buffer', and recenter the error
list."
  (interactive)
  (unless (derived-mode-p 'flycheck-error-list-mode)
    (user-error "The current buffer is no error list"))
  (let ((source flycheck-error-list-source-buffer))
    (when (buffer-live-p source)
      (unless (buffer-local-value 'flycheck-mode source)
        (user-error "Flycheck mode disabled in %S" source))
      (let ((errors (buffer-local-value 'flycheck-current-errors source)))
        (flycheck-error-list-add-errors errors source))
      (flycheck-error-list-recenter))))

(defun flycheck-list-errors (&optional pos)
  "List all errors at POS in current buffer.

Show all Flycheck errors at POS in the current buffer in a
separate window.  If POS is omitted or nil, show all errors.

Interactively, show all Flycheck errors.  With prefix arg, only
show errors at point.

Return the buffer containing the error listing."
  (interactive (when current-prefix-arg (list (point))))
  (unless flycheck-mode
    (user-error "Flycheck mode not enabled"))
  (let ((errors (if pos
                    (flycheck-overlay-errors-at pos)
                  flycheck-current-errors)))
    (flycheck-error-list-add-errors errors)
    (pop-to-buffer (flycheck-error-list-buffer))
    (flycheck-error-list-recenter)
    ;; Switch back to the old window
    (other-window -1)
    (flycheck-error-list-buffer)))


;;;; General error display
(defun flycheck-display-errors (errors)
  "Display ERRORS using `flycheck-display-errors-function'."
  (when flycheck-display-errors-function
    (funcall flycheck-display-errors-function errors)))

(defvar-local flycheck-error-show-error-timer nil
  "Timer to automatically show the error at point in minibuffer.")

(defun flycheck-cancel-error-show-error-timer ()
  "Cancel the error display timer for the current buffer."
  (when flycheck-error-show-error-timer
    (cancel-timer flycheck-error-show-error-timer)
    (setq flycheck-error-show-error-timer nil)))

(defun flycheck-show-error-at-point ()
  "Show the all error messages at point in minibuffer."
  (flycheck-cancel-error-show-error-timer)
  (when flycheck-mode
    (-when-let (errors (flycheck-overlay-errors-at (point)))
      (flycheck-display-errors errors))))

(defun flycheck-show-error-at-point-soon ()
  "Show the first error message at point in minibuffer asap.

Show the error message at point in minibuffer after a short delay."
  (flycheck-cancel-error-show-error-timer)
  (when (flycheck-overlays-at (point))
    (setq flycheck-error-show-error-timer
          (run-at-time 0.9 nil 'flycheck-show-error-at-point))))


;;;; Error display functions
(defconst flycheck-error-message-buffer "*Flycheck error messages*"
  "The name of the buffer to show long error messages in.")

(defun flycheck-error-message-buffer ()
  "Get the buffer object to show long error messages in.

Get the buffer named by variable `flycheck-error-message-buffer',
or nil if the buffer does not exist."
  (get-buffer flycheck-error-message-buffer))

(defun flycheck-may-use-echo-area-p ()
  "Determine whether the echo area may be used.

The echo area may be used if the cursor is not in the echo area,
and if the echo area is not occupied by minibuffer input."
  (not (or cursor-in-echo-area (active-minibuffer-window))))

(defun flycheck-display-error-messages (errors)
  "Display the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `display-message-or-buffer', which
shows the messages either in the echo area or in a separate
buffer, depending on the number of lines.

In the latter case, show messages in
`flycheck-error-message-buffer'."
  (-when-let (messages (-keep #'flycheck-error-message errors))
    (when (flycheck-may-use-echo-area-p)
      (display-message-or-buffer (s-join "\n\n" messages)
                                 flycheck-error-message-buffer))))

(defun flycheck-hide-error-buffer ()
  "Hide the Flycheck error buffer if necessary.

Hide the error buffer if there is no error under point."
  (-when-let* ((buffer (flycheck-error-message-buffer))
               (window (get-buffer-window buffer)))
    (unless (flycheck-overlays-at (point))
      (quit-window nil window))))

(defun flycheck-display-errors-in-list (errors)
  "Display ERRORS in the error list.

Add all ERRORS to the error list, but do *not* show the error
list.

Note that this function does *not* actually show the error list,
it just adds ERRORS to it."
  (flycheck-error-list-add-errors errors)
  (with-current-buffer (flycheck-error-list-buffer)
    (flycheck-error-list-recenter)))


;;;; Working with error messages
(defun flycheck-copy-messages-as-kill (pos)
  "Copy each error message under POS into kill ring.

Each error message under point is copied into the kill ring."
  (interactive "d")
  (-when-let* ((errors (flycheck-overlay-errors-at pos))
               (messages (-keep #'flycheck-error-message errors)))
    (-each (reverse messages) #'kill-new)
    (message (s-join "\n" messages))))

(defun flycheck-google-messages (pos &optional quote-flag)
  "Google each error message at POS.

Issue a separate Google query for each error message at POS.
Signal an error if there are more messages at POS than
`flycheck-google-max-messages'.

Enclose the Google query in quotation marks, if
`google-wrap-in-quotes' is t.  With QUOTE-FLAG, invert the effect
of `google-wrap-in-quotes'.

This function requires the Google This library from URL
`https://github.com/Bruce-Connor/emacs-google-this'."
  (interactive "d\nP")
  (if (fboundp 'google-string)
      (-when-let (messages (->> pos
                             flycheck-overlay-errors-at
                             (-keep #'flycheck-error-message)))
        (when (and flycheck-google-max-messages
                   (> (length messages) flycheck-google-max-messages))
          (user-error "More than %s messages at point"
                      flycheck-google-max-messages))
        (--each messages
          (google-string quote-flag it :no-confirm)))
    (user-error "Please install Google This from \
https://github.com/Bruce-Connor/emacs-google-this")))


;;;; Checker process management
(defvar-local flycheck-current-process nil
  "The current syntax checking process.")
(put 'flycheck-current-process 'permanent-local t)

(defun flycheck-running-p ()
  "Determine whether a syntax check is running."
  (when (and flycheck-current-process
             (memq (process-status flycheck-current-process) '(exit signal)))
    (flycheck-delete-process flycheck-current-process)
    (setq flycheck-current-process nil))
  (when flycheck-current-process t))

(defun flycheck-delete-process (process)
  "Delete PROCESS and clear it's resources."
  (flycheck-safe-delete-files (process-get process :flycheck-temp-files))
  (flycheck-safe-delete-directories
   (process-get process :flycheck-temp-directories))
  (delete-process process))

(defun flycheck-receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (let ((pending-output (process-get process :flycheck-pending-output)))
    (process-put process :flycheck-pending-output
                 (cons output pending-output))))

(defun flycheck-get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors
    (let ((pending-output (process-get process :flycheck-pending-output)))
      (apply #'concat (nreverse pending-output)))))

(defun flycheck-finish-syntax-check (checker exit-status files output)
  "Finish a syntax check from CHECKER with EXIT-STATUS.

FILES is a list of files given as input to the checker.  OUTPUT
is the output of the syntax checker.

Parse the OUTPUT and report an appropriate error status."
  (flycheck-report-status "")
  (let (errors)
    (condition-case err
        (setq errors (flycheck-parse-output output checker (current-buffer)))
      (error
       (message "Failed to parse errors from checker %S in output: %s\n\
Error: %s" checker output (error-message-string err))
       (flycheck-report-error)
       (setq errors :errored)))
    (unless (eq errors :errored)
      (setq errors (-> errors
                     (flycheck-fix-error-filenames files)
                     flycheck-relevant-errors))
      (setq flycheck-current-errors (-> errors
                                      (append flycheck-current-errors nil)
                                      flycheck-sort-errors))
      ;; Process all new errors
      (--each errors
        (run-hook-with-args-until-success 'flycheck-process-error-functions it))
      (flycheck-report-error-count flycheck-current-errors)
      (when (and (/= exit-status 0) (not errors))
        (message "Checker %S returned non-zero exit code %s, but no errors from \
output: %s\nChecker definition probably flawed."
                 checker exit-status output)
        (flycheck-report-status "?"))
      (let ((next-checker (flycheck-get-next-checker-for-buffer checker)))
        (if next-checker
            (flycheck-start-checker next-checker)
          (flycheck-delete-marked-overlays)
          (run-hooks 'flycheck-after-syntax-check-hook)
          (when (eq (current-buffer) (window-buffer))
            (flycheck-show-error-at-point)))))))

(defun flycheck-handle-signal (process _event)
  "Handle a signal from the syntax checking PROCESS.

_EVENT is ignored."
  (when (memq (process-status process) '(signal exit))
    (let ((checker (process-get process :flycheck-checker))
          (files (process-get process :flycheck-temp-files))
          (exit-status (process-exit-status process))
          (output (flycheck-get-output process))
          (buffer (process-buffer process)))
      (flycheck-delete-process process)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq flycheck-current-process nil)
          (flycheck-report-status "")
          (when flycheck-mode
            (condition-case err
                (flycheck-finish-syntax-check checker exit-status files output)
              (error
               (flycheck-report-error)
               (signal (car err) (cdr err))))))))))

(defun flycheck-start-checker (checker)
  "Start a syntax CHECKER."
  (condition-case err
      (let* ((command (flycheck-checker-substituted-command checker))
             (program (car command))
             (args (cdr command))
             (process-connection-type nil) ; Use pipes to receive checker output
             (process (apply 'start-process
                             "flycheck" (current-buffer)
                             program args)))
        (setq flycheck-current-process process)
        (set-process-filter process 'flycheck-receive-checker-output)
        (set-process-sentinel process 'flycheck-handle-signal)
        (set-process-query-on-exit-flag process nil)
        (flycheck-report-status "*")
        (process-put process :flycheck-temp-files
                     flycheck-temp-files)
        (process-put process :flycheck-temp-directories
                     flycheck-temp-directories)
        ;; Now that temporary files and directories are attached to the process,
        ;; we can reset the variables used to collect them
        (setq flycheck-temp-files nil
              flycheck-temp-directories nil)
        (process-put process :flycheck-checker checker))
    (error
     (flycheck-report-error)
     (flycheck-safe-delete-temporaries)
     (when flycheck-current-process
       ;; Clear the process if it's already there
       (flycheck-delete-process flycheck-current-process)
       (setq flycheck-current-process nil))
     (signal (car err) (cdr err)))))

(defun flycheck-stop-checker ()
  "Stop any syntax checker for the current buffer."
  (when (flycheck-running-p)
    (interrupt-process flycheck-current-process)))


;;;; Emacs project helpers
(defun flycheck-get-ede-cpp-project (file)
  "Get EDE C/C++ project for FILE.
If FILE does not belong to an EDE C/C++ project, or if EDE is not present, this
function returns nil."
  (when (featurep 'ede)
      (let ((project (ede-current-project (expand-file-name file))))
        (when (ede-cpp-root-project-p project)
            project))))

(defun flycheck-get-ede-cpp-project-includes (project)
  "Get include paths from EDE C/C++ PROJECT."
  (when (ede-cpp-root-project-p project)
      (let* ((root-path (ede-project-root-directory project))
             (include-paths (oref project include-path)))
        (mapcar (lambda (path) (expand-file-name path root-path)) include-paths))))

(defun flycheck-get-cpp-project-includes (option-name file)
  "Construct a list of includes using OPTION-NAME for the specified FILE.
If FILE is associated with a supported project type and if it has a list of
include paths a list consisting of OPTION-NAME concatenated with each path
will be constructed."
  (mapcar (lambda (path) (concat option-name path))
          (let ((ede-proj (flycheck-get-ede-cpp-project file)))
            (cond
             (ede-proj (flycheck-get-ede-cpp-project-includes ede-proj))))))

(defun flycheck-get-cpp-project-definitions (option-name file)
  "Construct a list of definitions using OPTION-NAME for the specified FILE.
If FILE is associated with a supported project type and if it has a list of
definitions a list consisting of OPTION-NAME concatenated with the name of the
definition followed by an optional =<value> will be constructed."
  (mapcar (lambda (define)
            (let ((macro (car define))
                  (value (cdr define)))
              (concat option-name macro
                      (unless (string= "" value)
                        (concat "=" value)))))
          (let ((ede-proj (flycheck-get-ede-cpp-project file)))
            (cond
             (ede-proj (ede-preprocessor-map ede-proj))))))


;;;; Built-in checkers
(flycheck-define-checker bash
  "A Bash syntax checker using the Bash shell.

See URL `http://www.gnu.org/software/bash/'."
  :command ("bash" "--norc" "-n" "--" source)
  :error-patterns ((error line-start
                          (file-name) ":" (one-or-more (not (any digit)))
                          line (zero-or-more " ") ":" (zero-or-more " ")
                          (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'bash)))

(flycheck-def-option-var flycheck-clang-warnings '("all" "extra") c/c++-clang
  "A list of additional warnings to enable in Clang.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the Clang manual at URL
`http://clang.llvm.org/docs/UsersManual.html' for more
information about warnings."
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-clang-include-path nil c/c++-clang
  "A list of include directories for Clang.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Clang.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-define-checker c/c++-clang
  "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
  :command ("clang"
            "-fsyntax-only"
            "-fno-color-diagnostics"    ; Do not include color codes in output
            "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            (option-list "-W" flycheck-clang-warnings s-prepend)
            (option-list "-I" flycheck-clang-include-path)
            (eval
             (let ((source-file (buffer-file-name (current-buffer))))
               (append
                (flycheck-get-cpp-project-includes "-I" source-file)
                (flycheck-get-cpp-project-definitions "-D" source-file))))
            "-x" (eval
                  (cl-case major-mode
                    (c++-mode "c++")
                    (c-mode "c"))) source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode c++-mode)
  :next-checkers ((warnings-only . c/c++-cppcheck)))

(flycheck-def-option-var flycheck-cppcheck-checks '("style") c/c++-cppcheck
  "Enabled checks for Cppcheck.

The value of this variable is a list of strings, where each
string is the name of an additional check to enable.  By default,
all coding style checks are enabled.

See section \"Enable message\" in the Cppcheck manual at URL
`http://cppcheck.sourceforge.net/manual.pdf', and the
documentation of the `--enable' option for more information,
including a list of supported checks."
  :type '(repeat :tag "Additional checks"
                 (string :tag "Check name"))
  ;; Quote this lambda, to allow `describe-variable' to display the lambda
  ;; properly
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-define-checker c/c++-cppcheck
  "A C/C++ checker using cppcheck.

See URL `http://cppcheck.sourceforge.net/'."
  :command ("cppcheck" "--quiet" "--xml-version=2" "--inline-suppr"
            (option "--enable=" flycheck-cppcheck-checks
                    flycheck-option-comma-separated-list)
            (eval
             (let ((source-file (buffer-file-name (current-buffer))))
               (append
                (flycheck-get-cpp-project-includes "-I" source-file)
                (flycheck-get-cpp-project-definitions "-D" source-file))))
            source)
  :error-parser flycheck-parse-cppcheck
  :modes (c-mode c++-mode))

(flycheck-def-config-file-var flycheck-coffeelintrc coffee-coffeelint
                              ".coffeelint.json"
  :safe #'stringp)

(flycheck-define-checker coffee-coffeelint
  "A CoffeeScript syntax and style checker using coffeelint.

See URL `http://www.coffeelint.org/'."
  :command
  ("coffeelint" (config-file "--file" flycheck-coffeelintrc) "--csv" source)
  :error-patterns
  ((error "SyntaxError: " (message) " on line " line)
   (error line-start (file-name) "," line ",error," (message) line-end)
   (warning line-start (file-name) "," line ",warn," (message) line-end))
  :modes coffee-mode)

(flycheck-define-checker css-csslint
  "A CSS syntax and style checker using csslint.

See URL `https://github.com/stubbornella/csslint'."
  :command ("csslint" "--format=compact" source)
  :error-patterns
  ((error line-start
          (file-name)
          ": line " line
          ", col " column
          ", Error - " (message) line-end)
   (warning line-start
            (file-name) ": line "
            line ", col "
            column ", Warning - " (message) line-end))
  :modes css-mode)

(flycheck-define-checker elixir
  "An Elixir syntax checker using the Elixir interpreter.

See URL `http://elixir-lang.org/'."
  :command ("elixirc"
            "-o" temporary-directory    ; Move compiler output out of the way
            "--ignore-module-conflict"  ; Prevent tedious module redefinition
                                        ; warning.
            source)
  :error-patterns
  ((error line-start "** (" (zero-or-more not-newline) ") "
          (file-name) ":" line ": " (message) line-end)
   (warning line-start
            (file-name) ":"
            line ": "
            (message)
            line-end))
  :modes elixir-mode)

(defconst flycheck-emacs-command
  `(,(concat invocation-directory invocation-name)
    "--no-site-file" "--no-site-lisp" "--batch")
  "A command to execute an Emacs Lisp form in a background process.")

(defun flycheck-temp-compilation-buffer-p ()
  "Determine whether the current buffer is a temporary buffer.

Return t if the current buffer is a temporary buffer created
during byte-compilation or autoloads generation, or nil otherwise."
  (or (member (buffer-name) '(" *Compiler Input*" " *autoload-file*"))
      (s-ends-with? "-autoloads.el" (buffer-name))))

(defconst flycheck-emacs-lisp-check-form
  '(progn
     ;; Try best to make local dependencies available
     (defun flycheck-extend-load-path (filename)
       (add-to-list 'load-path (file-name-directory filename)))

     (defvar flycheck-byte-compiled-files nil)
     (defun flycheck-byte-compile-dest-file (source)
       (let ((temp-file (make-temp-file (file-name-nondirectory source))))
         (add-to-list 'flycheck-byte-compiled-files temp-file)
         temp-file))

     (setq byte-compile-dest-file-function 'flycheck-byte-compile-dest-file)
     (mapc 'flycheck-extend-load-path command-line-args-left)
     (mapc 'byte-compile-file command-line-args-left)
     (mapc 'delete-file flycheck-byte-compiled-files)))

(flycheck-def-option-var flycheck-emacs-lisp-load-path nil emacs-lisp
  "Load path to use in the Emacs Lisp syntax checker.

When set to a list of strings, add each directory in this list to
the `load-path' before invoking the byte compiler.  Relative
paths in this list are expanded against the `default-directory'
of the buffer to check.  When nil, only use the built-in
`load-path' of Emacs.

The directory of the file being checked is always part of the
`load-path' while checking, regardless of the value of this
variable.

Set this variable to `load-path' to use the `load-path' of your
Emacs session for syntax checking.

Note that changing this variable can lead to wrong results of the
syntax check, e.g. if an unexpected version of a required library
is used."
  :type '(repeat directory)
  :risky t
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-emacs-lisp-initialize-packages
    'auto emacs-lisp
  "Whether to initialize packages in the Emacs Lisp syntax checker.

To initialize packages, call `package-initialize' before
byte-compiling the file to check.

When nil, never initialize packages.  When `auto', initialize
packages only when checking files from `user-emacs-directory'.
For any other non-nil value, always initialize packages."
  :type '(choice (const :tag "Do not initialize packages" nil)
                 (const :tag "Initialize packages for configuration only" auto)
                 (const :tag "Always initialize packages" t))
  :risky t
  :package-version '(flycheck . "0.14"))

(defun flycheck-option-emacs-lisp-package-initialize (value)
  "Option filter for `flycheck-emacs-lisp-initialize-packages'."
  (when (eq value 'auto)
    (let ((user-dir (expand-file-name user-emacs-directory)))
      (setq value (s-starts-with? user-dir (buffer-file-name)))))
  ;; Return the function name, if packages shall be initialized, otherwise
  ;; return nil to have Flycheck drop the whole option
  (when value "package-initialize"))

(flycheck-def-option-var flycheck-emacs-lisp-package-user-dir nil emacs-lisp
  "Package directory for the Emacs Lisp syntax checker.

When set to a string, set `package-user-dir' to the value of this
variable before initializing packages.

This variable has no effect, if
`flycheck-emacs-lisp-initialize-packages' is nil."
  :type '(choice (const :tag "Default package directory" nil)
                 (directory :tag "Custom package directory"))
  :risky t
  :package-version '(flycheck . "0.14"))

(defun flycheck-option-emacs-lisp-package-user-dir (value)
  "Option filter for `flycheck-emacs-lisp-package-user-dir'."
  (unless value
    ;; Inherit the package directory from our Emacs session
    (setq value package-user-dir))
  (when value
    (flycheck-sexp-to-string `(setq package-user-dir ,value))))

(flycheck-define-checker emacs-lisp
  "An Emacs Lisp syntax checker using the Emacs Lisp Byte compiler."
  :command ((eval flycheck-emacs-command)
            (option-list "--directory" flycheck-emacs-lisp-load-path nil
                         ;; Expand relative paths against the directory of the
                         ;; buffer to check
                         expand-file-name)
            (option "--eval" flycheck-emacs-lisp-package-user-dir
                    flycheck-option-emacs-lisp-package-user-dir)
            (option "--funcall" flycheck-emacs-lisp-initialize-packages
                    flycheck-option-emacs-lisp-package-initialize)
            "--eval"
            (eval (flycheck-sexp-to-string flycheck-emacs-lisp-check-form))
            source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":Error:"
          (message (zero-or-more not-newline)
                   (zero-or-more "\n    " (zero-or-more not-newline)))
          line-end)
   (warning line-start (file-name) ":" line ":" column ":Warning:"
            (message (zero-or-more not-newline)
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end))
  :modes (emacs-lisp-mode lisp-interaction-mode)
  ;; Ensure that we only check buffers with a backing file.  For buffers without
  ;; a backing file we cannot guarantee that file names in error messages are
  ;; properly resolved, because `byte-compile-file' emits file names *relative
  ;; to the directory of the checked file* instead of the working directory.
  ;; Hence our backwards-substitution will fail, because the checker process has
  ;; a different base directory to resolve relative file names than the flycheck
  ;; code working on the buffer to check.
  :predicate
  (lambda ()
    (and (buffer-file-name)
         ;; Do not check buffers which should not be byte-compiled.  The
         ;; checker process will refuse to compile these anyway
         (not (and (boundp 'no-byte-compile) no-byte-compile))
         ;; Checking temporary buffers from `byte-compile-file' or autoload
         ;; buffers interferes with package installation. See
         ;; https://github.com/lunaryorn/flycheck/issues/45 and
         ;; https://github.com/bbatsov/prelude/issues/248
         (not (flycheck-temp-compilation-buffer-p))))
  :next-checkers (emacs-lisp-checkdoc))

(defconst flycheck-emacs-lisp-checkdoc-form
  '(progn
     (require 'checkdoc)

     (let ((filename (car command-line-args-left))
           (process-default-directory default-directory))
       (with-temp-buffer
         (insert-file-contents filename t)
         (setq buffer-file-name filename)
         (setq default-directory process-default-directory)
         (with-demoted-errors
           (checkdoc-current-buffer t)
           (with-current-buffer checkdoc-diagnostic-buffer
             (princ (buffer-substring-no-properties (point-min)
                                                    (point-max)))
             (kill-buffer)))))))

(flycheck-define-checker emacs-lisp-checkdoc
  "An Emacs Lisp style checker using CheckDoc.

The checker runs `checkdoc-current-buffer'."
  :command ((eval flycheck-emacs-command) "--eval"
            (eval (flycheck-sexp-to-string flycheck-emacs-lisp-checkdoc-form))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": " (message) line-end))
  :modes (emacs-lisp-mode)
  :predicate
  (lambda ()
    (and (not (flycheck-temp-compilation-buffer-p))
         ;; Do not check Cask/Carton files.  These really don't need to follow
         ;; Checkdoc conventions
         (not (and (buffer-file-name)
                   (member (file-name-nondirectory (buffer-file-name))
                           '("Cask" "Carton")))))))

(flycheck-define-checker erlang
  "An Erlang syntax checker using the Erlang interpreter."
  :command ("erlc" "-o" temporary-directory "-Wall" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes erlang-mode)

(flycheck-define-checker go-gofmt
  "A Go syntax and style checker using the gofmt utility.

See URL `http://golang.org/cmd/gofmt/'."
  :command ("gofmt" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
:modes go-mode
  :next-checkers ((no-errors . go-build) (no-errors . go-test)))

(flycheck-define-checker go-build
  "A Go syntax and type checker using the `go build' command.

See URL `https://golang.org/cmd/go'.

This syntax checker may cause bogus warnings due to an upstream
bug in Go.

See URL `https://code.google.com/p/go/issues/detail?id=4851' for
more information."
  :command ("go" "build" "-o" "/dev/null")
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes go-mode
  :predicate
  (lambda ()
    (and (not (s-ends-with? "_test.go" (buffer-file-name)))
         (not (buffer-modified-p)))))

(flycheck-define-checker go-test
  "A Go syntax and type checker using the `go test' command.

See URL `https://golang.org/cmd/go'."
  ;; This command builds the test executable and leaves it in the current
  ;; directory.  Unfortunately 'go test -c' does not have the '-o' option.
  :command ("go" "test" "-c")
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes go-mode
  :predicate
  (lambda ()
    (and (s-ends-with? "_test.go" (buffer-file-name))
         (not (buffer-modified-p)))))

(flycheck-define-checker haml
  "A Haml syntax checker using the Haml compiler.

See URL `http://haml.info'."
  :command ("haml" "-c" source)
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message) line-end))
  :modes haml-mode)

(flycheck-define-checker haskell-hdevtools
  "A Haskell syntax and type checker using hdevtools.

See URL `https://github.com/bitc/hdevtools'."
  :command ("hdevtools" "check" "-g" "-Wall" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (one-or-more " ")
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n" (one-or-more " ")
                   (message (one-or-more not-newline)
                            (zero-or-more "\n"
                                          (one-or-more " ")
                                          (one-or-more not-newline)))))
          line-end))
  :modes haskell-mode
  :next-checkers ((warnings-only . haskell-hlint)))

(flycheck-define-checker haskell-ghc
  "A Haskell syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
  :command ("ghc" "-Wall" "-fno-code" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (one-or-more " ")
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n" (one-or-more " ")
                   (message (one-or-more not-newline)
                            (zero-or-more "\n"
                                          (one-or-more " ")
                                          (one-or-more not-newline)))))
          line-end))
  :modes haskell-mode
  :next-checkers ((warnings-only . haskell-hlint)))

(flycheck-define-checker haskell-hlint
  "A Haskell style checker using hlint.

See URL `http://community.haskell.org/~ndm/hlint/'."
  :command ("hlint" source-inplace)
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column
            ": Warning: "
            (message (one-or-more not-newline)
                     (one-or-more "\n" (one-or-more not-newline)))
            line-end)
   (error line-start
          (file-name) ":" line ":" column
          ": Error: "
          (message (one-or-more not-newline)
                   (one-or-more "\n" (one-or-more not-newline)))
          line-end))
  :modes haskell-mode)

(flycheck-def-config-file-var flycheck-tidyrc html-tidy ".tidyrc"
  :safe #'stringp)

(flycheck-define-checker html-tidy
  "A HTML syntax and style checker using Tidy.

See URL `https://github.com/w3c/tidy-html5'."
  :command ("tidy" (config-file "-config" flycheck-tidyrc) "-e" "-q" source)
  :error-patterns
  ((error line-start
          "line " line
          " column " column
          " - Error: " (message) line-end)
   (warning line-start
            "line " line
            " column " column
            " - Warning: " (message) line-end))
  :modes (html-mode nxhtml-mode web-mode))

(flycheck-def-config-file-var flycheck-jshintrc javascript-jshint ".jshintrc"
  :safe #'stringp)

(flycheck-define-checker javascript-jshint
  "A JavaScript syntax and style checker using jshint.

See URL `http://www.jshint.com'."
  :command ("jshint" "--checkstyle-reporter"
            (config-file "--config" flycheck-jshintrc)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (js-mode js2-mode js3-mode))

(flycheck-define-checker json-jsonlint
  "A JSON syntax and style checker using jsonlint.

See URL `https://github.com/zaach/jsonlint'."
  :command ("jsonlint" "-c" "-q" source)
  :error-patterns
  ((error line-start
          (file-name)
          ": line " line
          ", col " column ", "
          (message) line-end))
  :predicate
  (lambda ()
    (or
     (eq major-mode 'json-mode)
     (and buffer-file-name
          (string= "json" (file-name-extension buffer-file-name))))))

(flycheck-define-checker less
  "A LESS syntax checker using lessc.
At least version 1.4 of lessc is required.

See URL `http://lesscss.org'."
  :command ("lessc" "--lint" "--no-color" source)
  :error-patterns
  ((error line-start (one-or-more word) ":"
          (message)
          " in "
          (file-name)

          " on line " line
          ", column " column ":"
          line-end))
  :modes less-css-mode)

(flycheck-define-checker lua
  "A Lua syntax checker using the Lua compiler.

See URL `http://www.lua.org/'."
  :command ("luac" "-p" source)
  :error-patterns
  ((error line-start
          (minimal-match (zero-or-more not-newline)) ; Skip name of the luac
                                                     ; executable
          ": " (file-name) ":" line ": " (message) line-end))
  :modes lua-mode)

(flycheck-define-checker perl
  "A Perl syntax checker using the Perl interpreter.

See URL `http://www.perl.org'."
  :command ("perl" "-w" "-c" source)
  :error-patterns
  ((error line-start (minimal-match (message))
          " at " (file-name) " line " line
          (or "." (and ", " (zero-or-more not-newline))) line-end))
  :modes (perl-mode cperl-mode))

(flycheck-define-checker php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode)
  :next-checkers ((warnings-only . php-phpcs)))

(flycheck-def-option-var flycheck-phpcs-standard nil php-phpcs
  "The coding standard for PHP CodeSniffer.

When nil, use the default standard from the global PHP
CodeSniffer configuration.  When set to a string, pass the string
to PHP CodeSniffer which will interpret it as name as a standard,
or as path to a standard specification."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Standard name or file"))
  :safe #'stringp)

(flycheck-define-checker php-phpcs
  "A PHP style checker using PHP_CodeSniffer.

See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
  :command ("phpcs" "--report=emacs"
            (option "--standard=" flycheck-phpcs-standard)
            source)
  ;; Though phpcs supports Checkstyle output which we could feed to
  ;; `flycheck-parse-checkstyle', we are still using error patterns here,
  ;; because PHP has notoriously unstable output habits.  See URL
  ;; `https://github.com/lunaryorn/flycheck/issues/78' and URL
  ;; `https://github.com/lunaryorn/flycheck/issues/118'
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column ": error - " (message)
          line-end)
   (warning line-start
            (file-name) ":" line ":" column ": warning - " (message)
            line-end))
  :modes (php-mode php+-mode))

(flycheck-define-checker puppet-parser
  "A Puppet DSL syntax checker using puppet's own parser.

See URL `http://www.puppetlabs.com/'."
  :command ("puppet" "parser" "validate" "--color=false" source)
  :error-patterns
  ((error line-start
          (minimal-match (zero-or-more not-newline)) ; Skip puppetc executable
                                                     ; name
          ": Could not parse for environment " (one-or-more word)
          ": " (message (minimal-match (zero-or-more anything)))
          " at "  (file-name "/" (zero-or-more not-newline)) ":" line line-end))
  :modes puppet-mode
  :next-checkers ((no-errors . puppet-lint)))

(flycheck-define-checker puppet-lint
  "A Puppet DSL style checker using puppet-lint.

See URL `http://www.puppet-lint.com/'."
  :command ("puppet-lint" "--with-filename" source-original)
  :error-patterns
  ((warning line-start
            (file-name) " - WARNING: " (message) " on line " line
            line-end)
   (error line-start
          (file-name) " - ERROR: " (message) " on line " line
          line-end))
  :modes puppet-mode
  :predicate (lambda () (and (buffer-file-name) (not (buffer-modified-p)))))

(flycheck-def-config-file-var flycheck-flake8rc python-flake8 ".flake8rc"
  :safe #'stringp)

(flycheck-def-option-var flycheck-flake8-maximum-complexity nil python-flake8
  "The maximum McCabe complexity of methods.

If nil, do not check the complexity of methods.  If set to an
integer, report any complexity greater than the value of this
variable as warning.

If set to an integer, this variable overrules any similar setting
in the configuration file denoted by `flycheck-flake8rc'."
  :type '(choice (const :tag "Do not check McCabe complexity" nil)
                 (integer :tag "Maximum complexity"))
  :safe #'integerp)

(flycheck-def-option-var flycheck-flake8-maximum-line-length nil python-flake8
  "The maximum length of lines.

If set to an integer, the value of this variable denotes the
maximum length of lines, overruling any similar setting in the
configuration file denoted by `flycheck-flake8rc'.  An error will
be reported for any line longer than the value of this variable.

If set to nil, use the maximum line length from the configuration
file denoted by `flycheck-flake8rc', or the PEP 8 recommendation
of 79 characters if there is no configuration with this setting."
  :type '(choice (const :tag "Default value")
                 (integer :tag "Maximum line length in characters"))
  :safe #'integerp)

(flycheck-define-checker python-flake8
  "A Python syntax and style checker using Flake8.

For best error reporting, use Flake8 2.0 or newer.

See URL `http://pypi.python.org/pypi/flake8'."
  :command ("flake8"
            (config-file "--config" flycheck-flake8rc)
            (option "--max-complexity"
                    flycheck-flake8-maximum-complexity
                    flycheck-option-int)
            (option "--max-line-length"
                    flycheck-flake8-maximum-line-length
                    flycheck-option-int)
            source-inplace)
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" (optional column ":") " "
          (message "E" (one-or-more digit) (zero-or-more not-newline))
          line-end)
   (warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (message (or "F"            ; Pyflakes in Flake8 >= 2.0
                         "W"            ; Pyflakes in Flake8 < 2.0
                         "C"            ; McCabe in Flake >= 2.0
                         "N")           ; pep8-naming in Flake8 >= 2.0
                     (one-or-more digit) (zero-or-more not-newline))
            line-end)
   ;; Syntax errors in Flake8 < 2.0, in Flake8 >= 2.0 syntax errors are caught
   ;; by the E.* pattern above
   (error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

(flycheck-define-checker python-pylint
  "A Python syntax and style checker using Pylint.

See URL `http://pypi.python.org/pypi/pylint'."
  :command ("epylint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line
            ": Warning (W" (zero-or-more not-newline) "): "
            (message) line-end)
   (error line-start (file-name) ":" line
          ": Error (E" (zero-or-more not-newline) "): "
          (message) line-end)
   (error line-start (file-name) ":" line ": [F] " (message) line-end))
  :modes python-mode)

(flycheck-define-checker rst
  "A ReStructuredText (RST) syntax checker using Docutils.

See URL `http://docutils.sourceforge.net/'."
  :command ("rst2pseudoxml.py" "--report=2" "--exit-status=1" "--halt=5" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": (WARNING/2) " (message) line-end)
   (error line-start
          (file-name) ":" line
          ": (" (or "ERROR/3" "SEVERE/4") ") "
          (message) line-end))
  :modes rst-mode)

(flycheck-def-config-file-var flycheck-rubocoprc ruby-rubocop ".rubocop.yml"
  :safe #'stringp)

(flycheck-define-checker ruby-rubocop
  "A Ruby syntax and style checker using the RuboCop tool.

See URL `http://batsov.com/rubocop/'."
  :command ("rubocop" "--format" "emacs" "--silent"
            (config-file "--config" flycheck-rubocoprc)
            source)
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
            line-end)
   (error line-start
          (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
          line-end))
  :modes ruby-mode)

(flycheck-define-checker ruby
  "A Ruby syntax checker using the standard (MRI) Ruby interpreter.

See URL `http://www.ruby-lang.org/'."
  :command ("ruby" "-w" "-c" source)
  :error-patterns
  ;; These patterns support output from JRuby, too, to deal with RVM or Rbenv
  ((error line-start
          "SyntaxError in " (file-name) ":" line ": " (message)
          line-end)
   (warning line-start
            (file-name) ":" line ":" (optional column ":")
            " warning: " (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes 'ruby-mode)

(flycheck-define-checker ruby-jruby
  "A Ruby syntax checker using the JRuby interpreter.

See URL `http://jruby.org/'."
  :command ("jruby" "-w" "-c" source)
  :error-patterns
  ((error line-start
          "SyntaxError in " (file-name) ":" line ": " (message)
          line-end)
   (warning line-start (file-name) ":" line " warning: " (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes ruby-mode)

(flycheck-define-checker rust
  "A Rust syntax checker using Rust compiler.

See URL `http://rust-lang.org'."
  :command ("rustc" "--parse-only" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": "
          (one-or-more digit) ":" (one-or-more digit) " error: "
          (message) line-end))
  :modes rust-mode)

(flycheck-define-checker sass
  "A Sass syntax checker using the Sass compiler.

See URL `http://sass-lang.com'."
  :command ("sass" "-c" source)
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message))
   (warning line-start "WARNING on line " line " of " (file-name)
            ":" (optional "\r") "\n" (message) line-end)
   (error line-start
          "Syntax error: " (message)
          (optional "\r") "\n        on line " line " of " (file-name)
          line-end))
  :modes sass-mode)

(flycheck-define-checker scala
  "A Scala syntax checker using the Scala compiler.

See URL `http://www.scala-lang.org/'."
  :command ("scalac" "-Ystop-after:parser" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes scala-mode)

(flycheck-define-checker scss
  "A SCSS syntax checker using the SCSS compiler.

See URL `http://sass-lang.com'."
  :command ("scss" "-c" source)
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message))
   (warning line-start "WARNING on line " line " of " (file-name)
            ":" (optional "\r") "\n" (message) line-end)
   (error line-start
          "Syntax error: " (message)
          (optional "\r") "\n        on line " line " of " (file-name)
          line-end))
  :modes scss-mode)

(flycheck-define-checker sh-dash
  "A POSIX Shell syntax checker using the Dash shell.

See URL `http://gondor.apana.org.au/~herbert/dash/'."
  :command ("dash" "-n" source)
  :error-patterns
  ((error line-start (file-name) ": " line ": " (backref 1) ": " (message)))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh)))

(flycheck-define-checker sh-bash
  "A POSIX Shell syntax checker using the Bash shell.

See URL `http://www.gnu.org/software/bash/'."
  :command ("bash" "--posix" "--norc" "-n" "--" source)
  :error-patterns
  ((error line-start
          (file-name) ":" (one-or-more (not (any digit)))
          line (zero-or-more " ") ":" (zero-or-more " ")
          (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh)))

(flycheck-def-config-file-var flycheck-chktexrc tex-chktex ".chktexrc"
  :safe #'stringp)

(flycheck-define-checker tex-chktex
  "A TeX and LaTeX syntax and style checker using chktex.

See URL `http://baruch.ev-en.org/proj/chktex/'."
  :command ("chktex" (config-file "-l" flycheck-chktexrc) "-v0" "-q" "-I"
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes (latex-mode plain-tex-mode))

(flycheck-define-checker tex-lacheck
  "A LaTeX syntax and style checker using lacheck.

See URL `http://www.ctan.org/pkg/lacheck'."
  :command ("lacheck" source-inplace)
  :error-patterns
  ((warning line-start
            "\"" (file-name) "\", line " line ": " (message)
            line-end))
  :modes latex-mode)

(flycheck-define-checker xml-xmlstarlet
  "A XML syntax checker and validator using the xmlstarlet utility.

See URL `http://xmlstar.sourceforge.net/'."
  :command ("xmlstarlet" "val" "-e" "-q" source)
  :error-patterns
  ((error line-start (file-name) ":" line "." column ": " (message) line-end))
  :modes (xml-mode nxml-mode))

(flycheck-define-checker zsh
  "A Zsh syntax checker using the Zsh shell.

See URL `http://www.zsh.org/'."
  :command ("zsh" "-n" "-d" "-f" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'zsh)))

(provide 'flycheck)

;; Local Variables:
;; coding: utf-8
;; End:

;;; flycheck.el ends here
