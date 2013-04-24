;;; flycheck.el --- On-the-fly syntax checking (Flymake done right) -*- lexical-binding: t; -*-

;; Copyright (c) 2012, 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck
;; Keywords: convenience languages tools
;; Version: 0.10
;; Package-Requires: ((s "1.3.1") (dash "1.2") (cl-lib "0.1") (emacs "24.1"))

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
;; Support for new modes and languages can be added by declaring a new syntax
;; checker (see `flycheck-declare-checker').

;;; Code:

(eval-when-compile
  ;; For JKA workarounds in `flycheck-temp-file-system'
  (require 'jka-compr)
  ;; For integration into Compile Mode
  (require 'compile))

(require 'cl-lib)
(require 's)
(require 'dash)


;;;; Compatibility
(eval-and-compile
  (unless (and (fboundp 'defvar-local)
               (eq (car (symbol-function 'defvar-local)) 'macro))
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      ;; Can't use backquote here, it's too early in the bootstrap.
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var))))))

(eval-and-compile
  (unless (fboundp 'user-error)
    ;; Provide `user-error' for Emacs 24.2
    (defalias 'user-error 'error)
    ;; And make the debugger ignore our Flycheck user errors in Emacs 24.2
    (add-to-list 'debug-ignored-errors "\\`No more Flycheck errors\\'")
    (add-to-list 'debug-ignored-errors "\\`Flycheck mode disabled\\'")
    (add-to-list 'debug-ignored-errors
                 "\\`Configured syntax checker .* cannot be used\\'")))


;;;; Customization
(defgroup flycheck nil
  "On-the-fly syntax checking (aka \"flymake done right\")."
  :prefix "flycheck-"
  :group 'tools)

(defgroup flycheck-config-files nil
  "Configuration files for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-options nil
  "Options for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-faces nil
  "Faces used by on-the-fly syntax checking."
  :prefix "flycheck-"
  :group 'flycheck)

(defcustom flycheck-checkers
  '(bash
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
    html-tidy
    javascript-jshint
    json-jsonlint
    lua
    perl
    php
    php-phpcs
    python-flake8
    python-pylint
    rst
    ruby-rubocop
    ruby
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

Syntax checkers in this list must be declared with
`flycheck-declare-checker'."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker")))

(defvar-local flycheck-checker nil
  "Syntax checker to use for the current buffer.

If unset or nil, automatically select a suitable syntax checker
from `flycheck-checkers' on every syntax check.

If set to a syntax checker only use this syntax checker and never
select one from `flycheck-checkers' automatically.  If the syntax
checker is unusable in the current buffer an error is signaled.

A syntax checker assigned to this variable must be declared with
`flycheck-declare-checker'.

Use the command `flycheck-select-checker' to select a syntax
checker for the current buffer, or set this variable as file
local variable to always use a specific syntax checker for a
file.")
(put 'flycheck-checker 'safe-local-variable 'flycheck-registered-checker-p)

(defface flycheck-error-face
  '((t :inherit error))
  "Face for on-the-fly syntax checking errors."
  :group 'flycheck-faces)

(defface flycheck-warning-face
  '((t :inherit warning))
  "Face for on-the-fly syntax checking warnings."
  :group 'flycheck-faces)

(defcustom flycheck-indication-mode 'left-fringe
  "The indication mode for Flycheck errors and warnings.

Controls how Flycheck indicates errors in buffers.  May either be
`left-fringe', `right-fringe', or nil.

If set to `left-fringe' or `right-fringe', indicate errors and
warnings via icons in the left and right fringe respectively.

If set to nil, errors and warnings are not indicated.  However,
they may still be highlighted according to
`flycheck-highlighting-mode'."
  :group 'flycheck
  :type '(choice (const :tag "Indicate in the left fringe" left-fringe)
                 (const :tag "Indicate in the right fringe" right-fringe)
                 (const :tag "Do not indicate" nil)))

(defcustom flycheck-highlighting-mode 'columns
  "The highlighting mode for Flycheck errors and warnings.

Controls how Flycheck highlights errors in buffers.  May either
be `columns', `lines' or nil.

If set to `columns' highlight specific columns if errors are
specific to a column.  If set to `lines' always highlight the
whole line regardless of whether the error is specific to a
column.  If nil do no highlight errors at all, but only show
fringe icons.

Note that this does not affect error navigation.  When navigating
errors with `next-error' and `previous-error' Flycheck always
jumps to the error column regardless of the highlighting mode."
  :group 'flycheck
  :type '(choice (const :tag "Highlight columns only" columns)
                 (const :tag "Highlight whole lines" lines)
                 (const :tag "Do not highlight errors" nil))
  :package-version '(flycheck . "0.6"))

(defcustom flycheck-check-syntax-automatically '(save new-line mode-enabled)
  "When Flycheck should check syntax automatically.

This variable is a list of events that may trigger syntax checks.
The following events are known:

`mode-enabled' checks syntax automatically when `flycheck-mode'
is enabled.

`save' checks syntax automatically each time the buffer is saved.

`new-line' checks syntax automatically each time a new line is
inserted into the buffer.

For instance, set this variable to '(mode-enabled save) to only
check syntax automatically when saving a buffer, but never when
modifying its contents.

If nil, never check syntax automatically.  Use `flycheck-buffer'
to start a syntax check manually."
  :group 'flycheck
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After `flycheck-mode' was enabled" mode-enabled))
  :package-version '(flycheck . "0.11"))

(defcustom flycheck-mode-hook nil
  "Hooks to run after `flycheck-mode'."
  :group 'flycheck
  :type 'hook)

(defcustom flycheck-after-syntax-check-hook nil
  "Hooks to run after each syntax check.

This hook is run after the syntax check process finished, all
error messages were parsed and properly reported (including
overlay setup)."
  :group 'flycheck
  :type 'hook)


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
    (define-key pmap (kbd "C-w") 'flycheck-copy-messages-as-kill)
    (define-key pmap "/" 'flycheck-google-messages)
    (define-key pmap "s" 'flycheck-select-checker)
    (define-key pmap "?" 'flycheck-describe-checker)
    (define-key pmap "i" 'flycheck-info)
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
    ;; Tear down Flycheck if the buffer or Emacs is kill
    (kill-buffer-hook                 . flycheck-teardown)
    (kill-emacs-hook                  . flycheck-teardown)
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
  (cond
   (flycheck-mode
    (flycheck-clear)

    (--each flycheck-hooks-alist
      (add-hook (car it) (cdr it) nil t))

    (setq flycheck-previous-next-error-function next-error-function)
    (setq next-error-function 'flycheck-next-error-function)

    (flycheck-buffer-automatically 'mode-enabled))
   (t
    (setq next-error-function flycheck-previous-next-error-function)

    (--each flycheck-hooks-alist
      (remove-hook (car it) (cdr it) t))

    (flycheck-teardown))))


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

A check has to be deferred if the buffer is not visible.

Return t if the check is to be deferred, or nil otherwise."
  (not (get-buffer-window)))

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

(defun flycheck-buffer-automatically (&optional condition)
  "Automatically check syntax at CONDITION.

Syntax is not checked if `flycheck-may-check-automatically'
returns nil for CONDITION.

The syntax check is deferred if `flycheck-must-defer-check'
returns t."
  (if (flycheck-may-check-automatically condition)
      (if (flycheck-must-defer-check)
          (flycheck-buffer-deferred)
        (with-demoted-errors
          (flycheck-buffer)))
    (message "Cannot perform a syntax check in buffer %s."
             (buffer-name))))

(defun flycheck-handle-change (beg end _len)
  "Handle a buffer change between BEG and END.

BEG and END mark the beginning and end of the change text.  _LEN
is ignored.

Start a syntax check if a new line has been inserted into the
buffer."
  (when (and flycheck-mode
             (s-contains? "\n" (buffer-substring beg end)))
    (flycheck-buffer-automatically 'new-line)))

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
(defun flycheck-string-to-number-safe (string)
  "Safely convert STRING to a number.

If STRING is of string type, and a numeric string (see
`s-numeric?'), convert STRING to a number and return it.
Otherwise return nil."
  (when (and (stringp string) (s-numeric? string))
    (string-to-number string)))

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
    ;; With no filename, fall back to a copy in the system directory.
    (flycheck-temp-file-system filename prefix)))

(defun flycheck-root-directory-p (directory)
  "Determine if DIRECTORY is the root directory.

In order to work in work in a platform-neutral way,
check to see if DIRECTORY is its own parent.

Return t if root directory, otherwise nil."
  (string= (directory-file-name directory)
           (file-name-directory (directory-file-name directory))))

(defun flycheck-find-file-in-tree (filename directory)
  "Find FILENAME in DIRECTORY and all of its ancestors.

Start looking for a file named FILENAME in DIRECTORY and traverse
upwards through all of its ancestors up to the file system root
until the file is found or the root is reached.

Return the absolute path of the file, or nil if the file was not
found in DIRECTORY or any of its ancestors."
  (let ((full-path (expand-file-name filename directory)))
    (cond
     ((flycheck-root-directory-p directory)
      (when (file-exists-p full-path) full-path))
     ((file-exists-p full-path) full-path)
     (:else
      (let ((parent-directory (file-name-directory
                               (directory-file-name
                                (file-name-directory full-path)))))
        (flycheck-find-file-in-tree filename parent-directory))))))

(defun flycheck-find-file-for-buffer (filename)
  "Find FILENAME for the current buffer.

First try to find the file in the buffer's directory and any of
its ancestors (see `flycheck-find-file-in-tree').  If that fails
or if the buffer has no `buffer-file-name' try to find the file
in the home directory.  If the file is not found anywhere return
nil."
  (let ((directory (when (buffer-file-name)
                     (file-name-directory (buffer-file-name)))))
    (or (when directory (flycheck-find-file-in-tree filename directory))
        (let ((home-path (expand-file-name filename "~")))
          (when (file-exists-p home-path) home-path)))))

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


;;;; Checker declarations
;;;###autoload
(defmacro flycheck-declare-checker (symbol docstring &rest properties)
  "Declare SYMBOL as syntax checker with DOCSTRING and PROPERTIES.

DOCSTRING provides documentation for the new checker.  Use
`flycheck-checker-documentation' to access the documentation
string of a checker, and `flycheck-describe-checker' to get help
about a checker.

The following PROPERTIES are understood:

`:command' A list with the executable (in `car') and the
arguments (in `cdr') of the syntax checker.  The executable is
checked for existence with `executable-find' before executing the
checker.  The arguments are substituted with
`flycheck-substitute-argument' before execution, see the
documentation of this function for a list of special tags allowed
in arguments.

`:error-patterns' A list of error patterns to parse the output of
the checker.  Each pattern is a list (REGEXP LEVEL).  REGEXP is a
regular expression that matches an error.  This regular
expression may contain match groups extracting specific
information about the error.  The 1st group is the file name, the
2nd group the line number, the 3rd group the column number and
the 4th group the error message.  A group is ignored if it did
not match or the match returned an empty string.  LEVEL is either
warning or error and determines the severity of the error message
parsed with the pattern.

`:error-parser' A function symbol to parse errors with.  The
function must accept three arguments OUTPUT CHECKER BUFFER, where
OUTPUT is the output as string and CHECKER the checker symbol
that was used to check BUFFER.  The function must return a list
of `flycheck-error' objects parsed from OUTPUT.

`:modes' A major mode symbol or a list thereof.  If present the
checker is only used in these modes.

`:predicate' An Emacs Lisp form.  If present the checker is only
used if the form evaluates to a non-nil result in the buffer to
check.

`:next-checkers' A list where each element is either a checker
symbol to run after this checker or a cons cell (PREDICATE
. CHECKER).  In the latter case, CHECKER is the checker symbol to
run, and the PREDICATE symbol specifies when to run the checker:
If PREDICATE is `no-errors' run the next checker only if this
checker returned no errors at all.  If PREDICATE is
`warnings-only', run the next checker only if this checker
returned only warnings.  Only the first usable and
registered (see `flycheck-registered-checker-p') is run.

A checker must have a `:command' property, either
`:error-patterns' or `:error-parser' (but not both), and at least
one of `:predicate' and `:modes'.  If `:predicate' and `:modes'
are present, both must match for the checker to be used."
  (declare (indent 1)
           (doc-string 2))
  `(flycheck--declare-checker-1 (quote ,symbol) ,docstring ,@properties)
  )

(defun flycheck-undeclare-checker (symbol)
  "Un-declare the syntax checker denoted by SYMBOL."
  (--each
      '(:flycheck-checker :flycheck-command :flycheck-error-parser
                          :flycheck-error-patterns :flycheck-modes
                          :flycheck-predicate :flycheck-next-checkers
                          :flycheck-documentation :flycheck-file)
    (put symbol it nil)))

(defun flycheck--declare-checker-1 (symbol docstring &rest properties)
  "Declare SYMBOL as checker with DOCSTRING and PROPERTIES."
  ;; Un-declare any previous checker for this mode
  (flycheck-undeclare-checker symbol)
  ;; Store the checker properties
  (put symbol :flycheck-command (plist-get properties :command))
  (put symbol :flycheck-error-patterns (plist-get properties :error-patterns))
  (put symbol :flycheck-error-parser (plist-get properties :error-parser))
  (put symbol :flycheck-modes (plist-get properties :modes))
  (put symbol :flycheck-predicate (plist-get properties :predicate))
  (put symbol :flycheck-next-checkers (plist-get properties :next-checkers))
  (put symbol :flycheck-documentation docstring)
  ;; Record the location of the definition of the checker.  If we're loading
  ;; from a file, record the file loaded from.  Otherwise use the current
  ;; buffer name, in case of `eval-buffer' and the like.
  (-when-let (filename (if load-in-progress load-file-name (buffer-file-name)))
    (when (s-ends-with? ".elc" filename)
      (setq filename (s-chop-suffix "c" filename)))
    (put symbol :flycheck-file filename))
  ;; Verify the checker and declare it valid if succeeded
  (flycheck-verify-checker symbol)
  (put symbol :flycheck-checker t))


;;;###autoload
(defmacro flycheck-def-config-file-var (symbol checker &optional file-name)
  "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable (see `defcustom`)
providing a configuration file for CHECKER.  The CHECKER argument
is used for documentation purposes only.  If given use FILE-NAME
as initial value.

Use this together with the `config-file' cell in syntax checker
commands."
  (declare (indent 3))
  `(progn
     (put (quote ,checker) :flycheck-config-file-var (quote ,symbol))
     (defcustom ,symbol ,file-name
       ,(format "Configuration file for `%s'.

When set to a plain file name without any slash search for this
file name in the directory of the buffer being check, any
ancestors thereof or the home directory.  If buffer being checked
has no backing file, search in the home directory only.  If the
file is found pass it to the checker as configuration file.
Otherwise invoke the checker without a configuration file.

When set to a file path containing a slash expand the file name
with `expand-file-named' and pass this file to checker, if it
exists.  Otherwise invoke the checker without a configuration
file.

Use this variable as file-local variable if you need a specific
configuration file a buffer." checker)
       :type '(choice (const :tag "No configuration file" nil)
                      (string :tag "File name or path"))
       :group 'flycheck-config-files)
     (put (quote ,symbol) 'safe-local-variable #'stringp)
     (make-variable-buffer-local (quote ,symbol))))

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
     (let ((options (flycheck-checker-option-vars (quote ,checker))))
       (put (quote ,checker) :flycheck-option-vars
            (-uniq (cons (quote ,symbol) options))))
     (defcustom ,symbol ,init-value
       ,(format "%s

This variable is an option for the syntax checker `%s'." docstring checker)
       :group 'flycheck-options
       ,@custom-args)
     (make-variable-buffer-local (quote ,symbol))))

(defun flycheck-error-pattern-p (pattern)
  "Check whether PATTERN is a valid error pattern."
  (and
   (listp pattern)                      ; A pattern must be a list...
   (= (length pattern) 2)               ; ...of length 2...
   (stringp (car pattern))              ; ...whose 1st element is a string
   (memq (cadr pattern) '(warning error)) ; ...and whose 2nd element a category
   ))

(defun flycheck-error-patterns-list-p (patterns)
  "Check whether PATTERNS is a list of valid error patterns."
  (-all? 'flycheck-error-pattern-p patterns))

(defun flycheck-command-argument-cell-p (cell)
  "Determine whether CELL is a valid argument cell."
  (pcase cell
    (`(config-file ,option-name ,config-file-var)
     (and (stringp option-name)
          (symbolp config-file-var)))
    (`(option ,option-name ,option-var)
     (and (stringp option-name)
          (symbolp option-var)))
    (`(option ,option-name ,option-var ,filter)
     (and (stringp option-name)
          (symbolp option-var)
          (symbolp filter)))
    (`(eval ,_) t)
    (_ nil)))

(defun flycheck-command-argument-p (arg)
  "Check whether ARG is a valid command argument."
  (or
   (memq arg '(source source-inplace source-original temporary-directory))
   (stringp arg)
   (and (listp arg) (flycheck-command-argument-cell-p arg))))

(defun flycheck-command-arguments-list-p (arguments)
  "Check whether ARGUMENTS is a list of valid arguments."
  (-all? 'flycheck-command-argument-p arguments))

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
    (unless (or patterns parser)
      (error "Checker %s must have an :error-parser or :error-patterns" checker))
    (when (and patterns parser)
      (error "Checker %s must not have :error-parser and :error-patterns"
             checker))
    (unless (or (null patterns) (flycheck-error-patterns-list-p patterns))
      (error "Checker %s has invalid :error-patterns" checker))
    (unless (or (null parser) (fboundp parser))
      (error "Function definition of error parser %s of checker %s is void"
             parser checker))
    (unless (or modes predicate)
      (error "Checker %s must have :modes or :predicate" checker))
    (unless (or
             (null next-checkers)
             (and (listp next-checkers)
                  (--all? (or (symbolp it)
                              (and (listp it)
                                   (memq (car it) '(no-errors warnings-only))
                                   (symbolp (cdr it))))
                          next-checkers)))
      (error "Checker %s has invalid next checkers" checker))))


;;;; Checker API
(defun flycheck-declared-checkers ()
  "Find all declared syntax checkers.

The returned list is sorted alphapetically by the symbol name of
the syntax checkers."
  (let (declared-checkers)
    (mapatoms (lambda (symbol)
                (when (flycheck-valid-checker-p symbol)
                  (push symbol declared-checkers))))
    (sort declared-checkers #'string<)))

(defun flycheck-registered-checker-p (checker)
  "Determine whether CHECKER is registered.

A checker is registered if it is contained in `flycheck-checkers'."
  (memq checker flycheck-checkers))

(defun flycheck-valid-checker-p (checker)
  "Check whether a CHECKER is valid.

A valid checker is a symbol declared as checker with
`flycheck-declare-checker'."
  (get checker :flycheck-checker))

(defun flycheck-checker-modes (checker)
  "Get the modes of CHECKER."
  (let ((modes (get checker :flycheck-modes)))
    (if (and modes (symbolp modes))
        (list modes)
      modes)))

(defun flycheck-checker-predicate (checker)
  "Get the predicate of CHECKER."
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

(defun flycheck-checker-executable (checker)
  "Get the executable of CHECKER.

The executable is the `car' of the checker command as returned by
`flycheck-checker-command'."
  (car (flycheck-checker-command checker)))

(defun flycheck-checker-error-patterns (checker)
  "Get the error patterns of CHECKER."
  (get checker :flycheck-error-patterns))

(defun flycheck-checker-error-parser (checker)
  "Get the error parser of CHECKER."
  (get checker :flycheck-error-parser))

(defun flycheck-checker-pattern-to-error-regexp (pattern)
  "Convert PATTERN into an error regexp for compile.el.

Return a list representing PATTERN, suitable as element in
`compilation-error-regexp-alist'."
  (let* ((regexp (car pattern))
         (level (cadr pattern))
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

(defun flycheck-check-modes (checker)
  "Check the allowed modes of CHECKER.

Check the current `major-mode' against the modes allowed for
CHECKER.  Return t if the modes match or nil otherwise."
  (let ((modes (flycheck-checker-modes checker)))
    (or (not modes) (memq major-mode modes))))

(defun flycheck-check-predicate (checker)
  "Check the predicate of CHECKER.

Check the predicate of CHECKER, and return t if the checker has
no predicate or the result of the predicate evaluation."
  (let ((predicate (flycheck-checker-predicate checker)))
    (or (not predicate) (eval predicate))))

(defun flycheck-check-executable (checker)
  "Check the executable of the CHECKER."
  (when (executable-find (flycheck-checker-executable checker)) t))

(defun flycheck-may-use-checker (checker)
  "Determine whether a CHECKER may be used.

Return t if CHECKER may be used for the current buffer and nil
otherwise."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is no declared flycheck syntax checker (see `flycheck-declare-checker')"
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

(defun flycheck-find-config-file (file-name)
  "Find the configuration file FILE-NAME.

If FILE-NAME contains a slash, return FILE-NAME expanded with
`expand-file-name'.

If FILE-NAME does not contain a slash, search the file with
`flycheck-find-file-name' and return the result."
  (when file-name
    (if (s-contains? "/" file-name)
        (let ((file-name (expand-file-name file-name)))
          (when (file-exists-p file-name) file-name))
      (flycheck-find-file-for-buffer file-name))))

(defun flycheck-substitute-argument-cell (cell)
  "Substitute an argument CELL.

Generally, a CELL is a form `(SYMBOL ARGS...) where SYMBOL is a special tag,
and ARGS the arguments for this tag.

If CELL is a form `(config-file OPTION VARIABLE)' search the
configuration file bound to VARIABLE with
`flycheck-find-config-file' and return a list of arguments that
pass this configuration file to the syntax checker, or nil if the
configuration file was not found.  If OPTION ends with a =
character, the returned list contains a single element only,
being the concatenation of OPTION and the path of the
configuration file.  Otherwise the list has two items, the first
being OPTION, the second the path of the configuration file.

If CELL is a form `(option OPTION VARIABLE [FILTER])' retrieve
the value of VARIABLE and return a list of arguments that pass
this value as value for OPTION to the syntax checker.  FILTER is
an optional function to be applied to the value of VARIABLE.
This function must return nil or a string.  In the former case,
return nil.  In the latter case, return a list of arguments as
described above.  If OPTION ends with a =, process it like in a
`config-file' cell (see above).

If CELL is a form `(eval FORM), return the result of evaluating
FORM in the buffer to be checked.  FORM must either return a
string or a list of strings, or nil to indicate that nothing
should be substituted for CELL.  In case of other return values
an error is signaled.  _No_ further substitutions are performed,
neither in FORM before it is evaluated, nor in the result of
evaluating FORM.

In all other cases, signal an error."
  (pcase cell
    (`(config-file ,option-name ,file-name-var)
     (-when-let* ((value (symbol-value file-name-var))
                  (file-name (flycheck-find-config-file value)))
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
    (`(eval ,form)
     (let ((result (eval form)))
       (if (or (null result)
               (stringp result)
               (and (listp result) (-all? #'stringp result)))
           result
         (error "Invalid result from evaluation of %S: %S" form result))))
    (_ (error "Unsupported argument cell %S" cell))))

(defun flycheck-substitute-argument-symbol (symbol)
  "Substitute an argument SYMBOL.

If SYMBOL is `source' or `source-inplace', create a temporary
file to check and return its path.  With `source', try to retain
the non-directory component of the buffer's file name in the
temporary file.

If SYMBOL is `source-original', return the path of the actual file
to check, or an empty string if the buffer has no file name.
Note that the contents of the file may not be up to date with the
contents of the buffer to check.  Do not use this as primary
input to a checker!

If SYMBOL is `temporary-directory', create a unique temporary
directory and return its path.

In all other cases, signal an error."
  (cl-case symbol
    (source
     (let ((filename (flycheck-temp-file-system (buffer-file-name) "flycheck")))
       (flycheck-save-buffer-to-file filename)
       filename))
    (source-inplace
     (let ((filename (flycheck-temp-file-inplace (buffer-file-name) "flycheck")))
       (flycheck-save-buffer-to-file filename)
       filename))
    (source-original
     (or (buffer-file-name) ""))
    (temporary-directory
     (flycheck-temp-dir-system "flycheck"))
    (t
     (error "Unsupported argument symbol %S" symbol))))

(defun flycheck-substitute-argument (arg)
  "Substitute ARG with file to check is possible.

If ARG is a string, return ARG unchanged.

If ARG is a symbol, substitute it with
`flycheck-substitute-argument-symbol'.

If ARG is a list, substitute it with
`flycheck-substitute-argument-cell'.

In all other cases, signal an error."
  (cond
   ((stringp arg) arg)
   ((symbolp arg) (flycheck-substitute-argument-symbol arg))
   ((listp arg) (flycheck-substitute-argument-cell arg))
   (:else (error "Unsupported argument %S" arg))))

(defun flycheck-checker-substituted-command (checker)
  "Get the substituted command of a CHECKER.

Substitute each argument in the command of CHECKER using
`flycheck-substitute-argument'.  This replaces any special
symbols in the command."
  (-flatten (-keep #'flycheck-substitute-argument
                   (flycheck-checker-command checker))))

(defun flycheck-substitute-shell-argument-symbol (symbol)
  "Substitute a shell argument SYMBOL.

If SYMBOL is `source', `source-inplace' or `source-original',
return the buffer file name quoted with `shell-quote-argument'.

Otherwise signal an error."
  (if (memq symbol '(source source-inplace source-original))
      (shell-quote-argument (buffer-file-name))
    (error "Unsupported argument symbol %S" symbol)))

(defun flycheck-substitute-shell-argument-cell (cell)
  "Substitute a shell argument CELL.

Like `flycheck-substitute-argument-cell', but return a single
string suitable for a shell command, i.e. quoted as necessary
with `shell-quote-argument'."
  (let ((args (flycheck-substitute-argument-cell cell)))
    (if (stringp args)
        (shell-quote-argument args)
      (s-join " " (-map #'shell-quote-argument args)))))

(defun flycheck-substitute-shell-argument (arg)
  "Substitute ARG with file to check is possible.

If ARG is a string, return ARG quoted with
`shell-quote-argument'.

If ARG is a symbol, substitute it with
`flycheck-substitute-shell-argument-symbol'.

If ARG is a list, substitute it with
`flycheck-substitute-shell-argument-cell'.

In all other cases, signal an error."
  (cond
   ((stringp arg) (shell-quote-argument arg))
   ((symbolp arg) (flycheck-substitute-shell-argument-symbol arg))
   ((listp arg) (flycheck-substitute-shell-argument-cell arg))
   (:else (error "Unsupported argument %S" arg))))

(defun flycheck-checker-shell-command (checker)
  "Get a shell command for CHECKER.

Substitutions are performed like in
`flycheck-checker-substituted-command', but with
`flycheck-substitute-shell-argument'.

Return the command of CHECKER as single string, suitable for
shell execution."
  (s-join " " (-map #'flycheck-substitute-shell-argument
                    (flycheck-checker-command checker))))


;;;; Option filters
(defun flycheck-option-int (value)
  "Convert an integral option VALUE to a string.

If VALUE is nil, return nil.  Otherwise return VALUE converted to
a string."
  (when value
    (number-to-string value)))


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
      ;; If a checker was configured, try to use it!
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

;; Define our custom help button to navigation to checker definitions.  Be
;; auto-load friendly, and delay the definition until after 'help-mode was
;; loaded.
(eval-after-load 'help-mode
  '(define-button-type 'help-flycheck-checker-def
     :supertype 'help-xref
     'help-function 'flycheck-goto-checker-definition
     'help-echo (purecopy "mouse-2, RET: find Flycheck checker definition")))

;; Plug Flycheck into find-func, to provide navigation to checker definitions.
;; Again we are friendly to autoload.
(eval-after-load 'find-func
  '(progn
     (defconst flycheck-find-checker-regexp
       (concat "^\\s-*(flycheck-declare-checker "
               find-function-space-re "%s\\(\\s-\\|$\\)")
       "Regular expression to find a checker definition.")
     (add-to-list 'find-function-regexp-alist
                  '(flycheck-checker . flycheck-find-checker-regexp))))

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
  ;; A checker is like a variable, but doesn't have to be bound...
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
              (predicate (flycheck-checker-predicate checker))
              (config-file-var (flycheck-checker-config-file-var checker))
              (option-vars (sort (flycheck-checker-option-vars checker)
                                 #'string<)))
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
          (cond
           ((and modes predicate)
            (princ (format "  It checks syntax in the major mode(s) %s if the predicate %s is fulfilled. "
                           (s-join ", " (--map (format "`%s'" it) modes))
                           predicate)))
           (modes
            (princ (format "  It checks syntax in the major mode(s) %s. "
                           (s-join ", " (--map (format "`%s'" it) modes)))))
           (predicate
            (princ (format "  It checks syntax if the predicate %s is fulfilled. "
                           (prin1-to-string predicate)))))
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
               (:constructor flycheck-error-new))
  buffer filename line column message level)

(defmacro flycheck-error-with-buffer (err &rest forms)
  "Switch to the buffer of ERR and evaluate FORMS.

If the buffer of ERR is not live, FORMS are not evaluated."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (flycheck-error-buffer ,err))
     (with-current-buffer (flycheck-error-buffer ,err)
       ,@forms)))

(defun flycheck-error-region (err &optional ignore-column)
  "Get the region of ERR.

ERR is a flycheck error whose region to get.  If IGNORE-COLUMN is
given and t ignore the column number of ERR when determining the
region.  Hence the region will always extend over the whole line.

Return a cons cell (BEG . END).  BEG is the beginning of the
error region and END its end.  If ERR has a column number and
IGNORE-COLUMN is omitted or nil BEG and END mark a region that
marks that column only.  Otherwise BEG is the position of the
first non-whitespace character on the ERR line and END its end."
  (flycheck-error-with-buffer err
    (save-excursion
      (save-restriction
        ;; Error regions are absolute in relation to the buffer, so remove
        ;; point restrictions temporarily while determining the error region
        (widen)
        (goto-char (point-min))
        (forward-line (- (flycheck-error-line err) 1))
        (back-to-indentation)
        (let* ((col (if ignore-column nil (flycheck-error-column err)))
               (beg (point))
               (end (line-end-position)))
          (cond
           ((= beg end)
            (forward-line -1)
            (setq beg (line-end-position)))
           (col
            (setq end (min (+ (line-beginning-position) col)
                           (+ (line-end-position) 1)))
            (setq beg (- end 1))))
          (cons beg end))))))

(defun flycheck-error-pos (err)
  "Get the buffer position of ERR.

If ERR has a column return exactly that column.  Otherwise return
the beginning of the line of ERR."
  (car (flycheck-error-region err)))


;;;; General error parsing
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let* ((parser (or (flycheck-checker-error-parser checker)
                     'flycheck-parse-output-with-patterns))
         (errors (funcall parser output checker buffer)))
    ;; Attach originating buffer to each error
    (--each errors (setf (flycheck-error-buffer it) buffer))
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
        ;; If the error has a file name, expand it relative to the default
        ;; directory of its buffer and back substitute the file name
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
  (s-join "\\|" (--map (format "\\(?:%s\\)" (car it)) patterns)))

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
         (level (cadr pattern))
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

(defun flycheck-parse-output-with-patterns (output checker _buffer)
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

(defun flycheck-parse-checkstyle-error-node (node filename)
  "Parse a single error NODE for FILENAME in a Checkstyle doc.

Return the corresponding Flycheck error, or nil of NODE is not an
error node."
  (when (listp node)                    ; Ignore text nodes
    (let* ((name (car node))
           (attrs (cadr node))
           (line (flycheck-string-to-number-safe (cdr (assq 'line attrs))))
           (column (flycheck-string-to-number-safe (cdr (assq 'column attrs))))
           (severity (cdr (assq 'severity attrs)))
           (message (cdr (assq 'message attrs))))
      (when (eq name 'error)
        (flycheck-error-new
         :filename filename
         :line line
         :column (when (and column (> column 0)) column)
         :message message
         :level (if (string= severity "error") 'error 'warning))))))

(defun flycheck-parse-checkstyle-file-node (node)
  "Parse a single file NODE in a Checkstyle document.

Return a list of all errors contained in the NODE, or nil if NODE
is not a file node."
  (when (listp node)                    ; Ignore text nodes
    (let* ((name (car node))
           (attrs (cadr node))
           (body (cddr node))
           (filename (cdr (assq 'name attrs))))
      (when (eq name 'file)
        (--keep (flycheck-parse-checkstyle-error-node it filename) body)))))

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
    (-flatten (-keep #'flycheck-parse-checkstyle-file-node (cddr root)))))


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

(defconst flycheck-error-overlay nil
  "Overlay category for flycheck errors.")
(put 'flycheck-error-overlay 'flycheck-overlay t)
(put 'flycheck-error-overlay 'face 'flycheck-error-face)
(put 'flycheck-error-overlay 'priority 100)
(put 'flycheck-error-overlay 'help-echo "Unknown error.")
(put 'flycheck-error-overlay 'flycheck-fringe-bitmap
     flycheck-fringe-exclamation-mark)

(defconst flycheck-warning-overlay nil
  "Overlay category for flycheck warning.")
(put 'flycheck-warning-overlay 'flycheck-overlay t)
(put 'flycheck-warning-overlay 'face 'flycheck-warning-face)
(put 'flycheck-warning-overlay 'priority 100)
(put 'flycheck-warning-overlay 'help-echo "Unknown warning.")
(put 'flycheck-warning-overlay 'flycheck-fringe-bitmap 'question-mark)

(defun flycheck-create-overlay (err)
  "Get or create the overlay for ERR."
  (flycheck-error-with-buffer err
    (let* ((mode flycheck-highlighting-mode)
           (region (flycheck-error-region err (not (eq mode 'columns))))
           (overlay (make-overlay (car region) (cdr region))))
      (overlay-put overlay 'flycheck-error err)
      overlay)))

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
          (face (get category 'face)))
      (propertize "!" 'display (list flycheck-indication-mode bitmap face)))))

(defun flycheck-add-overlay (err)
  "Add overlay for ERR."
  (let* ((overlay (flycheck-create-overlay err))
         (level (flycheck-error-level err))
         (category (cl-case level
                     (warning 'flycheck-warning-overlay)
                     (error 'flycheck-error-overlay)
                     (t (error "Invalid error level %S" level)))))
    ;; TODO: Consider hooks to re-check if overlay contents change
    (overlay-put overlay 'category category)
    (unless flycheck-highlighting-mode
      ;; Erase the highlighting from the overlay if requested by the user
      (overlay-put overlay 'face nil))
    (overlay-put overlay 'flycheck-error err)
    (-when-let (icon (flycheck-make-fringe-icon category))
      (overlay-put overlay 'before-string icon))
    (overlay-put overlay 'help-echo (flycheck-error-message err))))

(defun flycheck-add-overlays (errors)
  "Add overlays for ERRORS."
  (mapc #'flycheck-add-overlay errors))

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

(defun flycheck-overlay-messages-at (pos)
  "Return a list of all flycheck messages overlayed at POS."
  (--map (overlay-get it 'help-echo) (flycheck-overlays-at pos)))

(defun flycheck-overlay-messages-string-at (pos)
  "Return a single string containing all error messages at POS."
  (s-join "\n\n" (flycheck-overlay-messages-at pos)))

(defvar-local flycheck-overlays-to-delete nil
  "Overlays mark for deletion after all syntax checks completed.")

(defun flycheck-delete-all-overlays ()
  "Remove all flycheck overlays in the current buffer."
  (flycheck-delete-marked-overlays)
  (save-restriction
    (widen)
    (-each (flycheck-overlays-in (point-min) (point-max)) #'delete-overlay)))

(defun flycheck-mark-all-overlays-for-deletion ()
  "Mark all current overlays for deletion."
  (setq flycheck-overlays-to-delete
        (flycheck-overlays-in (point-min) (point-max))))

(defun flycheck-delete-marked-overlays ()
  "Delete all overlays marked for deletion."
  (-each flycheck-overlays-to-delete #'delete-overlay)
  (setq flycheck-overlays-to-delete nil))


;;;; Error navigation
(defun flycheck-navigatable-position-p (pos)
  "Determine whether POS can be navigated to."
  (and (>= pos (point-min))
       (<= pos (point-max))))

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


;;;; Error message echoing
(defconst flycheck-error-message-buffer "*Flycheck errors*"
  "The name of the buffer to show long error messages in.")

(defun flycheck-error-message-buffer ()
  "Get the buffer object to show long error messages in.

Get the buffer named by variable `flycheck-error-message-buffer',
or nil if the buffer does not exist."
  (get-buffer flycheck-error-message-buffer))

(defun flycheck-display-error-messages (error-messages)
  "Display Flycheck ERROR-MESSAGES."
  (when error-messages
    (display-message-or-buffer error-messages
                               flycheck-error-message-buffer)))

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
    (flycheck-display-error-messages
     (flycheck-overlay-messages-string-at (point)))))

(defun flycheck-show-error-at-point-soon ()
  "Show the first error message at point in minibuffer asap.

Show the error message at point in minibuffer after a short delay."
  (flycheck-cancel-error-show-error-timer)
  (when (flycheck-overlays-at (point))
    (setq flycheck-error-show-error-timer
          (run-at-time 0.9 nil 'flycheck-show-error-at-point))))

(defun flycheck-hide-error-buffer ()
  "Hide the Flycheck error buffer if necessary.

Hide the error buffer if there is no error under point."
  (-when-let* ((buffer (flycheck-error-message-buffer))
               (window (get-buffer-window buffer)))
    (unless (flycheck-overlays-at (point))
      (quit-window nil window))))

(defun flycheck-copy-messages-as-kill (pos)
  "Copy message under POS into kill ring."
  (interactive "d")
  (-when-let (error-messages (flycheck-overlay-messages-string-at pos))
    (kill-new error-messages)
    (flycheck-display-error-messages error-messages)))

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
  :package-version '(flycheck . "0.1"))

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
  (unless (require 'google-this nil :no-error)
    (user-error "Please install Google This from \
https://github.com/Bruce-Connor/emacs-google-this"))
  (let ((messages (flycheck-overlay-messages-at pos)))
    (when (and flycheck-google-max-messages
               (> (length messages) flycheck-google-max-messages))
      (user-error "More than %s messages at point"
                  flycheck-google-max-messages))
    (--each messages
      (google-string quote-flag it :no-confirm))))


;;;; Checker process management
(defvar-local flycheck-current-process nil
  "The current syntax checking process.")

(defun flycheck-running-p ()
  "Determine whether a syntax check is running."
  (when (and flycheck-current-process
             (memq (process-status flycheck-current-process) '(exit signal)))
    (flycheck-delete-process flycheck-current-process)
    (setq flycheck-current-process nil))
  (when flycheck-current-process t))

(defun flycheck-delete-process (process)
  "Delete PROCESS and clear it's resources."
  ;; Remove temporary files and directories created for this process
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
      (flycheck-add-overlays errors)
      (setq flycheck-current-errors (-> errors
                                      (append flycheck-current-errors nil)
                                      flycheck-sort-errors))
      (flycheck-report-error-count flycheck-current-errors)
      (when (and (/= exit-status 0) (not errors))
        ;; Report possibly flawed checker definition
        (message "Checker %S returned non-zero exit code %s, but no errors from \
output: %s\nChecker definition probably flawed."
                 checker exit-status output)
        (flycheck-report-status "?"))
      (let ((next-checker (flycheck-get-next-checker-for-buffer checker)))
        (if next-checker
            (flycheck-start-checker next-checker)
          ;; Delete overlays from the last syntax check
          (flycheck-delete-marked-overlays)
          (run-hooks 'flycheck-after-syntax-check-hook)
          ;; Update the error display
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
      ;; First, let's clean up all the garbage
      (flycheck-delete-process process)
      (setq flycheck-current-process nil)
      ;; Now, if the checked buffer is still live, parse and report the errors.
      (when (and (buffer-live-p buffer) flycheck-mode)
        (with-current-buffer buffer
          (condition-case err
              (flycheck-finish-syntax-check checker exit-status files output)
            (error
             (flycheck-report-error)
             (signal (car err) (cdr err)))))))))

(defun flycheck-start-checker (checker)
  "Start a syntax CHECKER."
  (condition-case err
      (let* ((command (flycheck-checker-substituted-command checker))
             (program (car command))
             (args (cdr command))
             (process-connection-type nil) ; Use pipes to receive checker output
             (process (apply 'start-file-process
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
        ;; Temporary files and directories are not attached to the process, so
        ;; let's reset the variables
        (setq flycheck-temp-files nil
              flycheck-temp-directories nil)
        (process-put process :flycheck-checker checker))
    (error
     (flycheck-report-error)
     ;; Remove all temporary files created for the process
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


;;;; Built-in checkers
(flycheck-declare-checker bash
  "A Bash syntax checker using the bash executable.

See URL `http://www.gnu.org/software/bash/'."
  :command '("bash" "--norc" "-n" "--" source)
  :error-patterns '(("^\\(?1:.+\\):[^0-9]+\\(?2:[0-9]+\\) *: *\\(?4:.*\\)$" error))
  :modes 'sh-mode
  :predicate '(eq sh-shell 'bash))

(flycheck-def-config-file-var flycheck-coffeelintrc coffee-coffeelint
                              ".coffeelint.json")

(flycheck-declare-checker coffee-coffeelint
  "A CoffeeScript syntax and style checker using coffeelint.

See URL `http://www.coffeelint.org/'."
  :command '("coffeelint" (config-file "--file" flycheck-coffeelintrc)
             "--csv" source)
  :error-patterns
  '(("SyntaxError: \\(?4:.*\\) on line \\(?2:[0-9]+\\)" error)
    ("\\(?1:.+\\),\\(?2:[0-9]+\\),error,\\(?4:.+\\)" error)
    ("\\(?1:.+\\),\\(?2:[0-9]+\\),warn,\\(?4:.+\\)" warning))
  :modes 'coffee-mode)

(flycheck-declare-checker css-csslint
  "A CSS syntax and style checker using csslint.

See URL `https://github.com/stubbornella/csslint'."
  :command '("csslint" "--format=compact" source)
  :error-patterns
  '(("^\\(?1:.*\\): line \\(?2:[0-9]+\\), col \\(?3:[0-9]+\\), Error - \\(?4:.+\\)$"
     error)
    ("^\\(?1:.*\\): line \\(?2:[0-9]+\\), col \\(?3:[0-9]+\\), Warning - \\(?4:.+\\)$"
     warning))
  :modes 'css-mode)

(flycheck-declare-checker elixir
  "An Elixir syntax checker using the Elixir interpreter."
  :command '("elixirc"
             "-o" temporary-directory   ; Move compiler output out of the way
             "--ignore-module-conflict" ; Prevent tedious module redefinition
                                        ; warning.
             source)
  :error-patterns
  '(("^\\*\\* (.*) \\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:redefining.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*obsolete\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*unused\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*shadowed.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*always matches.*\\)$" warning)
    ;; the following regexp cannot be consistently tested
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*deprecated.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*future reserved.*\\)$" warning))
  :modes 'elixir-mode)

(defconst flycheck-emacs-command
  `(,(concat invocation-directory invocation-name)
    "--no-site-file" "--no-site-lisp" "--batch" "--eval")
  "A command to execute an Emacs Lisp form in a background process.")

(defun flycheck-temp-compilation-buffer-p ()
  "Determine whether the current buffer is a temporary buffer.

Return t if the current buffer is a temporary buffer created
during byte-compilation or autoloads generation, or nil otherwise."
  ;; Detect temporary buffers of `byte-compile-file' or autoload buffers created
  ;; during package installation.  Checking these interferes with package
  ;; installation, see https://github.com/lunaryorn/flycheck/issues/45 and
  ;; https://github.com/bbatsov/prelude/issues/248
  (or (member (buffer-name) '(" *Compiler Input*" " *autoload-file*"))
      (s-ends-with? "-autoloads.el" (buffer-name))))

(defconst flycheck-emacs-lisp-check-form
  '(progn
     ;; Initialize packages to at least try to load dependencies
     (package-initialize)

     (setq byte-compiled-files nil)
     (defun byte-compile-dest-file (source)
       (let ((temp-file (make-temp-file (file-name-nondirectory source))))
         (add-to-list 'byte-compiled-files temp-file)
         temp-file))

     (setq byte-compile-dest-file-function 'byte-compile-dest-file)
     (mapc 'byte-compile-file command-line-args-left)
     (mapc 'delete-file byte-compiled-files)))

(flycheck-declare-checker emacs-lisp
  "An Emacs Lisp syntax checker.

This checker simply attempts to byte compile the contents of the
buffer using the currently running Emacs executable."
  :command `(,@flycheck-emacs-command
             ,(prin1-to-string flycheck-emacs-lisp-check-form)
             source-inplace)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\):Warning:\\(?4:.*\\(?:\n    .*\\)*\\)$"
     warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\):Error:\\(?4:.*\\(?:\n    .*\\)*\\)$"
     error))
  :modes '(emacs-lisp-mode lisp-interaction-mode)
  ;; Ensure that we only check buffers with a backing file.  For buffers without
  ;; a backing file we cannot guarantee that file names in error messages are
  ;; properly resolved, because `byte-compile-file' emits file names *relative
  ;; to the directory of the checked file* instead of the working directory.
  ;; Hence our backwards-substitution will fail, because the checker process has
  ;; a different base directory to resolve relative file names than the flycheck
  ;; code working on the buffer to check.
  :predicate '(and (buffer-file-name)
                   ;; Do not check buffers which should not be byte-compiled.
                   ;; The checker process will refuse to compile these anyway
                   (not (and (boundp 'no-byte-compile) no-byte-compile))
                   (not (flycheck-temp-compilation-buffer-p)))
  :next-checkers '(emacs-lisp-checkdoc))

(defconst flycheck-emacs-lisp-checkdoc-form
  '(progn
     (require 'checkdoc)

     (let ((filename (car command-line-args-left))
           (process-default-directory default-directory))
       (with-temp-buffer
         (insert-file-contents filename t)
         (setq buffer-file-name filename)
         (setq default-directory process-default-directory)
         (condition-case err
             (progn
               (checkdoc-current-buffer t)
               (with-current-buffer checkdoc-diagnostic-buffer
                 (princ (buffer-substring-no-properties (point-min)
                                                        (point-max)))
                 (kill-buffer)))
           (error
            (message "Ignoring error: %S" err)))))))

(flycheck-declare-checker emacs-lisp-checkdoc
  "An Emacs Lisp style checker using CheckDoc.

The checker runs `checkdoc-current-buffer'."
  :command `(,@flycheck-emacs-command
             ,(prin1-to-string flycheck-emacs-lisp-checkdoc-form)
             source)
  :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)" warning))
  :modes '(emacs-lisp-mode lisp-interaction-mode)
  :predicate
  '(and (not (flycheck-temp-compilation-buffer-p))
        (not (and (buffer-file-name)
                  (string= (file-name-nondirectory (buffer-file-name))
                           "Carton")))))

(flycheck-declare-checker erlang
  "An Erlang syntax checker using the Erlang interpreter."
  :command '("erlc" "-o" temporary-directory "-Wall" source)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): Warning:\\(?4:.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'erlang-mode)

(flycheck-declare-checker go-gofmt
  "A Go syntax and style checker using the gofmt utility.

See URL `http://golang.org/cmd/gofmt/'."
  :command '("gofmt" source)
  :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'go-mode
  :next-checkers '((no-errors . go-build) (no-errors . go-test)))

(flycheck-declare-checker go-build
  "A Go syntax and style checker using the go build command.

See URL `https://golang.org/cmd/go'.

This syntax checker may cause bogus warnings due to an upstream
bug in Go.

See URL `https://code.google.com/p/go/issues/detail?id=4851' for
more information."
  :command '("go" "build" "-o" "/dev/null")
  :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'go-mode
  :predicate '(and (not (s-ends-with? "_test.go" (buffer-file-name)))
                   (not (buffer-modified-p))))

(flycheck-declare-checker go-test
  "A Go syntax and style checker using the go test command.

See URL `https://golang.org/cmd/go'."
  ;; This command builds the test executable without running it
  ;; and leaves the executable in the current directory.
  ;; Unfortunately 'go test -c' does not have the '-o' option.
  :command '("go" "test" "-c")
  :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'go-mode
  :predicate '(and (s-ends-with? "_test.go" (buffer-file-name))
                   (not (buffer-modified-p))))

(flycheck-declare-checker haml
  "A Haml syntax checker using the Haml compiler.

See URL `http://haml.info'."
  :command '("haml" "-c" source)
  :error-patterns
  '(("^Syntax error on line \\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'haml-mode)

(flycheck-def-config-file-var flycheck-tidyrc html-tidy ".tidyrc")

(flycheck-declare-checker html-tidy
  "A HTML syntax and style checker using Tidy.

See URL `https://github.com/w3c/tidy-html5'."
  :command '("tidy" (config-file "-config" flycheck-tidyrc) "-e" "-q" source)
  :error-patterns
  '(("^line \\(?2:[0-9]+\\) column \\(?3:[0-9]+\\) - Error: \\(?4:.*\\)$" error)
    ("^line \\(?2:[0-9]+\\) column \\(?3:[0-9]+\\) - Warning: \\(?4:.*\\)$"
     warning))
  :modes '(html-mode nxhtml-mode))

(flycheck-def-config-file-var flycheck-jshintrc javascript-jshint ".jshintrc")

(flycheck-declare-checker javascript-jshint
  "A JavaScript syntax and style checker using jshint.

See URL `http://www.jshint.com'."
  :command '("jshint" "--checkstyle-reporter"
             (config-file "--config" flycheck-jshintrc)
             source)
  :error-parser 'flycheck-parse-checkstyle
  :modes '(js-mode js2-mode js3-mode))

(flycheck-declare-checker json-jsonlint
  "A JSON syntax and style checker using jsonlint.

See URL `https://github.com/zaach/jsonlint'."
  :command '("jsonlint" "-c" "-q" source)
  :error-patterns
  '(("^\\(?1:.+\\)\: line \\(?2:[0-9]+\\), col \\(?3:[0-9]+\\), \\(?4:.+\\)$"
     error))
  :predicate '(or
               (eq major-mode 'json-mode)
               (and buffer-file-name
                    (string= "json" (file-name-extension buffer-file-name)))))

(flycheck-declare-checker lua
  "A Lua syntax checker using the Lua compiler.

See URL `http://www.lua.org/'."
  :command '("luac" "-p" source)
  :error-patterns
  '(("^.*?: \\(?1:.*?\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'lua-mode)

(flycheck-declare-checker perl
  "A Perl syntax checker using the Perl interpreter.

See URL `http://www.perl.org'."
  :command '("perl" "-w" "-c" source)
  :error-patterns
  '(("^\\(?4:.*?\\) at \\(?1:.*?\\) line \\(?2:[0-9]+\\)\\.$" error)
    ("^\\(?4:.*?\\) at \\(?1:.*?\\) line \\(?2:[0-9]+\\), .*$" error))
  :modes '(perl-mode cperl-mode))

(flycheck-declare-checker php
  "A PHP syntax checker using the PHP command line.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command '("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
             "-d" "log_errors=0" source)
  :error-patterns
  '(("\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \\(?4:.*\\) in \\(?1:.*\\) on line \\(?2:[0-9]+\\)"
     error))
  :modes '(php-mode php+-mode)
  :next-checkers '((warnings-only . php-phpcs)))

(flycheck-def-option-var flycheck-phpcs-standard nil php-phpcs
  "The coding standard for PHP CodeSniffer.

When nil, use the default standard from the global PHP
CodeSniffer configuration.  When set to a string, pass the string
to PHP CodeSniffer which will interpret it as name as a standard,
or as path to a standard specification."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Standard name or file")))
(put 'flycheck-phpcs-standard 'safe-local-variable #'stringp)

(flycheck-declare-checker php-phpcs
  "A PHP syntax checker using PHP_CodeSniffer.

See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
  :command '("phpcs" "--report=emacs"
             (option "--standard=" flycheck-phpcs-standard)
             source)
  ;; Though phpcs supports Checkstyle output which we could feed to
  ;; `flycheck-parse-checkstyle', we are still using error patterns here,
  ;; because PHP has notoriously unstable output habits.  See URL
  ;; `https://github.com/lunaryorn/flycheck/issues/78' and URL
  ;; `https://github.com/lunaryorn/flycheck/issues/118'
  :error-patterns
  '(("\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error - \\(?4:.*\\)" error)
    ("\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning - \\(?4:.*\\)" warning))
  :modes '(php-mode php+-mode))

(flycheck-def-config-file-var flycheck-flake8rc python-flake8 ".flake8rc")

(flycheck-def-option-var flycheck-flake8-maximum-complexity nil python-flake8
  "The maximum McCabe complexity of methods.

If nil, do not check the complexity of methods.  If set to an
integer, report any complexity greater than the value of this
variable as warning.

If set to an integer, this variable overrules any similar setting
in the configuration file denoted by `flycheck-flake8rc'."
  :type '(choice (const :tag "Do not check McCabe complexity" nil)
                 (integer :tag "Maximum complexity")))
(put 'flycheck-flake8-maximum-complexity 'safe-local-variable #'integerp)

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
                 (integer :tag "Maximum line length in characters")))
(put 'flycheck-flake8-maximum-line-length 'safe-local-variable #'integerp)

(flycheck-declare-checker python-flake8
  "A Python syntax and style checker using the flake8 utility.

For best error reporting, use Flake8 2.0 or newer.

See URL `http://pypi.python.org/pypi/flake8'."
  :command '("flake8"
             (config-file "--config" flycheck-flake8rc)
             (option "--max-complexity"
                     flycheck-flake8-maximum-complexity
                     flycheck-option-int)
             (option "--max-line-length"
                     flycheck-flake8-maximum-line-length
                     flycheck-option-int)
             source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:E[0-9]+.*\\)$"
     error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:F[0-9]+.*\\)$"
     warning)                           ; Flake8 >= 2.0
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:W[0-9]+.*\\)$"
     warning)                           ; Flake8 < 2.0
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:C[0-9]+.*\\)$"
     warning)                           ; McCabe complexity in Flake8 > 2.0
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:N[0-9]+.*\\)$"
     warning)                           ; pep8-naming Flake8 plugin.
    ;; Syntax errors in Flake8 < 2.0, in Flake8 >= 2.0 syntax errors are caught
    ;; by the E.* pattern above
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'python-mode)

(flycheck-declare-checker python-pylint
  "A Python syntax and style checker using the pylint utility.

See URL `http://pypi.python.org/pypi/pylint'."
  :command '("epylint" source-inplace)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): Warning (W.*): \\(?4:.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): Error (E.*): \\(?4:.*\\)$" error)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\[F\\] \\(?4:.*\\)$" error))
  :modes 'python-mode)

(flycheck-declare-checker rst
  "A ReStructuredText (RST) syntax checker using Docutils.

See URL `http://docutils.sourceforge.net/'."
  :command '("rst2pseudoxml.py" "--report=2" "--exit-status=1" "--halt=5" source)
  :error-patterns
  '(("^\\(?1:.+\\):\\(?2:[0-9]+\\): (WARNING/2) \\(?4:.+\\)$" warning)
    ("^\\(?1:.+\\):\\(?2:[0-9]+\\): (ERROR/3) \\(?4:.+\\)$" error)
    ("^\\(?1:.+\\):\\(?2:[0-9]+\\): (SEVERE/4) \\(?4:.+\\)$" error))
  :modes 'rst-mode)

(flycheck-def-config-file-var flycheck-rubocoprc ruby-rubocop ".rubocop.yml")

(flycheck-declare-checker ruby-rubocop
  "A Ruby syntax checker using the RuboCop tool.

See URL `https://github.com/bbatsov/rubocop'."
  :command '("rubocop" "--emacs" "--silent"
             (config-file "--config" flycheck-rubocoprc)
             source)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): C: \\(?4:.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): W: \\(?4:.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): E: \\(?4:.*\\)$" error))
  :modes 'ruby-mode)

(flycheck-declare-checker ruby
  "A Ruby syntax checker using the Ruby interpreter."
  :command '("ruby" "-w" "-c" source)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): warning: \\(?4:.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'ruby-mode)

(flycheck-declare-checker rust
  "A Rust syntax checker using rustc parsing option.

See URL `http://rust-lang.org'."
  :command '("rustc" "--parse-only" source)
  :error-patterns '(("^\\(?1:.+\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): [0-9]+:[0-9]+ error: \\(?4:.+\\)$" error))
  :modes 'rust-mode)

(flycheck-declare-checker sass
  "A Sass syntax checker using the Sass compiler.

See URL `http://sass-lang.com'."
  :command '("sass" "-c" source)
  :error-patterns
  '(("^Syntax error on line \\(?2:[0-9]+\\): \\(?4:.*\\)$" error)
    ("^WARNING on line \\(?2:[0-9]+\\) of \\(?1:.*\\):\r?\n\\(?4:.*\\)$"
     warning)
    ("^Syntax error: \\(?4:.*\\)\r?\n        on line \\(?2:[0-9]+\\) of \\(?1:.*\\)$"
     error))
  :modes 'sass-mode)

(flycheck-declare-checker scala
  "A Scala syntax checker using the Scala compiler.

See URL `http://www.scala-lang.org/'."
  :command '("scalac" "-Ystop-after:parser" source)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): error: \\(?4:.*\\)$" error))
  :modes 'scala-mode)

(flycheck-declare-checker scss
  "A SCSS syntax checker using the SCSS compiler.

See URL `http://sass-lang.com'."
  :command '("scss" "-c" source)
  :error-patterns
  '(("^Syntax error on line \\(?2:[0-9]+\\): \\(?4:.*\\)$" error)
    ("^WARNING on line \\(?2:[0-9]+\\) of \\(?1:.*\\):\r?\n\\(?4:.*\\)$"
     warning)
    ("^Syntax error: \\(?4:.*\\)\r?\n        on line \\(?2:[0-9]+\\) of \\(?1:.*\\)$"
     error))
  :modes 'scss-mode)

(flycheck-declare-checker sh-dash
  "A POSIX Shell syntax checker using the Dash shell.

See URL `http://gondor.apana.org.au/~herbert/dash/'."
  :command '("dash" "-n" source)
  :error-patterns '(("^\\(?1:.+\\): \\(?2:[0-9]+\\): \\1: \\(?4:.*\\)$" error))
  :modes 'sh-mode
  :predicate '(eq sh-shell 'sh))

(flycheck-declare-checker sh-bash
  "A POSIX Shell syntax checker using the Bash shell.

See URL `http://www.gnu.org/software/bash/'."
  :command '("bash" "--posix" "--norc" "-n" "--" source)
  :error-patterns '(("^\\(?1:.+\\):[^0-9]+\\(?2:[0-9]+\\) *: *\\(?4:.*\\)$" error))
  :modes 'sh-mode
  :predicate '(eq sh-shell 'sh))

(flycheck-def-config-file-var flycheck-chktexrc tex-chktex ".chktexrc")

(flycheck-declare-checker tex-chktex
  "A TeX and LaTeX syntax and style checker using chktex.

See URL `http://baruch.ev-en.org/proj/chktex/'."
  :command '("chktex" (config-file "-l" flycheck-chktexrc) "-v0" "-q" "-I"
             source-inplace)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\):\\(?4:[0-9]+:.*\\)$"
     warning))
  :modes '(latex-mode plain-tex-mode))

(flycheck-declare-checker tex-lacheck
  "A LaTeX syntax and style checker using lacheck.

See URL `http://www.ctan.org/pkg/lacheck'."
  :command '("lacheck" source-inplace)
  :error-patterns
  '(("^\"\\(?1:.*\\)\", line \\(?2:[0-9]+\\): \\(?4:.*\\)$" warning))
  :modes 'latex-mode)

(flycheck-declare-checker xml-xmlstarlet
  "A XML validator using the xmlstarlet utility.

See URL `http://xmlstar.sourceforge.net/'."
  :command '("xmlstarlet" "val" "-e" "-q" source)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\)\\.\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes '(xml-mode nxml-mode))

(flycheck-declare-checker zsh
  "A Zsh syntax checker using the zsh executable.

See URL `http://www.zsh.org/'."
  :command '("zsh" "-n" "-d" "-f" source)
  :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'sh-mode
  :predicate '(eq sh-shell 'zsh))

(provide 'flycheck)

;; Local Variables:
;; coding: utf-8
;; End:

;;; flycheck.el ends here
