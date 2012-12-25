;;; flycheck.el --- Flymake reloaded with useful checkers

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck
;; Version: 0.4
;; Keywords: convenience languages tools
;; Package-Requires: ((s "1.3.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; A replacement for `flymake-mode' with much improved configuration and checker
;; API.

;; Like the built-in `flymake-mode' `flycheck-mode' performs on-the-fly syntax
;; and style checkers in various modes.  However it features an improved
;; configuration API based on major modes (instead of file name masks) and a
;; much nicer and easier declarative syntax for checker definitions.

;; See `flycheck-checkers' for a list of provided checkers.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'sh-script))

(require 's)

;; Customization

(defgroup flycheck nil
  "Customization for flymake checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defcustom flycheck-checkers
  '(flycheck-checker-bash
    flycheck-checker-coffee
    flycheck-checker-css
    flycheck-checker-emacs-lisp
    flycheck-checker-haml
    flycheck-checker-html
    flycheck-checker-json
    flycheck-checker-javascript-jshint
    flycheck-checker-javascript-jslint
    flycheck-checker-php
    flycheck-checker-python-flake8
    flycheck-checker-python-pylint
    flycheck-checker-python-pyflakes
    flycheck-checker-ruby
    flycheck-checker-sh
    flycheck-checker-tex-chktex
    flycheck-checker-tex-lacheck
    flycheck-checker-zsh)
  "Flycheck checkers.

A list of flycheck checkers to try for the current buffer.  A
checker is either a variable, which contains a checker definition
or a function that is called upon each syntax check to obtain the
checker definition."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker")))


;; Utility functions
(defun flycheck-temp-file-system (filename prefix)
  "Create a copy of FILENAME with PREFIX in temp directory.

Return the path of the file."
  ;; HACK: Prevent re-compression to work around a supposed bug in Emacs.
  ;; `make-temp-file' calls `write-region' to set the contents of the new
  ;; temporary file, which in turn calls `jka-compr-write-region' for compressed
  ;; files. If `jka-compr-really-do-compress' is non-nil this function uses END
  ;; even though START is a string, hence breaking the `write-region' API that
  ;; flymake relies on.  Report upstream!
  (let ((jka-compr-really-do-compress nil)
        (extension (when filename (file-name-extension filename))))
    (make-temp-file prefix nil
                    (when extension (concat "." extension)))))

(defun flycheck-temp-file-inplace (filename prefix)
  "Create an in-place copy of FILENAME with PREFIX added.

If FILENAME is nil, fall back to `flycheck-temp-file-system'.

Return the path of the file."
  (if filename
      (let* ((directory (file-name-directory filename))
             (name (file-name-nondirectory filename)))
        (expand-file-name (format "flycheck-%s" name) directory))
    ;; With no filename, fall back to a copy in the system directory.
    (flycheck-temp-file-system filename prefix)))

(defun flycheck-find-file-in-tree (filename directory)
  "Find FILENAME in DIRECTORY and all of its ancestors.

Start looking for a file named FILENAME in DIRECTORY and traverse
upwards through all of its ancestors up to the file system root
until the file is found or the root is reached.

Return the absolute path of the file, or nil if the file was not
found in DIRECTORY or any of its ancestors."
  (let ((full-path (expand-file-name filename directory)))
    (cond ((string= directory "/") (when (file-exists-p full-path) full-path))
          ((file-exists-p full-path) full-path)
          ((let ((parent-directory (file-name-directory
                                    (directory-file-name
                                     (file-name-directory full-path)))))
             (flycheck-find-file-in-tree filename parent-directory))))))

(defun flycheck-find-file-for-buffer (filename)
  "Find FILENAME for the current buffer.

First try to find the file in the buffer's directory and any of
its ancestors (see `flycheck-find-file-in-tree').  If that fails
try to find the file in the home directory.  If the file is not
found anywhere return nil."
  (let* ((directory (file-name-directory buffer-file-name))
         (filepath (flycheck-find-file-in-tree filename directory)))
    (or filepath
        (let ((home-path (expand-file-name filename)))
          (when (file-exists-p home-path) home-path)))))

(defun flycheck-canonical-file-name (filename)
  "Turn FILENAME into canonical form.

Return FILENAME without double slashes and without trailing
slash."
  (directory-file-name (expand-file-name filename)))

(defun flycheck-same-files-p (file1 file2)
  "Determine whether two files FILE1 and FILE2 are the same."
  (string= (flycheck-canonical-file-name file1)
           (flycheck-canonical-file-name file2)))

(defun flycheck-save-buffer-to-file (file-name)
  "Save the contents of the current buffer to FILE-NAME."
  (make-directory (file-name-directory file-name) t)
  (write-region nil nil file-name nil 0))

(defun flycheck-temp-buffer-copy (temp-file-fn)
  "Copy current buffer to temp file returned by TEMP-FILE-FN.

Return the name of the temporary file."
  (let ((temp-file (funcall temp-file-fn (buffer-file-name) "flycheck")))
    (flycheck-save-buffer-to-file temp-file)
    temp-file))


;; Checker API
(defun flycheck-get-checker-properties (checker)
  "Get the properties of CHECKER.

CHECKER is a symbol pointing either to a bound variable or to a
function.  In the former case, the `symbol-value' is returned, in
the latter case the return value of the function being invoked
with no arguments.

If CHECKER is a unbound symbol, or not a symbol at all, an error
is signaled."
  (cond
   ((and (symbolp checker) (boundp checker)) (symbol-value checker))
   ((and (symbolp checker) (functionp checker)) (funcall checker))
   (t (error "Invalid checker, expected variable or function, but was: %S"
             checker))))

(defun flycheck-valid-checker-p (properties)
  "Check whether the checker PROPERTIES are valid.

A valid checker must have a :command, and at least one of :modes
and :predicate.

Signal an error if PROPERTIES are invalid.  Otherwise return t."
  (unless (plist-get properties :command)
    (error "Checker %S lacks :command" properties))
  (unless (or (plist-get properties :modes)
              (plist-get properties :predicate))
    (error "Checker %S lacks :modes and :predicate" properties))
  t)

(defun flycheck-check-modes (properties)
  "Check the :modes of PROPERTIES.

If PROPERTIES specifies :modes, check `major-mode' against these.
Otherwise return t."
  (let ((modes (plist-get properties :modes)))
    (or (not modes)
        (and (listp modes) (memq major-mode modes))  ; A list of modes
        (eq major-mode modes))))        ; A single mode

(defun flycheck-check-predicate (properties)
  "Check the :predicate of PROPERTIES.

If PROPERTIES contains a :predicate, eval it and return the
result, otherwise return t."
  (let ((predicate (plist-get properties :predicate)))
    (or (not predicate) (eval predicate))))

(defun flycheck-check-executable (properties)
  "Check the executable of the checker PROPERTIES.

Return t, if the executable in the :command of PROPERTIES exists,
or nil otherwise."
  (let ((executable (car (plist-get properties :command))))
    (if (executable-find executable) t)))

(defun flycheck-may-use-checker (properties)
  "Determine whether the checker described by PROPERTIES may be used.

Return t if so, or nil otherwise."
  (unless (flycheck-valid-checker-p properties)
    (error "Checker %s is not valid" properties))
  (and (flycheck-valid-checker-p properties)
       (flycheck-check-modes properties)
       (flycheck-check-predicate properties)
       (flycheck-check-executable properties)))

(defvar flycheck-substituted-files nil
  "A list of all files created for argument substitution.")
(make-variable-buffer-local 'flycheck-substituted-files)

(defun flycheck-clean-substituted-files ()
  "Remove all substituted files."
  (dolist (file-name flycheck-substituted-files)
    (when (file-exists-p file-name)
      (delete-file file-name))
    (setq flycheck-substituted-files nil)))

(defun flycheck-substitute-argument (arg)
  "Substitute ARG with file to check is possible.

If ARG is `source' or `source-inplace', create a temporary file
to checker and return its path, otherwise return ARG unchanged."
  (let ((temp-file-function
         (cond ((eq arg 'source) 'flycheck-temp-file-system)
               ((eq arg 'source-inplace) 'flycheck-temp-file-inplace))))
    (if temp-file-function
        (let ((temp-file (flycheck-temp-buffer-copy temp-file-function)))
          (add-to-list 'flycheck-substituted-files temp-file)
          temp-file)
      arg)))

(defun flycheck-get-substituted-command (properties)
  "Get the substitute :command from PROPERTIES."
  (mapcar 'flycheck-substitute-argument
          (plist-get properties :command)))

(defun flycheck-error-pattern-p (pattern)
  "Check whether PATTERN is a valid error pattern."
  (and
   (listp pattern)                      ; A pattern must be a list...
   (= (length pattern) 6)               ; ...of length 6...
   (stringp (car pattern))              ; ...whose first element is a string
   ))

(defun flycheck-error-patterns-list-p (patterns)
  "Check whether PATTERNS is a list of valid error patterns."
  (let ((result nil))
    (dolist (pattern patterns result)
      (setq result (flycheck-error-pattern-p pattern))
      (unless result (return)))))

(defun flycheck-get-error-patterns (properties)
  "Get the error patterns from PROPERTIES.

PROPERTIES is a property list with information about the checker.

Return a list of error patterns of the given checker."
  (let ((patterns (plist-get properties :error-patterns)))
    (when patterns
      (cond
       ;; A single pattern was given, wrap it up in a list
       ((flycheck-error-pattern-p patterns) (list patterns))
       ;; A list of patterns
       ((flycheck-error-patterns-list-p patterns) patterns)
       (t (error "Invalid type for :error-patterns: %S" patterns))))))

(defun flycheck-get-checker-for-buffer ()
  "Find the checker for the current buffer.

Return the checker properties if there is a checker for the
current buffer, or nil otherwise."
  (dolist (checker flycheck-checkers)
    (let ((properties (flycheck-get-checker-properties checker)))
      (when (flycheck-may-use-checker properties)
        (return properties)))))


;; Error API
(defstruct (flycheck-error
            (:constructor flycheck-make-error))
  buffer file-name line-no col-no text level)

(defmacro flycheck-error-with-buffer (err &rest forms)
  "Switch to the buffer of ERR and evaluate FORMS.

If the buffer of ERR is not live, FORMS are not evaluated."
  (declare (indent 1))
  `(when (buffer-live-p (flycheck-error-buffer ,err))
    (with-current-buffer (flycheck-error-buffer ,err)
      ,@forms)))

(defun flycheck-parse-output (output buffer patterns)
  "Parse OUTPUT from BUFFER with PATTERNS.

PATTERNS is a list of flycheck error patterns.

Return a list of parsed errors and warnings (as `flycheck-error`
objects)."
  (let ((errors nil)
        (last-match 0))
    (dolist (pattern patterns)
      (let ((file-idx (nth 1 pattern))
            (line-idx (nth 2 pattern))
            (col-idx (nth 3 pattern))
            (text-idx (nth 4 pattern))
            (level (nth 5 pattern)))
        (while (string-match (nth 0 pattern) output last-match)
          (setq errors
                (cons
                 (flycheck-make-error
                  :buffer buffer
                  :file-name (when file-idx (match-string file-idx output))
                  :line-no (when line-idx
                             (string-to-number (match-string line-idx output)))
                  :col-no (when col-idx
                            (string-to-number (match-string col-idx output)))
                  :text (when text-idx (match-string text-idx output))
                  :level level)
                 errors))
          (setq last-match (match-end 0))))
      (setq last-match 0))
    errors))

(defun flycheck-relevant-error-p (err)
  "Determine whether ERR is relevant for the current buffer.

Return t if ERR may be shown for the current buffer, or nil
otherwise."
  (flycheck-error-with-buffer err
    (let ((file-name (flycheck-error-file-name err)))
      (and
       ;; If the error includes a file name it must refer to its buffer's file
       (or (not file-name) (flycheck-same-files-p file-name (buffer-file-name)))
       ;; The message must have a text
       (not (s-blank? (flycheck-error-text err)))
       ;; And it should have a line
       (flycheck-error-line-no err)))))

(defun flycheck-back-substitute-filename (err)
  "Reverse substitute the file name in ERR.

Substitute the file name of ERR with the `buffer-file-name' of
the corresponding buffer if it matches and file in
`flycheck-substituted-files'."
  (flycheck-error-with-buffer err
    (let ((file-name (flycheck-error-file-name err)))
      (when file-name
        (dolist (substituted-file flycheck-substituted-files)
          (when (flycheck-same-files-p file-name substituted-file)
            (setf (flycheck-error-file-name err) (buffer-file-name))
            (return err))))
      err)))

(defun flycheck-sanitize-error (err)
  "Sanitize ERR.

Clean up the error file name and the error message."
  ;; Expand the file name
  (flycheck-error-with-buffer err
    ;; Clean up the file name
    (let ((filename (flycheck-error-file-name err))
          (text (flycheck-error-text err)))
      ;; Collapse white space in messages to remove any new lines and
      ;; indentation.
      (setf (flycheck-error-text err) (s-collapse-whitespace text))
      (when filename
        ;; If the error has a file name, expand it relative to the default
        ;; directory of its buffer and back substitute the file name
        (setf (flycheck-error-file-name err) (expand-file-name filename))
        (flycheck-back-substitute-filename err)))))

(defun flycheck-sanitize-errors (errors)
  "Sanitize ERRORS.

Remove all errors that do not belong to the current file."
  (let ((sanitized-errors nil))
    (dolist (err errors)
      (setq err (flycheck-sanitize-error err))
      (when (flycheck-relevant-error-p err)
        (setq sanitized-errors (cons err sanitized-errors))))
    sanitized-errors))

(defun flycheck-count-errors (errors)
  "Count the number of warnings and errors in ERRORS.

Return a cons cell whose `car' is the number of errors and whose
`car' is the number of warnings."
  (let ((no-errors 0)
        (no-warnings 0))
    (dolist (err errors)
      (let ((level (flycheck-error-level err)))
        (cond
         ((eq level 'error) (setq no-errors (+ no-errors 1)))
         ((eq level 'warning) (setq no-warnings (+ no-warnings 1))))))
    `(,no-errors . ,no-warnings)))

(defun flycheck-report-errors (errors)
  "Report ERRORS in the current buffer.

Add overlays and report a proper flycheck status."
  (flycheck-add-overlays errors)
  (if errors
      (let ((no-err-warnings (flycheck-count-errors errors)))
        (flycheck-report-status
         (format ":%s/%s" (car no-err-warnings) (cdr no-err-warnings))))
    (flycheck-report-status "")))

(defvar flycheck-current-errors nil
  "A list of all errors and warnings in the current buffer.")
(make-variable-buffer-local 'flycheck-current-errors)


;; Overlay management
(defface flycheck-error-face
  '((t (:inherit flymake-errline)))
  "Face for flycheck errors."
  :group 'flycheck)

(defface flycheck-warning-face
  '((t (:inherit flymake-warnline)))
  "Face for flycheck warnings."
  :group 'flycheck)

(defconst flycheck-error-overlay nil
  "Overlay category for flycheck errors.")
(put 'flycheck-error-overlay 'face 'flycheck-error-face)
(put 'flycheck-error-overlay 'priority 100)
(put 'flycheck-error-overlay 'line-prefix "⚠")
(put 'flycheck-error-overlay 'help-echo "Unknown error.")

(defconst flycheck-warning-overlay nil
  "Overlay category for flycheck warning.")
(put 'flycheck-warning-overlay 'face 'flycheck-warning-face)
(put 'flycheck-warning-overlay 'priority 100)
(put 'flycheck-warning-overlay 'line-prefix "⚠")
(put 'flycheck-warning-overlay 'help-echo "Unknown warning.")

(defconst flycheck-overlay-categories-alist
  '((warning . flycheck-warning-overlay)
    (error . flycheck-error-overlay))
  "Overlay categories for error levels.")

(defun flycheck-add-overlay (err)
  "Add overlay for ERR."
  (flycheck-error-with-buffer err
    (save-excursion
      (goto-char (point-min))
      (forward-line (- (flycheck-error-line-no err) 1))
      ;; TODO: Consider column number
      (let* ((beg (line-beginning-position))
             (end (line-end-position))
             (category (cdr (assq (flycheck-error-level err)
                                  flycheck-overlay-categories-alist)))
             (text (flycheck-error-text err))
             (overlay (make-overlay beg end (flycheck-error-buffer err))))
        ;; TODO: Consider hooks to re-check if overlay contents change
        (overlay-put overlay 'category category)
        (unless (s-blank? text)
          (overlay-put overlay 'help-echo text))))))

(defun flycheck-add-overlays (errors)
  "Add overlays for ERRORS."
  (mapc #'flycheck-add-overlay errors))

(defun flycheck-clean-overlays ()
  "Remove all flycheck overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'category
                   'flycheck-warning-overlay)
  (remove-overlays (point-min) (point-max) 'category
                   'flycheck-error-overlay))


;; Process management
(defvar flycheck-current-patterns nil
  "Patterns to parse the output of the current process.")
(make-variable-buffer-local 'flycheck-current-patterns)

(defvar flycheck-current-process nil
  "The current syntax checking process.")
(make-variable-buffer-local 'flycheck-current-process)

(defun flycheck-running-p ()
  "Determine whether a syntax check is running."
  (when flycheck-current-process t))

(defvar flycheck-pending-output nil
  "A list of outputs by the current syntax checking process.")
(make-variable-buffer-local 'flycheck-pending-output)

(defun flycheck-receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (let ((source-buffer (process-buffer process)))
    (when (buffer-live-p source-buffer)
      ;; Aggregate output in the right buffer
      (with-current-buffer source-buffer
        (setq flycheck-pending-output
              (cons output flycheck-pending-output))))))

(defun flycheck-handle-signal (process event)
  "Handle a syntax checking PROCESS EVENT."
  (let ((status (process-status process)))
    (when (memq status '(signal exit))
      (let ((source-buffer (process-buffer process)))
        (when (buffer-live-p source-buffer)
          ;; Only parse and show errors if the mode is still active
          (with-current-buffer source-buffer
            (flycheck-report-status "")
            (delete-process process)
            (setq flycheck-current-process nil)
            (when flycheck-mode
              ;; Parse error messages if flycheck mode is active
              (let ((output (apply #'concat
                                   (nreverse flycheck-pending-output))))
                (setq flycheck-current-errors
                      (flycheck-sanitize-errors
                       (flycheck-parse-output output (current-buffer)
                                              flycheck-current-patterns))))
              (setq flycheck-pending-output nil)
              (flycheck-report-errors flycheck-current-errors))
            ;; Remove substituted files
            (flycheck-clean-substituted-files)))))))

(defun flycheck-start-checker (properties)
  "Start the syntax checker defined by PROPERTIES."
  (let* ((command (flycheck-get-substituted-command properties))
         (program (car command))
         (args (cdr command))
         (process (apply 'start-file-process
                         "flycheck" (current-buffer)
                         program args)))
    ;; Report that flycheck is running
    (flycheck-report-status "*")
    (setq flycheck-current-process process)
    (setq flycheck-pending-output nil)
    ;; Clean previous error information
    (setq flycheck-current-errors nil)
    ;; Clean previous overlays
    (flycheck-clean-overlays)
    ;; Remember the patterns to use to parse the output of this process
    (setq flycheck-current-patterns (flycheck-get-error-patterns properties))
    ;; Register handlers for the process
    (set-process-filter process 'flycheck-receive-checker-output)
    (set-process-sentinel process 'flycheck-handle-signal)))

(defun flycheck-stop-checker ()
  "Stop any syntax checker for the current buffer."
  (when (flycheck-running-p)
    (interrupt-process flycheck-current-process)))


;; Syntax checking mode
(defun flycheck-buffer ()
  "Check syntax in the current buffer."
  (interactive)
  (when (and flycheck-mode (not (flycheck-running-p)))
    (let ((properties (flycheck-get-checker-for-buffer)))
      (when properties
        (flycheck-start-checker properties)))))

;;;###autoload
(defconst flycheck-mode-line-lighter " FlyC"
  "The standard lighter for flycheck mode.")

(defvar flycheck-mode-line nil
  "The mode line lighter of variable `flycheck-mode'.")

(defun flycheck-report-status (status)
  "Report flycheck STATUS."
  (let ((mode-line flycheck-mode-line-lighter))
    (setq mode-line (concat mode-line status))
    (setq flycheck-mode-line mode-line)
    (force-mode-line-update)))

;;;###autoload
(define-minor-mode flycheck-mode
  "Toggle on-the-fly syntax checking."
  :init-value nil
  :lighter flycheck-mode-line
  :require 'flycheck
  (cond
   (flycheck-mode
    (flycheck-report-status "")

    ;; Configure hooks
    (add-hook 'after-save-hook 'flycheck-buffer nil t)

    ;; Start an initial syntax check
    (flycheck-buffer))
   (t
    (flycheck-clean-overlays)

    ;; Remove hooks
    (remove-hook 'after-save-hook 'flycheck-buffer t)

    (flycheck-stop-checker))))

;;;###autoload
(defun flycheck-mode-on ()
  "Unconditionally enable variable `flycheck-mode'."
  (flycheck-mode 1))

;;;###autoload
(defun flycheck-mode-off ()
  "Unconditionally disable variable `flycheck-mode'."
  (flycheck-mode -1))


;; Checkers
(defvar flycheck-checker-bash
  '(:command
    ("bash" "--norc" "-n" source)
    :error-patterns
    (("^\\(.+\\): line \\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 error))
    :modes sh-mode
    :predicate (eq sh-shell 'bash)))

(defvar flycheck-checker-coffee
  '(:command
    ("coffeelint" "--csv" source)
    :error-patterns
    (("SyntaxError: \\(.*\\) on line \\([0-9]+\\)" nil 2 nil 1 error)
     ("\\(.+\\),\\([0-9]+\\),error,\\(.+\\)" 1 2 nil 3 error)
     ("\\(.+\\),\\([0-9]+\\),warn,\\(.+\\)" 1 2 nil 3 warning))
    :modes coffee-mode))

(defvar flycheck-checker-css
  '(:command
    ("csslint" "--format=compact" source)
    :error-patterns
    ("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"
     1 2 3 4 error)
    :modes css-mode))

(defconst flycheck-checker-emacs-lisp-check-form
  '(progn
     (setq byte-compiled-files nil)
     (defun byte-compile-dest-file (source)
       (let ((temp-file (expand-file-name (make-temp-file source)
                                          temporary-file-directory)))
         (add-to-list 'byte-compiled-files temp-file)
         temp-file))

     (setq byte-compile-dest-file-function 'byte-compile-dest-file)
     (mapc 'byte-compile-file command-line-args-left)
     (mapc 'delete-file byte-compiled-files)))

(defun flycheck-checker-emacs-lisp-check-form-s ()
  "Return `flycheck-checker-emacs-lisp-check-form as string."
   (with-temp-buffer
     (print flycheck-checker-emacs-lisp-check-form (current-buffer))
     (buffer-substring-no-properties (point-min) (point-max))))

(defvar flycheck-checker-emacs-lisp
  (let ((executable (concat invocation-directory invocation-name))
        (check-form-s (flycheck-checker-emacs-lisp-check-form-s)))
    `(:command
      (,executable "--no-site-file" "--no-site-lisp" "--batch" "--eval"
                   ;; byte-compile-file has an awkward way of writing file names
                   ;; to stdout, namely it always outputs relative file names
                   ;; even if absolute file names are passed to it, and it
                   ;; completely *omits* the directory in case of warning
                   ;; messages.  Hence we use an in-place copy here to have a
                   ;; reliable base directory from which to expand file names.
                   ;; Otherwise back-substitution will fail because file names
                   ;; in the error messages lack directory information
                   ,check-form-s source-inplace)
      :modes emacs-lisp-mode
      :error-patterns
      (("^\\(.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):Warning:\\(.*\\(?:\n    .*\\)*\\)$"
        1 2 3 4 warning)
       ("^\\(.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):Error:\\(.*\\(?:\n    .*\\)*\\)$"
        1 2 3 4 error)))))

(defvar flycheck-checker-haml
  '(:command
    ("haml" "-c" source)
    :error-patterns
    ("^Syntax error on line \\([0-9]+\\): \\(.*\\)$" nil 1 nil 2 error)
    :modes haml-mode))

(defvar flycheck-checker-html
  '(:command
    ("tidy" "-e" "-q" source)
    :error-patterns
    (("line \\([0-9]+\\) column \\([0-9]+\\) - Error: \\(.*\\)"
      nil 1 2 4 error)
     ("line \\([0-9]+\\) column \\([0-9]+\\) - Warning: \\(.*\\)"
      nil 1 2 4 warning))
    :modes html-mode))

(defvar flycheck-jshintrc nil
  "The path to .jshintrc.

This variable denotes the configuration file for jshint to use
when checking a buffer with jshint.

The path contained in this variable is expanded via
`expand-file-name' before being passed to jshint, thus ~ is
replaced with your $HOME directory.

Use this variable as file local variable to use a specific
configuration file for a buffer.")
(put 'flycheck-jshintrc 'safe-local-variable 'stringp)

(defun flycheck-checker-javascript-jshint ()
  "Check javascript with jshint.

Use .jshintrc from either `flycheck-jshintrc' or – if that
variable is nil – the buffer's directory, any ancestors thereof
or the $HOME directory.

If .jshintrc is not found run jshint with default settings."
  (let ((jshintrc (or flycheck-jshintrc
                      (flycheck-find-file-for-buffer ".jshintrc"))))
    `(:command
      ("jshint"
       ,@(when jshintrc `("--config" ,(expand-file-name jshintrc)))
       source)
      :error-patterns
      (("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"
        1 2 3 4 error))
      :modes js-mode)))

(defvar flycheck-checker-javascript-jslint
  '(:command
    ("jsl" "-process" source)
    :error-patterns
    (("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$"
      nil 2 nil 3 error)
     ("^\\(.+\\)(\\([0-9]+\\)): \\(SyntaxError:.+\\)$"
      nil 2 nil 3 error)
     ("^\\(.+\\)(\\([0-9]+\\)): \\(lint \\)?\\(warning:.+\\)$"
      nil 2 nil 4 warning)
     ("^\\(.+\\)\:\\([0-9]+\\)\: strict \\(warning: trailing comma.+\\)\:$"
      nil 2 nil 3 warning))
    :modes js-mode))

(defvar flycheck-checker-json
  '(:command
    ("jsonlint" "-c" "-q" source)
    :error-patterns
    (("^\\(.+\\)\: line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)$"
      nil 2 3 4 error))
    :predicate
    (or (eq major-mode 'json-mode)
        (and buffer-file-name
             (string= "json" (file-name-extension buffer-file-name))))))

(defvar flycheck-checker-php
  '(:command
    ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
      "-d" "log_errors=0" source)
    :error-patterns
    (("\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"
       2 3 nil 1 error))
    :modes php-mode))

(defvar flycheck-checker-python-flake8
  '(:command ("flake8" source-inplace) :modes python-mode))

(defvar flycheck-checker-python-pylint
  '(:command ("epylint" source-inplace) :modes python-mode))

(defvar flycheck-checker-python-pyflakes
  '(:command ("pyflakes" source-inplace) :modes python-mode))

(defvar flycheck-checker-ruby
  '(:command ("ruby" "-w" "-c" source) :modes ruby-mode))

(defvar flycheck-checker-sh
  '(:command
    ("sh" "-n" source)
    :modes sh-mode
    :predicate (eq sh-shell 'sh)))

(defvar flycheck-checker-tex-chktex
  '(:command
    ("chktex" "-v0" "-q" "-I" source-inplace)
    :modes (latex-mode plain-tex-mode)))

(defvar flycheck-checker-tex-lacheck
  '(:command
    ("lacheck" source-inplace)
    :modes latex-mode))

(defvar flycheck-checker-zsh
  '(:command
    ("zsh" "-n" "-d" "-f" source)
    :modes sh-mode
    :predicate (eq sh-shell 'zsh)))

(provide 'flycheck)

;;; flycheck.el ends here
