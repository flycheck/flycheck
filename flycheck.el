;;; flycheck.el --- Flymake done right

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck
;; Version: 0.5
;; Keywords: convenience languages tools
;; Package-Requires: ((s "1.3.0") (dash "1.0.3"))

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

;; On-the-fly syntax checking for GNU Emacs (aka "flymake done right")

;; Provide `flycheck-mode' which enables on-the-fly syntax checking for a large
;; number of different modes and languages (see `flycheck-checkers' for a
;; complete list).
;;
;; Support for new modes and languages can be added by declaring a new syntax
;; checker.  Read README.md for more information and take a look at built-in
;; checkers like `flycheck-checkers-python-pylint'.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'sh-script))

(require 's)
(require 'dash)

;; Customization

(defgroup flycheck nil
  "Customization for flymake checkers."
  :prefix "flycheck-"
  :group 'tools)

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
    flycheck-checker-lua
    flycheck-checker-perl
    flycheck-checker-php
    flycheck-checker-python-flake8
    flycheck-checker-python-pylint
    flycheck-checker-python-pyflakes
    flycheck-checker-ruby
    flycheck-checker-sass
    flycheck-checker-sh
    flycheck-checker-tex-chktex
    flycheck-checker-tex-lacheck
    flycheck-checker-xml-xmlstarlet
    flycheck-checker-zsh)
  "Flycheck checkers.

A list of flycheck checkers to try for the current buffer.

If `flycheck-checker' is nil automatically select a suitable
checker from this list on every syntax check.

A checker is either a variable, which contains a checker
definition or a function that is called upon each syntax check to
obtain the checker definition."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker")))

;; TODO: Find out how we can use defvar-local from Emacs 24.3
(defvar flycheck-checker nil
  "Checker to use for the current buffer.

If unset automatically select a suitable checker from
`flycheck-checkers' on every syntax check.

If set to a checker only use this checker.  Do never
automatically select a checker from `flycheck-checkers' is
skipped even if this checker is unusable in the current
buffer (e.g. it does not exist, the major modes do not match,
etc.).  In this case signal an error.

A checker is either a variable bound to a checker definition or a
function that is called upon each syntax check to obtain a
checker definition (see `flycheck-checkers').

Use the command `flycheck-select-checker' to select a checker for
the current buffer, or set this variable as file local variable
to always use a specific checker for a file.")
(make-variable-buffer-local 'flycheck-checker)
(put 'flycheck-checker 'safe-local-variable 'flycheck-registered-checker-p)

(defface flycheck-error-face
  '((t (:inherit 'error)))
  "Face for flycheck errors."
  :group 'flycheck)

(defface flycheck-warning-face
  '((t (:inherit 'warning)))
  "Face for flycheck warnings."
  :group 'flycheck)

(defcustom flycheck-ignore-columns nil
  "Ignore column numbers when highlighting errors.

If nil only highlight the affected column an error refers to
specific column.  If t columns are ignored and the whole line his
highlighted regardless of whether an error refers to a column or
a complete line.

Note that this does not affect error navigation.  When navigating
errors with `next-error' and `previous-error' Flycheck always
just to the error column."
  :group 'flycheck
  :type 'boolean)

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

;;;###autoload
(defvar flycheck-mode nil
  "Whether `flycheck-mode' is enabled or disabled.

See function `flycheck-mode' for a description of this minor
mode.")


;; Utility functions
(defun flycheck-temp-file-system (filename prefix)
  "Create a copy of FILENAME with PREFIX in temp directory.

Return the path of the file."
  ;; HACK: Prevent re-compression to work around a supposed bug in Emacs.
  ;; `make-temp-file' calls `write-region' to set the contents of the new
  ;; temporary file, which in turn calls `jka-compr-write-region' for compressed
  ;; files. If `jka-compr-really-do-compress' is non-nil this function uses END
  ;; even though START is a string, hence breaking the `write-region' API that
  ;; we rely on.  Report upstream!
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
        (expand-file-name (format "%s-%s" prefix name) directory))
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
or if the buffer has no `buffer-file-name' try to find the file
in the home directory.  If the file is not found anywhere return
nil."
  (let ((directory (when (buffer-file-name)
                     (file-name-directory (buffer-file-name)))))
    (or (when directory (flycheck-find-file-in-tree filename directory))
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

(defun flycheck-registered-checker-p (checker)
  "Determine whether CHECKER is registered.

A checker is registered if it is contained in `flycheck-checkers'."
  (memq checker flycheck-checkers))

(defun flycheck-valid-checker-p (properties)
  "Check whether the checker PROPERTIES are valid.

A valid checker must have a :command, :error-patterns, and at
least one of :modes and :predicate."
  (and
   (plist-get properties :command)
   (plist-get properties :error-patterns)
   (or (plist-get properties :modes)
       (plist-get properties :predicate))))

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
    (error "Checker %s is not valid.  Add :command, :error-patterns and :modes\
 or :predicate" properties))
  (and (flycheck-check-modes properties)
       (flycheck-check-predicate properties)
       (flycheck-check-executable properties)))

(defvar flycheck-substituted-files nil
  "A list of all files created for argument substitution.")
(make-variable-buffer-local 'flycheck-substituted-files)

(defun flycheck-clean-substituted-files ()
  "Remove all substituted files."
  (--each flycheck-substituted-files (ignore-errors (delete-file it)))
  (setq flycheck-substituted-files nil))

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
  (-all? 'flycheck-error-pattern-p patterns))

(defun flycheck-get-error-patterns (properties)
  "Get the error patterns from PROPERTIES.

PROPERTIES is a property list with information about the checker.

Return a list of error patterns of the given checker."
  (let ((patterns (plist-get properties :error-patterns)))
    (if (flycheck-error-patterns-list-p patterns)
        patterns
      (error "Invalid type for :error-patterns: %S" patterns))))

(defvar flycheck-last-checker nil
  "The last checker used for the current buffer.")
(make-variable-buffer-local 'flycheck-last-checker)

(defun flycheck-try-last-checker-for-buffer ()
  "Try the last checker for the current buffer.

Return the properties of the last checker if it may be used, or
nil otherwise."
  ;; We should not use the last checker if it was removed from the list of
  ;; allowed checkers in the meantime
  (when (flycheck-registered-checker-p flycheck-last-checker)
    (let ((last-checker (flycheck-get-checker-properties
                         flycheck-last-checker)))
      ;; Only use the last checker if we really can use it
      (when (flycheck-may-use-checker last-checker)
        last-checker))))

(defun flycheck-get-new-checker-for-buffer ()
  "Find a new checker for the current buffer.

If a checker is found set `flycheck-last-checker' to re-use this
checker for the next check.

Return the properties of the checker or nil otherwise."
  (dolist (checker flycheck-checkers)
    (let ((properties (flycheck-get-checker-properties checker)))
      (when (flycheck-may-use-checker properties)
        (setq flycheck-last-checker checker)
        (return properties)))))

(defun flycheck-get-checker-for-buffer ()
  "Find the checker for the current buffer.

Return the checker properties if there is a checker for the
current buffer, or nil otherwise."
  (if flycheck-checker
      ;; If a checker was configured, try to use it!
      (let ((properties (flycheck-get-checker-properties flycheck-checker)))
        (if (and properties (flycheck-may-use-checker properties))
            properties
          (error "Configured checker %s cannot be used"
                 flycheck-checker)))
    (let ((last-checker (flycheck-try-last-checker-for-buffer)))
      (or last-checker (flycheck-get-new-checker-for-buffer)))))

(defvar read-flycheck-checker-history nil
  "History of `read-flycheck-checker'.")

(defun read-flycheck-checker (prompt)
  "Read a flycheck checker from minibuffer with PROMPT.

Complete with all registered checkers (see
`flycheck-registered-checker-p').

Return the checker as symbol, or nil if no checker was
chosen."
  (let* ((checkers (mapcar #'symbol-name flycheck-checkers))
         (input (completing-read "Checker: " obarray
                                 'flycheck-registered-checker-p nil
                                 nil 'read-flycheck-checker-history)))
    (if (string= input "") nil (intern input))))

(defun flycheck-select-checker (checker)
  "Select CHECKER for the current buffer.

CHECKER is a symbol providing a checker definition (see
`flycheck-checkers') or nil.  If nil deselect the current
checker (if any) and use automatic checker selection via
`flycheck-checkers'.

If called interactively prompt for CHECKER.  If no checker is
entered deselect the current checker.  With prefix arg
immediately deselect the current checker without any prompt.

Set `flycheck-checker' to CHECKER and automatically start a new
syntax check if the checker changed."
  (interactive
   (if current-prefix-arg
       (list nil)
     (list (read-flycheck-checker "Checker: "))))
  (when (not (eq checker flycheck-checker))
    (setq flycheck-checker checker)
    (flycheck-buffer)))


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

(defun flycheck-error-region (err &optional ignore-column)
  "Get the region of ERR.

ERR is a flycheck error whose region to get.  If IGNORE-COLUMN is
given and t ignore the column number of ERR when determining the
region.  Hence the region will always extend over the whole line.

Return a cons cell (BEG . END).  BEG is the beginning of the
error region and END its end.  If ERR has a column number and
IGNORE-COLUMN is omitted or nil BEG and END are equal and refer
to the error column.  Otherwise BEG is the beginning of the ERR
line and END its end."
  (save-excursion
    (goto-char (point-min))
    (forward-line (- (flycheck-error-line-no err) 1))
    (let* ((col (if ignore-column nil (flycheck-error-col-no err)))
           (beg (+ (line-beginning-position) (or col 0)))
           (end (if col beg (line-end-position))))
      `(,beg . ,end))))

(defun flycheck-error-pos (err)
  "Get the buffer position of ERR.

If ERR has a column return exactly that column.  Otherwise return
the beginning of the line of ERR."
  (car (flycheck-error-region err)))

(defun flycheck-parse-output-with-pattern (output buffer pattern)
  "Parse OUTPUT from BUFFER with PATTERN.

PATTERN is a flycheck error pattern.

Return a list of parsed errors and warnings as `flycheck-error`
objects."
  (let ((file-idx (nth 1 pattern))
        (line-idx (nth 2 pattern))
        (col-idx (nth 3 pattern))
        (text-idx (nth 4 pattern))
        (level (nth 5 pattern))
        (errors nil)
        (last-match 0))
    (while (string-match (nth 0 pattern) output last-match)
      (!cons
       (flycheck-make-error
        :buffer buffer
        :file-name (when file-idx (match-string file-idx output))
        :line-no (when line-idx
                   (string-to-number (match-string line-idx output)))
        :col-no (when col-idx
                  (string-to-number (match-string col-idx output)))
        :text (when text-idx (match-string text-idx output))
        :level level)
       errors)
      (setq last-match (match-end 0)))
    errors))

(defun flycheck-parse-output (output buffer patterns)
  "Parse OUTPUT from BUFFER with PATTERNS.

PATTERNS is a list of flycheck error patterns.

Return a list of parsed errors and warnings (as `flycheck-error`
objects)."
  (--mapcat (flycheck-parse-output-with-pattern output buffer it) patterns))

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
        (--each
          flycheck-substituted-files
          (when (flycheck-same-files-p file-name it)
            (setf (flycheck-error-file-name err) (buffer-file-name)))))
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
      (when text
        (setf (flycheck-error-text err) (s-collapse-whitespace text)))
      (when filename
        ;; If the error has a file name, expand it relative to the default
        ;; directory of its buffer and back substitute the file name
        (setf (flycheck-error-file-name err) (expand-file-name filename))
        (flycheck-back-substitute-filename err))))
  err)

(defun flycheck-sanitize-errors (errors)
  "Sanitize ERRORS.

Remove all errors that do not belong to the current file."
  (-filter 'flycheck-relevant-error-p (-map 'flycheck-sanitize-error errors)))

(defun flycheck-error-<= (err1 err2)
  "Determine whether ERR1 goes before ERR2."
  (let ((line1 (flycheck-error-line-no err1))
        (line2 (flycheck-error-line-no err2)))
    (if (= line1 line2)
        ;; Sort by column number if possible
        (let ((col1 (flycheck-error-col-no err1))
              (col2 (flycheck-error-col-no err2)))
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
    `(,(length errors) . ,(length warnings))))

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

(defun flycheck-clear-errors ()
  "Remove all error information from the current buffer."
  (setq flycheck-current-errors nil)
  (flycheck-report-status ""))


;; Overlay management
(define-fringe-bitmap 'flycheck-fringe-exclamation-mark
  [24 60 60 24 24 0 0 24 24] nil nil 'center)

(defconst flycheck-error-overlay nil
  "Overlay category for flycheck errors.")
(put 'flycheck-error-overlay 'flycheck-overlay t)
(put 'flycheck-error-overlay 'face 'flycheck-error-face)
(put 'flycheck-error-overlay 'priority 100)
(put 'flycheck-error-overlay 'help-echo "Unknown error.")
;; TODO: Figure out how to use the new exclamation-mark bitmap from Emacs 24.3
(put 'flycheck-error-overlay 'flycheck-fringe-bitmap
     'flycheck-fringe-exclamation-mark)

(defconst flycheck-warning-overlay nil
  "Overlay category for flycheck warning.")
(put 'flycheck-warning-overlay 'flycheck-overlay t)
(put 'flycheck-warning-overlay 'face 'flycheck-warning-face)
(put 'flycheck-warning-overlay 'priority 100)
(put 'flycheck-warning-overlay 'help-echo "Unknown warning.")
(put 'flycheck-warning-overlay 'flycheck-fringe-bitmap 'question-mark)

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
      (let* ((level (flycheck-error-level err))
             (region (flycheck-error-region err flycheck-ignore-columns))
             (end (cdr region))
             ;; Highlight the column appropriately
             (beg (if (= (car region) end) (- end 1) (car region)))
             (category (cdr (assq level flycheck-overlay-categories-alist)))
             (text (flycheck-error-text err))
             (overlay (make-overlay beg end (flycheck-error-buffer err)))
             (fringe-icon `(left-fringe ,(get category 'flycheck-fringe-bitmap)
                                        ,(get category 'face))))
        ;; TODO: Consider hooks to re-check if overlay contents change
        (overlay-put overlay 'category category)
        (overlay-put overlay 'flycheck-error err)
        (overlay-put overlay 'before-string
                     (propertize "!" 'display fringe-icon))
        (unless (s-blank? text)
          (overlay-put overlay 'help-echo text))))))

(defun flycheck-add-overlays (errors)
  "Add overlays for ERRORS."
  (mapc #'flycheck-add-overlay errors))

(defun flycheck-overlays-at (pos)
  "Return a list of all flycheck overlays at POS."
  (--filter (overlay-get it 'flycheck-overlay) (overlays-at pos)))

(defun flycheck-overlay-errors-at (pos)
  "Return a list of all flycheck errors overlayed at POS."
  (--map (overlay-get it 'flycheck-error) (flycheck-overlays-at pos)))

(defun flycheck-overlay-messages-at (pos)
  "Return a list of all flycheck messages overlayed at POS."
  (--map (overlay-get it 'help-echo) (flycheck-overlays-at pos)))

(defun flycheck-remove-overlays ()
  "Remove all flycheck overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'flycheck-overlay t))


;; Error navigation
(defun flycheck-next-error (no-errors reset)
  "Advance NO-ERRORS, optionally RESET before.

NO-ERRORS is a number specifying how many errors to move forward.
IF RESET is t, move to beginning of buffer first."
  (when reset
    (point-min))
  ;; TODO: Horribly inefficient, possibly improve by considering less errors.
  (let* ((err-positions (-map 'flycheck-error-pos flycheck-current-errors))
         ;; Remove the current point for the errors because we don't want to
         ;; navigate to the current error again
         (navigatable-errors (--remove (= (point) it) err-positions))
         ;; Take errors before the point and errors after the point
         (splitted (--split-with (>= (point) it) navigatable-errors))
         (pos-before (nreverse (car splitted)))
         (pos-after (cadr splitted))
         (positions (if (< no-errors 0) pos-before pos-after))
         ;; Eventually get the position and navigate to it
         (pos (nth (- (abs no-errors) 1) positions)))
    (when pos
      (goto-char pos))))


;; Error message echoing
(defun flycheck-may-show-message ()
  "Determine whether the minibuffer is free to show a message.

Return t if the minibuffer is free to show message or nil otherwise.

The minibuffer is considered free if the minibuffer is not active
and the cursor is not in the minibuffer."
  (and (not (active-minibuffer-window)) (not cursor-in-echo-area)))

(defvar flycheck-error-display-timer nil
  "Timer to automatically show the error at point in minibuffer.")
(make-variable-buffer-local 'flycheck-error-display-timer)

(defun flycheck-cancel-error-display-timer ()
  "Cancel the error display timer for the current buffer."
  (when flycheck-error-display-timer
    (cancel-timer flycheck-error-display-timer)
    (setq flycheck-error-display-timer nil)))

(defun flycheck-show-error-at-point ()
  "Show the first error message at point in minibuffer."
  (interactive)
  (flycheck-cancel-error-display-timer)
  (when flycheck-mode
    (if (flycheck-may-show-message)
        (let ((message (car (flycheck-overlay-messages-at (point)))))
          (if message
              (message "%s" message)
            ;; Clear the current message
            (message nil)))
      ;; The minibuffer is not available, so let's try again in some seconds.
      (flycheck-show-error-at-point-soon))))

(defun flycheck-show-error-at-point-soon ()
  "Show the first error message at point in minibuffer asap.

Show the error message at point in minibuffer after a short delay."
  (flycheck-cancel-error-display-timer)
  (when (flycheck-overlays-at (point))
    (setq flycheck-error-display-timer
          (run-at-time 0.9 nil 'flycheck-show-error-at-point))))


;; Process management
(defvar flycheck-current-process nil
  "The current syntax checking process.")
(make-variable-buffer-local 'flycheck-current-process)

(defun flycheck-running-p ()
  "Determine whether a syntax check is running."
  (when flycheck-current-process t))

(defun flycheck-post-syntax-check-cleanup (&optional process)
  "Cleanup after a syntax check PROCESS."
  ;; Clean temporary copies of the buffer
  (unwind-protect
      (let ((process (or process flycheck-current-process)))
        (when process
          (setq flycheck-current-process nil)
          (delete-process process)))
    (flycheck-clean-substituted-files)))

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

(defun flycheck-finish-syntax-check (properties exit-status output)
  "Finish a syntax check.

PROPERTIES are the checker properties.  EXIT-STATUS is the
integral exit code of the syntax checker and OUTPUT its output a
string.

Parse the output and report an appropriate error status."
  ;; Clear running state
  (flycheck-report-status "")
  (let* ((error-patterns (flycheck-get-error-patterns properties))
         (parsed-errors (flycheck-parse-output output (current-buffer)
                                               error-patterns))
         (errors (flycheck-sort-errors
                  (flycheck-sanitize-errors parsed-errors))))
    (when flycheck-mode
      ;; Parse error messages if flycheck mode is active
      (setq flycheck-current-errors errors)
      (flycheck-report-errors errors)
      (when (and (/= exit-status 0) (not errors))
        ;; Report possibly flawed checker definition
        (message "Checker %s returned non-zero exit code %s, but no errors from\
output: %s\nChecker definition probably flawed."
                 properties exit-status output)
        (flycheck-report-status "?"))
      ;; Update any errors messages in minibuffer
      (when (eq (current-buffer) (window-buffer))
        (flycheck-show-error-at-point))
      ;; Eventually run post-check hooks
      (run-hooks 'flycheck-after-syntax-check-hook))))

(defun flycheck-handle-signal (process event)
  "Handle a syntax checking PROCESS EVENT."
  (when (memq (process-status process) '(signal exit))
    (with-current-buffer (process-buffer process)
      ;; Try hard to clean up after the party
      (unwind-protect
          (condition-case-unless-debug err
              (when (buffer-live-p (process-buffer process))
                ;; Only parse and show errors if the mode is still active
                (flycheck-finish-syntax-check
                 (process-get process :flycheck-checker)
                 (process-exit-status process)
                 (flycheck-get-output process)))
            (error
             ;; Report and re-signal errors
             (flycheck-report-status "!")
             (signal (car err) (cdr err))))
        (flycheck-post-syntax-check-cleanup process)))))

(defun flycheck-start-checker (properties)
  "Start the syntax checker defined by PROPERTIES."
  (condition-case err
      (let* ((command (flycheck-get-substituted-command properties))
             (program (car command))
             (args (cdr command))
             (process (apply 'start-file-process
                             "flycheck" (current-buffer)
                             program args)))
        ;; Remember this process
        (setq flycheck-current-process process)
        ;; Register handlers for the process
        (set-process-filter process 'flycheck-receive-checker-output)
        (set-process-sentinel process 'flycheck-handle-signal)
        ;; Report that flycheck is running
        (flycheck-report-status "*")
        ;; Attach checker information to the process
        (process-put process :flycheck-checker properties))
      (error
       ;; Report error status, clean-up and re-signal error in case process
       ;; start or setup failed
       (flycheck-report-status "!")
       (flycheck-post-syntax-check-cleanup)
       (signal (car err) (cdr err)))))

(defun flycheck-stop-checker ()
  "Stop any syntax checker for the current buffer."
  (when (flycheck-running-p)
    (interrupt-process flycheck-current-process)))


;; Syntax checking mode
(defun flycheck-handle-change (beg end len)
  "Handle a buffer change between BEG and END with LEN.

BEG and END mark the beginning and end of the change text.  LEN is ignored.

Start a syntax check if a new line has been inserted into the buffer."
  (let ((new-text (buffer-substring beg end)))
    (when (and flycheck-mode (s-contains? "\n" new-text))
      (flycheck-buffer-safe))))

(defun flycheck-clear ()
  "Clear all errors in the current buffer."
  (interactive)
  (flycheck-remove-overlays)
  (flycheck-clear-errors))

(defun flycheck-buffer ()
  "Check syntax in the current buffer."
  (interactive)
  (flycheck-clear)
  (if flycheck-mode
      (when (not (flycheck-running-p))
        (let ((properties (flycheck-get-checker-for-buffer)))
          (when properties (flycheck-start-checker properties))))
    (error "Flycheck mode disabled")))

(defun flycheck-buffer-safe ()
  "Safely check syntax in the current buffer.

Like `flycheck-buffer', but do not check buffers that need not be
checked (i.e. read-only buffers) and demote all errors to messages.

Use when checking buffers automatically."
  (if (not buffer-read-only)
      (with-demoted-errors
        (flycheck-buffer))
    (message "flycheck will not check read-only buffers.")))

;;;###autoload
(defconst flycheck-mode-line-lighter " FlyC"
  "The standard lighter for flycheck mode.")

(defvar flycheck-mode-line nil
  "The mode line lighter of variable `flycheck-mode'.")
(make-variable-buffer-local 'flycheck-mode-line)

(defun flycheck-report-status (status)
  "Report flycheck STATUS."
  (let ((mode-line flycheck-mode-line-lighter))
    (setq mode-line (concat mode-line status))
    (setq flycheck-mode-line mode-line)
    (force-mode-line-update)))

(defun flycheck-teardown ()
  "Teardown flyheck.

Completely clear the whole flycheck state.  Remove overlays, kill
running checks, and empty all variables used by flycheck."
  (flycheck-clear)
  (flycheck-stop-checker)
  (flycheck-cancel-error-display-timer)
  (flycheck-post-syntax-check-cleanup)
  (setq flycheck-checker nil)
  (setq flycheck-last-checker nil))

;;;###autoload
(define-minor-mode flycheck-mode
  "Minor mode for on-the-fly syntax checking.

When called interactively, toggle `flycheck-mode'.  With prefix
ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `flycheck-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :lighter flycheck-mode-line
  :require 'flycheck
  (cond
   (flycheck-mode
    (flycheck-report-status "")

    ;; Configure hooks
    (add-hook 'after-save-hook 'flycheck-buffer-safe nil t)
    (add-hook 'after-change-functions 'flycheck-handle-change nil t)
    (add-hook 'post-command-hook 'flycheck-show-error-at-point-soon nil t)

    ;; Enable navigation through Flycheck errors
    (setq next-error-function 'flycheck-next-error)

    ;; Start an initial syntax check
    (flycheck-buffer-safe))
   (t
    ;; Remove hooks
    (remove-hook 'after-save-hook 'flycheck-buffer-safe t)
    (remove-hook 'after-change-functions 'flycheck-handle-change t)
    (remove-hook 'post-command-hook 'flycheck-show-error-at-point-soon t)

    ;; Disable Flycheck error navigation again
    (setq next-error-function nil)

    ;; and clear internal state
    (flycheck-teardown))))

;;;###autoload
(defun flycheck-mode-on ()
  "Unconditionally enable variable `flycheck-mode'."
  (flycheck-mode 1))
(make-obsolete 'flycheck-mode-on 'flycheck-mode "0.5")

;;;###autoload
(defun flycheck-mode-off ()
  "Unconditionally disable variable `flycheck-mode'."
  (flycheck-mode -1))
(make-obsolete 'flycheck-mode-off "Use (flycheck-mode -1)." "0.5")


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
    (("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"
      1 2 3 4 error))
    :modes css-mode))

(defconst flycheck-checker-emacs-lisp-check-form
  '(progn
     ;; Initialize packages to at least try to load dependencies
     (package-initialize)

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
      :error-patterns
      (("^\\(.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):Warning:\\(.*\\(?:\n    .*\\)*\\)$"
        1 2 3 4 warning)
       ("^\\(.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):Error:\\(.*\\(?:\n    .*\\)*\\)$"
        1 2 3 4 error))
      :modes (emacs-lisp-mode lisp-interaction-mode)
      ;; Prevent Emacs Lisp checking in temporary buffers because we cannot
      ;; reliably parse file names from error messages thanks to aforementioned
      ;; idiocy
      :predicate (and (buffer-file-name) (not no-byte-compile)))))

(defvar flycheck-checker-haml
  '(:command
    ("haml" "-c" source)
    :error-patterns
    (("^Syntax error on line \\([0-9]+\\): \\(.*\\)$" nil 1 nil 2 error))
    :modes haml-mode))

(defvar flycheck-checker-html
  '(:command
    ("tidy" "-e" "-q" source)
    :error-patterns
    (("^line \\([0-9]+\\) column \\([0-9]+\\) - Error: \\(.*\\)$"
      nil 1 2 3 error)
     ("^line \\([0-9]+\\) column \\([0-9]+\\) - Warning: \\(.*\\)$"
      nil 1 2 3 warning))
    :modes (html-mode nxhtml-mode)))

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

(defvar flycheck-checker-lua
  '(:command
    ("luac" "-p" source)
    :error-patterns
    (("^.*?: \\(.*?\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 error))
    :modes lua-mode))

(defvar flycheck-checker-perl
  '(:command
    ("perl" "-w" "-c" source)
    :error-patterns
    (("^\\(.*?\\) at \\(.*?\\) line \\([0-9]+\\)\\.$" 2 3 nil 1 error)
     ("^\\(.*?\\) at \\(.*?\\) line \\([0-9]+\\), .*$" 2 3 nil 1 error))
    :modes (perl-mode cperl-mode)))

(defvar flycheck-checker-php
  '(:command
    ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
      "-d" "log_errors=0" source)
    :error-patterns
    (("\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"
       2 3 nil 1 error))
    :modes php-mode))

(defvar flycheck-checker-python-flake8
  '(:command
    ("flake8" source-inplace)
    :error-patterns
    (("^\\(.*?\\):\\([0-9]+\\):\\([0-9]*\\):? \\(E.*\\)$" 1 2 3 4 error)
     ("^\\(.*?\\):\\([0-9]+\\):\\([0-9]*\\):? \\(W.*\\)$" 1 2 3 4 warning))
    :modes python-mode))

(defvar flycheck-checker-python-pylint
  '(:command
    ("epylint" source-inplace)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\): Warning (W.*): \\(.*\\)$" 1 2 nil 3 warning)
     ("^\\(.*\\):\\([0-9]+\\): Error (E.*): \\(.*\\)$" 1 2 nil 3 error)
     ("^\\(.*\\):\\([0-9]+\\): \\[F\\] \\(.*\\)$" 1 2 nil 3 error))
    :modes python-mode))

(defvar flycheck-checker-python-pyflakes
  '(:command
    ("pyflakes" source-inplace)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 warning))
    :modes python-mode))

(defvar flycheck-checker-ruby
  '(:command
    ("ruby" "-w" "-c" source)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 error))
    :modes ruby-mode))

(defvar flycheck-checker-sass
  '(:command
    ("sass" "-c" source)
    :error-patterns
    (("^Syntax error on line \\([0-9]+\\): \\(.*\\)$" nil 1 nil 2 error)
     ("^WARNING on line \\([0-9]+\\) of .*?:\r?\n\\(.*\\)$" nil 1 nil 2 warning)
     ("^Syntax error: \\(.*\\)\r?\n        on line \\([0-9]+\\) of .*?$"
      nil 2 nil 1 error))
    :modes sass-mode))

(defvar flycheck-checker-sh
  '(:command
    ("sh" "-n" source)
    :error-patterns
    (("^\\(.+\\): line \\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 error))
    :modes sh-mode
    :predicate (eq sh-shell 'sh)))

(defvar flycheck-checker-tex-chktex
  '(:command
    ("chktex" "-v0" "-q" "-I" source-inplace)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:\\(.*\\)$" 1 2 3 4 warning))
    :modes (latex-mode plain-tex-mode)))

(defvar flycheck-checker-tex-lacheck
  '(:command
    ("lacheck" source-inplace)
    :error-patterns
    (("^\"\\(.*\\)\", line \\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 warning))
    :modes latex-mode))

(defvar flycheck-checker-xml-xmlstarlet
  '(:command
    ("xmlstarlet" "val" "-e" "-q" source)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\)\\.\\([0-9]+\\): \\(.*\\)$" 1 2 3 4 error))
    :modes (xml-mode nxml-mode)))

(defvar flycheck-checker-zsh
  '(:command
    ("zsh" "-n" "-d" "-f" source)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 error))
    :modes sh-mode
    :predicate (eq sh-shell 'zsh)))

(provide 'flycheck)

;;; flycheck.el ends here
