;;; flycheck-buttercup.el --- Flycheck: Extensions to Buttercup -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2025 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;             fmdkdd <fmdkdd@gmail.com>
;; Keywords: lisp, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; Extensions to Buttercup to write BDD tests for Flycheck.
;;
;; Buttercup is a BDD testing framework for Emacs, see URL
;; `https://github.com/jorgenschaefer/emacs-buttercup/'.  Flycheck uses
;; Buttercup extensively for new tests.
;;
;; This library provides extensions to Buttercup to write Specs for Flycheck.
;;
;; * Custom matchers
;;
;; (expect 'foo :to-be-local) - Is `foo' a local variable in the current buffer?

;;; Code:

(require 'buttercup)
(require 'flycheck)
(require 'seq)
(require 'cl-lib)
(require 'macroexp)


;;; Buttercup helpers

(defun flycheck-buttercup-format-error-list (errors)
  "Format ERRORS into a human-readable string."
  (mapconcat (lambda (e) (flycheck-error-format e 'with-file-name))
             errors "\n"))


;;; Data matchers

(buttercup-define-matcher :to-be-empty-string (s)
  "Match that S is an empty string."
  (let ((s (funcall s)))
    (if (equal s "")
        (cons t (format "Expected %S not to be an empty string" s))
      (cons nil (format "Expected %S to be an empty string" s)))))

(buttercup-define-matcher :to-match-with-group (re s index match)
  "Match that regexp RE matches string S with MATCH in group INDEX."
  (let* ((re (funcall re))
         (s (funcall s))
         (index (funcall index))
         (match (funcall match))
         (matches? (string-match re s))
         (result (and matches? (match-string index s))))
    (if (and matches? (equal result match))
        (cons t (format "Expected %S not to match %S with %S in group %s"
                        re s match index))

      (cons nil (format "Expected %S to match %S with %S in group %s, %s"
                        re s match index
                        (if matches?
                            (format "but got %S" result)
                          "but did not match"))))))


;;; Emacs feature matchers

(buttercup-define-matcher :to-be-live (buffer)
  "Match that BUFFER is a live buffer."
  (let ((buffer (get-buffer (funcall buffer))))
    (if (buffer-live-p buffer)
        (cons t (format "Expected %S not to be a live buffer, but it is"
                        buffer))
      (cons nil (format "Expected %S to be a live buffer, but it is not"
                        buffer)))))

(buttercup-define-matcher :to-be-visible (buffer)
  "Match that BUFFER is displayed in a window."
  (let ((buffer (get-buffer (funcall buffer))))
    (cond
     ((and buffer (get-buffer-window buffer))
      (cons t (format "Expected %S not to be a visible buffer, but it is"
                      buffer)))
     ((not (bufferp buffer))
      (cons nil
            (format "Expected %S to be a visible buffer, but it is not a buffer"
                    buffer)))
     (t (cons
         nil
         (format "Expected %S to be a visible buffer, but it is not visible"
                 buffer))))))

(buttercup-define-matcher :to-be-local (symbol)
  "Match that SYMBOL is a buffer-local variable in the current buffer."
  (let ((symbol (funcall symbol)))
    (if (local-variable-p symbol)
        (cons t (format "Expected %S not to be a local variable, but it is"
                        symbol))
      (cons nil (format "Expected %S to be a local variable, but it is not"
                        symbol)))))

(buttercup-define-matcher :to-contain-match (buffer re)
  "Match that BUFFER contains text matching regexp RE."
  (let ((buffer (funcall buffer))
        (re (funcall re)))
    (if (not (get-buffer buffer))
        (cons nil (format "Expected %S to contain a match of %s, \
but is not a buffer" buffer re))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward re nil 'noerror)
              (cons t (format "Expected %S not to contain a match \
for %s, but it did" buffer re))
            (cons nil (format "Expected %S to contain a match for \
%s, but it did not." buffer re))))))))


;;; Flycheck matchers

(buttercup-define-matcher :to-be-equal-flycheck-errors (a b)
  "Match that flycheck error lists A and B are equal."
  (let* ((a (funcall a))
         (b (funcall b))
         (a-formatted (flycheck-buttercup-format-error-list a))
         (b-formatted (flycheck-buttercup-format-error-list b)))
    (if (equal a b)
        (cons t (format "Expected
%s
not to be equal to
%s" a-formatted b-formatted))
      (cons nil (format "Expected
%s
to be equal to
%s" a-formatted b-formatted)))))


;;; Internal variables

(defvar flycheck-buttercup--resource-directory nil
  "The directory to get resources from in this test suite.")


;;; Resource management

(defun flycheck-buttercup-resource-filename (resource-file)
  "Determine the absolute file name of a RESOURCE-FILE.

Relative file names are expanded against
`flycheck-buttercup--resource-directory'."
  (expand-file-name resource-file flycheck-buttercup--resource-directory))

(defun flycheck-buttercup-initialize (resource-dir)
  "Initialize a test suite with RESOURCE-DIR.

RESOURCE-DIR is the directory, `flycheck-buttercup-resource-filename'
should use to lookup resource files."
  (when flycheck-buttercup--resource-directory
    (error "Test suite already initialized"))
  (setq flycheck-buttercup--resource-directory resource-dir))


;;; Buffer utility macros

(defmacro flycheck-buttercup-with-temp-buffer (&rest body)
  "Eval BODY within a temporary buffer.

Like `with-temp-buffer', but resets the modification state of the
temporary buffer to make sure that it is properly killed even if
it has a backing file and is modified."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (unwind-protect
         ,(macroexp-progn body)
       ;; Reset modification state of the buffer, and unlink it from its
       ;; backing file, if any, because Emacs refuses to kill modified
       ;; buffers with backing files, even if they are temporary.
       (set-buffer-modified-p nil)
       (set-visited-file-name nil 'no-query))))

(defmacro flycheck-buttercup-with-file-buffer (file-name &rest body)
  "Create a buffer from FILE-NAME and eval BODY.

BODY is evaluated with `current-buffer' being a buffer with the
contents of FILE-NAME, its `visited-file-name' set to FILE-NAME,
and `default-directory' set to the file's directory."
  (declare (indent 1) (debug t))
  `(let ((file-name ,file-name))
     (unless (file-exists-p file-name)
       (error "%s does not exist" file-name))
     (flycheck-buttercup-with-temp-buffer
       (insert-file-contents file-name 'visit)
       (set-visited-file-name file-name 'no-query)
       (cd (file-name-directory file-name))
       ;; Mark the buffer as not modified, because we just loaded the
       ;; file up to now.
       (set-buffer-modified-p nil)
       ,@body)))

(defmacro flycheck-buttercup-with-help-buffer (&rest body)
  "Execute BODY and kill the help buffer afterwards.

Use this macro to test functions that create a Help buffer."
  (declare (indent 0))
  `(unwind-protect
       ,(macroexp-progn body)
     (when (buffer-live-p (get-buffer (help-buffer)))
       (kill-buffer (help-buffer)))))

(defmacro flycheck-buttercup-with-global-mode (&rest body)
  "Execute BODY with Global Flycheck Mode enabled.

After BODY, restore the old state of Global Flycheck Mode."
  (declare (indent 0))
  `(let ((old-state global-flycheck-mode))
     (unwind-protect
         (progn
           (global-flycheck-mode 1)
           ,@body)
       (global-flycheck-mode (if old-state 1 -1)))))

(defmacro flycheck-buttercup-with-env (env &rest body)
  "Add ENV to `process-environment' in BODY.

Execute BODY with a `process-environment' which contains all
variables from ENV added.

ENV is an alist, where each cons cell `(VAR . VALUE)' is a
environment variable VAR to be added to `process-environment'
with VALUE."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence process-environment)))
     (pcase-dolist (`(,var . ,value) ,env)
       (setenv var value))
     ,@body))

(defmacro flycheck-buttercup-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY.

The absolute file name of RESOURCE-FILE is determined with
`flycheck-buttercup-resource-filename'."
  (declare (indent 1))
  `(flycheck-buttercup-with-file-buffer
       (flycheck-buttercup-resource-filename ,resource-file)
     ,@body))


;;; Syntax checking in tests

(defvar-local flycheck-buttercup-syntax-checker-finished nil
  "Non-nil if the current checker has finished.")

(add-hook 'flycheck-after-syntax-check-hook
          (lambda ()
            (setq flycheck-buttercup-syntax-checker-finished t)))

(defconst flycheck-buttercup-checker-wait-time 10
  "Time to wait until a checker is finished in seconds.

After this time has elapsed, the checker is considered to have
failed, and the test aborted with failure.")

(define-error 'flycheck-buttercup-syntax-check-timed-out
  "Syntax check timed out.")

(define-error 'flycheck-buttercup-suspicious-checker
  "Suspicious state from checker")

(defun flycheck-buttercup-wait-for-syntax-checker ()
  "Wait until the syntax check in the current buffer is finished.

Signal `flycheck-buttercup-syntax-check-timed-out' if the check
does not complete within `flycheck-buttercup-checker-wait-time'
seconds."
  (let ((starttime (float-time)))
    (while (and (not flycheck-buttercup-syntax-checker-finished)
                (< (- (float-time) starttime)
                   flycheck-buttercup-checker-wait-time))
      (accept-process-output nil 0.02))
    (unless (< (- (float-time) starttime)
               flycheck-buttercup-checker-wait-time)
      (flycheck-stop)
      (signal 'flycheck-buttercup-syntax-check-timed-out nil)))
  (setq flycheck-buttercup-syntax-checker-finished nil))

(defun flycheck-buttercup-buffer-sync ()
  "Like `flycheck-buffer', but synchronously."
  (setq flycheck-buttercup-syntax-checker-finished nil)
  (expect (flycheck-running-p) :not :to-be-truthy)
  (flycheck-mode)                  ;; This will only start a deferred check,
  (expect (flycheck-get-checker-for-buffer) :to-be-truthy)
  (flycheck-buffer)                ;; …so we need an explicit manual check
  ;; After starting the check, the checker should either be running now, or
  ;; already be finished (if it was fast).
  (expect (or flycheck-current-syntax-check
              flycheck-buttercup-syntax-checker-finished)
          :to-be-truthy)
  ;; Also there should be no deferred check pending anymore
  (expect (flycheck-deferred-check-p) :not :to-be-truthy)
  (flycheck-buttercup-wait-for-syntax-checker))

(defun flycheck-buttercup-ensure-clear ()
  "Clear Flycheck errors and overlays from the current buffer.

Raise an assertion error if errors or overlays remain afterwards."
  (flycheck-clear)
  (expect flycheck-current-errors :not :to-be-truthy)
  (expect (seq-find (lambda (ov) (overlay-get ov 'flycheck-overlay))
                    (overlays-in (point-min) (point-max)))
          :not :to-be-truthy))


;;; Error utilities

(defun flycheck-buttercup-error-without-group (err)
  "Return a copy of ERR with the `group' property set to nil."
  (let ((copy (copy-flycheck-error err)))
    (setf (flycheck-error-group copy) nil)
    copy))

(defun flycheck-buttercup-sort-errors (errors)
  "Sort ERRORS by `flycheck-error-<'."
  (seq-sort #'flycheck-error-< errors))


;;; Test assertions

(defun flycheck-buttercup-should-overlay (error)
  "Test that ERROR has a proper overlay in the current buffer.

ERROR is a Flycheck error object."
  (let* ((overlay (seq-find
                   (lambda (ov)
                     (equal (flycheck-buttercup-error-without-group
                             (overlay-get ov 'flycheck-error))
                            (flycheck-buttercup-error-without-group error)))
                   (flycheck-overlays-in 0 (+ 1 (buffer-size)))))
         (region
          ;; Overlays of errors from other files are on the first line
          (if (flycheck-relevant-error-other-file-p error)
              (cons (point-min)
                    (save-excursion (goto-char (point-min))
                                    (line-end-position)))
            (flycheck-error-region-for-mode error 'symbols)))
         (level (flycheck-error-level error))
         (category (flycheck-error-level-overlay-category level))
         (face (get category 'face))
         (fringe-bitmap (flycheck-error-level-fringe-bitmap level))
         (fringe-face (flycheck-error-level-fringe-face level))
         (fringe-icon (list 'left-fringe fringe-bitmap fringe-face)))
    (expect overlay :to-be-truthy)
    (expect (overlay-get overlay 'flycheck-overlay) :to-be-truthy)
    (expect (overlay-start overlay) :to-equal (car region))
    (expect (overlay-end overlay) :to-equal (cdr region))
    (expect (overlay-get overlay 'face) :to-equal face)
    (expect (get-char-property 0 'display
                               (overlay-get overlay 'before-string))
            :to-equal fringe-icon)
    (expect (overlay-get overlay 'category) :to-equal category)
    (expect (flycheck-buttercup-error-without-group
             (overlay-get overlay 'flycheck-error))
            :to-equal
            (flycheck-buttercup-error-without-group error))))

(defun flycheck-buttercup-should-errors (&rest errors)
  "Test that the current buffer has ERRORS.

ERRORS is a list of errors expected to be present in the current
buffer.  Each error is given as a list of arguments to
`flycheck-error-new-at'.

If ERRORS are omitted, test that there are no errors at all in
the current buffer.

With ERRORS, test that each error in ERRORS is present in the
current buffer, and that the number of errors in the current
buffer is equal to the number of given ERRORS.  In other words,
check that the buffer has all ERRORS, and no other errors."
  (let ((expected (flycheck-buttercup-sort-errors
                   (mapcar (apply-partially #'apply #'flycheck-error-new-at)
                           errors)))
        (current (flycheck-buttercup-sort-errors flycheck-current-errors)))
    (expect (mapcar #'flycheck-buttercup-error-without-group expected)
            :to-equal
            (mapcar #'flycheck-buttercup-error-without-group current))
    ;; Check that related errors are the same
    (cl-mapcar
     (lambda (err1 err2)
       (expect (flycheck-buttercup-sort-errors
                (mapcar #'flycheck-buttercup-error-without-group
                        (flycheck-related-errors err1 expected)))
               :to-equal
               (flycheck-buttercup-sort-errors
                (mapcar #'flycheck-buttercup-error-without-group
                        (flycheck-related-errors err2)))))
     expected current)
    (mapc #'flycheck-buttercup-should-overlay expected))
  (expect (length errors)
          :to-equal
          (length (flycheck-overlays-in (point-min) (point-max)))))

(defun flycheck-buttercup-should-syntax-check-in-buffer (&rest errors)
  "Test a syntax check in the current buffer, expecting ERRORS.

This is like `flycheck-buttercup-should-syntax-check', but with a
buffer in the right mode instead of a file."
  ;; Load safe file-local variables because some tests depend on them
  (let ((enable-local-variables :safe)
        ;; Disable all hooks at this place, to prevent 3rd party packages
        ;; from interfering
        (hack-local-variables-hook))
    (hack-local-variables))
  ;; Configure config file locating for unit tests
  (let ((process-hook-called 0)
        (suspicious nil))
    (add-hook 'flycheck-process-error-functions
              (lambda (_err)
                (setq process-hook-called (1+ process-hook-called))
                nil)
              nil :local)
    (add-hook 'flycheck-status-changed-functions
              (lambda (status)
                (when (eq status 'suspicious)
                  (setq suspicious t)))
              nil :local)
    (flycheck-buttercup-buffer-sync)
    (when suspicious
      (signal 'flycheck-buttercup-suspicious-checker nil))
    (apply #'flycheck-buttercup-should-errors errors)
    (expect process-hook-called :to-equal (length errors)))
  (flycheck-buttercup-ensure-clear))

(defun flycheck-buttercup-should-syntax-check (resource-file modes &rest errors)
  "Test a syntax check in RESOURCE-FILE with MODES.

RESOURCE-FILE is the file to check.  MODES is a single major mode
symbol or a list thereof, specifying the major modes to syntax
check with.  If more than one major mode is specified, the test
is run for each mode separately, so if you give three major
modes, the entire test will run three times.  ERRORS is the list
of expected errors, as in `flycheck-buttercup-should-errors'.  If
omitted, the syntax check must not emit any errors.  The errors
are cleared after each test.

The syntax checker is selected via standard syntax checker
selection.  To test a specific checker, you need to set
`flycheck-checker' or `flycheck-disabled-checkers' accordingly
before using this predicate, depending on whether you want to use
manual or automatic checker selection.

During the syntax check, configuration files of syntax checkers
are also searched in the `config-files' sub-directory of the
resource directory."
  (when (symbolp modes)
    (setq modes (list modes)))
  (dolist (mode modes)
    (unless (fboundp mode)
      (buttercup-skip (format "%S missing" mode)))
    (flycheck-buttercup-with-resource-buffer resource-file
      (funcall mode)
      (apply #'flycheck-buttercup-should-syntax-check-in-buffer errors))))


;;; Additional matchers

(buttercup-define-matcher :to-be-at-flycheck-error (point-val n-val)
  "Match that POINT-VAL is at the N-VAL'th Flycheck error."
  (let* ((pt (funcall point-val))
         (n (funcall n-val))
         (error (nth (1- n) flycheck-current-errors))
         (mode flycheck-highlighting-mode)
         (region (flycheck-error-region-for-mode error mode))
         (at-error (and (member error (flycheck-overlay-errors-at pt))
                        (= pt (car region)))))
    (if at-error
        (cons t (format "Expected point %s not to be at error %s, but it is"
                        pt n))
      (let ((errors (flycheck-overlay-errors-at pt)))
        (if (null errors)
            (cons nil
                  (format "Expected to be at error %s, but no error at point %s"
                          n pt))
          (let ((pos (cl-position (car errors) flycheck-current-errors)))
            (cons nil
                  (format "Expected to be at point %s and error %s, \
but point %s is at error %s"
                          (car (flycheck-error-region-for-mode
                                (nth (1- n) flycheck-current-errors)
                                flycheck-highlighting-mode))
                          n pt (1+ pos)))))))))


;;; Test case definitions

(defun flycheck-buttercup--parse-keys-and-body (keys-and-body)
  "Parse KEYS-AND-BODY into keyword-value pairs and body forms.

KEYS-AND-BODY is a list of alternating keyword-value pairs
followed by body forms.  Return a cons cell (KEYS . BODY)."
  (let ((keys nil)
        (body keys-and-body))
    (while (keywordp (car body))
      (let ((key (pop body))
            (val (pop body)))
        (push key keys)
        (push val keys)))
    (cons (nreverse keys) body)))

(defmacro flycheck-buttercup-def-checker-test (checker language name
                                                       &rest keys-and-body)
  "Define a buttercup test case for a syntax CHECKER for LANGUAGE.

CHECKER is a symbol or a list of symbols denoting syntax checkers
being tested by the test.  The test case is skipped, if any of
these checkers cannot be used.  LANGUAGE is a symbol or a list of
symbols denoting the programming languages supported by the
syntax checkers.  This is currently only used for naming the test
appropriately.

NAME is a symbol denoting the local name of the test.  The test
itself is ultimately named
`flycheck-define-checker/CHECKER/NAME'.

Optionally, the keyword arguments `:tags' and `:expected-result'
may be given.  `:expected-result :failed' will generate an `xit'
instead of `it' to mark the test as pending.

The remaining forms KEYS-AND-BODY denote the body of the test
case, including assertions and setup code."
  (declare (indent 3))
  (unless checker
    (error "No syntax checkers specified"))
  (unless language
    (error "No languages specified"))
  (let* ((checkers (if (symbolp checker) (list checker) checker))
         (checker (car checkers))
         (local-name (or name 'default))
         (full-name (format "flycheck-define-checker/%s/%s"
                            checker local-name))
         (parsed (flycheck-buttercup--parse-keys-and-body keys-and-body))
         (keys (car parsed))
         (body (cdr parsed))
         (expected-result (plist-get keys :expected-result))
         (it-fn (if (eq expected-result :failed) 'xit 'it))
         (skip-forms (mapcar (lambda (c)
                               `(assume
                                 ;; Ignore non-command checkers
                                 (or (not (flycheck-checker-get ',c 'command))
                                     (executable-find
                                      (flycheck-checker-executable ',c)))))
                             checkers)))
    `(,it-fn ,full-name
             (lambda ()
               ,@skip-forms
               ,@body))))

(provide 'flycheck-buttercup)

;; Disable byte compilation for this library, to prevent package.el choking on a
;; missing `buttercup' library.  See
;; https://github.com/flycheck/flycheck/issues/860

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; flycheck-buttercup.el ends here
