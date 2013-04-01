;;; testhelpers.el --- Test helpers for Flycheck -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

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

;; Provide macros and functions to aid test writing.

;; Resource handling
;; -----------------
;;
;; These functions can be used to load resources within a test.
;;
;; `flycheck-with-resource-buffer' executes forms with a temporary buffer using
;; a resource file from the test suite.

;; Checking buffers
;; ----------------
;;
;; The following functions can be used to perform syntax checks in a test.
;;
;; `flycheck-buffer-sync' starts a syntax check in the current buffer and waits
;; for the syntax check to complete.
;;
;; `flycheck-wait-for-syntax-checker'  waits until the syntax check in the
;; current buffer is finished.
;;
;; `flycheck-disable-checkers' disables syntax checkers in the current buffers.

;; Expecting failures
;; ------------------
;;
;; These functions are intended for use with as value for the `:expected-result'
;; keyword to `ert-deftest'.
;;
;; `flycheck-fail-unless-checkers' marks a test as expected to fail if the given
;; syntax checkers are not installed.

;; Test predicates
;; ---------------
;;
;; `flycheck-should-errors' tests that the current buffer has ERRORS.
;;
;; `flycheck-ensure-clear' clear the error state of the current buffer, and
;; signals a test failure if clearing failed.

;; Test helpers
;; ------------
;;
;; `flycheck-windows-p' determines whether the tests are running on Windows.
;;
;; `flycheck-min-emacs-version-p' determines whether Emacs has at least the
;; given version.

;;; Code:

(require 'ert)

(require 'dash)
(require 's)

(require 'flycheck)

(defun flycheck-should-overlay (overlay error)
  "Test that OVERLAY is in REGION and corresponds to ERROR."
  (let* ((region (flycheck-error-region error))
         (message (flycheck-error-message error))
         (level (flycheck-error-level error))
         (face (if (eq level 'warning)
                   'flycheck-warning-face
                 'flycheck-error-face))
         (category (if (eq level 'warning)
                       'flycheck-warning-overlay
                     'flycheck-error-overlay))
         (fringe-icon (if (eq level 'warning)
                          '(left-fringe question-mark flycheck-warning-face)
                        `(left-fringe ,flycheck-fringe-exclamation-mark
                                      flycheck-error-face))))
    (should overlay)
    (should (overlay-get overlay 'flycheck-overlay))
    (should (= (overlay-start overlay) (car region)))
    (should (= (overlay-end overlay) (cdr region)))
    (should (eq (overlay-get overlay 'face) face))
    (should (equal (get-char-property 0 'display
                                      (overlay-get overlay 'before-string))
                   fringe-icon))
    (should (eq (overlay-get overlay 'category) category))
    (should (equal (overlay-get overlay 'flycheck-error) error))
    (should (string= (overlay-get overlay 'help-echo) message))))

(defun flycheck-should-error (expected-err)
  "Test that EXPECTED-ERR is an error in the current buffer.

Test that the error is contained in `flycheck-current-errors',
and that there is an overlay for this error at the correct
position.

EXPECTED-ERR is a list (LINE COLUMN MESSAGE LEVEL [NO-FILENAME]).
LINE and COLUMN are integers specifying the expected line and
column number respectively.  COLUMN may be nil to indicate that
the error has no column.  MESSAGE is a string with the expected
error message.  LEVEL is either `error' or `warning' and
indicates the expected error level.  If given and non-nil,
`NO-FILENAME' indicates that the error is expected to not have a
file name.

Signal a test failure if this error is not present."
  (let* ((no-filename (nth 4 expected-err))
         (real-error (flycheck-error-new
                      :buffer (current-buffer)
                      :filename (if no-filename nil (buffer-file-name))
                      :line (nth 0 expected-err)
                      :column (nth 1 expected-err)
                      :message (nth 2 expected-err)
                      :level (nth 3 expected-err)))
         (overlay (--first (equal (overlay-get it 'flycheck-error) real-error)
                           (flycheck-overlays-at (flycheck-error-pos real-error)))))
    (should (-contains? flycheck-current-errors real-error))
    (flycheck-should-overlay overlay real-error)))

(defun flycheck-should-errors (&rest errors)
  "Test that the current buffers has ERRORS.

Without ERRORS test that there are any errors in the current
buffer.

With ERRORS, test that each error in ERRORS is present in the
current buffer, and that the number of errors in the current
buffer is equal to the number of given ERRORS.

Each error in ERRORS is a list as expected by
`flycheck-should-error'."
  (if (not errors)
      (should flycheck-current-errors)
    (dolist (err errors)
      (flycheck-should-error err))
    (should (= (length errors) (length flycheck-current-errors)))))

(defvar-local flycheck-syntax-checker-finished nil
  "Non-nil if the current checker has finished.")

(add-hook 'flycheck-after-syntax-check-hook
          (lambda () (setq flycheck-syntax-checker-finished t)))

(defconst flycheck-checker-wait-time 5
  "Time to wait until a checker is finished in seconds.

After this time has elapsed, the checker is considered to have
failed, and the test aborted with failure.")

(defun flycheck-wait-for-syntax-checker ()
  "Wait until the syntax check in the current buffer is finished."
  (let ((starttime (float-time)))
    (while (and (not flycheck-syntax-checker-finished)
                (< (- (float-time) starttime) flycheck-checker-wait-time))
      (sleep-for 1))
    (unless (< (- (float-time) starttime) flycheck-checker-wait-time)
      (flycheck-stop-checker)
      (error "Syntax check did not finish after %s seconds"
             flycheck-checker-wait-time)))
  (setq flycheck-syntax-checker-finished nil))

(defun flycheck-disable-checkers (&rest checkers)
  "Disable all CHECKERS for the current buffer.

Each argument is a syntax checker symbol to be disabled for the
current buffer.

Turn `flycheck-checkers' into a buffer-local variable and remove
all CHECKERS from its definition."
  (set (make-local-variable 'flycheck-checkers)
       (--remove (memq it checkers) flycheck-checkers)))

(defun flycheck-buffer-sync ()
  "Check the current buffer synchronously."
  (setq flycheck-syntax-checker-finished nil)
  (should (not (flycheck-running-p)))
  (flycheck-mode)                       ; This will only start a deferred check,
  (flycheck-buffer)                     ; so we need an explicit manual check
  ;; After starting the check, the checker should either be running now, or
  ;; already be finished (if it was fast).
  (should (or flycheck-current-process flycheck-syntax-checker-finished))
  ;; Also there should be no deferred check pending anymore
  (should-not (flycheck-deferred-check-p))
  (flycheck-wait-for-syntax-checker))

(defun flycheck-ensure-clear ()
  "Clear the current buffer.

Raise an assertion error if the buffer is not clear afterwards."
  (flycheck-clear)
  (should (not flycheck-current-errors))
  (should (not (--any? (overlay-get it 'flycheck-overlay)
                       (overlays-in (point-min) (point-max))))))

(defmacro flycheck-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY.

If RESOURCE-FILE is a relative file name, it is expanded against
`testsuite-dir'."
  (declare (indent 1))
  `(let ((filename (expand-file-name ,resource-file testsuite-dir)))
     (should (file-exists-p filename))
     (with-temp-buffer
       (insert-file-contents filename t)
       (cd (file-name-directory filename))
       ,@body)))

(defun flycheck-fail-unless-checkers (&rest checkers)
  "Skip the test unless all CHECKERS are present on the system.

Return `:passed' if all CHECKERS are installed, or `:failed' otherwise."
  (if (-all? 'flycheck-check-executable checkers)
      :passed
    :failed))

(defalias 'flycheck-fail-unless-checker 'flycheck-fail-unless-checkers)

(defun flycheck-windows-p ()
  "Determine whether the testsuite is running on Windows."
  (memq system-type '(ms-dos windows-nt cygwin)))

(defun flycheck-min-emacs-version-p (major &optional minor)
  "Determine whether Emacs has the required version.

Return t if Emacs is at least MAJOR.MINOR, or nil otherwise."
  (when (>= emacs-major-version major)
    (or (null minor) (>= emacs-minor-version minor))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; testhelpers.el ends here
