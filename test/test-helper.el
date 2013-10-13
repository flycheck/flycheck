;;; test-helper.el --- Test helpers for Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

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

;; Unit test helpers for Flycheck.

;; Test discovery and loading
;; --------------------------
;;
;; Find and load all tests.
;;
;; Test files and directories must match the
;; `flycheck-testsuite-test-file-pattern'.  Essentially this pattern matches all
;; directories and files starting with the prefix "test-".
;;
;; `flycheck-testsuite-load-tests' finds and loads all tests.

;; Resource handling
;; -----------------
;;
;; These functions can be used to load resources within a test.
;;
;; `flycheck-testsuite-resource-filename' gets the file name of resources.
;;
;; `flycheck-testsuite-with-resource-buffer' executes forms with a temporary
;; buffer using a resource file from the test suite.

;; Test helpers
;; ------------
;;
;; `flycheck-testsuite-with-hook' executes the body with a specified hook form.
;;
;; `flycheck-mode-no-check' enables Flycheck Mode in the current buffer without
;; starting a syntax check immediately.
;;
;; `flycheck-with-global-mode' enables Global Flycheck Mode while the body is
;; evaluated.
;;
;; `flycheck-testsuite-delete-temps' deletes Flycheck temporary files after the
;; body was evaluated.
;;
;; `flycheck-testsuite-with-help-buffer' kills the Help buffer after the body
;; was evaluated.
;;
;; `flycheck-testsuite-not-on-travis' signals an error if the test is run on
;; Travis CI.  Use in combination with `:expected-result' to skip tests on
;; Travis CI.

;; Test environment utilities
;; --------------------------
;;
;; `flycheck-testsuite-windows-p' determines whether the tests are running on
;; Windows.
;;
;; `flycheck-testsuite-min-emacs-version-p' determines whether Emacs has at
;; least the given version.
;;
;; `flycheck-testsuite-user-error-type' provides the type of `user-error',
;; depending on the Emacs version.

;; Test results
;; ------------
;;
;; The following functions are intended for use with as value for the
;; `:expected-result' keyword to `ert-deftest'.
;;
;; `flycheck-testsuite-fail-unless-checkers' marks a test as expected to fail if
;; the given syntax checkers are not installed.

;; Checking buffers
;; ----------------
;;
;; The following functions can be used to perform syntax checks in a test.
;;
;; `flycheck-testsuite-buffer-sync' starts a syntax check in the current buffer
;; and waits for the syntax check to complete.
;;
;; `flycheck-testsuite-wait-for-syntax-checker' waits until the syntax check in
;; the current buffer is finished.
;;
;; `flycheck-testsuite-disable-checkers' disables syntax checkers in the current
;; buffers.

;; Test predicates
;; ---------------
;;
;; `flycheck-testsuite-should-errors' tests that the current buffer has ERRORS.
;;
;; `flycheck-testsuite-should-syntax-check' tests that a syntax check results in
;; the specified errors.
;;
;; `flycheck-testsuite-ensure-clear' clear the error state of the current
;; buffer, and signals a test failure if clearing failed.

;;; Code:


;;;; Directories
(require 'f)

(eval-and-compile
  (defconst flycheck-testsuite-dir (f-parent (f-this-file))
    "The testsuite directory.")

  (defconst flycheck-testsuite-resources-dir
    (f-join flycheck-testsuite-dir "resources")
    "Directory of test resources."))


;;;; Requires

(require 'flycheck (f-join flycheck-testsuite-dir "../flycheck"))
(require 'ert)


;;;; Testsuite resource handling
(defun flycheck-testsuite-resource-filename (resource-file)
  "Determine the absolute file name of a RESOURCE-FILE.

Relative file names are expanded against
`flycheck-testsuite-resource-dir'."
  (f-join flycheck-testsuite-resources-dir resource-file))

(defun flycheck-testsuite-locate-config-file (filename _checker)
  "Find a configuration FILENAME in the testsuite.

_CHECKER is ignored."
  (let* ((directory (flycheck-testsuite-resource-filename "checkers/config-files"))
         (filepath (expand-file-name filename directory)))
    (when (f-exists? filepath)
      filepath)))

(defmacro flycheck-testsuite-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY.

The absolute file name of RESOURCE-FILE is determined with
`flycheck-testsuite-resource-filename'."
  (declare (indent 1))
  `(let ((filename (flycheck-testsuite-resource-filename ,resource-file)))
     (should (f-exists? filename))
     (with-temp-buffer
       (insert-file-contents filename t)
       (cd (f-parent filename))
       (rename-buffer (f-filename filename))
       ,@body)))


;;;; Test helpers
(defmacro flycheck-testsuite-with-hook (hook-var form &rest body)
  "Set HOOK-VAR to FORM and evaluate BODY.

HOOK-VAR is a hook variable or a list thereof, which is set to
FORM before evaluating BODY.

After evaluation of BODY, set HOOK-VAR to nil."
  (declare (indent 2))
  `(let ((hooks (quote ,(if (listp hook-var) hook-var (list hook-var)))))
     (unwind-protect
          (progn
            (--each hooks (add-hook it #'(lambda () ,form)))
            ,@body)
        (--each hooks (set it nil)))))

(defun flycheck-mode-no-check ()
  "Enable Flycheck mode without checking automatically."
  (emacs-lisp-mode)
  (let ((flycheck-check-syntax-automatically nil))
    (flycheck-mode))
  (should-not (flycheck-deferred-check-p)))

(defmacro flycheck-with-global-mode (&rest body)
  "Execute BODY with Global Flycheck Mode enabled."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (global-flycheck-mode 1)
         ,@body)
     (global-flycheck-mode -1)))

(defmacro flycheck-testsuite-delete-temps (&rest body)
  "Execute BODY and delete Flycheck's temporary files."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (flycheck-safe-delete-temporaries)))

(defmacro flycheck-testsuite-trap-temp-dir (dirname &rest body)
  "Trap a temporary DIRNAME inside BODY."
  (declare (indent 1))
  `(progn
     (flycheck-testsuite-delete-temps
       (should (member ,dirname flycheck-temporaries))
       ,@body)
     (should-not (f-exists? dirname))))

(defmacro flycheck-testsuite-trap-temp-file (filename &rest body)
  "Trap a temporary FILENAME inside BODY."
  (declare (indent 1))
  `(progn
     (flycheck-testsuite-delete-temps
       (should (member ,filename flycheck-temporaries))
       ,@body)
     (should-not (f-exists? filename))))

(defmacro flycheck-testsuite-with-help-buffer (&rest body)
  "Execute BODY and kill the help buffer afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when (buffer-live-p (get-buffer (help-buffer)))
       (kill-buffer (help-buffer)))))

(defun flycheck-testsuite-not-on-travis ()
  "Signal an error if run on Travis CI.

Use together with `:expected-result' to skip tests on travis CI."
  (when (flycheck-testsuite-travis-ci-p)
    (error "Test skipped on Travis CI.")))


;;;; Test environment helpers
(defun flycheck-testsuite-windows-p ()
  "Determine whether the testsuite is running on Windows."
  (memq system-type '(ms-dos windows-nt cygwin)))

(defun flycheck-testsuite-travis-ci-p ()
  "Determine whether the testsuite is running on Travis CI."
  (string= (getenv "TRAVIS") "true"))

(defun flycheck-testsuite-vagrant-p ()
  "Whether the testsuite is running on our testing VM."
  (string= (user-login-name) "vagrant"))

(defun flycheck-testsuite-min-emacs-version-p (major &optional minor)
  "Determine whether Emacs has the required version.

Return t if Emacs is at least MAJOR.MINOR, or nil otherwise."
  (when (>= emacs-major-version major)
    (or (null minor) (>= emacs-minor-version minor))))

(defconst flycheck-testsuite-user-error-type
  (if (flycheck-testsuite-min-emacs-version-p 24 3) 'user-error 'error)
  "The `user-error' type used by Flycheck.")


;;;; Test results
(defun flycheck-testsuite-fail-unless-checkers (&rest checkers)
  "Skip the test unless all CHECKERS are present on the system.

Return `:passed' if all CHECKERS are installed, or `:failed' otherwise."
  (if (or (flycheck-testsuite-travis-ci-p)
          (flycheck-testsuite-vagrant-p)
          (-all? 'flycheck-check-executable checkers))
      :passed
    :failed))

(defalias 'flycheck-testsuite-fail-unless-checker
  'flycheck-testsuite-fail-unless-checkers)


;;;; Checking buffers
(defvar-local flycheck-testsuite-syntax-checker-finished nil
  "Non-nil if the current checker has finished.")

(add-hook 'flycheck-after-syntax-check-hook
          (lambda () (setq flycheck-testsuite-syntax-checker-finished t)))

(defconst flycheck-testsuite-checker-wait-time 5
  "Time to wait until a checker is finished in seconds.

After this time has elapsed, the checker is considered to have
failed, and the test aborted with failure.")

(defun flycheck-testsuite-wait-for-syntax-checker ()
  "Wait until the syntax check in the current buffer is finished."
  (let ((starttime (float-time)))
    (while (and (not flycheck-testsuite-syntax-checker-finished)
                (< (- (float-time) starttime) flycheck-testsuite-checker-wait-time))
      (sleep-for 1))
    (unless (< (- (float-time) starttime) flycheck-testsuite-checker-wait-time)
      (flycheck-stop-checker)
      (error "Syntax check did not finish after %s seconds"
             flycheck-testsuite-checker-wait-time)))
  (setq flycheck-testsuite-syntax-checker-finished nil))

(defun flycheck-testsuite-buffer-sync ()
  "Check the current buffer synchronously."
  (setq flycheck-testsuite-syntax-checker-finished nil)
  (should (not (flycheck-running-p)))
  (flycheck-mode)                       ; This will only start a deferred check,
  (flycheck-buffer)                     ; so we need an explicit manual check
  ;; After starting the check, the checker should either be running now, or
  ;; already be finished (if it was fast).
  (should (or flycheck-current-process
              flycheck-testsuite-syntax-checker-finished))
  ;; Also there should be no deferred check pending anymore
  (should-not (flycheck-deferred-check-p))
  (flycheck-testsuite-wait-for-syntax-checker))

(defun flycheck-testsuite-ensure-clear ()
  "Clear the current buffer.

Raise an assertion error if the buffer is not clear afterwards."
  (flycheck-clear)
  (should (not flycheck-current-errors))
  (should (not (--any? (overlay-get it 'flycheck-overlay)
                       (overlays-in (point-min) (point-max))))))


;;;; Test predicates
(defun flycheck-testsuite-should-overlay (overlay error)
  "Test that OVERLAY is in REGION and corresponds to ERROR."
  (let* ((region (flycheck-error-region-for-mode error 'symbols))
         (message (flycheck-error-message error))
         (level (flycheck-error-level error))
         (face (if (eq level 'warning)
                   'flycheck-warning
                 'flycheck-error))
         (category (if (eq level 'warning)
                       'flycheck-warning-overlay
                     'flycheck-error-overlay))
         (fringe-icon (if (eq level 'warning)
                          '(left-fringe question-mark flycheck-fringe-warning)
                        `(left-fringe ,flycheck-fringe-exclamation-mark
                                      flycheck-fringe-error))))
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

(defun flycheck-testsuite-should-error (line column message level &rest properties)
  "Test that EXPECTED-ERR is an error in the current buffer.

Test that the error is contained in `flycheck-current-errors',
and that there is an overlay for this error at the correct
position.

LINE, COLUMN, MESSAGE and LEVEL are the expected properties of
the error.  PROPERTIES specify additional properties of the expected ERROR.

Signal a test failure if this error is not present."
  (let* ((filename (-if-let (member (plist-member properties :filename))
                     (cadr member) (buffer-file-name)))
         (checker (-if-let (member (plist-member properties :checker))
                    (cadr member) (or flycheck-checker flycheck-last-checker)))
         (buffer (or (plist-get properties :buffer) (current-buffer)))
         (real-error (flycheck-error-new
                      :buffer buffer :filename filename :checker checker
                      :line line :column column :message message :level level))
         (overlay (--first (equal (overlay-get it 'flycheck-error) real-error)
                           (flycheck-overlays-in 0 (+ 1 (buffer-size))))))
    (should (-contains? flycheck-current-errors real-error))
    (flycheck-testsuite-should-overlay overlay real-error)))

(defun flycheck-testsuite-should-errors (&rest errors)
  "Test that the current buffers has ERRORS.

Without ERRORS test that there are any errors in the current
buffer.

With ERRORS, test that each error in ERRORS is present in the
current buffer, and that the number of errors in the current
buffer is equal to the number of given ERRORS.

Each error in ERRORS is a list as expected by
`flycheck-testsuite-should-error'."
  (if (not errors)
      (should flycheck-current-errors)
    (dolist (err errors)
      (apply #'flycheck-testsuite-should-error err))
    (should (= (length errors) (length flycheck-current-errors)))
    (should (= (length errors)
               (length (flycheck-overlays-in (point-min) (point-max)))))))

(defun flycheck-testsuite-should-syntax-check
  (resource-file modes disabled-checkers &rest errors)
  "Test a syntax check in RESOURCE-FILE with MODES.

RESOURCE-FILE is the file to check.  MODES is a single major mode
symbol or a list thereof, specifying the major modes to syntax
check with.  DISABLED-CHECKERS is a syntax checker or a list
thereof to disable before checking the file.  ERRORS is the list
of expected errors."
  (when (symbolp modes)
    (setq modes (list modes)))
  (when (symbolp disabled-checkers)
    (setq disabled-checkers (list disabled-checkers)))
  (--each modes
    (let ((flycheck-checkers (--remove (memq it disabled-checkers)
                                       flycheck-checkers)))
     (flycheck-testsuite-with-resource-buffer resource-file
       (funcall it)
       ;; Configure config file locating for unit tests
       (--each '(flycheck-locate-config-file-absolute-path
                 flycheck-testsuite-locate-config-file)
         (add-hook 'flycheck-locate-config-file-functions it :append :local))
       (let ((process-hook-called 0))
         (add-hook 'flycheck-process-error-functions
                   (lambda (_err)
                     (setq process-hook-called (1+ process-hook-called))
                     nil)
                   nil :local)
         (flycheck-testsuite-buffer-sync)
         (should (= process-hook-called (length errors))))
       (if errors
           (apply #'flycheck-testsuite-should-errors errors)
         (should-not flycheck-current-errors))
       (flycheck-testsuite-ensure-clear)))))

(provide 'test-helper)

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-helper.el ends here
