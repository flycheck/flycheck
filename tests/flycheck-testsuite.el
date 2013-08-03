;;; flycheck-testsuite.el --- Testsuite for Flycheck -*- lexical-binding: t; -*-

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

;; Entry point of the Flycheck test suite.

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

(require 'flycheck)

(require 'ert)

(require 'cl)                           ; Required for letf used in mocker
(require 'mocker)

(require 'dash)
(require 's)

(require 'rx)
(require 'compile)                      ; For `compilation-next-error'
(require 'sh-script)                    ; For `sh-set-shell'

;; Modes used by our tests
(--each '(sh-script
          c-mode
          c++-mode
          coffee-mode
          css-mode
          elixir-mode
          erlang
          elixir-mode
          go-mode
          haml-mode
          haskell-mode
          web-mode
          js2-mode
          js3-mode
          less-css-mode
          lua-mode
          cperl-mode
          php-mode
          puppet-mode
          rust-mode
          sass-mode
          scala-mode2
          scss-mode)
    (require it nil :no-error))


;;;; Directories
(defconst flycheck-testsuite-dir
  (file-name-directory (if load-in-progress load-file-name (buffer-file-name)))
  "The testsuite directory.")

(defconst flycheck-testsuite-resources-dir
  (expand-file-name "resources" flycheck-testsuite-dir)
  "Directory of test resources.")


;;;; Testsuite resource handling
(defun flycheck-testsuite-resource-filename (resource-file)
  "Determine the absolute file name of a RESOURCE-FILE.

Relative file names are expanded against
`flycheck-testsuite-resource-dir'."
  (expand-file-name resource-file flycheck-testsuite-resources-dir))

(defun flycheck-testsuite-locate-config-file (filename _checker)
  "Find a configuration FILENAME in the testsuite.

_CHECKER is ignored."
  (let* ((directory (flycheck-testsuite-resource-filename "checkers/config-files"))
         (filepath (expand-file-name filename directory)))
    (when (file-exists-p filepath)
      filepath)))

(defmacro flycheck-testsuite-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY.

The absolute file name of RESOURCE-FILE is determined with
`flycheck-testsuite-resource-filename'."
  (declare (indent 1))
  `(let ((filename (flycheck-testsuite-resource-filename ,resource-file)))
     (should (file-exists-p filename))
     (with-temp-buffer
       (insert-file-contents filename t)
       (cd (file-name-directory filename))
       (rename-buffer (file-name-nondirectory filename))
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
     (flycheck-safe-delete-files flycheck-temp-files)
     (flycheck-safe-delete-directories flycheck-temp-directories)
     (setq flycheck-temp-files nil)
     (setq flycheck-temp-directories nil)))

(defmacro flycheck-testsuite-trap-temp-dir (dirname &rest body)
  "Trap a temporary DIRNAME inside BODY."
  (declare (indent 1))
  `(progn
     (flycheck-testsuite-delete-temps
      (should (equal flycheck-temp-directories (list ,dirname)))
      ,@body)
     (should-not (file-exists-p dirname))))

(defmacro flycheck-testsuite-trap-temp-file (filename &rest body)
  "Trap a temporary FILENAME inside BODY."
  (declare (indent 1))
  `(progn
     (flycheck-testsuite-delete-temps
       (should (equal flycheck-temp-files (list ,filename)))
       ,@body)
     (should-not (file-exists-p filename))))

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
  (if (-all? 'flycheck-check-executable checkers)
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
  (let* ((region (flycheck-error-region-for-mode error 'sexps))
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


;;;; Customization
(ert-deftest flycheck-checkers-not-empty ()
  "Test that there are any registered checkers."
  (should flycheck-checkers))

(ert-deftest flycheck-checkers-sound ()
  "Test that `flycheck-checkers' is sound.

Any checker in this list should be valid and registered."
  (dolist (checker flycheck-checkers)
    (should (flycheck-valid-checker-p checker))
    (should (flycheck-registered-checker-p checker))))

(ert-deftest flycheck-checkers-complete ()
  "Test that `flycheck-checkers' is complete.

All declared checkers should be registered."
  (let ((declared-checkers (flycheck-defined-checkers)))
    (should declared-checkers)
    (dolist (checker declared-checkers)
      (should (memq checker flycheck-checkers))
      (should (flycheck-registered-checker-p checker)))))

(ert-deftest flycheck-checkers-patterns-or-parser ()
  "Test that all `flycheck-checkers' have parser and patterns."
  (dolist (checker flycheck-checkers)
    (let ((patterns (flycheck-checker-error-patterns checker))
          (parser (flycheck-checker-error-parser checker)))
      (should checker)
      (should (or (and (eq parser 'flycheck-parse-with-patterns) patterns)
                  (null patterns))))))

(ert-deftest flycheck-locate-config-file-functions ()
  (should (equal flycheck-locate-config-file-functions
                 '(flycheck-locate-config-file-absolute-path
                   flycheck-locate-config-file-projectile
                   flycheck-locate-config-file-ancestor-directories
                   flycheck-locate-config-file-home))))

(ert-deftest flycheck-process-error-functions ()
  (should (equal flycheck-process-error-functions '(flycheck-add-overlay))))

(ert-deftest flycheck-highlighting-mode ()
  (should (eq flycheck-highlighting-mode 'sexps)))

(ert-deftest flycheck-check-syntax-automatically ()
  (should (equal flycheck-check-syntax-automatically
                 '(save idle-change new-line mode-enabled))))

(ert-deftest flycheck-display-errors-function ()
  (should (eq flycheck-display-errors-function
              #'flycheck-display-error-messages)))


;;;; Version information
(ert-deftest flycheck-library-version ()
  :expected-result (if (executable-find "carton") :passed :failed)
  (let* ((source-dir (expand-file-name ".." flycheck-testsuite-dir))
         (default-directory (file-name-as-directory source-dir))
         (version (car (process-lines "carton" "version"))))
    (should (string= version (flycheck-library-version)))))

(ert-deftest flycheck-package-version ()
  (require 'package)
  (let ((package-alist '((flycheck . [(2 12) nil "Foo"]))))
    (should (string= "2.12" (flycheck-package-version)))))

(ert-deftest flycheck-version ()
  (let* ((source-dir (expand-file-name ".." flycheck-testsuite-dir))
         (default-directory (file-name-as-directory source-dir))
         (version (car (process-lines "carton" "version"))))
    ;; Just the library version
    (should (string= version (flycheck-version)))
    ;; Library and package version
    (let ((package-alist '((flycheck . [(0 5) nil "Foo"]))))
      (should (string= (format "%s (package: 0.5)" version)
                       (flycheck-version))))))


;;;; Global syntax checking
(ert-deftest flycheck-may-enable-mode-no-undo-buffers ()
  (with-temp-buffer
    (should-not (flycheck-may-enable-mode)))
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (rename-buffer " foo")
    (should (string= (buffer-name) " foo"))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode-tramp ()
  :expected-result :failed
  (error "Implement me: Find a dummy tramp backend to use for this test!"))

(ert-deftest flycheck-may-enable-mode-no-checker-found ()
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (text-mode)
    (should-not (s-starts-with? " " (buffer-name)))
    (should-not (flycheck-get-checker-for-buffer))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode-checker-found ()
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (should (flycheck-may-enable-mode))))

(ert-deftest flycheck-global-mode-no-undo-buffers ()
  (flycheck-with-global-mode
    (with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer " foo")
      (emacs-lisp-mode)
      (should-not flycheck-mode))))

(ert-deftest flycheck-global-mode-no-checker-found ()
  (flycheck-with-global-mode
    (with-temp-buffer
      (rename-buffer "foo")
      (text-mode)
      (should-not flycheck-mode))))

(ert-deftest flycheck-global-mode-checker-found ()
  (flycheck-with-global-mode
    (with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer "foo")
      (emacs-lisp-mode)
      (should flycheck-mode))))


;;;; Deferred syntax checking
(ert-deftest flycheck-buffer-deferred ()
  "Test that deferred checking is enabled correctly."
  (with-temp-buffer
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-deferred)
    (should (flycheck-deferred-check-p))
    (flycheck-clean-deferred-check)
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-perform-deferred-syntax-check-no-check ()
  "Test that a deferred syntax check is correctly performed."
  (mocker-let
      ((flycheck-buffer-automatically (&optional error condition)
                                      ((:min-occur 0 :max-occur 0))))
    (with-temp-buffer
      (flycheck-perform-deferred-syntax-check)))
  (mocker-let
      ((flycheck-buffer-automatically (&optional error condition)
                                      ((:input '(nil nil)))))
    (with-temp-buffer
      (flycheck-buffer-deferred)
      (flycheck-perform-deferred-syntax-check))))

;;;; Automatic syntax checking
(ert-deftest flycheck-may-check-automatically ()
  (--each '(save idle-change new-line mode-enabled)
    (with-temp-buffer
      (should (flycheck-may-check-automatically it))
      (should (flycheck-may-check-automatically))
      ;; No automatic check allowed
      (let ((flycheck-check-syntax-automatically nil))
        (should-not (flycheck-may-check-automatically it))
        (should (flycheck-may-check-automatically)))
      ;; Disable just a specific event
      (let ((flycheck-check-syntax-automatically
             (remq it flycheck-check-syntax-automatically)))
        (should flycheck-check-syntax-automatically)
        (should-not (flycheck-may-check-automatically it))
        (should (flycheck-may-check-automatically))))))

(ert-deftest flycheck-check-syntax-automatically ()
  ;; If an automatic check was about to happen, we have a deferred check pending
  ;; afterwards, because the temporary resource buffer has no associated window
  ;; and is thus not checked immediately.  This allows us to neatly test whether
  ;; an automatic test happened
  (--each '(save idle-change new-line mode-enabled)
    (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
      (flycheck-mode-no-check)
      (flycheck-buffer-automatically it)
      (should (flycheck-deferred-check-p)))
    ;; Disable automatic checks completely
    (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
      (flycheck-mode-no-check)
      (let ((flycheck-check-syntax-automatically nil))
        (flycheck-buffer-automatically it))
      (should-not (flycheck-deferred-check-p)))
    ;; Disable just a specific event
    (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
      (flycheck-mode-no-check)
      (let ((flycheck-check-syntax-automatically
             (remq it flycheck-check-syntax-automatically)))
        (should flycheck-check-syntax-automatically)
        (flycheck-buffer-automatically it))
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically-mode-enabled ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically
           (remq 'mode-enabled flycheck-check-syntax-automatically)))
      (flycheck-mode))
    (should-not (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically '(mode-enabled)))
      (flycheck-mode))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-check-syntax-automatically-idle-change ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically
           (remq 'idle-change flycheck-check-syntax-automatically)))
      (insert "Hello world")
      (sleep-for 0.55))
    (should-not (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically '(idle-change)))
      (insert "Hello world")
      (sleep-for 0.55))
    (should (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically '(idle-change))
          (flycheck-idle-change-delay 1.5))
      (insert "Hello world")
      (sleep-for 0.55)
      (should-not (flycheck-deferred-check-p))
      (sleep-for 1))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-check-syntax-automatically-new-line ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically
           (remq 'new-line flycheck-check-syntax-automatically)))
      (insert "\n"))
    (should-not (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically '(new-line)))
      (insert "\n"))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-check-syntax-automatically-save ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically
           (remq 'save flycheck-check-syntax-automatically)))
      (save-buffer 0)
      (should-not (flycheck-deferred-check-p))))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically '(save)))
      (save-buffer 0))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-automatically-mode-disabled ()
  (with-temp-buffer
    (should-not flycheck-mode)
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-automatically-deferred ()
  "Test that `flycheck-buffer-safe' properly defers the check."
  (with-temp-buffer
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should (flycheck-deferred-check-p))))


;;;; Utility functions
(ert-deftest flycheck-temp-dir-system ()
  (let ((dirname (flycheck-temp-dir-system "flycheck-test")))
    (flycheck-testsuite-trap-temp-dir dirname
      (should (s-starts-with? temporary-file-directory dirname))
      (should (s-starts-with? "flycheck-test"
                              (file-name-nondirectory dirname)))
      (should (file-directory-p dirname)))))

(ert-deftest flycheck-temp-file-system-no-filename ()
  "Test `flycheck-temp-file-system' without a filename."
  (let ((filename (flycheck-temp-file-system nil "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (s-starts-with? temporary-file-directory filename))
      (should (s-starts-with? "flycheck-test" (file-name-nondirectory filename)))
      (should (file-exists-p filename)))))

(ert-deftest flycheck-temp-file-system-filename ()
  "Test `flycheck-temp-file-system' with an extension."
  (let* ((filename (flycheck-temp-file-system "spam/with/eggs.el"
                                              "flycheck-test"))
         (dirname (directory-file-name (file-name-directory filename))))
    (flycheck-testsuite-trap-temp-dir dirname
      (should (string= "eggs.el" (file-name-nondirectory filename)))
      (should (s-starts-with? temporary-file-directory filename))
      (should (s-starts-with? "flycheck-test" (file-name-nondirectory dirname)))
      (should (file-directory-p dirname)))))

(ert-deftest flycheck-temp-file-inplace-basename ()
  "Test `flycheck-temp-file-inplace' with a base name."
  (let ((filename (flycheck-temp-file-inplace "eggs.el" "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (string= filename (expand-file-name "flycheck-test-eggs.el" nil)))
      (should-not (file-exists-p filename)))))

(ert-deftest flycheck-temp-file-inplace-path ()
  "Test `flycheck-temp-file-inplace' with complete path."
  (let ((filename (flycheck-temp-file-inplace "spam/with/eggs.el"
                                              "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (string= filename (expand-file-name "flycheck-test-eggs.el"
                                                  "spam/with")))
      (should-not (file-exists-p filename)))))

(ert-deftest flycheck-temp-file-inplace-no-filename ()
  "Test `flycheck-temp-file-inplace' without a path."
  (let ((filename (flycheck-temp-file-inplace nil "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should-not (file-name-extension filename))
      (should (s-starts-with? "flycheck-test"
                              (file-name-nondirectory filename)))
      (should (file-exists-p filename)))))

(ert-deftest flycheck-same-files-p ()
  "Test `flycheck-same-files-p'."
  (should (flycheck-same-files-p "./flycheck.el" "./flycheck.el"))
  (should (flycheck-same-files-p "./flycheck.el" "flycheck.el"))
  (should-not (flycheck-same-files-p "../flycheck/flycheck.el" "tests.el")))

(ert-deftest flycheck-save-buffer-to-file ()
  "Test `flycheck-save-buffer-to-file'."
  (let ((filename (expand-file-name "tests-temp")))
    (unwind-protect
        (with-temp-buffer
          (should-not (file-exists-p filename))
          (insert "Hello world")
          (flycheck-save-buffer-to-file filename)
          (should (file-exists-p filename))
          (with-temp-buffer
            (insert-file-contents-literally filename)
            (should (string= (buffer-string) "Hello world"))))
      (ignore-errors (delete-file filename)))))

(ert-deftest flycheck-option-with-value-argument ()
  "Test concatenation of options and arguments."
  (should (equal (flycheck-option-with-value-argument "--foo" "bar")
                 '("--foo" "bar")))
  (should (equal (flycheck-option-with-value-argument "--foo=" "bar")
                 '("--foo=bar"))))

(ert-deftest flycheck-prepend-with-option ()
  (should (null (flycheck-prepend-with-option "-f" nil)))
  (should (equal (flycheck-prepend-with-option "-L" '("foo" "bar"))
                 '("-L" "foo" "-L" "bar")))
  (should (equal (flycheck-prepend-with-option "-L" '("foo" "bar") #'s-prepend)
                 '("-Lfoo" "-Lbar"))))

(ert-deftest flycheck-temporary-buffer-p ()
  (with-temp-buffer
    (should (flycheck-temporary-buffer-p)))
  (with-temp-buffer
    (rename-buffer " foo")
    (should (flycheck-temporary-buffer-p)))
  (with-temp-buffer
    (rename-buffer "foo")
    (should-not (flycheck-temporary-buffer-p))))

(ert-deftest flycheck-safe-delete-directories-recursive ()
  (let ((dirname (flycheck-temp-dir-system "flycheck-test")))
    (unwind-protect
        (let ((filename (expand-file-name "foo" dirname)))
          (process-lines "touch" filename)
          (should (s-starts-with? dirname filename))
          (should (file-exists-p filename))
          (flycheck-safe-delete-directories (list dirname))
          (should-not (file-exists-p filename))
          (should-not (file-directory-p dirname))
          (should-not (file-exists-p dirname)))
      (ignore-errors (delete-directory dirname :recursive)))))


;;;; Checker declarations
(ert-deftest flycheck-error-pattern-p ()
  (should (flycheck-error-pattern-p '("foo" . warning)))
  (should-not (flycheck-error-pattern-p '("bar" . foo)))
  (should-not (flycheck-error-pattern-p '("bar" warning)))
  (should-not (flycheck-error-pattern-p "foo"))
  (should-not (flycheck-error-pattern-p 'warning)))

(ert-deftest flycheck-command-argument-p-symbols ()
  (--each '(source source-inplace source-original temporary-directory)
    (should (flycheck-command-argument-p it))))

(ert-deftest flycheck-command-argument-p-config-file ()
  (should (flycheck-command-argument-p '(config-file "foo" bar)))
  (should-not (flycheck-command-argument-p '(config-file "foo" 'bar)))
  (should-not (flycheck-command-argument-p '(config-file "foo"))))

(ert-deftest flycheck-command-argument-p-option ()
  (should (flycheck-command-argument-p '(option "foo" bar)))
  (should (flycheck-command-argument-p '(option "foo" bar filter)))
  (should-not (flycheck-command-argument-p '(option "foo" 'bar)))
  (should-not (flycheck-command-argument-p '(option "foo" bar 'filter)))
  (should-not (flycheck-command-argument-p '(option "foo"))))

(ert-deftest flycheck-command-argument-p-option-list ()
  (should (flycheck-command-argument-p '(option-list "foo" bar)))
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn)))
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn filter)))
  (should-not (flycheck-command-argument-p '(option-list "foo" 'bar)))
  (should-not (flycheck-command-argument-p '(option-list "foo" bar 'prepend-fn)))
  (should-not (flycheck-command-argument-p '(option-list "foo"))))

(ert-deftest flycheck-command-argument-p-eval ()
  (should (flycheck-command-argument-p '(eval bar)))
  (should (flycheck-command-argument-p '(eval (bar))))
  (should-not (flycheck-command-argument-p '(eval)))
  (should-not (flycheck-command-argument-p '(eval foo bar))))

(ert-deftest flycheck-command-argument-p-invalid-argument ()
  (should-not (flycheck-command-argument-p 100))
  (should-not (flycheck-command-argument-p 'foo))
  (should-not (flycheck-command-argument-p '(foo bar))))


;;;; Checker API
(defvar flycheck-test-config-var)
(defvar flycheck-test-option-var)

(ert-deftest flycheck-substitute-argument-source ()
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (unwind-protect
        (progn
          (should (equal (flycheck-substitute-argument 'source-original 'emacs-lisp)
                         (buffer-file-name)))

          (let ((filename (flycheck-substitute-argument 'source-inplace 'emacs-lisp)))
            (should (equal filename (flycheck-testsuite-resource-filename
                                     "flycheck-substitute-dummy")))
            (should (file-exists-p filename)))

          (let ((filename (flycheck-substitute-argument 'source 'emacs-lisp)))
            (should (s-starts-with? temporary-file-directory filename))
            (should (file-exists-p filename)))))
    (flycheck-safe-delete-files flycheck-temp-files)))

(ert-deftest flycheck-substitute-argument-temporary-directory ()
  (with-temp-buffer
    (unwind-protect
        (progn
          (let ((dirname (flycheck-substitute-argument 'temporary-directory
                                                       'emacs-lisp)))
            (should (file-directory-p dirname))
            (should (s-starts-with? temporary-file-directory dirname))))
      (flycheck-safe-delete-directories flycheck-temp-directories))))

(ert-deftest flycheck-substitute-argument-config-file ()
  (let ((flycheck-test-config-var "substitute-dummy")
        (config-file (flycheck-testsuite-resource-filename "substitute-dummy")))
    (mocker-let
        ((locate-config-file-nil
          (filename checker)
          ((:input '("substitute-dummy" emacs-lisp) :output nil
                   :min-occur 2 :max-occur 2)))
         (locate-config-file-real
          (filename checker)
          ((:input '("substitute-dummy" emacs-lisp) :output config-file
                   :min-occur 2 :max-occur 2))))
      (let ((flycheck-locate-config-file-functions
             '(locate-config-file-nil locate-config-file-real)))
        (should (equal (flycheck-substitute-argument
                        '(config-file "--foo" flycheck-test-config-var)
                        'emacs-lisp)
                       (list "--foo" config-file)))
        (should (equal (flycheck-substitute-argument
                        '(config-file "--foo=" flycheck-test-config-var)
                        'emacs-lisp)
                       (list (concat "--foo=" config-file))))))))

(ert-deftest flycheck-substitute-argument-option ()
  (let ((flycheck-test-option-var "bar"))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var) 'emacs-lisp)
                   '("--foo" "bar")))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var) 'emacs-lisp)
                   '("--foo=bar"))))
  (let ((flycheck-test-option-var 200))
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var number-to-string) 'emacs-lisp)
                   '("--foo" "200")))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var number-to-string) 'emacs-lisp)
                   '("--foo=200"))))
  (let (flycheck-test-option-var)
    (should-not (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var) 'emacs-lisp))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)
    (should-error (flycheck-substitute-argument
                   '(option "--foo=" flycheck-test-option-var number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-argument-option-list ()
  (let ((flycheck-test-option-var "spam"))
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var '("spam" "eggs")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                   '("-I" "spam" "-I" "eggs")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var s-prepend) 'emacs-lisp)
                   '("-Ispam" "-Ieggs"))))
  (let ((flycheck-test-option-var '(10 20)))
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                   '("-I" "10" "-I" "20")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var
                                  s-prepend number-to-string) 'emacs-lisp)
                   '("-I10" "-I20"))))
  (let (flycheck-test-option-var)
    (should-not (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var '(nil)))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-argument-eval ()
  (let ((flycheck-test-option-var '("Hello " "World")))
    (should (equal (flycheck-substitute-argument '(eval flycheck-test-option-var) 'emacs-lisp)
                   '("Hello " "World"))))
  (should (equal (flycheck-substitute-argument '(eval (concat "Hello" "World")) 'emacs-lisp)
                 "HelloWorld"))
  (should-not (flycheck-substitute-argument
               '(eval (when (string= "foo" "bar") "yes")) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval 200) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval '("foo" 200)) 'emacs-lisp)))

(ert-deftest flycheck-substitute-argument-unknown ()
  (--each '(flycheck-substitute-argument flycheck-substitute-shell-argument)
    (should-error (funcall it '(foo "bar") 'emacs-lisp))
    (should-error (funcall it 200 'emacs-lisp))))

(ert-deftest flycheck-substitute-shell-argument-source ()
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (--each '(source source-inplace source-original)
      (should (equal (flycheck-substitute-shell-argument it 'emacs-lisp)
                     (buffer-file-name))))))

(ert-deftest flycheck-substitute-shell-argument-temporary-directory ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '(temporary-directory emacs-lisp) :output "spam with eggs"))))
    (should (equal (flycheck-substitute-shell-argument 'temporary-directory
                                                       'emacs-lisp)
                   "spam\\ with\\ eggs"))))

(ert-deftest flycheck-substitute-shell-argument-config-file ()
  (let ((filename "spam with eggs"))
    (mocker-let
        ((flycheck-substitute-argument
          (arg checker)
          ((:input '((config-file "--foo" flycheck-test-config-var) emacs-lisp)
                   :output (list "--foo" filename))
           (:input '((config-file "--foo=" flycheck-test-config-var) emacs-lisp)
                   :output (list (concat "--foo=" filename))))))
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo" flycheck-test-config-var)
                      'emacs-lisp)
                     (concat "--foo " (shell-quote-argument filename))))
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo=" flycheck-test-config-var)
                      'emacs-lisp)
                     (shell-quote-argument (concat "--foo=" filename)))))))

(ert-deftest flycheck-substitute-shell-argument-option ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '((option "--foo" flycheck-test-option-var) emacs-lisp)
                 :output '("--foo" "spam with eggs"))
         (:input '((option "--foo=" flycheck-test-option-var) emacs-lisp)
                 :output '("--foo=spam with eggs"))
         (:input '((option "--foo" flycheck-test-option-var number-to-string)
                   emacs-lisp)
                 :output '("--foo" "spam with eggs"))
         (:input '((option "--foo=" flycheck-test-option-var number-to-string)
                   emacs-lisp)
                 :output '("--foo=spam with eggs")))))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo" flycheck-test-option-var)
                    'emacs-lisp)
                   "--foo spam\\ with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo=" flycheck-test-option-var)
                    'emacs-lisp)
                   "--foo\\=spam\\ with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo" flycheck-test-option-var number-to-string)
                    'emacs-lisp)
                   "--foo spam\\ with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo=" flycheck-test-option-var number-to-string)
                    'emacs-lisp)
                   "--foo\\=spam\\ with\\ eggs"))))

(ert-deftest flycheck-substitute-shell-argument-option-list ()
  (let ((flycheck-test-option-var "spam"))
    (should-error (flycheck-substitute-shell-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var '("spam" "with eggs")))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                   "-I spam -I with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var s-prepend) 'emacs-lisp)
                   "-Ispam -Iwith\\ eggs")))
  (let ((flycheck-test-option-var '(10 20)))
    (should-error (flycheck-substitute-shell-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                   "-I 10 -I 20"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var s-prepend number-to-string) 'emacs-lisp)
                   "-I10 -I20")))
  (let (flycheck-test-option-var)
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                   "")))
  (let ((flycheck-test-option-var '(nil)))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-shell-argument
                   '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-shell-argument-eval ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '((eval foo) emacs-lisp) :output '("foo bar" "spam eggs")))))
    (should (equal (flycheck-substitute-shell-argument '(eval foo) 'emacs-lisp)
                   "foo\\ bar spam\\ eggs"))))

(ert-deftest flycheck-checker-modes ()
  (dolist (checker flycheck-checkers)
    (should (listp (flycheck-checker-modes checker)))
    (should (-all? #'symbolp (flycheck-checker-modes checker)))))

(ert-deftest flycheck-checker-executable ()
  (dolist (checker flycheck-checkers)
    (should (stringp (flycheck-checker-executable checker)))))

(ert-deftest flycheck-check-executable ()
  (dolist (checker flycheck-checkers)
    (if (executable-find (flycheck-checker-executable checker))
        (should (flycheck-check-executable checker))
      (should-not (flycheck-check-executable checker)))))


;;;; Configuration file functions
(ert-deftest flycheck-locate-config-file-absolute-path ()
  (with-temp-buffer
    (cd flycheck-testsuite-dir)
    (should-not (flycheck-locate-config-file-absolute-path "flycheck-testsuite.el"
                                                           'emacs-lisp))
    (should (equal (flycheck-locate-config-file-absolute-path "../Makefile"
                                                              'emacs-lisp)
                   (expand-file-name "../Makefile" flycheck-testsuite-dir)))))

(ert-deftest flycheck-locate-config-file-projectile ()
  (require 'projectile)
  (with-temp-buffer
    (set-visited-file-name (expand-file-name "foo" flycheck-testsuite-dir)
                           :no-query)
    (should (projectile-project-p))
    (should (equal
             (flycheck-locate-config-file-projectile "Makefile" 'emacs-lisp)
             (expand-file-name "../Makefile" flycheck-testsuite-dir)))
    (should-not (flycheck-locate-config-file-projectile "Foo" 'emacs-lisp)))
  (with-temp-buffer
    (set-visited-file-name (expand-file-name "foo" temporary-file-directory)
                           :no-query)
    (should-not (projectile-project-p))
    (should-not (flycheck-locate-config-file-projectile "Foo" 'emacs-dir))))

(ert-deftest flycheck-locate-config-file-ancestor-directories ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-testsuite.el"
                                             flycheck-testsuite-dir))
    (should-not (flycheck-locate-config-file-ancestor-directories "foo" 'emacs-lisp))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "flycheck-testrunner.el" 'emacs-lisp)
                   (expand-file-name "flycheck-testrunner.el" flycheck-testsuite-dir)))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "Makefile" 'emacs-lisp)
                   (expand-file-name "../Makefile" flycheck-testsuite-dir)))))

(ert-deftest flycheck-locate-config-file-home ()
  (let ((old-home (getenv "HOME")))
    (unwind-protect
        (progn
          (setenv "HOME" flycheck-testsuite-dir)
          (should-not (flycheck-locate-config-file-home "foo" 'emacs-lisp))
          (should-not (flycheck-locate-config-file-home "Makefile" 'emacs-lisp))
          (should (equal (flycheck-locate-config-file-home
                          "flycheck-testsuite.el" 'emacs-lisp)
                         (expand-file-name "flycheck-testsuite.el"
                                           flycheck-testsuite-dir))))
      (setenv "HOME" old-home))))


;;;; Generic option filters
(ert-deftest flycheck-option-int ()
  (should (null (flycheck-option-int nil)))
  (should (equal (flycheck-option-int 10) "10")))

(ert-deftest flycheck-option-comma-separated-list ()
  (should (null (flycheck-option-comma-separated-list nil)))
  (should (null (flycheck-option-comma-separated-list '(nil))))
  (should (null (flycheck-option-comma-separated-list '(10 20) nil (lambda (x) nil))))
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar"))
                 "foo,bar"))
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar") ":")
                 "foo:bar"))
  (should (equal (flycheck-option-comma-separated-list '(10 20) nil #'number-to-string)
                 "10,20")))


;;;; Checker selection
(ert-deftest flycheck-select-checker-automatically ()
  "Test automatic checker selection.

Have Flycheck select a checker automatically, then change the
list of registered checkers, and test that a new checker is
selected."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-testsuite-with-resource-buffer "errors-from-different-checkers.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint python-flake8))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 nil "Unused import re" 'warning)
    (should (not flycheck-checker))
    (should (eq flycheck-last-checker 'python-pylint))
    (flycheck-clear-errors)
    (should (not flycheck-current-errors))
    (setq flycheck-checkers '(python-flake8))
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 1 "F401 're' imported but unused" 'warning)
    (should (not flycheck-checker))
    (should (eq flycheck-last-checker 'python-flake8))))

(ert-deftest flycheck-select-checker ()
  "Test that checkers are properly selected.

Select two different checkers and test that each one is properly
executed, and has its errors reported."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-testsuite-with-resource-buffer "errors-from-different-checkers.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint))
    (should (-all? 'flycheck-may-use-checker flycheck-checkers))
    (should (not flycheck-checker))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 nil "Unused import re" 'warning)
    (flycheck-select-checker 'python-flake8)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 1 "F401 're' imported but unused" 'warning)
    (should (eq flycheck-checker 'python-flake8))))

(ert-deftest flycheck-select-checker-unusable ()
  "Test that selecting an unusable checker fails.

Select a checker that is not usable in a buffer, and test that an
error is signaled on all subsequent checks."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint 'bash)
  (flycheck-testsuite-with-resource-buffer "errors-from-different-checkers.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint))
    (should (not flycheck-checker))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 nil "Unused import re" 'warning)
    (let* ((error-data (should-error (flycheck-select-checker 'bash)
                                     :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr error-data)
                       "Configured syntax checker bash cannot be used"))
      (should (string= flycheck-mode-line " FlyC!"))
      ;; A subsequent syntax checker should still fail, and not fall back to
      ;; automatic selection
      (should-error (flycheck-buffer) :type flycheck-testsuite-user-error-type)
      (should (string= flycheck-mode-line " FlyC!")))))


;;;; Documentation
(defmacro flycheck-with-doc-buffer (doc-file &rest body)
  "Create a temp buffer from DOC-FILE and execute BODY."
  (declare (indent 1))
  `(let* ((filename (expand-file-name (concat "../doc/" ,doc-file)
                                      flycheck-testsuite-dir)))
     (should (file-exists-p filename))
     (with-temp-buffer
       (insert-file-contents filename)
       ,@body)))

(ert-deftest doc-all-checkers-documented ()
  "Test that all registered checkers are documented in the Flycheck manual."
  (flycheck-with-doc-buffer "checkers.texi"
    ;; Search for the beginning of the list of checkers
    (re-search-forward (rx "@itemize"))
    (dolist (checker flycheck-checkers)
      (forward-line 1)
      (should (looking-at (rx line-start "@iflyc " symbol-start
                              (group (one-or-more not-newline))
                              symbol-end line-end)))
      (should (equal (match-string 1) (symbol-name checker))))
    (forward-line 1)
    (should (looking-at (rx "@end itemize")))))

(ert-deftest doc-all-options-documented ()
  "Tests that all option variables are documented in the manual."
  (let ((config-vars (sort (-flatten (-keep #'flycheck-checker-option-vars
                                            (flycheck-defined-checkers)))
                           #'string<)))
    (flycheck-with-doc-buffer "usage.texi"
      ;; Go to the beginning of the configuration section
      (re-search-forward (rx "@node Configuration"))
      ;; Go to the beginning of the option variable listing
      (re-search-forward (rx "configured via options."))
      ;; Verify that all variables are documented
      (dolist (var config-vars)
        (re-search-forward (rx line-start "@defopt " symbol-start
                               (group (one-or-more not-newline))
                               symbol-end line-end))
        (should (equal (match-string 1) (symbol-name var)))))))

(ert-deftest doc-all-config-vars-documented ()
  "Tests that all configuration file variables are documented in the manual."
  (let ((option-file-vars (sort (-keep #'flycheck-checker-config-file-var
                                       (flycheck-defined-checkers))
                                #'string<)))
    (flycheck-with-doc-buffer "usage.texi"
      ;; Go to the beginning of the configuration section
      (re-search-forward (rx "@node Configuration"))
      ;; Go to the beginning of the option variable listing
      (re-search-forward (rx "configuration file variables"))
      ;; Verify that all variables are documented
      (dolist (var option-file-vars)
        (re-search-forward (rx line-start "@defopt " symbol-start
                               (group (one-or-more not-newline))
                               symbol-end line-end))
        (should (equal (match-string 1) (symbol-name var)))))))

(ert-deftest flycheck-describe-checker-pops-up-help ()
  "Test that describing a syntax checker pops up a help buffer."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (should (buffer-live-p (get-buffer (help-buffer))))
      (should (get-buffer-window (help-buffer)))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward (rx symbol-start (group (one-or-more not-newline))
                               symbol-end " is a Flycheck syntax checker"))
        (should (= (match-beginning 0) 1))
        (should (string= (match-string 1) (symbol-name checker)))))))

(ert-deftest flycheck-describe-checker-navigate-to-source ()
  "Test that checkers are properly described."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         (rx "`" (minimal-match (zero-or-more not-newline)) "'"))
        (should (string= (match-string 0) "`flycheck.el'"))
        (push-button (+ 2 (match-beginning 0)))
        (unwind-protect
            (progn
              (should (string= (buffer-name) "flycheck.el"))
              (should (looking-at
                       (rx line-start "("
                           symbol-start "flycheck-define-checker" symbol-end " "
                           symbol-start (group (one-or-more not-newline)) symbol-end
                           line-end)))
              (should (string= (match-string 1) (symbol-name checker))))
          (kill-buffer))))))

(ert-deftest flycheck-describe-checker-executable-name ()
  "Test that the command name appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "This\\s-+syntax\\s-+checker\\s-+executes\\s-+\"\\(.+?\\)\"\\(?:\\.\\|,\\)")
        (should (string= (match-string 1)
                         (flycheck-checker-executable checker)))))))

(ert-deftest flycheck-describe-checker-config-file-var ()
  "Test that the config file var appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (let ((config-file-var (flycheck-checker-config-file-var checker)))
          (if (not config-file-var)
              (should-not (s-contains? "configuration file"
                                       (buffer-substring (point-min) (point-max))))
            (goto-char (point-min))
            (re-search-forward
             ", using\\s-+a\\s-+configuration\\s-+file\\s-+from\\s-+`\\(.+?\\)'\\.")
            (should (equal (match-string 1) (symbol-name config-file-var)))))))))

(ert-deftest flycheck-describe-checker-option-vars ()
  "Test that option variables appear in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (let ((option-vars (sort (flycheck-checker-option-vars checker)
                                 #'string<))
              ;; The regular expression to find the beginning of the option
              ;; variable list
              (regexp "This\\s-+syntax\\s-+checker\\s-+can\\s-+be\\s-+configured\\s-+with\\s-+these\\s-+options:\n"))
          (goto-char (point-min))
          (if (not option-vars)
              ;; If there are no variables, we should not see a list of them
              (should-not (re-search-forward regexp nil :no-error))
            ;; Find the beginning of the option var listing
            (re-search-forward regexp)
            (goto-char (match-end 0))
            ;; Test that each variable is properly listed
            (dolist (var option-vars)
              (forward-line 1)
              (should (looking-at "^     \\* `\\(.+?\\)'$"))
              (should (equal (match-string 1) (symbol-name var))))
            ;; After the list of options there should be a blank line to
            ;; separate the variable list from the actual docstring
            (forward-line 1)
            (should (looking-at "^$"))))))))

(ert-deftest flycheck-describe-checker-docstring ()
  "Test that the docstring appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (should (s-contains? (flycheck-checker-documentation checker)
                             (buffer-substring (point-min) (point-max))))))))


;;;; Checker error API
(ert-deftest flycheck-error-line-region ()
  (with-temp-buffer
    (insert "Hello\n    World\n")
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 1 1))
                   '(1 . 6)))
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 2 4))
                   '(11 . 16)))
    ;; An error column beyond the end of the line is simply ignored just like
    ;; all other error columns
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 2 10))
                   '(11 . 16)))
    ;; An error line beyond the end of file should highlight the last line
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 4 3))
                   '(16 . 17)))))

(ert-deftest flycheck-error-column-region ()
  (with-temp-buffer
    (insert "Hello\n    World\n")
    (should-not (flycheck-error-column-region (flycheck-error-new-at 1 nil)))
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 1 4))
                   '(4 . 5)))
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 2 6))
                   '(12 . 13)))
    ;; A column beyond the end of a line
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 1 7))
                   '(6 . 7)))
    ;; A column right at the end of the last empty line of a file (an important
    ;; special case, because the Emacs Lisp checker reports undefined functions
    ;; at this place!)
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 3 1))
                   '(16 . 17)))
    ;; A column beyond the end of file
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 4 2))
                   '(16 . 17)))))

(ert-deftest flycheck-error-sexp-region ()
  (with-temp-buffer
    (insert "    (message)\n    (message")
    (emacs-lisp-mode)
    (should-not (flycheck-error-sexp-region (flycheck-error-new-at 1 2)))
    (should (equal (flycheck-error-sexp-region (flycheck-error-new-at 1 5))
                   '(5 . 14)))
    (should (equal (flycheck-error-sexp-region (flycheck-error-new-at 1 8))
                   '(6 . 13)))
    ;; An incomplete expression
    (should-not (flycheck-error-sexp-region (flycheck-error-new-at 2 5)))))

(ert-deftest flycheck-error-region-for-mode ()
  (with-temp-buffer
    (insert "    (message) ;; Hello world\n    (message")
    (emacs-lisp-mode)
    ;; Test an expression at the error column for all modes
    (let ((err (flycheck-error-new-at 1 7)))
      (should (equal (flycheck-error-region-for-mode err 'lines) '(5 . 29)))
      (should (equal (flycheck-error-region-for-mode err 'columns) '(7 . 8)))
      (should (equal (flycheck-error-region-for-mode err 'sexps) '(6 . 13))))
    ;; Test an error column which does not point to an expression
    (let ((err (flycheck-error-new-at 2 5)))
      (should (equal (flycheck-error-region-for-mode err 'lines) '(34 . 42)))
      (--each '(columns sexps)
        (should (equal (flycheck-error-region-for-mode err it) '(34 . 35)))))
    ;; Test an error without column for all modes
    (let ((err (flycheck-error-new-at 1 nil)))
      (--each '(lines columns sexps)
        (should (equal (flycheck-error-region-for-mode err it) '(5 . 29)))))))

(ert-deftest flycheck-error-pos ()
  (with-temp-buffer
    (insert "    Hello\n   World\n")
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 1)) 1))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 4)) 4))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 nil)) 5))
    (should (= (flycheck-error-pos (flycheck-error-new-at 2 nil)) 14))
    (should (= (flycheck-error-pos (flycheck-error-new-at 3 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 nil)) 19))))

(ert-deftest flycheck-error-format ()
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 3 5 'warning "Hello world"
                                           :checker 'emacs-lisp))
                   "3:5:warning: Hello world (emacs-lisp)"))
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 20 7 'error "Spam with eggs"
                                           :checker 'ruby))
                   "20:7:error: Spam with eggs (ruby)"))
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 nil 'warning "Oh no"
                                           :checker 'python-flake8))
                   "14:warning: Oh no (python-flake8)"))
  ;; Specific test for https://github.com/magnars/s.el/issues/34
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 15 'error "dash\\nbroken"
                                           :checker 'foo))
                   "14:15:error: dash\\nbroken (foo)")))


;;;; Error parsers
(defconst flycheck-checkstyle-xml
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<checkstyle version=\"4.3\">
  <file name=\"test-javascript/missing-semicolon.js\">
    <error line=\"3\" column=\"21\" severity=\"error\" message=\"Missing semicolon.\" source=\"Missing semicolon.\" />
    <error line=\"3\" severity=\"warning\" message=\"Implied global &apos;alert&apos;\" source=\"jshint.implied-globals\" />
  </file>
  <file name=\"test-javascript/missing-quote.js\">
    <error line=\"undefined\" column=\"undefined\" severity=\"error\" message=\"Cannot read property &apos;id&apos; of undefined\" source=\"\" />
  </file>
</checkstyle>"
  "Example Checkstyle output from jshint.")

(defconst flycheck-expected-errors
  (list
   (flycheck-error-new
    :filename "test-javascript/missing-semicolon.js"
    :line 3
    :column 21
    :level 'error
    :message "Missing semicolon.")
   (flycheck-error-new
    :filename "test-javascript/missing-semicolon.js"
    :line 3
    :column nil
    :level 'warning
    :message "Implied global 'alert'")
   (flycheck-error-new
    :filename "test-javascript/missing-quote.js"
    :line nil
    :column nil
    :level 'error
    :message "Cannot read property 'id' of undefined"))
  "Errors to be parsed from `flycheck-checkstyle-xml'.")


(ert-deftest flycheck-parse-checkstyle-xml ()
  "Test Checkstyle parsing with xml.el"
  (let ((flycheck-xml-parser 'flycheck-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-expected-errors))))

(ert-deftest flycheck-parse-checkstyle-libxml2 ()
  "Test Checkstyle parsing with libxml2."
  :expected-result (if (fboundp 'libxml-parse-xml-region) :passed :failed)
  (let ((flycheck-xml-parser 'libxml-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-expected-errors))))

(ert-deftest flycheck-parse-checkstyle-auto ()
  "Test Checkstyle parsing with the automatically chosen parsed."
  (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                 flycheck-expected-errors)))


;;;; Error overlay management
(ert-deftest flycheck-overlay-categories ()
  (--each '(flycheck-error-overlay flycheck-warning-overlay)
    (should (get it 'flycheck-overlay)))
  (should (= (get 'flycheck-error-overlay 'priority) 110))
  (should (= (get 'flycheck-warning-overlay 'priority) 100))
  (should (eq (get 'flycheck-error-overlay 'face) 'flycheck-error))
  (should (eq (get 'flycheck-warning-overlay 'face) 'flycheck-warning))
  (should (eq (get 'flycheck-error-overlay 'flycheck-fringe-face)
              'flycheck-fringe-error))
  (should (eq (get 'flycheck-warning-overlay 'flycheck-fringe-face)
              'flycheck-fringe-warning))
  (should (eq (get 'flycheck-error-overlay 'flycheck-fringe-bitmap)
              flycheck-fringe-exclamation-mark))
  (should (symbolp flycheck-fringe-exclamation-mark))
  (should (eq (get 'flycheck-warning-overlay 'flycheck-fringe-bitmap)
              'question-mark))
  (should (string= (get 'flycheck-error-overlay 'help-echo)
                   "Unknown error."))
  (should (string= (get 'flycheck-warning-overlay 'help-echo)
                   "Unknown warning.")))

(ert-deftest flycheck-add-overlay-category ()
  (with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'warning))))
      (should (eq (overlay-get overlay 'category) 'flycheck-warning-overlay)))
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'error))))
      (should (eq (overlay-get overlay 'category) 'flycheck-error-overlay)))
    (let ((err (should-error (flycheck-add-overlay (flycheck-error-new-at 1 1 'foo)))))
      (should (string= (cadr err) "Invalid error level foo")))
    (let ((err (should-error (flycheck-add-overlay (flycheck-error-new-at 1 1)))))
      (should (string= (cadr err) "Invalid error level nil")))))

(ert-deftest flycheck-add-overlay-help-echo ()
  (with-temp-buffer
    (--each '(error warning)
      (let ((overlay (flycheck-add-overlay
                      (flycheck-error-new-at 1 1 it "A bar message"))))
        (should (string= (overlay-get overlay 'help-echo) "A bar message"))))))

(ert-deftest flycheck-add-overlay-flycheck-error ()
  (with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'warning))
           (overlay (flycheck-add-overlay err)))
      (should (eq (overlay-get overlay 'flycheck-error) err)))))

(ert-deftest flycheck-add-overlay-fringe-icon ()
  (with-temp-buffer
    (insert "Hello\n    World")
    (let ((flycheck-indication-mode nil))
      (--each '(error warning)
        (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 it))))
          (should-not (overlay-get overlay 'before-string)))))
    ;; An unknown indication mode should cause no error indication
    (let ((flycheck-indication-mode 'foo))
      (--each '(error warning)
        (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 it))))
          (should-not (overlay-get overlay 'before-string)))))
    ;; Test the bitmap and face of fringe icons
    (--each '(warning error)
      (pcase-let* ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 it)))
                   (category (overlay-get overlay 'category))
                   (before-string (overlay-get overlay 'before-string))
                   (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
        (should (eq face (get category 'flycheck-fringe-face)))
        (should (eq bitmap (get category 'flycheck-fringe-bitmap)))))
    ;; Test the various indication modes
    (--each '(left-fringe right-fringe)
      (let ((flycheck-indication-mode it))
        (pcase-let* ((err (flycheck-error-new-at 1 1 'error))
                     (overlay (flycheck-add-overlay err))
                     (before-string (overlay-get overlay 'before-string))
                     (`(,side _ _) (get-text-property 0 'display before-string)))
          (should (eq side it)))))))

(ert-deftest flycheck-add-overlay-with-narrowing ()
  "Test that all overlays are added at the right positions with narrowing in place."
  (flycheck-testsuite-with-resource-buffer "narrowing.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    ;; Narrow to the function and check the buffer
    (re-search-forward "(defun .*")
    (forward-line 1)
    (narrow-to-defun)
    (should (buffer-narrowed-p))
    (flycheck-testsuite-buffer-sync)
    ;; We should have two errors highlighted between point min and max now
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 2))
    ;; Remove restrictions and test that all errors are reported
    (widen)
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 4))
    (flycheck-testsuite-should-errors
     '(9 1 "`message' called with 0 args to fill 1\n    format field(s)" warning)
     '(11 8 "`message' called with 0 args to fill 1\n    format field(s)" warning)
     '(12 nil "First sentence should end with punctuation" warning
          :checker emacs-lisp-checkdoc)
     '(15 1 "`message' called with 0 args to fill 1\n    format field(s)" warning))))


;;;; Error navigation
(defun flycheck-test-next-error-function (next-error-fn)
   (let (error-data)
      (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        (flycheck-testsuite-buffer-sync)
        (goto-char (point-min))
        (funcall next-error-fn)
        (should (= (point) 152))
        (funcall next-error-fn)
        (should (= (point) 175))
        (funcall next-error-fn)
        (should (= (point) 244))
        (setq error-data (should-error (funcall next-error-fn)
                                       :type flycheck-testsuite-user-error-type))
        (should (string= (cadr error-data) "No more Flycheck errors"))
        ;; Now try prefix argument and reset
        (funcall next-error-fn 2 t)
        (should (= (point) 175))
        ;; And a negative prefix argument now
        (funcall next-error-fn -1)
        (should (= (point) 152))
        (setq error-data (should-error (funcall next-error-fn 10)
                                       :type flycheck-testsuite-user-error-type))
        (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error-compile-mode ()
  "Test navigation to the next error by means of compile mode."
  (flycheck-test-next-error-function #'next-error))

(ert-deftest flycheck-next-error ()
  "Test navigation to the next error."
  (flycheck-test-next-error-function #'flycheck-next-error))

(defun flycheck-test-previous-error-function (previous-error-fn)
  (let (error-data)
    (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
      (emacs-lisp-mode)
      (flycheck-mode)
      (flycheck-testsuite-buffer-sync)
      (goto-char (point-max))
      (funcall previous-error-fn)
      (should (= (point) 244))
      (funcall previous-error-fn)
      (should (= (point) 175))
      (funcall previous-error-fn)
      (should (= (point) 152))
      (setq error-data (should-error (funcall previous-error-fn)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors"))
      ;; Now go to buffer end again, and try a prefix arg
      (goto-char (point-max))
      (funcall previous-error-fn 2)
      (should (= (point) 175))
      (funcall previous-error-fn -1)
      (should (= (point) 244))
      (setq error-data (should-error (funcall previous-error-fn 10)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error-compile-mode ()
  "Test navigation to the previous error by means of compile mode."
  (flycheck-test-previous-error-function #'previous-error))

(ert-deftest flycheck-previous-error ()
  "Test navigation to the previous error."
  (flycheck-test-previous-error-function #'flycheck-previous-error))

(defun flycheck-test-first-error-function (first-error-fn)
  (let (error-data)
    (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
      (emacs-lisp-mode)
      (flycheck-mode)
      (flycheck-testsuite-buffer-sync)
      (goto-char (point-max))
      (funcall first-error-fn)
      (should (= (point) 152))
      (funcall first-error-fn)
      (should (= (point) 152))
      (funcall first-error-fn 2)
      (should (= (point) 175))
      (setq error-data (should-error (funcall first-error-fn 10)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error-compile-mode ()
  "Test navigation to the first error by means of compile mode."
  (flycheck-test-first-error-function #'first-error))

(ert-deftest flycheck-first-error ()
  "Test navigation to the first error."
  (flycheck-test-first-error-function #'flycheck-first-error))

(ert-deftest flycheck-next-error-does-not-cross-narrowing ()
  "Test that error navigation does not cross restrictions"
  (flycheck-testsuite-with-resource-buffer "narrowing.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (re-search-forward "(defun .*")
    (forward-line 1)
    (narrow-to-defun 1)
    (should (buffer-narrowed-p))
    (flycheck-testsuite-buffer-sync)
    (flycheck-next-error 1 :reset)
    (should (= (point) 166))
    (flycheck-next-error)
    (should (= (point) 198))
    (should-error (flycheck-next-error)
                  :type flycheck-testsuite-user-error-type)
    (flycheck-previous-error)
    (should (= (point) 166))
    (should-error (flycheck-previous-error)
                  :type flycheck-testsuite-user-error-type)))


;;;; Error list
(ert-deftest flycheck-error-list-buffer-label ()
  (with-temp-buffer
    (rename-buffer "Foo")
    (should (string= (flycheck-error-list-buffer-label (current-buffer))
                     "#<buffer Foo>")))
  (with-temp-buffer
    (set-visited-file-name (expand-file-name "foo/bar" flycheck-testsuite-dir)
                           :no-query)
    (cd flycheck-testsuite-dir)
    (should (string= (flycheck-error-list-buffer-label (current-buffer))
                     "foo/bar"))))

(ert-deftest flycheck-error-list-error-label ()
  (with-temp-buffer
    (rename-buffer "Foo")
    (should (string= (flycheck-error-list-error-label (flycheck-error-new-at 1 1))
                     "#<buffer Foo>")))
  (with-temp-buffer
    (set-visited-file-name (expand-file-name "foo/bar" flycheck-testsuite-dir)
                           :no-query)
    (cd flycheck-testsuite-dir)
    (should (string= (flycheck-error-list-error-label (flycheck-error-new-at 1 1))
                     "foo/bar")))
  (with-temp-buffer
    (cd flycheck-testsuite-dir)
    (let* ((filename (expand-file-name "spam/with/eggs" flycheck-testsuite-dir))
           (err (flycheck-error-new-at 1 1 'warning "Foo" :filename filename)))
      (should (string= (flycheck-error-list-error-label err)
                       "spam/with/eggs")))))

(ert-deftest flycheck-error-list-insert-header ()
  (with-temp-buffer
    (rename-buffer "Foo")
    (flycheck-error-list-insert-header (current-buffer))
    (should (string= (buffer-string)
                     (format "

\C-l
*** #<buffer Foo>: Syntax and style errors (Flycheck v%s)
"
                             (flycheck-version)))))
  (with-temp-buffer
    (set-visited-file-name (expand-file-name "spam/with/eggs" flycheck-testsuite-dir)
                           :no-query)
    (cd flycheck-testsuite-dir)
    (flycheck-error-list-insert-header (current-buffer))
    (should (string= (buffer-string)
                     (format "

\C-l
*** spam/with/eggs: Syntax and style errors (Flycheck v%s)
"
                             (flycheck-version))))))

(ert-deftest flycheck-error-list-insert-errors ()
  (let (buf1 buf2)
    (with-temp-buffer
      (setq buf1 (current-buffer))
      (cd (expand-file-name ".." flycheck-testsuite-dir))
      (rename-buffer "Spam")
      (with-temp-buffer
        (setq buf2 (current-buffer))
        (set-visited-file-name (expand-file-name "spam/with/eggs" flycheck-testsuite-dir)
                               :no-query)
        (cd flycheck-testsuite-dir)
        (with-temp-buffer
          (let ((errors (list (flycheck-error-new-at 4 nil 'warning "Warning 1"
                                                     :buffer buf1 :checker 'emacs-lisp)
                              (flycheck-error-new-at 6 10 'error "Error 1"
                                                     :buffer buf2 :checker 'ruby)
                              (flycheck-error-new-at 15 8 'error "Error 2"
                                                     :buffer buf1 :checker 'python-flake8
                                                     :filename (expand-file-name "foo/bar" flycheck-testsuite-dir)))))
            (flycheck-error-list-insert-errors errors))
          (should (string= (buffer-string) "\
#<buffer Spam>:4:warning: Warning 1 (emacs-lisp)
spam/with/eggs:6:10:error: Error 1 (ruby)
foo/bar:15:8:error: Error 2 (python-flake8)
")))))))

(ert-deftest flycheck-error-list-refresh ()
  (unwind-protect
      (flycheck-testsuite-with-resource-buffer "many-errors-for-error-list.el"
        (emacs-lisp-mode)
        (flycheck-testsuite-buffer-sync)
        (flycheck-list-errors)
        (with-current-buffer (flycheck-error-list-buffer)
          (should (string= (buffer-string) (format "

\C-l
*** many-errors-for-error-list.el: Syntax and style errors (Flycheck v%s)
many-errors-for-error-list.el:7:warning: You should have a section marked \";;; Code:\" (emacs-lisp-checkdoc)
many-errors-for-error-list.el:7:1:warning: `message' called with 0
    args to fill 1 format field(s) (emacs-lisp)
many-errors-for-error-list.el:9:2:warning: princ called with 0
    arguments, but requires 1-2 (emacs-lisp)
many-errors-for-error-list.el:14:1:warning: the function
    `i-do-not-exist' is not known to be defined. (emacs-lisp)
" (flycheck-version)))))
        (with-current-buffer "many-errors-for-error-list.el"
          ;; Remove a bunch of errors
          (setq flycheck-current-errors (-drop 2 flycheck-current-errors)))
        (with-current-buffer (flycheck-error-list-buffer)
          (flycheck-error-list-refresh)
          (should (string= (buffer-string) (format "

\C-l
*** many-errors-for-error-list.el: Syntax and style errors (Flycheck v%s)
many-errors-for-error-list.el:7:warning: You should have a section marked \";;; Code:\" (emacs-lisp-checkdoc)
many-errors-for-error-list.el:7:1:warning: `message' called with 0
    args to fill 1 format field(s) (emacs-lisp)
many-errors-for-error-list.el:9:2:warning: princ called with 0
    arguments, but requires 1-2 (emacs-lisp)
many-errors-for-error-list.el:14:1:warning: the function
    `i-do-not-exist' is not known to be defined. (emacs-lisp)


\C-l
*** many-errors-for-error-list.el: Syntax and style errors (Flycheck v%s)
many-errors-for-error-list.el:9:2:warning: princ called with 0
    arguments, but requires 1-2 (emacs-lisp)
many-errors-for-error-list.el:14:1:warning: the function
    `i-do-not-exist' is not known to be defined. (emacs-lisp)
" (flycheck-version) (flycheck-version))))))
    (kill-buffer (flycheck-error-list-buffer))))

(ert-deftest flycheck-list-errors ()
  (with-temp-buffer
    (should-not flycheck-mode)
    (let ((err (should-error (flycheck-list-errors)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) "Flycheck mode not enabled"))))
  (should-not (get-buffer flycheck-error-list-buffer))
  (unwind-protect
      (flycheck-testsuite-with-resource-buffer "many-errors-for-error-list.el"
        (emacs-lisp-mode)
        (flycheck-testsuite-buffer-sync)
        (flycheck-list-errors)
        (let ((list-buffer (get-buffer flycheck-error-list-buffer)))
          (should list-buffer)
          ;; The list buffer should not be selected!
          (should-not (eq (current-buffer) list-buffer)))
        (with-current-buffer flycheck-error-list-buffer
          ;; Source buffer should be tracked
          (should (eq flycheck-error-list-source-buffer
                      (get-buffer "many-errors-for-error-list.el")))
          ;; Point must be on the beginning of the header line
          (should (looking-at "^*** many-errors-for-error-list\\.el:"))
          ;; Test the contents of the error buffer
          (should (string= (buffer-string) (format "

\C-l
*** many-errors-for-error-list.el: Syntax and style errors (Flycheck v%s)
many-errors-for-error-list.el:7:warning: You should have a section marked \";;; Code:\" (emacs-lisp-checkdoc)
many-errors-for-error-list.el:7:1:warning: `message' called with 0
    args to fill 1 format field(s) (emacs-lisp)
many-errors-for-error-list.el:9:2:warning: princ called with 0
    arguments, but requires 1-2 (emacs-lisp)
many-errors-for-error-list.el:14:1:warning: the function
    `i-do-not-exist' is not known to be defined. (emacs-lisp)
" (flycheck-version))))
          ;; Test navigation
          (compilation-next-error 1)
          (should (looking-at "^many-errors-for-error-list.el:7:warning:"))
          (compilation-next-error 1)
          (should (looking-at "^many-errors-for-error-list.el:7:1:warning:"))
          (compilation-next-error 1)
          (should (looking-at "^many-errors-for-error-list.el:9:2:warning:"))
          (compilation-next-error 1)
          (should (looking-at "^many-errors-for-error-list.el:14:1:warning:"))
          (should-error (compilation-next-error 1)))

        (kill-buffer (flycheck-error-list-buffer))
        (set-buffer "many-errors-for-error-list.el")

        ;; Test listing at current position only
        (goto-char (point-min))
        (goto-char (+ (line-beginning-position 8) 2))
        (flycheck-list-errors (point))
        (with-current-buffer flycheck-error-list-buffer
          (should (looking-at "^*** many-errors-for-error-list\\.el:"))
          ;; Test the contents of the error buffer
          (should (string= (buffer-string) (format "

\C-l
*** many-errors-for-error-list.el: Syntax and style errors (Flycheck v%s)
many-errors-for-error-list.el:9:2:warning: princ called with 0
    arguments, but requires 1-2 (emacs-lisp)
" (flycheck-version))))))
    (kill-buffer (flycheck-error-list-buffer))))


;;;; General error display
(ert-deftest flycheck-display-errors-function ()
  (should (eq flycheck-display-errors-function
              #'flycheck-display-error-messages)))

(ert-deftest flycheck-display-errors-no-function ()
  (let ((err (flycheck-error-new-at 10 20 'warning "This is a Flycheck error."))
        (flycheck-display-errors-function nil))
    ;; Error display must not fail with nil
    (with-current-buffer "*Messages*"
      (erase-buffer))
    (flycheck-display-errors (list err))
    (with-current-buffer "*Messages*"
      (should-not (s-contains? (flycheck-error-message err)
                               (buffer-string))))))

(ert-deftest flycheck-display-errors-custom-function ()
  (let ((err (flycheck-error-new-at 10 20 'warning "Foo")))
    (mocker-let
        ((display-function (errors) ((:input `((,err))))))
      (let ((flycheck-display-errors-function 'display-function))
        (flycheck-display-errors (list err))))))


;;;; Error display functions
(ert-deftest flycheck-display-error-messages ()
  (let ((err (flycheck-error-new-at 10 20 'warning
                                    "This is a Flycheck error.")))
    (with-current-buffer "*Messages*"
      (erase-buffer))
    (flycheck-display-error-messages (list err))
    (with-current-buffer "*Messages*"
      (should (s-contains? (flycheck-error-message err) (buffer-string))))))

(ert-deftest flycheck-display-errors-in-list ()
  (unwind-protect
      (flycheck-testsuite-with-resource-buffer "many-errors-for-error-list.el"
        (emacs-lisp-mode)
        (flycheck-testsuite-buffer-sync)

        (flycheck-display-errors-in-list (-take 2 flycheck-current-errors))
        (let ((list-buffer (get-buffer flycheck-error-list-buffer)))
          (should list-buffer)
          ;; The list buffer should not be selected!
          (should-not (eq (current-buffer) list-buffer)))
        (with-current-buffer (flycheck-error-list-buffer)
          (should (eq flycheck-error-list-source-buffer
                      (get-buffer "many-errors-for-error-list.el")))
          (should (looking-at "^*** many-errors-for-error-list\\.el:"))
          (should (string= (buffer-string) (format "

\C-l
*** many-errors-for-error-list.el: Syntax and style errors (Flycheck v%s)
many-errors-for-error-list.el:7:warning: You should have a section marked \";;; Code:\" (emacs-lisp-checkdoc)
many-errors-for-error-list.el:7:1:warning: `message' called with 0
    args to fill 1 format field(s) (emacs-lisp)
" (flycheck-version))))))
    (kill-buffer (flycheck-error-list-buffer))))


;;;; Working with error messages
(ert-deftest flycheck-copy-messages-as-kill ()
  (with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (-each errors #'flycheck-add-overlay)
      (mocker-let
          ((message (errors) ((:input '("1st message\n2nd message")))))
        (let ((flycheck-display-errors-function 'display-function))
          (flycheck-copy-messages-as-kill 10))))
    (should (equal (-take 2 kill-ring) '("1st message" "2nd message")))))

(ert-deftest flycheck-google-messages ()
  (with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (-each errors #'flycheck-add-overlay)
      (let ((err (should-error (flycheck-google-messages 10)
                               :type flycheck-testsuite-user-error-type)))
        (should (string= (cadr err) "Please install Google This from https://github.com/Bruce-Connor/emacs-google-this")))

      (mocker-let
          ((google-string (quote-flag s confirm)
                          ((:input '(nil "1st message" :no-confirm))
                           (:input '(nil "2nd message" :no-confirm))
                           (:input '(:quote "1st message" :no-confirm))
                           (:input '(:quote "2nd message" :no-confirm)))))
        (let* ((flycheck-google-max-messages 1)
               (err (should-error (flycheck-google-messages 10)
                                  :type flycheck-testsuite-user-error-type)))
          (should (string= (cadr err) "More than 1 messages at point")))
        (flycheck-google-messages 10)
        (flycheck-google-messages 10 :quote)))))


;;;; Checker process management
(ert-deftest flycheck-chaining-preserves-early-errors ()
  "Test that chaining preserves all errors from all checkers."
  (flycheck-testsuite-should-syntax-check
   "chained-errors.el" 'emacs-lisp-mode nil
   '(8 nil "You should have a section marked \";;; Code:\"" warning
       :checker emacs-lisp-checkdoc)
   '(8 1 "`message' called with 0 args to fill 1\n    format field(s)" warning)
   '(10 2 "princ called with 0 arguments, but\n    requires 1-2" warning)
   '(15 1 "the function `i-do-not-exist' is not\n    known to be defined." warning)))


;;;; Built-in checkers
(ert-deftest checker-bash-missing-semicolon ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'bash)
  (flycheck-testsuite-with-hook sh-mode-hook (sh-set-shell "bash" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/bash-syntax-error.bash" 'sh-mode nil
     '(5 nil "syntax error near unexpected token `fi'" error)
     '(5 nil "`fi'" error))))

(ert-deftest checker-c/c++-clang-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-warning.c" 'c-mode 'c/c++-cppcheck
   '(5 10 "unused variable 'unused'" warning)
   '(7 15 "comparison of integers of different signs: 'int' and 'unsigned int'" warning)))

(ert-deftest checker-c/c++-clang-warning-customized ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c-mode-hook
      ;; Disable conversion checks by removing -Wextra, but additionally warn
      ;; about missing prototypes, which isn't included in -Wextra
      (setq flycheck-clang-warnings '("all" "missing-prototypes"))
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-warning.c" 'c-mode 'c/c++-cppcheck
     '(3 5 "no previous prototype for function 'f'" warning)
     '(5 10 "unused variable 'unused'" warning))))

(ert-deftest checker-c/c++-clang-fatal-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-fatal-error.c" 'c-mode nil
   '(1 10 "'c_c++-clang-header.h' file not found" error)))

(ert-deftest checker-c/c++-clang-include-path ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c-mode-hook
      (setq flycheck-clang-include-path '("."))
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-fatal-error.c" 'c-mode nil)))

(ert-deftest checker-c/c++-clang-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-error.cpp" 'c++-mode nil
   '(5 18 "implicit instantiation of undefined template 'test<false>'" error)))

(ert-deftest checker-c/c++-cppcheck-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-cppcheck-error.c" 'c-mode 'c/c++-clang
   '(4 nil "Null pointer dereference" error)))

(ert-deftest checker-c/c++-cppcheck-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-cppcheck-warning.c" 'c-mode 'c/c++-clang
   '(2 nil "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0." warning)))

(ert-deftest checker-c/c++-cppcheck-style ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-cppcheck-style.c" 'c-mode 'c/c++-clang
   '(3 nil "Unused variable: unused" warning)))

(ert-deftest checker-c/c++-cppcheck-style-suppressed ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-with-hook c-mode-hook
      (setq flycheck-cppcheck-checks nil)
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-cppcheck-style.c" 'c-mode 'c/c++-clang)))

(ert-deftest checker-c/c++-cppcheck-multiple-checks ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-with-hook c++-mode-hook
      (setq flycheck-cppcheck-checks '("performance" "portability"))
      (flycheck-testsuite-should-syntax-check
       "checkers/c_c++-cppcheck-multiple-checks.cpp" 'c++-mode 'c/c++-clang
       '(2 nil "Extra qualification 'A::' unnecessary and considered an error by many compilers." warning)
       '(9 nil "Prefix ++/-- operators should be preferred for non-primitive types. Pre-increment/decrement can be more efficient than post-increment/decrement. Post-increment/decrement usually involves keeping a copy of the previous value around and adds a little extra code." warning))))

(ert-deftest checker-coffeelint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee-coffeelint)
  (flycheck-testsuite-should-syntax-check
   "checkers/coffee-coffeelint-error.coffee" 'coffee-mode nil
   '(4 nil "Throwing strings is forbidden" error)))

(ert-deftest checker-coffeelint-warning ()
  "Test a coffeelint error demoted to a warning via config file."
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee-coffeelint)
  (flycheck-testsuite-with-hook coffee-mode-hook
      (setq flycheck-coffeelintrc "coffeelint.json")
    (flycheck-testsuite-should-syntax-check
     "checkers/coffee-coffeelint-error.coffee" 'coffee-mode nil
     '(4 nil "Throwing strings is forbidden" warning))))

(ert-deftest checker-coffeelint-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee-coffeelint)
  (flycheck-testsuite-should-syntax-check
   "checkers/coffee-syntax-error.coffee" 'coffee-mode nil
   '(4 nil "missing \", starting" error :filename nil)))

(ert-deftest checker-css-csslint-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'css-csslint)
  (flycheck-testsuite-should-syntax-check
   "checkers/css-csslint-warning.css" 'css-mode nil
   '(3 6 "Heading (h1) should not be qualified." warning)))

(ert-deftest checker-css-csslint-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'css-csslint)
  (flycheck-testsuite-should-syntax-check
   "checkers/css-syntax-error.css" 'css-mode nil
   '(4 16 "Expected LBRACE at line 4, col 16." error)
   '(4 16 "Unexpected token '100%' at line 4, col 16." error)
   '(4 20 "Unexpected token ';' at line 4, col 20." error)
   '(5 1 "Unexpected token '}' at line 5, col 1." error)))

(ert-deftest checker-elixir-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-error.ex" 'elixir-mode nil
   '(5 nil "function puts/1 undefined" error)))

(ert-deftest checker-elixir-warnings ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warnings.ex" 'elixir-mode nil
   '(5 nil "variable a is unused" warning)
   '(6 nil "variable a shadowed in 'fun'" warning)
   '(14 nil "this clause cannot match because a previous clause at line 11 always matches" warning)))

(ert-deftest checker-emacs-lisp-checkdoc-warning ()
  "Test a checkdoc warning caused by a missing period in a docstring."
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-checkdoc-warning.el"
   'emacs-lisp-mode nil                 ; Checkdoc is chained after Emacs Lisp
   '(12 nil "First sentence should end with punctuation" warning
        :checker emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-checkdoc-warning-compressed ()
  "Test a checkdoc warning caused by a missing period in a docstring."
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-checkdoc-warning.el.gz"
   'emacs-lisp-mode nil
   '(12 nil "First sentence should end with punctuation" warning
        :checker emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-checkdoc-works-without-buffer-file-name ()
  "Test checkdoc checker in buffers without file names.

Regression test for https://github.com/lunaryorn/flycheck/issues/73 and
https://github.com/bbatsov/prelude/issues/259."
  (with-temp-buffer
    (insert ";;; Hello world\n(message \"foo\")")
    (emacs-lisp-mode)
    (should-not (buffer-file-name))
    (flycheck-testsuite-buffer-sync)
    ;; Just check that there are any errors, i.e. that the checker was used and
    ;; worked.
    (flycheck-testsuite-should-errors)))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-autoloads ()
  "Test that CheckDoc does not check autoloads buffers."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (rename-buffer "foo-autoloads.el")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-autoloads-source ()
  "Test that CheckDoc does no check temporary autoload buffers."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (rename-buffer " *autoload-file*")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-compiler-input ()
  "Test that CheckDoc does not check byte compiler input buffers."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (rename-buffer " *Compiler Input*")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-cask ()
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Cartony")  ; No real carton file
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Carton")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Cask")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-sytnax-error ()
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-syntax-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(3 1 "End of file during parsing" error)))

(ert-deftest checker-emacs-lisp-syntax-error-compressed ()
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-syntax-error.el.gz" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(3 1 "End of file during parsing" error)))

(ert-deftest checker-emacs-lisp-error ()
  ;; Determine how the Emacs message for load file errors looks like: In Emacs
  ;; Snapshot, the message has three parts because the underlying file error is
  ;; contained in the message.  In stable release the file error itself is
  ;; missing and the message has only two parts.
  (let* ((parts (condition-case err
                    (require 'does-not-exist)
                  (file-error (cdr err))))
         (msg (format "Cannot open load file: %sdummy-package"
                      (if (= (length parts) 2) ""
                        "no such file or directory, "))))
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
     `(3 1 ,msg error))))

(ert-deftest checker-emacs-lisp-error-load-path ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-load-path
            (list (flycheck-testsuite-resource-filename
                   "dummy-elpa/dummy-package-0.1")))
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-error-packages ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-package-user-dir
            (flycheck-testsuite-resource-filename "dummy-elpa")
            flycheck-emacs-lisp-initialize-packages t)
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-warning ()
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-warning.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(4 6 "message called with 0 arguments,\n    but requires 1+" warning)
   '(8 1 "the function `dummy-package-foo'\n    is not known to be defined." warning)))

(ert-deftest checker-emacs-lisp-warning-packages ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-package-user-dir
            (flycheck-testsuite-resource-filename "dummy-elpa")
            flycheck-emacs-lisp-initialize-packages t)
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-warning.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
     '(4 6 "message called with 0 arguments,\n    but requires 1+" warning))))

(ert-deftest checker-emacs-lisp-inhibited-no-byte-compile ()
  "Test that Emacs Lisp does not check when byte compilation is
  disabled."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (set (make-local-variable 'no-byte-compile) t)
    (should (buffer-file-name))
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-no-file-name ()
  "Test that Emacs Lisp does not check buffers without file names."
  (with-temp-buffer
    (insert "(message \"Hello World\")")
    (emacs-lisp-mode)
    (should (not (buffer-file-name)))
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-autoloads ()
  "Test that Emacs Lisp does not check autoloads buffers.

These buffers are temporary buffers generated during package
installation, which may not be byte compiled, and hence the
checker will refuse to check these.

See URL `https://github.com/lunaryorn/flycheck/issues/45' and URL
`https://github.com/bbatsov/prelude/issues/253'."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp))
    (rename-buffer "foo-autoloads.el")
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-compiler-input ()
  "Test that Emacs Lisp does not check byte compiler input buffers.

These temporary buffers are created during byte compilation, and
checking them interfers with package installation.

See URL `https://github.com/lunaryorn/flycheck/issues/45' and URL
`https://github.com/bbatsov/prelude/issues/253'."
(flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp))
    (rename-buffer " *Compiler Input*")
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-erlang-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-error.erl" 'erlang-mode nil
   '(7 nil "head mismatch" error)))

(ert-deftest checker-erlang-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-warning.erl" 'erlang-mode nil
   '(6 nil "wrong number of arguments in format call" warning)))

(ert-deftest checker-go-gofmt-syntax-error ()
  "Test a syntax error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-gofmt)
  (flycheck-testsuite-should-syntax-check
   "checkers/go-syntax-error.go" 'go-mode nil
   '(5 9 "expected '(', found 'IDENT' ta" error)
   '(6 1 "expected ')', found '}'" error)))

(ert-deftest checker-go-build-error ()
  "Test an import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-build)
  (flycheck-testsuite-should-syntax-check
   "checkers/go-testpackage/go-build-error.go" 'go-mode nil
   '(6 nil "undefined: fmt" error :checker go-build)))

(ert-deftest checker-go-test-error ()
  "Test an import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-test)
  (flycheck-testsuite-should-syntax-check
   "checkers/go-testpackage/go-test-error_test.go" 'go-mode nil
   '(8 nil "undefined: fmt" error :checker go-test)))

(ert-deftest checker-haml-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haml)
  (flycheck-testsuite-should-syntax-check
   "checkers/haml-error.haml" 'haml-mode nil
   '(5 nil "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces." error :filename nil)))

(ert-deftest checker-haskell-hdevtools-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hdevtools)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hdevtools-error.hs" 'haskell-mode nil
   '(1 8 "Not in scope: `unknown'" error
       :checker haskell-hdevtools)))

(ert-deftest checker-haskell-hdevtools-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hdevtools)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hdevtools-warning.hs" 'haskell-mode nil
   '(3 1 "Top-level binding with no type signature: foo :: Integer" warning
       :checker haskell-hdevtools)))

(ert-deftest checker-haskell-ghc-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-ghc)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-ghc-error.hs" 'haskell-mode 'haskell-hdevtools
   '(3 1 "parse error on input `module'" error
       :checker haskell-ghc)))

(ert-deftest checker-haskell-ghc-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-ghc)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-ghc-warning.hs" 'haskell-mode 'haskell-hdevtools
   '(3 1 "Top-level binding with no type signature: foo :: Integer" warning
       :checker haskell-ghc)))

(ert-deftest checker-haskell-hlint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hlint-error.hs" 'haskell-mode nil
   '(4 1 "Eta reduce\nFound:\n  warnMe xs = map lines xs\nWhy not:\n  warnMe = map lines" error
       :checker haskell-hlint)))

(ert-deftest checker-haskell-hlint-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hlint-warning.hs" 'haskell-mode nil
   '(2 8 "Redundant bracket\nFound:\n  (putStrLn \"Foobar\")\nWhy not:\n  putStrLn \"Foobar\"" warning
       :checker haskell-hlint)))

(ert-deftest checker-html-tidy-warning-and-error ()
  "Test an error caused by an unknown tag."
  :expected-result (flycheck-testsuite-fail-unless-checker 'html-tidy)
  (flycheck-testsuite-should-syntax-check
   "checkers/html-tidy-warning-and-error.html" '(html-mode web-mode) nil
   '(3 1 "missing <!DOCTYPE> declaration" warning :filename nil)
   '(8 5 "<spam> is not recognized!" error :filename nil)
   '(8 5 "discarding unexpected <spam>" warning :filename nil)))

(ert-deftest checker-javascript-jshint-syntax-error ()
  "A missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  ;; Silence JS2 and JS3 parsers
  (let ((js2-mode-show-parse-errors nil)
        (js2-mode-show-strict-warnings nil)
        (js3-mode-show-parse-errors nil))
    (flycheck-testsuite-should-syntax-check
     "checkers/javascript-jshint-syntax-error.js" '(js-mode js2-mode js3-mode) nil
     '(3 25 "Unclosed string." error)
     '(4 1 "Unclosed string." error)
     '(3 11 "Unclosed string." error)
     '(3 nil "Unused variable: 'foo'" warning)
     '(4 1 "Missing semicolon." error))))

(ert-deftest checker-javascript-jshint-error ()
  "Use eval()"
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  (flycheck-testsuite-should-syntax-check
   "checkers/javascript-jshint-error.js" '(js-mode js2-mode js3-mode) nil
   '(3 1 "eval can be harmful." error)))

(ert-deftest checker-javascript-jshint-warning ()
  "An unused variable."
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  (flycheck-testsuite-should-syntax-check
   "checkers/javascript-jshint-warning.js" '(js-mode js2-mode js3-mode) nil
   '(5 nil "Unused variable: 'foo'" warning)))

(ert-deftest checker-json-jsonlint-error ()
  "Test a syntax error from multiple top-level objects."
  :expected-result (flycheck-testsuite-fail-unless-checker 'json-jsonlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/json-jsonlint-error.json" 'text-mode nil
    '(1 42 "found: ',' - expected: 'EOF'." error)))

(ert-deftest checker-less-file-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'less)
  (flycheck-testsuite-should-syntax-check
   "checkers/less-file-error.less" 'less-css-mode nil
   '(3 1 "'no-such-file.less' wasn't found" error)))

(ert-deftest checker-less-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'less)
  (flycheck-testsuite-should-syntax-check
   "checkers/less-syntax-error.less" 'less-css-mode nil
   '(2 1 "missing closing `}`" error)))

(ert-deftest checker-lua-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'lua)
  (flycheck-testsuite-should-syntax-check
   "checkers/lua-syntax-error.lua" 'lua-mode nil
   '(5 nil "unfinished string near '\"oh no'" error)))

(ert-deftest checker-perl-error ()
  "Test an unused variable with the Perl checker."
  :expected-result (flycheck-testsuite-fail-unless-checker 'perl)
  (flycheck-testsuite-should-syntax-check
   "checkers/perl-error.pl" '(perl-mode cperl-mode) nil
   '(4 nil "Name \"main::x\" used only once: possible typo" error)))

(ert-deftest checker-perl-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'perl)
  "Test a syntax error with the Perl checker."
  (flycheck-testsuite-should-syntax-check
   "checkers/perl-syntax-error.pl" '(perl-mode cperl-mode) nil
   '(4 nil "syntax error" error)))

(ert-deftest checker-php-syntax-error ()
  "Test the T_PAAMAYIM_NEKUDOTAYIM error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'php)
  (flycheck-testsuite-should-syntax-check
   "checkers/php-syntax-error.php" 'php-mode nil
   '(8 nil "syntax error, unexpected ')', expecting '('" error)))

(ert-deftest checker-php-phpcs-error ()
  "Test an uppercase keyword error by phpcs."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-testsuite-should-syntax-check
   "checkers/php-phpcs-error.php" 'php-mode nil
   '(19 8 "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\"" error
        :checker php-phpcs)))

(ert-deftest checker-php-phpcs-error-phpcs-standard ()
  "Test an uppercase keyword error by phpcs."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-testsuite-with-hook php-mode-hook
      (setq flycheck-phpcs-standard "Zend")
    (flycheck-testsuite-should-syntax-check
     "checkers/php-phpcs-error.php" 'php-mode nil
     '(21 1 "A closing tag is not permitted at the end of a PHP file" error
          :checker php-phpcs))))

(ert-deftest checker-puppet-parser-singleline-syntax-error ()
  "Test a real syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-singleline.pp" 'puppet-mode nil
   '(3 nil "Syntax error at ','; expected '}'" error)))

(ert-deftest checker-puppet-parser-multiline-syntax-error ()
  "Test a real (multi line) syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-multiline.pp" 'puppet-mode nil
   '(8 nil "Unclosed quote after '' in 'something
}
'" error)))

(ert-deftest checker-puppet-lint-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-lint-warning.pp" 'puppet-mode nil
   '(2 nil "case statement without a default case" warning
       :checker puppet-lint)))

(ert-deftest checker-puppet-lint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-lint/error/manifests/puppet-lint-error.pp" 'puppet-mode nil
   '(2 nil "mlayout not in autoload module layout" error
       :checker puppet-lint)))

(ert-deftest checker-python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode 'python-pylint
    '(3 13 "E901 SyntaxError: invalid syntax" error)))

(ert-deftest checker-python-flake8-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-warning.py" 'python-mode 'python-pylint
    '(3 1 "F401 're' imported but unused" warning)))

(ert-deftest checker-python-flake8-warning-ignored ()
  "Test an unused import being ignored with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-warning.py" 'python-mode 'python-pylint)))

(ert-deftest checker-python-flake8-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-error.py" 'python-mode 'python-pylint
   '(6 13 "E251 unexpected spaces around keyword / parameter equals" error)
   '(6 15 "E251 unexpected spaces around keyword / parameter equals" error)))

(ert-deftest checker-python-flake8-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-error.py" 'python-mode 'python-pylint)))

(ert-deftest checker-python-flake8-warning-maximum-complexity ()
  "Test superfluous spaces with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-complexity 4)
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-warning-maximum-complexity.py"
     'python-mode 'python-pylint
     '(6 1 "C901 'foo' is too complex (4)" warning))))

(ert-deftest checker-python-flake8-error-maximum-line-length ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-line-length 50)
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-error-maximum-line-length.py"
     'python-mode 'python-pylint
     '(5 51 "E501 line too long (61 > 50 characters)" error))))

(ert-deftest checker-python-flake8-warning-naming ()
  "PEP8 compliant names with Flake8 and pep8-naming."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-warning-naming.py" 'python-mode 'python-pylint
   '(6 7 "N801 class names should use CapWords convention" warning)
   '(7 9 "N802 function name should be lowercase" warning)
   '(8 9 "N806 variable in function should be lowercase" warning)))

(ert-deftest checker-python-pylint-syntax-error ()
  "Test a real syntax error with pylint."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode 'python-flake8
   '(3 nil "invalid syntax" error)))

(ert-deftest checker-python-pylint-error ()
  "Test an unknown module with pylint."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-pylint-error.py" 'python-mode 'python-flake8
   '(3 nil "Unable to import 'spam'" error)))

(ert-deftest checker-python-pylint-used-map ()
  "Test usage of the map() builtin with the pylint checker."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-pylint-warning.py" 'python-mode 'python-flake8
   '(3 nil "Used builtin function 'map'" warning)))

(ert-deftest checker-rst-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "checkers/rst-warning.rst" 'rst-mode nil
   '(8 nil "Title underline too short." warning)
   '(11 nil "Title underline too short." warning)))

(ert-deftest checker-rst-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "checkers/rst-error.rst" 'rst-mode nil
   '(5 nil "Unknown target name: \"restructuredtext\"." error)
   '(7 nil "Unknown target name: \"cool\"." error)))

(ert-deftest checker-rst-severe ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "checkers/rst-severe.rst" 'rst-mode nil
   '(6 nil "Unexpected section title." error)
   '(11 nil "Unexpected section title." error)))

(defun flycheck-testsuite-jruby-expected-result ()
  (if (flycheck-testsuite-travis-ci-p) :failed
    (flycheck-testsuite-fail-unless-checker 'ruby-jruby)))

(ert-deftest checker-ruby-jruby-syntax-error ()
  :expected-result (flycheck-testsuite-jruby-expected-result)
  (flycheck-testsuite-not-on-travis)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode '(ruby-rubocop ruby)
   '(5 nil "syntax error, unexpected tCONSTANT" error)))

(ert-deftest checker-ruby-jruby-warning ()
  :expected-result (flycheck-testsuite-jruby-expected-result)
  (flycheck-testsuite-not-on-travis)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-warning.rb" 'ruby-mode '(ruby-rubocop ruby)
   '(3 nil "Useless use of == in void context." warning)))

(ert-deftest checker-ruby-rubocop-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode nil
   '(5 7 "unexpected token tCONSTANT" error)
   '(5 24 "unterminated string meets end of file" error)))

(ert-deftest checker-ruby-rubocop-warnings ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-rubocop-warnings.rb" 'ruby-mode nil
   '(1 1 "Missing utf-8 encoding comment." warning)
   '(3 1 "Assigned but unused variable - arr" warning)
   '(3 14 "Use snake_case for symbols." warning)
   '(4 6 "Prefer single-quoted strings when you don't need string interpolation or special symbols." warning)))

(ert-deftest checker-ruby-rubocop-warnings-disabled ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-with-hook ruby-mode-hook
      (setq flycheck-rubocoprc "rubocop.yml")
    (flycheck-testsuite-should-syntax-check
     "checkers/ruby-rubocop-warnings.rb" 'ruby-mode nil
     '(1 1 "Missing utf-8 encoding comment." warning)
     '(3 1 "Assigned but unused variable - arr" warning)
     '(4 6 "Prefer single-quoted strings when you don't need string interpolation or special symbols." warning))))

(ert-deftest checker-ruby-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode '(ruby-rubocop)
   '(5 nil "syntax error, unexpected tCONSTANT, expecting $end" error)))

(ert-deftest checker-ruby-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-warning.rb" 'ruby-mode '(ruby-rubocop)
   '(3 nil "possibly useless use of == in void context" warning)))

(ert-deftest checker-rust-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rust)
  (flycheck-testsuite-should-syntax-check
   "checkers/rust-syntax-error.rs" 'rust-mode nil
   '(3 10 "expected `{` but found `bla`" error)))

(ert-deftest checker-sass-error ()
  "Test a syntax error caused by inconsistent indentation."
  :expected-result (flycheck-testsuite-fail-unless-checker 'sass)
  (flycheck-testsuite-should-syntax-check
   "checkers/sass-error.sass" 'sass-mode nil
    '(5 nil "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces." error)))

(ert-deftest checker-scala-syntax-error ()
  :expected-result (if (flycheck-testsuite-travis-ci-p) :failed
                     (flycheck-testsuite-fail-unless-checker 'scala))
  (flycheck-testsuite-not-on-travis)
  (flycheck-testsuite-should-syntax-check
   "checkers/scala-syntax-error.scala" 'scala-mode nil
   '(3 nil "identifier expected but '{' found." error)))

(ert-deftest checker-scss-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'scss)
  (flycheck-testsuite-should-syntax-check
   "checkers/scss-error.scss" 'scss-mode nil
   '(3 nil "Invalid CSS after \"        c olor:\": expected pseudoclass or pseudoelement, was \" red;\"" error)))

(ert-deftest checker-sh-bash-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'sh-bash)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/sh-bash-syntax-error.sh" 'sh-mode '(sh-dash)
     '(3 nil "syntax error near unexpected token `('" error)
     '(3 nil "`cat <(echo blah)'" error))))

(ert-deftest checker-sh-dash-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'sh-dash)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/sh-dash-syntax-error.sh" 'sh-mode '(sh-bash)
     '(5 nil "Syntax error: \"fi\" unexpected (expecting \"then\")" error))))

(ert-deftest checker-tex-chktex-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-chktex)
  (flycheck-testsuite-should-syntax-check
   "checkers/tex-chktex-warning.tex" 'latex-mode nil
   '(9 28 "13:Intersentence spacing (`\\@') should perhaps be used." warning)))

(ert-deftest checker-tex-lacheck-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-lacheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/tex-lacheck-warning.tex" 'latex-mode 'tex-chktex
    '(9 nil "possible unwanted space at \"{\"" warning)))

(ert-deftest checker-xml-xmlstarlet-syntax-error ()
  "Test a lone closing tag."
  :expected-result (flycheck-testsuite-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-testsuite-should-syntax-check
   "checkers/xml-syntax-error.xml" 'nxml-mode nil
   '(4 10 "Opening and ending tag mismatch: spam line 3 and with" error)))

(ert-deftest checker-zsh-syntax-error ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'zsh)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "zsh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/zsh-syntax-error.zsh" 'sh-mode nil
     '(5 nil "parse error near `fi'" error))))

(provide 'flycheck-testsuite)

;; Local Variables:
;; coding: utf-8
;; End:

;;; flycheck-testsuite.el ends here
