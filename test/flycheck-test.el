;;; flycheck-test.el --- Flycheck: Unit test suite   -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

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

;; Unit test suite of Flycheck

;; Please keep test cases in the same order as the corresponding definition in
;; flycheck.

;;; Code:


;;;; Requirements

(require 'flycheck)

;; Libraries required in tests
(require 'f)
(require 'projectile)
(require 'mocker)

(require 'ert)
(require 'epa-file)


;;;; Directories

(defconst flycheck-test-directory (f-parent (f-this-file))
  "The test directory.")

(defconst flycheck-test-resources-directory
  (f-join flycheck-test-directory "resources")
  "Directory of test resources.")

(defconst flycheck-test-source-directory (f-parent flycheck-test-directory)
  "The source directory.")

(defconst flycheck-test-package-directory
  (f-join flycheck-test-source-directory ".cask" emacs-version "elpa"))


;;;; Unit test helpers

(defun flycheck-test-min-emacs-version-p (major &optional minor)
  "Determine whether Emacs has the required version.

Return t if Emacs is at least MAJOR.MINOR, or nil otherwise."
  (when (>= emacs-major-version major)
    (or (null minor) (>= emacs-minor-version minor))))

(defun flycheck-test-resource-filename (resource-file)
  "Determine the absolute file name of a RESOURCE-FILE.

Relative file names are expanded against
`flycheck-test-resources-directory'."
  (f-join flycheck-test-resources-directory resource-file))

(defun flycheck-test-locate-config-file (filename _checker)
  "Find a configuration FILENAME within unit tests.

_CHECKER is ignored."
  (let* ((directory (flycheck-test-resource-filename "checkers/config-files"))
         (filepath (expand-file-name filename directory)))
    (when (f-exists? filepath)
      filepath)))

(defmacro flycheck-test-with-file-buffer (file-name &rest body)
  "Create a buffer from FILE-NAME and eval BODY.

BODY is evaluated with `current-buffer' being a buffer with the
contents FILE-NAME."
  (declare (indent 1))
  `(let ((file-name ,file-name))
     (unless (f-exists? file-name)
       (error "%s does not exist" file-name))
     (with-temp-buffer
       (insert-file-contents file-name 'visit)
       (set-visited-file-name file-name 'no-query)
       (cd (f-parent file-name))
       ;; Mark the buffer as not modified, because we just loaded the file up to
       ;; now.
       (set-buffer-modified-p nil)
       ,@body)))

(defmacro flycheck-test-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY.

The absolute file name of RESOURCE-FILE is determined with
`flycheck-test-resource-filename'."
  (declare (indent 1))
  `(flycheck-test-with-file-buffer
       (flycheck-test-resource-filename ,resource-file)
     ,@body))

(defmacro flycheck-test-with-env (env &rest body)
  "Add ENV to `process-environment' in BODY.

Execute BODY with a `process-environment' with contains all
variables from ENV added.

ENV is an alist, where each cons cell `(VAR . VALUE)' is a
environment variable VAR to be added to `process-environment'
with VALUE."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence process-environment)))
     (--each ,env
       (setenv (car it) (cdr it)))
     ,@body))

(defmacro flycheck-test-with-global-mode (&rest body)
  "Execute BODY with Global Flycheck Mode enabled."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (global-flycheck-mode 1)
         ,@body)
     (global-flycheck-mode -1)))

(defconst flycheck-test-user-error-type
  (if (flycheck-test-min-emacs-version-p 24 3) 'user-error 'error)
  "The `user-error' type used by Flycheck.")

(defvar-local flycheck-test-syntax-checker-finished nil
  "Non-nil if the current checker has finished.")

(add-hook 'flycheck-after-syntax-check-hook
          (lambda () (setq flycheck-test-syntax-checker-finished t)))

(defconst flycheck-test-checker-wait-time 5
  "Time to wait until a checker is finished in seconds.

After this time has elapsed, the checker is considered to have
failed, and the test aborted with failure.")

(defun flycheck-test-wait-for-syntax-checker ()
  "Wait until the syntax check in the current buffer is finished."
  (let ((starttime (float-time)))
    (while (and (not flycheck-test-syntax-checker-finished)
                (< (- (float-time) starttime) flycheck-test-checker-wait-time))
      (sleep-for 1))
    (unless (< (- (float-time) starttime) flycheck-test-checker-wait-time)
      (flycheck-stop-checker)
      (error "Syntax check did not finish after %s seconds"
             flycheck-test-checker-wait-time)))
  (setq flycheck-test-syntax-checker-finished nil))

(defun flycheck-test-buffer-sync ()
  "Check the current buffer synchronously."
  (setq flycheck-test-syntax-checker-finished nil)
  (should (not (flycheck-running-p)))
  (flycheck-mode)                       ; This will only start a deferred check,
  (flycheck-buffer)                     ; so we need an explicit manual check
  ;; After starting the check, the checker should either be running now, or
  ;; already be finished (if it was fast).
  (should (or flycheck-current-process
              flycheck-test-syntax-checker-finished))
  ;; Also there should be no deferred check pending anymore
  (should-not (flycheck-deferred-check-p))
  (flycheck-test-wait-for-syntax-checker))

(defun flycheck-test-ensure-clear ()
  "Clear the current buffer.

Raise an assertion error if the buffer is not clear afterwards."
  (flycheck-clear)
  (should (not flycheck-current-errors))
  (should (not (--any? (overlay-get it 'flycheck-overlay)
                       (overlays-in (point-min) (point-max))))))

(defun flycheck-test-fail-unless-checkers (&rest checkers)
  "Skip the test unless all CHECKERS are present on the system.

Return `:passed' if all CHECKERS are installed, or `:failed' otherwise."
  (if (-all? 'flycheck-check-executable checkers) :passed :failed))

(defalias 'flycheck-test-fail-unless-checker
  'flycheck-test-fail-unless-checkers)

(defmacro flycheck-test-with-help-buffer (&rest body)
  "Execute BODY and kill the help buffer afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when (buffer-live-p (get-buffer (help-buffer)))
       (kill-buffer (help-buffer)))))

(defmacro flycheck-test-with-hook (hook-var form &rest body)
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

(defmacro flycheck-test-without-checkers (checkers &rest body)
  "Execute BODY without CHECKERS.

Remove CHECKERS from `flycheck-checkers' while BODY is
evaluated.

CHECKERS is a syntax checker symbol or a list thereof."
  (declare (indent 1))
  (let ((checkers (if (symbolp checkers) (list checkers) checkers)))
    `(let* ((flycheck-checkers (--remove (memq it ',checkers)
                                         flycheck-checkers)))
       ,@body)))

(defun flycheck-test-travis-ci-p ()
  "Determine whether the tests are running on Travis CI."
  (string= (getenv "TRAVIS") "true"))

(defun flycheck-test-not-on-travis ()
  "Signal an error if run on Travis CI.

Use together with `:expected-result' to skip tests on travis CI."
  (when (flycheck-test-travis-ci-p)
    (error "Test skipped on Travis CI.")))


;;;; Unit test predicates
(defun flycheck-test-should-overlay (error)
  "Test that ERROR has an overlay."
  (let* ((overlay (--first (equal (overlay-get it 'flycheck-error) error)
                           (flycheck-overlays-in 0 (+ 1 (buffer-size)))))
         (region (flycheck-error-region-for-mode error 'symbols))
         (message (flycheck-error-message error))
         (level (flycheck-error-level error))
         (category (flycheck-error-level-overlay-category level))
         (face (get category 'face))
         (fringe-bitmap (flycheck-error-level-fringe-bitmap level))
         (fringe-face (flycheck-error-level-fringe-face level))
         (fringe-icon (list 'left-fringe fringe-bitmap fringe-face)))
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

(defun flycheck-test-should-errors (&rest errors)
  "Test that the current buffers has ERRORS.

Without ERRORS test that there are any errors in the current
buffer.

With ERRORS, test that each error in ERRORS is present in the
current buffer, and that the number of errors in the current
buffer is equal to the number of given ERRORS.

Each error in ERRORS is a list as expected by
`flycheck-test-should-error'."
  (if (not errors)
      (should flycheck-current-errors)
    (let ((expected (--map (apply #'flycheck-error-new-at it) errors)))
      (should (equal expected flycheck-current-errors))
      (-each expected #'flycheck-test-should-overlay))
    (should (= (length errors)
               (length (flycheck-overlays-in (point-min) (point-max)))))))

(defun flycheck-test-should-syntax-check
  (resource-file modes &rest errors)
  "Test a syntax check in RESOURCE-FILE with MODES.

RESOURCE-FILE is the file to check.  MODES is a single major mode
symbol or a list thereof, specifying the major modes to syntax
check with.  ERRORS is the list of expected errors."
  (when (symbolp modes)
    (setq modes (list modes)))
  (--each modes
    (flycheck-test-with-resource-buffer resource-file
      (funcall it)
      ;; Configure config file locating for unit tests
      (--each '(flycheck-locate-config-file-absolute-path
                flycheck-test-locate-config-file)
        (add-hook 'flycheck-locate-config-file-functions it :append :local))
      (let ((process-hook-called 0))
        (add-hook 'flycheck-process-error-functions
                  (lambda (_err)
                    (setq process-hook-called (1+ process-hook-called))
                    nil)
                  nil :local)
        (flycheck-test-buffer-sync)
        (if errors
            (apply #'flycheck-test-should-errors errors)
          (should-not flycheck-current-errors))
        (should (= process-hook-called (length errors))))
      (flycheck-test-ensure-clear))))

(defun flycheck-test-at-nth-error (n)
  (let* ((error (nth (1- n) flycheck-current-errors))
         (mode flycheck-highlighting-mode)
         (region (flycheck-error-region-for-mode error mode)))
    (and (member error (flycheck-overlay-errors-at (point)))
         (= (point) (car region)))))

(defun flycheck-test-explain--at-nth-error (n)
  (let ((errors (flycheck-overlay-errors-at (point))))
    (if (null errors)
        (format "Expected to be at error %s, but no error at point %s"
                n (point))
      (let ((pos (cl-position (car errors) flycheck-current-errors)))
        (format "Expected to be at error %s, but point %s is at error %s"
                n (point) (1+ pos))))))

(put 'flycheck-test-at-nth-error 'ert-explainer
     'flycheck-test-explain--at-nth-error)


;;;; Customization

(ert-deftest flycheck-checkers/there-are-registered-checkers ()
  (should flycheck-checkers))

(ert-deftest flycheck-checkers/all-registered-checkers-are-declared ()
  (dolist (checker flycheck-checkers)
    (should (flycheck-valid-checker-p checker))
    (should (flycheck-registered-checker-p checker))))

(ert-deftest flycheck-checkers/all-declared-checkers-are-registered ()
  (let ((declared-checkers (flycheck-defined-checkers)))
    (should declared-checkers)
    (dolist (checker declared-checkers)
      (should (memq checker flycheck-checkers))
      (should (flycheck-registered-checker-p checker)))))

(ert-deftest flycheck-checkers/should-have-either-patterns-or-parser ()
  (dolist (checker flycheck-checkers)
    (let ((patterns (flycheck-checker-error-patterns checker))
          (parser (flycheck-checker-error-parser checker)))
      (should checker)
      (should (or (and (eq parser 'flycheck-parse-with-patterns) patterns)
                  (null patterns))))))

(ert-deftest flycheck-locate-config-file-functions/default ()
  (should (equal flycheck-locate-config-file-functions
                 '(flycheck-locate-config-file-absolute-path
                   flycheck-locate-config-file-projectile
                   flycheck-locate-config-file-ancestor-directories
                   flycheck-locate-config-file-home))))

(ert-deftest flycheck-process-error-functions/defaults-to-add-overlay ()
  (should (equal flycheck-process-error-functions '(flycheck-add-overlay))))

(ert-deftest flycheck-display-errors-delay/defaults-to-0.9 ()
  (should (equal flycheck-display-errors-delay 0.9)))

(ert-deftest flycheck-display-errors-function/defaults-to-display-error-messages ()
  (should (eq flycheck-display-errors-function
              #'flycheck-display-error-messages)))

(ert-deftest flycheck-indication-mode/defaults-to-left-fringe ()
  (should (eq flycheck-indication-mode 'left-fringe)))

(ert-deftest flycheck-highlighting-mode/defaults-to-symbols ()
  (should (eq flycheck-highlighting-mode 'symbols)))

(ert-deftest flycheck-check-syntax-automatically/defaults-to-all-events ()
  (should (equal flycheck-check-syntax-automatically
                 '(save idle-change new-line mode-enabled))))

(ert-deftest flycheck-idle-change-delay/defaults-to-0.5 ()
  (should (equal flycheck-idle-change-delay 0.5)))

(ert-deftest flycheck-google-max-messages/defaults-to-5 ()
  (should (equal flycheck-google-max-messages 5)))

(ert-deftest flycheck-standard-error-navigation/default-to-t ()
  (should (eq flycheck-standard-error-navigation t)))

(ert-deftest flycheck-completion-system/defaults-to-ido ()
  (should (equal flycheck-completion-system 'ido)))

(ert-deftest flycheck-CHECKER-executable/is-special-variable ()
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (custom-variable-p variable)))))

(ert-deftest flycheck-CHECKER-executable/is-customizable ()
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (custom-variable-p variable)))))

(ert-deftest flycheck-CHECKER-executable/defaults-to-nil ()
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (null (symbol-value variable))))))


;;;; Minor mode definition

(ert-deftest flycheck-mode/enables-standard-error-navigation ()
  (with-temp-buffer
    (setq next-error-function :old)
    (flycheck-mode 1)
    (should flycheck-mode)
    (should (eq next-error-function 'flycheck-next-error-function))
    (flycheck-mode -1)
    (should-not flycheck-mode)
    (should (eq next-error-function :old))))

(ert-deftest flycheck-mode/does-not-enable-standard-error-navigation ()
  (with-temp-buffer
    (let ((flycheck-standard-error-navigation nil))
      (setq next-error-function :old)
      (flycheck-mode +1)
      (should flycheck-mode)
      (should (eq next-error-function :old))
      (should (eq flycheck-old-next-error-function :unset))
      (setq next-error-function :new)
      (flycheck-mode -1)
      (should-not flycheck-mode)
      ;; Disabling the mode should not affect `next-error-function' now
      (should (eq next-error-function :new)))))


;;;; Global syntax checking

(ert-deftest flycheck-may-enable-mode/not-in-ephemeral-buffers ()
  (with-temp-buffer
    (should-not (flycheck-may-enable-mode)))
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (rename-buffer " foo")
    (should (string= (buffer-name) " foo"))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode/not-in-encrypted-files ()
  (let* ((filename (flycheck-test-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-test-with-resource-buffer filename
      (emacs-lisp-mode)
      (should (flycheck-get-checker-for-buffer))
      (should-not (flycheck-may-enable-mode)))))

(ert-deftest flycheck-may-enable-mode/not-if-no-checker-is-found ()
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (text-mode)
    (should-not (s-starts-with? " " (buffer-name)))
    (should-not (flycheck-get-checker-for-buffer))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode/checker-found ()
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (should (flycheck-may-enable-mode))))

(ert-deftest global-flycheck-mode/does-not-enable-in-ephemeral-buffers ()
  (flycheck-test-with-global-mode
    (with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer " foo")
      (emacs-lisp-mode)
      (should-not flycheck-mode))))

(ert-deftest global-flycheck-mode/does-not-enable-in-encrypted-file ()
  (let* ((filename (flycheck-test-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-test-with-global-mode
      (flycheck-test-with-resource-buffer filename
        (emacs-lisp-mode)
        (should-not flycheck-mode)))))

(ert-deftest global-flycheck-mode/does-not-enable-if-no-checker-is-found ()
  (flycheck-test-with-global-mode
    (with-temp-buffer
      (rename-buffer "foo")
      (text-mode)
      (should-not flycheck-mode))))

(ert-deftest global-flycheck-mode/checker-found ()
  (flycheck-test-with-global-mode
    (with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer "foo")
      (emacs-lisp-mode)
      (should flycheck-mode))))


;;;; Deferred syntax checking

(ert-deftest flycheck-deferred-check-p/nil ()
  (let ((flycheck-deferred-syntax-check nil))
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-deferred-check-p/truthy ()
  (let ((flycheck-deferred-syntax-check t))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-deferred/schedules-a-deferred-syntax-check ()
  (with-temp-buffer
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-deferred)
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-clean-deferred-check/removes-a-deferred-syntax-check ()
  (with-temp-buffer
    (flycheck-buffer-deferred)
    (flycheck-clean-deferred-check)
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-perform-deferred-syntax-check/does-nothing-without-deferred-check ()
  (mocker-let
      ((flycheck-buffer-automatically (&optional error condition)
                                      ((:min-occur 0 :max-occur 0))))
    (with-temp-buffer
      (flycheck-perform-deferred-syntax-check))))

(ert-deftest flycheck-perform-deferred-syntax-check/conducts-an-automatic-check ()
  (mocker-let
      ((flycheck-buffer-automatically (&optional error condition)
                                      ((:input '(nil nil)))))
    (with-temp-buffer
      (flycheck-buffer-deferred)
      (flycheck-perform-deferred-syntax-check))))


;;;; Automatic syntax checking

(ert-deftest flycheck-may-check-automatically/not-in-ephemeral-buffers ()
  (with-temp-buffer
    (should-not (-any? #'flycheck-may-check-automatically
                       '(save idle-change new-line mode-enabled)))
    (should-not (flycheck-may-check-automatically))))

(ert-deftest flycheck-may-check-automatically/in-normal-buffers ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (should (-all? #'flycheck-may-check-automatically
                   '(save idle-change new-line mode-enabled)))
    (should (flycheck-may-check-automatically))))

(ert-deftest flycheck-may-check-automatically/automatic-checking-disabled ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (should-not (-any? #'flycheck-may-check-automatically
                         '(save idle-change new-line mode-enabled)))
      (should (flycheck-may-check-automatically)))))

(ert-deftest flycheck-may-check-automatically/specific-event-disabled ()
  (--each '(save idle-change new-line mode-enabled)
    (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
      ;; Disable just a specific event
      (let ((flycheck-check-syntax-automatically
             (remq it flycheck-check-syntax-automatically)))
        (should flycheck-check-syntax-automatically)
        (should-not (flycheck-may-check-automatically it))
        (should (-all? #'flycheck-may-check-automatically
                       flycheck-check-syntax-automatically))
        (should (flycheck-may-check-automatically))))))

(ert-deftest flycheck-check-syntax-automatically/mode-enabled-is-disabled ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically
           (remq 'mode-enabled flycheck-check-syntax-automatically)))
      (flycheck-mode)
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/mode-enabled-checks-syntax-after-flycheck-mode ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically '(mode-enabled)))
      (flycheck-mode)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/idle-change-is-disabled ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (let ((flycheck-check-syntax-automatically
           (remq 'idle-change flycheck-check-syntax-automatically)))
      (insert "Hello world")
      (sleep-for 0.55)
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/idle-change-checks-syntax-after-change ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (let ((flycheck-check-syntax-automatically '(idle-change)))
      (insert "Hello world")
      (sleep-for 0.55)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/idle-change-does-not-check-before-delay ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (let ((flycheck-check-syntax-automatically '(idle-change))
          (flycheck-idle-change-delay 1.5))
      (insert "Hello world")
      (sleep-for 0.55)
      (should-not (flycheck-deferred-check-p))
      (sleep-for 1)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/new-line-is-disabled ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (let ((flycheck-check-syntax-automatically
           (remq 'new-line flycheck-check-syntax-automatically)))
      (insert "\n")
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/new-line-checks-syntax-after-new-line ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (let ((flycheck-check-syntax-automatically '(new-line)))
      (insert "\n")
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/save-disabled ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically
           (remq 'save flycheck-check-syntax-automatically)))
      (save-buffer 0)
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/save-checks-syntax-after-save ()
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically '(save)))
      (save-buffer 0)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-buffer-automatically/does-not-check-with-disabled-mode ()
  (with-temp-buffer
    (should-not flycheck-mode)
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-automatically/defers-the-test ()
  (with-temp-buffer
    (flycheck-mode)
    ;; Flycheck won't check ephemeral buffers
    (rename-buffer "foo")
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should (flycheck-deferred-check-p))))


;;;; Mode line reporting

(ert-deftest flycheck-report-status/sets-mode-line ()
  (with-temp-buffer
    (flycheck-report-status "foo")
    (should (string= flycheck-mode-line " FlyCfoo"))))

(ert-deftest flycheck-report-status/empties-mode-line ()
  (with-temp-buffer
    (flycheck-report-status "")
    (should (string= flycheck-mode-line " FlyC"))))

(ert-deftest flycheck-report-error/sets-mode-line ()
  (with-temp-buffer
    (flycheck-report-error)
    (should (string= flycheck-mode-line " FlyC!"))))

(ert-deftest flycheck-report-error/runs-hook ()
  (with-temp-buffer
    (let* ((was-called nil)
           (flycheck-syntax-check-failed-hook
            (list (lambda () (setq was-called t)))))
      (flycheck-report-error)
      (should was-called))))

(ert-deftest flycheck-report-error/clears-errors ()
  (with-temp-buffer
    (let ((flycheck-current-errors (list 'foo)))
      (flycheck-report-error)
      (should-not flycheck-current-errors))))

(ert-deftest flycheck-report-error-count/no-errors ()
  (with-temp-buffer
    (flycheck-report-error-count nil)
    (should (string= flycheck-mode-line " FlyC"))))

(ert-deftest flycheck-report-error-count/errors-only ()
  (with-temp-buffer
    (flycheck-report-error-count
     (list (flycheck-error-new-at 1 2 'error)
           (flycheck-error-new-at 100 50 'error)
           (flycheck-error-new-at 8 9 'error)
           (flycheck-error-new-at 9 24 'info)))
    (should (string= flycheck-mode-line " FlyC:3/0"))))

(ert-deftest flycheck-report-error-count/warnings-only ()
  (with-temp-buffer
    (flycheck-report-error-count
     (list (flycheck-error-new-at 4 6 'warning)
           (flycheck-error-new-at 10 20 'warning)
           (flycheck-error-new-at 9 24 'info)))
    (should (string= flycheck-mode-line " FlyC:0/2"))))

(ert-deftest flycheck-report-error-count/errors-and-warnings ()
  (with-temp-buffer
    (flycheck-report-error-count
     (list (flycheck-error-new-at 1 2 'error)
           (flycheck-error-new-at 4 6 'warning)
           (flycheck-error-new-at 10 20 'warning)
           (flycheck-error-new-at 100 50 'error)
           (flycheck-error-new-at 8 9 'error)
           (flycheck-error-new-at 9 24 'info)))
    (should (string= flycheck-mode-line " FlyC:3/2"))))


;;;; Utility functions
(ert-deftest flycheck-sexp-to-string/implemented ()
  (error "Not implemented"))

(ert-deftest flycheck-string-to-number-safe/implemented ()
  (error "Not implemented"))

(ert-deftest flycheck-string-list-p/not-implemented ()
  (error "Not implemented"))

(ert-deftest flycheck-temp-dir-system ()
  (let ((dirname (flycheck-temp-dir-system "flycheck-test")))
    (unwind-protect
        (should (f-directory? dirname))
      (flycheck-safe-delete-temporaries))
    (should-not (f-exists? dirname))
    (should (f-child-of? dirname temporary-file-directory))
    (should (s-starts-with? "flycheck-test" (f-filename dirname)))))

(ert-deftest flycheck-temp-file-system/without-file-name ()
  (let ((filename (flycheck-temp-file-system nil "flycheck-test")))
    (unwind-protect
        (should (f-file? filename))
      (flycheck-safe-delete-temporaries))
    (should-not (f-exists? filename))
    (should (f-child-of? filename temporary-file-directory))
    (should (s-starts-with? "flycheck-test" (f-filename filename)))))

(ert-deftest flycheck-temp-file-system/with-complete-path ()
  (let* ((filename (flycheck-temp-file-system "spam/with/eggs.el"
                                              "flycheck-test"))
         (dirname (directory-file-name (f-parent filename))))
    (unwind-protect
        (progn
          ;; The file is not implicitly created, but the temporary directory is.
          (should-not (f-file? filename))
          (should (f-directory? dirname)))
      (flycheck-safe-delete-temporaries))
    (should-not (f-file? filename))
    (should-not (f-directory? dirname))
    ;; The file name should be preserved.  The temporary file should reside in a
    ;; subdirectory of the temporary directory
    (should (string= "eggs.el" (f-filename filename)))
    (should (f-child-of? dirname temporary-file-directory))
    (should (s-starts-with? "flycheck-test" (f-filename dirname)))))

(ert-deftest flycheck-temp-file-inplace/with-just-basename ()
  (let* ((default-directory flycheck-test-directory)
         (filename (flycheck-temp-file-inplace "eggs.el" "flycheck-test")))
    (unwind-protect
        ;; In place files should not be created early
        (should-not (f-exists? filename))
      (flycheck-safe-delete-temporaries))
    (should (string= filename (f-expand "flycheck-test_eggs.el")))))

(ert-deftest flycheck-temp-file-inplace/with-complete-path ()
  (let* ((default-directory flycheck-test-directory)
         (filename (flycheck-temp-file-inplace "spam/with/eggs.el"
                                               "flycheck-test")))
    (unwind-protect
        (should-not (f-exists? filename))
      (flycheck-safe-delete-temporaries))
    (should (string= filename
                     (f-expand (f-join "spam/with" "flycheck-test_eggs.el"))))))

(ert-deftest flycheck-temp-file-inplace/without-file-name ()
  (let ((filename (flycheck-temp-file-inplace nil "flycheck-test")))
    (unwind-protect
        (should (f-file? filename))
      (flycheck-safe-delete-temporaries))
    (should-not (file-name-extension filename))
    (should (s-starts-with? "flycheck-test" (f-filename filename)))))

(ert-deftest flycheck-save-buffer-to-file ()
  (let ((filename (f-expand "tests-temp")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (should-not (f-exists? filename))
            (insert "Hello world")
            (flycheck-save-buffer-to-file filename))
          (should (f-exists? filename))
          (should (string= (f-read filename) "Hello world")))
      (ignore-errors (f-delete filename)))))

(ert-deftest flycheck-option-with-value-argument/no-trailing-equal-sign ()
  (should (equal (flycheck-option-with-value-argument "--foo" "bar")
                 '("--foo" "bar"))))

(ert-deftest flycheck-option-with-value-argument/trailing-equal-sign ()
  (should (equal (flycheck-option-with-value-argument "--foo=" "bar")
                 '("--foo=bar"))))

(ert-deftest flycheck-prepend-with-option/empty-list ()
  (should (null (flycheck-prepend-with-option "-f" nil))))

(ert-deftest flycheck-prepend-with-option/default-prepend-function ()
  (should (equal (flycheck-prepend-with-option "-L" '("foo" "bar"))
                 '("-L" "foo" "-L" "bar"))))

(ert-deftest flycheck-prepend-with-option/prepend-by-string-concatentation ()
  (should (equal (flycheck-prepend-with-option "-L" '("foo" "bar") #'s-prepend)
                 '("-Lfoo" "-Lbar"))))

(ert-deftest flycheck-ephemeral-buffer-p/temporary-buffer ()
  (with-temp-buffer
    (should (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-ephemeral-buffer-p/buffer-with-leading-whitespace ()
  (with-temp-buffer
    (rename-buffer " foo")
    (should (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-ephemeral-buffer-p/buffer-without-leading-whitespace ()
  (with-temp-buffer
    (rename-buffer "foo")
    (should-not (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/unencrypted-temporary-buffer ()
  (with-temp-buffer
    (should-not (flycheck-encrypted-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/unencrypted-file-buffer ()
  (flycheck-test-with-resource-buffer "global-mode-dummy.el"
    (should-not (flycheck-encrypted-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/encrypted-file-buffer ()
  (let* ((filename (flycheck-test-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-test-with-resource-buffer filename
      (should (flycheck-encrypted-buffer-p)))))

(ert-deftest flycheck-autoloads-file-p/not-implemented ()
  (error "Not implemented"))

(ert-deftest flycheck-in-user-emacs-directory-p/not-implemented ()
  (error "Not implemented"))

(ert-deftest flycheck-safe-delete/recursive-removal ()
  (let ((dirname (flycheck-temp-dir-system "flycheck-test")))
    (unwind-protect
        (let ((filename (f-join dirname "foo")))
          (process-lines "touch" filename)
          (should (s-starts-with? dirname filename))
          (should (f-exists? filename))
          (flycheck-safe-delete (list dirname))
          (should-not (f-exists? filename))
          (should-not (f-directory? dirname))
          (should-not (f-exists? dirname)))
      (ignore-errors (f-delete dirname :force)))))


;;;; Checker definitions

(ert-deftest flycheck-command-argument-p/with-symbols ()
  (--each '(source source-inplace source-original
                   temporary-directory temporary-file-name)
    (should (flycheck-command-argument-p it))))

(ert-deftest flycheck-command-argument-p/config-file-with-variable-symbol ()
  (should (flycheck-command-argument-p '(config-file "foo" bar))))

(ert-deftest flycheck-command-argument-p/config-file-with-quoted-variable-symbol ()
  (should-not (flycheck-command-argument-p '(config-file "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/config-file-without-variable-symbol ()
  (should-not (flycheck-command-argument-p '(config-file "foo"))))

(ert-deftest flycheck-command-argument-p/option-without-filter ()
  (should (flycheck-command-argument-p '(option "foo" bar))))

(ert-deftest flycheck-command-argument-p/option-with-filter ()
  (should (flycheck-command-argument-p '(option "foo" bar filter))))

(ert-deftest flycheck-command-argument-p/option-with-quoted-variable-symbol ()
  (should-not (flycheck-command-argument-p '(option "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/option-with-quoted-filter-symbol ()
  (should-not (flycheck-command-argument-p '(option "foo" bar 'filter))))

(ert-deftest flycheck-command-argument-p/option-without-variable ()
  (should-not (flycheck-command-argument-p '(option "foo"))))

(ert-deftest flycheck-command-argument-p/option-list-without-filter-and-prepender ()
  (should (flycheck-command-argument-p '(option-list "foo" bar))))

(ert-deftest flycheck-command-argument-p/option-list-with-prepender ()
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn))))

(ert-deftest flycheck-command-argument-p/option-list-with-prepender-and-filter ()
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn filter))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-variable-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-prepender-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo" bar 'prepend-fn))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-filter-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo" bar prepend-fn 'filter))))

(ert-deftest flycheck-command-argument-p/option-list-without-variable-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo"))))

(ert-deftest flycheck-command-argument-p/eval-with-variable ()
  (should (flycheck-command-argument-p '(eval bar))))

(ert-deftest flycheck-command-argument-p/eval-with-function-call ()
  (should (flycheck-command-argument-p '(eval (spam "with eggs")))))

(ert-deftest flycheck-command-argument-p/eval-with-no-form ()
  (should-not (flycheck-command-argument-p '(eval))))

(ert-deftest flycheck-command-argument-p/eval-with-multiple-forms ()
  (should-not (flycheck-command-argument-p '(eval foo bar))))

(ert-deftest flycheck-command-argument-p/integer-literal ()
  (should-not (flycheck-command-argument-p 100)))

(ert-deftest flycheck-command-argument-p/unknown-argument-symbol ()
  (should-not (flycheck-command-argument-p 'foo)))

(ert-deftest flycheck-command-argument-p/unknown-argument-cell ()
  (should-not (flycheck-command-argument-p '(foo bar))))


;;;; Checker API

(defvar flycheck-test-config-var)
(defvar flycheck-test-option-var)

(ert-deftest flycheck-defined-checkers/are-valid ()
  (dolist (checker (flycheck-defined-checkers))
    (should (flycheck-valid-checker-p checker))))

(ert-deftest flycheck-defined-checkers/are-registered ()
  (dolist (checker (flycheck-defined-checkers))
    (should (flycheck-registered-checker-p checker))))

(ert-deftest flycheck-checker-executable/is-string ()
  (dolist (checker flycheck-checkers)
    (should (stringp (flycheck-checker-executable checker)))))

(ert-deftest flycheck-checker-executable/override-the-executable ()
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (equal (eval `(let ((,variable "some-nice-executable"))
                              (flycheck-checker-executable ',checker)))
                     "some-nice-executable")))))

(ert-deftest flycheck-checker-modes ()
  (dolist (checker flycheck-checkers)
    (should (listp (flycheck-checker-modes checker)))
    (should (-all? #'symbolp (flycheck-checker-modes checker)))))

(ert-deftest flycheck-substitute-argument/source ()
  (flycheck-test-with-resource-buffer "substitute-dummy"
    (unwind-protect
        (progn
          (should (equal (flycheck-substitute-argument 'source-original 'emacs-lisp)
                         (buffer-file-name)))

          (let ((filename (flycheck-substitute-argument 'source-inplace 'emacs-lisp)))
            (should (equal filename (flycheck-test-resource-filename
                                     "flycheck_substitute-dummy")))
            (should (f-exists? filename)))

          (let ((filename (flycheck-substitute-argument 'source 'emacs-lisp)))
            (should (s-starts-with? temporary-file-directory filename))
            (should (f-exists? filename)))))
    (flycheck-safe-delete flycheck-temporaries)))

(ert-deftest flycheck-substitute-argument/temporary-directory ()
  (with-temp-buffer
    (unwind-protect
        (let ((dirname (flycheck-substitute-argument 'temporary-directory
                                                     'emacs-lisp)))
          (should (f-directory? dirname))
          (should (s-starts-with? temporary-file-directory dirname)))
      (flycheck-safe-delete flycheck-temporaries))))

(ert-deftest flycheck-substitute-argument/temporary-filename ()
  (with-temp-buffer
    (unwind-protect
        (let ((filename (flycheck-substitute-argument 'temporary-file-name
                                                      'emacs-lisp)))
          ;; The filename should not exist, but it's parent directory should
          (should-not (f-exists? filename))
          (should (f-directory? (f-parent filename)))
          (should (s-starts-with? temporary-file-directory filename))
          (should (member (f-parent filename) flycheck-temporaries)))
      (flycheck-safe-delete flycheck-temporaries))))

(ert-deftest flycheck-substitute-argument/config-file ()
  (let ((flycheck-test-config-var "substitute-dummy")
        (config-file (flycheck-test-resource-filename "substitute-dummy")))
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

(ert-deftest flycheck-substitute-argument/option ()
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

(ert-deftest flycheck-substitute-argument/option-list ()
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

(ert-deftest flycheck-substitute-argument/option-flag ()
  (let ((flycheck-test-option-var nil))
    (should-not (flycheck-substitute-argument
                 '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var t))
    (should (equal (flycheck-substitute-argument
                    '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                   "--foo")))
  (let ((flycheck-test-option-var (list "bar")))
    (should (equal (flycheck-substitute-argument
                    '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                   "--foo"))))

(ert-deftest flycheck-substitute-argument/eval ()
  (let ((flycheck-test-option-var '("Hello " "World")))
    (should (equal (flycheck-substitute-argument '(eval flycheck-test-option-var) 'emacs-lisp)
                   '("Hello " "World"))))
  (should (equal (flycheck-substitute-argument '(eval (concat "Hello" "World")) 'emacs-lisp)
                 "HelloWorld"))
  (should-not (flycheck-substitute-argument
               '(eval (when (string= "foo" "bar") "yes")) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval 200) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval '("foo" 200)) 'emacs-lisp)))

(ert-deftest flycheck-substitute-argument/unknown ()
  (--each '(flycheck-substitute-argument flycheck-substitute-shell-argument)
    (should-error (funcall it '(foo "bar") 'emacs-lisp))
    (should-error (funcall it 200 'emacs-lisp))))

;; TODO: Refactor these syntax checks

(ert-deftest flycheck-substitute-shell-argument/source ()
  (flycheck-test-with-resource-buffer "substitute-dummy"
    (--each '(source source-inplace source-original)
      (should (equal (flycheck-substitute-shell-argument it 'emacs-lisp)
                     (buffer-file-name))))))

(ert-deftest flycheck-substitute-shell-argument/temporary-directory ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '(temporary-directory emacs-lisp) :output "spam with eggs"))))
    (should (equal (flycheck-substitute-shell-argument 'temporary-directory
                                                       'emacs-lisp)
                   "spam\\ with\\ eggs"))))

(ert-deftest flycheck-substitute-shell-argument/config-file ()
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

(ert-deftest flycheck-substitute-shell-argument/option ()
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

(ert-deftest flycheck-substitute-shell-argument/option-list ()
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

(ert-deftest flycheck-substitute-shell-argument/option-flag ()
  (let ((flycheck-test-option-var nil))
    (should  (s-blank? (flycheck-substitute-shell-argument
                        '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp))))
  (let ((flycheck-test-option-var t))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                   "--foo")))
  (let ((flycheck-test-option-var (list "bar")))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                   "--foo"))))

(ert-deftest flycheck-substitute-shell-argument/eval ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '((eval foo) emacs-lisp) :output '("foo bar" "spam eggs")))))
    (should (equal (flycheck-substitute-shell-argument '(eval foo) 'emacs-lisp)
                   "foo\\ bar spam\\ eggs"))))

(ert-deftest flycheck-check-executable ()
  (dolist (checker flycheck-checkers)
    (if (executable-find (flycheck-checker-executable checker))
        (should (flycheck-check-executable checker))
      (should-not (flycheck-check-executable checker)))))


;;;; Configuration file functions

(ert-deftest flycheck-locate-config-file-absolute-path/just-a-base-name ()
  (with-temp-buffer
    (cd flycheck-test-directory)
    (should-not (flycheck-locate-config-file-absolute-path "flycheck-test.el"
                                                           'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-absolute-path/with-path ()
  (with-temp-buffer
    (cd flycheck-test-directory)
    (should (equal (flycheck-locate-config-file-absolute-path "../Makefile"
                                                              'emacs-lisp)
                   (f-join flycheck-test-directory "../Makefile")))))

(ert-deftest flycheck-locate-config-file-projectile/existing-file-inside-a-project ()
  (with-temp-buffer
    (set-visited-file-name (f-join flycheck-test-directory "foo")
                           :no-query)
    (should (projectile-project-p))
    (should (equal
             (flycheck-locate-config-file-projectile "Makefile" 'emacs-lisp)
             (f-join flycheck-test-directory "../Makefile")))))

(ert-deftest flycheck-locate-config-file-projectile/not-existing-file-inside-a-project ()
  (with-temp-buffer
    (set-visited-file-name (f-join flycheck-test-directory "foo")
                           :no-query)
    (should (projectile-project-p))
    (should-not (flycheck-locate-config-file-projectile "Foo" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-projectile/outside-a-project ()
  (with-temp-buffer
    (set-visited-file-name (f-join temporary-file-directory "foo")
                           :no-query)
    (should-not (projectile-project-p))
    (should-not (flycheck-locate-config-file-projectile "Foo" 'emacs-dir))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/not-existing-file ()
  (with-temp-buffer
    (setq buffer-file-name (f-join flycheck-test-directory "flycheck-test.el"))
    (should-not (flycheck-locate-config-file-ancestor-directories
                 "foo" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/file-on-same-level ()
  (with-temp-buffer
    (setq buffer-file-name (f-join flycheck-test-directory "flycheck-test.el"))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "test-helper.el" 'emacs-lisp)
                   (f-join flycheck-test-directory "test-helper.el")))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/file-on-parent-level ()
  (with-temp-buffer
    (setq buffer-file-name (f-join flycheck-test-directory "flycheck-test.el"))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "Makefile" 'emacs-lisp)
                   (f-join flycheck-test-directory "../Makefile")))))

(ert-deftest flycheck-locate-config-file/not-existing-file ()
  (flycheck-test-with-env (list (cons "HOME" flycheck-test-directory))
    (should-not (flycheck-locate-config-file-home "foo" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file/existing-file-in-parent-directory ()
  (flycheck-test-with-env (list (cons "HOME" flycheck-test-directory))
    (should-not (flycheck-locate-config-file-home "Makefile" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file/existing-file-in-home-directory ()
  (flycheck-test-with-env (list (cons "HOME" flycheck-test-directory))
    (should (equal (flycheck-locate-config-file-home
                    "flycheck-test.el" 'emacs-lisp)
                   (f-join flycheck-test-directory "flycheck-test.el")))))


;;;; Generic option filters

(ert-deftest flycheck-option-int/pass-through-nil ()
  (should (null (flycheck-option-int nil))))

(ert-deftest flycheck-option-int/integer-argument ()
  (should (equal (flycheck-option-int 10) "10")))

(ert-deftest flycheck-option-comma-separated-list/empty-list ()
  (should (null (flycheck-option-comma-separated-list nil))))

(ert-deftest flycheck-option-comma-separated-list/with-single-nil ()
  (should (null (flycheck-option-comma-separated-list '(nil)))))

(ert-deftest flycheck-option-comma-separated-list/filter-returns-nil ()
  (should (null (flycheck-option-comma-separated-list '(10 20) nil
                                                      (lambda (_x) nil)))))

(ert-deftest flycheck-option-comma-separated-list/default-separator ()
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar"))
                 "foo,bar")))

(ert-deftest flycheck-option-comma-separated-list/custom-separator ()
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar") ":")
                 "foo:bar")))

(ert-deftest flycheck-option-comma-separated-list/custom-filter ()
  (should (equal (flycheck-option-comma-separated-list '(10 20) nil
                                                       #'number-to-string)
                 "10,20")))


;;;; Checker selection

(ert-deftest flycheck-checker/unusable-checker-causes-an-error ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-mode)
    (let* ((flycheck-checker 'bash)
           (err (should-error (flycheck-buffer)
                              :type flycheck-test-user-error-type)))
      (should (eq flycheck-checker 'bash))
      (should (string= (cadr err)
                       "Configured syntax checker bash cannot be used"))
      (should (string= flycheck-mode-line " FlyC!")))))

(ert-deftest flycheck-checker/usable-checker-is-used ()
  (flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checker 'emacs-lisp-checkdoc))
      (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
      (flycheck-test-buffer-sync)
      (flycheck-test-should-errors
       '(12 nil warning "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)))))

(ert-deftest flycheck-checker/unregistered-checker-is-used ()
  (flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checkers (remq 'emacs-lisp-checkdoc flycheck-checkers)))
      (should-not (flycheck-registered-checker-p 'emacs-lisp-checkdoc))
      (let ((flycheck-checker 'emacs-lisp-checkdoc))
        (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
        (flycheck-test-buffer-sync)
        (flycheck-test-should-errors
         '(12 nil warning "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))))))

(ert-deftest flycheck-select-checker/selecting-sets-the-syntax-checker ()
  :expected-result (flycheck-test-fail-unless-checker 'python-pylint)
  (with-temp-buffer
    (python-mode)
    (flycheck-select-checker 'python-pylint)
    (should (eq flycheck-checker 'python-pylint))))

(ert-deftest flycheck-select-checker/unselecting-unsets-the-syntax-checker ()
  :expected-result (flycheck-test-fail-unless-checker 'python-pylint)
  (with-temp-buffer
    (python-mode)
    (flycheck-select-checker 'python-pylint)
    (flycheck-select-checker nil)
    (should-not flycheck-checker)))

(ert-deftest flycheck-select-checker/selecting-runs-a-syntax-check ()
  (flycheck-test-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    ;; By default, Flake8 is preferred, so we get errors from Flake8
    (flycheck-test-buffer-sync)
    (flycheck-test-should-errors
     '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))
    ;; Selecting Pylint should give us its errors
    (flycheck-select-checker 'python-pylint)
    (flycheck-test-wait-for-syntax-checker)
    (flycheck-test-should-errors
     '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
     '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
     '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
         :checker python-pylint)
     '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
     '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
     '(9 4 info "Invalid method name \"withEggs\" (C0103)"
         :checker python-pylint)
     '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(10 15 warning "Used builtin function 'map' (W0141)"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))))

(ert-deftest flycheck-select-checker/unselecting-a-checker-goes-back-to-automatic-selection ()
  :expected-result (flycheck-test-fail-unless-checkers 'python-pylint
                                                            'python-flake8)
  (flycheck-test-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    (flycheck-select-checker 'python-pylint)
    (should (eq flycheck-checker 'python-pylint))
    (flycheck-test-wait-for-syntax-checker)
    (flycheck-test-should-errors
     '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
     '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
     '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
         :checker python-pylint)
     '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
     '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
     '(9 4 info "Invalid method name \"withEggs\" (C0103)"
         :checker python-pylint)
     '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(10 15 warning "Used builtin function 'map' (W0141)"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))
    (flycheck-select-checker nil)
    (should-not flycheck-checker)
    (flycheck-test-wait-for-syntax-checker)
    (flycheck-test-should-errors
     '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck/selects-checker-automatically ()
  :expected-result (flycheck-test-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-test-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    (flycheck-test-buffer-sync)
    (should-not flycheck-checker)
    (should (eq flycheck-last-checker 'python-flake8))
    (flycheck-test-should-errors
     '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))
    (let ((flycheck-checkers (remq 'python-flake8 flycheck-checkers)))
      (flycheck-test-buffer-sync)
      (should-not flycheck-checker)
      (should (eq flycheck-last-checker 'python-pylint))
      (flycheck-test-should-errors
       '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
       '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
       '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
           :checker python-pylint)
       '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
       '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
       '(9 4 info "Invalid method name \"withEggs\" (C0103)"
           :checker python-pylint)
       '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
       '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
       '(10 15 warning "Used builtin function 'map' (W0141)"
            :checker python-pylint)
       '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
       '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
       '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
            :checker python-pylint)
       '(22 nil error "Undefined variable 'antigravity' (E0602)"
            :checker python-pylint)))))


;;;; Documentation

(ert-deftest flycheck-describe-checker/pops-up-help-buffer ()
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (flycheck-describe-checker checker)
      (should (buffer-live-p (get-buffer (help-buffer))))
      (should (get-buffer-window (help-buffer)))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward (rx symbol-start (group (one-or-more not-newline))
                               symbol-end " is a Flycheck syntax checker"))
        (should (= (match-beginning 0) 1))
        (should (string= (match-string 1) (symbol-name checker)))))))

(ert-deftest flycheck-describe-checker/can-navigate-to-source ()
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
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

(ert-deftest flycheck-describe-checker/help-shows-executable-name ()
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "This\\s-+syntax\\s-+checker\\s-+executes\\s-+\"\\(.+?\\)\"\\(?:\\.\\|,\\)")
        (should (string= (match-string 1)
                         (flycheck-checker-default-executable checker)))))))

(ert-deftest flycheck-describe-checker/help-shows-executable-variable ()
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "The executable can be overridden with `\\(.+?\\)'.")
        (let ((var (flycheck-checker-executable-variable checker)))
          (should (string= (match-string 1) (symbol-name var))))))))

(ert-deftest flycheck-describe-checker/help-shows-config-file-var ()
  "Test that the config file var appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
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

(ert-deftest flycheck-describe-checker/help-shows-option-vars ()
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (let ((option-vars (-sort #'string<
                                  (flycheck-checker-option-vars checker)))
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

(ert-deftest flycheck-describe-checker/help-shows-checker-docstring ()
  "Test that the docstring appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (should (s-contains? (flycheck-checker-documentation checker)
                             (buffer-substring (point-min) (point-max))))))))

(defmacro flycheck-test-with-manual-buffer (&rest body)
  "Create a temp buffer with flycheck.texi and execute BODY."
  (declare (indent 0))
  `(flycheck-test-with-file-buffer
       (f-join flycheck-test-source-directory "doc" "flycheck.texi")
     ,@body))

(ert-deftest flycheck--manual/all-checkers-are-documented ()
  "Test that all registered checkers are documented in the Flycheck manual."
  (flycheck-test-with-manual-buffer
    (search-forward "@node Syntax checkers")
    (search-forward "@itemize")
    (dolist (checker flycheck-checkers)
      (forward-line 1)
      (should (looking-at (rx line-start "@iflyc " symbol-start
                              (group (one-or-more not-newline))
                              symbol-end line-end)))
      (should (equal (match-string 1) (symbol-name checker))))
    (forward-line 1)
    (should (looking-at (rx "@end itemize")))))

(ert-deftest flycheck--manual/all-options-are-documented ()
  "Tests that all option variables are documented in the manual."
  (let ((config-vars (-sort #'string<
                            (-flatten (-keep #'flycheck-checker-option-vars
                                            (flycheck-defined-checkers))))))
    (flycheck-test-with-manual-buffer
      ;; Go to the beginning of the configuration section
      (search-forward "@node Configuration")
      ;; Go to the beginning of the option variable listing
      (search-forward "configured via options.")
      ;; Verify that all variables are documented
      (dolist (var config-vars)
        (re-search-forward (rx line-start "@defopt " symbol-start
                               (group (one-or-more not-newline))
                               symbol-end line-end))
        (should (equal (match-string 1) (symbol-name var)))))))

(ert-deftest flycheck--manual/all-config-vars-are-documented ()
  "Tests that all configuration file variables are documented in the manual."
  (let ((option-file-vars (-sort #'string<
                                (-keep #'flycheck-checker-config-file-var
                                        (flycheck-defined-checkers)))))
    (flycheck-test-with-manual-buffer
      ;; Go to the beginning of the configuration section
      (search-forward "@node Configuration")
      ;; Go to the beginning of the option variable listing
      (search-forward "configuration file variables")
      ;; Verify that all variables are documented
      (dolist (var option-file-vars)
        (re-search-forward (rx line-start "@defopt " symbol-start
                               (group (one-or-more not-newline))
                               symbol-end line-end))
        (should (equal (match-string 1) (symbol-name var)))))))


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

(ert-deftest flycheck-error-thing-region ()
  (with-temp-buffer
    (insert "    (message)\n    (message")
    (emacs-lisp-mode)
    (should-not (flycheck-error-thing-region 'sexp (flycheck-error-new-at 1 2)))
    (should (equal (flycheck-error-thing-region 'sexp (flycheck-error-new-at 1 5))
                   '(5 . 14)))
    (should-not (flycheck-error-thing-region 'symbol (flycheck-error-new-at 1 5)))
    (should (equal (flycheck-error-thing-region 'sexp (flycheck-error-new-at 1 8))
                   '(6 . 13)))
    (should (equal (flycheck-error-thing-region 'symbol (flycheck-error-new-at 1 8))
                   '(6 . 13)))
    ;; An incomplete expression
    (should-not (flycheck-error-thing-region 'sexp (flycheck-error-new-at 2 5)))))

(ert-deftest flycheck-error-region-for-mode ()
  (with-temp-buffer
    (insert "    (message) ;; Hello world\n    (message")
    (emacs-lisp-mode)
    ;; Test an expression at the error column for all modes
    (let ((err (flycheck-error-new-at 1 7)))
      (should (equal (flycheck-error-region-for-mode err 'lines) '(5 . 29)))
      (should (equal (flycheck-error-region-for-mode err 'columns) '(7 . 8)))
      (should (equal (flycheck-error-region-for-mode err 'symbols) '(6 . 13)))
      (should (equal (flycheck-error-region-for-mode err 'sexps) '(6 . 13))))
    ;; Test an error column which does not point to an expression
    (let ((err (flycheck-error-new-at 2 5)))
      (should (equal (flycheck-error-region-for-mode err 'lines) '(34 . 42)))
      (--each '(columns symbols sexps)
        (should (equal (flycheck-error-region-for-mode err it) '(34 . 35)))))
    ;; Test an error without column for all modes
    (let ((err (flycheck-error-new-at 1 nil)))
      (--each '(lines columns symbols sexps)
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

(ert-deftest flycheck-error-format/level-warning ()
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 3 5 'warning "Hello world"
                                           :checker 'emacs-lisp))
                   "3:5:warning: Hello world (emacs-lisp)")))

(ert-deftest flycheck-error-format/level-error ()
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 20 7 'error "Spam with eggs"
                                           :checker 'ruby))
                   "20:7:error: Spam with eggs (ruby)")))

(ert-deftest flycheck-error-format/no-column ()
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 nil 'warning "Oh no"
                                           :checker 'python-flake8))
                   "14:warning: Oh no (python-flake8)")))

(ert-deftest flycheck-error-format/handles-line-breaks ()
  ;; Specific test for https://github.com/magnars/s.el/issues/34
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 15 'error "dash\\nbroken"
                                           :checker 'foo))
                   "14:15:error: dash\\nbroken (foo)")))


;;;; Error levels

;; A level for the following unit tests
(flycheck-define-error-level 'test-level
    :overlay-category 'category
    :fringe-bitmap 'left-triangle
    :fringe-face 'highlight)

(ert-deftest flycheck-define-error-level/is-error-level? ()
  (should (flycheck-error-level-p 'test-level)))

(ert-deftest flycheck-define-error-level/has-fringe-bitmap ()
  (should (eq (flycheck-error-level-fringe-bitmap 'test-level) 'left-triangle)))

(ert-deftest flycheck-define-error-level/has-fringe-face ()
  (should (eq (flycheck-error-level-fringe-face 'test-level) 'highlight)))

(ert-deftest flycheck-define-error-level/has-overlay-category ()
  (should (eq (flycheck-error-level-overlay-category 'test-level) 'category)))

(ert-deftest flycheck-error-level-make-fringe-icon/has-fringe-bitmap ()
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon
                      'test-level 'left-fringe))
               (`(_ ,bitmap _) (get-text-property 0 'display icon)))
    (should (eq bitmap 'left-triangle))))

(ert-deftest flycheck-error-level-make-fringe-icon/has-fringe-face ()
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon 'test-level 'left-fringe))
               (`(_ _ ,face) (get-text-property 0 'display icon)))
    (should (eq face 'highlight))))

(ert-deftest flycheck-error-level-make-fringe-icon/left-fringe ()
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon 'test-level 'left-fringe))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (eq side 'left-fringe))))

(ert-deftest flycheck-error-level-make-fringe-icon/right-fringe ()
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon 'test-level 'right-fringe))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (eq side 'right-fringe))))

(ert-deftest flycheck-error-level-make-fringe-icon/invalid-side ()
  (let ((err (should-error (flycheck-error-level-make-fringe-icon 'test-level
                                                                  'up-fringe))))
    (should (string= (cadr err) "Invalid fringe side: up-fringe"))))


;;;; Built-in error levels

(ert-deftest flycheck-error-level-error ()
  (should (eq (flycheck-error-level-fringe-bitmap 'error)
              flycheck-fringe-exclamation-mark))
  (should (eq (flycheck-error-level-fringe-face 'error)
              'flycheck-fringe-error))
  (should (eq (flycheck-error-level-overlay-category 'error)
              'flycheck-error-overlay)))

(ert-deftest flycheck-error-level-warning ()
  (should (eq (flycheck-error-level-fringe-bitmap 'warning) 'question-mark))
  (should (eq (flycheck-error-level-fringe-face 'warning)
              'flycheck-fringe-warning))
  (should (eq (flycheck-error-level-overlay-category 'warning)
              'flycheck-warning-overlay)))

(ert-deftest flycheck-error-level-info ()
  (should (eq (flycheck-error-level-fringe-bitmap 'info) 'empty-line))
  (should (eq (flycheck-error-level-fringe-face 'info)
              'flycheck-fringe-info))
  (should (eq (flycheck-error-level-overlay-category 'info)
              'flycheck-info-overlay)))


;;;; General error parsing

(ert-deftest flycheck-sanitize-error/trailing-whitespace ()
  (let ((err (flycheck-error-new-at 1 1 'error " foo " :checker 'emacs-lisp)))
    (equal (flycheck-sanitize-error err)
           (flycheck-error-new-at 1 1 'error "foo" :checker 'emacs-lisp))))

(ert-deftest flycheck-sanitize-error/zero-column ()
  (let ((err (flycheck-error-new-at 1 0 'error "foo" :checker 'emacs-lisp)))
    (equal (flycheck-sanitize-error err)
           (flycheck-error-new-at 1 nil 'error "foo" :checker 'emacs-lisp))))


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

(defconst flycheck-checkstyle-expected-errors
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


(ert-deftest flycheck-parse-checkstyle/with-builtin-xml ()
  "Test Checkstyle parsing with xml.el"
  (let ((flycheck-xml-parser 'flycheck-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle/with-libxml2 ()
  "Test Checkstyle parsing with libxml2."
  :expected-result (if (fboundp 'libxml-parse-xml-region) :passed :failed)
  (let ((flycheck-xml-parser 'libxml-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle/automatic-parser ()
  "Test Checkstyle parsing with the automatically chosen parsed."
  (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                 flycheck-checkstyle-expected-errors)))

 (defconst flycheck-cppcheck-xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.52\"/>
  <errors>
  <error id=\"toomanyconfigs\" severity=\"information\" msg=\"Too many #ifdef configurations - cppcheck only checks 12 configurations. Use --force to check all configurations. For more details, use --enable=information.\" verbose=\"The checking \
of the file will be interrupted because there are too many #ifdef configurations. Checking of all #ifdef configurations can be forced by --force command line option or from GUI preferences. However that may increase the checking time. For\
 more details, use --enable=information.\">
  </error>
  <error id=\"nullPointer\" severity=\"error\" msg=\"Null pointer dereference\" verbose=\"Null pointer dereference\">
  <location file=\"foo\" line=\"4\"/>
  <location file=\"bar\" line=\"6\"/>
  </error>
  <error id=\"comparisonOfBoolWithInt\" severity=\"warning\" msg=\"Comparison of a boolean with integer that is neither 1 nor 0\" verbose=\"The expression &quot;x&quot; is of type 'bool' and it is compared against a integer value that is neither 1 nor 0.\">
    <location file=\"eggs\" line=\"2\"/>
  </error>
  </errors>
</results>"
  "Example cppcheck output.")

(defconst flycheck-cppcheck-expected-errors
  (list
   (flycheck-error-new
    :filename "foo"
    :line 4
    :column nil
    :level 'error
    :message "Null pointer dereference")
   (flycheck-error-new
    :filename "bar"
    :line 6
    :column nil
    :level 'error
    :message "Null pointer dereference")
   (flycheck-error-new
    :filename "eggs"
    :line 2
    :column nil
    :level 'warning
    :message "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0.")))

(ert-deftest flycheck-parse-cppcheck ()
  (should (equal (flycheck-parse-cppcheck flycheck-cppcheck-xml nil nil)
                 flycheck-cppcheck-expected-errors)))

(ert-deftest flycheck-parse-cppcheck/empty-errors-list-with-automatic-parser ()
  (should-not (flycheck-parse-cppcheck "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.60.1\"/>
  <errors>
  </errors>
</results>" nil nil)))

(ert-deftest flycheck-parse-cppcheck/empty-errors-list-with-builtin-parser ()
  (let ((flycheck-xml-parser #'flycheck-parse-xml-region))
    (should-not (flycheck-parse-cppcheck "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.60.1\"/>
  <errors>
  </errors>
</results>" nil nil))))


;;;; Error overlay management


(ert-deftest flycheck-info-overlay/priority ()
  (should (= (get 'flycheck-info-overlay 'priority) 90)))

(ert-deftest flycheck-warning-overlay/priority ()
  (should (= (get 'flycheck-warning-overlay 'priority) 100)))

(ert-deftest flycheck-error-overlay/priority ()
  (should (= (get 'flycheck-error-overlay 'priority) 110)))

(ert-deftest flycheck-info-overlay/face ()
  (should (eq (get 'flycheck-info-overlay 'face) 'flycheck-info)))

(ert-deftest flycheck-warning-overlay/face ()
  (should (eq (get 'flycheck-warning-overlay 'face) 'flycheck-warning)))

(ert-deftest flycheck-error-overlay/face ()
  (should (eq (get 'flycheck-error-overlay 'face) 'flycheck-error)))

(ert-deftest flycheck-info-overlay/default-help-echo ()
  (should (string= (get 'flycheck-info-overlay 'help-echo) "Unknown info.")))

(ert-deftest flycheck-warning-overlay/default-help-echo ()
  (should (string= (get 'flycheck-warning-overlay 'help-echo)
                   "Unknown warning.")))

(ert-deftest flycheck-error-overlay/default-help-echo ()
  (should (string= (get 'flycheck-error-overlay 'help-echo) "Unknown error.")))

(ert-deftest flycheck-add-overlay/undefined-error-level ()
  (let ((err (should-error (flycheck-add-overlay
                            (flycheck-error-new-at 1 1 'foo)))))
      (should (string= (cadr err) "Undefined error level: foo"))))

(ert-deftest flycheck-add-overlay/no-error-level ()
  (let ((err (should-error (flycheck-add-overlay (flycheck-error-new-at 1 1)))))
      (should (string= (cadr err) "Undefined error level: nil"))))

(ert-deftest flycheck-add-overlay/info-category ()
  (with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'info))))
      (should (eq (overlay-get overlay 'category) 'flycheck-info-overlay)))))

(ert-deftest flycheck-add-overlay/warning-category ()
  (with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'warning))))
      (should (eq (overlay-get overlay 'category) 'flycheck-warning-overlay)))))

(ert-deftest flycheck-add-overlay/error-category ()
  (with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'error))))
      (should (eq (overlay-get overlay 'category) 'flycheck-error-overlay)))))

(ert-deftest flycheck-add-overlay/has-help-echo ()
  (with-temp-buffer
    (let ((overlay (flycheck-add-overlay
                    (flycheck-error-new-at 1 1 'info "A bar message"))))
      (should (string= (overlay-get overlay 'help-echo) "A bar message")))))

(ert-deftest flycheck-add-overlay/has-flycheck-overlay-property ()
  (with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'error))
           (overlay (flycheck-add-overlay err)))
      (should (overlay-get overlay 'flycheck-overlay)))))

(ert-deftest flycheck-add-overlay/has-flycheck-error-property ()
  (with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'warning))
           (overlay (flycheck-add-overlay err)))
      (should (eq (overlay-get overlay 'flycheck-error) err)))))

(ert-deftest flycheck-add-overlay/has-no-fringe-icon-with-disabled-indication ()
  (with-temp-buffer
    (insert "Hello\n    World")
    (let ((flycheck-indication-mode nil))
      (--each '(warning info error)
        (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 it))))
          (should-not (overlay-get overlay 'before-string)))))))

(ert-deftest flycheck-add-overlay/has-info-fringe-icon ()
  (with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'info)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-info))
      (should (eq bitmap 'empty-line)))))

(ert-deftest flycheck-add-overlay/has-warning-fringe-icon ()
  (with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'warning)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-warning))
      (should (eq bitmap 'question-mark)))))

(ert-deftest flycheck-add-overlay/has-error-fringe-icon ()
  (with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'error)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-error))
      (should (eq bitmap flycheck-fringe-exclamation-mark)))))

(ert-deftest flycheck-add-overlay/has-left-fringe-icon ()
  (with-temp-buffer
    (insert "Hello\n    World")
    ;; Test the various indication modes
    (let ((flycheck-indication-mode 'left-fringe))
      (pcase-let* ((overlay (flycheck-add-overlay
                             (flycheck-error-new-at 1 1 'error)))
                   (before-string (overlay-get overlay 'before-string))
                   (`(,side _ _) (get-text-property 0 'display before-string)))
        (should (eq side 'left-fringe))))))

(ert-deftest flycheck-add-overlay/has-right-fringe-icon ()
  (with-temp-buffer
    (insert "Hello\n    World")
    ;; Test the various indication modes
    (let ((flycheck-indication-mode 'right-fringe))
      (pcase-let* ((overlay (flycheck-add-overlay
                             (flycheck-error-new-at 1 1 'error)))
                   (before-string (overlay-get overlay 'before-string))
                   (`(,side _ _) (get-text-property 0 'display before-string)))
        (should (eq side 'right-fringe))))))

(ert-deftest flycheck-add-overlay/right-position-in-narrowed-buffer ()
  "Test that all overlays are added at the right positions with narrowing in place."
  (flycheck-test-with-resource-buffer "narrowing.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    ;; Narrow to the function and check the buffer
    (re-search-forward "(defun .*")
    (forward-line 1)
    (narrow-to-defun)
    (should (buffer-narrowed-p))
    (flycheck-test-buffer-sync)
    ;; We should have two errors highlighted between point min and max now
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 2))
    ;; Remove restrictions and test that all errors are reported
    (widen)
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 4))
    (flycheck-test-should-errors
     '(9 1 warning "`message' called with 0 args to fill 1\n    format field(s)"
         :checker emacs-lisp)
     '(11 8 warning "`message' called with 0 args to fill 1\n    format field(s)"
          :checker emacs-lisp)
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(15 1 warning "`message' called with 0 args to fill 1\n    format field(s)"
          :checker emacs-lisp))))


;;;; Error navigation

(defmacro flycheck-test-with-nav-buffer (&rest body)
  (declare (indent 0))
  `(flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
     (emacs-lisp-mode)
     (flycheck-mode)
     (flycheck-test-buffer-sync)
     (goto-char (point-min))
     ,@body))

(ert-deftest flycheck-next-error/goes-to-first-error ()
  (flycheck-test-with-nav-buffer
    (flycheck-next-error)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-next-error/goes-to-next-error ()
  (flycheck-test-with-nav-buffer
    (flycheck-next-error)
    (flycheck-next-error)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-next-error/errors-beyond-last-error ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/errors-when-moving-too-far ()
  (flycheck-test-with-nav-buffer
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/navigate-by-two-errors ()
  (flycheck-test-with-nav-buffer
    (flycheck-next-error 2)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-next-error/navigate-back-by-two-errors ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-next-error -2)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-next-error/reset-navigates-to-first-error ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-next-error 1 'reset)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-next-error/does-not-cross-narrowing ()
  (flycheck-test-with-nav-buffer
    (re-search-forward "(defun .*")
    (narrow-to-defun)
    (goto-char (point-min))
    (flycheck-next-error)
    (should (flycheck-test-at-nth-error 1))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/errors-before-first-error ()
  (flycheck-test-with-nav-buffer
    (let ((err (should-error (flycheck-previous-error)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/goes-to-last-error ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-previous-error)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-previous-error/navigate-by-two-errors ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-previous-error 2)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-previous-error/navigate-back-by-two-errors ()
  (flycheck-test-with-nav-buffer
    (flycheck-previous-error -2)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-previous-errors/errors-when-moving-too-far ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error/goes-to-first-error ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-first-error/stays-at-first-error-if-called-again ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error)
    (flycheck-first-error)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-first-error/goes-to-second-error ()
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error 2)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest next-error/calls-flycheck-next-error-function ()
  (flycheck-test-with-nav-buffer
    (mocker-let
     ((flycheck-next-error-function (n reset)
                                    ((:input '(1 nil))
                                     (:input '(2 nil))
                                     (:input '(2 reset)))))
     (goto-char (point-min))
     (next-error)
     (next-error 2)
     (next-error 2 'reset))))


;;;; Error list

(ert-deftest flycheck-error-list-buffer-label ()
  (with-temp-buffer
    (rename-buffer "Foo")
    (should (string= (flycheck-error-list-buffer-label (current-buffer))
                     "#<buffer Foo>")))
  (with-temp-buffer
    (set-visited-file-name (f-join flycheck-test-directory "foo/bar")
                           :no-query)
    (cd flycheck-test-directory)
    (should (string= (flycheck-error-list-buffer-label (current-buffer))
                     "foo/bar"))))

(ert-deftest flycheck-error-list-error-label ()
  (with-temp-buffer
    (rename-buffer "Foo")
    (should (string= (flycheck-error-list-error-label (flycheck-error-new-at 1 1))
                     "#<buffer Foo>")))
  (with-temp-buffer
    (set-visited-file-name (f-join flycheck-test-directory "foo/bar")
                           :no-query)
    (cd flycheck-test-directory)
    (should (string= (flycheck-error-list-error-label (flycheck-error-new-at 1 1))
                     "foo/bar")))
  (with-temp-buffer
    (cd flycheck-test-directory)
    (let* ((filename (f-join flycheck-test-directory "spam/with/eggs"))
           (err (flycheck-error-new-at 1 1 'warning "Foo" :filename filename)))
      (should (string= (flycheck-error-list-error-label err)
                       "spam/with/eggs")))))

(provide 'flycheck-test)


;;;; General error display

(ert-deftest flycheck-display-errors/no-display-function-set ()
  (let ((err (flycheck-error-new-at 10 20 'warning "This is a Flycheck error."))
        (flycheck-display-errors-function nil))
    ;; Error display must not fail with nil
    (with-current-buffer "*Messages*"
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (flycheck-display-errors (list err))
    (with-current-buffer "*Messages*"
      (should-not (s-contains? (flycheck-error-message err)
                               (buffer-string))))))

(ert-deftest flycheck-display-errors/custom-function ()
  (let* ((err (flycheck-error-new-at 10 20 'warning "Foo"))
         (displayed-errors nil)
         (flycheck-display-errors-function (lambda (errors)
                                             (--each errors
                                               (push it displayed-errors)))))
    (flycheck-display-errors (list err))
    (should (equal displayed-errors (list err)))))


;;;; Error display functions

(ert-deftest flycheck-display-error-messages ()
  (let ((err (flycheck-error-new-at 10 20 'warning
                                    "This is a Flycheck error.")))
    (with-current-buffer "*Messages*"
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (flycheck-display-error-messages (list err))
    (with-current-buffer "*Messages*"
      (should (s-contains? (flycheck-error-message err) (buffer-string))))))


;;;; Working with error messages

(ert-deftest flycheck-copy-messages-as-kill ()
  (with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (-each errors #'flycheck-add-overlay)
      (let ((flycheck-display-errors-function 'display-function))
        (flycheck-copy-messages-as-kill 10)))
    (should (equal (-take 2 kill-ring) '("1st message" "2nd message")))))

(ert-deftest flycheck-google-messages ()
  (with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (-each errors #'flycheck-add-overlay)
      (let ((err (should-error (flycheck-google-messages 10)
                               :type flycheck-test-user-error-type)))
        (should (string= (cadr err) "Please install Google This from https://github.com/Bruce-Connor/emacs-google-this")))

      (mocker-let
          ((google-string (quote-flag s confirm)
                          ((:input '(nil "1st message" :no-confirm))
                           (:input '(nil "2nd message" :no-confirm))
                           (:input '(:quote "1st message" :no-confirm))
                           (:input '(:quote "2nd message" :no-confirm)))))
        (let* ((flycheck-google-max-messages 1)
               (err (should-error (flycheck-google-messages 10)
                                  :type flycheck-test-user-error-type)))
          (should (string= (cadr err) "More than 1 messages at point")))
        (flycheck-google-messages 10)
        (flycheck-google-messages 10 :quote)))))


;;;; Syntax checker executables

(ert-deftest flycheck-overridden-executable ()
  (flycheck-test-with-hook 'emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-executable
            (flycheck-test-resource-filename "bin/dummy-emacs"))
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(17 4 error "t is not true!" :checker emacs-lisp)
     '(19 11 warning "This is a stupid message" :checker emacs-lisp))))

(ert-deftest flycheck-set-checker-executable/real-executable ()
  (with-temp-buffer
    ;; Create a temporary buffer to restrict the scope of
    ;; `flycheck-emacs-lisp-executable'
    (let ((file-name (flycheck-test-resource-filename "bin/dummy-emacs")))
      (should (f-exists? file-name))
      (should (f-executable? file-name))
      (flycheck-set-checker-executable 'emacs-lisp file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))))
  ;; The global value should remain unaffected
  (should-not flycheck-emacs-lisp-executable))

(ert-deftest flycheck-set-checker-executable/no-executable-given ()
  (with-temp-buffer
    (let ((file-name (flycheck-test-resource-filename "bin/dummy-emacs")))
      (setq flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp)
      (should-not flycheck-emacs-lisp-executable))))

(ert-deftest flycheck-set-checker-executable/executable-is-nil ()
  (with-temp-buffer
    (let ((file-name (flycheck-test-resource-filename "bin/dummy-emacs")))
      (setq flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp nil)
      (should-not flycheck-emacs-lisp-executable))))

(ert-deftest flycheck-set-checker-executable/non-existing-file ()
  (let ((file-name (flycheck-test-resource-filename "no-such-file")))
    (should-not (f-exists? file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))

(ert-deftest flycheck-set-checker-executable/file-not-executable ()
  (let ((file-name (flycheck-test-resource-filename "checkers/emacs-lisp.el")))
    (should (f-exists? file-name))
    (should-not (f-executable? file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))


;;;; Built-in syntax checkers

;; Tell the byte compiler about the variables we'll use
(defvar js2-mode-show-strict-warnings)
(defvar js2-mode-show-parse-errors)
(defvar js3-mode-show-parse-errors)
(autoload 'sh-set-shell "sh-script")

(ert-deftest flycheck-define-checker/asciidoc ()
  :expected-result (flycheck-test-fail-unless-checker 'asciidoc)
  (flycheck-test-should-syntax-check
   "checkers/asciidoc.adoc" 'adoc-mode
   '(1 nil warning "missing style: [paradef-default]: paragraph" :checker asciidoc)
   '(3 nil warning "old tables syntax" :checker asciidoc)
   '(11 nil error "[tabledef-default] illegal width=%60%" :checker asciidoc)))

(ert-deftest flycheck-define-checker/bash ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-test-fail-unless-checker 'bash)
  (flycheck-test-with-hook sh-mode-hook (sh-set-shell "bash" :no-query)
    (flycheck-test-should-syntax-check
     "checkers/bash-syntax-error.bash" 'sh-mode
     '(5 nil error "syntax error near unexpected token `fi'" :checker bash)
     '(5 nil error "`fi'" :checker bash))))

(ert-deftest flycheck-define-checker/c/c++-clang-warning ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-without-checkers c/c++-cppcheck
    (flycheck-test-should-syntax-check
     "checkers/c_c++-clang-warning.c" 'c-mode
     '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)
     '(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-warning-customized ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-with-hook c-mode-hook
      ;; Disable conversion checks by removing -Wextra, but additionally warn
      ;; about missing prototypes, which isn't included in -Wextra
      (setq flycheck-clang-warnings '("all" "missing-prototypes"))
    (flycheck-test-without-checkers c/c++-cppcheck
      (flycheck-test-should-syntax-check
       "checkers/c_c++-clang-warning.c" 'c-mode
       '(3 5 warning "no previous prototype for function 'f'"
           :checker c/c++-clang)
       '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)))))

(ert-deftest flycheck-define-checker/c/c++-clang-fatal-error ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-should-syntax-check
   "checkers/c_c++-clang-fatal-error.c" 'c-mode
   '(2 10 error "'c_c++-clang-library-header.h' file not found"
       :checker c/c++-clang)))

(ert-deftest flycheck-define-checker/c/c++-clang-include-path ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-with-hook c-mode-hook
      (setq flycheck-clang-include-path '("./include"))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-clang-fatal-error.c" 'c-mode)))

(ert-deftest flycheck-define-checker/c/c++-clang-includes ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-with-hook c++-mode-hook
      (setq flycheck-clang-includes
            (list (flycheck-test-resource-filename "checkers/include/c_c++-clang-library-header.h")))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-clang-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-should-syntax-check
   "checkers/c_c++-clang-error.cpp" 'c++-mode
   '(3 23 info "template is declared here" :checker c/c++-clang)
   '(8 17 error "implicit instantiation of undefined template 'test<false>'"
       :checker c/c++-clang)
   '(10 16 error "use of undeclared identifier 'nullptr'"
        :checker c/c++-clang)))

(ert-deftest flycheck-define-checker/c/c++-clang-error-language-standard ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-with-hook c++-mode-hook
      (setq flycheck-clang-language-standard "c++11")
    (flycheck-test-should-syntax-check
     "checkers/c_c++-clang-error.cpp" 'c++-mode
     '(3 23 info "template is declared here" :checker c/c++-clang)
     '(8 17 error "implicit instantiation of undefined template 'test<false>'"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error-definitions ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-with-hook c++-mode-hook
      (setq flycheck-clang-definitions '("FLYCHECK_LOCAL" "FLYCHECK_LIBRARY"))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-clang-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error-no-rtti ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-clang)
  (flycheck-test-with-hook c++-mode-hook
      (setq flycheck-clang-no-rtti t)
    ;; Clang doesn't throw errors for RTTI operators :|, so we basically just
    ;; test that the option flag doesn't cause any issues
    (flycheck-test-should-syntax-check
     "checkers/c_c++-clang-error-rtti.cpp" 'c++-mode)))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-error ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-test-without-checkers c/c++-clang
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-error.c" 'c-mode
     '(4 nil error "Null pointer dereference" :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-warning ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-test-without-checkers c/c++-clang
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-warning.c" 'c-mode
     '(2 nil warning "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0."
         :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-style ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-test-without-checkers c/c++-clang
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-style.c" 'c-mode
     '(3 nil warning "Unused variable: unused" :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-style-suppressed ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-test-with-hook c-mode-hook
      (setq flycheck-cppcheck-checks nil)
    (flycheck-test-without-checkers c/c++-clang
      (flycheck-test-should-syntax-check "checkers/c_c++-cppcheck-style.c"
                                              'c-mode))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-multiple-checks ()
  :expected-result (flycheck-test-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-test-with-hook c++-mode-hook
      (setq flycheck-cppcheck-checks '("performance" "portability"))
    (flycheck-test-without-checkers c/c++-clang
      (flycheck-test-should-syntax-check
       "checkers/c_c++-cppcheck-multiple-checks.cpp" 'c++-mode
       '(2 nil warning "Extra qualification 'A::' unnecessary and considered an error by many compilers."
           :checker c/c++-cppcheck)
       '(9 nil warning "Prefix ++/-- operators should be preferred for non-primitive types. Pre-increment/decrement can be more efficient than post-increment/decrement. Post-increment/decrement usually involves keeping a copy of the previous value around and adds a little extra code."
           :checker c/c++-cppcheck)))))

(ert-deftest flycheck-define-checker/cfengine-error ()
  :expected-result (flycheck-test-fail-unless-checker 'cfengine)
  (flycheck-test-should-syntax-check
   "checkers/cfengine-error.cf" 'cfengine-mode
   '(8 20 error "Unknown promise type 'nosuchpromisetype'"
       :checker cfengine)))

(ert-deftest flycheck-define-checker/cfengine-warning ()
  :expected-result (flycheck-test-fail-unless-checker 'cfengine)
  (flycheck-test-should-syntax-check
   "checkers/cfengine-warning.cf" 'cfengine-mode
   '(3 34 warning "Removed constraint 'host_licenses_paid' in promise type 'common' [-Wremoved]"
       :checker cfengine)))

(ert-deftest flycheck-define-checker/chef-foodcritic ()
  :expected-result (flycheck-test-fail-unless-checker 'chef-foodcritic)
  (flycheck-test-should-syntax-check
    "checkers/chef-foodcritic/recipes/chef-foodcritic-error.rb" 'ruby-mode
   '(3 nil error "FC002: Avoid string interpolation where not required"
       :checker chef-foodcritic)
   '(8 nil error "FC003: Check whether you are running with chef server before using server-specific features"
       :checker chef-foodcritic)
   '(11 nil error "FC004: Use a service resource to start and stop services"
        :checker chef-foodcritic)))

(ert-deftest flycheck-define-checker/coffee-syntax-error ()
  :expected-result (flycheck-test-fail-unless-checker 'coffee)
  (flycheck-test-should-syntax-check
   "checkers/coffee-syntax-error.coffee" 'coffee-mode
   '(4 7 error "missing \", starting" :checker coffee)))

(ert-deftest flycheck-define-checker/coffee-coffeelint-error ()
  :expected-result (flycheck-test-fail-unless-checker 'coffee-coffeelint)
  (flycheck-test-should-syntax-check
   "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
   '(4 nil error "Throwing strings is forbidden; context:"
       :checker coffee-coffeelint)))

(ert-deftest flycheck-define-checker/coffee-coffeelint-warning ()
  :expected-result (flycheck-test-fail-unless-checker 'coffee-coffeelint)
  (flycheck-test-with-hook coffee-mode-hook
      (setq flycheck-coffeelintrc "coffeelint.json")
    (flycheck-test-should-syntax-check
     "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
     '(4 nil warning "Throwing strings is forbidden; context:"
         :checker coffee-coffeelint))))

(ert-deftest flycheck-define-checker/css-csslint ()
  :expected-result (flycheck-test-fail-unless-checker 'css-csslint)
  (flycheck-test-should-syntax-check
   "checkers/css-csslint-warning.css" 'css-mode
   '(3 6 warning "Heading (h1) should not be qualified."
       :checker css-csslint)))

(ert-deftest flycheck-define-checker/css-csslint-syntax-error ()
  :expected-result (flycheck-test-fail-unless-checker 'css-csslint)
  (flycheck-test-should-syntax-check
   "checkers/css-syntax-error.css" 'css-mode
   '(4 16 error "Expected LBRACE at line 4, col 16." :checker css-csslint)
   '(4 16 error "Unexpected token '100%' at line 4, col 16."
       :checker css-csslint)
   '(4 20 error "Unexpected token ';' at line 4, col 20." :checker css-csslint)
   '(5 1 error "Unexpected token '}' at line 5, col 1." :checker css-csslint)))

(ert-deftest flycheck-d-module-name ()
  (with-temp-buffer
    (insert "Hello world")
    (should-not (flycheck-d-module-name)))
  (with-temp-buffer
    (insert "module spam.with.eggs;")
    (should (string= (flycheck-d-module-name) "spam.with.eggs"))))

(ert-deftest flycheck-d-base-directory ()
  (flycheck-test-with-resource-buffer "checkers/d-dmd-warning.d"
    (should (f-same? (flycheck-d-base-directory)
                     (f-join flycheck-test-resources-directory "checkers"))))
  (flycheck-test-with-resource-buffer "checkers/d-dmd-warning.d"
    (goto-char (point-min))
    (insert "module checkers.d_dmd_warning;")
    (should (f-same? (flycheck-d-base-directory)
                     flycheck-test-resources-directory)))
  (flycheck-test-with-resource-buffer "checkers/package.d"
    (should (f-same? (flycheck-d-base-directory)
                     flycheck-test-resources-directory))))

(ert-deftest flycheck-define-checker/d-dmd-syntax-error ()
  :expected-result (flycheck-test-fail-unless-checker 'd-dmd)
  (flycheck-test-should-syntax-check
   "checkers/d-dmd-syntax-error.d" 'd-mode
   '(2 nil error "module studio is in file 'std/studio.d' which cannot be read"
       :checker d-dmd)))

(ert-deftest flycheck-define-checker/d-dmd-syntax-error-without-module ()
  :expected-result (flycheck-test-fail-unless-checker 'd-dmd)
  (flycheck-test-should-syntax-check
   "checkers/d_dmd_syntax_error_without_module.d" 'd-mode
   '(5 nil error "undefined identifier writel, did you mean template write(T...)(T args) if (!is(T[0] : File))?"
       :checker d-dmd)))

(ert-deftest flycheck-define-checker/d-dmd-warning ()
  :expected-result (flycheck-test-fail-unless-checker 'd-dmd)
  (flycheck-test-should-syntax-check
   "checkers/d-dmd-warning.d" 'd-mode
   '(6 nil warning "statement is not reachable" :checker d-dmd)
   '(17 nil warning "function d_dmd_warning.bar is deprecated"
        :checker d-dmd)))

(ert-deftest flycheck-define-checker/elixir-error ()
  :expected-result (flycheck-test-fail-unless-checker 'elixir)
  (flycheck-test-should-syntax-check
   "checkers/elixir-error.ex" 'elixir-mode
   '(5 nil error "function puts/1 undefined" :checker elixir)))

(ert-deftest flycheck-define-checker/elixir-warnings ()
  :expected-result (flycheck-test-fail-unless-checker 'elixir)
  (flycheck-test-should-syntax-check
   "checkers/elixir-warnings.ex" 'elixir-mode
   '(7 nil warning "this clause cannot match because a previous clause at line 4 always matches"
        :checker elixir)))

(ert-deftest flycheck-define-checker/emacs-lisp ()
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
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     `(15 1 error ,msg :checker emacs-lisp))))

(ert-deftest flycheck-define-checker/emacs-lisp-load-path ()
  (flycheck-test-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-load-path
            (list (flycheck-test-resource-filename
                   "dummy-elpa/dummy-package-0.1")))
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but
    requires 1+" :checker emacs-lisp)
     '(23 1 warning "the function `dummy-package-foo' might
    not be defined at runtime." :checker emacs-lisp))))

(ert-deftest flycheck-define-checker/emacs-lisp-initialize-packages ()
  (flycheck-test-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-package-user-dir
            (flycheck-test-resource-filename "dummy-elpa")
            flycheck-emacs-lisp-initialize-packages t)
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but
    requires 1+" :checker emacs-lisp))))

(ert-deftest flycheck-define-checker/emacs-lisp-checks-compressed-file ()
  (flycheck-test-should-syntax-check
   "checkers/emacs-lisp.el.gz" 'emacs-lisp-mode
   '(12 nil warning "First sentence should end with punctuation"
        :checker emacs-lisp-checkdoc)
   '(16 6 warning "message called with 0 arguments, but
    requires 1+" :checker emacs-lisp)
   '(21 1 warning "the function `dummy-package-foo' is
    not known to be defined." :checker emacs-lisp)))

(ert-deftest flycheck-define-checker/emacs-lisp-sytnax-error ()
  (flycheck-test-without-checkers emacs-lisp-checkdoc
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp-syntax-error.el" 'emacs-lisp-mode
     '(3 1 error "End of file during parsing" :checker emacs-lisp))))

(ert-deftest flycheck-define-checker/emacs-lisp-without-file-name ()
  "Test checkdoc checker in buffers without file names.

Regression test for https://github.com/flycheck/flycheck/issues/73 and
https://github.com/bbatsov/prelude/issues/259."
  (flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
    (set-visited-file-name nil 'no-query)
    (emacs-lisp-mode)
    (should-not (buffer-file-name))
    (flycheck-test-buffer-sync)
    ;; TODO: Consider whether checkdoc is really useful in buffers without file
    ;; names…
    (flycheck-test-should-errors)))

(ert-deftest flycheck-define-checker/emacs-lisp-does-not-check-autoloads-buffers ()
  "Test that Emacs Lisp does not check autoloads buffers.

These buffers are temporary buffers generated during package
installation, which may not be byte compiled, and hence the
checker will refuse to check these.

See URL `https://github.com/flycheck/flycheck/issues/45' and URL
`https://github.com/bbatsov/prelude/issues/253'."
  (flycheck-test-with-file-buffer (locate-library "dash-autoloads")
    (should-not (flycheck-may-use-checker 'emacs-lisp))
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-define-checker/emacs-lisp-checkdoc-does-not-check-cask-files ()
  (flycheck-test-with-file-buffer (f-join flycheck-test-source-directory "Cask")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-define-checker/emacs-lisp-does-not-check-with-no-byte-compile ()
  (flycheck-test-with-hook emacs-lisp-mode-hook
      (set (make-local-variable 'no-byte-compile) t)
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc))))

(ert-deftest flycheck-define-checker/erlang-error ()
  :expected-result (flycheck-test-fail-unless-checker 'erlang)
  (flycheck-test-should-syntax-check
   "checkers/erlang-error.erl" 'erlang-mode
   '(7 nil error "head mismatch" :checker erlang)))

(ert-deftest flycheck-define-checker/erlang-warning ()
  :expected-result (flycheck-test-fail-unless-checker 'erlang)
  (flycheck-test-should-syntax-check
   "checkers/erlang-warning.erl" 'erlang-mode
   '(6 nil warning "wrong number of arguments in format call" :checker erlang)))

(ert-deftest flycheck-define-checker/go-gofmt ()
  "Test a syntax error."
  :expected-result (flycheck-test-fail-unless-checker 'go-gofmt)
  (flycheck-test-should-syntax-check
   "checkers/go/src/syntax/syntax-error.go" 'go-mode
   '(5 9 error "expected '(', found 'IDENT' ta" :checker go-gofmt)
   '(6 1 error "expected ')', found '}'" :checker go-gofmt)))

(ert-deftest flycheck-define-checker/go-build ()
  "Test an import error."
  :expected-result (flycheck-test-fail-unless-checker 'go-build)
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (flycheck-test-should-syntax-check
     "checkers/go/src/error/build-error.go" 'go-mode
     '(6 nil error "undefined: fmt" :checker go-build)) ))

(ert-deftest flycheck-define-checker/go-build-handles-packages ()
  :expected-result (flycheck-test-fail-unless-checker 'go-build)
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (flycheck-test-should-syntax-check "checkers/go/src/b1/main.go"
                                            'go-mode)))

(ert-deftest flycheck-define-checker/go-build-missing-package ()
  "Test successful go build with subpackages (used to verify the
GOPATH environment variable is set properly and subpackages can be
found)."
  :expected-result (flycheck-test-fail-unless-checker 'go-build)
  (let ((message (if (flycheck-test-travis-ci-p)
                     ;; The Go version in Travis has a different error message
                     "cannot find package \"b2\" in any of:"
                   "import \"b2\": cannot find package")))
    (flycheck-test-should-syntax-check
     "checkers/go/src/b1/main.go" 'go-mode
     `(4 2 error ,message :checker go-build))))

(ert-deftest flycheck-define-checker/go-test ()
  "Test an import error."
  :expected-result (flycheck-test-fail-unless-checker 'go-test)
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (flycheck-test-should-syntax-check
     "checkers/go/src/test/test-error_test.go" 'go-mode
     '(8 nil error "undefined: fmt" :checker go-test))))

(ert-deftest flycheck-define-checker/haml ()
  :expected-result (flycheck-test-fail-unless-checker 'haml)
  (flycheck-test-should-syntax-check
   "checkers/haml-error.haml" 'haml-mode
   '(5 nil error "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces."
       :checker haml :filename nil)))

(ert-deftest flycheck-define-checker/handlebars ()
  :expected-result (flycheck-test-fail-unless-checker 'handlebars)
  (flycheck-test-should-syntax-check
   "checkers/handlebars-error.hbs" 'handlebars-mode
   '(2 nil error "Expecting 'ID', 'DATA', got 'INVALID'"
       :checker handlebars :filename nil)))

(ert-deftest flycheck-define-checker/haskell-ghc-syntax-error ()
  :expected-result (flycheck-test-fail-unless-checker 'haskell-ghc)
  (flycheck-test-without-checkers haskell-hdevtools
    (flycheck-test-should-syntax-check
     "checkers/haskell-ghc-syntax-error.hs" 'haskell-mode
     '(3 1 error "parse error on input `module'" :checker haskell-ghc))))

(ert-deftest flycheck-define-checker/haskell ()
  :expected-result (flycheck-test-fail-unless-checker
                    'haskell-ghc 'haskell-hlint)
  (flycheck-test-without-checkers haskell-hdevtools
    (flycheck-test-should-syntax-check
     "checkers/haskell.hs" 'haskell-mode
     '(3 1 warning "Top-level binding with no type signature: foo :: Integer"
         :checker haskell-ghc)
     '(6 1 error "Eta reduce
Found:
  bar xs = map lines xs
Why not:
  bar = map lines" :checker haskell-hlint)
     '(9 8 warning "Redundant bracket
Found:
  (putStrLn \"Foobar\")
Why not:
  putStrLn \"Foobar\"" :checker haskell-hlint))))

(ert-deftest flycheck-define-checker/html-tidy ()
  "Test an error caused by an unknown tag."
  :expected-result (flycheck-test-fail-unless-checker 'html-tidy)
  (flycheck-test-should-syntax-check
   "checkers/html-tidy-warning-and-error.html" '(html-mode web-mode)
   '(3 1 warning "missing <!DOCTYPE> declaration"
       :checker html-tidy :filename nil)
   '(8 5 error "<spam> is not recognized!"
       :checker html-tidy :filename nil)
   '(8 5 warning "discarding unexpected <spam>"
       :checker html-tidy :filename nil)))

(ert-deftest flycheck-define-checker/javascript-jshint-syntax-error ()
  "A missing semicolon."
  :expected-result (flycheck-test-fail-unless-checker 'javascript-jshint)
  ;; Silence JS2 and JS3 parsers
  (let ((js2-mode-show-parse-errors nil)
        (js2-mode-show-strict-warnings nil)
        (js3-mode-show-parse-errors nil))
    (flycheck-test-should-syntax-check
     "checkers/javascript-syntax-error.js" '(js-mode js2-mode js3-mode)
     '(3 11 error "Unclosed string." :checker javascript-jshint)
     '(3 25 error "Unclosed string." :checker javascript-jshint)
     '(4 1 error "Unclosed string." :checker javascript-jshint)
     '(4 1 error "Missing semicolon." :checker javascript-jshint))))

(ert-deftest flycheck-define-checker/javascript-jshint-error-disabled ()
  "An unused variable."
  :expected-result (flycheck-test-fail-unless-checker 'javascript-jshint)
  (flycheck-test-should-syntax-check
   "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)))

(ert-deftest flycheck-define-checker/javascript-jshint ()
  "An unused variable."
  :expected-result (flycheck-test-fail-unless-checker 'javascript-jshint)
  (flycheck-test-with-hook (js-mode-hook js2-mode-hook js3-mode-hook)
      (setq flycheck-jshintrc "jshintrc")
    (flycheck-test-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 12 error "'foo' is defined but never used."
         :checker javascript-jshint))))

(ert-deftest flycheck-define-checker/javascript-gjslint ()
  :expected-result (flycheck-test-fail-unless-checker 'javascript-gjslint)
  (flycheck-test-without-checkers javascript-jshint
    (flycheck-test-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 nil error "E:0131: Single-quoted string preferred over double-quoted string."
         :checker javascript-gjslint :filename nil)
     '(4 nil error "E:0001: Extra space before \"]\""
         :checker javascript-gjslint :filename nil))))

(ert-deftest flycheck-define-checker/json-jsonlint ()
  "Test a syntax error from multiple top-level objects."
  :expected-result (flycheck-test-fail-unless-checker 'json-jsonlint)
  (flycheck-test-should-syntax-check
   "checkers/json-jsonlint-error.json" 'text-mode
    '(1 42 error "found: ',' - expected: 'EOF'." :checker json-jsonlint)))

(ert-deftest flycheck-define-checker/less ()
  :expected-result (flycheck-test-fail-unless-checker 'less)
  (flycheck-test-should-syntax-check
   "checkers/less-file-error.less" 'less-css-mode
   '(3 1 error "'no-such-file.less' wasn't found" :checker less)))

(ert-deftest flycheck-define-checker/less ()
  :expected-result (flycheck-test-fail-unless-checker 'less)
  (flycheck-test-should-syntax-check
   "checkers/less-syntax-error.less" 'less-css-mode
   '(2 1 error "missing closing `}`" :checker less)))

(ert-deftest flycheck-define-checker/lua ()
  :expected-result (flycheck-test-fail-unless-checker 'lua)
  (flycheck-test-should-syntax-check
   "checkers/lua-syntax-error.lua" 'lua-mode
   '(5 nil error "unfinished string near '\"oh no'"
       :checker lua :filename nil)))

(ert-deftest flycheck-define-checker/perl ()
  "Test an unused variable with the Perl checker."
  :expected-result (flycheck-test-fail-unless-checker 'perl)
  (flycheck-test-should-syntax-check
   "checkers/perl-error.pl" '(perl-mode cperl-mode)
   '(4 nil error "Name \"main::x\" used only once: possible typo"
       :checker perl)))

(ert-deftest flycheck-define-checker/perl-syntax-error ()
  :expected-result (flycheck-test-fail-unless-checker 'perl)
  "Test a syntax error with the Perl checker."
  (flycheck-test-should-syntax-check
   "checkers/perl-syntax-error.pl" '(perl-mode cperl-mode)
   '(4 nil error "syntax error" :checker perl)))

(ert-deftest flycheck-define-checker/php-syntax-error ()
  "Test the T_PAAMAYIM_NEKUDOTAYIM error."
  :expected-result (flycheck-test-fail-unless-checker 'php)
  (flycheck-test-should-syntax-check
   "checkers/php-syntax-error.php" 'php-mode
   '(8 nil error "syntax error, unexpected ')', expecting '('" :checker php)))

(ert-deftest flycheck-define-checker/php ()
  :expected-result (flycheck-test-fail-unless-checkers 'php-phpcs 'php-phpmd)
  (flycheck-test-should-syntax-check
   "checkers/php.php" 'php-mode
   '(19 6 error "Missing class doc comment" :checker php-phpcs)
   '(21 nil warning "Avoid unused private fields such as '$FOO'."
        :checker php-phpmd)
   '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore" :checker php-phpcs)
   '(22 nil warning "Avoid unused private methods such as 'bar'."
        :checker php-phpmd)
   '(22 nil warning "Avoid unused parameters such as '$baz'."
        :checker php-phpmd)
   '(22 13 error "Missing function doc comment" :checker php-phpcs)
   '(22 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
        :checker php-phpcs)
   '(24 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
        :checker php-phpmd)
   '(24 nil warning "Avoid unused local variables such as '$i'."
        :checker php-phpmd)
   '(24 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
        :checker php-phpcs)))

(ert-deftest flycheck-define-checker/php-phpmd-rulesets ()
  :expected-result (flycheck-test-fail-unless-checkers 'php-phpcs
                                                            'php-phpmd)
  (flycheck-test-with-hook php-mode-hook
      (setq flycheck-phpmd-rulesets (remove "unusedcode" flycheck-phpmd-rulesets))
    (flycheck-test-should-syntax-check
     "checkers/php.php" 'php-mode
     '(19 6 error "Missing class doc comment" :checker php-phpcs)
     '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore" :checker php-phpcs)
     '(22 13 error "Missing function doc comment" :checker php-phpcs)
     '(22 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
          :checker php-phpcs)
     '(24 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
          :checker php-phpmd)
     '(24 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
          :checker php-phpcs))))

(ert-deftest flycheck-define-checker/php-phpcs-standard ()
  :expected-result (flycheck-test-fail-unless-checkers 'php-phpcs
                                                            'php-phpmd)
  (flycheck-test-with-hook php-mode-hook
      (setq flycheck-phpcs-standard "Zend")
    (flycheck-test-should-syntax-check
     "checkers/php.php" 'php-mode
     '(21 nil warning "Avoid unused private fields such as '$FOO'."
          :checker php-phpmd)
     '(21 20 error "Private member variable \"FOO\" must contain a leading underscore"
          :checker php-phpcs)
     '(22 nil warning "Avoid unused private methods such as 'bar'."
          :checker php-phpmd)
     '(22 nil warning "Avoid unused parameters such as '$baz'."
          :checker php-phpmd)
     '(24 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
          :checker php-phpmd)
     '(24 nil warning "Avoid unused local variables such as '$i'."
          :checker php-phpmd)
     '(28 1 error "A closing tag is not permitted at the end of a PHP file"
          :checker php-phpcs))))

(ert-deftest flycheck-define-checker/puppet-parser-singleline-syntax-error ()
  "Test a real syntax error with puppet parser."
  :expected-result (flycheck-test-fail-unless-checker 'puppet-parser)
  (flycheck-test-should-syntax-check
   "checkers/puppet-parser-singleline.pp" 'puppet-mode
   '(3 nil error "Syntax error at ','; expected '}'" :checker puppet-parser)))

(ert-deftest flycheck-define-checker/puppet-parser-multiline-syntax-error ()
  "Test a real (multi line) syntax error with puppet parser."
  :expected-result (flycheck-test-fail-unless-checker 'puppet-parser)
  (flycheck-test-should-syntax-check
   "checkers/puppet-parser-multiline.pp" 'puppet-mode
   '(8 nil error "Unclosed quote after '' in 'something
}
'" :checker puppet-parser)))

(ert-deftest flycheck-define-checker/puppet-lint ()
  :expected-result (flycheck-test-fail-unless-checker 'puppet-lint)
  (flycheck-test-should-syntax-check
   "checkers/puppet-lint.pp" 'puppet-mode
   '(2 nil error "foo::bar not in autoload module layout" :checker puppet-lint)
   '(3 nil warning "case statement without a default case"
       :checker puppet-lint)))

(ert-deftest flycheck-define-checker/python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  :expected-result (flycheck-test-fail-unless-checker 'python-flake8)
  (flycheck-test-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode
    '(3 13 error "E901 SyntaxError: invalid syntax" :checker python-flake8)))

(ert-deftest flycheck-define-checker/python-flake8-warning-ignored ()
  :expected-result (flycheck-test-fail-unless-checker 'python-flake8)
  (flycheck-test-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-test-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase"
         :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck-define-checker/python-flake8-maximum-complexity ()
  "Test superfluous spaces with flake8."
  :expected-result (flycheck-test-fail-unless-checker 'python-flake8)
  (flycheck-test-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-complexity 4)
    (flycheck-test-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(5 1 warning "F401 'antigravit' imported but unused"
         :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 1 warning "C901 'Spam.with_ham' is too complex (4)"
          :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck-define-checker/python-flake8-error-maximum-line-length ()
  :expected-result (flycheck-test-fail-unless-checker 'python-flake8)
  (flycheck-test-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-line-length 45)
    (flycheck-test-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(5 1 warning "F401 'antigravit' imported but unused"
         :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(10 46 error "E501 line too long (46 > 45 characters)"
          :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck-define-checker/python-flake8 ()
  "PEP8 compliant names with Flake8 and pep8-naming."
  :expected-result (flycheck-test-fail-unless-checker 'python-flake8)
  (flycheck-test-should-syntax-check
   "checkers/python/test.py" 'python-mode
   '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
   '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
   '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
   '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
        :checker python-flake8)
   '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
        :checker python-flake8)
   '(22 1 warning "F821 undefined name 'antigravity'"
        :checker python-flake8)))

(ert-deftest flycheck-define-checker/python-pylint-syntax-error ()
  "Test a real syntax error with pylint."
  :expected-result (flycheck-test-fail-unless-checker 'python-pylint)
  (flycheck-test-without-checkers python-flake8
    (flycheck-test-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     '(3 nil error "invalid syntax (E0001)" :checker python-pylint))))

(ert-deftest flycheck-define-checker/python-pylint ()
  :expected-result (flycheck-test-fail-unless-checker 'python-pylint)
  (flycheck-test-without-checkers python-flake8
    (flycheck-test-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
     '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
     '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
         :checker python-pylint)
     '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
     '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
     '(9 4 info "Invalid method name \"withEggs\" (C0103)"
         :checker python-pylint)
     '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(10 15 warning "Used builtin function 'map' (W0141)"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))))

(ert-deftest flycheck-define-checker/rst ()
  :expected-result (flycheck-test-fail-unless-checker 'rst)
  (flycheck-test-should-syntax-check
   "checkers/rst.rst" 'rst-mode
   '(8 nil warning "Title underline too short." :checker rst)
   '(14 nil error "Unexpected section title." :checker rst)
   '(16 nil error "Unknown target name: \"restructuredtext\"." :checker rst)
   '(19 nil warning "Title underline too short." :checker rst)
   '(21 nil error "Unknown target name: \"cool\"." :checker rst)
   '(26 nil error "Unexpected section title." :checker rst)))

(ert-deftest flycheck-define-checker/ruby-rubocop-syntax-error ()
  :expected-result (flycheck-test-fail-unless-checker 'ruby-rubocop)
  (flycheck-test-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode
   '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubocop)
   '(5 24 error "unterminated string meets end of file" :checker ruby-rubocop)))

(ert-deftest flycheck-define-checker/ruby-rubylint-syntax-error ()
  :expected-result (flycheck-test-fail-unless-checker 'ruby-rubocop)
  (flycheck-test-without-checkers ruby-rubocop
    (flycheck-test-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubylint))))

(ert-deftest flycheck-define-checker/ruby ()
  :expected-result (flycheck-test-fail-unless-checker 'ruby-rubocop
                                                           'ruby-rubylint)
  (flycheck-test-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(1 1 info "Missing utf-8 encoding comment." :checker ruby-rubocop)
     '(4 18 warning "unused argument name" :checker ruby-rubylint)
     '(5 5 warning "unused local variable arr" :checker ruby-rubylint)
     '(5 5 warning "Useless assignment to variable - arr" :checker ruby-rubocop)
     '(5 18 info "Use snake_case for symbols." :checker ruby-rubocop)
     '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
         :checker ruby-rubocop)
     '(10 5 info "the use of then/do is not needed here" :checker ruby-rubylint)
     '(10 5 info "Favor modifier if/unless usage when you have a single-line body. Another good alternative is the usage of control flow &&/||."
          :checker ruby-rubocop)
     '(10 5 info "Never use then for multi-line if/unless."
          :checker ruby-rubocop)
     '(10 8 warning "Literal true appeared in a condition."
          :checker ruby-rubocop)
     '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
     '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
          :checker ruby-rubylint)))

(ert-deftest flycheck-define-checker/ruby-rubocop-disabled-warning ()
  :expected-result (flycheck-test-fail-unless-checker 'ruby-rubocop
                                                           'ruby-rubylint)
  (flycheck-test-with-hook ruby-mode-hook
      (setq flycheck-rubocoprc "rubocop.yml")
    (flycheck-test-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(1 1 info "Missing utf-8 encoding comment." :checker ruby-rubocop)
     '(4 18 warning "unused argument name" :checker ruby-rubylint)
     '(5 5 warning "unused local variable arr" :checker ruby-rubylint)
     '(5 5 warning "Useless assignment to variable - arr" :checker ruby-rubocop)
     '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
         :checker ruby-rubocop)
     '(10 5 info "the use of then/do is not needed here" :checker ruby-rubylint)
     '(10 5 info "Favor modifier if/unless usage when you have a single-line body. Another good alternative is the usage of control flow &&/||."
          :checker ruby-rubocop)
     '(10 5 info "Never use then for multi-line if/unless."
          :checker ruby-rubocop)
     '(10 8 warning "Literal true appeared in a condition."
          :checker ruby-rubocop)
     '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
     '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
          :checker ruby-rubylint))))

(ert-deftest flycheck-define-checker/rust ()
  :expected-result (flycheck-test-fail-unless-checker 'rust)
  (flycheck-test-should-syntax-check
   "checkers/rust-syntax-error.rs" 'rust-mode
   '(3 10 error "expected `{` but found `bla`" :checker rust)))

(ert-deftest flycheck-define-checker/sass ()
  "Test a syntax error caused by inconsistent indentation."
  :expected-result (flycheck-test-fail-unless-checker 'sass)
  (flycheck-test-should-syntax-check
   "checkers/sass-error.sass" 'sass-mode
    '(5 nil error "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces."
        :checker sass)))

(ert-deftest flycheck-define-checker/scala ()
  :expected-result (if (flycheck-test-travis-ci-p) :failed
                     (flycheck-test-fail-unless-checker 'scala))
  (flycheck-test-not-on-travis)
  (flycheck-test-should-syntax-check
   "checkers/scala-syntax-error.scala" 'scala-mode
   '(3 nil error "identifier expected but '{' found." :checker scala)))

(ert-deftest flycheck-define-checker/scss ()
  :expected-result (flycheck-test-fail-unless-checker 'scss)
  (flycheck-test-should-syntax-check
   "checkers/scss-error.scss" 'scss-mode
   '(3 nil error "Invalid CSS after \"        c olor:\": expected pseudoclass or pseudoelement, was \" red;\""
       :checker scss)))

(ert-deftest flycheck-define-checker/sh-bash ()
  :expected-result (flycheck-test-fail-unless-checker 'sh-bash)
  (flycheck-test-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-test-without-checkers sh-dash
      (flycheck-test-should-syntax-check
       "checkers/sh-syntax-error.sh" 'sh-mode
       '(3 nil error "syntax error near unexpected token `('" :checker sh-bash)
       '(3 nil error "`cat <(echo blah)'" :checker sh-bash)))))

(ert-deftest flycheck-define-checker/sh-dash ()
  :expected-result (flycheck-test-fail-unless-checker 'sh-dash)
  (flycheck-test-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-test-should-syntax-check
     "checkers/sh-syntax-error.sh" 'sh-mode
     '(3 nil error "Syntax error: \"(\" unexpected" :checker sh-dash))))

(ert-deftest flycheck-define-checker/slim ()
  (flycheck-test-fail-unless-checker 'slim)
  (let* ((slim-version (cadr (split-string (car (process-lines "slimrb" "-v")))))
         ;; Old Slim compilers do not report column information
         (column (if (version<= "1.3.1" slim-version) 1 nil)))
    (flycheck-test-should-syntax-check
     "checkers/slim-error.slim" 'slim-mode
     `(2 ,column error "Unexpected indentation" :checker slim))))

(ert-deftest flycheck-define-checker/tex-chktex ()
  :expected-result (flycheck-test-fail-unless-checker 'tex-chktex)
  (flycheck-test-should-syntax-check
   "checkers/tex-warning.tex" 'latex-mode
   '(5 28 warning "13:Intersentence spacing (`\\@') should perhaps be used."
       :checker tex-chktex)))

(ert-deftest flycheck-define-checker/tex-lacheck ()
  :expected-result (flycheck-test-fail-unless-checker 'tex-lacheck)
  (flycheck-test-without-checkers tex-chktex
    (flycheck-test-should-syntax-check
     "checkers/tex-warning.tex" 'latex-mode
     '(5 nil warning "missing `\\@' before `.' in \"GNU.\""
         :checker tex-lacheck)
     '(7 nil warning "possible unwanted space at \"{\""
         :checker tex-lacheck))))

(ert-deftest flycheck-define-checker/xml-xmlstarlet ()
  :expected-result (flycheck-test-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-test-should-syntax-check
   "checkers/xml-syntax-error.xml" 'nxml-mode
   '(4 10 error "Opening and ending tag mismatch: spam line 3 and with"
       :checker xml-xmlstarlet)))

(ert-deftest flycheck-define-checker/xml-xmllint-error ()
  :expected-result (flycheck-test-fail-unless-checker 'xml-xmllint)
  (flycheck-test-without-checkers xml-xmlstarlet
    (flycheck-test-should-syntax-check
     "checkers/xml-syntax-error.xml" 'nxml-mode
     '(4 nil error "parser error : Opening and ending tag mismatch: spam line 3 and with"
         :checker xml-xmllint)
     '(5 nil error "parser error : Extra content at the end of the document"
         :checker xml-xmllint))))

(ert-deftest flycheck-define-checker/yaml-jsyaml ()
  :expected-result (flycheck-test-fail-unless-checker 'yaml-jsyaml)
  (flycheck-test-should-syntax-check
   "checkers/yaml-syntax-error.yaml" 'yaml-mode
   '(4 5 error "bad indentation of a mapping entry"
       :checker yaml-jsyaml :filename nil)))

(ert-deftest flycheck-define-checker/yaml-ruby ()
  (let* ((ruby-version (car (process-lines "ruby" "-e" "puts RUBY_VERSION")))
         (psych-version (when (version<= "1.9.3" ruby-version)
                          (car (process-lines "ruby" "-rpsych"
                                              "-e" "puts Psych::VERSION"))))
         (expected-error
          (cond
           ;; Syck parser in Ruby 1.9.2 and lower
           ((version< ruby-version "1.9.3")
            '(3 5 error "a1: bar" :checker yaml-ruby))
           ;; Psych parser in Ruby 1.9.3 and up.  The Psych errors apparently
           ;; vary between different versions, so we have to adapt.  Ruby, you
           ;; suck.
           ((version< psych-version "1.2.2")
            '(3 4 error "couldn't parse YAML" :checker yaml-ruby))
           (:else
            '(4 5 error "mapping values are not allowed in this context"
                :checker yaml-ruby)))))
    (flycheck-test-without-checkers yaml-jsyaml
      (flycheck-test-should-syntax-check
       "checkers/yaml-syntax-error.yaml" 'yaml-mode expected-error))))

(ert-deftest flycheck-define-checker/zsh ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-test-fail-unless-checker 'zsh)
  (flycheck-test-with-hook sh-mode-hook
      (sh-set-shell "zsh" :no-query)
    (flycheck-test-should-syntax-check
     "checkers/zsh-syntax-error.zsh" 'sh-mode
     '(5 nil error "parse error near `fi'" :checker zsh))))

;;; flycheck-test.el ends here
