;;; flycheck-test.el --- Flycheck: Unit test suite   -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
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

;;; Tags

;; For new tests, please add the the section tag.  Additionally, add any of the
;; following tags, if appropriate:
;;
;; - `external-tool' for tests which need external tools
;; - `language-LANG' for tests for the language LANG

;;; Code:


;;; Requirements

(require 'flycheck)
(require 'dash)
(require 'cl-lib)
(require 'macroexp)                     ; For macro utilities
(require 'epa-file)                     ; To test encrypted buffers
(require 'ert)                          ; Unit test library
(require 'shut-up)                      ; Silence Emacs and intercept `message'

;; Make a best effort to make Coq Mode available
(mapc (lambda (dir)
        (add-to-list 'load-path (expand-file-name "coq/" dir)))
      '("/usr/share/emacs/site-lisp/"
        "/usr/local/share/emacs/site-lisp/"))

(autoload 'coq-mode "coq")


;;; Compatibility

(eval-and-compile
  ;; Provide `ert-skip' and friends for Emacs 24.3
  (defconst flycheck-test-ert-can-skip (fboundp 'ert-skip)
    "Whether ERT supports test skipping.")

  (unless flycheck-test-ert-can-skip
    ;; Fake skipping

    (put 'flycheck-test-skipped 'error-message "Test skipped")
    (put 'flycheck-test-skipped 'error-conditions '(error))

    (defun ert-skip (data)
      (signal 'flycheck-test-skipped data))

    (defmacro skip-unless (form)
      `(unless (ignore-errors ,form)
         (signal 'flycheck-test-skipped ',form)))

    (defun ert-test-skipped-p (result)
      (and (ert-test-failed-p result)
           (eq (car (ert-test-failed-condition result))
               'flycheck-test-skipped)))))


;;; Directories

(defconst flycheck-test-directory
  (file-name-directory (flycheck-current-load-file))
  "The test directory.")

(defconst flycheck-test-resources-directory
  (expand-file-name "resources/" flycheck-test-directory)
  "Directory of test resources.")

(defconst flycheck-test-source-directory
  (file-name-directory (directory-file-name flycheck-test-directory))
  "The source directory.")

(defconst flycheck-test-package-directory
  (expand-file-name (format ".cask/%s/elpa/" emacs-version)
                    flycheck-test-source-directory))


;;; Utilities

(defmacro flycheck-test-with-temp-buffer (&rest body)
  "Eval BODY within a temporary buffer.

Like `with-temp-buffer', but resets the modification state of the
temporary buffer to make sure that it is properly killed even if
it has a backing file and is modified."
  (declare (indent 0))
  `(with-temp-buffer
     (unwind-protect
         ,(macroexp-progn body)
       ;; Reset modification state of the buffer, and unlink it from its backing
       ;; file, if any, because Emacs refuses to kill modified buffers with
       ;; backing files, even if they are temporary.
       (set-buffer-modified-p nil)
       (set-visited-file-name nil 'no-query))))

(defmacro flycheck-test-with-file-buffer (file-name &rest body)
  "Create a buffer from FILE-NAME and eval BODY.

BODY is evaluated with `current-buffer' being a buffer with the
contents FILE-NAME."
  (declare (indent 1))
  `(let ((file-name ,file-name))
     (unless (file-exists-p file-name)
       (error "%s does not exist" file-name))
     (flycheck-test-with-temp-buffer
       (insert-file-contents file-name 'visit)
       (set-visited-file-name file-name 'no-query)
       (cd (file-name-directory file-name))
       ;; Mark the buffer as not modified, because we just loaded the file up to
       ;; now.
       (set-buffer-modified-p nil)
       ,@body)))

(defmacro flycheck-test-with-env (env &rest body)
  "Add ENV to `process-environment' in BODY.

Execute BODY with a `process-environment' with contains all
variables from ENV added.

ENV is an alist, where each cons cell `(VAR . VALUE)' is a
environment variable VAR to be added to `process-environment'
with VALUE."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence process-environment)))
     (pcase-dolist (`(,var . ,value) ,env)
       (setenv var value))
     ,@body))

(defmacro flycheck-test-with-global-mode (&rest body)
  "Execute BODY with Global Flycheck Mode enabled."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (global-flycheck-mode 1)
         ,@body)
     (global-flycheck-mode -1)))

(defmacro flycheck-test-with-help-buffer (&rest body)
  "Execute BODY and kill the help buffer afterwards."
  (declare (indent 0))
  `(unwind-protect
       ,(macroexp-progn body)
     (when (buffer-live-p (get-buffer (help-buffer)))
       (kill-buffer (help-buffer)))))


;;; Environment information and tests

(defun flycheck-test-min-emacs-version-p (major &optional minor)
  "Determine whether Emacs has the required version.

Return t if Emacs is at least MAJOR.MINOR, or nil otherwise."
  (when (>= emacs-major-version major)
    (or (null minor) (>= emacs-minor-version minor))))

(defconst flycheck-test-user-error-type
  (if (flycheck-test-min-emacs-version-p 24 3) 'user-error 'error)
  "The `user-error' type used by Flycheck.")

(defun flycheck-test-travis-ci-p ()
  "Determine whether the tests are running on Travis CI."
  (string= (getenv "TRAVIS") "true"))

(defun flycheck-test-check-gpg ()
  "Check whether GPG is available."
  (or (epg-check-configuration (epg-configuration)) t))

(defun flycheck-test-extract-version-command (re executable &rest args)
  "Use RE to extract the version from EXECUTABLE with ARGS.

Run EXECUTABLE with ARGS, catch the output, and apply RE to find
the version number.  Return the text captured by the first group
in RE, or nil, if EXECUTABLE is missing, or if RE failed to
match."
  (-when-let (executable (executable-find executable))
    (with-temp-buffer
      (apply #'call-process executable nil t nil args)
      (goto-char (point-min))
      (when (re-search-forward re nil 'no-error)
        (match-string 1)))))

(defun flycheck-test-texinfo-version ()
  "Determine the version of Texinfo.

Return the version as string, or nil, if the texinfo version
could not be determined."
  (flycheck-test-extract-version-command
   (rx "makeinfo (GNU texinfo) "
       (group (one-or-more (any digit)) "." (one-or-more (any digit))))
   "makeinfo" "--version"))

(defun flycheck-test-cppcheck-version ()
  "Determine the version of Cppcheck.

Return the version as string, or nil, if the Cppcheck version
could not be determined."
  (flycheck-test-extract-version-command
   (rx "Cppcheck "
       (group (one-or-more (any digit)) "." (one-or-more (any digit)))
       (zero-or-more any))
   "cppcheck" "--version"))

(defun flycheck-test-rubylint-version ()
  "Determine the version of rubylint.

Return the version as string, or nil, if the rubylint version
could not be determined."
  (flycheck-test-extract-version-command
   (rx "ruby-lint v" (group (one-or-more (any digit))
                            (one-or-more "." (one-or-more (any digit)))) " on")
   "ruby-lint" "--version"))

(defun flycheck-test-coq-version ()
  "Determine the version of Coq.

Return the version as string, or nil if the version could not be
determined."
  (flycheck-test-extract-version-command
   (rx "The Coq Proof Assistant, version "
       (group (one-or-more (any digit)) "." (one-or-more (any digit))))
   "coqtop" "-v"))


;;; Test resources

(defun flycheck-test-resource-filename (resource-file)
  "Determine the absolute file name of a RESOURCE-FILE.

Relative file names are expanded against
`flycheck-test-resources-directory'."
  (expand-file-name resource-file flycheck-test-resources-directory))

(defun flycheck-test-locate-config-file (filename _checker)
  "Find a configuration FILENAME within unit tests.

_CHECKER is ignored."
  (let* ((directory (flycheck-test-resource-filename "checkers/config-files"))
         (filepath (expand-file-name filename directory)))
    (when (file-exists-p filepath)
      filepath)))

(defmacro flycheck-test-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY.

The absolute file name of RESOURCE-FILE is determined with
`flycheck-test-resource-filename'."
  (declare (indent 1))
  `(flycheck-test-with-file-buffer
       (flycheck-test-resource-filename ,resource-file)
     ,@body))


;;; Syntax checking in checks

(defvar-local flycheck-test-syntax-checker-finished nil
  "Non-nil if the current checker has finished.")

(add-hook 'flycheck-after-syntax-check-hook
          (lambda () (setq flycheck-test-syntax-checker-finished t)))

(defconst flycheck-test-checker-wait-time 10
  "Time to wait until a checker is finished in seconds.

After this time has elapsed, the checker is considered to have
failed, and the test aborted with failure.")

(put 'flycheck-test-syntax-check-timed-out 'error-message
     "Syntax check timed out.")
(put 'flycheck-test-syntax-check-timed-out 'error-conditions '(error))

(defun flycheck-test-wait-for-syntax-checker ()
  "Wait until the syntax check in the current buffer is finished."
  (let ((starttime (float-time)))
    (while (and (not flycheck-test-syntax-checker-finished)
                (< (- (float-time) starttime) flycheck-test-checker-wait-time))
      (sleep-for 1))
    (unless (< (- (float-time) starttime) flycheck-test-checker-wait-time)
      (flycheck-stop-checker)
      (signal 'flycheck-test-syntax-check-timed-out nil)))
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
  (should (not (-any? (lambda (ov) (overlay-get ov 'flycheck-overlay))
                      (overlays-in (point-min) (point-max))))))


;;; Unit test predicates

(defun flycheck-test-should-overlay (error)
  "Test that ERROR has an overlay."
  (let* ((overlay (-first (lambda (ov) (equal (overlay-get ov 'flycheck-error)
                                              error))
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
    (let ((expected (mapcar (apply-partially #'apply #'flycheck-error-new-at)
                            errors)))
      (should (equal expected flycheck-current-errors))
      (mapc #'flycheck-test-should-overlay expected))
    (should (= (length errors)
               (length (flycheck-overlays-in (point-min) (point-max)))))))

(defun flycheck-test-should-syntax-check (resource-file modes &rest errors)
  "Test a syntax check in RESOURCE-FILE with MODES.

RESOURCE-FILE is the file to check.  MODES is a single major mode
symbol or a list thereof, specifying the major modes to syntax
check with.  ERRORS is the list of expected errors."
  (when (symbolp modes)
    (setq modes (list modes)))
  (dolist (mode modes)
    (unless (fboundp mode)
      (ert-skip (format "%S missing" mode)))
    (flycheck-test-with-resource-buffer resource-file
      (funcall mode)
      ;; Configure config file locating for unit tests
      (dolist (fn '(flycheck-locate-config-file-absolute-path
                    flycheck-test-locate-config-file))
        (add-hook 'flycheck-locate-config-file-functions fn 'append 'local))
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


;;; Test results

(defun flycheck-test-failed-on-travis-ci-p (result)
  "Determine whether RESULT is failed on Travis CI."
  (and (flycheck-test-travis-ci-p) (ert-test-failed-p result)))


;;; Code style
(ert-deftest flycheck-code-style/source-properly-indented ()
  :tags '(style)
  (cl-letf ((flycheck (expand-file-name "flycheck.el"
                                        flycheck-test-source-directory))
            ((get 'with-demoted-errors 'lisp-indent-function) 1))
    (flycheck-test-with-file-buffer flycheck
      (emacs-lisp-mode)
      (shut-up
        (indent-region (point-min) (point-max)))
      (should-not (buffer-modified-p)))))

(ert-deftest flycheck-code-style/test-suite-properly-indented ()
  :tags '(style)
  (let ((flycheck-test (expand-file-name "flycheck-test.el"
                                         flycheck-test-directory)))
    (flycheck-test-with-file-buffer flycheck-test
      (emacs-lisp-mode)
      (shut-up
        (indent-region (point-min) (point-max)))
      (should-not (buffer-modified-p)))))


;;; Customization

(ert-deftest flycheck-checkers/there-are-registered-checkers ()
  :tags '(customization)
  (should flycheck-checkers))

(ert-deftest flycheck-checkers/all-registered-checkers-are-declared ()
  :tags '(customization)
  (dolist (checker flycheck-checkers)
    (should (flycheck-valid-checker-p checker))
    (should (flycheck-registered-checker-p checker))))

(ert-deftest flycheck-checkers/all-declared-checkers-are-registered ()
  :tags '(customization)
  (let ((declared-checkers (flycheck-defined-checkers)))
    (should declared-checkers)
    (dolist (checker declared-checkers)
      (should (memq checker flycheck-checkers))
      (should (flycheck-registered-checker-p checker)))))

(ert-deftest flycheck-checkers/should-have-either-patterns-or-parser ()
  :tags '(customization)
  (dolist (checker flycheck-checkers)
    (let ((patterns (flycheck-checker-error-patterns checker))
          (parser (flycheck-checker-error-parser checker)))
      (should checker)
      (should (or (and (eq parser 'flycheck-parse-with-patterns) patterns)
                  (null patterns))))))

(ert-deftest flycheck-checkers/all-registered-checkers-are-enabled ()
  :tags '(customization)
  (dolist (checker flycheck-checkers)
    (should (flycheck-enabled-checker-p checker)))
  (should (equal flycheck-checkers (flycheck-enabled-checkers))))

(ert-deftest flycheck-checkers/no-registered-checker-is-enabled ()
  :tags '(customization)
  (dolist (checker flycheck-checkers)
    (should-not (flycheck-disabled-checker-p checker))))

(ert-deftest flycheck-disabled-checkers/is-empty ()
  :tags '(customization)
  (should-not (default-value 'flycheck-disabled-checkers)))

(ert-deftest flycheck-locate-config-file-functions/default ()
  :tags '(customization)
  (should (equal flycheck-locate-config-file-functions
                 '(flycheck-locate-config-file-absolute-path
                   flycheck-locate-config-file-ancestor-directories
                   flycheck-locate-config-file-home))))

(ert-deftest flycheck-process-error-functions/defaults-to-add-overlay ()
  :tags '(customization)
  (should (equal flycheck-process-error-functions '(flycheck-add-overlay))))

(ert-deftest flycheck-display-errors-delay/defaults-to-0.9 ()
  :tags '(customization)
  (should (equal flycheck-display-errors-delay 0.9)))

(ert-deftest flycheck-display-errors-function/defaults-to-display-error-messages ()
  :tags '(customization)
  (should (eq flycheck-display-errors-function
              #'flycheck-display-error-messages)))

(ert-deftest flycheck-indication-mode/defaults-to-left-fringe ()
  :tags '(customization)
  (should (eq flycheck-indication-mode 'left-fringe)))

(ert-deftest flycheck-highlighting-mode/defaults-to-symbols ()
  :tags '(customization)
  (should (eq flycheck-highlighting-mode 'symbols)))

(ert-deftest flycheck-check-syntax-automatically/defaults-to-all-events ()
  :tags '(customization)
  (should (equal flycheck-check-syntax-automatically
                 '(save idle-change new-line mode-enabled))))

(ert-deftest flycheck-idle-change-delay/defaults-to-0.5 ()
  :tags '(customization)
  (should (equal flycheck-idle-change-delay 0.5)))

(ert-deftest flycheck-google-max-messages/defaults-to-5 ()
  :tags '(customization)
  (should (equal flycheck-google-max-messages 5)))

(ert-deftest flycheck-standard-error-navigation/default-to-t ()
  :tags '(customization)
  (should (eq flycheck-standard-error-navigation t)))

(ert-deftest flycheck-completion-system/defaults-to-nil ()
  :tags '(customization)
  (should (eq flycheck-completion-system nil)))

(ert-deftest flycheck-CHECKER-executable/is-special-variable ()
  :tags '(customization)
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (custom-variable-p variable)))))

(ert-deftest flycheck-CHECKER-executable/is-customizable ()
  :tags '(customization)
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (custom-variable-p variable)))))

(ert-deftest flycheck-CHECKER-executable/defaults-to-nil ()
  :tags '(customization)
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (null (symbol-value variable))))))

(ert-deftest flycheck-keymap-prefix/customize-variable-will-modify-keymap ()
  :tags '(customization)
  (unwind-protect
      (progn
        (custom-set-variables '(flycheck-keymap-prefix (kbd "C-c e")))
        (should (eq 'flycheck-next-error
                    (lookup-key flycheck-mode-map (kbd "C-c e n")))))
    (ignore-errors
      (custom-set-variables '(flycheck-keymap-prefix (kbd "C-c !"))))))

(ert-deftest flycheck-temp-prefix/default ()
  :tags '(customization)
  (should (equal flycheck-temp-prefix "flycheck")))


;;; Minor mode definition

(ert-deftest flycheck-mode/enables-standard-error-navigation ()
  :tags '(minor-mode)
  (flycheck-test-with-temp-buffer
    (setq next-error-function :old)
    (flycheck-mode 1)
    (should flycheck-mode)
    (should (eq next-error-function 'flycheck-next-error-function))
    (flycheck-mode -1)
    (should-not flycheck-mode)
    (should (eq next-error-function :old))))

(ert-deftest flycheck-mode/does-not-enable-standard-error-navigation ()
  :tags '(minor-mode)
  (flycheck-test-with-temp-buffer
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

(ert-deftest flycheck-mode/clears-errors-after-revert ()
  :tags '(minor-mode language-emacs-lisp)
  (flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (goto-char (point-min))
    (insert "foo-bar")
    (flycheck-mode)
    (flycheck-test-buffer-sync)
    (should flycheck-current-errors)
    (revert-buffer 'ignore-auto 'no-confirm)
    (should-not flycheck-current-errors)
    (should-not (flycheck-deferred-check-p))))


;;; Global syntax checking

(ert-deftest flycheck-may-enable-mode/not-in-ephemeral-buffers ()
  :tags '(global-mode)
  (flycheck-test-with-temp-buffer
    (should-not (flycheck-may-enable-mode)))
  (flycheck-test-with-temp-buffer
    (setq buffer-file-name "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (rename-buffer " foo")
    (should (string= (buffer-name) " foo"))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode/not-in-encrypted-files ()
  :tags '(global-mode external-tool)
  (skip-unless (flycheck-test-check-gpg))
  (let* ((filename (flycheck-test-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-test-with-resource-buffer filename
      (emacs-lisp-mode)
      (should (flycheck-get-checker-for-buffer))
      (should-not (flycheck-may-enable-mode)))))

(ert-deftest flycheck-may-enable-mode/not-if-no-checker-is-found ()
  :tags '(global-mode)
  (flycheck-test-with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (text-mode)
    (should-not (string-prefix-p " " (buffer-name)))
    (should-not (flycheck-get-checker-for-buffer))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode/checker-found ()
  :tags '(global-mode)
  (flycheck-test-with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (should (flycheck-may-enable-mode))))

(ert-deftest global-flycheck-mode/does-not-enable-in-ephemeral-buffers ()
  :tags '(global-mode)
  (flycheck-test-with-global-mode
    (flycheck-test-with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer " foo")
      (emacs-lisp-mode)
      (should-not flycheck-mode))))

(ert-deftest global-flycheck-mode/does-not-enable-in-encrypted-file ()
  :tags '(global-mode external-tool)
  (skip-unless (flycheck-test-check-gpg))
  (let* ((filename (flycheck-test-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-test-with-global-mode
      (flycheck-test-with-resource-buffer filename
        (emacs-lisp-mode)
        (should-not flycheck-mode)))))

(ert-deftest global-flycheck-mode/does-not-enable-if-no-checker-is-found ()
  :tags '(global-mode)
  (flycheck-test-with-global-mode
    (flycheck-test-with-temp-buffer
      (rename-buffer "foo")
      (text-mode)
      (should-not flycheck-mode))))

(ert-deftest global-flycheck-mode/checker-found ()
  :tags '(global-mode)
  (flycheck-test-with-global-mode
    (flycheck-test-with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer "foo")
      (emacs-lisp-mode)
      (should flycheck-mode))))


;;; Deferred syntax checking

(ert-deftest flycheck-deferred-check-p/nil ()
  :tags '(deferred)
  (let ((flycheck-deferred-syntax-check nil))
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-deferred-check-p/truthy ()
  :tags '(deferred)
  (let ((flycheck-deferred-syntax-check t))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-deferred/schedules-a-deferred-syntax-check ()
  :tags '(deferred)
  (flycheck-test-with-temp-buffer
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-deferred)
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-clean-deferred-check/removes-a-deferred-syntax-check ()
  :tags '(deferred)
  (flycheck-test-with-temp-buffer
    (flycheck-buffer-deferred)
    (flycheck-clean-deferred-check)
    (should-not (flycheck-deferred-check-p))))


;;; Automatic syntax checking

(ert-deftest flycheck-may-check-automatically/not-in-ephemeral-buffers ()
  :tags '(automatic)
  (flycheck-test-with-temp-buffer
    (should-not (-any? #'flycheck-may-check-automatically
                       '(save idle-change new-line mode-enabled)))
    (should-not (flycheck-may-check-automatically))))

(ert-deftest flycheck-may-check-automatically/in-normal-buffers ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (should (-all? #'flycheck-may-check-automatically
                   '(save idle-change new-line mode-enabled)))
    (should (flycheck-may-check-automatically))))

(ert-deftest flycheck-may-check-automatically/automatic-checking-disabled ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (should-not (-any? #'flycheck-may-check-automatically
                         '(save idle-change new-line mode-enabled)))
      (should (flycheck-may-check-automatically)))))

(ert-deftest flycheck-may-check-automatically/specific-event-disabled ()
  :tags '(automatic)
  (dolist (event '(save idle-change new-line mode-enabled))
    (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
      ;; Disable just a specific event
      (let ((flycheck-check-syntax-automatically
             (remq event flycheck-check-syntax-automatically)))
        (should flycheck-check-syntax-automatically)
        (should-not (flycheck-may-check-automatically event))
        (should (-all? #'flycheck-may-check-automatically
                       flycheck-check-syntax-automatically))
        (should (flycheck-may-check-automatically))))))

(ert-deftest flycheck-check-syntax-automatically/mode-enabled-is-disabled ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically
           (remq 'mode-enabled flycheck-check-syntax-automatically)))
      (flycheck-mode)
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/mode-enabled-checks-syntax-after-flycheck-mode ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically '(mode-enabled)))
      (flycheck-mode)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/idle-change-is-disabled ()
  :tags '(automatic)
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
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (let ((flycheck-check-syntax-automatically '(idle-change)))
      (flycheck-mode)
      (insert "Hello world")
      (sleep-for 0.55)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/idle-change-does-not-check-before-delay ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (let ((flycheck-check-syntax-automatically '(idle-change))
          (flycheck-idle-change-delay 1.5))
      (flycheck-mode)
      (insert "Hello world")
      (sleep-for 0.55)
      (should-not (flycheck-deferred-check-p))
      (sleep-for 1)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/new-line-is-disabled ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (let ((flycheck-check-syntax-automatically
           (remq 'new-line flycheck-check-syntax-automatically)))
      (insert "\n")
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/new-line-checks-syntax-after-new-line ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically '(new-line)))
      (flycheck-mode)
      (insert "\n")
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/save-disabled ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically
           (remq 'save flycheck-check-syntax-automatically)))
      (save-buffer 0)
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/save-checks-syntax-after-save ()
  :tags '(automatic)
  (flycheck-test-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically '(save)))
      (flycheck-mode)
      (set-buffer-modified-p t)
      (save-buffer 0)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-buffer-automatically/does-not-check-with-disabled-mode ()
  :tags '(automatic)
  (flycheck-test-with-temp-buffer
    (should-not flycheck-mode)
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-automatically/defers-the-test ()
  :tags '(automatic)
  (flycheck-test-with-temp-buffer
    (flycheck-mode)
    ;; Flycheck won't check ephemeral buffers
    (rename-buffer "foo")
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should (flycheck-deferred-check-p))))


;;; Status reporting

(ert-deftest flycheck-report-status/runs-functions ()
  :tags '(status-reporting)
  (flycheck-test-with-temp-buffer
    (let* ((was-called nil)
           (flycheck-status-changed-functions
            (list (lambda (status) (setq was-called status)))))
      (flycheck-report-status 'running)
      (should (eq was-called 'running)))))

(ert-deftest flycheck-report-error/runs-hook ()
  :tags '(status-reporting)
  (flycheck-test-with-temp-buffer
    (let* ((was-called nil)
           (flycheck-syntax-check-failed-hook
            (list (lambda () (setq was-called t)))))
      (flycheck-report-error)
      (should was-called))))

(ert-deftest flycheck-report-error/clears-errors ()
  :tags '(status-reporting)
  (flycheck-test-with-temp-buffer
    (let ((flycheck-current-errors (list 'foo)))
      (flycheck-report-error)
      (should-not flycheck-current-errors))))


;;; Utility functions
(ert-deftest flycheck-string-to-number-safe/nil ()
  :tags '(utility)
  (should-not (flycheck-string-to-number-safe nil)))

(ert-deftest flycheck-string-to-number-safe/not-a-string ()
  :tags '(utility)
  (should-not (flycheck-string-to-number-safe [1 2 3])))

(ert-deftest flycheck-string-to-number-safe/already-a-number ()
  :tags '(utility)
  (should-not (flycheck-string-to-number-safe 3)))

(ert-deftest flycheck-string-to-number-safe/a-non-numeric-string ()
  :tags '(utility)
  (should-not (flycheck-string-to-number-safe "123helloworld")))

(ert-deftest flycheck-string-to-number-safe/a-numeric-string ()
  :tags '(utility)
  (should (= (flycheck-string-to-number-safe "123") 123)))

(ert-deftest flycheck-string-to-number-safe/a-numeric-string-with-leading-whitespace ()
  :tags '(utility)
  (should-not (flycheck-string-to-number-safe " 123")))

(ert-deftest flycheck-string-to-number-safe/a-numeric-string-with-trailing-whitespace ()
  :tags '(utility)
  (should-not (flycheck-string-to-number-safe "123 ")))

(ert-deftest flycheck-string-list-p/not-a-list ()
  :tags '(utility)
  (should-not (flycheck-string-list-p ["foo" "bar"])))

(ert-deftest flycheck-string-list-p/a-plain-string ()
  :tags '(utility)
  (should-not (flycheck-string-list-p "foo")))

(ert-deftest flycheck-string-list-p/a-plain-integer ()
  :tags '(utility)
  (should-not (flycheck-string-list-p 1)))

(ert-deftest flycheck-string-list-p/a-plain-symbol ()
  :tags '(utility)
  (should-not (flycheck-string-list-p 'foo)))

(ert-deftest flycheck-string-list-p/a-list-with-mixed-types ()
  :tags '(utility)
  (should-not (flycheck-string-list-p '("foo" 1 test))))

(ert-deftest flycheck-string-list-p/a-list-of-symbols ()
  :tags '(utility)
  (should-not (flycheck-string-list-p '(foo bar))))

(ert-deftest flycheck-string-list-p/a-list-of-strings ()
  :tags '(utility)
  (should (flycheck-string-list-p '("foo" "bar"))))

(ert-deftest flycheck-string-list-p/an-empty-list ()
  :tags '(utility)
  (should (flycheck-string-list-p '())))

(ert-deftest flycheck-symbol-list-p/not-a-list ()
  :tags '(utility)
  (should-not (flycheck-symbol-list-p ["foo" "bar"])))

(ert-deftest flycheck-symbol-list-p/a-plain-string ()
  :tags '(utility)
  (should-not (flycheck-symbol-list-p "foo")))

(ert-deftest flycheck-symbol-list-p/a-plain-integer ()
  :tags '(utility)
  (should-not (flycheck-symbol-list-p 1)))

(ert-deftest flycheck-symbol-list-p/a-plain-symbol ()
  :tags '(utility)
  (should-not (flycheck-symbol-list-p 'foo)))

(ert-deftest flycheck-symbol-list-p/a-list-with-mixed-types ()
  :tags '(utility)
  (should-not (flycheck-symbol-list-p '("foo" 1 test))))

(ert-deftest flycheck-symbol-list-p/a-list-of-strings ()
  :tags '(utility)
  (should-not (flycheck-symbol-list-p '("foo" "bar"))))

(ert-deftest flycheck-symbol-list-p/a-list-of-symbols ()
  :tags '(utility)
  (should (flycheck-symbol-list-p '(foo bar))))

(ert-deftest flycheck-symbol-list-p/an-empty-list ()
  :tags '(utility)
  (should (flycheck-symbol-list-p '())))

(ert-deftest flycheck-temp-dir-system ()
  :tags '(utility)
  (let ((dirname (flycheck-temp-dir-system)))
    (unwind-protect
        (should (file-directory-p dirname))
      (flycheck-safe-delete-temporaries))
    (should-not (file-exists-p dirname))
    (should (string-prefix-p (file-name-as-directory
                              (file-truename temporary-file-directory))
                             (file-truename dirname)))
    (should (string-prefix-p flycheck-temp-prefix
                             (file-name-nondirectory
                              (directory-file-name dirname))))))

(ert-deftest flycheck-temp-file-system/without-file-name ()
  :tags '(utility)
  (let ((filename (flycheck-temp-file-system nil)))
    (unwind-protect
        (should (file-exists-p filename))
      (flycheck-safe-delete-temporaries))
    (should-not (file-exists-p filename))
    (should (string-prefix-p (file-name-as-directory
                              (file-truename temporary-file-directory))
                             (file-truename filename)))
    (should (string-prefix-p flycheck-temp-prefix
                             (file-name-nondirectory filename)))))

(ert-deftest flycheck-temp-file-system/with-complete-path ()
  :tags '(utility)
  (let* ((filename (flycheck-temp-file-system "spam/with/eggs.el"))
         (dirname (directory-file-name (file-name-directory filename))))
    (unwind-protect
        (progn
          ;; The file is not implicitly created, but the temporary directory is.
          (should-not (file-exists-p filename))
          (should (file-directory-p dirname)))
      (flycheck-safe-delete-temporaries))
    (should-not (file-exists-p filename))
    (should-not (file-directory-p dirname))
    ;; The file name should be preserved.  The temporary file should reside in a
    ;; subdirectory of the temporary directory
    (should (string= "eggs.el" (file-name-nondirectory filename)))
    (should (string-prefix-p flycheck-temp-prefix
                             (file-name-nondirectory
                              (directory-file-name dirname))))
    (should (string-prefix-p flycheck-temp-prefix
                             (file-name-nondirectory
                              (directory-file-name dirname))))))

(ert-deftest flycheck-temp-file-inplace/with-just-basename ()
  :tags '(utility)
  (let* ((default-directory flycheck-test-directory)
         (filename (flycheck-temp-file-inplace "eggs.el")))
    (unwind-protect
        ;; In place files should not be created early
        (should-not (file-exists-p filename))
      (flycheck-safe-delete-temporaries))
    (should (string= filename (expand-file-name
                               (concat flycheck-temp-prefix "_eggs.el"))))))

(ert-deftest flycheck-temp-file-inplace/with-complete-path ()
  :tags '(utility)
  (let* ((default-directory flycheck-test-directory)
         (filename (flycheck-temp-file-inplace "spam/with/eggs.el")))
    (unwind-protect
        (should-not (file-exists-p filename))
      (flycheck-safe-delete-temporaries))
    (should (string= filename (expand-file-name (concat "spam/with/"
                                                        flycheck-temp-prefix
                                                        "_eggs.el"))))))

(ert-deftest flycheck-temp-file-inplace/without-file-name ()
  :tags '(utility)
  (let ((filename (flycheck-temp-file-inplace nil)))
    (unwind-protect
        (should (file-exists-p filename))
      (flycheck-safe-delete-temporaries))
    (should-not (file-name-extension filename))
    (should (string-prefix-p flycheck-temp-prefix
                             (file-name-nondirectory filename)))))

(ert-deftest flycheck-save-buffer-to-file ()
  :tags '(utility)
  (let ((filename (expand-file-name "tests-temp")))
    (unwind-protect
        (progn
          (flycheck-test-with-temp-buffer
            (should-not (file-exists-p filename))
            (insert "Hello world")
            (flycheck-save-buffer-to-file filename))
          (should (file-exists-p filename))
          (should (string= "Hello world" (with-temp-buffer
                                           (insert-file-contents filename)
                                           (buffer-string)))))
      (ignore-errors (delete-file filename)))))

(ert-deftest flycheck-prepend-with-option/empty-list ()
  :tags '(utility)
  (should-not (flycheck-prepend-with-option "-f" nil)))

(ert-deftest flycheck-prepend-with-option/default-prepend-function ()
  :tags '(utility)
  (should (equal (flycheck-prepend-with-option "-L" '("foo" "bar"))
                 '("-L" "foo" "-L" "bar"))))

(ert-deftest flycheck-prepend-with-option/prepend-by-string-concatentation ()
  :tags '(utility)
  (should (equal (flycheck-prepend-with-option "-L" '("foo" "bar") #'concat)
                 '("-Lfoo" "-Lbar"))))

(ert-deftest flycheck-find-in-buffer/returns-first-match-on-success ()
  :tags '(utility)
  (with-temp-buffer
    (insert "foo\nbar")
    (should (string= "bar" (flycheck-find-in-buffer (rx (group "bar")))))))

(ert-deftest flycheck-find-in-buffer/returns-nil-on-failure ()
  :tags '(utility)
  (with-temp-buffer
    (insert "spam\nwith eggs")
    (should-not (flycheck-find-in-buffer (rx (group "bar"))))))

(ert-deftest flycheck-find-in-buffer/saves-excursion ()
  :tags '(utility)
  (with-temp-buffer
    (insert "spam\nwith eggs")
    (goto-char (point-min))
    (forward-line)
    (should (flycheck-find-in-buffer (rx (group "spam"))))
    (should (= (point) 6))))

(ert-deftest flycheck-find-in-buffer/ ()
  (with-temp-buffer
    (insert "spam\nwith eggs")
    (should-not (flycheck-find-in-buffer (rx (group "bar"))))))

(ert-deftest flycheck-ephemeral-buffer-p/temporary-buffer ()
  :tags '(utility)
  (flycheck-test-with-temp-buffer
    (should (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-ephemeral-buffer-p/buffer-with-leading-whitespace ()
  :tags '(utility)
  (flycheck-test-with-temp-buffer
    (rename-buffer " foo")
    (should (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-ephemeral-buffer-p/buffer-without-leading-whitespace ()
  :tags '(utility)
  (flycheck-test-with-temp-buffer
    (rename-buffer "foo")
    (should-not (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/unencrypted-temporary-buffer ()
  :tags '(utility)
  (flycheck-test-with-temp-buffer
    (should-not (flycheck-encrypted-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/unencrypted-file-buffer ()
  :tags '(utility)
  (flycheck-test-with-resource-buffer "global-mode-dummy.el"
    (should-not (flycheck-encrypted-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/encrypted-file-buffer ()
  :tags '(utility external-tool)
  (skip-unless (flycheck-test-check-gpg))
  (let* ((filename (flycheck-test-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-test-with-resource-buffer filename
      (should (flycheck-encrypted-buffer-p)))))

(ert-deftest flycheck-autoloads-file-p/ephemeral-buffer ()
  :tags '(utility)
  (flycheck-test-with-temp-buffer
    (should-not (flycheck-autoloads-file-p))))

(ert-deftest flycheck-autoloads-file-p/autoloads-without-backing-file ()
  :tags '(utility)
  (flycheck-test-with-temp-buffer
    (rename-buffer "foo-autoloads.el")
    (should (flycheck-autoloads-file-p))))

(ert-deftest flycheck-autoloads-file-p/autoloads-with-backing-file ()
  :tags '(utility)
  (flycheck-test-with-file-buffer (locate-library "dash-autoloads")
    (should (flycheck-autoloads-file-p))))

(ert-deftest flycheck-autoloads-file-p/a-plain-file ()
  :tags '(utility)
  (flycheck-test-with-file-buffer
      (expand-file-name "Cask" flycheck-test-source-directory)
    (should-not (flycheck-autoloads-file-p))))

(ert-deftest flycheck-in-user-emacs-directory-p/no-child-of-user-emacs-directory ()
  :tags '(utility)
  (should-not (flycheck-in-user-emacs-directory-p
               (flycheck-test-resource-filename "checkers/emacs-lisp.el"))))

(ert-deftest flycheck-in-user-emacs-directory-p/direct-child-of-user-emacs-directory ()
  :tags '(utility)
  (let ((user-emacs-directory flycheck-test-directory))
    (should (flycheck-in-user-emacs-directory-p
             (expand-file-name "flycheck-test.el" flycheck-test-directory)))))

(ert-deftest flycheck-in-user-emacs-directory-p/indirect-child-of-user-emacs-directory ()
  :tags '(utility)
  (let ((user-emacs-directory flycheck-test-directory))
    (should (flycheck-in-user-emacs-directory-p
             (flycheck-test-resource-filename "checkers/emacs-lisp.el")))))

(ert-deftest flycheck-safe-delete/recursive-removal ()
  :tags '(utility)
  (let ((dirname (flycheck-temp-dir-system)))
    (unwind-protect
        (let ((filename (expand-file-name "foo" dirname)))
          (process-lines "touch" filename)
          (should (string-prefix-p dirname filename))
          (should (file-exists-p filename))
          (flycheck-safe-delete dirname)
          (should-not (file-exists-p filename))
          (should-not (file-directory-p dirname))
          (should-not (file-exists-p dirname)))
      (ignore-errors (delete-directory dirname 'recurse)))))

(ert-deftest flycheck-module-root-directory/no-module-name-and-no-file-name ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory))
    (should (string= flycheck-test-resources-directory
                     (flycheck-module-root-directory nil)))))

(ert-deftest flycheck-module-root-directory/no-module-name ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-test-resource-filename "checkers/emacs-lisp.el")))
    (should (string= (flycheck-test-resource-filename "checkers/")
                     (flycheck-module-root-directory nil file-name)))))

(ert-deftest flycheck-module-root-directory/module-name-as-string ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-test-resource-filename "checkers/emacs-lisp.el")))
    (should (string= flycheck-test-resources-directory
                     (flycheck-module-root-directory "checkers.emacs-lisp"
                                                     file-name)))))

(ert-deftest flycheck-module-root-directory/module-name-as-list ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-test-resource-filename "checkers/emacs-lisp.el")))
    (should (string= flycheck-test-resources-directory
                     (flycheck-module-root-directory '("checkers" "emacs-lisp")
                                                     file-name)))))

(ert-deftest flycheck-module-root-directory/mismatching-module-name ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-test-resource-filename "checkers/emacs-lisp.el")))
    (should (string= (flycheck-test-resource-filename "checkers/")
                     (flycheck-module-root-directory '("foo" "emacs-lisp")
                                                     file-name)))))


;;; Checker definitions

(ert-deftest flycheck-command-argument-p/with-symbols ()
  :tags '(definition)
  (dolist (symbol '(source
                    source-inplace
                    source-original
                    temporary-directory
                    temporary-file-name
                    null-device))
    (should (flycheck-command-argument-p symbol))))

(ert-deftest flycheck-command-argument-p/config-file-with-variable-symbol ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(config-file "foo" bar))))

(ert-deftest flycheck-command-argument-p/config-file-with-quoted-variable-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(config-file "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/config-file-without-variable-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(config-file "foo"))))

(ert-deftest flycheck-command-argument-p/option-without-filter ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(option "foo" bar))))

(ert-deftest flycheck-command-argument-p/option-with-filter ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(option "foo" bar filter))))

(ert-deftest flycheck-command-argument-p/option-with-quoted-variable-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(option "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/option-with-quoted-filter-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(option "foo" bar 'filter))))

(ert-deftest flycheck-command-argument-p/option-without-variable ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(option "foo"))))

(ert-deftest flycheck-command-argument-p/option-list-without-filter-and-prepender ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(option-list "foo" bar))))

(ert-deftest flycheck-command-argument-p/option-list-with-prepender ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn))))

(ert-deftest flycheck-command-argument-p/option-list-with-prepender-and-filter ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn filter))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-variable-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(option-list "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-prepender-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(option-list "foo" bar 'prepend-fn))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-filter-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(option-list "foo" bar prepend-fn 'filter))))

(ert-deftest flycheck-command-argument-p/option-list-without-variable-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(option-list "foo"))))

(ert-deftest flycheck-command-argument-p/eval-with-variable ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(eval bar))))

(ert-deftest flycheck-command-argument-p/eval-with-function-call ()
  :tags '(definition)
  (should (flycheck-command-argument-p '(eval (spam "with eggs")))))

(ert-deftest flycheck-command-argument-p/eval-with-no-form ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(eval))))

(ert-deftest flycheck-command-argument-p/eval-with-multiple-forms ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(eval foo bar))))

(ert-deftest flycheck-command-argument-p/integer-literal ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p 100)))

(ert-deftest flycheck-command-argument-p/unknown-argument-symbol ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p 'foo)))

(ert-deftest flycheck-command-argument-p/unknown-argument-cell ()
  :tags '(definition)
  (should-not (flycheck-command-argument-p '(foo bar))))

(ert-deftest flycheck-validate-next-checker/any-symbol ()
  :tags '(definition)
  (should (flycheck-validate-next-checker 'foo))
  (should-error (flycheck-validate-next-checker 'foo t)))

(ert-deftest flycheck-validate-next-checker/syntax-checker-symbol ()
  :tags '(definition)
  (should (flycheck-validate-next-checker 'emacs-lisp t)))

(ert-deftest flycheck-validate-next-checker/string ()
  :tags '(definition)
  (should-error (flycheck-validate-next-checker "foo")))

(ert-deftest flycheck-validate-next-checker/invalid-form ()
  :tags '(definition)
  (should-error (flycheck-validate-next-checker '(warnings-only emacs-lisp))))

(ert-deftest flycheck-validate-next-checker/invalid-level ()
  :tags '(definition)
  (should-error (flycheck-validate-next-checker '("foo" . emacs-lisp)))
  (should-error (flycheck-validate-next-checker '(foo . emacs-lisp) 'strict)))

(ert-deftest flycheck-validate-next-checker/valid-predicate-with-any-symbol ()
  :tags '(definition)
  (should (flycheck-validate-next-checker '(warning . bar)))
  (should-error (flycheck-validate-next-checker '(warning . bar) 'strict)))

(ert-deftest flycheck-validate-next-checker/valid-predicate-with-syntax-checker-symbol ()
  :tags '(definition)
  (should (flycheck-validate-next-checker '(warning . emacs-lisp) 'strict)))


;;; Checker extensions
(ert-deftest flycheck-add-next-checker/no-valid-checker ()
  :tags '(extending)
  (let ((err-data (should-error (flycheck-add-next-checker 'foo 'emacs-lisp))))
    (should (string= (cadr err-data) "foo is not a valid syntax checker"))))

(ert-deftest flycheck-add-next-checker/no-valid-next-checker ()
  :tags '(extending)
  (should-error (flycheck-add-next-checker 'emacs-lisp '(warnings-only bar)))
  (should-error (flycheck-add-next-checker 'emacs-lisp "foo"))
  (should-error (flycheck-add-next-checker 'emacs-lisp 'bar))
  (should-error (flycheck-add-next-checker 'emacs-lisp '(warnings-only . bar)))
  (should-error (flycheck-add-next-checker 'emacs-lisp '(foo . emacs-lisp))))

(ert-deftest flycheck-add-next-checker/prepend ()
  :tags '(extending)
  (let ((next-checkers (flycheck-checker-next-checkers 'emacs-lisp)))
    (flycheck-add-next-checker 'emacs-lisp 'texinfo)
    (unwind-protect
        (should (equal (flycheck-checker-next-checkers 'emacs-lisp)
                       (cons 'texinfo next-checkers)))
      (put 'emacs-lisp 'flycheck-next-checkers next-checkers)
      (should (equal (flycheck-checker-next-checkers 'emacs-lisp)
                     next-checkers)))))

(ert-deftest flycheck-add-next-checker/append ()
  :tags '(extending)
  (let ((next-checkers (flycheck-checker-next-checkers 'emacs-lisp)))
    (flycheck-add-next-checker 'emacs-lisp 'texinfo 'append)
    (unwind-protect
        (should (equal (flycheck-checker-next-checkers 'emacs-lisp)
                       (append next-checkers '(texinfo))))
      (put 'emacs-lisp 'flycheck-next-checkers next-checkers)
      (should (equal (flycheck-checker-next-checkers 'emacs-lisp)
                     next-checkers)))))


;;; Checker API
(ert-deftest flycheck-valid-checker-p/not-a-symbol ()
  :tags '(checker-api)
  (should-not (flycheck-valid-checker-p "foo")))

(ert-deftest flycheck-valid-checker-p/no-checker-version ()
  :tags '(checker-api)
  (should-not (get 'foo 'flycheck-checker-version))
  (should-not (flycheck-valid-checker-p 'foo)))

(ert-deftest flycheck-valid-checker-p/checker-version-too-low ()
  :tags '(checker-api)
  (cl-letf* ((version (- flycheck-checker-version 1))
             ((get 'foo 'flycheck-checker-version) version))
    (should (= (get 'foo 'flycheck-checker-version) version))
    (should-not (flycheck-valid-checker-p 'foo)))
  (should-not (get 'foo 'flycheck-checker-version)))

(ert-deftest flycheck-valid-checker-p/checker-version-too-high ()
  :tags '(checker-api)
  (cl-letf* ((version (+ flycheck-checker-version 1))
             ((get 'foo 'flycheck-checker-version) version))
    (should (= (get 'foo 'flycheck-checker-version) version))
    (should-not (flycheck-valid-checker-p 'foo)))
  (should-not (get 'foo 'flycheck-checker-version)))

(ert-deftest flycheck-valid-checker-p/checker-version-ok ()
  :tags '(checker-api)
  (cl-letf* ((version flycheck-checker-version)
             ((get 'foo 'flycheck-checker-version) version))
    (should (= (get 'foo 'flycheck-checker-version) version))
    (should (flycheck-valid-checker-p 'foo)))
  (should-not (get 'foo 'flycheck-checker-version)))

(ert-deftest flycheck-disabled-checker-p/enabled-checker ()
  :tags '(checker-api)
  (let ((flycheck-checkers '(emacs-lisp)))
    (should-not (flycheck-disabled-checker-p 'emacs-lisp))))

(ert-deftest flycheck-disabled-checker-p/disabled-checker ()
  :tags '(checker-api)
  (let ((flycheck-disabled-checkers '(emacs-lisp)))
    (should (flycheck-disabled-checker-p 'emacs-lisp))))

(ert-deftest flycheck-enabled-checker-p/enabled-checker ()
  :tags '(checker-api)
  (let ((flycheck-checkers '(emacs-lisp)))
    (should (flycheck-enabled-checker-p 'emacs-lisp))))

(ert-deftest flycheck-enabled-checker-p/unregistered-checker ()
  :tags '(checker-api)
  (let ((flycheck-checkers '(emacs-lisp)))
    (should-not (flycheck-enabled-checker-p 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-enabled-checker-p/disabled-checker ()
  :tags '(checker-api)
  (let ((flycheck-checkers '(emacs-lisp))
        (flycheck-disabled-checkers '(emacs-lisp)))
    (should-not (flycheck-enabled-checker-p 'emacs-lisp))))

(ert-deftest flycheck-enabled-checkers/all-registered-checkers-are-enabled ()
  :tags '(checker-api)
  (let ((flycheck-checkers '(emacs-lisp emacs-lisp-checkdoc)))
    (should (equal (flycheck-enabled-checkers)
                   '(emacs-lisp emacs-lisp-checkdoc)))))

(ert-deftest flycheck-enabled-checkers/some-checkers-are-disabled ()
  :tags '(checker-api)
  (let ((flycheck-checkers '(emacs-lisp emacs-lisp-checkdoc coffee zsh))
        (flycheck-disabled-checkers '(emacs-lisp coffee)))
    (should (equal (flycheck-enabled-checkers) '(emacs-lisp-checkdoc zsh)))))

(defvar flycheck-test-config-var)
(defvar flycheck-test-option-var)

(ert-deftest flycheck-defined-checkers/are-valid ()
  :tags '(checker-api)
  (dolist (checker (flycheck-defined-checkers))
    (should (flycheck-valid-checker-p checker))))

(ert-deftest flycheck-defined-checkers/are-registered ()
  :tags '(checker-api)
  (dolist (checker (flycheck-defined-checkers))
    (should (flycheck-registered-checker-p checker))))

(ert-deftest flycheck-checker-executable/is-string ()
  :tags '(checker-api)
  (dolist (checker flycheck-checkers)
    (should (stringp (flycheck-checker-executable checker)))))

(ert-deftest flycheck-checker-executable/override-the-executable ()
  :tags '(checker-api)
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (equal (eval `(let ((,variable "some-nice-executable"))
                              (flycheck-checker-executable ',checker)))
                     "some-nice-executable")))))

(ert-deftest flycheck-checker-modes ()
  :tags '(checker-api)
  (dolist (checker flycheck-checkers)
    (should (listp (flycheck-checker-modes checker)))
    (should (-all? #'symbolp (flycheck-checker-modes checker)))))

(ert-deftest flycheck-substitute-argument/source ()
  :tags '(checker-api)
  (flycheck-test-with-resource-buffer "substitute-dummy"
    (unwind-protect
        (progn
          (should (equal (flycheck-substitute-argument 'source-original 'emacs-lisp)
                         (list (buffer-file-name))))

          (let ((filename (flycheck-substitute-argument 'source-inplace 'emacs-lisp)))
            (should (equal filename (list (flycheck-test-resource-filename
                                           (concat flycheck-temp-prefix
                                                   "_substitute-dummy")))))
            (should (file-exists-p (car filename))))

          (let ((filename (flycheck-substitute-argument 'source 'emacs-lisp)))
            (should (string-prefix-p temporary-file-directory (car filename)))
            (should (file-exists-p (car filename))))))
    (mapc #'flycheck-safe-delete flycheck-temporaries)))

(ert-deftest flycheck-substitute-argument/temporary-directory ()
  :tags '(checker-api)
  (unwind-protect
      (let ((dirname (car (flycheck-substitute-argument 'temporary-directory
                                                        'emacs-lisp))))
        (should (file-directory-p dirname))
        (should (string-prefix-p temporary-file-directory dirname)))
    (mapc #'flycheck-safe-delete flycheck-temporaries)))

(ert-deftest flycheck-substitute-argument/temporary-filename ()
  :tags '(checker-api)
  (unwind-protect
      (let ((filename (car (flycheck-substitute-argument 'temporary-file-name
                                                         'emacs-lisp))))
        ;; The filename should not exist, but it's parent directory should
        (should-not (file-exists-p filename))
        (should (file-directory-p (file-name-directory filename)))
        (should (string-prefix-p temporary-file-directory filename))
        (should (member (directory-file-name (file-name-directory filename))
                        flycheck-temporaries)))
    (mapc #'flycheck-safe-delete flycheck-temporaries)))

(ert-deftest flycheck-substitute-argument/null-device ()
  :tags '(checker-api)
  (should (equal (flycheck-substitute-argument 'null-device 'emacs-lisp)
                 (list null-device))))

(ert-deftest flycheck-substitute-argument/config-file ()
  :tags '(checker-api)
  (let* ((flycheck-test-config-var "substitute-dummy")
         (config-file (flycheck-test-resource-filename "substitute-dummy"))
         first-args second-args
         (locate-nil (lambda (&rest args) (setq first-args args) nil))
         (locate-real (lambda (&rest args) (setq second-args args)
                        config-file))
         (flycheck-locate-config-file-functions (list locate-nil locate-real)))
    (should (equal (flycheck-substitute-argument
                    '(config-file "--foo" flycheck-test-config-var)
                    'emacs-lisp)
                   (list "--foo" config-file)))
    (should (equal first-args (list "substitute-dummy" 'emacs-lisp)))
    (should (equal second-args (list "substitute-dummy" 'emacs-lisp)))
    (setq first-args nil
          second-args nil)
    (should (equal (flycheck-substitute-argument
                    '(config-file "--foo=" flycheck-test-config-var concat)
                    'emacs-lisp)
                   (list (concat "--foo=" config-file))))))

(ert-deftest flycheck-substitute-argument/option ()
  :tags '(checker-api)
  (let ((flycheck-test-option-var "bar"))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var) 'emacs-lisp)
                   '("--foo" "bar")))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var concat)
                    'emacs-lisp)
                   '("--foo=bar"))))
  (let ((flycheck-test-option-var 200))
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var nil number-to-string)
                    'emacs-lisp)
                   '("--foo" "200")))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var concat number-to-string) 'emacs-lisp)
                   '("--foo=200"))))
  (let (flycheck-test-option-var)
    (should-not (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var) 'emacs-lisp))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var nil number-to-string)
                   'emacs-lisp)
                  :type 'wrong-type-argument)
    (should-error (flycheck-substitute-argument
                   '(option "--foo=" flycheck-test-option-var concat number-to-string)
                   'emacs-lisp)
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-argument/option-list ()
  :tags '(checker-api)
  (let ((flycheck-test-option-var "spam"))
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var '("spam" "eggs")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                   '("-I" "spam" "-I" "eggs")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var concat) 'emacs-lisp)
                   '("-Ispam" "-Ieggs"))))
  (let ((flycheck-test-option-var '(10 20)))
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                   '("-I" "10" "-I" "20")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var
                                  concat number-to-string) 'emacs-lisp)
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
  :tags '(checker-api)
  (let ((flycheck-test-option-var nil))
    (should-not (flycheck-substitute-argument
                 '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var t))
    (should (equal (flycheck-substitute-argument
                    '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                   (list "--foo"))))
  (let ((flycheck-test-option-var (list "bar")))
    (should (equal (flycheck-substitute-argument
                    '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                   (list "--foo")))))

(ert-deftest flycheck-substitute-argument/eval ()
  :tags '(checker-api)
  (let ((flycheck-test-option-var '("Hello " "World")))
    (should (equal (flycheck-substitute-argument '(eval flycheck-test-option-var) 'emacs-lisp)
                   '("Hello " "World"))))
  (should (equal (flycheck-substitute-argument '(eval (concat "Hello" "World")) 'emacs-lisp)
                 '("HelloWorld")))
  (should-not (flycheck-substitute-argument
               '(eval (when (string= "foo" "bar") "yes")) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval 200) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval '("foo" 200)) 'emacs-lisp)))

(ert-deftest flycheck-substitute-argument/unknown ()
  :tags '(checker-api)
  (should-error (flycheck-substitute-argument '(foo "bar") 'emacs-lisp))
  (should-error (flycheck-substitute-argument  200 'emacs-lisp)))

(ert-deftest flycheck-check-executable ()
  :tags '(checker-api)
  (dolist (checker flycheck-checkers)
    (if (executable-find (flycheck-checker-executable checker))
        (should (flycheck-check-executable checker))
      (should-not (flycheck-check-executable checker)))))

(ert-deftest flycheck-may-use-checker/invalid-checker ()
  :tags '(checker-api)
  (should-not (flycheck-valid-checker-p 'foo))
  (shut-up                              ; Inhibit warning display on terminal
    (should-not (flycheck-may-use-checker 'foo)))
  (with-current-buffer "*Warnings*"
    (save-excursion
      (goto-char (point-min))
      (search-forward "Warning (flycheck): ")
      (should (string= (buffer-substring-no-properties (point) (point-max))
                       "foo is no valid Flycheck syntax checker.
Try to reinstall the package defining this syntax checker.\n")))))


;;; Configuration file functions

(ert-deftest flycheck-locate-config-file-absolute-path/just-a-base-name ()
  :tags '(configuration)
  (flycheck-test-with-temp-buffer
    (cd flycheck-test-directory)
    (should-not (flycheck-locate-config-file-absolute-path "flycheck-test.el"
                                                           'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-absolute-path/with-path ()
  :tags '(configuration)
  (flycheck-test-with-temp-buffer
    (cd flycheck-test-directory)
    (should (equal (flycheck-locate-config-file-absolute-path "../Makefile"
                                                              'emacs-lisp)
                   (expand-file-name "../Makefile" flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/not-existing-file ()
  :tags '(configuration)
  (flycheck-test-with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                             flycheck-test-directory))
    (should-not (flycheck-locate-config-file-ancestor-directories
                 "foo" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/file-on-same-level ()
  :tags '(configuration)
  (flycheck-test-with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                             flycheck-test-directory))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "run-tests.el" 'emacs-lisp)
                   (expand-file-name "run-tests.el"
                                     flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/file-on-parent-level ()
  :tags '(configuration)
  (flycheck-test-with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                             flycheck-test-directory))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "Makefile" 'emacs-lisp)
                   (expand-file-name "../Makefile"
                                     flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file/not-existing-file ()
  :tags '(configuration)
  (flycheck-test-with-env (list (cons "HOME" flycheck-test-directory))
    (should-not (flycheck-locate-config-file-home "foo" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file/existing-file-in-parent-directory ()
  :tags '(configuration)
  (flycheck-test-with-env (list (cons "HOME" flycheck-test-directory))
    (should-not (flycheck-locate-config-file-home "Makefile" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file/existing-file-in-home-directory ()
  :tags '(configuration)
  (flycheck-test-with-env (list (cons "HOME" flycheck-test-directory))
    (should (equal (flycheck-locate-config-file-home
                    "flycheck-test.el" 'emacs-lisp)
                   (expand-file-name "flycheck-test.el"
                                     flycheck-test-directory)))))


;;; Generic option filters

(ert-deftest flycheck-option-int/pass-through-nil ()
  :tags '(option-filters)
  (should (null (flycheck-option-int nil))))

(ert-deftest flycheck-option-int/integer-argument ()
  :tags '(option-filters)
  (should (equal (flycheck-option-int 10) "10")))

(ert-deftest flycheck-option-comma-separated-list/empty-list ()
  :tags '(option-filters)
  (should (null (flycheck-option-comma-separated-list nil))))

(ert-deftest flycheck-option-comma-separated-list/with-single-nil ()
  :tags '(option-filters)
  (should (null (flycheck-option-comma-separated-list '(nil)))))

(ert-deftest flycheck-option-comma-separated-list/filter-returns-nil ()
  :tags '(option-filters)
  (should (null (flycheck-option-comma-separated-list '(10 20) nil
                                                      (lambda (_x) nil)))))

(ert-deftest flycheck-option-comma-separated-list/default-separator ()
  :tags '(option-filters)
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar"))
                 "foo,bar")))

(ert-deftest flycheck-option-comma-separated-list/custom-separator ()
  :tags '(option-filters)
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar") ":")
                 "foo:bar")))

(ert-deftest flycheck-option-comma-separated-list/custom-filter ()
  :tags '(option-filters)
  (should (equal (flycheck-option-comma-separated-list '(10 20) nil
                                                       #'number-to-string)
                 "10,20")))


;;; Checker selection

(ert-deftest flycheck-checker/unusable-checker-causes-an-error ()
  :tags '(selection)
  (flycheck-test-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-mode)
    (let* ((flycheck-checker 'sh-bash)
           (err (should-error (flycheck-buffer)
                              :type flycheck-test-user-error-type)))
      (should (eq flycheck-checker 'sh-bash))
      (should (string= (cadr err)
                       "Configured syntax checker sh-bash cannot be used"))
      (should (string= flycheck-last-status-change 'errored)))))

(ert-deftest flycheck-checker/usable-checker-is-used ()
  :tags '(selection language-emacs-lisp)
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

(ert-deftest flycheck-checker/disabled-checker-is-used ()
  :tags '(selection language-emacs-lisp)
  (flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (let ((flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))
      (should-not (flycheck-get-checker-for-buffer))
      (let ((flycheck-checker 'emacs-lisp-checkdoc))
        (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
        (flycheck-test-buffer-sync)
        (flycheck-test-should-errors
         '(12 nil warning "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))))))

(ert-deftest flycheck-checker/unregistered-checker-is-used ()
  :tags '(selection language-emacs-lisp)
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
  :tags '(selection)
  (flycheck-test-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (should (eq flycheck-checker 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-select-checker/unselecting-unsets-the-syntax-checker ()
  :tags '(selection)
  (flycheck-test-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (flycheck-select-checker nil)
    (should-not flycheck-checker)))

(ert-deftest flycheck-select-checker/selecting-runs-a-syntax-check ()
  :tags '(selection external-tool language-python)
  (skip-unless (-all? #'flycheck-check-executable '(python-flake8
                                                    python-pylint)))
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
     '(12 nil info "No space allowed around keyword argument assignment"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(15 nil info "Unnecessary parens after u'print' keyword (C0325)"
          :checker python-pylint)
     '(17 nil info "Unnecessary parens after u'print' keyword (C0325)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))))

(ert-deftest flycheck-select-checker/unselecting-a-checker-goes-back-to-automatic-selection ()
  :tags '(selection external-tool language-python)
  (skip-unless (-all? #'flycheck-check-executable '(python-pylint
                                                    python-flake8)))
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
     '(12 nil info "No space allowed around keyword argument assignment"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(15 nil info "Unnecessary parens after u'print' keyword (C0325)"
          :checker python-pylint)
     '(17 nil info "Unnecessary parens after u'print' keyword (C0325)"
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

(ert-deftest flycheck/selects-checker-automatically/first-enabled-checker ()
  :tags '(selection external-tool language-python)
  (skip-unless (-all? #'flycheck-check-executable '(python-pylint
                                                    python-flake8)))
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
          :checker python-flake8))))

(ert-deftest flycheck/selects-checker-automatically/no-disabled-checker ()
  :tags '(selection language-emacs-lisp)
  (flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
    (let ((flycheck-disabled-checkers '(emacs-lisp)))
      (flycheck-test-buffer-sync)
      (should-not flycheck-checker)
      (should (eq flycheck-last-checker 'emacs-lisp-checkdoc))
      (flycheck-test-should-errors
       '(12 nil warning "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)))))


;;; Documentation

(ert-deftest flycheck-describe-checker/pops-up-help-buffer ()
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
      (should (buffer-live-p (get-buffer (help-buffer))))
      (should (get-buffer-window (help-buffer)))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward (rx symbol-start (group (one-or-more not-newline))
                               symbol-end " is a Flycheck syntax checker"))
        (should (= (match-beginning 0) 1))
        (should (string= (match-string 1) (symbol-name checker)))))))

(ert-deftest flycheck-describe-checker/can-navigate-to-source ()
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
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
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "This\\s-+syntax\\s-+checker\\s-+executes\\s-+\"\\(.+?\\)\"\\(?:\\.\\|,\\)")
        (should (string= (match-string 1)
                         (flycheck-checker-default-executable checker)))))))

(ert-deftest flycheck-describe-checker/help-shows-executable-variable ()
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "The executable can be overridden with `\\(.+?\\)'.")
        (let ((var (flycheck-checker-executable-variable checker)))
          (should (string= (match-string 1) (symbol-name var))))))))

(ert-deftest flycheck-describe-checker/help-shows-config-file-var ()
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
      (with-current-buffer (help-buffer)
        (let ((config-file-var (flycheck-checker-config-file-var checker)))
          (if (not config-file-var)
              (should-not (string-match-p
                           (rx "configuration file")
                           (buffer-substring (point-min) (point-max))))
            (goto-char (point-min))
            (re-search-forward
             ", using\\s-+a\\s-+configuration\\s-+file\\s-+from\\s-+`\\(.+?\\)'\\.")
            (should (equal (match-string 1) (symbol-name config-file-var)))))))))

(ert-deftest flycheck-describe-checker/help-shows-option-vars ()
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
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
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-test-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
      (with-current-buffer (help-buffer)
        (should (string-match-p
                 (regexp-quote (flycheck-checker-documentation checker))
                 (buffer-substring (point-min) (point-max))))))))

(ert-deftest flycheck--manual/all-checkers-are-documented ()
  :tags '(documentation)
  (flycheck-test-with-file-buffer
      (expand-file-name "doc/guide/languages.rst"
                        flycheck-test-source-directory)
    (dolist (checker flycheck-checkers)
      (re-search-forward (rx ".. flyc-checker:: "
                             (group (one-or-more not-newline))
                             line-end))
      (should (string= (symbol-name checker) (match-string 1))))))

(ert-deftest flycheck--manual/all-options-are-documented ()
  :tags '(documentation)
  (flycheck-test-with-file-buffer
      (expand-file-name "doc/guide/languages.rst"
                        flycheck-test-source-directory)
    (dolist (checker flycheck-checkers)
      (-when-let (vars (-sort #'string< (flycheck-checker-option-vars checker)))
        (re-search-forward (concat (rx line-start ".. flyc-checker::"
                                       (one-or-more space))
                                   (regexp-quote (symbol-name checker))))
        (re-search-forward (rx line-start "   .. rubric:: Options" line-end))
        (dolist (var vars)
          ;; Move across empty lines
          (while (progn
                   (forward-line 1)
                   (looking-at (rx line-start (zero-or-more space) line-end))))
          (should (looking-at (rx line-start "   .. option:: "
                                  (group (one-or-more not-newline))
                                  line-end)))
          (should (string= (match-string 1) (symbol-name var)))
          (forward-line 1)
          (should (looking-at (rx line-start (= 6 " ") ":auto:" line-end))))))))

(ert-deftest flycheck--manual/all-config-file-vars-are-documented ()
  :tags '(documentation)
  (flycheck-test-with-file-buffer
      (expand-file-name "doc/guide/languages.rst" flycheck-test-source-directory)
    (dolist (checker flycheck-checkers)
      (-when-let (config-file-var (flycheck-checker-config-file-var checker))
        (re-search-forward (concat (rx line-start ".. flyc-checker::"
                                       (one-or-more space))
                                   (regexp-quote (symbol-name checker))))
        (re-search-forward (rx line-start
                               "   .. rubric:: Configuration file"
                               line-end))
        (while (progn
                 (forward-line 1)
                 (looking-at (rx line-start (zero-or-more space) line-end))))
        (should (looking-at (rx line-start "   .. option:: "
                                (group (one-or-more not-newline))
                                line-end)))
        (should (string= (match-string 1) (symbol-name config-file-var)))
        (forward-line 1)
        (should (looking-at (rx line-start (= 6 " ") ":auto:" line-end)))))))


;;; Checker error API

(ert-deftest flycheck-error-line-region ()
  :tags '(error-api)
  (flycheck-test-with-temp-buffer
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
  :tags '(error-api)
  (flycheck-test-with-temp-buffer
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
  :tags '(error-api)
  (flycheck-test-with-temp-buffer
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
  :tags '(error-api)
  (flycheck-test-with-temp-buffer
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
      (dolist (mode '(columns symbols sexps))
        (should (equal (flycheck-error-region-for-mode err mode) '(34 . 35)))))
    ;; Test an error without column for all modes
    (let ((err (flycheck-error-new-at 1 nil)))
      (dolist (mode '(lines columns symbols sexps))
        (should (equal (flycheck-error-region-for-mode err mode) '(5 . 29)))))))

(ert-deftest flycheck-error-pos ()
  :tags '(error-api)
  (flycheck-test-with-temp-buffer
    (insert "    Hello\n   World\n")
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 1)) 1))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 4)) 4))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 nil)) 5))
    (should (= (flycheck-error-pos (flycheck-error-new-at 2 nil)) 14))
    (should (= (flycheck-error-pos (flycheck-error-new-at 3 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 nil)) 19))))

(ert-deftest flycheck-error-format/level-warning ()
  :tags '(error-api)
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 3 5 'warning "Hello world"
                                           :checker 'emacs-lisp))
                   "3:5:warning: Hello world (emacs-lisp)")))

(ert-deftest flycheck-error-format/level-error ()
  :tags '(error-api)
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 20 7 'error "Spam with eggs"
                                           :checker 'ruby))
                   "20:7:error: Spam with eggs (ruby)")))

(ert-deftest flycheck-error-format/no-column ()
  :tags '(error-api)
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 nil 'warning "Oh no"
                                           :checker 'python-flake8))
                   "14:warning: Oh no (python-flake8)")))

(ert-deftest flycheck-error-format/handles-line-breaks ()
  :tags '(error-api)
  ;; Specific test for https://github.com/magnars/s.el/issues/34
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 15 'error "dash\\nbroken"
                                           :checker 'foo))
                   "14:15:error: dash\\nbroken (foo)")))

(ert-deftest flycheck-error-</no-column ()
  :tags '(error-api)
  (should (flycheck-error-< (flycheck-error-new-at 10 nil)
                            (flycheck-error-new-at 11 nil)))
  (should-not (flycheck-error-< (flycheck-error-new-at 10 nil)
                                (flycheck-error-new-at 10 nil)))
  (should-not (flycheck-error-< (flycheck-error-new-at 10 nil)
                                (flycheck-error-new-at 9 nil))))

(ert-deftest flycheck-error-</by-line-with-column ()
  :tags '(error-api)
  (should (flycheck-error-< (flycheck-error-new-at 10 2)
                            (flycheck-error-new-at 11 1)))
  (should-not (flycheck-error-< (flycheck-error-new-at 10 1)
                                (flycheck-error-new-at 9 2))))

(ert-deftest flycheck-error-</by-column ()
  :tags '(error-api)
  (should (flycheck-error-< (flycheck-error-new-at 10 10)
                            (flycheck-error-new-at 10 11)))
  (should-not (flycheck-error-< (flycheck-error-new-at 10 10)
                                (flycheck-error-new-at 10 10)))
  (should-not (flycheck-error-< (flycheck-error-new-at 10 11)
                                (flycheck-error-new-at 10 10))))

(ert-deftest flycheck-error-level-</by-level-severity ()
  :tags '(error-api)
  (should (flycheck-error-level-< (flycheck-error-new-at 10 nil 'info)
                                  (flycheck-error-new-at 8 nil 'warning)))
  (should-not (flycheck-error-level-< (flycheck-error-new-at 8 nil 'warning)
                                      (flycheck-error-new-at 8 nil 'warning)))
  (should-not (flycheck-error-level-< (flycheck-error-new-at 7 nil 'error)
                                      (flycheck-error-new-at 8 nil 'warning))))

(ert-deftest flycheck-error-level-</by-level-name ()
  :tags '(error-api)
  (should (flycheck-error-level-< (flycheck-error-new-at 10 nil 'a)
                                  (flycheck-error-new-at 8 nil 'b)))
  (should-not (flycheck-error-level-< (flycheck-error-new-at 7 nil 'c)
                                      (flycheck-error-new-at 8 nil 'b))))

(ert-deftest flycheck-error-level-</by-location ()
  :tags '(error-api)
  (should (flycheck-error-level-< (flycheck-error-new-at 8 nil 'info)
                                  (flycheck-error-new-at 10 nil 'info)))
  (should-not (flycheck-error-level-< (flycheck-error-new-at 8 nil 'info)
                                      (flycheck-error-new-at 8 nil 'info)))
  (should-not (flycheck-error-level-< (flycheck-error-new-at 8 nil 'info)
                                      (flycheck-error-new-at 7 nil 'info))))


;;; Error levels

;; A level for the following unit tests
(flycheck-define-error-level 'test-level
  :severity 1337
  :overlay-category 'category
  :fringe-bitmap 'left-triangle
  :fringe-face 'highlight
  :error-list-face 'font-lock-constant-face)

(ert-deftest flycheck-define-error-level/is-error-level? ()
  :tags '(error-level)
  (should (flycheck-error-level-p 'test-level)))

(ert-deftest flycheck-define-error-level/has-severity ()
  :tags '(error-level)
  (should (= (flycheck-error-level-severity 'test-level) 1337)))

(ert-deftest flycheck-define-error-level/has-fringe-bitmap ()
  :tags '(error-level)
  (should (eq (flycheck-error-level-fringe-bitmap 'test-level) 'left-triangle)))

(ert-deftest flycheck-define-error-level/has-fringe-face ()
  :tags '(error-level)
  (should (eq (flycheck-error-level-fringe-face 'test-level) 'highlight)))

(ert-deftest flycheck-define-error-level/has-overlay-category ()
  :tags '(error-level)
  (should (eq (flycheck-error-level-overlay-category 'test-level) 'category)))

(ert-deftest flycheck-define-error-level/has-error-list-face ()
  :tags '(error-level)
  (should (eq (flycheck-error-level-error-list-face 'test-level)
              'font-lock-constant-face)))

(ert-deftest flycheck-error-level-make-fringe-icon/has-fringe-bitmap ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon
                      'test-level 'left-fringe))
               (`(_ ,bitmap _) (get-text-property 0 'display icon)))
    (should (eq bitmap 'left-triangle))))

(ert-deftest flycheck-error-level-make-fringe-icon/has-fringe-face ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon 'test-level 'left-fringe))
               (`(_ _ ,face) (get-text-property 0 'display icon)))
    (should (eq face 'highlight))))

(ert-deftest flycheck-error-level-make-fringe-icon/left-fringe ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon 'test-level 'left-fringe))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (eq side 'left-fringe))))

(ert-deftest flycheck-error-level-make-fringe-icon/right-fringe ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-fringe-icon 'test-level 'right-fringe))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (eq side 'right-fringe))))

(ert-deftest flycheck-error-level-make-fringe-icon/invalid-side ()
  :tags '(error-level)
  (let ((err (should-error (flycheck-error-level-make-fringe-icon 'test-level
                                                                  'up-fringe))))
    (should (string= (cadr err) "Invalid fringe side: up-fringe"))))


;;; Built-in error levels

(ert-deftest flycheck-error-level-error ()
  :tags '(error-level)
  (should (= (flycheck-error-level-severity 'error) 100))
  (should (eq (flycheck-error-level-fringe-bitmap 'error)
              'exclamation-mark))
  (should (eq (flycheck-error-level-fringe-face 'error)
              'flycheck-fringe-error))
  (should (eq (flycheck-error-level-overlay-category 'error)
              'flycheck-error-overlay))
  (should (eq (flycheck-error-level-error-list-face 'error)
              'flycheck-error-list-error)))

(ert-deftest flycheck-error-level-warning ()
  :tags '(error-level)
  (should (= (flycheck-error-level-severity 'warning) 10))
  (should (eq (flycheck-error-level-fringe-bitmap 'warning) 'question-mark))
  (should (eq (flycheck-error-level-fringe-face 'warning)
              'flycheck-fringe-warning))
  (should (eq (flycheck-error-level-overlay-category 'warning)
              'flycheck-warning-overlay))
  (should (eq (flycheck-error-level-error-list-face 'warning)
              'flycheck-error-list-warning)))

(ert-deftest flycheck-error-level-info ()
  :tags '(error-level)
  (should (= (flycheck-error-level-severity 'info) -1))
  (should (eq (flycheck-error-level-fringe-bitmap 'info) 'empty-line))
  (should (eq (flycheck-error-level-fringe-face 'info)
              'flycheck-fringe-info))
  (should (eq (flycheck-error-level-overlay-category 'info)
              'flycheck-info-overlay))
  (should (eq (flycheck-error-level-error-list-face 'info)
              'flycheck-error-list-info)))


;;; Error parsers

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
  :tags '(error-parsing)
  (let ((flycheck-xml-parser 'flycheck-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle/with-libxml2 ()
  :tags '(error-parsing)
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((flycheck-xml-parser 'libxml-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle/automatic-parser ()
  :tags '(error-parsing)
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
  :tags '(error-parsing)
  (should (equal (flycheck-parse-cppcheck flycheck-cppcheck-xml nil nil)
                 flycheck-cppcheck-expected-errors)))

(ert-deftest flycheck-parse-cppcheck/empty-errors-list-with-automatic-parser ()
  :tags '(error-parsing)
  (should-not (flycheck-parse-cppcheck "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.60.1\"/>
  <errors>
  </errors>
</results>" nil nil)))

(ert-deftest flycheck-parse-cppcheck/empty-errors-list-with-builtin-parser ()
  :tags '(error-parsing)
  (let ((flycheck-xml-parser #'flycheck-parse-xml-region))
    (should-not (flycheck-parse-cppcheck "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.60.1\"/>
  <errors>
  </errors>
</results>" nil nil))))


;;; Error filters

(ert-deftest flycheck-sanitize-errors/trailing-whitespace ()
  :tags '(error-filtering)
  (let ((err (flycheck-error-new-at 1 1 'error " foo ")))
    (should (equal (flycheck-sanitize-errors (list err))
                   (list (flycheck-error-new-at 1 1 'error "foo"))))))

(ert-deftest flycheck-sanitize-errors/zero-column ()
  :tags '(error-filtering)
  (let ((err (flycheck-error-new-at 1 0 'error "foo")))
    (should (equal (flycheck-sanitize-errors (list err))
                   (list (flycheck-error-new-at 1 nil 'error "foo"))))))

(ert-deftest flycheck-collapse-error-message-whitespace ()
  :tags '(error-filtering)
  (let ((err (flycheck-error-new-at 1 1 'error
                                    "spam  \nwith\t   eggs")))
    (should (equal (flycheck-collapse-error-message-whitespace (list err))
                   (list (flycheck-error-new-at 1 1 'error
                                                "spam with eggs"))))))


;;; Error analysis

(ert-deftest flycheck-has-max-errors-p ()
  :tags '(error-analysis)
  (should (flycheck-has-max-errors-p nil 'error))
  (let ((errors (list (flycheck-error-new-at 10 10 'warning)
                      (flycheck-error-new-at 10 10 'info))))
    (should (flycheck-has-max-errors-p errors 'error))
    (should (flycheck-has-max-errors-p errors 'warning))
    (should-not (flycheck-has-max-errors-p errors 'info))))


;;; Error overlay management

(ert-deftest flycheck-info-overlay/priority ()
  :tags '(overlay)
  (should (= (get 'flycheck-info-overlay 'priority) 90)))

(ert-deftest flycheck-warning-overlay/priority ()
  :tags '(overlay)
  (should (= (get 'flycheck-warning-overlay 'priority) 100)))

(ert-deftest flycheck-error-overlay/priority ()
  :tags '(overlay)
  (should (= (get 'flycheck-error-overlay 'priority) 110)))

(ert-deftest flycheck-info-overlay/face ()
  :tags '(overlay)
  (should (eq (get 'flycheck-info-overlay 'face) 'flycheck-info)))

(ert-deftest flycheck-warning-overlay/face ()
  :tags '(overlay)
  (should (eq (get 'flycheck-warning-overlay 'face) 'flycheck-warning)))

(ert-deftest flycheck-error-overlay/face ()
  :tags '(overlay)
  (should (eq (get 'flycheck-error-overlay 'face) 'flycheck-error)))

(ert-deftest flycheck-info-overlay/default-help-echo ()
  :tags '(overlay)
  (should (string= (get 'flycheck-info-overlay 'help-echo) "Unknown info.")))

(ert-deftest flycheck-warning-overlay/default-help-echo ()
  :tags '(overlay)
  (should (string= (get 'flycheck-warning-overlay 'help-echo)
                   "Unknown warning.")))

(ert-deftest flycheck-error-overlay/default-help-echo ()
  :tags '(overlay)
  (should (string= (get 'flycheck-error-overlay 'help-echo) "Unknown error.")))

(ert-deftest flycheck-add-overlay/undefined-error-level ()
  :tags '(overlay)
  (let ((err (should-error (flycheck-add-overlay
                            (flycheck-error-new-at 1 1 'foo)))))
    (should (string= (cadr err) "Undefined error level: foo"))))

(ert-deftest flycheck-add-overlay/no-error-level ()
  :tags '(overlay)
  (let ((err (should-error (flycheck-add-overlay (flycheck-error-new-at 1 1)))))
    (should (string= (cadr err) "Undefined error level: nil"))))

(ert-deftest flycheck-add-overlay/info-category ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'info))))
      (should (eq (overlay-get overlay 'category) 'flycheck-info-overlay)))))

(ert-deftest flycheck-add-overlay/warning-category ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'warning))))
      (should (eq (overlay-get overlay 'category) 'flycheck-warning-overlay)))))

(ert-deftest flycheck-add-overlay/error-category ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'error))))
      (should (eq (overlay-get overlay 'category) 'flycheck-error-overlay)))))

(ert-deftest flycheck-add-overlay/has-help-echo ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (let ((overlay (flycheck-add-overlay
                    (flycheck-error-new-at 1 1 'info "A bar message"))))
      (should (string= (overlay-get overlay 'help-echo) "A bar message")))))

(ert-deftest flycheck-add-overlay/has-flycheck-overlay-property ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'error))
           (overlay (flycheck-add-overlay err)))
      (should (overlay-get overlay 'flycheck-overlay)))))

(ert-deftest flycheck-add-overlay/has-flycheck-error-property ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'warning))
           (overlay (flycheck-add-overlay err)))
      (should (eq (overlay-get overlay 'flycheck-error) err)))))

(ert-deftest flycheck-add-overlay/has-no-fringe-icon-with-disabled-indication ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Hello\n    World")
    (let ((flycheck-indication-mode nil))
      (dolist (level '(warning info error))
        (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 level))))
          (should-not (overlay-get overlay 'before-string)))))))

(ert-deftest flycheck-add-overlay/has-info-fringe-icon ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'info)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-info))
      (should (eq bitmap 'empty-line)))))

(ert-deftest flycheck-add-overlay/has-warning-fringe-icon ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'warning)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-warning))
      (should (eq bitmap 'question-mark)))))

(ert-deftest flycheck-add-overlay/has-error-fringe-icon ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'error)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-error))
      (should (eq bitmap 'exclamation-mark)))))

(ert-deftest flycheck-add-overlay/has-left-fringe-icon ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Hello\n    World")
    ;; Test the various indication modes
    (let ((flycheck-indication-mode 'left-fringe))
      (pcase-let* ((overlay (flycheck-add-overlay
                             (flycheck-error-new-at 1 1 'error)))
                   (before-string (overlay-get overlay 'before-string))
                   (`(,side _ _) (get-text-property 0 'display before-string)))
        (should (eq side 'left-fringe))))))

(ert-deftest flycheck-add-overlay/has-right-fringe-icon ()
  :tags '(overlay)
  (flycheck-test-with-temp-buffer
    (insert "Hello\n    World")
    ;; Test the various indication modes
    (let ((flycheck-indication-mode 'right-fringe))
      (pcase-let* ((overlay (flycheck-add-overlay
                             (flycheck-error-new-at 1 1 'error)))
                   (before-string (overlay-get overlay 'before-string))
                   (`(,side _ _) (get-text-property 0 'display before-string)))
        (should (eq side 'right-fringe))))))

(ert-deftest flycheck-add-overlay/right-position-in-narrowed-buffer ()
  :tags '(overlay language-emacs-lisp)
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
     '(9 1 warning "`message' called with 0 args to fill 1 format field(s)"
         :checker emacs-lisp)
     '(11 8 warning "`message' called with 0 args to fill 1 format field(s)"
          :checker emacs-lisp)
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(15 1 warning "`message' called with 0 args to fill 1 format field(s)"
          :checker emacs-lisp))))


;;; Error navigation

(defmacro flycheck-test-with-nav-buffer (&rest body)
  (declare (indent 0))
  `(flycheck-test-with-resource-buffer "checkers/emacs-lisp.el"
     (emacs-lisp-mode)
     (flycheck-mode)
     (flycheck-test-buffer-sync)
     (goto-char (point-min))
     ,@body))

(ert-deftest flycheck-next-error/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (flycheck-next-error)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-next-error/goes-to-next-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (flycheck-next-error)
    (flycheck-next-error)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-next-error/errors-beyond-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (flycheck-next-error 2)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-next-error/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-next-error -2)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-next-error/reset-navigates-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-next-error 1 'reset)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-next-error/does-not-cross-narrowing ()
  :tags '(navigation)
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
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (let ((err (should-error (flycheck-previous-error)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/goes-to-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-previous-error)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-previous-error/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-previous-error 2)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-previous-error/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (flycheck-previous-error -2)
    (should (flycheck-test-at-nth-error 2))))

(ert-deftest flycheck-previous-error/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-first-error/stays-at-first-error-if-called-again ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error)
    (flycheck-first-error)
    (should (flycheck-test-at-nth-error 1))))

(ert-deftest flycheck-first-error/goes-to-second-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error 2)
    (should (flycheck-test-at-nth-error 2))))


;;; Error list

(ert-deftest flycheck-error-list-buffer/name ()
  :tags '(error-list)
  (should (string= flycheck-error-list-buffer "*Flycheck errors*")))

(ert-deftest flycheck-error-list-mode/derived-from-tabulated-list-mode ()
  :tags '(error-list)
  (with-temp-buffer
    (flycheck-error-list-mode)
    (derived-mode-p 'tabulated-list)))

(ert-deftest flycheck-error-list-mode/tabulated-list-format ()
  :tags '(error-list)
  (with-temp-buffer
    (flycheck-error-list-mode)
    (should (equal tabulated-list-format
                   [("Line" 4 flycheck-error-list-entry-< :right-align t)
                    ("Col" 3 nil :right-align t)
                    ("Level" 8 flycheck-error-list-entry-level-<)
                    ("Message" 0 nil)]))
    (should (local-variable-p 'tabulated-list-format))))

(ert-deftest flycheck-error-list-mode/tabulated-list-padding ()
  :tags '(error-list)
  (with-temp-buffer
    (flycheck-error-list-mode)
    (should (equal tabulated-list-padding 1))
    (should (local-variable-p 'tabulated-list-padding))))

(ert-deftest flycheck-error-list-mode/tabulated-list-entries ()
  :tags '(error-list)
  (with-temp-buffer
    (flycheck-error-list-mode)
    (should (eq tabulated-list-entries 'flycheck-error-list-entries))
    (should (local-variable-p 'tabulated-list-entries))))

(ert-deftest flycheck-error-list-mode/initializes-header ()
  :tags '(error-list)
  (with-temp-buffer
    (should-not header-line-format)
    (flycheck-error-list-mode)
    (should (string= header-line-format " Line Col Level Message "))))

(ert-deftest flycheck-error-list-source-buffer/is-permanently-local ()
  :tags '(error-list)
  (should (get 'flycheck-error-list-source-buffer 'permanent-local)))

(ert-deftest flycheck-error-list-make-entry/line-and-column ()
  :tags '(error-list)
  (let* ((error (flycheck-error-new-at 10 12 'warning "A foo warning"
                                       :checker 'emacs-lisp-checkdoc))
         (entry (flycheck-error-list-make-entry error))
         (cells (cadr entry)))
    (should (eq (car entry) error))
    (should (equal (aref cells 0)
                   (list "10"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-line-number)))
    (should (equal (aref cells 1)
                   (list "12"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-column-number)))
    (let ((face (flycheck-error-level-error-list-face 'warning)))
      (should (equal (aref cells 2)
                     (list "warning"
                           'type 'flycheck-error-list
                           'face face))))
    (should (equal (aref cells 3)
                   (list "A foo warning (emacs-lisp-checkdoc)"
                         'type 'flycheck-error-list
                         'face 'default)))))

(ert-deftest flycheck-error-list-make-entry/no-column ()
  :tags '(error-list)
  (let* ((error (flycheck-error-new-at 10 nil 'error "A foo error"
                                       :checker 'emacs-lisp-checkdoc))
         (entry (flycheck-error-list-make-entry error))
         (cells (cadr entry)))
    (should (eq (car entry) error))
    (should (equal (aref cells 0)
                   (list "10"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-line-number)))
    (should (equal (aref cells 1)
                   (list ""
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-column-number)))
    (let ((face (flycheck-error-level-error-list-face 'error)))
      (should (equal (aref cells 2)
                     (list "error"
                           'type 'flycheck-error-list
                           'face face))))
    (should (equal (aref cells 3)
                   (list "A foo error (emacs-lisp-checkdoc)"
                         'type 'flycheck-error-list
                         'face 'default)))))

(ert-deftest flycheck-error-list-make-entry/no-message ()
  :tags '(error-list)
  (let* ((error (flycheck-error-new-at 10 nil 'info nil :checker 'coq))
         (entry (flycheck-error-list-make-entry error))
         (cells (cadr entry)))
    (should (eq (car entry) error))
    (should (equal (aref cells 0)
                   (list "10"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-line-number)))
    (should (equal (aref cells 1)
                   (list ""
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-column-number)))
    (let ((face (flycheck-error-level-error-list-face 'info)))
      (should (equal (aref cells 2)
                     (list "info"
                           'type 'flycheck-error-list
                           'face face))))
    (should (equal (aref cells 3)
                   (list "Unknown info (coq)"
                         'type 'flycheck-error-list
                         'face 'default)))))


;;; General error display

(ert-deftest flycheck-display-errors/no-display-function-set ()
  :tags '(error-display)
  (let ((err (flycheck-error-new-at 10 20 'warning "This is a Flycheck error."))
        (flycheck-display-errors-function nil))
    (shut-up
      ;; Without an error function, error display should be a no-op.
      (flycheck-display-errors (list err))
      (should (equal (shut-up-current-output) "")))))

(ert-deftest flycheck-display-errors/custom-function ()
  :tags '(error-display)
  (let* ((err (flycheck-error-new-at 10 20 'warning "Foo"))
         (displayed-errors nil)
         (flycheck-display-errors-function (lambda (errors)
                                             (dolist (err errors)
                                               (push err displayed-errors)))))
    (flycheck-display-errors (list err))
    (should (equal displayed-errors (list err)))))


;;; Error display functions

(ert-deftest flycheck-display-error-messages ()
  :tags '(error-display)
  (let ((err (flycheck-error-new-at 10 20 'warning
                                    "This is a Flycheck error.")))
    (shut-up
      (flycheck-display-error-messages (list err))
      (should (equal (shut-up-current-output)
                     (concat (flycheck-error-message err) "\n"))))))


;;; Working with error messages

(ert-deftest flycheck-copy-messages-as-kill ()
  :tags '(error-messages)
  (flycheck-test-with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (mapc #'flycheck-add-overlay errors)
      (let ((flycheck-display-errors-function 'display-function))
        (flycheck-copy-messages-as-kill 10)))
    (should (equal (-take 2 kill-ring) '("1st message" "2nd message")))))

(ert-deftest flycheck-google-messages/error-on-too-many-messages ()
  :tags '(error-messages)
  (flycheck-test-with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (mapc #'flycheck-add-overlay errors)
      (let* ((flycheck-google-max-messages 1)
             (err (should-error (flycheck-google-messages 10)
                                :type flycheck-test-user-error-type)))
        (should (string= (cadr err) "More than 1 messages at point"))))))

(ert-deftest flycheck-google-messages/searches-google ()
  :tags '(error-messages)
  (flycheck-test-with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (mapc #'flycheck-add-overlay errors)
      (let* (browsed-urls
             (browse-url-browser-function (lambda (url &optional _)
                                            (push url browsed-urls))))
        (flycheck-google-messages 10)
        ;; Arguments come out backwards because of `push'
        (should (equal '("https://www.google.com/search?ion=1&q=2nd%20message"
                         "https://www.google.com/search?ion=1&q=1st%20message")
                       browsed-urls))))))

(ert-deftest flycheck-google-messages/searches-google-with-quoted-urls ()
  :tags '(error-messages)
  (flycheck-test-with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (mapc #'flycheck-add-overlay errors)
      (let* (browsed-urls
             (browse-url-browser-function (lambda (url &optional _)
                                            (push url browsed-urls))))
        (flycheck-google-messages 10 'quote)
        ;; Arguments come out backwards because of `push'
        (should (equal '("https://www.google.com/search?ion=1&q=%222nd%20message%22"
                         "https://www.google.com/search?ion=1&q=%221st%20message%22")
                       browsed-urls))))))


;;; Syntax checker executables

(ert-deftest flycheck-overridden-executable ()
  :tags '(executables language-emacs-lisp)
  (let ((flycheck-emacs-lisp-executable (flycheck-test-resource-filename
                                         "bin/dummy-emacs")))
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(17 4 error "t is not true!" :checker emacs-lisp)
     '(19 11 warning "This is a stupid message" :checker emacs-lisp))))

(ert-deftest flycheck-set-checker-executable/real-executable ()
  :tags '(executables)
  (flycheck-test-with-temp-buffer
    ;; Create a temporary buffer to restrict the scope of
    ;; `flycheck-emacs-lisp-executable'
    (let ((file-name (flycheck-test-resource-filename "bin/dummy-emacs")))
      (should (file-exists-p file-name))
      (should (file-executable-p file-name))
      (flycheck-set-checker-executable 'emacs-lisp file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))))
  ;; The global value should remain unaffected
  (should-not flycheck-emacs-lisp-executable))

(ert-deftest flycheck-set-checker-executable/no-executable-given ()
  :tags '(executables)
  (flycheck-test-with-temp-buffer
    (let ((file-name (flycheck-test-resource-filename "bin/dummy-emacs")))
      (setq flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp)
      (should-not flycheck-emacs-lisp-executable))))

(ert-deftest flycheck-set-checker-executable/executable-is-nil ()
  :tags '(executables)
  (flycheck-test-with-temp-buffer
    (let ((file-name (flycheck-test-resource-filename "bin/dummy-emacs")))
      (setq flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp nil)
      (should-not flycheck-emacs-lisp-executable))))

(ert-deftest flycheck-set-checker-executable/non-existing-file ()
  :tags '(executables)
  (let ((file-name (flycheck-test-resource-filename "no-such-file")))
    (should-not (file-exists-p file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))

(ert-deftest flycheck-set-checker-executable/file-not-executable ()
  :tags '(executables)
  (let ((file-name (flycheck-test-resource-filename "checkers/emacs-lisp.el")))
    (should (file-exists-p file-name))
    (should-not (file-executable-p file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-test-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))


;;; Built-in syntax checkers

;; Tell the byte compiler about the variables we'll use
(defvar js2-mode-show-strict-warnings)
(defvar js2-mode-show-parse-errors)
(defvar js3-mode-show-parse-errors)
(defvar python-indent-guess-indent-offset)

(ert-deftest flycheck-define-checker/ada-gnat-syntax-error ()
  :tags '(builtin-checker external-tool language-ada)
  (skip-unless (flycheck-check-executable 'ada-gnat))
  (flycheck-test-should-syntax-check
   "checkers/ada/syntaxerror.adb" 'ada-mode
   '(7  32 error "missing \";\"" :checker ada-gnat)
   '(8 5 error "misspelling of \"SYNTAXERROR\"" :checker ada-gnat)))

(ert-deftest flycheck-define-checker/ada-gnat-warnings ()
  :tags '(builtin-checker external-tool language-ada)
  (skip-unless (flycheck-check-executable 'ada-gnat))
  (flycheck-test-should-syntax-check
   "checkers/ada/hello.adb" 'ada-mode
   '(   6 4 warning "variable \"Name\" is not referenced" :checker ada-gnat)
   '(8  11 warning "unrecognized pragma \"Foo\"" :checker ada-gnat)))

(ert-deftest flycheck-define-checker/ada-gnat-disable-warnings ()
  :tags '(builtin-checker external-tool language-ada)
  (skip-unless (flycheck-check-executable 'ada-gnat))
  (let ((flycheck-gnat-warnings nil))
    (flycheck-test-should-syntax-check
     "checkers/ada/hello.adb" 'ada-mode
     '(8  11 warning "unrecognized pragma \"Foo\"" :checker ada-gnat))))

(ert-deftest flycheck-define-checker/asciidoc ()
  :tags '(builtin-checker external-tool language-asciidoc)
  (skip-unless (flycheck-check-executable 'asciidoc))
  (flycheck-test-should-syntax-check
   "checkers/asciidoc.adoc" 'adoc-mode
   '(1 nil warning "missing style: [paradef-default]: paragraph" :checker asciidoc)
   '(3 nil warning "old tables syntax" :checker asciidoc)
   '(11 nil error "[tabledef-default] illegal width=%60%" :checker asciidoc)))

(ert-deftest flycheck-define-checker/c/c++-clang-warning ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-disabled-checkers '(c/c++-gcc c/c++-cppcheck)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)
     '(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-warning-customized ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  ;; Disable conversion checks by removing -Wextra, but additionally warn about
  ;; missing prototypes, which isn't included in -Wextra
  (let ((flycheck-clang-warnings '("all" "missing-prototypes"))
        (flycheck-disabled-checkers '(c/c++-gcc c/c++-cppcheck)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(3 5 warning "no previous prototype for function 'f'"
         :checker c/c++-clang)
     '(5 10 warning "unused variable 'unused'" :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-fatal-error ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode
     '(2 10 error "'c_c++-library-header.h' file not found"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-include-path ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-clang-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode)))

(ert-deftest flycheck-define-checker/c/c++-clang-included-file-error ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-clang-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-included-file-error.cpp" 'c++-mode
     '(3 nil error "Errors in included file:
2:3:error: unknown type name 'this_is_bad' (c/c++-clang)
3:1:error: expected member name or ';' after declaration specifiers (c/c++-clang)
3:2:error: expected ';' after class (c/c++-clang)
1:1:error: anonymous structs and classes must be class members (c/c++-clang)"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-includes ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let  ((flycheck-clang-includes (list (flycheck-test-resource-filename
                                         "checkers/c_c++-include/c_c++-library-header.h")))
         (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(3 23 info "template is declared here" :checker c/c++-clang)
     '(8 17 error "implicit instantiation of undefined template 'test<false>'"
         :checker c/c++-clang)
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error-template ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error-template.cpp" 'c++-mode
     '(2 20 error "no member named 'bar' in 'A'"
         :checker c/c++-clang)
     '(6 19 info "in instantiation of function template specialization 'foo<A>' requested here"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error-language-standard ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-clang-language-standard "c++11")
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(3 23 info "template is declared here" :checker c/c++-clang)
     '(8 17 error "implicit instantiation of undefined template 'test<false>'"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error-definitions ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-clang-definitions '("FLYCHECK_LIBRARY"))
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-ms-extensions-disabled ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  ;; A sanity check for the following c/c++-clang-ms-extensions-enabled test
  ;; case
  (flycheck-test-should-syntax-check
   "checkers/c_c++-ms-extensions.c" 'c-mode
   '(7 5 warning "declaration does not declare anything" :checker c/c++-clang)
   '(14 24 error "field designator 'a' does not refer to any field in type 'outer_s'"
        :checker c/c++-clang)))

(ert-deftest flycheck-define-checker/c/c++-clang-ms-extensions-enabled ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-clang-ms-extensions t))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-ms-extensions.c" 'c-mode
     '(7 5 warning "anonymous structs are a Microsoft extension"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error-no-exceptions ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-disabled-checkers '(c/c++-gcc))
        (flycheck-clang-no-exceptions t))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error-exceptions.cpp" 'c++-mode
     '(1 14 error "cannot use 'throw' with exceptions disabled"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-error-no-rtti ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-clang-no-rtti t)
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error-rtti.cpp" 'c++-mode
     '(4 32 error "cannot use dynamic_cast with -fno-rtti"
         :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-clang-block-error ()
  ;; On Travis CI and OS X, -fblocks seems to be default for Clang, so this test
  ;; won't pass
  :expected-result '(or (satisfies flycheck-test-failed-on-travis-ci-p)
                        (satisfies (lambda (result)
                                     (and (eq system-type 'darwin)
                                          (ert-test-failed-p result))))
                        :passed)
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (flycheck-test-should-syntax-check
   "checkers/c_c++-clang-blocks.c" 'c-mode
   '(3 15 error "blocks support disabled - compile with -fblocks or pick a deployment target that supports them"
       :checker c/c++-clang)
   '(7 20 error "blocks support disabled - compile with -fblocks or pick a deployment target that supports them"
       :checker c/c++-clang)))

(ert-deftest flycheck-define-checker/c/c++-clang-blocks ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-clang))
  (let ((flycheck-clang-blocks t))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-clang-blocks.c" 'c-mode
     '(7 16 warning "unused variable 'p'" :checker c/c++-clang))))

(ert-deftest flycheck-define-checker/c/c++-gcc-warning ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(5 10 warning "unused variable ‘unused’" :checker c/c++-gcc)
     '(7 15 warning "comparison between signed and unsigned integer expressions"
         :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-warning-customized ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  ;; Disable conversion checks by removing -Wextra, but additionally warn about
  ;; missing prototypes, which isn't included in -Wextra
  (let ((flycheck-gcc-warnings '("all" "missing-prototypes"))
        (flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(3 5 warning "no previous prototype for ‘f’"
         :checker c/c++-gcc)
     '(5 10 warning "unused variable ‘unused’" :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-fatal-error ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode
     '(2 34 error "c_c++-library-header.h: No such file or directory"
         :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-include-path ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-gcc-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode)))

(ert-deftest flycheck-define-checker/c/c++-gcc-included-file-error ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-gcc-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-included-file-error.cpp" 'c++-mode
     '(3 nil error "Errors in included file:
2:3:error: ‘this_is_bad’ does not name a type (c/c++-gcc)
3:1:error: expected ‘;’ after class definition (c/c++-gcc)
3:1:error: abstract declarator ‘<anonymous class>’ used as declaration (c/c++-gcc)"
         :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-includes ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let  ((flycheck-gcc-includes (list (flycheck-test-resource-filename
                                       "checkers/c_c++-include/c_c++-library-header.h")))
         (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 5 warning "identifier ‘nullptr’ is a keyword in C++11"
          :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’"
          :checker c/c++-gcc)
     '(10 16 error "‘nullptr’ was not declared in this scope"
          :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-error ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(8 17 error "aggregate ‘test<false> t’ has incomplete type and cannot be defined"
         :checker c/c++-gcc)
     '(10 5 warning "identifier ‘nullptr’ is a keyword in C++11"
          :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’"
          :checker c/c++-gcc)
     '(10 16 error "‘nullptr’ was not declared in this scope"
          :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-error-template ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error-template.cpp" 'c++-mode
     '(2 18 error "‘struct A’ has no member named ‘bar’"
         :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-error-language-standard ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-gcc-language-standard "c++11")
        (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(8 17 error "aggregate ‘test<false> t’ has incomplete type and cannot be defined"
         :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’"
          :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-error-definitions ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-gcc-definitions '("FLYCHECK_LIBRARY"))
        (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 5 warning "identifier ‘nullptr’ is a keyword in C++11"
          :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’"
          :checker c/c++-gcc)
     '(10 16 error "‘nullptr’ was not declared in this scope"
          :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-error-no-exceptions ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-disabled-checkers '(c/c++-clang))
        (flycheck-gcc-no-exceptions t))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error-exceptions.cpp" 'c++-mode
     '(1 20 error "exception handling disabled, use -fexceptions to enable"
         :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-gcc-error-no-rtti ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-gcc))
  (let ((flycheck-disabled-checkers '(c/c++-clang))
        (flycheck-gcc-no-rtti t))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-error-rtti.cpp" 'c++-mode
     '(4 56 error "‘dynamic_cast’ not permitted with -fno-rtti"
         :checker c/c++-gcc))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-error ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-cppcheck))
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-error.c" 'c-mode
     '(4 nil error "Null pointer dereference" :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-warning ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-cppcheck))
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-warning.c" 'c-mode
     '(2 nil warning "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0."
         :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-style ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-cppcheck))
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-style.c" 'c-mode
     '(3 nil warning "Unused variable: unused" :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-style-suppressed ()
  :tags '(builtin-checker external-tool language-c)
  (skip-unless (flycheck-check-executable 'c/c++-cppcheck))
  (let ((flycheck-cppcheck-checks nil)
        (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-test-should-syntax-check "checkers/c_c++-cppcheck-style.c"
                                       'c-mode)))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-inconclusive ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-cppcheck))
  ;; Cppcheck 1.53 and older do not report inconclusive warnings when using
  ;; XML output.
  (skip-unless (version< "1.53" (flycheck-test-cppcheck-version)))
  (let ((flycheck-cppcheck-checks '("style"))
        (flycheck-cppcheck-inconclusive t)
        (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-inconclusive.cpp" 'c++-mode
     '(1 nil warning "Boolean variable 'a' is used in bitwise operation. Did you mean '&&'?"
         :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-inconclusive-suppressed ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-cppcheck))
  ;; Cppcheck 1.53 and older do not report inconclusive warnings when using
  ;; XML output.
  (skip-unless (version< "1.53" (flycheck-test-cppcheck-version)))
  (let ((flycheck-cppcheck-checks '("style"))
        (flycheck-cppcheck-inconclusive nil)
        (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-inconclusive.cpp" 'c++-mode)))

(ert-deftest flycheck-define-checker/c/c++-cppcheck-multiple-checks ()
  :tags '(builtin-checker external-tool language-c++)
  (skip-unless (flycheck-check-executable 'c/c++-cppcheck))
  (let ((flycheck-cppcheck-checks '("performance" "portability"))
        (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-test-should-syntax-check
     "checkers/c_c++-cppcheck-multiple-checks.cpp" 'c++-mode
     '(2 nil warning "Extra qualification 'A::' unnecessary and considered an error by many compilers."
         :checker c/c++-cppcheck)
     '(9 nil warning "Prefix ++/-- operators should be preferred for non-primitive types. Pre-increment/decrement can be more efficient than post-increment/decrement. Post-increment/decrement usually involves keeping a copy of the previous value around and adds a little extra code."
         :checker c/c++-cppcheck))))

(ert-deftest flycheck-define-checker/cfengine-error ()
  :tags '(builtin-checker external-tool language-cfengine)
  (skip-unless (fboundp 'cfengine3-mode))
  (skip-unless (flycheck-check-executable 'cfengine))
  (flycheck-test-should-syntax-check
   "checkers/cfengine-error.cf" 'cfengine3-mode
   '(8 20 error "Unknown promise type 'nosuchpromisetype'" :checker cfengine)))

(ert-deftest flycheck-define-checker/cfengine-warning ()
  :tags '(builtin-checker external-tool language-cfengine)
  (skip-unless (fboundp 'cfengine3-mode))
  (skip-unless (flycheck-check-executable 'cfengine))
  (flycheck-test-should-syntax-check
   "checkers/cfengine-warning.cf" 'cfengine3-mode
   '(3 34 warning "Removed constraint 'host_licenses_paid' in promise type 'common' [-Wremoved]"
       :checker cfengine)))

(ert-deftest flycheck-define-checker/chef-foodcritic ()
  :tags '(builtin-checker external-tool language-chef)
  (skip-unless (flycheck-check-executable 'chef-foodcritic))
  (flycheck-test-should-syntax-check
   "checkers/chef-foodcritic/recipes/chef-foodcritic-error.rb" 'ruby-mode
   '(3 nil error "FC002: Avoid string interpolation where not required"
       :checker chef-foodcritic)
   '(8 nil error "FC003: Check whether you are running with chef server before using server-specific features"
       :checker chef-foodcritic)
   '(11 nil error "FC004: Use a service resource to start and stop services"
        :checker chef-foodcritic)))

(ert-deftest flycheck-define-checker/coffee-syntax-error ()
  :tags '(builtin-checker external-tool language-coffee)
  (skip-unless (flycheck-check-executable 'coffee))
  (flycheck-test-should-syntax-check
   "checkers/coffee-syntax-error.coffee" 'coffee-mode
   '(4 7 error "missing \", starting" :checker coffee)))

(ert-deftest flycheck-define-checker/coffee-coffeelint-error ()
  :tags '(builtin-checker external-tool language-coffee)
  (skip-unless (flycheck-check-executable 'coffee-coffeelint))
  (flycheck-test-should-syntax-check
   "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
   '(4 nil error "Throwing strings is forbidden; context:"
       :checker coffee-coffeelint)))

(ert-deftest flycheck-define-checker/coffee-coffeelint-warning ()
  :tags '(builtin-checker external-tool language-coffee)
  (skip-unless (flycheck-check-executable 'coffee-coffeelint))
  (let ((flycheck-coffeelintrc "coffeelint.json"))
    (flycheck-test-should-syntax-check
     "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
     '(4 nil warning "Throwing strings is forbidden; context:"
         :checker coffee-coffeelint))))

(ert-deftest flycheck-define-checker/coq-syntax-error-simple ()
  :tags '(builtin-checker external-tool language-coq)
  (skip-unless (flycheck-check-executable 'coq))
  (let* ((version (flycheck-test-coq-version))
         (msg (if (version< "8.3" version)
                  "Lexer: Undefined token"
                "Undefined token.")))
    (flycheck-test-should-syntax-check
     "checkers/coq-syntax-error-simple.v" 'coq-mode
     `(3 18 error ,msg :checker coq))))

(ert-deftest flycheck-define-checker/coq-syntax-error ()
  :tags '(builtin-checker external-tool language-coq)
  (skip-unless (flycheck-check-executable 'coq))
  (flycheck-test-should-syntax-check
   "checkers/coq-syntax-error.v" 'coq-mode
   '(6 12 error "'end' expected after [branches] (in [match_constr])."
       :checker coq)))

(ert-deftest flycheck-define-checker/coq-error ()
  :tags '(builtin-checker external-tool language-coq)
  (skip-unless (flycheck-check-executable 'coq))
  (flycheck-test-should-syntax-check
   "checkers/coq-error.v" 'coq-mode
   '(7 21 error "In environment
evenb : nat -> bool
n : nat
n0 : nat
n' : nat
The term \"1\" has type \"nat\" while it is expected to have type
\"bool\"." :checker coq)))

(ert-deftest flycheck-define-checker/css-csslint ()
  :tags '(builtin-checker external-tool language-css)
  (skip-unless (flycheck-check-executable 'css-csslint))
  (flycheck-test-should-syntax-check
   "checkers/css-csslint-warning.css" 'css-mode
   '(3 6 warning "Heading (h1) should not be qualified."
       :checker css-csslint)))

(ert-deftest flycheck-define-checker/css-csslint-syntax-error ()
  :tags '(builtin-checker external-tool language-css)
  (skip-unless (flycheck-check-executable 'css-csslint))
  (flycheck-test-should-syntax-check
   "checkers/css-syntax-error.css" 'css-mode
   '(4 16 error "Expected LBRACE at line 4, col 16." :checker css-csslint)
   '(4 16 error "Unexpected token '100%' at line 4, col 16."
       :checker css-csslint)
   '(4 20 error "Unexpected token ';' at line 4, col 20." :checker css-csslint)
   '(5 1 error "Unexpected token '}' at line 5, col 1." :checker css-csslint)))

(ert-deftest flycheck-d-module-re/matches-module-name ()
  :tags '(builtin-checker language-d)
  (let ((s "module spam.with.eggs ;"))
    (should (string-match flycheck-d-module-re s))
    (should (string= "spam.with.eggs" (match-string 1 s)))))

(ert-deftest flycheck-d-base-directory/no-module-declaration ()
  :tags '(builtin-checker language-d)
  (flycheck-test-with-resource-buffer "checkers/d/src/dmd/no_module.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-test-resource-filename "checkers/d/src/dmd")))))

(ert-deftest flycheck-d-base-directory/with-module-declaration ()
  :tags '(builtin-checker language-d)
  (flycheck-test-with-resource-buffer "checkers/d/src/dmd/warning.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-test-resource-filename "checkers/d/src")))))

(ert-deftest flycheck-d-base-directory/package-file ()
  :tags '(builtin-checker language-d)
  (flycheck-test-with-resource-buffer "checkers/d/src/dmd/package.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-test-resource-filename "checkers/d/src")))))

(ert-deftest flycheck-define-checker/d-dmd-warning-include-path ()
  :tags '(builtin-checker external-tool language-d)
  (skip-unless (flycheck-check-executable 'd-dmd))
  (let ((flycheck-dmd-include-path '("../../lib")))
    (flycheck-test-should-syntax-check
     "checkers/d/src/dmd/warning.d" 'd-mode
     '(9 5 warning "statement is not reachable" :checker d-dmd)
     '(20 17 warning "function dmd.warning.bar is deprecated"
          :checker d-dmd))))

(ert-deftest flycheck-define-checker/d-dmd-missing-import ()
  :tags '(builtin-checker external-tool language-d)
  (skip-unless (flycheck-check-executable 'd-dmd))
  (flycheck-test-should-syntax-check
   "checkers/d/src/dmd/warning.d" 'd-mode
   '(4 8 error "module external_library is in file 'external_library.d' which cannot be read"
       :checker d-dmd)))

(ert-deftest flycheck-define-checker/elixir-error ()
  :tags '(builtin-checker external-tool language-elixir)
  (skip-unless (flycheck-check-executable 'elixir))
  (flycheck-test-should-syntax-check
   "checkers/elixir-error.ex" 'elixir-mode
   '(5 nil error "function puts/1 undefined" :checker elixir)))

(ert-deftest flycheck-define-checker/elixir-warnings ()
  :tags '(builtin-checker external-tool language-elixir)
  (skip-unless (flycheck-check-executable 'elixir))
  (flycheck-test-should-syntax-check
   "checkers/elixir-warnings.ex" 'elixir-mode
   '(7 nil warning "this clause cannot match because a previous clause at line 4 always matches"
       :checker elixir)))

(ert-deftest flycheck-define-checker/emacs-lisp ()
  :tags '(builtin-checker external-tool language-emacs-lisp)
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
  :tags '(builtin-checker external-tool language-emacs-lisp)
  (let ((flycheck-emacs-lisp-load-path (list (flycheck-test-resource-filename
                                              "dummy-elpa/dummy-package-0.1"))))
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but requires 1+"
          :checker emacs-lisp)
     '(23 1 warning "the function `dummy-package-foo' might not be defined at runtime."
          :checker emacs-lisp))))

(ert-deftest flycheck-define-checker/emacs-lisp-initialize-packages ()
  :tags '(builtin-checker external-tool language-emacs-lisp)
  (let ((flycheck-emacs-lisp-initialize-packages t)
        (flycheck-emacs-lisp-package-user-dir (flycheck-test-resource-filename
                                               "dummy-elpa")))
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but requires 1+"
          :checker emacs-lisp))))

(ert-deftest flycheck-define-checker/emacs-lisp-checks-compressed-file ()
  :tags '(builtin-checker external-tool language-emacs-lisp)
  (flycheck-test-should-syntax-check
   "checkers/emacs-lisp.el.gz" 'emacs-lisp-mode
   '(12 nil warning "First sentence should end with punctuation"
        :checker emacs-lisp-checkdoc)
   '(16 6 warning "message called with 0 arguments, but requires 1+"
        :checker emacs-lisp)
   '(21 1 warning "the function `dummy-package-foo' is not known to be defined."
        :checker emacs-lisp)))

(ert-deftest flycheck-define-checker/emacs-lisp-sytnax-error ()
  :tags '(builtin-checker external-tool language-emacs-lisp)
  (let ((flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
    (flycheck-test-should-syntax-check
     "checkers/emacs-lisp-syntax-error.el" 'emacs-lisp-mode
     '(3 1 error "End of file during parsing" :checker emacs-lisp))))

(ert-deftest flycheck-define-checker/emacs-lisp-without-file-name ()
  "Test checkdoc checker in buffers without file names.

Regression test for https://github.com/flycheck/flycheck/issues/73 and
https://github.com/bbatsov/prelude/issues/259."
  :tags '(builtin-checker external-tool language-emacs-lisp)
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
  :tags '(builtin-checker external-tool language-emacs-lisp)
  (flycheck-test-with-file-buffer (locate-library "dash-autoloads")
    (should-not (flycheck-may-use-checker 'emacs-lisp))
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-define-checker/emacs-lisp-checkdoc-does-not-check-cask-files ()
  :tags '(builtin-checker external-tool language-emacs-lisp)
  (flycheck-test-with-file-buffer
      (expand-file-name "Cask" flycheck-test-source-directory)
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-define-checker/emacs-lisp-does-not-check-with-no-byte-compile ()
  :tags '(builtin-checker external-tool language-emacs-lisp)
  ;; We need to use a hook here, because `no-byte-compile' seems to be
  ;; explicitly changed when loading Emacs Lisp files
  (let ((disable-byte-comp (lambda () (setq-local no-byte-compile t))))
    (add-hook 'emacs-lisp-mode-hook disable-byte-comp)
    (unwind-protect
        (flycheck-test-should-syntax-check
         "checkers/emacs-lisp.el" 'emacs-lisp-mode
         '(12 nil warning "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))
      (remove-hook 'emacs-lisp-mode-hook disable-byte-comp))))

(ert-deftest flycheck-define-checker/erlang-error ()
  :tags '(builtin-checker external-tool language-erlang)
  (skip-unless (flycheck-check-executable 'erlang))
  (flycheck-test-should-syntax-check
   "checkers/erlang-error.erl" 'erlang-mode
   '(7 nil error "head mismatch" :checker erlang)))

(ert-deftest flycheck-define-checker/erlang-warning ()
  :tags '(builtin-checker external-tool language-erlang)
  (skip-unless (flycheck-check-executable 'erlang))
  (flycheck-test-should-syntax-check
   "checkers/erlang-warning.erl" 'erlang-mode
   '(6 nil warning "wrong number of arguments in format call" :checker erlang)))

(ert-deftest flycheck-define-checker/eruby-erubis ()
  :tags '(builtin-checker external-tool language-eruby)
  (skip-unless (flycheck-check-executable 'eruby-erubis))
  (flycheck-test-should-syntax-check
   "checkers/eruby-error.erb" '(html-erb-mode rhtml-mode)
   '(5 nil error "syntax error, unexpected keyword_end" :checker eruby-erubis)))

(ert-deftest flycheck-define-checker/fortran-gfortran-error ()
  :tags '(builtin-checker external-tool language-fortran)
  (skip-unless (flycheck-check-executable 'fortran-gfortran))
  (flycheck-test-should-syntax-check
   "checkers/fortran-error.f" '(fortran-mode f90-mode)
   '(1 1 error "Non-numeric character in statement label at (1)"
       :checker fortran-gfortran)
   '(1 1 error "Unclassifiable statement at (1)" :checker fortran-gfortran)
   '(2 1 error "Non-numeric character in statement label at (1)"
       :checker fortran-gfortran)
   '(2 1 error "Unclassifiable statement at (1)" :checker fortran-gfortran)
   '(3 1 error "Non-numeric character in statement label at (1)"
       :checker fortran-gfortran)
   '(3 1 error "Unclassifiable statement at (1)" :checker fortran-gfortran)))

(ert-deftest flycheck-define-checker/fortran-gfortran-free-form-error ()
  :tags '(builtin-checker external-tool language-fortran)
  (skip-unless (flycheck-check-executable 'fortran-gfortran))
  (let ((flycheck-gfortran-layout 'free))
    (flycheck-test-should-syntax-check
     "checkers/fortran-error.f" '(fortran-mode f90-mode)
     '(3 3 error "Expecting END PROGRAM statement at (1)"
         :checker fortran-gfortran))))

(ert-deftest flycheck-define-checker/fortran-gfortran-warning ()
  :tags '(builtin-checker external-tool language-fortran)
  (skip-unless (flycheck-check-executable 'fortran-gfortran))
  (flycheck-test-should-syntax-check
   "checkers/fortran-warning.f90" '(fortran-mode f90-mode)
   '(1 20 warning "Unused dummy argument 'p' at (1)" :checker fortran-gfortran)
   '(18 9 warning "Same actual argument associated with INTENT(IN) argument 'a' and INTENT(OUT) argument 'b' at (1)"
        :checker fortran-gfortran)))

(ert-deftest flycheck-define-checker/fortran-gfortran-specific-warnings ()
  :tags '(builtin-checker external-tool language-fortran)
  (skip-unless (flycheck-check-executable 'fortran-gfortran))
  (let ((flycheck-gfortran-warnings '("unused-dummy-argument")))
    (flycheck-test-should-syntax-check
     "checkers/fortran-warning.f90" '(fortran-mode f90-mode)
     '(1 20 warning "Unused dummy argument 'p' at (1)"
         :checker fortran-gfortran))))

(ert-deftest flycheck-define-checker/go-syntax-error ()
  :tags '(builtin-checker external-tool language-go)
  (skip-unless (flycheck-check-executable 'go-gofmt))
  (flycheck-test-should-syntax-check
   "checkers/go/src/syntax/syntax-error.go" 'go-mode
   '(5 9 error "expected '(', found 'IDENT' ta" :checker go-gofmt)
   '(6 1 error "expected ')', found '}'" :checker go-gofmt)))

(ert-deftest flycheck-define-checker/go ()
  :tags '(builtin-checker external-tool language-go)
  (skip-unless (-all? #'flycheck-check-executable '(go-build go-golint go-vet)))
  (skip-unless (flycheck-check-predicate 'go-vet))
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (flycheck-test-should-syntax-check
     "checkers/go/src/warnings.go" 'go-mode
     '(4 nil error "imported and not used: \"fmt\"" :checker go-build)
     '(4 2 warning "should not use dot imports" :checker go-golint)
     '(7 1 warning "exported function Warn should have comment or be unexported"
         :checker go-golint)
     '(8 nil error "undefined: fmt" :checker go-build)
     '(11 1 warning "exported function Warnf should have comment or be unexported"
          :checker go-golint)
     '(12 nil error "undefined: fmt" :checker go-build)
     '(17 nil error "undefined: fmt" :checker go-build)
     '(17 nil warning "arg 1 for printf verb %s of wrong type: untyped int"
          :checker go-vet)
     '(19 nil error "cannot use 1 (type int) as type string in argument to Warnf"
          :checker go-build)
     '(23 nil warning "unreachable code" :checker go-vet)
     '(25 9 warning "if block ends with a return statement, so drop this else and outdent its block"
          :checker go-golint))))

(ert-deftest flycheck-define-checker/go-vet-print-functions ()
  :tags '(builtin-checker external-tool language-go)
  (skip-unless (-all? #'flycheck-check-executable '(go-build go-golint go-vet)))
  (skip-unless (flycheck-check-predicate 'go-vet))
  (let ((flycheck-go-vet-print-functions '("Warn:0" "Warnf:1"))
        (flycheck-disabled-checkers '(go-golint go-build go-errcheck)))
    (flycheck-test-with-env
        `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
      (flycheck-test-should-syntax-check
       "checkers/go/src/warnings.go" 'go-mode
       '(17 nil warning "arg 1 for printf verb %s of wrong type: untyped int"
            :checker go-vet)
       '(18 nil warning "possible formatting directive in Warn call"
            :checker go-vet)
       '(19 nil warning "constant 1 not a string in call to Warnf"
            :checker go-vet)
       '(23 nil warning "unreachable code" :checker go-vet)))))

(ert-deftest flycheck-define-checker/go-build-handles-packages ()
  :tags '(builtin-checker external-tool language-go)
  (skip-unless (flycheck-check-executable 'go-build))
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (flycheck-test-should-syntax-check "checkers/go/src/b1/main.go" 'go-mode)))

(ert-deftest flycheck-define-checker/go-build-missing-package ()
  :tags '(builtin-checker external-tool language-go)
  (skip-unless (flycheck-check-executable 'go-build))
  (let* ((go-root (or (getenv "GOROOT") "/usr/local/go"))
         (go-root-pkg (concat go-root "/src/pkg")))
    (flycheck-test-with-env '(("GOPATH" . nil))
      (flycheck-test-should-syntax-check
       "checkers/go/src/b1/main.go" 'go-mode
       `(4 2 error ,(format "cannot find package \"b2\" in any of:\n\t%s/b2 (from $GOROOT)\n\t($GOPATH not set)"
                            go-root-pkg)
           :checker go-build)))))

(ert-deftest flycheck-define-checker/go-test ()
  :tags '(builtin-checker external-tool language-go)
  (skip-unless (flycheck-check-executable 'go-test))
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (flycheck-test-should-syntax-check
     "checkers/go/src/test/test-error_test.go" 'go-mode
     '(8 nil error "undefined: fmt" :checker go-test))))

(ert-deftest flycheck-go-package-name/no-gopath ()
  :tags '(language-go)
  (flycheck-test-with-env '(("GOPATH" . nil))
    (should-not (flycheck-go-package-name
                 (flycheck-test-resource-filename
                  "checkers/go/src/errcheck/errcheck.go")))))

(ert-deftest flycheck-go-package-name/no-package-file ()
  :tags '(language-go)
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (should-not (flycheck-go-package-name
                 (flycheck-test-resource-filename
                  "checkers/emacs-lisp-syntax-error.el")))))

(ert-deftest flycheck-go-package-name/package-file ()
  :tags '(language-go)
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (should (equal "errcheck"
                   (flycheck-go-package-name
                    (flycheck-test-resource-filename
                     "checkers/go/src/errcheck/errcheck.go"))))))

(ert-deftest flycheck-define-checker/go-errcheck ()
  :tags '(builtin-checker external-tool language-go)
  (skip-unless (flycheck-check-executable 'go-errcheck))
  (flycheck-test-with-env
      `(("GOPATH" . ,(flycheck-test-resource-filename "checkers/go")))
    (flycheck-test-should-syntax-check
     "checkers/go/src/errcheck/errcheck.go" 'go-mode
     '(7 9 warning "Ignored `error` returned from `f.Close()`"
         :checker go-errcheck)
     '(9 9 warning "Ignored `error` returned from `os.Stat(\"enoent\")`"
         :checker go-errcheck))))

(ert-deftest flycheck-define-checker/haml ()
  :tags '(builtin-checker external-tool language-haml)
  (skip-unless (flycheck-check-executable 'haml))
  (flycheck-test-should-syntax-check
   "checkers/haml-error.haml" 'haml-mode
   '(5 nil error "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces."
       :checker haml :filename nil)))

(ert-deftest flycheck-define-checker/handlebars ()
  :tags '(builtin-checker external-tool language-handlebars)
  (skip-unless (flycheck-check-executable 'handlebars))
  (flycheck-test-should-syntax-check
   "checkers/handlebars-error.hbs" 'handlebars-mode
   '(2 nil error "Expecting 'ID', 'DATA', got 'INVALID'"
       :checker handlebars :filename nil)))

(ert-deftest flycheck-haskell-module-re/matches-module-name ()
  :tags '(builtin-checker language-haskell)
  (let ((s "module Foo.Bar where"))
    (should (string-match flycheck-haskell-module-re s))
    (should (string= "Foo.Bar" (match-string 1 s)))))

(ert-deftest flycheck-haskell-module-re/ignores-commented-code ()
  :tags '(builtin-checker language-haskell)
  (should-not (string-match-p flycheck-haskell-module-re
                              "-- | module Foo.Bar where")))

(ert-deftest flycheck-define-checker/haskell-ghc-syntax-error ()
  :tags '(builtin-checker external-tool language-haskell)
  (skip-unless (flycheck-check-executable 'haskell-ghc))
  (flycheck-test-should-syntax-check
   "checkers/Haskell/SyntaxError.hs" 'haskell-mode
   '(3 1 error "parse error on input ‘module’" :checker haskell-ghc)))

(ert-deftest flycheck-define-checker/haskell-ghc-no-user-package-database ()
  :expected-result :failed
  :tags '(builtin-checker language-haskell)
  (error "Not implemented!"))

(ert-deftest flycheck-define-checker/haskell-ghc-package-databases ()
  :expected-result :failed
  :tags '(builtin-checker language-haskell)
  (error "Not implemented!"))

(ert-deftest flycheck-define-checker/haskell-ghc-search-path ()
  :tags '(builtin-checker language-haskell)
  (skip-unless (flycheck-check-executable 'haskell-ghc))
  (let* ((lib-dir (flycheck-test-resource-filename "checkers/Haskell/lib"))
         (flycheck-ghc-search-path (list lib-dir)))
    (flycheck-test-should-syntax-check
     "checkers/Haskell/SearchPath.hs" 'haskell-mode
     '(5 1 warning "Top-level binding with no type signature: helloYou :: IO ()"
         :checker haskell-ghc))))

(ert-deftest flycheck-define-checker/haskell-ghc-missing-language-extension ()
  :tags '(builtin-checker language-haskell)
  (skip-unless (flycheck-check-executable 'haskell-ghc))
  (flycheck-test-should-syntax-check
   "checkers/Haskell/LanguageExtension.hs" 'haskell-mode
   '(4 18 error "Couldn't match expected type ‘BS.ByteString’
            with actual type ‘[Char]’
In the first argument of ‘BS.putStr’, namely ‘\"Hello World\"’
In the expression: BS.putStr \"Hello World\"
In an equation for ‘main’: main = BS.putStr \"Hello World\""
       :checker haskell-ghc)))

(ert-deftest flycheck-define-checker/haskell-ghc-language-extensions ()
  :tags '(builtin-checker language-haskell)
  (skip-unless (flycheck-check-executable 'haskell-ghc))
  (let ((flycheck-ghc-language-extensions '("OverloadedStrings")))
    (flycheck-test-should-syntax-check
     "checkers/Haskell/LanguageExtension.hs" 'haskell-mode)))

(ert-deftest flycheck-define-checker/haskell ()
  :tags '(builtin-checker external-tool language-haskell)
  (skip-unless (-all? #'flycheck-check-executable '(haskell-ghc haskell-hlint)))
  (flycheck-test-should-syntax-check
   "checkers/Haskell/Warnings.hs" 'haskell-mode
   '(6 1 warning "Top-level binding with no type signature: foo :: Integer"
       :checker haskell-ghc)
   '(9 1 error "Eta reduce
Found:
  spam eggs = map lines eggs
Why not:
  spam = map lines" :checker haskell-hlint)
   '(12 8 warning "Redundant bracket
Found:
  (putStrLn bar)
Why not:
  putStrLn bar" :checker haskell-hlint)))

(ert-deftest flycheck-define-checker/html-tidy ()
  :tags '(builtin-checker external-tool language-html)
  (skip-unless (flycheck-check-executable 'html-tidy))
  (flycheck-test-should-syntax-check
   "checkers/html-tidy-warning-and-error.html" '(html-mode)
   '(3 1 warning "missing <!DOCTYPE> declaration"
       :checker html-tidy :filename nil)
   '(8 5 error "<spam> is not recognized!"
       :checker html-tidy :filename nil)
   '(8 5 warning "discarding unexpected <spam>"
       :checker html-tidy :filename nil)))

(ert-deftest flycheck-define-checker/javascript-jshint-syntax-error ()
  :tags '(builtin-checker external-tool language-javascript)
  (skip-unless (flycheck-check-executable 'javascript-jshint))
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
  :tags '(builtin-checker external-tool language-javascript)
  (skip-unless (flycheck-check-executable 'javascript-jshint))
  (flycheck-test-should-syntax-check
   "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)))

(ert-deftest flycheck-define-checker/javascript-jshint ()
  :tags '(builtin-checker external-tool language-javascript)
  (skip-unless (flycheck-check-executable 'javascript-jshint))
  (let ((flycheck-jshintrc "jshintrc"))
    (flycheck-test-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 9 error "'foo' is defined but never used."
         :checker javascript-jshint))))

(ert-deftest flycheck-define-checker/javascript-eslint-error ()
  :tags '(builtin-checker external-tool language-javascript)
  (skip-unless (flycheck-check-executable 'javascript-eslint))
  (let ((flycheck-disabled-checkers '(javascript-jshint)))
    (flycheck-test-should-syntax-check
     "checkers/javascript-syntax-error.js" '(js-mode js2-mode js3-mode)
     '(3 26 error "Unexpected token ILLEGAL" :checker javascript-eslint))))

(ert-deftest flycheck-define-checker/javascript-eslint-warning ()
  :tags '(builtin-checker external-tool language-javascript)
  (skip-unless (flycheck-check-executable 'javascript-eslint))
  (let ((flycheck-eslintrc "eslint.json")
        (flycheck-disabled-checkers '(javascript-jshint)))
    (flycheck-test-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 8 warning "foo is defined but never used (no-unused-vars)"
         :checker javascript-eslint))))

(ert-deftest flycheck-define-checker/javascript-gjslint ()
  :tags '(builtin-checker external-tool language-javascript)
  (skip-unless (flycheck-check-executable 'javascript-gjslint))
  (let ((flycheck-disabled-checkers '(javascript-jshint javascript-eslint)))
    (flycheck-test-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 nil error "(0131) Single-quoted string preferred over double-quoted string."
         :checker javascript-gjslint)
     '(4 nil error "(0001) Extra space before \"]\""
         :checker javascript-gjslint))))

(ert-deftest flycheck-define-checker/json-jsonlint ()
  :tags '(builtin-checker external-tool language-json)
  "Test a syntax error from multiple top-level objects."
  (skip-unless (flycheck-check-executable 'json-jsonlint))
  (flycheck-test-should-syntax-check
   "checkers/json-jsonlint-error.json" 'text-mode
   '(1 42 error "found: ',' - expected: 'EOF'." :checker json-jsonlint)))

(ert-deftest flycheck-define-checker/less-file-error ()
  :tags '(builtin-checker external-tool language-less)
  (skip-unless (flycheck-check-executable 'less))
  (flycheck-test-should-syntax-check
   "checkers/less-file-error.less" 'less-css-mode
   '(3 1 error "'no-such-file.less' wasn't found" :checker less)))

(ert-deftest flycheck-define-checker/less-syntax-error ()
  :tags '(builtin-checker external-tool language-less)
  (skip-unless (flycheck-check-executable 'less))
  (flycheck-test-should-syntax-check
   "checkers/less-syntax-error.less" 'less-css-mode
   '(1 13 error "missing closing `}`" :checker less)))

(ert-deftest flycheck-define-checker/lua ()
  :tags '(builtin-checker external-tool language-lua)
  (skip-unless (flycheck-check-executable 'lua))
  (flycheck-test-should-syntax-check
   "checkers/lua-syntax-error.lua" 'lua-mode
   '(5 nil error "unfinished string near '\"oh no'"
       :checker lua :filename nil)))

(ert-deftest flycheck-define-checker/make ()
  :tags '(builtin-checker external-tool language-make)
  (skip-unless (flycheck-check-executable 'make))
  (flycheck-test-should-syntax-check
   "checkers/make.mk" '(makefile-mode makefile-gmake-mode)
   '(2 nil error "*** missing separator.  Stop." :checker make)))

(ert-deftest flycheck-define-checker/make-pmake ()
  :tags '(builtin-checker external-tool language-make)
  (let ((flycheck-make-executable "pmake"))
    (skip-unless (flycheck-check-executable 'make))
    (flycheck-test-should-syntax-check
     "checkers/make.mk" 'makefile-bsdmake-mode
     '(2 nil error "Need an operator" :checker make))))

(ert-deftest flycheck-define-checker/perl ()
  :tags '(builtin-checker external-tool language-perl)
  (skip-unless (-all? #'flycheck-check-executable '(perl perl-perlcritic)))
  (flycheck-test-should-syntax-check
   "checkers/perl.pl" '(perl-mode cperl-mode)
   '(6 nil error "Global symbol \"$x\" requires explicit package name"
       :checker perl)
   '(6 nil error "BEGIN not safe after errors--compilation aborted"
       :checker perl)
   '(6 6 error "Glob written as <...> (See page 167 of PBP)"
       :checker perl-perlcritic)))

(ert-deftest flycheck-define-checker/perl-perlcritic-verbosity-5 ()
  :tags '(builtin-checker external-tool language-perl)
  (skip-unless (-all? #'flycheck-check-executable '(perl perl-perlcritic)))
  (let ((flycheck-perlcritic-verbosity 1))
    (flycheck-test-should-syntax-check
     "checkers/perl.pl" '(perl-mode cperl-mode)
     '(1 1 warning "No package-scoped \"$VERSION\" variable found (See page 404 of PBP)"
         :checker perl-perlcritic)
     '(1 1 info "Package \"perl\" does not start with a upper case letter (See pages 45,46 of PBP)"
         :checker perl-perlcritic)
     '(6 nil error "Global symbol \"$x\" requires explicit package name"
         :checker perl)
     '(6 nil error "BEGIN not safe after errors--compilation aborted"
         :checker perl)
     '(6 6 error "Glob written as <...> (See page 167 of PBP)"
         :checker perl-perlcritic)
     '(8 1 info "Builtin function called with parentheses (See page 13 of PBP)"
         :checker perl-perlcritic)
     '(10 1 warning "\"die\" used instead of \"croak\" (See page 283 of PBP)"
          :checker perl-perlcritic))))

(ert-deftest flycheck-define-checker/php-syntax-error ()
  :tags '(builtin-checker external-tool language-php)
  (skip-unless (flycheck-check-executable 'php))
  (flycheck-test-should-syntax-check
   "checkers/php-syntax-error.php" 'php-mode
   '(8 nil error "syntax error, unexpected ')', expecting '('" :checker php)))

(ert-deftest flycheck-define-checker/php ()
  :tags '(builtin-checker external-tool language-php)
  (skip-unless (-all? #'flycheck-check-executable '(php-phpcs php-phpmd)))
  (flycheck-test-should-syntax-check
   "checkers/php.php" 'php-mode
   '(19 6 error "Missing class doc comment" :checker php-phpcs)
   '(21 nil warning "Avoid unused private fields such as '$FOO'."
        :checker php-phpmd)
   '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore"
        :checker php-phpcs)
   '(23 5 error "Doc comment for \"$baz\" missing" :checker php-phpcs)
   '(23 5 error "Missing @return tag in function comment" :checker php-phpcs)
   '(24 nil warning "Avoid unused private methods such as 'bar'."
        :checker php-phpmd)
   '(24 nil warning "Avoid unused parameters such as '$baz'."
        :checker php-phpmd)
   '(24 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
        :checker php-phpcs)
   '(26 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
        :checker php-phpmd)
   '(26 nil warning "Avoid unused local variables such as '$i'."
        :checker php-phpmd)
   '(26 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
        :checker php-phpcs)))

(ert-deftest flycheck-define-checker/php-phpmd-rulesets ()
  :tags '(builtin-checker external-tool language-php)
  (skip-unless (-all? #'flycheck-check-executable '(php-phpcs php-phpmd)))
  (let ((flycheck-phpmd-rulesets (remove "unusedcode" flycheck-phpmd-rulesets)))
    (flycheck-test-should-syntax-check
     "checkers/php.php" 'php-mode
     '(19 6 error "Missing class doc comment" :checker php-phpcs)
     '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore"
          :checker php-phpcs)
     '(23 5 error "Doc comment for \"$baz\" missing" :checker php-phpcs)
     '(23 5 error "Missing @return tag in function comment" :checker php-phpcs)
     '(24 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
          :checker php-phpcs)
     '(26 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
          :checker php-phpmd)
     '(26 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
          :checker php-phpcs))))

(ert-deftest flycheck-define-checker/php-phpcs-standard ()
  :tags '(builtin-checker external-tool language-php)
  (skip-unless (-all? #'flycheck-check-executable '(php-phpcs php-phpmd)))
  (let ((flycheck-phpcs-standard "Zend"))
    (flycheck-test-should-syntax-check
     "checkers/php.php" 'php-mode
     '(21 nil warning "Avoid unused private fields such as '$FOO'."
          :checker php-phpmd)
     '(21 20 error "Private member variable \"FOO\" must contain a leading underscore"
          :checker php-phpcs)
     '(24 nil warning "Avoid unused private methods such as 'bar'."
          :checker php-phpmd)
     '(24 nil warning "Avoid unused parameters such as '$baz'."
          :checker php-phpmd)
     '(26 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
          :checker php-phpmd)
     '(26 nil warning "Avoid unused local variables such as '$i'."
          :checker php-phpmd)
     '(30 1 error "A closing tag is not permitted at the end of a PHP file"
          :checker php-phpcs))))

(ert-deftest flycheck-define-checker/puppet-parser-singleline-syntax-error ()
  :tags '(builtin-checker external-tool language-puppet)
  (skip-unless (flycheck-check-executable 'puppet-parser))
  (flycheck-test-should-syntax-check
   "checkers/puppet-parser-singleline.pp" 'puppet-mode
   '(3 nil error "Syntax error at ','; expected '}'" :checker puppet-parser)))

(ert-deftest flycheck-define-checker/puppet-parser-multiline-syntax-error ()
  :tags '(builtin-checker external-tool language-puppet)
  (skip-unless (flycheck-check-executable 'puppet-parser))
  (flycheck-test-should-syntax-check
   "checkers/puppet-parser-multiline.pp" 'puppet-mode
   '(8 nil error "Unclosed quote after '' in 'something
}
'" :checker puppet-parser)))

(ert-deftest flycheck-define-checker/puppet-lint ()
  :tags '(builtin-checker external-tool language-puppet)
  (skip-unless (flycheck-check-executable 'puppet-lint))
  (flycheck-test-should-syntax-check
   "checkers/puppet-lint.pp" 'puppet-mode
   '(2 nil error "foo::bar not in autoload module layout (autoloader_layout)"
       :checker puppet-lint)
   '(3 nil warning "case statement without a default case (case_without_default)"
       :checker puppet-lint)))

(ert-deftest flycheck-define-checker/python-flake8-syntax-error ()
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-flake8))
  (let ((python-indent-guess-indent-offset nil))       ; Silence Python Mode!
    (flycheck-test-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     '(3 13 error "E901 SyntaxError: invalid syntax" :checker python-flake8))))

(ert-deftest flycheck-define-checker/python-flake8-warning-ignored ()
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-flake8))
  (let ((flycheck-flake8rc "flake8rc"))
    (flycheck-test-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase"
         :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck-define-checker/python-flake8-maximum-complexity ()
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-flake8))
  (let ((flycheck-flake8-maximum-complexity 4))
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
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-flake8))
  (let ((flycheck-flake8-maximum-line-length 45))
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
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-flake8))
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
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-pylint))
  (let ((flycheck-disabled-checkers '(python-flake8))
        (python-indent-guess-indent-offset nil))       ; Silence Python Mode
    (flycheck-test-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     '(3 nil error "invalid syntax (E0001)" :checker python-pylint))))

(ert-deftest flycheck-define-checker/python-pylint ()
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-pylint))
  (let ((flycheck-disabled-checkers '(python-flake8)))
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
     '(12 nil info "No space allowed around keyword argument assignment"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(15 nil info "Unnecessary parens after u'print' keyword (C0325)"
          :checker python-pylint)
     '(17 nil info "Unnecessary parens after u'print' keyword (C0325)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))))

(ert-deftest flycheck-define-checker/python-pylint-disabled-warnings ()
  :tags '(builtin-checker external-tool language-python)
  (skip-unless (flycheck-check-executable 'python-pylint))
  (let ((flycheck-pylintrc "pylintrc")
        (flycheck-disabled-checkers '(python-flake8)))
    (flycheck-test-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
     '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
         :checker python-pylint)
     '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
     '(10 15 warning "Used builtin function 'map' (W0141)"
          :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))))

(ert-deftest flycheck-define-checker/racket ()
  :tags '(builtin-checker external-tool language-racket)
  (skip-unless (flycheck-check-executable 'racket))
  (flycheck-test-should-syntax-check
   "checkers/racket-syntax-error.rkt" 'racket-mode
   '(4 2 error "read: expected a `)' to close `('" :checker racket)))

(ert-deftest flycheck-define-checker/rpm-rpmlint ()
  :tags '(builtin-checker external-tool language-rpm)
  (skip-unless (flycheck-check-executable 'rpm-rpmlint))
  (flycheck-test-should-syntax-check
   "checkers/rpm-warning.spec" '(sh-mode rpm-spec-mode)
   '(1 nil warning "no-cleaning-of-buildroot %install" :checker rpm-rpmlint)
   '(1 nil warning "no-cleaning-of-buildroot %clean" :checker rpm-rpmlint)
   '(1 nil warning "no-buildroot-tag" :checker rpm-rpmlint)
   '(7 nil error "buildarch-instead-of-exclusivearch-tag x86_64"
       :checker rpm-rpmlint)
   '(22 nil warning "macro-in-%changelog %{_bindir}" :checker rpm-rpmlint)))

(ert-deftest flycheck-locate-sphinx-source-directory/not-in-a-sphinx-project ()
  :tags '(builtin-checker language-rst)
  (flycheck-test-with-resource-buffer "checkers/rst.rst"
    (should-not (flycheck-locate-sphinx-source-directory))))

(ert-deftest flycheck-locate-sphinx-source-directory/in-a-sphinx-project ()
  :tags '(builtin-checker language-rst)
  (flycheck-test-with-resource-buffer "checkers/rst-sphinx/index.rst"
    (should (string= (flycheck-locate-sphinx-source-directory)
                     (flycheck-test-resource-filename "checkers/rst-sphinx/")))))

(ert-deftest flycheck-define-checker/rst ()
  :tags '(builtin-checker external-tool language-rst)
  (skip-unless (flycheck-check-executable 'rst))
  (flycheck-test-should-syntax-check
   "checkers/rst.rst" 'rst-mode
   '(8 nil warning "Title underline too short." :checker rst)
   '(14 nil error "Unexpected section title." :checker rst)
   '(16 nil error "Unknown target name: \"restructuredtext\"." :checker rst)
   '(19 nil warning "Title underline too short." :checker rst)
   '(21 nil error "Unknown target name: \"cool\"." :checker rst)
   '(26 nil error "Unexpected section title." :checker rst)))

(ert-deftest flycheck-define-checker/rst-not-in-a-sphinx-project ()
  :tags '(builtin-checker external-tool language-rst)
  (skip-unless (flycheck-check-executable 'rst))
  (flycheck-test-with-resource-buffer "checkers/rst-sphinx/index.rst"
    (rst-mode)
    (should-not (flycheck-may-use-checker 'rst))))

(ert-deftest flycheck-define-checker/rst-sphinx ()
  :tags '(builtin-checker external-tool language-rst)
  (skip-unless (flycheck-check-executable 'rst-sphinx))
  (flycheck-test-should-syntax-check
   "checkers/rst-sphinx/index.rst" 'rst-mode
   '(2 nil warning "Title underline too short." :checker rst-sphinx)
   '(9 nil error "Unknown target name: \"cool\"." :checker rst-sphinx)
   '(9 nil warning "envvar reference target not found: FOO"
       :checker rst-sphinx)))

(ert-deftest flycheck-define-checker/rst-sphinx-no-reference-warnings ()
  :tags '(builtin-checker external-tool language-rst)
  (skip-unless (flycheck-check-executable 'rst-sphinx))
  (let ((flycheck-sphinx-warn-on-missing-references nil))
    (flycheck-test-should-syntax-check
     "checkers/rst-sphinx/index.rst" 'rst-mode
     '(2 nil warning "Title underline too short." :checker rst-sphinx)
     '(9 nil error "Unknown target name: \"cool\"." :checker rst-sphinx))))

(ert-deftest flycheck-define-checker/rst-sphinx-not-outside-of-a-sphinx-project ()
  :tags '(builtin-checker external-tool language-rst)
  (skip-unless (flycheck-check-executable 'rst-sphinx))
  (flycheck-test-with-resource-buffer "checkers/rst.rst"
    (rst-mode)
    (should-not (flycheck-may-use-checker 'rst-sphinx))))

(ert-deftest flycheck-define-checker/ruby-rubocop-syntax-error ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby-rubocop))
  (flycheck-test-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode
   '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubocop)
   '(5 24 error "unterminated string meets end of file" :checker ruby-rubocop)))

(ert-deftest flycheck-define-checker/ruby-rubylint-syntax-error ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby-rubylint))
  (let ((flycheck-disabled-checkers '(ruby-rubocop)))
    (flycheck-test-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubylint))))

(ert-deftest flycheck-define-checker/ruby-syntax-error ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby))
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint)))
    (flycheck-test-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 nil error "syntax error, unexpected tCONSTANT, expecting $end"
         :checker ruby))))

(ert-deftest flycheck-define-checker/ruby-jruby-syntax-error ()
  :expected-result '(or (satisfies flycheck-test-failed-on-travis-ci-p)
                        :passed)
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby-jruby))
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint ruby)))
    (flycheck-test-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 nil error "syntax error, unexpected tCONSTANT" :checker ruby-jruby))))

(ert-deftest flycheck-define-checker/ruby-rubocop-and-rubylint ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (-all? #'flycheck-check-executable '(ruby-rubocop
                                                    ruby-rubylint)))
  (flycheck-test-should-syntax-check
   "checkers/ruby-warnings.rb" 'ruby-mode
   '(1 1 info "Use snake_case for source file names." :checker ruby-rubocop)
   '(3 1 info "Missing top-level class documentation comment."
       :checker ruby-rubocop)
   '(5 5 warning "unused local variable arr" :checker ruby-rubylint)
   '(5 5 warning "Useless assignment to variable - `arr`."
       :checker ruby-rubocop)
   '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
       :checker ruby-rubocop)
   '(10 5 info "the use of then/do is not needed here" :checker ruby-rubylint)
   '(10 5 info "Use a guard clause instead of wrapping the code inside a conditional expression."
        :checker ruby-rubocop)
   '(10 5 info "Favor modifier `if` usage when having a single-line body. Another good alternative is the usage of control flow `&&`/`||`."
        :checker ruby-rubocop)
   '(10 5 info "Never use `then` for multi-line `if`."
        :checker ruby-rubocop)
   '(10 8 warning "Literal `true` appeared in a condition."
        :checker ruby-rubocop)
   '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
   '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
        :checker ruby-rubylint)))

(ert-deftest flycheck-define-checker/ruby-rubocop-disabled-warning ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby-rubocop))
  (let ((flycheck-rubocoprc "rubocop.yml")
        (flycheck-disabled-checkers '(ruby-rubylint)))
    (flycheck-test-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(1 1 info "Use snake_case for source file names." :checker ruby-rubocop)
     '(3 1 info "Missing top-level class documentation comment."
         :checker ruby-rubocop)
     '(5 5 warning "Useless assignment to variable - `arr`."
         :checker ruby-rubocop)
     '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
         :checker ruby-rubocop)
     '(10 5 info "Use a guard clause instead of wrapping the code inside a conditional expression."
          :checker ruby-rubocop)
     '(10 5 info "Never use `then` for multi-line `if`."
          :checker ruby-rubocop)
     '(10 8 warning "Literal `true` appeared in a condition."
          :checker ruby-rubocop))))

(ert-deftest flycheck-define-checker/ruby-rubocop-lint-only ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby-rubocop))
  (let ((flycheck-rubocop-lint-only t)
        (flycheck-disabled-checkers '(ruby-rubylint)))
    (flycheck-test-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(5 5 warning "Useless assignment to variable - `arr`."
         :checker ruby-rubocop)
     '(10 8 warning "Literal `true` appeared in a condition."
          :checker ruby-rubocop))))

(ert-deftest flycheck-define-checker/ruby-rubylint-errors-only ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby-rubylint))
  (skip-unless (version<= "2.0.2" (flycheck-test-rubylint-version)))
  (let ((flycheck-disabled-checkers '(ruby-rubocop))
        (flycheck-rubylintrc "rubylint.yml"))
    (flycheck-test-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
     '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
          :checker ruby-rubylint))))

(ert-deftest flycheck-define-checker/ruby-warnings ()
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby))
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint)))
    (flycheck-test-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(5 nil warning "assigned but unused variable - arr" :checker ruby)
     '(16 nil warning "possibly useless use of == in void context"
          :checker ruby))))

(ert-deftest flycheck-define-checker/ruby-jruby-warnings ()
  :expected-result '(or (satisfies flycheck-test-failed-on-travis-ci-p)
                        :passed)
  :tags '(builtin-checker external-tool language-ruby)
  (skip-unless (flycheck-check-executable 'ruby-jruby))
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint ruby)))
    (flycheck-test-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(16 nil warning "Useless use of == in void context."
          :checker ruby-jruby))))

(ert-deftest flycheck-define-checker/rust-syntax-error ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (flycheck-test-should-syntax-check
   "checkers/rust-syntax-error.rs" 'rust-mode
   '(4 5 error "unresolved name `bla`." :checker rust)))

(ert-deftest flycheck-define-checker/rust-test-syntax-error ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (flycheck-test-should-syntax-check
   "checkers/rust-test-syntax-error.rs" 'rust-mode
   '(5 5 error "unresolved name `bla`." :checker rust)))

(ert-deftest flycheck-define-checker/rust-test-check-tests-disabled ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (let ((flycheck-rust-check-tests nil))
    (flycheck-test-should-syntax-check
     "checkers/rust-test-syntax-error.rs" 'rust-mode)))

(ert-deftest flycheck-define-checker/rust-warning ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (flycheck-test-should-syntax-check
   "checkers/rust-warning.rs" 'rust-mode
   '(3 1 warning "code is never used: `main`, #[warn(dead_code)] on by default"
       :checker rust)
   '(4 9 warning "unused variable: `x`, #[warn(unused_variable)] on by default"
       :checker rust)))

(ert-deftest flycheck-define-checker/rust-test-crate-type-bin ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (let ((flycheck-rust-crate-type "bin")
        (flycheck-rust-check-tests nil))
    (flycheck-test-should-syntax-check
     "checkers/rust-warning.rs" 'rust-mode
     '(4 9 warning "unused variable: `x`, #[warn(unused_variable)] on by default"
         :checker rust))))

(ert-deftest flycheck-define-checker/rust-info ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (flycheck-test-should-syntax-check
   "checkers/rust-info.rs" 'rust-mode
   '(11 9 info "`x` moved here because it has type `NonPOD`, which is moved by default (use `ref` to override)"
        :checker rust)
   '(12 9 error "use of moved value: `x`"
        :checker rust)))

(ert-deftest flycheck-define-checker/rust-library-path ()
  :expected-result :failed
  :tags '(builtin-checker external-tool language-rust)
  ;; TODO: How can we test this without adding binary libraries to our repo?
  (error "Not implemented!"))

(ert-deftest flycheck-define-checker/rust-crate-root-not-set ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (flycheck-test-should-syntax-check
   "checkers/rust_crate/foo.rs" 'rust-mode
   '(1 5 error "unresolved import `super::bar`" :checker rust)))

(ert-deftest flycheck-define-checker/rust-crate-root ()
  :tags '(builtin-checker external-tool language-rust)
  (skip-unless (flycheck-check-executable 'rust))
  (let ((flycheck-rust-crate-root (flycheck-test-resource-filename
                                   "checkers/rust_crate/main.rs")))
    (flycheck-test-should-syntax-check
     "checkers/rust_crate/foo.rs" 'rust-mode
     '(3 9 warning "unused variable: `x`, #[warn(unused_variable)] on by default"
         :checker rust))))

(ert-deftest flycheck-define-checker/sass ()
  :tags '(builtin-checker external-tool language-sass)
  (skip-unless (flycheck-check-executable 'sass))
  (flycheck-test-should-syntax-check
   "checkers/sass-error.sass" 'sass-mode
   '(5 nil error "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces."
       :checker sass)))

(ert-deftest flycheck-define-checker/sass-import-error ()
  :tags '(builtin-checker external-tool language-sass)
  (skip-unless (flycheck-check-executable 'sass))
  (flycheck-test-should-syntax-check
   "checkers/sass-compass.sass" 'sass-mode
   `(2 nil error ,(format "File to import not found or unreadable: compass/css3.
       Load path: %s" (flycheck-test-resource-filename "checkers"))
       :checker sass)))

(ert-deftest flycheck-define-checker/sass-compass ()
  :tags '(builtin-checker external-tool language-sass)
  (skip-unless (flycheck-check-executable 'sass))
  (let ((flycheck-sass-compass t))
    (flycheck-test-should-syntax-check
     "checkers/sass-compass.sass" 'sass-mode)))

(ert-deftest flycheck-define-checker/scala ()
  :expected-result '(or (satisfies flycheck-test-failed-on-travis-ci-p)
                        :passed)
  :tags '(builtin-checker external-tool language-scala)
  (skip-unless (flycheck-check-executable 'scala))
  (flycheck-test-should-syntax-check
   "checkers/scala-syntax-error.scala" 'scala-mode
   '(3 nil error "identifier expected but '{' found." :checker scala)))

(ert-deftest flycheck-define-checker/scala-scalastyle-error ()
  :tags '(builtin-checker external-tool language-scala)
  (skip-unless (flycheck-check-executable 'scala-scalastyle))
  (let ((flycheck-scalastylerc "scalastyle.xml")
        (flycheck-scalastyle-jar "/opt/scalastyle-batch_2.10-0.5.0/scalastyle-batch_2.10.jar"))
    (flycheck-test-should-syntax-check
     "checkers/scala-scalastyle-style-error.scala" 'scala-mode
     '(6 4 error "Don't use println" :checker scala-scalastyle))))

(ert-deftest flycheck-define-checker/scala-scalastyle-warning ()
  :tags '(builtin-checker external-tool language-scala)
  (skip-unless (flycheck-check-executable 'scala-scalastyle))
  (let ((flycheck-scalastylerc "scalastyle.xml")
        (flycheck-scalastyle-jar "/opt/scalastyle-batch_2.10-0.5.0/scalastyle-batch_2.10.jar"))
    (flycheck-test-should-syntax-check
     "checkers/scala-scalastyle-style-warning.scala" 'scala-mode
     '(5 8 warning "Redundant braces after class definition"
         :checker scala-scalastyle))))

(ert-deftest flycheck-define-checker/scala-scalastyle-inhibited-without-jar ()
  :tags '(builtin-checker external-tool language-scala)
  (skip-unless (flycheck-check-executable 'scala-scalastyle))
  (let ((flycheck-scalastylerc "scalastyle.xml"))
    (flycheck-test-should-syntax-check
     "checkers/scala-scalastyle-style-warning.scala" 'scala-mode)))

(ert-deftest flycheck-define-checker/scss ()
  :tags '(builtin-checker external-tool language-scss)
  (skip-unless (flycheck-check-executable 'scss))
  (flycheck-test-should-syntax-check
   "checkers/scss-error.scss" 'scss-mode
   '(3 nil error "Invalid CSS after \"...    c olor: red\": expected \"{\", was \";\""
       :checker scss)))

(ert-deftest flycheck-define-checker/scss-import-error ()
  :tags '(builtin-checker external-tool language-scss)
  (skip-unless (flycheck-check-executable 'scss))
  (flycheck-test-should-syntax-check
   "checkers/scss-compass.scss" 'scss-mode
   `(2 nil error ,(format "File to import not found or unreadable: compass/css3.
       Load path: %s" (flycheck-test-resource-filename "checkers"))
       :checker scss)))

(ert-deftest flycheck-define-checker/scss-compass ()
  :tags '(builtin-checker external-tool language-scss)
  (skip-unless (flycheck-check-executable 'scss))
  (let ((flycheck-scss-compass t))
    (flycheck-test-should-syntax-check
     "checkers/scss-compass.scss" 'scss-mode)))

(ert-deftest flycheck-define-checker/sh-bash ()
  :tags '(builtin-checker external-tool language-sh language-sh-bash)
  (skip-unless (flycheck-check-executable 'sh-bash))
  (flycheck-test-should-syntax-check
   "checkers/sh-bash-syntax-error.bash" 'sh-mode
   '(5 nil error "syntax error near unexpected token `fi'" :checker sh-bash)
   '(5 nil error "`fi'" :checker sh-bash)))

(ert-deftest flycheck-define-checker/sh-posix-dash ()
  :tags '(builtin-checker external-tool language-sh language-sh-posix)
  (skip-unless (flycheck-check-executable 'sh-posix-dash))
  (flycheck-test-should-syntax-check
   "checkers/sh-posix-syntax-error.sh" 'sh-mode
   '(3 nil error "Syntax error: \"(\" unexpected" :checker sh-posix-dash)))

(ert-deftest flycheck-define-checker/sh-posix-bash ()
  :tags '(builtin-checker external-tool language-sh language-sh-posix)
  (skip-unless (flycheck-check-executable 'sh-posix-bash))
  (let ((flycheck-disabled-checkers '(sh-posix-dash)))
    (flycheck-test-should-syntax-check
     "checkers/sh-posix-syntax-error.sh" 'sh-mode
     '(3 nil error "syntax error near unexpected token `('"
         :checker sh-posix-bash)
     '(3 nil error "`cat <(echo blah)'" :checker sh-posix-bash))))

(ert-deftest flycheck-define-checker/sh-zsh ()
  :tags '(builtin-checker external-tool language-sh language-sh-zsh)
  (skip-unless (flycheck-check-executable 'sh-zsh))
  (flycheck-test-should-syntax-check
   "checkers/sh-zsh-syntax-error.zsh" 'sh-mode
   '(5 nil error "parse error near `fi'" :checker sh-zsh)))

(ert-deftest flycheck-define-checker/sh-shellcheck ()
  :tags '(builtin-checker external-tool language-sh)
  (skip-unless (flycheck-check-executable 'sh-shellcheck))
  (flycheck-test-should-syntax-check
   "checkers/sh-shellcheck.sh" 'sh-mode
   '(2 5 warning "Note that ~ does not expand in quotes."
       :checker sh-shellcheck)
   '(3 7 error "Double quote array expansions, otherwise they're like $* and break on spaces."
       :checker sh-shellcheck)))

(ert-deftest flycheck-define-checker/slim ()
  :tags '(builtin-checker external-tool language-slim)
  (skip-unless (flycheck-check-executable 'slim))
  (let* ((slim-version (cadr (split-string (car (process-lines "slimrb" "-v")))))
         ;; Old Slim compilers do not report column information
         (column (if (version<= "1.3.1" slim-version) 1 nil)))
    (flycheck-test-should-syntax-check
     "checkers/slim-error.slim" 'slim-mode
     `(2 ,column error "Unexpected indentation" :checker slim))))

(ert-deftest flycheck-define-checker/tex-chktex ()
  :tags '(builtin-checker external-tool language-tex language-latex)
  (skip-unless (flycheck-check-executable 'tex-chktex))
  (flycheck-test-should-syntax-check
   "checkers/tex-warning.tex" 'latex-mode
   '(5 28 warning "13:Intersentence spacing (`\\@') should perhaps be used."
       :checker tex-chktex)))

(ert-deftest flycheck-define-checker/tex-lacheck ()
  :tags '(builtin-checker external-tool language-tex language-latex)
  (skip-unless (flycheck-check-executable 'tex-lacheck))
  (let ((flycheck-disabled-checkers '(tex-chktex)))
    (flycheck-test-should-syntax-check
     "checkers/tex-warning.tex" 'latex-mode
     '(5 nil warning "missing `\\@' before `.' in \"GNU.\""
         :checker tex-lacheck)
     '(7 nil warning "possible unwanted space at \"{\""
         :checker tex-lacheck))))

(ert-deftest flycheck-define-checker/texinfo-errors-only ()
  :tags '(builtin-checker external-tool language-texinfo)
  (skip-unless (flycheck-check-executable 'texinfo))
  ;; Before Texinfo 5, makeinfo only prints errors
  (skip-unless (version< (flycheck-test-texinfo-version) "5"))
  (flycheck-test-should-syntax-check
   "checkers/texinfo.texi" 'texinfo-mode
   '(7 nil error "Unknown command `bold'." :checker texinfo)
   '(7 nil error "Misplaced {." :checker texinfo)
   '(7 nil error "Misplaced }." :checker texinfo)))

(ert-deftest flycheck-define-checker/texinfo-errors-and-warnings ()
  :tags '(builtin-checker external-tool language-texinfo)
  (skip-unless (flycheck-check-executable 'texinfo))
  ;; Before Texinfo 5, makeinfo does not output any warnings
  (skip-unless (version<= "5" (flycheck-test-texinfo-version)))
  (flycheck-test-should-syntax-check
   "checkers/texinfo.texi" 'texinfo-mode
   '(   3 nil warning "@settitle missing argument" :checker texinfo)
   '(7 nil error "unknown command `bold'" :checker texinfo)
   '(7 nil error "misplaced {" :checker texinfo)
   '(7 nil error "misplaced }" :checker texinfo)
   '(9 nil warning "printindex before document beginning: @printindex cp"
       :checker texinfo)))

(ert-deftest flycheck-define-checker/verilog-verilator-error ()
  :tags '(builtin-checker external-tool language-verilog)
  (skip-unless (flycheck-check-executable 'verilog-verilator))
  (flycheck-test-should-syntax-check
   "checkers/verilog_verilator_error.v" 'verilog-mode
   '(4 nil error "syntax error, unexpected ')'"
       :checker verilog-verilator)))

(ert-deftest flycheck-define-checker/verilog-verilator-warning ()
  :tags '(builtin-checker external-tool language-verilog)
  (skip-unless (flycheck-check-executable 'verilog-verilator))
  (flycheck-test-should-syntax-check
   "checkers/verilog_verilator_warning.v" 'verilog-mode
   '(2 nil warning "Signal is not driven, nor used: val"
       :checker verilog-verilator)))

(ert-deftest flycheck-define-checker/xml-xmlstarlet ()
  :tags '(builtin-checker external-tool language-xml)
  (skip-unless (flycheck-check-executable 'xml-xmlstarlet))
  (flycheck-test-should-syntax-check
   "checkers/xml-syntax-error.xml" 'nxml-mode
   '(4 10 error "Opening and ending tag mismatch: spam line 3 and with"
       :checker xml-xmlstarlet)))

(ert-deftest flycheck-define-checker/xml-xmllint-error ()
  :tags '(builtin-checker external-tool language-xml)
  (skip-unless (flycheck-check-executable 'xml-xmllint))
  (let ((flycheck-disabled-checkers '(xml-xmlstarlet)))
    (flycheck-test-should-syntax-check
     "checkers/xml-syntax-error.xml" 'nxml-mode
     '(4 nil error "parser error : Opening and ending tag mismatch: spam line 3 and with"
         :checker xml-xmllint)
     '(5 nil error "parser error : Extra content at the end of the document"
         :checker xml-xmllint))))

(ert-deftest flycheck-define-checker/yaml-jsyaml ()
  :tags '(builtin-checker external-tool language-yaml)
  (skip-unless (flycheck-check-executable 'yaml-jsyaml))
  (flycheck-test-should-syntax-check
   "checkers/yaml-syntax-error.yaml" 'yaml-mode
   '(4 5 error "bad indentation of a mapping entry"
       :checker yaml-jsyaml :filename nil)))

(ert-deftest flycheck-define-checker/yaml-ruby ()
  :tags '(builtin-checker external-tool language-yaml)
  (skip-unless (flycheck-check-executable 'yaml-ruby))
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
    (let ((flycheck-disabled-checkers '(yaml-jsyaml)))
      (flycheck-test-should-syntax-check
       "checkers/yaml-syntax-error.yaml" 'yaml-mode expected-error))))


;;; Test results

(defun flycheck-test-syntax-check-timed-out-p (result)
  "Whether RESULT denotes a timed-out test."
  (and (ert-test-failed-p result)
       (eq (car (ert-test-failed-condition result))
           'flycheck-test-syntax-check-timed-out)))

(dolist (test (ert-select-tests t t))
  (let* ((result (ert-test-expected-result-type test))
         (result `(or ,result
                      (satisfies flycheck-test-syntax-check-timed-out-p))))
    (unless flycheck-test-ert-can-skip
      ;; For Emacs 24.3 and below, we mark skipped tests as expected failures,
      ;; but adjusting the expected result of all test cases, because ERT does
      ;; not yet support test skipping.  Not particularly pretty, but works :)
      (setq result `(or ,result (satisfies ert-test-skipped-p))))
    (setf (ert-test-expected-result-type test) result)))

(provide 'flycheck-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-test.el ends here
