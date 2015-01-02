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

(require 'dash)
(require 'cl-lib)
(require 'epa-file)                     ; To test encrypted buffers
(require 'ert)                          ; Unit test library
(require 'shut-up)                      ; Silence Emacs and intercept `message'

(require 'flycheck)
(require 'flycheck-ert)

;; Make a best effort to make Coq Mode available
(mapc (lambda (dir)
        (add-to-list 'load-path (expand-file-name "coq/" dir)))
      '("/usr/share/emacs/site-lisp/"
        "/usr/local/share/emacs/site-lisp/"))

(autoload 'coq-mode "coq")


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


;;; Environment information
(defun flycheck-test-texinfo-version ()
  "Determine the version of Texinfo.

Return the version as string, or nil, if the texinfo version
could not be determined."
  (flycheck-ert-extract-version-command
   (rx "makeinfo (GNU texinfo) "
       (group (one-or-more (any digit)) "." (one-or-more (any digit))))
   "makeinfo" "--version"))

(defun flycheck-test-cppcheck-version ()
  "Determine the version of Cppcheck.

Return the version as string, or nil, if the Cppcheck version
could not be determined."
  (flycheck-ert-extract-version-command
   (rx "Cppcheck "
       (group (one-or-more (any digit)) "." (one-or-more (any digit)))
       (zero-or-more any))
   "cppcheck" "--version"))

(defun flycheck-test-rubylint-version ()
  "Determine the version of rubylint.

Return the version as string, or nil, if the rubylint version
could not be determined."
  (flycheck-ert-extract-version-command
   (rx "ruby-lint v" (group (one-or-more (any digit))
                            (one-or-more "." (one-or-more (any digit)))) " on")
   "ruby-lint" "--version"))

(defun flycheck-test-coq-version ()
  "Determine the version of Coq.

Return the version as string, or nil if the version could not be
determined."
  (flycheck-ert-extract-version-command
   (rx "The Coq Proof Assistant, version "
       (group (one-or-more (any digit)) "." (one-or-more (any digit))))
   "coqtop" "-v"))


;;; Code style
(defmacro flycheck-test-def-indent-test (filename)
  "Define a test case for the indentation of FILENAME.

FILENAME is relative to the source directory.  The test case is
named `flycheck-code-style/FILENAME-BASE/indentation', where
FILENAME-BASE is FILENAME without leading directory components
and extension, as in `file-name-base'."
  (let ((testname (intern (format "flycheck-code-style/%s/indentation"
                                  (file-name-base filename)))))
    `(ert-deftest ,testname ()
       :tags '(style)
       (skip-unless (version<= "24.4" emacs-version))
       (flycheck-ert-with-file-buffer
           (expand-file-name ,filename
                             flycheck-test-source-directory)
         (set-auto-mode)
         (shut-up
           (indent-region (point-min) (point-max)))
         (should-not (buffer-modified-p))))))

(flycheck-test-def-indent-test "flycheck.el")
(flycheck-test-def-indent-test "flycheck-ert.el")
(flycheck-test-def-indent-test "test/run.el")
(flycheck-test-def-indent-test "test/flycheck-test.el")


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

(ert-deftest flycheck-checkers/no-registered-checker-is-disabled ()
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
  (flycheck-ert-with-temp-buffer
    (setq next-error-function :old)
    (flycheck-mode 1)
    (should flycheck-mode)
    (should (eq next-error-function 'flycheck-next-error-function))
    (flycheck-mode -1)
    (should-not flycheck-mode)
    (should (eq next-error-function :old))))

(ert-deftest flycheck-mode/does-not-enable-standard-error-navigation ()
  :tags '(minor-mode)
  (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (goto-char (point-min))
    (insert "foo-bar")
    (flycheck-mode)
    (flycheck-ert-buffer-sync)
    (should flycheck-current-errors)
    (revert-buffer 'ignore-auto 'no-confirm)
    (should-not flycheck-current-errors)
    (should-not (flycheck-deferred-check-p))))


;;; Global syntax checking

(ert-deftest flycheck-may-enable-mode/not-in-ephemeral-buffers ()
  :tags '(global-mode)
  (flycheck-ert-with-temp-buffer
    (should-not (flycheck-may-enable-mode)))
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (rename-buffer " foo")
    (should (string= (buffer-name) " foo"))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode/not-in-encrypted-files ()
  :tags '(global-mode external-tool)
  (skip-unless (flycheck-ert-check-gpg))
  (let* ((filename (flycheck-ert-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-ert-with-resource-buffer filename
      (emacs-lisp-mode)
      (should (flycheck-get-checker-for-buffer))
      (should-not (flycheck-may-enable-mode)))))

(ert-deftest flycheck-may-enable-mode/not-if-no-checker-is-found ()
  :tags '(global-mode)
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (text-mode)
    (should-not (string-prefix-p " " (buffer-name)))
    (should-not (flycheck-get-checker-for-buffer))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode/checker-found ()
  :tags '(global-mode)
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (should (flycheck-may-enable-mode))))

(ert-deftest global-flycheck-mode/does-not-enable-in-ephemeral-buffers ()
  :tags '(global-mode)
  (flycheck-ert-with-global-mode
    (flycheck-ert-with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer " foo")
      (emacs-lisp-mode)
      (should-not flycheck-mode))))

(ert-deftest global-flycheck-mode/does-not-enable-in-encrypted-file ()
  :tags '(global-mode external-tool)
  (skip-unless (flycheck-ert-check-gpg))
  (let* ((filename (flycheck-ert-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-ert-with-global-mode
      (flycheck-ert-with-resource-buffer filename
        (emacs-lisp-mode)
        (should-not flycheck-mode)))))

(ert-deftest global-flycheck-mode/does-not-enable-if-no-checker-is-found ()
  :tags '(global-mode)
  (flycheck-ert-with-global-mode
    (flycheck-ert-with-temp-buffer
      (rename-buffer "foo")
      (text-mode)
      (should-not flycheck-mode))))

(ert-deftest global-flycheck-mode/checker-found ()
  :tags '(global-mode)
  (flycheck-ert-with-global-mode
    (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-temp-buffer
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-deferred)
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-clean-deferred-check/removes-a-deferred-syntax-check ()
  :tags '(deferred)
  (flycheck-ert-with-temp-buffer
    (flycheck-buffer-deferred)
    (flycheck-clean-deferred-check)
    (should-not (flycheck-deferred-check-p))))


;;; Automatic syntax checking

(ert-deftest flycheck-may-check-automatically/not-in-ephemeral-buffers ()
  :tags '(automatic)
  (flycheck-ert-with-temp-buffer
    (should-not (-any? #'flycheck-may-check-automatically
                       '(save idle-change new-line mode-enabled)))
    (should-not (flycheck-may-check-automatically))))

(ert-deftest flycheck-may-check-automatically/in-normal-buffers ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (should (-all? #'flycheck-may-check-automatically
                   '(save idle-change new-line mode-enabled)))
    (should (flycheck-may-check-automatically))))

(ert-deftest flycheck-may-check-automatically/automatic-checking-disabled ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (should-not (-any? #'flycheck-may-check-automatically
                         '(save idle-change new-line mode-enabled)))
      (should (flycheck-may-check-automatically)))))

(ert-deftest flycheck-may-check-automatically/specific-event-disabled ()
  :tags '(automatic)
  (dolist (event '(save idle-change new-line mode-enabled))
    (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
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
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically
           (remq 'mode-enabled flycheck-check-syntax-automatically)))
      (flycheck-mode)
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/mode-enabled-checks-syntax-after-flycheck-mode ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically '(mode-enabled)))
      (flycheck-mode)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/idle-change-is-disabled ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
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
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (let ((flycheck-check-syntax-automatically '(idle-change)))
      (flycheck-mode)
      (insert "Hello world")
      (sleep-for 0.55)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/idle-change-does-not-check-before-delay ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (let ((flycheck-check-syntax-automatically '(idle-change))
          (flycheck-idle-change-delay 1.5))
      (flycheck-mode)
      (insert "Hello world")
      (sleep-for 0.55)
      (should-not (flycheck-deferred-check-p))
      (sleep-for 1.1)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/new-line-is-disabled ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (let ((flycheck-check-syntax-automatically
           (remq 'new-line flycheck-check-syntax-automatically)))
      (insert "\n")
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/new-line-checks-syntax-after-new-line ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically '(new-line)))
      (flycheck-mode)
      (insert "\n")
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/save-disabled ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically nil))
      (flycheck-mode))
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically
           (remq 'save flycheck-check-syntax-automatically)))
      (save-buffer 0)
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically/save-checks-syntax-after-save ()
  :tags '(automatic)
  (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
    (let ((flycheck-check-syntax-automatically '(save)))
      (flycheck-mode)
      (set-buffer-modified-p t)
      (save-buffer 0)
      (should (flycheck-deferred-check-p)))))

(ert-deftest flycheck-buffer-automatically/does-not-check-with-disabled-mode ()
  :tags '(automatic)
  (flycheck-ert-with-temp-buffer
    (should-not flycheck-mode)
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-automatically/defers-the-test ()
  :tags '(automatic)
  (flycheck-ert-with-temp-buffer
    (flycheck-mode)
    ;; Flycheck won't check ephemeral buffers
    (rename-buffer "foo")
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should (flycheck-deferred-check-p))))


;;; Status reporting

(ert-deftest flycheck-report-status/runs-functions ()
  :tags '(status-reporting)
  (flycheck-ert-with-temp-buffer
    (let* ((was-called nil)
           (flycheck-status-changed-functions
            (list (lambda (status) (setq was-called status)))))
      (flycheck-report-status 'running)
      (should (eq was-called 'running)))))

(ert-deftest flycheck-report-failed-syntax-check/runs-hook ()
  :tags '(status-reporting)
  (flycheck-ert-with-temp-buffer
    (let* ((was-called nil)
           (flycheck-syntax-check-failed-hook
            (list (lambda () (setq was-called t)))))
      (flycheck-report-failed-syntax-check)
      (should was-called))))

(ert-deftest flycheck-report-failed-syntax-check/clears-errors ()
  :tags '(status-reporting)
  (flycheck-ert-with-temp-buffer
    (let ((flycheck-current-errors (list 'foo)))
      (flycheck-report-failed-syntax-check)
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
          (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-temp-buffer
    (should (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-ephemeral-buffer-p/buffer-with-leading-whitespace ()
  :tags '(utility)
  (flycheck-ert-with-temp-buffer
    (rename-buffer " foo")
    (should (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-ephemeral-buffer-p/buffer-without-leading-whitespace ()
  :tags '(utility)
  (flycheck-ert-with-temp-buffer
    (rename-buffer "foo")
    (should-not (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/unencrypted-temporary-buffer ()
  :tags '(utility)
  (flycheck-ert-with-temp-buffer
    (should-not (flycheck-encrypted-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/unencrypted-file-buffer ()
  :tags '(utility)
  (flycheck-ert-with-resource-buffer "global-mode-dummy.el"
    (should-not (flycheck-encrypted-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/encrypted-file-buffer ()
  :tags '(utility external-tool)
  (skip-unless (flycheck-ert-check-gpg))
  (let* ((filename (flycheck-ert-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-ert-with-resource-buffer filename
      (should (flycheck-encrypted-buffer-p)))))

(ert-deftest flycheck-autoloads-file-p/ephemeral-buffer ()
  :tags '(utility)
  (flycheck-ert-with-temp-buffer
    (should-not (flycheck-autoloads-file-p))))

(ert-deftest flycheck-autoloads-file-p/autoloads-without-backing-file ()
  :tags '(utility)
  (flycheck-ert-with-temp-buffer
    (rename-buffer "foo-autoloads.el")
    (should (flycheck-autoloads-file-p))))

(ert-deftest flycheck-autoloads-file-p/autoloads-with-backing-file ()
  :tags '(utility)
  (flycheck-ert-with-file-buffer (locate-library "dash-autoloads")
    (should (flycheck-autoloads-file-p))))

(ert-deftest flycheck-autoloads-file-p/a-plain-file ()
  :tags '(utility)
  (flycheck-ert-with-file-buffer
      (expand-file-name "Cask" flycheck-test-source-directory)
    (should-not (flycheck-autoloads-file-p))))

(ert-deftest flycheck-in-user-emacs-directory-p/no-child-of-user-emacs-directory ()
  :tags '(utility)
  (should-not (flycheck-in-user-emacs-directory-p
               (flycheck-ert-resource-filename "checkers/emacs-lisp.el"))))

(ert-deftest flycheck-in-user-emacs-directory-p/direct-child-of-user-emacs-directory ()
  :tags '(utility)
  (let ((user-emacs-directory flycheck-test-directory))
    (should (flycheck-in-user-emacs-directory-p
             (expand-file-name "flycheck-test.el" flycheck-test-directory)))))

(ert-deftest flycheck-in-user-emacs-directory-p/indirect-child-of-user-emacs-directory ()
  :tags '(utility)
  (let ((user-emacs-directory flycheck-test-directory))
    (should (flycheck-in-user-emacs-directory-p
             (flycheck-ert-resource-filename "checkers/emacs-lisp.el")))))

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
        (file-name (flycheck-ert-resource-filename "checkers/emacs-lisp.el")))
    (should (string= (flycheck-ert-resource-filename "checkers/")
                     (flycheck-module-root-directory nil file-name)))))

(ert-deftest flycheck-module-root-directory/module-name-as-string ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-ert-resource-filename "checkers/emacs-lisp.el")))
    (should (string= flycheck-test-resources-directory
                     (flycheck-module-root-directory "checkers.emacs-lisp"
                                                     file-name)))))

(ert-deftest flycheck-module-root-directory/module-name-as-list ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-ert-resource-filename "checkers/emacs-lisp.el")))
    (should (string= flycheck-test-resources-directory
                     (flycheck-module-root-directory '("checkers" "emacs-lisp")
                                                     file-name)))))

(ert-deftest flycheck-module-root-directory/mismatching-module-name ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-ert-resource-filename "checkers/emacs-lisp.el")))
    (should (string= (flycheck-ert-resource-filename "checkers/")
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

(ert-deftest flycheck-add-mode/no-valid-checker ()
  :tags '(extending)
  (let ((err-data (should-error (flycheck-add-mode 'foo 'emacs-lisp-mode))))
    (should (string= (cadr err-data) "foo is not a valid syntax checker"))))

(ert-deftest flycheck-add-mode/no-valid-mode ()
  :tags '(extending)
  (let ((err-data (should-error (flycheck-add-mode 'python-pylint "foo"))))
    (should (string= (cadr err-data) "foo is not a symbol"))))

(ert-deftest flycheck-add-mode ()
  :tags '(extending)
  (let ((modes (flycheck-checker-modes 'python-pylint)))
    (flycheck-add-mode 'python-pylint 'emacs-lisp-mode)
    (unwind-protect
        (progn
          (should (equal (flycheck-checker-modes 'python-pylint)
                         (cons 'emacs-lisp-mode modes)))
          (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
            (should (flycheck-may-use-checker 'python-pylint))))
      (put 'python-pylint 'flycheck-modes modes)
      (should (equal (flycheck-checker-modes 'python-pylint) modes))
      (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
        (should-not (flycheck-may-use-checker 'python-pylint))))))


;;; Checker API
(ert-deftest flycheck-valid-checker-p/not-a-symbol ()
  :tags '(checker-api)
  (should-not (flycheck-valid-checker-p "foo")))

(ert-deftest flycheck-valid-checker-p/no-checker-version ()
  :tags '(checker-api)
  (should-not (get 'foo 'flycheck-generic-checker-version))
  (should-not (flycheck-valid-checker-p 'foo)))

(ert-deftest flycheck-valid-checker-p/checker-version-too-low ()
  :tags '(checker-api)
  (cl-letf* ((version (- flycheck-generic-checker-version 1))
             ((get 'foo 'flycheck-generic-checker-version) version))
    (should (= (get 'foo 'flycheck-generic-checker-version) version))
    (should-not (flycheck-valid-checker-p 'foo)))
  (should-not (get 'foo 'flycheck-generic-checker-version)))

(ert-deftest flycheck-valid-checker-p/checker-version-too-high ()
  :tags '(checker-api)
  (cl-letf* ((version (+ flycheck-generic-checker-version 1))
             ((get 'foo 'flycheck-generic-checker-version) version))
    (should (= (get 'foo 'flycheck-generic-checker-version) version))
    (should-not (flycheck-valid-checker-p 'foo)))
  (should-not (get 'foo 'flycheck-generic-checker-version)))

(ert-deftest flycheck-valid-checker-p/checker-version-ok ()
  :tags '(checker-api)
  (cl-letf* ((version flycheck-generic-checker-version)
             ((get 'foo 'flycheck-generic-checker-version) version))
    (should (= (get 'foo 'flycheck-generic-checker-version) version))
    (should (flycheck-valid-checker-p 'foo)))
  (should-not (get 'foo 'flycheck-generic-checker-version)))

(ert-deftest flycheck-disabled-checker-p/enabled-checker ()
  :tags '(checker-api)
  (let ((flycheck-checkers '(emacs-lisp)))
    (should-not (flycheck-disabled-checker-p 'emacs-lisp))))

(ert-deftest flycheck-disabled-checker-p/disabled-checker ()
  :tags '(checker-api)
  (let ((flycheck-disabled-checkers '(emacs-lisp)))
    (should (flycheck-disabled-checker-p 'emacs-lisp))))

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
  (flycheck-ert-with-resource-buffer "substitute-dummy"
    (unwind-protect
        (progn
          (should (equal (flycheck-substitute-argument 'source-original 'emacs-lisp)
                         (list (buffer-file-name))))

          (let ((filename (flycheck-substitute-argument 'source-inplace 'emacs-lisp)))
            (should (equal filename (list (flycheck-ert-resource-filename
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
         (config-file (flycheck-ert-resource-filename "substitute-dummy"))
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

(ert-deftest flycheck-may-use-checker/invalid-checker ()
  :tags '(checker-api)
  (should-not (flycheck-valid-checker-p 'foo))
  (should-not (flycheck-may-use-checker 'foo)))

(ert-deftest flycheck-may-use-checker/disabled-checker ()
  :tags '(checker-api)
  (flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-may-use-checker 'emacs-lisp)
    (let ((flycheck-disabled-checkers '(emacs-lisp)))
      (should-not (flycheck-may-use-checker 'emacs-lisp)))))


;;; Configuration file functions

(ert-deftest flycheck-locate-config-file-absolute-path/just-a-base-name ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (cd flycheck-test-directory)
    (should-not (flycheck-locate-config-file-absolute-path "flycheck-test.el"
                                                           'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-absolute-path/with-path ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (cd flycheck-test-directory)
    (should (equal (flycheck-locate-config-file-absolute-path "../Makefile"
                                                              'emacs-lisp)
                   (expand-file-name "../Makefile" flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/not-existing-file ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                             flycheck-test-directory))
    (should-not (flycheck-locate-config-file-ancestor-directories
                 "foo" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/file-on-same-level ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                             flycheck-test-directory))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "run.el" 'emacs-lisp)
                   (expand-file-name "run.el" flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file-ancestor-directories/file-on-parent-level ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                             flycheck-test-directory))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "Makefile" 'emacs-lisp)
                   (expand-file-name "../Makefile"
                                     flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file/not-existing-file ()
  :tags '(configuration)
  (flycheck-ert-with-env (list (cons "HOME" flycheck-test-directory))
    (should-not (flycheck-locate-config-file-home "foo" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file/existing-file-in-parent-directory ()
  :tags '(configuration)
  (flycheck-ert-with-env (list (cons "HOME" flycheck-test-directory))
    (should-not (flycheck-locate-config-file-home "Makefile" 'emacs-lisp))))

(ert-deftest flycheck-locate-config-file/existing-file-in-home-directory ()
  :tags '(configuration)
  (flycheck-ert-with-env (list (cons "HOME" flycheck-test-directory))
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
  (flycheck-ert-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-mode)
    (let* ((flycheck-checker 'sh-bash)
           (err (should-error (flycheck-buffer)
                              :type flycheck-ert-user-error-type)))
      (should (eq flycheck-checker 'sh-bash))
      (should (string= (cadr err)
                       "Selected syntax checker sh-bash cannot be used"))
      (should (string= flycheck-last-status-change 'errored)))))

(ert-deftest flycheck-checker/usable-checker-is-used ()
  :tags '(selection language-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checker 'emacs-lisp-checkdoc))
      (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
      (flycheck-ert-buffer-sync)
      (flycheck-ert-should-errors
       '(12 nil warning "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)))))

(ert-deftest flycheck-checker/disabled-checker-is-not-used ()
  :tags '(selection language-emacs-lisp)
  (flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (let ((flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))
      (should-not (flycheck-get-checker-for-buffer))
      (let* ((flycheck-checker 'emacs-lisp)
             (err (should-error (flycheck-buffer)
                                :type flycheck-ert-user-error-type)))
        (should (eq flycheck-checker 'emacs-lisp))
        (should (string= (cadr err)
                         "Selected syntax checker emacs-lisp cannot be used"))
        (should (string= flycheck-last-status-change 'errored))))))

(ert-deftest flycheck-checker/unregistered-checker-is-used ()
  :tags '(selection language-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checkers (remq 'emacs-lisp-checkdoc flycheck-checkers)))
      (should-not (flycheck-registered-checker-p 'emacs-lisp-checkdoc))
      (let ((flycheck-checker 'emacs-lisp-checkdoc))
        (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
        (flycheck-ert-buffer-sync)
        (flycheck-ert-should-errors
         '(12 nil warning "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))))))

(ert-deftest flycheck-select-checker/selecting-sets-the-syntax-checker ()
  :tags '(selection)
  (flycheck-ert-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (should (eq flycheck-checker 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-select-checker/unselecting-unsets-the-syntax-checker ()
  :tags '(selection)
  (flycheck-ert-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (flycheck-select-checker nil)
    (should-not flycheck-checker)))

(ert-deftest flycheck-select-checker/selecting-runs-a-syntax-check ()
  :tags '(selection external-tool language-python
                    checker-python-pylint checker-python-flake8)
  (skip-unless (executable-find (flycheck-checker-executable 'python-pylint)))
  (skip-unless (executable-find (flycheck-checker-executable 'python-flake8)))
  (flycheck-ert-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    ;; By default, Flake8 is preferred, so we get errors from Flake8
    (flycheck-ert-buffer-sync)
    (flycheck-ert-should-errors
     '(5 1 warning "'antigravit' imported but unused" :id "F401"
         :checker python-flake8)
     '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
         :checker python-flake8)
     '(9 9 info "function name should be lowercase"
         :checker python-flake8 :id "N802")
     '(12 29 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(12 31 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(22 1 error "undefined name 'antigravity'" :id "F821"
          :checker python-flake8))
    ;; Selecting Pylint should give us its errors
    (flycheck-select-checker 'python-pylint)
    (flycheck-ert-wait-for-syntax-checker)
    (flycheck-ert-should-errors
     '(1 1 info "Missing module docstring" :id "C0111" :checker python-pylint)
     '(4 1 error "Unable to import 'spam'" :id "F0401" :checker python-pylint)
     '(5 1 error "No name 'antigravit' in module 'python'" :id "E0611"
         :checker python-pylint)
     '(5 1 warning "Unused import antigravit" :id "W0611" :checker python-pylint)
     '(7 1 info "Missing class docstring" :id "C0111" :checker python-pylint)
     '(9 5 info "Invalid method name \"withEggs\"" :id "C0103"
         :checker python-pylint)
     '(9 5 info "Missing method docstring" :id "C0111" :checker python-pylint)
     '(9 5 warning "Method could be a function" :id "R0201" :checker python-pylint)
     '(10 16 warning "Used builtin function 'map'" :id "W0141"
          :checker python-pylint)
     '(12 1 info "No space allowed around keyword argument assignment"
          :id "C0326" :checker python-pylint)
     '(12 5 info "Missing method docstring" :id "C0111" :checker python-pylint)
     '(12 5 warning "Method could be a function" :id "R0201"
          :checker python-pylint)
     '(14 16 error "Module 'sys' has no 'python_version' member" :id "E1101"
          :checker python-pylint)
     '(15 1 info "Unnecessary parens after u'print' keyword" :id "C0325"
          :checker python-pylint)
     '(17 1 info "Unnecessary parens after u'print' keyword" :id "C0325"
          :checker python-pylint)
     '(22 1 error "Undefined variable 'antigravity'" :id "E0602"
          :checker python-pylint))))

(ert-deftest flycheck-select-checker/unselecting-a-checker-goes-back-to-automatic-selection ()
  :tags '(selection external-tool language-python
                    checker-python-pylint checker-python-flake8)
  (skip-unless (executable-find (flycheck-checker-executable 'python-pylint)))
  (skip-unless (executable-find (flycheck-checker-executable 'python-flake8)))
  (flycheck-ert-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    (flycheck-select-checker 'python-pylint)
    (should (eq flycheck-checker 'python-pylint))
    (flycheck-ert-wait-for-syntax-checker)
    (flycheck-ert-should-errors
     '(1 1 info "Missing module docstring" :id "C0111" :checker python-pylint)
     '(4 1 error "Unable to import 'spam'" :id "F0401" :checker python-pylint)
     '(5 1 error "No name 'antigravit' in module 'python'" :id "E0611"
         :checker python-pylint)
     '(5 1 warning "Unused import antigravit" :id "W0611"
         :checker python-pylint)
     '(7 1 info "Missing class docstring" :id "C0111" :checker python-pylint)
     '(9 5 info "Invalid method name \"withEggs\"" :id "C0103"
         :checker python-pylint)
     '(9 5 info "Missing method docstring" :id "C0111" :checker python-pylint)
     '(9 5 warning "Method could be a function" :id "R0201" :checker python-pylint)
     '(10 16 warning "Used builtin function 'map'" :id "W0141"
          :checker python-pylint)
     '(12 1 info "No space allowed around keyword argument assignment"
          :id "C0326" :checker python-pylint)
     '(12 5 info "Missing method docstring" :id "C0111" :checker python-pylint)
     '(12 5 warning "Method could be a function" :id "R0201"
          :checker python-pylint)
     '(14 16 error "Module 'sys' has no 'python_version' member" :id "E1101"
          :checker python-pylint)
     '(15 1 info "Unnecessary parens after u'print' keyword" :id "C0325"
          :checker python-pylint)
     '(17 1 info "Unnecessary parens after u'print' keyword" :id "C0325"
          :checker python-pylint)
     '(22 1 error "Undefined variable 'antigravity'" :id "E0602"
          :checker python-pylint))
    (flycheck-select-checker nil)
    (should-not flycheck-checker)
    (flycheck-ert-wait-for-syntax-checker)
    (flycheck-ert-should-errors
     '(5 1 warning "'antigravit' imported but unused" :id "F401"
         :checker python-flake8)
     '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
         :checker python-flake8)
     '(9 9 info "function name should be lowercase"
         :checker python-flake8 :id "N802")
     '(12 29 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(12 31 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(22 1 error "undefined name 'antigravity'" :id "F821"
          :checker python-flake8))))

(ert-deftest flycheck/selects-checker-automatically/first-enabled-checker ()
  :tags '(selection external-tool language-python checker-python-flake8)
  (skip-unless (executable-find (flycheck-checker-executable 'python-pylint)))
  (skip-unless (executable-find (flycheck-checker-executable 'python-flake8)))
  (flycheck-ert-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    (flycheck-ert-buffer-sync)
    (should-not flycheck-checker)
    (should (eq flycheck-last-checker 'python-flake8))
    (flycheck-ert-should-errors
     '(5 1 warning "'antigravit' imported but unused" :id "F401"
         :checker python-flake8)
     '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
         :checker python-flake8)
     '(9 9 info "function name should be lowercase"
         :checker python-flake8 :id "N802")
     '(12 29 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(12 31 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(22 1 error "undefined name 'antigravity'" :id "F821"
          :checker python-flake8))))

(ert-deftest flycheck/selects-checker-automatically/no-disabled-checker ()
  :tags '(selection language-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
    (let ((flycheck-disabled-checkers '(emacs-lisp)))
      (flycheck-ert-buffer-sync)
      (should-not flycheck-checker)
      (should (eq flycheck-last-checker 'emacs-lisp-checkdoc))
      (flycheck-ert-should-errors
       '(12 nil warning "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)))))


;;; Documentation

(ert-deftest flycheck-describe-checker/pops-up-help-buffer ()
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-ert-with-help-buffer
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
    (flycheck-ert-with-help-buffer
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
    (flycheck-ert-with-help-buffer
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
    (flycheck-ert-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "The\\s-+executable\\s-+can\\s-+be\\s-+overridden\\s-+with\\s-+`\\(.+?\\)'\\.")
        (let ((var (flycheck-checker-executable-variable checker)))
          (should (string= (match-string 1) (symbol-name var))))))))

(ert-deftest flycheck-describe-checker/help-shows-config-file-var ()
  :tags '(documentation)
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-ert-with-help-buffer
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
    (flycheck-ert-with-help-buffer
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
    (flycheck-ert-with-help-buffer
      (shut-up (flycheck-describe-checker checker))
      (with-current-buffer (help-buffer)
        (should (string-match-p
                 (regexp-quote (flycheck-checker-documentation checker))
                 (buffer-substring (point-min) (point-max))))))))

(ert-deftest flycheck--manual/all-checkers-are-documented ()
  :tags '(documentation)
  (flycheck-ert-with-file-buffer
      (expand-file-name "doc/guide/languages.rst"
                        flycheck-test-source-directory)
    (let (documented-checkers)
      (while (re-search-forward (rx line-start ".. flyc-checker:: "
                                    (group (1+ not-newline))
                                    "\n   :auto:" line-end)
                                nil 'noerror)
        (push (intern (match-string 1)) documented-checkers))
      (should (equal flycheck-checkers (nreverse documented-checkers))))))

(ert-deftest flycheck--manual/all-options-are-documented ()
  :tags '(documentation)
  (flycheck-ert-with-file-buffer
      (expand-file-name "doc/guide/languages.rst"
                        flycheck-test-source-directory)
    (while (re-search-forward (rx line-start ".. flyc-checker::" (1+ space)
                                  (group (1+ not-newline))
                                  line-end)
                              nil 'noerror)
      (let* ((checker (intern (match-string 1)))
             (bound (save-excursion
                      (search-forward ".. flyc-checker::" nil 'noerror)))
             documented-vars)
        (when (search-forward ".. rubric:: Options" bound 'noerror)
          (while (and (re-search-forward (rx line-start "   .. "
                                             (group (1+ (not (any ":"))))
                                             "::" (1+ space)
                                             (group (1+ not-newline))
                                             (group (optional "\n      :auto:"))
                                             line-end)
                                         bound 'noerror)
                      (equal (match-string 1) "option")
                      (not (string-empty-p (match-string 3))))
            (push (intern (match-string 2)) documented-vars)))
        (should (equal (-sort #'string< (flycheck-checker-option-vars checker))
                       (nreverse documented-vars)))))))

(ert-deftest flycheck--manual/all-config-file-vars-are-documented ()
  :tags '(documentation)
  (flycheck-ert-with-file-buffer
      (expand-file-name "doc/guide/languages.rst" flycheck-test-source-directory)
    (while (re-search-forward (rx line-start ".. flyc-checker::" (1+ space)
                                  (group (1+ not-newline)) line-end)
                              nil 'noerror)
      (let* ((checker (intern (match-string 1)))
             (bound (save-excursion
                      (search-forward ".. flyc-checker::" nil 'noerror)))
             documented-var)
        (when (search-forward ".. rubric:: Configuration file" bound 'noerror)
          (re-search-forward (rx "   .. option:: "
                                 (group (1+ not-newline))
                                 "\n      :auto:"
                                 line-end)
                             bound 'noerror)
          (setq documented-var (intern (match-string 1))))
        (should (equal (flycheck-checker-config-file-var checker)
                       documented-var))))))


;;; Checker error API

(ert-deftest flycheck-error-line-region ()
  :tags '(error-api)
  (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-temp-buffer
    (insert "    Hello\n   World\n")
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 1)) 1))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 4)) 4))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 nil)) 5))
    (should (= (flycheck-error-pos (flycheck-error-new-at 2 nil)) 14))
    (should (= (flycheck-error-pos (flycheck-error-new-at 3 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 nil)) 19))))

(ert-deftest flycheck-error-format-message-and-id/no-id ()
  :tags '(error-api)
  (should (string= (flycheck-error-format-message-and-id
                    (flycheck-error-new-at 3 5 'warning "Hello world"))
                   "Hello world")))

(ert-deftest flycheck-error-format-message-and-id/with-id ()
  :tags '(error-api)
  (should (string= (flycheck-error-format-message-and-id
                    (flycheck-error-new-at 3 5 'warning "Hello world"
                                           :id "Foo"))
                   "Hello world [Foo]")))

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

(ert-deftest flycheck-error-format/with-id ()
  :tags '(error-api)
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 15 'error "A message"
                                           :id "E001" :checker 'foo))
                   "14:15:error: A message [E001] (foo)")))

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

(ert-deftest flycheck-assert-error-list-p/all-flycheck-errors ()
  :tags '(error-api)
  (let ((errors (list (flycheck-error-new-at 8 10 nil 'info)
                      (flycheck-error-new-at 9 11 nil 'info))))
    (should (eq (flycheck-assert-error-list-p errors) errors))))

(ert-deftest flycheck-assert-error-list-p/no-list ()
  :tags '(error-api)
  (let ((data (should-error (flycheck-assert-error-list-p 'foo)
                            :type 'wrong-type-argument)))
    (should (equal (error-message-string data)
                   "Wrong type argument: listp, foo"))))

(ert-deftest flycheck-assert-error-list-p/nil-in-list ()
  :tags '(error-api)
  (let* ((errors (list (flycheck-error-new-at 8 10 nil 'info)
                       nil))
         (data (should-error (flycheck-assert-error-list-p errors)
                             :type 'wrong-type-argument)))
    (should (equal (error-message-string data)
                   "Wrong type argument: flycheck-error-p, nil"))))

(ert-deftest flycheck-assert-error-list-p/wrong-type-in-list ()
  :tags '(error-api)
  (let* ((errors (list (flycheck-error-new-at 8 10 nil 'info)
                       "foo"))
         (data (should-error (flycheck-assert-error-list-p errors)
                             :type 'wrong-type-argument)))
    (should (equal (error-message-string data)
                   "Wrong type argument: flycheck-error-p, \"foo\""))))



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
    <error line=\"3\" column=\"21\" severity=\"error\" message=\"Missing semicolon.\" source=\"Foo3\" />
    <error line=\"3\" severity=\"warning\" message=\"Implied global &apos;alert&apos;\" source=\"Foo4\" />
  </file>
  <file name=\"test-javascript/missing-quote.js\">
    <error line=\"undefined\" column=\"undefined\" severity=\"error\" message=\"Cannot read property &apos;id&apos; of undefined\" source=\"Foo1\" />
  </file>
</checkstyle>"
  "Example Checkstyle output from jshint.")

(defconst flycheck-checkstyle-expected-errors
  (list
   (flycheck-error-new
    :filename "test-javascript/missing-semicolon.js"
    :checker 'checker
    :buffer 'buffer
    :line 3
    :column 21
    :level 'error
    :message "Missing semicolon."
    :id "Foo3")
   (flycheck-error-new
    :filename "test-javascript/missing-semicolon.js"
    :checker 'checker
    :buffer 'buffer
    :line 3
    :column nil
    :level 'warning
    :message "Implied global 'alert'"
    :id "Foo4")
   (flycheck-error-new
    :filename "test-javascript/missing-quote.js"
    :checker 'checker
    :buffer 'buffer
    :line nil
    :column nil
    :level 'error
    :message "Cannot read property 'id' of undefined"
    :id "Foo1"))
  "Errors to be parsed from `flycheck-checkstyle-xml'.")

(ert-deftest flycheck-parse-checkstyle/with-builtin-xml ()
  :tags '(error-parsing checkstyle-xml)
  (let ((flycheck-xml-parser 'flycheck-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml
                                              'checker 'buffer)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle/with-libxml2 ()
  :tags '(error-parsing checkstyle-xml)
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((flycheck-xml-parser 'libxml-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml
                                              'checker 'buffer)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle/automatic-parser ()
  :tags '(error-parsing checkstyle-xml)
  (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml
                                            'checker 'buffer)
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
    :buffer 'buffer
    :checker 'checker
    :line 4
    :column nil
    :level 'error
    :message "Null pointer dereference"
    :id "nullPointer")
   (flycheck-error-new
    :filename "bar"
    :buffer 'buffer
    :checker 'checker
    :line 6
    :column nil
    :level 'error
    :message "Null pointer dereference"
    :id "nullPointer")
   (flycheck-error-new
    :filename "eggs"
    :buffer 'buffer
    :checker 'checker
    :line 2
    :column nil
    :level 'warning
    :message "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0."
    :id "comparisonOfBoolWithInt")))

(ert-deftest flycheck-parse-cppcheck ()
  :tags '(error-parsing cppcheck-xml)
  (should (equal (flycheck-parse-cppcheck flycheck-cppcheck-xml
                                          'checker 'buffer)
                 flycheck-cppcheck-expected-errors)))

(ert-deftest flycheck-parse-cppcheck/empty-errors-list-with-automatic-parser ()
  :tags '(error-parsing cppcheck-xml)
  (should-not (flycheck-parse-cppcheck "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.60.1\"/>
  <errors>
  </errors>
</results>" nil nil)))

(ert-deftest flycheck-parse-cppcheck/empty-errors-list-with-builtin-parser ()
  :tags '(error-parsing cppcheck-xml)
  (let ((flycheck-xml-parser #'flycheck-parse-xml-region))
    (should-not (flycheck-parse-cppcheck "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.60.1\"/>
  <errors>
  </errors>
</results>" nil nil))))

(defconst flycheck-phpmd-xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<pmd version=\"1.5.0\" timestamp=\"2014-12-02T18:13:44+00:00\">
  <file name=\"foo.php\">
    <violation beginline=\"21\" endline=\"21\" rule=\"UnusedPrivateField\" ruleset=\"Unused Code Rules\" externalInfoUrl=\"http://phpmd.org/rules/unusedcode.html#unusedprivatefield\" priority=\"3\">
      Avoid unused private fields such as '$FOO'.
    </violation>
    <violation beginline=\"24\" endline=\"27\" rule=\"UnusedPrivateMethod\" ruleset=\"Unused Code Rules\" package=\"Flycheck\" externalInfoUrl=\"http://phpmd.org/rules/unusedcode.html#unusedprivatemethod\" class=\"A\" method=\"bar\" priority=\"3\">
      Avoid unused private methods such as 'bar'.
    </violation>
    <violation beginline=\"24\" endline=\"24\" rule=\"UnusedFormalParameter\" ruleset=\"Unused Code Rules\" externalInfoUrl=\"http://phpmd.org/rules/unusedcode.html#unusedformalparameter\" priority=\"3\">
      Avoid unused parameters such as '$baz'.
    </violation>
  </file>
</pmd>"
  "Example phpmd output.")

(ert-deftest flycheck-parse-phpmd ()
  :tags '(error-parsing phpmd-xml)
  (should (equal (flycheck-parse-phpmd flycheck-phpmd-xml 'foo 'buffer)
                 (list
                  (flycheck-error-new-at 21 nil 'warning
                                         "Avoid unused private fields such as '$FOO'."
                                         :id "UnusedPrivateField"
                                         :checker 'foo
                                         :buffer 'buffer
                                         :filename "foo.php")
                  (flycheck-error-new-at 24 nil 'warning
                                         "Avoid unused private methods such as 'bar'."
                                         :id "UnusedPrivateMethod"
                                         :checker 'foo
                                         :buffer 'buffer
                                         :filename "foo.php")
                  (flycheck-error-new-at 24 nil 'warning
                                         "Avoid unused parameters such as '$baz'."
                                         :id "UnusedFormalParameter"
                                         :checker 'foo
                                         :buffer 'buffer
                                         :filename "foo.php")))))


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

(ert-deftest flycheck-increment-error-columns/ignores-nil ()
  :tags '(error-filtering)
  (let ((errors (list (flycheck-error-new-at 4 nil nil nil))))
    (should (equal (flycheck-increment-error-columns errors)
                   (list (flycheck-error-new-at 4 nil nil nil))))))

(ert-deftest flycheck-increment-error-columns/default-offset ()
  :tags '(error-filtering)
  (let ((errors (list (flycheck-error-new-at 4 6 nil nil)
                      (flycheck-error-new-at 7 9 nil nil))))
    (should (equal (flycheck-increment-error-columns errors)
                   (list (flycheck-error-new-at 4 7 nil nil)
                         (flycheck-error-new-at 7 10 nil nil))))))

(ert-deftest flycheck-increment-error-columns/custom-offset ()
  :tags '(error-filtering)
  (let ((errors (list (flycheck-error-new-at 4 6 nil nil)
                      (flycheck-error-new-at 7 9 nil nil))))
    (should (equal (flycheck-increment-error-columns errors 10)
                   (list (flycheck-error-new-at 4 16 nil nil)
                         (flycheck-error-new-at 7 19 nil nil))))))

(ert-deftest flycheck-collapse-error-message-whitespace ()
  :tags '(error-filtering)
  (let ((err (flycheck-error-new-at 1 1 'error
                                    "spam  \nwith\t   eggs")))
    (should (equal (flycheck-collapse-error-message-whitespace (list err))
                   (list (flycheck-error-new-at 1 1 'error
                                                "spam with eggs"))))))

(ert-deftest flycheck-dequalify-error-ids ()
  :tags '(error-filtering)
  (let ((errors (list (flycheck-error-new-at 1 2 nil nil :id "foo.bar")
                      (flycheck-error-new-at 1 2 nil nil :id "Spam.With.Eggs")
                      (flycheck-error-new-at 1 2 nil nil :id "foobar")
                      (flycheck-error-new-at 1 2 nil nil :id nil))))
    (should (equal (flycheck-dequalify-error-ids errors)
                   (list (flycheck-error-new-at 1 2 nil nil :id "bar")
                         (flycheck-error-new-at 1 2 nil nil :id "Eggs")
                         (flycheck-error-new-at 1 2 nil nil :id "foobar")
                         (flycheck-error-new-at 1 2))))))

(ert-deftest flycheck-remove-error-ids ()
  :tags '(error-filtering)
  (let ((errors (list (flycheck-error-new-at 1 2 nil nil :id "Foo.Bar")
                      (flycheck-error-new-at 1 2 nil nil :id "FooBar")
                      (flycheck-error-new-at 1 2 nil nil :id nil))))
    (should (equal (flycheck-remove-error-ids errors)
                   (list (flycheck-error-new-at 1 2)
                         (flycheck-error-new-at 1 2)
                         (flycheck-error-new-at 1 2))))))


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
  (flycheck-ert-with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'info))))
      (should (eq (overlay-get overlay 'category) 'flycheck-info-overlay)))))

(ert-deftest flycheck-add-overlay/warning-category ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'warning))))
      (should (eq (overlay-get overlay 'category) 'flycheck-warning-overlay)))))

(ert-deftest flycheck-add-overlay/error-category ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'error))))
      (should (eq (overlay-get overlay 'category) 'flycheck-error-overlay)))))

(ert-deftest flycheck-add-overlay/has-help-echo ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (let ((overlay (flycheck-add-overlay
                    (flycheck-error-new-at 1 1 'info "A bar message"))))
      (should (string= (overlay-get overlay 'help-echo) "A bar message")))))

(ert-deftest flycheck-add-overlay/has-flycheck-overlay-property ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'error))
           (overlay (flycheck-add-overlay err)))
      (should (overlay-get overlay 'flycheck-overlay)))))

(ert-deftest flycheck-add-overlay/has-flycheck-error-property ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'warning))
           (overlay (flycheck-add-overlay err)))
      (should (eq (overlay-get overlay 'flycheck-error) err)))))

(ert-deftest flycheck-add-overlay/has-no-fringe-icon-with-disabled-indication ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World")
    (let ((flycheck-indication-mode nil))
      (dolist (level '(warning info error))
        (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 level))))
          (should-not (overlay-get overlay 'before-string)))))))

(ert-deftest flycheck-add-overlay/has-info-fringe-icon ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'info)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-info))
      (should (eq bitmap 'empty-line)))))

(ert-deftest flycheck-add-overlay/has-warning-fringe-icon ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'warning)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-warning))
      (should (eq bitmap 'question-mark)))))

(ert-deftest flycheck-add-overlay/has-error-fringe-icon ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'error)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-error))
      (should (eq bitmap 'exclamation-mark)))))

(ert-deftest flycheck-add-overlay/has-left-fringe-icon ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
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
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World")
    ;; Test the various indication modes
    (let ((flycheck-indication-mode 'right-fringe))
      (pcase-let* ((overlay (flycheck-add-overlay
                             (flycheck-error-new-at 1 1 'error)))
                   (before-string (overlay-get overlay 'before-string))
                   (`(,side _ _) (get-text-property 0 'display before-string)))
        (should (eq side 'right-fringe))))))

(ert-deftest flycheck-add-overlay/right-position-in-narrowed-buffer ()
  :tags '(overlay language-emacs-lisp checker-emacs-lisp)
  "Test that all overlays are added at the right positions with narrowing in place."
  (flycheck-ert-with-resource-buffer "narrowing.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    ;; Narrow to the function and check the buffer
    (re-search-forward "(defun .*")
    (forward-line 1)
    (narrow-to-defun)
    (should (buffer-narrowed-p))
    (flycheck-ert-buffer-sync)
    ;; We should have two errors highlighted between point min and max now
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 2))
    ;; Remove restrictions and test that all errors are reported
    (widen)
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 4))
    (flycheck-ert-should-errors
     '(9 1 warning "`message' called with 0 args to fill 1 format field(s)"
         :checker emacs-lisp)
     '(11 8 warning "`message' called with 0 args to fill 1 format field(s)"
          :checker emacs-lisp)
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(15 1 warning "`message' called with 0 args to fill 1 format field(s)"
          :checker emacs-lisp))))


;;; Error navigation

(defmacro flycheck-test-with-nav-buffer (minimum-level &rest body)
  "Eval BODY in a temporary buffer for navigation.

Set `flycheck-navigation-minimum-level' to MINIMUM-LEVEL while
evaluating BODY."
  (declare (indent 1))
  `(flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
     (emacs-lisp-mode)
     (flycheck-mode)
     (when ,minimum-level
       (let ((flycheck-navigation-minimum-level ,minimum-level))
         (flycheck-ert-buffer-sync)
         (goto-char (point-min))
         ,@body))))

(ert-deftest flycheck-next-error/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/goes-to-next-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (flycheck-next-error)
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/errors-beyond-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (flycheck-next-error 2)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (flycheck-next-error -2)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/reset-navigates-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (flycheck-next-error 1 'reset)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/does-not-cross-narrowing ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (re-search-forward "(defun .*")
    (narrow-to-defun)
    (goto-char (point-min))
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 1))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (let ((err (should-error (flycheck-previous-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/goes-to-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (flycheck-previous-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-previous-error/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (flycheck-previous-error 2)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-previous-error/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (flycheck-previous-error -2)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-previous-error/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-first-error/stays-at-first-error-if-called-again ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (flycheck-first-error)
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-first-error/goes-to-second-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (goto-char (point-max))
    (flycheck-first-error 2)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/over-errors/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/over-errors/goes-to-next-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (flycheck-next-error)
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/errors-beyond-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-next-error 2)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error -2)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/reset-navigates-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (flycheck-next-error 1 'reset)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/over-errors/does-not-cross-narrowing ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (re-search-forward "(defun .*")
    (narrow-to-defun)
    (goto-char (point-min))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-errors/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-previous-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-errors/goes-to-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (flycheck-previous-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-previous-error/over-errors/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error -2)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-errors/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-previous-error -2)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-errors/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error/over-errors/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-first-error/over-errors/stays-at-first-error-if-called-again ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (flycheck-first-error)
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 2)))) ; second occurrence is an 'error

(ert-deftest flycheck-first-error/over-errors/goes-to-second-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-first-error 2)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-warnings/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/over-warnings/goes-to-next-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (flycheck-next-error)
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/over-warnings/errors-beyond-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-warnings/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-warnings/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-warnings/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (flycheck-next-error -2)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/over-warnings/reset-navigates-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (flycheck-next-error 1 'reset)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/over-warnings/does-not-cross-narrowing ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (re-search-forward "(defun .*")
    (narrow-to-defun)
    (goto-char (point-min))
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 1))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-warnings/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (let ((err (should-error (flycheck-previous-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-warnings/goes-to-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (flycheck-previous-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-previous-error/over-warnings/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (flycheck-previous-error 2)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-previous-error/over-warnings/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (flycheck-previous-error -2)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-previous-error/over-warnings/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error/over-warnings/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-first-error/over-warnings/stays-at-first-error-if-called-again ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (flycheck-first-error)
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-first-error/over-warnings/goes-to-second-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (goto-char (point-max))
    (flycheck-first-error 2)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/over-informational/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/over-informational/goes-to-next-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (flycheck-next-error)
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/over-informational/errors-beyond-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-informational/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-informational/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (flycheck-next-error 2)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-next-error/over-informational/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (flycheck-next-error -2)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/over-informational/reset-navigates-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (flycheck-next-error 1 'reset)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-next-error/over-informational/does-not-cross-narrowing ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (re-search-forward "(defun .*")
    (narrow-to-defun)
    (goto-char (point-min))
    (flycheck-next-error)
    (should (flycheck-ert-at-nth-error 1))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-informational/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (let ((err (should-error (flycheck-previous-error)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-informational/goes-to-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (flycheck-previous-error)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-previous-error/over-informational/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (flycheck-previous-error 2)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-previous-error/over-informational/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (flycheck-previous-error -2)
    (should (flycheck-ert-at-nth-error 2))))

(ert-deftest flycheck-previous-error/over-informational/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error/over-informational/goes-to-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-first-error/over-informational/stays-at-first-error-if-called-again ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (flycheck-first-error)
    (flycheck-first-error)
    (should (flycheck-ert-at-nth-error 1))))

(ert-deftest flycheck-first-error/over-informational/goes-to-second-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (goto-char (point-max))
    (flycheck-first-error 2)
    (should (flycheck-ert-at-nth-error 2))))


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
                    ("ID" 6 t)
                    ("Message" 0 t)
                    (" (Checker)" 8 t)]))
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
    (should (string= header-line-format " Line Col Level ID Message  (Checker) "))))

(ert-deftest flycheck-error-list-source-buffer/is-permanently-local ()
  :tags '(error-list)
  (should (get 'flycheck-error-list-source-buffer 'permanent-local)))

(ert-deftest flycheck-error-list-make-entry/line-and-column ()
  :tags '(error-list)
  (let* ((error (flycheck-error-new-at 10 12 'warning "A foo warning"
                                       :checker 'emacs-lisp-checkdoc
                                       :id "W1"))
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
                   (list "W1"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-id)))
    (should (equal (aref cells 4)
                   (list "A foo warning"
                         'type 'flycheck-error-list
                         'face 'default)))
    (should (equal (aref cells 5)
                   (list "(emacs-lisp-checkdoc)"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-checker-name)))))

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
                   (list ""
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-id)))
    (should (equal (aref cells 4)
                   (list "A foo error"
                         'type 'flycheck-error-list
                         'face 'default)))
    (should (equal (aref cells 5)
                   (list "(emacs-lisp-checkdoc)"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-checker-name)))))

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
                   (list ""
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-id)))
    (should (equal (aref cells 4)
                   (list "Unknown info"
                         'type 'flycheck-error-list
                         'face 'default)))
    (should (equal (aref cells 5)
                   (list "(coq)"
                         'type 'flycheck-error-list
                         'face 'flycheck-error-list-checker-name)))))


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
                                    "This is a Flycheck error."
                                    :id "spam")))
    (shut-up
      (flycheck-display-error-messages (list err))
      (should (equal (shut-up-current-output)
                     "This is a Flycheck error. [spam]\n")))))


;;; Working with errors

(ert-deftest flycheck-copy-errors-as-kill ()
  :tags '(errors-at-point)
  (flycheck-ert-with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"
                                               :id "foo"))))
      (mapc #'flycheck-add-overlay errors)
      (flycheck-copy-errors-as-kill 10)
      (should (equal (-take 2 kill-ring) '("1st message" "2nd message")))
      (flycheck-copy-errors-as-kill 10 #'flycheck-error-id)
      (should (equal (-take 1 kill-ring) '("foo")))
      (flycheck-copy-errors-as-kill 10 #'flycheck-error-format-message-and-id)
      (should (equal (-take 2 kill-ring)
                     '("1st message" "2nd message [foo]"))))))


;;; Syntax checker executables

(ert-deftest flycheck-overridden-executable ()
  :tags '(executables language-emacs-lisp checker-emacs-lisp)
  (let ((flycheck-emacs-lisp-executable (flycheck-ert-resource-filename
                                         "bin/dummy-emacs")))
    (flycheck-ert-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(17 4 error "t is not true!" :checker emacs-lisp)
     '(19 11 warning "This is a stupid message" :checker emacs-lisp))))

(ert-deftest flycheck-set-checker-executable/real-executable ()
  :tags '(executables)
  (flycheck-ert-with-temp-buffer
    ;; Create a temporary buffer to restrict the scope of
    ;; `flycheck-emacs-lisp-executable'
    (let ((file-name (flycheck-ert-resource-filename "bin/dummy-emacs")))
      (should (file-exists-p file-name))
      (should (file-executable-p file-name))
      (should-not (local-variable-p 'flycheck-emacs-lisp-executable))
      (flycheck-set-checker-executable 'emacs-lisp file-name)
      (should (local-variable-p 'flycheck-emacs-lisp-executable))
      (should (string= flycheck-emacs-lisp-executable file-name))))
  ;; The global value should remain unaffected
  (should-not flycheck-emacs-lisp-executable))

(ert-deftest flycheck-set-checker-executable/no-executable-given ()
  :tags '(executables)
  (flycheck-ert-with-temp-buffer
    (let ((file-name (flycheck-ert-resource-filename "bin/dummy-emacs")))
      (setq-local flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp)
      (should-not flycheck-emacs-lisp-executable)
      (should (local-variable-p 'flycheck-emacs-lisp-executable)))))

(ert-deftest flycheck-set-checker-executable/executable-is-nil ()
  :tags '(executables)
  (flycheck-ert-with-temp-buffer
    (let ((file-name (flycheck-ert-resource-filename "bin/dummy-emacs")))
      (setq-local flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp nil)
      (should-not flycheck-emacs-lisp-executable)
      (should (local-variable-p 'flycheck-emacs-lisp-executable)))))

(ert-deftest flycheck-set-checker-executable/non-existing-file ()
  :tags '(executables)
  (let ((file-name (flycheck-ert-resource-filename "no-such-file")))
    (should-not (file-exists-p file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))

(ert-deftest flycheck-set-checker-executable/file-not-executable ()
  :tags '(executables)
  (let ((file-name (flycheck-ert-resource-filename "checkers/emacs-lisp.el")))
    (should (file-exists-p file-name))
    (should-not (file-executable-p file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-ert-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))


;;; Built-in syntax checkers

;; Tell the byte compiler about the variables we'll use
(defvar js2-mode-show-strict-warnings)
(defvar js2-mode-show-parse-errors)
(defvar js3-mode-show-parse-errors)
(defvar python-indent-guess-indent-offset)

(flycheck-ert-def-checker-test ada-gnat ada syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/ada/syntaxerror.adb" 'ada-mode
   '(7  32 error "missing \";\"" :checker ada-gnat)
   '(8 5 error "misspelling of \"SYNTAXERROR\"" :checker ada-gnat)))

(flycheck-ert-def-checker-test ada-gnat ada warnings
  (flycheck-ert-should-syntax-check
   "checkers/ada/hello.adb" 'ada-mode
   '(   6 4 warning "variable \"Name\" is not referenced" :checker ada-gnat)
   '(8  11 warning "unrecognized pragma \"Foo\"" :checker ada-gnat)))

(flycheck-ert-def-checker-test ada-gnat ada disable-warnings
  (let ((flycheck-gnat-warnings nil))
    (flycheck-ert-should-syntax-check
     "checkers/ada/hello.adb" 'ada-mode
     '(8  11 warning "unrecognized pragma \"Foo\"" :checker ada-gnat))))

(flycheck-ert-def-checker-test asciidoc asciidoc nil
  (flycheck-ert-should-syntax-check
   "checkers/asciidoc.adoc" 'adoc-mode
   '(1 nil warning "missing style: [paradef-default]: paragraph" :checker asciidoc)
   '(3 nil warning "old tables syntax" :checker asciidoc)
   '(11 nil error "[tabledef-default] illegal width=%60%" :checker asciidoc)))

(flycheck-ert-def-checker-test c/c++-clang (c c++) warning
  (let ((flycheck-disabled-checkers '(c/c++-gcc c/c++-cppcheck)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)
     '(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
         :checker c/c++-clang)
     '(8 7 warning "no message" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) warning-customized
  ;; Disable conversion checks by removing -Wextra, but additionally warn about
  ;; missing prototypes, which isn't included in -Wextra
  (let ((flycheck-clang-warnings '("all" "missing-prototypes"))
        (flycheck-disabled-checkers '(c/c++-gcc c/c++-cppcheck)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(3 5 warning "no previous prototype for function 'f'"
         :checker c/c++-clang)
     '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)
     '(8 7 warning "no message" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) fatal-error
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode
     '(2 10 error "'c_c++-library-header.h' file not found"
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) include-path
  (let ((flycheck-clang-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode)))

(flycheck-ert-def-checker-test c/c++-clang (c c++) included-file-error
  (let ((flycheck-clang-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-gcc))
        (include-file (flycheck-ert-resource-filename
                       "checkers/c_c++-warning.c")))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-included-file-error.cpp" 'c++-mode
     `(3 nil warning ,(format "In include %s" include-file)
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) includes
  (let  ((flycheck-clang-includes (list (flycheck-ert-resource-filename
                                         "checkers/c_c++-include/c_c++-library-header.h")))
         (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang)
     '(12 2 error "no message" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) error
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(3 23 info "template is declared here" :checker c/c++-clang)
     '(8 17 error "implicit instantiation of undefined template 'test<false>'"
         :checker c/c++-clang)
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang)
     '(12 2 error "no message" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) error-template
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error-template.cpp" 'c++-mode
     '(2 20 error "no member named 'bar' in 'A'"
         :checker c/c++-clang)
     '(6 19 info "in instantiation of function template specialization 'foo<A>' requested here"
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) error-language-standard
  (let ((flycheck-clang-language-standard "c++11")
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(3 23 info "template is declared here" :checker c/c++-clang)
     '(8 17 error "implicit instantiation of undefined template 'test<false>'"
         :checker c/c++-clang)
     '(12 2 error "no message" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) error-definitions
  (let ((flycheck-clang-definitions '("FLYCHECK_LIBRARY"))
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang)
     '(12 2 error "no message" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) ms-extensions-disabled
  ;; A sanity check for the following c/c++-clang-ms-extensions-enabled test
  ;; case
  (flycheck-ert-should-syntax-check
   "checkers/c_c++-ms-extensions.c" 'c-mode
   '(7 5 warning "declaration does not declare anything" :checker c/c++-clang)
   '(14 24 error "field designator 'a' does not refer to any field in type 'outer_s'"
        :checker c/c++-clang)))

(flycheck-ert-def-checker-test c/c++-clang (c c++) ms-extensions-enabled
  (let ((flycheck-clang-ms-extensions t))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-ms-extensions.c" 'c-mode
     '(7 5 warning "anonymous structs are a Microsoft extension"
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) error-no-exceptions
  (let ((flycheck-disabled-checkers '(c/c++-gcc))
        (flycheck-clang-no-exceptions t))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error-exceptions.cpp" 'c++-mode
     '(1 14 error "cannot use 'throw' with exceptions disabled"
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) error-no-rtti
  (let ((flycheck-clang-no-rtti t)
        (flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error-rtti.cpp" 'c++-mode
     '(4 32 error "cannot use dynamic_cast with -fno-rtti"
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) block-error
  ;; On Travis CI and OS X, -fblocks seems to be default for Clang, so this test
  ;; won't pass
  :expected-result '(or (satisfies (lambda (result)
                                     (and (or (eq system-type 'darwin)
                                              (flycheck-ert-travis-ci-p))
                                          (ert-test-failed-p result))))
                        :passed)
  (flycheck-ert-should-syntax-check
   "checkers/c_c++-clang-blocks.c" 'c-mode
   '(3 15 error "blocks support disabled - compile with -fblocks or pick a deployment target that supports them"
       :checker c/c++-clang)
   '(7 20 error "blocks support disabled - compile with -fblocks or pick a deployment target that supports them"
       :checker c/c++-clang)))

(flycheck-ert-def-checker-test c/c++-clang (c c++) blocks
  (let ((flycheck-clang-blocks t))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-clang-blocks.c" 'c-mode
     '(7 16 warning "unused variable 'p'" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) warning
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(5 10 warning "unused variable ‘unused’" :checker c/c++-gcc)
     '(7 15 warning "comparison between signed and unsigned integer expressions"
         :checker c/c++-gcc)
     '(8 7 warning "#warning" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) warning-customized
  ;; Disable conversion checks by removing -Wextra, but additionally warn about
  ;; missing prototypes, which isn't included in -Wextra
  (let ((flycheck-gcc-warnings '("all" "missing-prototypes"))
        (flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-warning.c" 'c-mode
     '(3 5 warning "no previous prototype for ‘f’"
         :checker c/c++-gcc)
     '(5 10 warning "unused variable ‘unused’" :checker c/c++-gcc)
     '(8 7 warning "#warning" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) fatal-error
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode
     '(2 34 error "c_c++-library-header.h: No such file or directory"
         :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) include-path
  (let ((flycheck-gcc-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-fatal-error.c" 'c-mode)))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) included-file-error
  (let ((flycheck-gcc-include-path '("./c_c++-include"))
        (flycheck-disabled-checkers '(c/c++-clang))
        (include-file (flycheck-ert-resource-filename
                       "checkers/c_c++-warning.c")))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-included-file-error.cpp" 'c++-mode
     `(3 nil warning ,(format "In include %s" include-file)
         :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) includes
  (let  ((flycheck-gcc-includes (list (flycheck-ert-resource-filename
                                       "checkers/c_c++-include/c_c++-library-header.h")))
         (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 5 warning "identifier ‘nullptr’ is a keyword in C++11"
          :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’"
          :checker c/c++-gcc)
     '(10 16 error "‘nullptr’ was not declared in this scope"
          :checker c/c++-gcc)
     '(12 2 error "#error" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) error
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(8 17 error "aggregate ‘test<false> t’ has incomplete type and cannot be defined"
         :checker c/c++-gcc)
     '(10 5 warning "identifier ‘nullptr’ is a keyword in C++11"
          :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’"
          :checker c/c++-gcc)
     '(10 16 error "‘nullptr’ was not declared in this scope"
          :checker c/c++-gcc)
     '(12 2 error "#error" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) error-template
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error-template.cpp" 'c++-mode
     '(2 18 error "‘struct A’ has no member named ‘bar’"
         :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) error-language-standard
  (let ((flycheck-gcc-language-standard "c++11")
        (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(8 17 error "aggregate ‘test<false> t’ has incomplete type and cannot be defined"
         :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’" :checker c/c++-gcc)
     '(12 2 error "#error" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) error-definitions
  (let ((flycheck-gcc-definitions '("FLYCHECK_LIBRARY"))
        (flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error.cpp" 'c++-mode
     '(10 5 warning "identifier ‘nullptr’ is a keyword in C++11"
          :checker c/c++-gcc)
     '(10 10 warning "unused variable ‘foo’"
          :checker c/c++-gcc)
     '(10 16 error "‘nullptr’ was not declared in this scope"
          :checker c/c++-gcc)
     '(12 2 error "#error" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) error-no-exceptions
  (let ((flycheck-disabled-checkers '(c/c++-clang))
        (flycheck-gcc-no-exceptions t))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error-exceptions.cpp" 'c++-mode
     '(1 20 error "exception handling disabled, use -fexceptions to enable"
         :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) error-no-rtti
  (let ((flycheck-disabled-checkers '(c/c++-clang))
        (flycheck-gcc-no-rtti t))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-error-rtti.cpp" 'c++-mode
     '(4 56 error "‘dynamic_cast’ not permitted with -fno-rtti"
         :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) openmp-disabled
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck))
        (flycheck-gcc-language-standard "c99"))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-warning-openmp.c" 'c-mode
     '(3 8 warning "variable ‘a’ set but not used" :checker c/c++-gcc)
     '(5 nil warning "ignoring #pragma omp parallel" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) openmp-enabled
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck))
        (flycheck-gcc-language-standard "c99")
        (flycheck-gcc-openmp t))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-warning-openmp.c" 'c-mode
     '(3 8 warning "variable ‘a’ set but not used" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-cppcheck (c c++) nil
  :tags '(cppcheck-xml)
  (skip-unless (version< "1.53" (flycheck-test-cppcheck-version)))
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
        (flycheck-cppcheck-inconclusive nil)
        (flycheck-cppcheck-checks '("style")))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-cppcheck.cpp" '(c-mode c++-mode)
     '(5 nil info "Unused variable: unused" :id "unusedVariable"
         :checker c/c++-cppcheck)
     '(9 nil error "Division by zero." :id "zerodiv" :checker c/c++-cppcheck)
     '(14 nil warning "Parameter 'foo' is passed by value. It could be passed as a (const) reference which is usually faster and recommended in C++."
          :id "passedByValue" :checker c/c++-cppcheck))))

(flycheck-ert-def-checker-test c/c++-cppcheck (c c++) style-suppressed
  :tags '(cppcheck-xml)
  (let ((flycheck-cppcheck-checks nil)
        (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
        (dot (when (version< "1.53" (flycheck-test-cppcheck-version)) ".")))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-cppcheck.cpp" '(c-mode c++-mode)
     `(9 nil error ,(concat "Division by zero" dot) :id "zerodiv"
         :checker c/c++-cppcheck))))

(flycheck-ert-def-checker-test c/c++-cppcheck (c c++) inconclusive
  :tags '(cppcheck-xml)
  ;; Cppcheck 1.53 and older do not report inconclusive warnings when using
  ;; XML output.
  (skip-unless (version< "1.53" (flycheck-test-cppcheck-version)))
  (let ((flycheck-cppcheck-checks '("style"))
        (flycheck-cppcheck-inconclusive t)
        (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-cppcheck.cpp" '(c-mode c++-mode)
     '(5 nil info "Unused variable: unused" :id "unusedVariable"
         :checker c/c++-cppcheck)
     '(9 nil error "Division by zero." :id "zerodiv" :checker c/c++-cppcheck)
     '(12 nil info "Boolean variable 'a' is used in bitwise operation. Did you mean '&&'?"
          :id "bitwiseOnBoolean" :checker c/c++-cppcheck)
     '(14 nil warning "Parameter 'foo' is passed by value. It could be passed as a (const) reference which is usually faster and recommended in C++."
          :id "passedByValue" :checker c/c++-cppcheck))))

(flycheck-ert-def-checker-test c/c++-cppcheck (c c++) multiple-checks
  :tags '(cppcheck-xml)
  (skip-unless (version< "1.53" (flycheck-test-cppcheck-version)))
  (let ((flycheck-cppcheck-checks '("performance" "portability"))
        (flycheck-cppcheck-inconclusive nil)
        (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "checkers/c_c++-cppcheck.cpp" 'c++-mode
     '(9 nil error "Division by zero." :id "zerodiv" :checker c/c++-cppcheck)
     '(14 nil warning "Parameter 'foo' is passed by value. It could be passed as a (const) reference which is usually faster and recommended in C++."
          :id "passedByValue" :checker c/c++-cppcheck))))

(flycheck-ert-def-checker-test cfengine cfengine error
  (skip-unless (fboundp 'cfengine3-mode))
  (flycheck-ert-should-syntax-check
   "checkers/cfengine-error.cf" 'cfengine3-mode
   '(8 20 error "Unknown promise type 'nosuchpromisetype'" :checker cfengine)))

(flycheck-ert-def-checker-test cfengine cfengine warning
  (skip-unless (fboundp 'cfengine3-mode))
  (flycheck-ert-should-syntax-check
   "checkers/cfengine-warning.cf" 'cfengine3-mode
   '(3 34 warning "Removed constraint 'host_licenses_paid' in promise type 'common' [-Wremoved]"
       :checker cfengine)))

(flycheck-ert-def-checker-test chef-foodcritic chef nil
  (flycheck-ert-should-syntax-check
   "checkers/chef-foodcritic/recipes/chef-foodcritic-error.rb" 'ruby-mode
   '(3 nil error "FC002: Avoid string interpolation where not required"
       :checker chef-foodcritic)
   '(8 nil error "FC003: Check whether you are running with chef server before using server-specific features"
       :checker chef-foodcritic)
   '(11 nil error "FC004: Use a service resource to start and stop services"
        :checker chef-foodcritic)))

(flycheck-ert-def-checker-test coffee coffee syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/coffee-syntax-error.coffee" 'coffee-mode
   '(4 7 error "missing \", starting" :checker coffee)))

(flycheck-ert-def-checker-test coffee-coffeelint coffee error
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
   '(4 nil error "Throwing strings is forbidden; context:"
       :checker coffee-coffeelint)))

(flycheck-ert-def-checker-test coffee-coffeelint coffee warning
  :tags '(checkstyle-xml)
  (let ((flycheck-coffeelintrc "coffeelint.json"))
    (flycheck-ert-should-syntax-check
     "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
     '(4 nil warning "Throwing strings is forbidden; context:"
         :checker coffee-coffeelint))))

(flycheck-ert-def-checker-test coq coq simple-syntax-error
  (let* ((version (flycheck-test-coq-version))
         (msg (if (version< "8.3" version)
                  "Lexer: Undefined token"
                "Undefined token.")))
    (flycheck-ert-should-syntax-check
     "checkers/coq-syntax-error-simple.v" 'coq-mode
     `(3 18 error ,msg :checker coq))))

(flycheck-ert-def-checker-test coq coq syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/coq-syntax-error.v" 'coq-mode
   '(6 12 error "'end' expected after [branches] (in [match_constr])."
       :checker coq)))

(flycheck-ert-def-checker-test coq coq error
  (flycheck-ert-should-syntax-check
   "checkers/coq-error.v" 'coq-mode
   '(7 21 error "In environment
evenb : nat -> bool
n : nat
n0 : nat
n' : nat
The term \"1\" has type \"nat\" while it is expected to have type
\"bool\"." :checker coq)))

(flycheck-ert-def-checker-test css-csslint css nil
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "checkers/css-csslint-warning.css" 'css-mode
   '(3 6 warning "Heading (h1) should not be qualified."
       :id "Disallowqualifiedheadings" :checker css-csslint)))

(flycheck-ert-def-checker-test css-csslint css syntax-error
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "checkers/css-syntax-error.css" 'css-mode
   '(4 16 error "Expected LBRACE at line 4, col 16." :id "ParsingErrors"
       :checker css-csslint)
   '(4 16 error "Unexpected token '100%' at line 4, col 16." :id "ParsingErrors"
       :checker css-csslint)
   '(4 20 error "Unexpected token ';' at line 4, col 20." :id "ParsingErrors"
       :checker css-csslint)
   '(5 1 error "Unexpected token '}' at line 5, col 1." :id "ParsingErrors"
       :checker css-csslint)))

(ert-deftest flycheck-d-module-re/matches-module-name ()
  :tags '(language-d)
  (ert-skip "Skipped pending https://github.com/flycheck/flycheck/issues/531")
  (let ((s "module spam.with.eggs ;"))
    (should (string-match flycheck-d-module-re s))
    (should (string= "spam.with.eggs" (match-string 1 s)))))

(ert-deftest flycheck-d-base-directory/no-module-declaration ()
  :tags '(language-d)
  (ert-skip "Skipped pending https://github.com/flycheck/flycheck/issues/531")
  (flycheck-ert-with-resource-buffer "checkers/d/src/dmd/no_module.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-ert-resource-filename "checkers/d/src/dmd")))))

(ert-deftest flycheck-d-base-directory/with-module-declaration ()
  :tags '(language-d)
  (ert-skip "Skipped pending https://github.com/flycheck/flycheck/issues/531")
  (flycheck-ert-with-resource-buffer "checkers/d/src/dmd/warning.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-ert-resource-filename "checkers/d/src")))))

(ert-deftest flycheck-d-base-directory/package-file ()
  :tags '(language-d)
  (ert-skip "Skipped pending https://github.com/flycheck/flycheck/issues/531")
  (flycheck-ert-with-resource-buffer "checkers/d/src/dmd/package.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-ert-resource-filename "checkers/d/src")))))

(flycheck-ert-def-checker-test d-dmd d warning-include-path
  (ert-skip "Skipped pending https://github.com/flycheck/flycheck/issues/531")
  (let ((flycheck-dmd-include-path '("../../lib")))
    (flycheck-ert-should-syntax-check
     "checkers/d/src/dmd/warning.d" 'd-mode
     '(9 5 warning "statement is not reachable" :checker d-dmd)
     '(20 17 warning "function dmd.warning.bar is deprecated"
          :checker d-dmd))))

(flycheck-ert-def-checker-test d-dmd d missing-import
  (ert-skip "Skipped pending https://github.com/flycheck/flycheck/issues/531")
  (flycheck-ert-should-syntax-check
   "checkers/d/src/dmd/warning.d" 'd-mode
   '(4 8 error "module external_library is in file 'external_library.d' which cannot be read"
       :checker d-dmd)))

(flycheck-ert-def-checker-test elixir elixir error
  (flycheck-ert-should-syntax-check
   "checkers/elixir-error.ex" 'elixir-mode
   '(5 nil error "function puts/1 undefined" :checker elixir)))

(flycheck-ert-def-checker-test elixir elixir warnings
  (flycheck-ert-should-syntax-check
   "checkers/elixir-warnings.ex" 'elixir-mode
   '(7 nil warning "this clause cannot match because a previous clause at line 4 always matches"
       :checker elixir)))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp nil
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
    (flycheck-ert-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     `(15 1 error ,msg :checker emacs-lisp))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp load-path
  (let ((flycheck-emacs-lisp-load-path (list (flycheck-ert-resource-filename
                                              "dummy-elpa/dummy-package-0.1"))))
    (flycheck-ert-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but requires 1+"
          :checker emacs-lisp)
     '(23 1 warning "the function `dummy-package-foo' might not be defined at runtime."
          :checker emacs-lisp))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp initialize-packages
  (let ((flycheck-emacs-lisp-initialize-packages t)
        (flycheck-emacs-lisp-package-user-dir (flycheck-ert-resource-filename
                                               "dummy-elpa")))
    (flycheck-ert-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but requires 1+"
          :checker emacs-lisp))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp checks-compressed-file
  (flycheck-ert-should-syntax-check
   "checkers/emacs-lisp.el.gz" 'emacs-lisp-mode
   '(12 nil warning "First sentence should end with punctuation"
        :checker emacs-lisp-checkdoc)
   '(16 6 warning "message called with 0 arguments, but requires 1+"
        :checker emacs-lisp)
   '(21 1 warning "the function `dummy-package-foo' is not known to be defined."
        :checker emacs-lisp)))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp sytnax-error
  (let ((flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
    (flycheck-ert-should-syntax-check
     "checkers/emacs-lisp-syntax-error.el" 'emacs-lisp-mode
     '(3 1 error "End of file during parsing" :checker emacs-lisp))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp without-file-name
  ;; Regression test for checkdoc in buffers without file names. See
  ;; https://github.com/flycheck/flycheck/issues/73 and
  ;; https://github.com/bbatsov/prelude/issues/259
  (flycheck-ert-with-resource-buffer "checkers/emacs-lisp.el"
    (set-visited-file-name nil 'no-query)
    (emacs-lisp-mode)
    (should-not (buffer-file-name))
    (flycheck-ert-buffer-sync)
    ;; TODO: Consider whether checkdoc is really useful in buffers without file
    ;; names…
    (should flycheck-current-errors)))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp
                               does-not-check-autoloads-buffers
  ;; Regression test ensuring that Emacs Lisp won't check autoload buffers.
  ;; These buffers are temporary buffers created during package installation to
  ;; collect the autoloads of newly installed packages before writing the
  ;; autoloads file.  See `https://github.com/flycheck/flycheck/issues/45' and
  ;; `https://github.com/bbatsov/prelude/issues/253' for details.
  (flycheck-ert-with-file-buffer (locate-library "dash-autoloads")
    (should-not (flycheck-may-use-checker 'emacs-lisp))
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp
                               checkdoc-does-not-check-cask-files
  (flycheck-ert-with-file-buffer
      (expand-file-name "Cask" flycheck-test-source-directory)
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp
                               does-not-check-with-no-byte-compile
  ;; We need to use a hook here, because `no-byte-compile' seems to be
  ;; explicitly changed when loading Emacs Lisp files
  (let ((disable-byte-comp (lambda () (setq-local no-byte-compile t))))
    (add-hook 'emacs-lisp-mode-hook disable-byte-comp)
    (unwind-protect
        (flycheck-ert-should-syntax-check
         "checkers/emacs-lisp.el" 'emacs-lisp-mode
         '(12 nil warning "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))
      (remove-hook 'emacs-lisp-mode-hook disable-byte-comp))))

(flycheck-ert-def-checker-test erlang erlang error
  (flycheck-ert-should-syntax-check
   "checkers/erlang-error.erl" 'erlang-mode
   '(7 nil error "head mismatch" :checker erlang)))

(flycheck-ert-def-checker-test erlang erlang warning
  (flycheck-ert-should-syntax-check
   "checkers/erlang-warning.erl" 'erlang-mode
   '(6 nil warning "wrong number of arguments in format call" :checker erlang)))

(flycheck-ert-def-checker-test eruby-erubis eruby nil
  (flycheck-ert-should-syntax-check
   "checkers/eruby-error.erb" '(html-erb-mode rhtml-mode)
   '(5 nil error "syntax error, unexpected keyword_end" :checker eruby-erubis)))

(flycheck-ert-def-checker-test fortran-gfortran fortran error
  (flycheck-ert-should-syntax-check
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

(flycheck-ert-def-checker-test fortran-gfortran fortran free-form-error
  (let ((flycheck-gfortran-layout 'free))
    (flycheck-ert-should-syntax-check
     "checkers/fortran-error.f" '(fortran-mode f90-mode)
     '(3 3 error "Expecting END PROGRAM statement at (1)"
         :checker fortran-gfortran))))

(flycheck-ert-def-checker-test fortran-gfortran fortran warning
  (flycheck-ert-should-syntax-check
   "checkers/fortran-warning.f90" '(fortran-mode f90-mode)
   '(1 20 warning "Unused dummy argument 'p' at (1)" :checker fortran-gfortran)
   '(18 9 warning "Same actual argument associated with INTENT(IN) argument 'a' and INTENT(OUT) argument 'b' at (1)"
        :checker fortran-gfortran)))

(flycheck-ert-def-checker-test fortran-gfortran fortran specific-warnings
  (let ((flycheck-gfortran-warnings '("unused-dummy-argument")))
    (flycheck-ert-should-syntax-check
     "checkers/fortran-warning.f90" '(fortran-mode f90-mode)
     '(1 20 warning "Unused dummy argument 'p' at (1)"
         :checker fortran-gfortran))))

(flycheck-ert-def-checker-test go-gofmt go syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/go/src/syntax/syntax-error.go" 'go-mode
   '(5 9 error "expected '(', found 'IDENT' ta" :checker go-gofmt)
   '(6 1 error "expected ')', found '}'" :checker go-gofmt)))

(flycheck-ert-def-checker-test (go-build go-golint go-vet) go complete-chain
  (skip-unless (funcall (flycheck-checker-predicate 'go-vet)))
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
    (flycheck-ert-should-syntax-check
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

(flycheck-ert-def-checker-test (go-vet go-build go-golint) go print-functions
  (let ((flycheck-go-vet-print-functions '("Warn:0" "Warnf:1"))
        (flycheck-disabled-checkers '(go-golint go-build go-errcheck)))
    (flycheck-ert-with-env
        `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
      (flycheck-ert-should-syntax-check
       "checkers/go/src/warnings.go" 'go-mode
       '(17 nil warning "arg 1 for printf verb %s of wrong type: untyped int"
            :checker go-vet)
       '(18 nil warning "possible formatting directive in Warn call"
            :checker go-vet)
       '(19 nil warning "constant 1 not a string in call to Warnf"
            :checker go-vet)
       '(23 nil warning "unreachable code" :checker go-vet)))))

(flycheck-ert-def-checker-test go-build go handles-packages
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
    (flycheck-ert-should-syntax-check "checkers/go/src/b1/main.go" 'go-mode)))

(flycheck-ert-def-checker-test go-build go missing-package
  (let* ((go-root (or (getenv "GOROOT") "/usr/local/go"))
         (go-root-pkg (concat go-root "/src/pkg")))
    (flycheck-ert-with-env '(("GOPATH" . nil))
      (flycheck-ert-should-syntax-check
       "checkers/go/src/b1/main.go" 'go-mode
       `(4 2 error ,(format "cannot find package \"b2\" in any of:\n\t%s/b2 (from $GOROOT)\n\t($GOPATH not set)"
                            go-root-pkg)
           :checker go-build)))))

(flycheck-ert-def-checker-test go-test go nil
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
    (flycheck-ert-should-syntax-check
     "checkers/go/src/test/test-error_test.go" 'go-mode
     '(8 nil error "undefined: fmt" :checker go-test))))

(ert-deftest flycheck-go-package-name/no-gopath ()
  :tags '(language-go)
  (flycheck-ert-with-env '(("GOPATH" . nil))
    (should-not (flycheck-go-package-name
                 (flycheck-ert-resource-filename
                  "checkers/go/src/errcheck/errcheck.go")))))

(ert-deftest flycheck-go-package-name/no-package-file ()
  :tags '(language-go)
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
    (should-not (flycheck-go-package-name
                 (flycheck-ert-resource-filename
                  "checkers/emacs-lisp-syntax-error.el")))))

(ert-deftest flycheck-go-package-name/package-file ()
  :tags '(language-go)
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
    (should (equal "errcheck"
                   (flycheck-go-package-name
                    (flycheck-ert-resource-filename
                     "checkers/go/src/errcheck/errcheck.go"))))))

(flycheck-ert-def-checker-test go-errcheck go nil
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
    (flycheck-ert-should-syntax-check
     "checkers/go/src/errcheck/errcheck.go" 'go-mode
     '(7 9 warning "Ignored `error` returned from `f.Close()`"
         :checker go-errcheck)
     '(9 9 warning "Ignored `error` returned from `os.Stat(\"enoent\")`"
         :checker go-errcheck))))

(flycheck-ert-def-checker-test haml haml nil
  (flycheck-ert-should-syntax-check
   "checkers/haml-error.haml" 'haml-mode
   '(5 nil error "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces."
       :checker haml)))

(flycheck-ert-def-checker-test handlebars handlebars nil
  (flycheck-ert-should-syntax-check
   "checkers/handlebars-error.hbs" 'handlebars-mode
   '(2 nil error "Expecting 'ID', 'DATA', got 'INVALID'"
       :checker handlebars)))

(ert-deftest flycheck-haskell-module-re/matches-module-name ()
  :tags '(language-haskell)
  (let ((s "module Foo.Bar where"))
    (should (string-match flycheck-haskell-module-re s))
    (should (string= "Foo.Bar" (match-string 1 s)))))

(ert-deftest flycheck-haskell-module-re/ignores-commented-code ()
  :tags '(language-haskell)
  (should-not (string-match-p flycheck-haskell-module-re
                              "-- | module Foo.Bar where")))

(flycheck-ert-def-checker-test haskell-ghc haskell syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/Haskell/SyntaxError.hs" 'haskell-mode
   '(3 1 error "parse error on input ‘module’" :checker haskell-ghc)))

(flycheck-ert-def-checker-test haskell-ghc haskell type-error
  (flycheck-ert-should-syntax-check
   "checkers/Haskell/Error.hs" 'haskell-mode
   '(4 16 error "Couldn't match type ‘Bool’ with ‘[Char]’
Expected type: String
  Actual type: Bool
In the first argument of ‘putStrLn’, namely ‘True’
In the expression: putStrLn True" :checker haskell-ghc)))

(flycheck-ert-def-checker-test haskell-ghc haskell no-user-package-database
  :expected-result :failed
  (error "Not implemented!"))

(flycheck-ert-def-checker-test haskell-ghc haskell package-databases
  :expected-result :failed
  (error "Not implemented!"))

(flycheck-ert-def-checker-test haskell-ghc haskell search-path
  (let* ((lib-dir (flycheck-ert-resource-filename "checkers/Haskell/lib"))
         (flycheck-ghc-search-path (list lib-dir)))
    (flycheck-ert-should-syntax-check
     "checkers/Haskell/SearchPath.hs" 'haskell-mode
     '(5 1 warning "Top-level binding with no type signature: helloYou :: IO ()"
         :checker haskell-ghc))))

(flycheck-ert-def-checker-test haskell-ghc haskell missing-language-extension
  (flycheck-ert-should-syntax-check
   "checkers/Haskell/LanguageExtension.hs" 'haskell-mode
   '(4 18 error "Couldn't match expected type ‘BS.ByteString’
            with actual type ‘[Char]’
In the first argument of ‘BS.putStr’, namely ‘\"Hello World\"’
In the expression: BS.putStr \"Hello World\"
In an equation for ‘main’: main = BS.putStr \"Hello World\""
       :checker haskell-ghc)))

(flycheck-ert-def-checker-test haskell-ghc haskell language-extensions
  (let ((flycheck-ghc-language-extensions '("OverloadedStrings")))
    (flycheck-ert-should-syntax-check
     "checkers/Haskell/LanguageExtension.hs" 'haskell-mode)))

(flycheck-ert-def-checker-test (haskell-ghc haskell-hlint) haskell literate
  (flycheck-ert-should-syntax-check
   "checkers/Haskell/Literate.lhs" 'literate-haskell-mode
   '(6 1 warning "Top-level binding with no type signature: foo :: forall t. t"
       :checker haskell-ghc)))

(flycheck-ert-def-checker-test (haskell-ghc haskell-hlint) haskell
                               complete-chain
  (flycheck-ert-should-syntax-check
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

(flycheck-ert-def-checker-test html-tidy html nil
  (flycheck-ert-should-syntax-check
   "checkers/html-tidy-warning-and-error.html" '(html-mode)
   '(3 1 warning "missing <!DOCTYPE> declaration"
       :checker html-tidy)
   '(8 5 error "<spam> is not recognized!"
       :checker html-tidy)
   '(8 5 warning "discarding unexpected <spam>"
       :checker html-tidy)))

(flycheck-ert-def-checker-test javascript-jshint javascript syntax-error
  :tags '(checkstyle-xml)
  ;; Silence JS2 and JS3 parsers
  (let ((js2-mode-show-parse-errors nil)
        (js2-mode-show-strict-warnings nil)
        (js3-mode-show-parse-errors nil))
    (flycheck-ert-should-syntax-check
     "checkers/javascript-syntax-error.js" '(js-mode js2-mode js3-mode)
     '(3 11 error "Unclosed string." :id "E029" :checker javascript-jshint)
     '(3 25 warning "Unclosed string." :id "W112" :checker javascript-jshint)
     '(4 1 warning "Unclosed string." :id "W112" :checker javascript-jshint)
     '(4 1 warning "Missing semicolon." :id "W033"
         :checker javascript-jshint))))

(flycheck-ert-def-checker-test javascript-jshint javascript error-disabled
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)))

(flycheck-ert-def-checker-test javascript-jshint javascript nil
  :tags '(checkstyle-xml)
  (let ((flycheck-jshintrc "jshintrc"))
    (flycheck-ert-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 9 warning "'foo' is defined but never used." :id "W098"
         :checker javascript-jshint))))

(flycheck-ert-def-checker-test javascript-eslint javascript error
  :tags '(checkstyle-xml)
  (let ((flycheck-disabled-checkers '(javascript-jshint)))
    (flycheck-ert-should-syntax-check
     "checkers/javascript-syntax-error.js" '(js-mode js2-mode js3-mode)
     '(3 26 error "Unexpected token ILLEGAL" :checker javascript-eslint))))

(flycheck-ert-def-checker-test javascript-eslint javascript warning
  :tags '(checkstyle-xml)
  (let ((flycheck-eslintrc "eslint.json")
        (flycheck-disabled-checkers '(javascript-jshint)))
    (flycheck-ert-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(3 1 warning "Missing \"use strict\" statement." :id "strict"
         :checker javascript-eslint)
     '(4 8 warning "foo is defined but never used" :id "no-unused-vars"
         :checker javascript-eslint))))

(flycheck-ert-def-checker-test javascript-gjslint javascript nil
  (let ((flycheck-disabled-checkers '(javascript-jshint javascript-eslint)))
    (flycheck-ert-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 nil warning "Single-quoted string preferred over double-quoted string."
         :id "0131" :checker javascript-gjslint)
     '(4 nil warning "Extra space before \"]\""
         :id "0001" :checker javascript-gjslint))))

(flycheck-ert-def-checker-test json-jsonlint json nil
  (flycheck-ert-should-syntax-check
   "checkers/json-jsonlint-error.json" 'text-mode
   '(1 42 error "found: ',' - expected: 'EOF'." :checker json-jsonlint)))

(flycheck-ert-def-checker-test less less file-error
  (flycheck-ert-should-syntax-check
   "checkers/less-file-error.less" 'less-css-mode
   '(3 1 error "'no-such-file.less' wasn't found" :checker less)))

(flycheck-ert-def-checker-test less less syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/less-syntax-error.less" 'less-css-mode
   '(1 1 error "Unrecognised input" :checker less)))

(flycheck-ert-def-checker-test lua lua nil
  (flycheck-ert-should-syntax-check
   "checkers/lua-syntax-error.lua" 'lua-mode
   '(5 nil error "unfinished string near '\"oh no'"
       :checker lua)))

(flycheck-ert-def-checker-test make make nil
  (flycheck-ert-should-syntax-check
   "checkers/make.mk" '(makefile-mode makefile-gmake-mode)
   '(2 nil error "*** missing separator.  Stop." :checker make)))

(flycheck-ert-def-checker-test make make pmake
  (let ((flycheck-make-executable "pmake"))
    (skip-unless (executable-find (flycheck-checker-executable 'make)))
    (flycheck-ert-should-syntax-check
     "checkers/make.mk" 'makefile-bsdmake-mode
     '(2 nil error "Need an operator" :checker make))))

(flycheck-ert-def-checker-test perl perl nil
  (flycheck-ert-should-syntax-check
   "checkers/perl.pl" '(perl-mode cperl-mode)
   '(6 nil error "Global symbol \"$x\" requires explicit package name"
       :checker perl)
   '(6 nil error "BEGIN not safe after errors--compilation aborted"
       :checker perl)
   '(6 6 error "Glob written as <...> (See page 167 of PBP)"
       :id "BuiltinFunctions::RequireGlobFunction" :checker perl-perlcritic)))

(flycheck-ert-def-checker-test perl-perlcritic perl severity-1
  (let ((flycheck-perlcritic-severity 1))
    (flycheck-ert-should-syntax-check
     "checkers/perl.pl" '(perl-mode cperl-mode)
     '(1 1 warning "No package-scoped \"$VERSION\" variable found (See page 404 of PBP)"
         :id "Modules::RequireVersionVar"
         :checker perl-perlcritic)
     '(1 1 info "Package \"perl\" does not start with a upper case letter (See pages 45,46 of PBP)"
         :id "NamingConventions::Capitalization"
         :checker perl-perlcritic)
     '(6 nil error "Global symbol \"$x\" requires explicit package name"
         :checker perl)
     '(6 nil error "BEGIN not safe after errors--compilation aborted"
         :checker perl)
     '(6 6 error "Glob written as <...> (See page 167 of PBP)"
         :id "BuiltinFunctions::RequireGlobFunction"
         :checker perl-perlcritic)
     '(8 1 info "Builtin function called with parentheses (See page 13 of PBP)"
         :id "CodeLayout::ProhibitParensWithBuiltins"
         :checker perl-perlcritic)
     '(10 1 warning "\"die\" used instead of \"croak\" (See page 283 of PBP)"
          :id "ErrorHandling::RequireCarping"
          :checker perl-perlcritic))))

(flycheck-ert-def-checker-test php php syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/php-syntax-error.php" 'php-mode
   '(8 nil error "syntax error, unexpected ')', expecting '('" :checker php)))

(flycheck-ert-def-checker-test php php nil
  :tags '(phpmd-xml checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "checkers/php.php" 'php-mode
   '(19 1 error "Missing class doc comment"
        :id "PEAR.Commenting.ClassComment.Missing" :checker php-phpcs)
   '(21 nil warning "Avoid unused private fields such as '$FOO'."
        :id "UnusedPrivateField" :checker php-phpmd)
   '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore"
        :id "PEAR.NamingConventions.ValidVariableName.PrivateNoUnderscore"
        :checker php-phpcs)
   '(23 5 error "The open comment tag must be the only content on the line"
        :id "Generic.Commenting.DocComment.ContentAfterOpen"
        :checker php-phpcs)
   '(23 5 error "Doc comment for parameter \"$baz\" missing"
        :id "PEAR.Commenting.FunctionComment.MissingParamTag"
        :checker php-phpcs)
   '(23 9 error "Doc comment short description must be on the first line"
        :id "Generic.Commenting.DocComment.SpacingBeforeShort"
        :checker php-phpcs)
   '(23 29 error "The close comment tag must be the only content on the line"
        :id "Generic.Commenting.DocComment.ContentBeforeClose"
        :checker php-phpcs)
   '(23 29 error "Missing @return tag in function comment"
        :id "PEAR.Commenting.FunctionComment.MissingReturn"
        :checker php-phpcs)
   '(24 nil warning "Avoid unused private methods such as 'bar'."
        :id "UnusedPrivateMethod" :checker php-phpmd)
   '(24 nil warning "Avoid unused parameters such as '$baz'."
        :id "UnusedFormalParameter" :checker php-phpmd)
   '(24 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
        :id "PEAR.NamingConventions.ValidFunctionName.PrivateNoUnderscore"
        :checker php-phpcs)
   '(26 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
        :id "ShortVariable" :checker php-phpmd)
   '(26 nil warning "Avoid unused local variables such as '$i'."
        :id "UnusedLocalVariable" :checker php-phpmd)
   '(26 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
        :id "Generic.PHP.LowerCaseConstant.Found" :checker php-phpcs)))

(flycheck-ert-def-checker-test php-phpmd php rulesets
  :tags '(phpmd-xml)
  (let ((flycheck-phpmd-rulesets (remove "unusedcode" flycheck-phpmd-rulesets))
        (flycheck-disabled-checkers '(php-phpcs)))
    (flycheck-ert-should-syntax-check
     "checkers/php.php" 'php-mode
     '(26 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
          :id "ShortVariable" :checker php-phpmd))))

(flycheck-ert-def-checker-test php-phpcs php standard
  :tags '(checkstyle-xml)
  (let ((flycheck-phpcs-standard "Zend")
        (flycheck-disabled-checkers '(php-phpmd)))
    (flycheck-ert-should-syntax-check
     "checkers/php.php" 'php-mode
     '(21 20 error "Private member variable \"FOO\" must contain a leading underscore"
          :id "Zend.NamingConventions.ValidVariableName.PrivateNoUnderscore"
          :checker php-phpcs)
     '(30 1 error "A closing tag is not permitted at the end of a PHP file"
          :id "Zend.Files.ClosingTag.NotAllowed" :checker php-phpcs))))

(flycheck-ert-def-checker-test puppet-parser puppet singleline-syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/puppet-parser-singleline.pp" 'puppet-mode
   '(3 nil error "Syntax error at ','; expected '}'" :checker puppet-parser)))

(flycheck-ert-def-checker-test puppet-parser puppet multiline-syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/puppet-parser-multiline.pp" 'puppet-mode
   '(8 nil error "Unclosed quote after '' in 'something
}
'" :checker puppet-parser)))

(flycheck-ert-def-checker-test puppet-lint puppet nil
  (flycheck-ert-should-syntax-check
   "checkers/puppet-lint.pp" 'puppet-mode
   '(2 nil error "foo::bar not in autoload module layout (autoloader_layout)"
       :checker puppet-lint)
   '(3 nil warning "case statement without a default case (case_without_default)"
       :checker puppet-lint)))

(flycheck-ert-def-checker-test python-flake8 python syntax-error
  (let ((python-indent-guess-indent-offset nil))       ; Silence Python Mode!
    (flycheck-ert-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     '(3 13 error "SyntaxError: invalid syntax" :id "E901"
         :checker python-flake8))))

(flycheck-ert-def-checker-test python-flake8 python warning-ignored
  (let ((flycheck-flake8rc "flake8rc"))
    (flycheck-ert-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
         :checker python-flake8)
     '(9 9 info "function name should be lowercase"
         :checker python-flake8 :id "N802")
     '(22 1 error "undefined name 'antigravity'" :id "F821"
          :checker python-flake8))))

(flycheck-ert-def-checker-test python-flake8 python maximum-complexity
  (let ((flycheck-flake8-maximum-complexity 2))
    (flycheck-ert-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(5 1 warning "'antigravit' imported but unused" :id "F401"
         :checker python-flake8)
     '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
         :checker python-flake8)
     '(9 9 info "function name should be lowercase"
         :checker python-flake8 :id "N802")
     '(12 1 warning "'Spam.with_ham' is too complex (3)" :id "C901"
          :checker python-flake8)
     '(12 29 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(12 31 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(22 1 error "undefined name 'antigravity'" :id "F821"
          :checker python-flake8))))

(flycheck-ert-def-checker-test python-flake8 python error-maximum-line-length
  (let ((flycheck-flake8-maximum-line-length 45))
    (flycheck-ert-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(5 1 warning "'antigravit' imported but unused" :id "F401"
         :checker python-flake8)
     '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
         :checker python-flake8)
     '(9 9 info "function name should be lowercase"
         :checker python-flake8 :id "N802")
     '(10 46 warning "line too long (46 > 45 characters)" :id "E501"
          :checker python-flake8)
     '(12 29 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(12 31 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(22 1 error "undefined name 'antigravity'" :id "F821"
          :checker python-flake8))))

(flycheck-ert-def-checker-test python-flake8 python nil
  (flycheck-ert-should-syntax-check
   "checkers/python/test.py" 'python-mode
   '(5 1 warning "'antigravit' imported but unused" :id "F401"
       :checker python-flake8)
   '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
       :checker python-flake8)
   '(9 9 info "function name should be lowercase"
       :checker python-flake8 :id "N802")
   '(12 29 warning "unexpected spaces around keyword / parameter equals"
        :id "E251" :checker python-flake8)
   '(12 31 warning "unexpected spaces around keyword / parameter equals"
        :id "E251" :checker python-flake8)
   '(22 1 error "undefined name 'antigravity'" :id "F821"
        :checker python-flake8)))

(flycheck-ert-def-checker-test python-pylint python syntax-error
  (let ((flycheck-disabled-checkers '(python-flake8))
        (python-indent-guess-indent-offset nil)) ; Silence Python Mode
    (flycheck-ert-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     '(3 1 error "invalid syntax" :id "E0001" :checker python-pylint))))

(flycheck-ert-def-checker-test python-pylint python nil
  (let ((flycheck-disabled-checkers '(python-flake8)))
    (flycheck-ert-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(1 1 info "Missing module docstring" :id "C0111" :checker python-pylint)
     '(4 1 error "Unable to import 'spam'" :id "F0401" :checker python-pylint)
     '(5 1 error "No name 'antigravit' in module 'python'" :id "E0611"
         :checker python-pylint)
     '(5 1 warning "Unused import antigravit" :id "W0611"
         :checker python-pylint)
     '(7 1 info "Missing class docstring" :id "C0111" :checker python-pylint)
     '(9 5 info "Invalid method name \"withEggs\"" :id "C0103"
         :checker python-pylint)
     '(9 5 info "Missing method docstring" :id "C0111" :checker python-pylint)
     '(9 5 warning "Method could be a function" :id "R0201"
         :checker python-pylint)
     '(10 16 warning "Used builtin function 'map'" :id "W0141"
          :checker python-pylint)
     '(12 1 info "No space allowed around keyword argument assignment"
          :id "C0326" :checker python-pylint)
     '(12 5 info "Missing method docstring" :id "C0111" :checker python-pylint)
     '(12 5 warning "Method could be a function" :id "R0201"
          :checker python-pylint)
     '(14 16 error "Module 'sys' has no 'python_version' member" :id "E1101"
          :checker python-pylint)
     '(15 1 info "Unnecessary parens after u'print' keyword" :id "C0325"
          :checker python-pylint)
     '(17 1 info "Unnecessary parens after u'print' keyword" :id "C0325"
          :checker python-pylint)
     '(22 1 error "Undefined variable 'antigravity'" :id "E0602"
          :checker python-pylint))))

(flycheck-ert-def-checker-test python-pylint python disabled-warnings
  (let ((flycheck-pylintrc "pylintrc")
        (flycheck-disabled-checkers '(python-flake8)))
    (flycheck-ert-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(4 1 error "Unable to import 'spam'" :id "F0401" :checker python-pylint)
     '(5 1 error "No name 'antigravit' in module 'python'" :id "E0611"
         :checker python-pylint)
     '(5 1 warning "Unused import antigravit" :id "W0611"
         :checker python-pylint)
     '(10 16 warning "Used builtin function 'map'" :id "W0141"
          :checker python-pylint)
     '(14 16 error "Module 'sys' has no 'python_version' member" :id "E1101"
          :checker python-pylint)
     '(22 1 error "Undefined variable 'antigravity'" :id "E0602"
          :checker python-pylint))))

(flycheck-ert-def-checker-test python-pycompile python python26
  (skip-unless (executable-find "python2.6"))
  (let ((flycheck-disabled-checkers '(python-flake8 python-pylint))
        (flycheck-python-pycompile-executable "python2.6")
        ;; Silence Python Mode
        (python-indent-guess-indent-offset nil))
    (flycheck-ert-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     '(3 12 error "invalid syntax" :checker python-pycompile))))

(flycheck-ert-def-checker-test python-pycompile python python27
  (skip-unless (executable-find "python2.7"))
  (let ((flycheck-disabled-checkers '(python-flake8 python-pylint))
        (flycheck-python-pycompile-executable "python2.7")
        (python-version (flycheck-ert-extract-version-command
                         (rx line-start
                             "Python " (group (1+ not-newline))
                             line-end)
                         "python2.7" "--version"))
        (python-indent-guess-indent-offset nil))
    (flycheck-ert-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     `(3 ,(if (version< python-version "2.7.6") 12 nil)
         error "invalid syntax" :checker python-pycompile))))

(flycheck-ert-def-checker-test python-pycompile python has-no-warnings
  (let ((flycheck-disabled-checkers '(python-flake8 python-pylint)))
    (flycheck-ert-should-syntax-check
     "checkers/python/test.py" 'python-mode)))

(flycheck-ert-def-checker-test racket racket nil
  (flycheck-ert-should-syntax-check
   "checkers/racket-syntax-error.rkt" 'racket-mode
   '(4 2 error "read: expected a `)' to close `('" :checker racket)))

(flycheck-ert-def-checker-test rpm-rpmlint rpm nil
  (flycheck-ert-should-syntax-check
   "checkers/rpm-warning.spec" '(sh-mode rpm-spec-mode)
   '(1 nil warning "no-cleaning-of-buildroot %install" :checker rpm-rpmlint)
   '(1 nil warning "no-cleaning-of-buildroot %clean" :checker rpm-rpmlint)
   '(1 nil warning "no-buildroot-tag" :checker rpm-rpmlint)
   '(7 nil error "buildarch-instead-of-exclusivearch-tag x86_64"
       :checker rpm-rpmlint)
   '(22 nil warning "macro-in-%changelog %{_bindir}" :checker rpm-rpmlint)))

(ert-deftest flycheck-locate-sphinx-source-directory/not-in-a-sphinx-project ()
  :tags '(language-rst)
  (flycheck-ert-with-resource-buffer "checkers/rst.rst"
    (should-not (flycheck-locate-sphinx-source-directory))))

(ert-deftest flycheck-locate-sphinx-source-directory/in-a-sphinx-project ()
  :tags '(language-rst)
  (flycheck-ert-with-resource-buffer "checkers/rst-sphinx/index.rst"
    (should (string= (flycheck-locate-sphinx-source-directory)
                     (flycheck-ert-resource-filename "checkers/rst-sphinx/")))))

(flycheck-ert-def-checker-test rst rst nil
  (flycheck-ert-should-syntax-check
   "checkers/rst.rst" 'rst-mode
   '(8 nil warning "Title underline too short." :checker rst)
   '(14 nil error "Unexpected section title." :checker rst)
   '(16 nil error "Unknown target name: \"restructuredtext\"." :checker rst)
   '(19 nil warning "Title underline too short." :checker rst)
   '(21 nil error "Unknown target name: \"cool\"." :checker rst)
   '(26 nil error "Unexpected section title." :checker rst)))

(flycheck-ert-def-checker-test rst rst not-in-a-sphinx-project
  (flycheck-ert-with-resource-buffer "checkers/rst-sphinx/index.rst"
    (rst-mode)
    (should-not (flycheck-may-use-checker 'rst))))

(flycheck-ert-def-checker-test rst-sphinx rst nil
  (flycheck-ert-should-syntax-check
   "checkers/rst-sphinx/index.rst" 'rst-mode
   '(2 nil warning "Title underline too short." :checker rst-sphinx)
   '(9 nil error "Unknown target name: \"cool\"." :checker rst-sphinx)
   '(9 nil warning "envvar reference target not found: FOO"
       :checker rst-sphinx)))

(flycheck-ert-def-checker-test rst-sphinx rst no-reference-warnings
  (let ((flycheck-sphinx-warn-on-missing-references nil))
    (flycheck-ert-should-syntax-check
     "checkers/rst-sphinx/index.rst" 'rst-mode
     '(2 nil warning "Title underline too short." :checker rst-sphinx)
     '(9 nil error "Unknown target name: \"cool\"." :checker rst-sphinx))))

(flycheck-ert-def-checker-test rst-sphinx rst not-outside-of-a-sphinx-project
  (flycheck-ert-with-resource-buffer "checkers/rst.rst"
    (rst-mode)
    (should-not (flycheck-may-use-checker 'rst-sphinx))))

(flycheck-ert-def-checker-test ruby-rubocop ruby syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode
   '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubocop)
   '(5 24 error "unterminated string meets end of file" :checker ruby-rubocop)))

(flycheck-ert-def-checker-test ruby-rubylint ruby syntax-error
  (let ((flycheck-disabled-checkers '(ruby-rubocop)))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubylint))))

(flycheck-ert-def-checker-test ruby ruby syntax-error
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint)))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 nil error "syntax error, unexpected tCONSTANT, expecting $end"
         :checker ruby))))

(flycheck-ert-def-checker-test ruby-jruby ruby syntax-error
  :expected-result '(or (satisfies (lambda (result)
                                     (and (flycheck-ert-travis-ci-p)
                                          (ert-test-failed-p result))))
                        :passed)
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint ruby)))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 nil error "syntax error, unexpected tCONSTANT" :checker ruby-jruby))))

(flycheck-ert-def-checker-test (ruby-rubocop ruby-rubylint) ruby with-rubylint
  (flycheck-ert-should-syntax-check
   "checkers/ruby-warnings.rb" 'ruby-mode
   '(1 1 info "Use snake_case for source file names." :id "Style/FileName"
       :checker ruby-rubocop)
   '(3 1 info "Missing top-level class documentation comment."
       :id "Style/Documentation" :checker ruby-rubocop)
   '(5 5 warning "unused local variable arr" :checker ruby-rubylint)
   '(5 5 warning "Useless assignment to variable - `arr`."
       :id "Lint/UselessAssignment" :checker ruby-rubocop)
   '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
       :id "Style/StringLiterals" :checker ruby-rubocop)
   '(10 5 info "the use of then/do is not needed here" :checker ruby-rubylint)
   '(10 5 info "Use a guard clause instead of wrapping the code inside a conditional expression."
        :id "Style/GuardClause":checker ruby-rubocop)
   '(10 5 info "Favor modifier `if` usage when having a single-line body. Another good alternative is the usage of control flow `&&`/`||`."
        :id "Style/IfUnlessModifier" :checker ruby-rubocop)
   '(10 8 warning "Literal `true` appeared in a condition."
        :id "Lint/LiteralInCondition" :checker ruby-rubocop)
   '(10 13 info "Do not use `then` for multi-line `if`."
        :id "Style/MultilineIfThen" :checker ruby-rubocop)
   '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
   '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
        :checker ruby-rubylint)))

(flycheck-ert-def-checker-test ruby-rubocop ruby disabled-warning
  (let ((flycheck-rubocoprc "rubocop.yml")
        (flycheck-disabled-checkers '(ruby-rubylint)))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(1 1 info "Use snake_case for source file names."
         :id "Style/FileName" :checker ruby-rubocop)
     '(3 1 info "Missing top-level class documentation comment."
         :id "Style/Documentation" :checker ruby-rubocop)
     '(5 5 warning "Useless assignment to variable - `arr`."
         :id "Lint/UselessAssignment" :checker ruby-rubocop)
     '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
         :id "Style/StringLiterals" :checker ruby-rubocop)
     '(10 5 info "Use a guard clause instead of wrapping the code inside a conditional expression."
          :id "Style/GuardClause" :checker ruby-rubocop)
     '(10 8 warning "Literal `true` appeared in a condition."
          :id "Lint/LiteralInCondition" :checker ruby-rubocop)
     '(10 13 info "Do not use `then` for multi-line `if`."
          :id "Style/MultilineIfThen" :checker ruby-rubocop))))

(flycheck-ert-def-checker-test ruby-rubocop ruby lint-only
  (let ((flycheck-rubocop-lint-only t)
        (flycheck-disabled-checkers '(ruby-rubylint)))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(5 5 warning "Useless assignment to variable - `arr`."
         :id "Lint/UselessAssignment" :checker ruby-rubocop)
     '(10 8 warning "Literal `true` appeared in a condition."
          :id "Lint/LiteralInCondition" :checker ruby-rubocop))))

(flycheck-ert-def-checker-test ruby-rubylint ruby errors-only
  (skip-unless (version<= "2.0.2" (flycheck-test-rubylint-version)))
  (let ((flycheck-disabled-checkers '(ruby-rubocop))
        (flycheck-rubylintrc "rubylint.yml"))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
     '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
          :checker ruby-rubylint))))

(flycheck-ert-def-checker-test ruby ruby warnings
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint)))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(5 nil warning "assigned but unused variable - arr" :checker ruby)
     '(16 nil warning "possibly useless use of == in void context"
          :checker ruby))))

(flycheck-ert-def-checker-test ruby-jruby ruby ()
  :expected-result '(or (satisfies (lambda (result)
                                     (and (flycheck-ert-travis-ci-p)
                                          (ert-test-failed-p result))))
                        :passed)
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint ruby)))
    (flycheck-ert-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(16 nil warning "Useless use of == in void context."
          :checker ruby-jruby))))

(flycheck-ert-def-checker-test rust rust syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/rust-syntax-error.rs" 'rust-mode
   '(4 5 error "unresolved name `bla`" :checker rust)))

(flycheck-ert-def-checker-test rust rust test-syntax-error
  (flycheck-ert-should-syntax-check
   "checkers/rust-test-syntax-error.rs" 'rust-mode
   '(5 5 error "unresolved name `bla`" :checker rust)))

(flycheck-ert-def-checker-test rust rust test-check-tests-disabled
  (let ((flycheck-rust-check-tests nil))
    (flycheck-ert-should-syntax-check
     "checkers/rust-test-syntax-error.rs" 'rust-mode)))

(flycheck-ert-def-checker-test rust rust warning
  (flycheck-ert-should-syntax-check
   "checkers/rust-warning.rs" 'rust-mode
   '(3 1 warning "function is never used: `main`, #[warn(dead_code)] on by default"
       :checker rust)
   '(4 9 warning "unused variable: `x`, #[warn(unused_variables)] on by default"
       :checker rust)))

(flycheck-ert-def-checker-test rust rust help
  (flycheck-ert-should-syntax-check
   "checkers/rust-help.rs" 'rust-mode
   '(3 1 error "not all control paths return a value"
       :checker rust)
   '(4 8 info "consider removing this semicolon:"
       :checker rust)))

(flycheck-ert-def-checker-test rust rust test-crate-type-bin
  (let ((flycheck-rust-crate-type "bin")
        (flycheck-rust-check-tests nil))
    (flycheck-ert-should-syntax-check
     "checkers/rust-warning.rs" 'rust-mode
     '(4 9 warning "unused variable: `x`, #[warn(unused_variables)] on by default"
         :checker rust))))

(flycheck-ert-def-checker-test rust rust info
  (flycheck-ert-should-syntax-check
   "checkers/rust-info.rs" 'rust-mode
   '(11 9 info "`x` moved here because it has type `NonPOD`, which is moved by default"
        :checker rust)
   '(11 9 info "use `ref` to override" :checker rust)
   '(12 9 error "use of moved value: `x`" :checker rust)))

(flycheck-ert-def-checker-test rust rust library-path
  :expected-result :failed
  ;; TODO: How can we test this without adding binary libraries to our repo?
  (error "Not implemented!"))

(flycheck-ert-def-checker-test rust rust crate-root-not-set
  (flycheck-ert-should-syntax-check
   "checkers/rust_crate/foo.rs" 'rust-mode
   '(1 5 error "unresolved import `super::bar`" :checker rust)))

(flycheck-ert-def-checker-test rust rust crate-root
  (let ((flycheck-rust-crate-root (flycheck-ert-resource-filename
                                   "checkers/rust_crate/main.rs")))
    (flycheck-ert-should-syntax-check
     "checkers/rust_crate/foo.rs" 'rust-mode
     '(3 9 warning "unused variable: `x`, #[warn(unused_variables)] on by default"
         :checker rust))))

(flycheck-ert-def-checker-test sass sass nil
  (flycheck-ert-should-syntax-check
   "checkers/sass-error.sass" 'sass-mode
   '(5 nil error "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces."
       :checker sass)))

(flycheck-ert-def-checker-test sass sass import-error
  (flycheck-ert-should-syntax-check
   "checkers/sass-compass.sass" 'sass-mode
   `(2 nil error ,(format "File to import not found or unreadable: compass/css3.
       Load path: %s" (flycheck-ert-resource-filename "checkers"))
       :checker sass)))

(flycheck-ert-def-checker-test sass sass compass
  (let ((flycheck-sass-compass t))
    (flycheck-ert-should-syntax-check
     "checkers/sass-compass.sass" 'sass-mode)))

(flycheck-ert-def-checker-test scala scala nil
  :expected-result '(or (satisfies (lambda (result)
                                     (and (flycheck-ert-travis-ci-p)
                                          (ert-test-failed-p result))))
                        :passed)
  (flycheck-ert-should-syntax-check
   "checkers/scala-syntax-error.scala" 'scala-mode
   '(3 nil error "identifier expected but '{' found." :checker scala)))

(flycheck-ert-def-checker-test scala-scalastyle scala error
  (let ((flycheck-scalastylerc "scalastyle.xml")
        (flycheck-scalastyle-jar "/opt/scalastyle-batch_2.10-0.5.0/scalastyle-batch_2.10.jar"))
    (flycheck-ert-should-syntax-check
     "checkers/scala-scalastyle-style-error.scala" 'scala-mode
     '(6 4 error "Don't use println" :checker scala-scalastyle))))

(flycheck-ert-def-checker-test scala-scalastyle scala warning
  (let ((flycheck-scalastylerc "scalastyle.xml")
        (flycheck-scalastyle-jar "/opt/scalastyle-batch_2.10-0.5.0/scalastyle-batch_2.10.jar"))
    (flycheck-ert-should-syntax-check
     "checkers/scala-scalastyle-style-warning.scala" 'scala-mode
     '(5 8 warning "Redundant braces after class definition"
         :checker scala-scalastyle))))

(flycheck-ert-def-checker-test scala-scalastyle scala inhibited-without-jar
  (let ((flycheck-scalastylerc "scalastyle.xml"))
    (flycheck-ert-should-syntax-check
     "checkers/scala-scalastyle-style-warning.scala" 'scala-mode)))

(flycheck-ert-def-checker-test scss scss nil
  (flycheck-ert-should-syntax-check
   "checkers/scss-error.scss" 'scss-mode
   '(3 nil error "Invalid CSS after \"...    c olor: red\": expected \"{\", was \";\""
       :checker scss)))

(flycheck-ert-def-checker-test scss scss import-error
  (flycheck-ert-should-syntax-check
   "checkers/scss-compass.scss" 'scss-mode
   `(2 nil error ,(format "File to import not found or unreadable: compass/css3.
       Load path: %s" (flycheck-ert-resource-filename "checkers"))
       :checker scss)))

(flycheck-ert-def-checker-test scss scss compass
  (let ((flycheck-scss-compass t))
    (flycheck-ert-should-syntax-check
     "checkers/scss-compass.scss" 'scss-mode)))

(flycheck-ert-def-checker-test sh-bash (sh sh-bash) nil
  (flycheck-ert-should-syntax-check
   "checkers/sh-bash-syntax-error.bash" 'sh-mode
   '(5 nil error "syntax error near unexpected token `fi'" :checker sh-bash)
   '(5 nil error "`fi'" :checker sh-bash)))

(flycheck-ert-def-checker-test sh-posix-dash (sh sh-posix) nil
  (flycheck-ert-should-syntax-check
   "checkers/sh-posix-syntax-error.sh" 'sh-mode
   '(3 nil error "Syntax error: \"(\" unexpected" :checker sh-posix-dash)))

(flycheck-ert-def-checker-test sh-posix-bash (sh sh-posix) nil
  (let ((flycheck-disabled-checkers '(sh-posix-dash)))
    (flycheck-ert-should-syntax-check
     "checkers/sh-posix-syntax-error.sh" 'sh-mode
     '(3 nil error "syntax error near unexpected token `('"
         :checker sh-posix-bash)
     '(3 nil error "`cat <(echo blah)'" :checker sh-posix-bash))))

(flycheck-ert-def-checker-test sh-zsh (sh sh-zsh) nil
  (flycheck-ert-should-syntax-check
   "checkers/sh-zsh-syntax-error.zsh" 'sh-mode
   '(5 nil error "parse error near `fi'" :checker sh-zsh)))

(flycheck-ert-def-checker-test sh-shellcheck sh nil
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "checkers/sh-shellcheck.sh" 'sh-mode
   '(2 5 warning "Note that ~ does not expand in quotes." :id "SC2088"
       :checker sh-shellcheck)
   '(3 7 error "Double quote array expansions, otherwise they're like $* and break on spaces."
       :id "SC2068" :checker sh-shellcheck)
   '(4 11 info "Use $(..) instead of deprecated `..`" :id "SC2006"
       :checker sh-shellcheck)))

(flycheck-ert-def-checker-test sh-shellcheck sh excluded-warning
  :tags '(checkstyle-xml)
  (let ((flycheck-shellcheck-excluded-warnings '("SC2088")))
    (flycheck-ert-should-syntax-check
     "checkers/sh-shellcheck.sh" 'sh-mode
     '(3 7 error "Double quote array expansions, otherwise they're like $* and break on spaces."
         :id "SC2068" :checker sh-shellcheck)
     '(4 11 info "Use $(..) instead of deprecated `..`" :id "SC2006"
         :checker sh-shellcheck))))

(flycheck-ert-def-checker-test slim slim nil
  (let* ((slim-version (cadr (split-string (car (process-lines "slimrb" "-v")))))
         ;; Old Slim compilers do not report column information
         (column (if (version<= "1.3.1" slim-version) 1 nil)))
    (flycheck-ert-should-syntax-check
     "checkers/slim-error.slim" 'slim-mode
     `(2 ,column error "Unexpected indentation" :checker slim))))

(flycheck-ert-def-checker-test tex-chktex (tex latex) nil
  (flycheck-ert-should-syntax-check
   "checkers/tex-warning.tex" 'latex-mode
   '(5 29 warning "Intersentence spacing (`\\@') should perhaps be used."
       :id "13" :checker tex-chktex)))

(flycheck-ert-def-checker-test tex-lacheck (tex latex) nil
  (let ((flycheck-disabled-checkers '(tex-chktex)))
    (flycheck-ert-should-syntax-check
     "checkers/tex-warning.tex" 'latex-mode
     '(5 nil warning "missing `\\@' before `.' in \"GNU.\""
         :checker tex-lacheck)
     '(7 nil warning "possible unwanted space at \"{\""
         :checker tex-lacheck))))

(flycheck-ert-def-checker-test texinfo texinfo errors-only
  ;; Before Texinfo 5, makeinfo only prints errors
  (skip-unless (version< (flycheck-test-texinfo-version) "5"))
  (flycheck-ert-should-syntax-check
   "checkers/texinfo.texi" 'texinfo-mode
   '(7 nil error "Unknown command `bold'." :checker texinfo)
   '(7 nil error "Misplaced {." :checker texinfo)
   '(7 nil error "Misplaced }." :checker texinfo)))

(flycheck-ert-def-checker-test texinfo texinfo errors-and-warnings
  ;; Before Texinfo 5, makeinfo does not output any warnings
  (skip-unless (version<= "5" (flycheck-test-texinfo-version)))
  (flycheck-ert-should-syntax-check
   "checkers/texinfo.texi" 'texinfo-mode
   '(   3 nil warning "@settitle missing argument" :checker texinfo)
   '(7 nil error "unknown command `bold'" :checker texinfo)
   '(7 nil error "misplaced {" :checker texinfo)
   '(7 nil error "misplaced }" :checker texinfo)
   '(9 nil warning "printindex before document beginning: @printindex cp"
       :checker texinfo)))

(flycheck-ert-def-checker-test verilog-verilator verilog error
  (flycheck-ert-should-syntax-check
   "checkers/verilog_verilator_error.v" 'verilog-mode
   '(4 nil error "syntax error, unexpected ')'"
       :checker verilog-verilator)))

(flycheck-ert-def-checker-test verilog-verilator verilog warning
  (flycheck-ert-should-syntax-check
   "checkers/verilog_verilator_warning.v" 'verilog-mode
   '(2 nil warning "Signal is not driven, nor used: val"
       :checker verilog-verilator)))

(flycheck-ert-def-checker-test xml-xmlstarlet xml nil
  (flycheck-ert-should-syntax-check
   "checkers/xml-syntax-error.xml" 'nxml-mode
   '(4 10 error "Opening and ending tag mismatch: spam line 3 and with"
       :checker xml-xmlstarlet)))

(flycheck-ert-def-checker-test xml-xmllint xml nil
  (let ((flycheck-disabled-checkers '(xml-xmlstarlet)))
    (flycheck-ert-should-syntax-check
     "checkers/xml-syntax-error.xml" 'nxml-mode
     '(4 nil error "parser error : Opening and ending tag mismatch: spam line 3 and with"
         :checker xml-xmllint)
     '(5 nil error "parser error : Extra content at the end of the document"
         :checker xml-xmllint))))

(flycheck-ert-def-checker-test yaml-jsyaml yaml nil
  (flycheck-ert-should-syntax-check
   "checkers/yaml-syntax-error.yaml" 'yaml-mode
   '(4 5 error "bad indentation of a mapping entry"
       :checker yaml-jsyaml)))

(flycheck-ert-def-checker-test yaml-ruby yaml nil
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
      (flycheck-ert-should-syntax-check
       "checkers/yaml-syntax-error.yaml" 'yaml-mode expected-error))))

(flycheck-ert-initialize flycheck-test-resources-directory)

(provide 'flycheck-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-test.el ends here
