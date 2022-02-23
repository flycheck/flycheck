;;; flycheck-test.el --- Flycheck: Unit test suite   -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Flycheck contributors
;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;             fmdkdd <fmdkdd@gmail.com>
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

;; For new tests, please add the section tag.  Additionally, add any of the
;; following tags, if appropriate:
;;
;; - `external-tool' for tests which need external tools
;; - `language-LANG' for tests for the language LANG

;;; Code:


;;; Requirements

(require 'dash)
(require 'cl-lib)
(require 'ert)                          ; Unit test library
(require 'shut-up)                      ; Silence Emacs and intercept `message'

(require 'flycheck)
(require 'flycheck-ert)

;; Make a best effort to make Coq Mode available.
;; If we run Emacs with the `-Q` flag (which we do in the CI), this prevents
;; site-lisp directories to be added to `load-path'.  In addition, in the Docker
;; images we use in the CI Emacs is installed under /opt/emacs and cannot pick
;; up site-lisp directories under /usr/share/emacs.  This is why we add the
;; site-lisp directories manually for coq.
(mapc (lambda (dir)
        (add-to-list 'load-path (expand-file-name "coq/" dir)))
      '("/usr/share/emacs/site-lisp/"
        "/usr/local/share/emacs/site-lisp/"))
(autoload 'coq-mode "gallina")

;; Load ESS for R-mode (its autoloads are broken)
(require 'ess-site nil 'noerror)


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
                 '(flycheck-locate-config-file-by-path
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

(ert-deftest flycheck-same-files-p/same-files ()
  :tags '(utility)
  (let ((default-directory flycheck-test-source-directory))
    (should (flycheck-same-files-p "flycheck.el" "flycheck.el"))))

(ert-deftest flycheck-same-files-p/different-files ()
  :tags '(utility)
  (let ((default-directory flycheck-test-source-directory))
    (should-not (flycheck-same-files-p "flycheck.el" "Makefile"))))

(ert-deftest flycheck-same-files-p/file-in-non-existing-directory ()
  :tags '(utility)
  (let ((default-directory flycheck-test-source-directory))
    (should-not (flycheck-same-files-p "flycheck.el" "foobar/flycheck.el"))))

(ert-deftest flycheck-same-files-p/non-existing-files ()
  :tags '(utility)
  (let ((default-directory flycheck-test-source-directory))
    (should (flycheck-same-files-p "foobar/foobar" "foobar/foobar"))))

(ert-deftest flycheck-same-files-p/across-symlinks ()
  :tags '(utility)
  (skip-unless (fboundp #'make-symbolic-link))
  (let ((directory (make-temp-file "flycheck-test-same-files-p-" 'directory)))
    (unwind-protect
        (let ((link (expand-file-name "foobar.el" directory))
              (flycheck (expand-file-name "flycheck.el" flycheck-test-source-directory)))
          (make-symbolic-link flycheck link)
          (should (flycheck-same-files-p flycheck link)))
      (delete-directory directory 'recursive))))

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
  (flycheck-ert-with-file-buffer (locate-library "shut-up-autoloads")
    (should (flycheck-autoloads-file-p))))

(ert-deftest flycheck-autoloads-file-p/a-plain-file ()
  :tags '(utility)
  (flycheck-ert-with-file-buffer
      (expand-file-name "Cask" flycheck-test-source-directory)
    (should-not (flycheck-autoloads-file-p))))

(ert-deftest flycheck-in-user-emacs-directory-p/no-child-of-user-emacs-directory ()
  :tags '(utility)
  (let ((user-emacs-directory "/flycheck-nonexisting"))
    (should-not (flycheck-in-user-emacs-directory-p
                 (flycheck-ert-resource-filename
                  "language/emacs-lisp/warnings.el")))))

(ert-deftest flycheck-in-user-emacs-directory-p/direct-child-of-user-emacs-directory ()
  :tags '(utility)
  (let ((user-emacs-directory flycheck-test-directory))
    (should (flycheck-in-user-emacs-directory-p
             (expand-file-name "flycheck-test.el" flycheck-test-directory)))))

(ert-deftest flycheck-in-user-emacs-directory-p/indirect-child-of-user-emacs-directory ()
  :tags '(utility)
  (let ((user-emacs-directory flycheck-test-directory))
    (should (flycheck-in-user-emacs-directory-p
             (flycheck-ert-resource-filename
              "language/emacs-lisp/warnings.el")))))

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
        (file-name (flycheck-ert-resource-filename
                    "language/emacs-lisp/warnings.el")))
    (should (string= (flycheck-ert-resource-filename "language/emacs-lisp/")
                     (flycheck-module-root-directory nil file-name)))))

(ert-deftest flycheck-module-root-directory/module-name-as-string ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-ert-resource-filename "language/emacs-lisp/warnings.el")))
    (should (string= flycheck-test-resources-directory
                     (flycheck-module-root-directory "language.emacs-lisp.warnings"
                                                     file-name)))))

(ert-deftest flycheck-module-root-directory/module-name-as-list ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-ert-resource-filename "language/emacs-lisp/warnings.el")))
    (should (string= flycheck-test-resources-directory
                     (flycheck-module-root-directory '("language" "emacs-lisp"
                                                       "warnings")
                                                     file-name)))))

(ert-deftest flycheck-module-root-directory/mismatching-module-name ()
  :tags '(utility)
  (let ((default-directory flycheck-test-resources-directory)
        (file-name (flycheck-ert-resource-filename "language/emacs-lisp/warnings.el")))
    (should (string= (flycheck-ert-resource-filename "language/emacs-lisp/")
                     (flycheck-module-root-directory '("foo" "warnings")
                                                     file-name)))))

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

(ert-deftest flycheck-checker-get/modes ()
  :tags '(checker-api)
  (dolist (checker flycheck-checkers)
    (should (listp (flycheck-checker-get checker 'modes)))
    (should (-all? #'symbolp (flycheck-checker-get checker 'modes)))))

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
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (flycheck-may-use-checker 'emacs-lisp)
    (let ((flycheck-disabled-checkers '(emacs-lisp)))
      (should-not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest flycheck-may-use-checker/checks-executable ()
  :tags '(checker-api)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (let* ((was-called nil)
           (flycheck-executable-find (lambda (_) (setq was-called t))))
      (should (flycheck-may-use-checker 'emacs-lisp))
      (should was-called))))

(ert-deftest flycheck-may-enable-checker/emacs-lisp ()
  :tags '(checker-api)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (should (flycheck-may-enable-checker 'emacs-lisp))
    (should-not (flycheck-may-enable-checker 'gibberish))
    (should (equal '(emacs-lisp) flycheck--automatically-enabled-checkers))))

(ert-deftest flycheck-may-enable-checker/respects-cache ()
  :tags '(checker-api)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (cl-letf* ((counter 0)
               (enabled t)
               ((flycheck-checker-get 'emacs-lisp 'enabled)
                (lambda (&rest _ignore) (cl-incf counter) enabled)))
      ;; :enabled isn't called when a positive cached result is available
      (let ((flycheck--automatically-enabled-checkers '(emacs-lisp)))
        (should (flycheck-may-enable-checker 'emacs-lisp))
        (should (= counter 0)))
      ;; :enabled isn't called when a negative cached result is available
      (let ((flycheck--automatically-disabled-checkers '(emacs-lisp)))
        (should (not (flycheck-may-enable-checker 'emacs-lisp)))
        (should (= counter 0)))
      ;; Returning a cached result doesn't change caches
      (should (eq flycheck--automatically-disabled-checkers nil))
      (should (eq flycheck--automatically-enabled-checkers nil))
      ;; :enabled should only be called once with positive results
      (setq enabled t)
      (dotimes (_ 10) (flycheck-may-enable-checker 'emacs-lisp))
      (should (equal flycheck--automatically-enabled-checkers '(emacs-lisp)))
      (setq flycheck--automatically-enabled-checkers nil)
      (should (= counter 1))
      ;; :enabled should only be called once with negative results
      (setq enabled nil counter 0)
      (dotimes (_ 10) (flycheck-may-enable-checker 'emacs-lisp))
      (should (equal flycheck--automatically-disabled-checkers '(emacs-lisp)))
      (should (= counter 1)))))


;;; Generic syntax checkers
(ert-deftest flycheck-checker-get/gets-a-property ()
  :tags '(generic-checkers)
  (should (equal (flycheck-checker-get 'emacs-lisp 'modes)
                 '(emacs-lisp-mode lisp-interaction-mode))))

(ert-deftest flycheck-checker-get/setf ()
  :tags '(generic-checkers)
  (let ((checker 'bar)
        (property 'foo))
    (should-not (flycheck-checker-get checker property))
    (setf (flycheck-checker-get checker property) "Hello world")
    (unwind-protect
        (should (equal (flycheck-checker-get checker property)
                       "Hello world"))
      (put checker 'flycheck-foo nil))))

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
(ert-deftest flycheck-remove-next-checker/no-valid-checker ()
  :tags '(extending)
  (let ((err-data (should-error (flycheck-remove-next-checker 'foo 'emacs-lisp))))
    (should (string= (cadr err-data) "foo is not a valid syntax checker"))))

(ert-deftest flycheck-remove-next-checker/plain ()
  :tags '(extending)
  (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
    (flycheck-remove-next-checker 'emacs-lisp 'emacs-lisp-checkdoc)
    (unwind-protect
        (should (equal (flycheck-checker-get 'emacs-lisp 'next-checkers) nil))
      (put 'emacs-lisp 'flycheck-next-checkers next-checkers))))

(ert-deftest flycheck-remove-next-checker/level ()
  :tags '(extending)
  (let ((next-checkers (flycheck-checker-get 'sh-zsh 'next-checkers)))
    (flycheck-remove-next-checker 'sh-zsh 'sh-shellcheck)
    (unwind-protect
        (should (equal (flycheck-checker-get 'sh-zsh 'next-checkers) nil))
      (put 'sh-zsh 'flycheck-next-checkers next-checkers))))

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

(ert-deftest flycheck-add-next-checker/redundant ()
  :tags '(extending)
  (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
    (flycheck-add-next-checker 'emacs-lisp 'texinfo)
    (flycheck-add-next-checker 'emacs-lisp 'texinfo)
    (flycheck-add-next-checker 'emacs-lisp '(t . texinfo))
    (flycheck-add-next-checker 'emacs-lisp '(warning . texinfo))
    (unwind-protect
        (should (equal (flycheck-checker-get 'emacs-lisp 'next-checkers)
                       (cons '(warning . texinfo) next-checkers)))
      (put 'emacs-lisp 'flycheck-next-checkers next-checkers))))

(ert-deftest flycheck-add-next-checker/prepend ()
  :tags '(extending)
  (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
    (flycheck-add-next-checker 'emacs-lisp 'texinfo)
    (unwind-protect
        (should (equal (flycheck-checker-get 'emacs-lisp 'next-checkers)
                       (cons 'texinfo next-checkers)))
      (put 'emacs-lisp 'flycheck-next-checkers next-checkers))))

(ert-deftest flycheck-add-next-checker/append ()
  :tags '(extending)
  (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
    (flycheck-add-next-checker 'emacs-lisp 'texinfo 'append)
    (unwind-protect
        (should (equal (flycheck-checker-get 'emacs-lisp 'next-checkers)
                       (append next-checkers '(texinfo))))
      (put 'emacs-lisp 'flycheck-next-checkers next-checkers))))

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
  (let ((modes (flycheck-checker-get 'python-pylint 'modes)))
    (flycheck-add-mode 'python-pylint 'emacs-lisp-mode)
    (unwind-protect
        (progn
          (should (equal (flycheck-checker-get 'python-pylint 'modes)
                         (cons 'emacs-lisp-mode modes))))
      (put 'python-pylint 'flycheck-modes modes)
      (should (equal (flycheck-checker-get 'python-pylint 'modes) modes)))))


;;; Syntax checking mode
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
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (goto-char (point-min))
    (insert "foo-bar")
    (flycheck-mode)
    (flycheck-ert-buffer-sync)
    (should flycheck-current-errors)
    (let ((hack-local-variables-hook))
      (revert-buffer 'ignore-auto 'no-confirm))
    (should-not flycheck-current-errors)
    (should-not (flycheck-deferred-check-p))))


;;; Syntax checker selection for the current buffer
(ert-deftest flycheck-checker/unusable-checker-causes-an-error ()
  :tags '(selection)
  (flycheck-ert-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-mode)
    (let* ((flycheck-checker 'sh-bash))
      (flycheck-buffer)
      (should (eq flycheck-checker 'sh-bash))
      (should (string= flycheck-last-status-change 'no-checker)))))

(ert-deftest flycheck-checker/usable-checker-is-used ()
  :tags '(selection language-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checker 'emacs-lisp-checkdoc))
      (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
      (flycheck-ert-buffer-sync)
      (flycheck-ert-should-errors
       '(12 nil info "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)))))

(ert-deftest flycheck-checker/disabled-checker-is-not-used ()
  :tags '(selection language-emacs-lisp)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (let ((flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))
      (should-not (flycheck-get-checker-for-buffer))
      (let* ((flycheck-checker 'emacs-lisp))
        (flycheck-buffer)
        (should (eq flycheck-checker 'emacs-lisp))
        (should (string= flycheck-last-status-change 'no-checker))))))

(ert-deftest flycheck-checker/unregistered-checker-is-used ()
  :tags '(selection language-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checkers (remq 'emacs-lisp-checkdoc flycheck-checkers)))
      (should-not (flycheck-registered-checker-p 'emacs-lisp-checkdoc))
      (let ((flycheck-checker 'emacs-lisp-checkdoc))
        (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
        (flycheck-ert-buffer-sync)
        (flycheck-ert-should-errors
         '(12 nil info "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))))))

(ert-deftest flycheck-select-checker/selecting-sets-the-syntax-checker ()
  :tags '(selection checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (should (eq flycheck-checker 'emacs-lisp-checkdoc))))

(ert-deftest flycheck-select-checker/unselecting-unsets-the-syntax-checker ()
  :tags '(selection checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (flycheck-select-checker nil)
    (should-not flycheck-checker)))

(ert-deftest flycheck-select-checker/selecting-runs-a-syntax-check ()
  :tags '(selection language-emacs-lisp
                    checker-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    ;; By default we should get both, because emacs-lisp chains to checkdoc
    (flycheck-ert-buffer-sync)
    (dolist (err flycheck-current-errors)
      (should (memq (flycheck-error-checker err)
                    '(emacs-lisp emacs-lisp-checkdoc))))
    ;; We select checkdoc, and should now only have checkdoc errors
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (flycheck-ert-wait-for-syntax-checker)
    (dolist (err flycheck-current-errors)
      (should (eq (flycheck-error-checker err) 'emacs-lisp-checkdoc)))))

(ert-deftest flycheck-select-checker/unselecting-a-checker-goes-back-to-automatic-selection ()
  :tags '(selection language-emacs-lisp
                    checker-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (flycheck-select-checker 'emacs-lisp-checkdoc)
    (should (eq flycheck-checker 'emacs-lisp-checkdoc))
    (flycheck-ert-wait-for-syntax-checker)
    (dolist (err flycheck-current-errors)
      (should (eq (flycheck-error-checker err) 'emacs-lisp-checkdoc)))
    (flycheck-select-checker nil)
    (should-not flycheck-checker)
    (flycheck-ert-wait-for-syntax-checker)
    (dolist (err flycheck-current-errors)
      (should (memq (flycheck-error-checker err)
                    '(emacs-lisp emacs-lisp-checkdoc))))))

(ert-deftest flycheck/selects-checker-automatically/no-disabled-checker ()
  :tags '(selection language-emacs-lisp checker-emacs-lisp-checkdoc)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (let ((flycheck-disabled-checkers '(emacs-lisp)))
      (flycheck-ert-buffer-sync)
      (should-not flycheck-checker)
      (flycheck-ert-should-errors
       '(12 nil info "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)))))

(ert-deftest flycheck-disable-checker/disables-checker ()
  :tags '(selection)
  (flycheck-ert-with-temp-buffer
    (flycheck-mode)
    (with-no-warnings
      (flycheck-disable-checker 'emacs-lisp))
    (should (equal '(emacs-lisp) flycheck-disabled-checkers))
    (should-not (default-value 'flycheck-disabled-checkers))
    ;; Disabling a disabled checker should be a no-op
    (with-no-warnings
      (flycheck-disable-checker 'emacs-lisp))
    (should (equal '(emacs-lisp) flycheck-disabled-checkers))))

(ert-deftest flycheck-disable-checker/enables-checker ()
  :tags '(selection)
  (flycheck-ert-with-temp-buffer
    (flycheck-mode)
    (setq flycheck-disabled-checkers '(emacs-lisp python-pylint))
    (with-no-warnings
      (flycheck-disable-checker 'emacs-lisp 'enable))
    (should (equal '(python-pylint) flycheck-disabled-checkers))))


;;; Automatic syntax checking in a buffer
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
      (should-not (flycheck-deferred-check-p))
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

(ert-deftest flycheck-check-syntax-automatically/idle-change-checks-changed-buffer ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '(idle-change))
        (flycheck-idle-change-delay 0.1))
    (flycheck-ert-with-resource-buffer "automatic-check-dummy.el"
      (let ((changed-buffer (current-buffer)))
        (emacs-lisp-mode)
        (flycheck-mode)
        (insert "Hello world")
        (switch-to-buffer "other-dummy2.el")
        (emacs-lisp-mode)
        (flycheck-mode)
        (sleep-for 0.2)
        (should-not (flycheck-deferred-check-p))
        (set-buffer changed-buffer)
        (should (flycheck-deferred-check-p))))))

(ert-deftest flycheck-check-syntax-automatically/does-not-check-after-buffer-switch-by-default ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '())
        (flycheck-idle-buffer-switch-delay 0)
        (checks 0))
    (find-file (flycheck-ert-resource-filename "automatic-check-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

    (switch-to-buffer "*scratch*")
    (switch-to-buffer "automatic-check-dummy.el")
    (sleep-for 0.1)
    (should (= checks 0))
    (kill-buffer "automatic-check-dummy.el")))

(ert-deftest flycheck-check-syntax-automatically/idle-buffer-switch-checks-after-buffer-switch ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '(idle-buffer-switch))
        (flycheck-idle-buffer-switch-delay 0)
        (checks 0))
    (find-file (flycheck-ert-resource-filename "automatic-check-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

    (switch-to-buffer "*scratch*")
    (switch-to-buffer "automatic-check-dummy.el")
    (sleep-for 0.1)
    (should (= checks 1))
    (kill-buffer "automatic-check-dummy.el")))

(ert-deftest flycheck-check-syntax-automatically/idle-change-cancels-idle-buffer-switch ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
        (flycheck-idle-change-delay 0.02)
        (flycheck-idle-buffer-switch-delay 0.01)
        (checks 0))
    (find-file (flycheck-ert-resource-filename "automatic-check-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

    (switch-to-buffer "*scratch*")
    (switch-to-buffer "automatic-check-dummy.el")
    (insert "Hello")
    (sleep-for 0.015)
    (should (= checks 0))
    (sleep-for 0.01)
    (should (= checks 1))
    (kill-buffer "automatic-check-dummy.el")))

(ert-deftest flycheck-check-syntax-automatically/idle-buffer-switch-cancels-idle-change ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
        (flycheck-idle-change-delay 0.01)
        (flycheck-idle-buffer-switch-delay 0.05)
        (checks 0))
    (find-file (flycheck-ert-resource-filename "automatic-check-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

    (insert "Hello")
    (switch-to-buffer "*scratch*")
    (switch-to-buffer "automatic-check-dummy.el")
    (sleep-for 0.02)
    (should (= checks 0))
    (sleep-for 0.06)
    (should (= checks 1))
    (kill-buffer "automatic-check-dummy.el")))

(ert-deftest flycheck-check-syntax-automatically/idle-buffer-switch-does-not-check-intermediate-buffers-by-default ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '(idle-buffer-switch))
        (flycheck-idle-buffer-switch-delay 0.01)
        (checks 0))
    (find-file (flycheck-ert-resource-filename "automatic-check-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (find-file (flycheck-ert-resource-filename "global-mode-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

    (switch-to-buffer "*scratch*")
    (switch-to-buffer "automatic-check-dummy.el")
    (switch-to-buffer "global-mode-dummy.el")
    (sleep-for 0.015)
    (should (= checks 1))
    ;; Since the buffer is not visible, the check would be automatically deferred
    (set-buffer "automatic-check-dummy.el")
    (should-not (flycheck-deferred-check-p))
    (kill-buffer "automatic-check-dummy.el")
    (kill-buffer "global-mode-dummy.el")))

(ert-deftest flycheck-check-syntax-automatically/idle-buffer-switch-checks-intermediate-buffers-with-option ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '(idle-buffer-switch))
        (flycheck-idle-buffer-switch-delay 0.01)
        (flycheck-buffer-switch-check-intermediate-buffers t)
        (checks 0))
    (find-file (flycheck-ert-resource-filename "automatic-check-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (find-file (flycheck-ert-resource-filename "global-mode-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)
    (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

    (switch-to-buffer "*scratch*")
    (switch-to-buffer "automatic-check-dummy.el")
    (switch-to-buffer "global-mode-dummy.el")
    (sleep-for 0.015)
    (should (= checks 1))
    ;; Since the buffer is not visible, the check will be automatically deferred
    (set-buffer "automatic-check-dummy.el")
    (should (flycheck-deferred-check-p))
    (kill-buffer "automatic-check-dummy.el")
    (kill-buffer "global-mode-dummy.el")))

(ert-deftest flycheck-check-syntax-automatically/buffer-switch-check-intermediate-buffers-does-not-cancel-idle-change ()
  :tags '(automatic)
  (let ((flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
        (flycheck-buffer-switch-check-intermediate-buffers t)
        (flycheck-idle-change-delay 0.01)
        (flycheck-idle-buffer-switch-delay 0))
    (find-file (flycheck-ert-resource-filename "automatic-check-dummy.el"))
    (emacs-lisp-mode)
    (flycheck-mode)

    (insert "Hello")
    (switch-to-buffer "*scratch*")
    (sleep-for 0.015)
    ;; Since the buffer is not visible, the check will be automatically deferred
    (set-buffer "automatic-check-dummy.el")
    (should (flycheck-deferred-check-p))
    (kill-buffer "automatic-check-dummy.el")))

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
    (rename-buffer "foo")
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


;;; Errors from syntax checks
(ert-deftest flycheck-error-line-region ()
  :tags '(error-api)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World\n")
    (should (equal (flycheck--line-region (flycheck-line-column-to-position 1 1)) '(1 . 6)))
    (should (equal (flycheck--line-region (flycheck-line-column-to-position 2 4)) '(11 . 16)))
    ;; An error column beyond the end of the line is simply ignored just like
    ;; all other error columns
    (should (equal (flycheck--line-region (flycheck-line-column-to-position 2 10)) '(11 . 16)))
    ;; An error line beyond the end of file should highlight the last line
    (should (equal (flycheck--line-region (flycheck-line-column-to-position 4 3)) '(16 . 17)))))

(ert-deftest flycheck-error-column-region ()
  :tags '(error-api)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World\n")
    (should (equal (flycheck--column-region (flycheck-line-column-to-position 1 4))
                   '(4 . 5)))
    (should (equal (flycheck--column-region (flycheck-line-column-to-position 2 6))
                   '(12 . 13)))
    ;; A column beyond the end of a line
    (should (equal (flycheck--column-region (flycheck-line-column-to-position 1 7))
                   '(6 . 7)))
    ;; A column right at the end of the last empty line of a file (an important
    ;; special case, because the Emacs Lisp checker reports undefined functions
    ;; at this place!)
    (should (equal (flycheck--column-region (flycheck-line-column-to-position 3 1))
                   '(16 . 17)))
    ;; A column beyond the end of file
    (should (equal (flycheck--column-region (flycheck-line-column-to-position 4 2))
                   '(16 . 17)))))

(ert-deftest flycheck-bounds-of-things-at-point ()
  :tags '(error-api)
  (flycheck-ert-with-temp-buffer
    (insert "    (message)\n    (message")
    (emacs-lisp-mode)
    (should-not (flycheck-bounds-of-thing-at-point
                 'sexp (flycheck-line-column-to-position 1 2)))
    (should (equal (flycheck-bounds-of-thing-at-point
                    'sexp (flycheck-line-column-to-position 1 5))
                   '(5 . 14)))
    (should-not (flycheck-bounds-of-thing-at-point
                 'symbol (flycheck-line-column-to-position 1 5)))
    (should (equal (flycheck-bounds-of-thing-at-point
                    'sexp (flycheck-line-column-to-position 1 8))
                   '(6 . 13)))
    (should (equal (flycheck-bounds-of-thing-at-point
                    'symbol (flycheck-line-column-to-position 1 8))
                   '(6 . 13)))
    ;; An incomplete expression
    (should-not (flycheck-bounds-of-thing-at-point
                 'sexp (flycheck-line-column-to-position 2 5)))))

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
        (should (equal (flycheck-error-region-for-mode err mode) '(5 . 29)))))
    ;; With end-line and end-col set, MODE shouldn't make a difference
    (should (equal (flycheck-error-region-for-mode
                    (flycheck-error-new-at
                     1 18 nil nil :end-line 1 :end-column 29)
                    nil)
                   '(18 . 29)))
    (should (equal (flycheck-error-region-for-mode
                    (flycheck-error-new-at
                     1 14 nil nil :end-line 1 :end-column 18)
                    nil)
                   '(14 . 18)))
    (should (equal (flycheck-error-region-for-mode
                    (flycheck-error-new-at ;;
                     1 5 nil nil :end-line 2 :end-column 13)
                    nil)
                   '(5 . 42)))
    (should (equal (flycheck-error-region-for-mode
                    (flycheck-error-new-at ;; Missing end column
                     1 nil nil nil :end-line 2)
                    'lines)
                   '(5 . 29)))
    (should (equal (flycheck-error-region-for-mode
                    (flycheck-error-new-at ;; Implicit end line
                     2 5 nil nil :end-column 13)
                    nil)
                   '(34 . 42)))
    (should (equal (flycheck-error-region-for-mode
                    (flycheck-error-new-at ;; Out of bounds start
                     10 1 nil nil :end-line 10 :end-column 3)
                    nil)
                   '(41 . 42)))
    (should (equal (flycheck-error-region-for-mode
                    (flycheck-error-new-at ;; Out of bounds end
                     1 1 nil nil :end-line 10 :end-column 1)
                    nil)
                   '(1 . 42)))))

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

(ert-deftest flycheck-error-format-position ()
  :tags '(error-api)
  (cl-flet ((fmt
             (l c el ec)
             (flycheck-error-format-position
              (flycheck-error-new-at
               l c 'error "err" :end-line el :end-column ec
               :checker 'emacs-lisp))))
    (should (string= (fmt 14 nil nil nil) "14"))
    (should (string= (fmt 14 nil nil 1)   "14"))
    (should (string= (fmt 14 nil 14  nil) "14"))
    (should (string= (fmt 14 nil 15  nil) "14-15"))
    (should (string= (fmt 14 nil 15  1)   "14-15"))
    (should (string= (fmt 14 1   nil nil) "14:1"))
    (should (string= (fmt 14 1   nil 2)   "14:1"))
    (should (string= (fmt 14 1   nil 1)   "14:1-1"))
    (should (string= (fmt 14 1   14  nil) "14:1"))
    (should (string= (fmt 14 1   14  2)   "14:1"))
    (should (string= (fmt 14 1   14  1)   "14:1-1"))
    (should (string= (fmt 14 1   14  5)   "14:1-5"))
    (should (string= (fmt 14 1   15  5)   "(14:1)-(15:5)"))))

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

(ert-deftest flycheck-related-errors ()
  :tags '(error-api)
  (let ((flycheck-current-errors
         (list (flycheck-error-new-at 5 7 'error "foo" :checker 'a :group 1)
               (flycheck-error-new-at 8 9 'error "bar" :checker 'a :group 2)
               (flycheck-error-new-at 1 4 'error "gul" :checker 'a :group 1)
               (flycheck-error-new-at 4 5 'error "lag" :checker 'b :group 1))))
    (should (equal (flycheck-related-errors (nth 0 flycheck-current-errors))
                   (list (nth 0 flycheck-current-errors)
                         (nth 2 flycheck-current-errors))))
    (should (equal (flycheck-related-errors (nth 1 flycheck-current-errors))
                   (list (nth 1 flycheck-current-errors))))))


;;; Errors in the current buffer

(ert-deftest flycheck-fill-and-expand-error-file-names ()
  :tags '(errors)
  (flycheck-ert-with-resource-buffer "global-mode-dummy.el"
    (let* ((absolute-fn (flycheck-ert-resource-filename "substitute-dummy"))
           (cwd (file-name-directory absolute-fn))
           (relative-fn (file-name-nondirectory absolute-fn))
           (errors (list (flycheck-error-new :filename "foo")
                         (flycheck-error-new :filename absolute-fn)
                         (flycheck-error-new :filename relative-fn)
                         (flycheck-error-new :filename nil))))
      (should (equal (mapcar #'flycheck-error-filename
                             (flycheck-fill-and-expand-error-file-names errors
                                                                        cwd))
                     (list (flycheck-ert-resource-filename "foo")
                           absolute-fn
                           absolute-fn
                           (flycheck-ert-resource-filename
                            "global-mode-dummy.el")))))))


;;; Status reporting for the current buffer
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


;;; Error levels
;; A level for the following unit tests
(flycheck-define-error-level 'test-level
  :severity 1337
  :overlay-category 'category
  :margin-spec ">>"
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

(ert-deftest flycheck-error-level-make-indicator/has-margin-spec ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-indicator
                      'test-level 'left-margin))
               (`(_ ,spec) (get-text-property 0 'display icon)))
    (should (equal spec ">>"))))

(ert-deftest flycheck-error-level-make-indicator/has-fringe-bitmap ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-indicator
                      'test-level 'left-fringe))
               (`(_ ,bitmap _) (get-text-property 0 'display icon)))
    (should (eq bitmap 'left-triangle))))

(ert-deftest flycheck-error-level-make-indicator/has-fringe-face ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-indicator 'test-level 'left-fringe))
               (`(_ _ ,face) (get-text-property 0 'display icon)))
    (should (eq face 'highlight))))

(ert-deftest flycheck-error-level-make-indicator/left-fringe ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-indicator 'test-level 'left-fringe))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (eq side 'left-fringe))))

(ert-deftest flycheck-error-level-make-indicator/right-fringe ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-indicator 'test-level 'right-fringe))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (eq side 'right-fringe))))

(ert-deftest flycheck-error-level-make-indicator/left-margin ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-indicator 'test-level 'left-margin))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (equal side '(margin left-margin)))))

(ert-deftest flycheck-error-level-make-indicator/right-margin ()
  :tags '(error-level)
  (pcase-let* ((icon (flycheck-error-level-make-indicator 'test-level 'right-margin))
               (`(,side _ _) (get-text-property 0 'display icon)))
    (should (equal side '(margin right-margin)))))

(ert-deftest flycheck-error-level-make-indicator/invalid-side ()
  :tags '(error-level)
  (let ((err (should-error (flycheck-error-level-make-indicator 'test-level
                                                                'up-fringe))))
    (should (string= (cadr err) "Invalid fringe side: up-fringe"))))


;;; Built-in error levels
(ert-deftest flycheck-error-level-error ()
  :tags '(error-level)
  (should (= (flycheck-error-level-severity 'error) 100))
  (should (eq (flycheck-error-level-fringe-bitmap 'error)
              'flycheck-fringe-bitmap-double-arrow))
  (should (eq (flycheck-error-level-fringe-face 'error)
              'flycheck-fringe-error))
  (should (eq (flycheck-error-level-overlay-category 'error)
              'flycheck-error-overlay))
  (should (eq (flycheck-error-level-error-list-face 'error)
              'flycheck-error-list-error)))

(ert-deftest flycheck-error-level-warning ()
  :tags '(error-level)
  (should (= (flycheck-error-level-severity 'warning) 10))
  (should (eq (flycheck-error-level-fringe-bitmap 'warning)
              'flycheck-fringe-bitmap-double-arrow))
  (should (eq (flycheck-error-level-fringe-face 'warning)
              'flycheck-fringe-warning))
  (should (eq (flycheck-error-level-overlay-category 'warning)
              'flycheck-warning-overlay))
  (should (eq (flycheck-error-level-error-list-face 'warning)
              'flycheck-error-list-warning)))

(ert-deftest flycheck-error-level-info ()
  :tags '(error-level)
  (should (= (flycheck-error-level-severity 'info) -10))
  (should (eq (flycheck-error-level-fringe-bitmap 'info)
              'flycheck-fringe-bitmap-double-arrow))
  (should (eq (flycheck-error-level-fringe-face 'info)
              'flycheck-fringe-info))
  (should (eq (flycheck-error-level-overlay-category 'info)
              'flycheck-info-overlay))
  (should (eq (flycheck-error-level-error-list-face 'info)
              'flycheck-error-list-info)))


;;; Error analysis
(ert-deftest flycheck-has-max-errors-p ()
  :tags '(error-analysis)
  (should (flycheck-has-max-errors-p nil 'error))
  (let ((errors (list (flycheck-error-new-at 10 10 'warning)
                      (flycheck-error-new-at 10 10 'info))))
    (should (flycheck-has-max-errors-p errors 'error))
    (should (flycheck-has-max-errors-p errors 'warning))
    (should-not (flycheck-has-max-errors-p errors 'info))))


;;; Error overlays in the current buffer
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
      (should (eq (overlay-get overlay 'help-echo) #'flycheck-help-echo)))))

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
      (should (eq bitmap 'flycheck-fringe-bitmap-double-arrow)))))

(ert-deftest flycheck-add-overlay/has-warning-fringe-icon ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'warning)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-warning))
      (should (eq bitmap 'flycheck-fringe-bitmap-double-arrow)))))

(ert-deftest flycheck-add-overlay/has-error-fringe-icon ()
  :tags '(overlay)
  (flycheck-ert-with-temp-buffer
    (insert "Hello\n    World")
    (pcase-let* ((overlay (flycheck-add-overlay
                           (flycheck-error-new-at 1 1 'error)))
                 (before-string (overlay-get overlay 'before-string))
                 (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
      (should (eq face 'flycheck-fringe-error))
      (should (eq bitmap 'flycheck-fringe-bitmap-double-arrow)))))

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
  :tags '(overlay language-emacs-lisp
                  checker-emacs-lisp checker-emacs-lisp-checkdoc)
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
     '(12 nil info "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(15 1 warning "`message' called with 0 args to fill 1 format field(s)"
          :checker emacs-lisp))))

(ert-deftest flycheck-add-overlay/help-echo-is-error-message ()
  :tags '(overlay)
  "Check for default help at point."
  (flycheck-ert-with-temp-buffer
    (insert " ")
    (goto-char 1)
    (flycheck-add-overlay (flycheck-error-new-at 1 1 'info "A bar message"))
    (should (string= (help-at-pt-string) "A bar message"))))

(ert-deftest flycheck-add-overlay/can-suppress-help-echo ()
  :tags '(overlay)
  "Check that setting help-echo function to nil removes help echoes."
  (flycheck-ert-with-temp-buffer
    (insert " ")
    (goto-char 1)
    (flycheck-add-overlay (flycheck-error-new-at 1 1 'info "info"))
    (let ((flycheck-help-echo-function nil))
      (should (string= (help-at-pt-string) nil)))))

(ert-deftest flycheck-add-overlay/help-echo-for-nil-message-is-default ()
  :tags '(overlay)
  "Check that null error messages are replaced by 'Unkown [level]'."
  (flycheck-ert-with-temp-buffer
    (insert " ")
    (goto-char 1)
    (flycheck-add-overlay (flycheck-error-new-at 1 1 'info))
    (should (string= (help-at-pt-string) "Unknown info"))))

(ert-deftest flycheck-add-overlay/help-echo-stacks-errors ()
  :tags '(overlay)
  "Check that help-echo messages contain all error messages at point."
  (flycheck-ert-with-temp-buffer
    (insert " ")
    (goto-char 1)
    (flycheck-add-overlay (flycheck-error-new-at 1 1 'info "info"))
    (flycheck-add-overlay (flycheck-error-new-at 1 1 'warning "warning"))
    (flycheck-add-overlay (flycheck-error-new-at 1 1 'error "error"))
    (should (string= (help-at-pt-string) "info\nwarning\nerror"))))

(ert-deftest flycheck-add-overlay/help-echo-stacks-errors-region ()
  :tags '(overlay)
  "Check that help-echo messages include snippets when available."
  (flycheck-ert-with-temp-buffer
    (insert "int main() {}")
    (goto-char 5)
    (flycheck-add-overlay
     (flycheck-error-new-at 1 1 'info "info" :end-column 14))
    (flycheck-add-overlay
     (flycheck-error-new-at 1 5 'warning "warning" :end-column 9))
    (flycheck-add-overlay
     (flycheck-error-new-at 1 5 'error "error" :end-column 11))
    (let ((text-quoting-style 'grave))
      (should (string=
               (help-at-pt-string)
               "`int main() {}': info\n`main': warning\n`main()': error")))))


;;; Error navigation in the current buffer
(defmacro flycheck-test-with-nav-buffer (minimum-level &rest body)
  "With MINIMUM-LEVEL, eval BODY in a temporary buffer for navigation.

Set `flycheck-navigation-minimum-level' to MINIMUM-LEVEL while
evaluating BODY."
  (declare (indent 1))
  `(flycheck-ert-with-resource-buffer "language/emacs-lisp/errors-and-warnings.el"
     (emacs-lisp-mode)
     (flycheck-mode)
     (let ((flycheck-navigation-minimum-level ,minimum-level))
       (flycheck-ert-buffer-sync)
       (goto-char (point-min))
       ,@body)))

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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer nil
    (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
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
    (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/errors-beyond-last-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-next-error 2) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-errors/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error -2) :type 'user-error)))
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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/minimum-level/ignores-minimum-level-if-no-error-beyond-it ()
  :tags '(navigation)
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (let ((flycheck-navigation-minimum-level 'error))
      (flycheck-ert-buffer-sync)
      (goto-char (point-min))
      (flycheck-next-error 1)
      (should (flycheck-ert-at-nth-error 1)))))

(ert-deftest flycheck-previous-error/over-errors/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
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
    (let ((err (should-error (flycheck-previous-error -2) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-errors/navigate-back-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (let ((err (should-error (flycheck-previous-error -2) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-errors/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'error
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
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
    (let ((err (should-error (flycheck-first-error 2) :type 'user-error)))
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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-warnings/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-warnings/navigate-by-two-errors ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-warnings/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'warning
    (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
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
    (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/over-informational/errors-when-moving-too-far ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
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
    (let ((err (should-error (flycheck-next-error) :type 'user-error)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/over-informational/errors-before-first-error ()
  :tags '(navigation)
  (flycheck-test-with-nav-buffer 'info
    (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
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
    (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
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


;;; Displaying errors in buffers
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


;;; Functions to display errors
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


;;; Syntax checkers using external commands
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

(ert-deftest flycheck-start-command-checker/wraps-command ()
  :tags '(command-checker)
  (let* ((was-called 0)
         (flycheck-command-wrapper-function (lambda (cmd)
                                              (cl-incf was-called)
                                              (cons "echo" cmd))))
    ;; Since we just `echo' the command, there should be zero errors
    (flycheck-ert-should-syntax-check
     "language/emacs-lisp/warnings.el" 'emacs-lisp-mode)

    ;; Called once for `emacs-lisp', and a second time for checkdoc
    (should (equal was-called 2))))

(define-derived-mode truncated-stdin-mode prog-mode "trunc")

(flycheck-define-command-checker 'truncated-stdin
  "Reply with an error after reading after reading 12345 bytes."
  ;; This checker will close its stdin before we can write all of our input.
  :command `(;; CI machines have python3 as "python"
             ,(or (executable-find "python3") "python")
             "-c" "import sys; sys.stdin.close(); print('stdin:1:1:error')")
  :error-patterns '((error bol "stdin" ":" line ":" column ":" (message)))
  :modes '(truncated-stdin-mode)
  :standard-input t)

(defconst flycheck-test--truncated-stdin
  (symbol-plist 'truncated-stdin))

;; Forget about this checker, otherwise later tests that ensure that all
;; checkers are registered and documented documentation and fail
(setf (symbol-plist 'truncated-stdin) nil)

;; https://github.com/flycheck/flycheck/issues/1278
(ert-deftest flycheck-start-command-checker/truncated-stdin-with-errors ()
  :tags '(command-checker)
  (cl-letf* ((flycheck-checker 'truncated-stdin)
             ((symbol-plist 'truncated-stdin) flycheck-test--truncated-stdin))
    (dolist (buffer-size '(4095 65537))
      ;; As long as the checker reports at least one error closing stdin early
      ;; isn't an issue.
      (flycheck-ert-with-temp-buffer
        (truncated-stdin-mode)
        (insert (make-string buffer-size ?a))
        (flycheck-ert-should-syntax-check-in-buffer
         '(1 1 error "error" :checker truncated-stdin))))))

;; https://github.com/flycheck/flycheck/issues/1278
(ert-deftest flycheck-start-command-checker/truncated-stdin-without-errors ()
  :tags '(command-checker)
  (cl-letf* ((flycheck-checker 'truncated-stdin)
             ((symbol-plist 'truncated-stdin) flycheck-test--truncated-stdin)
             ((flycheck-checker-get 'truncated-stdin 'error-patterns)
              '(("\\`_\\`" . error))))
    ;; If the checker closes stdin early without reporting errors, something
    ;; might have gone wrong, so warn the user.
    (flycheck-ert-with-temp-buffer
      (truncated-stdin-mode)
      (insert (make-string 65537 ?\n))
      (should-error (shut-up (flycheck-ert-should-syntax-check-in-buffer))
                    :type 'flycheck-ert-suspicious-checker))))


;;; Executables of command checkers

(ert-deftest flycheck-overridden-executable ()
  :tags '(executables language-emacs-lisp
                      checker-emacs-lisp checker-emacs-lisp-checkdoc)
  (let ((flycheck-emacs-lisp-executable (flycheck-ert-resource-filename
                                         "bin/dummy-emacs")))
    (flycheck-ert-should-syntax-check
     "language/emacs-lisp/warnings.el" 'emacs-lisp-mode
     '(12 nil info "First sentence should end with punctuation"
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
      (with-no-warnings
        (flycheck-set-checker-executable 'emacs-lisp file-name))
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
      (with-no-warnings
        (flycheck-set-checker-executable 'emacs-lisp))
      (should-not flycheck-emacs-lisp-executable)
      (should (local-variable-p 'flycheck-emacs-lisp-executable)))))

(ert-deftest flycheck-set-checker-executable/executable-is-nil ()
  :tags '(executables)
  (flycheck-ert-with-temp-buffer
    (let ((file-name (flycheck-ert-resource-filename "bin/dummy-emacs")))
      (setq-local flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (with-no-warnings
        (flycheck-set-checker-executable 'emacs-lisp nil))
      (should-not flycheck-emacs-lisp-executable)
      (should (local-variable-p 'flycheck-emacs-lisp-executable)))))

(ert-deftest flycheck-set-checker-executable/non-existing-file ()
  :tags '(executables)
  (let ((file-name (flycheck-ert-resource-filename "no-such-file")))
    (should-not (file-exists-p file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name) :type 'user-error)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))

(ert-deftest flycheck-set-checker-executable/file-not-executable ()
  :tags '(executables)
  (let ((file-name (flycheck-ert-resource-filename "language/emacs-lisp/warnings.el")))
    (should (file-exists-p file-name))
    (should-not (file-executable-p file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name) :type 'user-error)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))


;;; Configuration files and options for command syntax checkers

(ert-deftest flycheck-locate-config-file-by-path/just-a-base-name ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (cd flycheck-test-directory)
    (should-not (flycheck-locate-config-file-by-path "flycheck-test.el"
                                                     'emacs-lisp))))

(ert-deftest flycheck-locate-config-file-by-path/with-path ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (cd flycheck-test-directory)
    (should (equal (flycheck-locate-config-file-by-path "../Makefile"
                                                        'emacs-lisp)
                   (expand-file-name "../Makefile" flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file-by-path/non-existing-file ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (cd flycheck-test-directory)
    (should-not (flycheck-locate-config-file-by-path "../foobar" 'emacs-lisp))))

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

(ert-deftest flycheck-locate-config-file/multiple-files ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name (expand-file-name "specs/test-documentation.el"
                                             flycheck-test-directory))
    (should (equal (flycheck-locate-config-file
                    '("test-documentation.el" "flycheck-test.el") 'emacs-lisp)
                   (expand-file-name "specs/test-documentation.el"
                                     flycheck-test-directory)))))

(ert-deftest flycheck-locate-config-file/multiple-files-ordered ()
  :tags '(configuration)
  (flycheck-ert-with-temp-buffer
    (setq buffer-file-name (expand-file-name "specs/test-documentation.el"
                                             flycheck-test-directory))
    (should (equal (flycheck-locate-config-file
                    '("flycheck-test.el" "test-documentation.el") 'emacs-lisp)
                   (expand-file-name "flycheck-test.el" flycheck-test-directory)))))

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


;;; Built-in checkers

;; Tell the byte compiler about the variables we'll use
(eval-when-compile
  (defvar js2-mode-show-strict-warnings)
  (defvar js2-mode-show-parse-errors)
  (defvar js3-mode-show-parse-errors)
  (defvar python-indent-guess-indent-offset))

(flycheck-ert-def-checker-test ada-gnat ada syntax-error
  (require 'speedbar)
  (flycheck-ert-should-syntax-check
   "language/ada/syntaxerror.adb" 'ada-mode
   '(7  32 error "missing \";\"" :checker ada-gnat)
   '(8 5 error "misspelling of \"SYNTAXERROR\"" :checker ada-gnat)))

(flycheck-ert-def-checker-test ada-gnat ada warnings
  (require 'speedbar)
  (flycheck-ert-should-syntax-check
   "language/ada/hello.adb" 'ada-mode
   '(   6 4 warning "variable \"Name\" is not referenced" :checker ada-gnat)
   '(8  11 warning "unrecognized pragma \"Foo\"" :checker ada-gnat)))

(flycheck-ert-def-checker-test asciidoc asciidoc nil
  (let ((flycheck-disabled-checkers '(asciidoctor)))
    (flycheck-ert-should-syntax-check
     "language/asciidoc.adoc" 'adoc-mode
     '(1 nil warning "missing style: [paradef-default]: paragraph" :checker asciidoc)
     '(3 nil info "old tables syntax" :checker asciidoc)
     '(11 nil error "[tabledef-default] illegal width=%60%" :checker asciidoc))))

(flycheck-ert-def-checker-test asciidoctor asciidoc nil
  (flycheck-ert-should-syntax-check
   "language/asciidoctor.adoc" 'adoc-mode
   '(4 nil warning "section title out of sequence: expected level 1, got level 2" :checker asciidoctor)
   '(6 nil error "unmatched preprocessor directive: endif::[]" :checker asciidoctor)))

(flycheck-ert-def-checker-test awk-gawk awk syntax-error
  (flycheck-ert-should-syntax-check
   "language/awk/syntax-error.awk" 'awk-mode
   '(2 nil warning "x=|\n  ^ syntax error" :checker awk-gawk)))

(flycheck-ert-def-checker-test bazel-buildifier bazel error
  (flycheck-ert-should-syntax-check
   "language/bazel/syntax-error.bazel" 'bazel-mode
   '(1 11 error "syntax error near !" :checker bazel-buildifier)))

(flycheck-ert-def-checker-test bazel-buildifier bazel nil
  (flycheck-ert-should-syntax-check
   "language/bazel/warnings.bazel" 'bazel-mode
   '(1 nil warning "The file has no module docstring. (https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#module-docstring)" :id "module-docstring" :checker bazel-buildifier)))

(flycheck-ert-def-checker-test c/c++-clang (c c++) error
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "language/c_c++/error.cpp" 'c++-mode
     '(2 20 error "no member named 'bar' in 'A'"
         :checker c/c++-clang)
     '(6 19 info "in instantiation of function template specialization 'foo<A>' requested here"
         :checker c/c++-clang)
     '(8 9 warning "unknown pragma ignored"
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) fatal-error
  (let ((flycheck-disabled-checkers '(c/c++-gcc)))
    (flycheck-ert-should-syntax-check
     "language/c_c++/includes.c" 'c-mode
     '(2 10 error "'library.h' file not found"
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) warnings
  (let ((flycheck-disabled-checkers '(c/c++-gcc c/c++-cppcheck)))
    (flycheck-ert-should-syntax-check
     "language/c_c++/warning.c" 'c-mode
     '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)
     '(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
         :checker c/c++-clang)
     '(8 7 warning "no message" :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-clang (c c++) included-file-warning
  (let ((flycheck-clang-include-path '("./include"))
        (flycheck-disabled-checkers '(c/c++-gcc))
        (flycheck-relevant-error-other-file-minimum-level 'warning))
    (flycheck-ert-should-syntax-check
     "language/c_c++/in-included-file.cpp" 'c++-mode
     `(5 10 warning "unused variable 'unused'"
         :filename ,(flycheck-ert-resource-filename "language/c_c++/warning.c")
         :checker c/c++-clang)
     `(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
         :filename ,(flycheck-ert-resource-filename "language/c_c++/warning.c")
         :checker c/c++-clang)
     `(8 7 warning "no message"
         :filename ,(flycheck-ert-resource-filename "language/c_c++/warning.c")
         :checker c/c++-clang))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) error
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "language/c_c++/error.cpp" 'c++-mode
     '(2 20 error "'struct A' has no member named 'bar'"
         :checker c/c++-gcc)
     '(8 nil warning "ignoring #pragma nope"
         :id "-Wunknown-pragmas" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) fatal-error
  (let ((flycheck-disabled-checkers '(c/c++-clang)))
    (flycheck-ert-should-syntax-check
     "language/c_c++/includes.c" 'c-mode
     '(2 10 error "library.h: No such file or directory"
         :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) warning
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck)))
    (flycheck-ert-should-syntax-check
     "language/c_c++/warning.c" 'c-mode
     '(5 10 warning "unused variable 'unused'"
         :id "-Wunused-variable" :checker c/c++-gcc)
     '(7 15 warning "comparison of integer expressions of different signedness: 'int' and 'unsigned int'"
         :id "-Wsign-compare" :checker c/c++-gcc)
     '(8 7 warning "#warning" :id "-Wcpp" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-gcc (c c++) included-file-warning
  (let ((flycheck-gcc-include-path '("./include"))
        (flycheck-disabled-checkers '(c/c++-clang))
        (flycheck-relevant-error-other-file-minimum-level 'warning))
    (flycheck-ert-should-syntax-check
     "language/c_c++/in-included-file.cpp" 'c++-mode
     `(5 10 warning "unused variable 'unused'"
         :filename ,(flycheck-ert-resource-filename "language/c_c++/warning.c")
         :id "-Wunused-variable" :checker c/c++-gcc)
     `(7 15 warning "comparison of integer expressions of different signedness: 'int' and 'unsigned int'"
         :filename ,(flycheck-ert-resource-filename "language/c_c++/warning.c")
         :id "-Wsign-compare" :checker c/c++-gcc)
     `(8 7 warning "#warning"
         :filename ,(flycheck-ert-resource-filename "language/c_c++/warning.c")
         :id "-Wcpp" :checker c/c++-gcc))))

(flycheck-ert-def-checker-test c/c++-cppcheck (c c++) nil
  :tags '(cppcheck-xml)
  (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
        (flycheck-cppcheck-inconclusive nil)
        (flycheck-cppcheck-checks '("style")))
    (flycheck-ert-should-syntax-check
     "language/c_c++/style2.cpp" 'c++-mode
     '(3 nil info "The scope of the variable 'i' can be reduced. Warning: Be careful when fixing this message, especially when there are inner loops. Here is an example where cppcheck will write that the scope for 'i' can be reduced:\nvoid f(int x)\n{\n    int i = 0;\n    if (x) {\n        // it's safe to move 'int i = 0;' here\n        for (int n = 0; n < 10; ++n) {\n            // it is possible but not safe to move 'int i = 0;' here\n            do_something(&i);\n        }\n    }\n}\nWhen you see this message it is always safe to reduce the variable scope 1 level."
         :id "variableScope" :checker c/c++-cppcheck))

    (flycheck-ert-should-syntax-check
     "language/c_c++/style.cpp" 'c-mode
     '(12 nil error "Code 'std::string' is invalid C code. Use --std or --language to configure the language."
          :id "syntaxError" :checker c/c++-cppcheck))

    (flycheck-ert-should-syntax-check
     "language/c_c++/style.cpp" 'c++-mode
     '(3 nil error "Division by zero." :id "zerodiv" :checker c/c++-cppcheck)
     '(5 nil info "Unused variable: unused" :id "unusedVariable"
         :checker c/c++-cppcheck)
     '(9 nil error "Division by zero." :id "zerodiv" :checker c/c++-cppcheck)
     '(12 nil warning "Parameter 'foo' is passed by value. It could be passed as a const reference which is usually faster and recommended in C++."
          :id "passedByValue" :checker c/c++-cppcheck))))

(flycheck-ert-def-checker-test cfengine cfengine error
  (skip-unless (fboundp 'cfengine3-mode))
  (flycheck-ert-should-syntax-check
   "language/cfengine/error.cf" 'cfengine3-mode
   '(8 21 error "Unknown promise type 'nosuchpromisetype'" :checker cfengine)))

(flycheck-ert-def-checker-test cfengine cfengine warning
  (skip-unless (fboundp 'cfengine3-mode))
  (flycheck-ert-should-syntax-check
   "language/cfengine/warning.cf" 'cfengine3-mode
   '(3 35 warning "Removed constraint 'host_licenses_paid' in promise type 'common' [-Wremoved]"
       :checker cfengine)))

(flycheck-ert-def-checker-test chef-foodcritic chef nil
  (flycheck-ert-should-syntax-check
   "language/chef/recipes/error.rb" 'ruby-mode
   `(1 nil error "Missing README in markdown format"
       :checker chef-foodcritic :id "FC011"
       :filename ,(flycheck-ert-resource-filename "language/chef/README.md"))
   `(1 nil error "Cookbook without metadata.rb file"
       :checker chef-foodcritic :id "FC031"
       :filename ,(flycheck-ert-resource-filename "language/chef/metadata.rb"))
   `(1 nil error "Missing LICENSE file"
       :checker chef-foodcritic :id "FC071"
       :filename ,(flycheck-ert-resource-filename "language/chef/LICENSE"))
   '(3 nil error "Avoid string interpolation where not required"
       :checker chef-foodcritic :id "FC002")
   '(11 nil error "Use a service resource to start and stop services"
        :checker chef-foodcritic :id "FC004")))

(flycheck-ert-def-checker-test coffee coffee syntax-error
  (flycheck-ert-should-syntax-check
   "language/coffee/syntax-error.coffee" 'coffee-mode
   '(4 7 error "missing \"" :checker coffee)))

(flycheck-ert-def-checker-test coffee-coffeelint coffee error
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "language/coffee/error.coffee" 'coffee-mode
   '(4 nil error "Throwing strings is forbidden; context:"
       :checker coffee-coffeelint)))

(flycheck-ert-def-checker-test coffee-coffeelint coffee warning
  :tags '(checkstyle-xml)
  (let ((flycheck-coffeelintrc "lint.json"))
    (flycheck-ert-should-syntax-check
     "language/coffee/error.coffee" 'coffee-mode
     '(4 nil warning "Throwing strings is forbidden; context:"
         :checker coffee-coffeelint))))

(flycheck-ert-def-checker-test coq coq syntax-error
  (skip-unless (shut-up (load "gallina" 'noerror 'nomessage)))
  (flycheck-ert-should-syntax-check
   "language/coq/syntax-error.v" 'coq-mode
   '(6 12 error "\'end\' expected after [branches] (in [match_constr])."
       :checker coq
       :end-line 6
       :end-column 14)))

(flycheck-ert-def-checker-test coq coq error
  (skip-unless (shut-up (load "gallina" 'noerror 'nomessage)))
  (flycheck-ert-should-syntax-check
   "language/coq/error.v" 'coq-mode
   '(7 21 error "In environment
evenb : nat -> bool
n : nat
n0 : nat
n' : nat
The term \"1\" has type \"nat\" while it is expected to have type \"bool\"."
       :checker coq
       :end-line 7
       :end-column 22)))

(flycheck-ert-def-checker-test css-csslint css nil
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "language/css/warning.css" 'css-mode
   '(3 6 warning "Heading (h1) should not be qualified."
       :id "Disallowqualifiedheadings" :checker css-csslint)))

(flycheck-ert-def-checker-test css-csslint css syntax-error
  :tags '(checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "language/css/syntax-error.css" 'css-mode
   '(4 14 error "Expected a `FUNCTION` or `IDENT` after colon at line 4, col 14."
       :id "ParsingErrors" :checker css-csslint)))

(flycheck-ert-def-checker-test cuda cuda syntax-error
  (flycheck-ert-should-syntax-check
   "language/cuda/syntax-error.cu" 'cuda-mode
   '(5 0 error "error: identifier \"ac\" is undefined"
       :checker cuda)))

(flycheck-ert-def-checker-test cuda cuda syntax-warning
  (flycheck-ert-should-syntax-check
   "language/cuda/syntax-warning.cu" 'cuda-mode
   '(3 0 warning "variable \"b\" was set but never used"
       :checker cuda)))

(flycheck-ert-def-checker-test cwl cwl syntax-error
  (let ((flycheck-cwl-schema-path "schema/CommonWorkflowLanguage.yml"))
    (flycheck-ert-should-syntax-check
     "language/cwl/cwl.cwl" 'cwl-mode
     '(6 5 error "value is a str, expected null or CommandLineBinding"
         :checker cwl))))

(ert-deftest flycheck-d-module-re/matches-module-name ()
  :tags '(language-d)
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (let ((s "module spam.with.eggs ;"))
    (should (string-match flycheck-d-module-re s))
    (should (string= "spam.with.eggs" (match-string 1 s)))))

(ert-deftest flycheck-d-base-directory/no-module-declaration ()
  :tags '(language-d)
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (flycheck-ert-with-resource-buffer "language/d/src/dmd/no_module.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-ert-resource-filename "language/d/src/dmd")))))

(ert-deftest flycheck-d-base-directory/with-module-declaration ()
  :tags '(language-d)
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (flycheck-ert-with-resource-buffer "language/d/src/dmd/warning.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-ert-resource-filename "language/d/src")))))

(ert-deftest flycheck-d-base-directory/package-file ()
  :tags '(language-d)
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (flycheck-ert-with-resource-buffer "language/d/src/dmd/package.d"
    (should (flycheck-same-files-p
             (flycheck-d-base-directory)
             (flycheck-ert-resource-filename "language/d/src")))))

(flycheck-ert-def-checker-test d-dmd d warning-include-path
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (let ((flycheck-dmd-include-path '("../../lib")))
    (flycheck-ert-should-syntax-check
     "language/d/src/dmd/warning.d" 'd-mode
     '(9 5 warning "statement is not reachable" :checker d-dmd)
     '(20 17 warning "function `dmd.warning.bar` is deprecated"
          :checker d-dmd))))

(flycheck-ert-def-checker-test d-dmd d missing-import
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (flycheck-ert-should-syntax-check
   "language/d/src/dmd/warning.d" 'd-mode
   '(4 8 error "module `external_library` is in file 'external_library.d' which cannot be read"
       :checker d-dmd)))

(flycheck-ert-def-checker-test d-dmd d continuation-line
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (flycheck-ert-should-syntax-check
   "language/d/src/dmd/continuation.d" 'd-mode
   '(5 12 error "undefined identifier `invalid`"
       :checker d-dmd)
   '(10 12 error "template instance `continuation.T!()` error instantiating"
        :checker d-dmd)
   '(13 1 info "instantiated from here: `U!()`"
        :checker d-dmd)))

(flycheck-ert-def-checker-test d-dmd d non-d-extension
  (skip-unless (fboundp 'd-mode))
  (unless (version<= "24.4" emacs-version)
    (ert-skip "Skipped because CC Mode is broken on 24.3.
See https://github.com/flycheck/flycheck/issues/531 and Emacs bug #19206"))
  (flycheck-ert-with-temp-buffer
    (insert "!invalid")
    (d-mode)
    (flycheck-ert-buffer-sync)
    (flycheck-ert-should-errors
     '(1 1 error "declaration expected, not `!`"
         :checker d-dmd))))

(flycheck-ert-def-checker-test dockerfile-hadolint dockerfile error
  (flycheck-ert-should-syntax-check
   "language/dockerfile/Dockerfile.error" 'dockerfile-mode
   '(2 1 error "unexpected 'I' expecting '#', '\\', ADD, ARG, CMD, COPY, ENTRYPOINT, ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER, ONBUILD, RUN, SHELL, STOPSIGNAL, USER, VOLUME, WORKDIR, at least one space, or end of input"
       :checker dockerfile-hadolint)))

(flycheck-ert-def-checker-test dockerfile-hadolint dockerfile warnings
  (flycheck-ert-should-syntax-check
   "language/dockerfile/Dockerfile.warning" 'dockerfile-mode
   '(1 nil warning "Always tag the version of an image explicitly"
       :id "DL3006" :checker dockerfile-hadolint)
   '(2 nil error "Do not use apt-get upgrade or dist-upgrade"
       :id "DL3005" :checker dockerfile-hadolint)
   '(2 nil info "Delete the apt-get lists after installing something"
       :id "DL3009" :checker dockerfile-hadolint)
   '(3 nil error "Use absolute WORKDIR"
       :id "DL3000" :checker dockerfile-hadolint)))

(flycheck-ert-def-checker-test elixir-credo elixir infos-without-strict-mode
  (flycheck-ert-should-syntax-check
   "language/elixir/lib/infos.ex" 'elixir-mode
   '(1 11 info "Modules should have a @moduledoc tag."
       :checker elixir-credo)))

(flycheck-ert-def-checker-test elixir-credo elixir infos-with-strict-mode
  (let ((flycheck-elixir-credo-strict t))
    (flycheck-ert-should-syntax-check
     "language/elixir/lib/infos.ex" 'elixir-mode
     '(1 11 info "Modules should have a @moduledoc tag."
         :checker elixir-credo)
     '(2 nil info "Do not use parentheses when defining a function which has no arguments."
         :checker elixir-credo))))

(flycheck-ert-def-checker-test elixir-credo elixir warnings
  (flycheck-ert-should-syntax-check
   "language/elixir/lib/warnings.ex" 'elixir-mode
   '(5 nil warning "There are identical sub-expressions to the left and to the right of the '&&' operator."
       :checker elixir-credo)
   '(8 8 warning "length(list) == 0 is expensive. Prefer Enum.empty?/1 or list == []"
       :checker elixir-credo)))

(flycheck-ert-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp nil
  (flycheck-ert-should-syntax-check
   "language/emacs-lisp/warnings.el" 'emacs-lisp-mode
   '(12 nil info "First sentence should end with punctuation"
        :checker emacs-lisp-checkdoc)
   '(16 6 warning "foobar called with 1 argument, but accepts only 0"
        :checker emacs-lisp)
   '(21 1 warning "the function `dummy-package-foo' is not known to be defined."
        :checker emacs-lisp )))

(flycheck-ert-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                               uses-right-major-mode
  (flycheck-ert-should-syntax-check
   "language/emacs-lisp/checkdoc-elisp-mode-regression.el" 'emacs-lisp-mode
   '(11 nil info "All variables and subroutines might as well have a documentation string"
        :checker emacs-lisp-checkdoc)))

(flycheck-ert-def-checker-test (emacs-lisp-checkdoc) emacs-lisp
                               inherits-checkdoc-variables
  ;; This test doesn't run on 24.3 and earlier because the corresponding
  ;; checkdoc variables were only introduced in 24.4.
  (skip-unless (version<= "24.4" emacs-version))
  (flycheck-ert-should-syntax-check
   "language/emacs-lisp/local-checkdoc-variables.el" 'emacs-lisp-mode))

(flycheck-ert-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                               checks-compressed-file
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/emacs-lisp/warnings.el.gz" 'emacs-lisp-mode
     '(12 nil info "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(16 6 warning "foobar called with 1 argument, but accepts only 0"
          :checker emacs-lisp)
     '(21 1 warning "the function `dummy-package-foo' is not known to be defined."
          :checker emacs-lisp))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp syntax-error
  (let ((flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
    (flycheck-ert-should-syntax-check
     "language/emacs-lisp/syntax-error.el" 'emacs-lisp-mode
     '(3 1 error "End of file during parsing" :checker emacs-lisp))))

(flycheck-ert-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                               without-file-name
  ;; Regression test for checkdoc in buffers without file names. See
  ;; https://github.com/flycheck/flycheck/issues/73 and
  ;; https://github.com/bbatsov/prelude/issues/259
  (flycheck-ert-with-resource-buffer "language/emacs-lisp/warnings.el"
    (set-visited-file-name nil 'no-query)
    (emacs-lisp-mode)
    (should-not (buffer-file-name))
    (flycheck-ert-buffer-sync)
    ;; TODO: Consider whether checkdoc is really useful in buffers without file
    ;; names…
    (should flycheck-current-errors)))

(flycheck-ert-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                               does-not-check-autoloads-buffers
  ;; Regression test ensuring that Emacs Lisp won't check autoload buffers.
  ;; These buffers are temporary buffers created during package installation to
  ;; collect the autoloads of newly installed packages before writing the
  ;; autoloads file.  See `https://github.com/flycheck/flycheck/issues/45' and
  ;; `https://github.com/bbatsov/prelude/issues/253' for details.
  (flycheck-ert-with-file-buffer (locate-library "shut-up-autoloads")
    (should-not (flycheck-may-use-checker 'emacs-lisp))
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(flycheck-ert-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                               checkdoc-does-not-check-cask-files
  (flycheck-ert-with-file-buffer
      (expand-file-name "Cask" flycheck-test-source-directory)
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(flycheck-ert-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                               does-not-check-with-no-byte-compile
  ;; We need to use a hook here, because `no-byte-compile' seems to be
  ;; explicitly changed when loading Emacs Lisp files
  (let ((disable-byte-comp (lambda () (setq-local no-byte-compile t))))
    (add-hook 'emacs-lisp-mode-hook disable-byte-comp)
    (unwind-protect
        (flycheck-ert-should-syntax-check
         "language/emacs-lisp/warnings.el" 'emacs-lisp-mode
         '(12 nil info "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))
      (remove-hook 'emacs-lisp-mode-hook disable-byte-comp))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp check-declare-warnings
  (let ((flycheck-emacs-lisp-check-declare t))
    (flycheck-ert-should-syntax-check
     "language/emacs-lisp/check-declare-warnings.el" 'emacs-lisp-mode
     (cond
      ((version< emacs-version "25")
       '(0 nil warning "`this-function-is-not-declared' was defined in this-file-does-not-exist.el: file not found"
           :checker emacs-lisp))
      ((version< emacs-version "26")
       '(9 1 warning "`this-function-is-not-declared' was defined in this-file-does-not-exist.el: file not found"
           :checker emacs-lisp))
      (t
       '(9 nil warning "`this-function-is-not-declared' was defined in this-file-does-not-exist.el: file not found"
           :checker emacs-lisp))))))

(flycheck-ert-def-checker-test emacs-lisp emacs-lisp disable-check-declare
  (let ((flycheck-emacs-lisp-check-declare nil))
    (flycheck-ert-should-syntax-check
     "language/emacs-lisp/check-declare-warnings.el" 'emacs-lisp-mode)))

(flycheck-ert-def-checker-test ember-template ember-template error
  (flycheck-ert-should-syntax-check
   "language/ember-template-lint/ember-template-lint/error.hbs" 'web-mode
   '(2 16 error "Incorrect indentation for `<span>` beginning at L2:C16. Expected `<span>` to be at an indentation of 2 but was found at 16." :id "block-indentation" :checker ember-template)))

(flycheck-ert-def-checker-test ember-template ember-template warning
  (flycheck-ert-should-syntax-check
   "language/ember-template-lint/ember-template-lint/warning.hbs" 'web-mode
   '(1 nil warning "Non-translated string used" :id "no-bare-strings" :checker ember-template)))

(flycheck-ert-def-checker-test erlang erlang error
  (shut-up
    (flycheck-ert-should-syntax-check
     "language/erlang/erlang/error.erl" 'erlang-mode
     '(3 nil warning "export_all flag enabled - all functions will be exported" :checker erlang)
     '(7 nil error "head mismatch" :checker erlang))))

(flycheck-ert-def-checker-test erlang erlang warning
  (flycheck-ert-should-syntax-check
   "language/erlang/erlang/warning.erl" 'erlang-mode
   '(3 nil warning "export_all flag enabled - all functions will be exported" :checker erlang)
   '(6 nil warning "wrong number of arguments in format call" :checker erlang)))

(flycheck-ert-def-checker-test erlang-rebar3 erlang error
  (flycheck-ert-should-syntax-check
   "language/erlang/rebar3/src/erlang-error.erl" 'erlang-mode
   '(3 nil warning "export_all flag enabled - all functions will be exported" :checker erlang-rebar3)
   '(7 nil error "head mismatch" :checker erlang-rebar3)))

(flycheck-ert-def-checker-test erlang-rebar3 erlang build
  (shut-up
    (flycheck-ert-should-syntax-check
     "language/erlang/rebar3/_checkouts/dependency/src/dependency.erl" 'erlang-mode
     `(7 nil error "head mismatch" :checker erlang-rebar3
         :filename ,(flycheck-ert-resource-filename "language/erlang/rebar3/src/erlang-error.erl"))))
  ;; Ensure that the dependency file wasn't built as standalone
  ;; project which would create a separate _build directory
  (should (not (file-exists-p
                (flycheck-ert-resource-filename
                 "language/erlang/rebar3/_build/default/lib/dependency/_build")))))

(flycheck-ert-def-checker-test eruby-erubis eruby nil
  (let ((flycheck-disabled-checkers '(eruby-ruumba)))
    (flycheck-ert-should-syntax-check
     "language/eruby.erb" '(html-erb-mode rhtml-mode)
     '(9 nil error "syntax error, unexpected end-of-input, expecting `end'" :checker eruby-erubis))))

(flycheck-ert-def-checker-test eruby-ruumba eruby syntax-error
  (let ((flycheck-disabled-checkers '(eruby-erubis)))
    (flycheck-ert-should-syntax-check
     "language/eruby.erb" '(html-erb-mode rhtml-mode)
     '(8 1 error "unexpected token $end (Using Ruby 2.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
         :id "Lint/Syntax"  :checker eruby-ruumba))))

(flycheck-ert-def-checker-test fortran-gfortran fortran error
  (flycheck-ert-should-syntax-check
   "language/fortran/error.f" '(fortran-mode f90-mode)
   '(1 1 error "Non-numeric character in statement label at (1)"
       :checker fortran-gfortran)
   '(2 1 error "Non-numeric character in statement label at (1)"
       :checker fortran-gfortran)
   '(3 1 error "Non-numeric character in statement label at (1)"
       :checker fortran-gfortran)))

(flycheck-ert-def-checker-test fortran-gfortran fortran free-form-error
  (let ((flycheck-gfortran-layout 'free))
    (flycheck-ert-should-syntax-check
     "language/fortran/error.f" '(fortran-mode f90-mode)
     '(3 3 error "Expecting END PROGRAM statement at (1)"
         :checker fortran-gfortran))))

(flycheck-ert-def-checker-test fortran-gfortran fortran warning
  (flycheck-ert-should-syntax-check
   "language/fortran/warning.f90" '(fortran-mode f90-mode)
   '(1 20 warning "Unused dummy argument 'p' at (1)"
       :checker fortran-gfortran)
   '(18 9 warning "Same actual argument associated with INTENT(IN) argument 'a' and INTENT(OUT) argument 'b' at (1)"
        :checker fortran-gfortran)))

(flycheck-ert-def-checker-test go-gofmt go syntax-error
  (flycheck-ert-should-syntax-check
   "language/go/src/syntax/syntax-error.go" 'go-mode
   '(5 9 error "expected '(', found ta" :checker go-gofmt)
   '(6 1 error "expected declaration, found '}'" :checker go-gofmt)))

(flycheck-ert-def-checker-test (go-build go-golint go-vet) go complete-chain
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "language/go")))
    (flycheck-ert-should-syntax-check
     "language/go/src/warnings.go" 'go-mode
     '(4 2 error "imported and not used: \"fmt\"" :checker go-build)
     '(4 2 warning "should not use dot imports" :checker go-golint)
     '(7 1 warning "exported function Warn should have comment or be unexported"
         :checker go-golint)
     '(8 2 error "undefined: fmt" :checker go-build)
     '(11 1 warning "exported function Warnf should have comment or be unexported"
          :checker go-golint)
     '(12 2 error "undefined: fmt" :checker go-build)
     '(17 2 error "undefined: fmt" :checker go-build)
     '(19 13 error "cannot use 1 (type untyped int) as type string in argument to Warnf"
          :checker go-build)
     '(25 9 warning "if block ends with a return statement, so drop this else and outdent its block"
          :checker go-golint))))

(flycheck-ert-def-checker-test go-build go handles-packages
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "language/go")))
    (flycheck-ert-should-syntax-check "language/go/src/b1/main.go" 'go-mode)))

(flycheck-ert-def-checker-test go-build go missing-package
  (let ((go-root (or (getenv "GOROOT") "/usr/local/go"))
        (go-path (concat (getenv "HOME") "/go")))
    (flycheck-ert-with-env '(("GOPATH" . nil))
      (flycheck-ert-should-syntax-check
       "language/go/src/b1/main.go" 'go-mode
       `(4 2 error ,(format "cannot find package \"b2\" in any of:\n\t%s/src/b2 (from $GOROOT)\n\t%s/src/b2 (from $GOPATH)"
                            go-root go-path)
           :checker go-build)))))

(flycheck-ert-def-checker-test go-test go nil
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "checkers/go")))
    (flycheck-ert-should-syntax-check
     "language/go/src/test/test-error_test.go" 'go-mode
     '(8 2 error "undefined: fmt" :checker go-test))))

(flycheck-ert-def-checker-test go-errcheck go nil
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "language/go")))
    (flycheck-ert-should-syntax-check
     "language/go/src/errcheck/errcheck.go" 'go-mode
     '(7 9 warning "Ignored `error` returned from `f.Close()`"
         :checker go-errcheck)
     '(9 9 warning "Ignored `error` returned from `os.Stat(\"enoent\")`"
         :checker go-errcheck))))

(flycheck-ert-def-checker-test go-unconvert go nil
  :tags '(language-go external-tool)
  (flycheck-ert-with-env
      `(("GOPATH" . ,(flycheck-ert-resource-filename "language/go")))
    (flycheck-ert-should-syntax-check
     "language/go/src/unconvert/unconvert.go" 'go-mode
     '(7 17 warning "unnecessary conversion"
         :checker go-unconvert))))

(flycheck-ert-def-checker-test go-staticcheck go nil
  :tags '(language-go external-tool)
  (let ((flycheck-disabled-checkers '(go-golint go-unconvert)))
    (flycheck-ert-with-env
        `(("GOPATH" . ,(flycheck-ert-resource-filename "language/go")))
      (flycheck-ert-should-syntax-check
       "language/go/src/staticcheck/staticcheck1.go" 'go-mode
       '(8 6 error "unnecessary assignment to the blank identifier"
           :checker go-staticcheck :id "S1005")
       '(12 39 error "calling strings.Replace with n == 0 will return no results, did you mean -1?"
            :checker go-staticcheck :id "SA1018")
       '(16 6 error "func unused is unused"
            :checker go-staticcheck :id "U1000")))))

(flycheck-ert-def-checker-test groovy groovy syntax-error
  ;; Work around
  ;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues/11
  (require 'cl)
  (flycheck-ert-should-syntax-check
   "language/groovy.groovy" 'groovy-mode
   '(2 14 error "unexpected token: {" :checker groovy)))

(flycheck-ert-def-checker-test haml haml "haml-error"
  (flycheck-ert-should-syntax-check
   "language/haml/haml-error.haml" 'haml-mode
   '(5 nil error "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces."
       :checker haml)))

(flycheck-ert-def-checker-test haml haml "ruby-error"
  (flycheck-ert-should-syntax-check
   "language/haml/ruby-error.haml" 'haml-mode
   '(1 nil error "unexpected end-of-input"
       :checker haml)))

(flycheck-ert-def-checker-test handlebars handlebars nil
  (flycheck-ert-should-syntax-check
   "language/handlebars.hbs" '(handlebars-mode web-mode)
   '(2 nil error "Expecting 'ID', 'STRING', 'NUMBER', 'BOOLEAN', 'UNDEFINED', 'NULL', 'DATA', got 'INVALID'"
       :checker handlebars)))

(flycheck-ert-def-checker-test haskell-stack-ghc haskell syntax-error
  (skip-unless (file-exists-p (getenv "HOME")))
  (let ((flycheck-disabled-checkers '(haskell-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/SyntaxError.hs" 'haskell-mode
     '(3 1 error "parse error on input `module'" :checker haskell-stack-ghc))))

(flycheck-ert-def-checker-test haskell-stack-ghc haskell type-error
  (skip-unless (file-exists-p (getenv "HOME")))
  (let ((flycheck-disabled-checkers '(haskell-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/Error.hs" 'haskell-mode
     '(4 16 error "* Couldn't match type `Bool' with `[Char]'
  Expected type: String
    Actual type: Bool
* In the first argument of `putStrLn', namely `True'
  In the expression: putStrLn True
  In an equation for `foo': foo = putStrLn True" :checker haskell-stack-ghc))))

(flycheck-ert-def-checker-test (haskell-stack-ghc haskell-hlint) haskell literate
  (skip-unless (file-exists-p (getenv "HOME")))
  (let ((flycheck-disabled-checkers '(haskell-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/Literate.lhs" 'haskell-literate-mode
     '(6 1 warning "Top-level binding with no type signature: foo :: a"
         :id "-Wmissing-signatures"
         :checker haskell-stack-ghc))))

(flycheck-ert-def-checker-test (haskell-stack-ghc haskell-hlint) haskell
                               complete-chain
  (skip-unless (file-exists-p (getenv "HOME")))
  (let ((flycheck-disabled-checkers '(haskell-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/Warnings.hs" 'haskell-mode
     '(4 1 warning "Eta reduce
Found:
  spam eggs = map lines eggs
Perhaps:
  spam = map lines" :checker haskell-hlint)
     '(4 1 warning "Top-level binding with no type signature:
  spam :: [String] -> [[String]]"
         :id "-Wmissing-signatures"
         :checker haskell-stack-ghc)
     '(7 8 info "Redundant bracket
Found:
  (putStrLn \"hello world\")
Perhaps:
  putStrLn \"hello world\"" :checker haskell-hlint))))

(flycheck-ert-def-checker-test
    haskell-stack-ghc haskell nonstandard-stack-yaml-file
  (skip-unless (file-exists-p (getenv "HOME")))

  (let* ((flycheck-disabled-checkers '(haskell-ghc))
         (proj-dir "language/haskell/stack-project-with-renamed-stack-yaml")
         (flycheck-ghc-stack-project-file
          (expand-file-name "stack-nonstandard.yaml"
                            (flycheck-ert-resource-filename proj-dir))))

    (flycheck-ert-should-syntax-check
     (concat proj-dir "/src/Foo.hs")
     'haskell-mode)))

(flycheck-ert-def-checker-test haskell-ghc haskell syntax-error
  (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/SyntaxError.hs" 'haskell-mode
     '(3 1 error "parse error on input `module'" :checker haskell-ghc))))

(flycheck-ert-def-checker-test haskell-ghc haskell type-error
  (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/Error.hs" 'haskell-mode
     '(4 16 error "* Couldn't match type `Bool' with `[Char]'
  Expected type: String
    Actual type: Bool
* In the first argument of `putStrLn', namely `True'
  In the expression: putStrLn True
  In an equation for `foo': foo = putStrLn True" :checker haskell-ghc))))

(flycheck-ert-def-checker-test (haskell-ghc haskell-hlint) haskell literate
  (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/Literate.lhs" 'haskell-literate-mode
     '(6 1 warning "Top-level binding with no type signature: foo :: a"
         :id "-Wmissing-signatures"
         :checker haskell-ghc))))

(flycheck-ert-def-checker-test (haskell-ghc haskell-hlint) haskell
                               complete-chain
  (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
    (flycheck-ert-should-syntax-check
     "language/haskell/Warnings.hs" 'haskell-mode
     '(4 1 warning "Eta reduce
Found:
  spam eggs = map lines eggs
Perhaps:
  spam = map lines" :checker haskell-hlint)
     '(4 1 warning "Top-level binding with no type signature:
  spam :: [String] -> [[String]]"
         :id "-Wmissing-signatures"
         :checker haskell-ghc)
     '(7 8 info "Redundant bracket
Found:
  (putStrLn \"hello world\")
Perhaps:
  putStrLn \"hello world\"" :checker haskell-hlint))))

(flycheck-ert-def-checker-test html-tidy html nil
  (flycheck-ert-should-syntax-check
   "language/html.html" '(html-mode)
   '(3 1 warning "missing <!DOCTYPE> declaration"
       :checker html-tidy)
   '(8 5 error "<spam> is not recognized!"
       :checker html-tidy)
   '(8 5 warning "discarding unexpected <spam>"
       :checker html-tidy)))


(defconst flycheck-test-javascript-modes '(js-mode
                                           js2-mode
                                           js3-mode
                                           js2-jsx-mode
                                           rjsx-mode))

(when (version<= "25" emacs-version)
  (add-to-list 'flycheck-test-javascript-modes 'js-jsx-mode))

(flycheck-ert-def-checker-test javascript-jshint javascript syntax-error
  :tags '(checkstyle-xml)
  ;; Silence JS2 and JS3 parsers
  (let ((js2-mode-show-parse-errors nil)
        (js2-mode-show-strict-warnings nil)
        (js3-mode-show-parse-errors nil)
        (inhibit-message t)
        (flycheck-disabled-checkers
         '(javascript-eslint javascript-gjslint)))
    (flycheck-ert-should-syntax-check
     "language/javascript/syntax-error.js" '(js-mode js2-mode js3-mode rjsx-mode)
     '(3 1 error "Unrecoverable syntax error. (75% scanned)."
         :checker javascript-jshint :id "E041")
     '(3 25 error "Expected an identifier and instead saw ')'."
         :checker javascript-jshint :id "E030"))))

(flycheck-ert-def-checker-test javascript-jshint javascript nil
  :tags '(checkstyle-xml)
  (let ((flycheck-jshintrc "jshintrc")
        (inhibit-message t)
        (flycheck-disabled-checkers
         '(javascript-eslint javascript-gjslint)))
    (flycheck-ert-should-syntax-check
     "language/javascript/warnings.js" '(js-mode js2-mode js3-mode rjsx-mode)
     '(4 9 warning "'foo' is defined but never used." :id "W098"
         :checker javascript-jshint))))

(flycheck-ert-def-checker-test javascript-eslint javascript error
  (let ((flycheck-disabled-checkers '(javascript-jshint))
        (inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/javascript/syntax-error.js" flycheck-test-javascript-modes
     '(3 25 error "Parsing error: Unexpected token )" :checker javascript-eslint))))

(flycheck-ert-def-checker-test javascript-eslint javascript warning
  (let ((flycheck-disabled-checkers '(javascript-jshint))
        (inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/javascript/warnings.js" flycheck-test-javascript-modes
     '(3 2 warning "Use the function form of 'use strict'." :id "strict"
         :checker javascript-eslint
         :end-line 5 :end-column 2)
     '(4 9 warning "'foo' is assigned a value but never used."
         :id "no-unused-vars" :checker javascript-eslint
         :end-line 4 :end-column 12))))

(flycheck-ert-def-checker-test javascript-standard javascript error
  (let ((flycheck-checker 'javascript-standard)
        (inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/javascript/style.js" flycheck-test-javascript-modes
     '(3 10 error "Missing space before function parentheses."
         :checker javascript-standard)
     '(4 1 error "Unexpected tab character."
         :checker javascript-standard)
     '(4 1 error "Expected indentation of 2 spaces but found 1 tab."
         :checker javascript-standard)
     '(4 2 error "Unexpected var, use let or const instead."
         :checker javascript-standard)
     '(4 6 error "'foo' is assigned a value but never used."
         :checker javascript-standard)
     '(4 13 error "Strings must use singlequote."
         :checker javascript-standard)
     '(4 27 error "Extra semicolon."
         :checker javascript-standard)
     '(5 5 error "Extra semicolon."
         :checker javascript-standard))))

(flycheck-ert-def-checker-test javascript-standard javascript semistandard
  (let ((flycheck-checker 'javascript-standard)
        (flycheck-javascript-standard-executable "semistandard")
        (inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/javascript/style.js" flycheck-test-javascript-modes
     '(3 10 error "Missing space before function parentheses."
         :checker javascript-standard)
     '(4 1 error "Unexpected tab character."
         :checker javascript-standard)
     '(4 1 error "Expected indentation of 2 spaces but found 1 tab."
         :checker javascript-standard)
     '(4 2 error "Unexpected var, use let or const instead."
         :checker javascript-standard)
     '(4 6 error "'foo' is assigned a value but never used."
         :checker javascript-standard)
     '(4 13 error "Strings must use singlequote."
         :checker javascript-standard))))

(flycheck-ert-def-checker-test json-jsonlint json nil
  (flycheck-ert-should-syntax-check
   "language/json.json" 'json-mode
   '(1 44 error "found: ',' - expected: 'EOF'." :checker json-jsonlint)))

(flycheck-ert-def-checker-test json-python-json json nil
  (let ((flycheck-disabled-checkers '(json-jsonlint)))
    (flycheck-ert-should-syntax-check
     "language/json.json" 'json-mode
     '(1 44 error "Extra data" :checker json-python-json))))

(flycheck-ert-def-checker-test json-jq json nil
  (let ((flycheck-disabled-checkers '(json-jsonlint json-python-json)))
    (flycheck-ert-should-syntax-check
     "language/json.json" 'json-mode
     '(1 44 error "Expected value before ','" :checker json-jq))))

(flycheck-ert-def-checker-test less less file-error
  (let* ((candidates (list "no-such-file.less"
                           "npm://no-such-file.less"
                           "no-such-file.less"))
         (message (string-join candidates ",")))
    (flycheck-ert-should-syntax-check
     "language/less/file-error.less" 'less-css-mode
     `(3 1 error ,(concat "'no-such-file.less' wasn't found. Tried - "
                          message)
         :checker less))))

(flycheck-ert-def-checker-test less less syntax-error
  (flycheck-ert-should-syntax-check
   "language/less/syntax-error.less" 'less-css-mode
   '(1 1 error "Unrecognised input" :checker less)))

(flycheck-ert-def-checker-test llvm-llc llvm nil
  (flycheck-ert-should-syntax-check
   "language/llvm.ll" 'llvm-mode
   '(4 19 error "'%tmp' defined with type 'i32'" :checker llvm-llc)))

(flycheck-ert-def-checker-test lua-luacheck lua syntax-error
  (flycheck-ert-should-syntax-check
   "language/lua/syntax-error.lua" 'lua-mode
   '(5 7 error "unfinished string" :id "E011" :checker lua-luacheck)))

(flycheck-ert-def-checker-test lua-luacheck lua warnings
  (flycheck-ert-should-syntax-check
   "language/lua/warnings.lua" 'lua-mode
   '(1 1 warning "setting non-standard global variable 'global_var'"
       :id "W111" :checker lua-luacheck)
   '(3 16 warning "unused function 'test'"
       :id "W211" :checker lua-luacheck)
   '(3 21 warning "unused argument 'arg'"
       :id "W212" :checker lua-luacheck)
   '(4 11 warning "variable 'var2' is never set"
       :id "W221" :checker lua-luacheck)))

(flycheck-ert-def-checker-test lua-luacheck lua custom-luacheckrc
  (let ((flycheck-luacheckrc "custom.luacheckrc"))
    (flycheck-ert-should-syntax-check
     "language/lua/warnings.lua" 'lua-mode
     '(1 1 warning "setting non-standard global variable 'global_var'"
         :id "W111" :checker lua-luacheck)
     '(3 16 warning "unused function 'test'"
         :id "W211" :checker lua-luacheck)
     '(4 11 warning "variable 'var2' is never set"
         :id "W221" :checker lua-luacheck))))

(flycheck-ert-def-checker-test lua-luacheck lua custom-standards
  (let ((flycheck-luacheck-standards '("ngx_lua")))
    (flycheck-ert-should-syntax-check
     "language/lua/ngx_lua.warnings.lua" 'lua-mode
     '(3 16 warning "unused function 'test'"
         :id "W211" :checker lua-luacheck)
     '(3 21 warning "unused argument 'arg'"
         :id "W212" :checker lua-luacheck)
     '(4 11 warning "variable 'var2' is never set"
         :id "W221" :checker lua-luacheck))))

(flycheck-ert-def-checker-test lua lua nil
  (let ((flycheck-disabled-checkers '(lua-luacheck)))
    (flycheck-ert-should-syntax-check
     "language/lua/syntax-error.lua" 'lua-mode
     '(5 nil error "unfinished string near '\"oh no'"
         :checker lua))))

(flycheck-ert-def-checker-test opam opam nil
  (flycheck-ert-should-syntax-check
   "language/opam.opam" 'tuareg-opam-mode
   '(0 nil error "Missing field 'maintainer'"
       :id "23" :checker opam)
   '(0 nil warning "Missing field 'authors'"
       :id "25" :checker opam)
   '(0 nil warning "Missing field 'homepage'"
       :id "35" :checker opam)
   '(0 nil warning "Missing field 'bug-reports'"
       :id "36" :checker opam)
   '(2 1 error "Invalid field maintainers"
       :id "3" :checker opam)))

(flycheck-ert-def-checker-test (perl perl-perlcritic) perl nil
  (flycheck-ert-should-syntax-check
   "language/perl.pl" '(perl-mode cperl-mode)
   '(6 nil error "Global symbol \"$x\" requires explicit package name (did you forget to declare \"my $x\"?)"
       :checker perl)
   '(6 nil error "BEGIN not safe after errors--compilation aborted"
       :checker perl)
   '(6 6 error "Glob written as <...> (See page 167 of PBP)"
       :id "BuiltinFunctions::RequireGlobFunction" :checker perl-perlcritic)))

(flycheck-ert-def-checker-test perl perl modules
  ;; Files that require unlisted modules should fail to check
  (flycheck-ert-should-syntax-check
   "language/perl/Script.pl" '(perl-mode cperl-mode)
   '(3 nil error "Global symbol \"$dependency_a\" requires explicit package name (did you forget to declare \"my $dependency_a\"?)"
       :checker perl)
   '(4 nil error "Global symbol \"$dependency_b\" requires explicit package name (did you forget to declare \"my $dependency_b\"?)"
       :checker perl))
  ;; Including those modules should allow them to check
  (let ((flycheck-perl-include-path '("."))
        (flycheck-perl-module-list '("DependencyA")))
    (flycheck-ert-should-syntax-check
     "language/perl/Script.pl" '(perl-mode cperl-mode)
     '(4 nil error "Global symbol \"$dependency_b\" requires explicit package name (did you forget to declare \"my $dependency_b\"?)"
         :checker perl)))
  ;; ;; Multiple modules should be allowed
  (let ((flycheck-perl-include-path '("."))
        (flycheck-perl-module-list '("DependencyA" "DependencyB")))
    (flycheck-ert-should-syntax-check
     "language/perl/Script.pl" '(perl-mode cperl-mode))))

(flycheck-ert-def-checker-test php php syntax-error
  (when (version<= emacs-version "25")
    (ert-skip "PHP mode (via CC mode) seems broken on 24.5."))
  (flycheck-ert-should-syntax-check
   "language/php/syntax-error.php" 'php-mode
   '(8 nil error "Assignments can only happen to writable values" :checker php)))

(flycheck-ert-def-checker-test (php php-phpcs php-phpmd) php nil
  :tags '(phpmd-xml checkstyle-xml)
  (flycheck-ert-should-syntax-check
   "language/php/warnings.php" 'php-mode
   '(1 1 error "Missing file doc comment"
       :id "PEAR.Commenting.FileComment.Missing" :checker php-phpcs)
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

(flycheck-ert-def-checker-test proselint (text markdown) nil
  ;; Unset LC_ALL which is set to LC_ALL=C for other checkers in ./run.el,
  ;; because Click, used by ProseLint, when running with python 3 will refuse to
  ;; work unless an Unicode locale is exported. See:
  ;; http://click.pocoo.org/5/python3/#python-3-surrogate-handling
  (let ((flycheck-disabled-checkers '(markdown-markdownlint-cli markdown-mdl)))
    (flycheck-ert-with-env '(("LC_ALL" . nil))
      (flycheck-ert-should-syntax-check
       "language/text/text.txt" '(text-mode markdown-mode)
       '(1 7 warning "Substitute 'damn' every time you're inclined to write 'very'; your editor will delete it and the writing will be just as it should be."
           :id "weasel_words.very"
           :checker proselint
           :end-line 1
           :end-column 12)
       '(2 4 warning "Redundancy. Use 'associate' instead of 'associate together'."
           :id "redundancy.garner"
           :checker proselint
           :end-line 3
           :end-column 1)
       '(3 5 warning "Gender bias. Use 'lawyer' instead of 'lady lawyer'."
           :id "sexism.misc"
           :checker proselint
           :end-line 3
           :end-column 17)))))

(flycheck-ert-def-checker-test processing processing syntax-error
  (flycheck-ert-should-syntax-check
   "language/processing/syntax_error/syntax_error.pde" 'processing-mode
   '(4 2 error "Syntax error, maybe a missing semicolon?"
       :checker processing)))

(flycheck-ert-def-checker-test protobuf-protoc protobuf syntax-error
  (flycheck-ert-should-syntax-check
   "language/protobuf/protobuf/syntax-error.proto" 'protobuf-mode
   '(2 23 error "Missing field number."
       :checker protobuf-protoc)))

(flycheck-ert-def-checker-test protobuf-prototool protobuf warnings
  (flycheck-ert-should-syntax-check
   "language/protobuf/prototool/missing_syntax.proto" 'protobuf-mode
   '(1 1 warning "No syntax specified. Please use 'syntax = \"proto2\";' or 'syntax = \"proto3\";' to specify a syntax version."
       :checker protobuf-prototool)
   '(4 3 warning "Expected \"required\", \"optional\", or \"repeated\"."
       :checker protobuf-prototool)))

(flycheck-ert-def-checker-test pug pug syntax-error
  (flycheck-ert-should-syntax-check
   "language/pug/pug.pug" 'pug-mode
   '(2 1 error "unexpected token \"indent\"" :checker pug)))

(flycheck-ert-def-checker-test pug pug non-block-or-mixin-at-top-level-of-extended-template-error
  (flycheck-ert-should-syntax-check
   "language/pug/foo.pug" 'pug-mode
   '(9 1
       error "Only named blocks and mixins can appear at the top level of an extending template"
       :checker pug)))
(flycheck-ert-def-checker-test pug pug unknown-filter
  (flycheck-ert-should-syntax-check
   "language/pug/foo-unknown-filter.pug" 'pug-mode
   '(1 1
       error "unknown filter \":myfilter\""
       :checker pug)))

(flycheck-ert-def-checker-test pug pug include-extends-error
  (flycheck-ert-should-syntax-check
   "language/pug/pug-extends.pug" 'pug-mode
   '(1 nil error "the \"basedir\" option is required to use includes and extends with \"absolute\" paths"
       :checker pug)))

(flycheck-ert-def-checker-test pug pug type-error
  (flycheck-ert-should-syntax-check
   "language/pug/pug-runtime-error.pug" 'pug-mode
   '(5 nil  error "Cannot read property 'bar' of undefined"  :checker pug)))

;; N.B. the puppet 4 and 3 tests are mutually exclusive
;; due to one having column and the other not
(flycheck-ert-def-checker-test puppet-parser puppet parser-error-puppet-4
  (skip-unless (version<= "4" (shell-command-to-string
                               "printf %s \"$(puppet --version)\"")))
  (flycheck-ert-should-syntax-check
   "language/puppet/parser-error.pp" 'puppet-mode
   '(3 9 error "Syntax error at '>'" :checker puppet-parser)))

(flycheck-ert-def-checker-test puppet-parser puppet parser-error-puppet-3
  (skip-unless (version<= (shell-command-to-string
                           "printf %s \"$(puppet --version)\"") "4"))
  (flycheck-ert-should-syntax-check
   "language/puppet/puppet3-parser-error.pp" 'puppet-mode
   '(4 3 error "Syntax error at 'helloagain'"
       :checker puppet-parser))
  (flycheck-ert-should-syntax-check
   "language/puppet/puppet3-parser-multiline-error.pp" 'puppet-mode
   '(4 25 error "Unclosed quote after \"'\" followed by '\\n}\\n...'"
       :checker puppet-parser)))

(flycheck-ert-def-checker-test puppet-lint puppet nil
  (flycheck-ert-should-syntax-check
   "language/puppet/warnings.pp" 'puppet-mode
   '(2 nil error "foo::bar not in autoload module layout (autoloader_layout)"
       :checker puppet-lint)
   '(3 nil warning "case statement without a default case (case_without_default)"
       :checker puppet-lint)))

(flycheck-ert-def-checker-test python-flake8 python syntax-error
  (let ((python-indent-guess-indent-offset nil) ; Silence Python Mode!
        (flycheck-python-flake8-executable "python3"))
    (flycheck-ert-should-syntax-check
     "language/python/syntax-error.py" 'python-mode
     '(3 7 error "SyntaxError: invalid syntax" :id "E999"
         :checker python-flake8))))

(flycheck-ert-def-checker-test python-flake8 python nil
  (let ((flycheck-python-flake8-executable "python3"))
    (flycheck-ert-should-syntax-check
     "language/python/test.py" 'python-mode
     '(5 1 warning "'.antigravit' imported but unused" :id "F401"
         :checker python-flake8)
     '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
         :checker python-flake8)
     '(12 29 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(12 31 warning "unexpected spaces around keyword / parameter equals"
          :id "E251" :checker python-flake8)
     '(22 1 error "undefined name 'antigravity'" :id "F821"
          :checker python-flake8))))

(flycheck-ert-def-checker-test python-pyright python nil
  (let ((flycheck-disabled-checkers '(python-mypy))
        (flycheck-checkers '(python-pyright)))
    (flycheck-ert-should-syntax-check
     "language/python/invalid_type.py" 'python-mode
     '(2 12 error "Expression of type \"str\" cannot be assigned to return type \"int\"\n  \"str\" is incompatible with \"int\""
         :checker python-pyright
         :end-line 2
         :end-column 13))))

(flycheck-ert-def-checker-test python-mypy python nil
  (let ((flycheck-disabled-checkers '(python-flake8))
        (flycheck-checkers '(python-mypy))
        (flycheck-python-mypy-cache-dir null-device))
    (flycheck-ert-should-syntax-check
     "language/python/invalid_type.py" 'python-mode
     '(2 12 error "Incompatible return value type (got \"str\", expected \"int\")"
         :checker python-mypy))))

(flycheck-ert-def-checker-test python-pylint python nil
  (let ((flycheck-disabled-checkers '(python-flake8 python-mypy))
        (flycheck-python-pylint-executable "python3"))
    (flycheck-ert-should-syntax-check
     "language/python/test.py" 'python-mode
     '(1 1 info "Missing module docstring" :id "missing-module-docstring" :checker python-pylint)
     '(5 1 error "No name 'antigravit' in module 'python'" :id "no-name-in-module"
         :checker python-pylint)
     '(5 1 warning "Unused import antigravit" :id "unused-import"
         :checker python-pylint)
     '(7 1 info "Missing class docstring" :id "missing-class-docstring" :checker python-pylint)
     '(7 1 warning "Class 'Spam' inherits from object, can be safely removed from bases in python3"
         :id "useless-object-inheritance" :checker python-pylint)
     '(9 5 info "Method name \"withEggs\" doesn't conform to snake_case naming style"
         :id "invalid-name" :checker python-pylint)
     '(9 5 info "Missing function or method docstring" :id "missing-function-docstring" :checker python-pylint)
     '(9 5 warning "Method could be a function" :id "no-self-use"
         :checker python-pylint)
     '(12 5 info "Missing function or method docstring" :id "missing-function-docstring" :checker python-pylint)
     '(12 5 warning "Either all return statements in a function should return an expression, or none of them should."
          :id "inconsistent-return-statements" :checker python-pylint)
     '(12 5 warning "Method could be a function"
          :id "no-self-use" :checker python-pylint)
     '(14 16 error "Module 'sys' has no 'python_version' member" :id "no-member"
          :checker python-pylint)
     '(22 1 error "Undefined variable 'antigravity'" :id "undefined-variable"
          :checker python-pylint))))

(flycheck-ert-def-checker-test python-pylint python no-symbolic-id
  (let ((flycheck-disabled-checkers '(python-flake8 python-mypy))
        (flycheck-pylint-use-symbolic-id nil)
        (flycheck-python-pylint-executable "python3"))
    (flycheck-ert-should-syntax-check
     "language/python/test.py" 'python-mode
     '(1 1 info "Missing module docstring" :id "C0114" :checker python-pylint)
     '(5 1 error "No name 'antigravit' in module 'python'" :id "E0611"
         :checker python-pylint)
     '(5 1 warning "Unused import antigravit" :id "W0611"
         :checker python-pylint)
     '(7 1 info "Missing class docstring" :id "C0115" :checker python-pylint)
     '(7 1 warning "Class 'Spam' inherits from object, can be safely removed from bases in python3"
         :id "R0205" :checker python-pylint)
     '(9 5 info "Method name \"withEggs\" doesn't conform to snake_case naming style"
         :id "C0103" :checker python-pylint)
     '(9 5 info "Missing function or method docstring" :id "C0116" :checker python-pylint)
     '(9 5 warning "Method could be a function" :id "R0201"
         :checker python-pylint)
     '(12 5 info "Missing function or method docstring" :id "C0116" :checker python-pylint)
     '(12 5 warning "Either all return statements in a function should return an expression, or none of them should."
          :id "R1710" :checker python-pylint)
     '(12 5 warning "Method could be a function"
          :id "R0201" :checker python-pylint)
     '(14 16 error "Module 'sys' has no 'python_version' member" :id "E1101"
          :checker python-pylint)
     '(22 1 error "Undefined variable 'antigravity'" :id "E0602"
          :checker python-pylint))))

(flycheck-ert-def-checker-test python-pylint python negative-columns
  (let ((flycheck-disabled-checkers '(python-flake8 python-mypy))
        (python-indent-guess-indent-offset nil) ; Silence Python Mode
        (flycheck-python-pylint-executable "python3"))
    (flycheck-ert-should-syntax-check
     "language/python/gh_1383.py" 'python-mode
     '(2 1 warning "Unused import sys"
         :id "unused-import" :checker python-pylint)
     '(4 1 warning "String statement has no effect"
         :id "pointless-string-statement" :checker python-pylint))))

(flycheck-ert-def-checker-test python-pycompile python python27
  (skip-unless (executable-find "python2"))
  (let ((flycheck-disabled-checkers '(python-flake8 python-pylint python-mypy))
        (flycheck-python-pycompile-executable "python2")
        (python-indent-guess-indent-offset nil))
    (flycheck-ert-should-syntax-check
     "language/python/syntax-error.py" 'python-mode
     `(3 nil error "invalid syntax" :checker python-pycompile))))

(flycheck-ert-def-checker-test python-pycompile python has-no-warnings
  (let ((flycheck-disabled-checkers '(python-flake8 python-pylint python-mypy)))
    (flycheck-ert-should-syntax-check
     "language/python/test.py" 'python-mode)))

(flycheck-ert-def-checker-test r-lintr r nil
  ;; Disable caching in lintr tests to make sure that the file is re-checked
  ;; every time
  (skip-unless (flycheck-r-has-lintr (flycheck-checker-executable 'r-lintr)))
  (let ((flycheck-lintr-caching nil))
    (flycheck-ert-should-syntax-check
     "language/r.R" 'R-mode
     '(1 28 info "Opening curly braces should never go on their own line and should always be followed by a new line."
         :checker r-lintr)
     '(1 56 info "Put spaces around all infix operators." :checker r-lintr)
     '(4 6 warning "Do not use absolute paths." :checker r-lintr)
     '(7 5 error "unexpected end of input" :checker r-lintr))))

(flycheck-ert-def-checker-test racket racket nil
  (skip-unless (funcall (flycheck-checker-get 'racket 'predicate)))
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/racket.rkt" 'racket-mode
     '(4 3 error "read: expected a `)' to close `('" :checker racket))))

(flycheck-ert-def-checker-test rpm-rpmlint rpm nil
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/rpm.spec" '(sh-mode rpm-spec-mode)
     '(1 nil warning "no-cleaning-of-buildroot %clean" :checker rpm-rpmlint)
     '(1 nil warning "no-cleaning-of-buildroot %install" :checker rpm-rpmlint)
     '(1 nil warning "no-buildroot-tag" :checker rpm-rpmlint)
     '(7 nil error "buildarch-instead-of-exclusivearch-tag x86_64"
         :checker rpm-rpmlint)
     '(22 nil warning "macro-in-%changelog %{_bindir}" :checker rpm-rpmlint))))

(flycheck-ert-def-checker-test tcl-nagelfar tcl nil
  (flycheck-ert-should-syntax-check
   "language/tcl/test.tcl" 'tcl-mode
   '(7 nil warning "Expr without braces"
       :checker tcl-nagelfar)
   '(8 nil info "Suspicious variable name \"val_${val}\""
       :checker tcl-nagelfar)
   '(9 nil info "Suspicious variable name \"val_${val}\""
       :checker tcl-nagelfar)
   '(12 nil error "Wrong number of arguments \(4\) to \"set\""
        :checker tcl-nagelfar)))

(flycheck-ert-def-checker-test terraform terraform nil
  (flycheck-ert-should-syntax-check
   "language/terraform/terraform/syntax-error.tf" 'terraform-mode
   '(2 nil error "The \";\" character is not valid. Use newlines to separate arguments and blocks,\nand commas to separate items in collection values."
       :checker terraform)
   '(2 nil error "An argument definition must end with a newline."
       :checker terraform)))

(flycheck-ert-def-checker-test terraform-tflint terraform nil
  (flycheck-ert-should-syntax-check
   "language/terraform/tflint/error.tf" 'terraform-mode
   '(2 12 warning "Module source \"git://hashicorp.com/consul.git\" is not pinned"
       :id "terraform_module_pinned_source" :checker terraform-tflint
       :end-line 2 :end-column 44)
   '(7 19 error "\"t1.2xlarge\" is an invalid value as instance_type"
       :id "aws_instance_invalid_type" :checker terraform-tflint
       :end-line 7 :end-column 31)))

(flycheck-ert-def-checker-test markdown-markdownlint-cli markdown nil
  (flycheck-ert-should-syntax-check
   "language/markdown.md" 'markdown-mode
   '(1 nil error "First line in a file should be a top-level heading [Context: \"## Second Header First\"]"
       :id "MD041/first-line-heading/first-line-h1" :checker markdown-markdownlint-cli)
   '(3 nil error "Multiple consecutive blank lines [Expected: 1; Actual: 2]"
       :id "MD012/no-multiple-blanks" :checker markdown-markdownlint-cli)
   '(4 15 error "Trailing spaces [Expected: 0 or 2; Actual: 7]"
       :id "MD009/no-trailing-spaces" :checker markdown-markdownlint-cli)))

(flycheck-ert-def-checker-test markdown-mdl markdown nil
  (let ((flycheck-disabled-checkers '(markdown-markdownlint-cli)))
    (flycheck-ert-should-syntax-check
     "language/markdown.md" 'markdown-mode
     '(1 nil error "First header should be a top level header"
         :id "MD002" :checker markdown-mdl)
     '(3 nil error "Multiple consecutive blank lines"
         :id "MD012" :checker markdown-mdl)
     '(4 nil error "Trailing spaces"
         :id "MD009" :checker markdown-mdl))))

(flycheck-ert-def-checker-test nix nix nil
  (flycheck-ert-should-syntax-check
   "language/nix/syntax-error.nix" 'nix-mode
   '(3 1 error "syntax error, unexpected IN, expecting ';'," :checker nix)))

(flycheck-ert-def-checker-test nix-linter nix nil
  (flycheck-ert-should-syntax-check
   "language/nix/warnings.nix" 'nix-mode
   '(1 1 warning "LetInInheritRecset" :id "LetInInheritRecset" :checker nix-linter)
   '(3 4 warning "Unneeded `rec` on set" :id "UnneededRec" :checker nix-linter)))

(ert-deftest flycheck-locate-sphinx-source-directory/not-in-a-sphinx-project ()
  :tags '(language-rst)
  (flycheck-ert-with-resource-buffer "language/rst/errors.rst"
    (should-not (flycheck-locate-sphinx-source-directory))))

(ert-deftest flycheck-locate-sphinx-source-directory/in-a-sphinx-project ()
  :tags '(language-rst)
  (flycheck-ert-with-resource-buffer "language/rst/sphinx/index.rst"
    (should (string= (flycheck-locate-sphinx-source-directory)
                     (flycheck-ert-resource-filename "language/rst/sphinx/")))))

(flycheck-ert-def-checker-test rst rst nil
  (flycheck-ert-should-syntax-check
   "language/rst/errors.rst" 'rst-mode
   '(8 nil warning "Title underline too short." :checker rst)
   '(14 nil error "Unexpected section title." :checker rst)
   '(16 nil error "Unknown target name: \"restructuredtext\"." :checker rst)
   '(19 nil warning "Title underline too short." :checker rst)
   '(21 nil error "Unknown target name: \"cool\"." :checker rst)
   '(26 nil error "Unexpected section title." :checker rst)))

(flycheck-ert-def-checker-test rst-sphinx rst nil
  (flycheck-ert-should-syntax-check
   "language/rst/sphinx/index.rst" 'rst-mode
   '(2 nil warning "Title underline too short." :checker rst-sphinx)
   '(9 nil warning "Unknown target name: \"cool\"." :checker rst-sphinx)
   '(9 nil warning "'envvar' reference target not found: FOO"
       :checker rst-sphinx)))

(flycheck-ert-def-checker-test rst-sphinx rst not-outside-of-a-sphinx-project
  (flycheck-ert-with-resource-buffer "language/rst/errors.rst"
    (rst-mode)
    (should-not (flycheck-may-use-checker 'rst-sphinx))))

(flycheck-ert-def-checker-test ruby-rubocop ruby syntax-error
  (flycheck-ert-should-syntax-check
   "language/ruby/syntax-error.rb" 'ruby-mode
   '(5 7 error "unexpected token tCONSTANT (Using Ruby 2.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
       :id "Lint/Syntax"
       :checker ruby-rubocop)
   '(5 24 error "unterminated string meets end of file (Using Ruby 2.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
       :id "Lint/Syntax"
       :checker ruby-rubocop)))

(flycheck-ert-def-checker-test ruby-standard ruby syntax-error
  (let ((flycheck-disabled-checkers '(ruby-rubocop)))
    (flycheck-ert-should-syntax-check
     "language/ruby/syntax-error.rb" 'ruby-mode
     '(5 7 error "unexpected token tCONSTANT (Using Ruby 2.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
         :id "Lint/Syntax"
         :checker ruby-standard)
     '(5 24 error "unterminated string meets end of file (Using Ruby 2.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
         :id "Lint/Syntax"
         :checker ruby-standard))))

(flycheck-ert-def-checker-test ruby-rubylint ruby syntax-error
  (ert-skip "Pending: https://github.com/YorickPeterse/ruby-lint/issues/202")
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-reek)))
    (flycheck-ert-should-syntax-check
     "language/ruby/syntax-error.rb" 'ruby-mode
     '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubylint))))

(flycheck-ert-def-checker-test ruby ruby syntax-error
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-reek ruby-rubylint)))
    (flycheck-ert-should-syntax-check
     "language/ruby/syntax-error.rb" 'ruby-mode
     '(4 nil warning "assigned but unused variable - days" :checker ruby)
     '(5 nil error "syntax error, unexpected constant, expecting end-of-input"
         :checker ruby))))

(flycheck-ert-def-checker-test ruby-jruby ruby syntax-error
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-reek ruby-rubylint ruby)))
    (flycheck-ert-should-syntax-check
     "language/ruby/syntax-error.rb" 'ruby-mode
     '(5 nil error "syntax error, unexpected tCONSTANT" :checker ruby-jruby))))

(flycheck-ert-def-checker-test (ruby-rubocop ruby-reek ruby-rubylint) ruby with-rubylint
  (flycheck-ert-should-syntax-check
   "language/ruby/warnings.rb" 'ruby-mode
   '(1 1 info "Missing frozen string literal comment."
       :id "[Correctable] Style/FrozenStringLiteralComment" :checker ruby-rubocop)
   '(3 nil warning "Person assumes too much for instance variable '@name'"
       :id "InstanceVariableAssumption" :checker ruby-reek)
   '(3 1 info "Missing top-level class documentation comment."
       :id "Style/Documentation" :checker ruby-rubocop)
   '(5 5 warning "unused local variable arr" :checker ruby-rubylint)
   '(5 5 warning "Useless assignment to variable - `arr`."
       :id "Lint/UselessAssignment" :checker ruby-rubocop)
   '(5 11 info "Use `%i` or `%I` for an array of symbols."
       :id "[Correctable] Style/SymbolArray" :checker ruby-rubocop)
   '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
       :id "[Correctable] Style/StringLiterals" :checker ruby-rubocop)
   '(10 5 info "the use of then/do is not needed here" :checker ruby-rubylint)
   '(10 5 info "Use a guard clause (`return unless true`) instead of wrapping the code inside a conditional expression."
        :id "Style/GuardClause":checker ruby-rubocop)
   '(10 5 info "Favor modifier `if` usage when having a single-line body. Another good alternative is the usage of control flow `&&`/`||`."
        :id "[Correctable] Style/IfUnlessModifier" :checker ruby-rubocop)
   '(10 8 warning "Literal `true` appeared as a condition."
        :id "Lint/LiteralAsCondition" :checker ruby-rubocop)
   '(10 13 info "Do not use `then` for multi-line `if`."
        :id "[Correctable] Style/MultilineIfThen" :checker ruby-rubocop)
   '(11 7 info "Redundant `return` detected."
        :id "[Correctable] Style/RedundantReturn" :checker ruby-rubocop)
   '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
   '(16 1 error "wrong number of arguments for 'test' (expected 2..3 but got 0)"
        :checker ruby-rubylint)))

(flycheck-ert-def-checker-test ruby-reek ruby warnings
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint)))
    (flycheck-ert-should-syntax-check
     "language/ruby/warnings.rb" 'ruby-mode
     '(3 nil warning "Person assumes too much for instance variable '@name'"
         :id "InstanceVariableAssumption" :checker ruby-reek))))

(flycheck-ert-def-checker-test ruby ruby warnings
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-reek ruby-rubylint)))
    (flycheck-ert-should-syntax-check
     "language/ruby/warnings.rb" 'ruby-mode
     '(5 nil warning "assigned but unused variable - arr" :checker ruby)
     '(16 nil warning "possibly useless use of == in void context"
          :checker ruby))))

(flycheck-ert-def-checker-test ruby-jruby ruby nil
  (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-reek ruby-rubylint ruby)))
    (flycheck-ert-should-syntax-check
     "language/ruby/warnings.rb" 'ruby-mode
     '(16 nil warning "Useless use of == in void context."
          :checker ruby-jruby))))

(defun flycheck-ert-cargo-clean (manifest-path)
  "Call `cargo clean` on the manifest at MANIFEST-PATH.

The manifest path is relative to
`flycheck-test-resources-directory'."
  ;; `cargo` builds binaries when we call the checker, so we need to start from
  ;; a clean directory each time we want to test.
  (call-process "cargo" nil nil nil "clean" "--manifest-path"
                (expand-file-name manifest-path
                                  flycheck-test-resources-directory)))

(flycheck-ert-def-checker-test rust-cargo rust warning
  (let ((flycheck-disabled-checkers '(rust))
        (flycheck-rust-crate-type "bin")
        (flycheck-rust-binary-name "flycheck-test"))
    (flycheck-ert-cargo-clean "language/rust/flycheck-test/Cargo.toml")
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/warnings.rs" 'rust-mode
     '(3 4 warning "function is never used: `main`"
         :checker rust-cargo :id "dead_code" :group 1
         :end-line 3 :end-column 8)
     '(3 4 info "`#[warn(dead_code)]` on by default"
         :checker rust-cargo :id "dead_code" :group 1
         :end-line 3 :end-column 8)
     '(4 9 warning "unused variable: `x`"
         :checker rust-cargo :id "unused_variables" :group 2
         :end-line 4 :end-column 10)
     '(4 9 info "if this is intentional, prefix it with an underscore: `_x`"
         :checker rust-cargo :id "unused_variables" :group 2
         :end-line 4 :end-column 10))))

(flycheck-ert-def-checker-test rust-cargo rust default-target
  (let ((flycheck-disabled-checkers '(rust))
        (flycheck-rust-crate-type nil)
        (flycheck-rust-binary-name nil))
    (flycheck-ert-cargo-clean "language/rust/flycheck-test/Cargo.toml")
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/warnings.rs" 'rust-mode
     '(3 4 warning "function is never used: `main`"
         :checker rust-cargo :id "dead_code" :group 1
         :end-line 3 :end-column 8)
     '(3 4 info "`#[warn(dead_code)]` on by default"
         :checker rust-cargo :id "dead_code" :group 1
         :end-line 3 :end-column 8)
     '(4 9 warning "unused variable: `x`"
         :checker rust-cargo :id "unused_variables" :group 2
         :end-line 4 :end-column 10)
     '(4 9 info "if this is intentional, prefix it with an underscore: `_x`"
         :checker rust-cargo :id "unused_variables" :group 2
         :end-line 4 :end-column 10))))

(flycheck-ert-def-checker-test rust-cargo rust lib-main
  (let ((flycheck-disabled-checkers '(rust))
        (flycheck-rust-crate-type "bin")
        (flycheck-rust-binary-name "lib-main"))
    (flycheck-ert-should-syntax-check
     "language/rust/lib-main/src/main.rs" 'rust-mode
     `(3 12 error "cannot find value `zorglub` in this scope (not found in this scope)"
         :checker rust-cargo :id "E0425"
         :filename ,(flycheck-ert-resource-filename "language/rust/lib-main/src/lib.rs")
         :group 1
         :end-line 3 :end-column 19))))

(flycheck-ert-def-checker-test rust-cargo rust conventional-layout
  (let ((flycheck-disabled-checkers '(rust)))
    (let ((flycheck-rust-crate-type "lib"))
      (flycheck-ert-cargo-clean "language/rust/cargo-targets/Cargo.toml")
      (flycheck-ert-should-syntax-check
       "language/rust/cargo-targets/src/lib.rs" 'rust-mode
       '(3 4 warning "function is never used: `foo_lib`"
           :checker rust-cargo :id "dead_code" :group 1
           :end-line 3 :end-column 11)
       '(6 17 warning "unused variable: `foo_lib_test`"
           :checker rust-cargo  :id "unused_variables" :group 2
           :end-line 6 :end-column 29)
       '(6 17 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 6 :end-column 29)
       '(6 17 info "if this is intentional, prefix it with an underscore: `_foo_lib_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 6 :end-column 29)))

    (let ((flycheck-rust-crate-type "lib"))
      (flycheck-ert-cargo-clean "language/rust/cargo-targets/Cargo.toml")
      (flycheck-ert-should-syntax-check
       "language/rust/cargo-targets/src/a.rs" 'rust-mode
       '(1 4 warning "function is never used: `foo_a`"
           :checker rust-cargo :id "dead_code" :group 1
           :end-line 1 :end-column 9)
       '(1 4 info "`#[warn(dead_code)]` on by default"
           :checker rust-cargo :id "dead_code" :group 1
           :end-line 1 :end-column 9)
       '(4 17 warning "unused variable: `foo_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 27)
       '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 27)))

    (let ((flycheck-rust-crate-type "bin")
          (flycheck-rust-binary-name "cargo-targets"))
      (flycheck-ert-cargo-clean "language/rust/cargo-targets/Cargo.toml")
      (flycheck-ert-should-syntax-check
       "language/rust/cargo-targets/src/main.rs" 'rust-mode
       '(1 17 warning "unused variable: `foo_main`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 25)
       '(1 17 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 25)
       '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_main`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 25)
       '(4 17 warning "unused variable: `foo_main_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 30)
       '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_main_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 30)))

    (let ((flycheck-rust-crate-type "bin")
          (flycheck-rust-binary-name "a"))
      (flycheck-ert-cargo-clean "language/rust/cargo-targets/Cargo.toml")
      (flycheck-ert-should-syntax-check
       "language/rust/cargo-targets/src/bin/a.rs" 'rust-mode
       '(1 17 warning "unused variable: `foo_bin_a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 26)
       '(1 17 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 26)
       '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_bin_a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 26)
       '(4 17 warning "unused variable: `foo_bin_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 31)
       '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_bin_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 31)))

    (let ((flycheck-rust-crate-type "bench")
          (flycheck-rust-binary-name "a"))
      (flycheck-ert-cargo-clean "language/rust/cargo-targets/Cargo.toml")
      (flycheck-ert-should-syntax-check
       "language/rust/cargo-targets/benches/a.rs" 'rust-mode
       '(1 17 warning "unused variable: `foo_bench_a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 28)
       '(1 17 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 28)
       '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_bench_a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 28)
       '(4 17 warning "unused variable: `foo_bench_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 33)
       '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_bench_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 33)))

    (let ((flycheck-rust-crate-type "test")
          (flycheck-rust-binary-name "a"))
      (flycheck-ert-cargo-clean "language/rust/cargo-targets/Cargo.toml")
      (flycheck-ert-should-syntax-check
       "language/rust/cargo-targets/tests/a.rs" 'rust-mode
       '(2 16 warning "unused variable: `foo_test_a_test`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 2 :end-column 31)
       '(2 16 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 2 :end-column 31)
       '(2 16 info "if this is intentional, prefix it with an underscore: `_foo_test_a_test`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 2 :end-column 31)
       '(4 4 warning "function is never used: `foo_test_a`"
           :checker rust-cargo :id "dead_code" :group 2
           :end-line 4 :end-column 14)
       '(4 4 info "`#[warn(dead_code)]` on by default"
           :checker rust-cargo :id "dead_code" :group 2
           :end-line 4 :end-column 14)))

    (let ((flycheck-rust-crate-type "example")
          (flycheck-rust-binary-name "a"))
      (flycheck-ert-cargo-clean "language/rust/cargo-targets/Cargo.toml")
      (flycheck-ert-should-syntax-check
       "language/rust/cargo-targets/examples/a.rs" 'rust-mode
       '(1 17 warning "unused variable: `foo_ex_a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 25)
       '(1 17 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 25)
       '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_ex_a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 1 :end-column 25)
       '(4 17 warning "unused variable: `foo_ex_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 30)
       '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_ex_a_test`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 30)))))

(flycheck-ert-def-checker-test rust-cargo rust workspace-subcrate
  (let ((flycheck-disabled-checkers '(rust))
        (flycheck-rust-crate-type "lib")
        (flycheck-rust-check-tests t))
    (flycheck-ert-cargo-clean "language/rust/workspace/crate1/Cargo.toml")
    (flycheck-ert-should-syntax-check
     "language/rust/workspace/crate1/src/lib.rs" 'rust-mode
     '(2 7 warning "unused variable: `a`"
         :checker rust-cargo :id "unused_variables" :group 1
         :end-line 2 :end-column 8)
     '(2 7 info "`#[warn(unused_variables)]` on by default"
         :checker rust-cargo :id "unused_variables" :group 1
         :end-line 2 :end-column 8)
     '(2 7 info "if this is intentional, prefix it with an underscore: `_a`"
         :checker rust-cargo :id "unused_variables" :group 1
         :end-line 2 :end-column 8))))

(flycheck-ert-def-checker-test rust-cargo rust dev-dependencies
  (let ((flycheck-disabled-checkers '(rust))
        (flycheck-rust-crate-type "lib")
        (flycheck-rust-check-tests t))
    (flycheck-ert-cargo-clean "language/rust/dev-deps/Cargo.toml")
    (flycheck-ert-should-syntax-check
     "language/rust/dev-deps/src/lib.rs" 'rust-mode
     '(2 1 warning "unused `#[macro_use]` import"
         :checker rust-cargo :id "unused_imports" :group 1
         :end-line 2 :end-column 13)
     '(2 1 info "`#[warn(unused_imports)]` on by default"
         :checker rust-cargo :id "unused_imports" :group 1
         :end-line 2 :end-column 13)
     '(8 9 warning "unused variable: `foo`"
         :checker rust-cargo :id "unused_variables" :group 2
         :end-line 8 :end-column 12)
     '(8 9 info "`#[warn(unused_variables)]` on by default"
         :checker rust-cargo :id "unused_variables" :group 2
         :end-line 8 :end-column 12)
     '(8 9 info "if this is intentional, prefix it with an underscore: `_foo`"
         :checker rust-cargo :id "unused_variables" :group 2
         :end-line 8 :end-column 12))))

(flycheck-ert-def-checker-test rust rust syntax-error
  (let ((flycheck-disabled-checkers '(rust-cargo)))
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/syntax-error.rs" 'rust-mode
     '(4 5 error "cannot find value `bla` in this scope (not found in this scope)"
         :checker rust :id "E0425" :group 1 :end-line 4 :end-column 8))))

(flycheck-ert-def-checker-test rust rust type-error
  (let ((flycheck-disabled-checkers '(rust-cargo)))
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/multiline-error.rs" 'rust-mode
     '(7 9 error "mismatched types (expected `u8`, found `i8`)"
         :checker rust :id "E0308" :group 1
         :end-line 7 :end-column 10)
     '(7 9 info "you can convert an `i8` to a `u8` and panic if the converted value doesn't fit: `i.try_into().unwrap()`"
         :checker rust :id "E0308" :group 1
         :end-line 7 :end-column 10))))

(flycheck-ert-def-checker-test rust rust warning
  (let ((flycheck-disabled-checkers '(rust-cargo)))
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/warnings.rs" 'rust-mode
     '(4 9 warning "unused variable: `x`"
         :checker rust :id "unused_variables" :group 1
         :end-line 4 :end-column 10)
     '(4 9 info "`#[warn(unused_variables)]` on by default"
         :checker rust :id "unused_variables" :group 1
         :end-line 4 :end-column 10)
     '(4 9 info "if this is intentional, prefix it with an underscore: `_x`"
         :checker rust :id "unused_variables" :group 1
         :end-line 4 :end-column 10))))

(flycheck-ert-def-checker-test rust rust note-and-help
  (let ((flycheck-disabled-checkers '(rust-cargo)))
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/note-and-help.rs" 'rust-mode
     '(10 9 info "move occurs because `_x` has type `NonPOD`, which does not implement the `Copy` trait"
          :checker rust :id "E0382" :group 1
          :end-line 10 :end-column 11)
     '(11 14 info "value moved here"
          :checker rust :id "E0382" :group 1
          :end-line 11 :end-column 16)
     '(12 14 error "use of moved value: `_x` (value used here after move)"
          :checker rust :id "E0382" :group 1
          :end-line 12 :end-column 16))))

(flycheck-ert-def-checker-test rust rust crate-root-not-set
  (let ((flycheck-disabled-checkers '(rust-cargo)))
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/importing.rs" 'rust-mode
     '(1 5 error "failed to resolve: there are too many leading `super` keywords (there are too many leading `super` keywords)"
         :checker rust :id "E0433" :group 2
         :end-line 1 :end-column 10)
     '(4 24 error "failed to resolve: use of undeclared crate or module `imported` (use of undeclared crate or module `imported`)"
         :checker rust :id "E0433" :group 3
         :end-line 4 :end-column 32))))

(flycheck-ert-def-checker-test rust rust macro-error
  (let ((flycheck-disabled-checkers '(rust-cargo)))
    (flycheck-ert-should-syntax-check
     "language/rust/flycheck-test/src/macro-error.rs" 'rust-mode
     '(2 13 error "1 positional argument in format string, but no arguments were given"
         :checker rust :group 1
         :end-line 2 :end-column 15))))

(flycheck-ert-def-checker-test sass sass nil
  (let ((flycheck-disabled-checkers '(sass/scss-sass-lint)))
    (flycheck-ert-should-syntax-check
     "language/sass/error.sass" 'sass-mode
     '(5 nil error "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces."
         :checker sass))))

(flycheck-ert-def-checker-test sass sass warning
  (let ((flycheck-disabled-checkers '(sass/scss-sass-lint)))
    (flycheck-ert-should-syntax-check
     "language/sass/warning.sass" 'sass-mode
     '(2 nil warning "this is deprecated" :checker sass))))

(flycheck-ert-def-checker-test scala scala nil
  (flycheck-ert-should-syntax-check
   "language/scala/syntax-error.scala" 'scala-mode
   '(3 nil error "identifier expected but '{' found." :checker scala)))

(flycheck-ert-def-checker-test scala-scalastyle scala error
  (let ((flycheck-scalastylerc "scalastyle.xml"))
    (flycheck-ert-should-syntax-check
     "language/scala/style-error.scala" 'scala-mode
     '(6 5 error "Don't use println" :checker scala-scalastyle))))

(flycheck-ert-def-checker-test scala-scalastyle scala warning
  (let ((flycheck-scalastylerc "scalastyle.xml"))
    (flycheck-ert-should-syntax-check
     "language/scala/style-warning.scala" 'scala-mode
     '(5 9 warning "Redundant braces after class definition"
         :checker scala-scalastyle))))

(defvar geiser-impl--implementation)

(defun flycheck/chicken-mode ()
  "Enable Scheme and Geiser mode for Chicken scheme."
  (interactive)
  (scheme-mode)
  (setq-local geiser-impl--implementation 'chicken)
  (geiser-mode))

(flycheck-ert-def-checker-test scheme-chicken scheme error-no-line-number
  (skip-unless (version<= "25.1" emacs-version))
  (flycheck-ert-should-syntax-check
   "language/chicken/error-no-line-number.scm" 'flycheck/chicken-mode
   '(0 nil error "(cddr) during expansion of (for-each ...) - bad argument type: ()\n\n\tCall history:\n\n\t<syntax>\t  (##core#begin (for-each))\n\t<syntax>\t  (for-each)\t<--"
       :checker scheme-chicken)))

(flycheck-ert-def-checker-test scheme-chicken scheme syntax-error
  (skip-unless (version<= "25.1" emacs-version))
  (flycheck-ert-should-syntax-check
   "language/chicken/syntax-error.scm" 'flycheck/chicken-mode
   '(1 nil error "not enough arguments\n\n\t(define)\n\n\tExpansion history:\n\n\t<syntax>\t  (##core#begin (define))\n\t<syntax>\t  (define)\t<--"
       :checker scheme-chicken)))

(flycheck-ert-def-checker-test scheme-chicken scheme syntax-error-no-line-number
  (skip-unless (version<= "25.1" emacs-version))
  (flycheck-ert-should-syntax-check
   "language/chicken/syntax-error-no-line-number.scm" 'flycheck/chicken-mode
   '(0 nil error "illegal atomic form\n\n\t()\n\n\tExpansion history:\n\n\t<syntax>\t  (##core#begin ())\t<--"
       :checker scheme-chicken)))

(flycheck-ert-def-checker-test scheme-chicken scheme syntax-read-error
  (skip-unless (version<= "25.1" emacs-version))
  (flycheck-ert-should-syntax-check
   "language/chicken/syntax-read-error.scm" 'flycheck/chicken-mode
   '(1 nil error "invalid sharp-sign read syntax: #\\n"
       :checker scheme-chicken)))

(flycheck-ert-def-checker-test scss-lint scss nil
  (let ((flycheck-scss-lintrc "scss-lint.yml"))
    (flycheck-ert-should-syntax-check
     "language/scss/lint-error.scss" 'scss-mode
     '(1 1 error "Avoid using id selectors"
         :checker scss-lint :id "IdSelector")
     '(3 16 warning "Color `red` should be written in hexadecimal form as `#ff0000`"
         :checker scss-lint :id "ColorKeyword"))))

(flycheck-ert-def-checker-test scss-lint scss syntax-error
  (flycheck-ert-should-syntax-check
   "language/scss/error.scss" 'scss-mode
   '(3 1 error "Syntax Error: Invalid CSS after \"...    c olor: red\": expected \"{\", was \";\""
       :checker scss-lint :id "Syntax")))

(flycheck-ert-def-checker-test scss scss nil
  (let ((flycheck-disabled-checkers '(scss-lint scss-stylelint sass/scss-sass-lint)))
    (flycheck-ert-should-syntax-check
     "language/scss/error.scss" 'scss-mode
     '(3 nil error "Invalid CSS after \"...    c olor: red\": expected \"{\", was \";\""
         :checker scss))))

(flycheck-ert-def-checker-test scss scss warning
  (let ((flycheck-disabled-checkers '(scss-lint scss-stylelint sass/scss-sass-lint)))
    (flycheck-ert-should-syntax-check
     "language/scss/warning.scss" 'scss-mode
     '(2 nil warning ".container is deprecated" :checker scss))))

(flycheck-ert-def-checker-test sh-bash (sh sh-bash) nil
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/sh/bash-syntax-error.bash" 'sh-mode
     '(5 nil error "syntax error near unexpected token `fi'" :checker sh-bash)
     '(5 nil error "`fi'" :checker sh-bash))))

(flycheck-ert-def-checker-test sh-posix-dash (sh sh-posix) nil
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/sh/posix-syntax-error.sh" 'sh-mode
     '(3 nil error "Syntax error: \"(\" unexpected" :checker sh-posix-dash))))

(flycheck-ert-def-checker-test sh-posix-bash (sh sh-posix) nil
  (let ((flycheck-disabled-checkers '(sh-posix-dash))
        (inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/sh/posix-syntax-error.sh" 'sh-mode
     '(3 nil error "syntax error near unexpected token `('"
         :checker sh-posix-bash)
     '(3 nil error "`cat <(echo blah)'" :checker sh-posix-bash))))

(flycheck-ert-def-checker-test sh-zsh (sh sh-zsh) nil
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/sh/zsh-syntax-error.zsh" 'sh-mode
     '(5 nil error "parse error near `fi'" :checker sh-zsh))))

(flycheck-ert-def-checker-test sh-shellcheck sh nil
  :tags '(checkstyle-xml)
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/sh/shellcheck.sh" 'sh-mode
     '(2 5 warning "Tilde does not expand in quotes. Use $HOME."
         :checker sh-shellcheck :id "SC2088")
     '(3 7 error "Double quote array expansions to avoid re-splitting elements."
         :checker sh-shellcheck :id "SC2068")
     '(4 8 warning "Declare and assign separately to avoid masking return values."
         :checker sh-shellcheck :id "SC2155")
     '(4 11 info "Use $(...) notation instead of legacy backticked `...`."
         :checker sh-shellcheck :id "SC2006")
     '(4 12 info "which is non-standard. Use builtin 'command -v' instead."
         :checker sh-shellcheck :id "SC2230"))))

(flycheck-ert-def-checker-test slim slim nil
  (flycheck-ert-should-syntax-check
   "language/slim.slim" 'slim-mode
   `(2 1 error "Unexpected indentation" :checker slim)))

(flycheck-ert-def-checker-test sqlint sql nil
  (flycheck-ert-should-syntax-check
   "language/sql.sql" 'sql-mode
   `(1 15 error "unterminated quoted string at or near \"';\n  \""
       :checker sql-sqlint)))

(flycheck-ert-def-checker-test systemd-analyze systemd nil
  (flycheck-ert-should-syntax-check
   "language/systemd-analyze-test.service" 'systemd-mode
   '(0 nil error "Service has no ExecStart=, ExecStop=, or SuccessAction=. Refusing."
       :checker systemd-analyze)
   '(3 nil error "Invalid URL, ignoring: foo://bar"
       :checker systemd-analyze)
   '(5 nil error "ListenStream= references a path below legacy directory /var/run/, updating /var/run/dbus/system_bus_socket → /run/dbus/system_bus_socket; please update the unit file accordingly."
       :filename "/lib/systemd/system/dbus.socket"
       :checker systemd-analyze)
   '(6 nil error "Unknown key name 'ExecSmart' in section 'Service', ignoring."
       :checker systemd-analyze)
   '(8 nil error "Unknown section 'Dog'. Ignoring."
       :checker systemd-analyze)))

(flycheck-ert-def-checker-test tex-chktex (tex latex) nil
  (flycheck-ert-should-syntax-check
   "language/tex.tex" 'latex-mode
   '(5 29 warning "Intersentence spacing (`\\@') should perhaps be used."
       :id "13" :checker tex-chktex)))

(flycheck-ert-def-checker-test tex-lacheck (tex latex) nil
  (let ((flycheck-disabled-checkers '(tex-chktex)))
    (flycheck-ert-should-syntax-check
     "language/tex.tex" 'latex-mode
     '(5 nil warning "missing `\\@' before `.' in \"GNU.\""
         :checker tex-lacheck)
     '(7 nil warning "possible unwanted space at \"{\""
         :checker tex-lacheck))))

(flycheck-ert-def-checker-test texinfo texinfo nil
  (flycheck-ert-should-syntax-check
   "language/texinfo.texi" 'texinfo-mode
   '(3 nil warning "@settitle missing argument" :checker texinfo)
   '(7 nil error "unknown command `bold'" :checker texinfo)
   '(7 nil error "misplaced {" :checker texinfo)
   '(7 nil error "misplaced }" :checker texinfo)
   '(9 nil warning "printindex before document beginning: @printindex cp"
       :checker texinfo)))

(flycheck-ert-def-checker-test textlint (text markdown) nil
  (let ((flycheck-disabled-checkers '(proselint markdown-markdownlint-cli markdown-mdl))
        (flycheck-textlint-config "language/text/textlintrc.json"))
    (flycheck-ert-should-syntax-check
     "language/text/text.txt" '(text-mode markdown-mode)
     '(1 7 error "\"very\" is a weasel word and can weaken meaning"
         :id "write-good" :checker textlint))))

(flycheck-ert-def-checker-test typescript-tslint typescript nil
  (let ((flycheck-disabled-checkers '(javascript-eslint)))
    (flycheck-ert-should-syntax-check
     "language/typescript/sample.ts" 'typescript-mode
     '(2 23 warning "Module 'chai' is not listed as dependency in package.json"
         :checker typescript-tslint :id "no-implicit-dependencies"
         :end-line 2 :end-column 29)
     '(5 3 warning "Forbidden 'var' keyword, use 'let' or 'const' instead"
         :checker typescript-tslint :id "no-var-keyword"
         :end-line 5 :end-column 6)
     '(6 15 warning "Missing semicolon"
         :checker typescript-tslint :id "semicolon"
         :end-line 6 :end-column 15))))

(flycheck-ert-def-checker-test verilog-verilator verilog error
  (flycheck-ert-should-syntax-check
   "language/verilog/verilator_error.v" 'verilog-mode
   '(4 nil error "Unsupported: $fopen with multichannel descriptor.  Add ,\"w\" as second argument to open a file descriptor."
       :checker verilog-verilator)))

(flycheck-ert-def-checker-test verilog-verilator verilog warning
  (flycheck-ert-should-syntax-check
   "language/verilog/verilator_warning.v" 'verilog-mode
   '(2 nil warning "Signal is not driven, nor used: 'val'"
       :checker verilog-verilator)))

(flycheck-ert-def-checker-test vhdl-ghdl vhdl error
  (flycheck-ert-should-syntax-check
   "language/vhdl.vhd" 'vhdl-mode
   '(4 1 error "';' is expected instead of '<EOF>'"
       :checker vhdl-ghdl)))

(flycheck-ert-def-checker-test xml-xmlstarlet xml nil
  (let ((inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/xml.xml" 'nxml-mode
     '(4 10 error "Opening and ending tag mismatch: spam line 3 and with"
         :checker xml-xmlstarlet))))

(flycheck-ert-def-checker-test xml-xmllint xml nil
  (let ((flycheck-disabled-checkers '(xml-xmlstarlet))
        (inhibit-message t))
    (flycheck-ert-should-syntax-check
     "language/xml.xml" 'nxml-mode
     '(4 nil error "parser error : Opening and ending tag mismatch: spam line 3 and with"
         :checker xml-xmllint)
     '(5 nil error "parser error : Extra content at the end of the document"
         :checker xml-xmllint))))

(flycheck-ert-def-checker-test yaml-jsyaml yaml nil
  (flycheck-ert-should-syntax-check
   "language/yaml.yaml" 'yaml-mode
   '(4 5 error "bad indentation of a mapping entry"
       :checker yaml-jsyaml)))

(flycheck-ert-def-checker-test yaml-ruby yaml nil
  (let ((flycheck-disabled-checkers '(yaml-jsyaml)))
    (flycheck-ert-should-syntax-check
     "language/yaml.yaml" 'yaml-mode
     '(4 5 error "mapping values are not allowed in this context"
         :checker yaml-ruby))))

(flycheck-ert-def-checker-test yaml-yamllint yaml nil
  (let ((flycheck-disabled-checkers '(yaml-yamlyaml)))
    (flycheck-ert-should-syntax-check
     "language/yaml.yaml" 'yaml-mode
     '(3 1 warning "missing document start "---" (document-start)"
         :checker yaml-yamllint))
    (flycheck-ert-should-syntax-check
     "language/yaml.yaml" 'yaml-mode
     '(4 5 error "syntax error: mapping values are not allowed here"
         :checker yaml-yamllint))))

(flycheck-ert-def-checker-test jsonnet jsonnet nil
  (flycheck-ert-should-syntax-check
   "language/jsonnet/static_error.jsonnet" 'jsonnet-mode
   '(1 23 "Not a unary operator: =" :checker jsonnet)))

(flycheck-ert-def-checker-test jsonnet jsonnet nil
  (flycheck-ert-should-syntax-check
   "language/jsonnet/runtime_error.jsonnet" 'jsonnet-mode
   '(2 6 "Field does not exist: flat" :checker jsonnet
       :end-line 2 :end-column 14)))

(flycheck-ert-initialize flycheck-test-resources-directory)

(provide 'flycheck-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-test.el ends here
