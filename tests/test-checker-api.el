;;; test-checker-api.el --- Tests for checker API -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert)
(require 'flycheck)

(ert-deftest flycheck-checker-modes ()
  "Test the :modes of checkers."
  (dolist (checker flycheck-checkers)
    (should (listp (flycheck-checker-modes checker)))
    (should (-all? #'symbolp (flycheck-checker-modes checker)))))

(ert-deftest flycheck-checker-executable ()
  "Test the :command of checkers."
  (dolist (checker flycheck-checkers)
    (should (equal (flycheck-checker-executable checker)
                   (car (flycheck-checker-command checker))))
    (should (stringp (flycheck-checker-executable checker)))))

(ert-deftest flycheck-check-executable ()
  "Test the verification of checker executables."
  (dolist (checker flycheck-checkers)
    (if (executable-find (flycheck-checker-executable checker))
        (should (flycheck-check-executable checker))
      (should-not (flycheck-check-executable checker)))))

(defvar flycheck-test-config-var)
(defvar flycheck-test-option-var)

(ert-deftest flycheck-substitute-argument-cell-config-file ()
  "Test substitution of `config-file' argument cell."
  ;; We need a real buffer for config-file search
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (let ((makefile-path (expand-file-name "../Makefile" flycheck-testsuite-dir))
          flycheck-test-config-var)
      ;; A non-existing file
      (setq flycheck-test-config-var
            "no-such-file-should-ever-exist-really")
      (should-not (flycheck-substitute-argument-cell
                   '(config-file "--foo" flycheck-test-config-var)))
      ;; An existing file, first by name and then by relative and absolute path
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-argument-cell
                      '(config-file "--foo" flycheck-test-config-var))
                     (list "--foo" makefile-path)))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-argument-cell
                      '(config-file "--foo" flycheck-test-config-var))
                     (list "--foo" makefile-path)))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-argument-cell
                      '(config-file "--foo" flycheck-test-config-var))
                     (list "--foo" makefile-path)))
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-argument-cell
                      '(config-file "--foo=" flycheck-test-config-var))
                     (list (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-argument-cell
                      '(config-file "--foo=" flycheck-test-config-var))
                     (list (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-argument-cell
                      '(config-file "--foo=" flycheck-test-config-var))
                     (list (concat "--foo=" makefile-path))))
      ;; Find a file from the home directory
      (let* ((filename (-first #'file-regular-p
                               (directory-files (getenv "HOME") :full-names))))
        (unless filename
          ;; Let the test fail if there is no file in $HOME
          (error "No file in $HOME found."))
        (setq flycheck-test-config-var (file-name-nondirectory filename))
        (should (equal (flycheck-substitute-argument-cell
                        '(config-file "--bar" flycheck-test-config-var))
                       (list "--bar" filename)))))))

(ert-deftest flycheck-substitute-argument-cell-option ()
  "Test substitution of `option' argument cell."
  (let ((flycheck-test-option-var "bar"))
    (should (equal (flycheck-substitute-argument-cell
                    '(option "--foo" flycheck-test-option-var))
                   '("--foo" "bar")))
     (should (equal (flycheck-substitute-argument-cell
                    '(option "--foo=" flycheck-test-option-var))
                   '("--foo=bar"))))
  (let ((flycheck-test-option-var 100))
    (should-error (flycheck-substitute-argument-cell
                   '(option "--foo" flycheck-test-option-var)))
    (should (equal (flycheck-substitute-argument-cell
                    '(option "--foo" flycheck-test-option-var number-to-string))
                   '("--foo" "100")))
    (should (equal (flycheck-substitute-argument-cell
                    '(option "--foo=" flycheck-test-option-var number-to-string))
                   '("--foo=100"))))
  (let (flycheck-test-option-var)
    (should-not (flycheck-substitute-argument-cell
                 '(option "--foo" flycheck-test-option-var)))
    (should-not (flycheck-substitute-argument-cell
                 '(option "--foo" flycheck-test-option-var #'number-to-string)))
    (should-not (flycheck-substitute-argument-cell
                 '(option "--foo=" flycheck-test-option-var #'number-to-string)))))

(ert-deftest flycheck-substitute-argument-cell-eval ()
  "Test substitution of `eval' argument cell."
  (let ((flycheck-test-option-var '("Hello " "World")))
    (should (equal (flycheck-substitute-argument-cell '(eval flycheck-test-option-var))
                   '("Hello " "World"))))
  (should (equal (flycheck-substitute-argument-cell
                  '(eval (concat "Hello" "World")))
                 "HelloWorld"))
  (should-not (flycheck-substitute-argument-cell
               '(eval (when (string= "foo" "bar") "yes"))))
  (should-error (flycheck-substitute-argument-cell '(eval 100)))
  (should-error (flycheck-substitute-argument-cell '(eval '("foo" 100)))))

(ert-deftest flycheck-substitute-argument-cell-unknown ()
  "Test substitution of an unknown argument cell."
  (should-error (flycheck-substitute-argument-cell '(foo "bar"))))

(ert-deftest flycheck-substitute-argument-symbol ()
  "Test substitution of argument symbols."
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (unwind-protect
        (progn
          (should (equal (flycheck-substitute-argument-symbol 'source-original)
                         (buffer-file-name)))
          (let ((filename (flycheck-substitute-argument-symbol 'source-inplace)))
            (should (equal filename (flycheck-testsuite-resource-filename
                                     "flycheck-substitute-dummy")))
            (should (file-exists-p filename)))
          (let ((filename (flycheck-substitute-argument-symbol 'source)))
            (should-not (equal (file-name-directory filename)
                               (file-name-as-directory flycheck-testsuite-resources-dir)))
            (should (file-exists-p filename)))
          (should-error (flycheck-substitute-argument-symbol 'foobar))))
    (flycheck-clean-files flycheck-temp-buffer-copies)))

(ert-deftest flycheck-substitute-shell-argument-cell-config-file ()
  "Test substitution of `config-file' shell argument cell."
  ;; We need a real buffer for config-file search
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (let ((makefile-path (expand-file-name "../Makefile" flycheck-testsuite-dir))
          flycheck-test-config-var)
      ;; A non-existing file
      (setq flycheck-test-config-var
            "no-such-file-should-ever-exist-really")
      (should (equal (flycheck-substitute-shell-argument-cell
                      '(config-file "--foo" flycheck-test-config-var))
                     ""))
      ;; An existing file, first by name and then by relative and absolute path
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-shell-argument-cell
                      '(config-file "--foo" flycheck-test-config-var))
                     (concat "--foo " (shell-quote-argument makefile-path))))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-shell-argument-cell
                      '(config-file "--foo" flycheck-test-config-var))
                     (concat "--foo " (shell-quote-argument makefile-path))))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-shell-argument-cell
                      '(config-file "--foo" flycheck-test-config-var))
                     (concat "--foo " (shell-quote-argument makefile-path))))
      ;; The same with an option ending with a =
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-shell-argument-cell
                      '(config-file "--foo=" flycheck-test-config-var))
                     (shell-quote-argument (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-shell-argument-cell
                      '(config-file "--foo=" flycheck-test-config-var))
                     (shell-quote-argument (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-shell-argument-cell
                      '(config-file "--foo=" flycheck-test-config-var))
                     (shell-quote-argument (concat "--foo=" makefile-path))))
      ;; Find a file from the home directory
      (let* ((filename (-first #'file-regular-p
                               (directory-files (getenv "HOME") :full-names))))
        (unless filename
          ;; Let the test fail if there is no file in $HOME
          (error "No file in $HOME found."))
        (setq flycheck-test-config-var (file-name-nondirectory filename))
        (should (equal (flycheck-substitute-shell-argument-cell
                        '(config-file "--bar" flycheck-test-config-var))
                       (concat "--bar " (shell-quote-argument filename))))))))

(ert-deftest flycheck-substitute-shell-argument-cell-option ()
  "Test substitution of `option' shell argument cell."
  (let ((flycheck-test-option-var "Hello World"))
    (should (equal (flycheck-substitute-shell-argument-cell
                    '(option "--foo" flycheck-test-option-var))
                   "--foo Hello\\ World"))
     (should (equal (flycheck-substitute-shell-argument-cell
                    '(option "--foo=" flycheck-test-option-var))
                    "--foo\\=Hello\\ World")))
  (let ((flycheck-test-option-var 100))
    (should-error (flycheck-substitute-shell-argument-cell
                   '(option "--foo" flycheck-test-option-var)))
    (should (equal (flycheck-substitute-shell-argument-cell
                    '(option "--foo" flycheck-test-option-var number-to-string))
                   "--foo 100"))
    (should (equal (flycheck-substitute-shell-argument-cell
                    '(option "--foo=" flycheck-test-option-var number-to-string))
                   "--foo\\=100")))
  (let (flycheck-test-option-var)
    (should (equal (flycheck-substitute-shell-argument-cell
                    '(option "--foo" flycheck-test-option-var))
                   ""))
    (should (equal (flycheck-substitute-shell-argument-cell
                    '(option "--foo" flycheck-test-option-var
                             #'number-to-string))
                   ""))
    (should (equal (flycheck-substitute-shell-argument-cell
                    '(option "--foo=" flycheck-test-option-var
                             #'number-to-string))
                   ""))))

(ert-deftest flycheck-substitute-shell-argument-cell-eval ()
  "Test substitution of `eval' argument cell."
  (let ((flycheck-test-option-var '("Hello" " World")))
    (should (equal (flycheck-substitute-shell-argument-cell '(eval flycheck-test-option-var))
                   "Hello \\ World")))
  (should (equal (flycheck-substitute-shell-argument-cell
                  '(eval (concat "Hello " "\"World\"")))
                 "Hello\\ \\\"World\\\""))
  (should (equal (flycheck-substitute-shell-argument-cell
                  '(eval (when (string= "foo" "bar") "yes")))
                 ""))
  (should-error (flycheck-substitute-shell-argument-cell '(eval 100)))
  (should-error (flycheck-substitute-shell-argument-cell '(eval '("foo" 100)))))

(ert-deftest flycheck-substitute-argument-cell-unknown ()
  "Test substitution of an unknown argument cell."
  (should-error (flycheck-substitute-shell-argument-cell '(foo "bar"))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-checker-api.el ends here
