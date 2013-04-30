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
  (dolist (checker flycheck-checkers)
    (should (listp (flycheck-checker-modes checker)))
    (should (-all? #'symbolp (flycheck-checker-modes checker)))))

(ert-deftest flycheck-checker-executable ()
  (dolist (checker flycheck-checkers)
    (should (equal (flycheck-checker-executable checker)
                   (car (flycheck-checker-command checker))))
    (should (stringp (flycheck-checker-executable checker)))))

(ert-deftest flycheck-check-executable ()
  (dolist (checker flycheck-checkers)
    (if (executable-find (flycheck-checker-executable checker))
        (should (flycheck-check-executable checker))
      (should-not (flycheck-check-executable checker)))))

(defvar flycheck-test-config-var)
(defvar flycheck-test-option-var)

(ert-deftest flycheck-substitute-argument-source ()
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (unwind-protect
        (progn
          (should (equal (flycheck-substitute-argument 'source-original)
                         (buffer-file-name)))
          (let ((filename (flycheck-substitute-argument 'source-inplace)))
            (should (equal filename (flycheck-testsuite-resource-filename
                                     "flycheck-substitute-dummy")))
            (should (file-exists-p filename)))
          (let ((filename (flycheck-substitute-argument 'source)))
            (should (s-starts-with? temporary-file-directory filename))
            (should (file-exists-p filename)))
          (should-error (flycheck-substitute-argument 'foobar))))
    (flycheck-safe-delete-files flycheck-temp-files)))

(ert-deftest flycheck-substitute-argument-temporary-directory ()
  (with-temp-buffer
    (unwind-protect
        (progn
          (let ((dirname (flycheck-substitute-argument 'temporary-directory)))
            (should (file-directory-p dirname))
            (should (s-starts-with? temporary-file-directory dirname))))
      (flycheck-safe-delete-directories flycheck-temp-directories))))

(ert-deftest flycheck-substitute-argument-config-file ()
  "Test substitution of `config-file' argument cell."
  ;; We need a real buffer for config-file search
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (let ((makefile-path (expand-file-name "../Makefile" flycheck-testsuite-dir))
          flycheck-test-config-var)
      ;; A non-existing file
      (setq flycheck-test-config-var
            "no-such-file-should-ever-exist-really")
      (should-not (flycheck-substitute-argument
                   '(config-file "--foo" flycheck-test-config-var)))
      ;; An existing file, first by name and then by relative and absolute path
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-argument
                      '(config-file "--foo" flycheck-test-config-var))
                     (list "--foo" makefile-path)))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-argument
                      '(config-file "--foo" flycheck-test-config-var))
                     (list "--foo" makefile-path)))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-argument
                      '(config-file "--foo" flycheck-test-config-var))
                     (list "--foo" makefile-path)))
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-argument
                      '(config-file "--foo=" flycheck-test-config-var))
                     (list (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-argument
                      '(config-file "--foo=" flycheck-test-config-var))
                     (list (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-argument
                      '(config-file "--foo=" flycheck-test-config-var))
                     (list (concat "--foo=" makefile-path))))
      ;; Find a file from the home directory
      (let* ((filename (-first #'file-regular-p
                               (directory-files (getenv "HOME") :full-names))))
        (unless filename
          ;; Let the test fail if there is no file in $HOME
          (error "No file in $HOME found."))
        (setq flycheck-test-config-var (file-name-nondirectory filename))
        (should (equal (flycheck-substitute-argument
                        '(config-file "--bar" flycheck-test-config-var))
                       (list "--bar" filename)))))))

(ert-deftest flycheck-substitute-argument-option ()
  "Test substitution of `option' argument cell."
  (let ((flycheck-test-option-var "bar"))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var))
                   '("--foo" "bar")))
     (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var))
                   '("--foo=bar"))))
  (let ((flycheck-test-option-var 100))
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var)))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var number-to-string))
                   '("--foo" "100")))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var number-to-string))
                   '("--foo=100"))))
  (let (flycheck-test-option-var)
    (should-not (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var)))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var number-to-string))
                  :type 'wrong-type-argument)
    (should-error (flycheck-substitute-argument
                   '(option "--foo=" flycheck-test-option-var number-to-string))
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-argument-eval ()
  (let ((flycheck-test-option-var '("Hello " "World")))
    (should (equal (flycheck-substitute-argument '(eval flycheck-test-option-var))
                   '("Hello " "World"))))
  (should (equal (flycheck-substitute-argument '(eval (concat "Hello" "World")))
                 "HelloWorld"))
  (should-not (flycheck-substitute-argument
               '(eval (when (string= "foo" "bar") "yes"))))
  (should-error (flycheck-substitute-argument '(eval 100)))
  (should-error (flycheck-substitute-argument '(eval '("foo" 100)))))

(ert-deftest flycheck-substitute-shell-argument-source ()
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (--each '(source source-inplace source-original)
      (should (equal (flycheck-substitute-shell-argument it)
                     (flycheck-testsuite-resource-filename "substitute-dummy"))))))

(ert-deftest flycheck-substitute-shell-argument-temporary-directory ()
  (with-temp-buffer
    (unwind-protect
        (progn
          (let ((dirname (flycheck-substitute-shell-argument 'temporary-directory)))
            (should (file-directory-p dirname))
            (should (s-starts-with? temporary-file-directory dirname))))
      (flycheck-safe-delete-directories flycheck-temp-directories))))

(ert-deftest flycheck-substitute-shell-argument-config-file ()
  ;; We need a real buffer for config-file search
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (let ((makefile-path (expand-file-name "../Makefile" flycheck-testsuite-dir))
          flycheck-test-config-var)
      ;; A non-existing file
      (setq flycheck-test-config-var
            "no-such-file-should-ever-exist-really")
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo" flycheck-test-config-var))
                     ""))
      ;; An existing file, first by name and then by relative and absolute path
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo" flycheck-test-config-var))
                     (concat "--foo " (shell-quote-argument makefile-path))))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo" flycheck-test-config-var))
                     (concat "--foo " (shell-quote-argument makefile-path))))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo" flycheck-test-config-var))
                     (concat "--foo " (shell-quote-argument makefile-path))))
      ;; The same with an option ending with a =
      (setq flycheck-test-config-var "Makefile")
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo=" flycheck-test-config-var))
                     (shell-quote-argument (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var "../../Makefile")
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo=" flycheck-test-config-var))
                     (shell-quote-argument (concat "--foo=" makefile-path))))
      (setq flycheck-test-config-var makefile-path)
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo=" flycheck-test-config-var))
                     (shell-quote-argument (concat "--foo=" makefile-path))))
      ;; Find a file from the home directory
      (let* ((filename (-first #'file-regular-p
                               (directory-files (getenv "HOME") :full-names))))
        (unless filename
          ;; Let the test fail if there is no file in $HOME
          (error "No file in $HOME found."))
        (setq flycheck-test-config-var (file-name-nondirectory filename))
        (should (equal (flycheck-substitute-shell-argument
                        '(config-file "--bar" flycheck-test-config-var))
                       (concat "--bar " (shell-quote-argument filename))))))))

(ert-deftest flycheck-substitute-shell-argument-option ()
  (let ((flycheck-test-option-var "Hello World"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo" flycheck-test-option-var))
                   "--foo Hello\\ World"))
     (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo=" flycheck-test-option-var))
                    "--foo\\=Hello\\ World")))
  (let ((flycheck-test-option-var 100))
    (should-error (flycheck-substitute-shell-argument
                   '(option "--foo" flycheck-test-option-var)))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo" flycheck-test-option-var number-to-string))
                   "--foo 100"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo=" flycheck-test-option-var number-to-string))
                   "--foo\\=100")))
  (let (flycheck-test-option-var)
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo" flycheck-test-option-var))
                   ""))
    (should-error (flycheck-substitute-shell-argument
                   '(option "--foo" flycheck-test-option-var number-to-string))
                  :type 'wrong-type-argument)
    (should-error (flycheck-substitute-shell-argument
                   '(option "--foo=" flycheck-test-option-var number-to-string))
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-shell-argument-eval ()
  "Test substitution of `eval' argument cell."
  (let ((flycheck-test-option-var '("Hello" " World")))
    (should (equal (flycheck-substitute-shell-argument '(eval flycheck-test-option-var))
                   "Hello \\ World")))
  (should (equal (flycheck-substitute-shell-argument
                  '(eval (concat "Hello " "\"World\"")))
                 "Hello\\ \\\"World\\\""))
  (should (equal (flycheck-substitute-shell-argument
                  '(eval (when (string= "foo" "bar") "yes")))
                 ""))
  (should-error (flycheck-substitute-shell-argument '(eval 100)))
  (should-error (flycheck-substitute-shell-argument '(eval '("foo" 100)))))

(ert-deftest flycheck-substitute-argument-unknown ()
  (should-error (flycheck-substitute-argument '(foo "bar")))
  (should-error (flycheck-substitute-argument 100)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-checker-api.el ends here
