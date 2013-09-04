;;; utility-test.el --- Tests for utility functions   -*- lexical-binding: t; -*-

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

;; Tests for utility functions.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-temp-dir-system ()
  (let ((dirname (flycheck-temp-dir-system "flycheck-test")))
    (flycheck-testsuite-trap-temp-dir dirname
      (should (s-starts-with? temporary-file-directory dirname))
      (should (s-starts-with? "flycheck-test" (f-filename dirname)))
      (should (f-directory? dirname)))))

(ert-deftest flycheck-temp-file-system-no-filename ()
  "Test `flycheck-temp-file-system' without a filename."
  (let ((filename (flycheck-temp-file-system nil "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (s-starts-with? temporary-file-directory filename))
      (should (s-starts-with? "flycheck-test" (f-filename filename)))
      (should (f-exists? filename)))))

(ert-deftest flycheck-temp-file-system-filename ()
  "Test `flycheck-temp-file-system' with an extension."
  (let* ((filename (flycheck-temp-file-system "spam/with/eggs.el"
                                              "flycheck-test"))
         (dirname (directory-file-name (file-name-directory filename))))
    (flycheck-testsuite-trap-temp-dir dirname
      (should (string= "eggs.el" (file-name-nondirectory filename)))
      (should (s-starts-with? temporary-file-directory filename))
      (should (s-starts-with? "flycheck-test" (file-name-nondirectory dirname)))
      (should (f-directory? dirname)))))

(ert-deftest flycheck-temp-file-inplace-basename ()
  "Test `flycheck-temp-file-inplace' with a base name."
  (let ((filename (flycheck-temp-file-inplace "eggs.el" "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (string= filename (f-expand "flycheck-test-eggs.el")))
      (should-not (f-exists? filename)))))

(ert-deftest flycheck-temp-file-inplace-path ()
  "Test `flycheck-temp-file-inplace' with complete path."
  (let ((filename (flycheck-temp-file-inplace "spam/with/eggs.el"
                                              "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (string= filename
                       (f-expand (f-join "spam/with" "flycheck-test-eggs.el"))))
      (should-not (f-exists? filename)))))

(ert-deftest flycheck-temp-file-inplace-no-filename ()
  "Test `flycheck-temp-file-inplace' without a path."
  (let ((filename (flycheck-temp-file-inplace nil "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should-not (file-name-extension filename))
      (should (s-starts-with? "flycheck-test"
                              (file-name-nondirectory filename)))
      (should (f-exists? filename)))))

(ert-deftest flycheck-save-buffer-to-file ()
  "Test `flycheck-save-buffer-to-file'."
  (let ((filename (f-expand "tests-temp")))
    (unwind-protect
        (with-temp-buffer
          (should-not (f-exists? filename))
          (insert "Hello world")
          (flycheck-save-buffer-to-file filename)
          (should (f-exists? filename))
          (with-temp-buffer
            (insert-file-contents-literally filename)
            (should (string= (buffer-string) "Hello world"))))
      (ignore-errors (f-delete filename)))))

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

(ert-deftest flycheck-ephemeral-buffer-p ()
  (with-temp-buffer
    (should (flycheck-ephemeral-buffer-p)))
  (with-temp-buffer
    (rename-buffer " foo")
    (should (flycheck-ephemeral-buffer-p)))
  (with-temp-buffer
    (rename-buffer "foo")
    (should-not (flycheck-ephemeral-buffer-p))))

(ert-deftest flycheck-safe-delete-recursive ()
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

;;; utility-test.el ends here
