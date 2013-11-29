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

(ert-deftest flycheck-temp-file-system/without-file-name ()
  (let ((filename (flycheck-temp-file-system nil "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (s-starts-with? temporary-file-directory filename))
      (should (s-starts-with? "flycheck-test" (f-filename filename)))
      (should (f-exists? filename)))))

(ert-deftest flycheck-temp-file-system/with-complete-path ()
  (let* ((filename (flycheck-temp-file-system "spam/with/eggs.el"
                                              "flycheck-test"))
         (dirname (directory-file-name (file-name-directory filename))))
    (flycheck-testsuite-trap-temp-dir dirname
      (should (string= "eggs.el" (file-name-nondirectory filename)))
      (should (s-starts-with? temporary-file-directory filename))
      (should (s-starts-with? "flycheck-test" (file-name-nondirectory dirname)))
      (should (f-directory? dirname)))))

(ert-deftest flycheck-temp-file-inplace/with-just-basename ()
  (let ((filename (flycheck-temp-file-inplace "eggs.el" "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (string= filename (f-expand "flycheck-test-eggs.el")))
      (should-not (f-exists? filename)))))

(ert-deftest flycheck-temp-file-inplace/with-complete-path ()
  (let ((filename (flycheck-temp-file-inplace "spam/with/eggs.el"
                                              "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should (string= filename
                       (f-expand (f-join "spam/with" "flycheck-test-eggs.el"))))
      (should-not (f-exists? filename)))))

(ert-deftest flycheck-temp-file-inplace/without-file-name ()
  (let ((filename (flycheck-temp-file-inplace nil "flycheck-test")))
    (flycheck-testsuite-trap-temp-file filename
      (should-not (file-name-extension filename))
      (should (s-starts-with? "flycheck-test"
                              (file-name-nondirectory filename)))
      (should (f-exists? filename)))))

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
  (flycheck-testsuite-with-resource-buffer "global-mode-dummy.el"
    (should-not (flycheck-encrypted-buffer-p))))

(ert-deftest flycheck-encrypted-buffer-p/encrypted-file-buffer ()
  (let* ((filename (flycheck-testsuite-resource-filename "encrypted-file.el.gpg"))
         ;; Tell EPA about our passphrase
         (epa-file-cache-passphrase-for-symmetric-encryption t)
         (epa-file-passphrase-alist (list (cons filename "foo"))))
    (flycheck-testsuite-with-resource-buffer filename
      (should (flycheck-encrypted-buffer-p)))))

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

;;; utility-test.el ends here
