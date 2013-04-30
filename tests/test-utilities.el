;;; test-utilities.el --- Tests for utility functions -*- lexical-binding: t; -*-

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
(require 's)
(require 'flycheck)

(defmacro flycheck-testsuite-delete-temps (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (flycheck-safe-delete-files flycheck-temp-files)
     (flycheck-safe-delete-directories flycheck-temp-directories)
     (setq flycheck-temp-files nil)
     (setq flycheck-temp-directories nil)))

(defmacro flycheck-testsuite-trap-temp-dir (dirname &rest body)
  (declare (indent 1))
  `(progn
     (flycheck-testsuite-delete-temps
       (should (equal flycheck-temp-directories (list ,dirname)))
       ,@body)
     (should-not (file-exists-p dirname))))

(defmacro flycheck-testsuite-trap-temp-file (filename &rest body)
  (declare (indent 1))
  `(progn
     (flycheck-testsuite-delete-temps
       (should (equal flycheck-temp-files (list ,filename)))
       ,@body)
     (should-not (file-exists-p filename))))

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

(ert-deftest flycheck-temp-dir-system ()
  (let ((dirname (flycheck-temp-dir-system "flycheck-test")))
    (flycheck-testsuite-trap-temp-dir dirname
      (should (s-starts-with? temporary-file-directory dirname))
      (should (s-starts-with? "flycheck-test"
                              (file-name-nondirectory dirname)))
      (should (file-directory-p dirname)))))

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

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-utilities.el ends here
