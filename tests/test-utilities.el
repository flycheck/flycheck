;;; test-utilities.el --- Tests for utility functions

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

(ert-deftest flycheck-temp-file-system-no-filename ()
  "Test `flycheck-temp-file-system' without a filename."
  (let ((filename (flycheck-temp-file-system nil "flycheck-test")))
    (unwind-protect
        (progn
          (should-not (file-name-extension filename))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

(ert-deftest flycheck-temp-file-system-filename-no-extension ()
  "Test `flycheck-temp-file-system' with an extension."
  (let ((filename (flycheck-temp-file-system "spam/with/eggs" "flycheck-test")))
    (unwind-protect
        (progn
          (should-not (file-name-extension filename))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

(ert-deftest flycheck-temp-file-system-filename-extension ()
  "Test `flycheck-temp-file-system' works with a complete
  filename."
  (let ((filename (flycheck-temp-file-system "spam/with/eggs.el"
                                             "flycheck-test")))
    (unwind-protect
        (progn
          (should (string= (file-name-extension filename) "el"))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

(ert-deftest flycheck-temp-file-inplace-basename ()
  "Test `flycheck-temp-file-inplace' with a base name."
  (let ((filename (flycheck-temp-file-inplace "eggs.el" "flycheck-test")))
    (should (string= filename (expand-file-name "flycheck-test-eggs.el" nil)))
    (should-not (file-exists-p filename))))

(ert-deftest flycheck-temp-file-inplace-path ()
  "Test `flycheck-temp-file-inplace' with complete path."
  (let ((filename (flycheck-temp-file-inplace "spam/with/eggs.el"
                                              "flycheck-test")))
    (should (string= filename (expand-file-name "flycheck-test-eggs.el"
                                                "spam/with")))
    (should-not (file-exists-p filename))))

(ert-deftest flycheck-temp-file-inplace-no-filename ()
  "Test `flycheck-temp-file-inplace' without a path."
  (let ((filename (flycheck-temp-file-inplace nil "flycheck-test")))
    (unwind-protect
        (progn
          (should-not (file-name-extension filename))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

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
      (ignore-errors
        (delete-file filename)))))

(ert-deftest flycheck-temp-buffer-copy-system-no-filename ()
  "Test `flycheck-temp-buffer-copy' with system tempfile and no
buffer filename."
  (with-temp-buffer
    (insert "Hello world")
    ;; No file name
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-system)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should-not (file-name-extension tempfile))
            (should (s-starts-with? "flycheck"
                                    (file-name-nondirectory tempfile)))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))

(ert-deftest flycheck-temp-buffer-copy-system-filename ()
  "Test `flycheck-temp-buffer-copy' with system tempfile and
buffer file name."
  (with-temp-buffer
    (setq buffer-file-name "testfile.txt")
    (insert "Hello world")
    ;; No file name
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-system)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should (string= (file-name-extension tempfile) "txt"))
            (should (s-starts-with? "flycheck"
                                    (file-name-nondirectory tempfile)))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))

(ert-deftest flycheck-temp-buffer-copy-inplace-no-filename ()
  "Test `flycheck-temp-buffer-copy' with inplace copy and no file
  name."
  (with-temp-buffer
    (insert "Hello world")
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-inplace)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should-not (file-name-extension tempfile))
            (should (s-starts-with? "flycheck"
                                    (file-name-nondirectory tempfile)))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))

(ert-deftest flycheck-temp-buffer-copy-inplace-filename ()
  "Test `flycheck-temp-buffer-copy' with inplace copy and file
  name."
  (with-temp-buffer
    (setq buffer-file-name "testfile.txt")
    (insert "Hello world")
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-inplace)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should (string= tempfile
                             (expand-file-name "flycheck-testfile.txt")))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))

(ert-deftest flycheck-root-directory-p-unix ()
  "Test whether `flycheck-root-directory-p' behaves correctly."
  :expected-result (if (not (flycheck-windows-p)) :passed :failed)
  (should (flycheck-root-directory-p "/"))
  (should (flycheck-root-directory-p "//"))
  (should-not (flycheck-root-directory-p "/home/foo/"))
  (should-not (flycheck-root-directory-p "/home/foo")))

(ert-deftest flycheck-root-directory-p-windows ()
  "Test whether `flycheck-root-directory-p' behaves correctly on Windows."
  :expected-result (if (flycheck-windows-p) :passed :failed)
  (should (flycheck-root-directory-p "C:\\"))
  (should (flycheck-root-directory-p "D:\\"))
  (should-not (flycheck-root-directory-p "C:\\Windows"))
  (should-not (flycheck-root-directory-p "C:\\Windows\\")))

;;; test-utilities.el ends here
