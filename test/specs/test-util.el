;;; test-util.el --- Flycheck Specs: Utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Specs for Flycheck's utility functions

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Utilities"

  (describe "flycheck-buffer-empty-p"

    (it "considers an empty buffer as empty"
      (with-temp-buffer
        (expect (flycheck-buffer-empty-p) :to-be-truthy)))

    (it "does not consider a buffer with content as empty"
      (with-temp-buffer
        (insert "foo bar")
        (expect (flycheck-buffer-empty-p) :not :to-be-truthy)))

    (it "detects emptiness of narrowed buffers"
      (with-temp-buffer
        (insert "foo\nbar")
        (goto-char (point-min))
        (narrow-to-region (point-min) (point-min))
        (expect (buffer-string) :to-equal "")
        (expect (flycheck-buffer-empty-p) :not :to-be-truthy))))

  (describe "flycheck-buffer-saved-p"

    (it "considers an unmodified buffer without backing file unsaved"
      (with-temp-buffer
        (expect (flycheck-buffer-saved-p) :not :to-be-truthy)))

    (it "considers a modified buffer without backing file unsaved"
      (with-temp-buffer
        (set-buffer-modified-p t)
        (expect (flycheck-buffer-saved-p) :not :to-be-truthy)))

    (it "considers an unmodified buffer with backing file saved"
      (spy-on 'file-exists-p :and-return-value t)
      (spy-on 'buffer-file-name :and-return-value "test-buffer-name")
      (with-temp-buffer
        (expect (flycheck-buffer-saved-p) :to-be-truthy))
      (expect (spy-calls-count 'file-exists-p) :to-equal 1)
      (expect (spy-calls-count 'buffer-file-name) :to-equal 1))

    (it "considers a modified buffer with backing file unsaved"
      (spy-on 'file-exists-p :and-return-value t)
      (spy-on 'buffer-file-name :and-return-value "test-buffer-name")
      (with-temp-buffer
        (set-buffer-modified-p t)
        (expect (flycheck-buffer-saved-p) :not :to-be-truthy))
      (expect (spy-calls-count 'file-exists-p) :to-equal 1)
      (expect (spy-calls-count 'buffer-file-name) :to-equal 1)))


  (describe "flycheck-default-executable-find"

    (describe "non-existing programs"
      (it "returns nil when given a non-existing program name"
        (let ((result (flycheck-default-executable-find
                       "flycheck-nonexisting")))
          (expect result :to-be nil)))

      (it "returns nil when given a non-existing relative program path"
        (let ((result (flycheck-default-executable-find
                       "dir/flycheck-nonexisting")))
          (expect result :to-be nil)))

      (it "returns nil when given a non-existing absolute program path"
        (let ((result (flycheck-default-executable-find
                       "/usr/bin/flycheck-nonexisting")))
          (expect result :to-be nil))))

    (describe "existing programs with implied suffix"
      (let (temp-dir program-path)
        (before-each
          (setq temp-dir (make-temp-file "flycheck-exec-find-root" 'dir-flag)
                program-path (expand-file-name
                              (if (memq system-type '(cygwin windows-nt ms-dos))
                                  "dir/flycheck-testprog.bat"
                                "dir/flycheck-testprog.program")
                              temp-dir))
          (make-directory (expand-file-name "dir" temp-dir) t)
          (write-region "" nil program-path)
          (set-file-modes program-path
                          (logior 73 (file-modes program-path))))
        (after-each
          (ignore-errors (delete-directory temp-dir 'recursive)))

        (it "resolves the path when given an existing program name"
          (let* ((default-directory temp-dir)
                 (exec-path (list (expand-file-name "dir" temp-dir)))
                 (exec-suffixes '(".program" ".bat"))
                 (result (flycheck-default-executable-find
                          "flycheck-testprog")))
            (expect result :to-equal program-path)))

        (it "resolves the path when given an existing relative program path"
          (let* ((default-directory temp-dir)
                 (exec-suffixes '(".program" ".bat"))
                 (result (flycheck-default-executable-find
                          "dir/flycheck-testprog")))
            (expect result :to-equal program-path)))

        (it "resolves the path when given an existing absolute program path"
          (let* ((default-directory temp-dir)
                 (exec-suffixes '(".program" ".bat"))
                 (result (flycheck-default-executable-find
                          (expand-file-name "dir/flycheck-testprog" temp-dir))))
            (expect result :to-equal program-path))))))

  (describe "flycheck-string-to-number-safe"

    (it "returns nil for nil"
      (expect (flycheck-string-to-number-safe nil) :not :to-be-truthy))

    (it "returns nil for not a string"
      (expect (flycheck-string-to-number-safe [1 2 3]) :not :to-be-truthy))

    (it "returns nil for already a number"
      (expect (flycheck-string-to-number-safe 3) :not :to-be-truthy))

    (it "returns nil for a non-numeric string"
      (expect (flycheck-string-to-number-safe "123helloworld") :not :to-be-truthy))

    (it "returns the number for a numeric string"
      (expect (flycheck-string-to-number-safe "123") :to-equal 123))

    (it "returns nil for a numeric string with leading whitespace"
      (expect (flycheck-string-to-number-safe " 123") :not :to-be-truthy))

    (it "returns nil for a numeric string with trailing whitespace"
      (expect (flycheck-string-to-number-safe "123 ") :not :to-be-truthy)))

  (describe "flycheck-string-list-p"

    (it "returns nil for not a list"
      (expect (flycheck-string-list-p ["foo" "bar"]) :not :to-be-truthy))

    (it "returns nil for a plain string"
      (expect (flycheck-string-list-p "foo") :not :to-be-truthy))

    (it "returns nil for a plain integer"
      (expect (flycheck-string-list-p 1) :not :to-be-truthy))

    (it "returns nil for a plain symbol"
      (expect (flycheck-string-list-p 'foo) :not :to-be-truthy))

    (it "returns nil for a list with mixed types"
      (expect (flycheck-string-list-p '("foo" 1 test)) :not :to-be-truthy))

    (it "returns nil for a list of symbols"
      (expect (flycheck-string-list-p '(foo bar)) :not :to-be-truthy))

    (it "returns true for a list of strings"
      (expect (flycheck-string-list-p '("foo" "bar")) :to-be-truthy))

    (it "returns true for an empty list"
      (expect (flycheck-string-list-p '()) :to-be-truthy)))

  (describe "flycheck-symbol-list-p"

    (it "returns nil for not a list"
      (expect (flycheck-symbol-list-p ["foo" "bar"]) :not :to-be-truthy))

    (it "returns nil for a plain string"
      (expect (flycheck-symbol-list-p "foo") :not :to-be-truthy))

    (it "returns nil for a plain integer"
      (expect (flycheck-symbol-list-p 1) :not :to-be-truthy))

    (it "returns nil for a plain symbol"
      (expect (flycheck-symbol-list-p 'foo) :not :to-be-truthy))

    (it "returns nil for a list with mixed types"
      (expect (flycheck-symbol-list-p '("foo" 1 test)) :not :to-be-truthy))

    (it "returns nil for a list of strings"
      (expect (flycheck-symbol-list-p '("foo" "bar")) :not :to-be-truthy))

    (it "returns true for a list of symbols"
      (expect (flycheck-symbol-list-p '(foo bar)) :to-be-truthy))

    (it "returns true for an empty list"
      (expect (flycheck-symbol-list-p '()) :to-be-truthy)))

  (describe "flycheck-same-files-p"

    (it "returns true for same files"
      (let ((default-directory flycheck-test-source-directory))
        (expect (flycheck-same-files-p "flycheck.el" "flycheck.el")
                :to-be-truthy)))

    (it "returns nil for different files"
      (let ((default-directory flycheck-test-source-directory))
        (expect (flycheck-same-files-p "flycheck.el" "Makefile")
                :not :to-be-truthy)))

    (it "returns nil for file in non-existing directory"
      (let ((default-directory flycheck-test-source-directory))
        (expect (flycheck-same-files-p "flycheck.el" "foobar/flycheck.el")
                :not :to-be-truthy)))

    (it "returns true for non-existing files"
      (let ((default-directory flycheck-test-source-directory))
        (expect (flycheck-same-files-p "foobar/foobar" "foobar/foobar")
                :to-be-truthy)))

    (it "returns true across symlinks"
      (assume (fboundp #'make-symbolic-link))
      (let ((directory (make-temp-file "flycheck-test-same-files-p-" 'directory)))
        (unwind-protect
            (let ((link (expand-file-name "foobar.el" directory))
                  (flycheck (expand-file-name "flycheck.el"
                                              flycheck-test-source-directory)))
              (make-symbolic-link flycheck link)
              (expect (flycheck-same-files-p flycheck link) :to-be-truthy))
          (delete-directory directory 'recursive)))))

  (describe "flycheck-temp-dir-system"

    (it "creates a temporary directory"
      (let ((dirname (flycheck-temp-dir-system)))
        (unwind-protect
            (expect (file-directory-p dirname) :to-be-truthy)
          (flycheck-safe-delete-temporaries))
        (expect (file-exists-p dirname) :not :to-be-truthy)
        (expect (string-prefix-p (file-name-as-directory
                                  (file-truename temporary-file-directory))
                                 (file-truename dirname))
                :to-be-truthy)
        (expect (string-prefix-p flycheck-temp-prefix
                                 (file-name-nondirectory
                                  (directory-file-name dirname)))
                :to-be-truthy))))

  (describe "flycheck-temp-file-system"

    (it "creates a temp file without file name"
      (let ((filename (flycheck-temp-file-system nil)))
        (unwind-protect
            (expect (file-exists-p filename) :to-be-truthy)
          (flycheck-safe-delete-temporaries))
        (expect (file-exists-p filename) :not :to-be-truthy)
        (expect (string-prefix-p (file-name-as-directory
                                  (file-truename temporary-file-directory))
                                 (file-truename filename))
                :to-be-truthy)
        (expect (string-prefix-p flycheck-temp-prefix
                                 (file-name-nondirectory filename))
                :to-be-truthy)))

    (it "creates a temp file with complete path"
      (let* ((filename (flycheck-temp-file-system "spam/with/eggs.el"))
             (dirname (directory-file-name (file-name-directory filename))))
        (unwind-protect
            (progn
              ;; The file is not implicitly created, but the temporary
              ;; directory is.
              (expect (file-exists-p filename) :not :to-be-truthy)
              (expect (file-directory-p dirname) :to-be-truthy))
          (flycheck-safe-delete-temporaries))
        (expect (file-exists-p filename) :not :to-be-truthy)
        (expect (file-directory-p dirname) :not :to-be-truthy)
        ;; The file name should be preserved.  The temporary file should
        ;; reside in a subdirectory of the temporary directory
        (expect (file-name-nondirectory filename) :to-equal "eggs.el")
        (expect (string-prefix-p flycheck-temp-prefix
                                 (file-name-nondirectory
                                  (directory-file-name dirname)))
                :to-be-truthy)
        (expect (string-prefix-p flycheck-temp-prefix
                                 (file-name-nondirectory
                                  (directory-file-name dirname)))
                :to-be-truthy))))

  (describe "flycheck-temp-file-inplace"

    (it "creates a temp file with just basename"
      (let* ((default-directory flycheck-test-directory)
             (filename (flycheck-temp-file-inplace "eggs.el")))
        (unwind-protect
            ;; In place files should not be created early
            (expect (file-exists-p filename) :not :to-be-truthy)
          (flycheck-safe-delete-temporaries))
        (expect filename :to-equal
                (expand-file-name
                 (concat flycheck-temp-prefix "_eggs.el")))))

    (it "creates a temp file with complete path"
      (let* ((default-directory flycheck-test-directory)
             (filename (flycheck-temp-file-inplace "spam/with/eggs.el")))
        (unwind-protect
            (expect (file-exists-p filename) :not :to-be-truthy)
          (flycheck-safe-delete-temporaries))
        (expect filename :to-equal
                (expand-file-name (concat "spam/with/"
                                          flycheck-temp-prefix
                                          "_eggs.el")))))

    (it "creates a temp file without file name"
      (let ((filename (flycheck-temp-file-inplace nil)))
        (unwind-protect
            (expect (file-exists-p filename) :to-be-truthy)
          (flycheck-safe-delete-temporaries))
        (expect (file-name-extension filename) :not :to-be-truthy)
        (expect (string-prefix-p flycheck-temp-prefix
                                 (file-name-nondirectory filename))
                :to-be-truthy))))

  (describe "flycheck-save-buffer-to-file"

    (it "saves buffer contents to a file"
      (let ((filename (expand-file-name "tests-temp")))
        (unwind-protect
            (progn
              (flycheck-buttercup-with-temp-buffer
                (expect (file-exists-p filename) :not :to-be-truthy)
                (insert "Hello world")
                (flycheck-save-buffer-to-file filename))
              (expect (file-exists-p filename) :to-be-truthy)
              (expect (with-temp-buffer
                        (insert-file-contents filename)
                        (buffer-string))
                      :to-equal "Hello world"))
          (ignore-errors (delete-file filename))))))

  (describe "flycheck-prepend-with-option"

    (it "returns nil for empty list"
      (expect (flycheck-prepend-with-option "-f" nil) :not :to-be-truthy))

    (it "prepends with default function"
      (expect (flycheck-prepend-with-option "-L" '("foo" "bar"))
              :to-equal '("-L" "foo" "-L" "bar")))

    (it "prepends by string concatenation"
      (expect (flycheck-prepend-with-option "-L" '("foo" "bar") #'concat)
              :to-equal '("-Lfoo" "-Lbar"))))

  (describe "flycheck-find-in-buffer"

    (it "returns first match on success"
      (with-temp-buffer
        (insert "foo\nbar")
        (expect (flycheck-find-in-buffer (rx (group "bar")))
                :to-equal "bar")))

    (it "returns nil on failure"
      (with-temp-buffer
        (insert "spam\nwith eggs")
        (expect (flycheck-find-in-buffer (rx (group "bar")))
                :not :to-be-truthy)))

    (it "saves excursion"
      (with-temp-buffer
        (insert "spam\nwith eggs")
        (goto-char (point-min))
        (forward-line)
        (expect (flycheck-find-in-buffer (rx (group "spam")))
                :to-be-truthy)
        (expect (point) :to-equal 6))))

  (describe "flycheck-ephemeral-buffer-p"

    (it "returns true for temporary buffer"
      (flycheck-buttercup-with-temp-buffer
        (expect (flycheck-ephemeral-buffer-p) :to-be-truthy)))

    (it "returns true for buffer with leading whitespace"
      (flycheck-buttercup-with-temp-buffer
        (rename-buffer " foo")
        (expect (flycheck-ephemeral-buffer-p) :to-be-truthy)))

    (it "returns nil for buffer without leading whitespace"
      (flycheck-buttercup-with-temp-buffer
        (rename-buffer "foo")
        (expect (flycheck-ephemeral-buffer-p) :not :to-be-truthy))))

  (describe "flycheck-autoloads-file-p"

    (it "returns nil for ephemeral buffer"
      (flycheck-buttercup-with-temp-buffer
        (expect (flycheck-autoloads-file-p) :not :to-be-truthy)))

    (it "returns true for autoloads without backing file"
      (flycheck-buttercup-with-temp-buffer
        (rename-buffer "foo-autoloads.el")
        (expect (flycheck-autoloads-file-p) :to-be-truthy)))

    (it "returns true for autoloads with backing file"
      (flycheck-buttercup-with-file-buffer (locate-library "shut-up-autoloads")
        (expect (flycheck-autoloads-file-p) :to-be-truthy)))

    (it "returns nil for a plain file"
      (flycheck-buttercup-with-file-buffer
          (expand-file-name "Eask" flycheck-test-source-directory)
        (expect (flycheck-autoloads-file-p) :not :to-be-truthy))))

  (describe "flycheck-in-user-emacs-directory-p"

    (it "returns nil for no child of user-emacs-directory"
      (let ((user-emacs-directory "/flycheck-nonexisting"))
        (expect (flycheck-in-user-emacs-directory-p
                 (flycheck-buttercup-resource-filename
                  "language/emacs-lisp/warnings.el"))
                :not :to-be-truthy)))

    (it "returns true for direct child of user-emacs-directory"
      (let ((user-emacs-directory flycheck-test-directory))
        (expect (flycheck-in-user-emacs-directory-p
                 (expand-file-name "init.el" flycheck-test-directory))
                :to-be-truthy)))

    (it "returns true for indirect child of user-emacs-directory"
      (let ((user-emacs-directory flycheck-test-directory))
        (expect (flycheck-in-user-emacs-directory-p
                 (flycheck-buttercup-resource-filename
                  "language/emacs-lisp/warnings.el"))
                :to-be-truthy))))

  (describe "flycheck-safe-delete"

    (it "recursively removes directory"
      (let ((dirname (flycheck-temp-dir-system)))
        (unwind-protect
            (let ((filename (expand-file-name "foo" dirname)))
              (process-lines "touch" filename)
              (expect (string-prefix-p dirname filename) :to-be-truthy)
              (expect (file-exists-p filename) :to-be-truthy)
              (flycheck-safe-delete dirname)
              (expect (file-exists-p filename) :not :to-be-truthy)
              (expect (file-directory-p dirname) :not :to-be-truthy)
              (expect (file-exists-p dirname) :not :to-be-truthy))
          (ignore-errors (delete-directory dirname 'recurse))))))

  (describe "flycheck-module-root-directory"

    (it "returns default-directory for no module name and no file name"
      (let ((default-directory flycheck-test-resources-directory))
        (expect (flycheck-module-root-directory nil)
                :to-equal flycheck-test-resources-directory)))

    (it "returns file directory for no module name"
      (let ((default-directory flycheck-test-resources-directory)
            (file-name (flycheck-buttercup-resource-filename
                        "language/emacs-lisp/warnings.el")))
        (expect (flycheck-module-root-directory nil file-name)
                :to-equal (flycheck-buttercup-resource-filename
                           "language/emacs-lisp/"))))

    (it "finds root for module name as string"
      (let ((default-directory flycheck-test-resources-directory)
            (file-name (flycheck-buttercup-resource-filename
                        "language/emacs-lisp/warnings.el")))
        (expect (flycheck-module-root-directory
                 "language.emacs-lisp.warnings" file-name)
                :to-equal flycheck-test-resources-directory)))

    (it "finds root for module name as list"
      (let ((default-directory flycheck-test-resources-directory)
            (file-name (flycheck-buttercup-resource-filename
                        "language/emacs-lisp/warnings.el")))
        (expect (flycheck-module-root-directory
                 '("language" "emacs-lisp" "warnings") file-name)
                :to-equal flycheck-test-resources-directory)))

    (it "falls back for mismatching module name"
      (let ((default-directory flycheck-test-resources-directory)
            (file-name (flycheck-buttercup-resource-filename
                        "language/emacs-lisp/warnings.el")))
        (expect (flycheck-module-root-directory
                 '("foo" "warnings") file-name)
                :to-equal (flycheck-buttercup-resource-filename
                           "language/emacs-lisp/"))))))

;;; test-util.el ends here
