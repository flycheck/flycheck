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
                              "dir/flycheck-testprog.program" temp-dir))
          (make-directory (expand-file-name "dir" temp-dir))
          (write-region "" nil program-path)
          (set-file-modes program-path
                          (logior 73 (file-modes program-path))))
        (after-each
          (ignore-errors (delete-directory temp-dir 'recursive)))

        (it "resolves the path when given an existing program name"
          (let* ((default-directory temp-dir)
                 (exec-path (list (expand-file-name "dir" temp-dir)))
                 (exec-suffixes '(".program"))
                 (result (flycheck-default-executable-find
                          "flycheck-testprog")))
            (expect result :to-equal program-path)))

        (it "resolves the path when given an existing relative program path"
          (let* ((default-directory temp-dir)
                 (exec-suffixes '(".program"))
                 (result (flycheck-default-executable-find
                          "dir/flycheck-testprog")))
            (expect result :to-equal program-path)))

        (it "resolves the path when given an existing absolute program path"
          (let* ((default-directory temp-dir)
                 (exec-suffixes '(".program"))
                 (result (flycheck-default-executable-find
                          (expand-file-name "dir/flycheck-testprog" temp-dir))))
            (expect result :to-equal program-path)))))))


;;; test-util.el ends here
