;;; test-code-style.el --- Flycheck Specs: Code style     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sebastian Wiesner

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

;; Specs for code style.

;;; Code:

(require 'flycheck-buttercup)
(require 'checkdoc)
(require 'seq)

(defun flycheck/checkstyle (file-name)
  "Run `checkstyle' on FILE-NAME and return the results."
  (with-temp-buffer
    (insert-file-contents file-name 'visit)
    (delay-mode-hooks (emacs-lisp-mode))
    (setq delay-mode-hooks nil)
    (with-demoted-errors "Error in checkdoc: %S"
      (checkdoc-current-buffer t)
      (with-current-buffer checkdoc-diagnostic-buffer
        (unwind-protect
            (progn
              (goto-char (point-min))
              ;; Skip over the checkdoc header
              (re-search-forward (rx line-start "***" (1+ not-newline)
                                     ": checkdoc-current-buffer"))
              (forward-line 1)
              (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer))))))

(defun flycheck/find-all-elisp-files ()
  "Find all Emacs Lisp files in our source code."
  (let* ((source-dir (locate-dominating-file default-directory "Cask"))
         (default-directory source-dir)
         (sources (seq-map #'expand-file-name (process-lines "git" "ls-files"))))
    (seq-filter (lambda (fn) (and (string-match-p (rx ".el" eos) fn)
                                  (not (string-match-p (rx "test/resources/") fn))))
                sources)))

(describe "Code style"
  (dolist (source (flycheck/find-all-elisp-files))
    (describe (format "File %s" (file-relative-name source))
      (before-each
        (assume (version<= "25" emacs-version) "Our style must match Emacs 25"))

      (it "has proper documentation format"
        (expect (flycheck/checkstyle source)
                :to-equal "")))))

;;; test-code-style.el ends here
