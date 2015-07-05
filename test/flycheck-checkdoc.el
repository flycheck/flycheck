;;; flycheck-checkdoc.el --- Flycheck: Checkdoc linter -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://www.flycheck.org

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

;; Lint files given on command line with checkdoc

;;; Code:

(require 'checkdoc)

(defun flycheck-checkdoc-file-batch (filename)
  "Check FILENAME with checkdoc and print results."
  (with-temp-buffer
    (insert-file-contents filename 'visit)
    (with-demoted-errors "Error in checkdoc: %S"
      (checkdoc-current-buffer t)
      (with-current-buffer checkdoc-diagnostic-buffer
        (goto-char (point-min))
        ;; Skip over the checkdoc header
        (re-search-forward (rx line-start "***" (1+ not-newline)
                               ": checkdoc-current-buffer"))
        (forward-line 1)
        (princ (buffer-substring-no-properties (point) (point-max)))
        (terpri)
        (kill-buffer)))))

(defun flycheck-checkdoc-batch-and-exit ()
  "Run checkdoc on given files."
  (when (string= (car command-line-args-left) "--")
    (pop command-line-args-left))
  (mapc #'flycheck-checkdoc-file-batch command-line-args-left)
  (kill-emacs 0))

(provide 'flycheck-checkdoc)
;;; flycheck-checkdoc.el ends here
