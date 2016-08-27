;;; flycheck-maint.el --- Flycheck: Maintenace library  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sebastian Wiesner and Flycheck contributors

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

;; This file provides helper functions for maintenance tools.

;;; Code:

(unless noninteractive
  (error "This file must not be used interactively"))

(defconst flycheck/source-dir (locate-dominating-file load-file-name "Cask")
  "The source directory of Flycheck.")

(defun flycheck/collect-el-files (directory &optional recursive)
  "Collect all Emacs Lisp files in DIRECTORY.

If RECURSIVE is given and non-nil collect files recursively."
  (let ((fn-re (rx ".el" eos)))
    (if recursive
        (directory-files-recursively directory fn-re)
      (directory-files directory 'full fn-re))))

(defun flycheck/all-source-files ()
  "Find all source files of Flycheck."
  (append
   (seq-mapcat (lambda (rel-name)
                 (flycheck/collect-el-files
                  (expand-file-name rel-name flycheck/source-dir)))
               '("." "maint/" "doc/" "test/"))
   (flycheck/collect-el-files
    (expand-file-name "test/specs/" flycheck/source-dir) 'recursive)))

(defun flycheck/already-loaded-p (filename)
  "Whether FILENAME is already loaded."
  (not (null (assoc filename load-history))))

(provide 'flycheck-maint)
;;; flycheck-maint.el ends here
