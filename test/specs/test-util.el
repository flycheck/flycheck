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

(describe "Utilties"

  (describe "flycheck-buffer-empty-p"

    (it "considers an empty buffer as empty"
      (with-temp-buffer
        (expect (flycheck-buffer-empty-p) :to-be-truthy)))

    (it "does not consider an buffer with content as empty"
      (with-temp-buffer
        (insert "foo bar")
        (expect (flycheck-buffer-empty-p) :not :to-be-truthy)))

    (it "detects emptiness of narrowed buffers"
      (with-temp-buffer
        (insert "foo\nbar")
        (goto-char (point-min))
        (narrow-to-region (point-min) (point-min))
        (expect (buffer-string) :to-equal "")
        (expect (flycheck-buffer-empty-p) :not :to-be-truthy)))))

;;; test-util.el ends here
