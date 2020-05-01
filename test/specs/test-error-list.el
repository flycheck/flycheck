;;; test-error-list.el --- Flycheck Specs: Error List  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for the error list.

;;; Code:

(require 'flycheck-buttercup)

(defmacro flycheck/with-error-list-buffer (&rest body)
  "Run BODY in a temporary error list buffer."
  (declare (indent 0))
  `(with-current-buffer (get-buffer-create flycheck-error-list-buffer)
     (prog1 (progn ,@body)
       (kill-buffer flycheck-error-list-buffer))))

(describe "Error List"
  (it "has the correct buffer name"
    (expect flycheck-error-list-buffer :to-equal "*Flycheck errors*"))

  (it "has a permanently local source buffer"
    (flycheck/with-error-list-buffer
      (expect (get 'flycheck-error-list-source-buffer 'permanent-local)
              :to-be-truthy)))

  (describe "Filter"
    (it "kills the filter variable when resetting the filter"
      (flycheck/with-error-list-buffer
        (setq-local flycheck-error-list-minimum-level 'error)
        (expect 'flycheck-error-list-minimum-level :to-be-local)
        (flycheck-error-list-reset-filter)
        (expect 'flycheck-error-list-minimum-level :not :to-be-local)))

    (describe "Mode Line"
      (it "shows no mode line indicator if no filter is set"
        (let ((flycheck-error-list-minimum-level nil))
          (expect (flycheck-error-list-mode-line-filter-indicator)
                  :to-be-empty-string)))

      (it "shows the level filter in the mode line if set"
        (let ((flycheck-error-list-minimum-level 'warning))
          (expect (flycheck-error-list-mode-line-filter-indicator)
                  :to-equal " [>= warning]"))))))

;;; test-error-list.el ends here
