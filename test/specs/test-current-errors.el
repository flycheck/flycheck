;;; test-current-errors.el --- Flycheck Specs: Errors in current buffer  -*- lexical-binding: t; -*-

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

;; Specs for errors in the current buffer, e.g. `flycheck-current-errors'.

;;; Code:

(describe "Flycheck Current Errors"

  (it "finds the most severe errors"
    (let ((flycheck-current-errors (list (flycheck-error-new-at 20 10 'warning)
                                         (flycheck-error-new-at 20 20 'error)
                                         (flycheck-error-new-at 5 15 'error))))
      (expect (flycheck-most-severe-current-error)
              :to-equal
              (flycheck-error-new-at 20 20 'error)))))

;;; test-current-errors.el ends here
