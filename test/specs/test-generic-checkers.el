;;; test-generic-checkers.el --- Flycheck Specs: Generic checkers  -*- lexical-binding: t; -*-

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

;; Specs for generic syntax checkers

;;; Code:

(require 'cl-lib)
(require 'flycheck-buttercup)

(describe "Generic syntax checkers"
  (describe "Major mode"
    (it "supports the current major mode"
      (cl-letf (((flycheck-checker-get 'foo 'modes) '(bar-mode foo-mode))
                (major-mode 'bar-mode))
        (expect (flycheck-checker-supports-major-mode-p 'foo)
                :to-be-truthy)))

    (it "supports a major mode"
      (cl-letf (((flycheck-checker-get 'foo 'modes) '(bar-mode foo-mode)))
        (expect (flycheck-checker-supports-major-mode-p 'foo 'foo-mode)
                :to-be-truthy)))

    (it "does not support a major mode"
      (cl-letf (((flycheck-checker-get 'foo 'modes) '(bar-mode foo-mode)))
        (expect (flycheck-checker-supports-major-mode-p 'foo 'spam-mode)
                :not :to-be-truthy)))))

;;; test-generic-checkers.el ends here
