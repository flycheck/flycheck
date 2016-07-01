;;; test-generic-checkers.el --- Flycheck Specs: Generic checkers  -*- lexical-binding: t; -*-

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

;; Specs for generic syntax checkers.

;;; Code:

(require 'flycheck-buttercup)

(describe "A Generic checker"
  ;; Fake a checker for `spam-mode' and `eggs-mode'
  (it "supports a mode it was declared for"
    (cl-letf (((flycheck-checker-get 'dummy-checker 'modes)
               '(spam-mode 'eggs-mode))
              (major-mode 'spam-mode))
      (expect (flycheck-checker-supports-major-mode-p 'dummy-checker)
              :to-be-truthy)))

  (it "does not support a mode it was not declared for"
    (cl-letf (((flycheck-checker-get 'dummy-checker 'modes)
               '(spam-mode 'eggs-mode))
              (major-mode 'foo-mode))
      (expect (flycheck-checker-supports-major-mode-p 'dummy-checker)
              :not :to-be-truthy))))

;;; test-generic-checkers.el ends here
