;;; test-disabled-checkers.el --- Flycheck Specs: Disabled checkers  -*- lexical-binding: t; -*-

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

;; Specs for disabled syntax checkers

;;; Code:

(require 'flycheck-buttercup)

(describe "Disabled checkers"
  (describe "flycheck-disable-checker"

    (before-each
      (spy-on 'flycheck-buffer :and-return-value t))

    (it "disables a syntax checker"
      (let ((flycheck-disabled-checkers '(hello-world)))
        (with-no-warnings
          (flycheck-disable-checker 'foo-bar))
        (expect flycheck-disabled-checkers :to-contain 'foo-bar)
        (expect flycheck-disabled-checkers :to-contain 'hello-world)
        (expect 'flycheck-buffer :to-have-been-called)))

    (it "enables a syntax checker again"
      (let ((flycheck-disabled-checkers '(spam-with-eggs foo-bar)))
        (with-no-warnings
          (flycheck-disable-checker 'spam-with-eggs 'enable))
        (expect flycheck-disabled-checkers :not :to-contain 'spam-with-eggs)
        (expect flycheck-disabled-checkers :to-contain 'foo-bar)
        (expect 'flycheck-buffer :to-have-been-called)))))

;;; test-disabled-checkers.el ends here
