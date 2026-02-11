;;; test-scala.el --- Flycheck Specs: Scala      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for Scala support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Scala"
  (flycheck-buttercup-def-checker-test scala scala nil
    (flycheck-buttercup-should-syntax-check
     "language/scala/syntax-error.scala" 'scala-mode
     '(3 nil error "identifier expected but '{' found." :checker scala)))

  (flycheck-buttercup-def-checker-test scala-scalastyle scala error
    (let ((flycheck-scalastylerc "scalastyle.xml"))
      (flycheck-buttercup-should-syntax-check
       "language/scala/style-error.scala" 'scala-mode
       '(6 5 error "Don't use println" :checker scala-scalastyle))))

  (flycheck-buttercup-def-checker-test scala-scalastyle scala warning
    (let ((flycheck-scalastylerc "scalastyle.xml"))
      (flycheck-buttercup-should-syntax-check
       "language/scala/style-warning.scala" 'scala-mode
       '(5 9 warning "Redundant braces after class definition"
           :checker scala-scalastyle)))))

;;; test-scala.el ends here
