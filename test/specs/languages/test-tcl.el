;;; test-tcl.el --- Flycheck Specs: Tcl      -*- lexical-binding: t; -*-

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

;; Specs for Tcl support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Tcl"
  (flycheck-buttercup-def-checker-test tcl-nagelfar tcl nil
    (flycheck-buttercup-should-syntax-check
     "language/tcl/test.tcl" 'tcl-mode
     '(7 nil warning "Expr without braces"
         :checker tcl-nagelfar)
     '(8 nil info "Suspicious variable name \"val_${val}\""
         :checker tcl-nagelfar)
     '(9 nil info "Suspicious variable name \"val_${val}\""
         :checker tcl-nagelfar)
     '(12 nil error "Wrong number of arguments \(4\) to \"set\""
          :checker tcl-nagelfar))))

;;; test-tcl.el ends here
