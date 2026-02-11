;;; test-vhdl.el --- Flycheck Specs: VHDL      -*- lexical-binding: t; -*-

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

;; Specs for VHDL support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language VHDL"
  (flycheck-buttercup-def-checker-test vhdl-ghdl vhdl error
    (flycheck-buttercup-should-syntax-check
     "language/vhdl.vhd" 'vhdl-mode
     '(4 1 error "';' is expected instead of '<EOF>'"
         :checker vhdl-ghdl))))

;;; test-vhdl.el ends here
