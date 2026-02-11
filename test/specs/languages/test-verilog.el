;;; test-verilog.el --- Flycheck Specs: Verilog      -*- lexical-binding: t; -*-

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

;; Specs for Verilog support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Verilog"
  (flycheck-buttercup-def-checker-test verilog-verilator verilog error
    (flycheck-buttercup-should-syntax-check
     "language/verilog/verilator_error.v" 'verilog-mode
     '(4 nil error "Unsupported: $fopen with multichannel descriptor.  Add ,\"w\" as second argument to open a file descriptor."
         :checker verilog-verilator)))

  (flycheck-buttercup-def-checker-test verilog-verilator verilog warning
    (flycheck-buttercup-should-syntax-check
     "language/verilog/verilator_warning.v" 'verilog-mode
     '(2 nil warning "Signal is not driven, nor used: 'val'"
         :checker verilog-verilator :id "UNUSED"))))

;;; test-verilog.el ends here
