;;; test-cpplint.el --- Test the cpplint checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>,
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>,
;; URL: https://github.com/lunaryorn/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

(require 'c++-mode nil :no-error)

(ert-deftest checker-c++-cpplint-warning ()
  "Test a warning."
  :expected-result (flycheck-testsuite-fail-unless-checker 'c++-cpplint)
  (flycheck-testsuite-should-syntax-check
   "checkers/c++-cpplint-warning.cpp" 'c++-mode nil
   '(4 nil "Blank line at the end of a code block.  Is this needed?  [whitespace/blank_line] [3]" warning)))

(ert-deftest checker-c++-cpplint-error ()
  "Test an error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'c++-cpplint)
  (flycheck-testsuite-should-syntax-check
   "checkers/c++-cpplint-error.cpp" 'c++-mode nil
   '(0 nil "No copyright message found.  You should have a line: \"Copyright [year] <Copyright Owner>\"  [legal/copyright] [5]" error)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-cpplint.el ends here
