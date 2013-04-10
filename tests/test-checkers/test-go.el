;;; test-go.el --- Test the go checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>,
;;                    Peter Vasil <mail@petervasil.net>,
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>,
;;         Peter Vasil <mail@petervasil.net>,
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

(require 'go-mode nil :no-error)

(ert-deftest checker-go-syntax-error ()
  "Test a syntax error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go)
  (flycheck-testsuite-should-syntax-check
   "syntax-error.go" 'go-mode nil
   '(5 nil "syntax error: unexpected name, expecting (" error)
   ))

(ert-deftest checker-go-missing-import ()
  "Test a a missing import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go)
  (flycheck-testsuite-should-syntax-check
   "go-test-files/missing-import/missing-import.go" 'go-mode nil
   '(6 nil "undefined: fmt" error)
   ))

(ert-deftest checker-go-unused-variable ()
  "Test an unused variable error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go)
  (flycheck-testsuite-should-syntax-check
   "go-test-files/unused-variable/unused-variable.go" 'go-mode nil
   '(6 nil "x declared and not used" error)
   ))

(ert-deftest checker-go-undefined-variable ()
  "Test an undefined variable error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go)
  (flycheck-testsuite-should-syntax-check
   "go-test-files/undefined-variable/undefined-variable.go" 'go-mode nil
   '(6 nil "undefined: x" error)
   '(6 nil "cannot assign to x" error)
   ))

(ert-deftest checker-go-syntax-error-in-test-file ()
  "Test a syntax error in a test file."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go)
  (flycheck-testsuite-should-syntax-check
   "go-test-files/syntax-error-in-test-file/syntax-error_test.go" 'go-mode nil
   '(6 nil "undefined: x" error)
   '(6 nil "cannot assign to x" error)
   ))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-go.el ends here
