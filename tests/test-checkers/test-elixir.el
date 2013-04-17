;;; test-elixir.el --- Test the Elixir checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sylvain Benner <sylvain.benner@gmail.com>
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

;; This file is part of flycheck.
;;
;; flycheck is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; flycheck is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with flycheck. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

(require 'elixir-mode)

(ert-deftest checker-elixir-syntax-error ()
  "Test a real Elixir syntax error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-syntax-error.ex" 'elixir-mode nil
   '(4 nil "function puts/1 undefined" error)))

(ert-deftest checker-elixir-warning-unused ()
  "Test an Elixir warning about unused variable."
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warning-unused-var.ex" 'elixir-mode nil
   '(2 nil "variable unused_var is unused" warning)))

(ert-deftest checker-elixir-warning-shadowed ()
  "Test an Elixir warning about shadowed variable."
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warning-shadowed.ex" 'elixir-mode nil
   '(5 nil "variable a shadowed in 'fun'" warning)))

(ert-deftest checker-elixir-warning-always-match ()
  "Test an Elixir warning about always match clause."
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warning-always-match.ex" 'elixir-mode nil
   '(6 nil "this clause cannot match because a previous clause at line 3 always matches"
       warning)))

(ert-deftest checker-elixir-warning-obsolete ()
  "Test an Elixir warning about always match clause."
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warning-obsolete.ex" 'elixir-mode nil
   '(3 nil "list/1 obsolete" warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-elixir.el ends here
