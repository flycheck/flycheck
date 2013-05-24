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

(require 'elixir-mode nil :no-error)

(ert-deftest checker-elixir-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-error.ex" 'elixir-mode nil
   '(5 nil "function puts/1 undefined" error)))

(ert-deftest checker-elixir-warnings ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warnings.ex" 'elixir-mode nil
   '(5 nil "variable a is unused" warning)
   '(6 nil "variable a shadowed in 'fun'" warning)
   '(14 nil "this clause cannot match because a previous clause at line 11 always matches" warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-elixir.el ends here
