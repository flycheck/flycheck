;;; test-erlang.el --- Test the Erlang checker -*- lexical-binding: t; -*-

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

(require 'erlang-start nil :no-error)

(ert-deftest checker-erlang-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-error.erl" 'erlang-mode nil
   '(7 nil "head mismatch" error)))

(ert-deftest checker-erlang-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-warning.erl" 'erlang-mode nil
   '(6 nil "wrong number of arguments in format call" warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-erlang.el ends here
