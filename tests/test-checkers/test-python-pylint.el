;;; test-python-pylint.el --- Test the pylint checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
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

(require 'python)

(ert-deftest checker-python-pylint-syntax-error ()
  "Test a real syntax error with pylint."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode 'python-flake8
   '(3 nil "invalid syntax" error)))

(ert-deftest checker-python-pylint-error ()
  "Test an unknown module with pylint."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-pylint-error.py" 'python-mode 'python-flake8
   '(3 nil "Unable to import 'spam'" error)))

(ert-deftest checker-python-pylint-used-map ()
  "Test usage of the map() builtin with the pylint checker."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-pylint-warning.py" 'python-mode 'python-flake8
   '(3 nil "Used builtin function 'map'" warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-python-pylint.el ends here
