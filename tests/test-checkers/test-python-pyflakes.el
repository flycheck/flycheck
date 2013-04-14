;;; test-python-pyflakes.el --- Test the pyflakes checker -*- lexical-binding: t; -*-

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

(ert-deftest checker-python-pyflakes-syntax-error ()
  "Test a real syntax error with pyflakes."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pyflakes)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode '(python-flake8 python-pylint)
    '(3 nil "invalid syntax" error)))

(ert-deftest checker-python-pyflakes-error ()
  "Test an unused import with pyflakes"
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pyflakes)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-pyflakes-error.py" 'python-mode '(python-flake8 python-pylint)
   '(3 nil "'re' imported but unused" warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-python-pyflakes.el ends here
