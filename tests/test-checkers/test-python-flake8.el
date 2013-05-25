;;; test-python-flake8.el --- Test the flake8 checker -*- lexical-binding: t; -*-

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

(ert-deftest checker-python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode 'python-pylint
    '(3 13 "E901 SyntaxError: invalid syntax" error)))

(ert-deftest checker-python-flake8-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-warning.py" 'python-mode 'python-pylint
    '(3 1 "F401 're' imported but unused" warning)))

(ert-deftest checker-python-flake8-warning-ignored ()
  "Test an unused import being ignored with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-warning.py" 'python-mode 'python-pylint)))

(ert-deftest checker-python-flake8-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-error.py" 'python-mode 'python-pylint
   '(6 13 "E251 unexpected spaces around keyword / parameter equals" error)
   '(6 15 "E251 unexpected spaces around keyword / parameter equals" error)))

(ert-deftest checker-python-flake8-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-error.py" 'python-mode 'python-pylint)))

(ert-deftest checker-python-flake8-warning-maximum-complexity ()
  "Test superfluous spaces with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-complexity 4)
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-warning-maximum-complexity.py"
     'python-mode 'python-pylint
     '(6 1 "C901 'foo' is too complex (4)" warning))))

(ert-deftest checker-python-flake8-error-maximum-line-length ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-line-length 50)
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-error-maximum-line-length.py"
     'python-mode 'python-pylint
     '(5 51 "E501 line too long (61 > 50 characters)" error))))

(ert-deftest checker-python-flake8-warning-naming ()
  "PEP8 compliant names with Flake8 and pep8-naming."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-warning-naming.py" 'python-mode 'python-pylint
   '(6 7 "N801 class names should use CapWords convention" warning)
   '(7 9 "N802 function name should be lowercase" warning)
   '(8 9 "N806 variable in function should be lowercase" warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-python-flake8.el ends here
