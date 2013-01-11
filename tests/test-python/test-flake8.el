;;; test-flake8.el --- Test the flake8 checker

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

(ert-deftest python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "syntax-error.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker 'python-flake8 '(6 nil "invalid syntax" error))))

(ert-deftest python-flake8-missing-quote ()
  "Test a syntax error with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "missing-quote.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker
     'python-flake8 '(5 nil "EOL while scanning string literal" error))))

(ert-deftest python-flake8-unused-import ()
  "Test an unused import with flake8"
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "unused-import.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker
     'python-flake8 '(5 nil "W402 're' imported but unused" warning))))

(ert-deftest python-flake8-superfluous-space ()
  "Test superfluous spaces with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "superfluous-space.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker
     'python-flake8
     '(6 13 "E251 no spaces around keyword / parameter equals" error)
     '(6 15 "E251 no spaces around keyword / parameter equals" error))))

(ert-deftest python-flake8-superfluous-space-ignored ()
  "Test superfluous space being ignored with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "superfluous-space.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (setq flycheck-flake8rc "flake8rc")
    (flycheck-should-checker 'python-flake8)))

(ert-deftest python-flake8-redefinition-of-unused-function ()
  "Test a redefinition of an unused function with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "redefinition-of-unused-function.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker
     'python-flake8
     '(10 nil "W806 redefinition of function 'foo' from line 7" warning))))

;;; test-flake8.el ends here
