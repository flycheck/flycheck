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
(require 's)
(require 'flycheck)

(require 'python)

(defun python-mode-no-indent-guessing ()
  (let ((python-indent-guess-indent-offset nil))
    (python-mode)))

(ert-deftest python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  (should-flycheck-checker
   (resource "syntax-error.py")
   'python-mode-no-indent-guessing
   'flycheck-checker-python-flake8
   '(6 nil "invalid syntax" error)))

(ert-deftest python-flake8-missing-quote ()
  "Test a syntax error with flake8."
  (should-flycheck-checker
   (resource "missing-quote.py")
   'python-mode-no-indent-guessing
   'flycheck-checker-python-flake8
   '(5 nil "EOL while scanning string literal" error)))

(ert-deftest python-flake8-unused-import ()
  "Test an unused import with flake8"
  (should-flycheck-checker
   (resource "unused-import.py")
   'python-mode-no-indent-guessing
   'flycheck-checker-python-flake8
   '(5 nil "W402 're' imported but unused" warning)))

(ert-deftest python-flake8-superfluous-space ()
  "Test superfluous spaces with flake8."
  (should-flycheck-checker
   (resource "superfluous-space.py")
   'python-mode-no-indent-guessing
   'flycheck-checker-python-flake8
   '(6 13 "E251 no spaces around keyword / parameter equals" error)
   '(6 15 "E251 no spaces around keyword / parameter equals" error)))

(ert-deftest python-flake8-redefinition-of-unused-function ()
  "Test a redefinition of an unused function with flake8."
  (should-flycheck-checker
   (resource "redefinition-of-unused-function.py")
   'python-mode-no-indent-guessing
   'flycheck-checker-python-flake8
   '(10 nil "W806 redefinition of function 'foo' from line 7" warning)))

;;; test-flake8.el ends here
