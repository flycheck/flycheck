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

(ert-deftest checker-python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "test-python/syntax-error.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-buffer-sync)
    (flycheck-should-errors '(6 13 "E901 SyntaxError: invalid syntax" error))))

(ert-deftest checker-python-flake8-missing-quote ()
  "Test a syntax error with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "test-python/missing-quote.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(5 14 "E901 SyntaxError: EOL while scanning string literal" error))))

(ert-deftest checker-python-flake8-unused-import ()
  "Test an unused import with flake8"
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "test-python/unused-import.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(5 1 "F401 're' imported but unused" warning))))

(ert-deftest checker-python-flake8-unused-import-ignored ()
  "Test an unused import being ignored with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "test-python/unused-import.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (setq flycheck-flake8rc "flake8rc")
    (flycheck-buffer-sync)
    (should-not flycheck-current-errors)))

(ert-deftest checker-python-flake8-superfluous-space ()
  "Test superfluous spaces with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "test-python/superfluous-space.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(6 13 "E251 no spaces around keyword / parameter equals" error)
     '(6 15 "E251 no spaces around keyword / parameter equals" error))))

(ert-deftest checker-python-flake8-superfluous-space-ignored ()
  "Test superfluous space being ignored with flake8."
  :expected-result (flycheck-fail-unless-checker 'python-flake8)
  (flycheck-with-resource-buffer "test-python/superfluous-space.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (setq flycheck-flake8rc "flake8rc")
    (flycheck-buffer-sync)
    (should-not flycheck-current-errors)))

;;; test-flake8.el ends here
