;;; test-pyflakes.el --- Test the pyflakes checker -*- lexical-binding: t; -*-

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
  :expected-result (flycheck-fail-unless-checker 'python-pyflakes)
  (flycheck-with-resource-buffer "test-python/syntax-error.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-disable-checkers 'python-flake8 'python-pylint)
    (flycheck-buffer-sync)
    (flycheck-should-errors '(6 nil "invalid syntax" error))))

(ert-deftest checker-python-pyflakes-missing-quote ()
  "Test a syntax error with pyflakes."
  :expected-result (flycheck-fail-unless-checker 'python-pyflakes)
  (flycheck-with-resource-buffer "test-python/missing-quote.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-disable-checkers 'python-flake8 'python-pylint)
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(5 nil "EOL while scanning string literal" error))))

(ert-deftest checker-python-pyflakes-unused-import ()
  "Test an unused import with pyflakes"
  :expected-result (flycheck-fail-unless-checker 'python-pyflakes)
  (flycheck-with-resource-buffer "test-python/unused-import.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-disable-checkers 'python-flake8 'python-pylint)
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(5 nil "'re' imported but unused" error))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-pyflakes.el ends here
