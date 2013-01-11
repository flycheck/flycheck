;;; test-pylint.el --- Test the pylint checker

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

(ert-deftest python-pylint-syntax-error ()
  "Test a real syntax error with pylint."
  :expected-result (flycheck-fail-unless-checker 'python-pylint)
  (flycheck-with-resource-buffer "syntax-error.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker 'python-pylint '(6 nil "invalid syntax" error))))

(ert-deftest python-pylint-missing-quote ()
  "Test a missing quote with pylint."
  :expected-result (flycheck-fail-unless-checker 'python-pylint)
  (flycheck-with-resource-buffer "missing-quote.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker
     'python-pylint '(5 nil "EOL while scanning string literal" error))))

(ert-deftest python-pylint-unknown-module ()
  "Test an unknown module with pylint."
  :expected-result (flycheck-fail-unless-checker 'python-pylint)
  (flycheck-with-resource-buffer "unknown-module.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker
     'python-pylint '(5 nil "Unable to import 'spam'" error))))

(ert-deftest python-pylint-unused-import ()
  "Test an unused import with pylint"
  :expected-result (flycheck-fail-unless-checker 'python-pylint)
  (flycheck-with-resource-buffer "unused-import.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker 'python-pylint
                             '(5 nil "Unused import re" warning))))

(ert-deftest python-pylint-used-map ()
  "Test usage of the map() builtin with the pylint checker."
  :expected-result (flycheck-fail-unless-checker 'python-pylint)
  (flycheck-with-resource-buffer "map-builtin.py"
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (flycheck-should-checker
     'python-pylint '(5 nil "Used builtin function 'map'" warning))))

;;; test-pylint.el ends here
