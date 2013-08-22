;;; selection-test.el --- Tests for syntax checker selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for syntax checker selection.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-select-checker-automatically ()
  "Test automatic checker selection.

Have Flycheck select a checker automatically, then change the
list of registered checkers, and test that a new checker is
selected."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-testsuite-with-resource-buffer "errors-from-different-checkers.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint python-flake8))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 nil "Unused import re (W0611)" 'warning)
    (should (not flycheck-checker))
    (should (eq flycheck-last-checker 'python-pylint))
    (flycheck-clear-errors)
    (should (not flycheck-current-errors))
    (setq flycheck-checkers '(python-flake8))
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 1 "F401 're' imported but unused" 'warning)
    (should (not flycheck-checker))
    (should (eq flycheck-last-checker 'python-flake8))))

(ert-deftest flycheck-select-checker ()
  "Test that checkers are properly selected.

Select two different checkers and test that each one is properly
executed, and has its errors reported."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-testsuite-with-resource-buffer "errors-from-different-checkers.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint))
    (should (-all? 'flycheck-may-use-checker flycheck-checkers))
    (should (not flycheck-checker))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 nil "Unused import re (W0611)" 'warning)
    (flycheck-select-checker 'python-flake8)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 1 "F401 're' imported but unused" 'warning)
    (should (eq flycheck-checker 'python-flake8))))

(ert-deftest flycheck-select-checker-unusable ()
  "Test that selecting an unusable checker fails.

Select a checker that is not usable in a buffer, and test that an
error is signaled on all subsequent checks."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint 'bash)
  (flycheck-testsuite-with-resource-buffer "errors-from-different-checkers.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint))
    (should (not flycheck-checker))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-testsuite-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-testsuite-should-error 3 nil "Unused import re (W0611)" 'warning)
    (let* ((error-data (should-error (flycheck-select-checker 'bash)
                                     :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr error-data)
                       "Configured syntax checker bash cannot be used"))
      (should (string= flycheck-mode-line " FlyC!"))
      ;; A subsequent syntax checker should still fail, and not fall back to
      ;; automatic selection
      (should-error (flycheck-buffer) :type flycheck-testsuite-user-error-type)
      (should (string= flycheck-mode-line " FlyC!")))))

;;; selection-test.el ends here
