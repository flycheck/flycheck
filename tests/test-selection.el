;;; test-selection.el --- Tests for checker selection

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

(ert-deftest flycheck-automatic-selection-no-checker ()
  "Test mode line status report if automatic selection failed."
  (with-temp-buffer
    (insert "Hello world\n")
    (fundamental-mode)                  ; This mode certainly has no checker
    (flycheck-mode)
    (flycheck-buffer)
    (should-not (or flycheck-current-process flycheck-syntax-checker-finished))
    (should (string= flycheck-mode-line " FlyC-"))))

(ert-deftest flycheck-select-checker ()
  "Test that checkers are properly selected.

Select two different checkers and test that each one is properly
executed, and has its errors reported."
  :expected-result (flycheck-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-with-resource-buffer "test-python/unused-import.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint))
    (should (-all? 'flycheck-may-use-checker flycheck-checkers))
    (should (not flycheck-checker))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-should-error '(5 nil "Unused import re" warning))
    (flycheck-select-checker 'python-flake8)
    (flycheck-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-should-error '(5 nil "W402 're' imported but unused" warning))
    (should (eq flycheck-checker 'python-flake8))))

(ert-deftest flycheck-select-checker-unusable ()
  "Test that selecting an unusable checker fails.

Select a checker that is not usable in a buffer, and test that an
error is signaled on all subsequent checks."
  :expected-result (flycheck-fail-unless-checkers 'python-pylint 'bash)
  (flycheck-with-resource-buffer "test-python/unused-import.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint))
    (should (not flycheck-checker))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-should-error '(5 nil "Unused import re" warning))
    (let* ((error-type (if (flycheck-min-emacs-version-p 24 3)
                          'user-error
                         'error))
           (error-data (should-error (flycheck-select-checker 'bash)
                                     :type error-type)))
      (should (string= (car (cdr error-data))
                       "Configured syntax checker bash cannot be used"))
      (should (string= flycheck-mode-line " FlyC!"))
      ;; A subsequent syntax checker should still fail, and not fall back to
      ;; automatic selection
      (should-error (flycheck-buffer) :type error-type)
      (should (string= flycheck-mode-line " FlyC!")))))

(ert-deftest flycheck-select-checker-automatically ()
  "Test automatic checker selection.

Have Flycheck select a checker automatically, then change the
list of registered checkers, and test that a new checker is
selected."
  :expected-result (flycheck-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-with-resource-buffer "test-python/unused-import.py"
    (python-mode)
    (set (make-local-variable 'flycheck-checkers) '(python-pylint python-flake8))
    (flycheck-mode)
    (flycheck-buffer)
    (flycheck-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-should-error '(5 nil "Unused import re" warning))
    (should (not flycheck-checker))
    (should (eq flycheck-last-checker 'python-pylint))
    (flycheck-clear-errors)
    (should (not flycheck-current-errors))
    (setq flycheck-checkers '(python-flake8))
    (flycheck-buffer)
    (flycheck-wait-for-syntax-checker)
    (should (= (length flycheck-current-errors) 1))
    (flycheck-should-error '(5 nil "W402 're' imported but unused" warning))
    (should (not flycheck-checker))
    (should (eq flycheck-last-checker 'python-flake8))))

;;; test-selection.el ends here
