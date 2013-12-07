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

(ert-deftest flycheck-checker/unusable-checker-causes-an-error ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (flycheck-mode)
    (let* ((flycheck-checker 'bash)
           (err (should-error (flycheck-buffer)
                              :type flycheck-testsuite-user-error-type)))
      (should (eq flycheck-checker 'bash))
      (should (string= (cadr err)
                       "Configured syntax checker bash cannot be used"))
      (should (string= flycheck-mode-line " FlyC!")))))

(ert-deftest flycheck-checker/usable-checker-is-used ()
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checker 'emacs-lisp-checkdoc))
      (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
      (flycheck-testsuite-buffer-sync)
      (flycheck-testsuite-should-errors
       '(12 nil warning "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)))))

(ert-deftest flycheck-checker/unregistered-checker-is-used ()
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp))
    (let ((flycheck-checkers (remq 'emacs-lisp-checkdoc flycheck-checkers)))
      (should-not (flycheck-registered-checker-p 'emacs-lisp-checkdoc))
      (let ((flycheck-checker 'emacs-lisp-checkdoc))
        (should (eq (flycheck-get-checker-for-buffer) 'emacs-lisp-checkdoc))
        (flycheck-testsuite-buffer-sync)
        (flycheck-testsuite-should-errors
         '(12 nil warning "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc))))))

(ert-deftest flycheck-select-checker/selecting-sets-the-syntax-checker ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (with-temp-buffer
    (python-mode)
    (flycheck-select-checker 'python-pylint)
    (should (eq flycheck-checker 'python-pylint))))

(ert-deftest flycheck-select-checker/unselecting-unsets-the-syntax-checker ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (with-temp-buffer
    (python-mode)
    (flycheck-select-checker 'python-pylint)
    (flycheck-select-checker nil)
    (should-not flycheck-checker)))

(ert-deftest flycheck-select-checker/selecting-runs-a-syntax-check ()
  (flycheck-testsuite-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    ;; By default, Flake8 is preferred, so we get errors from Flake8
    (flycheck-testsuite-buffer-sync)
    (flycheck-testsuite-should-errors
     '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))
    ;; Selecting Pylint should give us its errors
    (flycheck-select-checker 'python-pylint)
    (flycheck-testsuite-wait-for-syntax-checker)
    (flycheck-testsuite-should-errors
     '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
     '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
     '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
         :checker python-pylint)
     '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
     '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
     '(9 4 info "Invalid method name \"withEggs\" (C0103)"
         :checker python-pylint)
     '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(10 15 warning "Used builtin function 'map' (W0141)"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))))

(ert-deftest flycheck-select-checker/unselecting-a-checker-goes-back-to-automatic-selection ()
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint
                                                            'python-flake8)
  (flycheck-testsuite-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    (flycheck-select-checker 'python-pylint)
    (should (eq flycheck-checker 'python-pylint))
    (flycheck-testsuite-wait-for-syntax-checker)
    (flycheck-testsuite-should-errors
     '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
     '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
     '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
         :checker python-pylint)
     '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
     '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
     '(9 4 info "Invalid method name \"withEggs\" (C0103)"
         :checker python-pylint)
     '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(10 15 warning "Used builtin function 'map' (W0141)"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))
    (flycheck-select-checker nil)
    (should-not flycheck-checker)
    (flycheck-testsuite-wait-for-syntax-checker)
    (flycheck-testsuite-should-errors
     '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck/selects-checker-automatically ()
  :expected-result (flycheck-testsuite-fail-unless-checkers 'python-pylint 'python-flake8)
  (flycheck-testsuite-with-resource-buffer "checkers/python/test.py"
    (python-mode)
    (flycheck-mode)
    (flycheck-testsuite-buffer-sync)
    (should-not flycheck-checker)
    (should (eq flycheck-last-checker 'python-flake8))
    (flycheck-testsuite-should-errors
     '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))
    (let ((flycheck-checkers (remq 'python-flake8 flycheck-checkers)))
      (flycheck-testsuite-buffer-sync)
      (should-not flycheck-checker)
      (should (eq flycheck-last-checker 'python-pylint))
      (flycheck-testsuite-should-errors
       '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
       '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
       '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
           :checker python-pylint)
       '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
       '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
       '(9 4 info "Invalid method name \"withEggs\" (C0103)"
           :checker python-pylint)
       '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
       '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
       '(10 15 warning "Used builtin function 'map' (W0141)"
            :checker python-pylint)
       '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
       '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
       '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
            :checker python-pylint)
       '(22 nil error "Undefined variable 'antigravity' (E0602)"
            :checker python-pylint)))))

;;; selection-test.el ends here
