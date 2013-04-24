;;; test-ruby-rubocop.el --- Test the RuboCop checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as publirubyed by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You rubyould have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

(defvar ruby-mode-hook)

(ert-deftest checker-ruby-rubocop-syntax-error ()
  "Test a Ruby syntax error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode nil
   '(5 nil "Syntax error, unexpected tconstant, expecting $end" error)))

(ert-deftest checker-ruby-rubocop-warnings ()
  "Test some Ruby warnings emitted by RuboCop."
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-rubocop-warnings.rb" 'ruby-mode nil
   '(1 nil "Missing utf-8 encoding comment." warning)
   '(3 nil "Use snake_case for symbols." warning)
   '(4 nil "Prefer single-quoted strings when you don't need string interpolation or special symbols." warning)))

(ert-deftest checker-ruby-rubocop-warnings-disabled ()
  "Test Ruby RuboCop configuration file."
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-with-hook ruby-mode-hook
      (setq flycheck-rubocoprc "rubocop.yml")
    (flycheck-testsuite-should-syntax-check
     "checkers/ruby-rubocop-warnings.rb" 'ruby-mode nil
      '(1 nil "Missing utf-8 encoding comment." warning)
      '(4 nil "Prefer single-quoted strings when you don't need string interpolation or special symbols." warning))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-ruby-rubocop.el ends here
