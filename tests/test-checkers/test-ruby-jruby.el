;;; test-ruby-jruby.el --- Test the JRuby checker -*- lexical-binding: t; -*-

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

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

(defun flycheck-testsuite-jruby-expected-result ()
  (if (flycheck-testsuite-travis-ci-p) :failed
    (flycheck-testsuite-fail-unless-checker 'ruby-jruby)))

(ert-deftest checker-ruby-jruby-syntax-error ()
  "Test a Ruby syntax error."
  :expected-result (flycheck-testsuite-jruby-expected-result)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode '(ruby-rubocop ruby)
   '(5 nil "syntax error, unexpected tCONSTANT" error)))

(ert-deftest checker-ruby-jruby-warning ()
  "Test a Ruby syntax error."
  :expected-result (flycheck-testsuite-jruby-expected-result)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-warning.rb" 'ruby-mode '(ruby-rubocop ruby)
   '(3 nil "Useless use of == in void context." warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-ruby-jruby.el ends here
