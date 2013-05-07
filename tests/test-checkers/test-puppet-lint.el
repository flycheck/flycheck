;;; test-puppet-lint.el --- Test the puppet linter -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Mark Hellewell <mark.hellewell@gmail.com>
;;
;; Author: Mark Hellewell <mark.hellewell@gmail.com>

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

(require 'puppet-mode)

(ert-deftest checker-puppet-puppet-lint-case-without-default-warn ()
  "Test case statement missing default warning."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-puppet-lint-case_without_default_warn.pp" 'puppet-mode nil
   '(2 nil "case statement without a default case" warning)))

(ert-deftest checker-puppet-puppet-lint-mlayout ()
  "mlayout not in autoload module layout"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-puppet-lint/mlayout/manifests/wrong.pp" 'puppet-mode nil
   '(2 nil "mlayout not in autoload module layout" error)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-puppet-lint.el ends here
