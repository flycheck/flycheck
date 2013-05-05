;;; test-puppet-parser.el --- Test the puppet parser -*- lexical-binding: t; -*-

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

(ert-deftest checker-puppet-parser-singleline-syntax-error ()
  "Test a real syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-singleline.pp" 'puppet-mode nil
   '(3 nil "Syntax error at ','; expected '}'" error)))

(ert-deftest checker-puppet-parser-multiline-syntax-error ()
  "Test a real (multi line) syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-multiline.pp" 'puppet-mode nil
   '(8 nil "Unclosed quote after '' in 'something
}
'" error)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-puppet-parser.el ends here
