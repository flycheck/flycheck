;;; test-sh.el --- Test the sh checker

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

(require 'sh-script)

(ert-deftest sh-missing-quote ()
  "Test a syntax error from a missing quote."
  :expected-result (flycheck-fail-unless-checker 'sh)
  (flycheck-with-resource-buffer "test-sh/missing-quote.sh"
    (sh-mode)
    (sh-set-shell "sh" :no-query)
    (flycheck-should-checker
     'sh '(6 nil "Syntax error: Unterminated quoted string" error))))

(ert-deftest sh-missing-semicolon ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-fail-unless-checker 'sh)
  (flycheck-with-resource-buffer "test-sh/missing-semicolon.sh"
    (sh-mode)
    (sh-set-shell "sh" :no-query)
    (flycheck-should-checker
     'sh '(5 nil "Syntax error: \"fi\" unexpected (expecting \"then\")" error))))

;;; test-sh.el ends here
