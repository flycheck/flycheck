;;; test-zsh.el --- Test the Zsh checker -*- lexical-binding: t; -*-

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

(ert-deftest checker-zsh-missing-quote ()
  "Test a syntax error from a missing quote."
  :expected-result (flycheck-testsuite-fail-unless-checker 'zsh)
  (flycheck-testsuite-with-resource-buffer "missing-quote.zsh"
    (sh-mode)
    (sh-set-shell "zsh" :no-query)
    (flycheck-testsuite-buffer-sync)
    (flycheck-testsuite-should-errors '(6 nil "unmatched '" error))))

(ert-deftest checker-zsh-missing-semicolon ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'zsh)
  (flycheck-testsuite-with-resource-buffer "missing-semicolon.zsh"
    (sh-mode)
    (sh-set-shell "zsh" :no-query)
    (flycheck-testsuite-buffer-sync)
    (flycheck-testsuite-should-errors '(5 nil "parse error near `fi'" error))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-zsh.el ends here
