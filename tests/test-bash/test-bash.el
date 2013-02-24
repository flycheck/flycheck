;;; test-bash.el --- Test the bash checker

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

(ert-deftest checker-bash-missing-quote ()
  "Test a syntax error from a missing quote."
  :expected-result (flycheck-fail-unless-checker 'bash)
  (flycheck-with-resource-buffer "test-bash/missing-quote.bash"
    (sh-mode)
    (sh-set-shell "bash" :no-query)
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(3 nil "unexpected EOF while looking for matching `''" error)
     '(6 nil "syntax error: unexpected end of file" error))))

(ert-deftest checker-bash-missing-semicolon ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-fail-unless-checker 'bash)
  (flycheck-with-resource-buffer "test-bash/missing-semicolon.bash"
    (sh-mode)
    (sh-set-shell "bash" :no-query)
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(5 nil "syntax error near unexpected token `fi'" error)
     '(5 nil "`fi'" error))))

;;; test-bash.el ends here
