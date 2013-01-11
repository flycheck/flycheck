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

(ert-deftest bash-missing-quote ()
  "Test a syntax error from a missing quote."
  (flycheck-with-resource-buffer "missing-quote.bash"
    (sh-mode)
    (sh-set-shell "bash" "no-query")
    (flycheck-should-checker
     'bash '(3 nil "unexpected EOF while looking for matching `''" error))))

(ert-deftest bash-missing-semicolon ()
  "Test a syntax error from a missing semicolon."
  (flycheck-with-resource-buffer "missing-semicolon.bash"
    (sh-mode)
    (sh-set-shell "bash" "no-query")
    (flycheck-should-checker 'bash '(5 nil "`fi'" error))))

;;; test-bash.el ends here
