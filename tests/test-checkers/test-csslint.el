;;; test-csslint.el --- Test the CSSLint checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>,
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>,
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

(ert-deftest checker-css-csslint-qualified-heading ()
  "Test a warning caused by a qualified heading."
  :expected-result (flycheck-fail-unless-checker 'css-csslint)
  (flycheck-with-resource-buffer "qualified-heading.css"
    (css-mode)
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(3 6 "Heading (h1) should not be qualified." warning))))

(ert-deftest checker-css-csslint-missing-brace ()
  "Test a syntax error caused by a missing brace."
  :expected-result (flycheck-fail-unless-checker 'css-csslint)
  (flycheck-with-resource-buffer "missing-brace.css"
    (css-mode)
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(4 16 "Expected LBRACE at line 4, col 16." error)
     '(4 16 "Unexpected token '100%' at line 4, col 16." error)
     '(4 20 "Unexpected token ';' at line 4, col 20." error)
     '(5 1 "Unexpected token '}' at line 5, col 1." error))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-csslint.el ends here
