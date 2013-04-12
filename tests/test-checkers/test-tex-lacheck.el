;;; test-tex-lacheck.el --- Test the lacheck checker -*- lexical-binding: t; -*-

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

(ert-deftest checker-tex-lacheck-warning ()
  "Test unwanted space."
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-lacheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/tex-lacheck-warning.tex" 'latex-mode 'tex-chktex
    '(9 nil "possible unwanted space at \"{\"" warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-tex-lacheck.el ends here
