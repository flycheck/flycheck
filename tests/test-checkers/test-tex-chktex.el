;;; test-tex-chktex.el --- Test the chktex checker -*- lexical-binding: t; -*-

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

(ert-deftest checker-tex-chktex-warning ()
  "Test missing intersentence spacing."
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-chktex)
  (flycheck-testsuite-should-syntax-check
   "checkers/tex-chktex-warning.tex" 'latex-mode nil
   '(9 28 "13:Intersentence spacing (`\\@') should perhaps be used." warning)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-tex-chktex.el ends here
