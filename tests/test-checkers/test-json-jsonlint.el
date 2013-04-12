;;; test-json-jsonlint.el --- Test the JSON checker -*- lexical-binding: t; -*-

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

(ert-deftest checker-json-jsonlint-error ()
  "Test a syntax error from multiple top-level objects."
  :expected-result (flycheck-testsuite-fail-unless-checker 'json-jsonlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/json-jsonlint-error.json" 'text-mode nil
    '(1 42 "found: ',' - expected: 'EOF'." error)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-json-jsonlint.el ends here
