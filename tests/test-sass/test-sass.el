;;; test-sass.el --- Test the Sass checker

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

(require 'sass-mode)

(ert-deftest checker-sass-inconsistent-indentation ()
  "Test a syntax error caused by inconsistent indentation."
  :expected-result (flycheck-fail-unless-checker 'sass)
  (flycheck-with-resource-buffer "test-sass/inconsistent-indentation.sass"
    (sass-mode)
    (flycheck-buffer-sync)
    (flycheck-should-errors
     '(5 nil "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces." error))))

;;; test-sass.el ends here
