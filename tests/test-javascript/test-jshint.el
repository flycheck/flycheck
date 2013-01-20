;;; test-jshint.el --- Test the JSHint checker

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

(package-need 'js2-mode)
(require 'js2-mode)
(package-need 'js3-mode)
(require 'js3-mode)

(ert-deftest checker-javascript-jshint-missing-semicolon ()
  "A missing semicolon."
  :expected-result (flycheck-fail-unless-checker 'javascript-jshint)
  (flycheck-with-resource-buffer "test-javascript/missing-semicolon.js"
    (dolist (mode '(js-mode js2-mode js3-mode))
      (funcall mode)
      (flycheck-should-checker
       'javascript-jshint '(3 21 "Missing semicolon." error)))))

(ert-deftest checker-javascript-jshint-missing-quote ()
  "A missing quote."
  :expected-result (flycheck-fail-unless-checker 'javascript-jshint)
  (flycheck-with-resource-buffer "test-javascript/missing-quote.js"
    (dolist (mode '(js-mode js2-mode js3-mode))
      (funcall mode)
      (flycheck-should-checker
       'javascript-jshint '(3 7 "Unclosed string." error)))))

(ert-deftest checker-javascript-jshint-use-eval ()
  "Use eval()"
  :expected-result (flycheck-fail-unless-checker 'javascript-jshint)
  (flycheck-with-resource-buffer "test-javascript/use-eval.js"
    (dolist (mode '(js-mode js2-mode js3-mode))
      (funcall mode)
      (flycheck-should-checker
       'javascript-jshint '(3 1 "eval is evil." error)))))

;;; test-jshint.el ends here
