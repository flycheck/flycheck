;;; test-jshint.el --- Test the JSHint checker -*- lexical-binding: t; -*-

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

(require 'js2-mode nil t)
(require 'js3-mode nil t)

(ert-deftest checker-javascript-jshint-missing-semicolon ()
  "A missing semicolon."
  :expected-result (flycheck-fail-unless-checker 'javascript-jshint)
  (flycheck-with-resource-buffer "missing-semicolon.js"
    (dolist (mode '(js-mode js2-mode js3-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors '(6 23 "Missing semicolon." error))
      (flycheck-ensure-clear))))

(ert-deftest checker-javascript-jshint-use-eval ()
  "Use eval()"
  :expected-result (flycheck-fail-unless-checker 'javascript-jshint)
  (flycheck-with-resource-buffer "use-eval.js"
    (dolist (mode '(js-mode js2-mode js3-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors '(3 1 "eval can be harmful." error))
      (flycheck-ensure-clear))))

(ert-deftest checker-javascript-jshint-unused-variable ()
  "An unused variable."
  :expected-result (flycheck-fail-unless-checker 'javascript-jshint)
  (flycheck-with-resource-buffer "unused-variable.js"
    (dolist (mode '(js-mode js2-mode js3-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors '(5 nil "Unused variable: 'foo'" warning))
      (flycheck-ensure-clear))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-jshint.el ends here
