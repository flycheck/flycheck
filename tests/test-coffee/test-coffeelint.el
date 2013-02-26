;;; test-coffeelint.el --- Test the Coffeelint checker -*- lexical-binding: t; -*-

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

(require 'coffee-mode nil t)

(ert-deftest checker-coffeelint-throwing-strings ()
  "Test an error caused by throwing a string."
  :expected-result (flycheck-fail-unless-checker 'coffee-coffeelint)
  (flycheck-with-resource-buffer "test-coffee/throwing-string.coffee"
    (coffee-mode)
    (flycheck-buffer-sync)
    (flycheck-should-errors '(4 nil "Throwing strings is forbidden" error))))

(ert-deftest checker-coffeelint-throwing-strings-warning ()
  "Test a warning caused by throwing a string."
  :expected-result (flycheck-fail-unless-checker 'coffee-coffeelint)
  (flycheck-with-resource-buffer "test-coffee/throwing-string.coffee"
    (coffee-mode)
    (setq flycheck-coffeelintrc "coffeelint.json")
    (flycheck-buffer-sync)
    (flycheck-should-errors '(4 nil "Throwing strings is forbidden" warning))))

(ert-deftest checker-coffeelint-missing-quote ()
  "Test a syntax error caused by a missing quote."
  :expected-result (flycheck-fail-unless-checker 'coffee-coffeelint)
  (flycheck-with-resource-buffer "test-coffee/missing-quote.coffee"
    (coffee-mode)
    (flycheck-buffer-sync)
    (flycheck-should-errors '(4 nil "missing \", starting" error :no-filename))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-coffeelint.el ends here
