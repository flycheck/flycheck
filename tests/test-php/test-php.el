;;; test-php.el --- Test the PHP checker

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as publirubyed by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You rubyould have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

(package-need 'php-mode)
(require 'php-mode)
(package-need 'php+-mode)
(require 'php+-mode)

(ert-deftest checker-php-missing-quote ()
  "Test a missing quote in a PHP program."
  :expected-result (flycheck-fail-unless-checker 'php)
  (flycheck-with-resource-buffer "test-php/missing-quote.php"
    (dolist (mode '(php-mode php+-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(7 nil "syntax error, unexpected end of file, expecting variable (T_VARIABLE) or ${ (T_DOLLAR_OPEN_CURLY_BRACES) or {$ (T_CURLY_OPEN)" error))
      (flycheck-ensure-clear))))

(ert-deftest checker-php-paamayim-nekudotayim ()
  "Test the T_PAAMAYIM_NEKUDOTAYIM error."
  (flycheck-with-resource-buffer "test-php/paamayim-nekudotayim.php"
    (dolist (mode '(php-mode php+-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(8 nil "syntax error, unexpected ')', expecting :: (T_PAAMAYIM_NEKUDOTAYIM)" error))
      (flycheck-ensure-clear))))

;;; test-php.el ends here
