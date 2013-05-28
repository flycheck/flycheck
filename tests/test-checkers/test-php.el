;;; test-php.el --- Test the PHP checker -*- lexical-binding: t; -*-

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

(require 'php-mode nil t)

(ert-deftest checker-php-syntax-error ()
  "Test the T_PAAMAYIM_NEKUDOTAYIM error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'php)
  (flycheck-testsuite-should-syntax-check
   "checkers/php-syntax-error.php" 'php-mode nil
   '(8 nil "syntax error, unexpected ')', expecting :: (T_PAAMAYIM_NEKUDOTAYIM)" error)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-php.el ends here
