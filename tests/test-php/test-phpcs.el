;;; test-phpcs.el --- Test the PHP CodeSniffer checker -*- lexical-binding: t; -*-

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
(require 'php+-mode nil t)

(ert-deftest checker-php-phpcs-uppercase-keyqord ()
  "Test an uppercase keyword error by phpcs."
  :expected-result (flycheck-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-with-resource-buffer "test-php/uppercase-keyword.php"
    (dolist (mode '(php-mode php+-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(19 8 "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\"" error))
      (flycheck-ensure-clear))))

(ert-deftest checker-php-phpcs-uppercase-keyqord-explicit-standard ()
  "Test an uppercase keyword error by phpcs."
  :expected-result (flycheck-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-with-resource-buffer "test-php/uppercase-keyword.php"
    (dolist (mode '(php-mode php+-mode))
      (funcall mode)
      (setq flycheck-phpcs-standard "PEAR")
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(19 8 "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\"" error))
      (flycheck-ensure-clear))))

(ert-deftest checker-php-phpcs-uppercase-keyqord-different-standard ()
  "Test an uppercase keyword error by phpcs."
  :expected-result (flycheck-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-with-resource-buffer "test-php/uppercase-keyword.php"
    (dolist (mode '(php-mode php+-mode))
      (funcall mode)
      (setq flycheck-phpcs-standard "Zend")
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(21 1 "A closing tag is not permitted at the end of a PHP file" error))
      (flycheck-ensure-clear))))

(ert-deftest checker-php-phpcs-overlong-line ()
  "Test a phpcs warning about an overlong line."
  :expected-result (flycheck-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-with-resource-buffer "test-php/overlong-line.php"
    (dolist (mode '(php-mode php+-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(19 88 "Line exceeds 85 characters; contains 87 characters" warning))
      (flycheck-ensure-clear))))

(ert-deftest checker-php-phpcs-no-errors ()
  "Test a file without phpcs warnings and errors."
  :expected-result (flycheck-fail-unless-checkers 'php 'php-phpcs)
   (flycheck-with-resource-buffer "test-php/no-errors.php"
    (dolist (mode '(php-mode php+-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (should-not flycheck-current-errors)
      (flycheck-ensure-clear))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-phpcs.el ends here
