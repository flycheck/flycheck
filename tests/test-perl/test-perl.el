;;; test-perl.el --- Test the bash checker

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
(require 'dash)
(require 'flycheck)

(package-need 'cperl-mode)
(require 'cperl-mode)

(ert-deftest perl-unused-variable ()
  "Test an unused variable with the Perl checker."
  (--each '(perl-mode cperl-mode)
    (should-flycheck-checker
     (resource "unused-variable.pl") it 'flycheck-checker-perl
     '(4 nil "Name \"main::x\" used only once: possible typo" error))))

(ert-deftest perl-unqualified-variable ()
  "Test an unqualified variable with the Perl checker."
  (--each '(perl-mode cperl-mode)
    (should-flycheck-checker
     (resource "unqualified-variable.pl") it 'flycheck-checker-perl
     '(5 nil "Global symbol \"$x\" requires explicit package name" error))))

(ert-deftest perl-syntax-error ()
  "Test a syntax error with the Perl checker."
   (--each '(perl-mode cperl-mode)
    (should-flycheck-checker
     (resource "syntax-error.pl") it 'flycheck-checker-perl
     '(4 nil "syntax error" error))))

(require 'sh-script)

;;; test-perl.el ends here
