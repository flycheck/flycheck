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

(require 'cperl-mode)

(ert-deftest checker-perl-unused-variable ()
  "Test an unused variable with the Perl checker."
  :expected-result (flycheck-fail-unless-checker 'perl)
  (flycheck-with-resource-buffer "test-perl/unused-variable.pl"
    (dolist (mode '(perl-mode cperl-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(4 nil "Name \"main::x\" used only once: possible typo" error))
      (flycheck-ensure-clear))))

(ert-deftest checker-perl-unqualified-variable ()
  "Test an unqualified variable with the Perl checker."
  :expected-result (flycheck-fail-unless-checker 'perl)
  (flycheck-with-resource-buffer "test-perl/unqualified-variable.pl"
    (dolist (mode '(perl-mode cperl-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors
       '(5 nil "Global symbol \"$x\" requires explicit package name" error))
      (flycheck-ensure-clear))))

(ert-deftest checker-perl-syntax-error ()
  :expected-result (flycheck-fail-unless-checker 'perl)
  "Test a syntax error with the Perl checker."
  (flycheck-with-resource-buffer "test-perl/syntax-error.pl"
    (dolist (mode '(perl-mode cperl-mode))
      (funcall mode)
      (flycheck-buffer-sync)
      (flycheck-should-errors '(4 nil "syntax error" error))
      (flycheck-ensure-clear))))

(require 'sh-script)

;;; test-perl.el ends here
