;;; test-rst.el --- Test the RST checker -*- lexical-binding: t; -*-

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
(require 'flycheck)

(require 'rst)

(ert-deftest checker-rst-warning-underline-too-short ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "underline-too-short.rst" 'rst-mode nil
   '(8 nil "Title underline too short." warning)
   '(11 nil "Title underline too short." warning)))

(ert-deftest checker-rst-error-unknown-target-name ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "unknown-target-name.rst" 'rst-mode nil
   '(5 nil "Unknown target name: \"restructuredtext\"." error)
   '(7 nil "Unknown target name: \"cool\"." error)))

(ert-deftest checker-rst-severe-unexpected-section-title ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "unexpected-section-title.rst" 'rst-mode nil
   '(6 nil "Unexpected section title." error)
   '(11 nil "Unexpected section title." error)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-rst.el ends here
