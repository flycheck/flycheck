;;; test-tidy.el --- Test the Tidy checker -*- lexical-binding: t; -*-

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

(ert-deftest checker-tidy-missing-tag ()
  "Test a warning caused by a missing tag."
  :expected-result (flycheck-testsuite-fail-unless-checker 'html-tidy)
  (flycheck-testsuite-should-syntax-check
   "missing-tag.html" 'html-mode nil
   '(3 1 "missing <!DOCTYPE> declaration" warning :no-filename)
   '(5 5 "missing </title> before </head>" warning :no-filename)))

(ert-deftest checker-tidy-unknown-tag ()
  "Test an error caused by an unknown tag."
  :expected-result (flycheck-testsuite-fail-unless-checker 'html-tidy)
  (flycheck-testsuite-should-syntax-check
   "unknown-tag.html" 'html-mode nil
   '(3 1 "missing <!DOCTYPE> declaration" warning :no-filename)
   '(8 5 "<spam> is not recognized!" error :no-filename)
   '(8 5 "discarding unexpected <spam>" warning :no-filename)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-tidy.el ends here
