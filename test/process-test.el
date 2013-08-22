;;; process-test.el --- Tests for process management  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for process management.

;;; Code:

(ert-deftest flycheck-chaining-preserves-early-errors ()
  "Test that chaining preserves all errors from all checkers."
  (flycheck-testsuite-should-syntax-check
   "chained-errors.el" 'emacs-lisp-mode nil
   '(8 nil "You should have a section marked \";;; Code:\"" warning
       :checker emacs-lisp-checkdoc)
   '(8 1 "`message' called with 0 args to fill 1\n    format field(s)" warning)
   '(10 2 "princ called with 0 arguments, but\n    requires 1-2" warning)
   '(15 1 "the function `i-do-not-exist' is not\n    known to be defined." warning)))

(require 'test-helper)

;;; process-test.el ends here
