;;; run.el --- Flycheck: Test runner                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Flycheck test runner.
;;
;; Read an ERT selector expression from the command line. With no selector, run
;; all tests.
;;
;; Use from the top-level source directory:
;;
;; emacs -Q --batch -l init.el -l ../flycheck.elc -l flycheck-test.el -l run.el \
;;     -f flycheck-run-tests-batch-and-exit -- '(tag language-emacs-lisp)'
;;
;; Or use the Makefile:
;;
;; make ERTSELECTOR='(tag language-emacs-lisp)' test
;;
;; See `ert-select-tests' for selector expressions.

;;; Code:

(require 'thingatpt)                    ; For `read-from-whole-string'

(defun flycheck-run-tests-batch-and-exit ()
  "Run test cases matching tags in `argv' and exit."
  (when (string= (car argv) "--")
    ;; Skip over the command line argument separator
    (pop argv))
  (let ((selector (car argv)))
    (when (cdr argv)
      (message "WARNING: Unused trailing arguments: %S" (cdr argv)))
    ;; Reset argv after processing, to prevent Emacs from processing the options
    ;; on its own.
    (setq argv nil)
    ;; Parse the selector and run the matching tests
    (setq selector (if (and selector (> (length selector) 0))
                       (read-from-whole-string selector)
                     t))
    (ert-run-tests-batch-and-exit selector)))

;;; run.el ends here
