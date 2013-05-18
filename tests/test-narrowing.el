;;; test-narrowing.el --- Tests for narrowing compatibility -*- lexical-binding: t; -*-

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

;;; Commentary

;; Test compatibility with narrowing.

;;; Code:

(require 'ert)
(require 'flycheck)

(ert-deftest flycheck-add-overlays-with-narrowing ()
  "Test that all overlays are added at the right positions with narrowing in place."
  (flycheck-testsuite-with-resource-buffer "narrowing.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    ;; Narrow to the function and check the buffer
    (re-search-forward "(defun .*")
    (forward-line 1)
    (narrow-to-defun)
    (should (buffer-narrowed-p))
    (flycheck-testsuite-buffer-sync)
    ;; We should have two errors highlighted between point min and max now
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 2))
    ;; Remove restrictions and test that all errors are reported
    (widen)
    (should (= (length (flycheck-overlays-in (point-min) (point-max))) 4))
    (flycheck-testsuite-should-errors
     '(9 1 "`message' called with 0 args to fill 1\n    format field(s)" warning)
     '(11 8 "`message' called with 0 args to fill 1\n    format field(s)" warning)
     '(12 nil "First sentence should end with punctuation" warning
          :checker emacs-lisp-checkdoc)
     '(15 1 "`message' called with 0 args to fill 1\n    format field(s)" warning))))

(ert-deftest flycheck-navigate-does-not-cross-narrowing ()
  "Test that error navigation does not cross restrictions"
  (flycheck-testsuite-with-resource-buffer "narrowing.el"
    (emacs-lisp-mode)
    (flycheck-mode)
    (re-search-forward "(defun .*")
    (forward-line 1)
    (narrow-to-defun 1)
    (should (buffer-narrowed-p))
    (flycheck-testsuite-buffer-sync)
    (flycheck-next-error 1 :reset)
    (should (= (point) 166))
    (flycheck-next-error)
    (should (= (point) 198))
    (should-error (flycheck-next-error)
                  :type flycheck-testsuite-user-error-type)
    (flycheck-previous-error)
    (should (= (point) 166))
    (should-error (flycheck-previous-error)
                  :type flycheck-testsuite-user-error-type)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-narrowing.el ends here
