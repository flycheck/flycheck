;;; error-api-test.el --- Tests for Error API        -*- lexical-binding: t; -*-

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

;; Tests for the error API.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-error-line-region ()
  (with-temp-buffer
    (insert "Hello\n    World\n")
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 1 1))
                   '(1 . 6)))
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 2 4))
                   '(11 . 16)))
    ;; An error column beyond the end of the line is simply ignored just like
    ;; all other error columns
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 2 10))
                   '(11 . 16)))
    ;; An error line beyond the end of file should highlight the last line
    (should (equal (flycheck-error-line-region (flycheck-error-new-at 4 3))
                   '(16 . 17)))))

(ert-deftest flycheck-error-column-region ()
  (with-temp-buffer
    (insert "Hello\n    World\n")
    (should-not (flycheck-error-column-region (flycheck-error-new-at 1 nil)))
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 1 4))
                   '(4 . 5)))
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 2 6))
                   '(12 . 13)))
    ;; A column beyond the end of a line
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 1 7))
                   '(6 . 7)))
    ;; A column right at the end of the last empty line of a file (an important
    ;; special case, because the Emacs Lisp checker reports undefined functions
    ;; at this place!)
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 3 1))
                   '(16 . 17)))
    ;; A column beyond the end of file
    (should (equal (flycheck-error-column-region (flycheck-error-new-at 4 2))
                   '(16 . 17)))))

(ert-deftest flycheck-error-thing-region ()
  (with-temp-buffer
    (insert "    (message)\n    (message")
    (emacs-lisp-mode)
    (should-not (flycheck-error-thing-region 'sexp (flycheck-error-new-at 1 2)))
    (should (equal (flycheck-error-thing-region 'sexp (flycheck-error-new-at 1 5))
                   '(5 . 14)))
    (should-not (flycheck-error-thing-region 'symbol (flycheck-error-new-at 1 5)))
    (should (equal (flycheck-error-thing-region 'sexp (flycheck-error-new-at 1 8))
                   '(6 . 13)))
    (should (equal (flycheck-error-thing-region 'symbol (flycheck-error-new-at 1 8))
                   '(6 . 13)))
    ;; An incomplete expression
    (should-not (flycheck-error-thing-region 'sexp (flycheck-error-new-at 2 5)))))

(ert-deftest flycheck-error-region-for-mode ()
  (with-temp-buffer
    (insert "    (message) ;; Hello world\n    (message")
    (emacs-lisp-mode)
    ;; Test an expression at the error column for all modes
    (let ((err (flycheck-error-new-at 1 7)))
      (should (equal (flycheck-error-region-for-mode err 'lines) '(5 . 29)))
      (should (equal (flycheck-error-region-for-mode err 'columns) '(7 . 8)))
      (should (equal (flycheck-error-region-for-mode err 'symbols) '(6 . 13)))
      (should (equal (flycheck-error-region-for-mode err 'sexps) '(6 . 13))))
    ;; Test an error column which does not point to an expression
    (let ((err (flycheck-error-new-at 2 5)))
      (should (equal (flycheck-error-region-for-mode err 'lines) '(34 . 42)))
      (--each '(columns symbols sexps)
        (should (equal (flycheck-error-region-for-mode err it) '(34 . 35)))))
    ;; Test an error without column for all modes
    (let ((err (flycheck-error-new-at 1 nil)))
      (--each '(lines columns symbols sexps)
        (should (equal (flycheck-error-region-for-mode err it) '(5 . 29)))))))

(ert-deftest flycheck-error-pos ()
  (with-temp-buffer
    (insert "    Hello\n   World\n")
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 1)) 1))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 4)) 4))
    (should (= (flycheck-error-pos (flycheck-error-new-at 1 nil)) 5))
    (should (= (flycheck-error-pos (flycheck-error-new-at 2 nil)) 14))
    (should (= (flycheck-error-pos (flycheck-error-new-at 3 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 1)) 19))
    (should (= (flycheck-error-pos (flycheck-error-new-at 4 nil)) 19))))

(ert-deftest flycheck-error-format ()
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 3 5 'warning "Hello world"
                                           :checker 'emacs-lisp))
                   "3:5:warning: Hello world (emacs-lisp)"))
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 20 7 'error "Spam with eggs"
                                           :checker 'ruby))
                   "20:7:error: Spam with eggs (ruby)"))
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 nil 'warning "Oh no"
                                           :checker 'python-flake8))
                   "14:warning: Oh no (python-flake8)"))
  ;; Specific test for https://github.com/magnars/s.el/issues/34
  (should (string= (flycheck-error-format
                    (flycheck-error-new-at 14 15 'error "dash\\nbroken"
                                           :checker 'foo))
                   "14:15:error: dash\\nbroken (foo)")))

;;; error-api-test.el ends here
