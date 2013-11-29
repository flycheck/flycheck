;;; navigation-test.el --- Tests for error navigation  -*- lexical-binding: t; -*-

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

;; Tests for error navigation.

;;; Code:

(require 'test-helper)

(require 'cl-lib)
(require 'mocker)

(defmacro flycheck-testsuite-with-nav-buffer (&rest body)
  (declare (indent 0))
  `(flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp.el"
     (emacs-lisp-mode)
     (flycheck-mode)
     (flycheck-testsuite-buffer-sync)
     (goto-char (point-min))
     ,@body))

(defun flycheck-testsuite-at-nth-error (n)
  (let* ((error (nth (1- n) flycheck-current-errors))
         (mode flycheck-highlighting-mode)
         (region (flycheck-error-region-for-mode error mode)))
    (and (member error (flycheck-overlay-errors-at (point)))
         (= (point) (car region)))))

(defun flycheck-testsuite-explain--at-nth-error (n)
  (let ((errors (flycheck-overlay-errors-at (point))))
    (if (null errors)
        (format "Expected to be at error %s, but no error at point %s"
                n (point))
      (let ((pos (cl-position (car errors) flycheck-current-errors)))
        (format "Expected to be at error %s, but point %s is at error %s"
                n (point) (1+ pos))))))

(put 'flycheck-testsuite-at-nth-error 'ert-explainer
     'flycheck-testsuite-explain--at-nth-error)

(ert-deftest flycheck-next-error/goes-to-first-error ()
  (flycheck-testsuite-with-nav-buffer
    (flycheck-next-error)
    (should (flycheck-testsuite-at-nth-error 1))))

(ert-deftest flycheck-next-error/goes-to-next-error ()
  (flycheck-testsuite-with-nav-buffer
    (flycheck-next-error)
    (flycheck-next-error)
    (should (flycheck-testsuite-at-nth-error 2))))

(ert-deftest flycheck-next-error/errors-beyond-last-error ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/errors-when-moving-too-far ()
  (flycheck-testsuite-with-nav-buffer
    (let ((err (should-error (flycheck-next-error 4)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error/navigate-by-two-errors ()
  (flycheck-testsuite-with-nav-buffer
    (flycheck-next-error 2)
    (should (flycheck-testsuite-at-nth-error 2))))

(ert-deftest flycheck-next-error/navigate-back-by-two-errors ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (flycheck-next-error -2)
    (should (flycheck-testsuite-at-nth-error 1))))

(ert-deftest flycheck-next-error/reset-navigates-to-first-error ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (flycheck-next-error 1 'reset)
    (should (flycheck-testsuite-at-nth-error 1))))

(ert-deftest flycheck-next-error/does-not-cross-narrowing ()
  (flycheck-testsuite-with-nav-buffer
    (re-search-forward "(defun .*")
    (narrow-to-defun)
    (goto-char (point-min))
    (flycheck-next-error)
    (should (flycheck-testsuite-at-nth-error 1))
    (let ((err (should-error (flycheck-next-error)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/errors-before-first-error ()
  (flycheck-testsuite-with-nav-buffer
    (let ((err (should-error (flycheck-previous-error)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error/goes-to-last-error ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (flycheck-previous-error)
    (should (flycheck-testsuite-at-nth-error 2))))

(ert-deftest flycheck-previous-error/navigate-by-two-errors ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (flycheck-previous-error 2)
    (should (flycheck-testsuite-at-nth-error 1))))

(ert-deftest flycheck-previous-error/navigate-back-by-two-errors ()
  (flycheck-testsuite-with-nav-buffer
    (flycheck-previous-error -2)
    (should (flycheck-testsuite-at-nth-error 2))))

(ert-deftest flycheck-previous-errors/errors-when-moving-too-far ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (let ((err (should-error (flycheck-previous-error 4)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error/goes-to-first-error ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error)
    (should (flycheck-testsuite-at-nth-error 1))))

(ert-deftest flycheck-first-error/stays-at-first-error-if-called-again ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error)
    (flycheck-first-error)
    (should (flycheck-testsuite-at-nth-error 1))))

(ert-deftest flycheck-first-error/goes-to-second-error ()
  (flycheck-testsuite-with-nav-buffer
    (goto-char (point-max))
    (flycheck-first-error 2)
    (should (flycheck-testsuite-at-nth-error 2))))

(ert-deftest next-error/calls-flycheck-next-error-function ()
  (flycheck-testsuite-with-nav-buffer
    (mocker-let
     ((flycheck-next-error-function (n reset)
                                    ((:input '(1 nil))
                                     (:input '(2 nil))
                                     (:input '(2 reset)))))
     (goto-char (point-min))
     (next-error)
     (next-error 2)
     (next-error 2 'reset))))

;;; navigation-test.el ends here
