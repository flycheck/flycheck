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

(require 'compile)                      ; For `compilation-next-error'

(defun flycheck-test-next-error-function (next-error-fn)
   (let (error-data)
      (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        (flycheck-testsuite-buffer-sync)
        (goto-char (point-min))
        (funcall next-error-fn)
        (should (= (point) 152))
        (funcall next-error-fn)
        (should (= (point) 175))
        (funcall next-error-fn)
        (should (= (point) 240))
        (setq error-data (should-error (funcall next-error-fn)
                                       :type flycheck-testsuite-user-error-type))
        (should (string= (cadr error-data) "No more Flycheck errors"))
        ;; Now try prefix argument and reset
        (funcall next-error-fn 2 t)
        (should (= (point) 175))
        ;; And a negative prefix argument now
        (funcall next-error-fn -1)
        (should (= (point) 152))
        (setq error-data (should-error (funcall next-error-fn 10)
                                       :type flycheck-testsuite-user-error-type))
        (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-next-error-compile-mode ()
  "Test navigation to the next error by means of compile mode."
  (flycheck-test-next-error-function #'next-error))

(ert-deftest flycheck-next-error ()
  "Test navigation to the next error."
  (flycheck-test-next-error-function #'flycheck-next-error))

(defun flycheck-test-previous-error-function (previous-error-fn)
  (let (error-data)
    (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
      (emacs-lisp-mode)
      (flycheck-mode)
      (flycheck-testsuite-buffer-sync)
      (goto-char (point-max))
      (funcall previous-error-fn)
      (should (= (point) 240))
      (funcall previous-error-fn)
      (should (= (point) 175))
      (funcall previous-error-fn)
      (should (= (point) 152))
      (setq error-data (should-error (funcall previous-error-fn)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors"))
      ;; Now go to buffer end again, and try a prefix arg
      (goto-char (point-max))
      (funcall previous-error-fn 2)
      (should (= (point) 175))
      (funcall previous-error-fn -1)
      (should (= (point) 240))
      (setq error-data (should-error (funcall previous-error-fn 10)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-previous-error-compile-mode ()
  "Test navigation to the previous error by means of compile mode."
  (flycheck-test-previous-error-function #'previous-error))

(ert-deftest flycheck-previous-error ()
  "Test navigation to the previous error."
  (flycheck-test-previous-error-function #'flycheck-previous-error))

(defun flycheck-test-first-error-function (first-error-fn)
  (let (error-data)
    (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
      (emacs-lisp-mode)
      (flycheck-mode)
      (flycheck-testsuite-buffer-sync)
      (goto-char (point-max))
      (funcall first-error-fn)
      (should (= (point) 152))
      (funcall first-error-fn)
      (should (= (point) 152))
      (funcall first-error-fn 2)
      (should (= (point) 175))
      (setq error-data (should-error (funcall first-error-fn 10)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-first-error-compile-mode ()
  "Test navigation to the first error by means of compile mode."
  (flycheck-test-first-error-function #'first-error))

(ert-deftest flycheck-first-error ()
  "Test navigation to the first error."
  (flycheck-test-first-error-function #'flycheck-first-error))

(ert-deftest flycheck-next-error-does-not-cross-narrowing ()
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

;;; navigation-test.el ends here
