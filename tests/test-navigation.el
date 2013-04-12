;;; test-navigation.el --- Tests for error navigation -*- lexical-binding: t; -*-

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

;; Test error navigation.

;;; Code:

(require 'ert)
(require 'flycheck)

(defun flycheck-test-next-error-function (next-error-function)
   (let (error-data)
      (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        (flycheck-testsuite-buffer-sync)
        (goto-char (point-min))
        (funcall next-error-function)
        (should (= (point) 152))
        (funcall next-error-function)
        (should (= (point) 175))
        (funcall next-error-function)
        (should (= (point) 244))
        (setq error-data (should-error (funcall next-error-function)
                                       :type flycheck-testsuite-user-error-type))
        (should (string= (cadr error-data) "No more Flycheck errors"))
        ;; Now try prefix argument and reset
        (funcall next-error-function 2 t)
        (should (= (point) 175))
        ;; And a negative prefix argument now
        (funcall next-error-function -1)
        (should (= (point) 152))
        (setq error-data (should-error (funcall next-error-function 10)
                                       :type flycheck-testsuite-user-error-type))
        (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-navigate-next-error-compile-mode ()
  "Test navigation to the next error by means of compile mode."
  (flycheck-test-next-error-function #'next-error))

(ert-deftest flycheck-navigate-next-error ()
  "Test navigation to the next error."
  (flycheck-test-next-error-function #'flycheck-next-error))

(defun flycheck-test-previous-error-function (previous-error-function)
  (let (error-data)
    (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
      (emacs-lisp-mode)
      (flycheck-mode)
      (flycheck-testsuite-buffer-sync)
      (goto-char (point-max))
      (funcall previous-error-function)
      (should (= (point) 244))
      (funcall previous-error-function)
      (should (= (point) 175))
      (funcall previous-error-function)
      (should (= (point) 152))
      (setq error-data (should-error (funcall previous-error-function)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors"))
      ;; Now go to buffer end again, and try a prefix arg
      (goto-char (point-max))
      (funcall previous-error-function 2)
      (should (= (point) 175))
      (funcall previous-error-function -1)
      (should (= (point) 244))
      (setq error-data (should-error (funcall previous-error-function 10)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-navigate-previous-error-compile-mode ()
  "Test navigation to the previous error by means of compile mode."
  (flycheck-test-previous-error-function #'previous-error))

(ert-deftest flycheck-navigate-previous-error ()
  "Test navigation to the previous error."
  (flycheck-test-previous-error-function #'flycheck-previous-error))

(defun flycheck-test-first-error-function (first-error-function)
  (let (error-data)
    (flycheck-testsuite-with-resource-buffer "many-errors-for-navigation.el"
      (emacs-lisp-mode)
      (flycheck-mode)
      (flycheck-testsuite-buffer-sync)
      (goto-char (point-max))
      (funcall first-error-function)
      (should (= (point) 152))
      (funcall first-error-function)
      (should (= (point) 152))
      (funcall first-error-function 2)
      (should (= (point) 175))
      (setq error-data (should-error (funcall first-error-function 10)
                                     :type flycheck-testsuite-user-error-type))
      (should (string= (cadr error-data) "No more Flycheck errors")))))

(ert-deftest flycheck-navigate-first-error-compile-mode ()
  "Test navigation to the first error by means of compile mode."
  (flycheck-test-first-error-function #'first-error))

(ert-deftest flycheck-navigate-first-error ()
  "Test navigation to the first error."
  (flycheck-test-first-error-function #'flycheck-first-error))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-navigation.el ends here
