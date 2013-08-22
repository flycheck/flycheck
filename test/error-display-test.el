;;; error-display-test.el --- Tests for error display  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Keywords:

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

;; Tests for error display

;;; Code:

(require 'test-helper)


;;;; General error display
(ert-deftest flycheck-display-errors-function ()
  (should (eq flycheck-display-errors-function
              #'flycheck-display-error-messages)))

(ert-deftest flycheck-display-errors-no-function ()
  (let ((err (flycheck-error-new-at 10 20 'warning "This is a Flycheck error."))
        (flycheck-display-errors-function nil))
    ;; Error display must not fail with nil
    (with-current-buffer "*Messages*"
      (erase-buffer))
    (flycheck-display-errors (list err))
    (with-current-buffer "*Messages*"
      (should-not (s-contains? (flycheck-error-message err)
                               (buffer-string))))))

(ert-deftest flycheck-display-errors-custom-function ()
  (let ((err (flycheck-error-new-at 10 20 'warning "Foo")))
    (mocker-let
        ((display-function (errors) ((:input `((,err))))))
      (let ((flycheck-display-errors-function 'display-function))
        (flycheck-display-errors (list err))))))



;;;; Error display functions
(ert-deftest flycheck-display-error-messages ()
  (let ((err (flycheck-error-new-at 10 20 'warning
                                    "This is a Flycheck error.")))
    (with-current-buffer "*Messages*"
      (erase-buffer))
    (flycheck-display-error-messages (list err))
    (with-current-buffer "*Messages*"
      (should (s-contains? (flycheck-error-message err) (buffer-string))))))

(ert-deftest flycheck-display-errors-in-list ()
  (unwind-protect
      (flycheck-testsuite-with-resource-buffer "many-errors-for-error-list.el"
        (emacs-lisp-mode)
        (flycheck-testsuite-buffer-sync)

        (flycheck-display-errors-in-list (-take 2 flycheck-current-errors))
        (let ((list-buffer (get-buffer flycheck-error-list-buffer)))
          (should list-buffer)
          ;; The list buffer should not be selected!
          (should-not (eq (current-buffer) list-buffer)))
        (with-current-buffer (flycheck-error-list-buffer)
          (should (eq flycheck-error-list-source-buffer
                      (get-buffer "many-errors-for-error-list.el")))
          (should (looking-at "^*** many-errors-for-error-list\\.el:"))
          (should (string= (buffer-string) (format "

\C-l
*** many-errors-for-error-list.el: Syntax and style errors (Flycheck v%s)
many-errors-for-error-list.el:7:warning: You should have a section marked \";;; Code:\" (emacs-lisp-checkdoc)
many-errors-for-error-list.el:7:1:warning: `message' called with 0
    args to fill 1 format field(s) (emacs-lisp)
" (flycheck-version))))))
    (kill-buffer (flycheck-error-list-buffer))))

;;; error-display-test.el ends here
