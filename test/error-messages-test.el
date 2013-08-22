;;; error-messages-test.el --- Tests for error message functions  -*- lexical-binding: t; -*-

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

;; Tests for error message functions.

;;; Code:

(require 'test-helper)

(require 'mocker)
(require 'cl)                           ; Mocker wants `letf'

(ert-deftest flycheck-copy-messages-as-kill ()
  (with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (-each errors #'flycheck-add-overlay)
      (mocker-let
          ((message (errors) ((:input '("1st message\n2nd message")))))
        (let ((flycheck-display-errors-function 'display-function))
          (flycheck-copy-messages-as-kill 10))))
    (should (equal (-take 2 kill-ring) '("1st message" "2nd message")))))

(ert-deftest flycheck-google-messages ()
  (with-temp-buffer
    (insert "A test buffer to copy errors from")
    (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
          (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                        (flycheck-error-new-at 1 10 'warning "2nd message"))))
      (-each errors #'flycheck-add-overlay)
      (let ((err (should-error (flycheck-google-messages 10)
                               :type flycheck-testsuite-user-error-type)))
        (should (string= (cadr err) "Please install Google This from https://github.com/Bruce-Connor/emacs-google-this")))

      (mocker-let
          ((google-string (quote-flag s confirm)
                          ((:input '(nil "1st message" :no-confirm))
                           (:input '(nil "2nd message" :no-confirm))
                           (:input '(:quote "1st message" :no-confirm))
                           (:input '(:quote "2nd message" :no-confirm)))))
        (let* ((flycheck-google-max-messages 1)
               (err (should-error (flycheck-google-messages 10)
                                  :type flycheck-testsuite-user-error-type)))
          (should (string= (cadr err) "More than 1 messages at point")))
        (flycheck-google-messages 10)
        (flycheck-google-messages 10 :quote)))))

;;; error-messages-test.el ends here
