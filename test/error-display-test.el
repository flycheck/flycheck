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
(ert-deftest flycheck-display-errors/no-display-function-set ()
  (let ((err (flycheck-error-new-at 10 20 'warning "This is a Flycheck error."))
        (flycheck-display-errors-function nil))
    ;; Error display must not fail with nil
    (with-current-buffer "*Messages*"
      (erase-buffer))
    (flycheck-display-errors (list err))
    (with-current-buffer "*Messages*"
      (should-not (s-contains? (flycheck-error-message err)
                               (buffer-string))))))

(ert-deftest flycheck-display-errors/custom-function ()
  (let* ((err (flycheck-error-new-at 10 20 'warning "Foo"))
         (displayed-errors nil)
         (flycheck-display-errors-function (lambda (errors)
                                             (--each errors
                                               (push it displayed-errors)))))
    (flycheck-display-errors (list err))
    (should (equal displayed-errors (list err)))))



;;;; Error display functions
(ert-deftest flycheck-display-error-messages ()
  (let ((err (flycheck-error-new-at 10 20 'warning
                                    "This is a Flycheck error.")))
    (with-current-buffer "*Messages*"
      (erase-buffer))
    (flycheck-display-error-messages (list err))
    (with-current-buffer "*Messages*"
      (should (s-contains? (flycheck-error-message err) (buffer-string))))))

;;; error-display-test.el ends here
