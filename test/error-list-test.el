;;; error-list-test.el --- Tests for the error list  -*- lexical-binding: t; -*-

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

;; Tests for error list.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-error-list-buffer-label ()
  (with-temp-buffer
    (rename-buffer "Foo")
    (should (string= (flycheck-error-list-buffer-label (current-buffer))
                     "#<buffer Foo>")))
  (with-temp-buffer
    (set-visited-file-name (f-join flycheck-testsuite-dir "foo/bar")
                           :no-query)
    (cd flycheck-testsuite-dir)
    (should (string= (flycheck-error-list-buffer-label (current-buffer))
                     "foo/bar"))))

(ert-deftest flycheck-error-list-error-label ()
  (with-temp-buffer
    (rename-buffer "Foo")
    (should (string= (flycheck-error-list-error-label (flycheck-error-new-at 1 1))
                     "#<buffer Foo>")))
  (with-temp-buffer
    (set-visited-file-name (f-join flycheck-testsuite-dir "foo/bar")
                           :no-query)
    (cd flycheck-testsuite-dir)
    (should (string= (flycheck-error-list-error-label (flycheck-error-new-at 1 1))
                     "foo/bar")))
  (with-temp-buffer
    (cd flycheck-testsuite-dir)
    (let* ((filename (f-join flycheck-testsuite-dir "spam/with/eggs"))
           (err (flycheck-error-new-at 1 1 'warning "Foo" :filename filename)))
      (should (string= (flycheck-error-list-error-label err)
                       "spam/with/eggs")))))

;;; error-list-test.el ends here
