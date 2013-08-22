;;; option-filters-test.el --- Tests for option filters -*- lexical-binding: t; -*-

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

;; Tests for option filters.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-option-int ()
  (should (null (flycheck-option-int nil)))
  (should (equal (flycheck-option-int 10) "10")))

(ert-deftest flycheck-option-comma-separated-list ()
  (should (null (flycheck-option-comma-separated-list nil)))
  (should (null (flycheck-option-comma-separated-list '(nil))))
  (should (null (flycheck-option-comma-separated-list '(10 20) nil (lambda (_x) nil))))
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar"))
                 "foo,bar"))
  (should (equal (flycheck-option-comma-separated-list '("foo" "bar") ":")
                 "foo:bar"))
  (should (equal (flycheck-option-comma-separated-list '(10 20) nil #'number-to-string)
                 "10,20")))

;;; option-filters-test.el ends here
