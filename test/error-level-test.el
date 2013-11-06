;;; error-level-test.el --- Test for error levels -*- lexical-binding: t; -*-

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

;; Tests for error levels

;;; Code:

(require 'flycheck)
(require 'test-helper)

;; A level for the following unit tests
(flycheck-define-error-level 'test-level
    :overlay-category 'category
    :fringe-bitmap 'left-triangle
    :fringe-face 'highlight)

(ert-deftest flycheck-define-error-level ()
  (should (flycheck-error-level-p 'test-level))
  (should (eq (flycheck-error-level-fringe-bitmap 'test-level) 'left-triangle))
  (should (eq (flycheck-error-level-fringe-face 'test-level) 'highlight))
  (should (eq (flycheck-error-level-overlay-category 'test-level) 'category)))

(ert-deftest flycheck-error-level-make-fringe-icon ()
  (--each '(left-fringe right-fringe)
    (pcase-let* ((icon (flycheck-error-level-make-fringe-icon 'test-level it))
                 (`(,side ,bitmap ,face) (get-text-property 0 'display icon)))
      (should (eq side it))
      (should (eq bitmap 'left-triangle))
      (should (eq face 'highlight)))))


;; Test the builtin levels
(ert-deftest flycheck-error-level-error ()
  (should (eq (flycheck-error-level-fringe-bitmap 'error)
              flycheck-fringe-exclamation-mark))
  (should (eq (flycheck-error-level-fringe-face 'error)
              'flycheck-fringe-error))
  (should (eq (flycheck-error-level-overlay-category 'error)
              'flycheck-error-overlay)))

(ert-deftest flycheck-error-level-warning ()
  (should (eq (flycheck-error-level-fringe-bitmap 'warning) 'question-mark))
  (should (eq (flycheck-error-level-fringe-face 'warning)
              'flycheck-fringe-warning))
  (should (eq (flycheck-error-level-overlay-category 'warning)
              'flycheck-warning-overlay)))

(ert-deftest flycheck-error-level-info ()
  (should (eq (flycheck-error-level-fringe-bitmap 'info) 'empty-line))
  (should (eq (flycheck-error-level-fringe-face 'info)
              'flycheck-fringe-info))
  (should (eq (flycheck-error-level-overlay-category 'info)
              'flycheck-info-overlay)))

;;; error-level-test.el ends here
