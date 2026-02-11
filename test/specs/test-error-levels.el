;;; test-error-levels.el --- Flycheck Specs: Error Levels  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Specs for error levels.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Error Levels"

  (describe "flycheck-define-error-level"
    (it "is an error level"
      (expect (flycheck-error-level-p 'test-level) :to-be-truthy))

    (it "has the correct severity"
      (expect (flycheck-error-level-severity 'test-level) :to-equal 1337))

    (it "has the correct fringe bitmap"
      (expect (flycheck-error-level-fringe-bitmap 'test-level)
              :to-be 'left-triangle))

    (it "has the correct fringe face"
      (expect (flycheck-error-level-fringe-face 'test-level)
              :to-be 'highlight))

    (it "has the correct overlay category"
      (expect (flycheck-error-level-overlay-category 'test-level)
              :to-be 'category))

    (it "has the correct error list face"
      (expect (flycheck-error-level-error-list-face 'test-level)
              :to-be 'font-lock-constant-face)))

  (describe "flycheck-error-level-make-indicator"
    (it "has the correct margin spec"
      (pcase-let* ((icon (flycheck-error-level-make-indicator
                          'test-level 'left-margin))
                   (`(_ ,spec) (get-text-property 0 'display icon)))
        (expect spec :to-equal ">>")))

    (it "has the correct fringe bitmap"
      (pcase-let* ((icon (flycheck-error-level-make-indicator
                          'test-level 'left-fringe))
                   (`(_ ,bitmap _) (get-text-property 0 'display icon)))
        (expect bitmap :to-be 'left-triangle)))

    (it "has the correct fringe face"
      (pcase-let* ((icon (flycheck-error-level-make-indicator
                          'test-level 'left-fringe))
                   (`(_ _ ,face) (get-text-property 0 'display icon)))
        (expect face :to-be 'highlight)))

    (it "uses left fringe"
      (pcase-let* ((icon (flycheck-error-level-make-indicator
                          'test-level 'left-fringe))
                   (`(,side _ _) (get-text-property 0 'display icon)))
        (expect side :to-be 'left-fringe)))

    (it "uses right fringe"
      (pcase-let* ((icon (flycheck-error-level-make-indicator
                          'test-level 'right-fringe))
                   (`(,side _ _) (get-text-property 0 'display icon)))
        (expect side :to-be 'right-fringe)))

    (it "uses left margin"
      (pcase-let* ((icon (flycheck-error-level-make-indicator
                          'test-level 'left-margin))
                   (`(,side _ _) (get-text-property 0 'display icon)))
        (expect side :to-equal '(margin left-margin))))

    (it "uses right margin"
      (pcase-let* ((icon (flycheck-error-level-make-indicator
                          'test-level 'right-margin))
                   (`(,side _ _) (get-text-property 0 'display icon)))
        (expect side :to-equal '(margin right-margin))))

    (it "rejects an invalid side"
      (let ((err (should-error (flycheck-error-level-make-indicator
                                'test-level 'up-fringe))))
        (expect (cadr err) :to-equal "Invalid fringe side: up-fringe"))))

  (describe "Built-in error levels"
    (describe "flycheck-error-level-error"
      (it "has the correct severity"
        (expect (flycheck-error-level-severity 'error) :to-equal 100))

      (it "has the correct fringe bitmap"
        (expect (flycheck-error-level-fringe-bitmap 'error)
                :to-be 'flycheck-fringe-bitmap-double-arrow))

      (it "has the correct fringe face"
        (expect (flycheck-error-level-fringe-face 'error)
                :to-be 'flycheck-fringe-error))

      (it "has the correct overlay category"
        (expect (flycheck-error-level-overlay-category 'error)
                :to-be 'flycheck-error-overlay))

      (it "has the correct error list face"
        (expect (flycheck-error-level-error-list-face 'error)
                :to-be 'flycheck-error-list-error)))

    (describe "flycheck-error-level-warning"
      (it "has the correct severity"
        (expect (flycheck-error-level-severity 'warning) :to-equal 10))

      (it "has the correct fringe bitmap"
        (expect (flycheck-error-level-fringe-bitmap 'warning)
                :to-be 'flycheck-fringe-bitmap-double-arrow))

      (it "has the correct fringe face"
        (expect (flycheck-error-level-fringe-face 'warning)
                :to-be 'flycheck-fringe-warning))

      (it "has the correct overlay category"
        (expect (flycheck-error-level-overlay-category 'warning)
                :to-be 'flycheck-warning-overlay))

      (it "has the correct error list face"
        (expect (flycheck-error-level-error-list-face 'warning)
                :to-be 'flycheck-error-list-warning)))

    (describe "flycheck-error-level-info"
      (it "has the correct severity"
        (expect (flycheck-error-level-severity 'info) :to-equal -10))

      (it "has the correct fringe bitmap"
        (expect (flycheck-error-level-fringe-bitmap 'info)
                :to-be 'flycheck-fringe-bitmap-double-arrow))

      (it "has the correct fringe face"
        (expect (flycheck-error-level-fringe-face 'info)
                :to-be 'flycheck-fringe-info))

      (it "has the correct overlay category"
        (expect (flycheck-error-level-overlay-category 'info)
                :to-be 'flycheck-info-overlay))

      (it "has the correct error list face"
        (expect (flycheck-error-level-error-list-face 'info)
                :to-be 'flycheck-error-list-info)))))

;;; test-error-levels.el ends here
