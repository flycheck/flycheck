;;; test-mode-line.el --- Flycheck Specs: Mode Line  -*- lexical-binding: t; -*-

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

;; Specs for mode line reporting.

;;; Code:

(require 'flycheck-buttercup)

(defun flycheck/skip-if-noninteractive ()
  "Skip current spec if `noninteractive' is non-nil."
  ;; `format-mode-line' doesn't work in batch mode
  (when noninteractive
    (signal 'buttercup-pending "SKIPPED")))

(describe "Mode Line (interactive-only)"

  (describe "flycheck-count-errors"
    (it "sorts errors by severity"
      (let ((flycheck-current-errors
             (list (flycheck-error-new-at 1 1 'warning "warning")
                   (flycheck-error-new-at 3 1 'info "info")
                   (flycheck-error-new-at 2 1 'error "error")))
            (count (flycheck-count-errors flycheck-current-errors)))
        (expect count :to-equal
                (seq-sort (lambda (l1 l2)
                            (< (flycheck-compare-error-levels
                                (car l1) (car l2))
                               0))
                          count)))))

  (it "shows the number of errors and warnings"
    (flycheck/skip-if-noninteractive)
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'warning "warning 1")
                 (flycheck-error-new-at 1 1 'info "info")
                 (flycheck-error-new-at 2 2 'warning "warning 2")
                 (flycheck-error-new-at 1 1 'error "error"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC[1 2 1]")))

  (it "omits unnecessary levels"
    (flycheck/skip-if-noninteractive)
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'error "error"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC[1]"))
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'warning "warning")
                 (flycheck-error-new-at 1 1 'error "error"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC[1 1]")))

  (it "includes higher-priority levels when lower-priority ones are present"
    (flycheck/skip-if-noninteractive)
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'warning "warning"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC[0 1]"))
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'info "info"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC[0 0 1]")))

  (it "includes the prefix"
    (flycheck/skip-if-noninteractive)
    (let ((flycheck-mode-line-prefix "foobar")
          flycheck-current-errors)
      (expect (flycheck-mode-line-status-text 'finished) :to-equal " foobar"))))

;;; test-mode-line.el ends here
