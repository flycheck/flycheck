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

(describe "Mode Line"
  (it "shows the number of errors, warnings, and infos"
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'warning "warning 1")
                 (flycheck-error-new-at 2 2 'warning "warning 2")
                 (flycheck-error-new-at 1 1 'error "error"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC:1|2|0")))

  (it "shows the number of infos"
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'info "info"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC:0|0|1")))

  (it "includes the prefix"
    (let ((flycheck-mode-line-prefix "foobar")
          flycheck-current-errors)
      (expect (flycheck-mode-line-status-text 'finished) :to-equal " foobar:0")))

  (it "makes the error counts clickable"
    (let* ((flycheck-current-errors
            (list (flycheck-error-new-at 1 1 'error "boom")))
           (text (flycheck-mode-line-status-text 'finished))
           (counts-start (string-match ":" text)))
      (expect (get-text-property counts-start 'local-map text)
              :to-be flycheck-mode-line-counts-map)
      (expect (get-text-property counts-start 'help-echo text)
              :to-equal "mouse-1: list errors")))

  (it "binds mouse-1 to the error list"
    (expect (lookup-key flycheck-mode-line-counts-map [mode-line mouse-1])
            :to-be 'flycheck-mode-line-list-errors)
    (expect (lookup-key flycheck-mode-line-counts-map [header-line mouse-1])
            :to-be 'flycheck-mode-line-list-errors))

  (it "explains suppressed errors in the tooltip of the whole indicator"
    (flycheck-buttercup-with-temp-buffer
      (setq flycheck--suppressed-error-count 7)
      (let* ((flycheck-current-errors
              (list (flycheck-error-new-at 1 1 'error "boom")))
             (text (flycheck-mode-line-status-text 'finished))
             (counts-start (string-match ":" text))
             (plus-pos (string-match (rx "+" eos) text)))
        ;; The truncation marker is part of the clickable segment
        (expect (get-text-property plus-pos 'local-map text)
                :to-be flycheck-mode-line-counts-map)
        ;; The tooltip explains the suppression anywhere on the counts
        (dolist (pos (list counts-start plus-pos))
          (expect (get-text-property pos 'help-echo text)
                  :to-match "7 more errors not shown"))))))

;;; test-mode-line.el ends here
