;;; testhelpers.el --- Test helpers for Flycheck

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

;;; Commentary:

;; Provide macros and functions to aid test writing.

;;; Code:

(require 'dash)
(require 's)

(defun flycheck-should-overlay (overlay error)
  "Test that OVERLAY is in REGION and corresponds to ERROR."
  (let* ((region (flycheck-error-region error))
         (text (flycheck-error-text error))
         (level (flycheck-error-level error))
         (face (if (eq level 'warning)
                   'flycheck-warning-face
                 'flycheck-error-face))
         (category (if (eq level 'warning)
                       'flycheck-warning-overlay
                     'flycheck-error-overlay))
         (fringe-icon (if (eq level 'warning)
                          '(left-fringe question-mark flycheck-warning-face)
                        `(left-fringe ,flycheck-fringe-exclamation-mark
                                      flycheck-error-face))))
    (should overlay)
    (should (overlay-get overlay 'flycheck-overlay))
    (should (= (overlay-start overlay) (car region)))
    (should (= (overlay-end overlay) (cdr region)))
    (should (eq (overlay-get overlay 'face) face))
    (should (equal (get-char-property 0 'display
                                      (overlay-get overlay 'before-string))
                   fringe-icon))
    (should (eq (overlay-get overlay 'category) category))
    (should (equal (overlay-get overlay 'flycheck-error) error))
    (should (string= (overlay-get overlay 'help-echo) text))))

(defun flycheck-should-error (expected-err)
  "Test that ERR is an error in the current buffer."
  (let* ((no-filename (nth 4 expected-err))
         (real-error (flycheck-make-error
                      :buffer (current-buffer)
                      :file-name (if no-filename nil (buffer-file-name))
                      :line-no (nth 0 expected-err)
                      :col-no (nth 1 expected-err)
                      :text (nth 2 expected-err)
                      :level (nth 3 expected-err)))
         (overlay (--first (equal (overlay-get it 'flycheck-error) real-error)
                           (flycheck-overlays-at (flycheck-error-pos real-error)))))
    (should (-contains? flycheck-current-errors real-error))
    (flycheck-should-overlay overlay real-error)))

(defun flycheck-should-checker (checker &rest errors)
  "Test that checking the current buffer with CHECKER gives
ERRORS."
  (set (make-local-variable 'flycheck-checkers) (list checker))
  (flycheck-mode)
  (while (flycheck-running-p)
    (sleep-for 1))
  (if (not errors)
      (should-not flycheck-current-errors)
    (dolist (err errors)
      (flycheck-should-error err))))

(defmacro flycheck-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY."
  (declare (indent 1))
  `(let ((filename (expand-file-name ,resource-file testsuite-dir)))
     (should (file-exists-p filename))
     (with-temp-buffer
       (insert-file-contents filename t)
       ,@body)))

(defun flycheck-fail-unless-checker (checker)
  "Skip the test unless CHECKER is present on the system."
  (if (executable-find (flycheck-checker-executable checker))
      :passed
    :failed))

;;; testhelpers.el ends here
