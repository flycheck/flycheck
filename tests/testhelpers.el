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
(require 'flycheck)

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

(defvar-local flycheck-syntax-checker-finished nil
  "Non-nil if the current checker has finished.")

(add-hook 'flycheck-after-syntax-check-hook
          (lambda () (setq flycheck-syntax-checker-finished t)))

(defun flycheck-wait-for-syntax-checker ()
  "Wait until the syntax check in the current buffer is finished."
  (while (not flycheck-syntax-checker-finished)
    (sleep-for 1))
  (setq flycheck-syntax-checker-finished nil))

(defun flycheck-should-checker (checkers &rest errors)
  "Test that checking the current buffer with CHECKERS gives
ERRORS."
  (unless (listp checkers)
    (setq checkers (list checkers)))
  (set (make-local-variable 'flycheck-checkers) checkers)
  (setq flycheck-syntax-checker-finished nil)
  (should (not (flycheck-running-p)))
  (should (-all? #'flycheck-may-use-checker checkers))
  (flycheck-mode)
  (flycheck-wait-for-syntax-checker)
  (cond
   ((not errors) (should-not flycheck-current-errors))
   ;; If a single true argument is given, just check that the buffer has any
   ;; errors.
   ((and (= (length errors) 1) (car errors))
    (should flycheck-current-errors))
   (t
    (should (= (length errors) (length flycheck-current-errors)))
    (dolist (err errors)
      (flycheck-should-error err))))
  (flycheck-clear))

(defmacro flycheck-with-resource-buffer (resource-file &rest body)
  "Create a temp buffer from a RESOURCE-FILE and execute BODY."
  (declare (indent 1))
  `(let ((filename (expand-file-name ,resource-file testsuite-dir)))
     (should (file-exists-p filename))
     (with-temp-buffer
       (insert-file-contents filename t)
       (cd (file-name-directory filename))
       ,@body)))

(defun flycheck-fail-unless-checkers (&rest checkers)
  "Skip the test unless all CHECKERS are present on the system."
  (if (-all? 'flycheck-check-executable checkers)
      :passed
    :failed))

(defalias 'flycheck-fail-unless-checker 'flycheck-fail-unless-checkers)

(defun flycheck-travis-ci-p ()
  "Determine whether the tests run on Travis CI.

Return t if so, or nil otherwise.

See URL `http://travis-ci.org'."
  (string= (getenv "TRAVIS") "true"))

(defun flycheck-vagrant-p ()
  "Determine whether the tests run inside a Vagrant VM.

Return t if so, or nil otherwise."
  (string= user-login-name "vagrant"))

(defun flycheck-ci-p ()
  "Determine whether the tests run in a CI environment.

A CI environment is either Travis CI or Vagrant.

Return t if so, or nil otherwise."
  (or (flycheck-vagrant-p) (flycheck-travis-ci-p)))

(defun flycheck-min-emacs-version-p (major &optional minor)
  "Determine whether Emacs has the required version.

Return t if Emacs is at least MAJOR.MINOR, or nil otherwise."
  (when (>= emacs-major-version major)
    (or (null minor) (>= emacs-minor-version minor))))

;;; testhelpers.el ends here
