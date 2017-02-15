;;; flycheck-inline-.el --- Display Flycheck errors inline -*- lexical-binding: t; -*-

;; Copyright (C) 2017 fmdkdd

;; Author: fmdkdd
;; URL: https://github.com/flycheck/flycheck-inline
;; Keywords: tools, convenience
;; Version: 0.1-cvs
;; Package-Requires: ((emacs "25.1") (flycheck "31"))

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

;; Provide an error display function to show Flycheck errors inline, directly
;; below their location in the buffer.

;;;; Setup

;; (with-eval-after-load 'flycheck
;;   (flycheck-inline-mode))

;;; Code:

(require 'flycheck)

;;; Displaying line-long overlays (phantoms)
(defun phantom-display (msg &optional pos)
  "Display MSG in a phantom directly below POS.

MSG is a string that will be put in a line-long overlay (phantom)
at the line immediately following POS.  If POS is nil, current
point is used instead.

Return the displayed phantom."
  (pcase-let* ((p (or pos (point)))
               (`(,offset . ,pos-eol)
                (save-excursion
                  (goto-char p)
                  (cons (- p (point-at-bol)) (point-at-eol))))
               (ov (make-overlay pos-eol (1+ pos-eol)))
               (str (concat (make-string offset ?\s) msg "\n")))
    (overlay-put ov 'phantom t)
    (overlay-put ov 'after-string str)
    ov))

(defun phantom-delete (phantom)
  "Delete PHANTOM."
  (when (overlay-get phantom 'phantom)
    (delete-overlay phantom)))


;;; flycheck-inline mode proper
(defgroup flycheck-inline nil
  "Display Flycheck errors inline."
  :prefix "flycheck-inline-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-inline"))

(defface flycheck-inline-error
  '((t :inherit compilation-error))
  "Flycheck-inline face for errors."
  :package-version '(flycheck-inline . "0.1")
  :group 'flycheck-inline)

(defface flycheck-inline-warning
  '((t :inherit compilation-warning))
  "Flycheck-inline face for warnings."
  :package-version '(flycheck-inline . "0.1")
  :group 'flycheck-inline)

(defface flycheck-inline-info
  '((t :inherit compilation-info))
  "Flycheck-inline face for informational messages."
  :package-version '(flycheck-inline . "0.1")
  :group 'flycheck-inline)

(defvar-local flycheck-inline--phantoms nil
  "Remember which phantoms were added to the buffer.")

(defun flycheck-inline--display-phantom (err)
  "Display `flycheck-error' ERR in a phantom."
  (let* ((pos (car (flycheck-error-region-for-mode err 'columns)))
         (msg (propertize
               (flycheck-error-message err)
               'face (pcase (flycheck-error-level err)
                       (`info 'flycheck-inline-info)
                       (`warning 'flycheck-inline-warning)
                       (`error 'flycheck-inline-error)))))
    (push (phantom-display msg pos) flycheck-inline--phantoms)))

(defun flycheck-inline-display-messages (errors)
  "Display ERRORS, and all errors from their groups, inline.

ERRORS is a list of `flycheck-error' objects."
  (flycheck-inline-hide-messages)
  (mapc #'flycheck-inline--display-phantom
        (seq-uniq
         (seq-mapcat #'flycheck-errors-from-group errors))))

(defun flycheck-inline-hide-messages ()
  "Hide all inline messages currently being shown."
  (when (not flycheck-display-error-caused-by-next-error)
    (mapc #'phantom-delete flycheck-inline--phantoms)
    (setq flycheck-inline--phantoms nil)))

(defvar flycheck-inline-old-display-function nil
  "The former value of `flycheck-display-errors-function'.")

;;;###autoload
(define-minor-mode flycheck-inline-mode
  "A minor mode to show Flycheck error messages line.

When called interactively, toggle `flycheck-inline-mode'.  With
prefix ARG, enable `flycheck-inline-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `flycheck-inline-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`flycheck-inline-mode'.  Otherwise behave as if called
interactively.

In `flycheck-inline-mode', show Flycheck error messages inline,
directly below the error reported location."
  :global t
  :group 'flycheck
  (let ((hooks '(post-command-hook)))
    (cond
     ;; Use our display function and remember the old one but only if we haven't
     ;; yet configured it, to avoid activating twice.
     ((and flycheck-inline-mode
           (not (eq flycheck-display-errors-function
                    #'flycheck-inline-display-messages)))
      (setq flycheck-inline-old-display-function
            flycheck-display-errors-function
            flycheck-display-errors-function
            #'flycheck-inline-display-messages)
      (dolist (hook hooks)
        (add-hook hook #'flycheck-inline-hide-messages)))
     ;; Reset the display function and remove ourselves from all hooks but only
     ;; if the mode is still active.
     ((and (not flycheck-inline-mode)
           (eq flycheck-display-errors-function
               #'flycheck-inline-display-messages))
      (setq flycheck-display-errors-function
            flycheck-inline-old-display-function
            flycheck-inline-old-display-function nil)
      (flycheck-inline-hide-messages)
      (dolist (hook hooks)
        (remove-hook hook 'flycheck-inline-hide-messages))))))

(provide 'flycheck-inline)

;;; flycheck-inline.el ends here
