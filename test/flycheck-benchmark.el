;;; flycheck-benchmark.el --- How long Flycheck's hot paths take  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

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

;; Times the paths a keystroke or a finished check pays for: overlay
;; creation, error-list rendering, the inline-annotation rebuild, and
;; the post-command guard that decides whether to rebuild at all.  Run
;; with `make bench'.
;;
;; The numbers mean nothing across machines and everything across
;; branches: run on master, run on the branch, compare.  A timing
;; assertion in CI would flake, so nothing here asserts.

;;; Code:

(require 'flycheck)

(defconst flycheck-benchmark-lines 2000
  "Lines in the benchmark buffer.")

(defconst flycheck-benchmark-errors 1000
  "Errors spread over the benchmark buffer, one every other line.")

(defun flycheck-benchmark--buffer ()
  "Return a fresh benchmark buffer with errors on every other line."
  (let ((buffer (generate-new-buffer "*flycheck-benchmark*")))
    (with-current-buffer buffer
      (dotimes (i flycheck-benchmark-lines)
        (insert (format "line %d of unremarkable code\n" i)))
      (setq-local flycheck-mode t)
      (setq flycheck-current-errors
            (let (errors)
              (dotimes (i flycheck-benchmark-errors)
                (push (flycheck-error-new-at
                       (1+ (* 2 i)) 6 (if (zerop (mod i 10)) 'error 'warning)
                       (format "benchmark error %d with a message of usual length" i)
                       :id (format "B%03d" (mod i 100))
                       :checker 'emacs-lisp
                       :buffer buffer)
                      errors))
              (nreverse errors))))
    buffer))

(defmacro flycheck-benchmark--report (label repetitions &rest body)
  "Time BODY run REPETITIONS times and print it under LABEL."
  (declare (indent 2))
  `(let ((timing (benchmark-run ,repetitions ,@body)))
     (message "  %-42s %8.2f ms  (%d gcs)"
              ,label (/ (* 1000 (car timing)) ,repetitions) (nth 1 timing))))

(defun flycheck-benchmark-run ()
  "Run the benchmarks and print one line per path."
  (message "%d lines, %d errors; per-iteration times:"
           flycheck-benchmark-lines flycheck-benchmark-errors)
  ;; Overlay creation, what publishing a finished check pays
  (with-current-buffer (flycheck-benchmark--buffer)
    (flycheck-benchmark--report "add-overlay for every error" 5
      (mapc #'flycheck-add-overlay flycheck-current-errors)
      (flycheck-delete-all-overlays)))
  ;; The error list's rows, what the list pays to (re)display
  (with-current-buffer (flycheck-benchmark--buffer)
    (let ((flycheck-error-list-source-buffer (current-buffer)))
      (flycheck-benchmark--report "error-list entries" 5
        (flycheck-error-list-entries))))
  ;; The inline-annotation rebuild, what a command that moves onto an
  ;; annotated line pays with the mode on
  (with-current-buffer (flycheck-benchmark--buffer)
    (set-window-buffer (selected-window) (current-buffer))
    (mapc #'flycheck-add-overlay flycheck-current-errors)
    (goto-char (point-min))
    (let ((flycheck-annotate-mode t))
      (flycheck-benchmark--report "annotate refresh of the visible region" 20
        (flycheck-annotate--refresh)))
    ;; The guard for a command that moved point between two clean lines,
    ;; which is the skip the guard exists to make cheap; ping-pong so
    ;; every iteration really crosses
    (let ((flycheck-annotate-mode t)
          (here (point-min))
          (there (save-excursion (goto-char (point-min))
                                 (forward-line 2) (point))))
      (flycheck-annotate--refresh)
      (flycheck-benchmark--report "post-command guard, clean-line crossing" 200
        (goto-char (if (eql (point) here) there here))
        (flycheck-annotate--post-command)))))

(defun flycheck-benchmark-batch ()
  "Entry point for `make bench'."
  (flycheck-benchmark-run))

(provide 'flycheck-benchmark)
;;; flycheck-benchmark.el ends here
