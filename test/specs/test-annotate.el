;;; test-annotate.el --- Flycheck Specs: Inline display  -*- lexical-binding: t; -*-

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

;; Specs for the inline display of errors (`flycheck-annotate-mode').

;;; Code:

(require 'flycheck-buttercup)

(defun test-annotate/setup ()
  "Fill the current buffer with four lines and errors on lines 2 and 4.

Line 2 gets an error and a warning; line 4 gets a warning."
  (insert "line one here\nline two here\nline three ok\nline four here\n")
  (setq-local flycheck-mode t)
  (let ((e2 (flycheck-error-new-at 2 6 'error "bad thing" :id "E1"
                                   :buffer (current-buffer) :checker 'emacs-lisp))
        (e2b (flycheck-error-new-at 2 1 'warning "also warn"
                                    :buffer (current-buffer) :checker 'emacs-lisp))
        (e4 (flycheck-error-new-at 4 3 'warning "watch out"
                                   :buffer (current-buffer) :checker 'emacs-lisp)))
    (setq flycheck-current-errors (list e2 e2b e4))
    (mapc #'flycheck-add-overlay flycheck-current-errors)))

(defun test-annotate/anchors ()
  "Return an alist mapping each inline overlay's line to its text."
  (mapcar (lambda (ov)
            (cons (line-number-at-pos (overlay-start ov))
                  (substring-no-properties (overlay-get ov 'after-string))))
          flycheck-annotate--overlays))

(describe "Inline display"

  (describe "the default format function"
    (it "renders the message and the id"
      (expect (substring-no-properties
               (funcall flycheck-annotate-format-function
                        (flycheck-error-new-at 1 1 'error "boom" :id "X9")))
              :to-match "boom")
      (expect (substring-no-properties
               (funcall flycheck-annotate-format-function
                        (flycheck-error-new-at 1 1 'error "boom" :id "X9")))
              :to-match "X9")))

  (describe "flycheck-annotate--level-face"
    (it "maps the built-in levels to the inline faces"
      (expect (flycheck-annotate--level-face 'error) :to-be 'flycheck-annotate-error)
      (expect (flycheck-annotate--level-face 'warning) :to-be 'flycheck-annotate-warning)
      (expect (flycheck-annotate--level-face 'info) :to-be 'flycheck-annotate-info)))

  (describe "flycheck-annotate--filter-levels"
    (it "keeps every error when set to t"
      (let ((flycheck-annotate-levels t)
            (errs (list (flycheck-error-new-at 1 1 'error)
                        (flycheck-error-new-at 1 1 'warning))))
        (expect (flycheck-annotate--filter-levels errs) :to-equal errs)))
    (it "keeps only errors of the listed levels"
      (let* ((flycheck-annotate-levels '(error))
             (err (flycheck-error-new-at 1 1 'error))
             (warn (flycheck-error-new-at 1 1 'warning)))
        (expect (flycheck-annotate--filter-levels (list err warn))
                :to-equal (list err)))))

  (describe "flycheck-annotate-eol-style"
    (it "shows the most severe message and a count of the rest"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\n")
        (let* ((errs (list (flycheck-error-new-at 1 1 'error "big"
                                                  :checker 'emacs-lisp)
                           (flycheck-error-new-at 1 2 'warning "small"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-eol-style errs (line-end-position) nil))
               (s (substring-no-properties (overlay-get ov 'after-string))))
          (expect s :to-match "big")
          (expect s :to-match (regexp-quote "(+1)"))
          (expect (string-prefix-p "\n" s) :to-be nil)))))

  (describe "flycheck-annotate-below-style"
    (it "stacks each message on its own line under the code"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\n")
        (let* ((errs (list (flycheck-error-new-at 1 3 'error "one"
                                                  :checker 'emacs-lisp)
                           (flycheck-error-new-at 1 1 'warning "two"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs (line-end-position) t))
               (s (substring-no-properties (overlay-get ov 'after-string))))
          (expect (string-prefix-p "\n" s) :to-be t)
          (expect s :to-match "one")
          (expect s :to-match "two")
          ;; leading newline plus one line per error
          (expect (length (split-string s "\n")) :to-equal 3)))))

  (describe "flycheck-annotate--make-overlay"
    (it "tags and tracks the overlay"
      (flycheck-buttercup-with-temp-buffer
        (insert "abc\n")
        (let* ((flycheck-annotate--overlays nil)
               (ov (flycheck-annotate--make-overlay (point-min) 'after-string "x")))
          (expect (overlay-get ov 'flycheck-annotate) :to-be t)
          (expect (memq ov flycheck-annotate--overlays) :not :to-be nil)))))

  (describe "flycheck-annotate--suppresses-echo-p"
    (it "suppresses the echo message for an error rendered inline"
      (flycheck-buttercup-with-temp-buffer
        (test-annotate/setup)
        (goto-char (point-min))
        (forward-line 3)                ; line 4 warning at column 3
        (forward-char 2)                ; onto the warning
        (let ((flycheck-annotate-mode t)
              (flycheck-annotate-suppress-echo t)
              (flycheck-annotate-current-line-style 'below)
              (flycheck-annotate-levels t))
          (expect (flycheck-annotate--suppresses-echo-p) :to-be t))))
    (it "is nil when the current line is not annotated"
      (let ((flycheck-annotate-mode t)
            (flycheck-annotate-suppress-echo t)
            (flycheck-annotate-current-line-style nil))
        (expect (flycheck-annotate--suppresses-echo-p) :to-be nil)))
    (it "is nil when suppression is disabled"
      (let ((flycheck-annotate-mode t)
            (flycheck-annotate-suppress-echo nil)
            (flycheck-annotate-current-line-style 'below))
        (expect (flycheck-annotate--suppresses-echo-p) :to-be nil)))
    (it "does not suppress an error the inline display would filter out"
      (flycheck-buttercup-with-temp-buffer
        (test-annotate/setup)
        (goto-char (point-min))
        (forward-line 3)                ; line 4 has a warning at column 3
        (forward-char 2)                ; onto the warning
        (let ((flycheck-annotate-mode t)
              (flycheck-annotate-suppress-echo t)
              (flycheck-annotate-current-line-style 'below)
              (flycheck-annotate-levels '(error)))
          ;; the only error at point is a warning, excluded by the level
          ;; filter, so it must still reach the echo area
          (expect (flycheck-annotate--suppresses-echo-p) :to-be nil)))))

  (describe "the two-tier layout"
    (it "uses the below style on the line at point and eol elsewhere"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (goto-char (point-min))
          (forward-line 1)              ; line 2
          (flycheck-annotate-mode 1)
          (let ((anchors (test-annotate/anchors)))
            ;; line 2 is focused -> below (after-string begins with a newline)
            (expect (string-prefix-p "\n" (cdr (assq 2 anchors))) :to-be t)
            ;; line 4 is unfocused -> eol
            (expect (string-prefix-p "\n" (cdr (assq 4 anchors))) :to-be nil)))))

    (it "swaps the styles as point moves between lines"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (goto-char (point-min))
          (forward-line 3)              ; line 4
          (flycheck-annotate-mode 1)
          (let ((anchors (test-annotate/anchors)))
            (expect (string-prefix-p "\n" (cdr (assq 4 anchors))) :to-be t)
            (expect (string-prefix-p "\n" (cdr (assq 2 anchors))) :to-be nil))))))

  (describe "flycheck-annotate-mode"
    (it "clears the overlays when disabled"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (flycheck-annotate-mode 1)
          (expect flycheck-annotate--overlays :not :to-be nil)
          (flycheck-annotate-mode -1)
          (expect flycheck-annotate--overlays :to-be nil))))

    (it "drops the overlays on a manual clear"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (flycheck-annotate-mode 1)
          (expect flycheck-annotate--overlays :not :to-be nil)
          (flycheck-clear)
          (expect flycheck-annotate--overlays :to-be nil))))))

;;; test-annotate.el ends here
