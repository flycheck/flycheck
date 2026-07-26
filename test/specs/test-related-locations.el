;;; test-related-locations.el --- Flycheck Specs: related locations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

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

;; Specs for navigating an error's secondary source locations (its
;; `flycheck-error-relations'): formatting, jumping and the walk that
;; `flycheck-next-related-location' drives.

;;; Code:

(require 'flycheck-buttercup)
(require 'xref)

(defun test-related/setup ()
  "Fill the buffer and put an error with two related locations on line 1.

Point is left at the start of the error, so `flycheck-overlay-errors-at'
finds it."
  (insert "line one here\nline two here\nline three ok\n")
  (setq-local flycheck-mode t)
  (let ((err (flycheck-error-new-at
              1 1 'error "redefined"
              :buffer (current-buffer) :checker 'emacs-lisp
              :relations
              (list (flycheck-related-location-new
                     :line 2 :column 6 :message "first here")
                    (flycheck-related-location-new
                     :line 3 :column 1 :message "then here")))))
    (setq flycheck-current-errors (list err))
    (mapc #'flycheck-add-overlay flycheck-current-errors)
    (goto-char (point-min))
    err))

(describe "Related locations"

  (describe "flycheck-related-location-format"
    (it "combines message and file position"
      (expect (flycheck-related-location-format
               (flycheck-related-location-new
                :filename "/tmp/other.el" :line 2 :column 4
                :message "first here"))
              :to-equal "first here (other.el:2:4)"))
    (it "omits the file when there is none"
      (expect (flycheck-related-location-format
               (flycheck-related-location-new
                :line 7 :column 3 :message "here"))
              :to-equal "here (7:3)"))
    (it "is just the message when there is no position"
      (expect (flycheck-related-location-format
               (flycheck-related-location-new :message "here"))
              :to-equal "here")))

  (describe "flycheck-related-location-at-point"
    (it "flattens the relations of the errors at point"
      (flycheck-buttercup-with-temp-buffer
        (test-related/setup)
        (let ((locs (flycheck-related-location-at-point)))
          (expect (length locs) :to-equal 2)
          (expect (flycheck-related-location-message (car locs))
                  :to-equal "first here"))))
    (it "is nil away from any error"
      (flycheck-buttercup-with-temp-buffer
        (test-related/setup)
        (goto-char (point-max))
        (expect (flycheck-related-location-at-point) :to-be nil))))

  (describe "flycheck-goto-related-location"
    (it "moves point to the location's line and column"
      (flycheck-buttercup-with-temp-buffer
        (test-related/setup)
        (flycheck-goto-related-location
         (flycheck-related-location-new :line 2 :column 6))
        (expect (flycheck-line-column-at-point) :to-equal '(2 . 6))))
    (it "pushes onto the xref marker stack so the jump can be reverted"
      (flycheck-buttercup-with-temp-buffer
        (test-related/setup)
        (spy-on 'xref-push-marker-stack)
        (flycheck-goto-related-location
         (flycheck-related-location-new :line 3 :column 1))
        (expect 'xref-push-marker-stack :to-have-been-called))))

  (describe "flycheck-visit-related-location"
    (it "signals a user-error with no related location at point"
      (flycheck-buttercup-with-temp-buffer
        (insert "nothing here\n")
        (setq-local flycheck-mode t)
        (expect (flycheck-visit-related-location) :to-throw 'user-error)))
    (it "jumps straight to the sole location without prompting"
      (flycheck-buttercup-with-temp-buffer
        (insert "line one here\nline two here\n")
        (setq-local flycheck-mode t)
        (let ((err (flycheck-error-new-at
                    1 1 'error "x" :buffer (current-buffer) :checker 'emacs-lisp
                    :relations (list (flycheck-related-location-new
                                      :line 2 :column 6 :message "only")))))
          (setq flycheck-current-errors (list err))
          (mapc #'flycheck-add-overlay flycheck-current-errors)
          (goto-char (point-min)))
        (spy-on 'flycheck-completing-read)
        (flycheck-visit-related-location)
        (expect 'flycheck-completing-read :not :to-have-been-called)
        (expect (flycheck-line-column-at-point) :to-equal '(2 . 6))))
    (it "prompts and jumps to the chosen location when there are several"
      (flycheck-buttercup-with-temp-buffer
        (test-related/setup)
        (spy-on 'flycheck-completing-read
                :and-return-value "then here (3:1)")
        (flycheck-visit-related-location)
        (expect (flycheck-line-column-at-point) :to-equal '(3 . 1)))))

  (describe "flycheck-next-related-location / -previous"
    (it "starts from the locations at point and then cycles"
      (flycheck-buttercup-with-temp-buffer
        (test-related/setup)
        ;; First step lands on the first related location.
        (flycheck-next-related-location)
        (expect (flycheck-line-column-at-point) :to-equal '(2 . 6))
        ;; Continuing the walk advances to the second.
        (let ((last-command 'flycheck-next-related-location))
          (flycheck-next-related-location))
        (expect (flycheck-line-column-at-point) :to-equal '(3 . 1))
        ;; And wraps back around to the first.
        (let ((last-command 'flycheck-next-related-location))
          (flycheck-next-related-location))
        (expect (flycheck-line-column-at-point) :to-equal '(2 . 6))))
    (it "steps backward with -previous"
      (flycheck-buttercup-with-temp-buffer
        (test-related/setup)
        (flycheck-previous-related-location)
        ;; -1 from the fresh index of -1 wraps to the last location.
        (expect (flycheck-line-column-at-point) :to-equal '(3 . 1))))
    (it "signals a user-error with nothing at point"
      (flycheck-buttercup-with-temp-buffer
        (insert "nothing\n")
        (setq-local flycheck-mode t)
        (expect (flycheck-next-related-location) :to-throw 'user-error))))

  (describe "flycheck-error-format-relations"
    (it "is nil when the error has no related locations"
      (expect (flycheck-error-format-relations
               (flycheck-error-new-at 1 1 'error "x"))
              :to-be nil))
    (it "renders each related location as a button that visits it"
      (let* ((loc (flycheck-related-location-new
                   :line 2 :column 6 :message "first here"))
             (err (flycheck-error-new-at 1 1 'error "x" :relations (list loc)))
             (text (flycheck-error-format-relations err)))
        (expect (substring-no-properties text) :to-match "↳ first here (2:6)")
        ;; The formatted text is a button pointing back at the location.
        (expect (get-text-property (- (length text) 1)
                                   'flycheck-related-location text)
                :to-be loc))))

  (describe "display of related locations"
    (it "appends the related locations to the help-echo message"
      (let* ((loc (flycheck-related-location-new
                   :line 2 :column 6 :message "first here"))
             (err (flycheck-error-new-at 1 1 'error "redefined"
                                         :relations (list loc)))
             (msg (flycheck-help-echo-all-error-messages (list err))))
        (expect (substring-no-properties msg) :to-match "redefined")
        (expect (substring-no-properties msg) :to-match "↳ first here (2:6)")))
    (it "shows the related locations through eldoc"
      (flycheck-buttercup-with-temp-buffer
        (insert "line one here\nline two here\n")
        (flycheck-mode)
        (goto-char (point-min))
        (flycheck-add-overlay
         (flycheck-error-new-at
          1 1 'error "redefined" :end-column 5
          :relations (list (flycheck-related-location-new
                            :line 2 :column 6 :message "first here"))))
        (let (doc)
          (flycheck-eldoc-function (lambda (string &rest _) (setq doc string)))
          (expect (substring-no-properties doc) :to-match "error: redefined")
          (expect (substring-no-properties doc)
                  :to-match "↳ first here (2:6)"))))))

;;; test-related-locations.el ends here
