;;; test-overlays.el --- Flycheck Specs: Overlays  -*- lexical-binding: t; -*-

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

;; Specs for error overlays in the current buffer.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Overlays"

  (describe "Overlay priorities"
    (it "has the correct info overlay priority"
      (expect (get 'flycheck-info-overlay 'priority) :to-equal 90))

    (it "has the correct warning overlay priority"
      (expect (get 'flycheck-warning-overlay 'priority) :to-equal 100))

    (it "has the correct error overlay priority"
      (expect (get 'flycheck-error-overlay 'priority) :to-equal 110)))

  (describe "Overlay faces"
    (it "has the correct info overlay face"
      (expect (get 'flycheck-info-overlay 'face) :to-be 'flycheck-info))

    (it "has the correct warning overlay face"
      (expect (get 'flycheck-warning-overlay 'face) :to-be 'flycheck-warning))

    (it "has the correct error overlay face"
      (expect (get 'flycheck-error-overlay 'face) :to-be 'flycheck-error)))

  (describe "flycheck-add-overlay"
    (it "errors on undefined error level"
      (let ((err (should-error (flycheck-add-overlay
                                (flycheck-error-new-at 1 1 'foo)))))
        (expect (cadr err) :to-equal "Undefined error level: foo")))

    (it "errors on no error level"
      (let ((err (should-error (flycheck-add-overlay
                                (flycheck-error-new-at 1 1)))))
        (expect (cadr err) :to-equal "Undefined error level: nil")))

    (it "adds an info category"
      (flycheck-buttercup-with-temp-buffer
        (insert "Foo")
        (let ((overlay (flycheck-add-overlay
                        (flycheck-error-new-at 1 1 'info))))
          (expect (overlay-get overlay 'category)
                  :to-be 'flycheck-info-overlay))))

    (it "adds a warning category"
      (flycheck-buttercup-with-temp-buffer
        (insert "Foo")
        (let ((overlay (flycheck-add-overlay
                        (flycheck-error-new-at 1 1 'warning))))
          (expect (overlay-get overlay 'category)
                  :to-be 'flycheck-warning-overlay))))

    (it "adds an error category"
      (flycheck-buttercup-with-temp-buffer
        (insert "Foo")
        (let ((overlay (flycheck-add-overlay
                        (flycheck-error-new-at 1 1 'error))))
          (expect (overlay-get overlay 'category)
                  :to-be 'flycheck-error-overlay))))

    (it "has help echo"
      (flycheck-buttercup-with-temp-buffer
        (let ((overlay (flycheck-add-overlay
                        (flycheck-error-new-at 1 1 'info "A bar message"))))
          (expect (overlay-get overlay 'help-echo)
                  :to-be #'flycheck-help-echo))))

    (it "has the flycheck overlay property"
      (flycheck-buttercup-with-temp-buffer
        (insert "Foo bar")
        (let* ((err (flycheck-error-new-at 1 1 'error))
               (overlay (flycheck-add-overlay err)))
          (expect (overlay-get overlay 'flycheck-overlay) :to-be-truthy))))

    (it "has the flycheck error property"
      (flycheck-buttercup-with-temp-buffer
        (insert "Foo bar")
        (let* ((err (flycheck-error-new-at 1 1 'warning))
               (overlay (flycheck-add-overlay err)))
          (expect (overlay-get overlay 'flycheck-error) :to-be err))))

    (it "has no fringe icon with disabled indication"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World")
        (let ((flycheck-indication-mode nil))
          (dolist (level '(warning info error))
            (let ((overlay (flycheck-add-overlay
                            (flycheck-error-new-at 1 1 level))))
              (expect (overlay-get overlay 'before-string)
                      :not :to-be-truthy))))))

    (it "has an info fringe icon"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World")
        (pcase-let* ((overlay (flycheck-add-overlay
                               (flycheck-error-new-at 1 1 'info)))
                     (before-string (overlay-get overlay 'before-string))
                     (`(_ ,bitmap ,face)
                      (get-text-property 0 'display before-string)))
          (expect face :to-be 'flycheck-fringe-info)
          (expect bitmap :to-be 'flycheck-fringe-bitmap-double-arrow))))

    (it "has a warning fringe icon"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World")
        (pcase-let* ((overlay (flycheck-add-overlay
                               (flycheck-error-new-at 1 1 'warning)))
                     (before-string (overlay-get overlay 'before-string))
                     (`(_ ,bitmap ,face)
                      (get-text-property 0 'display before-string)))
          (expect face :to-be 'flycheck-fringe-warning)
          (expect bitmap :to-be 'flycheck-fringe-bitmap-double-arrow))))

    (it "has an error fringe icon"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World")
        (pcase-let* ((overlay (flycheck-add-overlay
                               (flycheck-error-new-at 1 1 'error)))
                     (before-string (overlay-get overlay 'before-string))
                     (`(_ ,bitmap ,face)
                      (get-text-property 0 'display before-string)))
          (expect face :to-be 'flycheck-fringe-error)
          (expect bitmap :to-be 'flycheck-fringe-bitmap-double-arrow))))

    (it "has a left fringe icon"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World")
        (let ((flycheck-indication-mode 'left-fringe))
          (pcase-let* ((overlay (flycheck-add-overlay
                                 (flycheck-error-new-at 1 1 'error)))
                       (before-string (overlay-get overlay 'before-string))
                       (`(,side _ _)
                        (get-text-property 0 'display before-string)))
            (expect side :to-be 'left-fringe)))))

    (it "has a right fringe icon"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World")
        (let ((flycheck-indication-mode 'right-fringe))
          (pcase-let* ((overlay (flycheck-add-overlay
                                 (flycheck-error-new-at 1 1 'error)))
                       (before-string (overlay-get overlay 'before-string))
                       (`(,side _ _)
                        (get-text-property 0 'display before-string)))
            (expect side :to-be 'right-fringe)))))

    (it "positions overlays correctly in a narrowed buffer"
      (flycheck-buttercup-with-resource-buffer "narrowing.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        ;; Narrow to the function and check the buffer
        (re-search-forward "(defun .*")
        (forward-line 1)
        (narrow-to-defun)
        (expect (buffer-narrowed-p) :to-be-truthy)
        (flycheck-buttercup-buffer-sync)
        ;; We should have two errors highlighted between point min and max now
        (expect (length (flycheck-overlays-in (point-min) (point-max)))
                :to-equal 2)
        ;; Remove restrictions and test that all errors are reported
        (widen)
        (expect (length (flycheck-overlays-in (point-min) (point-max)))
                :to-equal (if (< emacs-major-version 30) 4 5)))))

  (describe "Error message overlays"
    (it "shows the error message as help echo"
      (flycheck-buttercup-with-temp-buffer
        (insert " ")
        (goto-char 1)
        (flycheck-add-overlay
         (flycheck-error-new-at 1 1 'info "A bar message"))
        (expect (help-at-pt-string) :to-equal "A bar message")))

    (it "can suppress help echo"
      (flycheck-buttercup-with-temp-buffer
        (insert " ")
        (goto-char 1)
        (flycheck-add-overlay
         (flycheck-error-new-at 1 1 'info "info"))
        (let ((flycheck-help-echo-function nil))
          (expect (help-at-pt-string) :to-equal nil))))

    (it "shows a default message for nil error messages"
      (flycheck-buttercup-with-temp-buffer
        (insert " ")
        (goto-char 1)
        (flycheck-add-overlay (flycheck-error-new-at 1 1 'info))
        (expect (help-at-pt-string) :to-equal "Unknown info")))

    (it "stacks error messages"
      (flycheck-buttercup-with-temp-buffer
        (insert " ")
        (goto-char 1)
        (flycheck-add-overlay
         (flycheck-error-new-at 1 1 'info "info"))
        (flycheck-add-overlay
         (flycheck-error-new-at 1 1 'warning "warning"))
        (flycheck-add-overlay
         (flycheck-error-new-at 1 1 'error "error"))
        (expect (help-at-pt-string) :to-equal "info\nwarning\nerror")))

    (it "stacks error messages with region snippets"
      (flycheck-buttercup-with-temp-buffer
        (insert "int main() {}")
        (goto-char 5)
        (flycheck-add-overlay
         (flycheck-error-new-at 1 1 'info "info" :end-column 14))
        (flycheck-add-overlay
         (flycheck-error-new-at 1 5 'warning "warning" :end-column 9))
        (flycheck-add-overlay
         (flycheck-error-new-at 1 5 'error "error" :end-column 11))
        (let ((text-quoting-style 'grave))
          (expect (help-at-pt-string)
                  :to-equal
                  "`\N{FIRST STRONG ISOLATE}int main() {}\N{POP DIRECTIONAL ISOLATE}': info\n`\N{FIRST STRONG ISOLATE}main\N{POP DIRECTIONAL ISOLATE}': warning\n`\N{FIRST STRONG ISOLATE}main()\N{POP DIRECTIONAL ISOLATE}': error"))))))

;;; test-overlays.el ends here
