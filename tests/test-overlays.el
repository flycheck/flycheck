;;; test-overlays.el --- Tests for overlays -*- lexical-binding: t; -*-

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

;; Test overlays for errors.

;;; Code:

(require 'ert)
(require 's)
(require 'flycheck)

(ert-deftest flycheck-overlay-categories ()
  (--each '(flycheck-error-overlay flycheck-warning-overlay)
    (should (get it 'flycheck-overlay)))
  (should (= (get 'flycheck-error-overlay 'priority) 110))
  (should (= (get 'flycheck-warning-overlay 'priority) 100))
  (should (eq (get 'flycheck-error-overlay 'face) 'flycheck-error))
  (should (eq (get 'flycheck-warning-overlay 'face) 'flycheck-warning))
  (should (eq (get 'flycheck-error-overlay 'flycheck-fringe-face)
              'flycheck-fringe-error))
  (should (eq (get 'flycheck-warning-overlay 'flycheck-fringe-face)
              'flycheck-fringe-warning))
  (should (eq (get 'flycheck-error-overlay 'flycheck-fringe-bitmap)
              flycheck-fringe-exclamation-mark))
  (should (symbolp flycheck-fringe-exclamation-mark))
  (should (eq (get 'flycheck-warning-overlay 'flycheck-fringe-bitmap)
              'question-mark))
  (should (string= (get 'flycheck-error-overlay 'help-echo)
                   "Unknown error."))
  (should (string= (get 'flycheck-warning-overlay 'help-echo)
                   "Unknown warning.")))

(ert-deftest flycheck-add-overlay-category ()
  (with-temp-buffer
    (insert "Foo")
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'warning))))
      (should (eq (overlay-get overlay 'category) 'flycheck-warning-overlay)))
    (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 'error))))
      (should (eq (overlay-get overlay 'category) 'flycheck-error-overlay)))
    (let ((err (should-error (flycheck-add-overlay (flycheck-error-new-at 1 1 'foo)))))
      (should (string= (cadr err) "Invalid error level foo")))
    (let ((err (should-error (flycheck-add-overlay (flycheck-error-new-at 1 1)))))
      (should (string= (cadr err) "Invalid error level nil")))))

(ert-deftest flycheck-add-overlay-help-echo ()
  (with-temp-buffer
    (--each '(error warning)
      (let ((overlay (flycheck-add-overlay
                      (flycheck-error-new-at 1 1 it "A bar message"))))
        (should (string= (overlay-get overlay 'help-echo) "A bar message"))))))

(ert-deftest flycheck-add-overlay-flycheck-error ()
  (with-temp-buffer
    (insert "Foo bar")
    (let* ((err (flycheck-error-new-at 1 1 'warning))
           (overlay (flycheck-add-overlay err)))
      (should (eq (overlay-get overlay 'flycheck-error) err)))))

(ert-deftest flycheck-add-overlay-fringe-icon ()
  (with-temp-buffer
    (insert "Hello\n    World")
    (let ((flycheck-indication-mode nil))
      (--each '(error warning)
        (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 it))))
          (should-not (overlay-get overlay 'before-string)))))
    ;; An unknown indication mode should cause no error indication
    (let ((flycheck-indication-mode 'foo))
      (--each '(error warning)
        (let ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 it))))
          (should-not (overlay-get overlay 'before-string)))))
    ;; Test the bitmap and face of fringe icons
    (--each '(warning error)
      (pcase-let* ((overlay (flycheck-add-overlay (flycheck-error-new-at 1 1 it)))
                   (category (overlay-get overlay 'category))
                   (before-string (overlay-get overlay 'before-string))
                   (`(_ ,bitmap ,face) (get-text-property 0 'display before-string)))
        (should (eq face (get category 'flycheck-fringe-face)))
        (should (eq bitmap (get category 'flycheck-fringe-bitmap)))))
    ;; Test the various indication modes
    (--each '(left-fringe right-fringe)
      (let ((flycheck-indication-mode it))
        (pcase-let* ((err (flycheck-error-new-at 1 1 'error))
                     (overlay (flycheck-add-overlay err))
                     (before-string (overlay-get overlay 'before-string))
                     (`(,side _ _) (get-text-property 0 'display before-string)))
          (should (eq side it)))))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-overlays.el ends here
