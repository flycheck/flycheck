;;; test-error-list.el --- Flycheck Specs: Error List  -*- lexical-binding: t; -*-

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

;; Specs for the error list.

;;; Code:

(require 'flycheck-buttercup)

(defmacro flycheck/with-error-list-buffer (&rest body)
  "Run BODY in a temporary error list buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (delay-mode-hooks (flycheck-error-list-mode))
     (setq delayed-mode-hooks nil)
     ,@body))

(describe "Error List"
  (it "has the correct buffer name"
    (expect flycheck-error-list-buffer :to-equal "*Flycheck errors*"))

  (it "has a permanently local source buffer"
    (flycheck/with-error-list-buffer
      (expect (get 'flycheck-error-list-source-buffer 'permanent-local)
              :to-be-truthy)))

  (it "derives from Tabulated List Mode"
    (flycheck/with-error-list-buffer
      (expect (derived-mode-p 'tabulated-list-mode) :to-be-truthy)))

  (describe "Format"
    (it "sets the error list format locally"
      (flycheck/with-error-list-buffer
        (expect tabulated-list-format :not :to-equal nil)
        (expect 'tabulated-list-format :to-be-local)))

    (it "sets a proper padding locally"
      (flycheck/with-error-list-buffer
        (expect tabulated-list-padding :to-equal 1)
        (expect 'tabulated-list-padding :to-be-local)))

    (it "sets the list entries locally"
      (flycheck/with-error-list-buffer
        (expect tabulated-list-entries :to-equal 'flycheck-error-list-entries)
        (expect 'tabulated-list-entries :to-be-local)))

    (it "has a local header line"
      (flycheck/with-error-list-buffer
        (expect header-line-format
                :to-equal "  Line Col Level ID Message (Checker) ")
        (expect 'header-line-format :to-be-local))))

  (describe "Columns"
    (it "has the line number in the 1st column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 0)
                :to-equal
                '("Line" 5 flycheck-error-list-entry-< :right-align t))))

    (it "has the column number in the 2nd column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 1)
                :to-equal '("Col" 3 nil :right-align t))))

    (it "has the error level in the 3rd column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 2)
                :to-equal '("Level" 8 flycheck-error-list-entry-level-<))))

    (it "has the error ID in the 4th column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 3)
                :to-equal '("ID" 6 t))))

    (it "has the error message in the 5th column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 4)
                :to-equal '("Message (Checker)" 0 t)))))

  (describe "Entry"
    (let* ((warning (flycheck-error-new-at 10 12 'warning "A foo warning"
                                           :checker 'emacs-lisp-checkdoc
                                           :id "W1"))
           (entry (flycheck-error-list-make-entry warning))
           (cells (cadr entry)))

      (it "has the error object as ID"
        (expect (car entry) :to-be warning))

      (it "has the line number in the 1st cell"
        (expect (aref cells 0) :to-equal
                (list "10"
                      'type 'flycheck-error-list
                      'face 'flycheck-error-list-line-number)))

      (it "has the column number in the 2nd cell"
        (expect (aref cells 1) :to-equal
                (list "12"
                      'type 'flycheck-error-list
                      'face 'flycheck-error-list-column-number)))

      (it "has an empty 2nd cell if there is no column number"
        (cl-letf* (((flycheck-error-column warning) nil)
                   (entry (flycheck-error-list-make-entry warning))
                   (cells (cadr entry)))
          (expect (aref cells 1) :to-equal
                  (list ""
                        'type 'flycheck-error-list
                        'face 'flycheck-error-list-column-number))))

      (it "has the error level in the 3rd cell"
        (expect (aref cells 2) :to-equal
                (list "warning"
                      'type 'flycheck-error-list
                      'face (flycheck-error-level-error-list-face 'warning))))

      (it "has the error ID in the 4th cell"
        (expect (aref cells 3) :to-equal
                (list "W1"
                      'type 'flycheck-error-list-explain-error
                      'face 'flycheck-error-list-id
                      'help-echo "W1")))

      (let ((checker-name (propertize "emacs-lisp-checkdoc"
                                      'face 'flycheck-error-list-checker-name)))
        (it "has the error message in the 5th cell"
          (let* ((message (format (propertize "A foo warning (%s)"
                                              'face 'default)
                                  checker-name)))
            (expect (aref cells 4) :to-equal
                    (list message 'type 'flycheck-error-list
                          'help-echo message))))

        (it "has a default message in the 5th cell if there is no message"
          (cl-letf* (((flycheck-error-message warning) nil)
                     (entry (flycheck-error-list-make-entry warning))
                     (cells (cadr entry))
                     (message (format (propertize "Unknown warning (%s)"
                                                  'face 'default)
                                      checker-name)))
            (expect (aref cells 4) :to-equal
                    (list message 'type 'flycheck-error-list
                          'help-echo message)))))))

  (describe "Filter"
    (it "kills the filter variable when resetting the filter"
      (flycheck/with-error-list-buffer
        (setq-local flycheck-error-list-minimum-level 'error)
        (expect 'flycheck-error-list-minimum-level :to-be-local)
        (flycheck-error-list-reset-filter)
        (expect 'flycheck-error-list-minimum-level :not :to-be-local)))

    (it "filters errors with lower levels"
      (let ((flycheck-error-list-minimum-level 'warning)
            (errors (list (flycheck-error-new-at 10 10 'error)
                          (flycheck-error-new-at 20 20 'warning)
                          (flycheck-error-new-at 30 30 'info))))
        (expect (flycheck-error-list-apply-filter errors)
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 10 10 'error)
                      (flycheck-error-new-at 20 20 'warning)))))

    (describe "Mode Line"
      (it "shows no mode line indicator if no filter is set"
        (let ((flycheck-error-list-minimum-level nil))
          (expect (flycheck-error-list-mode-line-filter-indicator)
                  :to-be-empty-string)))

      (it "shows the level filter in the mode line if set"
        (let ((flycheck-error-list-minimum-level 'warning))
          (expect (flycheck-error-list-mode-line-filter-indicator)
                  :to-equal " [>= warning]"))))))

;;; test-error-list.el ends here
