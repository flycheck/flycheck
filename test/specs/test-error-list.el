;;; test-error-list.el --- Flycheck Specs: Error List  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016, 2021 Sebastian Wiesner and Flycheck contributors

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
  `(with-current-buffer (get-buffer-create flycheck-error-list-buffer)
     (delay-mode-hooks (flycheck-error-list-mode))
     (setq delayed-mode-hooks nil)
     (prog1 (progn ,@body)
       (kill-buffer flycheck-error-list-buffer))))

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
        (expect header-line-format :to-be-truthy)
        (expect 'header-line-format :to-be-local))))

  (describe "Columns"
    (it "has the file name in the 1st column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 0)
                :to-equal
                '("File" 12))))

    (it "has the line number in the 2nd column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 1)
                :to-equal
                '("Line" 5 flycheck-error-list-entry-< :right-align t))))

    (it "has the column number in the 3rd column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 2)
                :to-equal '("Col" 3 nil :right-align t))))

    (it "has the error level in the 4th column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 3)
                :to-equal '("Level" 8 flycheck-error-list-entry-level-<))))

    (it "has the error ID in the 5th column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 4)
                :to-equal '("ID" 6 t))))

    (it "has the error message in the 6th column"
      (flycheck/with-error-list-buffer
        (expect (aref tabulated-list-format 5)
                :to-equal '("Message (Checker)" 0 t)))))

  (describe "Entry"
    (let* ((warning (flycheck-error-new-at 10 12 'warning "A foo warning"
                                           :checker 'emacs-lisp-checkdoc
                                           :id "W1"))
           (entry (flycheck-error-list-make-entry warning))
           (cells (cadr entry)))

      (it "has the error object as ID"
        (expect (car entry) :to-be warning))

      (it "has the line number in the 2nd cell"
        (expect (aref cells 1) :to-equal
                (list "10"
                      'type 'flycheck-error-list
                      'face 'flycheck-error-list-line-number)))

      (it "has the column number in the 3rd cell"
        (expect (aref cells 2) :to-equal
                (list "12"
                      'type 'flycheck-error-list
                      'face 'flycheck-error-list-column-number)))

      (it "has an empty 3rd cell if there is no column number"
        (cl-letf* (((flycheck-error-column warning) nil)
                   (entry (flycheck-error-list-make-entry warning))
                   (cells (cadr entry)))
          (expect (aref cells 2) :to-equal
                  (list ""
                        'type 'flycheck-error-list
                        'face 'flycheck-error-list-column-number))))

      (it "has the error level in the 4th cell"
        (expect (aref cells 3) :to-equal
                (list "warning"
                      'type 'flycheck-error-list
                      'face (flycheck-error-level-error-list-face 'warning))))

      (it "has the error ID in the 5th cell"
        (expect (aref cells 4) :to-equal
                (list "W1"
                      'type 'flycheck-error-list-explain-error
                      'face 'flycheck-error-list-id
                      'help-echo "W1")))

      (let ((checker-name (propertize "emacs-lisp-checkdoc"
                                      'face 'flycheck-error-list-checker-name)))
        (it "has the error message in the 6th cell"
          (let* ((message (format (propertize "A foo warning (%s)"
                                              'face 'default)
                                  checker-name)))
            (expect (aref cells 5) :to-equal
                    (list message 'type 'flycheck-error-list
                          'help-echo message))))

        (it "has a default message in the 6th cell if there is no message"
          (cl-letf* (((flycheck-error-message warning) nil)
                     (entry (flycheck-error-list-make-entry warning))
                     (cells (cadr entry))
                     (message (format (propertize "Unknown warning (%s)"
                                                  'face 'default)
                                      checker-name)))
            (expect (aref cells 5) :to-equal
                    (list message 'type 'flycheck-error-list
                          'help-echo message)))))))

  (describe "Grouping by file"
    (let ((errors (list (flycheck-error-new-at 3 1 'error "in b"
                                               :filename "/p/b.el" :checker 'x)
                        (flycheck-error-new-at 8 1 'warning "second in a"
                                               :filename "/p/a.el" :checker 'x)
                        (flycheck-error-new-at 1 1 'error "first in a"
                                               :filename "/p/a.el" :checker 'x)))
          source)

      (before-each
        (setq source (generate-new-buffer " grouping-source"))
        (with-current-buffer source (setq default-directory "/p/")))
      (after-each (kill-buffer source))

      (it "lays out a header per file with its errors sorted under it"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (let ((entries (flycheck-error-list-entries)))
              ;; header a.el, its two errors (lines 1 then 8), header b.el, its
              ;; one error -- files and errors both in order.
              (expect (mapcar (lambda (e)
                                (if (flycheck-error-p (car e))
                                    (flycheck-error-line (car e))
                                  (car (aref (cadr e) 0))))
                              entries)
                      :to-equal '("a.el (2)" 1 8 "b.el (1)" 3))))))

      (it "blanks the file cell under a header"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (let ((error-row (nth 1 (flycheck-error-list-entries))))
              (expect (car (aref (cadr error-row) 0)) :to-equal "")))))

      (it "toggles grouping and the sort key together"
        (flycheck/with-error-list-buffer
          (expect flycheck-error-list-group-by :to-be nil)
          (flycheck-error-list-toggle-grouping)
          (expect flycheck-error-list-group-by :to-be 'file)
          (expect tabulated-list-sort-key :to-be nil)
          (flycheck-error-list-toggle-grouping)
          (expect flycheck-error-list-group-by :to-be nil)
          (expect tabulated-list-sort-key :to-equal '("Line"))))

      (it "sorts headers as equal instead of crashing on them"
        (let ((header (list (list 'flycheck-group "a.el") (make-vector 6 "")))
              (err (list (car errors) (make-vector 6 ""))))
          ;; A header entry carries a list id, not a `flycheck-error', so the
          ;; column sorters must not pass it to `flycheck-error-<' and friends.
          (expect (flycheck-error-list-entry-< header err) :to-be nil)
          (expect (flycheck-error-list-entry-< err header) :to-be nil)
          (expect (flycheck-error-list-entry-level-< header err) :to-be nil)
          (expect (flycheck-error-list-entry-level-< err header) :to-be nil)))

      (describe "next-error navigation"
        (before-each
          (spy-on 'flycheck-error-list-goto-error))

        (defun flycheck/error-list-print-grouped (source errors)
          "Render ERRORS grouped by file and move point to the first error."
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file
                tabulated-list-sort-key nil)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (setq tabulated-list-entries (flycheck-error-list-entries)))
          (tabulated-list-print)
          ;; The first row is the a.el header; step onto its first error.
          (goto-char (point-min))
          (forward-line 1))

        (it "skips a file header when moving forward"
          (flycheck/with-error-list-buffer
            (flycheck/error-list-print-grouped source errors)
            ;; From line 8 in a.el the next error is line 3 in b.el, past the
            ;; b.el header.
            (goto-char (point-min))
            (forward-line 2)
            (expect (flycheck-error-line
                     (tabulated-list-get-id)) :to-equal 8)
            (flycheck-error-list-next-error 1)
            (expect (flycheck-error-line
                     (tabulated-list-get-id)) :to-equal 3)))

        (it "counts errors, not headers, for a prefix argument"
          (flycheck/with-error-list-buffer
            (flycheck/error-list-print-grouped source errors)
            ;; Two errors forward from line 1 lands on line 3, skipping the
            ;; b.el header that sits between the second and third error rows.
            (expect (flycheck-error-line
                     (tabulated-list-get-id)) :to-equal 1)
            (flycheck-error-list-next-error 2)
            (expect (flycheck-error-line
                     (tabulated-list-get-id)) :to-equal 3)))

        (it "terminates at the top instead of looping forever"
          (flycheck/with-error-list-buffer
            (flycheck/error-list-print-grouped source errors)
            ;; Moving back from the first error has nowhere to go; this must
            ;; return rather than spin on the leading header.
            (let ((start (point)))
              (flycheck-error-list-next-error -1)
              (expect (point) :to-equal start)))))))

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
                  :to-equal " [>= warning]")))

      (it "shows the checker and message filters in the mode line"
        (let ((flycheck-error-list-minimum-level nil)
              (flycheck-error-list--checker-filter 'python-ruff)
              (flycheck-error-list--message-filter "unused"))
          (expect (flycheck-error-list-mode-line-filter-indicator)
                  :to-equal " [python-ruff] [/unused/]"))))

    (describe "Checker and message filters"
      (it "filters errors by checker"
        (let ((flycheck-error-list--checker-filter 'foo)
              (errors (list (flycheck-error-new-at 1 1 'error "a" :checker 'foo)
                            (flycheck-error-new-at 2 1 'error "b"
                                                   :checker 'bar))))
          (expect (flycheck-error-list-apply-filter errors)
                  :to-be-equal-flycheck-errors
                  (list (flycheck-error-new-at 1 1 'error "a"
                                               :checker 'foo)))))

      (it "filters errors by message and id regexp"
        (let ((flycheck-error-list--message-filter "unused\\|W10")
              (errors (list (flycheck-error-new-at 1 1 'error "unused var")
                            (flycheck-error-new-at 2 1 'error "bad" :id "W101")
                            (flycheck-error-new-at 3 1 'error "other"))))
          (expect (flycheck-error-list-apply-filter errors)
                  :to-be-equal-flycheck-errors
                  (list (flycheck-error-new-at 1 1 'error "unused var")
                        (flycheck-error-new-at 2 1 'error "bad" :id "W101")))))

      (it "combines all filters"
        (let ((flycheck-error-list-minimum-level 'warning)
              (flycheck-error-list--checker-filter 'foo)
              (flycheck-error-list--message-filter "spam")
              (errors (list (flycheck-error-new-at 1 1 'error "spam"
                                                   :checker 'foo)
                            (flycheck-error-new-at 2 1 'info "spam"
                                                   :checker 'foo)
                            (flycheck-error-new-at 3 1 'error "spam"
                                                   :checker 'bar)
                            (flycheck-error-new-at 4 1 'error "eggs"
                                                   :checker 'foo))))
          (expect (flycheck-error-list-apply-filter errors)
                  :to-be-equal-flycheck-errors
                  (list (flycheck-error-new-at 1 1 'error "spam"
                                               :checker 'foo)))))

      (it "resets all filters"
        (flycheck/with-error-list-buffer
          (setq-local flycheck-error-list-minimum-level 'error)
          (setq flycheck-error-list--checker-filter 'foo
                flycheck-error-list--message-filter "spam")
          (flycheck-error-list-reset-filter)
          (expect flycheck-error-list-minimum-level :not :to-be 'error)
          (expect flycheck-error-list--checker-filter :not :to-be-truthy)
          (expect flycheck-error-list--message-filter :not :to-be-truthy))))

    (describe "Dynamic columns"
      (it "fits the File and ID columns to the errors"
        (expect (flycheck-error-list--column-widths
                 (list (flycheck-error-new-at
                        1 1 'error "a" :filename "path/to/some-file.el")
                       (flycheck-error-new-at
                        2 1 'error "b" :id "reportGeneralTypeIssues")))
                :to-equal '(12 . 23)))

      (it "caps the column widths"
        (expect (flycheck-error-list--column-widths
                 (list (flycheck-error-new-at
                        1 1 'error "a"
                        :filename (make-string 100 ?x)
                        :id (make-string 100 ?y))))
                :to-equal '(40 . 24)))

      (it "uses minimum widths without errors"
        (expect (flycheck-error-list--column-widths nil)
                :to-equal '(4 . 2)))

      (it "fits the format to the displayed errors"
        (flycheck-buttercup-with-temp-buffer
          (setq flycheck-current-errors
                (list (flycheck-error-new-at
                       1 1 'error "a" :checker 'foo
                       :filename "a-rather-long-file-name.el")
                      (flycheck-error-new-at
                       2 1 'error "b" :checker 'bar :filename "s.el")))
          (let ((source (current-buffer)))
            (flycheck/with-error-list-buffer
              (setq flycheck-error-list-source-buffer source)
              (flycheck-error-list--update-format)
              (expect (cadr (aref tabulated-list-format 0))
                      :to-equal (length "a-rather-long-file-name.el"))
              ;; Errors hidden by a filter don't inflate the columns
              (setq flycheck-error-list--checker-filter 'bar)
              (flycheck-error-list--update-format)
              (expect (cadr (aref tabulated-list-format 0))
                      :to-equal (length "s.el"))
              (expect flycheck--error-list-msg-offset
                      :to-equal (+ (length "s.el") 1  ; file + padding
                                   5 1                ; line
                                   3 1                ; col
                                   8 1                ; level
                                   2 1                ; id
                                   1))))))            ; list padding

      (it "rejects an invalid message filter regexp"
        (expect (flycheck-error-list-set-message-filter "[")
                :to-throw 'user-error)))

    (describe "Display"
      (it "displays the error list according to the display action"
        (let (received-action)
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (_buffer &optional action &rest _)
                       (setq received-action action)
                       nil)))
            (flycheck-buttercup-with-temp-buffer
              (flycheck-mode)
              (flycheck-list-errors)))
          (expect received-action
                  :to-equal flycheck-error-list-display-buffer-action))))))

;;; test-error-list.el ends here
