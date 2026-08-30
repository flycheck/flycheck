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

  (describe "Related locations"
    (it "badges an error that carries related locations in the message cell"
      (let* ((err (flycheck-error-new-at
                   1 1 'error "redefined" :checker 'emacs-lisp
                   :relations
                   (list (flycheck-related-location-new :line 2 :message "a")
                         (flycheck-related-location-new :line 3 :message "b"))))
             (cells (cadr (flycheck-error-list-make-entry err))))
        (expect (substring-no-properties (car (aref cells 5))) :to-match "↳2")))

    (it "does not badge an error without related locations"
      (let* ((err (flycheck-error-new-at 1 1 'error "plain" :checker 'emacs-lisp))
             (cells (cadr (flycheck-error-list-make-entry err))))
        (expect (substring-no-properties (car (aref cells 5)))
                :not :to-match "↳")))

    ;; Put the error on the buffer text via `tabulated-list-id' rather than
    ;; stubbing `tabulated-list-get-id': it is a defsubst that inlines to
    ;; `get-text-property' when the command is byte-compiled, so a function
    ;; stub would not take effect there.
    (it "visits the sole related location of the error at point"
      (let* ((loc (flycheck-related-location-new :line 2 :column 1 :message "a"))
             (err (flycheck-error-new-at 1 1 'error "x" :checker 'emacs-lisp
                                         :relations (list loc)))
             visited)
        (with-temp-buffer
          (insert (propertize "row" 'tabulated-list-id err))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'flycheck-goto-related-location)
                     (lambda (location &rest _) (setq visited location))))
            (flycheck-error-list-visit-related-location)))
        (expect visited :to-be loc)))

    (it "signals a user-error when the error at point has no related locations"
      (with-temp-buffer
        (insert (propertize "row" 'tabulated-list-id
                            (flycheck-error-new-at 1 1 'error "x"
                                                   :checker 'emacs-lisp)))
        (goto-char (point-min))
        (expect (flycheck-error-list-visit-related-location)
                :to-throw 'user-error))))

  (describe "The fix badge"
    (defun test-fixbadge/render (err)
      "Return the row the error list builds for ERR, as one string."
      (cl-letf (((symbol-function 'flycheck--error-fix-buffer)
                 (lambda (_e) (current-buffer))))
        ;; each cell is (TEXT . PROPERTIES)
        (mapconcat (lambda (cell) (if (stringp cell) cell (car cell)))
                   (cadr (flycheck-error-list-make-entry err)) " ")))

    (it "marks an error whose fix is already in hand"
      (let ((err (flycheck-error-new-at
                  1 1 'error "x" :checker 'emacs-lisp
                  :fix (flycheck-fix-new :description "d" :edits nil :tick 0))))
        (expect (test-fixbadge/render err) :to-match "\\[fix\\] ")))

    (it "marks a fix that has to be fetched with a question mark"
      ;; An LSP code action is only requested when applied, so Flycheck
      ;; cannot promise it exists.  Saying nothing would hide that
      ;; `C-c ! f' is worth a try.
      (let ((err (flycheck-error-new-at 1 1 'error "x" :checker 'emacs-lisp
                                        :fix (lambda (_e) nil))))
        (expect (test-fixbadge/render err) :to-match "\\[fix\\?\\] ")))

    (it "marks nothing when the error carries no fix at all"
      (let ((err (flycheck-error-new-at 1 1 'error "x" :checker 'emacs-lisp)))
        (expect (test-fixbadge/render err) :not :to-match "\\[fix")))

    (it "marks an error about another file, whose fix opens that file"
      ;; Not rendered through `test-fixbadge/render': the point is what the
      ;; badge does when the error resolves to no buffer of its own.
      (let ((file (make-temp-file "flycheck-badge" nil ".el" "x\n")))
        (unwind-protect
            (let ((err (flycheck-error-new-at
                        1 1 'error "x" :checker 'emacs-lisp :buffer nil
                        :filename file
                        :fix (flycheck-fix-new :description "d" :edits nil
                                               :tick 0))))
              (expect (mapconcat
                       (lambda (cell) (if (stringp cell) cell (car cell)))
                       (cadr (flycheck-error-list-make-entry err)) " ")
                      :to-match "\\[fix\\] "))
          (delete-file file))))

    (it "marks nothing for an error whose file is gone"
      ;; The apply command would only refuse it, so promising a fix here
      ;; would send the user after one that cannot be applied.
      (let ((err (flycheck-error-new-at
                  1 1 'error "x" :checker 'emacs-lisp :buffer nil
                  :filename "/p/gone.el"
                  :fix (flycheck-fix-new :description "d" :edits nil
                                         :tick 0))))
        (expect (mapconcat (lambda (cell) (if (stringp cell) cell (car cell)))
                           (cadr (flycheck-error-list-make-entry err)) " ")
                :not :to-match "\\[fix")))

    (it "marks nothing for a fix with neither a buffer nor a file"
      (let ((err (flycheck-error-new-at
                  1 1 'error "x" :checker 'emacs-lisp :buffer nil
                  :filename nil
                  :fix (flycheck-fix-new :description "d" :edits nil
                                         :tick 0))))
        (expect (mapconcat (lambda (cell) (if (stringp cell) cell (car cell)))
                           (cadr (flycheck-error-list-make-entry err)) " ")
                :not :to-match "\\[fix"))))

  (describe "Applying fixes"
    (it "applies the fix of the error at point"
      (let* ((fix (flycheck-fix-new :description "Fix it" :edits nil :tick 0))
             (err (flycheck-error-new-at 1 1 'error "x" :checker 'emacs-lisp
                                         :fix fix))
             applied)
        (with-temp-buffer
          (insert (propertize "row" 'tabulated-list-id err))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'flycheck--error-fix-buffer)
                     (lambda (_e) (current-buffer)))
                    ((symbol-function 'flycheck-apply-fix)
                     (lambda (f _b) (setq applied f)))
                    ((symbol-function 'flycheck-error-list-refresh) #'ignore))
            (flycheck-error-list-apply-fix)))
        (expect applied :to-be fix)))

    (it "signals a user-error when the error at point has no fix"
      (with-temp-buffer
        (insert (propertize "row" 'tabulated-list-id
                            (flycheck-error-new-at 1 1 'error "x"
                                                   :checker 'emacs-lisp)))
        (goto-char (point-min))
        (expect (flycheck-error-list-apply-fix) :to-throw 'user-error)))

    (it "fixes all errors in the source buffer"
      (let ((src (generate-new-buffer " src")) called)
        (unwind-protect
            (with-temp-buffer
              (setq-local flycheck-error-list-source-buffer src)
              (cl-letf (((symbol-function 'flycheck-fix-all-errors)
                         (lambda () (interactive) (setq called t)))
                        ((symbol-function 'flycheck-error-list-refresh) #'ignore))
                (flycheck-error-list-fix-all))
              (expect called :to-be t))
          (kill-buffer src))))

    (it "signals a user-error when there is no live source buffer"
      (with-temp-buffer
        (setq-local flycheck-error-list-source-buffer nil)
        (expect (flycheck-error-list-fix-all) :to-throw 'user-error))))

  (describe "Applying fixes across the project"
    (defvar test-errorlist/dir nil)

    (defun test-errorlist/file (name)
      "Write a fixable file NAME in the spec's directory and return it."
      (let ((file (expand-file-name name test-errorlist/dir)))
        (with-temp-file file (insert "helo\n"))
        file))

    (defun test-errorlist/error (file)
      "Return an error about FILE carrying a fix, as a server offers one.

The fix comes from a provider, which computes it against the file as it
is when asked, so no earlier check has to vouch for it."
      (flycheck-error-new-at
       1 1 'error "typo" :checker 'emacs-lisp :filename file :buffer nil
       :fix (lambda (_err)
              (flycheck-fix-new
               :edits (list (flycheck-fix-edit-new
                             :line 1 :column 1 :end-line 1 :end-column 5
                             :replacement "hello"))))))

    (before-each
      (setq test-errorlist/dir
            (file-name-as-directory
             (file-truename (make-temp-file "flycheck-el" t)))))

    (after-each
      (dolist (buffer (buffer-list))
        (when-let* ((file (buffer-file-name buffer)))
          (when (string-prefix-p test-errorlist/dir file)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (ignore-errors (delete-directory test-errorlist/dir t)))

    (it "applies the fix of an error about another file, opening it"
      (let ((file (test-errorlist/file "b.el")))
        (with-temp-buffer
          (insert (propertize "row" 'tabulated-list-id
                              (test-errorlist/error file)))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'flycheck-error-list-refresh) #'ignore))
            (flycheck-error-list-apply-fix)))
        (with-current-buffer (find-buffer-visiting file)
          (expect (buffer-string) :to-equal "hello\n")))))

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
                      :to-equal '("▾ a.el (2)" 1 8 "▾ b.el (1)" 3))))))

      (it "blanks the file cell under a header"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (let ((error-row (nth 1 (flycheck-error-list-entries))))
              (expect (car (aref (cadr error-row) 0)) :to-equal "")))))

      (it "toggles dimensions and manages the sort key"
        (flycheck/with-error-list-buffer
          (expect flycheck-error-list-group-by :to-be nil)
          (flycheck-error-list-group-by-file)
          (expect flycheck-error-list-group-by :to-equal '(file))
          (expect tabulated-list-sort-key :to-be nil)
          ;; Adding a dimension nests it in canonical order.
          (flycheck-error-list-group-by-checker)
          (expect flycheck-error-list-group-by :to-equal '(file checker))
          ;; Toggling a dimension off leaves the rest.
          (flycheck-error-list-group-by-file)
          (expect flycheck-error-list-group-by :to-equal '(checker))
          (flycheck-error-list-group-by-none)
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

  (describe "Grouping by checker and level"
    (defun flycheck/group-headers ()
      "Return the header labels of the grouped list, in order."
      (delq nil
            (mapcar (lambda (e)
                      (unless (flycheck-error-p (car e))
                        (car (aref (cadr e) 0))))
                    (flycheck-error-list-entries))))

    (it "groups by checker, headers sorted by name"
      (flycheck/with-error-list-buffer
        (let ((errors (list (flycheck-error-new-at 1 1 'error nil :checker 'zebra)
                            (flycheck-error-new-at 2 1 'error nil :checker 'alpha))))
          (setq flycheck-error-list-group-by 'checker)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (expect (flycheck/group-headers)
                    :to-equal '("▾ alpha (1)" "▾ zebra (1)"))))))

    (it "groups by level, most severe group first"
      (flycheck/with-error-list-buffer
        (let ((errors (list (flycheck-error-new-at 1 1 'warning)
                            (flycheck-error-new-at 2 1 'error)
                            (flycheck-error-new-at 3 1 'error))))
          (setq flycheck-error-list-group-by 'level)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (expect (flycheck/group-headers)
                    :to-equal '("▾ error (2)" "▾ warning (1)"))))))

    (it "keeps each file's errors contiguous within a group"
      (flycheck/with-error-list-buffer
        (let ((errors (list (flycheck-error-new-at
                             5 1 'error nil :filename "/p/a.el" :checker 'x)
                            (flycheck-error-new-at
                             3 1 'error nil :filename "/p/b.el" :checker 'x)
                            (flycheck-error-new-at
                             10 1 'error nil :filename "/p/a.el" :checker 'x))))
          (setq flycheck-error-list-group-by 'checker)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            ;; Sorted by file then line, so a.el's rows stay together instead
            ;; of interleaving by line as a bare location sort would.
            (let ((files (delq nil
                               (mapcar (lambda (e)
                                         (and (flycheck-error-p (car e))
                                              (flycheck-error-filename (car e))))
                                       (flycheck-error-list-entries)))))
              (expect files :to-equal '("/p/a.el" "/p/a.el" "/p/b.el")))))))

    (it "falls back to a flat list for an unknown grouping"
      (flycheck/with-error-list-buffer
        (setq flycheck-error-list-group-by 'bogus)
        (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                   (lambda () (list (flycheck-error-new-at 1 1 'error)))))
          ;; Must not call seq-group-by with a nil key function.
          (let ((entries (flycheck-error-list-entries)))
            (expect (length entries) :to-equal 1)
            (expect (flycheck-error-p (car (car entries))) :to-be-truthy))))))

  (describe "Collapsing groups"
    (let ((errors (list (flycheck-error-new-at 1 1 'error nil
                                               :filename "/p/a.el" :checker 'x)
                        (flycheck-error-new-at 3 1 'error nil
                                               :filename "/p/b.el" :checker 'x)))
          source)
      (before-each
        (setq source (generate-new-buffer " collapse-source"))
        (with-current-buffer source (setq default-directory "/p/")))
      (after-each (kill-buffer source))

      (it "drops the errors of a collapsed group but keeps its header"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file
                flycheck-error-list--collapsed (make-hash-table :test 'equal))
          (puthash (list "/p/a.el") t flycheck-error-list--collapsed)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (let ((entries (flycheck-error-list-entries)))
              ;; a.el is collapsed: just its header, marked with the collapsed
              ;; triangle; b.el stays expanded with its error row.
              (expect (mapcar (lambda (e)
                                (if (flycheck-error-p (car e))
                                    (flycheck-error-line (car e))
                                  (car (aref (cadr e) 0))))
                              entries)
                      :to-equal '("▸ a.el (1)" "▾ b.el (1)" 3))))))

      (it "toggles the group under point on a header"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (setq tabulated-list-entries (flycheck-error-list-entries))
            (tabulated-list-print)
            (goto-char (point-min))       ; the a.el header
            (flycheck-error-list-toggle-group-at-point)
            (expect (gethash (list "/p/a.el") flycheck-error-list--collapsed) :to-be-truthy)
            (flycheck-error-list-toggle-group-at-point)
            (expect (gethash (list "/p/a.el") flycheck-error-list--collapsed) :to-be nil))))

      (it "toggles the parent group from an error row"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (setq tabulated-list-entries (flycheck-error-list-entries))
            (tabulated-list-print)
            (goto-char (point-min))
            (forward-line 1)              ; the a.el error row
            (expect (flycheck-error-p (tabulated-list-get-id)) :to-be-truthy)
            (flycheck-error-list-toggle-group-at-point)
            (expect (gethash (list "/p/a.el") flycheck-error-list--collapsed)
                    :to-be-truthy))))

      (it "expands a group that clears and later reappears"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by 'file
                flycheck-error-list--collapsed (make-hash-table :test 'equal))
          (puthash (list "/p/a.el") t flycheck-error-list--collapsed)
          ;; a.el has no errors this round, so its stale collapse key must be
          ;; pruned; otherwise a new a.el error would come back hidden.
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () (list (cadr errors)))))
            (flycheck-error-list-entries)
            (expect (gethash (list "/p/a.el") flycheck-error-list--collapsed)
                    :to-be nil))))

      (it "keeps collapse state when a filter hides the group"
        (flycheck/with-error-list-buffer
          (let ((errs (list (flycheck-error-new-at 1 1 'error "aaa"
                                                   :filename "/p/a.el" :checker 'x)
                            (flycheck-error-new-at 3 1 'error "bbb"
                                                   :filename "/p/b.el" :checker 'x))))
            (setq flycheck-error-list-source-buffer source
                  flycheck-error-list-group-by 'file
                  flycheck-error-list--collapsed (make-hash-table :test 'equal)
                  ;; A message filter that matches only b.el's error.
                  flycheck-error-list--message-filter "bbb")
            (puthash (list "/p/a.el") t flycheck-error-list--collapsed)
            (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                       (lambda () errs)))
              (flycheck-error-list-entries)
              ;; a.el is filtered out of this render but still exists, so its
              ;; collapse must survive to when the filter is relaxed.
              (expect (gethash (list "/p/a.el") flycheck-error-list--collapsed)
                      :to-be-truthy)))))

      (it "resets collapse state when the scope changes"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list--collapsed (make-hash-table :test 'equal))
          (puthash (list "/p/a.el") t flycheck-error-list--collapsed)
          (flycheck-error-list-toggle-scope)
          (expect flycheck-error-list--collapsed :to-be nil)))

      (it "moves to a button instead of toggling in a flat list"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by nil)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (setq tabulated-list-entries (flycheck-error-list-entries))
            (tabulated-list-print)
            (goto-char (point-min))
            ;; TAB must not be dead in a flat list: it should not create
            ;; collapse state and should leave point on an error row.
            (flycheck-error-list-toggle-group-at-point)
            (expect flycheck-error-list--collapsed :to-be nil)
            (expect (flycheck-error-p (tabulated-list-get-id))
                    :to-be-truthy))))))

  (describe "Nested grouping"
    (let ((errors (list (flycheck-error-new-at 1 1 'error "a"
                                               :filename "/p/a.el" :checker 'rubocop)
                        (flycheck-error-new-at 2 1 'warning "b"
                                               :filename "/p/a.el" :checker 'ruby)
                        (flycheck-error-new-at 3 1 'error "c"
                                               :filename "/p/b.el" :checker 'rubocop)))
          source)
      (before-each
        (setq source (generate-new-buffer " nested-source"))
        (with-current-buffer source (setq default-directory "/p/")))
      (after-each (kill-buffer source))

      (defun flycheck/nested-repr ()
        "Return the header labels and error lines of the grouped list."
        (mapcar (lambda (e)
                  (if (flycheck-error-p (car e))
                      (flycheck-error-line (car e))
                    (car (aref (cadr e) 0))))
                (flycheck-error-list-entries)))

      (it "nests dimensions and indents inner headers"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by '(file checker))
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            ;; file at depth 0, checker headers indented under it, then rows.
            (expect (flycheck/nested-repr)
                    :to-equal '("▾ a.el (2)"
                                "  ▾ rubocop (1)" 1
                                "  ▾ ruby (1)" 2
                                "▾ b.el (1)"
                                "  ▾ rubocop (1)" 3)))))

      (it "collapsing an outer group hides all its subgroups"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by '(file checker)
                flycheck-error-list--collapsed (make-hash-table :test 'equal))
          (puthash (list "/p/a.el") t flycheck-error-list--collapsed)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (expect (flycheck/nested-repr)
                    :to-equal '("▸ a.el (2)"
                                "▾ b.el (1)"
                                "  ▾ rubocop (1)" 3)))))

      (it "collapsing an inner group hides only that subgroup"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by '(file checker)
                flycheck-error-list--collapsed (make-hash-table :test 'equal))
          (puthash (list "/p/a.el" 'rubocop) t flycheck-error-list--collapsed)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () errors)))
            (expect (flycheck/nested-repr)
                    :to-equal '("▾ a.el (2)"
                                "  ▸ rubocop (1)"
                                "  ▾ ruby (1)" 2
                                "▾ b.el (1)"
                                "  ▾ rubocop (1)" 3)))))

      (it "derives an error's full group path"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-group-by '(file checker))
          (expect (flycheck-error-list--error-group-path (car errors))
                  :to-equal '("/p/a.el" rubocop))))

      (it "prunes only paths with no live errors"
        (flycheck/with-error-list-buffer
          (setq flycheck-error-list-source-buffer source
                flycheck-error-list-group-by '(file checker)
                flycheck-error-list--collapsed (make-hash-table :test 'equal))
          ;; A collapsed inner path that still has an error must survive; a
          ;; path for a file that is gone must be pruned.
          (puthash (list "/p/a.el" 'rubocop) t flycheck-error-list--collapsed)
          (puthash (list "/p/gone.el") t flycheck-error-list--collapsed)
          (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                     (lambda () (list (car errors)))))
            (flycheck-error-list-entries)
            (expect (gethash (list "/p/a.el" 'rubocop)
                             flycheck-error-list--collapsed) :to-be-truthy)
            (expect (gethash (list "/p/gone.el")
                             flycheck-error-list--collapsed) :to-be nil))))))

  (describe "Grouping controls line"
    (it "advertises every dimension and marks the active one"
      (flycheck/with-error-list-buffer
        (setq flycheck-error-list-group-by 'checker)
        (let ((line (flycheck-error-list--grouping-line)))
          (dolist (fragment '("M-1 flat" "M-2 file" "M-3 checker" "M-4 level"))
            (expect line :to-match (regexp-quote fragment)))
          ;; The active dimension carries the header face.
          (let ((start (string-match "M-3 checker" line)))
            (expect (get-text-property start 'face line)
                    :to-be 'flycheck-error-list-group-header))))))

  (describe "Mouse support"
    (it "makes each tab-line label toggle its dimension on click"
      (flycheck/with-error-list-buffer
        (setq flycheck-error-list-group-by nil)
        (let* ((line (flycheck-error-list--grouping-line))
               (start (string-match "M-2 file" line))
               (map (get-text-property start 'keymap line)))
          (expect (lookup-key map [tab-line mouse-1])
                  :to-be 'flycheck-error-list-group-by-file)
          (expect (get-text-property start 'mouse-face line) :to-be 'highlight)
          (expect (get-text-property start 'help-echo line) :to-be-truthy))))

    (it "toggles the error list's own grouping when clicked from elsewhere"
      ;; A tab-line click runs the command with the clicked window's buffer
      ;; current, which need not be the error list; it must still read and
      ;; toggle the error list's own grouping, not the current buffer's.
      (let ((el (get-buffer-create flycheck-error-list-buffer)))
        (unwind-protect
            (progn
              (with-current-buffer el
                (flycheck-error-list-mode)
                (setq flycheck-error-list-group-by '(file checker)))
              (with-temp-buffer
                (flycheck-error-list-group-by-file))
              (expect (buffer-local-value 'flycheck-error-list-group-by el)
                      :to-equal '(checker)))
          (kill-buffer el))))

    (it "makes group headers clickable to collapse or expand"
      (flycheck/with-error-list-buffer
        (let ((errors (list (flycheck-error-new-at 1 1 'error "a"
                                                   :filename "/p/a.el" :checker 'x)))
              (source (generate-new-buffer " mouse-source")))
          (unwind-protect
              (progn
                (with-current-buffer source (setq default-directory "/p/"))
                (setq flycheck-error-list-source-buffer source
                      flycheck-error-list-group-by '(file))
                (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                           (lambda () errors)))
                  (setq tabulated-list-entries (flycheck-error-list-entries))
                  (tabulated-list-print))
                (goto-char (point-min))
                (let ((button (next-button (point))))
                  ;; The header label is a group button; activating it (as a
                  ;; click would) toggles the group's collapse state.
                  (expect (button-type button)
                          :to-be 'flycheck-error-list-group)
                  (flycheck-error-list-goto-error (button-start button))
                  (expect (gethash (list "/p/a.el")
                                   flycheck-error-list--collapsed)
                          :to-be-truthy)))
            (kill-buffer source))))))

  (describe "Row position index"
    (defun flycheck/error-list-print (errors)
      "Render ERRORS as a flat list in the current error list buffer."
      (setq flycheck-error-list-group-by nil)
      (cl-letf (((symbol-function 'flycheck-error-list-current-errors)
                 (lambda () errors)))
        (setq tabulated-list-entries (flycheck-error-list-entries)))
      (tabulated-list-print))

    (it "maps each listed error to the row where it is shown"
      (flycheck/with-error-list-buffer
        (let ((errors (list (flycheck-error-new-at 10 1 'error)
                            (flycheck-error-new-at 20 1 'warning))))
          (flycheck/error-list-print errors)
          (let ((index (flycheck-error-list--positions)))
            (dolist (err errors)
              (let ((positions (gethash err index)))
                (expect (length positions) :to-equal 1)
                (expect (tabulated-list-get-id (car positions))
                        :to-be err)))))))

    (it "records every row an equal error appears on"
      (flycheck/with-error-list-buffer
        ;; Two distinct but `equal' errors (e.g. from different checkers
        ;; reporting the same thing) share a key and must both light up.
        (let ((errors (list (flycheck-error-new-at 10 1 'error "dup")
                            (flycheck-error-new-at 10 1 'error "dup"))))
          (flycheck/error-list-print errors)
          (let ((positions (gethash (car errors)
                                    (flycheck-error-list--positions))))
            (expect (length positions) :to-equal 2)))))

    (it "reuses the cached table until the list is reprinted"
      (flycheck/with-error-list-buffer
        (let ((errors (list (flycheck-error-new-at 10 1 'error))))
          (flycheck/error-list-print errors)
          (let ((first (flycheck-error-list--positions)))
            ;; No reprint, so the very same table is handed back.
            (expect (flycheck-error-list--positions) :to-be first)
            ;; Reprinting bumps the buffer tick and rebuilds the table.
            (tabulated-list-print)
            (expect (flycheck-error-list--positions) :not :to-be first))))))

  (describe "Highlighting errors at point"
    ;; `flycheck-error-list-highlight-errors' only runs when the list is
    ;; visible and reads the errors on the source line; stub both so the
    ;; overlay-building path can be exercised in batch.
    (defmacro flycheck/highlighting (errors at-point &rest body)
      "Print ERRORS, pretend AT-POINT are the source-line errors, run BODY."
      (declare (indent 2))
      `(flycheck/with-error-list-buffer
         (let ((source (generate-new-buffer " hl-source")))
           (unwind-protect
               (progn
                 (flycheck/error-list-print ,errors)
                 (setq flycheck-error-list-source-buffer source)
                 (cl-letf (((symbol-function 'get-buffer-window)
                            (lambda (&rest _) t))
                           ((symbol-function 'flycheck-overlay-errors-in)
                            (lambda (&rest _) ,at-point)))
                   ,@body))
             (kill-buffer source)))))

    (it "highlights the row of each error on the source line"
      (let ((errors (list (flycheck-error-new-at 10 1 'error)
                          (flycheck-error-new-at 20 1 'warning))))
        (flycheck/highlighting errors (list (car errors))
          (flycheck-error-list-highlight-errors 'preserve-pos)
          (let ((overlays flycheck-error-list-highlight-overlays))
            (expect (length overlays) :to-equal 1)
            (expect (tabulated-list-get-id (overlay-start (car overlays)))
                    :to-be (car errors))))))

    (it "creates one highlight per row for equal errors at point"
      ;; Two distinct but `equal' errors show as two rows and both are at
      ;; point; each row must get exactly one overlay, not one per error.
      (let ((errors (list (flycheck-error-new-at 10 1 'error "dup")
                          (flycheck-error-new-at 10 1 'error "dup"))))
        (flycheck/highlighting errors errors
          (flycheck-error-list-highlight-errors 'preserve-pos)
          (expect (length flycheck-error-list-highlight-overlays)
                  :to-equal 2))))

    (it "leaves point put when the errors at point are filtered out"
      ;; Regression: with no matching row the recenter must be skipped rather
      ;; than sending point to an unrelated row.
      (let ((errors (list (flycheck-error-new-at 10 1 'error)))
            (absent (flycheck-error-new-at 99 1 'warning)))
        (flycheck/highlighting errors (list absent)
          (goto-char (point-max))
          (let ((start (point)))
            (flycheck-error-list-highlight-errors nil)
            (expect flycheck-error-list-highlight-overlays :to-be nil)
            (expect (point) :to-equal start))))))

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
