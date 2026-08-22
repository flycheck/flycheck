;;; test-navigation.el --- Flycheck Specs: Error Navigation -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Error navigation"

  (describe "with no minimum level"

    (describe "flycheck-next-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer nil
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "goes to next error"
        (flycheck-test-with-nav-buffer nil
          (flycheck-next-error)
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "errors beyond last error"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer nil
          (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer nil
          (flycheck-next-error 2)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (flycheck-next-error -2)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "reset navigates to first error"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (flycheck-next-error 1 'reset)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "does not cross narrowing"
        (flycheck-test-with-nav-buffer nil
          (re-search-forward "(defun .*")
          (narrow-to-defun)
          (goto-char (point-min))
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 1)
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors")))))

    (describe "flycheck-previous-error"

      (it "errors before first error"
        (flycheck-test-with-nav-buffer nil
          (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "goes to last error"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (flycheck-previous-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (flycheck-previous-error 2)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer nil
          (flycheck-previous-error -2)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors")))))

    (describe "flycheck-first-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "stays at first error if called again"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (flycheck-first-error)
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "goes to second error"
        (flycheck-test-with-nav-buffer nil
          (goto-char (point-max))
          (flycheck-first-error 2)
          (expect (point) :to-be-at-flycheck-error 2)))))

  (describe "with minimum level error"

    (describe "flycheck-next-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer 'error
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "goes to next error"
        (flycheck-test-with-nav-buffer 'error
          (flycheck-next-error)
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors beyond last error"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer 'error
          (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer 'error
          (let ((err (should-error (flycheck-next-error 2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (let ((err (should-error (flycheck-next-error -2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "reset navigates to first error"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (flycheck-next-error 1 'reset)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "does not cross narrowing"
        (flycheck-test-with-nav-buffer 'error
          (re-search-forward "(defun .*")
          (narrow-to-defun)
          (goto-char (point-min))
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "signals error when no error at minimum level"
        (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
          (emacs-lisp-mode)
          (flycheck-mode)
          (let ((flycheck-navigation-minimum-level 'error))
            (flycheck-buttercup-buffer-sync)
            (goto-char (point-min))
            (let ((err (should-error (flycheck-next-error 1) :type 'user-error)))
              (expect (cadr err) :to-equal "No more Flycheck errors"))))))

    (describe "flycheck-previous-error"

      (it "errors before first error"
        (flycheck-test-with-nav-buffer 'error
          (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "goes to last error"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (flycheck-previous-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (let ((err (should-error (flycheck-previous-error -2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer 'error
          (let ((err (should-error (flycheck-previous-error -2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors")))))

    (describe "flycheck-first-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "stays at first error if called again"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (flycheck-first-error)
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "goes to second error"
        (flycheck-test-with-nav-buffer 'error
          (goto-char (point-max))
          (let ((err (should-error (flycheck-first-error 2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))))

  (describe "with minimum level warning"
    ;; The resource file has only 'info and 'error level errors (no exact
    ;; 'warning level), so with minimum-level 'warning only error 2
    ;; (error-level, severity 100) is navigable.

    (describe "flycheck-next-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer 'warning
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "goes to next error"
        (flycheck-test-with-nav-buffer 'warning
          (flycheck-next-error)
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors beyond last error"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer 'warning
          (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer 'warning
          (let ((err (should-error (flycheck-next-error 2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (let ((err (should-error (flycheck-next-error -2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "reset navigates to first error"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (flycheck-next-error 1 'reset)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "does not cross narrowing"
        (flycheck-test-with-nav-buffer 'warning
          (re-search-forward "(defun .*")
          (narrow-to-defun)
          (goto-char (point-min))
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors")))))

    (describe "flycheck-previous-error"

      (it "errors before first error"
        (flycheck-test-with-nav-buffer 'warning
          (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "goes to last error"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (flycheck-previous-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (let ((err (should-error (flycheck-previous-error 2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer 'warning
          (let ((err (should-error (flycheck-previous-error -2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors")))))

    (describe "flycheck-first-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "stays at first error if called again"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (flycheck-first-error)
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "goes to second error"
        (flycheck-test-with-nav-buffer 'warning
          (goto-char (point-max))
          (let ((err (should-error (flycheck-first-error 2) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))))

  (describe "with minimum level info"

    (describe "flycheck-next-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer 'info
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "goes to next error"
        (flycheck-test-with-nav-buffer 'info
          (flycheck-next-error)
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "errors beyond last error"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer 'info
          (let ((err (should-error (flycheck-next-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer 'info
          (flycheck-next-error 2)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (flycheck-next-error -2)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "reset navigates to first error"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (flycheck-next-error 1 'reset)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "does not cross narrowing"
        (flycheck-test-with-nav-buffer 'info
          (re-search-forward "(defun .*")
          (narrow-to-defun)
          (goto-char (point-min))
          (flycheck-next-error)
          (expect (point) :to-be-at-flycheck-error 1)
          (let ((err (should-error (flycheck-next-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors")))))

    (describe "flycheck-previous-error"

      (it "errors before first error"
        (flycheck-test-with-nav-buffer 'info
          (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors"))))

      (it "goes to last error"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (flycheck-previous-error)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "navigates by two errors"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (flycheck-previous-error 2)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "navigates back by two errors"
        (flycheck-test-with-nav-buffer 'info
          (flycheck-previous-error -2)
          (expect (point) :to-be-at-flycheck-error 2)))

      (it "errors when moving too far"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (let ((err (should-error (flycheck-previous-error 4) :type 'user-error)))
            (expect (cadr err) :to-equal "No more Flycheck errors")))))

    (describe "flycheck-first-error"

      (it "goes to first error"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "stays at first error if called again"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (flycheck-first-error)
          (flycheck-first-error)
          (expect (point) :to-be-at-flycheck-error 1)))

      (it "goes to second error"
        (flycheck-test-with-nav-buffer 'info
          (goto-char (point-max))
          (flycheck-first-error 2)
          (expect (point) :to-be-at-flycheck-error 2))))))

(defun flycheck-test--nested-nav-setup ()
  "Give the buffer a nested error pair on line 1 and a plain error on line 3."
  (insert "nested pair\nplain line\nlater error\n")
  ;; The variable alone: running the mode would kick off a real check
  (setq-local flycheck-mode t)
  (pcase-dolist (`(,line ,col ,end ,level ,msg)
                 '((1 2 10 warning "outer")
                   (1 4 7 error "inner")
                   (3 1 6 error "later")))
    (let ((err (flycheck-error-new-at line col level msg
                                      :end-line line :end-column end
                                      :buffer (current-buffer)
                                      :checker 'emacs-lisp)))
      (push err flycheck-current-errors)
      (flycheck-add-overlay err))))

(describe "Navigation among nested errors"
  ;; A region nested inside another used to read as one more error each
  ;; time the outer overlay resumed past it: forward navigation cycled
  ;; between the two starts forever, and backward silently skipped the
  ;; inner one.  See #1781.

  (it "visits the nested pair as two stops, then moves on and stops"
    (flycheck-buttercup-with-temp-buffer
      (flycheck-test--nested-nav-setup)
      (goto-char (point-min))
      (flycheck-next-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 1 2))
      (flycheck-next-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 1 4))
      (flycheck-next-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 3 1))
      (let ((err (should-error (flycheck-next-error) :type 'user-error)))
        (expect (cadr err) :to-equal "No more Flycheck errors"))))

  (it "visits the inner error backwards too"
    (flycheck-buttercup-with-temp-buffer
      (flycheck-test--nested-nav-setup)
      (goto-char (point-max))
      (flycheck-previous-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 3 1))
      (flycheck-previous-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 1 4))
      (flycheck-previous-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 1 2))
      (let ((err (should-error (flycheck-previous-error) :type 'user-error)))
        (expect (cadr err) :to-equal "No more Flycheck errors"))))

  (it "counts errors sharing a start as one stop"
    (flycheck-buttercup-with-temp-buffer
      (flycheck-test--nested-nav-setup)
      ;; A second error at the outer error's exact start
      (let ((err (flycheck-error-new-at 1 2 'error "twin"
                                        :end-line 1 :end-column 5
                                        :buffer (current-buffer)
                                        :checker 'emacs-lisp)))
        (push err flycheck-current-errors)
        (flycheck-add-overlay err))
      (goto-char (point-min))
      (flycheck-next-error 2)
      (expect (point) :to-equal (flycheck-line-column-to-position 1 4))))

  (it "walks past an error whose highlighted text was deleted"
    ;; Deleting the region collapses the overlay to nothing; a stop
    ;; there could only fall back to the outer error and jump backwards
    (flycheck-buttercup-with-temp-buffer
      (flycheck-test--nested-nav-setup)
      (delete-region (flycheck-line-column-to-position 1 4)
                     (flycheck-line-column-to-position 1 7))
      (goto-char (point-min))
      (flycheck-next-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 1 2))
      (flycheck-next-error)
      (expect (point) :to-equal (flycheck-line-column-to-position 3 1))
      (let ((err (should-error (flycheck-next-error) :type 'user-error)))
        (expect (cadr err) :to-equal "No more Flycheck errors")))))

(describe "Project navigation scope"

  (before-each
    (spy-on 'project-current :and-return-value nil)
    (clrhash flycheck--project-error-store))

  (defun flycheck-test--nav-record (dir &rest specs)
    "Record errors per SPECS, each (FILE LINE LEVEL), under DIR's project."
    (flycheck--project-record-errors
     (mapcar (lambda (spec)
               (flycheck-error-new-at
                (nth 1 spec) 1 (nth 2 spec) "e"
                :filename (expand-file-name (nth 0 spec) dir)
                :checker 'x))
             specs)))

  (defmacro flycheck-test--with-nav-project (files &rest body)
    "Run BODY with DIR bound to a temp project containing FILES on disk."
    (declare (indent 1))
    `(let ((dir (make-temp-file "flycheck-nav" t))
           ;; No live checks: these specs feed the store directly, and
           ;; a checker process would hold the directory against the
           ;; teardown below on Windows
           (flycheck-check-syntax-automatically nil))
       (unwind-protect
           (progn
             (dolist (file ,files)
               (write-region "one\ntwo\nthree\nfour\n" nil
                             (expand-file-name file dir)))
             ,@body)
         (dolist (buffer (buffer-list))
           (when-let* ((name (buffer-file-name buffer)))
             (when (string-prefix-p (file-name-as-directory dir)
                                    (expand-file-name name))
               (with-current-buffer buffer
                 (set-buffer-modified-p nil)
                 (kill-buffer)))))
         ;; Windows keeps just-deleted files in a pending state while
         ;; anything still holds a handle, which fails the removal of
         ;; their directory; give it a moment before believing that
         (let ((attempts 10))
           (while (and (> attempts 0)
                       (not (ignore-errors (delete-directory dir t) t)))
             (setq attempts (1- attempts))
             (sleep-for 0.1))
           (when (file-directory-p dir)
             (delete-directory dir t))))))

  (it "hands the session over so repeated next-error advances"
    ;; The real command re-runs in next-error-last-buffer; without the
    ;; hand-off it would jump to the same first error forever
    (flycheck-test--with-nav-project '("a.rb" "b.rb")
      ;; The target buffer is open with flycheck-mode, as the global
      ;; mode would have it, so the hand-off finds a next-error buffer
      (with-current-buffer (find-file-noselect (expand-file-name "b.rb" dir))
        (flycheck-mode 1))
      (with-current-buffer (find-file-noselect (expand-file-name "a.rb" dir))
        (flycheck-mode 1)
        (flycheck-test--nav-record dir '("b.rb" 1 error) '("b.rb" 3 error))
        (switch-to-buffer (current-buffer))
        (goto-char (point-max))
        (let ((flycheck-navigation-scope 'project))
          (next-error)
          ;; Emacs may visit the target under another spelling of the
          ;; same place - its true name - so compare files, not strings
          (expect (flycheck-same-files-p
                   (buffer-file-name (window-buffer))
                   (expand-file-name "b.rb" dir))
                  :to-be-truthy)
          (with-current-buffer (window-buffer)
            (expect (line-number-at-pos (point)) :to-equal 1)
            ;; The next step runs where the user landed, as it would
            ;; interactively
            (next-error))
          (with-current-buffer (window-buffer)
            (expect (line-number-at-pos (point)) :to-equal 3))))))

  (it "continues within the current file from what the store knows"
    ;; A freshly opened file's own later errors come before the next file
    (flycheck-test--with-nav-project '("a.rb" "b.rb")
      (with-current-buffer (find-file-noselect (expand-file-name "a.rb" dir))
        (flycheck-mode 1)
        (flycheck-test--nav-record dir '("a.rb" 3 error) '("b.rb" 1 error))
        (goto-char (point-min))
        (let* ((flycheck-navigation-scope 'project)
               (err (flycheck--next-project-error 1)))
          (expect (flycheck-error-filename err)
                  :to-equal (expand-file-name "a.rb" dir))
          (expect (flycheck-error-line err) :to-equal 3)))))

  (it "continues backwards into the previous file's last error"
    (flycheck-test--with-nav-project '("a.rb" "c.rb")
      (with-current-buffer (find-file-noselect (expand-file-name "c.rb" dir))
        (flycheck-mode 1)
        (flycheck-test--nav-record dir
                                   '("a.rb" 1 error) '("a.rb" 4 error))
        (goto-char (point-min))
        (let* ((flycheck-navigation-scope 'project)
               (err (flycheck--next-project-error -1)))
          (expect (flycheck-error-filename err)
                  :to-equal (expand-file-name "a.rb" dir))
          (expect (flycheck-error-line err) :to-equal 4)))))

  (it "does not mistake the current file's other spelling for a neighbor"
    (flycheck-test--with-nav-project '("a.rb")
      (let* ((real (expand-file-name "a.rb" dir))
             (link-dir (concat dir "-link")))
        (make-symbolic-link dir link-dir)
        (unwind-protect
            (with-current-buffer
                (find-file-noselect (expand-file-name "a.rb" link-dir))
              (flycheck-mode 1)
              ;; The store holds the truename spelling
              (flycheck--project-record-errors
               (list (flycheck-error-new-at 2 1 'error "e"
                                            :filename real :checker 'x)))
              (goto-char (point-max))
              (let ((flycheck-navigation-scope 'project))
                ;; Same file, so nothing to continue to
                (expect (flycheck--next-project-error 1) :to-be nil)))
          (delete-file link-dir)))))

  (it "orders files by their true names across spellings"
    ;; A buffer opened under a spelling that sorts after the store's -
    ;; as Windows short 8.3 names do - must still find the next file
    (flycheck-test--with-nav-project '("a.rb" "b.rb")
      (let ((link (concat dir "0"))
            (buffer nil))
        (make-symbolic-link dir link)
        (unwind-protect
            (with-current-buffer
                (setq buffer
                      (find-file-noselect (expand-file-name "a.rb" link)))
              (flycheck-mode 1)
              (flycheck-test--nav-record dir '("b.rb" 1 error))
              (goto-char (point-max))
              (let* ((flycheck-navigation-scope 'project)
                     (err (flycheck--next-project-error 1)))
                (expect (flycheck-error-filename err)
                        :to-equal (expand-file-name "b.rb" dir))))
          (when buffer
            (with-current-buffer buffer
              (set-buffer-modified-p nil)
              (kill-buffer)))
          (delete-file link)))))

  (it "passes over an error whose file is gone"
    (flycheck-test--with-nav-project '("a.rb" "c.rb")
      (with-current-buffer (find-file-noselect (expand-file-name "a.rb" dir))
        (flycheck-mode 1)
        (flycheck-test--nav-record dir
                                   '("b.rb" 1 error) '("c.rb" 2 error))
        (let* ((flycheck-navigation-scope 'project)
               (err (flycheck--next-project-error 1)))
          (expect (flycheck-error-filename err)
                  :to-equal (expand-file-name "c.rb" dir))))))

  (it "refuses a zero step"
    (flycheck-test--with-nav-project '("a.rb")
      (with-current-buffer (find-file-noselect (expand-file-name "a.rb" dir))
        (flycheck-mode 1)
        (flycheck-test--nav-record dir '("b.rb" 1 error))
        (let ((flycheck-navigation-scope 'project))
          (should-error (flycheck-next-error-function 0 nil)
                        :type 'user-error)))))

  (it "stays put under the default buffer scope"
    (flycheck-test--with-nav-project '("a.rb")
      (with-current-buffer (find-file-noselect (expand-file-name "a.rb" dir))
        (flycheck-mode 1)
        (flycheck-test--nav-record dir '("b.rb" 1 error))
        (let ((err (should-error (flycheck-next-error-function 1 nil)
                                 :type 'user-error)))
          (expect (cadr err) :to-equal "No more Flycheck errors")))))

  (it "says so when the whole project is exhausted"
    (flycheck-test--with-nav-project '("a.rb")
      (with-current-buffer (find-file-noselect (expand-file-name "a.rb" dir))
        (flycheck-mode 1)
        (let* ((flycheck-navigation-scope 'project)
               (err (should-error (flycheck-next-error-function 1 nil)
                                  :type 'user-error)))
          (expect (cadr err)
                  :to-equal "No more Flycheck errors in the project")))))

  (it "respects the minimum navigation level across files"
    (flycheck-test--with-nav-project '("a.rb")
      (with-current-buffer (find-file-noselect (expand-file-name "a.rb" dir))
        (flycheck-mode 1)
        (flycheck-test--nav-record dir '("b.rb" 1 info))
        (let ((flycheck-navigation-scope 'project)
              (flycheck-navigation-minimum-level 'warning))
          (should-error (flycheck-next-error-function 1 nil)
                        :type 'user-error)))))

  (it "keeps a buffer without a file within itself"
    (with-temp-buffer
      (setq default-directory temporary-file-directory)
      (let* ((flycheck-navigation-scope 'project)
             (err (should-error (flycheck-next-error-function 1 nil)
                                :type 'user-error)))
        ;; No project search happened, so no project claim either
        (expect (cadr err) :to-equal "No more Flycheck errors")))))

;;; test-navigation.el ends here
