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

;;; test-navigation.el ends here
