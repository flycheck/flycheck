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

;;; test-navigation.el ends here
