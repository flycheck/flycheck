;;; test-error-display.el --- Flycheck Specs: Error Display -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)
(require 'shut-up)

(describe "Error display"

  (describe "flycheck-display-errors-function"

    (it "no display function set"
      (let ((err (flycheck-error-new-at 10 20 'warning "This is a Flycheck error."))
            (flycheck-display-errors-function nil))
        (shut-up
          ;; Without an error function, error display should be a no-op.
          (flycheck-display-errors (list err))
          (expect (shut-up-current-output) :to-equal ""))))

    (it "custom function"
      (let* ((err (flycheck-error-new-at 10 20 'warning "Foo"))
             (displayed-errors nil)
             (flycheck-display-errors-function (lambda (errors)
                                                 (dolist (err errors)
                                                   (push err displayed-errors)))))
        (flycheck-display-errors (list err))
        (expect displayed-errors :to-equal (list err)))))

  (describe "flycheck-display-error-messages"

    (it "displays error messages"
      (assume (not (eq system-type 'windows-nt)))
      (let ((err (flycheck-error-new-at 10 20 'warning
                                        "This is a Flycheck error."
                                        :id "spam")))
        (shut-up
          (flycheck-display-error-messages (list err))
          (expect (shut-up-current-output)
                  :to-equal "This is a Flycheck error. [spam]\n")))))

  (describe "flycheck-copy-errors-as-kill"

    (it "copies errors to kill ring"
      (flycheck-buttercup-with-temp-buffer
        (insert "A test buffer to copy errors from")
        (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
              (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                            (flycheck-error-new-at 1 10 'warning "2nd message"
                                                   :id "foo"))))
          (mapc #'flycheck-add-overlay errors)
          (flycheck-copy-errors-as-kill 10)
          (expect (seq-take kill-ring 2) :to-equal '("1st message" "2nd message"))
          (flycheck-copy-errors-as-kill 10 #'flycheck-error-id)
          (expect (seq-take kill-ring 1) :to-equal '("foo"))
          (flycheck-copy-errors-as-kill 10 #'flycheck-error-format-message-and-id)
          (expect (seq-take kill-ring 2)
                  :to-equal '("1st message" "2nd message [foo]")))))))

;;; test-error-display.el ends here
