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

  (describe "flycheck-eldoc-function"

    (it "documents the errors at point"
      (flycheck-buttercup-with-temp-buffer
        (insert "hello world")
        (flycheck-mode)
        (goto-char 3)
        (flycheck-add-overlay
         (flycheck-error-new-at 1 1 'warning "Watch out" :end-column 6))
        (let (doc)
          (flycheck-eldoc-function (lambda (string &rest _) (setq doc string)))
          (expect doc :to-match "warning: Watch out"))))

    (it "registers with eldoc in flycheck-mode"
      (flycheck-buttercup-with-temp-buffer
        (flycheck-mode 1)
        (expect (member #'flycheck-eldoc-function
                        eldoc-documentation-functions)
                :to-be-truthy)
        (flycheck-mode -1)
        (expect (member #'flycheck-eldoc-function
                        eldoc-documentation-functions)
                :not :to-be-truthy)))

    (it "stays inert when the display function is customized"
      (flycheck-buttercup-with-temp-buffer
        (insert "hello")
        (flycheck-mode)
        (flycheck-add-overlay (flycheck-error-new-at 1 1 'error "Boom"))
        (goto-char 1)
        (let ((flycheck-display-errors-function
               #'flycheck-display-error-messages)
              (called nil))
          (flycheck-eldoc-function (lambda (&rest _) (setq called t)))
          (expect called :not :to-be-truthy))))

    (it "triggers an eldoc refresh from any display entry point"
      (flycheck-buttercup-with-temp-buffer
        (insert "hello")
        (flycheck-mode)
        (flycheck-add-overlay (flycheck-error-new-at 1 1 'error "Boom"))
        (goto-char 1)
        (let ((refreshed 0))
          (cl-letf (((symbol-function 'eldoc-print-current-symbol-info)
                     (lambda (&rest _) (cl-incf refreshed))))
            ;; The default display function refreshes Eldoc, so the
            ;; interactive command, error navigation and automatic
            ;; display keep working
            (flycheck-display-error-at-point)
            (expect refreshed :to-equal 1)))))

    (it "enables eldoc-mode when global-eldoc-mode allows it"
      (flycheck-buttercup-with-temp-buffer
        (let ((global-eldoc-mode t))
          (flycheck-mode 1)
          (expect (bound-and-true-p eldoc-mode) :to-be-truthy)
          (flycheck-mode -1))))

    (it "keeps the display timer off only while eldoc-mode is active"
      (flycheck-buttercup-with-temp-buffer
        ;; `eldoc-mode' refuses to activate without a documentation
        ;; source, so register Flycheck's first
        (flycheck-mode 1)
        (eldoc-mode 1)
        (flycheck-display-error-at-point-soon)
        (expect flycheck-display-error-at-point-timer :not :to-be-truthy)
        (eldoc-mode -1)
        ;; Without eldoc-mode the timer must pick up the slack
        (flycheck-display-error-at-point-soon)
        (expect flycheck-display-error-at-point-timer :to-be-truthy)
        (flycheck-cancel-error-display-error-at-point-timer)
        ;; The old default schedules the timer regardless
        (let ((flycheck-display-errors-function
               #'flycheck-display-error-messages))
          (flycheck-display-error-at-point-soon)
          (expect flycheck-display-error-at-point-timer :to-be-truthy)
          (flycheck-cancel-error-display-error-at-point-timer)))))

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
