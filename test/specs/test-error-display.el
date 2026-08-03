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

    (it "does not let the interactive request pop the doc window"
      ;; Documenting interactively is what refreshes the echo area after a
      ;; jump, but `eldoc-display-in-buffer' reads the same flag as a
      ;; request to display the *eldoc* window.  See #2201.
      (flycheck-buttercup-with-temp-buffer
        (let ((asked 'unset))
          (cl-letf (((symbol-function 'eldoc-print-current-symbol-info) #'ignore))
            (flycheck-display-errors-via-eldoc nil))
          ;; Eldoc may only get round to displaying long after the request,
          ;; so the suppression has to outlive it rather than ride on a
          ;; dynamic binding
          (flycheck--eldoc-suppress-doc-window
           (lambda (_docs interactive) (setq asked interactive)) nil t)
          (expect asked :to-be nil))))

    (it "only suppresses the window for the refresh it asked for"
      (flycheck-buttercup-with-temp-buffer
        (let ((asked 'unset))
          ;; nothing pending: an explicit `M-x eldoc' must still display
          (flycheck--eldoc-suppress-doc-window
           (lambda (_docs interactive) (setq asked interactive)) nil t)
          (expect asked :to-be t))))

    (it "suppresses the window once, not for every later display"
      (flycheck-buttercup-with-temp-buffer
        (let ((asked nil))
          (cl-letf (((symbol-function 'eldoc-print-current-symbol-info) #'ignore))
            (flycheck-display-errors-via-eldoc nil))
          (dotimes (_ 2)
            (flycheck--eldoc-suppress-doc-window
             (lambda (_docs interactive) (push interactive asked)) nil t))
          (expect (nreverse asked) :to-equal '(nil t)))))

    (it "asks eldoc to document interactively"
      ;; Left to itself Eldoc keeps out of the echo area unless the
      ;; command that ran is one of `eldoc-message-commands', which
      ;; error navigation is not.  See #2201.
      (let ((interactive 'unset))
        (cl-letf (((symbol-function 'eldoc-print-current-symbol-info)
                   (lambda (&optional arg) (setq interactive arg))))
          (flycheck-display-errors-via-eldoc nil)
          (expect interactive :to-be-truthy))))

    (it "keeps the display timer off only when eldoc refreshes on its own"
      (flycheck-buttercup-with-temp-buffer
        ;; `eldoc-mode' refuses to activate without a documentation
        ;; source, so register Flycheck's first
        (flycheck-mode 1)
        (eldoc-mode 1)
        ;; After ordinary motion Eldoc updates the echo area itself
        (let ((this-command 'forward-char))
          (flycheck-display-error-at-point-soon)
          (expect flycheck-display-error-at-point-timer :not :to-be-truthy))
        ;; After a jump it does not, so Flycheck has to (see #2201)
        (let ((this-command 'flycheck-next-error))
          (flycheck-display-error-at-point-soon)
          (expect flycheck-display-error-at-point-timer :to-be-truthy)
          (flycheck-cancel-error-display-error-at-point-timer))
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

    (it "copies every error at point as one entry"
      ;; One entry, so a single yank pastes the lot; they used to go in
      ;; separately, leaving the rest behind `yank-pop'
      (flycheck-buttercup-with-temp-buffer
        (insert "A test buffer to copy errors from")
        (let ((flycheck-highlighting-mode 'columns) ; Disable Sexps parsing
              (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                            (flycheck-error-new-at 1 10 'warning "2nd message"
                                                   :id "foo"))))
          (mapc #'flycheck-add-overlay errors)
          (flycheck-copy-errors-as-kill 10)
          (expect (car kill-ring) :to-equal "1st message\n2nd message")
          (flycheck-copy-errors-as-kill 10 #'flycheck-error-id)
          (expect (car kill-ring) :to-equal "foo")
          (flycheck-copy-errors-as-kill 10 #'flycheck-error-format-message-and-id)
          (expect (car kill-ring)
                  :to-equal "1st message\n2nd message [foo]"))))

    (it "shows everything it copied, not just the first"
      ;; `nreverse' used to eat the list before `string-join' read it
      (flycheck-buttercup-with-temp-buffer
        (insert "A test buffer to copy errors from")
        (let ((flycheck-highlighting-mode 'columns)
              (shown nil)
              (errors (list (flycheck-error-new-at 1 nil 'error "1st message")
                            (flycheck-error-new-at 1 10 'warning "2nd message"))))
          (mapc #'flycheck-add-overlay errors)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
            (flycheck-copy-errors-as-kill 10))
          (expect shown :to-equal "1st message\n2nd message"))))

    (it "does not treat a message as a format string"
      (flycheck-buttercup-with-temp-buffer
        (insert "A test buffer to copy errors from")
        (let ((flycheck-highlighting-mode 'columns)
              (shown nil)
              (errors (list (flycheck-error-new-at 1 nil 'error "100%% done"))))
          (mapc #'flycheck-add-overlay errors)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
            (flycheck-copy-errors-as-kill 10))
          (expect shown :to-equal "100%% done"))))))

;;; test-error-display.el ends here
