;;; test-automatic-checking.el --- Flycheck Specs: Automatic Checking -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Automatic syntax checking"

  (describe "flycheck-may-check-automatically"

    (it "not in ephemeral buffers"
      (flycheck-buttercup-with-temp-buffer
        (expect (seq-find #'flycheck-may-check-automatically
                          '(save idle-change new-line mode-enabled))
                :not :to-be-truthy)
        (expect (flycheck-may-check-automatically) :not :to-be-truthy)))

    (it "in normal buffers"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (expect (seq-every-p #'flycheck-may-check-automatically
                             '(save idle-change new-line mode-enabled))
                :to-be-truthy)
        (expect (flycheck-may-check-automatically) :to-be-truthy)))

    (it "automatic checking disabled"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (let ((flycheck-check-syntax-automatically nil))
          (expect (seq-find #'flycheck-may-check-automatically
                            '(save idle-change new-line mode-enabled))
                  :not :to-be-truthy)
          (expect (flycheck-may-check-automatically) :to-be-truthy))))

    (it "specific event disabled"
      (dolist (event '(save idle-change new-line mode-enabled))
        (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
          ;; Disable just a specific event
          (let ((flycheck-check-syntax-automatically
                 (remq event flycheck-check-syntax-automatically)))
            (expect flycheck-check-syntax-automatically :to-be-truthy)
            (expect (flycheck-may-check-automatically event) :not :to-be-truthy)
            (expect (seq-every-p #'flycheck-may-check-automatically
                                 flycheck-check-syntax-automatically)
                    :to-be-truthy)
            (expect (flycheck-may-check-automatically) :to-be-truthy))))))

  (describe "flycheck-check-syntax-automatically"

    (it "mode-enabled is disabled"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (emacs-lisp-mode)
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)
        (let ((flycheck-check-syntax-automatically
               (remq 'mode-enabled flycheck-check-syntax-automatically)))
          (flycheck-mode)
          (expect (flycheck-deferred-check-p) :not :to-be-truthy))))

    (it "mode-enabled checks syntax after flycheck-mode"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (emacs-lisp-mode)
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)
        (let ((flycheck-check-syntax-automatically '(mode-enabled)))
          (flycheck-mode)
          (expect (flycheck-deferred-check-p) :to-be-truthy))))

    (it "idle-change is disabled"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (emacs-lisp-mode)
        (let ((flycheck-check-syntax-automatically nil))
          (flycheck-mode))
        (let ((flycheck-check-syntax-automatically
               (remq 'idle-change flycheck-check-syntax-automatically)))
          (insert "Hello world")
          (sleep-for 0.55)
          (expect (flycheck-deferred-check-p) :not :to-be-truthy))))

    (it "idle-change checks syntax after change"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (emacs-lisp-mode)
        (let ((flycheck-check-syntax-automatically '(idle-change)))
          (flycheck-mode)
          (insert "Hello world")
          (expect (flycheck-deferred-check-p) :not :to-be-truthy)
          (sleep-for 1)
          (expect (flycheck-deferred-check-p) :to-be-truthy))))

    (it "idle-change does not check before delay"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (emacs-lisp-mode)
        (let ((flycheck-check-syntax-automatically '(idle-change))
              (flycheck-idle-change-delay 1.5))
          (flycheck-mode)
          (insert "Hello world")
          (sleep-for 0.55)
          (expect (flycheck-deferred-check-p) :not :to-be-truthy)
          (sleep-for 1.1)
          (expect (flycheck-deferred-check-p) :to-be-truthy))))

    (it "idle-change checks changed buffer"
      (let ((flycheck-check-syntax-automatically '(idle-change))
            (flycheck-idle-change-delay 0.1))
        (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
          (let ((changed-buffer (current-buffer)))
            (emacs-lisp-mode)
            (flycheck-mode)
            (insert "Hello world")
            (switch-to-buffer "other-dummy2.el")
            (emacs-lisp-mode)
            (flycheck-mode)
            (sleep-for 0.2)
            (expect (flycheck-deferred-check-p) :not :to-be-truthy)
            (set-buffer changed-buffer)
            (sleep-for 1)
            (expect (flycheck-deferred-check-p) :to-be-truthy)))))

    (it "does not check after buffer switch by default"
      (let ((flycheck-check-syntax-automatically '())
            (flycheck-idle-buffer-switch-delay 0)
            (checks 0))
        (find-file (flycheck-buttercup-resource-filename "automatic-check-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

        (switch-to-buffer "*scratch*")
        (switch-to-buffer "automatic-check-dummy.el")
        (sleep-for 0.1)
        (expect checks :to-equal 0)
        (kill-buffer "automatic-check-dummy.el")))

    (it "idle-buffer-switch checks after buffer switch"
      (let ((flycheck-check-syntax-automatically '(idle-buffer-switch))
            (flycheck-idle-buffer-switch-delay 0)
            (checks 0))
        (find-file (flycheck-buttercup-resource-filename "automatic-check-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

        (switch-to-buffer "*scratch*")
        (switch-to-buffer "automatic-check-dummy.el")
        (sleep-for 0.1)
        (expect checks :to-equal 1)
        (kill-buffer "automatic-check-dummy.el")))

    (it "idle-change cancels idle-buffer-switch"
      (let ((flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
            (flycheck-idle-change-delay 0.02)
            (flycheck-idle-buffer-switch-delay 0.01)
            (checks 0))
        (find-file (flycheck-buttercup-resource-filename "automatic-check-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

        (switch-to-buffer "*scratch*")
        (switch-to-buffer "automatic-check-dummy.el")
        (insert "Hello")
        (sleep-for 0.015)
        (expect checks :to-equal 0)
        (sleep-for 1)
        (expect checks :to-equal 1)
        (kill-buffer "automatic-check-dummy.el")))

    (it "idle-buffer-switch cancels idle-change"
      (let ((flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
            (flycheck-idle-change-delay 0.01)
            (flycheck-idle-buffer-switch-delay 0.05)
            (checks 0))
        (find-file (flycheck-buttercup-resource-filename "automatic-check-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

        (insert "Hello")
        (switch-to-buffer "*scratch*")
        (switch-to-buffer "automatic-check-dummy.el")
        (sleep-for 0.02)
        (expect checks :to-equal 0)
        (sleep-for 1)
        (expect checks :to-equal 1)
        (kill-buffer "automatic-check-dummy.el")))

    (it "idle-buffer-switch does not check intermediate buffers by default"
      (let ((flycheck-check-syntax-automatically '(idle-buffer-switch))
            (flycheck-idle-buffer-switch-delay 0.01)
            (checks 0))
        (find-file (flycheck-buttercup-resource-filename "automatic-check-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (find-file (flycheck-buttercup-resource-filename "global-mode-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

        (switch-to-buffer "*scratch*")
        (switch-to-buffer "automatic-check-dummy.el")
        (switch-to-buffer "global-mode-dummy.el")
        (sleep-for 1)
        (expect checks :to-equal 1)
        ;; Since the buffer is not visible, the check would be automatically deferred
        (set-buffer "automatic-check-dummy.el")
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)
        (kill-buffer "automatic-check-dummy.el")
        (kill-buffer "global-mode-dummy.el")))

    (it "idle-buffer-switch checks intermediate buffers with option"
      (let ((flycheck-check-syntax-automatically '(idle-buffer-switch))
            (flycheck-idle-buffer-switch-delay 0.01)
            (flycheck-buffer-switch-check-intermediate-buffers t)
            (checks 0))
        (find-file (flycheck-buttercup-resource-filename "automatic-check-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (find-file (flycheck-buttercup-resource-filename "global-mode-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (cl-incf checks)) nil 'local)

        (switch-to-buffer "*scratch*")
        (switch-to-buffer "automatic-check-dummy.el")
        (switch-to-buffer "global-mode-dummy.el")
        (sleep-for 0.1)
        (expect checks :to-equal 1)
        ;; Since the buffer is not visible, the check will be automatically deferred
        (set-buffer "automatic-check-dummy.el")
        (expect (flycheck-deferred-check-p) :to-be-truthy)
        (kill-buffer "automatic-check-dummy.el")
        (kill-buffer "global-mode-dummy.el")))

    (it "buffer-switch-check-intermediate-buffers does not cancel idle-change"
      (let ((flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
            (flycheck-buffer-switch-check-intermediate-buffers t)
            (flycheck-idle-change-delay 0.01)
            (flycheck-idle-buffer-switch-delay 0))
        (find-file (flycheck-buttercup-resource-filename "automatic-check-dummy.el"))
        (emacs-lisp-mode)
        (flycheck-mode)

        (insert "Hello")
        (switch-to-buffer "*scratch*")
        (sleep-for 0.015)
        ;; Since the buffer is not visible, the check will be automatically deferred
        (set-buffer "automatic-check-dummy.el")
        (expect (flycheck-deferred-check-p) :to-be-truthy)
        (kill-buffer "automatic-check-dummy.el")))

    (it "new-line is disabled"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (let ((flycheck-check-syntax-automatically nil))
          (flycheck-mode))
        (let ((flycheck-check-syntax-automatically
               (remq 'new-line flycheck-check-syntax-automatically)))
          (insert "\n")
          (expect (flycheck-deferred-check-p) :not :to-be-truthy))))

    (it "new-line checks syntax after new line"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (let ((flycheck-check-syntax-automatically '(new-line)))
          (flycheck-mode)
          (insert "\n")
          (expect (flycheck-deferred-check-p) :to-be-truthy))))

    (it "save disabled"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (let ((flycheck-check-syntax-automatically nil))
          (flycheck-mode))
        (set-buffer-modified-p t)
        (let ((flycheck-check-syntax-automatically
               (remq 'save flycheck-check-syntax-automatically)))
          (save-buffer 0)
          (expect (flycheck-deferred-check-p) :not :to-be-truthy))))

    (it "save checks syntax after save"
      (flycheck-buttercup-with-resource-buffer "automatic-check-dummy.el"
        (let ((flycheck-check-syntax-automatically '(save)))
          (flycheck-mode)
          (set-buffer-modified-p t)
          (save-buffer 0)
          (expect (flycheck-deferred-check-p) :to-be-truthy)))))

  (describe "flycheck-buffer-automatically"

    (it "does not check with disabled mode"
      (flycheck-buttercup-with-temp-buffer
        (rename-buffer "foo")
        (expect flycheck-mode :not :to-be-truthy)
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)
        (flycheck-buffer-automatically)
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)))

    (it "defers the test"
      (flycheck-buttercup-with-temp-buffer
        (flycheck-mode)
        ;; Flycheck won't check ephemeral buffers
        (rename-buffer "foo")
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)
        (flycheck-buffer-automatically)
        (expect (flycheck-deferred-check-p) :to-be-truthy))))

  (describe "Deferred syntax checking"

    (it "flycheck-deferred-check-p returns nil"
      (let ((flycheck-deferred-syntax-check nil))
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)))

    (it "flycheck-deferred-check-p returns truthy"
      (let ((flycheck-deferred-syntax-check t))
        (expect (flycheck-deferred-check-p) :to-be-truthy)))

    (it "flycheck-buffer-deferred schedules a deferred syntax check"
      (flycheck-buttercup-with-temp-buffer
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)
        (flycheck-buffer-deferred)
        (expect (flycheck-deferred-check-p) :to-be-truthy)))

    (it "flycheck-clean-deferred-check removes a deferred syntax check"
      (flycheck-buttercup-with-temp-buffer
        (flycheck-buffer-deferred)
        (flycheck-clean-deferred-check)
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)))))

;;; test-automatic-checking.el ends here
