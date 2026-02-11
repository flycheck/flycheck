;;; test-executables.el --- Flycheck Specs: Executables -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Executables of command checkers"

  (describe "flycheck-overridden-executable"

    (it "uses overridden executable"
      (let ((flycheck-emacs-lisp-executable (flycheck-buttercup-resource-filename
                                             "bin/dummy-emacs")))
        (when (eq system-type 'darwin)
          (buttercup-skip "Skipped because macOS will take forever to complete the task"))
        (flycheck-buttercup-should-syntax-check
         "language/emacs-lisp/warnings.el" 'emacs-lisp-mode
         '(12 nil info "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc)
         '(17 4 error "t is not true!" :checker emacs-lisp)
         '(19 11 warning "This is a stupid message" :checker emacs-lisp)))))

  (describe "flycheck-set-checker-executable"

    (it "real executable"
      (flycheck-buttercup-with-temp-buffer
        ;; Create a temporary buffer to restrict the scope of
        ;; `flycheck-emacs-lisp-executable'
        (let ((file-name (flycheck-buttercup-resource-filename "bin/dummy-emacs")))
          (expect (file-exists-p file-name) :to-be-truthy)
          (expect (file-executable-p file-name) :to-be-truthy)
          (expect (local-variable-p 'flycheck-emacs-lisp-executable) :not :to-be-truthy)
          (with-no-warnings
            (flycheck-set-checker-executable 'emacs-lisp file-name))
          (expect (local-variable-p 'flycheck-emacs-lisp-executable) :to-be-truthy)
          (expect flycheck-emacs-lisp-executable :to-equal file-name)))
      ;; The global value should remain unaffected
      (expect flycheck-emacs-lisp-executable :not :to-be-truthy))

    (it "no executable given"
      (flycheck-buttercup-with-temp-buffer
        (let ((file-name (flycheck-buttercup-resource-filename "bin/dummy-emacs")))
          (setq-local flycheck-emacs-lisp-executable file-name)
          (expect flycheck-emacs-lisp-executable :to-equal file-name)
          (with-no-warnings
            (flycheck-set-checker-executable 'emacs-lisp))
          (expect flycheck-emacs-lisp-executable :not :to-be-truthy)
          (expect (local-variable-p 'flycheck-emacs-lisp-executable) :to-be-truthy))))

    (it "executable is nil"
      (flycheck-buttercup-with-temp-buffer
        (let ((file-name (flycheck-buttercup-resource-filename "bin/dummy-emacs")))
          (setq-local flycheck-emacs-lisp-executable file-name)
          (expect flycheck-emacs-lisp-executable :to-equal file-name)
          (with-no-warnings
            (flycheck-set-checker-executable 'emacs-lisp nil))
          (expect flycheck-emacs-lisp-executable :not :to-be-truthy)
          (expect (local-variable-p 'flycheck-emacs-lisp-executable) :to-be-truthy))))

    (it "non-existing file"
      (let ((file-name (flycheck-buttercup-resource-filename "no-such-file")))
        (expect (file-exists-p file-name) :not :to-be-truthy)
        (let ((err (should-error (flycheck-set-checker-executable
                                  'emacs-lisp file-name) :type 'user-error)))
          (expect (cadr err) :to-equal (format "%s is no executable" file-name)))))

    (it "file not executable"
      (let ((file-name (flycheck-buttercup-resource-filename "language/emacs-lisp/warnings.el")))
        (expect (file-exists-p file-name) :to-be-truthy)
        (expect (file-executable-p file-name) :not :to-be-truthy)
        (let ((err (should-error (flycheck-set-checker-executable
                                  'emacs-lisp file-name) :type 'user-error)))
          (expect (cadr err) :to-equal (format "%s is no executable" file-name)))))))

;;; test-executables.el ends here
