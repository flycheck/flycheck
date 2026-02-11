;;; test-command-checker.el --- Flycheck Specs: Command Checker -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)
(require 'shut-up)

(describe "Command checker"

  (describe "flycheck-command-argument-p"

    (it "with symbols"
      (dolist (symbol '(source
                        source-inplace
                        source-original
                        temporary-directory
                        temporary-file-name
                        null-device))
        (expect (flycheck-command-argument-p symbol) :to-be-truthy)))

    (it "config-file with variable symbol"
      (expect (flycheck-command-argument-p '(config-file "foo" bar)) :to-be-truthy))

    (it "config-file with quoted variable symbol"
      (expect (flycheck-command-argument-p '(config-file "foo" 'bar)) :not :to-be-truthy))

    (it "config-file without variable symbol"
      (expect (flycheck-command-argument-p '(config-file "foo")) :not :to-be-truthy))

    (it "option without filter"
      (expect (flycheck-command-argument-p '(option "foo" bar)) :to-be-truthy))

    (it "option with filter"
      (expect (flycheck-command-argument-p '(option "foo" bar filter)) :to-be-truthy))

    (it "option with quoted variable symbol"
      (expect (flycheck-command-argument-p '(option "foo" 'bar)) :not :to-be-truthy))

    (it "option with quoted filter symbol"
      (expect (flycheck-command-argument-p '(option "foo" bar 'filter)) :not :to-be-truthy))

    (it "option without variable"
      (expect (flycheck-command-argument-p '(option "foo")) :not :to-be-truthy))

    (it "option-list without filter and prepender"
      (expect (flycheck-command-argument-p '(option-list "foo" bar)) :to-be-truthy))

    (it "option-list with prepender"
      (expect (flycheck-command-argument-p '(option-list "foo" bar prepend-fn)) :to-be-truthy))

    (it "option-list with prepender and filter"
      (expect (flycheck-command-argument-p '(option-list "foo" bar prepend-fn filter)) :to-be-truthy))

    (it "option-list with quoted variable symbol"
      (expect (flycheck-command-argument-p '(option-list "foo" 'bar)) :not :to-be-truthy))

    (it "option-list with quoted prepender symbol"
      (expect (flycheck-command-argument-p '(option-list "foo" bar 'prepend-fn)) :not :to-be-truthy))

    (it "option-list with quoted filter symbol"
      (expect (flycheck-command-argument-p '(option-list "foo" bar prepend-fn 'filter)) :not :to-be-truthy))

    (it "option-list without variable symbol"
      (expect (flycheck-command-argument-p '(option-list "foo")) :not :to-be-truthy))

    (it "eval with variable"
      (expect (flycheck-command-argument-p '(eval bar)) :to-be-truthy))

    (it "eval with function call"
      (expect (flycheck-command-argument-p '(eval (spam "with eggs"))) :to-be-truthy))

    (it "eval with no form"
      (expect (flycheck-command-argument-p '(eval)) :not :to-be-truthy))

    (it "eval with multiple forms"
      (expect (flycheck-command-argument-p '(eval foo bar)) :not :to-be-truthy))

    (it "integer literal"
      (expect (flycheck-command-argument-p 100) :not :to-be-truthy))

    (it "unknown argument symbol"
      (expect (flycheck-command-argument-p 'foo) :not :to-be-truthy))

    (it "unknown argument cell"
      (expect (flycheck-command-argument-p '(foo bar)) :not :to-be-truthy)))

  (describe "flycheck-start-command-checker"

    (it "wraps command"
      (let* ((was-called 0)
             (flycheck-command-wrapper-function (lambda (cmd)
                                                  (cl-incf was-called)
                                                  (cons "echo" cmd))))
        ;; Since we just `echo' the command, there should be zero errors
        (flycheck-buttercup-should-syntax-check
         "language/emacs-lisp/warnings.el" 'emacs-lisp-mode)

        ;; Called once for `emacs-lisp', and a second time for checkdoc
        (expect was-called :to-equal 2)))

    (it "truncated stdin with errors"
      ;; Skip on Emacs 28 where large pipe writes can cause SIGPIPE
      (assume (version<= "29" emacs-version))
      (cl-letf* ((flycheck-checker 'truncated-stdin)
                 ((symbol-plist 'truncated-stdin) flycheck-test--truncated-stdin))
        (dolist (buffer-size '(4095 65537))
          ;; As long as the checker reports at least one error closing stdin early
          ;; isn't an issue.
          (flycheck-buttercup-with-temp-buffer
            (truncated-stdin-mode)
            (insert (make-string buffer-size ?a))
            (flycheck-buttercup-should-syntax-check-in-buffer
             '(1 1 error "error" :checker truncated-stdin))))))

    (it "truncated stdin without errors"
      ;; Skip on Emacs 28 where large pipe writes can cause SIGPIPE
      (assume (version<= "29" emacs-version))
      (cl-letf* ((flycheck-checker 'truncated-stdin)
                 ((symbol-plist 'truncated-stdin) flycheck-test--truncated-stdin)
                 ((flycheck-checker-get 'truncated-stdin 'error-patterns)
                  '(("\\`_\\`" . error))))
        ;; If the checker closes stdin early without reporting errors, something
        ;; might have gone wrong, so warn the user.
        (flycheck-buttercup-with-temp-buffer
          (truncated-stdin-mode)
          (insert (make-string 65537 ?\n))
          (expect (shut-up (flycheck-buttercup-should-syntax-check-in-buffer))
                  :to-throw 'flycheck-buttercup-suspicious-checker))))))

;;; test-command-checker.el ends here
