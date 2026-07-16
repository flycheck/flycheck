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
                  :to-throw 'flycheck-buttercup-suspicious-checker))))

    (it "reports errors returned by the :handle-suspicious function"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'suspicious-exit)
                 ((symbol-plist 'suspicious-exit)
                  flycheck-test--suspicious-exit)
                 ((flycheck-checker-get 'suspicious-exit 'handle-suspicious)
                  (lambda (checker exit-status output)
                    (list (flycheck-error-new-at
                           1 1 'error
                           (format "exited with %d: %s" exit-status
                                   (string-trim output))
                           :checker checker)))))
        (flycheck-buttercup-with-temp-buffer
          (suspicious-exit-mode)
          (insert "hello\n")
          (flycheck-buttercup-should-syntax-check-in-buffer
           '(1 1 error "exited with 1: tool exploded"
               :checker suspicious-exit)))))

    (it "stays suspicious when the :handle-suspicious function declines"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'suspicious-exit)
                 ((symbol-plist 'suspicious-exit)
                  flycheck-test--suspicious-exit)
                 ((flycheck-checker-get 'suspicious-exit 'handle-suspicious)
                  (lambda (_checker _exit-status _output) 'suspicious)))
        (flycheck-buttercup-with-temp-buffer
          (suspicious-exit-mode)
          (insert "hello\n")
          (expect (shut-up (flycheck-buttercup-should-syntax-check-in-buffer))
                  :to-throw 'flycheck-buttercup-suspicious-checker))))

    (it "treats a non-zero exit without errors as suspicious by default"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'suspicious-exit)
                 ((symbol-plist 'suspicious-exit)
                  flycheck-test--suspicious-exit))
        (flycheck-buttercup-with-temp-buffer
          (suspicious-exit-mode)
          (insert "hello\n")
          (expect (shut-up (flycheck-buttercup-should-syntax-check-in-buffer))
                  :to-throw 'flycheck-buttercup-suspicious-checker))))
    (it "truncates excessive errors keeping the most severe"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'many-errors)
                 ((symbol-plist 'many-errors) flycheck-test--many-errors)
                 (flycheck-checker-error-threshold 3))
        (flycheck-buttercup-with-temp-buffer
          (many-errors-mode)
          (insert "hello\n")
          (flycheck-mode)
          (shut-up (flycheck-buttercup-buffer-sync))
          (expect (length flycheck-current-errors) :to-equal 3)
          ;; The two error-level errors outrank the six infos, and among
          ;; the infos the one closest to the top of the buffer survives
          (expect (seq-sort #'< (seq-map #'flycheck-error-line
                                         flycheck-current-errors))
                  :to-equal '(1 7 8))
          (expect flycheck--suppressed-error-count :to-equal 5)
          (expect (flycheck-mode-line-status-text) :to-match (rx "2|0|1+" eos))
          (expect (flycheck-automatically-disabled-checker-p 'many-errors)
                  :not :to-be-truthy)
          ;; Under the threshold everything is reported again
          (let ((flycheck-checker-error-threshold 100))
            (flycheck-buttercup-buffer-sync))
          (expect (length flycheck-current-errors) :to-equal 8)
          (expect flycheck--suppressed-error-count :to-equal 0))))

    (it "disables the checker over the threshold with the disable action"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'many-errors)
                 ((symbol-plist 'many-errors) flycheck-test--many-errors)
                 (flycheck-checker-error-threshold 3)
                 (flycheck-checker-error-threshold-action 'disable))
        (flycheck-buttercup-with-temp-buffer
          (many-errors-mode)
          (insert "hello\n")
          (flycheck-mode)
          (shut-up (flycheck-buttercup-buffer-sync))
          (expect flycheck-current-errors :not :to-be-truthy)
          (expect (flycheck-automatically-disabled-checker-p 'many-errors)
                  :to-be-truthy))))

    (it "notifies only when a checker newly exceeds the threshold"
      (flycheck-buttercup-with-temp-buffer
        (let ((flycheck-checker-error-threshold 1)
              (messages 0)
              (errs (list (flycheck-error-new-at 1 1 'info "spam")
                          (flycheck-error-new-at 2 1 'error "boom"))))
          (cl-letf (((symbol-function 'message)
                     (lambda (&rest _) (cl-incf messages))))
            ;; Truncation keeps the most severe error
            (let ((kept (flycheck--truncate-excessive-errors 'foo errs 2)))
              (expect (mapcar #'flycheck-error-level kept) :to-equal '(error)))
            ;; Repeated checks over the threshold notify only once
            (flycheck--truncate-excessive-errors 'foo errs 2)
            (expect messages :to-equal 1)
            ;; Passing under the threshold re-arms the notification
            (flycheck--handle-excessive-errors 'foo (cdr errs))
            (flycheck--truncate-excessive-errors 'foo errs 2)
            (expect messages :to-equal 2)))))

    (it "shows the suppressed error count in the error list mode line"
      (flycheck-buttercup-with-temp-buffer
        (setq flycheck--suppressed-error-count 5)
        (let ((source (current-buffer)))
          (with-temp-buffer
            (setq flycheck-error-list-source-buffer source)
            (expect (flycheck-error-list-mode-line-suppressed-indicator)
                    :to-equal " (+5 suppressed)")))))

    (it "shows no suppressed count without suppressed errors"
      (flycheck-buttercup-with-temp-buffer
        (let ((source (current-buffer)))
          (with-temp-buffer
            (setq flycheck-error-list-source-buffer source)
            (expect (flycheck-error-list-mode-line-suppressed-indicator)
                    :to-equal "")))))

    (it "forces English messages without overriding the character set"
      ;; LC_MESSAGES=C gives English output, while LC_ALL is left alone so
      ;; that the subprocess keeps the user's LC_CTYPE (see #2170).
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'print-locale)
                 ((symbol-plist 'print-locale) flycheck-test--print-locale))
        (flycheck-buttercup-with-env '(("LC_MESSAGES" . nil) ("LC_ALL" . nil))
          (flycheck-buttercup-with-temp-buffer
            (print-locale-mode)
            (insert "dummy\n")
            (flycheck-buttercup-should-syntax-check-in-buffer
             '(1 1 error "LC_MESSAGES=C LC_ALL=" :checker print-locale))))))))

;;; test-command-checker.el ends here
