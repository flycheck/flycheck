;;; test-command-checker.el --- Flycheck Specs: Command Checker -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)
(require 'shut-up)

(defmacro flycheck-test--with-fake-running-check (checker properties &rest body)
  "Run BODY with a fake running check by CHECKER defined with PROPERTIES.

Define CHECKER as a generic syntax checker with PROPERTIES (a
quoted plist), install a fake syntax check for it as
`flycheck-current-syntax-check', and clean everything up
afterwards."
  (declare (indent 2))
  `(flycheck-buttercup-with-temp-buffer
     (apply #'flycheck-define-generic-checker ,checker
            "A fake checker for interruption specs." ,properties)
     (unwind-protect
         (progn
           (setq flycheck-current-syntax-check
                 (flycheck-syntax-check-new
                  :buffer (current-buffer)
                  :checker ,checker
                  :context nil
                  :working-directory default-directory))
           ,@body)
       (setq flycheck-current-syntax-check nil)
       (flycheck-clean-deferred-check)
       (setf (symbol-plist ,checker) nil))))

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

    (it "disables the checker when the :handle-suspicious function says so"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'suspicious-exit)
                 ((symbol-plist 'suspicious-exit)
                  flycheck-test--suspicious-exit)
                 ((flycheck-checker-get 'suspicious-exit 'handle-suspicious)
                  (lambda (_checker _exit-status _output) 'disable)))
        (flycheck-buttercup-with-temp-buffer
          (suspicious-exit-mode)
          (insert "hello\n")
          ;; The check finishes cleanly with no errors and no suspicious
          ;; warning, and the checker is disabled like a failed :enabled
          (flycheck-buttercup-should-syntax-check-in-buffer)
          (expect (flycheck-automatically-disabled-checker-p 'suspicious-exit)
                  :to-be-truthy))))

    (it "selects the fallback checker after a self-disable"
      (assume (or (executable-find "python3") (executable-find "python")))
      (flycheck-buttercup-with-temp-buffer
        (rename-buffer "flycheck-self-disable-test")
        (cl-letf* (((symbol-plist 'suspicious-exit)
                    flycheck-test--suspicious-exit)
                   ((flycheck-checker-get 'suspicious-exit
                                          'handle-suspicious)
                    (lambda (_checker _exit-status _output) 'disable))
                   ((symbol-function 'get-buffer-window)
                    (lambda (&rest _) (selected-window))))
          (flycheck-define-generic-checker 'fallback-checker
            "Report a fixed warning synchronously."
            :start (lambda (checker callback)
                     (funcall callback 'finished
                              (list (flycheck-error-new-at
                                     1 1 'warning "fallback ran"
                                     :checker checker))))
            :modes '(suspicious-exit-mode))
          (unwind-protect
              (let ((flycheck-checkers '(suspicious-exit fallback-checker)))
                (suspicious-exit-mode)
                (insert "hello\n")
                (flycheck-mode)
                (shut-up (flycheck-buttercup-buffer-sync))
                ;; The self-disabled checker is out of the running...
                (expect (flycheck-automatically-disabled-checker-p
                         'suspicious-exit)
                        :to-be-truthy)
                ;; ...so the next check picks the fallback checker
                (expect (flycheck-get-checker-for-buffer)
                        :to-be 'fallback-checker)
                (shut-up (flycheck-buttercup-buffer-sync))
                (expect (mapcar #'flycheck-error-message
                                flycheck-current-errors)
                        :to-equal '("fallback ran")))
            (setf (symbol-plist 'fallback-checker) nil)))))

    (it "does not run the chain of a self-disabled checker"
      (assume (or (executable-find "python3") (executable-find "python")))
      (flycheck-buttercup-with-temp-buffer
        (rename-buffer "flycheck-chain-skip-test")
        (cl-letf* (((symbol-plist 'suspicious-exit)
                    flycheck-test--suspicious-exit)
                   ((flycheck-checker-get 'suspicious-exit
                                          'handle-suspicious)
                    (lambda (_checker _exit-status _output) 'disable))
                   ((flycheck-checker-get 'suspicious-exit 'next-checkers)
                    '(chain-target))
                   ((symbol-function 'get-buffer-window)
                    (lambda (&rest _) (selected-window))))
          (flycheck-define-generic-checker 'chain-target
            "Report a fixed error synchronously."
            :start (lambda (checker callback)
                     (funcall callback 'finished
                              (list (flycheck-error-new-at
                                     1 1 'error "chain ran"
                                     :checker checker))))
            :modes '(suspicious-exit-mode))
          (unwind-protect
              (let ((flycheck-checkers '(suspicious-exit)))
                (suspicious-exit-mode)
                (insert "hello\n")
                (flycheck-mode)
                (shut-up (flycheck-buttercup-buffer-sync))
                ;; The disabled checker's own chain must not run
                (expect (flycheck-automatically-disabled-checker-p
                         'suspicious-exit)
                        :to-be-truthy)
                (expect flycheck-current-errors :not :to-be-truthy))
            (setf (symbol-plist 'chain-target) nil)))))

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

    (it "interrupts a running check when a new one starts"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'slow-checker)
                 ((symbol-plist 'slow-checker) flycheck-test--slow-checker))
        (flycheck-buttercup-with-temp-buffer
          (slow-checker-mode)
          (insert "hello\n")
          (flycheck-mode)
          (unwind-protect
              (progn
                (flycheck-buffer)
                (expect (flycheck-running-p) :to-be-truthy)
                (let* ((first-check flycheck-current-syntax-check)
                       (first-process
                        (flycheck-syntax-check-context first-check)))
                  ;; Only interactive invocations interrupt
                  (funcall-interactively #'flycheck-buffer)
                  ;; The second check interrupted the first and took over,
                  ;; and the first check's process is dead
                  (expect (flycheck-running-p) :to-be-truthy)
                  (expect flycheck-current-syntax-check
                          :not :to-be first-check)
                  (expect (process-live-p first-process)
                          :not :to-be-truthy)))
            (flycheck-stop)))))

    (it "does not interrupt a running check when configured not to"
      (assume (or (executable-find "python3") (executable-find "python")))
      (cl-letf* ((flycheck-checker 'slow-checker)
                 ((symbol-plist 'slow-checker) flycheck-test--slow-checker)
                 (flycheck-interrupt-running-checks nil))
        (flycheck-buttercup-with-temp-buffer
          (slow-checker-mode)
          (insert "hello\n")
          (flycheck-mode)
          (unwind-protect
              (progn
                (flycheck-buffer)
                (let ((first-check flycheck-current-syntax-check))
                  ;; The running check keeps running even for an
                  ;; interactive invocation; the new check is a no-op
                  (funcall-interactively #'flycheck-buffer)
                  (expect flycheck-current-syntax-check :to-be first-check)
                  ;; Lisp calls never interrupt to begin with
                  (let ((flycheck-interrupt-running-checks t))
                    (flycheck-buffer)
                    (expect flycheck-current-syntax-check
                            :to-be first-check))))
            (flycheck-stop)))))

    (it "ignores reports from an interrupt that fires synchronously"
      ;; `delete-process' can run the process sentinel inline, so the
      ;; interrupted check's status report must not clear the buffer state
      (flycheck-buttercup-with-temp-buffer
        (let (captured-callback)
          (flycheck-define-generic-checker 'sync-interrupt
            "Capture the callback; report synchronously on interruption."
            :start (lambda (_checker callback)
                     (setq captured-callback callback))
            :interrupt (lambda (_checker _context)
                         (funcall captured-callback 'errored "interrupted"))
            :modes '(prog-mode))
          (unwind-protect
              (progn
                (setq flycheck-current-syntax-check
                      (flycheck-syntax-check-new
                       :buffer (current-buffer)
                       :checker 'sync-interrupt
                       :context nil
                       :working-directory default-directory))
                (setq captured-callback
                      (flycheck-buffer-status-callback
                       flycheck-current-syntax-check))
                (setq flycheck-current-errors
                      (list (flycheck-error-new-at 1 1 'error "keep me")))
                ;; The restart path interrupts via the internal function:
                ;; the synchronous 'errored report must be ignored, and
                ;; the displayed errors survive for the replacement check
                (flycheck--interrupt-current-syntax-check)
                (expect flycheck-current-errors :to-be-truthy)
                (expect flycheck-last-status-change :not :to-be 'errored)
                ;; A genuine stop clears deterministically, like older
                ;; Flycheck versions did via the inline sentinel report
                (setq flycheck-current-syntax-check
                      (flycheck-syntax-check-new
                       :buffer (current-buffer)
                       :checker 'sync-interrupt
                       :context nil
                       :working-directory default-directory))
                (setq captured-callback
                      (flycheck-buffer-status-callback
                       flycheck-current-syntax-check))
                (flycheck-stop)
                (expect flycheck-current-errors :not :to-be-truthy)
                (expect flycheck-last-status-change :to-be 'interrupted))
            (setf (symbol-plist 'sync-interrupt) nil)))))

    (it "does not interrupt checkers without an interrupt function"
      (flycheck-test--with-fake-running-check 'no-interrupt
          '(:start ignore :modes (prog-mode))
        (expect (flycheck--interruptible-check-p) :not :to-be-truthy)
        (let ((first-check flycheck-current-syntax-check)
              (flycheck-mode t))
          ;; Even an interactive check must not stop it and pile up a
          ;; second concurrent one
          (funcall-interactively #'flycheck-buffer)
          (expect flycheck-current-syntax-check :to-be first-check))))

    (it "only interrupts at content-changing trigger conditions"
      (flycheck-test--with-fake-running-check 'gate-checker
          '(:start ignore :interrupt ignore :modes (prog-mode))
        (cl-flet ((may-p (condition &optional option)
                    (let ((flycheck-interrupt-running-checks
                           (or option t)))
                      (flycheck--may-interrupt-at-condition-p condition))))
          (expect (may-p 'idle-change) :to-be-truthy)
          (expect (may-p 'save) :to-be-truthy)
          (expect (may-p '(idle-buffer-switch idle-change)) :to-be-truthy)
          ;; Per-keystroke triggers coalesce behind the running check
          (expect (may-p 'new-line) :not :to-be-truthy)
          ;; Buffer switches don't change the contents, so the running
          ;; check's results are not stale
          (expect (may-p 'idle-buffer-switch) :not :to-be-truthy)
          ;; The deferred-check drain passes no condition
          (expect (may-p nil) :not :to-be-truthy)
          ;; Numeric option: young checks are interruptible...
          (expect (may-p 'idle-change 10) :to-be-truthy)
          ;; ...checks that made real progress are protected
          (setf (flycheck-syntax-check-start-time
                 flycheck-current-syntax-check)
                (- (float-time) 60))
          (expect (may-p 'idle-change 10) :not :to-be-truthy))
        (let ((flycheck-interrupt-running-checks nil))
          (expect (flycheck--may-interrupt-at-condition-p 'idle-change)
                  :not :to-be-truthy))))

    (it "defers the new check when the running one is kept"
      (flycheck-test--with-fake-running-check 'keeper
          '(:start ignore :modes (prog-mode))
        (rename-buffer "flycheck-keeper-test")
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) (selected-window))))
          (let ((first-check flycheck-current-syntax-check)
                (flycheck-mode t))
            (flycheck-buffer-automatically 'idle-change)
            ;; No :interrupt function: the running check is kept and the
            ;; new one queued behind it
            (expect flycheck-current-syntax-check :to-be first-check)
            (expect (flycheck-deferred-check-p) :to-be-truthy)))))

    (it "keeps the chain's start time across chained checkers"
      (flycheck-test--with-fake-running-check 'chain-link
          '(:start ignore :modes (prog-mode))
        (setf (flycheck-syntax-check-start-time flycheck-current-syntax-check)
              1000.0)
        ;; Starting the next chain link inherits the start time of the
        ;; running check, so the interruption age limit covers the whole
        ;; chain
        (flycheck-start-current-syntax-check 'chain-link)
        (expect (flycheck-syntax-check-start-time
                 flycheck-current-syntax-check)
                :to-equal 1000.0)))

    (it "keeps the running check when the buffer is not visible"
      (flycheck-test--with-fake-running-check 'invisible-checker
          '(:start ignore :interrupt ignore :modes (prog-mode))
        (rename-buffer "flycheck-invisible-test")
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) nil)))
          (let ((first-check flycheck-current-syntax-check)
                (flycheck-mode t))
            ;; The replacement could only be deferred, so the running
            ;; check must not be killed
            (flycheck-buffer-automatically 'save)
            (expect flycheck-current-syntax-check :to-be first-check)
            (expect (flycheck-deferred-check-p) :to-be-truthy)))))

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
