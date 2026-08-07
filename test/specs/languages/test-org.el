;;; test-org.el --- Flycheck Specs: Org -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Org"
  (flycheck-buttercup-def-checker-test org-lint org nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/org/lint.org" 'org-mode
       '(21 nil info "Link to non-existent local file \"nonexistent.org\"" :checker org-lint)
       '(24 nil info "Link to non-existent local file \"/tmp/does-not-exist.org\"" :checker org-lint)
       '(28 nil info "Duplicate CUSTOM_ID property \"duplicate-id-123\"" :checker org-lint)
       '(33 nil info "Duplicate CUSTOM_ID property \"duplicate-id-123\"" :checker org-lint)
       '(40 nil info "Link to non-existent local file \"file with spaces and \\\"quotes\\\".org\"" :checker org-lint))))

  (flycheck-buttercup-def-checker-test org-lint org valid
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/org/valid.org" 'org-mode)))

  (describe "the org-lint checkers that run"
    ;; invalid-id-link rescans every org-id file in the session on each
    ;; invocation, in the main Emacs process; with an org-roam-sized
    ;; corpus that is tens of seconds per idle pause.  See #2161.

    (it "keeps the disabled checkers away from org-lint"
      (require 'org-lint)
      (let ((seen 'unset))
        (cl-letf (((symbol-function 'org-lint)
                   (lambda (&rest _) (setq seen org-lint--checkers) nil)))
          (flycheck-buttercup-with-temp-buffer
            (insert "* Heading\n")
            (org-mode)
            (funcall (flycheck-checker-get 'org-lint 'start)
                     'org-lint #'ignore)))
        (expect (seq-some (lambda (c)
                            (eq (org-lint-checker-name c) 'invalid-id-link))
                          seen)
                :to-be nil)
        ;; and it only removed, not replaced, the rest
        (expect (length seen)
                :to-equal (- (length org-lint--checkers)
                             (length flycheck-org-lint-disabled-checkers)))))

    (it "runs everything when nothing is disabled"
      (require 'org-lint)
      (let ((seen 'unset)
            (flycheck-org-lint-disabled-checkers nil))
        (cl-letf (((symbol-function 'org-lint)
                   (lambda (&rest _) (setq seen org-lint--checkers) nil)))
          (flycheck-buttercup-with-temp-buffer
            (insert "* Heading\n")
            (org-mode)
            (funcall (flycheck-checker-get 'org-lint 'start)
                     'org-lint #'ignore)))
        (expect (length seen) :to-equal (length org-lint--checkers))))))

;;; test-org.el ends here
