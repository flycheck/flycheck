;;; test-rpm.el --- Flycheck Specs: RPM -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language RPM"
  (flycheck-buttercup-def-checker-test rpm-rpmlint rpm nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/rpm.spec" '(sh-mode rpm-spec-mode)
       '(1 nil warning "no-cleaning-of-buildroot %clean" :checker rpm-rpmlint)
       '(1 nil warning "no-cleaning-of-buildroot %install" :checker rpm-rpmlint)
       '(1 nil warning "no-buildroot-tag" :checker rpm-rpmlint)
       '(7 nil error "buildarch-instead-of-exclusivearch-tag x86_64"
           :checker rpm-rpmlint)
       '(22 nil warning "macro-in-%changelog %{_bindir}" :checker rpm-rpmlint))))

  (describe "the rpmlint explainer"
    (it "asks rpmlint 2 to explain the message's id"
      (spy-on 'flycheck-call-checker-process-for-output
              :and-return-value "explanation")
      (expect (funcall (flycheck-checker-get 'rpm-rpmlint 'error-explainer)
                       (flycheck-error-new-at 1 nil 'warning
                                              "no-buildroot-tag"
                                              :checker 'rpm-rpmlint))
              :to-equal "explanation")
      (expect 'flycheck-call-checker-process-for-output
              :to-have-been-called-with
              'rpm-rpmlint nil t "--explain" "no-buildroot-tag"))))

;;; test-rpm.el ends here
