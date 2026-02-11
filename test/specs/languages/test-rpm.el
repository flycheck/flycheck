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
       '(22 nil warning "macro-in-%changelog %{_bindir}" :checker rpm-rpmlint)))))

;;; test-rpm.el ends here
