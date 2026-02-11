;;; test-perl.el --- Flycheck Specs: Perl -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Perl"
  (flycheck-buttercup-def-checker-test (perl perl-perlcritic) perl nil
    (flycheck-buttercup-should-syntax-check
     "language/perl.pl" '(perl-mode cperl-mode)
     '(6 nil error "Global symbol \"$x\" requires explicit package name (did you forget to declare \"my $x\"?)"
         :checker perl)
     '(6 nil error "BEGIN not safe after errors--compilation aborted"
         :checker perl)
     '(6 6 error "Glob written as <...> (See page 167 of PBP)"
         :id "BuiltinFunctions::RequireGlobFunction" :checker perl-perlcritic)))

  (flycheck-buttercup-def-checker-test perl perl modules
    (flycheck-buttercup-should-syntax-check
     "language/perl/Script.pl" '(perl-mode cperl-mode)
     '(3 nil error "Global symbol \"$dependency_a\" requires explicit package name (did you forget to declare \"my $dependency_a\"?)"
         :checker perl)
     '(4 nil error "Global symbol \"$dependency_b\" requires explicit package name (did you forget to declare \"my $dependency_b\"?)"
         :checker perl))
    (let ((flycheck-perl-include-path '("."))
          (flycheck-perl-module-list '("DependencyA")))
      (flycheck-buttercup-should-syntax-check
       "language/perl/Script.pl" '(perl-mode cperl-mode)
       '(4 nil error "Global symbol \"$dependency_b\" requires explicit package name (did you forget to declare \"my $dependency_b\"?)"
           :checker perl)))
    (let ((flycheck-perl-include-path '("."))
          (flycheck-perl-module-list '("DependencyA" "DependencyB")))
      (flycheck-buttercup-should-syntax-check
       "language/perl/Script.pl" '(perl-mode cperl-mode)))))

;;; test-perl.el ends here
