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
       "language/perl/Script.pl" '(perl-mode cperl-mode))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test perl "language/perl.pl"
      '(6 nil error "Global symbol \"$x\" requires explicit package name (did you forget to declare \"my $x\"?)")
      '(6 nil error "BEGIN not safe after errors--compilation aborted"))
    (flycheck-buttercup-def-parse-test perl-perlcritic "language/perl.pl"
      '(6 6 error "Glob written as <...> (See page 167 of PBP)" :id "BuiltinFunctions::RequireGlobFunction"))

    (flycheck-buttercup-def-parse-test perl-perlimports "language/perl/UnusedImport.pl"
      `(3 1 info "POSIX import arguments need tidying:
-use POSIX qw(ceil floor);
+use POSIX qw( ceil );"
          :end-line 3 :end-column 26
          :fix ,(flycheck-fix-new
                 :description "POSIX import arguments need tidying"
                 :edits (list (flycheck-fix-edit-new
                               :line 3 :column 1 :end-line 3 :end-column 26
                               :replacement "use POSIX qw( ceil );")))))

    (it "stamps perlimports fixes with the tick from the check's start"
      ;; The tick must come from when the check started, not from parse
      ;; time, so a fix goes stale when the buffer changes while the tool
      ;; runs (#2339)
      (flycheck-buttercup-with-temp-buffer
        (setq flycheck--syntax-check-modified-tick 42)
        (let* ((output (flycheck-buttercup-fixture 'perl-perlimports
                                                   "language/perl/UnusedImport.pl"))
               (err (car (flycheck-buttercup-parse 'perl-perlimports output))))
          (expect (flycheck-fix-tick (flycheck-error-fix err)) :to-equal 42))))))

;;; test-perl.el ends here
