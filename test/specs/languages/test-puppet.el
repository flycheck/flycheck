;;; test-puppet.el --- Flycheck Specs: Puppet -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Puppet"
  (flycheck-buttercup-def-checker-test puppet-parser puppet parser-error-puppet-4
    (assume (version<= "4" (shell-command-to-string
                             "printf %s \"$(puppet --version)\"")))
    (flycheck-buttercup-should-syntax-check
     "language/puppet/parser-error.pp" 'puppet-mode
     '(3 9 error "Syntax error at '>'" :checker puppet-parser)))

  (flycheck-buttercup-def-checker-test puppet-parser puppet parser-error-puppet-3
    (assume (version<= (shell-command-to-string
                         "printf %s \"$(puppet --version)\"") "4"))
    (flycheck-buttercup-should-syntax-check
     "language/puppet/puppet3-parser-error.pp" 'puppet-mode
     '(4 3 error "Syntax error at 'helloagain'"
         :checker puppet-parser))
    (flycheck-buttercup-should-syntax-check
     "language/puppet/puppet3-parser-multiline-error.pp" 'puppet-mode
     '(4 25 error "Unclosed quote after \"'\" followed by '\\n}\\n...'"
         :checker puppet-parser)))

  (flycheck-buttercup-def-checker-test puppet-lint puppet nil
    (flycheck-buttercup-should-syntax-check
     "language/puppet/warnings.pp" 'puppet-mode
     '(2 nil error "foo::bar not in autoload module layout (autoloader_layout)"
         :checker puppet-lint)
     '(3 nil warning "case statement without a default case (case_without_default)"
         :checker puppet-lint)))

  (describe "the puppet-lint checker command"
    (it "appends flycheck-puppet-lint-args before the source file"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (make-temp-file "flycheck-puppet" nil ".pp"))
        (let ((flycheck-puppet-lint-config nil)
              (flycheck-puppet-lint-disabled-checks nil)
              (flycheck-puppet-lint-args '("--relative")))
          (expect (flycheck-checker-substituted-arguments 'puppet-lint)
                  :to-contain "--relative")))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test puppet-parser "language/puppet/parser-error.pp"
      '(3 9 error "Syntax error at '>'"))
    (flycheck-buttercup-def-parse-test puppet-lint "language/puppet/warnings.pp"
      '(2 nil error "foo::bar not in autoload module layout (autoloader_layout)")
      '(3 nil warning "case statement without a default case (case_without_default)")
      '(3 nil warning "legacy fact 'operatingsystem' (legacy_facts)"))))

;;; test-puppet.el ends here
