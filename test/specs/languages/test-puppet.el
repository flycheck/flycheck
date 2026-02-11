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
         :checker puppet-lint))))

;;; test-puppet.el ends here
