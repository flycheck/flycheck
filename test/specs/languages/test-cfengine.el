;;; test-cfengine.el --- Flycheck Specs: CFEngine -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CFEngine"
  (flycheck-buttercup-def-checker-test cfengine cfengine error
    (assume (fboundp 'cfengine3-mode))
    (flycheck-buttercup-should-syntax-check
     "language/cfengine/error.cf" 'cfengine3-mode
     '(8 21 error "Unknown promise type 'nosuchpromisetype'" :checker cfengine)))

  (flycheck-buttercup-def-checker-test cfengine cfengine warning
    (assume (fboundp 'cfengine3-mode))
    (flycheck-buttercup-should-syntax-check
     "language/cfengine/warning.cf" 'cfengine3-mode
     '(3 35 warning "Removed constraint 'host_licenses_paid' in promise type 'common' [-Wremoved]"
         :checker cfengine))))

;;; test-cfengine.el ends here
