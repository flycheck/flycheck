;;; test-rst.el --- Flycheck Specs: reStructuredText -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language reStructuredText"
  (flycheck-buttercup-def-checker-test rst rst nil
    (flycheck-buttercup-should-syntax-check
     "language/rst/errors.rst" 'rst-mode
     '(8 nil warning "Title underline too short." :checker rst)
     '(14 nil error "Unexpected section title." :checker rst)
     '(16 nil error "Unknown target name: \"restructuredtext\"." :checker rst)
     '(19 nil warning "Title underline too short." :checker rst)
     '(21 nil error "Unknown target name: \"cool\"." :checker rst)
     '(26 nil error "Unexpected section title." :checker rst)))

  (flycheck-buttercup-def-checker-test rst-sphinx rst nil
    (flycheck-buttercup-should-syntax-check
     "language/rst/sphinx/index.rst" 'rst-mode
     '(2 nil warning "Title underline too short." :checker rst-sphinx)
     '(9 nil warning "Unknown target name: \"cool\"." :checker rst-sphinx)
     '(9 nil warning "'envvar' reference target not found: FOO"
         :checker rst-sphinx)))

  (flycheck-buttercup-def-checker-test rst-sphinx rst not-outside-of-a-sphinx-project
    (flycheck-buttercup-with-resource-buffer "language/rst/errors.rst"
      (rst-mode)
      (expect (flycheck-may-use-checker 'rst-sphinx) :not :to-be-truthy))))

;;; test-rst.el ends here
