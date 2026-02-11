;;; test-asciidoc.el --- Flycheck Specs: AsciiDoc -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language AsciiDoc"
  (flycheck-buttercup-def-checker-test asciidoc asciidoc nil
    (let ((flycheck-disabled-checkers '(asciidoctor)))
      (flycheck-buttercup-should-syntax-check
       "language/asciidoc.adoc" 'adoc-mode
       '(1 nil warning "missing style: [paradef-default]: paragraph" :checker asciidoc)
       '(3 nil info "old tables syntax" :checker asciidoc)
       '(11 nil error "[tabledef-default] illegal width=%60%" :checker asciidoc))))

  (flycheck-buttercup-def-checker-test asciidoctor asciidoc nil
    (flycheck-buttercup-should-syntax-check
     "language/asciidoctor.adoc" 'adoc-mode
     '(4 nil warning "section title out of sequence: expected level 1, got level 2" :checker asciidoctor)
     '(6 nil error "unmatched preprocessor directive: endif::[]" :checker asciidoctor))))

;;; test-asciidoc.el ends here
