;;; test-asciidoc.el --- Flycheck Specs: AsciiDoc -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language AsciiDoc"
  (flycheck-buttercup-def-checker-test asciidoctor asciidoc nil
    (flycheck-buttercup-should-syntax-check
     "language/asciidoctor.adoc" 'adoc-mode
     '(4 nil warning "section title out of sequence: expected level 1, got level 2" :checker asciidoctor)
     '(6 nil error "unmatched preprocessor directive: endif::[]" :checker asciidoctor)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test asciidoctor "language/asciidoctor.adoc"
      '(4 nil warning "section title out of sequence: expected level 1, got level 2")
      '(6 nil error "unmatched preprocessor directive: endif::[]"))))

;;; test-asciidoc.el ends here
