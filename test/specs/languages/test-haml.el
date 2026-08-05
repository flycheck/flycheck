;;; test-haml.el --- Flycheck Specs: HAML -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language HAML"
  ;; haml-lint is the only Haml checker: the compiler's own `haml -c
  ;; --stdin' has not existed since Haml 6, and haml-lint reports a
  ;; template that does not parse as well as one that parses but reads
  ;; badly.
  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    ;; The location inside the message is haml-lint repeating itself, and
    ;; is left as the tool words it
    (flycheck-buttercup-def-parse-test haml-lint "language/haml/haml-error.haml"
      '(4 nil error "Syntax: haml-error.haml:4 - Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces."))))

;;; test-haml.el ends here
