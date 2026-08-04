;;; test-ocaml.el --- Flycheck Specs: OCaml -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language OCaml"
  (flycheck-buttercup-def-checker-test ocaml ocaml syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/ocaml/syntaxerror.ml" 'tuareg-mode
     '(4 1 error "Syntax error" :end-line 4 :end-column 1 :checker ocaml)))

  (flycheck-buttercup-def-checker-test ocaml ocaml warning
    ;; The compiler counts characters from zero, Flycheck from one
    (flycheck-buttercup-should-syntax-check
     "language/ocaml/warning.ml" 'tuareg-mode
     '(3 9 warning "this optional argument cannot be erased."
         :end-line 3 :end-column 10 :id "16" :checker ocaml)))

  (describe "checker selection"

    (it "leaves a Dune project to the Dune checker"
      ;; On its own the compiler knows nothing about sibling modules, so
      ;; every reference to one would come back as an unbound module
      (flycheck-buttercup-with-resource-buffer
          "language/ocaml-dune/lib/typeerror.ml"
        (setq major-mode 'tuareg-mode)
        (expect (flycheck-ocaml--dune-root) :to-be-truthy)
        (expect (funcall (flycheck-checker-get 'ocaml 'predicate))
                :not :to-be-truthy)))

    (it "uses the compiler for a file that is in no Dune project"
      (flycheck-buttercup-with-resource-buffer "language/ocaml/warning.ml"
        (setq major-mode 'tuareg-mode)
        (expect (flycheck-ocaml--dune-root) :not :to-be-truthy)
        (expect (funcall (flycheck-checker-get 'ocaml 'predicate)) :to-be-truthy)
        (expect (funcall (flycheck-checker-get 'ocaml-dune 'predicate))
                :not :to-be-truthy)))

    (it "waits for the buffer to be saved before running Dune"
      ;; Dune builds from the files on disk, so an unsaved buffer would be
      ;; checked as it was before the edit
      (flycheck-buttercup-with-resource-buffer
          "language/ocaml-dune/lib/typeerror.ml"
        (setq major-mode 'tuareg-mode)
        (expect (funcall (flycheck-checker-get 'ocaml-dune 'predicate))
                :to-be-truthy)
        (set-buffer-modified-p t)
        (expect (funcall (flycheck-checker-get 'ocaml-dune 'predicate))
                :not :to-be-truthy))))

  (flycheck-buttercup-def-checker-test ocaml-dune ocaml-dune type-error
    ;; Dune builds the whole project, so the reference to `Helper' in the
    ;; same file resolves and only the genuine type error is reported
    (flycheck-buttercup-should-syntax-check
     "language/ocaml-dune/lib/typeerror.ml" 'tuareg-mode
     '(4 17 error "This constant has type string but an expression was expected of type\n         int"
         :end-line 4 :end-column 29 :checker ocaml-dune)))

  (describe "reading Dune's output"
    ;; The location line, the echoed source and the wrapped message all
    ;; have to be read as one error
    (flycheck-buttercup-def-parse-test ocaml-dune
        "language/ocaml-dune/lib/typeerror.ml"
      '(4 17 error "This constant has type string but an expression was expected of type\n         int"
          :end-line 4 :end-column 29)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test ocaml "language/ocaml/syntaxerror.ml"
      '(4 1 error "Syntax error" :end-line 4 :end-column 1))))

;;; test-ocaml.el ends here
