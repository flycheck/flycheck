;;; test-nix.el --- Flycheck Specs: Nix -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Nix"
  (flycheck-buttercup-def-checker-test nix nix nil
    (flycheck-buttercup-should-syntax-check
     "language/nix/syntax-error.nix" 'nix-mode
     '(3 1 error "syntax error, unexpected IN, expecting ';'," :checker nix)))

  (flycheck-buttercup-def-checker-test nix-nixf nix nil
    (let ((flycheck-disabled-checkers '(nix)))
      (flycheck-buttercup-should-syntax-check
       "language/nix/syntax-error.nix" 'nix-mode
       '(2 14 error "expected ;"
           :checker nix-nixf :id "parse-expected"
           :end-line 2 :end-column 14))

      (flycheck-buttercup-should-syntax-check
       "language/nix/warnings.nix" 'nix-mode
       '(3 3 warning "definition `y` in let-expression is not used"
           :checker nix-nixf :id "sema-unused-def-let"
           :end-line 3 :end-column 4)
       '(4 4 warning "attrset is not necessary to be `rec`ursive"
           :checker nix-nixf :id "sema-extra-rec"
           :end-line 4 :end-column 7))))

  (describe "nixf parser"
    (it "parses a warning and its fixes"
      (flycheck-buttercup-with-temp-buffer
        (let* ((errors (flycheck-parse-output
                        "[{\"args\":[\";\"],\"fixes\":[\
{\"edits\":[{\"newText\":\";\",\"range\":{\"lCur\":{\"column\":7,\
\"line\":1,\"offset\":11},\"rCur\":{\"column\":7,\"line\":1,\"offset\":11}}}],\
\"message\":\"insert ;\"}],\"kind\":3,\"message\":\"expected {}\",\
\"notes\":[],\"range\":{\"lCur\":{\"column\":7,\"line\":1,\"offset\":11},\
\"rCur\":{\"column\":7,\"line\":1,\"offset\":11}},\"severity\":1,\
\"sname\":\"parse-expected\",\"tag\":[]}]"
                        'nix-nixf (current-buffer)))
               (err (car errors))
               (edits (flycheck-fix-edits (flycheck-error-fix err))))
          (expect (flycheck-error-line err) :to-equal 2)
          (expect (flycheck-error-column err) :to-equal 8)
          (expect (flycheck-error-level err) :to-equal 'error)
          (expect (flycheck-error-message err) :to-equal "expected ;")
          (expect (flycheck-error-id err) :to-equal "parse-expected")
          (expect (length edits) :to-equal 1)
          (expect (flycheck-fix-edit-replacement (car edits)) :to-equal ";")))))

  (describe "the statix checker command"
    (it "appends flycheck-statix-args before the source file"
      (flycheck-buttercup-with-temp-buffer
        (let ((flycheck-statix-args '("--config" "statix.toml")))
          (let ((args (flycheck-checker-substituted-arguments 'statix)))
            (expect args :to-contain "--config")
            (expect args :to-contain "statix.toml"))))))

)

;;; test-nix.el ends here
