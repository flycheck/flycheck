;;; test-erlang.el --- Flycheck Specs: Erlang -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Erlang"
  ;; The compiler's wording changes between OTP releases; these match OTP 29.
  (flycheck-buttercup-def-checker-test erlang erlang error
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang)))
      (shut-up
        (flycheck-buttercup-should-syntax-check
         "language/erlang/erlang/error.erl" 'erlang-mode
         `(3 ,(when col 2) warning "export_all flag enabled - all functions will be exported" :checker erlang)
         `(7 ,(when col 1) error ,(concat "head mismatch: previous function great_func/0 is"
                                 " distinct from error_func/0. Is the semicolon in"
                                 " great_func/0 unwanted?")
             :checker erlang)))))

  (flycheck-buttercup-def-checker-test erlang erlang warning
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang)))
      (flycheck-buttercup-should-syntax-check
       "language/erlang/erlang/warning.erl" 'erlang-mode
       `(3 ,(when col 2) warning "export_all flag enabled - all functions will be exported" :checker erlang)
       `(6 ,(when col 37) warning ,(concat "the format string requires an empty argument"
                                    " list, but the argument list contains 1 argument")
              :checker erlang))))

  ;; rebar3 3.24 replaced its compiler diagnostics with a rich format that
  ;; draws a box around the offending line and colours it, so the file name,
  ;; the line number and the message each land on a line of their own with
  ;; escape sequences in between, and none of the patterns match any more.
  ;; `{compiler_error_format, minimal}' in rebar.config restores the old
  ;; `file:line:column: message' output, but only the project can set that.
  (flycheck-buttercup-def-checker-test erlang-rebar3 erlang error
    :expected-result :failed
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang-rebar3)))
      (flycheck-buttercup-should-syntax-check
       "language/erlang/rebar3/src/erlang-error.erl" 'erlang-mode
       `(3 ,(when col 2) warning "export_all flag enabled - all functions will be exported" :checker erlang-rebar3)
       `(7 ,(when col 1) error "head mismatch" :checker erlang-rebar3))))

  (flycheck-buttercup-def-checker-test erlang-rebar3 erlang build
    :expected-result :failed
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang-rebar3)))
      (shut-up
        (flycheck-buttercup-should-syntax-check
         "language/erlang/rebar3/_checkouts/dependency/src/dependency.erl" 'erlang-mode
         `(7 ,(when col 1) error "head mismatch" :checker erlang-rebar3
             :filename ,(flycheck-buttercup-resource-filename "language/erlang/rebar3/src/erlang-error.erl"))))
      (expect (not (file-exists-p
                    (flycheck-buttercup-resource-filename
                     "language/erlang/rebar3/_build/default/lib/dependency/_build")))
              :to-be-truthy)))

  (describe "the erlang checker command"
    (it "appends flycheck-erlang-args before the source file"
      (let ((flycheck-erlang-args '("-DDEBUG=1")))
        (expect (flycheck-checker-substituted-arguments 'erlang)
                :to-contain "-DDEBUG=1"))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test erlang "language/erlang/erlang/error.erl"
      '(7 1 error "head mismatch: previous function great_func/0 is distinct from error_func/0. Is the semicolon in great_func/0 unwanted?")
      '(3 2 warning "export_all flag enabled - all functions will be exported"))))

;;; test-erlang.el ends here
