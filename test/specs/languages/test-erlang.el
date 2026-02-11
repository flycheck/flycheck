;;; test-erlang.el --- Flycheck Specs: Erlang -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Erlang"
  (flycheck-buttercup-def-checker-test erlang erlang error
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang)))
      (shut-up
        (flycheck-buttercup-should-syntax-check
         "language/erlang/erlang/error.erl" 'erlang-mode
         '(3 (when col 2) warning "export_all flag enabled - all functions will be exported" :checker erlang)
         '(7 (when col 1) error "head mismatch" :checker erlang)))))

  (flycheck-buttercup-def-checker-test erlang erlang warning
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang)))
      (flycheck-buttercup-should-syntax-check
       "language/erlang/erlang/warning.erl" 'erlang-mode
       '(3 (when col 2) warning "export_all flag enabled - all functions will be exported" :checker erlang)
       '(6 (when col 37) warning "wrong number of arguments in format call" :checker erlang))))

  (flycheck-buttercup-def-checker-test erlang-rebar3 erlang error
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang-rebar3)))
      (flycheck-buttercup-should-syntax-check
       "language/erlang/rebar3/src/erlang-error.erl" 'erlang-mode
       '(3 (when col 2) warning "export_all flag enabled - all functions will be exported" :checker erlang-rebar3)
       '(7 (when col 1) error "head mismatch" :checker erlang-rebar3))))

  (flycheck-buttercup-def-checker-test erlang-rebar3 erlang build
    (let ((col (flycheck-buttercup-erlang-shows-column 'erlang-rebar3)))
      (shut-up
        (flycheck-buttercup-should-syntax-check
         "language/erlang/rebar3/_checkouts/dependency/src/dependency.erl" 'erlang-mode
         `(7 (when col 1) error "head mismatch" :checker erlang-rebar3
             :filename ,(flycheck-buttercup-resource-filename "language/erlang/rebar3/src/erlang-error.erl"))))
      (expect (not (file-exists-p
                    (flycheck-buttercup-resource-filename
                     "language/erlang/rebar3/_build/default/lib/dependency/_build")))
              :to-be-truthy))))

;;; test-erlang.el ends here
