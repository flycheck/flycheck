;;; test-elixir.el --- Flycheck Specs: Elixir -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Elixir"
  (flycheck-buttercup-def-checker-test elixir-credo elixir infos-without-strict-mode
    (flycheck-buttercup-should-syntax-check
     "language/elixir/lib/infos.ex" 'elixir-mode
     '(1 11 info "Modules should have a @moduledoc tag."
         :checker elixir-credo)))

  (flycheck-buttercup-def-checker-test elixir-credo elixir infos-with-strict-mode
    (let ((flycheck-elixir-credo-strict t))
      (flycheck-buttercup-should-syntax-check
       "language/elixir/lib/infos.ex" 'elixir-mode
       '(1 11 info "Modules should have a @moduledoc tag."
           :checker elixir-credo)
       '(2 nil info "Do not use parentheses when defining a function which has no arguments."
           :checker elixir-credo))))

  (flycheck-buttercup-def-checker-test elixir-credo elixir warnings
    (flycheck-buttercup-should-syntax-check
     "language/elixir/lib/warnings.ex" 'elixir-mode
     '(5 nil warning "There are identical sub-expressions to the left and to the right of the '&&' operator."
         :checker elixir-credo)
     '(8 8 warning "length(list) == 0 is expensive. Prefer Enum.empty?/1 or list == []"
         :checker elixir-credo))))

;;; test-elixir.el ends here
