;;; test-racket.el --- Flycheck Specs: Racket -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Racket"
  (flycheck-buttercup-def-checker-test racket racket nil
    (assume (funcall (flycheck-checker-get 'racket 'predicate)))
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/racket.rkt" 'racket-mode
       '(4 3 error "read: expected a `)' to close `('" :checker racket)))))

;;; test-racket.el ends here
