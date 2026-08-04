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
       '(4 3 error "read: expected a `)' to close `('" :checker racket))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test racket "language/racket.rkt"
      '(4 3 error "read-syntax: expected a `)` to close `(`"))))

;;; test-racket.el ends here
