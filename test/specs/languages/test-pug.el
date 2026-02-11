;;; test-pug.el --- Flycheck Specs: Pug -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Pug"
  (flycheck-buttercup-def-checker-test pug pug syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/pug/pug.pug" 'pug-mode
     '(2 1 error "unexpected token \"indent\"" :checker pug)))

  (flycheck-buttercup-def-checker-test pug pug non-block-or-mixin-at-top-level-of-extended-template-error
    (flycheck-buttercup-should-syntax-check
     "language/pug/foo.pug" 'pug-mode
     '(9 1
         error "Only named blocks and mixins can appear at the top level of an extending template"
         :checker pug)))

  (flycheck-buttercup-def-checker-test pug pug unknown-filter
    (flycheck-buttercup-should-syntax-check
     "language/pug/foo-unknown-filter.pug" 'pug-mode
     '(1 1
         error "unknown filter \":myfilter\""
         :checker pug)))

  (flycheck-buttercup-def-checker-test pug pug include-extends-error
    (flycheck-buttercup-should-syntax-check
     "language/pug/pug-extends.pug" 'pug-mode
     '(1 nil error "the \"basedir\" option is required to use includes and extends with \"absolute\" paths"
         :checker pug)))

  (flycheck-buttercup-def-checker-test pug pug type-error
    (flycheck-buttercup-should-syntax-check
     "language/pug/pug-runtime-error.pug" 'pug-mode
     '(5 nil error "Cannot read property 'bar' of undefined" :checker pug))))

;;; test-pug.el ends here
