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
     '(5 nil error "Cannot read property 'bar' of undefined" :checker pug)))

  (describe "the pug command"
    (it "takes a buffer without a file"
      (flycheck-buttercup-with-temp-buffer
        (expect (flycheck-checker-substituted-arguments 'pug)
                :to-equal nil)))

    (it "resolves includes against the buffer's file"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (expand-file-name "a.pug" temporary-file-directory))
        (expect (car (flycheck-checker-substituted-arguments 'pug))
                :to-equal "-p"))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test pug "language/pug/pug.pug"
      '(2 1 error "unexpected token \"indent\""))

    (it "keeps the errors of a buffer without a file in the buffer"
      ;; Without -p, pug 3 names the file "Pug"
      (flycheck-buttercup-with-temp-buffer
        (insert "doctype html\n html\n  head\n")
        (let ((errors (funcall (flycheck-checker-get 'pug 'error-filter)
                               (flycheck-parse-output
                                "Error: Pug:2:1
    1| doctype html
  > 2|  html
-------^
    3|   head
    4|\s

unexpected token \"indent\"
    at makeError (/usr/local/lib/node_modules/pug-cli/node_modules/pug-error/index.js:32:13)
"
                                'pug (current-buffer)))))
          (expect (mapcar (lambda (err)
                            (list (flycheck-error-line err)
                                  (flycheck-error-column err)
                                  (flycheck-error-filename err)))
                          errors)
                  :to-equal '((2 1 nil))))))))

;;; test-pug.el ends here
