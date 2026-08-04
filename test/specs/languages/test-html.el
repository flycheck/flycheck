;;; test-html.el --- Flycheck Specs: HTML -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language HTML"
  (flycheck-buttercup-def-checker-test html-tidy html nil
    (flycheck-buttercup-should-syntax-check
     "language/html.html" '(html-mode)
     '(3 1 warning "missing <!DOCTYPE> declaration"
         :checker html-tidy)
     '(8 5 error "<spam> is not recognized!"
         :checker html-tidy)
     '(8 5 warning "discarding unexpected <spam>"
         :checker html-tidy)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test html-tidy "language/html.html"
      '(3 1 warning "missing <!DOCTYPE> declaration")
      '(8 5 error "<spam> is not recognized!")
      '(8 5 warning "discarding unexpected <spam>"))))

;;; test-html.el ends here
