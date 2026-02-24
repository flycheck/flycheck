;;; test-markdown.el --- Flycheck Specs: Markdown -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Markdown"
  (flycheck-buttercup-def-checker-test markdown-markdownlint-cli markdown nil
    (flycheck-buttercup-should-syntax-check
     "language/markdown.md" 'markdown-mode
     '(1 nil error "First line in a file should be a top-level heading [Context: \"## Second Header First\"]"
         :id "MD041/first-line-heading/first-line-h1" :checker markdown-markdownlint-cli)
     '(3 nil error "Multiple consecutive blank lines [Expected: 1; Actual: 2]"
         :id "MD012/no-multiple-blanks" :checker markdown-markdownlint-cli)
     '(4 15 error "Trailing spaces [Expected: 0 or 2; Actual: 7]"
         :id "MD009/no-trailing-spaces" :checker markdown-markdownlint-cli)))

  (flycheck-buttercup-def-checker-test markdown-markdownlint-cli2 markdown nil
    (let ((flycheck-disabled-checkers '(markdown-markdownlint-cli))
          (flycheck-markdown-markdownlint-cli2-config nil))
      (flycheck-buttercup-should-syntax-check
       "language/markdown.md" 'markdown-mode
       '(1 nil error "First line in a file should be a top-level heading [Context: \"## Second Header First\"]"
           :id "MD041/first-line-heading/first-line-h1" :checker markdown-markdownlint-cli2)
       '(3 nil error "Multiple consecutive blank lines [Expected: 1; Actual: 2]"
           :id "MD012/no-multiple-blanks" :checker markdown-markdownlint-cli2)
       '(4 15 error "Trailing spaces [Expected: 0 or 2; Actual: 7]"
           :id "MD009/no-trailing-spaces" :checker markdown-markdownlint-cli2))))

  (flycheck-buttercup-def-checker-test markdown-mdl markdown nil
    (let ((flycheck-disabled-checkers '(markdown-markdownlint-cli markdown-markdownlint-cli2 markdown-pymarkdown)))
      (flycheck-buttercup-should-syntax-check
       "language/markdown.md" 'markdown-mode
       '(1 nil error "First header should be a top level header"
           :id "MD002" :checker markdown-mdl)
       '(3 nil error "Multiple consecutive blank lines"
           :id "MD012" :checker markdown-mdl)
       '(4 nil error "Trailing spaces"
           :id "MD009" :checker markdown-mdl))))

  (flycheck-buttercup-def-checker-test markdown-pymarkdown markdown nil
    (let ((flycheck-disabled-checkers '(markdown-markdownlint-cli markdown-markdownlint-cli2 markdown-mdl)))
      (flycheck-buttercup-should-syntax-check
       "language/markdown.md" 'markdown-mode
       '(1 nil error "Headings should be surrounded by blank lines. [Expected: 1; Actual: 2; Below] (blanks-around-headings,blanks-around-headers)"
           :id "MD022" :checker markdown-pymarkdown)
       '(1 nil error "First line in file should be a top level heading (first-line-heading,first-line-h1)"
           :id "MD041" :checker markdown-pymarkdown)
       '(3 nil error "Multiple consecutive blank lines [Expected: 1, Actual: 2] (no-multiple-blanks)"
           :id "MD012" :checker markdown-pymarkdown)
       '(4 nil error "Trailing spaces [Expected: 0 or 2; Actual: 7] (no-trailing-spaces)"
           :id "MD009" :checker markdown-pymarkdown)))))

;;; test-markdown.el ends here
