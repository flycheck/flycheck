;;; test-proselint.el --- Flycheck Specs: Proselint -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Proselint"
  (flycheck-buttercup-def-checker-test proselint (text markdown) nil
    (let ((flycheck-disabled-checkers '(markdown-markdownlint-cli markdown-markdownlint-cli2 markdown-mdl markdown-pymarkdown)))
      (flycheck-buttercup-with-env '(("LC_ALL" . nil))
        (flycheck-buttercup-should-syntax-check
         "language/text/text.txt" '(text-mode markdown-mode)
         '(1 7 warning "Substitute 'damn' every time you're inclined to write 'very'; your editor will delete it and the writing will be just as it should be."
             :id "weasel_words.very"
             :checker proselint
             :end-line 1
             :end-column 12)
         '(2 4 warning "Redundancy. Use 'associate' instead of 'associate together'."
             :id "redundancy.garner"
             :checker proselint
             :end-line 3
             :end-column 1)
         '(3 5 warning "Gender bias. Use 'lawyer' instead of 'lady lawyer'."
             :id "sexism.misc"
             :checker proselint
             :end-line 3
             :end-column 17))))))

;;; test-proselint.el ends here
