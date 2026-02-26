;;; test-less.el --- Flycheck Specs: Less -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Less"
  (flycheck-buttercup-def-checker-test less less file-error
    (let* ((candidates (list "no-such-file.less"
                             "npm://no-such-file.less"
                             "no-such-file.less"))
           (message (string-join candidates ",")))
      (flycheck-buttercup-should-syntax-check
       "language/less/file-error.less" 'less-css-mode
       `(3 1 error ,(concat "'no-such-file.less' wasn't found. Tried - "
                            message)
           :checker less))))

  (flycheck-buttercup-def-checker-test less less syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/less/syntax-error.less" 'less-css-mode
     '(1 1 error "Unrecognised input" :checker less)))

  (flycheck-buttercup-def-checker-test less-stylelint less nil
    (let ((flycheck-disabled-checkers '(less))
          (flycheck-stylelintrc
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/less/syntax-error.less" 'less-css-mode
       '(1 1 error "Unclosed block (CssSyntaxError)"
           :id "CssSyntaxError" :checker less-stylelint)))))

;;; test-less.el ends here
