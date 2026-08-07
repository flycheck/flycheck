;;; test-error-explainers.el --- Flycheck Specs: Error explainers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Specs for `:error-explainer' functionality: the `flycheck-error-explainer-from-url'
;; helper and the checker explainers built on it.

;;; Code:

(require 'flycheck-buttercup)

(defun test-explainer/explain (checker id)
  "Return the explanation CHECKER's `:error-explainer' gives an error with ID."
  (when-let* ((fn (flycheck-checker-get checker 'error-explainer)))
    (funcall fn (flycheck-error-new-at 1 1 'error "msg" :id id :checker checker))))

(describe "Error explainers"

  (describe "flycheck-error-explainer-from-url"
    (it "formats the error id into the url"
      (let ((ex (flycheck-error-explainer-from-url "https://x/%s")))
        (expect (funcall ex (flycheck-error-new-at 1 1 'error "m" :id "foo"))
                :to-equal '(url . "https://x/foo"))))

    (it "returns nil for an error without an id"
      (let ((ex (flycheck-error-explainer-from-url "https://x/%s")))
        (expect (funcall ex (flycheck-error-new-at 1 1 'error "m")) :to-be nil)))

    (it "applies the transform to the id"
      (let ((ex (flycheck-error-explainer-from-url "https://x/%s" #'upcase)))
        (expect (funcall ex (flycheck-error-new-at 1 1 'error "m" :id "foo"))
                :to-equal '(url . "https://x/FOO"))))

    (it "skips the error when the transform returns nil"
      (let ((ex (flycheck-error-explainer-from-url "https://x/%s"
                                                   (lambda (_id) nil))))
        (expect (funcall ex (flycheck-error-new-at 1 1 'error "m" :id "foo"))
                :to-be nil))))

  (describe "the plain url explainers"
    (it "links stylelint rules for every stylelint variant"
      (dolist (c '(css-stylelint less-stylelint scss-stylelint sass-stylelint))
        (expect (test-explainer/explain c "indentation")
                :to-equal
                '(url . "https://stylelint.io/user-guide/rules/indentation"))))

    (it "links shellcheck, staticcheck and mypy diagnostics"
      (expect (test-explainer/explain 'sh-shellcheck "SC2086")
              :to-equal '(url . "https://github.com/koalaman/shellcheck/wiki/SC2086"))
      (expect (test-explainer/explain 'go-staticcheck "SA1000")
              :to-equal '(url . "https://staticcheck.dev/docs/checks#SA1000"))
      (expect (test-explainer/explain 'python-mypy "name-defined")
              :to-equal
              '(url . "https://mypy.readthedocs.io/en/stable/error_code_list.html#code-name-defined")))

    (it "links core eslint rules but skips plugin rules"
      (expect (test-explainer/explain 'javascript-eslint "no-eval")
              :to-equal '(url . "https://eslint.org/docs/rules/no-eval"))
      (expect (test-explainer/explain 'javascript-eslint "react/jsx-key")
              :to-be nil))

    (it "links yamllint rules through their underscored anchors"
      (expect (test-explainer/explain 'yaml-yamllint "document-start")
              :to-equal
              '(url . "https://yamllint.readthedocs.io/en/stable/rules.html#module-yamllint.rules.document_start")))

    (it "links tflint's terraform rules but skips provider rules"
      (expect (test-explainer/explain 'terraform-tflint "terraform_deprecated_interpolation")
              :to-equal
              '(url . "https://github.com/terraform-linters/tflint-ruleset-terraform/blob/main/docs/rules/terraform_deprecated_interpolation.md"))
      ;; The provider rulesets keep no documentation file per rule
      (expect (test-explainer/explain 'terraform-tflint "aws_instance_invalid_type")
              :to-be nil))

    (it "links reek smells through their hyphenated names"
      (expect (test-explainer/explain 'ruby-reek "InstanceVariableAssumption")
              :to-equal
              '(url . "https://github.com/troessner/reek/blob/master/docs/Instance-Variable-Assumption.md")))

    (it "links pymarkdown rules through their lowercased files"
      (expect (test-explainer/explain 'markdown-pymarkdown "MD022")
              :to-equal
              '(url . "https://github.com/jackdewinter/pymarkdown/blob/main/docs/rules/rule_md022.md")))

    (it "links oxlint rules under their plugin, skipping bare ids"
      (expect (test-explainer/explain 'javascript-oxlint "eslint(no-unused-vars)")
              :to-equal
              '(url . "https://oxc.rs/docs/guide/usage/linter/rules/eslint/no-unused-vars.html"))
      (expect (test-explainer/explain 'javascript-oxlint "no-unused-vars")
              :to-be nil)))

  (describe "flycheck-dockerfile-hadolint-error-explainer"
    (it "routes DL rules to the hadolint wiki"
      (expect (test-explainer/explain 'dockerfile-hadolint "DL3006")
              :to-equal '(url . "https://github.com/hadolint/hadolint/wiki/DL3006")))

    (it "routes forwarded SC rules to the shellcheck wiki"
      (expect (test-explainer/explain 'dockerfile-hadolint "SC2086")
              :to-equal '(url . "https://github.com/koalaman/shellcheck/wiki/SC2086")))

    (it "returns nil for an unrecognised id"
      (expect (test-explainer/explain 'dockerfile-hadolint "XY1") :to-be nil)))

  (describe "flycheck-ruby-rubocop-error-explainer"
    (it "links a core cop to its department page and anchor"
      (expect (test-explainer/explain 'ruby-rubocop "Style/StringLiterals")
              :to-equal
              '(url . "https://docs.rubocop.org/rubocop/cops_style.html#stylestringliterals")))

    (it "skips cops from extensions, so it never yields a broken link"
      (expect (test-explainer/explain 'ruby-rubocop "RSpec/ExampleLength")
              :to-be nil))))

;;; test-error-explainers.el ends here
