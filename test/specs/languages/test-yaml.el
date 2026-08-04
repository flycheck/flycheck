;;; test-yaml.el --- Flycheck Specs: YAML      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for YAML support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language YAML"
  (flycheck-buttercup-def-checker-test yaml-jsyaml yaml nil
    (flycheck-buttercup-should-syntax-check
     "language/yaml.yaml" 'yaml-mode
     '(4 5 error "bad indentation of a mapping entry"
         :checker yaml-jsyaml)))

  (flycheck-buttercup-def-checker-test yaml-yamllint yaml nil
    (let ((flycheck-disabled-checkers '(yaml-jsyaml)))
      (flycheck-buttercup-should-syntax-check
       "language/yaml.yaml" 'yaml-mode
       '(3 1 warning "missing document start \"---\""
           :id "document-start" :checker yaml-yamllint))
      (flycheck-buttercup-should-syntax-check
       "language/yaml.yaml" 'yaml-mode
       '(4 5 error "syntax error: mapping values are not allowed here"
           :checker yaml-yamllint))))

  (describe "the yaml-yamllint checker command"
    (it "appends flycheck-yamllint-args before the stdin marker"
      (let ((flycheck-yamllint-config nil)
            (flycheck-yamllint-args '("--strict")))
        (expect (flycheck-checker-substituted-arguments 'yaml-yamllint)
                :to-equal '("-f" "parsable" "--strict" "-")))))

  (describe "the yaml-jsyaml checker command"
    (it "threads flycheck-yaml-jsyaml-args into js-yaml"
      (let ((flycheck-yaml-jsyaml-args '("--trace")))
        (expect (flycheck-checker-substituted-arguments 'yaml-jsyaml)
                :to-equal '("--trace")))))

  (describe "the yaml-actionlint checker command"
    (it "passes flycheck-yaml-actionlint-config via -config-file"
      (let ((flycheck-yaml-actionlint-config "actionlint.yaml"))
        (spy-on 'flycheck-locate-config-file :and-return-value "/c/actionlint.yaml")
        (expect (flycheck-checker-substituted-arguments 'yaml-actionlint)
                :to-contain "-config-file")))

    (it "threads flycheck-yaml-actionlint-args into actionlint"
      (let ((flycheck-yaml-actionlint-args '("-shellcheck=")))
        (expect (flycheck-checker-substituted-arguments 'yaml-actionlint)
                :to-contain "-shellcheck="))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test yaml-jsyaml "language/yaml.yaml"
      '(4 5 error "bad indentation of a mapping entry"))
    (flycheck-buttercup-def-parse-test yaml-yamllint "language/yaml.yaml"
      '(3 1 warning "missing document start \"---\"" :id "document-start")
      '(4 5 error "syntax error: mapping values are not allowed here" :id "syntax"))))

;;; test-yaml.el ends here
