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
       '(3 1 warning "missing document start \"---\" (document-start)"
           :checker yaml-yamllint))
      (flycheck-buttercup-should-syntax-check
       "language/yaml.yaml" 'yaml-mode
       '(4 5 error "syntax error: mapping values are not allowed here"
           :checker yaml-yamllint)))))

;;; test-yaml.el ends here
