;;; test-terraform.el --- Flycheck Specs: Terraform      -*- lexical-binding: t; -*-

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

;; Specs for Terraform support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Terraform"
  (flycheck-buttercup-def-checker-test terraform terraform nil
    (flycheck-buttercup-should-syntax-check
     "language/terraform/terraform/syntax-error.tf" 'terraform-mode
     '(2 nil error "The \";\" character is not valid. Use newlines to separate arguments and blocks,\nand commas to separate items in collection values."
         :checker terraform)
     '(2 nil error "An argument definition must end with a newline."
         :checker terraform)))

  (flycheck-buttercup-def-checker-test terraform-tflint terraform nil
    (flycheck-buttercup-should-syntax-check
     "language/terraform/tflint/error.tf" 'terraform-mode
     '(2 12 warning "Module source \"git://hashicorp.com/consul.git\" is not pinned"
         :id "terraform_module_pinned_source" :checker terraform-tflint
         :end-line 2 :end-column 44)
     '(7 19 error "\"t1.2xlarge\" is an invalid value as instance_type"
         :id "aws_instance_invalid_type" :checker terraform-tflint
         :end-line 7 :end-column 31)))

  (describe "the terraform-tflint checker command"
    (it "adds no config or rule flags by default"
      (let ((flycheck-tflint-config nil)
            (flycheck-tflint-enabled-rules nil)
            (flycheck-tflint-disabled-rules nil)
            (flycheck-tflint-variable-files nil)
            (flycheck-tflint-args nil))
        (expect (flycheck-checker-substituted-arguments 'terraform-tflint)
                :to-equal '("--format=json" "--force"))))

    (it "passes one --enable-rule and --disable-rule per rule"
      (let ((flycheck-tflint-config nil)
            (flycheck-tflint-enabled-rules '("terraform_unused_declarations"))
            (flycheck-tflint-disabled-rules '("terraform_deprecated_syntax"
                                              "aws_instance_invalid_type")))
        (let ((args (flycheck-checker-substituted-arguments 'terraform-tflint)))
          (expect args :to-contain "--enable-rule=terraform_unused_declarations")
          (expect args :to-contain "--disable-rule=terraform_deprecated_syntax")
          (expect args :to-contain "--disable-rule=aws_instance_invalid_type"))))

    (it "appends flycheck-tflint-args"
      (let ((flycheck-tflint-config nil)
            (flycheck-tflint-args '("--minimum-failure-severity=error")))
        (expect (flycheck-checker-substituted-arguments 'terraform-tflint)
                :to-contain "--minimum-failure-severity=error"))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test terraform "language/terraform/terraform/syntax-error.tf"
      '(2 nil error "The \";\" character is not valid. Use newlines to separate arguments and\nblocks, and commas to separate items in collection values.")
      '(2 nil error "An argument definition must end with a newline."))
    (flycheck-buttercup-def-parse-test terraform-tflint "language/terraform/tflint/error.tf"
      '(1 1 warning "terraform \"required_version\" attribute is required" :id "terraform_required_version" :end-line 1 :end-column 1)
      '(2 12 warning "Module source \"git://hashicorp.com/consul.git\" is not pinned" :id "terraform_module_pinned_source" :end-line 2 :end-column 44)
      '(5 1 warning "Missing version constraint for provider \"aws\" in `required_providers`" :id "terraform_required_providers" :end-line 5 :end-column 30))))

;;; test-terraform.el ends here
