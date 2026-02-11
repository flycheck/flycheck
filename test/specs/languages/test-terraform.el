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
         :end-line 7 :end-column 31))))

;;; test-terraform.el ends here
