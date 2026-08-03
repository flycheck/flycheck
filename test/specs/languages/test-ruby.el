;;; test-typescript.el --- Flycheck Specs: Ruby      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Saša Jovanić <sasa@simplify.ba>

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

;; Specs for Ruby support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Ruby"
  (describe "The Reek error parser"
    (let ((json "[{\"context\":\"ApplicationController#validate_type\",
                   \"lines\":[15,15,16],
                   \"message\":\"calls 'params['data']' 3 times\",
                   \"smell_type\":\"DuplicateMethodCall\",
                   \"source\":\"app/controllers/application_controller.rb\",
                   \"name\":\"params['data']\",
                   \"count\":3,
                   \"wiki_link\":\"https://github.com/troessner/reek/blob/master/docs/Duplicate-Method-Call.md\"},
                  {\"context\":\"ApplicationController#pagination_meta\",
                   \"lines\":[52],
                   \"message\":\"doesn't depend on instance state (maybe move it to another class?)\",
                   \"smell_type\":\"UtilityFunction\",
                   \"source\":\"app/controllers/application_controller.rb\",
                   \"wiki_link\":\"https://github.com/troessner/reek/blob/master/docs/Utility-Function.md\"}]"))
      (it "parses Reek JSON output"
        (expect (flycheck-parse-reek json 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 15 nil 'warning
                                        "ApplicationController#validate_type calls 'params['data']' 3 times"
                                        :id "DuplicateMethodCall"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "app/controllers/application_controller.rb")
                 (flycheck-error-new-at 16 nil 'warning
                                        "ApplicationController#validate_type calls 'params['data']' 3 times"
                                        :id "DuplicateMethodCall"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "app/controllers/application_controller.rb")
                 (flycheck-error-new-at 52 nil 'warning
                                        "ApplicationController#pagination_meta doesn't depend on instance state (maybe move it to another class?)"
                                        :id "UtilityFunction"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "app/controllers/application_controller.rb"))))))

  (describe "the ruby-rubocop checker command"
    ;; Bind the config-file var to nil so argument substitution never locates a
    ;; .rubocop.yml and adds a stray --config.
    (it "adds no extra flags by default"
      (let ((flycheck-rubocop-config nil)
            (flycheck-rubocop-lint-only nil)
            (flycheck-rubocop-server nil)
            (flycheck-rubocop-only nil)
            (flycheck-rubocop-except nil)
            (flycheck-rubocop-args nil))
        (let ((args (flycheck-checker-substituted-arguments 'ruby-rubocop)))
          (expect args :not :to-contain "--server")
          (expect args :not :to-contain "--only")
          (expect args :not :to-contain "--except"))))

    (it "passes --server when enabled"
      (let ((flycheck-rubocop-config nil)
            (flycheck-rubocop-server t))
        (expect (flycheck-checker-substituted-arguments 'ruby-rubocop)
                :to-contain "--server")))

    (it "passes --only and --except as comma-separated cop lists"
      (let ((flycheck-rubocop-config nil)
            (flycheck-rubocop-only '("Style/StringLiterals" "Lint"))
            (flycheck-rubocop-except '("Metrics")))
        (let ((args (flycheck-checker-substituted-arguments 'ruby-rubocop)))
          (expect args :to-contain "--only")
          (expect args :to-contain "Style/StringLiterals,Lint")
          (expect args :to-contain "--except")
          (expect args :to-contain "Metrics"))))

    (it "appends flycheck-rubocop-args"
      (let ((flycheck-rubocop-config nil)
            (flycheck-rubocop-args '("--display-time")))
        (expect (flycheck-checker-substituted-arguments 'ruby-rubocop)
                :to-contain "--display-time"))))

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test ruby-rubocop ruby syntax-error
      (flycheck-buttercup-should-syntax-check
       "language/ruby/syntax-error.rb" 'ruby-mode
       `(5 7 error ,(flycheck-buttercup-rubocop-syntax-message
                     "unexpected constant, expecting end-of-input")
           :id "Lint/Syntax"
           :checker ruby-rubocop)
       `(5 25 error ,(flycheck-buttercup-rubocop-syntax-message
                      "unterminated string meets end of file")
            :id "Lint/Syntax"
            :checker ruby-rubocop)))

    (flycheck-buttercup-def-checker-test ruby-standard ruby syntax-error
      (let ((flycheck-disabled-checkers '(ruby-rubocop)))
        (flycheck-buttercup-should-syntax-check
         "language/ruby/syntax-error.rb" 'ruby-mode
         `(5 7 error ,(flycheck-buttercup-rubocop-syntax-message
                     "unexpected constant, expecting end-of-input")
             :id "Lint/Syntax"
             :checker ruby-standard)
         `(5 25 error ,(flycheck-buttercup-rubocop-syntax-message
                      "unterminated string meets end of file")
              :id "Lint/Syntax"
              :checker ruby-standard))))

    (flycheck-buttercup-def-checker-test ruby ruby syntax-error
      (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-reek)))
        (flycheck-buttercup-should-syntax-check
         "language/ruby/syntax-error.rb" 'ruby-mode
         '(4 nil warning "assigned but unused variable - days" :checker ruby)
         ;; Ruby 3.4 parses with Prism, which reports differently
         `(5 nil error ,(if (version<= "3.4" (or (flycheck-buttercup-ruby-version) "0"))
                            "syntax errors found (SyntaxError)"
                          "syntax error, unexpected constant, expecting end-of-input")
             :checker ruby))))

    (flycheck-buttercup-def-checker-test (ruby-rubocop ruby-reek) ruby warnings
      (flycheck-buttercup-should-syntax-check
       "language/ruby/warnings.rb" 'ruby-mode
       '(1 1 info "Missing frozen string literal comment."
           :id "[Correctable] Style/FrozenStringLiteralComment" :checker ruby-rubocop)
       '(3 nil warning "Person assumes too much for instance variable '@name'"
           :id "InstanceVariableAssumption" :checker ruby-reek)
       '(3 1 info "Missing top-level class documentation comment."
           :id "Style/Documentation" :checker ruby-rubocop)
       '(5 5 warning "Useless assignment to variable - `arr`."
           :id "Lint/UselessAssignment" :checker ruby-rubocop)
       '(5 11 info "Use `%i` or `%I` for an array of symbols."
           :id "[Correctable] Style/SymbolArray" :checker ruby-rubocop)
       '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
           :id "[Correctable] Style/StringLiterals" :checker ruby-rubocop)
       '(10 5 info "Use a guard clause (`return unless true`) instead of wrapping the code inside a conditional expression."
            :id "Style/GuardClause":checker ruby-rubocop)
       '(10 5 info "Favor modifier `if` usage when having a single-line body. Another good alternative is the usage of control flow `&&`/`||`."
            :id "[Correctable] Style/IfUnlessModifier" :checker ruby-rubocop)
       '(10 8 warning "Literal `true` appeared as a condition."
            :id "Lint/LiteralAsCondition" :checker ruby-rubocop)
       '(10 13 info "Do not use `then` for multi-line `if`."
            :id "[Correctable] Style/MultilineIfThen" :checker ruby-rubocop)
       '(11 7 info "Redundant `return` detected."
            :id "[Correctable] Style/RedundantReturn" :checker ruby-rubocop)))

    (flycheck-buttercup-def-checker-test ruby-reek ruby warnings
      (let ((flycheck-disabled-checkers '(ruby-rubocop)))
        (flycheck-buttercup-should-syntax-check
         "language/ruby/warnings.rb" 'ruby-mode
         '(3 nil warning "Person assumes too much for instance variable '@name'"
             :id "InstanceVariableAssumption" :checker ruby-reek))))

    (flycheck-buttercup-def-checker-test ruby ruby warnings
      (let ((flycheck-disabled-checkers '(ruby-rubocop ruby-reek)))
        (flycheck-buttercup-should-syntax-check
         "language/ruby/warnings.rb" 'ruby-mode
         '(5 nil warning "assigned but unused variable - arr" :checker ruby)
         '(16 nil warning "possibly useless use of == in void context"
              :checker ruby))))

)

  (describe "Fatal-failure handling"
    ;; RuboCop exits 2 on a bad invocation or an unrecognised cop in the
    ;; configuration, and 1 when it found offences
    (it "disables the checker when RuboCop could not run"
      (expect (flycheck--rubocop-handle-suspicious
               'ruby-rubocop 2
               "Error: unrecognized cop NoSuch/Thing found in .rubocop.yml")
              :to-equal
              '(disable . "Error: unrecognized cop NoSuch/Thing found in .rubocop.yml")))

    (it "stays suspicious when RuboCop merely found offences"
      (expect (flycheck--rubocop-handle-suspicious 'ruby-rubocop 1 "")
              :to-be 'suspicious))

    (it "covers standard and cookstyle too"
      (dolist (checker '(ruby-standard ruby-chef-cookstyle))
        (expect (flycheck-checker-get checker 'handle-suspicious)
                :to-be 'flycheck--rubocop-handle-suspicious))))


  (describe "reading rubocop's output"
    (flycheck-buttercup-def-parse-test ruby-rubocop
        "language/ruby/syntax-error.rb"
      '(5 7 error "unexpected constant, expecting end-of-input (Using Ruby 3.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
          :id "Lint/Syntax")
      '(5 25 error "unterminated string meets end of file (Using Ruby 3.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
          :id "Lint/Syntax"))))

;;; test-ruby.el ends here
