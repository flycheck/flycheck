;;; test-puppet-lint.el --- Test the puppet linter -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Mark Hellewell <mark.hellewell@gmail.com>
;;
;; Author: Mark Hellewell <mark.hellewell@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

(require 'puppet-mode)

(ert-deftest checker-puppet-80-chars-warn ()
  "Test 80 chars warning."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/80_chars_warn.pp" 'puppet-mode nil
   '(4 nil "line has more than 80 characters" warning)))

(ert-deftest checker-puppet-case-without-default-warn ()
  "Test case statement missing default warning."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/case_without_default_warn.pp" 'puppet-mode nil
   '(2 nil "case statement without a default case" warning)))

(ert-deftest checker-puppet-comments1-warn ()
  "Test // comments warning.  Although this was going to test
puppet-lint, the chaining rules mean that puppet-lint doesn't
actually get to check the test file as puppet parser also raises
an error for // comments."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/comments1_warn.pp" 'puppet-mode nil
;;   '(2 nil "// comment found" warning)
   '(2 nil "Syntax error at '/'" error)))

(ert-deftest checker-puppet-comments2-warn ()
  "/* */ comment found"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/comments2_warn.pp" 'puppet-mode nil
   '(3 nil "/* */ comment found" warning)))

(ert-deftest checker-puppet-80_chars_warn ()
  "line has more than 80 characters"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/80_chars_warn.pp" 'puppet-mode nil
   '(4 nil "line has more than 80 characters" warning)))

(ert-deftest checker-puppet-case_without_default_warn ()
  "case statement without a default case"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/case_without_default_warn.pp" 'puppet-mode nil
   '(2 nil "case statement without a default case" warning)))

(ert-deftest checker-puppet-double_quoted_warn ()
  "double quoted string containing no variables"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/double_quoted_warn.pp" 'puppet-mode nil
   '(2 nil "double quoted string containing no variables" warning)))

(ert-deftest checker-puppet-duplicate_param_err ()
  "duplicate parameter found in resource"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/duplicate_param_err.pp" 'puppet-mode nil
   '(4 nil "duplicate parameter found in resource" error)))

(ert-deftest checker-puppet-ensure_not_first_warn ()
  "ensure found on line but it's not the first attribute"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/ensure_not_first_warn.pp" 'puppet-mode nil
   '(4 nil "ensure found on line but it's not the first attribute" warning)))

(ert-deftest checker-puppet-mode_warn ()
  "mode should be represented as a 4 digit octal value or symbolic mode"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/mode_warn.pp" 'puppet-mode nil
   '(3 nil "mode should be represented as a 4 digit octal value or symbolic mode" warning)))

(ert-deftest checker-puppet-not_properly_aligned_warn ()
  "indentation of => is not properly aligned"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/not_properly_aligned_warn.pp" 'puppet-mode nil
   '(4 nil "indentation of => is not properly aligned" warning)))

(ert-deftest checker-puppet-not_two_space_err ()
  "two-space soft tabs not used"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/not_two_space_err.pp" 'puppet-mode nil
   '(3 nil "two-space soft tabs not used" error)))

(ert-deftest checker-puppet-only_variable_warn ()
  "string containing only a variable"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/only_variable_warn.pp" 'puppet-mode nil
   '(2 nil "string containing only a variable" warning)))

(ert-deftest checker-puppet-quoted_bool_warn ()
  "quoted boolean value found"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/quoted_bool_warn.pp" 'puppet-mode nil
   '(3 nil "quoted boolean value found" warning)))

(ert-deftest checker-puppet-right_to_left_warn ()
  "right-to-left (<-) relationship"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/right_to_left_warn.pp" 'puppet-mode nil
   '(2 nil "right-to-left (<-) relationship" warning)))

(ert-deftest checker-puppet-selector_in_resource_warn ()
  "selector inside resource block"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/selector_in_resource_warn.pp" 'puppet-mode nil
   '(3 nil "selector inside resource block" warning)))

(ert-deftest checker-puppet-single_quoted_var_err ()
  "single quoted string containing a variable found"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/single_quoted_var_err.pp" 'puppet-mode nil
   '(2 nil "single quoted string containing a variable found" error)))

(ert-deftest checker-puppet-symlink_target_warn ()
  "symlink target specified in ensure attr"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/symlink_target_warn.pp" 'puppet-mode nil
   '(3 nil "symlink target specified in ensure attr" warning)))

(ert-deftest checker-puppet-tab_expected ()
  "tab character"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/tab_err.pp" 'puppet-mode nil
   '(3 nil "two-space soft tabs not used" error)
   '(3 nil "tab character found" error)))

(ert-deftest checker-puppet-trailing_whitespace_err ()
  "trailing whitespace found"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/trailing_whitespace_err.pp" 'puppet-mode nil
   '(3 nil "trailing whitespace found" error)))

(ert-deftest checker-puppet-unquoted_mode_warn ()
  "unquoted file mode"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/unquoted_mode_warn.pp" 'puppet-mode nil
   '(3 nil "unquoted file mode" warning)))

(ert-deftest checker-puppet-unquoted_resource_warn ()
  "unquoted resource title"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/unquoted_resource_warn.pp" 'puppet-mode nil
   '(2 nil "unquoted resource title" warning)))

(ert-deftest checker-puppet-variable_dash_warn ()
  "variable contains a dash"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/variable_dash_warn.pp" 'puppet-mode nil
   '(2 nil "variable contains a dash" warning)))

(ert-deftest checker-puppet-variable_not_enclosed_warn ()
  "variable not enclosed in {}"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/variable_not_enclosed_warn.pp" 'puppet-mode nil
   '(2 nil "variable not enclosed in {}" warning)))


(ert-deftest checker-puppet-definewithin ()
  "define defined inside a class"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/definewithin/manifests/init.pp" 'puppet-mode nil
   '(4 nil "define defined inside a class" warning)
   '(4 nil "define defined inside a class" warning)))

(ert-deftest checker-puppet-argorder ()
  "parameterised class parameter without a default value"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/argorder/manifests/init.pp" 'puppet-mode nil
   '(4 nil "parameterised class parameter without a default value" warning)))

(ert-deftest checker-puppet-classparamdefaults ()
  "parameterised class parameter without a default value"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/classparamdefaults/manifests/init.pp" 'puppet-mode nil
   '(3 nil "parameterised class parameter without a default value" warning)))

(ert-deftest checker-puppet-classwithin ()
  "class defined inside a class"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/classwithin/manifests/init.pp" 'puppet-mode nil
   '(4 nil "class defined inside a class" warning)
   '(4 nil "class defined inside a class" warning)))

(ert-deftest checker-puppet-acrossnamespaces ()
  "class inherits across module namespaces"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/acrossnamespaces/manifests/init.pp" 'puppet-mode nil
   '(2 nil "class inherits across module namespaces" warning)))

(ert-deftest checker-puppet-mlayout ()
  "mlayout not in autoload module layout"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/mlayout/manifests/wrong.pp" 'puppet-mode nil
   '(2 nil "mlayout not in autoload module layout" error)))

(ert-deftest checker-puppet-moduledashed ()
  "class name containing a dash"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/moduledashed/manifests/foo-bar.pp" 'puppet-mode nil
   '(2 nil "class name containing a dash" warning)))

(ert-deftest checker-puppet-topscope ()
  "Syntax error at '='; expected '}'"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/topscope/manifests/init.pp" 'puppet-mode nil
   '(4 nil "Syntax error at '='; expected '}'" error)))

(ert-deftest checker-puppet-undocumented ()
  "class not documented"
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet/linter/classtests/undocumented/manifests/init.pp" 'puppet-mode nil
   '(1 nil "class not documented" warning)))


;; Local Variables:
;; coding: utf-8
;; End:

;;; test-puppet-lint.el ends here
