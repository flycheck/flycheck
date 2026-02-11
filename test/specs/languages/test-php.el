;;; test-php.el --- Flycheck Specs: PHP      -*- lexical-binding: t; -*-

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

;; Specs for PHP support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language PHP"
  (describe "The PHDMD error parser"
    (it "parses PHPMD XML output"

      (let ((phpmd-xml  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<pmd version=\"1.5.0\" timestamp=\"2014-12-02T18:13:44+00:00\">
  <file name=\"foo.php\">
    <violation beginline=\"21\" endline=\"21\" rule=\"UnusedPrivateField\"
      ruleset=\"Unused Code Rules\"
      externalInfoUrl=\"http://phpmd.org/rules/unusedcode.html#unusedprivatefield\"
      priority=\"3\">
        Avoid unused private fields such as '$FOO'.
    </violation>
    <violation beginline=\"24\" endline=\"27\" rule=\"UnusedPrivateMethod\"
      ruleset=\"Unused Code Rules\" package=\"Flycheck\"
      externalInfoUrl=\"http://phpmd.org/rules/unusedcode.html#unusedprivatemethod\"
      class=\"A\" method=\"bar\" priority=\"3\">
        Avoid unused private methods such as 'bar'.
    </violation>
    <violation beginline=\"24\" endline=\"24\" rule=\"UnusedFormalParameter\"
      ruleset=\"Unused Code Rules\"
      externalInfoUrl=\"http://phpmd.org/rules/unusedcode.html#unusedformalparameter\"
      priority=\"3\">
        Avoid unused parameters such as '$baz'.
    </violation>
  </file>
</pmd>"))
        (expect (flycheck-parse-phpmd phpmd-xml 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 21 nil 'warning
                                        "Avoid unused private fields such as '$FOO'."
                                        :id "UnusedPrivateField"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "foo.php")
                 (flycheck-error-new-at 24 nil 'warning
                                        "Avoid unused private methods such as 'bar'."
                                        :id "UnusedPrivateMethod"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "foo.php")
                 (flycheck-error-new-at 24 nil 'warning
                                        "Avoid unused parameters such as '$baz'."
                                        :id "UnusedFormalParameter"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "foo.php"))))))

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test php php syntax-error
      (when (version<= emacs-version "25")
        (buttercup-skip "PHP mode (via CC mode) seems broken on 24.5."))
      (flycheck-buttercup-should-syntax-check
       "language/php/syntax-error.php" 'php-mode
       '(8 nil error "Assignments can only happen to writable values" :checker php)))

    (flycheck-buttercup-def-checker-test (php php-phpcs php-phpmd) php nil
      (flycheck-buttercup-should-syntax-check
       "language/php/warnings.php" 'php-mode
       '(1 1 error "Missing file doc comment"
           :id "PEAR.Commenting.FileComment.Missing" :checker php-phpcs)
       '(21 nil warning "Avoid unused private fields such as '$FOO'."
            :id "UnusedPrivateField" :checker php-phpmd)
       '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore"
            :id "PEAR.NamingConventions.ValidVariableName.PrivateNoUnderscore"
            :checker php-phpcs)
       '(23 5 error "The open comment tag must be the only content on the line"
            :id "Generic.Commenting.DocComment.ContentAfterOpen"
            :checker php-phpcs)
       '(23 5 error "Doc comment for parameter \"$baz\" missing"
            :id "PEAR.Commenting.FunctionComment.MissingParamTag"
            :checker php-phpcs)
       '(23 9 error "Doc comment short description must be on the first line"
            :id "Generic.Commenting.DocComment.SpacingBeforeShort"
            :checker php-phpcs)
       '(23 29 error "The close comment tag must be the only content on the line"
            :id "Generic.Commenting.DocComment.ContentBeforeClose"
            :checker php-phpcs)
       '(23 29 error "Missing @return tag in function comment"
            :id "PEAR.Commenting.FunctionComment.MissingReturn"
            :checker php-phpcs)
       '(24 nil warning "Avoid unused private methods such as 'bar'."
            :id "UnusedPrivateMethod" :checker php-phpmd)
       '(24 nil warning "Avoid unused parameters such as '$baz'."
            :id "UnusedFormalParameter" :checker php-phpmd)
       '(24 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
            :id "PEAR.NamingConventions.ValidFunctionName.PrivateNoUnderscore"
            :checker php-phpcs)
       '(26 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
            :id "ShortVariable" :checker php-phpmd)
       '(26 nil warning "Avoid unused local variables such as '$i'."
            :id "UnusedLocalVariable" :checker php-phpmd)
       '(26 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
            :id "Generic.PHP.LowerCaseConstant.Found" :checker php-phpcs)))))

;;; test-php.el ends here
