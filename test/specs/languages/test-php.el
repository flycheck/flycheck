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
                                        :filename "foo.php")))))))

;;; test-php.el ends here
