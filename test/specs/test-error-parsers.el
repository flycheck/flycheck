;;; test-code-style.el --- Flycheck Specs: Error parsers     -*- lexical-binding: t; -*-

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

;; Specs for generic error parsers.

;;; Code:

(require 'flycheck-buttercup)

(describe "Error parsers"
  (describe "The checkstyle parser"
    (let ((checkstyle-xml "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<checkstyle version=\"4.3\">
  <file name=\"test-javascript/missing-semicolon.js\">
    <error line=\"3\" column=\"21\" severity=\"error\"
           message=\"Missing semicolon.\" source=\"Foo3\" />
    <error line=\"3\" severity=\"warning\"
           message=\"Implied global &apos;alert&apos;\" source=\"Foo4\" />
  </file>
  <file name=\"test-javascript/missing-quote.js\">
    <error line=\"1\" column=\"undefined\" severity=\"error\"
           message=\"Cannot read property &apos;id&apos; of undefined\"
           source=\"Foo1\" />
  </file>
</checkstyle>")
          (checkstyle-expected-errors
           (list
            (flycheck-error-new
             :filename "test-javascript/missing-semicolon.js"
             :checker 'checker
             :buffer 'buffer
             :line 3
             :column 21
             :level 'error
             :message "Missing semicolon."
             :id "Foo3")
            (flycheck-error-new
             :filename "test-javascript/missing-semicolon.js"
             :checker 'checker
             :buffer 'buffer
             :line 3
             :column nil
             :level 'warning
             :message "Implied global 'alert'"
             :id "Foo4")
            (flycheck-error-new
             :filename "test-javascript/missing-quote.js"
             :checker 'checker
             :buffer 'buffer
             :line 1
             :column nil
             :level 'error
             :message "Cannot read property 'id' of undefined"
             :id "Foo1"))))
      (it "parses Checkstyle XML using built-in XML parser"
        (let ((flycheck-xml-parser 'flycheck-parse-xml-region))
          (expect (flycheck-parse-checkstyle checkstyle-xml
                                             'checker 'buffer)
                  :to-be-equal-flycheck-errors
                  checkstyle-expected-errors)))

      (it "parses Checkstyle XML using libxml"
        (assume (fboundp 'libxml-parse-xml-region) "libxml not available")
        (let ((flycheck-xml-parser 'libxml-parse-xml-region))
          (expect (flycheck-parse-checkstyle checkstyle-xml
                                             'checker 'buffer)
                  :to-be-equal-flycheck-errors
                  checkstyle-expected-errors)))

      (it "parses Checkstyle XML with automatic parser selection"
        (expect (flycheck-parse-checkstyle checkstyle-xml
                                           'checker 'buffer)
                :to-be-equal-flycheck-errors
                checkstyle-expected-errors)))))

;;; test-error-parsers.el ends here
