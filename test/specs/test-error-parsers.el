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
  (describe "The SARIF parser"
    (it "parses results with locations and rule metadata"
      (let ((sarif "{\"version\":\"2.1.0\",\"runs\":[{\"tool\":{\"driver\":\
{\"name\":\"demo\",\"rules\":[{\"id\":\"no-undef\",\
\"defaultConfiguration\":{\"level\":\"error\"}}]}},\"results\":[\
{\"ruleIndex\":0,\"message\":{\"text\":\"undefined name\"},\"locations\":[\
{\"physicalLocation\":{\"artifactLocation\":{\"uri\":\"src/a.c\"},\"region\":\
{\"startLine\":3,\"startColumn\":5,\"endLine\":3,\"endColumn\":9}}}]},\
{\"ruleId\":\"style/x\",\"level\":\"warning\",\"message\":{\"text\":\"style\"},\
\"locations\":[{\"physicalLocation\":{\"artifactLocation\":{\"uri\":\"src/b.c\"},\
\"region\":{\"startLine\":7}}}]}]}]}"))
        (expect (flycheck-parse-sarif sarif 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 ;; Level comes from the referenced rule's default
                 (flycheck-error-new
                  :filename "src/a.c" :checker 'checker :buffer 'buffer
                  :line 3 :column 5 :end-line 3 :end-column 9
                  :level 'error :message "undefined name" :id "no-undef")
                 ;; Level comes from the result, region has only a line
                 (flycheck-error-new
                  :filename "src/b.c" :checker 'checker :buffer 'buffer
                  :line 7 :column nil :level 'warning
                  :message "style" :id "style/x")))))

    (it "maps SARIF levels and handles missing levels"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"name\":\"d\"}},\
\"results\":[\
{\"level\":\"note\",\"message\":{\"text\":\"n\"}},\
{\"level\":\"none\",\"message\":{\"text\":\"z\"}},\
{\"message\":{\"text\":\"unspecified\"}}]}]}"))
        (expect (mapcar #'flycheck-error-level
                        (flycheck-parse-sarif sarif 'checker 'buffer))
                ;; note and none map to info; a missing level defaults to
                ;; warning, as the SARIF spec prescribes
                :to-equal '(info info warning))))

    (it "decodes file URIs and percent-escapes"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"name\":\"d\"}},\
\"results\":[{\"message\":{\"text\":\"m\"},\"locations\":[{\"physicalLocation\":\
{\"artifactLocation\":{\"uri\":\"file:///home/me/a%20b.c\"},\
\"region\":{\"startLine\":1}}}]}]}]}"))
        (expect (flycheck-error-filename
                 (car (flycheck-parse-sarif sarif 'checker 'buffer)))
                :to-equal "/home/me/a b.c")))

    (it "handles multiple runs and empty results"
      (let ((sarif "{\"runs\":[\
{\"tool\":{\"driver\":{\"name\":\"a\"}},\"results\":[]},\
{\"tool\":{\"driver\":{\"name\":\"b\"}},\"results\":[\
{\"message\":{\"text\":\"from second run\"}}]}]}"))
        (expect (mapcar #'flycheck-error-message
                        (flycheck-parse-sarif sarif 'checker 'buffer))
                :to-equal '("from second run"))))

    (it "tolerates an out-of-range or negative ruleIndex"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"name\":\"d\",\
\"rules\":[{\"id\":\"R1\"}]}},\"results\":[\
{\"ruleIndex\":-1,\"message\":{\"text\":\"negative\"}},\
{\"ruleIndex\":5,\"message\":{\"text\":\"out of range\"}}]}]}"))
        ;; The sentinel index -1 and an out-of-range index must not crash;
        ;; the result just carries no rule id
        (expect (mapcar #'flycheck-error-id
                        (flycheck-parse-sarif sarif 'checker 'buffer))
                :to-equal '(nil nil))))

    (it "resolves a rule referenced by id alone"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"name\":\"d\",\
\"rules\":[{\"id\":\"E001\",\"defaultConfiguration\":{\"level\":\"error\"}}]}},\
\"results\":[{\"ruleId\":\"E001\",\"message\":{\"text\":\"m\"}}]}]}"))
        ;; No ruleIndex and no explicit level: the rule's default level
        ;; must still be found via the id
        (expect (flycheck-error-level
                 (car (flycheck-parse-sarif sarif 'checker 'buffer)))
                :to-be 'error)))

    (it "strips a non-localhost file URI authority"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"name\":\"d\"}},\
\"results\":[{\"message\":{\"text\":\"m\"},\"locations\":[{\"physicalLocation\":\
{\"artifactLocation\":{\"uri\":\"file://myhost/abs/path.py\"},\
\"region\":{\"startLine\":1}}}]}]}]}"))
        (expect (flycheck-error-filename
                 (car (flycheck-parse-sarif sarif 'checker 'buffer)))
                :to-equal "/abs/path.py")))

    (it "utf-8-decodes percent escapes in file URIs"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"name\":\"d\"}},\
\"results\":[{\"message\":{\"text\":\"m\"},\"locations\":[{\"physicalLocation\":\
{\"artifactLocation\":{\"uri\":\"file:///src/caf%C3%A9.py\"},\
\"region\":{\"startLine\":1}}}]}]}]}"))
        (expect (flycheck-error-filename
                 (car (flycheck-parse-sarif sarif 'checker 'buffer)))
                :to-equal "/src/café.py"))))

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
