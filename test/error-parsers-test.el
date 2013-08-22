;;; error-parsers-test.el --- Tests for error parsers  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

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

;; Tests for error parser functions

;;; Code:

(require 'test-helper)

(defconst flycheck-checkstyle-xml
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<checkstyle version=\"4.3\">
  <file name=\"test-javascript/missing-semicolon.js\">
    <error line=\"3\" column=\"21\" severity=\"error\" message=\"Missing semicolon.\" source=\"Missing semicolon.\" />
    <error line=\"3\" severity=\"warning\" message=\"Implied global &apos;alert&apos;\" source=\"jshint.implied-globals\" />
  </file>
  <file name=\"test-javascript/missing-quote.js\">
    <error line=\"undefined\" column=\"undefined\" severity=\"error\" message=\"Cannot read property &apos;id&apos; of undefined\" source=\"\" />
  </file>
</checkstyle>"
  "Example Checkstyle output from jshint.")

(defconst flycheck-checkstyle-expected-errors
  (list
   (flycheck-error-new
    :filename "test-javascript/missing-semicolon.js"
    :line 3
    :column 21
    :level 'error
    :message "Missing semicolon.")
   (flycheck-error-new
    :filename "test-javascript/missing-semicolon.js"
    :line 3
    :column nil
    :level 'warning
    :message "Implied global 'alert'")
   (flycheck-error-new
    :filename "test-javascript/missing-quote.js"
    :line nil
    :column nil
    :level 'error
    :message "Cannot read property 'id' of undefined"))
  "Errors to be parsed from `flycheck-checkstyle-xml'.")


(ert-deftest flycheck-parse-checkstyle-xml ()
  "Test Checkstyle parsing with xml.el"
  (let ((flycheck-xml-parser 'flycheck-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle-libxml2 ()
  "Test Checkstyle parsing with libxml2."
  :expected-result (if (fboundp 'libxml-parse-xml-region) :passed :failed)
  (let ((flycheck-xml-parser 'libxml-parse-xml-region))
    (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                   flycheck-checkstyle-expected-errors))))

(ert-deftest flycheck-parse-checkstyle-auto ()
  "Test Checkstyle parsing with the automatically chosen parsed."
  (should (equal (flycheck-parse-checkstyle flycheck-checkstyle-xml nil nil)
                 flycheck-checkstyle-expected-errors)))

 (defconst flycheck-cppcheck-xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.52\"/>
  <errors>
  <error id=\"toomanyconfigs\" severity=\"information\" msg=\"Too many #ifdef configurations - cppcheck only checks 12 configurations. Use --force to check all configurations. For more details, use --enable=information.\" verbose=\"The checking \
of the file will be interrupted because there are too many #ifdef configurations. Checking of all #ifdef configurations can be forced by --force command line option or from GUI preferences. However that may increase the checking time. For\
 more details, use --enable=information.\">
  </error>
  <error id=\"nullPointer\" severity=\"error\" msg=\"Null pointer dereference\" verbose=\"Null pointer dereference\">
  <location file=\"foo\" line=\"4\"/>
  <location file=\"bar\" line=\"6\"/>
  </error>
  <error id=\"comparisonOfBoolWithInt\" severity=\"warning\" msg=\"Comparison of a boolean with integer that is neither 1 nor 0\" verbose=\"The expression &quot;x&quot; is of type 'bool' and it is compared against a integer value that is neither 1 nor 0.\">
    <location file=\"eggs\" line=\"2\"/>
  </error>
  </errors>
</results>"
  "Example cppcheck output.")

(defconst flycheck-cppcheck-expected-errors
  (list
   (flycheck-error-new
    :filename "foo"
    :line 4
    :column nil
    :level 'error
    :message "Null pointer dereference")
   (flycheck-error-new
    :filename "bar"
    :line 6
    :column nil
    :level 'error
    :message "Null pointer dereference")
   (flycheck-error-new
    :filename "eggs"
    :line 2
    :column nil
    :level 'warning
    :message "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0.")))

(ert-deftest flycheck-parse-cppcheck ()
  (should (equal (flycheck-parse-cppcheck flycheck-cppcheck-xml nil nil)
                 flycheck-cppcheck-expected-errors)))

;;; error-parsers-test.el ends here
