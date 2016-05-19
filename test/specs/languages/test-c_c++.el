;;; test-cpp.el --- Flycheck Specs: C/C++      -*- lexical-binding: t; -*-

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

;; Specs for C/C++ support.

;;; Code:

(require 'flycheck-buttercup)

(describe "Language C/C++"
  (describe "The Cppcheck error parser"
    (let ((cppcheck-xml-without-errors "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.60.1\"/>
  <errors>
  </errors>
</results>")
          (cppcheck-xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<results version=\"2\">
  <cppcheck version=\"1.52\"/>
  <errors>
  <error id=\"toomanyconfigs\" severity=\"information\"
         msg=\"Too many #ifdef configurations - cppcheck only checks 12 \
configurations. Use --force to check all configurations. For more details, use \
--enable=information.\" verbose=\"The checking of the file will be interrupted \
because there are too many #ifdef configurations. Checking of all #ifdef \
configurations can be forced by --force command line option or from GUI \
preferences. However that may increase the checking time. For more details, use \
--enable=information.\">
  </error>
  <error id=\"nullPointer\" severity=\"error\"
         msg=\"Null pointer dereference\" verbose=\"Null pointer dereference\">
    <location file=\"foo\" line=\"4\"/>
    <location file=\"bar\" line=\"6\"/>
  </error>
  <error id=\"comparisonOfBoolWithInt\" severity=\"warning\"
         msg=\"Comparison of a boolean with integer that is neither 1 nor 0\" \
verbose=\"The expression &quot;x&quot; is of type 'bool' and it is compared \
against a integer value that is neither 1 nor 0.\">
    <location file=\"eggs\" line=\"2\"/>
  </error>
  </errors>
</results>"))
      (it "parses Cppcheck XML using automatic parser selection"
        (expect (flycheck-parse-cppcheck cppcheck-xml
                                         'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new
                  :filename "foo"
                  :buffer 'buffer
                  :checker 'checker
                  :line 4
                  :column nil
                  :level 'error
                  :message "Null pointer dereference"
                  :id "nullPointer")
                 (flycheck-error-new
                  :filename "bar"
                  :buffer 'buffer
                  :checker 'checker
                  :line 6
                  :column nil
                  :level 'error
                  :message "Null pointer dereference"
                  :id "nullPointer")
                 (flycheck-error-new
                  :filename "eggs"
                  :buffer 'buffer
                  :checker 'checker
                  :line 2
                  :column nil
                  :level 'warning
                  :message "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0."
                  :id "comparisonOfBoolWithInt"))))

      (it "parses Cppcheck XML without errors using automatic parser selection"
        (expect (flycheck-parse-cppcheck cppcheck-xml-without-errors
                                         'checker 'buffer)
                :to-be-equal-flycheck-errors nil))

      (it "parses Cppcheck XML without errors using builtin parser"
        (let ((flycheck-xml-parser #'flycheck-parse-xml-region))
          (expect (flycheck-parse-cppcheck cppcheck-xml-without-errors
                                           'checker 'buffer)
                  :to-be-equal-flycheck-errors nil))))))

;;; test-c_c++.el ends here
