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
(require 'test-helpers)

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
                  :to-be-equal-flycheck-errors nil)))))

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test c/c++-clang (c c++) error
      (let ((flycheck-disabled-checkers '(c/c++-gcc)))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/error.cpp" 'c++-mode
         '(2 20 error "no member named 'bar' in 'A'"
             :checker c/c++-clang)
         '(6 19 info "in instantiation of function template specialization 'foo<A>' requested here"
             :checker c/c++-clang)
         '(8 9 warning "unknown pragma ignored"
             :checker c/c++-clang))))

    (flycheck-buttercup-def-checker-test c/c++-clang (c c++) fatal-error
      (let ((flycheck-disabled-checkers '(c/c++-gcc)))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/includes.c" 'c-mode
         '(2 10 error "'library.h' file not found"
             :checker c/c++-clang))))

    (flycheck-buttercup-def-checker-test c/c++-clang (c c++) warnings
      (let ((flycheck-disabled-checkers '(c/c++-gcc c/c++-cppcheck)))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/warning.c" 'c-mode
         '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)
         '(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
             :checker c/c++-clang)
         '(8 7 warning "no message" :checker c/c++-clang))))

    (flycheck-buttercup-def-checker-test c/c++-clang (c c++) included-file-warning
      (let ((flycheck-clang-include-path '("./include"))
            (flycheck-disabled-checkers '(c/c++-gcc))
            (flycheck-relevant-error-other-file-minimum-level 'warning))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/in-included-file.cpp" 'c++-mode
         `(5 10 warning "unused variable 'unused'"
             :filename ,(flycheck-buttercup-resource-filename "language/c_c++/warning.c")
             :checker c/c++-clang)
         `(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
             :filename ,(flycheck-buttercup-resource-filename "language/c_c++/warning.c")
             :checker c/c++-clang)
         `(8 7 warning "no message"
             :filename ,(flycheck-buttercup-resource-filename "language/c_c++/warning.c")
             :checker c/c++-clang))))

    (flycheck-buttercup-def-checker-test c/c++-gcc (c c++) error
      (let ((flycheck-disabled-checkers '(c/c++-clang)))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/error.cpp" 'c++-mode
         '(2 20 error "'struct A' has no member named 'bar'"
             :checker c/c++-gcc)
         '(8 nil warning "ignoring #pragma nope"
             :id "-Wunknown-pragmas" :checker c/c++-gcc))))

    (flycheck-buttercup-def-checker-test c/c++-gcc (c c++) fatal-error
      (let ((flycheck-disabled-checkers '(c/c++-clang)))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/includes.c" 'c-mode
         '(2 10 error "library.h: No such file or directory"
             :checker c/c++-gcc))))

    (flycheck-buttercup-def-checker-test c/c++-gcc (c c++) warning
      (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck)))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/warning.c" 'c-mode
         '(5 10 warning "unused variable 'unused'"
             :id "-Wunused-variable" :checker c/c++-gcc)
         '(7 15 warning "comparison of integer expressions of different signedness: 'int' and 'unsigned int'"
             :id "-Wsign-compare" :checker c/c++-gcc)
         '(8 7 warning "#warning" :id "-Wcpp" :checker c/c++-gcc))))

    (flycheck-buttercup-def-checker-test c/c++-gcc (c c++) included-file-warning
      (let ((flycheck-gcc-include-path '("./include"))
            (flycheck-disabled-checkers '(c/c++-clang))
            (flycheck-relevant-error-other-file-minimum-level 'warning))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/in-included-file.cpp" 'c++-mode
         `(5 10 warning "unused variable 'unused'"
             :filename ,(flycheck-buttercup-resource-filename "language/c_c++/warning.c")
             :id "-Wunused-variable" :checker c/c++-gcc)
         `(7 15 warning "comparison of integer expressions of different signedness: 'int' and 'unsigned int'"
             :filename ,(flycheck-buttercup-resource-filename "language/c_c++/warning.c")
             :id "-Wsign-compare" :checker c/c++-gcc)
         `(8 7 warning "#warning"
             :filename ,(flycheck-buttercup-resource-filename "language/c_c++/warning.c")
             :id "-Wcpp" :checker c/c++-gcc))))

    (flycheck-buttercup-def-checker-test c/c++-cppcheck (c c++) nil
      (let ((flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
            (flycheck-cppcheck-inconclusive nil)
            (flycheck-cppcheck-checks '("style")))
        (flycheck-buttercup-should-syntax-check
         "language/c_c++/style2.cpp" 'c++-mode
         '(3 nil info "The scope of the variable 'i' can be reduced. Warning: Be careful when fixing this message, especially when there are inner loops. Here is an example where cppcheck will write that the scope for 'i' can be reduced:\nvoid f(int x)\n{\n    int i = 0;\n    if (x) {\n        // it's safe to move 'int i = 0;' here\n        for (int n = 0; n < 10; ++n) {\n            // it is possible but not safe to move 'int i = 0;' here\n            do_something(&i);\n        }\n    }\n}\nWhen you see this message it is always safe to reduce the variable scope 1 level."
             :id "variableScope" :checker c/c++-cppcheck))

        (flycheck-buttercup-should-syntax-check
         "language/c_c++/style.cpp" 'c-mode
         '(12 nil error "Code 'std::string' is invalid C code. Use --std or --language to configure the language."
              :id "syntaxError" :checker c/c++-cppcheck))

        (flycheck-buttercup-should-syntax-check
         "language/c_c++/style.cpp" 'c++-mode
         '(3 nil error "Division by zero." :id "zerodiv" :checker c/c++-cppcheck)
         '(5 nil info "Unused variable: unused" :id "unusedVariable"
             :checker c/c++-cppcheck)
         '(9 nil error "Division by zero." :id "zerodiv" :checker c/c++-cppcheck)
         '(12 nil warning "Parameter 'foo' is passed by value. It could be passed as a const reference which is usually faster and recommended in C++."
              :id "passedByValue" :checker c/c++-cppcheck))))))

;;; test-c_c++.el ends here
