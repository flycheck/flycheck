;;; builtin-checkers-test.el --- Tests for all builtin checkers  -*- lexical-binding: t; -*-

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

;; Tests for all builtin checkers.

;;; Code:

(require 'test-helper)

;; Load all modes used by our tests
(--each '(sh-script
          cc-mode
          coffee-mode
          css-mode
          d-mode
          elixir-mode
          erlang
          elixir-mode
          go-mode
          haml-mode
          haskell-mode
          web-mode
          js2-mode
          js3-mode
          less-css-mode
          lua-mode
          cperl-mode
          php-mode
          puppet-mode
          rust-mode
          sass-mode
          scala-mode2
          scss-mode)
  (require it))

(eval-when-compile
  ;; Prevent warnings about unused lexical variables
  (require 'js2-mode)
  (require 'js3-mode))

(require 'sh-script)                    ; For `sh-set-shell'

(ert-deftest checker-bash-missing-semicolon ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'bash)
  (flycheck-testsuite-with-hook sh-mode-hook (sh-set-shell "bash" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/bash-syntax-error.bash" 'sh-mode nil
     '(5 nil "syntax error near unexpected token `fi'" error)
     '(5 nil "`fi'" error))))

(ert-deftest checker-c/c++-clang-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-warning.c" 'c-mode 'c/c++-cppcheck
   '(5 10 "unused variable 'unused'" warning)
   '(7 15 "comparison of integers of different signs: 'int' and 'unsigned int'" warning)))

(ert-deftest checker-c/c++-clang-warning-customized ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c-mode-hook
      ;; Disable conversion checks by removing -Wextra, but additionally warn
      ;; about missing prototypes, which isn't included in -Wextra
      (setq flycheck-clang-warnings '("all" "missing-prototypes"))
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-warning.c" 'c-mode 'c/c++-cppcheck
     '(3 5 "no previous prototype for function 'f'" warning)
     '(5 10 "unused variable 'unused'" warning))))

(ert-deftest checker-c/c++-clang-fatal-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-fatal-error.c" 'c-mode nil
   '(1 10 "'c_c++-clang-header.h' file not found" error)))

(ert-deftest checker-c/c++-clang-include-path ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c-mode-hook
      (setq flycheck-clang-include-path '("."))
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-fatal-error.c" 'c-mode nil)))

(ert-deftest checker-c/c++-clang-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-error.cpp" 'c++-mode nil
   '(5 18 "implicit instantiation of undefined template 'test<false>'" error)))

(ert-deftest checker-c/c++-cppcheck-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-cppcheck-error.c" 'c-mode 'c/c++-clang
   '(4 nil "Null pointer dereference" error)))

(ert-deftest checker-c/c++-cppcheck-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-cppcheck-warning.c" 'c-mode 'c/c++-clang
   '(2 nil "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0." warning)))

(ert-deftest checker-c/c++-cppcheck-style ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-cppcheck-style.c" 'c-mode 'c/c++-clang
   '(3 nil "Unused variable: unused" warning)))

(ert-deftest checker-c/c++-cppcheck-style-suppressed ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-with-hook c-mode-hook
      (setq flycheck-cppcheck-checks nil)
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-cppcheck-style.c" 'c-mode 'c/c++-clang)))

(ert-deftest checker-c/c++-cppcheck-multiple-checks ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-with-hook c++-mode-hook
      (setq flycheck-cppcheck-checks '("performance" "portability"))
      (flycheck-testsuite-should-syntax-check
       "checkers/c_c++-cppcheck-multiple-checks.cpp" 'c++-mode 'c/c++-clang
       '(2 nil "Extra qualification 'A::' unnecessary and considered an error by many compilers." warning)
       '(9 nil "Prefix ++/-- operators should be preferred for non-primitive types. Pre-increment/decrement can be more efficient than post-increment/decrement. Post-increment/decrement usually involves keeping a copy of the previous value around and adds a little extra code." warning))))

(ert-deftest checker-coffee-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee)
  (flycheck-testsuite-should-syntax-check
   "checkers/coffee-syntax-error.coffee" 'coffee-mode nil
   '(4 7 "missing \", starting" error)))

(ert-deftest checker-coffee-coffeelint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee-coffeelint)
  (flycheck-testsuite-should-syntax-check
   "checkers/coffee-coffeelint-error.coffee" 'coffee-mode nil
   '(4 nil "Throwing strings is forbidden" error
       :checker coffee-coffeelint)))

(ert-deftest checker-coffee-coffeelint-warning ()
  "Test a coffeelint error demoted to a warning via config file."
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee-coffeelint)
  (flycheck-testsuite-with-hook coffee-mode-hook
      (setq flycheck-coffeelintrc "coffeelint.json")
    (flycheck-testsuite-should-syntax-check
     "checkers/coffee-coffeelint-error.coffee" 'coffee-mode nil
     '(4 nil "Throwing strings is forbidden" warning
         :checker coffee-coffeelint))))

(ert-deftest checker-css-csslint-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'css-csslint)
  (flycheck-testsuite-should-syntax-check
   "checkers/css-csslint-warning.css" 'css-mode nil
   '(3 6 "Heading (h1) should not be qualified." warning)))

(ert-deftest checker-css-csslint-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'css-csslint)
  (flycheck-testsuite-should-syntax-check
   "checkers/css-syntax-error.css" 'css-mode nil
   '(4 16 "Expected LBRACE at line 4, col 16." error)
   '(4 16 "Unexpected token '100%' at line 4, col 16." error)
   '(4 20 "Unexpected token ';' at line 4, col 20." error)
   '(5 1 "Unexpected token '}' at line 5, col 1." error)))

(ert-deftest flycheck-d-module-name ()
  (with-temp-buffer
    (insert "Hello world")
    (should-not (flycheck-d-module-name)))
  (with-temp-buffer
    (insert "module spam.with.eggs;")
    (should (string= (flycheck-d-module-name) "spam.with.eggs"))))

(ert-deftest flycheck-d-base-directory ()
  (flycheck-testsuite-with-resource-buffer "checkers/d-dmd-warning.d"
    (should (f-same? (flycheck-d-base-directory)
                     (f-join flycheck-testsuite-resources-dir "checkers"))))
  (flycheck-testsuite-with-resource-buffer "checkers/d-dmd-warning.d"
    (goto-char (point-min))
    (insert "module checkers.d_dmd_warning;")
    (should (f-same? (flycheck-d-base-directory)
                     flycheck-testsuite-resources-dir)))
  (flycheck-testsuite-with-resource-buffer "checkers/package.d"
    (should (f-same? (flycheck-d-base-directory)
                     flycheck-testsuite-resources-dir))))

(ert-deftest checker-d-dmd-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'd-dmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/d-dmd-syntax-error.d" 'd-mode nil
   '(2 nil "module studio is in file 'std/studio.d' which cannot be read" error)))

(ert-deftest checker-d-dmd-syntax-error-without-module ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'd-dmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/d_dmd_syntax_error_without_module.d" 'd-mode nil
   '(5 nil "undefined identifier writel, did you mean template write(T...)(T args) if (!is(T[0] : File))?" error)))

(ert-deftest checker-d-dmd-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'd-dmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/d-dmd-warning.d" 'd-mode nil
   '(6 nil "statement is not reachable" warning)))

(ert-deftest checker-d-dmd-deprecated ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'd-dmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/d-dmd-deprecated.d" 'd-mode nil
   '(11 nil "function d_dmd_deprecated.foo is deprecated" warning)))

(ert-deftest checker-elixir-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-error.ex" 'elixir-mode nil
   '(5 nil "function puts/1 undefined" error)))

(ert-deftest checker-elixir-warnings ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warnings.ex" 'elixir-mode nil
   '(5 nil "variable a is unused" warning)
   '(6 nil "variable a shadowed in 'fun'" warning)
   '(14 nil "this clause cannot match because a previous clause at line 11 always matches" warning)))

(ert-deftest checker-emacs-lisp-checkdoc-warning ()
  "Test a checkdoc warning caused by a missing period in a docstring."
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-checkdoc-warning.el"
   'emacs-lisp-mode nil                 ; Checkdoc is chained after Emacs Lisp
   '(12 nil "First sentence should end with punctuation" warning
        :checker emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-checkdoc-warning-compressed ()
  "Test a checkdoc warning caused by a missing period in a docstring."
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-checkdoc-warning.el.gz"
   'emacs-lisp-mode nil
   '(12 nil "First sentence should end with punctuation" warning
        :checker emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-checkdoc-works-without-buffer-file-name ()
  "Test checkdoc checker in buffers without file names.

Regression test for https://github.com/flycheck/flycheck/issues/73 and
https://github.com/bbatsov/prelude/issues/259."
  (with-temp-buffer
    (insert ";;; Hello world\n(message \"foo\")")
    (emacs-lisp-mode)
    (should-not (buffer-file-name))
    (flycheck-testsuite-buffer-sync)
    ;; Just check that there are any errors, i.e. that the checker was used and
    ;; worked.
    (flycheck-testsuite-should-errors)))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-autoloads ()
  "Test that CheckDoc does not check autoloads buffers."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (rename-buffer "foo-autoloads.el")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-cask ()
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Cartony")  ; No real carton file
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Carton")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Cask")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-sytnax-error ()
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-syntax-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(3 1 "End of file during parsing" error)))

(ert-deftest checker-emacs-lisp-syntax-error-compressed ()
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-syntax-error.el.gz" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(3 1 "End of file during parsing" error)))

(ert-deftest checker-emacs-lisp-error ()
  ;; Determine how the Emacs message for load file errors looks like: In Emacs
  ;; Snapshot, the message has three parts because the underlying file error is
  ;; contained in the message.  In stable release the file error itself is
  ;; missing and the message has only two parts.
  (let* ((parts (condition-case err
                    (require 'does-not-exist)
                  (file-error (cdr err))))
         (msg (format "Cannot open load file: %sdummy-package"
                      (if (= (length parts) 2) ""
                        "no such file or directory, "))))
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
     `(3 1 ,msg error))))

(ert-deftest checker-emacs-lisp-error-load-path ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-load-path
            (list (flycheck-testsuite-resource-filename
                   "dummy-elpa/dummy-package-0.1")))
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-error-packages ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-package-user-dir
            (flycheck-testsuite-resource-filename "dummy-elpa")
            flycheck-emacs-lisp-initialize-packages t)
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc)))

(ert-deftest checker-emacs-lisp-warning ()
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-warning.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(4 6 "message called with 0 arguments,\n    but requires 1+" warning)
   '(8 1 "the function `dummy-package-foo'\n    is not known to be defined." warning)))

(ert-deftest checker-emacs-lisp-warning-packages ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-package-user-dir
            (flycheck-testsuite-resource-filename "dummy-elpa")
            flycheck-emacs-lisp-initialize-packages t)
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-warning.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
     '(4 6 "message called with 0 arguments,\n    but requires 1+" warning))))

(ert-deftest checker-emacs-lisp-inhibited-no-byte-compile ()
  "Test that Emacs Lisp does not check when byte compilation is
  disabled."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (set (make-local-variable 'no-byte-compile) t)
    (should (buffer-file-name))
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-no-file-name ()
  "Test that Emacs Lisp does not check buffers without file names."
  (with-temp-buffer
    (insert "(message \"Hello World\")")
    (emacs-lisp-mode)
    (should (not (buffer-file-name)))
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-autoloads ()
  "Test that Emacs Lisp does not check autoloads buffers.

These buffers are temporary buffers generated during package
installation, which may not be byte compiled, and hence the
checker will refuse to check these.

See URL `https://github.com/flycheck/flycheck/issues/45' and URL
`https://github.com/bbatsov/prelude/issues/253'."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp))
    (rename-buffer "foo-autoloads.el")
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-erlang-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-error.erl" 'erlang-mode nil
   '(7 nil "head mismatch" error)))

(ert-deftest checker-erlang-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-warning.erl" 'erlang-mode nil
   '(6 nil "wrong number of arguments in format call" warning)))

(ert-deftest checker-go-gofmt-syntax-error ()
  "Test a syntax error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-gofmt)
  (flycheck-testsuite-should-syntax-check
   "checkers/go-syntax-error.go" 'go-mode nil
   '(5 9 "expected '(', found 'IDENT' ta" error)
   '(6 1 "expected ')', found '}'" error)))

(ert-deftest checker-go-build-error ()
  "Test an import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-build)
  (flycheck-testsuite-should-syntax-check
   "checkers/go-testpackage/go-build-error.go" 'go-mode nil
   '(6 nil "undefined: fmt" error :checker go-build)))

(ert-deftest checker-go-test-error ()
  "Test an import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-test)
  (flycheck-testsuite-should-syntax-check
   "checkers/go-testpackage/go-test-error_test.go" 'go-mode nil
   '(8 nil "undefined: fmt" error :checker go-test)))

(ert-deftest checker-haml-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haml)
  (flycheck-testsuite-should-syntax-check
   "checkers/haml-error.haml" 'haml-mode nil
   '(5 nil "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces." error :filename nil)))

(ert-deftest checker-haskell-hdevtools-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hdevtools)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hdevtools-error.hs" 'haskell-mode nil
   '(1 8 "Not in scope: `unknown'" error
       :checker haskell-hdevtools)))

(ert-deftest checker-haskell-hdevtools-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hdevtools)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hdevtools-warning.hs" 'haskell-mode nil
   '(3 1 "Top-level binding with no type signature: foo :: Integer" warning
       :checker haskell-hdevtools)))

(ert-deftest checker-haskell-ghc-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-ghc)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-ghc-error.hs" 'haskell-mode 'haskell-hdevtools
   '(3 1 "parse error on input `module'" error
       :checker haskell-ghc)))

(ert-deftest checker-haskell-ghc-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-ghc)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-ghc-warning.hs" 'haskell-mode 'haskell-hdevtools
   '(3 1 "Top-level binding with no type signature: foo :: Integer" warning
       :checker haskell-ghc)))

(ert-deftest checker-haskell-hlint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hlint-error.hs" 'haskell-mode nil
   '(4 1 "Eta reduce\nFound:\n  warnMe xs = map lines xs\nWhy not:\n  warnMe = map lines" error
       :checker haskell-hlint)))

(ert-deftest checker-haskell-hlint-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-hlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/haskell-hlint-warning.hs" 'haskell-mode nil
   '(2 8 "Redundant bracket\nFound:\n  (putStrLn \"Foobar\")\nWhy not:\n  putStrLn \"Foobar\"" warning
       :checker haskell-hlint)))

(ert-deftest checker-html-tidy-warning-and-error ()
  "Test an error caused by an unknown tag."
  :expected-result (flycheck-testsuite-fail-unless-checker 'html-tidy)
  (flycheck-testsuite-should-syntax-check
   "checkers/html-tidy-warning-and-error.html" '(html-mode web-mode) nil
   '(3 1 "missing <!DOCTYPE> declaration" warning :filename nil)
   '(8 5 "<spam> is not recognized!" error :filename nil)
   '(8 5 "discarding unexpected <spam>" warning :filename nil)))

(ert-deftest checker-javascript-jshint-syntax-error ()
  "A missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  ;; Silence JS2 and JS3 parsers
  (let ((js2-mode-show-parse-errors nil)
        (js2-mode-show-strict-warnings nil)
        (js3-mode-show-parse-errors nil))
    (flycheck-testsuite-should-syntax-check
     "checkers/javascript-jshint-syntax-error.js" '(js-mode js2-mode js3-mode) nil
     '(3 25 "Unclosed string." error)
     '(4 1 "Unclosed string." error)
     '(3 11 "Unclosed string." error)
     '(3 nil "Unused variable: 'foo'" warning)
     '(4 1 "Missing semicolon." error))))

(ert-deftest checker-javascript-jshint-error ()
  "Use eval()"
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  (flycheck-testsuite-should-syntax-check
   "checkers/javascript-jshint-error.js" '(js-mode js2-mode js3-mode) nil
   '(3 1 "eval can be harmful." error)))

(ert-deftest checker-javascript-jshint-warning ()
  "An unused variable."
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  (flycheck-testsuite-should-syntax-check
   "checkers/javascript-jshint-warning.js" '(js-mode js2-mode js3-mode) nil
   '(5 nil "Unused variable: 'foo'" warning)))

(ert-deftest checker-json-jsonlint-error ()
  "Test a syntax error from multiple top-level objects."
  :expected-result (flycheck-testsuite-fail-unless-checker 'json-jsonlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/json-jsonlint-error.json" 'text-mode nil
    '(1 42 "found: ',' - expected: 'EOF'." error)))

(ert-deftest checker-less-file-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'less)
  (flycheck-testsuite-should-syntax-check
   "checkers/less-file-error.less" 'less-css-mode nil
   '(3 1 "'no-such-file.less' wasn't found" error)))

(ert-deftest checker-less-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'less)
  (flycheck-testsuite-should-syntax-check
   "checkers/less-syntax-error.less" 'less-css-mode nil
   '(2 1 "missing closing `}`" error)))

(ert-deftest checker-lua-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'lua)
  (flycheck-testsuite-should-syntax-check
   "checkers/lua-syntax-error.lua" 'lua-mode nil
   '(5 nil "unfinished string near '\"oh no'" error)))

(ert-deftest checker-perl-error ()
  "Test an unused variable with the Perl checker."
  :expected-result (flycheck-testsuite-fail-unless-checker 'perl)
  (flycheck-testsuite-should-syntax-check
   "checkers/perl-error.pl" '(perl-mode cperl-mode) nil
   '(4 nil "Name \"main::x\" used only once: possible typo" error)))

(ert-deftest checker-perl-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'perl)
  "Test a syntax error with the Perl checker."
  (flycheck-testsuite-should-syntax-check
   "checkers/perl-syntax-error.pl" '(perl-mode cperl-mode) nil
   '(4 nil "syntax error" error)))

(ert-deftest checker-php-syntax-error ()
  "Test the T_PAAMAYIM_NEKUDOTAYIM error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'php)
  (flycheck-testsuite-should-syntax-check
   "checkers/php-syntax-error.php" 'php-mode nil
   '(8 nil "syntax error, unexpected ')', expecting '('" error)))

(ert-deftest checker-php-phpcs-error ()
  "Test an uppercase keyword error by phpcs."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-testsuite-should-syntax-check
   "checkers/php-phpcs-error.php" 'php-mode nil
   '(19 8 "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\"" error
        :checker php-phpcs)))

(ert-deftest checker-php-phpcs-error-phpcs-standard ()
  "Test an uppercase keyword error by phpcs."
  :expected-result (flycheck-testsuite-fail-unless-checkers 'php 'php-phpcs)
  (flycheck-testsuite-with-hook php-mode-hook
      (setq flycheck-phpcs-standard "Zend")
    (flycheck-testsuite-should-syntax-check
     "checkers/php-phpcs-error.php" 'php-mode nil
     '(21 1 "A closing tag is not permitted at the end of a PHP file" error
          :checker php-phpcs))))

(ert-deftest checker-puppet-parser-singleline-syntax-error ()
  "Test a real syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-singleline.pp" 'puppet-mode nil
   '(3 nil "Syntax error at ','; expected '}'" error)))

(ert-deftest checker-puppet-parser-multiline-syntax-error ()
  "Test a real (multi line) syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-multiline.pp" 'puppet-mode nil
   '(8 nil "Unclosed quote after '' in 'something
}
'" error)))

(ert-deftest checker-puppet-lint-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-lint-warning.pp" 'puppet-mode nil
   '(2 nil "case statement without a default case" warning
       :checker puppet-lint)))

(ert-deftest checker-puppet-lint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-lint/error/manifests/puppet-lint-error.pp" 'puppet-mode nil
   '(2 nil "mlayout not in autoload module layout" error
       :checker puppet-lint)))

(ert-deftest checker-python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode 'python-pylint
    '(3 13 "E901 SyntaxError: invalid syntax" error)))

(ert-deftest checker-python-flake8-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-warning.py" 'python-mode 'python-pylint
    '(3 1 "F401 're' imported but unused" warning)))

(ert-deftest checker-python-flake8-warning-ignored ()
  "Test an unused import being ignored with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-warning.py" 'python-mode 'python-pylint)))

(ert-deftest checker-python-flake8-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-error.py" 'python-mode 'python-pylint
   '(6 13 "E251 unexpected spaces around keyword / parameter equals" error)
   '(6 15 "E251 unexpected spaces around keyword / parameter equals" error)))

(ert-deftest checker-python-flake8-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-error.py" 'python-mode 'python-pylint)))

(ert-deftest checker-python-flake8-warning-maximum-complexity ()
  "Test superfluous spaces with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-complexity 4)
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-warning-maximum-complexity.py"
     'python-mode 'python-pylint
     '(6 1 "C901 'foo' is too complex (4)" warning))))

(ert-deftest checker-python-flake8-error-maximum-line-length ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-line-length 50)
    (flycheck-testsuite-should-syntax-check
     "checkers/python-flake8-error-maximum-line-length.py"
     'python-mode 'python-pylint
     '(5 51 "E501 line too long (61 > 50 characters)" error))))

(ert-deftest checker-python-flake8-warning-naming ()
  "PEP8 compliant names with Flake8 and pep8-naming."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-flake8-warning-naming.py" 'python-mode 'python-pylint
   '(6 7 "N801 class names should use CapWords convention" warning)
   '(7 9 "N802 function name should be lowercase" warning)
   '(8 9 "N806 variable in function should be lowercase" warning)))

(ert-deftest checker-python-pylint-syntax-error ()
  "Test a real syntax error with pylint."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode 'python-flake8
   '(3 nil "invalid syntax (E0001)" error)))

(ert-deftest checker-python-pylint-error ()
  "Test an unknown module with pylint."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-pylint-error.py" 'python-mode 'python-flake8
   '(3 nil "Unable to import 'spam' (F0401)" error)))

(ert-deftest checker-python-pylint-used-map ()
  "Test usage of the map() builtin with the pylint checker."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-pylint-warning.py" 'python-mode 'python-flake8
   '(3 nil "Used builtin function 'map' (W0141)" warning)))

(ert-deftest checker-rst-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "checkers/rst-warning.rst" 'rst-mode nil
   '(8 nil "Title underline too short." warning)
   '(11 nil "Title underline too short." warning)))

(ert-deftest checker-rst-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "checkers/rst-error.rst" 'rst-mode nil
   '(5 nil "Unknown target name: \"restructuredtext\"." error)
   '(7 nil "Unknown target name: \"cool\"." error)))

(ert-deftest checker-rst-severe ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "checkers/rst-severe.rst" 'rst-mode nil
   '(6 nil "Unexpected section title." error)
   '(11 nil "Unexpected section title." error)))

(defun flycheck-testsuite-jruby-expected-result ()
  (if (flycheck-testsuite-travis-ci-p) :failed
    (flycheck-testsuite-fail-unless-checker 'ruby-jruby)))

(ert-deftest checker-ruby-jruby-syntax-error ()
  :expected-result (flycheck-testsuite-jruby-expected-result)
  (flycheck-testsuite-not-on-travis)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode '(ruby-rubocop ruby)
   '(5 nil "syntax error, unexpected tCONSTANT" error)))

(ert-deftest checker-ruby-jruby-warning ()
  :expected-result (flycheck-testsuite-jruby-expected-result)
  (flycheck-testsuite-not-on-travis)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-warning.rb" 'ruby-mode '(ruby-rubocop ruby)
   '(3 nil "Useless use of == in void context." warning)))

(ert-deftest checker-ruby-rubocop-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode nil
   '(5 7 "unexpected token tCONSTANT" error)
   '(5 24 "unterminated string meets end of file" error)))

(ert-deftest checker-ruby-rubocop-warnings ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-rubocop-warnings.rb" 'ruby-mode nil
   '(1 1 "Missing utf-8 encoding comment." warning)
   '(3 1 "Assigned but unused variable - arr" warning)
   '(3 14 "Use snake_case for symbols." warning)
   '(4 6 "Prefer single-quoted strings when you don't need string interpolation or special symbols." warning)))

(ert-deftest checker-ruby-rubocop-warnings-disabled ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-with-hook ruby-mode-hook
      (setq flycheck-rubocoprc "rubocop.yml")
    (flycheck-testsuite-should-syntax-check
     "checkers/ruby-rubocop-warnings.rb" 'ruby-mode nil
     '(1 1 "Missing utf-8 encoding comment." warning)
     '(3 1 "Assigned but unused variable - arr" warning)
     '(4 6 "Prefer single-quoted strings when you don't need string interpolation or special symbols." warning))))

(ert-deftest checker-ruby-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode '(ruby-rubocop)
   '(5 nil "syntax error, unexpected tCONSTANT, expecting $end" error)))

(ert-deftest checker-ruby-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-warning.rb" 'ruby-mode '(ruby-rubocop)
   '(3 nil "possibly useless use of == in void context" warning)))

(ert-deftest checker-rust-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rust)
  (flycheck-testsuite-should-syntax-check
   "checkers/rust-syntax-error.rs" 'rust-mode nil
   '(3 10 "expected `{` but found `bla`" error)))

(ert-deftest checker-sass-error ()
  "Test a syntax error caused by inconsistent indentation."
  :expected-result (flycheck-testsuite-fail-unless-checker 'sass)
  (flycheck-testsuite-should-syntax-check
   "checkers/sass-error.sass" 'sass-mode nil
    '(5 nil "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces." error)))

(ert-deftest checker-scala-syntax-error ()
  :expected-result (if (flycheck-testsuite-travis-ci-p) :failed
                     (flycheck-testsuite-fail-unless-checker 'scala))
  (flycheck-testsuite-not-on-travis)
  (flycheck-testsuite-should-syntax-check
   "checkers/scala-syntax-error.scala" 'scala-mode nil
   '(3 nil "identifier expected but '{' found." error)))

(ert-deftest checker-scss-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'scss)
  (flycheck-testsuite-should-syntax-check
   "checkers/scss-error.scss" 'scss-mode nil
   '(3 nil "Invalid CSS after \"        c olor:\": expected pseudoclass or pseudoelement, was \" red;\"" error)))

(ert-deftest checker-sh-bash-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'sh-bash)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/sh-bash-syntax-error.sh" 'sh-mode '(sh-dash)
     '(3 nil "syntax error near unexpected token `('" error)
     '(3 nil "`cat <(echo blah)'" error))))

(ert-deftest checker-sh-dash-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'sh-dash)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/sh-dash-syntax-error.sh" 'sh-mode '(sh-bash)
     '(5 nil "Syntax error: \"fi\" unexpected (expecting \"then\")" error))))

(ert-deftest checker-tex-chktex-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-chktex)
  (flycheck-testsuite-should-syntax-check
   "checkers/tex-chktex-warning.tex" 'latex-mode nil
   '(9 28 "13:Intersentence spacing (`\\@') should perhaps be used." warning)))

(ert-deftest checker-tex-lacheck-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-lacheck)
  (flycheck-testsuite-should-syntax-check
   "checkers/tex-lacheck-warning.tex" 'latex-mode 'tex-chktex
    '(9 nil "possible unwanted space at \"{\"" warning)))

(ert-deftest checker-xml-xmlstarlet-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-testsuite-should-syntax-check
   "checkers/xml-syntax-error.xml" 'nxml-mode 'xml-xmllint
   '(4 10 "Opening and ending tag mismatch: spam line 3 and with" error)))

(ert-deftest checker-xml-xmllint-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'xml-xmllint)
  (flycheck-testsuite-should-syntax-check
   "checkers/xml-syntax-error.xml" 'nxml-mode 'xml-xmlstarlet
   '(4 nil "parser error : Opening and ending tag mismatch: spam line 3 and with" error)
   '(5 nil "parser error : Extra content at the end of the document" error)))

(ert-deftest checker-zsh-syntax-error ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'zsh)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "zsh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/zsh-syntax-error.zsh" 'sh-mode nil
     '(5 nil "parse error near `fi'" error))))

;;; builtin-checkers-test.el ends here
