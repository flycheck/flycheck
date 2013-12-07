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

;; Tell the byte compiler about the variables we'll use
(defvar js2-mode-show-strict-warnings)
(defvar js2-mode-show-parse-errors)
(defvar js3-mode-show-parse-errors)
(autoload 'sh-set-shell "sh-script")

(ert-deftest flycheck/asciidoc ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'asciidoc)
  (flycheck-testsuite-should-syntax-check
   "checkers/asciidoc.adoc" 'adoc-mode
   '(1 nil warning "missing style: [paradef-default]: paragraph" :checker asciidoc)
   '(3 nil warning "old tables syntax" :checker asciidoc)
   '(11 nil error "[tabledef-default] illegal width=%60%" :checker asciidoc)))

(ert-deftest flycheck/bash ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'bash)
  (flycheck-testsuite-with-hook sh-mode-hook (sh-set-shell "bash" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/bash-syntax-error.bash" 'sh-mode
     '(5 nil error "syntax error near unexpected token `fi'" :checker bash)
     '(5 nil error "`fi'" :checker bash))))

(ert-deftest flycheck/c/c++-clang-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-without-checkers c/c++-cppcheck
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-warning.c" 'c-mode
     '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)
     '(7 15 warning "comparison of integers of different signs: 'int' and 'unsigned int'"
         :checker c/c++-clang))))

(ert-deftest flycheck/c/c++-clang-warning-customized ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c-mode-hook
      ;; Disable conversion checks by removing -Wextra, but additionally warn
      ;; about missing prototypes, which isn't included in -Wextra
      (setq flycheck-clang-warnings '("all" "missing-prototypes"))
    (flycheck-testsuite-without-checkers c/c++-cppcheck
      (flycheck-testsuite-should-syntax-check
       "checkers/c_c++-clang-warning.c" 'c-mode
       '(3 5 warning "no previous prototype for function 'f'"
           :checker c/c++-clang)
       '(5 10 warning "unused variable 'unused'" :checker c/c++-clang)))))

(ert-deftest flycheck/c/c++-clang-fatal-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-fatal-error.c" 'c-mode
   '(2 10 error "'c_c++-clang-library-header.h' file not found"
       :checker c/c++-clang)))

(ert-deftest flycheck/c/c++-clang-include-path ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c-mode-hook
      (setq flycheck-clang-include-path '("./include"))
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-fatal-error.c" 'c-mode)))

(ert-deftest flycheck/c/c++-clang-includes ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c++-mode-hook
      (setq flycheck-clang-includes
            (list (flycheck-testsuite-resource-filename "checkers/include/c_c++-clang-library-header.h")))
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang))))

(ert-deftest flycheck/c/c++-clang-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-should-syntax-check
   "checkers/c_c++-clang-error.cpp" 'c++-mode
   '(3 23 info "template is declared here" :checker c/c++-clang)
   '(8 17 error "implicit instantiation of undefined template 'test<false>'"
       :checker c/c++-clang)
   '(10 16 error "use of undeclared identifier 'nullptr'"
        :checker c/c++-clang)))

(ert-deftest flycheck/c/c++-clang-error-language-standard ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c++-mode-hook
      (setq flycheck-clang-language-standard "c++11")
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-error.cpp" 'c++-mode
     '(3 23 info "template is declared here" :checker c/c++-clang)
     '(8 17 error "implicit instantiation of undefined template 'test<false>'"
         :checker c/c++-clang))))

(ert-deftest flycheck/c/c++-clang-standard-library ()
  :expected-result :failed
  ;; I simply have no idea how to test for a standard library. Suggestions welcome
  (error "Not implemented"))

(ert-deftest flycheck/c/c++-clang-error-definitions ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c++-mode-hook
      (setq flycheck-clang-definitions '("FLYCHECK_LOCAL" "FLYCHECK_LIBRARY"))
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-error.cpp" 'c++-mode
     '(10 16 error "use of undeclared identifier 'nullptr'"
          :checker c/c++-clang))))

(ert-deftest flycheck/c/c++-clang-error-no-rtti ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-clang)
  (flycheck-testsuite-with-hook c++-mode-hook
      (setq flycheck-clang-no-rtti t)
    ;; Clang doesn't throw errors for RTTI operators :|, so we basically just
    ;; test that the option flag doesn't cause any issues
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-clang-error-rtti.cpp" 'c++-mode)))

(ert-deftest flycheck/c/c++-cppcheck-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-without-checkers c/c++-clang
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-cppcheck-error.c" 'c-mode
     '(4 nil error "Null pointer dereference" :checker c/c++-cppcheck))))

(ert-deftest flycheck/c/c++-cppcheck-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-without-checkers c/c++-clang
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-cppcheck-warning.c" 'c-mode
     '(2 nil warning "The expression \"x\" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0."
         :checker c/c++-cppcheck))))

(ert-deftest flycheck/c/c++-cppcheck-style ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-without-checkers c/c++-clang
    (flycheck-testsuite-should-syntax-check
     "checkers/c_c++-cppcheck-style.c" 'c-mode
     '(3 nil warning "Unused variable: unused" :checker c/c++-cppcheck))))

(ert-deftest flycheck/c/c++-cppcheck-style-suppressed ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-with-hook c-mode-hook
      (setq flycheck-cppcheck-checks nil)
    (flycheck-testsuite-without-checkers c/c++-clang
      (flycheck-testsuite-should-syntax-check "checkers/c_c++-cppcheck-style.c"
                                              'c-mode))))

(ert-deftest flycheck/c/c++-cppcheck-multiple-checks ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'c/c++-cppcheck)
  (flycheck-testsuite-with-hook c++-mode-hook
      (setq flycheck-cppcheck-checks '("performance" "portability"))
    (flycheck-testsuite-without-checkers c/c++-clang
      (flycheck-testsuite-should-syntax-check
       "checkers/c_c++-cppcheck-multiple-checks.cpp" 'c++-mode
       '(2 nil warning "Extra qualification 'A::' unnecessary and considered an error by many compilers."
           :checker c/c++-cppcheck)
       '(9 nil warning "Prefix ++/-- operators should be preferred for non-primitive types. Pre-increment/decrement can be more efficient than post-increment/decrement. Post-increment/decrement usually involves keeping a copy of the previous value around and adds a little extra code."
           :checker c/c++-cppcheck)))))

(ert-deftest flycheck/cfengine-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'cfengine)
  (flycheck-testsuite-should-syntax-check
   "checkers/cfengine-error.cf" 'cfengine-mode
   '(8 20 error "Unknown promise type 'nosuchpromisetype'"
       :checker cfengine)))

(ert-deftest flycheck/cfengine-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'cfengine)
  (flycheck-testsuite-should-syntax-check
   "checkers/cfengine-warning.cf" 'cfengine-mode
   '(3 34 warning "Removed constraint 'host_licenses_paid' in promise type 'common' [-Wremoved]"
       :checker cfengine)))

(ert-deftest flycheck/chef-foodcritic ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'chef-foodcritic)
  (flycheck-testsuite-should-syntax-check
    "checkers/chef-foodcritic/recipes/chef-foodcritic-error.rb" 'ruby-mode
   '(3 nil error "FC002: Avoid string interpolation where not required"
       :checker chef-foodcritic)
   '(8 nil error "FC003: Check whether you are running with chef server before using server-specific features"
       :checker chef-foodcritic)
   '(11 nil error "FC004: Use a service resource to start and stop services"
        :checker chef-foodcritic)))

(ert-deftest flycheck/coffee-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee)
  (flycheck-testsuite-should-syntax-check
   "checkers/coffee-syntax-error.coffee" 'coffee-mode
   '(4 7 error "missing \", starting" :checker coffee)))

(ert-deftest flycheck/coffee-coffeelint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee-coffeelint)
  (flycheck-testsuite-should-syntax-check
   "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
   '(4 nil error "Throwing strings is forbidden"
       :checker coffee-coffeelint)))

(ert-deftest flycheck/coffee-coffeelint-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'coffee-coffeelint)
  (flycheck-testsuite-with-hook coffee-mode-hook
      (setq flycheck-coffeelintrc "coffeelint.json")
    (flycheck-testsuite-should-syntax-check
     "checkers/coffee-coffeelint-error.coffee" 'coffee-mode
     '(4 nil warning "Throwing strings is forbidden"
         :checker coffee-coffeelint))))

(ert-deftest flycheck/css-csslint ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'css-csslint)
  (flycheck-testsuite-should-syntax-check
   "checkers/css-csslint-warning.css" 'css-mode
   '(3 6 warning "Heading (h1) should not be qualified."
       :checker css-csslint)))

(ert-deftest flycheck/css-csslint-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'css-csslint)
  (flycheck-testsuite-should-syntax-check
   "checkers/css-syntax-error.css" 'css-mode
   '(4 16 error "Expected LBRACE at line 4, col 16." :checker css-csslint)
   '(4 16 error "Unexpected token '100%' at line 4, col 16."
       :checker css-csslint)
   '(4 20 error "Unexpected token ';' at line 4, col 20." :checker css-csslint)
   '(5 1 error "Unexpected token '}' at line 5, col 1." :checker css-csslint)))

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

(ert-deftest flycheck/d-dmd-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'd-dmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/d-dmd-syntax-error.d" 'd-mode
   '(2 nil error "module studio is in file 'std/studio.d' which cannot be read"
       :checker d-dmd)))

(ert-deftest flycheck/d-dmd-syntax-error-without-module ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'd-dmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/d_dmd_syntax_error_without_module.d" 'd-mode
   '(5 nil error "undefined identifier writel, did you mean template write(T...)(T args) if (!is(T[0] : File))?"
       :checker d-dmd)))

(ert-deftest flycheck/d-dmd-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'd-dmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/d-dmd-warning.d" 'd-mode
   '(6 nil warning "statement is not reachable" :checker d-dmd)
   '(17 nil warning "function d_dmd_warning.bar is deprecated"
        :checker d-dmd)))

(ert-deftest flycheck/elixir-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-error.ex" 'elixir-mode
   '(5 nil error "function puts/1 undefined" :checker elixir)))

(ert-deftest flycheck/elixir-warnings ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'elixir)
  (flycheck-testsuite-should-syntax-check
   "checkers/elixir-warnings.ex" 'elixir-mode
   '(7 nil warning "this clause cannot match because a previous clause at line 4 always matches"
        :checker elixir)))

(ert-deftest flycheck/emacs-lisp ()
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
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     `(15 1 error ,msg :checker emacs-lisp))))

(ert-deftest flycheck/emacs-lisp-load-path ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-load-path
            (list (flycheck-testsuite-resource-filename
                   "dummy-elpa/dummy-package-0.1")))
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but
    requires 1+" :checker emacs-lisp)
     '(23 1 warning "the function `dummy-package-foo' might
    not be defined at runtime." :checker emacs-lisp))))

(ert-deftest flycheck/emacs-lisp-initialize-packages ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-package-user-dir
            (flycheck-testsuite-resource-filename "dummy-elpa")
            flycheck-emacs-lisp-initialize-packages t)
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(18 6 warning "message called with 0 arguments, but
    requires 1+" :checker emacs-lisp))))

(ert-deftest flycheck/emacs-lisp-checks-compressed-file ()
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp.el.gz" 'emacs-lisp-mode
   '(12 nil warning "First sentence should end with punctuation"
        :checker emacs-lisp-checkdoc)
   '(16 6 warning "message called with 0 arguments, but
    requires 1+" :checker emacs-lisp)
   '(21 1 warning "the function `dummy-package-foo' is
    not known to be defined." :checker emacs-lisp)))

(ert-deftest flycheck/emacs-lisp-sytnax-error ()
  (flycheck-testsuite-without-checkers emacs-lisp-checkdoc
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp-syntax-error.el" 'emacs-lisp-mode
     '(3 1 error "End of file during parsing" :checker emacs-lisp))))

(ert-deftest flycheck/emacs-lisp-without-file-name ()
  "Test checkdoc checker in buffers without file names.

Regression test for https://github.com/flycheck/flycheck/issues/73 and
https://github.com/bbatsov/prelude/issues/259."
  (with-temp-buffer
    (insert-file-contents
     (flycheck-testsuite-resource-filename "checkers/emacs-lisp.el"))
    (emacs-lisp-mode)
    (should-not (buffer-file-name))
    (flycheck-testsuite-buffer-sync)
    ;; TODO: Consider whether checkdoc is really useful in buffers without file
    ;; names…
    (flycheck-testsuite-should-errors)))

(ert-deftest flycheck/emacs-lisp-does-not-check-autoloads-buffers ()
  "Test that Emacs Lisp does not check autoloads buffers.

These buffers are temporary buffers generated during package
installation, which may not be byte compiled, and hence the
checker will refuse to check these.

See URL `https://github.com/flycheck/flycheck/issues/45' and URL
`https://github.com/bbatsov/prelude/issues/253'."
  (let ((autoloads-file (locate-library "dash-autoloads")))
    (should (f-file? autoloads-file))
    (with-temp-buffer
      (insert-file-contents autoloads-file 'visit)
      (should-not (flycheck-may-use-checker 'emacs-lisp))
      (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc)))))

(ert-deftest flycheck/emacs-lisp-checkdoc-does-not-check-cask-files ()
  (let ((cask-file (f-join flycheck-testsuite-source-dir "Cask")))
    (with-temp-buffer
      (insert-file-contents cask-file 'visit)
      (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc)))))

(ert-deftest flycheck/emacs-lisp-does-not-check-with-no-byte-compile ()
  (flycheck-testsuite-with-hook emacs-lisp-mode-hook
      (set (make-local-variable 'no-byte-compile) t)
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc))))

(ert-deftest flycheck/erlang-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-error.erl" 'erlang-mode
   '(7 nil error "head mismatch" :checker erlang)))

(ert-deftest flycheck/erlang-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'erlang)
  (flycheck-testsuite-should-syntax-check
   "checkers/erlang-warning.erl" 'erlang-mode
   '(6 nil warning "wrong number of arguments in format call" :checker erlang)))

(ert-deftest flycheck/go-gofmt ()
  "Test a syntax error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-gofmt)
  (flycheck-testsuite-should-syntax-check
   "checkers/go/src/syntax/syntax-error.go" 'go-mode
   '(5 9 error "expected '(', found 'IDENT' ta" :checker go-gofmt)
   '(6 1 error "expected ')', found '}'" :checker go-gofmt)))

(ert-deftest flycheck/go-build ()
  "Test an import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-build)
  (flycheck-testsuite-with-env
      `(("GOPATH" . ,(flycheck-testsuite-resource-filename "checkers/go")))
    (flycheck-testsuite-should-syntax-check
     "checkers/go/src/error/build-error.go" 'go-mode
     '(6 nil error "undefined: fmt" :checker go-build)) ))

(ert-deftest flycheck/go-build-handles-packages ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-build)
  (flycheck-testsuite-with-env
      `(("GOPATH" . ,(flycheck-testsuite-resource-filename "checkers/go")))
    (flycheck-testsuite-should-syntax-check "checkers/go/src/b1/main.go"
                                            'go-mode)))

(ert-deftest flycheck/go-build-missing-package ()
  "Test successful go build with subpackages (used to verify the
GOPATH environment variable is set properly and subpackages can be
found)."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-build)
  (let ((message (if (flycheck-testsuite-travis-ci-p)
                     ;; The Go version in Travis has a different error message
                     "cannot find package \"b2\" in any of:"
                   "import \"b2\": cannot find package")))
    (flycheck-testsuite-should-syntax-check
     "checkers/go/src/b1/main.go" 'go-mode
     `(4 2 error ,message :checker go-build))))

(ert-deftest flycheck/go-test ()
  "Test an import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-test)
  (flycheck-testsuite-with-env
      `(("GOPATH" . ,(flycheck-testsuite-resource-filename "checkers/go")))
    (flycheck-testsuite-should-syntax-check
     "checkers/go/src/test/test-error_test.go" 'go-mode
     '(8 nil error "undefined: fmt" :checker go-test))))

(ert-deftest flycheck/haml ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haml)
  (flycheck-testsuite-should-syntax-check
   "checkers/haml-error.haml" 'haml-mode
   '(5 nil error "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces."
       :checker haml :filename nil)))

(ert-deftest flycheck/handlebars ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'handlebars)
  (flycheck-testsuite-should-syntax-check
   "checkers/handlebars-error.hbs" 'handlebars-mode
   '(2 nil error "Expecting 'ID', 'DATA', got 'INVALID'"
       :checker handlebars :filename nil)))

(ert-deftest flycheck/haskell-ghc-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'haskell-ghc)
  (flycheck-testsuite-without-checkers haskell-hdevtools
    (flycheck-testsuite-should-syntax-check
     "checkers/haskell-ghc-syntax-error.hs" 'haskell-mode
     '(3 1 error "parse error on input `module'" :checker haskell-ghc))))

(ert-deftest flycheck/haskell ()
  :expected-result (flycheck-testsuite-fail-unless-checker
                    'haskell-ghc 'haskell-hlint)
  (flycheck-testsuite-without-checkers haskell-hdevtools
    (flycheck-testsuite-should-syntax-check
     "checkers/haskell.hs" 'haskell-mode
     '(3 1 warning "Top-level binding with no type signature: foo :: Integer"
         :checker haskell-ghc)
     '(6 1 error "Eta reduce
Found:
  bar xs = map lines xs
Why not:
  bar = map lines" :checker haskell-hlint)
     '(9 8 warning "Redundant bracket
Found:
  (putStrLn \"Foobar\")
Why not:
  putStrLn \"Foobar\"" :checker haskell-hlint))))

(ert-deftest flycheck/html-tidy ()
  "Test an error caused by an unknown tag."
  :expected-result (flycheck-testsuite-fail-unless-checker 'html-tidy)
  (flycheck-testsuite-should-syntax-check
   "checkers/html-tidy-warning-and-error.html" '(html-mode web-mode)
   '(3 1 warning "missing <!DOCTYPE> declaration"
       :checker html-tidy :filename nil)
   '(8 5 error "<spam> is not recognized!"
       :checker html-tidy :filename nil)
   '(8 5 warning "discarding unexpected <spam>"
       :checker html-tidy :filename nil)))

(ert-deftest flycheck/javascript-jshint-syntax-error ()
  "A missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  ;; Silence JS2 and JS3 parsers
  (let ((js2-mode-show-parse-errors nil)
        (js2-mode-show-strict-warnings nil)
        (js3-mode-show-parse-errors nil))
    (flycheck-testsuite-should-syntax-check
     "checkers/javascript-syntax-error.js" '(js-mode js2-mode js3-mode)
     '(3 11 error "Unclosed string." :checker javascript-jshint)
     '(3 25 error "Unclosed string." :checker javascript-jshint)
     '(4 1 error "Unclosed string." :checker javascript-jshint)
     '(4 1 error "Missing semicolon." :checker javascript-jshint))))

(ert-deftest flycheck/javascript-jshint-error-disabled ()
  "An unused variable."
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  (flycheck-testsuite-should-syntax-check
   "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)))

(ert-deftest flycheck/javascript-jshint ()
  "An unused variable."
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-jshint)
  (flycheck-testsuite-with-hook (js-mode-hook js2-mode-hook js3-mode-hook)
      (setq flycheck-jshintrc "jshintrc")
    (flycheck-testsuite-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 12 error "'foo' is defined but never used."
         :checker javascript-jshint))))

(ert-deftest flycheck/javascript-gjslint ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'javascript-gjslint)
  (flycheck-testsuite-without-checkers javascript-jshint
    (flycheck-testsuite-should-syntax-check
     "checkers/javascript-warnings.js" '(js-mode js2-mode js3-mode)
     '(4 nil error "E:0131: Single-quoted string preferred over double-quoted string."
         :checker javascript-gjslint :filename nil)
     '(4 nil error "E:0001: Extra space before \"]\""
         :checker javascript-gjslint :filename nil))))

(ert-deftest flycheck/json-jsonlint ()
  "Test a syntax error from multiple top-level objects."
  :expected-result (flycheck-testsuite-fail-unless-checker 'json-jsonlint)
  (flycheck-testsuite-should-syntax-check
   "checkers/json-jsonlint-error.json" 'text-mode
    '(1 42 error "found: ',' - expected: 'EOF'." :checker json-jsonlint)))

(ert-deftest flycheck/less ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'less)
  (flycheck-testsuite-should-syntax-check
   "checkers/less-file-error.less" 'less-css-mode
   '(3 1 error "'no-such-file.less' wasn't found" :checker less)))

(ert-deftest flycheck/less ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'less)
  (flycheck-testsuite-should-syntax-check
   "checkers/less-syntax-error.less" 'less-css-mode
   '(2 1 error "missing closing `}`" :checker less)))

(ert-deftest flycheck/lua ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'lua)
  (flycheck-testsuite-should-syntax-check
   "checkers/lua-syntax-error.lua" 'lua-mode
   '(5 nil error "unfinished string near '\"oh no'"
       :checker lua :filename nil)))

(ert-deftest flycheck/perl ()
  "Test an unused variable with the Perl checker."
  :expected-result (flycheck-testsuite-fail-unless-checker 'perl)
  (flycheck-testsuite-should-syntax-check
   "checkers/perl-error.pl" '(perl-mode cperl-mode)
   '(4 nil error "Name \"main::x\" used only once: possible typo"
       :checker perl)))

(ert-deftest flycheck/perl-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'perl)
  "Test a syntax error with the Perl checker."
  (flycheck-testsuite-should-syntax-check
   "checkers/perl-syntax-error.pl" '(perl-mode cperl-mode)
   '(4 nil error "syntax error" :checker perl)))

(ert-deftest flycheck/php-syntax-error ()
  "Test the T_PAAMAYIM_NEKUDOTAYIM error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'php)
  (flycheck-testsuite-should-syntax-check
   "checkers/php-syntax-error.php" 'php-mode
   '(8 nil error "syntax error, unexpected ')', expecting '('" :checker php)))

(ert-deftest flycheck/php ()
  :expected-result (flycheck-testsuite-fail-unless-checkers 'php-phpcs
                                                            'php-phpmd)
  (flycheck-testsuite-should-syntax-check
   "checkers/php.php" 'php-mode
   '(19 6 error "Missing class doc comment" :checker php-phpcs)
   '(21 nil warning "Avoid unused private fields such as '$FOO'."
        :checker php-phpmd)
   '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore" :checker php-phpcs)
   '(22 nil warning "Avoid unused private methods such as 'bar'."
        :checker php-phpmd)
   '(22 nil warning "Avoid unused parameters such as '$baz'."
        :checker php-phpmd)
   '(22 13 error "Missing function doc comment" :checker php-phpcs)
   '(22 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
        :checker php-phpcs)
   '(24 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
        :checker php-phpmd)
   '(24 nil warning "Avoid unused local variables such as '$i'."
        :checker php-phpmd)
   '(24 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
        :checker php-phpcs)))

(ert-deftest flycheck/php-phpmd-rulesets ()
  :expected-result (flycheck-testsuite-fail-unless-checkers 'php-phpcs
                                                            'php-phpmd)
  (flycheck-testsuite-with-hook php-mode-hook
      (setq flycheck-phpmd-rulesets (remove "unusedcode" flycheck-phpmd-rulesets))
    (flycheck-testsuite-should-syntax-check
     "checkers/php.php" 'php-mode
     '(19 6 error "Missing class doc comment" :checker php-phpcs)
     '(21 20 error "Private member variable \"FOO\" must be prefixed with an underscore" :checker php-phpcs)
     '(22 13 error "Missing function doc comment" :checker php-phpcs)
     '(22 13 error "Private method name \"A::bar\" must be prefixed with an underscore"
          :checker php-phpcs)
     '(24 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
          :checker php-phpmd)
     '(24 12 error "TRUE, FALSE and NULL must be lowercase; expected \"false\" but found \"FALSE\""
          :checker php-phpcs))))

(ert-deftest flycheck/php-phpcs-standard ()
  :expected-result (flycheck-testsuite-fail-unless-checkers 'php-phpcs
                                                            'php-phpmd)
  (flycheck-testsuite-with-hook php-mode-hook
      (setq flycheck-phpcs-standard "Zend")
    (flycheck-testsuite-should-syntax-check
     "checkers/php.php" 'php-mode
     '(21 nil warning "Avoid unused private fields such as '$FOO'."
          :checker php-phpmd)
     '(21 20 error "Private member variable \"FOO\" must contain a leading underscore"
          :checker php-phpcs)
     '(22 nil warning "Avoid unused private methods such as 'bar'."
          :checker php-phpmd)
     '(22 nil warning "Avoid unused parameters such as '$baz'."
          :checker php-phpmd)
     '(24 nil warning "Avoid variables with short names like $i. Configured minimum length is 3."
          :checker php-phpmd)
     '(24 nil warning "Avoid unused local variables such as '$i'."
          :checker php-phpmd)
     '(28 1 error "A closing tag is not permitted at the end of a PHP file"
          :checker php-phpcs))))

(ert-deftest flycheck/puppet-parser-singleline-syntax-error ()
  "Test a real syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-singleline.pp" 'puppet-mode
   '(3 nil error "Syntax error at ','; expected '}'" :checker puppet-parser)))

(ert-deftest flycheck/puppet-parser-multiline-syntax-error ()
  "Test a real (multi line) syntax error with puppet parser."
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-parser)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-parser-multiline.pp" 'puppet-mode
   '(8 nil error "Unclosed quote after '' in 'something
}
'" :checker puppet-parser)))

(ert-deftest flycheck/puppet-lint ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'puppet-lint)
  (flycheck-testsuite-should-syntax-check
   "checkers/puppet-lint.pp" 'puppet-mode
   '(2 nil error "foo::bar not in autoload module layout" :checker puppet-lint)
   '(3 nil warning "case statement without a default case"
       :checker puppet-lint)))

(ert-deftest flycheck/python-flake8-syntax-error ()
  "Test a real syntax error with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python-syntax-error.py" 'python-mode
    '(3 13 error "E901 SyntaxError: invalid syntax" :checker python-flake8)))

(ert-deftest flycheck/python-flake8-warning-ignored ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8rc "flake8rc")
    (flycheck-testsuite-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase"
         :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck/python-flake8-maximum-complexity ()
  "Test superfluous spaces with flake8."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-complexity 4)
    (flycheck-testsuite-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(5 1 warning "F401 'antigravit' imported but unused"
         :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(12 1 warning "C901 'Spam.with_ham' is too complex (4)"
          :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck/python-flake8-error-maximum-line-length ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-with-hook python-mode-hook
      (setq flycheck-flake8-maximum-line-length 45)
    (flycheck-testsuite-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(5 1 warning "F401 'antigravit' imported but unused"
         :checker python-flake8)
     '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
     '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
     '(10 46 error "E501 line too long (46 > 45 characters)"
          :checker python-flake8)
     '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
          :checker python-flake8)
     '(22 1 warning "F821 undefined name 'antigravity'"
          :checker python-flake8))))

(ert-deftest flycheck/python-flake8 ()
  "PEP8 compliant names with Flake8 and pep8-naming."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-flake8)
  (flycheck-testsuite-should-syntax-check
   "checkers/python/test.py" 'python-mode
   '(5 1 warning "F401 'antigravit' imported but unused" :checker python-flake8)
   '(7 1 error "E302 expected 2 blank lines, found 1" :checker python-flake8)
   '(9 9 info "N802 function name should be lowercase" :checker python-flake8)
   '(12 29 error "E251 unexpected spaces around keyword / parameter equals"
        :checker python-flake8)
   '(12 31 error "E251 unexpected spaces around keyword / parameter equals"
        :checker python-flake8)
   '(22 1 warning "F821 undefined name 'antigravity'"
        :checker python-flake8)))

(ert-deftest flycheck/python-pylint-syntax-error ()
  "Test a real syntax error with pylint."
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-without-checkers python-flake8
    (flycheck-testsuite-should-syntax-check
     "checkers/python-syntax-error.py" 'python-mode
     '(3 nil error "invalid syntax (E0001)" :checker python-pylint))))

(ert-deftest flycheck/python-pylint ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'python-pylint)
  (flycheck-testsuite-without-checkers python-flake8
    (flycheck-testsuite-should-syntax-check
     "checkers/python/test.py" 'python-mode
     '(1 nil info "Missing module docstring (C0111)" :checker python-pylint)
     '(4 nil error "Unable to import 'spam' (F0401)" :checker python-pylint)
     '(5 nil error "No name 'antigravit' in module 'python' (E0611)"
         :checker python-pylint)
     '(5 nil warning "Unused import antigravit (W0611)" :checker python-pylint)
     '(7 nil info "Missing class docstring (C0111)" :checker python-pylint)
     '(9 4 info "Invalid method name \"withEggs\" (C0103)"
         :checker python-pylint)
     '(9 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(9 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(10 15 warning "Used builtin function 'map' (W0141)"
          :checker python-pylint)
     '(12 4 info "Missing method docstring (C0111)" :checker python-pylint)
     '(12 4 warning "Method could be a function (R0201)" :checker python-pylint)
     '(14 15 error "Module 'sys' has no 'python_version' member (E1101)"
          :checker python-pylint)
     '(22 nil error "Undefined variable 'antigravity' (E0602)"
          :checker python-pylint))))

(ert-deftest flycheck/rst ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rst)
  (flycheck-testsuite-should-syntax-check
   "checkers/rst.rst" 'rst-mode
   '(8 nil warning "Title underline too short." :checker rst)
   '(14 nil error "Unexpected section title." :checker rst)
   '(16 nil error "Unknown target name: \"restructuredtext\"." :checker rst)
   '(19 nil warning "Title underline too short." :checker rst)
   '(21 nil error "Unknown target name: \"cool\"." :checker rst)
   '(26 nil error "Unexpected section title." :checker rst)))

(ert-deftest flycheck/ruby-rubocop-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-should-syntax-check
   "checkers/ruby-syntax-error.rb" 'ruby-mode
   '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubocop)
   '(5 24 error "unterminated string meets end of file" :checker ruby-rubocop)))

(ert-deftest flycheck/ruby-rubylint-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop)
  (flycheck-testsuite-without-checkers ruby-rubocop
    (flycheck-testsuite-should-syntax-check
     "checkers/ruby-syntax-error.rb" 'ruby-mode
     '(5 7 error "unexpected token tCONSTANT" :checker ruby-rubylint))))

(ert-deftest flycheck/ruby ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop
                                                           'ruby-rubylint)
  (flycheck-testsuite-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(1 1 info "Missing utf-8 encoding comment." :checker ruby-rubocop)
     '(4 18 warning "unused argument name" :checker ruby-rubylint)
     '(5 5 warning "unused local variable arr" :checker ruby-rubylint)
     '(5 5 warning "Useless assignment to variable - arr" :checker ruby-rubocop)
     '(5 18 info "Use snake_case for symbols." :checker ruby-rubocop)
     '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
         :checker ruby-rubocop)
     '(10 5 info "the use of then/do is not needed here" :checker ruby-rubylint)
     '(10 5 info "Favor modifier if/unless usage when you have a single-line body. Another good alternative is the usage of control flow &&/||."
          :checker ruby-rubocop)
     '(10 5 info "Never use then for multi-line if/unless."
          :checker ruby-rubocop)
     '(10 8 warning "Literal true appeared in a condition."
          :checker ruby-rubocop)
     '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
     '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
          :checker ruby-rubylint)))

(ert-deftest flycheck/ruby-rubocop-disabled-warning ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'ruby-rubocop
                                                           'ruby-rubylint)
  (flycheck-testsuite-with-hook ruby-mode-hook
      (setq flycheck-rubocoprc "rubocop.yml")
    (flycheck-testsuite-should-syntax-check
     "checkers/ruby-warnings.rb" 'ruby-mode
     '(1 1 info "Missing utf-8 encoding comment." :checker ruby-rubocop)
     '(4 18 warning "unused argument name" :checker ruby-rubylint)
     '(5 5 warning "unused local variable arr" :checker ruby-rubylint)
     '(5 5 warning "Useless assignment to variable - arr" :checker ruby-rubocop)
     '(6 10 info "Prefer single-quoted strings when you don't need string interpolation or special symbols."
         :checker ruby-rubocop)
     '(10 5 info "the use of then/do is not needed here" :checker ruby-rubylint)
     '(10 5 info "Favor modifier if/unless usage when you have a single-line body. Another good alternative is the usage of control flow &&/||."
          :checker ruby-rubocop)
     '(10 5 info "Never use then for multi-line if/unless."
          :checker ruby-rubocop)
     '(10 8 warning "Literal true appeared in a condition."
          :checker ruby-rubocop)
     '(11 24 error "undefined instance variable @name" :checker ruby-rubylint)
     '(16 1 error "wrong number of arguments (expected 2..3 but got 0)"
          :checker ruby-rubylint))))

(ert-deftest flycheck/rust ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'rust)
  (flycheck-testsuite-should-syntax-check
   "checkers/rust-syntax-error.rs" 'rust-mode
   '(3 10 error "expected `{` but found `bla`" :checker rust)))

(ert-deftest flycheck/sass ()
  "Test a syntax error caused by inconsistent indentation."
  :expected-result (flycheck-testsuite-fail-unless-checker 'sass)
  (flycheck-testsuite-should-syntax-check
   "checkers/sass-error.sass" 'sass-mode
    '(5 nil error "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces."
        :checker sass)))

(ert-deftest flycheck/scala ()
  :expected-result (if (flycheck-testsuite-travis-ci-p) :failed
                     (flycheck-testsuite-fail-unless-checker 'scala))
  (flycheck-testsuite-not-on-travis)
  (flycheck-testsuite-should-syntax-check
   "checkers/scala-syntax-error.scala" 'scala-mode
   '(3 nil error "identifier expected but '{' found." :checker scala)))

(ert-deftest flycheck/scss ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'scss)
  (flycheck-testsuite-should-syntax-check
   "checkers/scss-error.scss" 'scss-mode
   '(3 nil error "Invalid CSS after \"        c olor:\": expected pseudoclass or pseudoelement, was \" red;\""
       :checker scss)))

(ert-deftest flycheck/sh-bash ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'sh-bash)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-testsuite-without-checkers sh-dash
      (flycheck-testsuite-should-syntax-check
       "checkers/sh-syntax-error.sh" 'sh-mode
       '(3 nil error "syntax error near unexpected token `('" :checker sh-bash)
       '(3 nil error "`cat <(echo blah)'" :checker sh-bash)))))

(ert-deftest flycheck/sh-dash ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'sh-dash)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "sh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/sh-syntax-error.sh" 'sh-mode
     '(3 nil error "Syntax error: \"(\" unexpected" :checker sh-dash))))

(ert-deftest flycheck/slim ()
  (flycheck-testsuite-fail-unless-checker 'slim)
  (let* ((slim-version (cadr (split-string (car (process-lines "slimrb" "-v")))))
         ;; Old Slim compilers do not report column information
         (column (if (version<= "1.3.1" slim-version) 1 nil)))
    (flycheck-testsuite-should-syntax-check
     "checkers/slim-error.slim" 'slim-mode
     `(2 ,column error "Unexpected indentation" :checker slim))))

(ert-deftest flycheck/tex-chktex ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-chktex)
  (flycheck-testsuite-should-syntax-check
   "checkers/tex-warning.tex" 'latex-mode
   '(5 28 warning "13:Intersentence spacing (`\\@') should perhaps be used."
       :checker tex-chktex)))

(ert-deftest flycheck/tex-lacheck ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'tex-lacheck)
  (flycheck-testsuite-without-checkers tex-chktex
    (flycheck-testsuite-should-syntax-check
     "checkers/tex-warning.tex" 'latex-mode
     '(5 nil warning "missing `\\@' before `.' in \"GNU.\""
         :checker tex-lacheck)
     '(7 nil warning "possible unwanted space at \"{\""
         :checker tex-lacheck))))

(ert-deftest flycheck/xml-xmlstarlet ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-testsuite-should-syntax-check
   "checkers/xml-syntax-error.xml" 'nxml-mode
   '(4 10 error "Opening and ending tag mismatch: spam line 3 and with"
       :checker xml-xmlstarlet)))

(ert-deftest flycheck/xml-xmllint-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'xml-xmllint)
  (flycheck-testsuite-without-checkers xml-xmlstarlet
    (flycheck-testsuite-should-syntax-check
     "checkers/xml-syntax-error.xml" 'nxml-mode
     '(4 nil error "parser error : Opening and ending tag mismatch: spam line 3 and with"
         :checker xml-xmllint)
     '(5 nil error "parser error : Extra content at the end of the document"
         :checker xml-xmllint))))

(ert-deftest flycheck/yaml-jsyaml ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'yaml-jsyaml)
  (flycheck-testsuite-should-syntax-check
   "checkers/yaml-syntax-error.yaml" 'yaml-mode
   '(4 5 error "bad indentation of a mapping entry"
       :checker yaml-jsyaml :filename nil)))

(ert-deftest flycheck/yaml-ruby ()
  (let* ((ruby-version (car (process-lines "ruby" "-e" "puts RUBY_VERSION")))
         (psych-version (when (version<= "1.9.3" ruby-version)
                          (car (process-lines "ruby" "-rpsych"
                                              "-e" "puts Psych::VERSION"))))
         (expected-error
          (cond
           ;; Syck parser in Ruby 1.9.2 and lower
           ((version< ruby-version "1.9.3")
            '(3 5 error "a1: bar" :checker yaml-ruby))
           ;; Psych parser in Ruby 1.9.3 and up.  The Psych errors apparently
           ;; vary between different versions, so we have to adapt.  Ruby, you
           ;; suck.
           ((version< psych-version "1.2.2")
            '(3 4 error "couldn't parse YAML" :checker yaml-ruby))
           (:else
            '(4 5 error "mapping values are not allowed in this context"
                :checker yaml-ruby)))))
    (flycheck-testsuite-without-checkers yaml-jsyaml
      (flycheck-testsuite-should-syntax-check
       "checkers/yaml-syntax-error.yaml" 'yaml-mode expected-error))))

(ert-deftest flycheck/zsh ()
  "Test a syntax error from a missing semicolon."
  :expected-result (flycheck-testsuite-fail-unless-checker 'zsh)
  (flycheck-testsuite-with-hook sh-mode-hook
      (sh-set-shell "zsh" :no-query)
    (flycheck-testsuite-should-syntax-check
     "checkers/zsh-syntax-error.zsh" 'sh-mode
     '(5 nil error "parse error near `fi'" :checker zsh))))

;;; builtin-checkers-test.el ends here
