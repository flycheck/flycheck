;;; test-haskell.el --- Flycheck Specs: Haskell      -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Specs for Haskell support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Haskell"

  (describe "Module names"
    (it "does not extract a module name from commented code"
      (expect flycheck-haskell-module-re
              :not :to-match "-- | module Foo.Bar where"))

    (it "extracts a simple module name without exports"
      (expect flycheck-haskell-module-re
              :to-match-with-group "module Foo.Bar where" 1 "Foo.Bar"))

    (it "extracts a simple module name at the end of a line"
      (expect flycheck-haskell-module-re
              :to-match-with-group "module Hello.World\nwhere" 1 "Hello.World"))

    (it "extracts a module name with exports"
      (expect flycheck-haskell-module-re
              :to-match-with-group "module Spam.With.Eggs (eggs) where"
              1 "Spam.With.Eggs"))

    (it "extracts a module name with exports right after the name"
      (expect flycheck-haskell-module-re
              :to-match-with-group "module Hello.World(hello) where"
              1 "Hello.World")))

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test haskell-stack-ghc haskell syntax-error
      (assume (file-exists-p (getenv "HOME")))
      (let ((flycheck-disabled-checkers '(haskell-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/SyntaxError.hs" 'haskell-mode
         '(3 1 error "parse error on input `module'" :checker haskell-stack-ghc))))

    (flycheck-buttercup-def-checker-test haskell-stack-ghc haskell type-error
      (assume (file-exists-p (getenv "HOME")))
      (let ((flycheck-disabled-checkers '(haskell-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/Error.hs" 'haskell-mode
         '(4 16 error "* Couldn't match type `Bool' with `[Char]'
  Expected type: String
    Actual type: Bool
* In the first argument of `putStrLn', namely `True'
  In the expression: putStrLn True
  In an equation for `foo': foo = putStrLn True" :checker haskell-stack-ghc))))

    (flycheck-buttercup-def-checker-test (haskell-stack-ghc haskell-hlint) haskell literate
      (assume (file-exists-p (getenv "HOME")))
      (let ((flycheck-disabled-checkers '(haskell-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/Literate.lhs" 'haskell-literate-mode
         '(6 1 warning "Top-level binding with no type signature: foo :: a"
             :id "-Wmissing-signatures"
             :checker haskell-stack-ghc))))

    (flycheck-buttercup-def-checker-test (haskell-stack-ghc haskell-hlint) haskell
                                         complete-chain
      (assume (file-exists-p (getenv "HOME")))
      (let ((flycheck-disabled-checkers '(haskell-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/Warnings.hs" 'haskell-mode
         '(4 1 warning "Eta reduce
Found:
  spam eggs = map lines eggs
Perhaps:
  spam = map lines" :checker haskell-hlint)
         '(4 1 warning "Top-level binding with no type signature:
  spam :: [String] -> [[String]]"
             :id "-Wmissing-signatures"
             :checker haskell-stack-ghc)
         '(7 8 info "Redundant bracket
Found:
  (putStrLn \"hello world\")
Perhaps:
  putStrLn \"hello world\"" :checker haskell-hlint))))

    (flycheck-buttercup-def-checker-test
        haskell-stack-ghc haskell nonstandard-stack-yaml-file
      (assume (file-exists-p (getenv "HOME")))

      (let* ((flycheck-disabled-checkers '(haskell-ghc))
             (proj-dir "language/haskell/stack-project-with-renamed-stack-yaml")
             (flycheck-ghc-stack-project-file
              (expand-file-name "stack-nonstandard.yaml"
                                (flycheck-buttercup-resource-filename proj-dir))))

        (flycheck-buttercup-should-syntax-check
         (concat proj-dir "/src/Foo.hs")
         'haskell-mode)))

    (flycheck-buttercup-def-checker-test haskell-ghc haskell syntax-error
      (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/SyntaxError.hs" 'haskell-mode
         '(3 1 error "parse error on input `module'" :checker haskell-ghc))))

    (flycheck-buttercup-def-checker-test haskell-ghc haskell type-error
      (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/Error.hs" 'haskell-mode
         '(4 16 error "* Couldn't match type `Bool' with `[Char]'
  Expected type: String
    Actual type: Bool
* In the first argument of `putStrLn', namely `True'
  In the expression: putStrLn True
  In an equation for `foo': foo = putStrLn True" :checker haskell-ghc))))

    (flycheck-buttercup-def-checker-test (haskell-ghc haskell-hlint) haskell literate
      (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/Literate.lhs" 'haskell-literate-mode
         '(6 1 warning "Top-level binding with no type signature: foo :: a"
             :id "-Wmissing-signatures"
             :checker haskell-ghc))))

    (flycheck-buttercup-def-checker-test (haskell-ghc haskell-hlint) haskell
                                         complete-chain
      (let ((flycheck-disabled-checkers '(haskell-stack-ghc)))
        (flycheck-buttercup-should-syntax-check
         "language/haskell/Warnings.hs" 'haskell-mode
         '(4 1 warning "Eta reduce
Found:
  spam eggs = map lines eggs
Perhaps:
  spam = map lines" :checker haskell-hlint)
         '(4 1 warning "Top-level binding with no type signature:
  spam :: [String] -> [[String]]"
             :id "-Wmissing-signatures"
             :checker haskell-ghc)
         '(7 8 info "Redundant bracket
Found:
  (putStrLn \"hello world\")
Perhaps:
  putStrLn \"hello world\"" :checker haskell-hlint))))))

;;; test-haskell.el ends here
