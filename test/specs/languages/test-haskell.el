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

  (describe "GHC language flag"
    (it "defaults -x to hs in non-literate modes"
      (flycheck-buttercup-with-temp-buffer
        (setq major-mode 'haskell-ts-mode)
        (let ((args (flycheck-checker-substituted-arguments 'haskell-ghc)))
          (expect (cadr (member "-x" args)) :to-equal "hs"))
        (flycheck-safe-delete-temporaries)))

    (it "passes -x lhs in haskell-literate-mode"
      (flycheck-buttercup-with-temp-buffer
        (setq major-mode 'haskell-literate-mode)
        (let ((args (flycheck-checker-substituted-arguments 'haskell-ghc)))
          (expect (cadr (member "-x" args)) :to-equal "lhs"))
        (flycheck-safe-delete-temporaries))))

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
  spam = map lines" :id "Eta reduce" :end-line 4 :end-column 27
     :checker haskell-hlint)
         '(4 1 warning "Top-level binding with no type signature:
  spam :: [String] -> [[String]]"
             :id "-Wmissing-signatures"
             :checker haskell-stack-ghc)
         '(7 8 info "Redundant bracket
Found:
  (putStrLn \"hello world\")
Perhaps:
  putStrLn \"hello world\"" :id "Redundant bracket" :end-line 7 :end-column 32
     :checker haskell-hlint))))

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
  spam = map lines" :id "Eta reduce" :end-line 4 :end-column 27
     :checker haskell-hlint)
         '(4 1 warning "Top-level binding with no type signature:
  spam :: [String] -> [[String]]"
             :id "-Wmissing-signatures"
             :checker haskell-ghc)
         '(7 8 info "Redundant bracket
Found:
  (putStrLn \"hello world\")
Perhaps:
  putStrLn \"hello world\"" :id "Redundant bracket" :end-line 7 :end-column 32
     :checker haskell-hlint)))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test haskell-ghc "language/haskell/SyntaxError.hs"
      '(3 1 error "parse error on input ‘module’" :id "GHC-58481"))))

  (describe "reading hlint's output"

    (it "attaches the replacement as a machine-applicable fix"
      ;; The comparable-error matcher strips fixes, so assert directly
      (with-temp-buffer
        (let* ((errors (flycheck-buttercup-parse
                        'haskell-hlint
                        (flycheck-buttercup-fixture
                         'haskell-hlint "language/haskell/Warnings.hs")))
               (fix (flycheck-error-fix (car errors)))
               (edit (car (flycheck-fix-edits fix))))
          (expect (flycheck-fix-description fix) :to-equal "Eta reduce")
          (expect (flycheck-fix-edit-line edit) :to-equal 4)
          (expect (flycheck-fix-edit-column edit) :to-equal 1)
          (expect (flycheck-fix-edit-end-line edit) :to-equal 4)
          (expect (flycheck-fix-edit-end-column edit) :to-equal 27)
          (expect (flycheck-fix-edit-replacement edit)
                  :to-equal "spam = map lines"))))

    (flycheck-buttercup-def-parse-test haskell-hlint "language/haskell/Warnings.hs"
      ;; An idea with a replacement carries a machine-applicable fix
      `(4 1 warning "Eta reduce
Found:
  spam eggs = map lines eggs
Perhaps:
  spam = map lines"
          :id "Eta reduce" :end-line 4 :end-column 27
          :fix ,(flycheck-fix-new
                 :description "Eta reduce"
                 :edits (list (flycheck-fix-edit-new
                               :line 4 :column 1 :end-line 4 :end-column 27
                               :replacement "spam = map lines"))))
      `(7 8 info "Redundant bracket
Found:
  (putStrLn \"hello world\")
Perhaps:
  putStrLn \"hello world\""
          :id "Redundant bracket" :end-line 7 :end-column 32
          :fix ,(flycheck-fix-new
                 :description "Redundant bracket"
                 :edits (list (flycheck-fix-edit-new
                               :line 7 :column 8 :end-line 7 :end-column 32
                               :replacement "putStrLn \"hello world\""))))))

;;; test-haskell.el ends here
