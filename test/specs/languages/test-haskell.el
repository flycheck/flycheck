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
              1 "Hello.World"))))

;;; test-haskell.el ends here
