;;; test-scala.el --- Flycheck Specs: Scala      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
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

;; Specs for Scala support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(defun test-scala/scalac-major-version ()
  "The major version of the scalac the `scala' checker would run, or nil."
  (when-let* ((scalac (flycheck-find-checker-executable 'scala)))
    (with-temp-buffer
      ;; scalac prints its version to stderr
      (call-process scalac nil (list t t) nil "-version")
      (goto-char (point-min))
      (when (re-search-forward "version \\([0-9]+\\)\\." nil 'noerror)
        (string-to-number (match-string 1))))))

(describe "Language Scala"
  (flycheck-buttercup-def-checker-test scala scala nil
    (assume (eql (test-scala/scalac-major-version) 2)
            "The installed scalac is not Scala 2")
    (flycheck-buttercup-should-syntax-check
     "language/scala/syntax-error.scala" 'scala-mode
     '(3 nil error "identifier expected but '{' found." :checker scala)))

  (flycheck-buttercup-def-checker-test scala scala scala3
    (assume (eql (test-scala/scalac-major-version) 3)
            "The installed scalac is not Scala 3")
    (flycheck-buttercup-should-syntax-check
     "language/scala/syntax-error.scala" 'scala-mode
     '(3 8 error "an identifier expected, but '{' found"
         :id "E040" :checker scala)))

  (flycheck-buttercup-def-checker-test scala-scalastyle scala error
    (let ((flycheck-scalastyle-config "scalastyle.xml"))
      (flycheck-buttercup-should-syntax-check
       "language/scala/style-error.scala" 'scala-mode
       '(6 5 error "Don't use println" :checker scala-scalastyle))))

  (flycheck-buttercup-def-checker-test scala-scalastyle scala warning
    (let ((flycheck-scalastyle-config "scalastyle.xml"))
      (flycheck-buttercup-should-syntax-check
       "language/scala/style-warning.scala" 'scala-mode
       '(5 9 warning "Redundant braces after class definition"
           :checker scala-scalastyle))))

  (describe "the scala-scalastyle checker command"
    (it "appends flycheck-scalastyle-args"
      (let ((flycheck-scalastyle-config nil)
            (flycheck-scalastyle-args '("--quiet" "true")))
        (expect (flycheck-checker-substituted-arguments 'scala-scalastyle)
                :to-contain "--quiet"))))

  (describe "reading scalac's output"
    ;; The E008 and E019 samples are verbatim from the reports in
    ;; https://github.com/flycheck/flycheck/pull/2106

    (flycheck-buttercup-def-parse-test scala "language/scala/syntax-error.scala"
      '(3 8 error "an identifier expected, but '{' found" :id "E040"))

    (it "reads a Scala 3 box"
      (expect
       (flycheck-buttercup-parse
        'scala
        "-- [E008] Not Found Error: /home/cpc/scala3-ansi-colors/src/main/scala/Main.scala:5:43 --------------------------
5 |def msg =  \"I was compiled by Scala 3. :)\" / 2
  |           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  |           value / is not a member of String
one error found
")
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              5 44 'error "value / is not a member of String"
              :id "E008" :checker 'scala :buffer (current-buffer)
              :filename "/home/cpc/scala3-ansi-colors/src/main/scala/Main.scala"))))

    (it "reads a header too long for any padding dashes"
      ;; The dashes pad the header to the terminal width, so the long
      ;; temporary path of a real check leaves none, and every live
      ;; check parsed nothing while the short sample paths passed.
      (expect
       (flycheck-buttercup-parse
        'scala
        "-- [E019] Syntax Error: /var/folders/1l/xqbgb73s21vd3wrrp_tyxmy00000gn/T/flycheckB2ap0M/averyveryverylongdirectoryname/anotherlongsubdirectoryname/scala-test.scala:3:17
3 |  implicit def mi
  |                 ^
  |                 Missing return type
1 error found
")
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              3 18 'error "Missing return type"
              :id "E019" :checker 'scala :buffer (current-buffer)
              :filename "/var/folders/1l/xqbgb73s21vd3wrrp_tyxmy00000gn/T/flycheckB2ap0M/averyveryverylongdirectoryname/anotherlongsubdirectoryname/scala-test.scala"))))

    (it "skips the caret line and the hint about -explain"
      (expect
       (flycheck-buttercup-parse
        'scala
        "-- [E019] Syntax Error: /tmp/flycheckVLFiiu/foo.scala:2:15 ---------------------
2 |implicit def mi
  |               ^
  |               Missing return type
  |
  | longer explanation available when compiling with `-explain`
1 error found
")
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              2 16 'error "Missing return type"
              :id "E019" :checker 'scala :buffer (current-buffer)
              :filename "/tmp/flycheckVLFiiu/foo.scala"))))

    (it "reads through the colors Scala 3 always prints"
      (expect
       (flycheck-buttercup-parse
        'scala
        (concat
         "\e[31m\e[31m-- [E019] Syntax Error: /tmp/flycheckVLFiiu/foo.scala"
         ":2:15 ---------------------\e[0m\e[0m\n"
         "\e[31m2 |\e[0m\e[33mimplicit\e[0m \e[33mdef\e[0m \e[36mmi\e[0m\n"
         "\e[31m\e[31m  |\e[0m               ^\e[0m\n"
         "\e[31m  |\e[0m               Missing return type\n"
         "\e[31m  |\e[0m\n"
         "\e[31m  |\e[0m longer explanation available when compiling"
         " with ‘-explain’\n"
         "1 error found\n"))
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              2 16 'error "Missing return type"
              :id "E019" :checker 'scala :buffer (current-buffer)
              :filename "/tmp/flycheckVLFiiu/foo.scala"))))

    (it "reads a header without an id"
      (expect
       (flycheck-buttercup-parse
        'scala
        "-- Warning: /tmp/flycheckVLFiiu/foo.scala:3:8 ----------------------------------
3 |@nowarn(\"id\")
  |        ^^^^
  |        Invalid message filter
1 warning found
")
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              3 9 'warning "Invalid message filter"
              :checker 'scala :buffer (current-buffer)
              :filename "/tmp/flycheckVLFiiu/foo.scala"))))

    (it "reads every box, keeping a message's lines together"
      (expect
       (flycheck-buttercup-parse
        'scala
        "-- [E007] Type Mismatch Error: /tmp/flycheckVLFiiu/foo.scala:3:13 --------------
3 |val i: Int = \"42\"
  |             ^^^^
  |             Found:    (\"42\" : String)
  |             Required: Int
  |
  | longer explanation available when compiling with `-explain`
-- [E198] Unused Symbol Warning: /tmp/flycheckVLFiiu/foo.scala:1:7 -------------
1 |import scala.util.Try
  |       ^^^^^^^^^^^^^^
  |       unused import
1 warning found
1 error found
")
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              3 14 'error "Found:    (\"42\" : String)\nRequired: Int"
              :id "E007" :checker 'scala :buffer (current-buffer)
              :filename "/tmp/flycheckVLFiiu/foo.scala")
             (flycheck-error-new-at
              1 8 'warning "unused import"
              :id "E198" :checker 'scala :buffer (current-buffer)
              :filename "/tmp/flycheckVLFiiu/foo.scala"))))

    (it "skips indented source and keeps a wrapped message aligned"
      (expect
       (flycheck-buttercup-parse
        'scala
        "-- [E007] Type Mismatch Error: /tmp/flycheckVLFiiu/foo.scala:4:2 ---------------
4 |  foo(bar)
  |  ^^^^^^^^
  |  Found:    List[
  |              Int]
  |  Required: Int
1 error found
")
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              4 3 'error "Found:    List[\n            Int]\nRequired: Int"
              :id "E007" :checker 'scala :buffer (current-buffer)
              :filename "/tmp/flycheckVLFiiu/foo.scala"))))

    (it "falls back to the plain form Scala 2 prints"
      (expect
       (flycheck-buttercup-parse
        'scala
        "/tmp/flycheckVLFiiu/foo.scala:3: error: identifier expected but '{' found.
object {
       ^
")
       :to-be-equal-flycheck-errors
       (list (flycheck-error-new-at
              3 nil 'error "identifier expected but '{' found."
              :checker 'scala :buffer (current-buffer)
              :filename "/tmp/flycheckVLFiiu/foo.scala"))))))

;;; test-scala.el ends here
