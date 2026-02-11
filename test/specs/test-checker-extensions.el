;;; test-checker-extensions.el --- Flycheck Specs: Checker Extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2025 Sebastian Wiesner and Flycheck contributors

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

;; Specs for checker extensions (next-checker, add-mode, etc.)

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Checker Extensions"

  (describe "flycheck-checker-get"
    (it "gets-a-property"
      (expect (flycheck-checker-get 'emacs-lisp 'modes)
              :to-equal '(emacs-lisp-mode lisp-interaction-mode)))

    (it "setf"
      (let ((checker 'bar)
            (property 'foo))
        (expect (flycheck-checker-get checker property) :not :to-be-truthy)
        (setf (flycheck-checker-get checker property) "Hello world")
        (unwind-protect
            (expect (flycheck-checker-get checker property)
                    :to-equal "Hello world")
          (put checker 'flycheck-foo nil)))))

  (describe "flycheck-validate-next-checker"
    (it "any-symbol"
      (expect (flycheck-validate-next-checker 'foo) :to-be-truthy)
      (expect (flycheck-validate-next-checker 'foo t) :to-throw))

    (it "syntax-checker-symbol"
      (expect (flycheck-validate-next-checker 'emacs-lisp t) :to-be-truthy))

    (it "string"
      (expect (flycheck-validate-next-checker "foo") :to-throw))

    (it "invalid-form"
      (expect (flycheck-validate-next-checker '(warnings-only emacs-lisp)) :to-throw))

    (it "invalid-level"
      (expect (flycheck-validate-next-checker '("foo" . emacs-lisp)) :to-throw)
      (expect (flycheck-validate-next-checker '(foo . emacs-lisp) 'strict) :to-throw))

    (it "valid-predicate-with-any-symbol"
      (expect (flycheck-validate-next-checker '(warning . bar)) :to-be-truthy)
      (expect (flycheck-validate-next-checker '(warning . bar) 'strict) :to-throw))

    (it "valid-predicate-with-syntax-checker-symbol"
      (expect (flycheck-validate-next-checker '(warning . emacs-lisp) 'strict)
              :to-be-truthy)))

  (describe "flycheck-remove-next-checker"
    (it "no-valid-checker"
      (let ((err-data (should-error (flycheck-remove-next-checker 'foo 'emacs-lisp))))
        (expect (cadr err-data) :to-equal "foo is not a valid syntax checker")))

    (it "plain"
      (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
        (flycheck-remove-next-checker 'emacs-lisp 'emacs-lisp-checkdoc)
        (unwind-protect
            (expect (flycheck-checker-get 'emacs-lisp 'next-checkers)
                    :to-equal nil)
          (put 'emacs-lisp 'flycheck-next-checkers next-checkers))))

    (it "level"
      (let ((next-checkers (flycheck-checker-get 'sh-zsh 'next-checkers)))
        (flycheck-remove-next-checker 'sh-zsh 'sh-shellcheck)
        (unwind-protect
            (expect (flycheck-checker-get 'sh-zsh 'next-checkers)
                    :to-equal nil)
          (put 'sh-zsh 'flycheck-next-checkers next-checkers)))))

  (describe "flycheck-add-next-checker"
    (it "no-valid-checker"
      (let ((err-data (should-error (flycheck-add-next-checker 'foo 'emacs-lisp))))
        (expect (cadr err-data) :to-equal "foo is not a valid syntax checker")))

    (it "no-valid-next-checker"
      (expect (flycheck-add-next-checker 'emacs-lisp '(warnings-only bar)) :to-throw)
      (expect (flycheck-add-next-checker 'emacs-lisp "foo") :to-throw)
      (expect (flycheck-add-next-checker 'emacs-lisp 'bar) :to-throw)
      (expect (flycheck-add-next-checker 'emacs-lisp '(warnings-only . bar)) :to-throw)
      (expect (flycheck-add-next-checker 'emacs-lisp '(foo . emacs-lisp)) :to-throw))

    (it "redundant"
      (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
        (flycheck-add-next-checker 'emacs-lisp 'texinfo)
        (flycheck-add-next-checker 'emacs-lisp 'texinfo)
        (flycheck-add-next-checker 'emacs-lisp '(t . texinfo))
        (flycheck-add-next-checker 'emacs-lisp '(warning . texinfo))
        (unwind-protect
            (expect (flycheck-checker-get 'emacs-lisp 'next-checkers)
                    :to-equal (cons '(warning . texinfo) next-checkers))
          (put 'emacs-lisp 'flycheck-next-checkers next-checkers))))

    (it "prepend"
      (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
        (flycheck-add-next-checker 'emacs-lisp 'texinfo)
        (unwind-protect
            (expect (flycheck-checker-get 'emacs-lisp 'next-checkers)
                    :to-equal (cons 'texinfo next-checkers))
          (put 'emacs-lisp 'flycheck-next-checkers next-checkers))))

    (it "append"
      (let ((next-checkers (flycheck-checker-get 'emacs-lisp 'next-checkers)))
        (flycheck-add-next-checker 'emacs-lisp 'texinfo 'append)
        (unwind-protect
            (expect (flycheck-checker-get 'emacs-lisp 'next-checkers)
                    :to-equal (append next-checkers '(texinfo)))
          (put 'emacs-lisp 'flycheck-next-checkers next-checkers)))))

  (describe "flycheck-add-mode"
    (it "no-valid-checker"
      (let ((err-data (should-error (flycheck-add-mode 'foo 'emacs-lisp-mode))))
        (expect (cadr err-data) :to-equal "foo is not a valid syntax checker")))

    (it "no-valid-mode"
      (let ((err-data (should-error (flycheck-add-mode 'python-pylint "foo"))))
        (expect (cadr err-data) :to-equal "foo is not a symbol")))

    (it "adds-mode"
      (let ((modes (flycheck-checker-get 'python-pylint 'modes)))
        (flycheck-add-mode 'python-pylint 'emacs-lisp-mode)
        (unwind-protect
            (expect (flycheck-checker-get 'python-pylint 'modes)
                    :to-equal (cons 'emacs-lisp-mode modes))
          (put 'python-pylint 'flycheck-modes modes)
          (expect (flycheck-checker-get 'python-pylint 'modes)
                  :to-equal modes))))))

;;; test-checker-extensions.el ends here
