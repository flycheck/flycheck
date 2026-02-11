;;; test-scheme.el --- Flycheck Specs: Scheme      -*- lexical-binding: t; -*-

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

;; Specs for Scheme support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Scheme"
  (flycheck-buttercup-def-checker-test scheme-chicken scheme error-no-line-number
    (assume (version<= "25.1" emacs-version))
    (flycheck-buttercup-should-syntax-check
     "language/chicken/error-no-line-number.scm" 'flycheck/chicken-mode
     '(0 nil error "(cddr) during expansion of (for-each ...) - bad argument type: ()\n\n\tCall history:\n\n\t<syntax>\t  (##core#begin (for-each))\n\t<syntax>\t  (for-each)\t<--"
         :checker scheme-chicken)))

  (flycheck-buttercup-def-checker-test scheme-chicken scheme syntax-error
    (assume (version<= "25.1" emacs-version))
    (flycheck-buttercup-should-syntax-check
     "language/chicken/syntax-error.scm" 'flycheck/chicken-mode
     '(1 nil error "not enough arguments\n\n\t(define)\n\n\tExpansion history:\n\n\t<syntax>\t  (##core#begin (define))\n\t<syntax>\t  (define)\t<--"
         :checker scheme-chicken)))

  (flycheck-buttercup-def-checker-test scheme-chicken scheme syntax-error-no-line-number
    (assume (version<= "25.1" emacs-version))
    (flycheck-buttercup-should-syntax-check
     "language/chicken/syntax-error-no-line-number.scm" 'flycheck/chicken-mode
     '(0 nil error "illegal atomic form\n\n\t()\n\n\tExpansion history:\n\n\t<syntax>\t  (##core#begin ())\t<--"
         :checker scheme-chicken)))

  (flycheck-buttercup-def-checker-test scheme-chicken scheme syntax-read-error
    (assume (version<= "25.1" emacs-version))
    (flycheck-buttercup-should-syntax-check
     "language/chicken/syntax-read-error.scm" 'flycheck/chicken-mode
     '(1 nil error "invalid sharp-sign read syntax: #\\n"
         :checker scheme-chicken))))

;;; test-scheme.el ends here
