;;; test-sh.el --- Flycheck Specs: Shell      -*- lexical-binding: t; -*-

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

;; Specs for Shell script support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Shell"
  (flycheck-buttercup-def-checker-test sh-bash (sh sh-bash) nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/sh/bash-syntax-error.bash" 'sh-mode
       '(5 nil error "syntax error near unexpected token `fi'" :checker sh-bash)
       '(5 nil error "`fi'" :checker sh-bash))))

  (flycheck-buttercup-def-checker-test sh-posix-dash (sh sh-posix) nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/sh/posix-syntax-error.sh" 'sh-mode
       '(3 nil error "Syntax error: \"(\" unexpected" :checker sh-posix-dash))))

  (flycheck-buttercup-def-checker-test sh-posix-bash (sh sh-posix) nil
    (let ((flycheck-disabled-checkers '(sh-posix-dash))
          (inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/sh/posix-syntax-error.sh" 'sh-mode
       '(3 nil error "syntax error near unexpected token `('"
           :checker sh-posix-bash)
       '(3 nil error "`cat <(echo blah)'" :checker sh-posix-bash))))

  (flycheck-buttercup-def-checker-test sh-zsh (sh sh-zsh) nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/sh/zsh-syntax-error.zsh" 'sh-mode
       '(5 nil error "parse error near `fi'" :checker sh-zsh))))

  (flycheck-buttercup-def-checker-test sh-shellcheck sh nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/sh/shellcheck.sh" 'sh-mode
       ;; Matches output from shellcheck 0.11.0:
       '(2 5 warning "Tilde does not expand in quotes. Use $HOME."
           :checker sh-shellcheck :id "SC2088")
       '(3 7 error "Double quote array expansions to avoid re-splitting elements."
           :checker sh-shellcheck :id "SC2068")
       '(4 8 warning "Declare and assign separately to avoid masking return values."
           :checker sh-shellcheck :id "SC2155")
       '(4 11 warning "Quote this to prevent word splitting."
           :checker sh-shellcheck :id "SC2046")
       '(4 11 info "Use $(...) notation instead of legacy backticks `...`."
           :checker sh-shellcheck :id "SC2006"))))

  (flycheck-buttercup-def-checker-test sh-shellcheck sh infer-shell
    (let ((inhibit-message t)
          (flycheck-shellcheck-supported-shells '(bash ksh88 sh zsh))
          (flycheck-shellcheck-infer-shell t))
      (flycheck-buttercup-should-syntax-check
       "language/sh/shellcheck-infer.sh" 'sh-mode
       '(4 15 warning "Remove quotes from right-hand side of =~ to match as a regex rather than literally."
           :checker sh-shellcheck :id "SC2076")))))

;;; test-sh.el ends here
