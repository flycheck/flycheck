;;; test-emacs-lisp.el --- Flycheck Specs: Emacs Lisp      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>

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

;; Specs for Emacs Lisp support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)
(require 'checkdoc)

(defconst flycheck/excluded-checkdoc-vars
  '(checkdoc-autofix-flag checkdoc-bouncy-flag checkdoc-minor-mode-string)
  "Checkdoc variables that should not be passed to the checker.")

(defun flycheck/custom-var-p (var)
  "Check if VAR is a customization pair describing a variable."
  (eq (cadr var) 'custom-variable))

(describe "Language Emacs Lisp"

  (describe "Checkdoc"

    (it "exports all customizable variables"
      (assume (version<= "25" emacs-version)
              "Checkdoc has additional variables in Emacs 25")
      (let* ((checkdoc-custom-group (get 'checkdoc 'custom-group))
             (checkdoc-custom-pairs
              (seq-filter #'flycheck/custom-var-p checkdoc-custom-group))
             (checkdoc-custom-vars (seq-map #'car checkdoc-custom-pairs))
             (checkdoc-relevant-vars
              (cons 'sentence-end-double-space
                    (seq-difference checkdoc-custom-vars
                                    flycheck/excluded-checkdoc-vars))))
        (expect (seq-sort #'string-lessp flycheck-emacs-lisp-checkdoc-variables)
                :to-equal
                (seq-sort #'string-lessp checkdoc-relevant-vars)))))

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp nil
      (flycheck-buttercup-should-syntax-check
       "language/emacs-lisp/warnings.el" 'emacs-lisp-mode
       '(12 nil info "First sentence should end with punctuation"
            :checker emacs-lisp-checkdoc)
       '(16 6 warning "foobar called with 1 argument, but accepts only 0"
            :checker emacs-lisp)
       '(21 1 warning "the function `dummy-package-foo' is not known to be defined."
            :checker emacs-lisp )))

    (flycheck-buttercup-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                                         uses-right-major-mode
      (flycheck-buttercup-should-syntax-check
       "language/emacs-lisp/checkdoc-elisp-mode-regression.el" 'emacs-lisp-mode
       '(11 nil info "All variables and subroutines might as well have a documentation string"
            :checker emacs-lisp-checkdoc)))

    (flycheck-buttercup-def-checker-test (emacs-lisp-checkdoc) emacs-lisp
                                         inherits-checkdoc-variables
      (flycheck-buttercup-should-syntax-check
       "language/emacs-lisp/local-checkdoc-variables.el" 'emacs-lisp-mode))

    (flycheck-buttercup-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                                         checks-compressed-file
      (let ((inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/emacs-lisp/warnings.el.gz" 'emacs-lisp-mode
         '(12 nil info "First sentence should end with punctuation"
              :checker emacs-lisp-checkdoc)
         '(16 6 warning "foobar called with 1 argument, but accepts only 0"
              :checker emacs-lisp)
         '(21 1 warning "the function `dummy-package-foo' is not known to be defined."
              :checker emacs-lisp))))

    (flycheck-buttercup-def-checker-test emacs-lisp emacs-lisp syntax-error
      (let ((flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
        (flycheck-buttercup-should-syntax-check
         "language/emacs-lisp/syntax-error.el" 'emacs-lisp-mode
         '(3 1 error "End of file during parsing" :checker emacs-lisp))))

    (flycheck-buttercup-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                                         without-file-name
      ;; Regression test for checkdoc in buffers without file names.
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (set-visited-file-name nil 'no-query)
        (emacs-lisp-mode)
        (expect (buffer-file-name) :not :to-be-truthy)
        (flycheck-buttercup-buffer-sync)
        (expect flycheck-current-errors :to-be-truthy)))

    (flycheck-buttercup-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                                         does-not-check-autoloads-buffers
      ;; Regression test ensuring that Emacs Lisp won't check autoload buffers.
      (flycheck-buttercup-with-file-buffer (locate-library "shut-up-autoloads")
        (expect (flycheck-may-use-checker 'emacs-lisp) :not :to-be-truthy)
        (expect (flycheck-may-use-checker 'emacs-lisp-checkdoc) :not :to-be-truthy)))

    (flycheck-buttercup-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                                         checkdoc-does-not-check-cask-files
      (flycheck-buttercup-with-file-buffer
          (expand-file-name "Cask" flycheck-test-source-directory)
        (expect (flycheck-may-use-checker 'emacs-lisp-checkdoc) :not :to-be-truthy)))

    (flycheck-buttercup-def-checker-test (emacs-lisp emacs-lisp-checkdoc) emacs-lisp
                                         does-not-check-with-no-byte-compile
      ;; We need to use a hook here, because `no-byte-compile' seems to be
      ;; explicitly changed when loading Emacs Lisp files
      (let ((disable-byte-comp (lambda () (setq-local no-byte-compile t))))
        (add-hook 'emacs-lisp-mode-hook disable-byte-comp)
        (unwind-protect
            (flycheck-buttercup-should-syntax-check
             "language/emacs-lisp/warnings.el" 'emacs-lisp-mode
             '(12 nil info "First sentence should end with punctuation"
                  :checker emacs-lisp-checkdoc))
          (remove-hook 'emacs-lisp-mode-hook disable-byte-comp))))

    (flycheck-buttercup-def-checker-test emacs-lisp emacs-lisp check-declare-warnings
      (let ((flycheck-emacs-lisp-check-declare t))
        (flycheck-buttercup-should-syntax-check
         "language/emacs-lisp/check-declare-warnings.el" 'emacs-lisp-mode
         (cond
          ((version< emacs-version "25")
           '(0 nil warning "`this-function-is-not-declared' was defined in this-file-does-not-exist.el: file not found"
               :checker emacs-lisp))
          ((version< emacs-version "26")
           '(9 1 warning "`this-function-is-not-declared' was defined in this-file-does-not-exist.el: file not found"
               :checker emacs-lisp))
          (t
           '(9 nil warning "`this-function-is-not-declared' was defined in this-file-does-not-exist.el: file not found"
               :checker emacs-lisp))))))

    (flycheck-buttercup-def-checker-test emacs-lisp emacs-lisp disable-check-declare
      (let ((flycheck-emacs-lisp-check-declare nil))
        (flycheck-buttercup-should-syntax-check
         "language/emacs-lisp/check-declare-warnings.el" 'emacs-lisp-mode)))))

;;; test-emacs-lisp.el ends here
