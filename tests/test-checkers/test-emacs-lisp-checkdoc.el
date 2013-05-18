;;; test-emacs-lisp-checkdoc.el --- Test the CheckDoc checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>,
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>,
;; URL: https://github.com/lunaryorn/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

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

Regression test for https://github.com/lunaryorn/flycheck/issues/73 and
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

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-autoloads-source ()
  "Test that CheckDoc does no check temporary autoload buffers."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (rename-buffer " *autoload-file*")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-compiler-input ()
  "Test that CheckDoc does not check byte compiler input buffers."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (rename-buffer " *Compiler Input*")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

(ert-deftest checker-emacs-lisp-checkdoc-inhibited-carton ()
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-checkdoc-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Cartony")  ; No real carton file
    (should (flycheck-may-use-checker 'emacs-lisp-checkdoc))
    (setq buffer-file-name "/foo/bar/Carton")
    (should-not (flycheck-may-use-checker 'emacs-lisp-checkdoc))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-emacs-lisp-checkdoc.el ends here
