;;; test-emacs-lisp.el --- Test the Emacs Lisp checker -*- lexical-binding: t; -*-

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
(require 'bytecomp)

(ert-deftest checker-emacs-lisp-sytnax-error ()
  "Test a syntax error caused by a missing parenthesis."
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-syntax-error.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(3 1 "End of file during parsing" error)))

(ert-deftest checker-emacs-lisp-syntax-error-compressed ()
  "Test a syntax error caused by a missing parenthesis."
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-syntax-error.el.gz" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(3 1 "End of file during parsing" error)))

(ert-deftest checker-emacs-lisp-warning ()
  "Test a warning caused by a missing argument."
  (flycheck-testsuite-should-syntax-check
   "checkers/emacs-lisp-warning.el" 'emacs-lisp-mode 'emacs-lisp-checkdoc
   '(4 6 "message called with 0 arguments,\n    but requires 1+" warning)))

(ert-deftest checker-emacs-lisp-inhibited-no-byte-compile ()
  "Test that Emacs Lisp does not check when byte compilation is
  disabled."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (set (make-local-variable 'no-byte-compile) t)
    (should (buffer-file-name))
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-no-file-name ()
  "Test that Emacs Lisp does not check buffers without file names."
  (with-temp-buffer
    (insert "(message \"Hello World\")")
    (emacs-lisp-mode)
    (should (not (buffer-file-name)))
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-autoloads ()
  "Test that Emacs Lisp does not check autoloads buffers.

These buffers are temporary buffers generated during package
installation, which may not be byte compiled, and hence the
checker will refuse to check these.

See URL `https://github.com/lunaryorn/flycheck/issues/45' and URL
`https://github.com/bbatsov/prelude/issues/253'."
  (flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp))
    (rename-buffer "foo-autoloads.el")
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest checker-emacs-lisp-inhibited-compiler-input ()
  "Test that Emacs Lisp does not check byte compiler input buffers.

These temporary buffers are created during byte compilation, and
checking them interfers with package installation.

See URL `https://github.com/lunaryorn/flycheck/issues/45' and URL
`https://github.com/bbatsov/prelude/issues/253'."
(flycheck-testsuite-with-resource-buffer "checkers/emacs-lisp-warning.el"
    (emacs-lisp-mode)
    (should (flycheck-may-use-checker 'emacs-lisp))
    (rename-buffer " *Compiler Input*")
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-emacs-lisp.el ends here
