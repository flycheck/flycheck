;;; test-emacs-lisp.el --- Test the Emacs Lisp checker

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

(ert-deftest emacs-lisp-missing-parenthesis ()
  "Test a syntax error caused by a missing parenthesis."
  (flycheck-with-resource-buffer "test-emacs-lisp/missing-parenthesis.el"
    (emacs-lisp-mode)
    (flycheck-should-checker
     'emacs-lisp '(3 1 "End of file during parsing" error))))

(ert-deftest emacs-lisp-missing-argument ()
  "Test a warning caused by a missing argument."
  (flycheck-with-resource-buffer "test-emacs-lisp/missing-argument.el"
    (emacs-lisp-mode)
    (flycheck-should-checker
     'emacs-lisp
     '(4 6 "message called with 0 arguments, but requires 1+" warning))))

(ert-deftest emacs-lisp-inhibited-no-byte-compile ()
  "Test that Emacs Lisp does not check when byte compilation is
  disabled."
  (flycheck-with-resource-buffer "test-emacs-lisp/missing-argument.el"
    (emacs-lisp-mode)
    (set (make-local-variable 'no-byte-compile) t)
    (should (buffer-file-name))
    (should (not (flycheck-may-use-checker 'emacs-lisp)))))

(ert-deftest emacs-lisp-inhibited-no-file-name ()
  "Test that Emacs Lisp does not check buffers without file names."
  (with-temp-buffer
    (insert "(message \"Hello World\")")
    (emacs-lisp-mode)
    (should (not (buffer-file-name)))))

;;; test-emacs-lisp.el ends here
