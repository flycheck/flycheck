;;; executable-test.el --- Tests for checker executables  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

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

;; Tests for checker executables

;;; Code:

(require 'test-helper)
(require 'flycheck)

(ert-deftest flycheck-overridden-executable ()
  (flycheck-testsuite-with-hook 'emacs-lisp-mode-hook
      (setq flycheck-emacs-lisp-executable
            (flycheck-testsuite-resource-filename "bin/dummy-emacs"))
    (flycheck-testsuite-should-syntax-check
     "checkers/emacs-lisp.el" 'emacs-lisp-mode
     '(12 nil warning "First sentence should end with punctuation"
          :checker emacs-lisp-checkdoc)
     '(17 4 error "t is not true!" :checker emacs-lisp)
     '(19 11 warning "This is a stupid message" :checker emacs-lisp))))

(ert-deftest flycheck-set-checker-executable/real-executable ()
  (with-temp-buffer
    ;; Create a temporary buffer to restrict the scope of
    ;; `flycheck-emacs-lisp-executable'
    (let ((file-name (flycheck-testsuite-resource-filename "bin/dummy-emacs")))
      (should (f-exists? file-name))
      (should (f-executable? file-name))
      (flycheck-set-checker-executable 'emacs-lisp file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))))
  ;; The global value should remain unaffected
  (should-not flycheck-emacs-lisp-executable))

(ert-deftest flycheck-set-checker-executable/no-executable-given ()
  (with-temp-buffer
    (let ((file-name (flycheck-testsuite-resource-filename "bin/dummy-emacs")))
      (setq flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp)
      (should-not flycheck-emacs-lisp-executable))))

(ert-deftest flycheck-set-checker-executable/executable-is-nil ()
  (with-temp-buffer
    (let ((file-name (flycheck-testsuite-resource-filename "bin/dummy-emacs")))
      (setq flycheck-emacs-lisp-executable file-name)
      (should (string= flycheck-emacs-lisp-executable file-name))
      (flycheck-set-checker-executable 'emacs-lisp nil)
      (should-not flycheck-emacs-lisp-executable))))

(ert-deftest flycheck-set-checker-executable/non-existing-file ()
  (let ((file-name (flycheck-testsuite-resource-filename "no-such-file")))
    (should-not (f-exists? file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))

(ert-deftest flycheck-set-checker-executable/file-not-executable ()
  (let ((file-name (flycheck-testsuite-resource-filename "checkers/emacs-lisp.el")))
    (should (f-exists? file-name))
    (should-not (f-executable? file-name))
    (let ((err (should-error (flycheck-set-checker-executable
                              'emacs-lisp file-name)
                             :type flycheck-testsuite-user-error-type)))
      (should (string= (cadr err) (format "%s is no executable" file-name))))))

;;; executable-test.el ends here
