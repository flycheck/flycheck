;;; test-automatic-check.el.el --- Tests for automatic checking -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
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
(require 'dash)

(ert-deftest flycheck-may-check-automatically ()
  (--each '(save idle-change new-line mode-enabled)
    (with-temp-buffer
      (should (flycheck-may-check-automatically it))
      (should (flycheck-may-check-automatically))
      ;; No automatic check allowed
      (let ((flycheck-check-syntax-automatically nil))
        (should-not (flycheck-may-check-automatically it))
        (should (flycheck-may-check-automatically)))
      ;; Disable just a specific event
      (let ((flycheck-check-syntax-automatically
             (remq it flycheck-check-syntax-automatically)))
        (should flycheck-check-syntax-automatically)
        (should-not (flycheck-may-check-automatically it))
        (should (flycheck-may-check-automatically))))))

(defun flycheck-mode-no-check ()
  (emacs-lisp-mode)
  (let ((flycheck-check-syntax-automatically nil))
    (flycheck-mode))
  (should-not (flycheck-deferred-check-p)))

(ert-deftest flycheck-check-syntax-automatically ()
  ;; If an automatic check was about to happen, we have a deferred check pending
  ;; afterwards, because the temporary resource buffer has no associated window
  ;; and is thus not checked immediately.  This allows us to neatly test whether
  ;; an automatic test happened
  (--each '(save idle-change new-line mode-enabled)
    (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
      (flycheck-mode-no-check)
      (flycheck-buffer-automatically it)
      (should (flycheck-deferred-check-p)))
    ;; Disable automatic checks completely
    (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
      (flycheck-mode-no-check)
      (let ((flycheck-check-syntax-automatically nil))
        (flycheck-buffer-automatically it))
      (should-not (flycheck-deferred-check-p)))
    ;; Disable just a specific event
    (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
      (flycheck-mode-no-check)
      (let ((flycheck-check-syntax-automatically
             (remq it flycheck-check-syntax-automatically)))
        (should flycheck-check-syntax-automatically)
        (flycheck-buffer-automatically it))
      (should-not (flycheck-deferred-check-p)))))

(ert-deftest flycheck-check-syntax-automatically-mode-enabled ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically
           (remq 'mode-enabled flycheck-check-syntax-automatically)))
      (flycheck-mode))
    (should-not (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (should-not (flycheck-deferred-check-p))
    (let ((flycheck-check-syntax-automatically '(mode-enabled)))
      (flycheck-mode))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-check-syntax-automatically-idle-change ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically
           (remq 'idle-change flycheck-check-syntax-automatically)))
      (insert "Hello world")
      (sleep-for 0.55))
    (should-not (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically '(idle-change)))
      (insert "Hello world")
      (sleep-for 0.55))
    (should (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (emacs-lisp-mode)
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically '(idle-change))
          (flycheck-idle-change-delay 1.5))
      (insert "Hello world")
      (sleep-for 0.55)
      (should-not (flycheck-deferred-check-p))
      (sleep-for 1))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-check-syntax-automatically-new-line ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically
           (remq 'new-line flycheck-check-syntax-automatically)))
      (insert "\n"))
    (should-not (flycheck-deferred-check-p)))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (let ((flycheck-check-syntax-automatically '(new-line)))
      (insert "\n"))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-check-syntax-automatically-save ()
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically
           (remq 'save flycheck-check-syntax-automatically)))
      (save-buffer 0)
      (should-not (flycheck-deferred-check-p))))
  (flycheck-testsuite-with-resource-buffer "automatic-check-dummy.el"
    (flycheck-mode-no-check)
    (set-buffer-modified-p t)
    (let ((flycheck-check-syntax-automatically '(save)))
      (save-buffer 0))
    (should (flycheck-deferred-check-p))))

(ert-deftest flycheck-buffer-automatically-mode-disabled ()
  (with-temp-buffer
    (should-not flycheck-mode)
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-automatically)
    (should-not (flycheck-deferred-check-p))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; checkers-automatic-check.el ends here
