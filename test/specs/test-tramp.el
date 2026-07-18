;;; test-tramp.el --- Flycheck Specs: Remote checking over TRAMP -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

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

;;; Commentary:

;; Specs exercising Flycheck over a TRAMP connection.  They use TRAMP's
;; "mock" method, which runs a local shell through the full remote
;; file-name handler stack, so `process-file', `start-file-process',
;; `make-nearby-temp-file' and remote `executable-find' all take their
;; remote code paths against localhost -- no real remote host needed.

;;; Code:

(require 'flycheck-buttercup)
(require 'tramp)
(require 'python)

(defconst flycheck-test-tramp-remote-prefix "/mock:localhost:"
  "TRAMP prefix of the local mock connection used by these specs.")

(defun flycheck-test-tramp-setup-method ()
  "Register TRAMP's mock method, running a local shell as the remote."
  (add-to-list 'tramp-methods
               '("mock"
                 (tramp-login-program "sh")
                 (tramp-login-args (("-i")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))
                 (tramp-connection-timeout 10)))
  (add-to-list 'tramp-default-host-alist '("\\`mock\\'" nil "localhost")))

(defun flycheck-test-tramp-connectable-p ()
  "Return non-nil if a mock TRAMP connection can be established."
  (ignore-errors
    (let ((default-directory flycheck-test-tramp-remote-prefix))
      (file-exists-p (concat flycheck-test-tramp-remote-prefix "/")))))

(describe "Remote syntax checking over TRAMP"
  (before-all
    (flycheck-test-tramp-setup-method))

  (after-each
    (ignore-errors (tramp-cleanup-all-connections)))

  (it "checks a remote buffer end to end and maps error filenames back"
    ;; Skip (rather than fail) when the mock connection can't be brought
    ;; up or python3 is unavailable -- these are environment issues, not
    ;; regressions.  Everything past the assumes is a hard assertion.
    (assume (flycheck-test-tramp-connectable-p) "no mock TRAMP connection")
    (let ((default-directory flycheck-test-tramp-remote-prefix))
      (assume (executable-find "python3" t) "python3 not on remote"))
    (let* ((local (make-temp-file "flycheck-tramp-" nil ".py"
                                  "import os\n\ndef broken(:\n    pass\n"))
           (remote (concat flycheck-test-tramp-remote-prefix local))
           (buf (find-file-noselect remote)))
      (unwind-protect
          (with-current-buffer buf
            (expect (file-remote-p default-directory) :to-be-truthy)
            ;; Flycheck must be willing to run here now.
            (expect (flycheck-may-enable-mode) :to-be-truthy)
            (python-mode)
            (let ((flycheck-checkers '(python-pycompile))
                  (flycheck-check-syntax-automatically nil)
                  (done nil)
                  (deadline (+ 30 (float-time))))
              (add-hook 'flycheck-after-syntax-check-hook
                        (lambda () (setq done t)) nil t)
              (flycheck-mode)
              (flycheck-buffer)
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.2))
              (expect done :to-be-truthy)
              (expect flycheck-current-errors :to-be-truthy)
              (let ((err (car flycheck-current-errors)))
                (expect (flycheck-error-level err) :to-be 'error)
                ;; The filename reported by the remote checker is mapped
                ;; back to the full remote name, not a bare local path.
                (expect (flycheck-error-filename err)
                        :to-equal remote))))
        (kill-buffer buf)
        (ignore-errors (delete-file local))))))

(provide 'test-tramp)

;;; test-tramp.el ends here
