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

(defconst flycheck-test-tramp-fake-prefix "/ssh:flycheck-nonexistent-host:"
  "A remote prefix used to drive the remote code paths of pure functions.

`file-remote-p' and `file-local-name' parse a prefix without opening a
connection.  The sweep below additionally stubs out every primitive that
would reach for the host, so it stays offline on any machine.")

(describe "Remote paths on a checker's command line"

  ;; A checker process runs on the host of `default-directory', so every
  ;; path handed to it has to be plain and local to that host.  Letting a
  ;; TRAMP name through hands the remote tool a path it cannot resolve.
  ;;
  ;; This walks the whole catalog rather than naming checkers, so a new
  ;; checker that forgets `file-local-name' fails here rather than in a
  ;; user's remote buffer.
  (it "never leaks a TRAMP prefix into any checker's arguments"
    (let* ((prefix flycheck-test-tramp-fake-prefix)
           (dir (concat prefix "/home/u/proj/"))
           (file (concat dir "src/checked"))
           (offenders nil)
           (substituted 0)
           ;; Walked only when the option is set, and it is where a
           ;; relative entry picks up the remote directory.
           (flycheck-emacs-lisp-load-path '("lisp"))
           ;; Probed per host and cached globally; keep the sweep out of
           ;; the real table.
           (flycheck--proselint-old-args-by-host (make-hash-table :test 'equal)))
      (cl-letf (;; Every primitive below would otherwise open a
                ;; connection to the fake host.  They answer with remote
                ;; names on purpose: stripping those is what is tested.
                ((symbol-function 'flycheck-save-buffer-to-temp)
                 (lambda (&rest _) (concat dir "source.tmp")))
                ((symbol-function 'flycheck-temp-dir-system) (lambda () dir))
                ((symbol-function 'make-nearby-temp-file)
                 (lambda (&rest _) (concat dir "nearby.tmp")))
                ;; "Found" at the project root, so the checkers that look
                ;; for a project marker take their real path.
                ((symbol-function 'locate-dominating-file) (lambda (&rest _) dir))
                ;; Likewise for a config file, whose search would stat the
                ;; remote host.  Substitution has to reduce this one too.
                ((symbol-function 'flycheck-locate-config-file)
                 (lambda (&rest _) (concat dir "config")))
                ;; Version probes shell out.
                ((symbol-function 'process-file) (lambda (&rest _) 1))
                ((symbol-function 'flycheck-find-checker-executable)
                 (lambda (&rest _) nil))
                ((symbol-function 'flycheck--file-truename) (lambda (f) f)))
        (dolist (checker flycheck-checkers)
          (when-let* ((mode (car (flycheck-checker-get checker 'modes))))
            (with-temp-buffer
              ;; Start the mode on a local name: a mode that stats its
              ;; file would otherwise reach for the host.
              (setq buffer-file-name "/home/u/proj/src/checked")
              ;; Substitution reads `major-mode' and mode-local
              ;; variables, so a bare setting stands in for a mode whose
              ;; package is not installed here.
              (unless (and (fboundp mode)
                           (ignore-errors (delay-mode-hooks (funcall mode)) t))
                (setq major-mode mode))
              (setq default-directory dir buffer-file-name file)
              (condition-case err
                  (let ((args (flycheck-checker-substituted-arguments checker)))
                    (setq substituted (1+ substituted))
                    (dolist (arg args)
                      ;; Not `file-remote-p': it only matches a leading
                      ;; prefix, and these paths hide inside "-I/ssh:..."
                      ;; and inside sexps handed to --eval.
                      (when (and (stringp arg)
                                 (string-match-p (regexp-quote prefix) arg))
                        (push (list checker arg) offenders))))
                ;; A checker that cannot build its arguments is not
                ;; covered, so say so rather than passing quietly.
                (error (push (list checker 'failed-to-substitute
                                   (error-message-string err))
                             offenders)))))))
      ;; Guard the guard: with nothing substituted the sweep proves
      ;; nothing, so say so instead of passing vacuously.
      (expect substituted :to-be-greater-than 100)
      (expect offenders :to-equal nil))))

(provide 'test-tramp)

;;; test-tramp.el ends here
