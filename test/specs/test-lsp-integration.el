;;; test-lsp-integration.el --- Flycheck Specs: LSP end-to-end  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

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

;; End-to-end specs for the LSP bridges, against the scripted server in
;; test/mock-lsp-server.py.  The unit specs in test-lsp.el and
;; test-eglot.el simulate the reports the bridges receive; these run the
;; real thing: a server process, the jsonrpc session, the asynchronous
;; initialize handshake, and (through Eglot) whatever report cadence the
;; running Emacs actually uses.  That cadence is the part that has
;; drifted under us before: Emacs 31 answers one request for
;; diagnostics with two reports where Emacs 30 sends one, which is how
;; the re-check storms of #2262 and #2291 got past the unit specs.
;;
;; The server counts every method it receives into a stats file, so the
;; specs can assert not only that diagnostics arrive but that a
;; republishing or pull-model server is not asked again and again.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

;; Bound by eglot, which the spec that uses them loads first
(defvar eglot-server-programs)
(defvar eglot-sync-connect)

(defconst test-lsp-integration/server
  (expand-file-name "mock-lsp-server.py" flycheck-test-directory)
  "Path to the scripted LSP server.")

(defun test-lsp-integration/possible-p ()
  "Whether this machine can run the end-to-end LSP specs at all."
  (and (not (memq system-type '(windows-nt ms-dos cygwin)))
       (executable-find "python3")))

(defun test-lsp-integration/wait-until (pred timeout)
  "Pump events until PRED returns non-nil or TIMEOUT seconds pass.

Alternates process output with `sit-for', which is what runs the
timers the bridges lean on (Eglot's settle timer, the handshake's
timeout).  Return PRED's final answer."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (sit-for 0.02))
    (funcall pred)))

(defun test-lsp-integration/pump (seconds)
  "Pump events for SECONDS, without waiting on anything in particular."
  (test-lsp-integration/wait-until #'ignore seconds))

(defvar test-lsp-integration/stats-file nil
  "Where the current spec's server writes its method counts.")

(defun test-lsp-integration/stats ()
  "Return the mock server's method counts, as an alist of (METHOD . N)."
  (when (file-exists-p test-lsp-integration/stats-file)
    (mapcar (lambda (line)
              (pcase-let ((`(,method ,n) (split-string line " ")))
                (cons method (and n (string-to-number n)))))
            (split-string (with-temp-buffer
                            (insert-file-contents test-lsp-integration/stats-file)
                            (buffer-string))
                          "\n" t))))

(defmacro test-lsp-integration/with-file-buffer (&rest body)
  "Run BODY in a `makefile-gmake-mode' buffer visiting a fresh temp file.

The buffer is shown in the selected window: a check in a buffer no
window displays is deferred (`flycheck-must-defer-check'), and nothing
in a batch session would ever run it.  The servers BODY starts inherit
`test-lsp-integration/stats-file' through the environment, and are shut
down before the temp directory goes away."
  (declare (indent 0))
  (let ((dir (make-symbol "dir"))
        (file (make-symbol "file"))
        (buffer (make-symbol "buffer")))
    `(let* ((,dir (make-temp-file "flycheck-lsp-integration" 'dir))
            (,file (expand-file-name "sample.mk" ,dir))
            (test-lsp-integration/stats-file (expand-file-name "stats" ,dir))
            (process-environment
             (cons (concat "MOCK_LSP_STATS=" test-lsp-integration/stats-file)
                   process-environment)))
       (write-region "help:\n\techo hi\n" nil ,file nil 'quiet)
       (let ((,buffer (find-file-noselect ,file)))
         (unwind-protect
             (save-window-excursion
               (set-window-buffer (selected-window) ,buffer)
               (with-current-buffer ,buffer
                 (makefile-gmake-mode)
                 ,@body))
           (ignore-errors
             (with-current-buffer ,buffer (flycheck-lsp-mode -1)))
           (kill-buffer ,buffer)
           ;; Before the directory disappears under the stats file
           (flycheck-lsp--shutdown-all)
           (delete-directory ,dir 'recursive))))))

;; Enabling a bridge calls `flycheck-add-mode', which permanently grows
;; the checker's global `modes' list; restore what the specs pollute
(defmacro test-lsp-integration/preserving-checker (checker &rest body)
  "Run BODY and restore CHECKER's `modes' and `next-checkers' after it."
  (declare (indent 1))
  `(let ((modes (flycheck-checker-get ,checker 'modes))
         (next (flycheck-checker-get ,checker 'next-checkers)))
     (unwind-protect
         (progn ,@body)
       (setf (flycheck-checker-get ,checker 'modes) modes)
       (setf (flycheck-checker-get ,checker 'next-checkers) next))))

(describe "The native lsp checker, end to end"

  (it "reports the diagnostics a scripted server pushes"
    (assume (test-lsp-integration/possible-p) "No python3, or Windows")
    (test-lsp-integration/preserving-checker 'flycheck-lsp
      (test-lsp-integration/with-file-buffer
        (let ((flycheck-lsp-servers
               `((makefile-gmake-mode . ("python3" ,test-lsp-integration/server
                                         "change"))))
              (flycheck-checkers '(flycheck-lsp)))
          (flycheck-lsp-mode 1)
          (flycheck-buffer)
          (expect (test-lsp-integration/wait-until
                   (lambda () (= 2 (length flycheck-current-errors))) 10)
                  :to-be-truthy)
          (let ((messages (mapcar #'flycheck-error-message flycheck-current-errors)))
            (expect messages :to-have-same-items-as
                    '("mock warning" "mock error")))
          (expect (mapcar #'flycheck-error-level flycheck-current-errors)
                  :to-have-same-items-as '(warning error))))))

  (it "pulls a pull-model server's diagnostics, and its workspace on demand"
    (assume (test-lsp-integration/possible-p) "No python3, or Windows")
    (test-lsp-integration/preserving-checker 'flycheck-lsp
      (test-lsp-integration/with-file-buffer
        (let ((flycheck-lsp-servers
               `((makefile-gmake-mode . ("python3" ,test-lsp-integration/server
                                         "workspace"))))
              (flycheck-checkers '(flycheck-lsp)))
          (flycheck-lsp-mode 1)
          (flycheck-buffer)
          ;; The server never pushes: the buffer's diagnostics come from
          ;; the document pull its check sends
          (expect (test-lsp-integration/wait-until
                   (lambda () (= 1 (length flycheck-current-errors))) 10)
                  :to-be-truthy)
          (expect (flycheck-error-message (car flycheck-current-errors))
                  :to-equal "mock warning")
          (let ((key (flycheck--buffer-project-key)))
            ;; Checking the project asks the server about its whole
            ;; workspace, which surfaces the sibling nobody opened
            (flycheck-check-project)
            (expect (test-lsp-integration/wait-until
                     (lambda ()
                       (seq-some (lambda (err)
                                   (and (equal (flycheck-error-message err)
                                               "mock error")
                                        (equal (file-name-nondirectory
                                                (flycheck-error-filename err))
                                               "other.mk")))
                                 (flycheck--project-errors key)))
                     10)
                    :to-be-truthy)
            ;; The next pull hands the result ids back, and the server
            ;; has nothing new to say
            (flycheck-check-project)
            (expect (test-lsp-integration/wait-until
                     (lambda ()
                       (equal (cdr (assoc "workspace/diagnostic/unchanged"
                                          (test-lsp-integration/stats)))
                              1))
                     10)
                    :to-be-truthy))
          ;; And pulling did not turn into a feedback loop
          (test-lsp-integration/pump 1)
          (expect (or (cdr (assoc "textDocument/diagnostic"
                                  (test-lsp-integration/stats)))
                      0)
                  :to-be-less-than 10)))))

  (it "checks once for an indexing server's republish storm"
    (assume (test-lsp-integration/possible-p) "No python3, or Windows")
    (test-lsp-integration/preserving-checker 'flycheck-lsp
      (test-lsp-integration/with-file-buffer
        (let ((flycheck-lsp-servers
               `((makefile-gmake-mode . ("python3" ,test-lsp-integration/server
                                         "push"))))
              (flycheck-checkers '(flycheck-lsp)))
          (flycheck-lsp-mode 1)
          (flycheck-buffer)
          ;; All five identical pushes must arrive...
          (expect (test-lsp-integration/wait-until
                   (lambda () (>= flycheck-lsp--push-count 5)) 10)
                  :to-be-truthy)
          ;; ...and the diagnostic with them
          (expect (test-lsp-integration/wait-until
                   (lambda () flycheck-current-errors) 10)
                  :to-be-truthy)
          ;; but only the first, which changed something, cost a re-check
          (expect flycheck-lsp--recheck-count :to-equal 1))))))

(describe "The Eglot bridge, end to end"

  (it "delivers the diagnostics without storming the server"
    (assume (test-lsp-integration/possible-p) "No python3, or Windows")
    (assume (require 'eglot nil 'noerror) "Eglot not available")
    (assume (and (fboundp 'eglot--connect) (fboundp 'eglot--guess-contact)
                 ;; A signature change would error out of `apply', not skip
                 (equal (func-arity (symbol-function 'eglot--connect)) '(5 . 5)))
            "Eglot's connect entry points moved")
    (test-lsp-integration/preserving-checker 'eglot-check
      (test-lsp-integration/with-file-buffer
        (let ((eglot-server-programs
               `((makefile-gmake-mode . ("python3" ,test-lsp-integration/server
                                         "pull"))))
              (eglot-sync-connect t)
              (flycheck-checkers '(eglot-check))
              (server nil))
          (unwind-protect
              (progn
                (setq server (apply #'eglot--connect (eglot--guess-contact)))
                (expect (eglot-managed-p) :to-be-truthy)
                (flycheck-eglot-mode 1)
                (flycheck-buffer)
                ;; The server's answer reaches the buffer whichever cadence
                ;; this Emacs uses: one pushed report (Emacs 30), or the
                ;; pulled and the pushed halves of one answer (Emacs 31)
                (expect (test-lsp-integration/wait-until
                         (lambda ()
                           (seq-some (lambda (err)
                                       (equal (flycheck-error-message err)
                                              "mock warning"))
                                     flycheck-current-errors))
                         10)
                        :to-be-truthy)
                ;; Give a feedback loop, which would ask hundreds of times a
                ;; second (#2291), a full second to show itself
                (test-lsp-integration/pump 1)
                (let ((pulls (or (cdr (assoc "textDocument/diagnostic"
                                             (test-lsp-integration/stats)))
                                 0)))
                  (expect pulls :to-be-less-than 10)))
            (flycheck-eglot-mode -1)
            (advice-remove 'flymake-diagnostics
                           #'flycheck-eglot--flymake-diagnostics)
            (when server
              (ignore-errors (eglot-shutdown server)))))))))

;;; test-lsp-integration.el ends here
