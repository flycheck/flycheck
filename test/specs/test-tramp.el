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

(describe "Standard input on a remote host"

  ;; Tramp hands back the shell connection it multiplexes every remote
  ;; command through, and an end of file cannot be expressed on it: a
  ;; checker reading to the end of its input waits for one for ever.  The
  ;; buffer goes to a file on that host instead, and the remote shell
  ;; redirects it.  The mock method drives all of that against localhost.

  (before-all
    (flycheck-test-tramp-setup-method))

  (before-each
    ;; Counting bytes answers both questions at once: whether the input
    ;; arrived at all, and whether it arrived unchanged.
    (flycheck-define-command-checker 'test-stdin
      "Reports the size of what it was given on standard input."
      :command '("wc" "-c")
      :standard-input t
      :error-parser
      (lambda (output _checker _buffer)
        (list (flycheck-error-new-at 1 1 'info (string-trim output))))
      :modes '(text-mode)))

  (after-each
    (setf (symbol-plist 'test-stdin) nil)
    (ignore-errors (tramp-cleanup-all-connections)))

  (defun flycheck-test-stdin-size (file &optional narrow)
    "Check FILE with the stdin checker and return the size it saw.
With NARROW, narrow the buffer first: a narrowing must not change what
the checker is given."
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer buffer
            (text-mode)
            (when narrow
              (narrow-to-region (point-min) (min (point-max) (+ (point-min) 2))))
            (let ((flycheck-checkers '(test-stdin))
                  (flycheck-check-syntax-automatically nil)
                  (done nil)
                  (deadline (+ 30 (float-time))))
              (add-hook 'flycheck-after-syntax-check-hook
                        (lambda () (setq done t)) nil t)
              (flycheck-mode)
              (flycheck-buffer)
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.2))
              ;; Without the redirect this is where it stops: the check
              ;; never finishes, so there is nothing to compare.
              (expect done :to-be-truthy)
              (when-let* ((err (car flycheck-current-errors)))
                (string-to-number (flycheck-error-message err)))))
        (kill-buffer buffer))))

  (it "gives a remote checker the whole buffer, byte for byte"
    (assume (flycheck-test-tramp-connectable-p) "no mock TRAMP connection")
    ;; A carriage return and a literal C-d are what a pty would rewrite
    ;; and swallow on the way, so they are what the count has to prove.
    (let* ((content (concat "a\r\nb" (string ?\C-d) "c\n"))
           (local (make-temp-file "flycheck-stdin-" nil ".txt" content))
           (remote (concat flycheck-test-tramp-remote-prefix local)))
      (unwind-protect
          (expect (flycheck-test-stdin-size remote)
                  :to-equal (string-bytes content))
        (ignore-errors (delete-file local)))))

  (it "gives it the whole buffer even when the buffer is narrowed"
    (assume (flycheck-test-tramp-connectable-p) "no mock TRAMP connection")
    (let* ((content "one\ntwo\nthree\n")
           (local (make-temp-file "flycheck-stdin-" nil ".txt" content))
           (remote (concat flycheck-test-tramp-remote-prefix local)))
      (unwind-protect
          (expect (flycheck-test-stdin-size remote 'narrow)
                  :to-equal (string-bytes content))
        (ignore-errors (delete-file local)))))

  (it "encodes the buffer the way the local path would"
    (assume (flycheck-test-tramp-connectable-p) "no mock TRAMP connection")
    ;; A file whose lines end CRLF decodes to a buffer that has none.
    ;; Writing it back out with the buffer's own coding system would put
    ;; them back, handing the checker two bytes the local path never sends.
    (let* ((local (make-temp-file "flycheck-stdin-" nil ".txt"))
           (remote (concat flycheck-test-tramp-remote-prefix local)))
      (let ((coding-system-for-write 'binary))
        (write-region "one\r\ntwo\r\n" nil local nil 'quiet))
      (unwind-protect
          (progn
            ;; Guard the guard: the carriage returns have to be on disk for
            ;; this to be testing anything.
            (expect (file-attribute-size (file-attributes local)) :to-equal 10)
            (expect (flycheck-test-stdin-size remote) :to-equal 8))
        (ignore-errors (delete-file local)))))

  (it "does not also send the buffer on standard input"
    (assume (flycheck-test-tramp-connectable-p) "no mock TRAMP connection")
    (let* ((local (make-temp-file "flycheck-stdin-" nil ".txt" "one\n"))
           (remote (concat flycheck-test-tramp-remote-prefix local)))
      (unwind-protect
          (progn
            (spy-on 'flycheck-process-send-buffer)
            (flycheck-test-stdin-size remote)
            ;; It went into the file, so writing it into the connection as
            ;; well would leave the buffer's text sitting in the remote
            ;; shell's input.
            (expect 'flycheck-process-send-buffer :not :to-have-been-called))
        (ignore-errors (delete-file local)))))

  (it "leaves the local path sending on standard input"
    (let* ((content "one\ntwo\n")
           (local (make-temp-file "flycheck-stdin-" nil ".txt" content)))
      (unwind-protect
          (progn
            (spy-on 'flycheck-process-send-buffer :and-call-through)
            (expect (flycheck-test-stdin-size local)
                    :to-equal (string-bytes content))
            (expect 'flycheck-process-send-buffer :to-have-been-called))
        (ignore-errors (delete-file local))))))

(describe "Interrupting a check on a remote host"

  ;; `delete-process' drops Emacs's end of a remote process and leaves the
  ;; command running on the other host.  The mock method runs it locally,
  ;; so the spec can watch for it and say whether it really stopped.

  (before-all
    (flycheck-test-tramp-setup-method))

  (after-each
    (setf (symbol-plist 'test-slow) nil)
    (ignore-errors (tramp-cleanup-all-connections)))

  (defun flycheck-test-checker-process ()
    "Return the process of the check running in this buffer, if any.
Flycheck notes the buffer on the process, which is what tells this
buffer\='s check from one an earlier spec left behind."
    (let ((buffer (current-buffer)))
      (seq-find (lambda (process)
                  (eq buffer (process-get process 'flycheck-buffer)))
                (process-list))))

  ;; The mock method runs the "remote" command as a local child of Emacs,
  ;; in Emacs's own process group, so deleting the process stops it there
  ;; whether or not it was interrupted first.  Only a real other host shows
  ;; the difference, which is why this pins the mechanism rather than
  ;; watching for the process: measured over ssh and over docker, a stopped
  ;; check left its command running until the interrupt was added.

  (it "interrupts the command on the other host, rather than only dropping it"
    (assume (flycheck-test-tramp-connectable-p) "no mock TRAMP connection")
    (let* ((local (make-temp-file "flycheck-interrupt-" nil ".txt" "hello\n"))
           (remote (concat flycheck-test-tramp-remote-prefix local))
           (buffer nil))
      (flycheck-define-command-checker 'test-slow
        "Reads standard input and then waits, long enough to be caught at it."
        :command '("sh" "-c" "cat > /dev/null; sleep 60")
        :standard-input t
        :error-parser (lambda (_output _checker _buffer) nil)
        :modes '(text-mode))
      (unwind-protect
          (progn
            (setq buffer (find-file-noselect remote))
            (with-current-buffer buffer
              (text-mode)
              (let ((flycheck-checkers '(test-slow))
                    (flycheck-check-syntax-automatically nil))
                (flycheck-mode)
                (flycheck-buffer)
                ;; Guard the guard: there is nothing to interrupt unless a
                ;; check is running, and nothing remote to signal unless
                ;; Tramp noted the pid it started over there.
                (let ((process (flycheck-test-checker-process)))
                  (expect process :to-be-truthy)
                  (expect (process-get process 'remote-pid) :to-be-truthy))
                (spy-on 'interrupt-process :and-call-through)
                (flycheck-stop)
                (expect 'interrupt-process :to-have-been-called))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (ignore-errors (delete-file local)))))

  (it "leaves a local check to be deleted as before"
    (let* ((local (make-temp-file "flycheck-interrupt-" nil ".txt" "hello\n"))
           (buffer nil))
      (flycheck-define-command-checker 'test-slow
        "Reads standard input and then waits."
        :command '("sh" "-c" "cat > /dev/null; sleep 60")
        :standard-input t
        :error-parser (lambda (_output _checker _buffer) nil)
        :modes '(text-mode))
      (unwind-protect
          (progn
            (setq buffer (find-file-noselect local))
            (with-current-buffer buffer
              (text-mode)
              (let ((flycheck-checkers '(test-slow))
                    (flycheck-check-syntax-automatically nil))
                (flycheck-mode)
                (spy-on 'interrupt-process :and-call-through)
                (flycheck-buffer)
                ;; Guard the guard: an absence proves nothing if no check
                ;; ever ran.
                (let ((process (flycheck-test-checker-process)))
                  (expect process :to-be-truthy)
                  (flycheck-stop)
                  ;; Locally the process is Emacs's own, and deleting it is
                  ;; what has always stopped the checker.
                  (expect 'interrupt-process :not :to-have-been-called)
                  (expect (process-live-p process) :to-be nil)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (ignore-errors (delete-file local))))))

(describe "flycheck--redirect-command"

  (it "runs the command through a shell that redirects the file"
    (expect (flycheck--redirect-command '("ruff" "check" "-") "/tmp/in")
            :to-equal '("sh" "-c" "exec ruff check - < /tmp/in")))

  (it "quotes what a shell would otherwise take apart"
    ;; Asserted by running it: the arguments have to reach the command as
    ;; they were given, and the file has to be found under a name with a
    ;; space in it.
    (let* ((file (make-temp-file "flycheck-quote " nil ".txt" "from the file\n"))
           (command (flycheck--redirect-command
                     (list "sh" "-c" "printf '%s\n' \"$1\" \"$2\"; cat" "--"
                           "two words" "$(echo substituted)")
                     file)))
      (unwind-protect
          (expect (with-temp-buffer
                    (apply #'call-process (car command) nil t nil (cdr command))
                    (buffer-string))
                  :to-equal "two words\n$(echo substituted)\nfrom the file\n")
        (ignore-errors (delete-file file))))))

(provide 'test-tramp)

;;; test-tramp.el ends here
