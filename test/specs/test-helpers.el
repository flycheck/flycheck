;;; test-helpers.el --- Shared test helpers for Flycheck specs -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2025 Flycheck contributors

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

;; Shared helper functions, macros, and variables used across multiple
;; Flycheck Buttercup spec files.

;;; Code:

(require 'flycheck-buttercup)
(require 'tramp)
(require 'record-fixture)
(require 'shut-up)


;;; Directories

(defconst flycheck-test-specs-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The specs directory.")

(defconst flycheck-test-directory
  (expand-file-name "../" flycheck-test-specs-directory)
  "The test directory.")

(defconst flycheck-test-resources-directory
  (expand-file-name "resources/" flycheck-test-directory)
  "Directory of test resources.")

(defconst flycheck-test-source-directory
  (file-name-directory (directory-file-name flycheck-test-directory))
  "The source directory.")


;;; Initialize buttercup resource directory

(unless flycheck-buttercup--resource-directory
  (flycheck-buttercup-initialize flycheck-test-resources-directory))


;;; Navigation test helper macro

(defmacro flycheck-test-with-nav-buffer (minimum-level &rest body)
  "With MINIMUM-LEVEL, eval BODY in a temporary buffer for navigation.

Set `flycheck-navigation-minimum-level' to MINIMUM-LEVEL while
evaluating BODY."
  (declare (indent 1))
  `(flycheck-buttercup-with-resource-buffer "language/emacs-lisp/errors-and-warnings.el"
     (emacs-lisp-mode)
     (flycheck-mode)
     (let ((flycheck-navigation-minimum-level ,minimum-level))
       (flycheck-buttercup-buffer-sync)
       (goto-char (point-min))
       ,@body)))


;;; Fixture-driven checker specs
;;
;; A checker integration is three separable things: the command Flycheck
;; builds, the output the tool prints, and the errors Flycheck reads back
;; out of it.  Only the middle one needs the tool installed.  Recording
;; what a tool printed once (see test/record-fixture.el) lets the other
;; two be tested everywhere, so coverage stops depending on which
;; binaries happen to be on the machine.

(defun flycheck-buttercup-fixture (checker resource)
  "Return CHECKER's recorded output over RESOURCE."
  (let ((file (flycheck-record-fixture-file checker resource)))
    (unless (file-exists-p file)
      (error "No recorded output for %s over %s; record it with %s"
             checker resource "test/record-fixture.el"))
    (with-temp-buffer
      ;; Recordings hold whatever bytes the tool emitted, and some hold
      ;; UTF-8 that a machine with another locale would otherwise mangle:
      ;; pyright indents with non-breaking spaces
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (buffer-string))))

(defun flycheck-buttercup-parse (checker output &optional buffer)
  "Parse OUTPUT as CHECKER would, and return the errors.

Runs the same parser and error filter a real check runs, so this
covers everything between the tool's output and the errors Flycheck
reports, without the tool having to be installed."
  (flycheck-filter-errors
   (flycheck-parse-output output checker (or buffer (current-buffer)))
   checker))

(defun flycheck-buttercup--comparable-error (err)
  "Return ERR reduced to the fields a parse spec asserts on.

The buffer and the file name depend on where the check ran, not on
how the output was read, and are dropped."
  (let ((copy (flycheck-buttercup-error-without-group err)))
    (setf (flycheck-error-buffer copy) nil)
    (setf (flycheck-error-filename copy) nil)
    copy))

(defun flycheck-buttercup-should-parse (checker resource &rest errors)
  "Test that CHECKER reads its recorded output over RESOURCE as ERRORS.

ERRORS are given as arguments to `flycheck-error-new-at', as in
`flycheck-buttercup-should-errors'.  The tool is never run."
  (let* ((output (flycheck-buttercup-fixture checker resource))
         (parsed (flycheck-buttercup-parse checker output))
         (expected (mapcar (lambda (err)
                             (apply #'flycheck-error-new-at
                                    (append err (list :checker checker))))
                           errors)))
    (expect (mapcar #'flycheck-buttercup--comparable-error parsed)
            :to-equal
            (mapcar #'flycheck-buttercup--comparable-error expected))))

(defmacro flycheck-buttercup-def-parse-test (checker resource &rest errors)
  "Define a spec that CHECKER reads its output over RESOURCE as ERRORS.

The spec runs everywhere, because it reads recorded output instead
of running the tool."
  (declare (indent 2))
  `(it ,(format "reads %s's output over %s"
                checker (file-name-nondirectory resource))
     (flycheck-buttercup-should-parse ',checker ,resource ,@errors)))

(defun flycheck-buttercup-command (checker resource)
  "Return the command CHECKER would run over RESOURCE.

The executable is reported by name rather than resolved, so this
works whether or not the tool is installed."
  (flycheck-buttercup-with-resource-buffer resource
    (cons (flycheck-checker-default-executable checker)
          (condition-case nil
              (flycheck-checker-substituted-arguments checker)
            ;; Substitution can want a saved buffer or a project root
            (error nil)))))

;;; Python helper

(defun flycheck-buttercup-python-module-available-p (checker module)
  "Whether CHECKER's interpreter can import MODULE.

The Python checkers run their linter as `INTERPRETER -m MODULE',
so that switching Python versions switches the linter with it (see
URL `https://github.com/flycheck/flycheck/issues/1055').  Their
executable is therefore the interpreter, and finding it says
nothing about whether the linter is installed: without the module
the checker steps aside and the next one in the chain answers
instead."
  (when-let* ((python (flycheck-find-checker-executable checker)))
    (eq 0 (call-process python nil nil nil "-c" (format "import %s" module)))))


;;; C/C++ helper

(defun flycheck-buttercup-gcc-is-gnu-p ()
  "Whether the `gcc' the C/C++ checker finds is really GCC.

On macOS the name belongs to Clang, which words its diagnostics
differently and counts them differently, so expectations written
against GCC cannot hold there.  What Flycheck reads out of either
one is covered by the recorded output instead."
  (when-let* ((gcc (flycheck-find-checker-executable 'c/c++-gcc)))
    (with-temp-buffer
      (call-process gcc nil t nil "--version")
      (goto-char (point-min))
      (not (re-search-forward "clang" nil 'noerror)))))


;;; Ruby helper

(defun flycheck-buttercup-ruby-version ()
  "The version of the Ruby the `ruby' checker would use."
  (when-let* ((ruby (flycheck-find-checker-executable 'ruby)))
    (with-temp-buffer
      (when (eq 0 (call-process ruby nil t nil "-e" "print RUBY_VERSION"))
        (buffer-string)))))

(defun flycheck-buttercup-rubocop-syntax-message (message)
  "MESSAGE followed by the parser note RuboCop appends to syntax errors.

The note names the Ruby that RuboCop parsed with, which follows
`TargetRubyVersion' and so differs from machine to machine.  It is
built here rather than written out, so the spec asserts the part
RuboCop is actually reporting."
  (format "%s\n(Using Ruby %s parser; configure using \
`TargetRubyVersion` parameter, under `AllCops`)"
          message
          (let ((version (or (flycheck-buttercup-ruby-version) "")))
            ;; RuboCop names the series, not the patch level
            (if (string-match "\\`\\([0-9]+\\.[0-9]+\\)" version)
                (match-string 1 version)
              version))))


;;; Shell helper

(defun flycheck-buttercup-bash-rejects-process-substitution-p ()
  "Whether this Bash refuses process substitution in POSIX mode.

Bash 3 rejects `<(…)' under `--posix', later versions accept it,
which decides whether there is a syntax error for `sh-posix-bash'
to find at all."
  (when-let* ((bash (flycheck-find-checker-executable 'sh-posix-bash)))
    (with-temp-buffer
      (insert "cat <(echo blah)\n")
      (/= 0 (call-process-region (point-min) (point-max) bash nil nil nil
                                 "--posix" "--norc" "-n" "--")))))


;;; Erlang helper

(defun flycheck-buttercup-erlang-shows-column (mode-sym)
  "Return whether Erlang error messages contain columns.
MODE-SYM is the Erlang mode name, one of `erlang' and
`erlang-rebar3'."
  (let* ((cmd (cond ((eq mode-sym 'erlang) "erl -version")
                    ((eq mode-sym 'erlang-rebar3) "rebar3 version")
                    (t (error "Unknown erlang mode symbol"))))
         (erts-version (string-trim (shell-command-to-string cmd)))
         (version-string (car (last (split-string erts-version)))))
    (version<= "12" version-string)))


;;; Rust helper

(defun flycheck-buttercup-cargo-clean (manifest-path)
  "Call `cargo clean' on the manifest at MANIFEST-PATH.

The manifest path is relative to
`flycheck-test-resources-directory'."
  (call-process "cargo" nil nil nil "clean" "--manifest-path"
                (expand-file-name manifest-path
                                  flycheck-test-resources-directory)))


;;; Scheme helper

(defvar geiser-impl--implementation)

(defun flycheck/chicken-mode ()
  "Enable Scheme and Geiser mode for Chicken scheme."
  (interactive)
  (scheme-mode)
  (setq-local geiser-impl--implementation 'chicken)
  (geiser-mode))


;;; R helper

(defun flycheck-r-has-lintr (executable)
  "Check if R at EXECUTABLE has lintr installed."
  (when executable
    (= 0 (call-process executable nil nil nil
                        "--slave" "-e"
                        "library('lintr')"))))


;;; JavaScript modes constant

(defconst flycheck-test-javascript-modes '(js-mode
                                           js2-mode
                                           js3-mode
                                           js2-jsx-mode
                                           rjsx-mode)
  "Major modes for JavaScript tests.")

(when (version<= "25" emacs-version)
  (add-to-list 'flycheck-test-javascript-modes 'js-jsx-mode))


;;; Test variables for checker-api tests

(defvar flycheck-test-config-var nil
  "A configuration variable for testing.")

(defvar flycheck-test-option-var nil
  "An option variable for testing.")


;;; Truncated-stdin test helpers (for command-checker tests)

(define-derived-mode truncated-stdin-mode prog-mode "trunc")

(flycheck-define-command-checker 'truncated-stdin
  "Reply with an error after reading after reading 12345 bytes."
  :command `(,(or (executable-find "python3") "python")
             "-c" "import sys; sys.stdin.close(); print('stdin:1:1:error')")
  :error-patterns '((error bol "stdin" ":" line ":" column ":" (message)))
  :modes '(truncated-stdin-mode)
  :standard-input t)

(defconst flycheck-test--truncated-stdin
  (symbol-plist 'truncated-stdin)
  "Saved plist for the truncated-stdin checker.")

;; Forget about this checker, otherwise later tests that ensure that all
;; checkers are registered and documented fail
(setf (symbol-plist 'truncated-stdin) nil)


;;; Locale test helpers (for command-checker tests)

(define-derived-mode print-locale-mode prog-mode "locale")

(flycheck-define-command-checker 'print-locale
  "Report the LC_MESSAGES and LC_ALL the subprocess inherits."
  :command `(,(or (executable-find "python3") "python")
             "-c" ,(concat "import os; print('locale:1:1:LC_MESSAGES=%s LC_ALL=%s'"
                           " % (os.environ.get('LC_MESSAGES', ''),"
                           " os.environ.get('LC_ALL', '')))"))
  :error-patterns '((error bol "locale" ":" line ":" column ":" (message)))
  :modes '(print-locale-mode))

(defconst flycheck-test--print-locale
  (symbol-plist 'print-locale)
  "Saved plist for the print-locale checker.")

(setf (symbol-plist 'print-locale) nil)


;;; Suspicious-state test helpers (for command-checker tests)

(define-derived-mode suspicious-exit-mode prog-mode "suspicious")

(flycheck-define-command-checker 'suspicious-exit
  "Exit with a non-zero code without producing parsable errors."
  :command `(,(or (executable-find "python3") "python")
             "-c" "import sys; print('tool exploded'); sys.exit(1)")
  :error-patterns '((error bol "never-matches:" line ":" (message)))
  :modes '(suspicious-exit-mode))

(defconst flycheck-test--suspicious-exit
  (symbol-plist 'suspicious-exit)
  "Saved plist for the suspicious-exit checker.")

(setf (symbol-plist 'suspicious-exit) nil)

;;; Chain-crash test helpers (for the mid-chain crash tests)

(define-derived-mode chain-crash-mode prog-mode "chain-crash")

(flycheck-define-command-checker 'chain-first
  "Report one warning, then hand over to a checker that dies."
  :command `(,(or (executable-find "python3") "python")
             "-c" "print('finding:1:first checker finding')")
  :error-patterns '((warning bol "finding:" line ":" (message) eol))
  :modes '(chain-crash-mode)
  :next-checkers '(chain-killed))

(flycheck-define-command-checker 'chain-killed
  "Die by a signal instead of exiting."
  :command '("sh" "-c" "kill -9 $$")
  :error-patterns '((error bol "never-matches:" line ":" (message)))
  :modes '(chain-crash-mode))

(defconst flycheck-test--chain-first
  (symbol-plist 'chain-first)
  "Saved plist for the chain-first checker.")

(defconst flycheck-test--chain-killed
  (symbol-plist 'chain-killed)
  "Saved plist for the chain-killed checker.")

(setf (symbol-plist 'chain-first) nil)
(setf (symbol-plist 'chain-killed) nil)


;;; Excessive-errors test helpers (for error-threshold tests)

(define-derived-mode many-errors-mode prog-mode "many")

(flycheck-define-command-checker 'many-errors
  "Report six info-level and two error-level errors."
  :command `(,(or (executable-find "python3") "python")
             "-c" ,(concat
                    "print('\\n'.join("
                    "['many:%d:1:info:spam' % i for i in range(1, 7)]"
                    " + ['many:7:1:error:boom', 'many:8:1:error:bang']))"))
  :error-patterns '((info bol "many:" line ":" column ":info:" (message))
                    (error bol "many:" line ":" column ":error:" (message)))
  :modes '(many-errors-mode))

(defconst flycheck-test--many-errors
  (symbol-plist 'many-errors)
  "Saved plist for the many-errors checker.")

(setf (symbol-plist 'many-errors) nil)


;;; Slow-checker test helpers (for check-interruption tests)

(define-derived-mode slow-checker-mode prog-mode "slow")

(flycheck-define-command-checker 'slow-checker
  "Sleep for a minute before reporting an error."
  :command `(,(or (executable-find "python3") "python")
             "-c" "import time; time.sleep(60); print('slow:1:1:too late')")
  :error-patterns '((error bol "slow:" line ":" column ":" (message)))
  :modes '(slow-checker-mode))

(defconst flycheck-test--slow-checker
  (symbol-plist 'slow-checker)
  "Saved plist for the slow-checker checker.")

(setf (symbol-plist 'slow-checker) nil)


;;; Custom error level for error-level tests

(flycheck-define-error-level 'test-level
  :severity 1337
  :overlay-category 'category
  :margin-spec ">>"
  :fringe-bitmap 'left-triangle
  :fringe-face 'highlight
  :error-list-face 'font-lock-constant-face)


;;; Byte compiler declarations

(eval-when-compile
  (defvar js2-mode-show-strict-warnings)
  (defvar js2-mode-show-parse-errors)
  (defvar js3-mode-show-parse-errors)
  (defvar python-indent-guess-indent-offset))

;; Load ESS for R-mode (its autoloads are broken)
(require 'ess-site nil 'noerror)


(defconst flycheck-test-tramp-remote-prefix "/mock:localhost:"
  "TRAMP prefix of the local mock connection used by the specs.")

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

(provide 'test-helpers)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-helpers.el ends here
