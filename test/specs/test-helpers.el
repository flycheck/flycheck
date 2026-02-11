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
(require 'shut-up)


;;; Directories

(defconst flycheck-test-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The specs directory.")

(defconst flycheck-test-resources-directory
  (expand-file-name "resources/" (expand-file-name "../" flycheck-test-directory))
  "Directory of test resources.")

(defconst flycheck-test-source-directory
  (file-name-directory (directory-file-name
                        (expand-file-name "../" flycheck-test-directory)))
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


(provide 'test-helpers)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-helpers.el ends here
