;;; flymake-checkers.el --- Flymake reloaded with useful checkers

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flymake-checkers
;; Version: 0.1
;; Keywords: convenience languages tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Additional checkers for `flymake-mode'.

;; Provide checkers for:
;;
;; - Emacs Lisp
;; - Shell scripts (only with `sh-mode')
;; - Python wither either flake8, pyflakes or pylint
;; - Ruby with ruby
;; - CoffeeScript with coffeelint
;; - TeX/LaTeX with chktex

;;; Code:

(require 'flymake)

(eval-when-compile
  (require 'sh-script))

;; Customization

(defgroup flymake-checkers nil
  "Customization for flymake checkers."
  :prefix "flymake-checkers-"
  :group 'flymake)

(defcustom flymake-checkers-major-mode-checkers-alist
  '((emacs-lisp-mode . flymake-checkers-emacs-lisp)
    (python-mode . flymake-checkers-python)
    (ruby-mode . flymake-checkers-ruby)
    (coffee-mode . flymake-checkers-coffee)
    (sh-mode . flymake-checkers-sh)
    (latex-mode . flymake-checkers-tex)
    (plain-tex-mode . flymake-checkers-tex))
  "Flymake checkers for major modes.

The `car' of each cell is a major mode symbol, the `cdr' a
checker function."
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (symbol :tag "Checker"))
  :package-version '(flymake-checkers . "0.2"))

;; Utility functions

(defun flymake-checkers-create-temp-system (filename prefix)
  "Create a copy of FILENAME with PREFIX in temp directory.

Return the path of the file."
  (make-temp-file (or prefix "flymake-checkers") nil
                  (concat "." (file-name-extension filename))))


;; Flymake integration

(defvar flymake-checkers-cleanup-function nil
  "The cleanup function to use for the current checker.")
(make-variable-buffer-local 'flymake-checkers-cleanup-function)

;;;###autoload
(defun flymake-checkers-get-checker ()
  "Get the checker to use for the current mode.

This function looks the current `major-mode' up in
`flymake-checkers-major-mode-checkers-alist'.  If this is
successful, return the init function for this checker, otherwise
return nil."
  (flymake-log 3 "Searching checker for buffer %s with major mode %s"
               (current-buffer) major-mode)
  (let ((checker (assq major-mode flymake-checkers-major-mode-checkers-alist)))
    (when checker
      (flymake-log 2 "Found checker %s for buffer %s with major mode %s"
                   (cdr checker) (current-buffer) major-mode)
      (cdr checker))))

;;;###autoload
(defadvice flymake-get-init-function
  (around flymake-checkers-get-init-function first activate compile)
  "Get the flymake checker.

If `flymake-checkers-mode' is on, return the flymake-checkers checkers,
otherwise fallback to standard flymake."
  (if flymake-checkers-mode
      (setq ad-return-value (flymake-checkers-get-checker))
    (setq ad-return-value ad-do-it)))

;;;###autoload
(defun flymake-checkers-cleanup (cleanup-function)
  "Perform cleanup for flymake-checkers with CLEANUP-FUNCTION."
  (kill-local-variable 'flymake-checkers-cleanup-function)
  (kill-local-variable 'flymake-err-line-patterns)
  (if cleanup-function
      (funcall cleanup-function)
    (flymake-simple-cleanup)))

;;;###autoload
(defadvice flymake-get-cleanup-function
  (around flymake-checkers-get-cleanup-function activate compile)
  "Get the cleanup function for the current checker."
  (if flymake-checkers-mode
      (setq ad-return-value
            (apply-partially 'flymake-checkers-cleanup
                             flymake-checkers-cleanup-function))
    (setq ad-return-value ad-do-it)))


;; Entry function
;;;###autoload
(define-minor-mode flymake-checkers-mode
  "Toggle extended on-the-fly syntax checking.

Extended on-the-fly syntax checking based on flymake, but with
easier configuration and improved checkers.

Note: Pure flymake is INCOMPATIBLE with this mode."
  :init-value nil
  :lighter " FlyC"
  :require 'flymake-checkers
  (cond
   (flymake-checkers-mode
    ;; Do not bug the user
    (set (make-local-variable 'flymake-gui-warnings-enabled) nil)
    (flymake-mode 1))
   (t
    (kill-local-variable 'flymake-gui-warnings-enabled)
    (flymake-mode -1))))

;;;###autoload
(defun flymake-checkers-mode-on ()
  "Unconditionally enable `flymake-checkers-mode'."
  (flymake-checkers-mode 1))

;;;###autoload
(defun flymake-checkers-mode-off ()
  "Unconditionally disable `flymake-checkers-mode'."
  (flymake-checkers-mode -1))


;; Checker helpers

(defun flymake-checkers-substitute-argument (arg)
  "Substitute ARG with file to check is possible.

If ARG is `source' or `source-inplace', return the file to
check, otherwise return ARG unchanged."
  (let ((temp-file-function
         (cond ((eq arg 'source) 'flymake-checkers-create-temp-system)
               ((eq arg 'source-inplace) 'flymake-create-temp-inplace))))
    (if temp-file-function
        (flymake-init-create-temp-buffer-copy temp-file-function)
      arg)))

(defun flymake-checkers-get-substituted-command (properties)
  "Get the substituted command from PROPERTIES.

PROPERTIES is a property list with information about the checker.

Return the substituted command as list."
  (flymake-log 3 "Substituting command for properties %s" properties)
  (let ((command (plist-get properties :command)))
    (when command
        (mapcar 'flymake-checkers-substitute-argument command))))

(defun flymake-checkers-simple (&rest properties)
  "Declare a simple flymake checker with PROPERTIES.

Call this function in the init function of your checker to
quickly setup a simple syntax checker.

PROPERTIES is a property list providing information about the check
command in the following property:

:command The check command as list (mandatory).  In this list,
the symbol `source' is substituted with a temporary copy of the
file to check.  Use `source-inplace' instead of `source' to have
the copy created in the same directory as the source file.

:error-pattern The error pattern for parsing the output
of :command (optional). If omitted, patterns from
`flymake-err-line-patterns' are used.  Has the same for as an
element in this variable."
  ;; Enforce a standard cleanup function
  (setq flymake-checkers-cleanup-function 'flymake-simple-cleanup)
  (let ((command (flymake-checkers-get-substituted-command properties))
        (error-pattern (plist-get properties :error-pattern)))
    (when error-pattern
      (set (make-local-variable 'flymake-err-line-patterns)
           (list error-pattern)))
    (if command
        (if (executable-find (car command))
            (list (car command) (cdr command))
          (flymake-log 0 "Command %s was not found, syntax checking disabled"
                       (car command)))
      (error "Checker does not provide a command"))))


;; Checkers

;; Emacs Lisp
(defconst flymake-checkers-emacs-lisp-check-form
  '(progn
     (setq byte-compile-dest-file-function 'make-temp-file)
     (dolist (file command-line-args-left)
       (byte-compile-file file))))

(defun flymake-checkers-emacs-lisp-check-form-s ()
  "Return `flymake-checkers-emacs-lisp-check-form as string."
   (with-temp-buffer
     (print flymake-checkers-emacs-lisp-check-form (current-buffer))
     (buffer-substring-no-properties (point-min) (point-max))))

(defun flymake-checkers-emacs-lisp ()
  "Initialize flymake checking for Emacs Lisp."
  (let ((executable (concat invocation-directory invocation-name)))
    (flymake-checkers-simple
     :command `(,executable "--no-site-file" "--no-site-lisp"
                            "--batch" "--eval"
                            ,(flymake-checkers-emacs-lisp-check-form-s)
                            source))))

;; TeX/LaTeX
(defun flymake-checkers-tex ()
  "Provide a flymake checker for TeX/LaTeX."
  (flymake-checkers-simple :command '("chktex" "-v0" "-q" "-I" source-inplace)))

;; Shell script
(defconst flymake-checkers-sh-options
  '((zsh . ("-n" "-d" "-f"))            ; -n: do not execute, -d: no global rcs,
                                        ; -f: no local rcs
    (bash . ("-n" "--norc"))            ; -n: do not execute, --norc: no rc
                                        ; files
    (sh . ("-n"))                       ; -n: do not execute (as by POSIX)
    )
  "Options to pass to shells for syntax checking.")

(defun flymake-checkers-sh ()
  "Provide a flymake checker for shell scripts."
  (let ((options (cdr (assq sh-shell flymake-checkers-sh-options))))
    (when  options
      (flymake-checkers-simple
       :command `(,(symbol-name sh-shell) ,@options source)))))

;; Python
(defconst flymake-checkers-python-supported-checkers
  '((flake8 . "flake8")
    (pyflakes . "pyflakes")
    (epylint . "epylint"))
  "Supported Python checkers.

Ordered by preference for `flymake-checkers-python-find-checker'.")

(defun flymake-checkers-python-find-checker ()
  "Find an installed checker from Python.

Search for any checker from
`flymake-checkers-python-supported-checkers', and return the
symbol for the first checker found, or nil, if no checker was
found."
  (let ((found-checker nil)
        (remaining flymake-checkers-python-supported-checkers))
    (while (and (not found-checker) remaining)
      (let ((current-checker (car remaining)))
        (when (executable-find (cdr current-checker))
          (setq found-checker (car current-checker))))
      (setq remaining (cdr remaining)))
    found-checker))

(defcustom flymake-checkers-python-checker
  (flymake-checkers-python-find-checker)
  "Checker to use for Python files.

Set to `flake8', `pyflakes' or `epylint' to use the corresponding
checker for Python files.  If nil, syntax checking of Python
files is disabled."
  :group 'flymake-checkers
  :type '(choice (const :tag "Off" nil)
                 (const flake8)
                 (const pyflakes)
                 (const epylint)
                 (string :tag "Custom checker")))

(defun flymake-checkers-python-get-checker ()
  "Get the checker to use for Python.

Return a string with the executable path of the checker, or nil
if the checker was not found."
  (let ((checker flymake-checkers-python-checker))
    (when checker
      (when (symbolp checker)
        (setq checker
              (cdr (assq checker flymake-checkers-python-supported-checkers))))
      (if (stringp checker) checker
        (error "Invalid value for flymake-checkers-python-checker: %S"
               flymake-checkers-python-checker)))))

(defun flymake-checkers-python ()
  "Provide a flymake checker for Python."
  (let ((checker (flymake-checkers-python-get-checker)))
    (when checker
      (flymake-checkers-simple :command `(,checker source-inplace)))))

;; Ruby
(defun flymake-checkers-ruby ()
  "Provide a flymake checker for Ruby."
  (flymake-checkers-simple :command '("ruby" "-w" "-c" source)))

;; CoffeeScript
(defun flymake-checkers-coffee ()
  "Provide a flymake checker for CoffeeScript."
  (flymake-checkers-simple
   :command '("coffeelint" "--csv" source)
   :error-pattern '("\\(.+\\),\\([0-9]+\\),\\(?:warn\\|error\\),\\(.+\\)"
                    1 2 nil 3)))

(provide 'flymake-checkers)

;;; flymake-checkers.el ends here
