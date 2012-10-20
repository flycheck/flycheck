;;; flymake-checkers.el --- Flymake reloaded with useful checkers

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flymake-checkers
;; Version: 0.3
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

;; A replacement for `flymake-mode' with much improved configuration and checker
;; API.

;; Like `flymake-mode', `flycheck-mode' performs on-the-fly syntax and style
;; checkers in various modes.  However it features an improved configuration API
;; based on major modes (instead of file name masks) and a much nicer and easier
;; declarative syntax for checker definitions.

;; `flycheck-mode' is intentionally incompatible to `flymake-mode'.  Attempting
;; to enable one while the other is active causes an error.

;; `flycheck-mode' provides syntax and style checkers for the following
;; languages:
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
  (require 'cl)
  (require 'sh-script))

;; Customization

(defgroup flymake-checkers nil
  "Customization for flymake checkers."
  :prefix "flymake-checkers-"
  :group 'flymake)

(defcustom flymake-checkers-checkers
  '(flymake-checkers-coffee
    flymake-checkers-emacs-lisp
    flymake-checkers-php
    flymake-checkers-python-flake8
    flymake-checkers-python-pylint
    flymake-checkers-python-pyflakes
    flymake-checkers-ruby
    flymake-checkers-php
    flymake-checkers-sh
    flymake-checkers-sh-bash
    flymake-checkers-sh-zsh
    flymake-checkers-tex)
  "Flymake checkers.

A list of flymake checkers to try for the current buffer.  A
checker is either a variable, which contains a checker definition
or a function that is called upon each syntax check to obtain the
checker definition."
  :group 'flymake-checkers
  :type '(repeat (symbol :tag "Checker")))


;; Utility functions

(defun flymake-checkers-create-temp-system (filename prefix)
  "Create a copy of FILENAME with PREFIX in temp directory.

Return the path of the file."
  (make-temp-file (or prefix "flymake-checkers") nil
                  (concat "." (file-name-extension filename))))


;; Checker API

(defun flymake-checkers-get-checker-properties (checker)
  "Get the properties of CHECKER.

CHECKER is a symbol pointing either to a bound variable or to a
function.  In the former case, the `symbol-value' is returned, in
the latter case the return value of the function being invoked
with no arguments.

If CHECKER is a unbound symbol, or not a symbol at all, an error
is signaled."
  (cond
   ((and (symbolp checker) (boundp checker)) (symbol-value checker))
   ((and (symbolp checker) (functionp checker)) (funcall checker))
   (t (error "Invalid checker, expected variable or function, but was: %S"
             checker))))

(defun flymake-checkers-valid-checker-p (properties)
  "Check whether the checker PROPERTIES are valid.

A valid checker must have a :command, and at least one of :modes
and :predicate.

Signal an error if PROPERTIES are invalid.  Otherwise return t."
  (unless (plist-get properties :command)
    (error "Checker %S lacks :command" properties))
  (unless (or (plist-get properties :modes)
              (plist-get properties :predicate))
    (error "Checker %S lacks :modes and :predicate." properties))
  t)

(defun flymake-checkers-check-modes (properties)
  "Check the :modes of PROPERTIES.

If PROPERTIES specifies :modes, check `major-mode' against these.
Otherwise return t."
  (let ((modes (plist-get properties :modes)))
    (or (not modes)
        (and (listp modes) (memq major-mode modes))  ; A list of modes
        (eq major-mode modes))))        ; A single mode

(defun flymake-checkers-check-predicate (properties)
  "Check the :predicate of PROPERTIES.

If PROPERTIES contains a :predicate, eval it and return the
result, otherwise return t."
  (let ((predicate (plist-get properties :predicate)))
    (or (not predicate) (eval predicate))))

(defun flymake-checkers-check-executable (properties)
  "Check the executable of the checker PROPERTIES.

Return t, if the executable in the :command of PROPERTIES exists,
or nil otherwise."
  (let ((executable (car (plist-get properties :command))))
    (if (executable-find executable) t
      (flymake-log 1 "Executable %s not found, not using checker %S"
                   executable properties))))

(defun flymake-checkers-may-use-checker (properties)
  "Determine whether the checker described by PROPERTIES may be used.

Return t if so, or nil otherwise."
  (and (flymake-checkers-valid-checker-p properties)
       (flymake-checkers-check-modes properties)
       (flymake-checkers-check-predicate properties)
       (flymake-checkers-check-executable properties)))

(defun flymake-checkers-substitute-argument (arg)
  "Substitute ARG with file to check is possible.

If ARG is `source' or `source-inplace', create a temporary file
to checker and return its path, otherwise return ARG unchanged."
  (let ((temp-file-function
         (cond ((eq arg 'source) 'flymake-checkers-create-temp-system)
               ((eq arg 'source-inplace) 'flymake-create-temp-inplace))))
    (if temp-file-function
        (flymake-init-create-temp-buffer-copy temp-file-function)
      arg)))

(defun flymake-checkers-get-substituted-command (properties)
  "Get the substitute :command from PROPERTIES."
  (mapcar 'flymake-checkers-substitute-argument
          (plist-get properties :command)))

(defun flymake-checkers-error-pattern-p (pattern)
  "Check whether PATTERN is a valid error pattern."
  (and
   (listp pattern)                      ; A pattern must be a list...
   (= (length pattern) 5)               ; ...of length 5...
   (stringp (car pattern))              ; ...whose first element is a string
   ))

(defun flymake-checkers-error-patterns-list-p (patterns)
  "Check whether PATTERNS is a list of valid error patterns."
  (let ((result nil))
    (dolist (pattern patterns result)
      (setq result (flymake-checkers-error-pattern-p pattern))
      (unless result (return)))))

(defun flymake-checkers-get-error-patterns (properties)
  "Get the error patterns from PROPERTIES.

PROPERTIES is a property list with information about the checker.

Return a list of error patterns compatible with
`flymake-err-line-patterns'."
  (flymake-log 3 "Extracting error patterns from properties %s" properties)
  (let ((patterns (plist-get properties :error-patterns)))
    (when patterns
      (cond
       ;; A single pattern was given, wrap it up in a list
       ((flymake-checkers-error-pattern-p patterns) (list patterns))
       ;; A list of patterns
       ((flymake-checkers-error-patterns-list-p patterns) patterns)
       (t (error "Invalid type for :error-patterns: %S" patterns))))))


;; Flymake hacking

(defvar flymake-checkers-cleanup-function nil
  "The cleanup function to use for the current checker.")
(make-variable-buffer-local 'flymake-checkers-cleanup-function)

(defun flymake-checkers-init-checker (properties)
  "Initialize the checker described by PROPERTIES.

Setup buffer local flymake variables based on PROPERTIES, and
return a command list for flymake."
  (let ((command (flymake-checkers-get-substituted-command properties))
        (error-patterns (flymake-checkers-get-error-patterns properties)))
    (setq flymake-checkers-cleanup-function 'flymake-simple-cleanup)
    (when error-patterns
      (set (make-local-variable 'flymake-err-line-patterns) error-patterns))
    (list (car command) (cdr command))))

;;;###autoload
(defun flymake-checkers-init ()
  "Wrap checker PROPERTIES into an init function.

PROPERTIES is the properties list describing a checker.

Use this function `apply-partially' to construct a real init
function for flymake."
  (dolist (checker flymake-checkers-checkers)
    (let ((properties (flymake-checkers-get-checker-properties checker)))
      (flymake-log 3 "Trying checker %S with properties %S" checker properties)
      (when (flymake-checkers-may-use-checker properties)
        (return (flymake-checkers-init-checker properties))))))

;;;###autoload
(defadvice flymake-get-init-function
  (around flymake-checkers-get-init-function first activate compile)
  "Get the flymake checker.

Return `flymake-checkers-init-function', if `flymake-checkers-mode' is enabled."
  (setq ad-return-value (if flymake-checkers-mode
                            'flymake-checkers-init
                          ad-do-it)))

;;;###autoload
(defun flymake-checkers-cleanup ()
  "Perform cleanup for flymake-checkers."
  (when flymake-checkers-cleanup-function
    (funcall flymake-checkers-cleanup-function))
  (kill-local-variable 'flymake-checkers-cleanup-function)
  (kill-local-variable 'flymake-err-line-patterns))

;;;###autoload
(defadvice flymake-get-cleanup-function
  (around flymake-checkers-get-cleanup-function activate compile)
  "Get the cleanup function for the current checker."
  (setq ad-return-value (if flymake-checkers-mode
                            'flymake-checkers-cleanup
                          ad-do-it)))

;;;###autoload
(defadvice flymake-mode (around flymake-checkers-flymake-mode activate compile)
  "`flymake-mode' is incompatible with `flymake-checkers-mode'.
Signal an error if the latter is active."
  (if flymake-checkers-mode
      (error ("flymake-mode is incompatible with flymake-checkers-mode. \
Use either flymake-mode or flymake-checkers-mode"))
    (setq ad-return-value ad-do-it)))

(defvar flymake-checkers-mode-line nil
  "The mode line lighter of flymake-checkers-mode.")

(defadvice flymake-report-status
  (around flymake-checkers-report-status activate compile)
  "Update the status of flymake-checkers-mode."
  (let ((e-w (ad-get-arg 0))
        (status (ad-get-arg 1))
        (mode-line (if flymake-checkers-mode " FlyC" " Flymake"))
        (target (if flymake-checkers-mode
                    'flymake-checkers-mode-line
                  'flymake-mode-line)))
    (when e-w
      (setq flymake-mode-line-e-w e-w))
    (when status
      (setq flymake-mode-line-status status))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (format "%s:%s" mode-line flymake-mode-line-e-w)))
    (set target (concat mode-line flymake-mode-line-status))
    (force-mode-line-update)))

(defadvice flymake-report-fatal-status
  (around flymake-checkers-fatal-status activate compile)
  "Ignore fatal status warnings in flymake-checkers mode."
  (if flymake-checkers-mode
      (let ((status (ad-get-arg 0))
            (warning (ad-get-arg 1)))
        (flymake-log 0 "Fatal status %s, warning %s in flymake-checkers-mode \
buffer %s" status warning (buffer-name))
        (flymake-report-status nil (format "!%s" status)))
    (setq ad-return-value ad-do-it)))

;; Entry function

;;;###autoload
(define-minor-mode flymake-checkers-mode
  "Toggle extended on-the-fly syntax checking.

Extended on-the-fly syntax checking based on flymake, but with
easier configuration and improved checkers.

`flymake-checkers-mode' is incompatible with `flymake-mode'.
Signal an error if the latter is active.  Note: Pure flymake is
INCOMPATIBLE with this mode."
  :init-value nil
  :lighter flymake-checkers-mode-line
  :require 'flymake-checkers
  (when flymake-mode
    (error "flymake-checkers-mode is incompatible with flymake-mode. \
Use either flymake-mode or flymake-checkers-mode"))
  (cond
   (flymake-checkers-mode
    ;; Enable flymake-cursor if available
    (when (fboundp 'flyc/show-fly-error-at-point-pretty-soon)
      (add-hook 'post-command-hook
                'flyc/show-fly-error-at-point-pretty-soon
                nil t))

    (add-hook 'after-change-functions 'flymake-after-change-function nil t)
    (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
    (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t)

    (flymake-report-status "" "")
    (setq flymake-timer
          (run-at-time nil 1 'flymake-on-timer-event (current-buffer)))

    (flymake-start-syntax-check))
   (t
    (remove-hook 'after-change-functions 'flymake-after-change-function t)
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function flymake-find-file-hook) t)

    (flymake-delete-own-overlays)

    (when flymake-timer
      (cancel-timer flymake-timer)
      (setq flymake-timer nil))

    (setq flymake-is-running nil))))

;;;###autoload
(defun flymake-checkers-mode-on ()
  "Unconditionally enable `flymake-checkers-mode'."
  (flymake-checkers-mode 1))

;;;###autoload
(defun flymake-checkers-mode-off ()
  "Unconditionally disable `flymake-checkers-mode'."
  (flymake-checkers-mode -1))


;; Checkers

(defvar flymake-checkers-coffee
  '(:command
    '("coffeelint" "--csv" source)
    :error-patterns
    (("SyntaxError: \\(.*\\) on line \\([0-9]+\\)" nil 2 nil 1)
     ("\\(.+\\),\\([0-9]+\\),\\(?:warn\\|error\\),\\(.+\\)" 1 2 nil 3))
    :modes coffee-mode))

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

(defvar flymake-checkers-emacs-lisp
  (let ((executable (concat invocation-directory invocation-name))
        (check-form-s (flymake-checkers-emacs-lisp-check-form-s)))
    `(:command
      (,executable "--no-site-file" "--no-site-lisp" "--batch" "--eval"
                   ,check-form-s source)
      :modes emacs-lisp-mode)))

(defvar flymake-checkers-php
  '(:command
    '("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
      "-d" "log_errors=0" source)
    :error-patterns ("\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \
\\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"
                      2 3 nil 1)
    :modes php-mode))

(defvar flymake-checkers-python-flake8
  '(:command ("flake8" source-inplace) :modes python-mode))

(defvar flymake-checkers-python-pylint
  '(:command ("epylint" source-inplace) :modes python-mode))

(defvar flymake-checkers-python-pyflakes
  '(:command ("pyflakes" source-inplace) :modes python-mode))

(defvar flymake-checkers-ruby
  '(:command '("ruby" "-w" "-c" source) :modes ruby-mode))

(defvar flymake-checkers-sh
  '(:command
    ("sh" "-n" source)
    :modes sh-mode
    :predicate (eq sh-shell 'sh)))

(defvar flymake-checkers-sh-zsh
  '(:command
    ("zsh" "-n" "-d" "-f" source)
    :modes sh-mode
    :predicate (eq sh-shell 'zsh)))

(defvar flymake-checkers-sh-bash
  '(:command
    ("bash"  "-n" "--norc" source)
    :modes sh-mode
    :predicate (eq sh-shell 'bash)))

(defvar flymake-checkers-tex
  '(:command
    ("chktex" "-v0" "-q" "-I" source-inplace)
    :modes (latex-mode plain-tex-mode)))

(provide 'flymake-checkers)

;;; flymake-checkers.el ends here
