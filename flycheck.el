;;; flycheck.el --- Flymake reloaded with useful checkers

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck
;; Version: 0.4
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

;; See `flycheck-checkers' for a list of provided checkers.

;;; Code:

(require 'flymake)

(eval-when-compile
  (require 'cl)
  (require 'sh-script))

;; Customization

(defgroup flycheck nil
  "Customization for flymake checkers."
  :prefix "flycheck-"
  :group 'flymake)

(defcustom flycheck-checkers
  '(flycheck-checker-bash
    flycheck-checker-coffee
    flycheck-checker-css
    flycheck-checker-emacs-lisp
    flycheck-checker-haml
    flycheck-checker-html
    flycheck-checker-json
    flycheck-checker-javascript-jshint
    flycheck-checker-javascript-jslint
    flycheck-checker-php
    flycheck-checker-python-flake8
    flycheck-checker-python-pylint
    flycheck-checker-python-pyflakes
    flycheck-checker-ruby
    flycheck-checker-php
    flycheck-checker-sh
    flycheck-checker-tex-chktex
    flycheck-checker-tex-lacheck
    flycheck-checker-zsh)
  "Flymake checkers.

A list of flymake checkers to try for the current buffer.  A
checker is either a variable, which contains a checker definition
or a function that is called upon each syntax check to obtain the
checker definition."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker")))


;; Utility functions

(defun flycheck-create-temp-system (filename prefix)
  "Create a copy of FILENAME with PREFIX in temp directory.

Return the path of the file."
  ;; HACK: Prevent re-compression to work around a supposed bug in Emacs.
  ;; `make-temp-file' calls `write-region' to set the contents of the new
  ;; temporary file, which in turn calls `jka-compr-write-region' for compressed
  ;; files. If `jka-compr-really-do-compress' is non-nil this function uses END
  ;; even though START is a string, hence breaking the `write-region' API that
  ;; flymake relies on.  Report upstream!
  (let ((jka-compr-really-do-compress nil))
    (make-temp-file (or prefix "flycheck") nil
                    (concat "." (file-name-extension filename)))))

(defun flycheck-find-file-in-tree (filename directory)
  "Find FILENAME in DIRECTORY and all of its ancestors.

Start looking for a file named FILENAME in DIRECTORY and traverse
upwards through all of its ancestors up to the file system root
until the file is found or the root is reached.

Return the absolute path of the file, or nil if the file was not
found in DIRECTORY or any of its ancestors."
  (let ((full-path (expand-file-name filename directory)))
    (cond ((string= directory "/") (when (file-exists-p full-path) full-path))
          ((file-exists-p full-path) full-path)
          ((let ((parent-directory (file-name-directory
                                    (directory-file-name
                                     (file-name-directory full-path)))))
             (flycheck-find-file-in-tree filename parent-directory))))))

(defun flycheck-find-file-for-buffer (filename)
  "Find FILENAME for the current buffer.

First try to find the file in the buffer's directory and any of
its ancestors (see `flycheck-find-file-in-tree').  If that fails
try to find the file in the home directory.  If the file is not
found anywhere return nil."
  (let* ((directory (file-name-directory buffer-file-name))
         (filepath (flycheck-find-file-in-tree filename directory)))
    (or filepath
        (let ((home-path (expand-file-name filename)))
          (when (file-exists-p home-path) home-path)))))

;; Checker API

(defun flycheck-get-checker-properties (checker)
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

(defun flycheck-valid-checker-p (properties)
  "Check whether the checker PROPERTIES are valid.

A valid checker must have a :command, and at least one of :modes
and :predicate.

Signal an error if PROPERTIES are invalid.  Otherwise return t."
  (unless (plist-get properties :command)
    (error "Checker %S lacks :command" properties))
  (unless (or (plist-get properties :modes)
              (plist-get properties :predicate))
    (error "Checker %S lacks :modes and :predicate" properties))
  t)

(defun flycheck-check-modes (properties)
  "Check the :modes of PROPERTIES.

If PROPERTIES specifies :modes, check `major-mode' against these.
Otherwise return t."
  (let ((modes (plist-get properties :modes)))
    (or (not modes)
        (and (listp modes) (memq major-mode modes))  ; A list of modes
        (eq major-mode modes))))        ; A single mode

(defun flycheck-check-predicate (properties)
  "Check the :predicate of PROPERTIES.

If PROPERTIES contains a :predicate, eval it and return the
result, otherwise return t."
  (let ((predicate (plist-get properties :predicate)))
    (or (not predicate) (eval predicate))))

(defun flycheck-check-executable (properties)
  "Check the executable of the checker PROPERTIES.

Return t, if the executable in the :command of PROPERTIES exists,
or nil otherwise."
  (let ((executable (car (plist-get properties :command))))
    (if (executable-find executable) t
      (flymake-log 1 "Executable %s not found, not using checker %S"
                   executable properties))))

(defun flycheck-may-use-checker (properties)
  "Determine whether the checker described by PROPERTIES may be used.

Return t if so, or nil otherwise."
  (and (flycheck-valid-checker-p properties)
       (flycheck-check-modes properties)
       (flycheck-check-predicate properties)
       (flycheck-check-executable properties)))

(defun flycheck-substitute-argument (arg)
  "Substitute ARG with file to check is possible.

If ARG is `source' or `source-inplace', create a temporary file
to checker and return its path, otherwise return ARG unchanged."
  (let ((temp-file-function
         (cond ((eq arg 'source) 'flycheck-create-temp-system)
               ((eq arg 'source-inplace) 'flymake-create-temp-inplace))))
    (if temp-file-function
        (flymake-init-create-temp-buffer-copy temp-file-function)
      arg)))

(defun flycheck-get-substituted-command (properties)
  "Get the substitute :command from PROPERTIES."
  (mapcar 'flycheck-substitute-argument
          (plist-get properties :command)))

(defun flycheck-error-pattern-p (pattern)
  "Check whether PATTERN is a valid error pattern."
  (and
   (listp pattern)                      ; A pattern must be a list...
   (= (length pattern) 5)               ; ...of length 5...
   (stringp (car pattern))              ; ...whose first element is a string
   ))

(defun flycheck-error-patterns-list-p (patterns)
  "Check whether PATTERNS is a list of valid error patterns."
  (let ((result nil))
    (dolist (pattern patterns result)
      (setq result (flycheck-error-pattern-p pattern))
      (unless result (return)))))

(defun flycheck-get-error-patterns (properties)
  "Get the error patterns from PROPERTIES.

PROPERTIES is a property list with information about the checker.

Return a list of error patterns compatible with
`flymake-err-line-patterns'."
  (flymake-log 3 "Extracting error patterns from properties %s" properties)
  (let ((patterns (plist-get properties :error-patterns)))
    (when patterns
      (cond
       ;; A single pattern was given, wrap it up in a list
       ((flycheck-error-pattern-p patterns) (list patterns))
       ;; A list of patterns
       ((flycheck-error-patterns-list-p patterns) patterns)
       (t (error "Invalid type for :error-patterns: %S" patterns))))))


;; Flymake hacking

(defvar flycheck-cleanup-function nil
  "The cleanup function to use for the current checker.")
(make-variable-buffer-local 'flycheck-cleanup-function)

(defun flycheck-init-checker (properties)
  "Initialize the checker described by PROPERTIES.

Setup buffer local flymake variables based on PROPERTIES, and
return a command list for flymake."
  (let ((command (flycheck-get-substituted-command properties))
        (error-patterns (flycheck-get-error-patterns properties)))
    (setq flycheck-cleanup-function 'flymake-simple-cleanup)
    (when error-patterns
      (set (make-local-variable 'flymake-err-line-patterns) error-patterns))
    (list (car command) (cdr command))))

;;;###autoload
(defun flycheck-init ()
  "Wrap checker PROPERTIES into an init function.

PROPERTIES is the properties list describing a checker.

Use this function `apply-partially' to construct a real init
function for flymake."
  (dolist (checker flycheck-checkers)
    (let ((properties (flycheck-get-checker-properties checker)))
      (flymake-log 3 "Trying checker %S with properties %S" checker properties)
      (when (flycheck-may-use-checker properties)
        (return (flycheck-init-checker properties))))))

;;;###autoload
(defadvice flymake-get-init-function
  (around flycheck-get-init-function first activate compile)
  "Get the flymake checker.

Return `flycheck-init-function', if variable `flycheck-mode' is enabled."
  (setq ad-return-value (if flycheck-mode
                            'flycheck-init
                          ad-do-it)))

;;;###autoload
(defun flycheck-cleanup ()
  "Perform cleanup for flycheck."
  (when flycheck-cleanup-function
    (funcall flycheck-cleanup-function))
  (kill-local-variable 'flycheck-cleanup-function)
  (kill-local-variable 'flymake-err-line-patterns))

;;;###autoload
(defadvice flymake-get-cleanup-function
  (around flycheck-get-cleanup-function activate compile)
  "Get the cleanup function for the current checker."
  (setq ad-return-value (if flycheck-mode
                            'flycheck-cleanup
                          ad-do-it)))

;;;###autoload
(defadvice flymake-mode (around flycheck-flymake-mode activate compile)
  "Variable `flymake-mode' is incompatible with variable `flycheck-mode'.
Signal an error if the latter is active."
  (if flycheck-mode
      (error "Flymake-mode is incompatible with flycheck-mode.  \
Use either flymake-mode or flycheck-mode")
    (setq ad-return-value ad-do-it)))

(defvar flycheck-mode-line nil
  "The mode line lighter of variable `flycheck-mode'.")

(defadvice flymake-report-status
  (around flycheck-report-status (e-w &optional status) activate compile)
  "Update the status of variable `flycheck-mode'."
  (let ((mode-line (if flycheck-mode " FlyC" " Flymake"))
        (target (if flycheck-mode
                    'flycheck-mode-line
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
  (around flycheck-report-fatal-status (status warning) activate compile)
  "Ignore fatal status warnings in variable `flycheck-mode'."
  (if flycheck-mode
      (flymake-log 0 "Fatal status %s, warning %s in flycheck-mode \
buffer %s" status warning (buffer-name))
    (flymake-report-status nil (format "!%s" status))
    (setq ad-return-value ad-do-it)))

;; Entry function

;;;###autoload
(define-minor-mode flycheck-mode
  "Toggle extended on-the-fly syntax checking.

Extended on-the-fly syntax checking based on flymake, but with
easier configuration and improved checkers.

`flycheck-mode' is incompatible with `flymake-mode'.
Signal an error if the latter is active.  Note: Pure flymake is
INCOMPATIBLE with this mode."
  :init-value nil
  :lighter flycheck-mode-line
  :require 'flycheck
  (when flymake-mode
    (error "Flycheck-mode is incompatible with flymake-mode.  \
Use either flymake-mode or flycheck-mode"))
  (cond
   (flycheck-mode
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

    (flymake-delete-own-overlays)

    (when flymake-timer
      (cancel-timer flymake-timer)
      (setq flymake-timer nil))

    (setq flymake-is-running nil))))

;;;###autoload
(defun flycheck-mode-on ()
  "Unconditionally enable variable `flycheck-mode'."
  (flycheck-mode 1))

;;;###autoload
(defun flycheck-mode-off ()
  "Unconditionally disable variable `flycheck-mode'."
  (flycheck-mode -1))


;; Checkers

(defvar flycheck-checker-bash
  '(:command
    ("bash"  "-n" "--norc" source)
    :modes sh-mode
    :predicate (eq sh-shell 'bash)))

(defvar flycheck-checker-coffee
  '(:command
    ("coffeelint" "--csv" source)
    :error-patterns
    (("SyntaxError: \\(.*\\) on line \\([0-9]+\\)" nil 2 nil 1)
     ("\\(.+\\),\\([0-9]+\\),\\(?:warn\\|error\\),\\(.+\\)" 1 2 nil 3))
    :modes coffee-mode))

(defvar flycheck-checker-css
  '(:command
    ("csslint" "--format=compact" source)
    :error-patterns
    ("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$" 1 2 3 4)
    :modes css-mode))

(defconst flycheck-checker-emacs-lisp-check-form
  '(progn
     (setq byte-compile-dest-file-function 'make-temp-file)
     (dolist (file command-line-args-left)
       (byte-compile-file file))))

(defun flycheck-checker-emacs-lisp-check-form-s ()
  "Return `flycheck-checker-emacs-lisp-check-form as string."
   (with-temp-buffer
     (print flycheck-checker-emacs-lisp-check-form (current-buffer))
     (buffer-substring-no-properties (point-min) (point-max))))

(defvar flycheck-checker-emacs-lisp
  (let ((executable (concat invocation-directory invocation-name))
        (check-form-s (flycheck-checker-emacs-lisp-check-form-s)))
    `(:command
      (,executable "--no-site-file" "--no-site-lisp" "--batch" "--eval"
                   ,check-form-s source)
      :modes emacs-lisp-mode)))

(defvar flycheck-checker-haml
  '(:command
    ("haml" "-c" source)
    :error-patterns
    ("^Syntax error on line \\([0-9]+\\): \\(.*\\)$" nil 1 nil 2)
    :modes haml-mode))

(defvar flycheck-checker-html
  '(:command
    ("tidy" "-e" "-q" source)
    :error-patterns
    (("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)" nil 1 2 4))
    :modes html-mode))

(defvar flycheck-jshintrc nil
  "The path to .jshintrc.

This variable denotes the configuration file for jshint to use
when checking a buffer with jshint.

The path contained in this variable is expanded via
`expand-file-name' before being passed to jshint, thus ~ is
replaced with your $HOME directory.

Use this variable as file local variable to use a specific
configuration file for a buffer.")
(put 'flycheck-jshintrc 'safe-local-variable 'stringp)

(defun flycheck-checker-javascript-jshint ()
  "Check javascript with jshint.

Use .jshintrc from either `flycheck-jshintrc' or – if that
variable is nil – the buffer's directory, any ancestors thereof
or the $HOME directory.

If .jshintrc is not found run jshint with default settings."
  (let ((jshintrc (or flycheck-jshintrc
                      (flycheck-find-file-for-buffer ".jshintrc"))))
    `(:command
      ("jshint"
       ,@(when jshintrc `("--config" ,(expand-file-name jshintrc)))
       source)
      :error-patterns
      (("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$" 1 2 3 4))
      :modes js-mode)))

(defvar flycheck-checker-javascript-jslint
  '(:command
    ("jsl" "-process" source)
    :error-patterns
    (("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" nil 2 nil 3)
     ("^\\(.+\\)(\\([0-9]+\\)): \\(SyntaxError:.+\\)$" nil 2 nil 3)
     ("^\\(.+\\)(\\([0-9]+\\)): \\(lint \\)?\\(warning:.+\\)$" nil 2 nil 4)
     ("^\\(.+\\)\:\\([0-9]+\\)\: strict \\(warning: trailing comma.+\\)\:$" nil 2 nil 3))
    :modes js-mode))

(defvar flycheck-checker-json
  '(:command
    ("jsonlint" "-c" "-q" source)
    :error-patterns
    (("^\\(.+\\)\: line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)$" nil 2 3 4))
    :predicate
    (or (eq major-mode 'json-mode)
        (and buffer-file-name
             (string= "json" (file-name-extension buffer-file-name))))))

(defvar flycheck-checker-php
  '(:command
    ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
      "-d" "log_errors=0" source)
    :error-patterns ("\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"
                      2 3 nil 1)
    :modes php-mode))

(defvar flycheck-checker-python-flake8
  '(:command ("flake8" source-inplace) :modes python-mode))

(defvar flycheck-checker-python-pylint
  '(:command ("epylint" source-inplace) :modes python-mode))

(defvar flycheck-checker-python-pyflakes
  '(:command ("pyflakes" source-inplace) :modes python-mode))

(defvar flycheck-checker-ruby
  '(:command ("ruby" "-w" "-c" source) :modes ruby-mode))

(defvar flycheck-checker-sh
  '(:command
    ("sh" "-n" source)
    :modes sh-mode
    :predicate (eq sh-shell 'sh)))

(defvar flycheck-checker-tex-chktex
  '(:command
    ("chktex" "-v0" "-q" "-I" source-inplace)
    :modes (latex-mode plain-tex-mode)))

(defvar flycheck-checker-tex-lacheck
  '(:command
    ("lacheck" source-inplace)
    :modes latex-mode))

(defvar flycheck-checker-zsh
  '(:command
    ("zsh" "-n" "-d" "-f" source)
    :modes sh-mode
    :predicate (eq sh-shell 'zsh)))

(provide 'flycheck)

;;; flycheck.el ends here
