;;; flycheck.el --- Modern on-the-fly syntax checking for GNU Emacs -*- lexical-binding: t; -*-

;; Copyright (c) 2012, 2013, 2014 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://flycheck.readthedocs.org
;; Keywords: convenience languages tools
;; Version: 0.22-cvs
;; Package-Requires: ((dash "2.4.0") (pkg-info "0.4") (cl-lib "0.3") (emacs "24.1"))

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

;; Modern on-the-fly syntax checking for GNU Emacs.

;; Provide `flycheck-mode' which enables on-the-fly syntax checking for a large
;; number of different modes and languages (see `flycheck-checkers' for a
;; complete list).
;;
;; Support for new modes and languages can be added by defining a new syntax
;; checker (see `flycheck-define-checker').

;;; Code:

(eval-when-compile
  (require 'cl-lib)         ; `cl-defstruct'
  (require 'compile)        ; Compile Mode integration
  (require 'package)        ; Tell Emacs about package-user-dir
  (require 'sh-script)      ; `sh-shell' for sh checker predicates
  (require 'jka-compr)      ; For JKA workarounds in `flycheck-temp-file-system'
  )

(require 'dash)

(require 'subr-x nil 'no-error)  ; Additional utilities, Emacs 24.4 and upwards
(require 'pcase)                 ; Pattern matching for Emacs Lisp
(require 'tabulated-list)        ; To list errors
(require 'easymenu)             ; Flycheck Mode menu definition
(require 'rx)                    ; Regexp fanciness in `flycheck-define-checker'
(require 'help-mode)             ; `define-button-type'
(require 'find-func)             ; `find-function-regexp-alist'

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))


;;; Compatibility
(eval-and-compile
  ;; `defvar-local' Emacs 24.2 and below
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  (unless (fboundp 'setq-local)
    ;; `setq-local' for Emacs 24.2 and below
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val)))

  (unless (fboundp 'user-error)
    ;; `user-error' for Emacs 24.2 and below
    (defalias 'user-error 'error)

    (add-to-list 'debug-ignored-errors
                 (rx string-start "No more Flycheck errors" string-end))
    (add-to-list 'debug-ignored-errors
                 (rx string-start "Flycheck mode disabled" string-end))
    (add-to-list 'debug-ignored-errors
                 (rx string-start "Configured syntax checker "
                     symbol-start (one-or-more not-newline) symbol-end
                     " cannot be used" string-end)))

  (unless (fboundp 'string-suffix-p)
    ;; `string-suffix-p' for Emacs 24.3 and below
    (defun string-suffix-p (suffix string  &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case))))))

  (unless (featurep 'subr-x)
    ;; `subr-x' function for Emacs 24.3 and below
    (defsubst string-join (strings &optional separator)
      "Join all STRINGS using SEPARATOR."
      (mapconcat 'identity strings separator))

    (defsubst string-trim-left (string)
      "Remove leading whitespace from STRING."
      (if (string-match "\\`[ \t\n\r]+" string)
          (replace-match "" t t string)
        string))

    (defsubst string-trim-right (string)
      "Remove trailing whitespace from STRING."
      (if (string-match "[ \t\n\r]+\\'" string)
          (replace-match "" t t string)
        string))

    (defsubst string-trim (string)
      "Remove leading and trailing whitespace from STRING."
      (string-trim-left (string-trim-right string)))

    (defsubst string-empty-p (string)
      "Check whether STRING is empty."
      (string= string ""))))

(when (and (not (get 'exclamation-mark 'fringe))
           (fboundp 'define-fringe-bitmap))
  ;; Provide `exclamation-mark' bitmap for Emacs 24.2 and below.  We also check,
  ;; whether `define-fringe-bitmap' is defined, because this function is not
  ;; available if Emacs is built without GUI support.  See
  ;; https://github.com/flycheck/flycheck/issues/57
  (define-fringe-bitmap 'exclamation-mark
    [24 60 60 24 24 0 0 24 24] nil nil 'center))


;;; Customization
(defgroup flycheck nil
  "Modern on-the-fly syntax checking for GNU Emacs."
  :prefix "flycheck-"
  :group 'tools
  :link '(url-link :tag "Online manual" "http://flycheck.readthedocs.org")
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck")
  :link '(custom-manual "(flycheck)Top")
  :link '(info-link "(flycheck)Usage"))

(defgroup flycheck-config-files nil
  "Configuration files for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck
  :link '(custom-manual "(flycheck)Syntax checker configuration files"))

(defgroup flycheck-options nil
  "Options for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck
  :link '(custom-manual "(flycheck)Syntax checker options"))

(defgroup flycheck-executables nil
  "Executables of syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck
  :link '(custom-manual "(flycheck)Syntax checker executables"))

(defgroup flycheck-faces nil
  "Faces used by on-the-fly syntax checking."
  :prefix "flycheck-"
  :group 'flycheck
  :link '(info-link "(flycheck)Error reporting"))

(defcustom flycheck-checkers
  '(ada-gnat
    asciidoc
    c/c++-clang
    c/c++-gcc
    c/c++-cppcheck
    cfengine
    chef-foodcritic
    coffee
    coffee-coffeelint
    coq
    css-csslint
    d-dmd
    elixir
    emacs-lisp
    emacs-lisp-checkdoc
    erlang
    eruby-erubis
    fortran-gfortran
    go-gofmt
    go-golint
    go-vet
    go-build
    go-test
    go-errcheck
    haml
    handlebars
    haskell-ghc
    haskell-hlint
    html-tidy
    javascript-jshint
    javascript-eslint
    javascript-gjslint
    json-jsonlint
    less
    lua
    make
    perl
    perl-perlcritic
    php
    php-phpmd
    php-phpcs
    puppet-parser
    puppet-lint
    python-flake8
    python-pylint
    racket
    rpm-rpmlint
    rst
    rst-sphinx
    ruby-rubocop
    ruby-rubylint
    ruby
    ruby-jruby
    rust
    sass
    scala
    scala-scalastyle
    scss
    sh-bash
    sh-posix-dash
    sh-posix-bash
    sh-zsh
    sh-shellcheck
    slim
    tex-chktex
    tex-lacheck
    texinfo
    verilog-verilator
    xml-xmlstarlet
    xml-xmllint
    yaml-jsyaml
    yaml-ruby)
  "Syntax checkers available for automatic selection.

A list of Flycheck syntax checkers to choose from when syntax
checking a buffer.  Flycheck will automatically select a suitable
syntax checker from this list, unless `flycheck-checker' is set,
either directly or with `flycheck-select-checker'.

You should not need to change this variable normally.  In order
to disable syntax checkers, please use
`flycheck-disabled-checkers'.  This variable is intended for 3rd
party extensions to tell Flycheck about new syntax checkers.

Syntax checkers in this list must be defined with
`flycheck-define-checker'."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker"))
  :risky t)

(defcustom flycheck-disabled-checkers nil
  "Syntax checkers excluded from automatic selection.

A list of Flycheck syntax checkers to exclude from automatic
selection.  Flycheck will never automatically select a syntax
checker in this list, regardless of the value of
`flycheck-checkers'.

However, syntax checkers in this list are still available for
manual selection with `flycheck-select-checker'.

Use this variable to disable syntax checkers, instead of removing
the syntax checkers from `flycheck-checkers'.  You may also use
this option as a file or directory local variable to disable
specific checkers in individual files and directories
respectively."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker"))
  :package-version '(flycheck . "0.16")
  :safe #'flycheck-symbol-list-p)
(make-variable-buffer-local 'flycheck-disabled-checkers)

(defvar-local flycheck-checker nil
  "Syntax checker to use for the current buffer.

If unset or nil, automatically select a suitable syntax checker
from `flycheck-checkers' on every syntax check.

If set to a syntax checker only use this syntax checker and never
select one from `flycheck-checkers' automatically.  The syntax
checker is used regardless of whether it is contained in
`flycheck-checkers' or `flycheck-disabled-checkers'.  If the
syntax checker is unusable in the current buffer an error is
signaled.

A syntax checker assigned to this variable must be defined with
`flycheck-define-checker'.

Use the command `flycheck-select-checker' to select a syntax
checker for the current buffer, or set this variable as file
local variable to always use a specific syntax checker for a
file.  See Info Node `(emacs)Specifying File Variables' for more
information about file variables.")
(put 'flycheck-checker 'safe-local-variable 'flycheck-registered-checker-p)

(defcustom flycheck-locate-config-file-functions
  '(flycheck-locate-config-file-absolute-path
    flycheck-locate-config-file-ancestor-directories
    flycheck-locate-config-file-home)
  "Functions to locate syntax checker configuration files.

Each function in this hook must accept two arguments: The value
of the configuration file variable, and the syntax checker
symbol.  It must return either a string with an absolute path to
the configuration file, or nil, if it cannot locate the
configuration file.

The functions in this hook are called in order of appearance, until a
function returns non-nil.  The configuration file returned by that
function is then given to the syntax checker if it exists.

This variable is an abnormal hook.  See Info
node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-checker-error-threshold 400
  "Maximum errors allowed per syntax checker.

The value of this variable is either an integer denoting the
maximum number of errors per syntax checker and buffer, or nil to
not limit the errors reported from a syntax checker.

If this variable is a number and a syntax checker reports more
errors than the value of this variable, its errors are not
discarded, and not highlighted in the buffer or available in the
error list.  The affected syntax checker is also disabled for
future syntax checks of the buffer."
  :group 'flycheck
  :type '(choice (const :tag "Do not limit reported errors" nil)
                 (integer :tag "Maximum number of errors"))
  :risky t
  :package-version '(flycheck . "0.22"))

(defcustom flycheck-process-error-functions '(flycheck-add-overlay)
  "Functions to process errors.

Each function in this hook must accept a single argument: A
Flycheck error to process.

All functions in this hook are called in order of appearance,
until a function returns non-nil.  Thus, a function in this hook
may return nil, to allow for further processing of the error, or
any non-nil value, to indicate that the error was fully processed
and inhibit any further processing.

The functions are called for each newly parsed error immediately
after the corresponding syntax checker finished.  At this stage,
the overlays from the previous syntax checks are still present,
and there may be further syntax checkers in the chain.

This variable is an abnormal hook.  See Info
node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :package-version '(flycheck . "0.13")
  :risky t)

(defcustom flycheck-display-errors-delay 0.9
  "Delay in seconds before displaying errors at point.

Use floating point numbers to express fractions of seconds."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "0.15")
  :safe #'numberp)

(defcustom flycheck-display-errors-function #'flycheck-display-error-messages
  "Function to display error messages.

If set to a function, call the function with the list of errors
to display as single argument.  Each error is an instance of the
`flycheck-error' struct.

If set to nil, do not display errors at all."
  :group 'flycheck
  :type '(choice (const :tag "Display error messages"
                        flycheck-display-error-messages)
                 (const :tag "Display error messages only if no error list"
                        flycheck-display-error-messages-unless-error-list)
                 (function :tag "Error display function"))
  :package-version '(flycheck . "0.13")
  :risky t)

(defcustom flycheck-indication-mode 'left-fringe
  "The indication mode for Flycheck errors and warnings.

This variable controls how Flycheck indicates errors in buffers.
May either be `left-fringe', `right-fringe', or nil.

If set to `left-fringe' or `right-fringe', indicate errors and
warnings via icons in the left and right fringe respectively.

If set to nil, do not indicate errors and warnings, but just
highlight them according to `flycheck-highlighting-mode'."
  :group 'flycheck
  :type '(choice (const :tag "Indicate in the left fringe" left-fringe)
                 (const :tag "Indicate in the right fringe" right-fringe)
                 (const :tag "Do not indicate" nil))
  :safe #'symbolp)

(defcustom flycheck-highlighting-mode 'symbols
  "The highlighting mode for Flycheck errors and warnings.

The highlighting mode controls how Flycheck highlights errors in
buffers.  The following modes are known:

`columns'
     Highlight the error column.  If the error does not have a column,
     highlight the whole line.

`symbols'
     Highlight the symbol at the error column, if there is any,
     otherwise behave like `columns'.  This is the default.

`sexps'
     Highlight the expression at the error column, if there is
     any, otherwise behave like `columns'.  Note that this mode
     can be *very* slow in some major modes.

`lines'
     Highlight the whole line.

nil
     Do not highlight errors at all.  However, errors will still
     be reported in the mode line and in error message popups,
     and indicated according to `flycheck-indication-mode'."
  :group 'flycheck
  :type '(choice (const :tag "Highlight columns only" columns)
                 (const :tag "Highlight symbols" symbols)
                 (const :tag "Highlight expressions" sexps)
                 (const :tag "Highlight whole lines" lines)
                 (const :tag "Do not highlight errors" nil))
  :package-version '(flycheck . "0.14")
  :safe #'symbolp)

(defcustom flycheck-check-syntax-automatically '(save
                                                 idle-change
                                                 new-line
                                                 mode-enabled)
  "When Flycheck should check syntax automatically.

This variable is a list of events that may trigger syntax checks.
The following events are known:

`save'
     Check syntax immediately after the buffer was saved.

`idle-change'
     Check syntax a short time (see `flycheck-idle-change-delay')
     after the last change to the buffer.

`new-line'
     Check syntax immediately after a new line was inserted into
     the buffer.

`mode-enabled'
     Check syntax immediately when `flycheck-mode' is enabled.

Flycheck performs a syntax checks only on events, which are
contained in this list.  For instance, if the value of this
variable is `(mode-enabled save)', Flycheck will only check if
the mode is enabled or the buffer was saved, but never after
changes to the buffer contents.

If nil, never check syntax automatically.  In this case, use
`flycheck-buffer' to start a syntax check manually."
  :group 'flycheck
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After the buffer was changed and idle" idle-change)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After `flycheck-mode' was enabled" mode-enabled))
  :package-version '(flycheck . "0.12")
  :safe #'symbolp)

(defcustom flycheck-idle-change-delay 0.5
  "How many seconds to wait before checking syntax automatically.

After the buffer was changed, Flycheck will wait as many seconds
as the value of this variable before starting a syntax check.  If
the buffer is modified during this time, Flycheck will wait
again.

This variable has no effect, if `idle-change' is not contained in
`flycheck-check-syntax-automatically'."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "0.13")
  :safe #'numberp)

(defcustom flycheck-google-max-messages 5
  "How many messages to google at once.

If set to an integer, `flycheck-google-messages' will signal an
error if there are more Flycheck messages at point than the value
of this variable.

If set to nil, `flycheck-google-messages' will always google *all*
messages at point.  This setting is *not* recommended."
  :group 'flycheck
  :type '(choice (const :tag "Always google all messages" nil)
                 (integer :tag "Maximum messages to google"))
  :package-version '(flycheck . "0.10")
  :safe #'numberp)

(defcustom flycheck-standard-error-navigation t
  "Whether to support error navigation with `next-error'.

If non-nil, enable navigation of Flycheck errors with
`next-error', `previous-error' and `first-error'.  Otherwise,
these functions just navigate errors from compilation modes.

Flycheck error navigation with `flycheck-next-error',
`flycheck-previous-error' and `flycheck-first-error' is always
enabled, regardless of the value of this variable.

Note that this setting only takes effect when `flycheck-mode' is
enabled.  Changing it will not affect buffers which already have
`flycheck-mode' enabled."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "0.15")
  :safe #'booleanp)

(defcustom flycheck-navigation-minimum-level nil
  "The minimum level of errors to navigate.

If set to an error level, only navigate errors whose error level
is at least as severe as this one.  If nil, navigate all errors."
  :group 'flycheck
  :type '(radio (const :tag "All locations" nil)
                (const :tag "Informational messages" info)
                (const :tag "Warnings" warning)
                (const :tag "Errors" error)
                (symbol :tag "Custom error level"))
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "0.21"))

(defcustom flycheck-completion-system nil
  "The completion system to use.

`ido'
     Use IDO.

     IDO is a built-in alternative completion system, without
     good flex matching and a powerful UI.  You may want to
     install flx-ido (see URL `https://github.com/lewang/flx') to
     improve the flex matching in IDO.

`grizzl'
     Use Grizzl.

     Grizzl is an alternative completion system with powerful
     flex matching, but a very limited UI.  See URL
     `https://github.com/d11wtq/grizzl'.

nil
     Use the standard unfancy `completing-read'.

     `completing-read' has a very simple and primitive UI, and
     does not offer flex matching.  This is the default setting,
     though, to match Emacs' defaults.  With this system, you may
     want enable option `icomplete-mode' to improve the display
     of completion candidates at least."
  :group 'flycheck
  :type '(choice (const :tag "IDO" ido)
                 (const :tag "Grizzl" grizzl)
                 (const :tag "Completing read" nil))
  :package-version '(flycheck . "0.17"))

(defcustom flycheck-temp-prefix "flycheck"
  "Prefix for temporary files created by Flycheck."
  :group 'flycheck
  :type 'string
  :package-version '(flycheck . "0.19")
  :risky t)

(defcustom flycheck-mode-hook nil
  "Hooks to run after `flycheck-mode'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-after-syntax-check-hook nil
  "Functions to run after each syntax check.

This hook is run after a syntax check was finished.

At this point, *all* chained checkers were run, and all errors
were parsed, highlighted and reported.  The variable
`flycheck-current-errors' contains all errors from all syntax
checkers run during the syntax check, so you can apply any error
analysis functions.

Note that this hook does *not* run after each individual syntax
checker in the syntax checker chain, but only after the *last
checker*.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-before-syntax-check-hook nil
  "Functions to run before each syntax check.

This hook is run right before a syntax check starts.

Error information from the previous syntax check is *not*
cleared before this hook runs.

Note that this hook does *not* run before each individual syntax
checker in the syntax checker chain, but only before the *first
checker*.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-syntax-check-failed-hook nil
  "Functions to run if a syntax check failed.

This hook is run whenever an error occurs during Flycheck's
internal processing.  No information about the error is given to
this hook.

You should use this hook to conduct additional cleanup actions
when Flycheck failed.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-status-changed-functions nil
  "Functions to run if the Flycheck status changed.

This hook is run whenever the status of Flycheck changes.  Each
hook function takes the status symbol as sinlge argument, as
given to `flycheck-report-status', which see.

This variable is a abnormal hook.  See Info
node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t
  :package-version '(flycheck . "0.20"))

(defcustom flycheck-error-list-after-refresh-hook nil
  "Functions to run after the error list was refreshed.

This hook is run whenever the error list is refreshed.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t
  :package-version '(flycheck . "0.21"))

(defface flycheck-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Flycheck face for errors."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-warning
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (t
     :underline t :inherit warning))
  "Flycheck face for warnings."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-info
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "ForestGreen"))
    (t
     :underline t :inherit success))
  "Flycheck face for informational messages."
  :package-version '(flycheck . "0.15")
  :group 'flycheck-faces)

(defface flycheck-fringe-error
  '((t :inherit error))
  "Flycheck face for fringe error indicators."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-fringe-warning
  '((t :inherit warning))
  "Flycheck face for fringe warning indicators."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-fringe-info
  ;; Semantically `success' is probably not the right face, but it looks nice as
  ;; a base face
  '((t :inherit success))
  "Flycheck face for fringe info indicators."
  :package-version '(flycheck . "0.15")
  :group 'flycheck-faces)

(defface flycheck-error-list-error
  '((t :inherit error))
  "Flycheck face for error messages in the error list."
  :package-version '(flycheck . "0.16")
  :group 'flycheck-faces)

(defface flycheck-error-list-warning
  '((t :inherit warning))
  "Flycheck face for warning messages in the error list."
  :package-version '(flycheck . "0.16")
  :group 'flycheck-faces)

(defface flycheck-error-list-info
  '((t :inherit success))
  "Flycheck face for info messages in the error list."
  :package-version '(flycheck . "0.16")
  :group 'flycheck-faces)

;; The base faces for the following two faces are inspired by Compilation Mode
(defface flycheck-error-list-line-number
  '((t :inherit font-lock-keyword-face))
  "Face for line numbers in the error list."
  :group 'flycheck-faces
  :version '(flycheck . "0.16"))

(defface flycheck-error-list-column-number
  '((t :inherit font-lock-doc-face))
  "Face for line numbers in the error list."
  :group 'flycheck-faces
  :version '(flycheck . "0.16"))

(defface flycheck-error-list-checker-name
  '((t :inherit font-lock-type-face))
  "Face for the syntax checker name in the error list."
  :group 'flycheck-faces
  :version '(flycheck . "0.21"))

(defface flycheck-error-list-highlight
  '((t :inherit highlight))
  "Flycheck face to highlight errors in the error list."
  :package-version '(flycheck . "0.15")
  :group 'flycheck-faces)

(defvar flycheck-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'flycheck-buffer)
    (define-key map "C" 'flycheck-clear)
    (define-key map (kbd "C-c") 'flycheck-compile)
    (define-key map "n" 'flycheck-next-error)
    (define-key map "p" 'flycheck-previous-error)
    (define-key map "l" 'flycheck-list-errors)
    (define-key map (kbd "C-w") 'flycheck-copy-messages-as-kill)
    (define-key map "/" 'flycheck-google-messages)
    (define-key map "s" 'flycheck-select-checker)
    (define-key map "e" 'flycheck-set-checker-executable)
    (define-key map "?" 'flycheck-describe-checker)
    (define-key map "i" 'flycheck-info)
    (define-key map "V" 'flycheck-version)
    map)
  "Keymap of Flycheck interactive commands.")

(defcustom flycheck-keymap-prefix (kbd "C-c !")
  "Prefix for key bindings of Flycheck.

Changing this variable outside Customize does not have any
effect.  To change the keymap prefix from Lisp, you need to
explicitly re-define the prefix key:

    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd \"C-c f\"))
    (define-key flycheck-mode-map flycheck-keymap-prefix
                flycheck-command-map)

Please note that Flycheck's manual documents the default
keybindings.  Changing this variable is at your own risk."
  :group 'flycheck
  :package-version '(flycheck . "0.19")
  :type 'string
  :risky t
  :set
  (lambda (variable key)
    (when (and (boundp variable) (boundp 'flycheck-mode-map))
      (define-key flycheck-mode-map (symbol-value variable) nil)
      (define-key flycheck-mode-map key flycheck-command-map))
    (set-default variable key)))

(defcustom flycheck-mode-line '(:eval (flycheck-mode-line-status-text))
  "Mode line lighter for Flycheck.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
more information.  Note that it should contain a _single_ mode
line construct only.

Customize this variable to change how Flycheck reports its status
in the mode line.  You may use `flycheck-mode-line-status-text'
to obtain a human-readable status text, including an
error/warning count.

You may also assemble your own status text.  The current status
of Flycheck is available in `flycheck-last-status-change'.  The
errors in the current buffer are stored in
`flycheck-current-errors', and the function
`flycheck-count-errors' may be used to obtain the number of
errors grouped by error level.

Set this variable to nil to disable the mode line completely."
  :group 'flycheck
  :type 'sexp
  :risky t
  :package-version '(flycheck . "0.20"))

(defcustom flycheck-error-list-mode-line
  `(,(propertized-buffer-identification "%12b")
    " for buffer "
    (:eval (flycheck-error-list-propertized-source-name)))
  "Mode line construct for Flycheck error list.

The value of this variable is a mode line template as in
`mode-line-format', to be used as
`mode-line-buffer-identification' in `flycheck-error-list-mode'.
See Info Node `(elisp)Mode Line Format' for more information.

Customize this variable to change how the error list appears in
the mode line.  The default shows the name of the buffer and the
name of the source buffer, i.e. the buffer whose errors are
currently listed."
  :group 'flycheck
  :type 'sexp
  :risky t
  :package-version '(flycheck . "0.20"))


;;; Minor mode definition
(defvar flycheck-mode-menu-map
  (easy-menu-create-menu
   "Syntax Checking"
   '(["Enable on-the-fly syntax checking" flycheck-mode
      :style toggle :selected flycheck-mode
      ;; Don't let users toggle the mode if there is no syntax checker for this
      ;; buffer
      :enable (or flycheck-mode (flycheck-get-checker-for-buffer))]
     ["Check current buffer" flycheck-buffer flycheck-mode]
     ["Clear errors in buffer" flycheck-clear t]
     "---"
     ["Go to next error" flycheck-next-error flycheck-mode]
     ["Go to previous error" flycheck-previous-error flycheck-mode]
     ["Show all errors" flycheck-list-errors flycheck-mode]
     "---"
     ["Copy messages at point" flycheck-copy-messages-as-kill
      (flycheck-overlays-at (point))]
     ["Google messages at point" flycheck-google-messages
      (flycheck-overlays-at (point))]
     "---"
     ["Select syntax checker" flycheck-select-checker flycheck-mode]
     ["Set executable of syntax checker" flycheck-set-checker-executable
      flycheck-mode]
     "---"
     ["Describe syntax checker" flycheck-describe-checker t]
     ["Show Flycheck version" flycheck-version t]
     ["Read the Flycheck manual" flycheck-info t]))
  "Menu of `flycheck-mode'.")

(defvar flycheck-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map flycheck-keymap-prefix flycheck-command-map)
    ;; We place the menu under a custom menu key.  Since this menu key is not
    ;; present in the menu of the global map, no top-level menu entry is added
    ;; to the global menu bar.  However, it still appears on the mode line
    ;; lighter.
    (define-key map [menu-bar flycheck] flycheck-mode-menu-map)
    map)
  "Keymap of `flycheck-mode'.")

(easy-menu-add-item nil '("Tools") flycheck-mode-menu-map "Spell Checking")

(defun flycheck-teardown ()
  "Teardown Flycheck in the current buffer..

Completely clear the whole Flycheck state.  Remove overlays, kill
running checks, and empty all variables used by Flycheck."
  (flycheck-safe-delete-temporaries)
  (flycheck-stop-checker)
  (flycheck-clean-deferred-check)
  (flycheck-clear)
  (flycheck-cancel-error-display-error-at-point-timer)
  (flycheck-clear-checker))

(defvar-local flycheck-old-next-error-function nil
  "Remember the old `next-error-function'.")

(defconst flycheck-hooks-alist
  '(
    ;; Handle events that may start automatic syntax checks
    (after-save-hook                  . flycheck-handle-save)
    (after-change-functions           . flycheck-handle-change)
    ;; Handle events that may triggered pending deferred checks
    (window-configuration-change-hook . flycheck-perform-deferred-syntax-check)
    (post-command-hook                . flycheck-perform-deferred-syntax-check)
    ;; Teardown Flycheck whenever the buffer state is about to get lost, to
    ;; clean up temporary files and directories.
    (kill-buffer-hook                 . flycheck-teardown)
    (change-major-mode-hook           . flycheck-teardown)
    (before-revert-hook               . flycheck-teardown)
    ;; Update the error list if necessary
    (post-command-hook                . flycheck-error-list-update-source)
    (post-command-hook                . flycheck-error-list-highlight-errors)
    ;; Show or hide error popups after commands
    (post-command-hook                . flycheck-display-error-at-point-soon)
    (post-command-hook                . flycheck-hide-error-buffer)
    ;; Immediately show error popups when navigating to an error
    (next-error-hook                  . flycheck-display-error-at-point))
  "Hooks which Flycheck needs to hook in.

The `car' of each pair is a hook variable, the `cdr' a function
to be added or removed from the hook variable if Flycheck mode is
enabled and disabled respectively.")

;;;###autoload
(define-minor-mode flycheck-mode
  "Minor mode for on-the-fly syntax checking.

When called interactively, toggle `flycheck-mode'.  With prefix
ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `flycheck-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
Otherwise behave as if called interactively.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

\\{flycheck-mode-map}"
  :init-value nil
  :keymap flycheck-mode-map
  :lighter flycheck-mode-line
  :group 'flycheck
  :require 'flycheck
  :after-hook (flycheck-buffer-automatically 'mode-enabled 'force-deferred)
  (cond
   (flycheck-mode
    (flycheck-clear)

    (pcase-dolist (`(,hook . ,fn) flycheck-hooks-alist)
      (add-hook hook fn nil 'local))

    (setq flycheck-old-next-error-function (if flycheck-standard-error-navigation
                                               next-error-function
                                             :unset))
    (when flycheck-standard-error-navigation
      (setq next-error-function #'flycheck-next-error-function)))
   (t
    (unless (eq flycheck-old-next-error-function :unset)
      (setq next-error-function flycheck-old-next-error-function))

    (pcase-dolist (`(,hook . ,fn) flycheck-hooks-alist)
      (remove-hook hook fn 'local))

    (flycheck-teardown))))

(defun flycheck-global-teardown ()
  "Teardown Flycheck in all buffers.

Completely clear the whole Flycheck state in all buffers, stop
all running checks, remove all temporary files, and empty all
variables of Flycheck."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when flycheck-mode
        (flycheck-teardown)))))

;; Clean up the entire state of Flycheck when Emacs is killed, to get rid of any
;; pending temporary files.
(add-hook 'kill-emacs-hook #'flycheck-global-teardown)

(defun flycheck-unload-function ()
  "Unload function for Flycheck."
  (global-flycheck-mode -1)
  (easy-menu-remove-item nil '("Tools") (cadr flycheck-mode-menu-map))
  (remove-hook 'kill-emacs-hook #'flycheck-global-teardown)
  (setq find-function-regexp-alist
        (assq-delete-all 'flycheck-checker find-function-regexp-alist)))


;;; Version information
(defun flycheck-version (&optional show-version)
  "Get the Flycheck version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'flycheck)))
    (when show-version
      (message "Flycheck version: %s" version))
    version))


;;; Global syntax checking
(defun flycheck-may-enable-mode ()
  "Determine whether Flycheck mode may be enabled.

Flycheck mode is not enabled for

- ephemeral buffers (see `flycheck-ephemeral-buffer-p'),
- encrypted buffers (see `flycheck-encrypted-buffer-p'),
- remote files (see `file-remote-p'),
- or if no suitable syntax checker exists.

Return t if Flycheck mode may be enabled, and nil otherwise."
  (and (not (flycheck-ephemeral-buffer-p))
       (not (flycheck-encrypted-buffer-p))
       (not (and (buffer-file-name) (file-remote-p (buffer-file-name) 'method)))
       (flycheck-get-checker-for-buffer)))

(defun flycheck-mode-on-safe ()
  "Enable `flycheck-mode' if it is safe to do so.

`flycheck-mode' is only enabled if `flycheck-may-enable-mode'
returns t."
  (when (flycheck-may-enable-mode)
    (flycheck-mode)))

;;;###autoload
(define-globalized-minor-mode global-flycheck-mode flycheck-mode
  flycheck-mode-on-safe
  :init-value nil
  :group 'flycheck
  :require 'flycheck)


;;; Manual syntax checking
(defun flycheck-clear ()
  "Clear all errors in the current buffer."
  (interactive)
  (flycheck-delete-all-overlays)
  (flycheck-clear-errors)
  (flycheck-error-list-refresh)
  (flycheck-hide-error-buffer))

(defun flycheck-buffer ()
  "Check syntax in the current buffer."
  (interactive)
  (flycheck-clean-deferred-check)
  (if flycheck-mode
      (unless (flycheck-running-p)
        ;; Clear error list and mark all overlays for deletion.  We do not
        ;; delete all overlays immediately to avoid excessive re-displays and
        ;; flickering, if the same errors gets highlighted again after the check
        ;; completed.
        (run-hooks 'flycheck-before-syntax-check-hook)
        (flycheck-clear-errors)
        (flycheck-mark-all-overlays-for-deletion)
        (condition-case err
            (let ((checker (flycheck-get-checker-for-buffer)))
              (if checker
                  (flycheck-start-checker checker)
                (flycheck-clear)
                (flycheck-report-status 'no-checker)))
          (error
           (flycheck-report-error)
           (signal (car err) (cdr err)))))
    (user-error "Flycheck mode disabled")))


;;; Deferred syntax checking
(defvar-local flycheck-deferred-syntax-check nil
  "If non-nil, a deferred syntax check is pending.")

(defun flycheck-must-defer-check ()
  "Determine whether the syntax check has to be deferred.

A check has to be deferred if the buffer is not visible, or if the buffer is
currently being reverted.

Return t if the check is to be deferred, or nil otherwise."
  (or (not (get-buffer-window))
      ;; We defer the syntax check if Flycheck is already running, to
      ;; immediately start a new syntax check after the current one finished,
      ;; because the result of the current check will most likely be outdated by
      ;; the time it is finished.
      (flycheck-running-p)
      ;; We must defer checks while a buffer is being reverted, to avoid race
      ;; conditions while the buffer contents are being restored.
      revert-buffer-in-progress-p))

(defun flycheck-deferred-check-p ()
  "Determine whether the current buffer has a deferred check.

Return t if so, or nil otherwise."
  flycheck-deferred-syntax-check)

(defun flycheck-buffer-deferred ()
  "Defer syntax check for the current buffer."
  (setq flycheck-deferred-syntax-check t))

(defun flycheck-clean-deferred-check ()
  "Clean an deferred syntax checking state."
  (setq flycheck-deferred-syntax-check nil))

(defun flycheck-perform-deferred-syntax-check ()
  "Perform any deferred syntax checks."
  (when (flycheck-deferred-check-p)
    (flycheck-clean-deferred-check)
    (flycheck-buffer-automatically)))


;;; Automatic syntax checking
(defun flycheck-may-check-automatically (&optional condition)
  "Determine whether the buffer may be checked under CONDITION.

Read-only buffers may never be checked automatically.

If CONDITION is non-nil, determine whether syntax may checked
automatically according to
`flycheck-check-syntax-automatically'."
  (and (not (or buffer-read-only (flycheck-ephemeral-buffer-p)))
       (or (not condition)
           (memq condition flycheck-check-syntax-automatically))))

(defun flycheck-buffer-automatically (&optional condition force-deferred)
  "Automatically check syntax at CONDITION.

Syntax is not checked if `flycheck-may-check-automatically'
returns nil for CONDITION.

The syntax check is deferred if FORCE-DEFERRED is non-nil, or if
`flycheck-must-defer-check' returns t."
  (when (and flycheck-mode (flycheck-may-check-automatically condition))
    (if (or force-deferred (flycheck-must-defer-check))
        (flycheck-buffer-deferred)
      (with-demoted-errors "Error while checking syntax automatically: %S"
        (flycheck-buffer)))))

(defvar-local flycheck-idle-change-timer nil
  "Timer to mark the idle time since the last change.")

(defun flycheck-clear-idle-change-timer ()
  "Clear the idle change timer."
  (when flycheck-idle-change-timer
    (cancel-timer flycheck-idle-change-timer)
    (setq flycheck-idle-change-timer nil)))

(defun flycheck-handle-change (beg end _len)
  "Handle a buffer change between BEG and END.

BEG and END mark the beginning and end of the change text.  _LEN
is ignored.

Start a syntax check if a new line has been inserted into the
buffer."
  ;; Save and restore the match data, as recommended in (elisp)Change Hooks
  (save-match-data
    (when flycheck-mode
      ;; The buffer was changed, thus clear the idle timer
      (flycheck-clear-idle-change-timer)
      (if (string-match-p (rx "\n") (buffer-substring beg end))
          (flycheck-buffer-automatically 'new-line 'force-deferred)
        (setq flycheck-idle-change-timer
              (run-at-time flycheck-idle-change-delay nil
                           #'flycheck-handle-idle-change))))))

(defun flycheck-handle-idle-change ()
  "Handle an expired idle time since the last change."
  (flycheck-clear-idle-change-timer)
  (flycheck-buffer-automatically 'idle-change))

(defun flycheck-handle-save ()
  "Handle a save of the buffer."
  (flycheck-buffer-automatically 'save))


;;; Utility functions
(defun flycheck-sexp-to-string (sexp)
  "Convert SEXP to a string.

Like `prin1-to-string' but ensure that the returned string
is loadable."
  (let ((print-quoted t)
        (print-length nil)
        (print-level nil))
    (prin1-to-string sexp)))

(defun flycheck-string-to-number-safe (string)
  "Safely convert STRING to a number.

If STRING is of string type and a numeric string, convert STRING
to a number and return it.  Otherwise return nil."
  (let ((number-re (rx string-start (one-or-more (any digit)) string-end)))
    (when (and (stringp string) (string-match-p number-re string))
      (string-to-number string))))

(defun flycheck-string-list-p (obj)
  "Determine if OBJ is a list of strings."
  (and (listp obj) (-all? #'stringp obj)))

(defun flycheck-symbol-list-p (obj)
  "Determine if OBJ is a list of symbols."
  (and (listp obj) (-all? #'symbolp obj)))

(defun flycheck-same-files-p (file-a file-b)
  "Determine whether FILE-A and FILE-B refer to the same file."
  (string= (directory-file-name (expand-file-name file-a))
           (directory-file-name (expand-file-name file-b))))

(defvar-local flycheck-temporaries nil
  "Temporary files and directories created by Flycheck.")

(defun flycheck-temp-dir-system ()
  "Create a unique temporary directory.

Use `flycheck-temp-prefix' as prefix, and add the directory to
`flycheck-temporaries'.

Return the path of the directory"
  (let* ((tempdir (make-temp-file flycheck-temp-prefix 'directory)))
    (push tempdir flycheck-temporaries)
    tempdir))

(defun flycheck-temp-file-system (filename)
  "Create a temporary file named after FILENAME.

If FILENAME is non-nil, this function creates a temporary
directory with `flycheck-temp-dir-system', and creates a file
with the same name as FILENAME in this directory.

Otherwise this function creates a temporary file with
`flycheck-temp-prefix' and a random suffix.  The path of the file
is added to `flycheck-temporaries'.

Add the path of the file to `flycheck-temporaries'.

Return the path of the file."
  (let (tempfile)
    (if filename
        (let ((directory (flycheck-temp-dir-system)))
          (setq tempfile (expand-file-name (file-name-nondirectory filename)
                                           directory)))
      (setq tempfile (make-temp-file flycheck-temp-prefix)))
    (push tempfile flycheck-temporaries)
    tempfile))

(defun flycheck-temp-file-inplace (filename)
  "Create an in-place copy of FILENAME.

Prefix the file with `flycheck-temp-prefix' and add the path of
the file to `flycheck-temporaries'.

If FILENAME is nil, fall back to `flycheck-temp-file-system'.

Return the path of the file."
  (if filename
      (let* ((tempname (format "%s_%s"
                               flycheck-temp-prefix
                               (file-name-nondirectory filename)))
             (tempfile (expand-file-name tempname
                                         (file-name-directory filename))))
        (push tempfile flycheck-temporaries)
        tempfile)
    (flycheck-temp-file-system filename)))

(defun flycheck-save-buffer-to-file (file-name)
  "Save the contents of the current buffer to FILE-NAME."
  (make-directory (file-name-directory file-name) t)
  (let ((jka-compr-inhibit t))
    (write-region nil nil file-name nil 0)))

(defun flycheck-save-buffer-to-temp (temp-file-fn)
  "Save buffer to temp file returned by TEMP-FILE-FN.

Return the name of the temporary file."
  (let ((filename (funcall temp-file-fn (buffer-file-name))))
    ;; Do not flush short-lived temporary files onto disk
    (let ((write-region-inhibit-fsync t))
      (flycheck-save-buffer-to-file filename))
    filename))

(defun flycheck-prepend-with-option (option items &optional prepend-fn)
  "Prepend OPTION to each item in ITEMS, using PREPEND-FN.

Prepend OPTION to each item in ITEMS.

ITEMS is a list of strings to pass to the syntax checker.  OPTION
is the option, as string.  PREPEND-FN is a function called to
prepend OPTION to each item in ITEMS.  It receives the option and
a single item from ITEMS as argument, and must return a string or
a list of strings with OPTION prepended to the item.  If
PREPEND-FN is nil or omitted, use `list'.

Return a list of strings where OPTION is prepended to each item
in ITEMS using PREPEND-FN.  If PREPEND-FN returns a list, it is
spliced into the resulting list."
  (unless (stringp option)
    (error "Option %S is not a string" option))
  (unless prepend-fn
    (setq prepend-fn #'list))
  (let ((prepend
         (lambda (item)
           (let ((result (funcall prepend-fn option item)))
             (cond
              ((and (listp result) (-all? #'stringp result)) result)
              ((stringp result) (list result))
              (t (error "Invalid result type for option: %S" result)))))))
    (apply #'append (mapcar prepend items))))

(defun flycheck-find-in-buffer (pattern)
  "Find PATTERN in the current buffer.

Return the result of the first matching group of PATTERN, or nil,
if PATTERN did not match."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward pattern nil 'no-error)
        (match-string-no-properties 1)))))

(defun flycheck-ephemeral-buffer-p ()
  "Determine whether the current buffer is an ephemeral buffer.

See Info node `(elisp)Buffer Names' for information about
ephemeral buffers."
  (string-prefix-p " " (buffer-name)))

(defun flycheck-encrypted-buffer-p ()
  "Determine whether the current buffer is an encrypted file.

See Info node `(epa)Top' for Emacs' interface to encrypted
files."
  ;; The EPA file handler sets this variable locally to remember the recipients
  ;; of the encrypted file for re-encryption.  Hence, a local binding of this
  ;; variable is a good indication that the buffer is encrypted.  I haven't
  ;; found any better indicator anyway.
  (local-variable-p 'epa-file-encrypt-to))

(defun flycheck-autoloads-file-p ()
  "Determine whether the current buffer is a autoloads file.

Autoloads are generated by package.el during installation."
  (string-suffix-p "-autoloads.el" (buffer-name)))

(defun flycheck-in-user-emacs-directory-p (filename)
  "Whether FILENAME is in `user-emacs-directory'."
  (string-prefix-p (file-name-as-directory (file-truename user-emacs-directory))
                   (file-truename filename)))

(defun flycheck-safe-delete (file-or-dir)
  "Safely delete FILE-OR-DIR."
  (ignore-errors
    (if (file-directory-p file-or-dir)
        (delete-directory file-or-dir 'recursive)
      (delete-file file-or-dir))))

(defun flycheck-safe-delete-temporaries ()
  "Safely delete all temp files and directories of Flycheck.

Safely delete all files and directories listed in
`flycheck-temporaries' and set the variable's value to nil."
  (mapc #'flycheck-safe-delete flycheck-temporaries)
  (setq flycheck-temporaries nil))

(eval-and-compile
  (defun flycheck-rx-message (form)
    "Translate the `(message)' FORM into a regular expression."
    (let ((body (or (cdr form) '((one-or-more not-newline)))))
      (rx-submatch-n `(group-n 4 ,@body))))

  (defun flycheck-rx-file-name (form)
    "Translate the `(file-name)' FORM into a regular expression."
    (let ((body (or (cdr form) '((minimal-match
                                  (one-or-more not-newline))))))
      (rx-submatch-n `(group-n 1 ,@body))))

  (defun flycheck-rx-to-string (form &optional no-group)
    "Like `rx-to-string' for FORM, but with special keywords:

`line'
     matches the line number.

`column'
     matches the column number.

`(file-name SEXP ...)'
     matches the file name.  SEXP constitutes the body of the message.  If no
     SEXP is given, use a default body  of `(minimal-match
     (one-or-more not-newline))'.

`(message SEXP ...)'
     matches the message. SEXP constitutes the body of the message.  If no SEXP
     is given, use a default body of `(one-or-more not-newline)'.

NO-GROUP is passed to `rx-to-string'."
    (let ((rx-constituents
           (append
            `((line . ,(rx (group-n 2 (one-or-more digit))))
              (column . ,(rx (group-n 3 (one-or-more digit))))
              (file-name flycheck-rx-file-name 0 nil)
              (message flycheck-rx-message 0 nil))
            rx-constituents nil)))
      (rx-to-string form no-group))))

(defun flycheck-current-load-file ()
  "Get the source file currently being loaded.

Always return the name of the corresponding source file, never
any byte-compiled file.

Return nil, if the currently loaded file cannot be determined."
  (-when-let* ((this-file (cond
                           (load-in-progress load-file-name)
                           ((bound-and-true-p byte-compile-current-file))
                           (t (buffer-file-name))))
               ;; A best guess for the source file of a compiled library. Works
               ;; well in most cases, and especially for ELPA packages
               (source-file (concat (file-name-sans-extension this-file)
                                    ".el")))
    (when (file-exists-p source-file)
      source-file)))

(defun flycheck-module-root-directory (module &optional file-name)
  "Get the root directory for a MODULE in FILE-NAME.

MODULE is a qualified module name, either a string with
components separated by a dot, or as list of components.
FILE-NAME is the name of the file or directory containing the
module as string.  When nil or omitted, defaults to the return
value of function `buffer-file-name'.

Return the root directory of the module, that is, the directory,
from which FILE-NAME can be reached by descending directories
along each part of MODULE.

If the MODULE name does not match the directory hierarchy upwards
from FILE-NAME, return the directory containing FILE-NAME.  When
FILE-NAME is nil, return `default-directory'."
  (let ((file-name (or file-name (buffer-file-name)))
        (module-components (if (stringp module)
                               (split-string module (rx "."))
                             (copy-sequence module))))
    (if (and module-components file-name)
        (let ((parts (nreverse module-components))
              (base-directory (directory-file-name
                               (file-name-sans-extension file-name))))
          (while (and parts
                      (string= (file-name-nondirectory base-directory)
                               (car parts)))
            (pop parts)
            (setq base-directory (directory-file-name
                                  (file-name-directory base-directory))))
          (file-name-as-directory base-directory))
      (if file-name
          (file-name-directory file-name)
        (expand-file-name default-directory)))))


;;; Minibuffer tools
(defvar read-flycheck-checker-history nil
  "`completing-read' history of `read-flycheck-checker'.")

(defun read-flycheck-checker (prompt &optional default)
  "Read a flycheck checker from minibuffer with PROMPT and DEFAULT.

Return the checker as symbol, or default, if no checker was
chosen.  If DEFAULT is nil and no checker was chosen, signal a
`user-error', if the underlying completion system does not
provide a default on its own."
  (when (and default (not (flycheck-valid-checker-p default)))
    (error "%S is no valid Flycheck checker" default))
  (let* ((candidates (mapcar #'symbol-name (flycheck-defined-checkers)))
         (default (and default (symbol-name default)))
         (input (pcase flycheck-completion-system
                  (`ido (ido-completing-read prompt candidates nil
                                             'require-match nil
                                             'read-flycheck-checker-history
                                             default))
                  (`grizzl (if (and (fboundp 'grizzl-make-index)
                                    (fboundp 'grizzl-completing-read))
                               (grizzl-completing-read
                                prompt (grizzl-make-index candidates))
                             (user-error "Please install Grizzl from \
https://github.com/d11wtq/grizzl")))
                  (_ (completing-read prompt candidates nil 'require-match
                                      nil 'read-flycheck-checker-history
                                      default)))))
    (when (string-empty-p input)
      (unless default
        (user-error "No syntax checker entered"))
      (setq input default))
    (let ((checker (intern input)))
      (unless (flycheck-valid-checker-p checker)
        (error "%S is no Flycheck syntax checker" checker))
      checker)))


;;; Checker definitions
(defun flycheck-command-argument-p (arg)
  "Check whether ARG is a valid command argument."
  (pcase arg
    ((pred stringp) t)
    ((or `source `source-inplace `source-original) t)
    ((or `temporary-directory `temporary-file-name) t)
    (`null-device t)
    (`(config-file ,option-name ,config-file-var)
     (and (stringp option-name)
          (symbolp config-file-var)))
    (`(config-file ,option-name ,config-file-var ,prepender)
     (and (stringp option-name)
          (symbolp config-file-var)
          (symbolp prepender)))
    (`(,(or `option `option-list) ,option-name ,option-var)
     (and (stringp option-name)
          (symbolp option-var)))
    (`(,(or `option `option-list) ,option-name ,option-var ,prepender)
     (and (stringp option-name)
          (symbolp option-var)
          (symbolp prepender)))
    (`(,(or `option `option-list) ,option-name ,option-var ,prepender ,filter)
     (and (stringp option-name)
          (symbolp option-var)
          (symbolp prepender)
          (symbolp filter)))
    (`(option-flag ,option-name ,option-var)
     (and (stringp option-name)
          (symbolp option-var)))
    (`(eval ,_) t)
    (_ nil)))

(defun flycheck-validate-next-checker (next &optional strict)
  "Validate NEXT checker.

With STRICT non-nil, also check whether the syntax checker and
the error level in NEXT are valid.  Otherwise just check whether
these are symbols.

Signal an error if NEXT is not a valid entry for
`:next-checkers'."
  (when (symbolp next)
    (setq next (cons t next)))
  (pcase next
    (`(,level . ,checker)
     (if strict
         (progn
           (unless (or (eq level t) (flycheck-error-level-p level))
             (error "%S is not a valid Flycheck error level" level))
           (unless (flycheck-valid-checker-p checker)
             (error "%s is not a valid Flycheck syntax checker" checker)))
       (unless (symbolp level)
         (error "Error level %S must be a symbol" level))
       (unless (symbolp checker)
         (error "Checker %S must be a symbol" checker))))
    (_ (error "%S must be a symbol or cons cell" next)))
  t)

(defconst flycheck-checker-version 1
  "The internal version of syntax checker declarations.

Flycheck will not use syntax checkers whose version is less than
this constant.")

(defun flycheck-verify-checker-properties (symbol)
  "Verify the syntax checker properties of SYMBOL."
  (let ((command (get symbol 'flycheck-command))
        (parser (get symbol 'flycheck-error-parser))
        (patterns (get symbol 'flycheck-error-patterns))
        (filter (get symbol 'flycheck-error-filter))
        (modes (get symbol 'flycheck-modes))
        (predicate (get symbol 'flycheck-predicate))
        (next-checkers (get symbol 'flycheck-next-checkers)))
    (when (null command)
      (error "Missing :command"))
    (unless (stringp (car command))
      (error "Invalid command executable %S" (car command)))
    (unless (-all? 'flycheck-command-argument-p (cdr command))
      (error "Invalid command arguments %S" command))
    (when (and (null parser) (null patterns))
      (error "Missing :error-patterns or :error-parser"))
    (unless (functionp parser)
      (error ":error-parser %S is not a function" parser))
    (unless (functionp filter)
      (error ":error-filter %S is not a function" filter))
    (unless (or modes predicate)
      (error "Missing :modes or :predicate"))
    (unless (-all? #'symbolp modes)
      (error "Invalid :modes %s, must be a symbol or a list thereof" modes))
    (unless (or (null predicate) (functionp predicate))
      (error ":predicate %S is not a function" predicate))
    (dolist (checker next-checkers)
      (flycheck-validate-next-checker checker))))

(defun flycheck-set-checker-properties (symbol properties)
  "Set and verify syntax checker PROPERTIES on SYMBOL.

PROPERTIES is an alist of properties to set."
  (pcase-dolist (`(,prop . ,value) properties)
    (put symbol prop value))
  (flycheck-verify-checker-properties symbol)
  (put symbol 'flycheck-checker-version flycheck-checker-version))

(defmacro flycheck-define-checker (symbol docstring &rest properties)
  "Define SYMBOL as syntax checker with DOCSTRING and PROPERTIES.

DOCSTRING provides documentation for the new syntax checker.  Use
`flycheck-describe-checker' to view the documentation of a syntax
checker.

The following PROPERTIES constitute a syntax checker.  `:command'
is mandatory.  A syntax checker must also have either `:modes' or
`:predicate', and either `:error-patterns' or `:error-parser'.
`:next-checkers' is entirely optional.

Signal an error at macro-expansion time if a mandatory property
is missing or if any property as an invalid value.

Any syntax checker defined with this macro is eligible for manual
syntax checker selection with `flycheck-select-checker'.  To make
the new syntax checker available for automatic selection, it must
be registered in `flycheck-checkers'.

`:command (EXECUTABLE ARG ...)'
     An unquoted list describing the syntax checker command to
     execute.

     EXECUTABLE must be a string with the executable of this
     syntax checker.

     A variable `flycheck-CHECKER-executable' is implicitly
     defined to allow overriding of the executable.  The variable
     is customizable and buffer-local.  If this variable is
     non-nil, Flycheck uses the value of the variable as
     executable, otherwise it falls back to EXECUTABLE.  In
     either case, the executable is checked with
     `executable-find' before use.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns ((LEVEL SEXP ...) ...)'
     An unquoted list of error patterns to parse the output of
     the syntax checker `:command'.

     LEVEL is either `error' or `warning' and denotes the
     severity of errors matched by the pattern.  The LEVEL is
     followed by one or more RX `SEXP's which parse the error and
     extract line, column, file name and error message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for special RX forms provided by
     Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

`:error-parser FUNCTION'
`:error-parser (lambda (output checker buffer) BODY ...)'
     A function to parse errors with, either as unquoted symbol,
     or `lambda' form.

     The function must accept three arguments OUTPUT CHECKER
     BUFFER, where OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

`:error-filter FUNCTION'
`:error-filter (lambda (errors) BODY ...)'
     A function to filter or modify errors parsed by
     `:error-parser' or `:error-patterns'.

     The function takes the list of parsed `flycheck-error'
     objects as sole argument and shall return a list of
     `flycheck-error' as the result of the syntax checker.  The
     function is free to add or remove errors, or modify
     individual errors.  It may modify the list of errors as well
     as individual errors in place.

     Syntax checkers will mostly use this feature to fix
     misplaced error locations, or improve error messages.

`:modes MODE'
`:modes (MODE ...)'
     An unquoted major mode symbol or a list thereof.  If
     present, the syntax checker is only used if the major mode
     of the buffer to check is equal (as in `eq') to any given
     MODE.

     If a `:predicate' is present, it is additionally used in
     each buffer of these MODEs.

`:predicate FUNCTION'
`:predicate (lambda () BODY ...)'
     A function to determine whether to use the syntax checker in
     the current buffer, either as unquoted function symbol or
     `lambda' form.  The syntax checker is only used if this
     function returns non-nil when called in the buffer to check.
     If `:modes' is given, the function is only called in
     matching modes.

`:next-checkers (ITEM ...)'
     An unquoted list defining the syntax checker to run after
     this syntax checker.

     Each ITEM is a cons cell `(LEVEL . CHECKER)'.  CHECKER is a
     syntax checker to run after this syntax checker.  LEVEL is
     an error level.  CHECKER will only be used if there are no
     current errors of at least LEVEL.  LEVEL may also be t, in
     which case CHECKER is used regardless of the current errors.

     ITEM may also be a syntax checker symbol, which is
     equivalent to `(t . ITEM)'.

     Flycheck tries all items in order of declaration, and uses
     the first whose LEVEL matches and whose CHECKER is
     registered and can be used for the current buffer."
  (declare (indent 1)
           (doc-string 2))
  (let ((modes (plist-get properties :modes))
        (command (plist-get properties :command))
        (executable-var (intern (format "flycheck-%s-executable" symbol))))
    `(progn
       (defcustom ,executable-var nil
         ,(format "The executable of the %s syntax checker.

Either a string containing the name or the path of the
executable, or nil to use the default executable from the syntax
checker declaration.

The default executable is %S." symbol (car command))
         :type '(choice (const :tag "Default executable" nil)
                        (string :tag "Name or path"))
         :group 'flycheck-executables
         :risky t)

       (make-variable-buffer-local ',executable-var)
       (flycheck-set-checker-properties
        ',symbol
        (list '(flycheck-documentation . ,docstring)
              '(flycheck-executable-var . ,executable-var)
              '(flycheck-command . ,command)
              (cons 'flycheck-error-parser
                    #',(or (plist-get properties :error-parser)
                           'flycheck-parse-with-patterns))
              '(flycheck-error-patterns
                .
                ,(mapcar
                  (lambda (p)
                    (cons (flycheck-rx-to-string `(and ,@(cdr p)) 'no-group)
                          (car p)))
                  (plist-get properties :error-patterns)))
              (cons 'flycheck-error-filter
                    #',(or (plist-get properties :error-filter)
                           'flycheck-sanitize-errors))
              '(flycheck-modes . ,(if (and modes (symbolp modes))
                                      (list modes)
                                    modes))
              (cons 'flycheck-predicate #',(plist-get properties :predicate))
              '(flycheck-next-checkers . ,(plist-get properties :next-checkers))
              (cons 'flycheck-file (flycheck-current-load-file)))))))

;;;###autoload
(defmacro flycheck-def-config-file-var (symbol checker &optional file-name
                                               &rest custom-args)
  "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable, buffer-local variable using
`defcustom', to provide a configuration file for the given syntax
CHECKER.  CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'."
  (declare (indent 3))
  `(progn
     (put ',checker 'flycheck-config-file-var ',symbol)
     (defcustom ,symbol ,file-name
       ,(format "Configuration file for `%s'.

If set to a string, locate the configuration file using the
functions from `flycheck-locate-config-file-functions'.  If the
file is found pass it to the syntax checker as configuration
file.

If no configuration file is found, or if this variable is set to
nil, invoke the syntax checker without a configuration file.

Use this variable as file-local variable if you need a specific
configuration file a buffer." checker)
       :type '(choice (const :tag "No configuration file" nil)
                      (string :tag "File name or path"))
       :group 'flycheck-config-files
       ,@custom-args)
     (make-variable-buffer-local ',symbol)))

;;;###autoload
(defmacro flycheck-def-option-var (symbol init-value checker docstring
                                          &rest custom-args)
  "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable, buffer-local
variable using `defcustom', to provide an option for the given
syntax CHECKER.  INIT-VALUE is the initial value of the variable,
and DOCSTRING is its docstring.  CUSTOM-ARGS are forwarded to
`defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'."
  (declare (indent 3)
           (doc-string 4))
  `(progn
     (let ((options (flycheck-checker-option-vars ',checker)))
       (unless (memq ',symbol options)
         (put ',checker 'flycheck-option-vars (cons ',symbol options))))
     (defcustom ,symbol ,init-value
       ,(format "%s

This variable is an option for the syntax checker `%s'." docstring checker)
       :group 'flycheck-options
       ,@custom-args)
     (make-variable-buffer-local ',symbol)))


;;; Checker extensions
(defun flycheck-add-next-checker (checker next &optional append)
  "Add a NEXT checker after CHECKER.

CHECKER is a syntax checker symbol, to which to add NEXT checker.

NEXT is a cons cell `(LEVEL . NEXT-CHECKER)'.  NEXT-CHECKER is a
symbol denoting the syntax checker to run after CHECKER.  LEVEL
is an error level.  NEXT-CHECKER will only be used if there is no
current error whose level is more severe than LEVEL.  LEVEL may
also be t, in which case NEXT-CHECKER is used regardless of the
current errors.

NEXT can also be a syntax checker symbol only, which is
equivalent to `(t . NEXT)'.

NEXT-CHECKER is prepended before other next checkers, unless
APPEND is non-nil."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is not a valid syntax checker" checker))
  (flycheck-validate-next-checker next 'strict)
  (if append
      (put checker 'flycheck-next-checkers
           (append (get checker 'flycheck-next-checkers) (list next)))
    (push next (get checker 'flycheck-next-checkers))))

(defun flycheck-add-mode (checker mode)
  "Add a new major MODE to CHECKER.

CHECKER and MODE are symbols denoting a syntax checker and a
major mode respectively.

Add MODE to the `:modes' property of CHECKER, so that CHECKER
will be used in buffers with MODE."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is not a valid syntax checker" checker))
  (unless (symbolp mode)
    (error "%s is not a symbol" mode))
  (push mode (get checker 'flycheck-modes)))


;;; Checker API
(defun flycheck-valid-checker-p (checker)
  "Check whether a CHECKER is valid.

A valid checker is a symbol defined as syntax checker with
`flycheck-define-checker'."
  (and (symbolp checker)
       (= (or (get checker 'flycheck-checker-version) 0)
          flycheck-checker-version)))

(defun flycheck-defined-checkers ()
  "Find all defined syntax checkers.

The returned list is sorted alphapetically by the symbol name of
the syntax checkers."
  (let (defined-checkers)
    (mapatoms (lambda (symbol)
                (when (flycheck-valid-checker-p symbol)
                  (push symbol defined-checkers))))
    (sort defined-checkers #'string<)))

(defun flycheck-registered-checker-p (checker)
  "Determine whether CHECKER is registered.

A checker is registered if it is contained in
`flycheck-checkers'."
  (and (flycheck-valid-checker-p checker)
       (memq checker flycheck-checkers)))

(defun flycheck-disabled-checker-p (checker)
  "Determine whether CHECKER is disabled.

A checker is disabled if it is contained in
`flycheck-disabled-checkers'."
  (memq checker flycheck-disabled-checkers))

(defun flycheck-checker-executable-variable (checker)
  "Get the executable variable of CHECKER."
  (get checker 'flycheck-executable-var))

(defun flycheck-checker-default-executable (checker)
  "Get the default executable of CHECKER."
  (car (get checker 'flycheck-command)))

(defun flycheck-checker-executable (checker)
  "Get the command executable of CHECKER.

The executable is either the value of the variable
`flycheck-CHECKER-executable', or the default executable given in
the syntax checker definition, if the variable is nil."
  (or (symbol-value (flycheck-checker-executable-variable checker))
      (flycheck-checker-default-executable checker)))

(defun flycheck-checker-arguments (checker)
  "Get the command arguments of CHECKER."
  (cdr (get checker 'flycheck-command)))

(defun flycheck-checker-modes (checker)
  "Get the modes of CHECKER."
  (let ((modes (get checker 'flycheck-modes)))
    (if (and modes (symbolp modes))
        (list modes)
      modes)))

(defun flycheck-checker-predicate (checker)
  "Get the predicate function of CHECKER."
  (get checker 'flycheck-predicate))

(defun flycheck-checker-next-checkers (checker)
  "Get the next checkers for CHECKER."
  (get checker 'flycheck-next-checkers))

(defun flycheck-checker-error-patterns (checker)
  "Get the error patterns of CHECKER.

Each pattern has the form `(REGEXP . LEVEL)' where REGEXP is the
regular expression, and LEVEL the corresponding level symbol."
  (get checker 'flycheck-error-patterns))

(defun flycheck-checker-error-parser (checker)
  "Get the error parser of CHECKER."
  (get checker 'flycheck-error-parser))

(defun flycheck-checker-error-filter (checker)
  "Get the error filter of CHECKER."
  (get checker 'flycheck-error-filter))

(defun flycheck-checker-pattern-to-error-regexp (pattern)
  "Convert PATTERN into an error regexp for compile.el.

Return a list representing PATTERN, suitable as element in
`compilation-error-regexp-alist'."
  (let* ((regexp (car pattern))
         (level (cdr pattern))
         (level-no (pcase level
                     (`error 2)
                     (`warning 1))))
    (list regexp 1 2 3 level-no)))

(defun flycheck-checker-compilation-error-regexp-alist (checker)
  "Convert error patterns of CHECKER for use with compile.el.

Return an alist of all error patterns of CHECKER, suitable for
use with `compilation-error-regexp-alist'."
  (mapcar #'flycheck-checker-pattern-to-error-regexp
          (flycheck-checker-error-patterns checker)))

(defun flycheck-checker-documentation (checker)
  "Get the documentation of CHECKER."
  (documentation-property checker 'flycheck-documentation))

(defun flycheck-checker-file (checker)
  "Get the file CHECKER was defined in.

Return nil if the file cannot be determined."
  (get checker 'flycheck-file))

(defun flycheck-checker-config-file-var (checker)
  "Get the associated configuration file variable of CHECKER.

Return nil if CHECKER has no associated configuration file
variable."
  (get checker 'flycheck-config-file-var))

(defun flycheck-checker-option-vars (checker)
  "Get the associated option variables of CHECKER.

Return a (possibly empty) list of variable symbols."
  (get checker 'flycheck-option-vars))

(defun flycheck-locate-config-file (filename checker)
  "Locate the configuration file FILENAME for CHECKER.

Locate the configuration file using
`flycheck-locate-config-file-functions'.

Return the absolute path of the configuration file, or nil if no
configuration file was found."
  (-when-let (filepath (run-hook-with-args-until-success
                        'flycheck-locate-config-file-functions
                        filename checker))
    (when (file-exists-p filepath)
      filepath)))

(defun flycheck-substitute-argument (arg checker)
  "Substitute ARG for CHECKER.

Return a list of real arguments for the executable of CHECKER,
substituted for the symbolic argument ARG.  Single arguments,
e.g. if ARG is a literal strings, are wrapped in a list.

ARG may be one of the following forms:

STRING
     Return ARG unchanged.

`source', `source-inplace'
     Create a temporary file to check and return its path.  With
     `source-inplace' create the temporary file in the same
     directory as the original file.  The value of
     `flycheck-temp-prefix' is used as prefix of the file name.

     With `source', try to retain the non-directory component of
     the buffer's file name in the temporary file.

     `source' is the preferred way to pass the input file to a
     syntax checker.  `source-inplace' should only be used if the
     syntax checker needs other files from the source directory,
     such as include files in C.

`source-original'
     Return the path of the actual file to check, or an empty
     string if the buffer has no file name.

     Note that the contents of the file may not be up to date
     with the contents of the buffer to check.  Do not use this
     as primary input to a checker, unless absolutely necessary.

     When using this symbol as primary input to the syntax
     checker, add `flycheck-buffer-saved-p' to the `:predicate'.

`temporary-directory'
     Create a unique temporary directory and return its path.

`temporary-file-name'
     Return a unique temporary filename.  The file is *not*
     created.

     To ignore the output of syntax checkers, try `null-device'
     first.

`null-device'
     Return the value of `null-device', i.e the system null
     device.

     Use this option to ignore the output of a syntax checker.
     If the syntax checker cannot handle the null device, or
     won't write to an existing file, try `temporary-file-name'
     instead.

`(config-file OPTION VARIABLE [PREPEND-FN])'
     Search the configuration file bound to VARIABLE with
     `flycheck-locate-config-file' and return a list of arguments
     that pass this configuration file to the syntax checker, or
     nil if the configuration file was not found.

     PREPEND-FN is called with the OPTION and the located
     configuration file, and should return OPTION prepended
     before the file, either a string or as list.  If omitted,
     PREPEND-FN defaults to `list'.

`(option OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE and return a list of
     arguments that pass this value as value for OPTION to the
     syntax checker.

     PREPEND-FN is called with the OPTION and the value of
     VARIABLE, and should return OPTION prepended before the
     file, either a string or as list.  If omitted, PREPEND-FN
     defaults to `list'.

     FILTER is an optional function to be applied to the value of
     VARIABLE before prepending.  This function must return nil
     or a string.  In the former case, return nil.  In the latter
     case, return a list of arguments as described above.

`(option-list OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE, which must be a list,
     and prepend OPTION before each item in this list, using
     PREPEND-FN.

     PREPEND-FN is called with the OPTION and each item of the
     list as second argument, and should return OPTION prepended
     before the item, either as string or as list.  If omitted,
     PREPEND-FN defaults to `list'.

     FILTER is an optional function to be applied to each item in
     the list before prepending OPTION.  It shall return the
     option value for each item as string, or nil, if the item is
     to be ignored.

`(option-flag OPTION VARIABLE)'
     Retrieve the value of VARIABLE and return OPTION, if the
     value is non-nil.  Otherwise return nil.

`(eval FORM)'
     Return the result of evaluating FORM in the buffer to be
     checked.  FORM must either return a string or a list of
     strings, or nil to indicate that nothing should be
     substituted for CELL.  For all other return types, signal an
     error

     _No_ further substitutions are performed, neither in FORM
     before it is evaluated, nor in the result of evaluating
     FORM.

In all other cases, signal an error.

Note that substitution is *not* recursive.  No symbols or cells
are substituted within the body of cells!"
  (pcase arg
    ((pred stringp) (list arg))
    (`source
     (list (flycheck-save-buffer-to-temp #'flycheck-temp-file-system)))
    (`source-inplace
     (list (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace)))
    (`source-original (list (or (buffer-file-name) "")))
    (`temporary-directory (list (flycheck-temp-dir-system)))
    (`temporary-file-name
     (let ((directory (flycheck-temp-dir-system)))
       (list (make-temp-name (expand-file-name "flycheck" directory)))))
    (`null-device (list null-device))
    (`(config-file ,option-name ,file-name-var)
     (-when-let* ((value (symbol-value file-name-var))
                  (file-name (flycheck-locate-config-file value checker)))
       (flycheck-prepend-with-option option-name (list file-name))))
    (`(config-file ,option-name ,file-name-var ,prepend-fn)
     (-when-let* ((value (symbol-value file-name-var))
                  (file-name (flycheck-locate-config-file value checker)))
       (flycheck-prepend-with-option option-name (list file-name) prepend-fn)))
    (`(option ,option-name ,variable)
     (-when-let (value (symbol-value variable))
       (unless (stringp value)
         (error "Value %S of %S for option %s is not a string"
                value variable option-name))
       (flycheck-prepend-with-option option-name (list value))))
    (`(option ,option-name ,variable ,prepend-fn)
     (-when-let (value (symbol-value variable))
       (unless (stringp value)
         (error "Value %S of %S for option %s is not a string"
                value variable option-name))
       (flycheck-prepend-with-option option-name (list value) prepend-fn)))
    (`(option ,option-name ,variable ,prepend-fn ,filter)
     (-when-let (value (funcall filter (symbol-value variable)))
       (unless (stringp value)
         (error "Value %S of %S (filter: %S) for option %s is not a string"
                value variable filter option-name))
       (flycheck-prepend-with-option option-name (list value) prepend-fn)))
    (`(option-list ,option-name ,variable)
     (let ((value (symbol-value variable)))
       (unless (and (listp value) (-all? #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value)))
    (`(option-list ,option-name ,variable ,prepend-fn)
     (let ((value (symbol-value variable)))
       (unless (and (listp value) (-all? #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(option-list ,option-name ,variable ,prepend-fn ,filter)
     (let ((value (delq nil (mapcar filter (symbol-value variable)))))
       (unless (and (listp value) (-all? #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(option-flag ,option-name ,variable)
     (when (symbol-value variable)
       (list option-name)))
    (`(eval ,form)
     (let ((result (eval form)))
       (cond
        ((and (listp result) (-all? #'stringp result)) result)
        ((stringp result) (list result))
        (t (error "Invalid result from evaluation of %S: %S" form result)))))
    (_ (error "Unsupported argument %S" arg))))

(defun flycheck-checker-substituted-arguments (checker)
  "Get the substituted arguments of a CHECKER.

Substitute each argument of CHECKER using
`flycheck-substitute-argument'.  This replaces any special
symbols in the command."
  (apply #'append
         (mapcar (lambda (arg) (flycheck-substitute-argument arg checker))
                 (flycheck-checker-arguments checker))))

(defun flycheck-checker-shell-command (checker)
  "Get a shell command for CHECKER.

Perform substitution in the arguments of CHECKER, but with
`flycheck-substitute-shell-argument'.

Return the command of CHECKER as single string, suitable for
shell execution."
  ;; Note: Do NOT use `combine-and-quote-strings' here.  Despite it's name it
  ;; does not properly quote shell arguments, and actually breaks for special
  ;; characters.  See https://github.com/flycheck/flycheck/pull/522
  (mapconcat #'shell-quote-argument
   (cons (flycheck-checker-executable checker)
         (apply #'append
                (mapcar (lambda (arg)
                          (if (memq arg '(source source-inplace source-original))
                              (list (or (buffer-file-name) ""))
                            (flycheck-substitute-argument arg checker)))
                        (flycheck-checker-arguments checker)))) " "))

(defun flycheck-check-modes (checker)
  "Check the allowed modes of CHECKER.

Check the current `major-mode' against the modes allowed for
CHECKER.  Return t if the modes match or nil otherwise."
  (let ((modes (flycheck-checker-modes checker)))
    (or (not modes) (memq major-mode modes))))

(defun flycheck-check-predicate (checker)
  "Check the predicate of CHECKER.

Check the predicate of CHECKER.  Return t if CHECKER has no
predicate function, otherwise return the result of calling the
predicate function."
  (let ((predicate (flycheck-checker-predicate checker)))
    (or (not predicate) (funcall predicate))))

(defun flycheck-check-executable (checker)
  "Check the executable of the CHECKER."
  (when (executable-find (flycheck-checker-executable checker)) t))

(defun flycheck-may-use-checker (checker)
  "Determine whether a CHECKER may be used.

Return t if CHECKER may be used for the current buffer and nil
otherwise."
  (if (flycheck-valid-checker-p checker)
      (and (flycheck-check-modes checker)
           (flycheck-check-predicate checker)
           (flycheck-check-executable checker)
           (not (flycheck-disabled-checker-p checker)))
    (lwarn 'flycheck :warning "%S is no valid Flycheck syntax checker.
Try to reinstall the package defining this syntax checker." checker)
    nil))

(defun flycheck-may-use-next-checker (next-checker)
  "Determine whether NEXT-CHECKER may be used."
  (when (symbolp next-checker)
    (push t next-checker))
  (let ((level (car next-checker))
        (next-checker (cdr next-checker)))
    (and (or (eq level t)
             (flycheck-has-max-current-errors-p level))
         (flycheck-registered-checker-p next-checker)
         (flycheck-may-use-checker next-checker))))


;;; Configuration file functions
(defun flycheck-locate-config-file-absolute-path (filepath _checker)
  "Locate a configuration file by a FILEPATH.

If FILEPATH is a contains a path separator, expand it against the
default directory and return it.  Otherwise return nil.

_CHECKER is ignored."
  ;; If the path is just a plain file name, skip it.
  (unless (string= (file-name-nondirectory filepath) filepath)
    (expand-file-name filepath)))

(defun flycheck-locate-config-file-ancestor-directories (filename _checker)
  "Locate a configuration FILENAME in ancestor directories.

If the current buffer has a file name, search FILENAME in the
directory of the current buffer and all ancestors thereof (see
`locate-dominating-file').  If the file is found, return its
absolute path.  Otherwise return nil.

_CHECKER is ignored."
  (-when-let* ((basefile (buffer-file-name))
               (directory (locate-dominating-file basefile filename)))
    (expand-file-name filename directory)))

(defun flycheck-locate-config-file-home (filename _checker)
  "Locate a configuration FILENAME in the home directory.

Return the absolute path, if FILENAME exists in the user's home
directory, or nil otherwise."
  (let ((path (expand-file-name filename "~")))
    (when (file-exists-p path)
      path)))

(mapc (apply-partially #'custom-add-frequent-value
                       'flycheck-locate-config-file-functions)
      '(flycheck-locate-config-file-absolute-path
        flycheck-locate-config-file-ancestor-directories
        flycheck-locate-config-file-home))


;;; Generic option filters
(defun flycheck-option-int (value)
  "Convert an integral option VALUE to a string.

If VALUE is nil, return nil.  Otherwise return VALUE converted to
a string."
  (when value
    (number-to-string value)))

(defun flycheck-option-comma-separated-list (value &optional separator filter)
  "Convert VALUE into a list separated by SEPARATOR.

SEPARATOR is a string to separate items in VALUE, defaulting to
\",\".  FILTER is an optional function, which takes a single
argument and returns either a string or nil.

If VALUE is a list, apply FILTER to each item in VALUE, remove
all nil items, and return a single string of all remaining items
separated by SEPARATOR.

Otherwise, apply FILTER to VALUE and return the result.
SEPARATOR is ignored in this case."
  (let ((filter (or filter #'identity))
        (separator (or separator ",")))
    (if (listp value)
        (-when-let (value (delq nil (mapcar filter value)))
          (string-join value separator))
      (funcall filter value))))


;;; Syntax checker predicates
(defun flycheck-buffer-saved-p (&optional buffer)
  "Determine whether BUFFER is saved to a file.

BUFFER is the buffer to check.  If omitted or nil, use the
current buffer as BUFFER.

Return non-nil if the BUFFER is backed by a file, and not
modified, or nil otherwise."
  (and (buffer-file-name buffer) (not (buffer-modified-p buffer))))


;;; Checker selection
(defvar-local flycheck-last-checker nil
  "The last checker used for the current buffer.")

(defun flycheck-clear-checker ()
  "Clear configured and remembered checkers in the current buffer."
  (setq flycheck-last-checker nil))

(defun flycheck-try-last-checker-for-buffer ()
  "Try the last checker for the current buffer.

Return the checker if it may be used, or nil otherwise."
  ;; We should not use the last checker if it was removed from the list of
  ;; allowed checkers in the meantime
  (when (and (flycheck-registered-checker-p flycheck-last-checker)
             (flycheck-may-use-checker flycheck-last-checker))
    flycheck-last-checker))

(defun flycheck-get-new-checker-for-buffer ()
  "Find a new checker for the current buffer.

If a checker is found set `flycheck-last-checker' to re-use this
checker for the next check.

Return the checker if there is any, or nil otherwise."
  (let ((checkers flycheck-checkers))
    (while (and checkers (not (flycheck-may-use-checker (car checkers))))
      (setq checkers (cdr checkers)))
    (when checkers
      (setq flycheck-last-checker (car checkers)))))

(defun flycheck-get-checker-for-buffer ()
  "Find the checker for the current buffer.

Return checker if there is a checker for the current buffer, or
nil otherwise."
  (if flycheck-checker
      (if (flycheck-may-use-checker flycheck-checker)
          flycheck-checker
        (user-error "Selected syntax checker %s cannot be used"
                    flycheck-checker))
    (or (flycheck-try-last-checker-for-buffer)
        (flycheck-get-new-checker-for-buffer))))

(defun flycheck-get-next-checker-for-buffer (checker)
  "Get the checker to run after CHECKER for the current buffer."
  (let ((next-checkers (flycheck-checker-next-checkers checker)))
    (while (and next-checkers
                (not (flycheck-may-use-next-checker (car next-checkers))))
      (setq next-checkers (cdr next-checkers)))
    (when next-checkers
      (if (symbolp (car next-checkers))
          (car next-checkers)
        (cdar next-checkers)))))

(defun flycheck-select-checker (checker)
  "Select CHECKER for the current buffer.

CHECKER is a syntax checker symbol (see `flycheck-checkers') or
nil.  In the former case, use CHECKER for the current buffer,
otherwise deselect the current syntax checker (if any) and use
automatic checker selection via `flycheck-checkers'.

If called interactively prompt for CHECKER.  With prefix arg
deselect the current syntax checker and enable automatic
selection again.

Set `flycheck-checker' to CHECKER and automatically start a new
syntax check if the syntax checker changed.

CHECKER will be used, even if it is not contained in
`flycheck-checkers', or if it is disabled via
`flycheck-disabled-checkers'."
  (interactive
   (if current-prefix-arg
       (list nil)
     (list (read-flycheck-checker "Select checker: " flycheck-last-checker))))
  (when (not (eq checker flycheck-checker))
    (unless (or (not checker) (flycheck-may-use-checker checker))
      (user-error "Can't use syntax checker %S in this buffer" checker))
    (setq flycheck-checker checker)
    (when flycheck-mode
      (flycheck-buffer))))


;;; Checkers as compile commands
(defun flycheck-compile-name (_name)
  "Get a name for a Flycheck compilation buffer.

_NAME is ignored."
  (format "*Flycheck %s*" (buffer-file-name)))

(defun flycheck-compile (checker)
  "Run CHECKER via `compile'.

CHECKER must be a valid syntax checker.  Interactively, prompt
for a syntax checker to run.

Instead of highlighting errors in the buffer, this command pops
up a separate buffer with the entire output of the syntax checker
tool, just like `compile' (\\[compile])."
  (interactive
   (list (read-flycheck-checker "Run syntax checker as compile command: "
                                (or flycheck-checker flycheck-last-checker))))
  (unless (flycheck-valid-checker-p checker)
    (user-error "%S is not a valid syntax checker" checker))
  (unless (buffer-file-name)
    (user-error "Cannot compile buffers without backing file"))
  (unless (flycheck-may-use-checker checker)
    (user-error "Cannot use syntax checker %S in this buffer" checker))
  (let* ((command (flycheck-checker-shell-command checker))
         (buffer (compilation-start command nil #'flycheck-compile-name)))
    (with-current-buffer buffer
      (set (make-local-variable 'compilation-error-regexp-alist)
           (flycheck-checker-compilation-error-regexp-alist checker)))))


;;; Documentation
;;;###autoload
(defun flycheck-info ()
  "Open the Flycheck manual."
  (interactive)
  (info "flycheck"))

(define-button-type 'help-flycheck-checker-def
  :supertype 'help-xref
  'help-function 'flycheck-goto-checker-definition
  'help-echo (purecopy "mouse-2, RET: find Flycheck checker definition"))

(defconst flycheck-find-checker-regexp
  (rx line-start (zero-or-more (syntax whitespace))
      "(" symbol-start "flycheck-define-checker" symbol-end
      (eval (list 'regexp find-function-space-re))
      symbol-start
      "%s"
      symbol-end
      (or (syntax whitespace) line-end))
  "Regular expression to find a checker definition.")

(add-to-list 'find-function-regexp-alist
             '(flycheck-checker . flycheck-find-checker-regexp))

(defun flycheck-goto-checker-definition (checker file)
  "Go to to the definition of CHECKER in FILE."
  (let ((location (find-function-search-for-symbol
                   checker 'flycheck-checker file)))
    (pop-to-buffer (car location))
    (if (cdr location)
        (goto-char (cdr location))
      (message "Unable to find checker location in file"))))

(defun flycheck-checker-at-point ()
  "Return the Flycheck checker found at or before point.

Return 0 if there is no checker."
  (let ((symbol (variable-at-point 'any-symbol)))
    (if (and (symbolp symbol) (flycheck-valid-checker-p symbol))
        symbol
      0)))

(defun flycheck-describe-checker (checker)
  "Display the documentation of CHECKER.

CHECKER is a checker symbol.

Pop up a help buffer with the documentation of CHECKER."
  (interactive
   (let* ((checker (flycheck-checker-at-point))
          (enable-recursive-minibuffers t)
          (prompt (if (symbolp checker)
                      (format "Describe syntax checker (default %s): " checker)
                    "Describe syntax checker: "))
          (reply (read-flycheck-checker prompt)))
     (list (or reply checker))))
  (if (or (null checker) (not (flycheck-valid-checker-p checker)))
      (message "You didn't specify a Flycheck syntax checker.")
    (help-setup-xref (list #'flycheck-describe-checker checker)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (let ((executable (flycheck-checker-default-executable checker))
              (filename (flycheck-checker-file checker))
              (modes (flycheck-checker-modes checker))
              (config-file-var (flycheck-checker-config-file-var checker))
              (option-vars (-sort #'string<
                                  (flycheck-checker-option-vars checker))))
          (princ (format "%s is a Flycheck syntax checker" checker))
          (when filename
            (princ (format " in `%s'" (file-name-nondirectory filename)))
            (with-current-buffer standard-output
              (save-excursion
                (re-search-backward "`\\([^`']+\\)'" nil t)
                (help-xref-button 1 'help-flycheck-checker-def checker filename))))
          (princ ".\n\n")
          (princ (format "  This syntax checker executes \"%s\"" executable))
          (if config-file-var
              (princ (format ", using a configuration file from `%s'.\n"
                             config-file-var))
            (princ ".\n"))
          (when modes
            (princ (format "  It checks syntax in the major mode(s) %s. "
                           (string-join
                            (mapcar (apply-partially #'format "`%s'") modes)
                            ", "))))
          (with-current-buffer (help-buffer)
            (save-excursion
              (goto-char (point-min))
              (forward-paragraph)
              (fill-region-as-paragraph (point) (point-max))))
          (princ "\n\n")
          (princ (format "  The executable can be overridden with `%s'."
                         (flycheck-checker-executable-variable checker)))
          (princ "\n")
          (when option-vars
            (princ "\n  This syntax checker can be configured with these options:\n\n")
            (dolist (var option-vars)
              (princ (format "     * `%s'\n" var)))))
        (princ (format "\nDocumentation:\n%s"
                       (flycheck-checker-documentation checker)))))))


;;; Checker error API
(cl-defstruct (flycheck-error
               (:constructor flycheck-error-new)
               (:constructor flycheck-error-new-at (line column
                                                         &optional level message
                                                         &key checker
                                                         (filename (buffer-file-name))
                                                         (buffer (current-buffer)))))
  "Structure representing an error reported by a syntax checker.
Slots:

`buffer'
     The buffer the reported was reported for, as buffer object.

`checker'
     The syntax checker which reported this error, as symbol.

`filename'
     The file name the error refers to, as string.

`line'
     The line number the error refers to, as number.

`column'
     The column number the error refers to, as number.

`level'
     The error level, as either `warning' or `error'."
  buffer checker filename line column message level)

(defmacro flycheck-error-with-buffer (err &rest forms)
  "Switch to the buffer of ERR and evaluate FORMS.

If the buffer of ERR is not live, FORMS are not evaluated."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (flycheck-error-buffer ,err))
     (with-current-buffer (flycheck-error-buffer ,err)
       ,@forms)))

(defun flycheck-error-line-region (err)
  "Get the line region of ERR.

ERR is a Flycheck error whose region to get.

Return a cons cell `(BEG . END)' where BEG is the first
non-whitespace character on the line ERR refers to, and END the
end of the line."
  (flycheck-error-with-buffer err
    (save-restriction
      (save-excursion
        (widen)
        (goto-char (point-min))
        (forward-line (- (flycheck-error-line err) 1))
        ;; We are at the beginning of the line now, so move to the beginning of
        ;; its indentation, similar to `back-to-indentation'
        (let ((end (line-end-position)))
          (skip-syntax-forward " " end)
          (backward-prefix-chars)
          ;; If the current line is empty, include the previous line break
          ;; character(s) to have any region at all.  When called with 0,
          ;; `line-end-position' gives us the end of the previous line
          (cons (if (eolp) (line-end-position 0) (point)) end))))))

(defun flycheck-error-column-region (err)
  "Get the error column region of ERR.

ERR is a Flycheck error whose region to get.

Return a cons cell `(BEG . END)' where BEG is the character
before the error column, and END the actual error column, or nil
if ERR has no column."
  (flycheck-error-with-buffer err
    (save-restriction
      (save-excursion
        (-when-let (column (flycheck-error-column err))
          (widen)
          (goto-char (point-min))
          (forward-line (- (flycheck-error-line err) 1))
          (let ((line (flycheck-error-line err)))
            (cond
             ((eobp)                    ; Line beyond EOF
              ;; If we are at the end of the file (i.e. the line was beyond the
              ;; end of the file), use the very last column in the file.
              (cons (- (point-max) 1) (point-max)))
             ((eolp)                    ; Empty line
              ;; If the target line is empty, there's no column to highlight on
              ;; this line, so return the last column of the previous line.
              (cons (line-end-position 0) (point)))
             (t
              ;; The end is either the column offset of the line, or the end of
              ;; the line, if the column offset points beyond the end of the
              ;; line.
              (let ((end (min (+ (point) column)
                              (+ (line-end-position) 1))))
                (cons (- end 1) end))))))))))

(defun flycheck-error-thing-region (thing err)
  "Get the region of THING at the column of ERR.

ERR is a Flycheck error whose region to get.  THING is a
understood by `thing-at-point'.

Return a cons cell `(BEG . END)' where BEG is the beginning of
the THING at the error column, and END the end of the symbol.  If
ERR has no error column, or if there is no THING at this column,
return nil."
  (-when-let (column (car (flycheck-error-column-region err)))
    (flycheck-error-with-buffer err
      (save-excursion
        (save-restriction
          (widen)
          (goto-char column)
          (bounds-of-thing-at-point thing))))))

(defun flycheck-error-region-for-mode (err mode)
  "Get the region of ERR for the highlighting MODE.

ERR is a Flycheck error.  MODE may be one of the following symbols:

`columns'
     Get the column region of ERR, or the line region if ERR
     has no column.

`symbols'
     Get the symbol region of ERR, or the result of `columns', if
     there is no sexp at the error column.

`sexps'
     Get the sexp region of ERR, or the result of `columns', if
     there is no sexp at the error column.

`lines'
     Return the line region.

Otherwise signal an error."
  (pcase mode
    (`columns (or (flycheck-error-column-region err)
                  (flycheck-error-line-region err)))
    (`symbols (or (flycheck-error-thing-region 'symbol err)
                  (flycheck-error-region-for-mode err 'columns)))
    (`sexps (or (flycheck-error-thing-region 'sexp err)
                (flycheck-error-region-for-mode err 'columns)))
    (`lines (flycheck-error-line-region err))
    (_ (error "Invalid mode %S" mode))))

(defun flycheck-error-pos (err)
  "Get the buffer position of ERR.

ERR is a Flycheck error whose position to get.

The error position is the error column, or the first
non-whitespace character of the error line, if ERR has no error column."
  (car (or (flycheck-error-column-region err)
           (flycheck-error-line-region err))))

(defun flycheck-error-format (err)
  "Format ERR as human-readable string.

Return a string that represents the given ERR.  This string does
_not_ include the file name."
  (let* ((line (flycheck-error-line err))
         (column (flycheck-error-column err))
         (level (symbol-name (flycheck-error-level err)))
         (checker (symbol-name (flycheck-error-checker err)))
         (message (flycheck-error-message err))
         (format `(,(number-to-string line) ":"
                   ,@(when column (list (number-to-string column) ":"))
                   ,level ": "
                   ,message " (" ,checker ")")))
    (apply #'concat format)))

(defun flycheck-error-< (err1 err2)
  "Determine whether ERR1 goes before ERR2 by location.

Compare by line numbers and then by column numbers."
  (let ((line1 (flycheck-error-line err1))
        (line2 (flycheck-error-line err2)))
    (if (= line1 line2)
        (let ((col1 (flycheck-error-column err1))
              (col2 (flycheck-error-column err2)))
          (and col2
               ;; Sort errors for the whole line first
               (or (not col1) (< col1 col2))))
      (< line1 line2))))

(defun flycheck-error-level-< (err1 err2)
  "Determine whether ERR1 goes before ERR2 by error level.

Like `flycheck-error-<', but compares by error level severity
first.  Levels of the same severity are compared by name."
  (let* ((level1 (flycheck-error-level err1))
         (level2 (flycheck-error-level err2))
         (severity1 (flycheck-error-level-severity level1))
         (severity2 (flycheck-error-level-severity level2)))
    (cond
     ((= severity1 severity2)
      (if (string= level1 level2)
          (flycheck-error-< err1 err2)
        (string< level1 level2)))
     (t (< severity1 severity2)))))


;;; Error levels

;;;###autoload
(defun flycheck-define-error-level (level &rest properties)
  "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Other useful
     properties for error level categories are `priority' to
     influence the stacking of multiple error level overlays, and
     `help-echo' to define a default error messages for errors
     without messages.

`:fringe-bitmap BITMAP'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level.  See Info node `(elisp)Fringe
     Bitmaps' for more information about fringe bitmaps,
     including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'."
  (declare (indent 1))
  (put level 'flycheck-error-level t)
  (put level 'flycheck-error-severity
       (or (plist-get properties :severity) 0))
  (put level 'flycheck-overlay-category
       (plist-get properties :overlay-category))
  (put level 'flycheck-fringe-bitmap
       (plist-get properties :fringe-bitmap))
  (put level 'flycheck-fringe-face
       (plist-get properties :fringe-face))
  (put level 'flycheck-error-list-face
       (plist-get properties :error-list-face)))

(defun flycheck-error-level-p (level)
  "Determine whether LEVEL is a Flycheck error level."
  (get level 'flycheck-error-level))

(defun flycheck-error-level-severity (level)
  "Get the numeric severity of LEVEL."
  (or (get level 'flycheck-error-severity) 0))

(defun flycheck-error-level-overlay-category (level)
  "Get the overlay category for LEVEL."
  (get level 'flycheck-overlay-category))

(defun flycheck-error-level-fringe-bitmap (level)
  "Get the fringe bitmap for LEVEL."
  (get level 'flycheck-fringe-bitmap))

(defun flycheck-error-level-fringe-face (level)
  "Get the fringe face for LEVEL."
  (get level 'flycheck-fringe-face))

(defun flycheck-error-level-error-list-face (level)
  "Get the error list face for LEVEL."
  (get level 'flycheck-error-list-face))

(defun flycheck-error-level-make-fringe-icon (level side)
  "Create the fringe icon for LEVEL at SIDE.

Return a propertized string that shows a fringe bitmap according
to LEVEL and the given fringe SIDE.

LEVEL is a Flycheck error level defined with
`flycheck-define-error-level', and SIDE is either `left-fringe'
or `right-fringe'.

Return a propertized string representing the fringe icon,
intended for use as `before-string' of an overlay to actually
show the icon."
  (unless (memq side '(left-fringe right-fringe))
    (error "Invalid fringe side: %S" side))
  (propertize "!" 'display
              (list side
                    (flycheck-error-level-fringe-bitmap level)
                    (flycheck-error-level-fringe-face level))))


;;; Built-in error levels
(put 'flycheck-error-overlay 'face 'flycheck-error)
(put 'flycheck-error-overlay 'priority 110)
(put 'flycheck-error-overlay 'help-echo "Unknown error.")

(flycheck-define-error-level 'error
  :severity 100
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'exclamation-mark
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)

(put 'flycheck-warning-overlay 'face 'flycheck-warning)
(put 'flycheck-warning-overlay 'priority 100)
(put 'flycheck-warning-overlay 'help-echo "Unknown warning.")

(flycheck-define-error-level 'warning
  :severity 10
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'question-mark
  :fringe-face 'flycheck-fringe-warning
  :error-list-face 'flycheck-error-list-warning)

(put 'flycheck-info-overlay 'face 'flycheck-info)
(put 'flycheck-info-overlay 'priority 90)
(put 'flycheck-info-overlay 'help-echo "Unknown info.")

(flycheck-define-error-level 'info
  :severity -1
  :overlay-category 'flycheck-info-overlay
  ;; Not exactly the right indicator, but looks pretty, and I prefer to use
  ;; built-in bitmaps over diving into the hassle of messing around with custom
  ;; fringe bitmaps
  :fringe-bitmap 'empty-line
  :fringe-face 'flycheck-fringe-info
  :error-list-face 'flycheck-error-list-info)


;;; General error parsing
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let* ((parser (or (flycheck-checker-error-parser checker)
                     #'flycheck-parse-with-patterns))
         (errors (funcall parser output checker buffer)))
    (dolist (err errors)
      ;; Remember the source buffer and checker in the error
      (setf (flycheck-error-buffer err) buffer)
      (setf (flycheck-error-checker err) checker))
    errors))

(defun flycheck-fix-error-filename (err buffer-files)
  "Fix the file name of ERR from BUFFER-FILES.

Make the file name of ERR absolute.  If the absolute file name of
ERR is in BUFFER-FILES, replace it with the return value of the
function `buffer-file-name'."
  (flycheck-error-with-buffer err
    (-when-let (filename (flycheck-error-filename err))
      (setq filename (expand-file-name filename))
      (when (-any? (apply-partially #'flycheck-same-files-p filename)
                   buffer-files)
        (setq filename (buffer-file-name)))
      (setf (flycheck-error-filename err) filename)))
  err)


;;; Error parsing with regular expressions
(defun flycheck-get-regexp (patterns)
  "Create a single regular expression from PATTERNS."
  (rx-to-string `(or ,@(mapcar (lambda (p) (list 'regexp (car p))) patterns))
                'no-group))

(defun flycheck-tokenize-output-with-patterns (output patterns)
  "Tokenize OUTPUT with PATTERNS.

Split the output into error tokens, using all regular expressions
from the error PATTERNS.  An error token is simply a string
containing a single error from OUTPUT.  Such a token can then be
parsed into a structured error by applying the PATTERNS again,
see `flycheck-parse-errors-with-patterns'.

Return a list of error tokens."
  (let ((regexp (flycheck-get-regexp patterns))
        (last-match 0)
        errors)
    (while (string-match regexp output last-match)
      (push (match-string 0 output) errors)
      (setq last-match (match-end 0)))
    (reverse errors)))

(defun flycheck-try-parse-error-with-pattern (err pattern)
  "Try to parse a single ERR with a PATTERN.

Return the parsed error if PATTERN matched ERR, or nil
otherwise."
  (let ((regexp (car pattern))
        (level (cdr pattern)))
    (when (string-match regexp err)
      (let ((filename (match-string 1 err))
            (line (match-string 2 err))
            (column (match-string 3 err))
            (message (match-string 4 err)))
        (flycheck-error-new
         :filename (unless (string-empty-p filename) filename)
         :line (flycheck-string-to-number-safe line)
         :column (flycheck-string-to-number-safe column)
         :message (unless (string-empty-p message) message)
         :level level)))))

(defun flycheck-parse-error-with-patterns (err patterns)
  "Parse a single ERR with error PATTERNS.

Apply each pattern in PATTERNS to ERR, in the given order, and
return the first parsed error."
  ;; Try to parse patterns in the order of declaration to make sure that the
  ;; first match wins.
  (let (parsed-error)
    (while (and patterns
                (not (setq parsed-error
                           (flycheck-try-parse-error-with-pattern
                            err (car patterns)))))
      (setq patterns (cdr patterns)))
    parsed-error))

(defun flycheck-parse-errors-with-patterns (errors patterns)
  "Parse ERRORS with PATTERNS.

ERRORS is a list of strings where each string is an unparsed
error message, typically from `flycheck-split-output'.  PATTERNS
is a list of error patterns to parse ERRORS with.

Return a list of parsed errors."
  (mapcar (lambda (e) (flycheck-parse-error-with-patterns e patterns)) errors))

(defun flycheck-parse-with-patterns (output checker _buffer)
  "Parse OUTPUT from CHECKER with error patterns.

Uses the error patterns of CHECKER to tokenize the output and
tries to parse each error token with all patterns, in the order
of declaration.  Hence an error is never matched twice by two
different patterns.  The pattern declared first always wins.

_BUFFER is ignored.

Return a list of parsed errors and warnings (as `flycheck-error'
objects)."
  (let ((patterns (flycheck-checker-error-patterns checker)))
    (flycheck-parse-errors-with-patterns
     (flycheck-tokenize-output-with-patterns output patterns) patterns)))


;;; Error parsers
(defun flycheck-parse-xml-region (beg end)
  "Parse the xml region between BEG and END.

Wrapper around `xml-parse-region' which transforms the return
value of this function into one compatible to
`libxml-parse-xml-region' by simply returning the first element
from the node list."
  (car (xml-parse-region beg end)))

(defvar flycheck-xml-parser
  (if (fboundp 'libxml-parse-xml-region)
      'libxml-parse-xml-region 'flycheck-parse-xml-region)
  "Parse an xml string from a region.

Use libxml if Emacs is built with libxml support.  Otherwise fall
back to `xml-parse-region', via `flycheck-parse-xml-region'.")

(defun flycheck-parse-xml-string (xml)
  "Parse an XML string.

Return the document tree parsed from XML in the form `(ROOT ATTRS
BODY...)'.  ROOT is a symbol identifying the name of the root
element.  ATTRS is an alist of the attributes of the root node.
BODY is zero or more body elements, either as strings (in case of
text nodes) or as XML nodes, in the same for as the root node."
  (with-temp-buffer
    (insert xml)
    (funcall flycheck-xml-parser (point-min) (point-max))))

(defun flycheck-parse-checkstyle-file-node (node)
  "Parse a single file NODE in a Checkstyle document.

Return a list of all errors contained in the NODE, or nil if NODE
is not a file node."
  (let ((filename (cdr (assq 'name (cadr node))))
        errors)
    (dolist (child-node (cddr node))
      (when (and (listp child-node) (eq (car child-node) 'error))
        (let* ((attrs (cadr child-node))
               (line (flycheck-string-to-number-safe
                      (cdr (assq 'line attrs))))
               (column (flycheck-string-to-number-safe
                        (cdr (assq 'column attrs))))
               (severity (cdr (assq 'severity attrs)))
               (message (cdr (assq 'message attrs))))
          (push (flycheck-error-new
                 :filename filename
                 :line line
                 :column (when (and column (> column 0)) column)
                 :message message
                 :level (pcase severity
                          (`"error"   'error)
                          (`"warning" 'warning)
                          (`"info"    'info)
                          ;; Default to error for unknown severity
                          (_          'error)))
                errors))))
    (nreverse errors)))

(defun flycheck-parse-checkstyle (output _checker _buffer)
  "Parse Checkstyle errors from OUTPUT.

Parse Checkstyle-like XML output.  Use this error parser for
checkers that have an option to output errors in this format.

_CHECKER and _BUFFER are ignored.

See URL `http://checkstyle.sourceforge.net/' for information
about Checkstyle."
  (let ((root (flycheck-parse-xml-string output)))
    (when (eq (car root) 'checkstyle)
      (-mapcat #'flycheck-parse-checkstyle-file-node
               (-filter (lambda (n) (and (listp n) (eq (car n) 'file)))
                        (cddr root))))))

(defun flycheck-parse-cppcheck-error-node (node)
  "Parse a single error NODE from Cppcheck XML.

Return a list of all Flycheck errors this node represents."
  (let ((node-attrs (cadr node))
        errors)
    (dolist (child-node (cddr node))
      (when (and (listp child-node) (eq (car child-node) 'location))
        (let ((child-attrs (cadr child-node)))
          (push (flycheck-error-new
                 :filename (cdr (assq 'file child-attrs))
                 :line (flycheck-string-to-number-safe
                        (cdr (assq 'line child-attrs)))
                 :message (cdr (assq 'verbose node-attrs))
                 :level (if (string= (cdr (assq 'severity node-attrs)) "error")
                            'error 'warning))
                errors))))
    (nreverse errors)))

(defun flycheck-parse-cppcheck (output _checker _buffer)
  "Parse Cppcheck errors from OUTPUT.

Parse Cppcheck XML v2 output.

_BUFFER and _ERROR are ignored.

See URL `http://cppcheck.sourceforge.net/' for more information
about Cppcheck."
  (let ((root (flycheck-parse-xml-string output)))
    (when (eq (car root) 'results)
      (let ((nodes (cddr root)))
        (while (and nodes
                    (not (and (listp (car nodes)) (eq (caar nodes) 'errors))))
          (setq nodes (cdr nodes)))
        ;; Filter error nodes
        (-mapcat #'flycheck-parse-cppcheck-error-node
                 (-filter (lambda (n) (and (listp n) (eq (car n) 'error)))
                          (car nodes)))))))


;;; Error filtering

(defun flycheck-filter-errors (errors checker)
  "Filter ERRORS from CHECKER.

Apply the error filter of CHECKER to ERRORs and return the
result.  If CHECKER has no error filter, fall back to
`flycheck-sanitize-errors'."
  (let ((filter (or (flycheck-checker-error-filter checker)
                    #'flycheck-sanitize-errors)))
    (funcall filter errors)))

(defun flycheck-sanitize-errors (errors)
  "Sanitize ERRORS.

Sanitize ERRORS by trimming leading and trailing whitespace in
all error messages, and by replacing 0 columns and empty error
messages with nil.

Returns sanitized ERRORS."
  (dolist (err errors)
    (flycheck-error-with-buffer err
      (let ((message (flycheck-error-message err))
            (column (flycheck-error-column err)))
        (when message
          (setq message (string-trim message))
          (setf (flycheck-error-message err)
                (if (string-empty-p message) nil message)))
        (when (eq column 0)
          (setf (flycheck-error-column err) nil)))))
  errors)

(defun flycheck-collapse-error-message-whitespace (errors)
  "Collapse whitespace in all messages of ERRORS.

Return ERRORS."
  (dolist (err errors)
    (-when-let (message (flycheck-error-message err))
      (setf (flycheck-error-message err)
            (replace-regexp-in-string (rx (one-or-more (any space "\n" "\r")))
                                      " " message 'fixed-case 'literal))))
  errors)

(defun flycheck-dedent-error-messages (errors)
  "Dedent all messages of ERRORS.

For each error in ERRORS, determine the indentation offset from
the leading whitespace of the first line, and dedent all further
lines accordingly.

Return ERRORS, with in-place modifications."
  (dolist (err errors)
    (-when-let (message (flycheck-error-message err))
      (with-temp-buffer
        (insert message)
        ;; Determine the indentation offset
        (goto-char (point-min))
        (back-to-indentation)
        (let* ((indent-offset (- (point) (point-min))))
          ;; Now iterate over all lines and dedent each according to
          ;; `indent-offset'
          (while (not (eobp))
            (back-to-indentation)
            ;; If the current line starts with sufficient whitespace, delete the
            ;; indendation offset.  Otherwise keep the line intact, as we might
            ;; loose valuable information
            (when (>= (- (point) (line-beginning-position)) indent-offset)
              (delete-char (- indent-offset)))
            (forward-line 1)))
        (delete-trailing-whitespace (point-min) (point-max))
        (setf (flycheck-error-message err)
              (buffer-substring-no-properties (point-min) (point-max))))))
  errors)

(defun flycheck-fold-include-errors (errors sentinel-message)
  "Fold errors from included files.

ERRORS is a list of errors in which to fold errors.
SENTINEL-MESSAGE is the error message which denotes an error on
an include.

The function will fold the messages of all subsequent errors in
the included file into the error on the include.

Returns ERRORS, with folded messages."
  ;; Fold messages from faulty includes into the errors on the corresponding
  ;; include lines.  The user still needs to visit the affected include to
  ;; list and navigate these errors, but they can at least get an idea of
  ;; what is wrong.
  (let (including-filename              ; The name of the file including a
                                        ; faulty include
        include-error                   ; The error on the include line
        errors-in-include)              ; All errors in the include, as strings
    (dolist (err errors)
      (-when-let* ((message (flycheck-error-message err))
                   (filename (flycheck-error-filename err)))
        (cond
         ((and (string= message sentinel-message)
               ;; Don't handle faulty includes recursively, we are only
               ;; interested in “top-level” errors
               (not including-filename))
          ;; We are looking at an error denoting a faulty include, so let's
          ;; remember the error and the name of the include, and initialize
          ;; our folded error message
          (setq include-error err
                including-filename filename
                errors-in-include (list "Errors in included file:")))
         ((and include-error (not (string= filename including-filename)))
          ;; We are looking at an error *inside* the last faulty include, so
          ;; let's record err, as human-readable string
          (push (flycheck-error-format err) errors-in-include))
         (include-error
          ;; We are looking at an unrelated error, so fold all include
          ;; errors, if there are any
          (when (and include-error errors-in-include)
            (setf (flycheck-error-message include-error)
                  (string-join (nreverse errors-in-include) "\n")))
          (setq include-error nil
                including-filename nil
                errors-in-include nil)))))
    ;; If there are still pending errors to be folded, do so now
    (when (and include-error errors-in-include)
      (setf (flycheck-error-message include-error)
            (string-join (nreverse errors-in-include) "\n"))))
  errors)


;;; Error analysis
(defvar-local flycheck-current-errors nil
  "A list of all errors and warnings in the current buffer.")

(defun flycheck-clear-errors ()
  "Remove all error information from the current buffer."
  (setq flycheck-current-errors nil)
  (flycheck-report-status 'not-checked))

(defun flycheck-relevant-error-p (err)
  "Determine whether ERR is relevant for the current buffer.

Return t if ERR may be shown for the current buffer, or nil
otherwise."
  (flycheck-error-with-buffer err
    (let ((file-name (flycheck-error-filename err))
          (message (flycheck-error-message err)))
      (and
       (or (not file-name) (flycheck-same-files-p file-name (buffer-file-name)))
       message
       (not (string-empty-p message))
       (flycheck-error-line err)))))

(defun flycheck-relevant-errors (errors)
  "Filter the relevant errors from ERRORS.

Return a list of all errors that are relevant for their
corresponding buffer."
  (-filter #'flycheck-relevant-error-p errors))

(defun flycheck-count-errors (errors)
  "Count the number of ERRORS, grouped by level..

Return an alist, where each ITEM is a cons cell whose `car' is an
error level, and whose `cdr' is the number of errors of that
level."
  (let (counts-by-level)
    (dolist (err errors)
      (let* ((level (flycheck-error-level err))
             (item (assq level counts-by-level)))
        (if item
            (setcdr item (1+ (cdr item)))
          (push (cons level 1) counts-by-level))))
    counts-by-level))

(defun flycheck-has-max-errors-p (errors level)
  "Check if there is no error in ERRORS more severe than LEVEL."
  (let ((severity (flycheck-error-level-severity level))
        found)
    (while (and errors (not found))
      (setq found (< severity (flycheck-error-level-severity
                               (flycheck-error-level (pop errors))))))
    (not found)))

(defun flycheck-has-max-current-errors-p (level)
  "Check if there is no current error more severe than LEVEL."
  (flycheck-has-max-errors-p flycheck-current-errors level))

(defun flycheck-has-errors-p (errors level)
  "Determine if there are any ERRORS with LEVEL."
  (let (found)
    (while (and errors (not found))
      (setq found (eq (flycheck-error-level (pop errors)) level)))
    found))

(defun flycheck-has-current-errors-p (&optional level)
  "Determine if the current buffer has errors with LEVEL.

If LEVEL is omitted if the current buffer has any errors at all."
  (if level
      (flycheck-has-errors-p flycheck-current-errors level)
    (and flycheck-current-errors t)))


;;; Error overlay management
(defun flycheck-add-overlay (err)
  "Add overlay for ERR.

Return the created overlay."
  ;; We must have a proper error region for the sake of fringe indication,
  ;; error display and error navigation, even if the highlighting is disabled.
  ;; We erase the highlighting later on in this case
  (pcase-let* ((`(,beg . ,end) (flycheck-error-region-for-mode
                                err (or flycheck-highlighting-mode 'lines)))
               (overlay (make-overlay beg end))
               (level (flycheck-error-level err))
               (category (flycheck-error-level-overlay-category level)))
    (unless (flycheck-error-level-p level)
      (error "Undefined error level: %S" level))
    (overlay-put overlay 'flycheck-overlay t)
    (overlay-put overlay 'flycheck-error err)
    ;; TODO: Consider hooks to re-check if overlay contents change
    (overlay-put overlay 'category category)
    (unless flycheck-highlighting-mode
      ;; Erase the highlighting from the overlay if requested by the user
      (overlay-put overlay 'face nil))
    (when flycheck-indication-mode
      (overlay-put overlay 'before-string
                   (flycheck-error-level-make-fringe-icon
                    level flycheck-indication-mode)))
    (overlay-put overlay 'help-echo (flycheck-error-message err))
    overlay))

(defun flycheck-filter-overlays (overlays)
  "Get all Flycheck overlays from OVERLAYS."
  (-filter (lambda (o) (overlay-get o 'flycheck-overlay)) overlays))

(defun flycheck-overlays-at (pos)
  "Get all Flycheck overlays at POS."
  (flycheck-filter-overlays (overlays-at pos)))

(defun flycheck-overlays-in (beg end)
  "Get all Flycheck overlays between BEG and END."
  (flycheck-filter-overlays (overlays-in beg end)))

(defun flycheck-overlay-errors-at (pos)
  "Return a list of all flycheck errors overlayed at POS."
  (mapcar (lambda (o) (overlay-get o 'flycheck-error))
          (flycheck-overlays-at pos)))

(defun flycheck-overlay-errors-in (beg end)
  "Return a list of all flycheck errors overlayed between BEG and END."
  (mapcar (lambda (o) (overlay-get o 'flycheck-error))
          (flycheck-overlays-in beg end)))

(defvar-local flycheck-overlays-to-delete nil
  "Overlays mark for deletion after all syntax checks completed.")
(put 'flycheck-overlays-to-delete 'permanent-local t)

(defun flycheck-delete-all-overlays ()
  "Remove all flycheck overlays in the current buffer."
  (flycheck-delete-marked-overlays)
  (save-restriction
    (widen)
    (mapc #'delete-overlay (flycheck-overlays-in (point-min) (point-max)))))

(defun flycheck-mark-all-overlays-for-deletion ()
  "Mark all current overlays for deletion."
  (setq flycheck-overlays-to-delete
        (append (flycheck-overlays-in (point-min) (point-max))
                flycheck-overlays-to-delete)))

(defun flycheck-delete-marked-overlays ()
  "Delete all overlays marked for deletion."
  (mapc #'delete-overlay flycheck-overlays-to-delete)
  (setq flycheck-overlays-to-delete nil))


;;; Error navigation
(defun flycheck-error-level-interesting-at-pos-p (pos)
  "Check if error severity at POS passes `flycheck-error-level-interesting-p'."
  (flycheck-error-level-interesting-p (get-char-property pos 'flycheck-error)))

(defun flycheck-error-level-interesting-p (err)
  "Check if ERR severity is >= `flycheck-navigation-minimum-level'."
  (when (flycheck-error-p err)
    (-if-let (min-level flycheck-navigation-minimum-level)
        (<= (flycheck-error-level-severity min-level)
            (flycheck-error-level-severity (flycheck-error-level err)))
      t)))

(defun flycheck-next-error-pos (n &optional reset)
  "Get the position of the N-th next error.

With negative n, get the position of the (-N)-th previous error
instead.  With non-nil RESET, search from `point-min', otherwise
search from the current point.

Return the position of the next or previous error, or nil if
there is none."
  (let ((n (or n 1))
        (pos (if reset (point-min) (point))))
    (if (>= n 0)
        ;; Search forwards
        (while (and pos (> n 0))
          (setq n (1- n))
          (when (get-char-property pos 'flycheck-error)
            ;; Move beyond from the current error if any
            (setq pos (next-single-char-property-change pos 'flycheck-error)))
          (while (not (or (= pos (point-max))
                          (flycheck-error-level-interesting-at-pos-p pos)))
            ;; Scan for the next error
            (setq pos (next-single-char-property-change pos 'flycheck-error)))
          (when (and (= pos (point-max))
                     (not (flycheck-error-level-interesting-at-pos-p pos)))
            ;; If we reached the end of the buffer, but no error, we didn't find
            ;; any
            (setq pos nil)))
      ;; Search backwards
      (while (and pos (< n 0))
        (setq n (1+ n))
        ;; Loop until we find an error.  We need to check the position *before*
        ;; the current one, because `previous-single-char-property-change'
        ;; always moves to the position *of* the change.
        (while (not (or (= pos (point-min))
                        (flycheck-error-level-interesting-at-pos-p (1- pos))))
          (setq pos (previous-single-char-property-change pos 'flycheck-error)))
        (when (and (= pos (point-min))
                   (not (flycheck-error-level-interesting-at-pos-p pos)))
          ;; We didn't find any error.
          (setq pos nil))
        (when pos
          ;; We found an error, so move to its beginning
          (setq pos (previous-single-char-property-change pos
                                                          'flycheck-error)))))
    pos))

(defun flycheck-next-error-function (n reset)
  "Visit the N-th error from the current point.

Intended for use with `next-error-function'."
  (let ((pos (flycheck-next-error-pos n reset)))
    (if pos
        (goto-char pos)
      (user-error "No more Flycheck errors"))))

(defun flycheck-next-error (&optional n reset)
  "Visit the N-th error from the current point.

If RESET is given and non-nil, re-start from the beginning of the buffer.

N specifies how many errors to move forwards.  If negative, move backwards."
  (interactive "P")
  (when (consp n)
    ;; Universal prefix argument means reset
    (setq reset t n nil))
  (flycheck-next-error-function n reset))

(defun flycheck-previous-error (&optional n)
  "Visit the N-th previous error.

If given, N specifies the number of errors to move backwards.  If
N is negative, move forwards instead."
  (interactive "P")
  (flycheck-next-error (- (or n 1))))

(defun flycheck-first-error (&optional n)
  "Visit the N-th error from beginning of the buffer.

If given, N specifies the number of errors to move forward from
the beginning of the buffer."
  (interactive "P")
  (flycheck-next-error n 'reset))


;;; Error list
(defconst flycheck-error-list-buffer "*Flycheck errors*"
  "The name of the buffer to show error lists.")

(defvar flycheck-error-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'flycheck-error-list-next-error)
    (define-key map (kbd "p") #'flycheck-error-list-previous-error)
    (define-key map (kbd "RET") #'flycheck-error-list-goto-error)
    map)
  "The keymap of `flycheck-error-list-mode'.")

(defconst flycheck-error-list-format
  [("Line" 4 flycheck-error-list-entry-< :right-align t)
   ("Col" 3 nil :right-align t)
   ("Level" 8 flycheck-error-list-entry-level-<)
   ("Message" 0 t)
   (" (Checker)" 8 t)]
  "Table format for the error list.")

(define-derived-mode flycheck-error-list-mode tabulated-list-mode "Flycheck errors"
  "Major mode for listing Flycheck errors.

\\{flycheck-error-list-mode-map}"
  (setq tabulated-list-format flycheck-error-list-format
        ;; Sort by location initially
        tabulated-list-sort-key (cons "Line" nil)
        tabulated-list-padding 1
        tabulated-list-entries #'flycheck-error-list-entries
        ;; `revert-buffer' updates the mode line for us, so all we need to do is
        ;; set the corresponding mode line construct.
        mode-line-buffer-identification flycheck-error-list-mode-line)
  (tabulated-list-init-header))

(defvar-local flycheck-error-list-source-buffer nil
  "The current source buffer of the error list.")
;; Needs to permanently local to preserve the source buffer across buffer
;; reversions
(put 'flycheck-error-list-source-buffer 'permanent-local t)

(defun flycheck-error-list-set-source (buffer)
  "Set BUFFER as the source buffer of the error list."
  (when (get-buffer flycheck-error-list-buffer)
    (with-current-buffer flycheck-error-list-buffer
      ;; Only update the source when required
      (unless (eq buffer flycheck-error-list-source-buffer)
        (setq flycheck-error-list-source-buffer buffer)
        (flycheck-error-list-refresh)))))

(defun flycheck-error-list-update-source ()
  "Update the source buffer of the error list."
  (when (not (eq (current-buffer) (get-buffer flycheck-error-list-buffer)))
    ;; We must not update the source buffer, if the current buffer is the error
    ;; list itself.
    (flycheck-error-list-set-source (current-buffer))))

(define-button-type 'flycheck-error-list
  'action #'flycheck-error-list-button-goto-error
  'help-echo (purecopy "mouse-2, RET: goto error"))

(defun flycheck-error-list-button-goto-error (button)
  "Go to the error at BUTTON."
  (flycheck-error-list-goto-error (button-start button)))

(defsubst flycheck-error-list-make-cell (text &optional face)
  "Make an error list cell with TEXT and FACE."
  (let ((face (or face 'default)))
    (list text 'type 'flycheck-error-list 'face face)))

(defsubst flycheck-error-list-make-number-cell (number face)
  "Make a table cell for a NUMBER with FACE.

Convert NUMBER to string, fontify it with FACE and return the
string with attached text properties."
  (flycheck-error-list-make-cell
   (if (numberp number) (number-to-string number) "")
   face))

(defun flycheck-error-list-make-entry (error)
  "Make a table cell for the given ERROR.

Return a list with the contents of the table cell."
  (let* ((level (flycheck-error-level error))
         (level-face (flycheck-error-level-error-list-face level))
         (line (flycheck-error-line error))
         (column (flycheck-error-column error))
         (message (or (flycheck-error-message error)
                      (format "Unknown %s" (symbol-name level))))
         (checker (flycheck-error-checker error)))
    (list error
          (vector (flycheck-error-list-make-number-cell
                   line 'flycheck-error-list-line-number)
                  (flycheck-error-list-make-number-cell
                   column 'flycheck-error-list-column-number)
                  (flycheck-error-list-make-cell
                   (symbol-name (flycheck-error-level error)) level-face)
                  (flycheck-error-list-make-cell message)
                  (flycheck-error-list-make-cell
                   (format "(%s)" checker)
                   'flycheck-error-list-checker-name)))))

(defun flycheck-error-list-entries ()
  "Create the entries for the error list."
  (when (buffer-live-p flycheck-error-list-source-buffer)
    (let ((errors (buffer-local-value 'flycheck-current-errors
                                      flycheck-error-list-source-buffer)))
      (mapcar #'flycheck-error-list-make-entry errors))))

(defun flycheck-error-list-entry-< (entry1 entry2)
  "Determine whether ENTRY1 is before ENTRY2 by location.

See `flycheck-error-<'."
  (flycheck-error-< (car entry1) (car entry2)))

(defun flycheck-error-list-entry-level-< (entry1 entry2)
  "Determine whether ENTRY1 is before ENTRY2 by level.

See `flycheck-error-level-<'."
  (not (flycheck-error-level-< (car entry1) (car entry2))))

(defvar flycheck-error-list-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      #'flycheck-error-list-mouse-switch-to-source)
    map)
  "Keymap for error list mode line.")

(defun flycheck-error-list-propertized-source-name ()
  "Get the name of the current source buffer for the mode line.

Propertize the name of the current source buffer for use in the
mode line indication of `flycheck-error-list-mode'."
  (let ((name (replace-regexp-in-string
               (rx "%") "%%"
               (buffer-name flycheck-error-list-source-buffer)
               'fixed-case 'literal)))
    (propertize name 'face 'mode-line-buffer-id
                'mouse-face 'mode-line-highlight
                'help-echo "mouse-1: switch to source"
                'local-map flycheck-error-list-mode-line-map)))

(defun flycheck-error-list-mouse-switch-to-source (event)
  "Switch to the error list source buffer of the EVENT window."
  (interactive "e")
  (save-selected-window
    (when (eventp event)
      (select-window (posn-window (event-start event))))
    (when (buffer-live-p flycheck-error-list-source-buffer)
      (switch-to-buffer flycheck-error-list-source-buffer))))

(defun flycheck-get-error-list-window-list (&optional all-frames)
  "Get all windows displaying the error list.

ALL-FRAMES specifies the frames to consider, as in
`get-buffer-window-list'."
  (-when-let (buf (get-buffer flycheck-error-list-buffer))
    (get-buffer-window-list buf nil all-frames)))

(defun flycheck-get-error-list-window (&optional all-frames)
  "Get a window displaying the error list, or nil if none.

ALL-FRAMES specifies the frames to consider, as in
`get-buffer-window'."
  (-when-let (buf (get-buffer flycheck-error-list-buffer))
    (get-buffer-window buf all-frames)))

(defun flycheck-error-list-recenter-at (pos)
  "Recenter the error list at POS."
  (dolist (window (flycheck-get-error-list-window-list t))
    (with-selected-window window
      (goto-char pos)
      (recenter))))

(defun flycheck-error-list-refresh ()
  "Refresh the current error list.

Add all errors currently reported for the current
`flycheck-error-list-source-buffer', and recenter the error
list."
  (interactive)
  ;; We only refresh the error list, when it is visible in a window, and we
  ;; select this window while reverting, because Tabulated List mode attempts to
  ;; recenter the error at the old location, so it must have the proper window
  ;; selected.
  (-when-let (window (flycheck-get-error-list-window t))
    (with-selected-window window
      (revert-buffer))
    (run-hooks 'flycheck-error-list-after-refresh-hook)
    (let ((preserve-pos (eq (current-buffer)
                            (get-buffer flycheck-error-list-buffer))))
      ;; If the error list is the current buffer, don't recenter when
      ;; highlighting
      (flycheck-error-list-highlight-errors preserve-pos))))

(defun flycheck-error-list-goto-error (&optional pos)
  "Go to the location of the error at POS in the error list.

POS defaults to `point'."
  (interactive)
  (-when-let* ((error (tabulated-list-get-id pos))
               (buffer (flycheck-error-buffer error)))
    (when (buffer-live-p buffer)
      (if (eq (window-buffer) (get-buffer flycheck-error-list-buffer))
          ;; When called from within the error list, keep the error list,
          ;; otherwise replace the current buffer.
          (pop-to-buffer buffer 'other-window)
        (switch-to-buffer buffer))
      (let ((pos (flycheck-error-pos error)))
        (unless (eq (goto-char pos) (point))
          ;; If widening gets in the way of moving to the right place, remove it
          ;; and try again
          (widen)
          (goto-char pos)))
      ;; Re-highlight the errors
      (flycheck-error-list-highlight-errors 'preserve-pos))))

(defun flycheck-error-list-next-error-pos (pos &optional n)
  "Get the N'th next error in the error list from POS.

N defaults to 1.  If N is negative, search for the previous error
instead.

Get the beginning position of the N'th next error from POS, or
nil, if there is no next error."
  (let ((n (or n 1)))
    (if (>= n 0)
        ;; Search forward
        (while (and pos (/= n 0))
          (setq n (1- n))
          (setq pos (next-single-property-change pos 'tabulated-list-id)))
      ;; Search backwards
      (while (/= n 0)
        (setq n (1+ n))
        ;; We explicitly give the limit here to explicitly have the minimum
        ;; point returned, to be able to move to the first error (which starts
        ;; at `point-min')
        (setq pos (previous-single-property-change pos 'tabulated-list-id
                                                   nil (point-min)))))
    pos))

(defun flycheck-error-list-previous-error (n)
  "Go to the N'th previous error in the error list."
  (interactive "P")
  (flycheck-error-list-next-error (- (or n 1))))

(defun flycheck-error-list-next-error (n)
  "Go to the N'th next error in the error list."
  (interactive "P")
  (let ((pos (flycheck-error-list-next-error-pos (point) n)))
    (when (and pos (/= pos (point)))
      (goto-char pos)
      (save-selected-window
        ;; Keep the error list selected, so that the user can navigate errors by
        ;; repeatedly pressing n/p, without having to re-select the error list
        ;; window.
        (flycheck-error-list-goto-error)))))

(defvar-local flycheck-error-list-highlight-overlays nil
  "Error highlight overlays in the error list buffer.")
(put 'flycheck-error-list-highlight-overlays 'permanent-local t)

(defun flycheck-error-list-highlight-errors (&optional preserve-pos)
  "Highlight errors in the error list.

Highlight all errors in the error lists that are at point in the
source buffer, and on the same line as point.  Then recenter the
error list to the highlighted error, unless PRESERVE-POS is
non-nil."
  (when (get-buffer flycheck-error-list-buffer)
    (let ((current-errors (flycheck-overlay-errors-in (line-beginning-position)
                                                      (line-end-position))))
      (with-current-buffer flycheck-error-list-buffer
        (let ((old-overlays flycheck-error-list-highlight-overlays)
              (min-point (point-max))
              (max-point (point-min)))
          ;; Display the new overlays first, to avoid re-display flickering
          (setq flycheck-error-list-highlight-overlays nil)
          (when current-errors
            (let ((next-error-pos (point-min)))
              (while next-error-pos
                (let* ((beg next-error-pos)
                       (end (flycheck-error-list-next-error-pos beg))
                       (err (tabulated-list-get-id beg)))
                  (when (member err current-errors)
                    (setq min-point (min min-point beg)
                          max-point (max max-point beg))
                    (let ((ov (make-overlay beg
                                            ;; Extend overlay to the beginning of
                                            ;; the next line, to highlight the
                                            ;; whole line
                                            (or end (point-max)))))
                      (push ov flycheck-error-list-highlight-overlays)
                      (overlay-put ov 'flycheck-error-highlight-overlay t)
                      (overlay-put ov 'face 'flycheck-error-list-highlight)))
                  (setq next-error-pos end)))))
          ;; Delete the old overlays
          (mapc #'delete-overlay old-overlays)
          (when (and (not preserve-pos) current-errors)
            ;; Move point to the middle error
            (goto-char (+ min-point (/ (- max-point min-point) 2)))
            (beginning-of-line)
            ;; And recenter the error list at this position
            (flycheck-error-list-recenter-at (point))))))))

(defun flycheck-list-errors ()
  "Show the error list for the current buffer."
  (interactive)
  (unless flycheck-mode
    (user-error "Flycheck mode not enabled"))
  ;; Create and initialize the error list
  (unless (get-buffer flycheck-error-list-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
      (flycheck-error-list-mode)))
  (flycheck-error-list-set-source (current-buffer))
  ;; Show the error list in a window, and re-select the old window
  (display-buffer flycheck-error-list-buffer)
  ;; Finally, refresh the error list to show the most recent errors
  (flycheck-error-list-refresh))

(defalias 'list-flycheck-errors 'flycheck-list-errors)


;;; Status reporting
(defvar-local flycheck-last-status-change 'not-checked
  "The last status change in the current buffer.")

(defun flycheck-report-error ()
  "Report a Flycheck error status.

Clears all Flycheck errors first, runs
`flycheck-syntax-check-failed-hook' and reports the status with
`flycheck-report-status'."
  (flycheck-clear)
  (run-hooks 'flycheck-syntax-check-failed-hook)
  (flycheck-report-status 'errored))

(defun flycheck-report-status (status)
  "Report Flycheck STATUS.

STATUS is one of the following symbols:

`not-checked'
     The current buffer was not checked.

`no-checker'
     Automatic syntax checker selection did not find a suitable
     syntax checker.

`running'
     A syntax check is now running in the current buffer.

`errored'
     The current syntax check has errored.

`finished'
     The current syntax check was finished normally.

`interrupted'
     The current syntax check was interrupted.

`suspicious'
     The last syntax check had a suspicious result.

Set `flycheck-last-status-change' and call
`flycheck-status-changed-functions' with STATUS.  Afterwards
refresh the mode line."
  (setq flycheck-last-status-change status)
  (run-hook-with-args 'flycheck-status-changed-functions status)
  (force-mode-line-update))


;;; Mode line reporting
(defun flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running "*")
                (`errored "!")
                (`finished
                 (if flycheck-current-errors
                     (let ((error-counts (flycheck-count-errors
                                          flycheck-current-errors)))
                       (format ":%s/%s"
                               (or (cdr (assq 'error error-counts)) 0)
                               (or (cdr (assq 'warning error-counts)) 0)))
                   ""))
                (`interrupted "-")
                (`suspicious "?"))))
    (concat " FlyC" text)))


;;; General error display
(defun flycheck-display-errors (errors)
  "Display ERRORS using `flycheck-display-errors-function'."
  (when flycheck-display-errors-function
    (funcall flycheck-display-errors-function errors)))

(defvar-local flycheck-display-error-at-point-timer nil
  "Timer to automatically show the error at point in minibuffer.")

(defun flycheck-cancel-error-display-error-at-point-timer ()
  "Cancel the error display timer for the current buffer."
  (when flycheck-display-error-at-point-timer
    (cancel-timer flycheck-display-error-at-point-timer)
    (setq flycheck-display-error-at-point-timer nil)))

(defun flycheck-display-error-at-point ()
  "Display the all error messages at point in minibuffer."
  (flycheck-cancel-error-display-error-at-point-timer)
  (when flycheck-mode
    (-when-let (errors (flycheck-overlay-errors-at (point)))
      (flycheck-display-errors errors))))

(defun flycheck-display-error-at-point-soon ()
  "Display the first error message at point in minibuffer delayed."
  (flycheck-cancel-error-display-error-at-point-timer)
  (when (flycheck-overlays-at (point))
    (setq flycheck-display-error-at-point-timer
          (run-at-time flycheck-display-errors-delay nil 'flycheck-display-error-at-point))))


;;; Error display functions
(defconst flycheck-error-message-buffer "*Flycheck error messages*"
  "The name of the buffer to show long error messages in.")

(defun flycheck-error-message-buffer ()
  "Get the buffer object to show long error messages in.

Get the buffer named by variable `flycheck-error-message-buffer',
or nil if the buffer does not exist."
  (get-buffer flycheck-error-message-buffer))

(defun flycheck-may-use-echo-area-p ()
  "Determine whether the echo area may be used.

The echo area may be used if the cursor is not in the echo area,
and if the echo area is not occupied by minibuffer input."
  (not (or cursor-in-echo-area (active-minibuffer-window))))

(defun flycheck-display-error-messages (errors)
  "Display the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `display-message-or-buffer', which
shows the messages either in the echo area or in a separate
buffer, depending on the number of lines.  See Info
node `(elisp)Displaying Messages' for more information.

In the latter case, show messages in
`flycheck-error-message-buffer'."
  (let ((messages (delq nil (mapcar #'flycheck-error-message errors))))
    (when (and errors (flycheck-may-use-echo-area-p))
      (display-message-or-buffer (string-join messages "\n\n")
                                 flycheck-error-message-buffer))))

(defun flycheck-display-error-messages-unless-error-list (errors)
  "Show messages of ERRORS unless the error list is visible.

Like `flycheck-display-error-messages', but only if the error
list (see `flycheck-list-errors') is not visible in any window in
the current frame."
  (unless (flycheck-get-error-list-window 'current-frame)
    (flycheck-display-error-messages errors)))

(defun flycheck-hide-error-buffer ()
  "Hide the Flycheck error buffer if necessary.

Hide the error buffer if there is no error under point."
  (-when-let* ((buffer (flycheck-error-message-buffer))
               (window (get-buffer-window buffer)))
    (unless (flycheck-overlays-at (point))
      (quit-window nil window))))


;;; Working with error messages
(defun flycheck-copy-messages-as-kill (pos)
  "Copy each error message under POS into kill ring.

Each error message under point is copied into the kill ring."
  (interactive "d")
  (let ((messages (delq nil (mapcar #'flycheck-error-message
                                    (flycheck-overlay-errors-at pos)))))
    (when messages
      (mapc #'kill-new (reverse messages))
      (message (string-join messages "\n")))))

(defun flycheck-google-messages (pos &optional quote-flag)
  "Google each error message at POS.

Issue a separate Google query for each error message at POS.
Signal an error if there are more messages at POS than
`flycheck-google-max-messages'.

Enclose the Google query in quotation marks, if
`google-wrap-in-quotes' is t.  With QUOTE-FLAG, invert the effect
of `google-wrap-in-quotes'.

This function requires the Google This library from URL
`https://github.com/Bruce-Connor/emacs-google-this'."
  (interactive "d\nP")
  (if (fboundp 'google-this-string)
      (let ((messages (delq nil (mapcar #'flycheck-error-message
                                        (flycheck-overlay-errors-at pos)))))
        (when (and flycheck-google-max-messages
                   (> (length messages) flycheck-google-max-messages))
          (user-error "More than %s messages at point"
                      flycheck-google-max-messages))
        (dolist (msg messages)
          (google-this-string quote-flag msg 'no-confirm)))
    (user-error "Please install Google This from \
https://github.com/Bruce-Connor/emacs-google-this")))


;;; Checker process management
(defvar-local flycheck-current-process nil
  "The current syntax checking process.")
(put 'flycheck-current-process 'permanent-local t)

(defun flycheck-running-p ()
  "Determine whether a syntax check is running."
  (when (and flycheck-current-process
             (memq (process-status flycheck-current-process) '(exit signal)))
    (flycheck-delete-process flycheck-current-process)
    (setq flycheck-current-process nil))
  (when flycheck-current-process t))

(defun flycheck-delete-process (process)
  "Delete PROCESS and clear it's resources."
  (mapc #'flycheck-safe-delete (process-get process 'flycheck-temporaries))
  (delete-process process))

(defun flycheck-receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (push output (process-get process 'flycheck-pending-output)))

(defun flycheck-get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((pending-output (process-get process 'flycheck-pending-output)))
      (apply #'concat (nreverse pending-output)))))

(defun flycheck-finish-syntax-check (checker exit-status files output)
  "Finish a syntax check from CHECKER with EXIT-STATUS.

FILES is a list of files given as input to the checker.  OUTPUT
is the output of the syntax checker.

Parse the OUTPUT and report an appropriate error status."
  (let (errors)
    (condition-case err
        (setq errors (flycheck-parse-output output checker (current-buffer)))
      (error
       (message "Failed to parse errors from checker %S in output: %s\n\
Error: %s" checker output (error-message-string err))
       (flycheck-report-error)
       (setq errors :errored)))
    (unless (eq errors :errored)
      (when (and (/= exit-status 0) (not errors))
        ;; Warn about a suspicious result from the syntax checker.  We do right
        ;; after parsing the errors, before filtering, because a syntax checker
        ;; might report errors from other files (e.g. includes) even if there
        ;; are no errors in the file being checked.
        (message "Checker %S returned non-zero exit code %s, but no errors from \
output: %s\nChecker definition probably flawed."
                 checker exit-status output)
        (flycheck-report-status 'suspicious))
      (setq errors (flycheck-relevant-errors
                    (flycheck-filter-errors
                     (mapcar (lambda (e) (flycheck-fix-error-filename e files))
                             errors) checker)))
      (unless (flycheck-disable-excessive-checker checker errors)
        ;; Remember and process the new errors if allowed
        (setq flycheck-current-errors
              (sort (append errors flycheck-current-errors) #'flycheck-error-<))
        (mapc (apply-partially #'run-hook-with-args-until-success
                               'flycheck-process-error-functions) errors))
      (flycheck-report-status 'finished)
      (let ((next-checker (flycheck-get-next-checker-for-buffer checker)))
        (if next-checker
            (flycheck-start-checker next-checker)
          (flycheck-delete-marked-overlays)
          (flycheck-error-list-refresh)
          (run-hooks 'flycheck-after-syntax-check-hook)
          (when (eq (current-buffer) (window-buffer))
            (flycheck-display-error-at-point))
          ;; Immediately try to run any pending deferred syntax check, which
          ;; were triggered by intermediate automatic check event, to make sure
          ;; that we quickly refine outdated error information
          (flycheck-perform-deferred-syntax-check))))))

(defun flycheck-disable-excessive-checker (checker errors)
  "Disable CHECKER if it reported excessive ERRORS.

If ERRORS has more items than `flycheck-checker-error-threshold',
add CHECKER to `flycheck-disabled-checkers', and show a warning.

Return t when CHECKER was disabled, or nil otherwise."
  (when (and flycheck-checker-error-threshold
             (> (length errors) flycheck-checker-error-threshold))
    ;; Disable CHECKER for this buffer (`flycheck-disabled-checkers' is a local
    ;; variable).
    (lwarn '(flycheck syntax-checker) :warning
           "Syntax checker %s reported too many errors (%s) and is disabled."
           checker (length errors))
    (push checker flycheck-disabled-checkers)
    t))

(defun flycheck-handle-signal (process _event)
  "Handle a signal from the syntax checking PROCESS.

_EVENT is ignored."
  (when (memq (process-status process) '(signal exit))
    (let ((checker (process-get process 'flycheck-checker))
          (files (process-get process 'flycheck-temporaries))
          (exit-status (process-exit-status process))
          (output (flycheck-get-output process))
          (buffer (process-get process 'flycheck-buffer)))
      (flycheck-delete-process process)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq flycheck-current-process nil)
          (condition-case err
              (pcase (process-status process)
                (`signal
                 ;; The process was killed, so let's just delete all overlays,
                 ;; and report a bad state
                 (flycheck-delete-marked-overlays)
                 (flycheck-report-status 'interrupted))
                (`exit
                 (when flycheck-mode
                   (flycheck-finish-syntax-check checker exit-status
                                                 files output))))
            (error
             (flycheck-report-error)
             (signal (car err) (cdr err)))))))))

(defun flycheck-start-checker (checker)
  "Start a syntax CHECKER."
  (condition-case err
      (let* ((program (flycheck-checker-executable checker))
             (args (flycheck-checker-substituted-arguments checker))
             ;; Use pipes to receive output from the syntax checker.  They are
             ;; more efficient and more robust than PTYs, which Emacs uses by
             ;; default, and since we don't need any job control features, we
             ;; can easily use pipes.
             (process-connection-type nil)
             ;; We pass do not associate the process with any buffer, by passing
             ;; nil for the BUFFER argument of `start-process'.  Instead, we
             ;; just remember the buffer being checked in a process property
             ;; (see below).  This neatly avoids all side-effects implied by
             ;; attached a process to a buffer, which may cause conflicts with
             ;; other packages.
             ;;
             ;; See https://github.com/flycheck/flycheck/issues/298 for an
             ;; example for such a conflict.
             (process (apply 'start-process "flycheck" nil program args)))
        (setq flycheck-current-process process)
        (set-process-filter process 'flycheck-receive-checker-output)
        (set-process-sentinel process 'flycheck-handle-signal)
        (set-process-query-on-exit-flag process nil)
        (flycheck-report-status 'running)
        (process-put process 'flycheck-temporaries flycheck-temporaries)
        ;; Now that temporary files and directories are attached to the process,
        ;; we can reset the variables used to collect them
        (setq flycheck-temporaries nil)
        ;; Remember the syntax checker and the buffer.
        (process-put process 'flycheck-checker checker)
        (process-put process 'flycheck-buffer (current-buffer)))
    (error
     (flycheck-report-error)
     (flycheck-safe-delete-temporaries)
     (when flycheck-current-process
       ;; Clear the process if it's already there
       (flycheck-delete-process flycheck-current-process)
       (setq flycheck-current-process nil))
     (signal (car err) (cdr err)))))

(defun flycheck-stop-checker ()
  "Stop any syntax checker for the current buffer."
  (when (flycheck-running-p)
    ;; Killing the current process will force the sentinel, which does the
    ;; cleanup
    (kill-process flycheck-current-process)))


;;; Syntax checker executable
(defun flycheck-set-checker-executable (checker &optional executable)
  "Set the EXECUTABLE of CHECKER.

CHECKER is a syntax checker symbol.  EXECUTABLE is a string with
the name of a executable or the path to an executable file, which
is to be used as executable for CHECKER.  If omitted or nil,
reset the executable of CHECKER.

Interactively, prompt for a syntax checker and an executable
file, and set the executable of the selected syntax checker.
With prefix arg, prompt for a syntax checker only, and reset the
executable of the select checker to the default.

Set the executable variable of CHECKER, that is,
`flycheck-CHECKER-executable' to EXECUTABLE.  Signal
`user-error', if EXECUTABLE does not denote a command or an
executable file.

This command is intended for interactive use only.  In Lisp, just
`let'-bind the corresponding variable, or set it directly.  Use
`flycheck-checker-executable-variable' to obtain the executable
variable symbol for a syntax checker."
  (interactive
   (let* ((checker (read-flycheck-checker "Syntax checker: "))
          (default-executable (flycheck-checker-default-executable checker))
          (executable (if current-prefix-arg
                          nil
                        (read-file-name "Executable: " nil default-executable
                                        nil nil #'executable-find))))
     (list checker executable)))
  (when (and executable (not (executable-find executable)))
    (user-error "%s is no executable" executable))
  (let ((variable (flycheck-checker-executable-variable checker)))
    (set variable executable)))
(put 'flycheck-set-checker-executable 'interactive-only
     "Set the executable variable directly instead")


;;; Built-in checkers
(flycheck-def-option-var flycheck-gnat-include-path nil ada-gnat
  "A list of include directories for GNAT.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of gcc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gnat-language-standard "2012" ada-gnat
  "The language standard to use in GNAT.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard. When non-nil, pass
the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gnat-warnings
    '("wa") ada-gnat
  "A list of additional Ada warnings to enable in GNAT.

The value of this variable is a list of strings, where each
string is the name of a warning category to enable. By default,
most optional warnings are recommended, as in `-gnata'.

Refer to Info Node `(gnat_ugn_unw)Warning Message Control' for
more information about GNAT warnings."
  :type '(repeat :tag "Warnings" (string :tag "Warning name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker ada-gnat
  "An Ada syntax checker using GNAT.

Uses the GNAT compiler from GCC.  See URL
`https://gcc.gnu.org/onlinedocs/gnat_ugn_unw/'."
  :command ("gnatmake"
            "-c"                        ; Just compile, don't bind
            "-f"                        ; Force re-compilation
            "-u"                        ; Compile the main file only
            "-gnatf"                    ; Full error information
            "-gnatef"                   ; Full source file name
            "-D" temporary-directory
            (option-list "-gnat" flycheck-gnat-warnings concat)
            (option-list "-I" flycheck-gnat-include-path concat)
            (option "-gnat" flycheck-gnat-language-standard concat)
            source)
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ;no specific error prefix in Ada
          ": " (message) line-end))
  :modes (ada-mode))

(flycheck-define-checker asciidoc
  "A AsciiDoc syntax checker using the AsciiDoc compiler.

See URL `http://www.methods.co.nz/asciidoc'."
  :command ("asciidoc" source)
  :error-patterns
  ((error line-start "asciidoc: ERROR: " (file-name)
          ": Line " line ": " (message) line-end)
   (warning line-start "asciidoc: " (or "WARNING" "DEPRECATED") ": " (file-name)
            ": Line " line ": " (message) line-end))
  :modes adoc-mode)

(flycheck-def-option-var flycheck-clang-blocks nil c/c++-clang
  "Enable blocks in Clang.

When non-nil, enable blocks in Clang with `-fblocks'.  See URL
`http://clang.llvm.org/docs/BlockLanguageSpec.html' for more
information about blocks."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-clang-definitions nil c/c++-clang
  "Additional preprocessor definitions for Clang.

The value of this variable is a list of strings, where each
string is an additional definition to pass to Clang, via the `-D'
option."
  :type '(repeat (string :tag "Definition"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-include-path nil c/c++-clang
  "A list of include directories for Clang.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Clang.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-clang-includes nil c/c++-clang
  "A list of additional include files for Clang.

The value of this variable is a list of strings, where each
string is a file to include before syntax checking.  Relative
paths are relative to the file being checked."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-language-standard nil c/c++-clang
  "The language standard to use in Clang.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'stringp
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-ms-extensions nil c/c++-clang
  "Whether to enable Microsoft extensions to C/C++ in Clang.

When non-nil, enable Microsoft extensions to C/C++ via
`-fms-extensions'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-clang-no-exceptions nil c/c++-clang
  "Whether to disable exceptions in Clang.

When non-nil, disable exceptions for syntax checks, via
`-fno-exceptions'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-clang-no-rtti nil c/c++-clang
  "Whether to disable RTTI in Clang.

When non-nil, disable RTTI for syntax checks, via `-fno-rtti'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-standard-library nil c/c++-clang
  "The standard library to use for Clang.

The value of this variable is the name of a standard library as
string, or nil to use the default standard library.

Refer to the Clang manual at URL
`http://clang.llvm.org/docs/UsersManual.html' for more
information about the standard library."
  :type '(choice (const "libc++")
                 (const :tag "GNU libstdc++" "libstdc++")
                 (string :tag "Library name"))
  :safe #'stringp
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-warnings '("all" "extra") c/c++-clang
  "A list of additional warnings to enable in Clang.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the Clang manual at URL
`http://clang.llvm.org/docs/UsersManual.html' for more
information about warnings."
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(defun flycheck-c/c++-quoted-include-directory ()
  "Get the directory for quoted includes.

C/C++ compiles typicall look up includes with quotation marks in
the directory of the file being compiled.  However, since
Flycheck uses temporary copies for syntax checking, it needs to
explicitly determine the directory for quoted includes.

This function determines the directory by looking at
`buffer-file-name', or if that is nil, at `default-directory'."
  (-if-let (fn (buffer-file-name))
      (file-name-directory fn)
    ;; If the buffer has no file name, fall back to its default directory
    default-directory))

(flycheck-define-checker c/c++-clang
  "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
  :command ("clang"
            "-fsyntax-only"
            "-fno-color-diagnostics"    ; Do not include color codes in output
            "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-clang-language-standard concat)
            (option "-stdlib=" flycheck-clang-standard-library concat)
            (option-flag "-fms-extensions" flycheck-clang-ms-extensions)
            (option-flag "-fno-exceptions" flycheck-clang-no-exceptions)
            (option-flag "-fno-rtti" flycheck-clang-no-rtti)
            (option-flag "-fblocks" flycheck-clang-blocks)
            (option-list "-include" flycheck-clang-includes)
            (option-list "-W" flycheck-clang-warnings concat)
            (option-list "-D" flycheck-clang-definitions concat)
            (option-list "-I" flycheck-clang-include-path)
            "-x" (eval
                  (pcase major-mode
                    (`c++-mode "c++")
                    (`c-mode "c")))
            source)
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (optional (message)) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (optional (message)) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (optional (message)) line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
        ;; Clang will output empty messages for #error/#warning pragmas without
        ;; messages.  We fill these empty errors with a dummy message to get
        ;; them past our error filtering
        (setf (flycheck-error-message err)
              (or (flycheck-error-message err) "no message")))
      (flycheck-fold-include-errors errors "In file included from")))
  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(flycheck-def-option-var flycheck-gcc-definitions nil c/c++-gcc
  "Additional preprocessor definitions for GCC.

The value of this variable is a list of strings, where each
string is an additional definition to pass to GCC, via the `-D'
option."
  :type '(repeat (string :tag "Definition"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-include-path nil c/c++-gcc
  "A list of include directories for GCC.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of gcc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-includes nil c/c++-gcc
  "A list of additional include files for GCC.

The value of this variable is a list of strings, where each
string is a file to include before syntax checking.  Relative
paths are relative to the file being checked."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-language-standard nil c/c++-gcc
  "The language standard to use in GCC.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-no-exceptions nil c/c++-gcc
  "Whether to disable exceptions in GCC.

When non-nil, disable exceptions for syntax checks, via
`-fno-exceptions'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-no-rtti nil c/c++-gcc
  "Whether to disable RTTI in GCC.

When non-nil, disable RTTI for syntax checks, via `-fno-rtti'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-openmp nil c/c++-gcc
  "Whether to enable OpenMP in GCC.

When non-nil, enable OpenMP for syntax checkers, via
`-fopenmp'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.21"))

(flycheck-def-option-var flycheck-gcc-warnings '("all" "extra") c/c++-gcc
  "A list of additional warnings to enable in GCC.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the gcc manual at URL
`https://gcc.gnu.org/onlinedocs/gcc/' for more information about
warnings."
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker c/c++-gcc
  "A C/C++ syntax checker using GCC.

Requires GCC 4.8 or newer.  See URL `https://gcc.gnu.org/'."
  :command ("gcc"
            "-fshow-column"
            "-fno-diagnostics-show-caret" ; Do not visually indicate the source location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gcc-language-standard concat)
            (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
            (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
            (option-flag "-fopenmp" flycheck-gcc-openmp)
            (option-list "-include" flycheck-gcc-includes)
            (option-list "-W" flycheck-gcc-warnings concat)
            (option-list "-D" flycheck-gcc-definitions concat)
            (option-list "-I" flycheck-gcc-include-path)
            "-x" (eval
                  (pcase major-mode
                    (`c++-mode "c++")
                    (`c-mode "c")))
            source
            ;; GCC performs full checking only when actually compiling, so
            ;; `-fsyntax-only' is not enough. Just let it generate assembly
            ;; code.
            "-S" "-o" null-device)
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-fold-include-errors (flycheck-sanitize-errors errors)
                                  "In file included from"))
  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(flycheck-def-option-var flycheck-cppcheck-checks '("style") c/c++-cppcheck
  "Enabled checks for Cppcheck.

The value of this variable is a list of strings, where each
string is the name of an additional check to enable.  By default,
all coding style checks are enabled.

See section \"Enable message\" in the Cppcheck manual at URL
`http://cppcheck.sourceforge.net/manual.pdf', and the
documentation of the `--enable' option for more information,
including a list of supported checks."
  :type '(repeat :tag "Additional checks"
                 (string :tag "Check name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-cppcheck-inconclusive nil c/c++-cppcheck
  "Whether to enable Cppcheck inconclusive checks.

When non-nil, enable Cppcheck inconclusive checks.  This allows Cppcheck to
report warnings it's not certain of, but it may result in false positives.

This will have no effect when using Cppcheck 1.53 and older."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.19"))

(flycheck-define-checker c/c++-cppcheck
  "A C/C++ checker using cppcheck.

See URL `http://cppcheck.sourceforge.net/'."
  :command ("cppcheck" "--quiet" "--xml-version=2" "--inline-suppr"
            (option "--enable=" flycheck-cppcheck-checks concat
                    flycheck-option-comma-separated-list)
            (option-flag "--inconclusive" flycheck-cppcheck-inconclusive)
            source)
  :error-parser flycheck-parse-cppcheck
  :modes (c-mode c++-mode))

(flycheck-define-checker cfengine
  "A CFEngine syntax checker using cf-promises.

See URL `http://cfengine.com/'."
  :command ("cf-promises" "-Wall" "-f"
            ;; We must stay in the same directory to resolve @include
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": error: " (message) line-end))
  :modes (cfengine-mode cfengine3-mode))

(flycheck-define-checker chef-foodcritic
  "A Chef cookbooks syntax checker using Foodcritic.

See URL `http://acrmp.github.io/foodcritic/'."
  :command ("foodcritic" source)
  :error-patterns
  ((error line-start (message) ": " (file-name) ":" line line-end))
  :modes (enh-ruby-mode ruby-mode)
  :predicate
  (lambda ()
    (let ((parent-dir (file-name-directory
                       (directory-file-name
                        (expand-file-name default-directory)))))
      (or
       ;; Chef CookBook
       ;; http://docs.opscode.com/chef/knife.html#id38
       (locate-dominating-file parent-dir "recipes")
       ;; Knife Solo
       ;; http://matschaffer.github.io/knife-solo/#label-Init+command
       (locate-dominating-file parent-dir "cookbooks")))))

(flycheck-define-checker coffee
  "A CoffeeScript syntax checker using coffee.

See URL `http://coffeescript.org/'."
  ;; --print suppresses generation of compiled .js files
  :command ("coffee" "--compile" "--print" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column
          ": error: " (message) line-end))
  :modes coffee-mode
  :next-checkers ((warning . coffee-coffeelint)))

(flycheck-def-config-file-var flycheck-coffeelintrc coffee-coffeelint
                              ".coffeelint.json"
  :safe #'stringp)

(flycheck-define-checker coffee-coffeelint
  "A CoffeeScript style checker using coffeelint.

This syntax checker requires coffeelint 1.0 or newer.

See URL `http://www.coffeelint.org/'."
  :command
  ("coffeelint"
   (config-file "--file" flycheck-coffeelintrc)
   "--checkstyle" source)
  :error-parser flycheck-parse-checkstyle
  :modes coffee-mode)

(flycheck-define-checker coq
  "A Coq syntax checker using the Coq compiler.

See URL `http://coq.inria.fr/'."
  ;; We use coqtop in batch mode, because coqc is picky about file names.
  :command ("coqtop" "-batch" "-load-vernac-source" source)
  :error-patterns
  ((error line-start "File \"" (file-name) "\", line " line
          ;; TODO: Parse the end column, once Flycheck supports that
          ", characters " column "-" (one-or-more digit) ":\n"
          (or "Syntax error: " "Error: ")
          ;; Most Coq error messages span multiple lines, and end with a dot.
          ;; There are simple one-line messages, too, though.
          (message (or (and (one-or-more (or not-newline "\n")) ".")
                       (one-or-more not-newline)))
          line-end))
  :error-filter
  (lambda (errors)
    (dolist (err errors)
      ;; Coq uses zero-based indexing for columns, so we need to fix column
      ;; indexes.  Also, delete trailing whitespace from all lines in the error
      ;; message
      (let ((column (flycheck-error-column err))
            (message (flycheck-error-message err)))
        (setf (flycheck-error-column err) (1+ column))
        (with-temp-buffer
          (insert message)
          (delete-trailing-whitespace)
          (setf (flycheck-error-message err)
                (buffer-substring-no-properties (point-min) (point-max))))))
    (flycheck-sanitize-errors errors))
  :modes coq-mode)

(flycheck-define-checker css-csslint
  "A CSS syntax and style checker using csslint.

See URL `https://github.com/CSSLint/csslint'."
  :command ("csslint" "--format=checkstyle-xml" source)
  :error-parser flycheck-parse-checkstyle
  :modes css-mode)

(defconst flycheck-d-module-re
  (rx "module" (one-or-more (syntax whitespace))
      (group (one-or-more (not (syntax whitespace))))
      (zero-or-more (syntax whitespace))
      ";")
  "Regular expression to match a D module declaration.")

(defun flycheck-d-base-directory ()
  "Get the relative base directory path for this module."
  (let* ((file-name (buffer-file-name))
         (module-file (if (string= (file-name-nondirectory file-name)
                                   "package.d")
                          (directory-file-name (file-name-directory file-name))
                        file-name)))
    (flycheck-module-root-directory
     (flycheck-find-in-buffer flycheck-d-module-re)
     module-file)))

(flycheck-def-option-var flycheck-dmd-include-path nil d-dmd
  "A list of include directories for dmd.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of dmd.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.18"))

(flycheck-define-checker d-dmd
  "A D syntax checker using the DMD compiler.

Requires DMD 2.066 or newer.  See URL `http://dlang.org/'."
  :command ("dmd"
            "-debug"                    ; Compile in debug mode
            "-o-"                       ; Don't generate an object file
            "-vcolumns"                 ; Add columns in output
            "-wi" ; Compilation will continue even if there are warnings
            (eval (concat "-I" (flycheck-d-base-directory)))
            (option-list "-I" flycheck-dmd-include-path concat)
            source)
  :error-patterns
  ((error line-start
          (file-name) "(" line "," column "): Error: " (message)
          line-end)
   (warning line-start (file-name) "(" line "," column "): "
            (or "Warning" "Deprecation") ": " (message) line-end))
  :modes d-mode)

(flycheck-define-checker elixir
  "An Elixir syntax checker using the Elixir interpreter.

See URL `http://elixir-lang.org/'."
  :command ("elixirc"
            "-o" temporary-directory    ; Move compiler output out of the way
            "--ignore-module-conflict"  ; Prevent tedious module redefinition
                                        ; warning.
            source)
  :error-patterns
  ;; Elixir compiler errors
  ((error line-start "** (" (zero-or-more not-newline) ") "
          (file-name) ":" line ": " (message) line-end)
   ;; Warnings from Elixir >= 0.12.4
   (warning line-start (file-name) ":" line ": warning:" (message) line-end)
   ;; Warnings from older Elixir versions
   (warning line-start (file-name) ":" line ": " (message) line-end))
  :modes elixir-mode)

(defconst flycheck-this-emacs-executable
  (concat invocation-directory invocation-name)
  "The path to the currently running Emacs executable.")

(defconst flycheck-emacs-args '("-Q" "--batch")
  "Common arguments to Emacs invocations.")

(defmacro flycheck-prepare-emacs-lisp-form (&rest body)
  "Prepare BODY for use as check form in a subprocess."
  (declare (indent 0))
  `(flycheck-sexp-to-string
    '(progn
       (defvar jka-compr-inhibit)
       (unwind-protect
           ;; Flycheck inhibits compression of temporary files, thus we
           ;; must not attempt to decompress.
           (let ((jka-compr-inhibit t))
             ;; Strip option-argument separator from arguments, if present
             (when (equal (car command-line-args-left) "--")
               (setq command-line-args-left (cdr command-line-args-left)))
             ,@body)
         ;; Prevent Emacs from processing the arguments on its own, see
         ;; https://github.com/flycheck/flycheck/issues/319
         (setq command-line-args-left nil)))))

(defconst flycheck-emacs-lisp-check-form
  (flycheck-prepare-emacs-lisp-form
    ;; Keep track of the generated bytecode files, to delete them after byte
    ;; compilation.
    (defvar flycheck-byte-compiled-files nil)
    (let ((byte-compile-dest-file-function
           (lambda (source)
             (let ((temp-file (make-temp-file (file-name-nondirectory source))))
               (push temp-file flycheck-byte-compiled-files)
               temp-file))))
      (unwind-protect
          (byte-compile-file (car command-line-args-left))
        (mapc (lambda (f) (ignore-errors (delete-file f)))
              flycheck-byte-compiled-files)))))

(flycheck-def-option-var flycheck-emacs-lisp-load-path nil emacs-lisp
  "Load path to use in the Emacs Lisp syntax checker.

When set to `inherit', use the `load-path' of the current Emacs
session during syntax checking.

When set to a list of strings, add each directory in this list to
the `load-path' before invoking the byte compiler.  Relative
paths in this list are expanded against the `default-directory'
of the buffer to check.

When nil, do not explicitly set the `load-path' during syntax
checking.  The syntax check only uses the built-in `load-path' of
Emacs in this case.

Note that changing this variable can lead to wrong results of the
syntax check, e.g. if an unexpected version of a required library
is used."
  :type '(choice (const :tag "Inherit current `load-path'" inherit)
                 (repeat :tag "Load path" directory))
  :risky t
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-emacs-lisp-initialize-packages
    'auto emacs-lisp
  "Whether to initialize packages in the Emacs Lisp syntax checker.

To initialize packages, call `package-initialize' before
byte-compiling the file to check.

When nil, never initialize packages.  When `auto', initialize
packages only when checking files from `user-emacs-directory'.
For any other non-nil value, always initialize packages."
  :type '(choice (const :tag "Do not initialize packages" nil)
                 (const :tag "Initialize packages for configuration only" auto)
                 (const :tag "Always initialize packages" t))
  :risky t
  :package-version '(flycheck . "0.14"))

(defconst flycheck-emacs-lisp-package-initialize-form
  (flycheck-sexp-to-string
   '(with-demoted-errors "Error during package initialization: %S"
      (package-initialize)))
  "Form used to initialize packages.")

(defun flycheck-option-emacs-lisp-package-initialize (value)
  "Option VALUE filter for `flycheck-emacs-lisp-initialize-packages'."
  (let ((shall-initialize
         (if (eq value 'auto)
             (flycheck-in-user-emacs-directory-p (buffer-file-name))
           value)))
    (when shall-initialize
      ;; If packages shall be initialized, return the corresponding form,
      ;; otherwise make Flycheck ignore the option by returning nil.
      flycheck-emacs-lisp-package-initialize-form)))

(flycheck-def-option-var flycheck-emacs-lisp-package-user-dir nil emacs-lisp
  "Package directory for the Emacs Lisp syntax checker.

When set to a string, set `package-user-dir' to the value of this
variable before initializing packages.

This variable has no effect, if
`flycheck-emacs-lisp-initialize-packages' is nil."
  :type '(choice (const :tag "Default package directory" nil)
                 (directory :tag "Custom package directory"))
  :risky t
  :package-version '(flycheck . "0.14"))

(defun flycheck-option-emacs-lisp-package-user-dir (value)
  "Option VALUE filter for `flycheck-emacs-lisp-package-user-dir'."
  (unless value
    ;; Inherit the package directory from our Emacs session
    (setq value (and (boundp 'package-user-dir) package-user-dir)))
  (when value
    (flycheck-sexp-to-string `(setq package-user-dir ,value))))

(flycheck-define-checker emacs-lisp
  "An Emacs Lisp syntax checker using the Emacs Lisp Byte compiler.

See Info Node `(elisp)Byte Compilation'."
  :command ("emacs" (eval flycheck-emacs-args)
            (eval
             (let ((path (pcase flycheck-emacs-lisp-load-path
                           (`inherit load-path)
                           (p (mapcar #'expand-file-name p)))))
               (flycheck-prepend-with-option "--directory" path)))
            (option "--eval" flycheck-emacs-lisp-package-user-dir nil
                    flycheck-option-emacs-lisp-package-user-dir)
            (option "--eval" flycheck-emacs-lisp-initialize-packages nil
                    flycheck-option-emacs-lisp-package-initialize)
            "--eval" (eval flycheck-emacs-lisp-check-form)
            "--"
            source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":Error:"
          (message (zero-or-more not-newline)
                   (zero-or-more "\n    " (zero-or-more not-newline)))
          line-end)
   (warning line-start (file-name) ":" line ":" column ":Warning:"
            (message (zero-or-more not-newline)
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end))
  :error-filter
  (lambda (errors)
    (flycheck-collapse-error-message-whitespace
     (flycheck-sanitize-errors errors)))
  :modes (emacs-lisp-mode lisp-interaction-mode)
  :predicate
  (lambda ()
    (and
     ;; Ensure that we only check buffers with a backing file.  For buffers
     ;; without a backing file we cannot guarantee that file names in error
     ;; messages are properly resolved, because `byte-compile-file' emits file
     ;; names *relative to the directory of the checked file* instead of the
     ;; working directory.  Hence our backwards-substitution will fail, because
     ;; the checker process has a different base directory to resolve relative
     ;; file names than the Flycheck code working on the buffer to check.
     (buffer-file-name)
     ;; Do not check buffers which should not be byte-compiled.  The checker
     ;; process will refuse to compile these, which would confuse Flycheck
     (not (bound-and-true-p no-byte-compile))
     ;; Do not check buffers used for autoloads generation during package
     ;; installation.  These buffers are too short-lived for being checked, and
     ;; doing so causes spurious errors.  See
     ;; https://github.com/flycheck/flycheck/issues/45 and
     ;; https://github.com/bbatsov/prelude/issues/248.  We must also not check
     ;; compilation buffers, but as these are ephemeral, Flycheck won't check
     ;; them anyway.
     (not (flycheck-autoloads-file-p))))
  :next-checkers (emacs-lisp-checkdoc))

(defconst flycheck-emacs-lisp-checkdoc-form
  (flycheck-prepare-emacs-lisp-form
    (require 'checkdoc)

    (let ((source (car command-line-args-left))
          ;; Remember the default directory of the process
          (process-default-directory default-directory))
      (with-temp-buffer
        (insert-file-contents source 'visit)
        (setq buffer-file-name source)
        ;; And change back to the process default directory to make file-name
        ;; back-substutition work
        (setq default-directory process-default-directory)
        (with-demoted-errors "Error in checkdoc: %S"
          (checkdoc-current-buffer t)
          (with-current-buffer checkdoc-diagnostic-buffer
            (princ (buffer-substring-no-properties (point-min) (point-max)))
            (kill-buffer)))))))

(flycheck-define-checker emacs-lisp-checkdoc
  "An Emacs Lisp style checker using CheckDoc.

The checker runs `checkdoc-current-buffer'."
  :command ("emacs" (eval flycheck-emacs-args)
            "--eval" (eval flycheck-emacs-lisp-checkdoc-form)
            "--" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": " (message) line-end))
  :modes (emacs-lisp-mode)
  :predicate
  (lambda ()
    ;; Do not check Autoloads, Cask/Carton and dir-locals files.  These files
    ;; really don't need to follow Checkdoc conventions.
    (not (or (flycheck-autoloads-file-p)
             (and (buffer-file-name)
                  (member (file-name-nondirectory (buffer-file-name))
                          '("Cask" "Carton" ".dir-locals.el")))))))

(progn
  ;; Use the currently running Emacs by default.
  (setcar (get 'emacs-lisp 'flycheck-command) flycheck-this-emacs-executable)
  (setcar (get 'emacs-lisp-checkdoc 'flycheck-command)
          flycheck-this-emacs-executable))

(flycheck-define-checker erlang
  "An Erlang syntax checker using the Erlang interpreter.

See URL `http://www.erlang.org/'."
  :command ("erlc" "-o" temporary-directory "-Wall" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes erlang-mode)

(flycheck-define-checker eruby-erubis
  "A eRuby syntax checker using the `erubis' command.

See URL `http://www.kuwata-lab.com/erubis/'."
  :command ("erubis" "-z" source)
  :error-patterns
  ((error line-start  (file-name) ":" line ": " (message) line-end))
  :modes (html-erb-mode rhtml-mode))

(flycheck-def-option-var flycheck-gfortran-include-path nil fortran-gfortran
  "A list of include directories for GCC Fortran.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of gcc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gfortran-language-standard "f95" fortran-gfortran
  "The language standard to use in GFortran.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gfortran-layout nil fortran-gfortran
  "The source code layout to use in GFortran.

The value of this variable is one of the following symbols:

nil
     Let gfortran determine the layout from the extension

`free'
     Use free form layout


`fixed'
     Use fixed form layout

In any other case, an error is signaled.")

(defun flycheck-option-gfortran-layout (value)
  "Option VALUE filter for `flycheck-gfortran-layout'."
  (pcase value
    (`nil nil)
    (`free "free-form")
    (`fixed "fixed-form")
    (_ (error "Invalid value for flycheck-gfortran-layout: %S" value))))

(flycheck-def-option-var flycheck-gfortran-warnings  '("all" "extra")
                         fortran-gfortran
  "A list of warnings for GCC Fortran.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the gfortran manual at URL
`https://gcc.gnu.org/onlinedocs/gfortran/' for more information
about warnings")

(flycheck-define-checker fortran-gfortran
  "An Fortran syntax checker using GCC.

Uses GCC's Fortran compiler gfortran.  See URL
`https://gcc.gnu.org/onlinedocs/gfortran/'."
  :command ("gfortran"
            "-fsyntax-only"
            "-fshow-column"
            "-fno-diagnostics-show-caret" ; Do not visually indicate the source location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            ;; Fortran has similar include processing as C/C++
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gfortran-language-standard concat)
            (option "-f" flycheck-gfortran-layout concat
                    flycheck-option-gfortran-layout)
            (option-list "-W" flycheck-gfortran-warnings concat)
            (option-list "-I" flycheck-gfortran-include-path concat)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line "." column ":\n"
          (= 3 (zero-or-more not-newline) "\n")
          (or "Error" "Fatal Error") ": " (message) line-end)
   (warning line-start (file-name) ":" line "." column ":\n"
            (= 3 (zero-or-more not-newline) "\n")
            "Warning: " (message) line-end))
  :modes (fortran-mode f90-mode))

(flycheck-define-checker go-gofmt
  "A Go syntax and style checker using the gofmt utility.

See URL `http://golang.org/cmd/gofmt/'."
  :command ("gofmt" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes go-mode
  :next-checkers ((warning . go-golint)
                  ;; Fall back, if go-golint doesn't exist
                  (warning . go-vet)
                  ;; Fall back, if go-vet doesn't exist
                  (warning . go-build) (warning . go-test)
                  (warning . go-errcheck)))

(flycheck-define-checker go-golint
  "A Go style checker using Golint.

See URL `https://github.com/golang/lint'."
  :command ("golint" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes go-mode
  :next-checkers (go-vet
                  ;; Fall back, if go-vet doesn't exist
                  go-build go-test go-errcheck))

(flycheck-def-option-var flycheck-go-vet-print-functions nil go-vet
  "A comma-separated list of print-like functions for `go tool vet'.

Go vet will check these functions for format string problems and
issues, such as a mismatch between the number of formats used,
and the number of arguments given.

Each entry is in the form Name:N where N is the zero-based
argument position of the first argument involved in the print:
either the format or the first print argument for non-formatted
prints.  For example, if you have Warn and Warnf functions that
take an io.Writer as their first argument, like Fprintf,
-printfuncs=Warn:1,Warnf:1 "
  :type '(repeat :tag "print-like functions"
                 (string :tag "function"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker go-vet
  "A Go syntax checker using the `go tool vet' command.

See URL `http://golang.org/cmd/go/' and URL
`http://godoc.org/code.google.com/p/go.tools/cmd/vet'."
  :command ("go" "tool" "vet"
            (option "-printfuncs=" flycheck-go-vet-print-functions concat
                    flycheck-option-comma-separated-list)
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": " (message) line-end))
  :modes go-mode
  ;; We must explicitly check whether the "vet" tool is available
  :predicate (lambda ()
               (let ((go (flycheck-checker-executable 'go-vet)))
                 (member "vet" (ignore-errors (process-lines go "tool")))))
  :next-checkers (go-build
                  go-test
                  ;; Fall back if `go build' or `go test' can be used
                  go-errcheck))

(flycheck-define-checker go-build
  "A Go syntax and type checker using the `go build' command.

See URL `http://golang.org/cmd/go'."
  ;; We need to use `temporary-file-name' instead of `null-device', because Go
  ;; can't write to the null device.  It's “too magic”.  See
  ;; https://code.google.com/p/go/issues/detail?id=4851 for details.
  :command ("go" "build" "-o" temporary-file-name)
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes go-mode
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))
  :next-checkers ((warning . go-errcheck)))

(flycheck-define-checker go-test
  "A Go syntax and type checker using the `go test' command.

See URL `http://golang.org/cmd/go'."
  ;; This command builds the test executable and leaves it in the current
  ;; directory.  Unfortunately 'go test -c' does not have the '-o' option.
  :command ("go" "test" "-c")
  :error-patterns
  ((error line-start (file-name) ":" line ": "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes go-mode
  :predicate
  (lambda () (and (flycheck-buffer-saved-p)
                  (string-suffix-p "_test.go" (buffer-file-name))))
  :next-checkers ((warning . go-errcheck)))

(defun flycheck-go-package-name (&optional file-name gopath)
  "Determine the package name for FILE-NAME and GOPATH.

FILE-NAME defaults to `buffer-file-name'.  GOPATH defaults to
$GOPATH.

Return the package name for FILE-NAME, or nil if FILE-NAME is not
part of any package or if GOPATH is nil."
  (-when-let* ((gopath (or gopath (getenv "GOPATH")))
               (file-name (or file-name (buffer-file-name))))
    (let ((gosrc (file-name-as-directory (expand-file-name "src/" gopath)))
          (file-name (expand-file-name file-name)))
      (when (string-prefix-p gosrc file-name)
        ;; The file is part of a package, so determine the package name, as
        ;; relative file name to the GO source directory
        (directory-file-name            ; Remove trailing /
         (file-relative-name (file-name-directory file-name) gosrc))))))

(flycheck-define-checker go-errcheck
  "A Go checker for unchecked errors.

See URL `https://github.com/kisielk/errcheck'."
  :command ("errcheck" (eval (flycheck-go-package-name)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column (one-or-more "\t")
            (message)
            line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors))
          (gosrc (expand-file-name "src/" (getenv "GOPATH"))))
      (dolist (err errors)
        ;; File names are relative to the Go source directory, so we need to
        ;; unexpand and re-expand them
        (setf (flycheck-error-filename err)
              (expand-file-name
               ;; Get the relative name back, since Flycheck has already
               ;; expanded the name for us
               (file-relative-name (flycheck-error-filename err))
               ;; And expand it against the Go source directory
               gosrc))
        (-when-let (message (flycheck-error-message err))
          ;; Improve the messages reported by errcheck to make them more clear.
          (setf (flycheck-error-message err)
                (format "Ignored `error` returned from `%s`" message)))))
    errors)
  :modes go-mode
  :predicate
  (lambda ()
    ;; We need a valid package name, since errcheck only works on entire
    ;; packages, and can't check individual Go files.
    (and (flycheck-buffer-saved-p) (flycheck-go-package-name))))

(flycheck-define-checker haml
  "A Haml syntax checker using the Haml compiler.

See URL `http://haml.info'."
  :command ("haml" "-c" source)
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message) line-end))
  :modes haml-mode)

(flycheck-define-checker handlebars
  "A Handlebars syntax checker using the Handlebars compiler.

See URL `http://handlebarsjs.com/'."
  :command ("handlebars" source)
  :error-patterns
  ((error line-start
          "Error: Parse error on line " line ":" (optional "\r") "\n"
          (zero-or-more not-newline) "\n" (zero-or-more not-newline) "\n"
          (message) line-end))
  :modes (handlebars-mode handlebars-sgml-mode))

(defconst flycheck-haskell-module-re
  (rx line-start (zero-or-more (or "\n" (any space)))
      "module" (one-or-more (or "\n" (any space)))
      (group (one-or-more (not (any space "\n")))))
  "Regular expression for a Haskell module name.")

(flycheck-def-option-var flycheck-ghc-no-user-package-database nil haskell-ghc
  "Whether to disable the user package database in GHC.

When non-nil, disable the user package database in GHC, via
`-no-user-package-db'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-ghc-package-databases nil haskell-ghc
  "Additional module databases for GHC.

The value of this variable is a list of strings, where each
string is a directory of a package database.  Each package
database is given to GHC via `-package-db'."
  :type '(repeat (directory :tag "Package database"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-ghc-search-path nil haskell-ghc
  "Module search path for GHC.

The value of this variable is a list of strings, where each
string is a directory containing Haskell modules.  Each directory
is added to the GHC search path via `-i'."
  :type '(repeat (directory :tag "Module directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-ghc-language-extensions nil haskell-ghc
  "Language extensions for GHC.

The value of this variable is a list of strings, where each
string is a Haskell language extension, as in the LANGUAGE
pragma.  Each extension is enabled via `-X'."
  :type '(repeat (string :tag "Language extension"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.19"))

(flycheck-define-checker haskell-ghc
  "A Haskell syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
  :command ("ghc" "-Wall" "-fno-code"
            (option-flag "-no-user-package-db"
                         flycheck-ghc-no-user-package-database)
            (option-list "-package-db" flycheck-ghc-package-databases)
            (option-list "-i" flycheck-ghc-search-path concat)
            ;; Include the parent directory of the current module tree, to
            ;; properly resolve local imports
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            ;; Force GHC to treat the file as Haskell file, even if it doesn't
            ;; have an extension.  Otherwise GHC would fail on files without an
            ;; extension
            "-x" "hs" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more not-newline)))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes haskell-mode
  :next-checkers ((warning . haskell-hlint)))

(flycheck-define-checker haskell-hlint
  "A Haskell style checker using hlint.

See URL `https://github.com/ndmitchell/hlint'."
  :command ("hlint" source-inplace)
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column
            ": Warning: "
            (message (one-or-more not-newline)
                     (one-or-more "\n" (one-or-more not-newline)))
            line-end)
   (error line-start
          (file-name) ":" line ":" column
          ": Error: "
          (message (one-or-more not-newline)
                   (one-or-more "\n" (one-or-more not-newline)))
          line-end))
  :modes haskell-mode)

(flycheck-def-config-file-var flycheck-tidyrc html-tidy ".tidyrc"
  :safe #'stringp)

(flycheck-define-checker html-tidy
  "A HTML syntax and style checker using Tidy.

See URL `https://github.com/w3c/tidy-html5'."
  :command ("tidy" (config-file "-config" flycheck-tidyrc) "-e" "-q" source)
  :error-patterns
  ((error line-start
          "line " line
          " column " column
          " - Error: " (message) line-end)
   (warning line-start
            "line " line
            " column " column
            " - Warning: " (message) line-end))
  :modes (html-mode nxhtml-mode))

(flycheck-def-config-file-var flycheck-jshintrc javascript-jshint ".jshintrc"
  :safe #'stringp)

(flycheck-define-checker javascript-jshint
  "A JavaScript syntax and style checker using jshint.

See URL `http://www.jshint.com'."
  :command ("jshint" "--checkstyle-reporter"
            (config-file "--config" flycheck-jshintrc)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (js-mode js2-mode js3-mode))

(flycheck-def-option-var flycheck-eslint-rulesdir nil javascript-eslint
  "The directory of custom rules for ESLint.

The value of this variable is either a string containing the path
to a directory with custom rules, or nil, to not give any custom
rules to ESLint.

Refer to the ESLint manual at URL
`https://github.com/eslint/eslint/tree/master/docs/command-line-interface#--rulesdir'
for more information about the custom directory."
  :type '(choice (const :tag "No custom rules directory" nil)
                 (directory :tag "Custom rules directory"))
  :safe #'stringp
  :package-version '(flycheck . "0.16"))

(flycheck-def-config-file-var flycheck-eslintrc javascript-eslint nil
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker javascript-eslint
  "A JavaScript syntax and style checker using eslint.

See URL `https://github.com/eslint/eslint'."
  :command ("eslint" "--format=compact"
            (config-file "--config" flycheck-eslintrc)
            (option "--rulesdir" flycheck-eslint-rulesdir)
            ;; We need to use source-inplace because eslint looks for
            ;; configuration files in the directory of the file being checked.
            ;; See https://github.com/flycheck/flycheck/issues/447
            source-inplace)
  :error-patterns
  ((warning line-start (file-name)
            ": line " line ", col " column ", Warning - " (message) line-end)
   (error line-start (file-name)
          ": line " line ", col " column ", Error - " (message) line-end))
  :modes (js-mode js2-mode js3-mode))

(flycheck-def-config-file-var flycheck-gjslintrc javascript-gjslint ".gjslintrc"
  :safe #'stringp)

(flycheck-define-checker javascript-gjslint
  "A JavaScript syntax and style checker using Closure Linter.

See URL `https://developers.google.com/closure/utilities'."
  :command ("gjslint" "--unix_mode"
            (config-file "--flagfile" flycheck-gjslintrc)
            source)
  :error-patterns ((error line-start
                          (file-name) ":" line ":" (message)
                          line-end))
  :modes (js-mode js2-mode js3-mode))

(flycheck-define-checker json-jsonlint
  "A JSON syntax and style checker using jsonlint.

See URL `https://github.com/zaach/jsonlint'."
  :command ("jsonlint" "-c" "-q" source)
  :error-patterns
  ((error line-start
          (file-name)
          ": line " line
          ", col " column ", "
          (message) line-end))
  :predicate
  (lambda ()
    (or
     (eq major-mode 'json-mode)
     (and (buffer-file-name)
          (string= (file-name-extension (buffer-file-name)) "json")))))

(flycheck-define-checker less
  "A LESS syntax checker using lessc.

At least version 1.4 of lessc is required.

See URL `http://lesscss.org'."
  :command ("lessc" "--lint" "--no-color"
            ;; We need `source-inplace' to resolve relative `data-uri' paths,
            ;; see https://github.com/flycheck/flycheck/issues/471
            source-inplace)
  :error-patterns
  ((error line-start (one-or-more word) ":"
          (message)
          " in "
          (file-name)

          " on line " line
          ", column " column ":"
          line-end))
  :modes less-css-mode)

(flycheck-define-checker lua
  "A Lua syntax checker using the Lua compiler.

See URL `http://www.lua.org/'."
  :command ("luac" "-p" source)
  :error-patterns
  ((error line-start
          ;; Skip the name of the luac executable.  We also skip the file name,
          ;; because luac is stupid enough as to abbreviate file names in its
          ;; output, which for obvious reasons breaks our file name
          ;; detection. See https://github.com/flycheck/flycheck/issues/251
          (minimal-match (zero-or-more not-newline))
          ":" line ": " (message) line-end))
  :modes lua-mode)

(flycheck-define-checker make
  "A Makefile syntax checker using the POSIX compatible Make command.

See URL `http://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html'."
  :command ("make" "-n" "-f" source-inplace)
  :error-patterns
  (;; GNU Make
   ;; http://www.gnu.org/software/make/
   (error line-start (file-name) ":" line ": " (message) line-end)
   ;; NetBSD Make
   ;; http://netbsd.gw.com/cgi-bin/man-cgi?make++NetBSD-current
   (error line-start
          (zero-or-more not-newline) ; make command name
          ": \"" (file-name) "\" line " line ": " (message) line-end)
   ;; FreeBSD Make (unmaintained)
   ;; http://www.freebsd.org/cgi/man.cgi?query=make&sektion=1
   (error line-start "\"" (file-name) "\", line " line ": " (message) line-end)
   ;; OpenBSD Make (unmaintained)
   ;; http://www.openbsd.org/cgi-bin/man.cgi?query=make
   (error line-start (message) " (" (file-name) ":" line ")" line-end))
  :modes (makefile-mode makefile-gmake-mode makefile-bsdmake-mode))

(flycheck-define-checker perl
  "A Perl syntax checker using the Perl interpreter.

See URL `http://www.perl.org'."
  :command ("perl" "-w" "-c" source)
  :error-patterns
  ((error line-start (minimal-match (message))
          " at " (file-name) " line " line
          (or "." (and ", " (zero-or-more not-newline))) line-end))
  :modes (perl-mode cperl-mode)
  :next-checkers (perl-perlcritic))

(flycheck-def-option-var flycheck-perlcritic-verbosity nil perl-perlcritic
  "The message severity for Perl Critic.

The value of this variable is a severity level as integer, for
the `--severity' option to Perl Critic."
  :type '(integer :tag "Severity level")
  :safe #'integerp
  :package-version '(flycheck . "0.18"))

(flycheck-define-checker perl-perlcritic
  "A Perl syntax checker using Perl::Critic.

See URL `https://metacpan.org/pod/Perl::Critic'."
  :command ("perlcritic" "--no-color" "--verbose" "%f:%l:%c:%s:%m (%e)\n"
            (option "--severity" flycheck-perlcritic-verbosity nil
                    flycheck-option-int)
            source)
  :error-patterns
  ((info line-start
         (file-name) ":" line ":" column ":" (any "1") ":" (message)
         line-end)
   (warning line-start
            (file-name) ":" line ":" column ":" (any "234") ":" (message)
            line-end)
   (error line-start
          (file-name) ":" line ":" column ":" (any "5") ":" (message)
          line-end))
  :modes (cperl-mode perl-mode))

(flycheck-define-checker php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode)
  :next-checkers ((warning . php-phpmd)
                  (warning . php-phpcs)))

(flycheck-def-option-var flycheck-phpmd-rulesets
    '("cleancode" "codesize" "controversial" "design" "naming" "unusedcode")
    php-phpmd
  "The rule sets for PHP Mess Detector.

Set default rule sets and custom rule set files.

See section \"Using multiple rule sets\" in the PHP Mess Detector
manual at URL `http://phpmd.org/documentation/index.html'."
  :type '(repeat :tag "rule sets"
                 (string :tag "A filename or rule set"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker php-phpmd
  "A PHP style checker using PHP Mess Detector.

See URL `http://phpmd.org/'."
  :command ("phpmd" source "text"
            (eval (flycheck-option-comma-separated-list
                   flycheck-phpmd-rulesets)))
  :error-patterns
  ((warning line-start(file-name) ":" line (message) line-end))
  :modes (php-mode php+-mode)
  :next-checkers (php-phpcs))

(flycheck-def-option-var flycheck-phpcs-standard nil php-phpcs
  "The coding standard for PHP CodeSniffer.

When nil, use the default standard from the global PHP
CodeSniffer configuration.  When set to a string, pass the string
to PHP CodeSniffer which will interpret it as name as a standard,
or as path to a standard specification."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Standard name or file"))
  :safe #'stringp)

(flycheck-define-checker php-phpcs
  "A PHP style checker using PHP_CodeSniffer.

See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
  :command ("phpcs" "--report=emacs"
            (option "--standard=" flycheck-phpcs-standard concat)
            source)
  ;; Though phpcs supports Checkstyle output which we could feed to
  ;; `flycheck-parse-checkstyle', we are still using error patterns here,
  ;; because PHP has notoriously unstable output habits.  See URL
  ;; `https://github.com/flycheck/flycheck/issues/78' and URL
  ;; `https://github.com/flycheck/flycheck/issues/118'
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column ": error - " (message)
          line-end)
   (warning line-start
            (file-name) ":" line ":" column ": warning - " (message)
            line-end))
  :modes (php-mode php+-mode))

(flycheck-define-checker puppet-parser
  "A Puppet DSL syntax checker using puppet's own parser.

See URL `http://puppetlabs.com/'."
  :command ("puppet" "parser" "validate" "--color=false" source)
  :error-patterns
  ((error line-start
          ;; Skip over the path of the Puppet executable
          (minimal-match (zero-or-more not-newline))
          ": Could not parse for environment " (one-or-more word)
          ": " (message (minimal-match (zero-or-more anything)))
          " at "  (file-name "/" (zero-or-more not-newline)) ":" line line-end))
  :modes puppet-mode
  :next-checkers ((warning . puppet-lint)))

(flycheck-define-checker puppet-lint
  "A Puppet DSL style checker using puppet-lint.

See URL `http://puppet-lint.com/'."
  ;; We must check the original file, because Puppetlint is quite picky on the
  ;; names of files and there place in the directory structure, to comply with
  ;; Puppet's autoload directory layout.  For instance, a class foo::bar is
  ;; required to be in a file foo/bar.pp.  Any other place, such as a Flycheck
  ;; temporary file will cause an error.
  :command ("puppet-lint"
            "--log-format" "%{path}:%{linenumber}:%{kind}: %{message} (%{check})"
            source-original)
  :error-patterns
  ((warning line-start (file-name) ":" line ":warning: " (message) line-end)
   (error line-start (file-name) ":" line ":error: " (message) line-end))
  :modes puppet-mode
  ;; Since we check the original file, we can only use this syntax checker if
  ;; the buffer is actually linked to a file, and if it is not modified.
  :predicate flycheck-buffer-saved-p)

(flycheck-def-config-file-var flycheck-flake8rc python-flake8 ".flake8rc"
  :safe #'stringp)

(flycheck-def-option-var flycheck-flake8-maximum-complexity nil python-flake8
  "The maximum McCabe complexity of methods.

If nil, do not check the complexity of methods.  If set to an
integer, report any complexity greater than the value of this
variable as warning.

If set to an integer, this variable overrules any similar setting
in the configuration file denoted by `flycheck-flake8rc'."
  :type '(choice (const :tag "Do not check McCabe complexity" nil)
                 (integer :tag "Maximum complexity"))
  :safe #'integerp)

(flycheck-def-option-var flycheck-flake8-maximum-line-length nil python-flake8
  "The maximum length of lines.

If set to an integer, the value of this variable denotes the
maximum length of lines, overruling any similar setting in the
configuration file denoted by `flycheck-flake8rc'.  An error will
be reported for any line longer than the value of this variable.

If set to nil, use the maximum line length from the configuration
file denoted by `flycheck-flake8rc', or the PEP 8 recommendation
of 79 characters if there is no configuration with this setting."
  :type '(choice (const :tag "Default value")
                 (integer :tag "Maximum line length in characters"))
  :safe #'integerp)

(flycheck-define-checker python-flake8
  "A Python syntax and style checker using Flake8.

For best error reporting, use Flake8 2.0 or newer.

See URL `https://pypi.python.org/pypi/flake8'."
  :command ("flake8"
            (config-file "--config" flycheck-flake8rc)
            (option "--max-complexity" flycheck-flake8-maximum-complexity nil
                    flycheck-option-int)
            (option "--max-line-length" flycheck-flake8-maximum-line-length nil
                    flycheck-option-int)
            source)
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" (optional column ":") " "
          (message "E" (one-or-more digit) (zero-or-more not-newline))
          line-end)
   (warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (message (or "F"            ; Pyflakes in Flake8 >= 2.0
                         "W"            ; Pyflakes in Flake8 < 2.0
                         "C")           ; McCabe in Flake >= 2.0
                     (one-or-more digit) (zero-or-more not-newline))
            line-end)
   (info line-start
         (file-name) ":" line ":" (optional column ":") " "
         (message "N"              ; pep8-naming in Flake8 >= 2.0
                  (one-or-more digit) (zero-or-more not-newline))
         line-end)
   ;; Syntax errors in Flake8 < 2.0, in Flake8 >= 2.0 syntax errors are caught
   ;; by the E.* pattern above
   (error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

(flycheck-def-config-file-var flycheck-pylintrc python-pylint
                              ".pylintrc"
  :safe #'stringp)

(flycheck-define-checker python-pylint
  "A Python syntax and style checker using Pylint.

This syntax checker requires Pylint 1.0 or newer.

See URL `http://www.pylint.org/'."
  ;; -r n disables the scoring report
  :command ("pylint" "-r" "n"
            "--msg-template" "{path}:{line}:{column}:{C}:{msg} ({msg_id})"
            (config-file "--rcfile" flycheck-pylintrc)
            ;; Need `source-inplace' for relative imports (e.g. `from .foo
            ;; import bar'), see https://github.com/flycheck/flycheck/issues/280
            source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":"
          (or "E" "F") ":" (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":"
            (or "W" "R") ":" (message) line-end)
   (info line-start (file-name) ":" line ":" column ":"
         "C:" (message) line-end))
  :modes python-mode)

(flycheck-define-checker racket
  "A Racket syntax checker using the Racket compiler.

See URL `http://racket-lang.org/'."
  :command ("racket" "-f" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes racket-mode)

(flycheck-define-checker rpm-rpmlint
  "A RPM SPEC file syntax checker using rpmlint.

See URL `http://sourceforge.net/projects/rpmlint/'."
  :command ("rpmlint" source)
  :error-patterns
  ((error line-start
          (file-name) ":" (optional line ":") " E: " (message)
          line-end)
   (warning line-start
            (file-name) ":" (optional line ":") " W: " (message)
            line-end))
  :error-filter
  ;; Add fake line numbers if they are missing in the lint output
  (lambda (errors)
    (dolist (err errors)
      (unless (flycheck-error-line err)
        (setf (flycheck-error-line err) 1)))
    errors)
  :modes (sh-mode rpm-spec-mode)
  :predicate (lambda () (or (not (eq major-mode 'sh-mode))
                            ;; In `sh-mode', we need the proper shell
                            (eq sh-shell 'rpm))))

(defun flycheck-locate-sphinx-source-directory ()
  "Locate the Sphinx source directory for the current buffer.

Return the source directory, or nil, if the current buffer is not
part of a Sphinx project."
  (-when-let* ((filename (buffer-file-name))
               (dir (locate-dominating-file filename "conf.py")))
    (expand-file-name dir)))

(flycheck-define-checker rst
  "A ReStructuredText (RST) syntax checker using Docutils.

See URL `http://docutils.sourceforge.net/'."
  ;; We need to use source-inplace to properly resolve relative paths in
  ;; include:: directives
  :command ("rst2pseudoxml.py" "--report=2" "--halt=5" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ": (WARNING/2) " (message) line-end)
   (error line-start
          (file-name) ":" line
          ": (" (or "ERROR/3" "SEVERE/4") ") "
          (message) line-end))
  :modes rst-mode
  ;; Don't use the generic ReST checker in Sphinx projects, because it'll
  ;; produce a lot of false positives.
  :predicate (lambda () (not (flycheck-locate-sphinx-source-directory))))

(flycheck-def-option-var flycheck-sphinx-warn-on-missing-references t rst-sphinx
  "Whether to warn about missing references in Sphinx.

When non-nil (the default), warn about all missing references in
Sphinx via `-n'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.17"))

(flycheck-define-checker rst-sphinx
  "A ReStructuredText (RST) syntax checker using Sphinx.

Requires Sphinx 1.2 or newer.  See URL `http://sphinx-doc.org'."
  :command ("sphinx-build" "-b" "pseudoxml"
            "-q" "-N"                   ; Reduced output and no colors
            (option-flag "-n" flycheck-sphinx-warn-on-missing-references)
            (eval (flycheck-locate-sphinx-source-directory))
            temporary-directory         ; Redirect the output to a temporary
                                        ; directory
            source-original)            ; Sphinx needs the original document
  :error-patterns
  ((warning line-start (file-name) ":" line ": WARNING: " (message) line-end)
   (error line-start
          (file-name) ":" line
          ": " (or "ERROR" "SEVERE") ": "
          (message) line-end))
  :modes rst-mode
  :predicate (lambda () (and (flycheck-buffer-saved-p)
                             (flycheck-locate-sphinx-source-directory))))

(flycheck-def-config-file-var flycheck-rubocoprc ruby-rubocop ".rubocop.yml"
  :safe #'stringp)

(flycheck-def-option-var flycheck-rubocop-lint-only nil ruby-rubocop
  "Whether to only report code issues in Rubocop.

When non-nil, only report code issues in Rubocop, via `--lint'.
Otherwise report style issues as well.")

(flycheck-define-checker ruby-rubocop
  "A Ruby syntax and style checker using the RuboCop tool.

See URL `http://batsov.com/rubocop/'."
  :command ("rubocop" "--format" "emacs"
            (config-file "--config" flycheck-rubocoprc)
            (option-flag "--lint" flycheck-rubocop-lint-only)
            source)
  :error-patterns
  ((info line-start
         (file-name) ":" line ":" column ": C: " (message)
         line-end)
   (warning line-start
            (file-name) ":" line ":" column ": W: " (message)
            line-end)
   (error line-start
          (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
          line-end))
  :modes (enh-ruby-mode ruby-mode)
  :next-checkers ((warning . ruby-rubylint)))

;; Default to `nil' to let Rubylint find its configuration file by itself, and
;; to maintain backwards compatibility with older Rubylint and Flycheck releases
(flycheck-def-config-file-var flycheck-rubylintrc ruby-rubylint nil
  :safe #'stringp)

(flycheck-define-checker ruby-rubylint
  "A Ruby syntax and code analysis checker using ruby-lint.

Requires ruby-lint 2.0 or newer.  To use `flycheck-rubylintrc',
ruby-lint 2.0.2 or newer is required.  See URL
`https://github.com/YorickPeterse/ruby-lint'."
  :command ("ruby-lint" "--presenter=syntastic"
            (config-file "--config" flycheck-rubylintrc)
            source)
  :error-patterns
  ((info line-start
         (file-name) ":I:" line ":" column ": " (message) line-end)
   (warning line-start
            (file-name) ":W:" line ":" column ": " (message) line-end)
   (error line-start
          (file-name) ":E:" line ":" column ": " (message) line-end))
  :modes (enh-ruby-mode ruby-mode))

(flycheck-define-checker ruby
  "A Ruby syntax checker using the standard Ruby interpreter.

Please note that the output of different Ruby versions and
implementations varies wildly.  This syntax checker supports
current versions of MRI and JRuby, but may break when used with
other implementations or future versions of these
implementations.

Please consider using `ruby-rubocop' or `ruby-rubylint' instead.

See URL `https://www.ruby-lang.org/'."
  :command ("ruby" "-w" "-c" source)
  :error-patterns
  ;; These patterns support output from JRuby, too, to deal with RVM or Rbenv
  ((error line-start
          "SyntaxError in " (file-name) ":" line ": " (message)
          line-end)
   (warning line-start
            (file-name) ":" line ":" (optional column ":")
            " warning: " (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes (enh-ruby-mode ruby-mode)
  :next-checkers ((warning . ruby-rubylint)))

(flycheck-define-checker ruby-jruby
  "A Ruby syntax checker using the JRuby interpreter.

This syntax checker is very primitive, and may break on future
versions of JRuby.

Please consider using `ruby-rubocop' or `ruby-rubylint' instead.

See URL `http://jruby.org/'."
  :command ("jruby" "-w" "-c" source)
  :error-patterns
  ((error line-start
          "SyntaxError in " (file-name) ":" line ": " (message)
          line-end)
   (warning line-start (file-name) ":" line " warning: " (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes (enh-ruby-mode ruby-mode)
  :next-checkers ((warning . ruby-rubylint)))

(flycheck-def-option-var flycheck-rust-check-tests t rust
  "Whether to check test code in Rust.

When non-nil, `rustc' is passed the `--test' flag, which will
check any code marked with the `#[cfg(test)]' attribute and any
functions marked with `#[test]'. Otherwise, `rustc' is not passed
`--test' and test code will not be checked.  Skipping `--test' is
necessary when using `#![no_std]', because compiling the test
runner requires `std'."
  :type 'boolean
  :safe #'booleanp
  :package-version '("flycheck" . "0.19"))

(flycheck-def-option-var flycheck-rust-crate-root nil rust
  "A path to the crate root for the current buffer.

The value of this variable is either a string with the path to
the crate root for the current buffer, or nil if the current buffer
is a crate.  A relative path is relative to the current buffer.

If this variable is non nil the current buffer will only be checked
if it is not modified, i.e. after it has been saved."
  :type 'string
  :package-version '(flycheck . "0.20")
  :safe #'stringp)

(flycheck-def-option-var flycheck-rust-crate-type "lib" rust
  "The type of the Rust Crate to check.

The value of this variable is a string denoting the crate type,
for the `--crate-type' flag."
  :type 'string
  :safe #'stringp
  :package-version '("flycheck" . "0.20"))

(flycheck-def-option-var flycheck-rust-library-path nil rust
  "A list of library directories for Rust.

The value of this variable is a list of strings, where each
string is a directory to add to the library path of Rust.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Library directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.18"))

(flycheck-define-checker rust
  "A Rust syntax checker using Rust compiler.

This syntax checker needs Rust 0.10 or newer.

See URL `http://www.rust-lang.org'."
  :command ("rustc" "--no-trans"
            (option "--crate-type" flycheck-rust-crate-type)
            (option-flag "--test" flycheck-rust-check-tests)
            (option-list "-L" flycheck-rust-library-path concat)
            (eval (or flycheck-rust-crate-root
                      (flycheck-substitute-argument 'source-inplace 'rust))))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": "
          (one-or-more digit) ":" (one-or-more digit) " error: "
          (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": "
            (one-or-more digit) ":" (one-or-more digit) " warning: "
            (message) line-end)
   (info line-start (file-name) ":" line ":" column ": "
         (one-or-more digit) ":" (one-or-more digit) " " (or "note" "help") ": "
         (message) line-end))
  :modes rust-mode
  :predicate (lambda ()
               (or (not flycheck-rust-crate-root) (flycheck-buffer-saved-p))))

(defvar flycheck-sass-scss-cache-directory nil
  "The cache directory for `sass' and `scss'.")

(defun flycheck-sass-scss-cache-location ()
  "Get the cache location for `sass' and `scss'.

If no cache directory exists yet, create one and return it.
Otherwise return the previously used cache directory."
  (setq flycheck-sass-scss-cache-directory
        (or flycheck-sass-scss-cache-directory
            (make-temp-file "flycheck-sass-scss-cache" 'directory))))

(flycheck-def-option-var flycheck-sass-compass nil sass
  "Whether to enable the Compass CSS framework.

When non-nil, enable the Compass CSS framework, via `--compass'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-define-checker sass
  "A Sass syntax checker using the Sass compiler.

See URL `http://sass-lang.com'."
  :command ("sass"
            "--cache-location" (eval (flycheck-sass-scss-cache-location))
            (option-flag "--compass" flycheck-sass-compass)
            "-c" source)
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message))
   (warning line-start "WARNING on line " line " of " (file-name)
            ":" (optional "\r") "\n" (message) line-end)
   (error line-start
          (or "Syntax error: " "Error: ")
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more " ")
                                 (one-or-more not-newline)))
          (optional "\r") "\n        on line " line " of " (file-name)
          line-end))
  :modes sass-mode)

(flycheck-define-checker scala
  "A Scala syntax checker using the Scala compiler.

See URL `http://www.scala-lang.org/'."
  :command ("scalac" "-Ystop-after:parser" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes scala-mode
  :next-checkers ((warning . scala-scalastyle)))

(flycheck-def-config-file-var flycheck-scalastylerc scala-scalastyle nil
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-scalastyle-jar nil scala-scalastyle
  "The path to the main JAR file of Scalastyle.

If this option is nil, or points to a non-existing file,
`scala-scalastyle' can not be used."
  :type '(file :must-match t)
  :safe #'stringp
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker scala-scalastyle
  "A Scala style checker using scalastyle.

Note that this syntax checker is not used if
`flycheck-scalastyle-jar' or `flycheck-scalastylerc' are nil or
point to non-existing files.

See URL `http://www.scalastyle.org'."
  :command ("java"
            (option "-jar" flycheck-scalastyle-jar)
            (config-file "-c" flycheck-scalastylerc)
            source)
  :error-patterns
  ((error line-start "error file=" (file-name) " message="
          (message) " line=" line (optional " column=" column) line-end)
   (warning line-start "warning file=" (file-name) " message="
            (message) " line=" line (optional " column=" column) line-end))
  :modes scala-mode
  :predicate
  ;; Inhibit this syntax checker if the JAR or the configuration are unset or
  ;; missing
  (lambda () (and flycheck-scalastyle-jar flycheck-scalastylerc
                  (file-exists-p flycheck-scalastyle-jar)
                  (file-exists-p (flycheck-locate-config-file
                                  flycheck-scalastylerc 'scala-scalastyle)))))

(flycheck-def-option-var flycheck-scss-compass nil scss
  "Whether to enable the Compass CSS framework.

When non-nil, enable the Compass CSS framework, via `--compass'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-define-checker scss
  "A SCSS syntax checker using the SCSS compiler.

See URL `http://sass-lang.com'."
  :command ("scss"
            "--cache-location" (eval (flycheck-sass-scss-cache-location))
            (option-flag "--compass" flycheck-scss-compass)
            "-c" source)
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message))
   (warning line-start "WARNING on line " line " of " (file-name)
            ":" (optional "\r") "\n" (message) line-end)
   (error line-start
          (or "Syntax error: " "Error: ")
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more " ")
                                 (one-or-more not-newline)))
          (optional "\r") "\n" (one-or-more " ") "on line " line " of " (file-name)
          line-end))
  :modes scss-mode)

(flycheck-define-checker sh-bash
  "A Bash syntax checker using the Bash shell.

See URL `http://www.gnu.org/software/bash/'."
  :command ("bash" "--norc" "-n" "--" source)
  :error-patterns ((error line-start
                          (file-name) ":" (one-or-more (not (any digit)))
                          line (zero-or-more " ") ":" (zero-or-more " ")
                          (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'bash))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-posix-dash
  "A POSIX Shell syntax checker using the Dash shell.

See URL `http://gondor.apana.org.au/~herbert/dash/'."
  :command ("dash" "-n" source)
  :error-patterns
  ((error line-start (file-name) ": " line ": " (backref 1) ": " (message)))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-posix-bash
  "A POSIX Shell syntax checker using the Bash shell.

See URL `http://www.gnu.org/software/bash/'."
  :command ("bash" "--posix" "--norc" "-n" "--" source)
  :error-patterns
  ((error line-start
          (file-name) ":" (one-or-more (not (any digit)))
          line (zero-or-more " ") ":" (zero-or-more " ")
          (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-zsh
  "A Zsh syntax checker using the Zsh shell.

See URL `http://www.zsh.org/'."
  :command ("zsh" "-n" "-d" "-f" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'zsh))
  :next-checkers ((warning . sh-shellcheck)))

(defconst flycheck-shellcheck-supported-shells '(bash ksh88 sh zsh)
  "Shells supported by ShellCheck.")

(flycheck-def-option-var flycheck-shellcheck-excluded-warnings nil sh-shellcheck
  "A list of excluded warnings for ShellCheck.

The value of this variable is a list of strings, where each
string is a warning code to be excluded from ShellCheck reports.
By default, no warnings are excluded."
  :type '(repeat :tag "Excluded warnings"
                 (string :tag "Warning code"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.21"))

(flycheck-define-checker sh-shellcheck
  "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :command ("shellcheck"
            ;; Use GCC output format to have the warning code in the messages
            "--format" "gcc"
            "--shell" (eval (symbol-name sh-shell))
            (option "--exclude" flycheck-shellcheck-excluded-warnings list
                    flycheck-option-comma-separated-list)
            source)
  :modes sh-mode
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column ": error: " (message)
          line-end)
   (warning line-start
            (file-name) ":" line ":" column ": warning: " (message)
            line-end)
   (info line-start
         (file-name) ":" line ":" column ": note: " (message)
         line-end))
  :predicate (lambda () (memq sh-shell flycheck-shellcheck-supported-shells)))

(flycheck-define-checker slim
  "A Slim syntax checker using the Slim compiler.

See URL `http://slim-lang.com'."
  :command ("slimrb" "-c" source)
  :error-patterns
  ((error line-start
          "Slim::Parser::SyntaxError:" (message) (optional "\r") "\n  "
          (file-name) ", Line " line (optional ", Column " column)
          line-end))
  :modes slim-mode)

(flycheck-def-config-file-var flycheck-chktexrc tex-chktex ".chktexrc"
  :safe #'stringp)

(flycheck-define-checker tex-chktex
  "A TeX and LaTeX syntax and style checker using chktex.

See URL `http://www.nongnu.org/chktex/'."
  :command ("chktex" (config-file "-l" flycheck-chktexrc) "-v0" "-q" "-I"
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes (latex-mode plain-tex-mode))

(flycheck-define-checker tex-lacheck
  "A LaTeX syntax and style checker using lacheck.

See URL `http://www.ctan.org/pkg/lacheck'."
  :command ("lacheck" source-inplace)
  :error-patterns
  ((warning line-start
            "\"" (file-name) "\", line " line ": " (message)
            line-end))
  :modes latex-mode)

(flycheck-define-checker texinfo
  "A Texinfo syntax checker using makeinfo.

See URL `http://www.gnu.org/software/texinfo/'."
  :command ("makeinfo" "-o" null-device source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":"
            line (optional ":" column) ": "
            "warning: " (message) line-end)
   (error line-start (file-name) ":"
          line (optional ":" column) ": "
          (message) line-end))
  :modes texinfo-mode)

(flycheck-define-checker verilog-verilator
  "A Verilog syntax checker using the Verilator Verilog HDL simulator.

See URL `http://www.veripool.org/wiki/verilator'."
  :command ("verilator" "--lint-only" "-Wall" source)
  :error-patterns
  ((warning line-start "%Warning-" (zero-or-more not-newline) ": "
            (file-name) ":" line ": " (message) line-end)
   (error line-start "%Error: " (file-name) ":"
          line ": " (message) line-end))
  :modes (verilog-mode))

(flycheck-define-checker xml-xmlstarlet
  "A XML syntax checker and validator using the xmlstarlet utility.

See URL `http://xmlstar.sourceforge.net/'."
  :command ("xmlstarlet" "val" "-e" "-q" source)
  :error-patterns
  ((error line-start (file-name) ":" line "." column ": " (message) line-end))
  :modes (xml-mode nxml-mode))

(flycheck-define-checker xml-xmllint
  "A XML syntax checker and validator using the xmllint utility.

The xmllint is part of libxml2, see URL
`http://www.xmlsoft.org/'."
  :command ("xmllint" "--noout" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes (xml-mode nxml-mode))

(flycheck-define-checker yaml-jsyaml
  "A YAML syntax checker using JS-YAML.

See URL `https://github.com/nodeca/js-yaml'."
  :command ("js-yaml" source)
  :error-patterns
  ((error line-start
          "JS-YAML: " (message) " at line " line ", column " column ":"
          line-end))
  :modes yaml-mode)

(flycheck-define-checker yaml-ruby
  "A YAML syntax checker using Ruby's YAML parser.

This syntax checker uses the YAML parser from Ruby's standard
library.

See URL `http://www.ruby-doc.org/stdlib-2.0.0/libdoc/yaml/rdoc/YAML.html'."
  :command ("ruby" "-ryaml" "-e" "begin;
   file_name = ARGV[0]; \
   YAML.load(ARGF); \
 rescue Exception => e; \
   STDERR.puts \"#{file_name}:#{e}\"; \
 end" source)
  :error-patterns
  (;; Syck (Ruby <= 1.9.2, unmaintained)
   (error line-start (file-name)
          ":syntax error on line " line ", col " column ": `" (message) "'" line-end)
   ;; Psych (Ruby >= 1.9.3)
   (error line-start (file-name) ":" (zero-or-more not-newline) ":" (message)
          "at line " line " column " column  line-end))
  :modes yaml-mode)

(provide 'flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck.el ends here
