;;; flycheck.el --- On-the-fly syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2025 Flycheck contributors
;; Copyright (C) 2012-2016 Sebastian Wiesner and Flycheck contributors
;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;             fmdkdd <fmdkdd@gmail.com>
;;             Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/flycheck/flycheck
;; Keywords: convenience, languages, tools
;; Version: 39.0-snapshot
;; Package-Requires: ((emacs "28.1") (seq "2.24"))

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; On-the-fly syntax checking for GNU Emacs.
;;
;; Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs,
;; intended as replacement for the older Flymake extension which is part of GNU
;; Emacs.
;;
;; Flycheck automatically checks buffers for errors while you type, and reports
;; warnings and errors directly in the buffer and in an optional IDE-like error
;; list.
;;
;; It comes with a rich interface for custom syntax checkers and other
;; extensions, and has already many 3rd party extensions adding new features.
;;
;; Please read the online manual at https://www.flycheck.org for more
;; information.  You can open the manual directly from Emacs with `M-x
;; flycheck-manual'.
;;
;; # Setup
;;
;; Flycheck works best on Unix systems.  It does not officially support Windows,
;; but tries to maintain Windows compatibility and should generally work fine on
;; Windows, too.
;;
;; To enable Flycheck add the following to your init file:
;;
;;    (add-hook 'after-init-hook #'global-flycheck-mode)
;;
;; Flycheck will then automatically check buffers in supported languages, as
;; long as all necessary tools are present.  Use `flycheck-verify-setup' to
;; troubleshoot your Flycheck setup.

;;; Code:

(eval-when-compile
  (require 'let-alist)      ; `let-alist'
  (require 'compile)        ; Compile Mode integration
  (require 'jka-compr)      ; To inhibit compression of temp files
  (require 'pcase)          ; `pcase-dolist' (`pcase' itself is autoloaded)
  )

(require 'seq)                   ; Sequence functions
(require 'subr-x)                ; Additional utilities
(require 'cl-lib)                ; `cl-defstruct' and CL utilities
(require 'tabulated-list)        ; To list errors
(require 'easymenu)              ; Flycheck Mode menu definition
(require 'rx)                    ; Regexp fanciness in `flycheck-define-checker'
(require 'help-mode)             ; `define-button-type'
(require 'find-func)             ; `find-function-regexp-alist'
(require 'ansi-color)            ; `flycheck-parse-with-patterns-without-color'
(require 'eldoc)                 ; The default error display
(require 'url-util)              ; `url-unhex-string' for `flycheck-parse-sarif'
(require 'mule-util)             ; `truncate-string-ellipsis', not autoloaded


;; Declare a bunch of dynamic variables that we need from other modes
(defvar sh-shell)                       ; For shell script checker predicates
(defvar ess-language)                   ; For r-lintr predicate
(defvar tramp-remote-process-environment) ; For remote checker env, see below
(defvar markdown-hide-markup)                     ;
(defvar markdown-fontify-code-block-default-mode) ; For rust-error-explainer
(defvar markdown-fontify-code-blocks-natively)    ;

;; Tell the byte compiler about autoloaded functions from packages
(declare-function org-lint "org-lint" (&optional arg))
;; Emacs 30 and newer; guarded with `fboundp' where it is called
(declare-function trusted-content-p "subr" ())
(declare-function xref-push-marker-stack "xref" (&optional m))


;;; Customization
(defgroup flycheck nil
  "Modern on-the-fly syntax checking for GNU Emacs."
  :prefix "flycheck-"
  :group 'tools
  :link '(url-link :tag "Website" "https://www.flycheck.org")
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck"))

(defgroup flycheck-config-files nil
  "Configuration files for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-options nil
  "Options for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-executables nil
  "Executables of syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-faces nil
  "Faces used by on-the-fly syntax checking."
  :prefix "flycheck-"
  :group 'flycheck)

(defcustom flycheck-checkers
  '(ada-gnat
    asciidoctor
    awk-gawk
    bazel-build-buildifier
    bazel-module-buildifier
    bazel-starlark-buildifier
    bazel-workspace-buildifier
    c/c++-clang
    c/c++-gcc
    c/c++-cppcheck
    cfengine
    coffee
    css-stylelint
    cuda-nvcc
    cwl
    d-dmd
    dockerfile-hadolint
    elixir-credo
    emacs-lisp
    emacs-lisp-checkdoc
    ember-template
    erlang-rebar3
    erlang
    fortran-gfortran
    go-gofmt
    go-vet
    go-build
    go-test
    go-errcheck
    go-unconvert
    go-staticcheck
    groovy
    haml-lint
    handlebars
    haskell-stack-ghc
    haskell-ghc
    haskell-hlint
    html-tidy
    javascript-eslint
    javascript-oxlint
    javascript-standard
    json-python-json
    json-jq
    jsonnet
    less
    less-stylelint
    llvm-llc
    lua-luacheck
    lua
    markdown-markdownlint-cli2
    markdown-markdownlint-cli
    markdown-mdl
    markdown-pymarkdown
    nix
    ocaml-dune
    ocaml
    opam
    org-lint
    perl
    perl-perlcritic
    perl-perlimports
    php
    php-phpmd
    php-phpcs
    php-phpcs-changed
    processing
    proselint
    protobuf-protoc
    pug
    puppet-parser
    puppet-lint
    python-ruff
    python-flake8
    python-pylint
    python-pyright
    python-pycompile
    python-mypy
    r-lintr
    r
    racket
    rpm-rpmlint
    rst-sphinx
    rst
    ruby-rubocop
    ruby-chef-cookstyle
    ruby-standard
    ruby-reek
    ruby
    rust-cargo
    rust
    rust-clippy
    salt-lint
    scala
    scala-scalastyle
    scheme-chicken
    sass-stylelint
    scss-stylelint
    sh-bash
    sh-posix-dash
    sh-posix-bash
    sh-zsh
    sh-shellcheck
    slim
    slim-lint
    swift
    sql-sqlint
    statix
    systemd-analyze
    tcl-nagelfar
    terraform
    terraform-tflint
    tex-chktex
    tex-lacheck
    texinfo
    textlint
    verilog-verilator
    vhdl-ghdl
    xml-xmllint
    yaml-actionlint
    yaml-jsyaml
    yaml-yamllint
    ;; Only ever selected when `flycheck-eglot-mode' is on (see its predicate).
    eglot-check
    ;; Only ever selected when `flycheck-lsp-mode' is on (see its predicate).
    flycheck-lsp)
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

(defvar-local flycheck--automatically-disabled-checkers nil
  "List of syntax checkers automatically disabled for this buffer.

A checker can be automatically disabled in two cases:

1. Its `:enabled' predicate returned false.
2. It returned too many errors (see `flycheck-checker-error-threshold')
   and `flycheck-checker-error-threshold-action' is set to `disable'.

To trigger a reverification from Emacs Lisp code, do not modify
this variable: use `flycheck-reset-enabled-checker'.")

(defvar-local flycheck--suppressed-error-count 0
  "Number of errors suppressed in the last syntax check.

Incremented when a syntax checker exceeds
`flycheck-checker-error-threshold' and the excessive errors are
truncated, per `flycheck-checker-error-threshold-action'.")

(defvar-local flycheck--excessive-checkers nil
  "Syntax checkers that last exceeded the error threshold.

Used to notify about the threshold only when a checker newly
exceeds it, instead of after every syntax check.")

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
file.  See Info Node `(Emacs)Specifying File Variables' for more
information about file variables.")
(put 'flycheck-checker 'safe-local-variable 'flycheck-registered-checker-p)

(defcustom flycheck-locate-config-file-functions nil
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
errors than the value of this variable,
`flycheck-checker-error-threshold-action' determines what happens
to the excessive errors."
  :group 'flycheck
  :type '(choice (const :tag "Do not limit reported errors" nil)
                 (integer :tag "Maximum number of errors"))
  :risky t
  :package-version '(flycheck . "0.22"))

(defcustom flycheck-interrupt-running-checks 10
  "When a new syntax check should interrupt one already running.

The value is a number of seconds, t or nil (see below); it is not a
plain on/off flag.

When a syntax check is triggered while one is already running in
the buffer, the running check can either be interrupted, so that
the new check starts immediately and its results reflect the
latest buffer contents, or the new check can be deferred until
the running one finishes, like older Flycheck versions did.

If the value is a number, interrupt running checks that are
younger than that many seconds, and defer behind older ones.
This is the default: fast syntax checkers restart immediately on
every change, while slow checkers (think cargo or mypy on a large
project) are left alone once they have made real progress, so
they complete and publish their results instead of being
restarted forever.

If t, always interrupt running checks.  If nil, never interrupt,
and always defer new checks, as older Flycheck versions did.

Regardless of the value, checks triggered on every keystroke (the
`new-line' condition) coalesce behind a running check, and
checkers without an `:interrupt' function are never interrupted.
Interactive checks (\\[flycheck-buffer]) are exempt from the age
limit -- you asked for fresh results explicitly -- but nil
disables interruption even for them.

This variable is buffer-local friendly: set it to nil via file or
directory local variables in projects whose syntax checkers you
never want interrupted."
  :group 'flycheck
  :type '(choice (number :tag "Interrupt checks younger than N seconds")
                 (const :tag "Always interrupt" t)
                 (const :tag "Never interrupt; defer new checks" nil))
  :safe (lambda (value) (or (booleanp value) (numberp value)))
  :package-version '(flycheck . "37"))

(defcustom flycheck-checker-error-threshold-action 'truncate
  "What to do when a checker exceeds `flycheck-checker-error-threshold'.

`truncate'
     Keep the most severe errors up to the threshold and discard
     the rest.  The mode line indicates that some errors were
     suppressed.  This is the default.

`disable'
     Discard all errors reported by the syntax checker and
     disable it in the buffer for subsequent syntax checks.
     The checker can be re-enabled with
     \\[universal-argument] \\[flycheck-disable-checker].
     Compared to `truncate' this avoids re-parsing excessive
     output on every syntax check, at the cost of no feedback at
     all."
  :group 'flycheck
  :type '(choice (const :tag "Keep the most severe errors" truncate)
                 (const :tag "Disable the checker in the buffer" disable))
  :safe #'symbolp
  :package-version '(flycheck . "37"))

(defcustom flycheck-process-error-functions nil
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

(defcustom flycheck-auto-display-errors-after-checking t
  "Whether to automatically display errors at the current point after checking.

Set this to nil to keep Flycheck from displaying error messages on its
own.  That is useful together with `flycheck-posframe', which would
otherwise display errors at point over and over."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "35")
  :safe #'booleanp)

(defcustom flycheck-display-errors-delay 0.9
  "Delay in seconds before displaying errors at point.

Use floating point numbers to express fractions of seconds."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "0.15")
  :safe #'numberp)

(defcustom flycheck-display-errors-function #'flycheck-display-errors-via-eldoc
  "Function to display error messages.

If set to a function, call the function with the list of errors
to display as single argument.  Each error is an instance of the
`flycheck-error' struct.

With the default value `flycheck-display-errors-via-eldoc',
errors at point are documented through Eldoc.  This composes with
other Eldoc sources (e.g. Eglot) and honors Eldoc display
customizations such as `eldoc-echo-area-use-multiline-p' or
alternative Eldoc frontends.  Use \\[eldoc-doc-buffer] to read
messages that don't fit into the echo area in full.

If set to nil, do not display errors at all."
  :group 'flycheck
  :type '(choice (const :tag "Display errors via Eldoc"
                        flycheck-display-errors-via-eldoc)
                 (const :tag "Display error messages"
                        flycheck-display-error-messages)
                 (const :tag "Display error messages only if no error list"
                        flycheck-display-error-messages-unless-error-list)
                 (function :tag "Error display function"))
  :package-version '(flycheck . "37")
  :risky t)

(defcustom flycheck-clear-displayed-errors-function #'flycheck-clear-displayed-error-messages
  "Function to hide error message displayed by `flycheck-display-errors-function'.

If set to a function, it will be called with no arguments to
clear all displayed errors at point."
  :group 'flycheck
  :type '(choice (const :tag "Clear displayed error messages"
                        flycheck-clear-displayed-error-messages)
                 (function :tag "Clear displayed errors function"))
  :package-version '(flycheck . "34.2")
  :risky t)

(defcustom flycheck-help-echo-function #'flycheck-help-echo-all-error-messages
  "Function to compute the contents of the error tooltips.

If set to a function, call the function with the list of errors
to display as single argument.  Each error is an instance of the
`flycheck-error' struct.  The function is used to set the
help-echo property of flycheck error overlays.  It should return
a string, which is displayed when the user hovers over an error
or presses \\[display-local-help].

If set to nil, do not show error tooltips."
  :group 'flycheck
  :type '(choice (const :tag "Concatenate error messages to form a tooltip"
                        flycheck-help-echo-all-error-messages)
                 (function :tag "Help echo function"))
  :package-version '(flycheck . "0.25")
  :risky t)

(defcustom flycheck-command-wrapper-function #'identity
  "Function to modify checker commands before execution.

The value of this option is a function which is given a list
containing the full command of a syntax checker after
substitution through `flycheck-substitute-argument' but before
execution.  The function may return a new command for Flycheck to
execute.

The default value is `identity' which does not change the
command.  You may provide your own function to run Flycheck
commands through `bundle exec', `nix-shell' or similar wrappers."
  :group 'flycheck
  :type '(choice (const :tag "Do not modify commands" identity)
                 (function :tag "Modify command with a custom function"))
  :package-version '(flycheck . "0.25")
  :risky t)

(defcustom flycheck-executable-find #'flycheck-default-executable-find
  "Function to search for executables.

The value of this option is a function which is given the name or
path of an executable and shall return the full path to the
executable, or nil if the executable does not exist.

The default is `flycheck-default-executable-find', which searches
variable `exec-path' when given a command name, and resolves
paths to absolute ones.  You can customize this option to search
for checkers in other environments such as bundle or NixOS
sandboxes."
  :group 'flycheck
  :type '(choice
          (const :tag "Search executables in `exec-path'"
                 flycheck-default-executable-find)
          (function :tag "Search executables with a custom function"))
  :package-version '(flycheck . "32")
  :risky t)

(defun flycheck-default-executable-find (executable)
  "Resolve EXECUTABLE to a full path.

Like `executable-find', but supports relative paths, and resolves
EXECUTABLE on the remote host when `default-directory' is remote,
so that checkers can run over TRAMP.

Attempts invoking `executable-find' first; if that returns nil,
and EXECUTABLE contains a directory component, expands to a full
path and tries invoking `executable-find' again."
  (let ((remote (file-remote-p default-directory)))
    ;; file-name-directory returns non-nil iff the given path has a
    ;; directory component.
    (or
     (executable-find executable remote)
     (when (file-name-directory executable)
       (executable-find (expand-file-name executable) remote)))))

(defcustom flycheck-indication-mode 'auto
  "The indication mode for Flycheck errors.

This variable controls how Flycheck indicates errors in buffers.
May be `auto', `left-fringe', `right-fringe', `left-margin',
`right-margin', or nil.

If set to `auto', indicate errors in the left fringe on graphical
displays, and in the left margin on text terminals, where fringes
are not available.  This is the default.

If set to `left-fringe' or `right-fringe', indicate errors via
icons in the left and right fringe respectively.  If set to
`left-margin' or `right-margin', use the margins instead.

If set to nil, do not indicate errors and warnings, but just
highlight them according to `flycheck-highlighting-mode'."
  :group 'flycheck
  :type '(choice (const :tag "Automatically choose fringe or margin" auto)
                 (const :tag "Indicate in the left fringe" left-fringe)
                 (const :tag "Indicate in the right fringe" right-fringe)
                 (const :tag "Indicate in the left margin" left-margin)
                 (const :tag "Indicate in the right margin" right-margin)
                 (const :tag "Do not indicate" nil))
  :safe #'symbolp
  :package-version '(flycheck . "37"))

(defcustom flycheck-fixable-indicator t
  "Whether to mark lines whose error carries a fix with a distinct indicator.

When non-nil and `flycheck-indication-mode' shows indicators, a line
whose error has a machine-applicable fix (applicable with
\\[flycheck-fix-error-at-point]) uses a distinct fringe bitmap or margin
string in the error's colour, in the spirit of an editor's \"fix
available\" lightbulb.  Set to nil to use the usual level indicator for
fixable and non-fixable errors alike."
  :group 'flycheck
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "38"))

(defcustom flycheck-highlighting-mode 'symbols
  "The highlighting mode for Flycheck errors and warnings.

The highlighting mode controls how Flycheck highlights errors in
buffers when a checker only reports the starting position of an
error.  The following modes are known:

`columns'
     Highlight a single character.  If the error does not have a column,
     highlight the whole line.

`symbols'
     Highlight a full symbol if there is any, otherwise behave like `columns'.
     This is the default.

`sexps'
     Highlight a full expression, if there is any, otherwise behave like
     `columns'.  Note that this mode can be *very* slow in some major modes.

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

(defvar flycheck-current-errors)
(defun flycheck-refresh-fringes-and-margins ()
  "Refresh fringes and margins of all windows displaying the current buffer.

If any errors are currently shown, launch a new check, to adjust
to a potential new indication mode."
  (dolist (win (get-buffer-window-list))
    (set-window-margins win left-margin-width right-margin-width)
    (set-window-fringes win left-fringe-width right-fringe-width))
  (when flycheck-current-errors
    (flycheck-buffer)))

(defun flycheck--resolve-indication-mode ()
  "Resolve `flycheck-indication-mode' to a concrete side, or nil.

The value `auto' resolves to `left-fringe' when the current
buffer is displayed on a graphical frame with a visible left
fringe, and to `left-margin' otherwise; fringes are not available
on text terminals.

The resolution considers the frame of the first window displaying
the buffer, falling back to the selected frame when the buffer is
not displayed anywhere."
  (if (not (eq flycheck-indication-mode 'auto))
      flycheck-indication-mode
    (let* ((window (get-buffer-window (current-buffer) 'visible))
           (frame (if window (window-frame window) (selected-frame))))
      (if (and (display-graphic-p frame)
               ;; The buffer-local fringe width takes precedence over
               ;; the frame's fringe; nil means inherit from the frame
               (> (or left-fringe-width
                      (frame-parameter frame 'left-fringe)
                      0)
                  0))
          'left-fringe
        'left-margin))))

(defvar-local flycheck--provisioned-margin nil
  "The margin side that Flycheck widened in this buffer, if any.")

(defun flycheck--margin-width-var (side)
  "Return the margin width variable for margin SIDE."
  (if (eq side 'left-margin) 'left-margin-width 'right-margin-width))

(defun flycheck--update-window-margins ()
  "Apply the buffer's margin widths to all windows displaying it.

Unlike `flycheck-refresh-fringes-and-margins' this doesn't launch
a new syntax check, so it is safe to call while reporting errors."
  (dolist (win (get-buffer-window-list nil nil t))
    (set-window-margins win left-margin-width right-margin-width)))

(defun flycheck--sync-margin ()
  "Reconcile the widened margin with the resolved indication mode.

When `flycheck-indication-mode' resolves to a margin that isn't
visible, widen it by one column, and remember doing so in
`flycheck--provisioned-margin'.  Undo a previous widening when
indicators no longer resolve to that margin, e.g. after the
buffer moved to a graphical frame.  Margins configured by the
user or other packages are left alone."
  (let ((side (flycheck--resolve-indication-mode)))
    (unless (eq side flycheck--provisioned-margin)
      (flycheck--release-margin))
    (when (memq side '(left-margin right-margin))
      (let ((width-var (flycheck--margin-width-var side)))
        ;; A nil margin width also means no margin
        (when (zerop (or (symbol-value width-var) 0))
          (set width-var 1)
          (setq flycheck--provisioned-margin side)
          (flycheck--update-window-margins))))))

(defun flycheck--release-margin ()
  "Undo the margin widening done by `flycheck--sync-margin'."
  (when flycheck--provisioned-margin
    (let ((width-var (flycheck--margin-width-var
                      flycheck--provisioned-margin)))
      ;; Leave the margin alone if something else widened it meanwhile.
      ;; Another package could also render into the one-column margin we
      ;; widened without changing its width; that cannot be detected, so
      ;; the column is reclaimed regardless.
      (when (eql (symbol-value width-var) 1)
        (set width-var 0))
      (setq flycheck--provisioned-margin nil)
      (flycheck--update-window-margins))))

(defun flycheck-set-indication-mode (&optional mode)
  "Set `flycheck-indication-mode' to MODE in the current buffer.

Widen the margin of the current buffer if MODE requires one that
is not visible, as by `flycheck--sync-margin'.  When MODE is nil,
only adjust the margins for the current value of
`flycheck-indication-mode'.

This function no longer shrinks fringes or margins configured by
you or other packages; set the fringe and margin width variables
directly to reclaim the space of unused indication areas."
  (interactive (list (intern (completing-read
                              "Mode: " '("auto" "left-fringe" "right-fringe"
                                         "left-margin" "right-margin")
                              nil t nil nil
                              (prin1-to-string flycheck-indication-mode)))))
  (when mode
    (unless (memq mode '(auto left-fringe right-fringe
                              left-margin right-margin))
      (user-error "Invalid indication mode: %S" mode))
    (setq-local flycheck-indication-mode mode))
  (flycheck--sync-margin)
  (pcase (flycheck--resolve-indication-mode)
    ((and (or `left-fringe `right-fringe) side)
     ;; Unlike margins, fringes configured away are not widened back;
     ;; at least tell the user why nothing shows up
     (let ((width-var (if (eq side 'left-fringe)
                          'left-fringe-width
                        'right-fringe-width)))
       (when (zerop (or (symbol-value width-var)
                        (frame-parameter nil side)
                        0))
         (message "The %s is disabled in this buffer; customize `%s' \
to make Flycheck's indicators visible" side width-var)))))
  (flycheck-refresh-fringes-and-margins))

(define-widget 'flycheck-highlighting-style 'lazy
  "A value for `flycheck-highlighting-style'."
  :offset 2
  :format "%t: Use %v"
  :type '(choice
          :format "%[Value Menu%] %v"
          (const :tag "no highlighting" nil)
          (const :tag "a face indicating the error level" level-face)
          (list :tag "a pair of delimiters"
                (const :format "" delimiters)
                (string :tag "Before")
                (string :tag "After"))
          (list :tag "a conditional mix of styles"
                (const :format "" conditional)
                (integer :tag "Up to this many lines")
                (flycheck-highlighting-style :format "Use %v")
                (flycheck-highlighting-style :format "Otherwise, use %v"))))

(defun flycheck--make-highlighting-delimiter (char)
  "Make a highlighting bracket symbol by repeating CHAR twice."
  (compose-chars ?\s
                 ;; '(Bl . Br) ?\s
                 '(Bc Br 30 0) char
                 '(Bc Bl -30 0) char))

(defcustom flycheck-highlighting-style
  `(conditional 4 level-face (delimiters "" ""))
  "The highlighting style for Flycheck errors and warnings.

The highlighting style controls how Flycheck highlights error
regions in buffers.  The following styles are supported:

nil
     Do not highlight errors.  Same as setting
     `flycheck-highlighting-mode' to nil.

`level-face'
     Choose a face depending on the severity of the error, and
     apply it to the whole error text.  See also the
     `flycheck-define-error-level' and `flycheck-error',
     `flycheck-warning', and `flycheck-info' faces.

\(`delimiters' BEFORE AFTER)
     Draw delimiters on each side of the error.  BEFORE and AFTER
     indicate which delimiters to use.  If they are strings, they
     are used as-is.  If they are characters, they are repeated
     twice and composed into a single character.  Delimiters use
     the fringe face corresponding to the severity of each error,
     as well as the `flycheck-error-delimiter' face.  Delimited
     text has the `flycheck-delimited-error' face.

\(`conditional' NLINES S1 S2)
     Use style S1 for errors spanning up to NLINES lines, and
     style S2 otherwise.

See also `flycheck-highlighting-mode' and
`flycheck-indication-mode'."
  :group 'flycheck
  :type 'flycheck-highlighting-style
  :package-version '(flycheck . "32")
  :safe t)

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

`idle-buffer-switch'
     Check syntax a short time (see `flycheck-idle-buffer-switch-delay')
     after the user switches to a buffer.

`new-line'
     Check syntax immediately after a new line was inserted into
     the buffer.

`mode-enabled'
     Check syntax immediately when variable `flycheck-mode' is
     non-nil.

Flycheck performs syntax checks only on events, which are
contained in this list.  For instance, if the value of this
variable is `(mode-enabled save)', Flycheck will only check if
the mode is enabled or the buffer was saved, but never after
changes to the buffer contents.

If nil, never check syntax automatically.  In this case, use
`flycheck-buffer' to start a syntax check manually."
  :group 'flycheck
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After the buffer was changed and idle" idle-change)
              (const
               :tag "After switching the current buffer" idle-buffer-switch)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After `flycheck-mode' was enabled" mode-enabled))
  :package-version '(flycheck . "0.12")
  :safe #'flycheck-symbol-list-p)

(defcustom flycheck-check-syntax-automatically-remote '(save mode-enabled)
  "When Flycheck should check syntax automatically in remote buffers.

Like `flycheck-check-syntax-automatically', but used for buffers
visiting remote files (see `file-remote-p').  Checking a remote
buffer spawns a process on the remote host over TRAMP, which is
slow, so the change-driven triggers (`idle-change', `new-line',
`idle-buffer-switch') are excluded by default and remote buffers
are only checked on `save' and `mode-enabled'.

Set to the symbol t to check remote buffers on the same events as
local ones, i.e. to use `flycheck-check-syntax-automatically'
unchanged.  A manual \\[flycheck-buffer] always works regardless
of this option."
  :group 'flycheck
  :type '(choice
          (const :tag "Same as local buffers" t)
          (set (const :tag "After the buffer was saved" save)
               (const :tag "After the buffer was changed and idle" idle-change)
               (const
                :tag "After switching the current buffer" idle-buffer-switch)
               (const :tag "After a new line was inserted" new-line)
               (const :tag "After `flycheck-mode' was enabled" mode-enabled)))
  :package-version '(flycheck . "38")
  :safe (lambda (value) (or (eq value t) (flycheck-symbol-list-p value))))

(defcustom flycheck-idle-change-delay 0.5
  "How many seconds to wait after a change before checking syntax.

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

(defcustom flycheck-idle-buffer-switch-delay 0.5
  "How many seconds to wait after switching buffers before checking syntax.

After the user switches to a new buffer, Flycheck will wait as
many seconds as the value of this variable before starting a
syntax check.  If the user switches to another buffer during this
time, whether a syntax check is still performed depends on the
value of `flycheck-buffer-switch-check-intermediate-buffers'.

This variable has no effect if `idle-buffer-switch' is not
contained in `flycheck-check-syntax-automatically'."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "32")
  :safe #'numberp)

(defcustom flycheck-buffer-switch-check-intermediate-buffers nil
  "Whether to check syntax in a buffer you only visit briefly.

If nil, then when you switch to a buffer but switch to another
buffer before the syntax check is performed, then the check is
canceled.  If non-nil, then syntax checks due to switching
buffers are always performed.  This only affects buffer switches
that happen less than `flycheck-idle-buffer-switch-delay' seconds
apart.

This variable has no effect if `idle-buffer-switch' is not
contained in `flycheck-check-syntax-automatically'."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "32")
  :safe #'booleanp)

(defcustom flycheck-standard-error-navigation t
  "Whether to support error navigation with `next-error'.

If non-nil, enable navigation of Flycheck errors with
`next-error', `previous-error' and `first-error'.  Otherwise,
these functions just navigate errors from compilation modes.

Flycheck error navigation with `flycheck-next-error',
`flycheck-previous-error' and `flycheck-first-error' is always
enabled, regardless of the value of this variable.

Note that this setting only takes effect when variable
`flycheck-mode' is non-nil.  Changing it will not affect buffers
where variable `flycheck-mode' is already non-nil."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "0.15")
  :safe #'booleanp)

(define-widget 'flycheck-minimum-level 'lazy
  "A radio-type choice of minimum error levels.

See `flycheck-navigation-minimum-level' and
`flycheck-error-list-minimum-level'."
  :type '(radio (const :tag "All locations" nil)
                (const :tag "Informational messages" info)
                (const :tag "Warnings" warning)
                (const :tag "Errors" error)
                (symbol :tag "Custom error level")))

(defcustom flycheck-navigation-minimum-level nil
  "The minimum level of errors to navigate.

If set to an error level, only navigate errors whose error level
is at least as severe as this one.  If nil, navigate all errors."
  :group 'flycheck
  :type 'flycheck-minimum-level
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "0.21"))

(defcustom flycheck-error-list-minimum-level nil
  "The minimum level of errors to display in the error list.

If set to an error level, only display errors whose error level
is at least as severe as this one in the error list.  If nil,
display all errors.

This is the default level, used when the error list is opened.
You can temporarily change the level using
\\[flycheck-error-list-set-filter], or reset it to this value
using \\[flycheck-error-list-reset-filter]."
  :group 'flycheck
  :type 'flycheck-minimum-level
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "0.24"))

(defcustom flycheck-relevant-error-other-file-minimum-level 'error
  "The minimum level of errors from other files to display in this buffer.

If set to an error level, only display errors from other files
whose error level is at least as severe as this one.  If nil,
display all errors from other files."
  :group 'flycheck
  :type 'flycheck-minimum-level
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "32"))

(defcustom flycheck-relevant-error-other-file-show t
  "Whether to show errors from other files.

When non-nil, errors reported by a checker that reference files
other than the one being checked are shown in the error list and
highlighted in the buffer."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "32")
  :safe #'booleanp)

(defcustom flycheck-temp-prefix "flycheck"
  "Prefix for temporary files created by Flycheck."
  :group 'flycheck
  :type 'string
  :package-version '(flycheck . "0.19")
  :risky t)

(defcustom flycheck-mode-hook nil
  "Hooks to run after command `flycheck-mode' is toggled."
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
hook function takes the status symbol as single argument, as
given to `flycheck-report-status', which see.

This variable is an abnormal hook.  See Info
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

(defface flycheck-error-delimiter
  `((t))
  "Flycheck face for errors spanning multiple lines.

See `flycheck-highlighting-style' for details on when this face
is used."
  :package-version '(flycheck . "32")
  :group 'flycheck-faces)

(defface flycheck-delimited-error
  `((t))
  "Flycheck face for errors spanning multiple lines.

See `flycheck-highlighting-style' for details on when this face
is used."
  :package-version '(flycheck . "32")
  :group 'flycheck-faces)

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

(defface flycheck-unnecessary
  '((t :inherit shadow))
  "Flycheck face for code an error marks as having no effect.

Added to the error's own face, not used instead of it, so an unused
import still shows that it is a warning.  Matches how Eglot renders the
same LSP tag."
  :package-version '(flycheck . "39")
  :group 'flycheck-faces)

(defface flycheck-deprecated
  '((t :inherit shadow :strike-through t))
  "Flycheck face for code an error marks as deprecated.

Added to the error's own face, not used instead of it.  Matches how
Eglot renders the same LSP tag."
  :package-version '(flycheck . "39")
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

(defface flycheck-error-list-line-number
  '((t))
  "Face for line numbers in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.16"))

(defface flycheck-error-list-column-number
  '((t))
  "Face for column numbers in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.16"))

(defface flycheck-error-list-filename
  '((t :inherit mode-line-buffer-id :bold nil))
  "Face for filenames in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "32"))

(defface flycheck-error-list-id
  '((t :inherit font-lock-type-face))
  "Face for the error ID in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.22"))

(defface flycheck-error-list-id-with-explainer
  '((t :inherit flycheck-error-list-id
       :box (:style released-button)))
  "Face for the error ID in the error list, for errors that have an explainer."
  :group 'flycheck-faces
  :package-version '(flycheck . "30"))

(defface flycheck-error-list-checker-name
  '((t :inherit font-lock-function-name-face))
  "Face for the syntax checker name in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.21"))

(defface flycheck-error-list-error-message
  '((t))
  "Face for the error message in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "33"))

(defface flycheck-error-list-highlight
  '((t :bold t))
  "Flycheck face to highlight errors in the error list."
  :package-version '(flycheck . "0.15")
  :group 'flycheck-faces)

(defface flycheck-verify-select-checker
  '((t :box (:style released-button)))
  "Flycheck face for the `select' button in the verify setup buffer."
  :package-version '(flycheck . "32")
  :group 'flycheck-faces)

(defvar flycheck-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c"         #'flycheck-buffer)
    (define-key map "C"         #'flycheck-clear)
    (define-key map (kbd "C-c") #'flycheck-compile)
    (define-key map "n"         #'flycheck-next-error)
    (define-key map "p"         #'flycheck-previous-error)
    (define-key map "l"         #'flycheck-list-errors)
    (define-key map (kbd "C-w") #'flycheck-copy-errors-as-kill)
    (define-key map "s"         #'flycheck-select-checker)
    (define-key map "?"         #'flycheck-describe-checker)
    (define-key map "h"         #'flycheck-display-error-at-point)
    (define-key map "e"         #'flycheck-explain-error-at-point)
    (define-key map "j"         #'flycheck-visit-related-location)
    (define-key map "f"         #'flycheck-fix-error-at-point)
    (define-key map "F"         #'flycheck-fix-all-errors)
    (define-key map "H"         #'display-local-help)
    (define-key map "i"         #'flycheck-manual)
    (define-key map "V"         #'flycheck-version)
    (define-key map "v"         #'flycheck-verify-setup)
    (define-key map "x"         #'flycheck-disable-checker)
    map)
  "Keymap of Flycheck interactive commands.")

(fset 'flycheck-command-map flycheck-command-map)

(defcustom flycheck-keymap-prefix (kbd "C-c !")
  "Prefix for key bindings of Flycheck.

Changing this variable outside Customize does not have any
effect.  To change the keymap prefix from Lisp, use
`customize-set-variable':

    (customize-set-variable \\='flycheck-keymap-prefix (kbd \"C-c f\"))

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

(defcustom flycheck-mode-line-color t
  "Whether to color the Flycheck mode line status (on by default)."
  :group 'flycheck
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "35"))

(defcustom flycheck-mode-line-prefix "FlyC"
  "Base mode line lighter for Flycheck.

This will have an effect only with the default
`flycheck-mode-line'.

If you've customized `flycheck-mode-line' then the customized
function must be updated to use this variable."
  :group 'flycheck
  :type 'string
  :safe #'stringp
  :package-version '(flycheck . "26"))

(define-obsolete-variable-alias 'flycheck-mode-success-indicator
  'flycheck-mode-line-success-indicator "39")
(defcustom flycheck-mode-line-success-indicator ":0"
  "Success indicator appended to `flycheck-mode-line-prefix'."
  :group 'flycheck
  :type 'string
  :safe #'stringp
  :package-version '(flycheck . "35"))

(defcustom flycheck-error-list-mode-line
  `(,(propertized-buffer-identification "%12b")
    " for buffer "
    (:eval (flycheck-error-list-propertized-source-name))
    (:eval (flycheck-error-list-mode-line-scope-indicator))
    (:eval (flycheck-error-list-mode-line-filter-indicator))
    (:eval (flycheck-error-list-mode-line-suppressed-indicator)))
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

(defcustom flycheck-error-list-display-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.25)
    (preserve-size . (nil . t)))
  "The `display-buffer' action for the error list buffer.

By default the error list pops up in a side window at the bottom
of the frame, a quarter of the frame tall.  Set to nil to fall
back to the default behavior of `display-buffer'.

Entries in `display-buffer-alist' matching the error list buffer
take precedence over this action, so this option composes with
window-management configurations."
  :group 'flycheck
  :type 'sexp
  :risky t
  :package-version '(flycheck . "37"))

(defcustom flycheck-global-modes t
  "Modes for which option `flycheck-mode' is turned on.

If t, Flycheck Mode is turned on for all major modes.  If a list,
Flycheck Mode is turned on for all `major-mode' symbols in that
list.  If the `car' of the list is `not', Flycheck Mode is turned
on for all `major-mode' symbols _not_ in that list.  If nil,
Flycheck Mode is never turned on by command
`global-flycheck-mode'.

Note that Flycheck is never turned on for modes whose
`mode-class' property is `special' (see Info node `(elisp)Major
Mode Conventions'), regardless of the value of this option.

Only has effect when variable `global-flycheck-mode' is non-nil."
  :group 'flycheck
  :type '(choice (const :tag "none" nil)
                 (const :tag "all" t)
                 (set :menu-tag "mode specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode"))))
  :risky t
  :package-version '(flycheck . "0.23"))

;; Add built-in functions to our hooks, via `add-hook', to make sure that our
;; functions are really present, even if the variable was implicitly defined by
;; another call to `add-hook' that occurred before Flycheck was loaded.  See
;; https://lists.gnu.org/archive/html/emacs-devel/2015-02/msg01271.html for why
;; we don't initialize the hook variables right away.  We append our own
;; functions, because a user likely expects that their functions come first,
;; even if they added them before Flycheck was loaded.
(dolist (hook (list #'flycheck-locate-config-file-by-path
                    #'flycheck-locate-config-file-ancestor-directories
                    #'flycheck-locate-config-file-home))
  (add-hook 'flycheck-locate-config-file-functions hook 'append))

(add-hook 'flycheck-process-error-functions #'flycheck-add-overlay 'append)


;;; Global Flycheck menu
(defvar flycheck-mode-menu-map
  (easy-menu-create-menu
   "Syntax Checking"
   '(["Enable on-the-fly syntax checking" flycheck-mode
      :style toggle :selected flycheck-mode
      :enable (or flycheck-mode
                  ;; Don't let users toggle the mode if there is no syntax
                  ;; checker for this buffer
                  (seq-find #'flycheck-checker-supports-major-mode-p
                            flycheck-checkers))]
     ["Check current buffer" flycheck-buffer flycheck-mode]
     ["Clear errors in buffer" flycheck-clear t]
     ["Run checker as compile command" flycheck-compile flycheck-mode]
     "---"
     ["Go to next error" flycheck-next-error flycheck-mode]
     ["Go to previous error" flycheck-previous-error flycheck-mode]
     ["Show all errors" flycheck-list-errors flycheck-mode]
     ["Show messages inline" flycheck-annotate-mode
      :style toggle :selected (bound-and-true-p flycheck-annotate-mode)]
     "---"
     ["Copy messages at point" flycheck-copy-errors-as-kill
      (flycheck-overlays-at (point))]
     ["Explain error at point" flycheck-explain-error-at-point]
     ["Visit related location" flycheck-visit-related-location
      (flycheck-related-location-at-point)]
     ["Apply fix at point" flycheck-fix-error-at-point]
     ["Apply all fixes in buffer" flycheck-fix-all-errors]
     "---"
     ["Select syntax checker" flycheck-select-checker flycheck-mode]
     ["Disable syntax checker" flycheck-disable-checker flycheck-mode]
     ["Set executable of syntax checker" flycheck-set-checker-executable
      flycheck-mode]
     "---"
     ["Describe syntax checker" flycheck-describe-checker t]
     ["Verify setup" flycheck-verify-setup t]
     ["Show Flycheck version" flycheck-version t]
     ["Flycheck quick help" flycheck-quick-help t]
     ["Read the Flycheck manual" flycheck-manual t]))
  "Menu of command `flycheck-mode'.")

(when (lookup-key global-map [menu-bar tools])
  (easy-menu-add-item nil '("Tools") flycheck-mode-menu-map "Spell Checking"))



(defconst flycheck-version "39.0-snapshot"
  "The current version of Flycheck.

Kept in sync with the `Version' header and the Eask package version, which
a spec enforces.  Used as fallback when `package-get-version' returns nil,
which is the case when Flycheck was not installed as a package.")

(defun flycheck--pkg-version ()
  "Extract FLYCHECK's package version from its package metadata."
  (or (package-get-version) flycheck-version))

;;; Version information, manual and loading of Flycheck
(defun flycheck-version (&optional show-version)
  "Get the Flycheck version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both are present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (flycheck--pkg-version)))
    (when show-version
      (message "Flycheck version: %s" version))
    version))

(defun flycheck-unload-function ()
  "Unload function for Flycheck."
  (global-flycheck-mode -1)
  (when (lookup-key global-map [menu-bar tools])
    (easy-menu-remove-item nil '("Tools") (cadr flycheck-mode-menu-map)))
  (remove-hook 'kill-emacs-hook #'flycheck-global-teardown)
  (setq find-function-regexp-alist
        (assq-delete-all 'flycheck-checker find-function-regexp-alist)))

;;;###autoload
(defun flycheck-manual ()
  "Open the Flycheck manual."
  (interactive)
  (browse-url "https://www.flycheck.org"))

;;;###autoload
(defun flycheck-quick-help ()
  "Display brief Flycheck help."
  (interactive)
  (with-current-buffer (get-buffer-create "*flycheck-quick-help*")
    (with-help-window (current-buffer)
      (flycheck-mode) ;; so that we can expand \\[flycheck-<function>]
      (let ((help
             (substitute-command-keys
        "Flycheck automatically runs checks on writable files when changed.
Mode line status for the current buffer:
  FlyC        Not been checked yet
  FlyC*       Flycheck is running
  FlyC:0      Last check resulted in no errors and no warnings
  FlyC:3|5|1  Checker reported three errors, five warnings and one info
  FlyC:3|5|1+ Some errors were suppressed over the error threshold
  FlyC-       No checker available
  FlyC!       The checker crashed
  FlyC.       The last syntax check was manually interrupted
  FlyC?       The checker did something unexpected

Key bindings:
  \\[flycheck-buffer]     Check current buffer
  \\[flycheck-clear]     Clear errors in current buffer
  \\[flycheck-compile]   Run checker as compile command

  \\[flycheck-next-error]     Next error
  \\[flycheck-previous-error]     Previous error
  \\[flycheck-list-errors]     List all errors

  \\[flycheck-copy-errors-as-kill]   Copy error messages at point
  \\[flycheck-display-error-at-point]     Show error at point
  \\[flycheck-explain-error-at-point]     Explain error at point
  \\[flycheck-visit-related-location]     Visit a related location
  \\[flycheck-fix-error-at-point]     Apply fix at point
  \\[flycheck-fix-all-errors]     Apply all fixes in buffer
")))
        (help-mode)
        (read-only-mode 0)
        (insert help)))))


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
  (and (listp obj) (seq-every-p #'stringp obj)))

(defun flycheck-string-or-string-list-p (obj)
  "Determine if OBJ is a string or a list of strings."
  (or (stringp obj) (flycheck-string-list-p obj)))

(defun flycheck-symbol-list-p (obj)
  "Determine if OBJ is a list of symbols."
  (and (listp obj) (seq-every-p #'symbolp obj)))

(defvar-local flycheck--file-truename-cache nil)

(defun flycheck--file-truename (file)
  "Memoize the result of `file-truename' on (directory-file-name FILE)."
  ;; `file-truename' is slow, but alternatives are incomplete, so memoizing is
  ;; our best bet.  See https://github.com/flycheck/flycheck/pull/1698.
  (unless flycheck--file-truename-cache
    (setq-local flycheck--file-truename-cache (make-hash-table :test 'equal)))
  (or (gethash file flycheck--file-truename-cache)
      (puthash file (file-truename (directory-file-name file))
               flycheck--file-truename-cache)))

(defun flycheck--expand-file-name (filename directory)
  "Expand FILENAME against DIRECTORY, honoring a remote DIRECTORY.

Like `expand-file-name', but when DIRECTORY is remote and
FILENAME is a host-local path -- as a checker running on the
remote host over TRAMP reports -- the result names the file on
that host, so it compares against the remote temporary files and
opens the right file when jumped to."
  (if-let* ((remote (and (not (file-remote-p filename))
                         (file-remote-p directory))))
      (concat remote (expand-file-name filename (file-local-name directory)))
    (expand-file-name filename directory)))

(defun flycheck-buffer-file-local-name (&optional fallback)
  "Return the visited file's name as a plain local name.

Strip any remote (TRAMP) prefix with `file-local-name', so a
checker running on the host of the buffer's file receives a path
that is valid there.  Return FALLBACK when the buffer has no
backing file."
  (if buffer-file-name (file-local-name buffer-file-name) fallback))

(defun flycheck-same-files-p (file-a file-b)
  "Determine whether FILE-A and FILE-B refer to the same file.

Files are the same if (in the order checked) they are equal, or
if they resolve to the same canonical paths."
  (or (string= file-a file-b)
      (string= (flycheck--file-truename file-a)
               (flycheck--file-truename file-b))))

(defvar-local flycheck-temporaries nil
  "Temporary files and directories created by Flycheck.")

(defun flycheck-temp-dir-system ()
  "Create a unique temporary directory.

Use `flycheck-temp-prefix' as prefix, and add the directory to
`flycheck-temporaries'.

Return the path of the directory.

The directory is created on the remote host when
`default-directory' is remote, so that checkers running over
TRAMP can access it."
  (let* ((tempdir (make-nearby-temp-file flycheck-temp-prefix 'directory)))
    (push tempdir flycheck-temporaries)
    tempdir))

(defun flycheck-temp-file-system (filename &optional suffix)
  "Create a temporary file named after FILENAME.

If FILENAME is non-nil, this function creates a temporary
directory with `flycheck-temp-dir-system', and creates a file
with the same name as FILENAME in this directory.

Otherwise this function creates a temporary file starting with
`flycheck-temp-prefix'.  If present, SUFFIX is appended;
otherwise, a random suffix is used.  The path of the file is
added to `flycheck-temporaries'.

Return the path of the file."
  (let ((tempfile (convert-standard-filename
                   (if filename
                       (expand-file-name (file-name-nondirectory filename)
                                         (flycheck-temp-dir-system))
                     (make-nearby-temp-file flycheck-temp-prefix nil suffix)))))
    (push tempfile flycheck-temporaries)
    tempfile))

(defun flycheck-temp-file-inplace (filename &optional suffix)
  "Create an in-place copy of FILENAME.

Prefix the file with `flycheck-temp-prefix' and add the path of
the file to `flycheck-temporaries'.

If FILENAME is nil, fall back to `flycheck-temp-file-system' with
the specified SUFFIX.

Return the path of the file."
  (if filename
      (let* ((tempname (format "%s_%s"
                               flycheck-temp-prefix
                               (file-name-nondirectory filename)))
             (tempfile (convert-standard-filename
                        (expand-file-name tempname
                                          (file-name-directory filename)))))
        (push tempfile flycheck-temporaries)
        tempfile)
    (flycheck-temp-file-system filename suffix)))

(defun flycheck-temp-directory (checker)
  "Return the directory where CHECKER writes temporary files.

Return nil if the CHECKER does not write temporary files."
  (let ((args (flycheck-checker-arguments checker)))
    (cond
     ;; `flycheck-temp-file-system' creates the file with
     ;; `make-nearby-temp-file', i.e. on the host of `default-directory',
     ;; so probe that host's temporary directory, not the local one.
     ((memq 'source args) (temporary-file-directory))
     ((memq 'source-inplace args)
      (if buffer-file-name (file-name-directory buffer-file-name)
        temporary-file-directory))
     (t nil))))

(defun flycheck-temp-files-writable-p (checker)
  "Whether CHECKER can write temporary files.

If CHECKER has `source' or `source-inplace' in its `:command',
return whether flycheck has the permissions to create the
respective temporary files.

Return t if CHECKER does not use temporary files."
  (let ((dir (flycheck-temp-directory checker)))
    (or (not dir) (file-writable-p dir))))

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
              ((and (listp result) (seq-every-p #'stringp result)) result)
              ((stringp result) (list result))
              (t (error "Invalid result type for option: %S" result)))))))
    (seq-mapcat prepend items)))

(defun flycheck-find-in-buffer (pattern)
  "Find PATTERN in the current buffer.

Return the result of the first matching group of PATTERN, or nil,
if PATTERN did not match."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward pattern nil 'no-error)
        (match-string-no-properties 1)))))

(defun flycheck-buffer-empty-p (&optional buffer)
  "Check whether a BUFFER is empty, defaulting to the current one."
  (= (buffer-size buffer) 0))

(defun flycheck-buffer-nonempty-p (&optional buffer)
  "Check whether a BUFFER is nonempty, defaulting to the current one."
  (> (buffer-size buffer) 0))

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
  "Determine whether the current buffer is an autoloads file.

Autoloads are generated by package.el during installation."
  (string-suffix-p "-autoloads.el" (buffer-name)))

(defun flycheck-in-user-emacs-directory-p (filename)
  "Whether FILENAME is in `user-emacs-directory'."
  (string-prefix-p (file-name-as-directory
                    (flycheck--file-truename user-emacs-directory))
                   (flycheck--file-truename filename)))

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
  (seq-do #'flycheck-safe-delete flycheck-temporaries)
  (setq flycheck-temporaries nil))

;; We use a custom tree-walking expander instead of `rx-define' or
;; `rx-let' because those don't support optional arguments with
;; defaults.  For example, `(file-name)' should use a default body
;; while `(file-name SEXP ...)' uses a custom one — this conditional
;; logic cannot be expressed with `rx-define's parameter mechanism,
;; which only supports required args and `&rest'.
(defun flycheck--rx-expand (form)
  "Expand Flycheck-specific rx constructs in FORM.

Recursively walks the form tree and expands `file-name', `line',
`column', `message', `id', `end-line', and `end-column'."
  (pcase form
    ('line '(group-n 2 (one-or-more digit)))
    ('column '(group-n 3 (one-or-more digit)))
    ('end-line '(group-n 6 (one-or-more digit)))
    ('end-column '(group-n 7 (one-or-more digit)))
    (`(file-name . ,body)
     (let ((body (or body '((minimal-match (one-or-more not-newline))))))
       `(group-n 1 ,@(mapcar #'flycheck--rx-expand body))))
    (`(message . ,body)
     (let ((body (or body '((one-or-more not-newline)))))
       `(group-n 4 ,@(mapcar #'flycheck--rx-expand body))))
    (`(id . ,body)
     `(group-n 5 ,@(mapcar #'flycheck--rx-expand body)))
    ((pred consp)
     (mapcar #'flycheck--rx-expand form))
    (_ form)))

(defun flycheck-rx-to-string (form &optional no-group)
  "Like `rx-to-string' for FORM, but with special keywords:

`line'
     matches the initial line number.

`column'
     matches the initial column number.

`end-line'
     matches the final line number.

`end-column'
     matches the final column number (exclusive).


`(file-name SEXP ...)'
     matches the file name.  SEXP describes the file name.  If no
     SEXP is given, use a default body of `(minimal-match
     (one-or-more not-newline))'.

`(message SEXP ...)'
     matches the message.  SEXP constitutes the body of the
     message.  If no SEXP is given, use a default body
     of `(one-or-more not-newline)'.

`(id SEXP ...)'
     matches an error ID.  SEXP describes the ID.

NO-GROUP is passed to `rx-to-string'.

See `rx' for a complete list of all built-in `rx' forms."
  (rx-to-string (flycheck--rx-expand form) no-group))

(defun flycheck-current-load-file ()
  "Get the source file currently being loaded.

Always return the name of the corresponding source file, never
any byte-compiled file.

Return nil, if the currently loaded file cannot be determined."
  (when-let* ((this-file (cond
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

(cl-defstruct (flycheck-line-cache
               (:constructor flycheck-line-cache-new))
  "Cache structure used to speed up `flycheck-goto-line'."
  tick point line)

(defvar-local flycheck--line-cache nil
  "Cache used to speed up `flycheck-goto-line'.")

(defsubst flycheck--init-line-cache ()
  "Initialize or reinitialize `flycheck--line-cache'."
  (let ((tick (buffer-modified-tick)))
    (if flycheck--line-cache
        (unless (= (flycheck-line-cache-tick flycheck--line-cache) tick)
          (setf (flycheck-line-cache-tick flycheck--line-cache) tick
                (flycheck-line-cache-point flycheck--line-cache) 1
                (flycheck-line-cache-line flycheck--line-cache) 1))
      (setq-local flycheck--line-cache
                  (flycheck-line-cache-new :tick tick :point 1 :line 1)))))

(defun flycheck-goto-line (line)
  "Move point to beginning of line number LINE.

This function assumes that the current buffer is not narrowed."
  (flycheck--init-line-cache)
  (goto-char (flycheck-line-cache-point flycheck--line-cache))
  (let ((delta (- line (flycheck-line-cache-line flycheck--line-cache))))
    (when (= 0 (forward-line delta))
      (setf (flycheck-line-cache-point flycheck--line-cache) (point))
      (setf (flycheck-line-cache-line flycheck--line-cache) line))))

(defun flycheck-line-column-to-position (line column)
  "Return the point closest to LINE, COLUMN on line LINE.

COLUMN is one-based."
  (save-excursion
    (flycheck-goto-line line)
    (min (+ (point) (1- column)) (line-end-position))))

(defun flycheck-line-column-at-point ()
  "Return the line and column number at point."
  (cons (line-number-at-pos) (1+ (- (point) (line-beginning-position)))))

(defun flycheck-line-column-at-pos (pos)
  "Return the line and column number at position POS.

COLUMN is one-based."
  (let ((inhibit-field-text-motion t))
    (save-excursion
      (goto-char pos)
      (flycheck-line-column-at-point))))


;;; Minibuffer tools
(defvar flycheck-read-checker-history nil
  "`completing-read' history of `flycheck-read-checker'.")

(defun flycheck-completing-read (prompt candidates default &optional history)
  "Read a value from the minibuffer.

Show PROMPT and read one of CANDIDATES, defaulting to DEFAULT.
HISTORY is passed to `completing-read'.

Note that `completing-read' may return an empty string instead of
nil, even when \"\" isn't among the candidates.  Customize
`completing-read-function' to change the completion UI globally."
  (completing-read prompt candidates nil 'require-match nil history default))

(defun flycheck-read-checker (prompt &optional default property candidates)
  "Read a flycheck checker from minibuffer with PROMPT and DEFAULT.

PROMPT is a string to show in the minibuffer as prompt.  It
should end with a single space.  DEFAULT is a symbol denoting the
default checker to use, if the user did not select any checker.
PROPERTY is a symbol denoting a syntax checker property.  If
non-nil, only complete syntax checkers which have a non-nil value
for PROPERTY.  CANDIDATES is an optional list of all syntax
checkers available for completion, defaulting to all defined
checkers.  If given, PROPERTY is ignored.

Return the checker as symbol, or DEFAULT if no checker was
chosen.  If DEFAULT is nil and no checker was chosen, signal a
`user-error' if the underlying completion system does not provide
a default on its own."
  (when (and default (not (flycheck-valid-checker-p default)))
    (error "%S is no valid Flycheck checker" default))
  (let* ((candidates (mapcar #'symbol-name
                              (or candidates
                                  (flycheck-defined-checkers property))))
         (default (and default (symbol-name default)))
         (input (flycheck-completing-read
                 prompt candidates default
                 'flycheck-read-checker-history)))
    (when (string-empty-p input)
      (unless default
        (user-error "No syntax checker selected"))
      (setq input default))
    (let ((checker (intern input)))
      (unless (flycheck-valid-checker-p checker)
        (error "%S is not a valid Flycheck syntax checker" checker))
      checker)))

(defun flycheck-read-error-level (prompt)
  "Read an error level from the user with PROMPT.

Only offers levels for which errors currently exist, in addition
to the default levels."
  (let* ((levels (mapcar #'flycheck-error-level
                          (flycheck-error-list-current-errors)))
         (levels-with-defaults (append '(info warning error) levels))
         (uniq-levels (seq-uniq levels-with-defaults))
         (level (flycheck-completing-read prompt uniq-levels nil)))
    (when (string-empty-p level) (setq level nil))
    (and level (intern level))))


;;; Checker API
(defun flycheck-defined-checkers (&optional property)
  "Find all defined syntax checkers, optionally with PROPERTY.

PROPERTY is a symbol.  If given, only return syntax checkers with
a non-nil value for PROPERTY.

The returned list is sorted alphabetically by the symbol name of
the syntax checkers."
  (let (defined-checkers)
    (mapatoms (lambda (symbol)
                (when (and (flycheck-valid-checker-p symbol)
                           (or (null property)
                               (flycheck-checker-get symbol property)))
                  (push symbol defined-checkers))))
    (sort defined-checkers #'string<)))

(defun flycheck-registered-checker-p (checker)
  "Determine whether CHECKER is registered.

A checker is registered if it is contained in
`flycheck-checkers'."
  (and (flycheck-valid-checker-p checker)
       (memq checker flycheck-checkers)))

(defun flycheck-disabled-checker-p (checker)
  "Determine whether CHECKER is disabled, manually or automatically."
  (or (flycheck-manually-disabled-checker-p checker)
      (flycheck-automatically-disabled-checker-p checker)))

(defun flycheck-manually-disabled-checker-p (checker)
  "Determine whether CHECKER has been manually disabled.

A checker has been manually disabled if it is contained in
`flycheck-disabled-checkers'."
  (memq checker flycheck-disabled-checkers))

(defun flycheck-automatically-disabled-checker-p (checker)
  "Determine whether CHECKER has been automatically disabled.

A checker has been automatically disabled if it is contained in
`flycheck--automatically-disabled-checkers'."
  (memq checker flycheck--automatically-disabled-checkers))


;;; Generic syntax checkers
(defconst flycheck-generic-checker-version 2
  "The internal version of generic syntax checker declarations.

Flycheck will not use syntax checkers whose generic version is
less than this constant.")

(defsubst flycheck--checker-property-name (property)
  "Return the SYMBOL property for checker PROPERTY."
  (intern (concat "flycheck-" (symbol-name property))))

(defun flycheck-checker-get (checker property)
  "Get the value of CHECKER's PROPERTY."
  (get checker (flycheck--checker-property-name property)))

(gv-define-setter flycheck-checker-get (value checker property)
  `(setf (get ,checker (flycheck--checker-property-name ,property)) ,value))

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

(defun flycheck-define-generic-checker (symbol docstring &rest properties)
  "Define SYMBOL as generic syntax checker.

Any syntax checker defined with this function is eligible for manual
syntax checker selection with `flycheck-select-checker'.  To make
the new syntax checker available for automatic selection, it must
be registered in `flycheck-checkers'.

DOCSTRING is the documentation of the syntax checker, for
`flycheck-describe-checker'.  The following PROPERTIES constitute
a generic syntax checker.  Unless otherwise noted, all properties
are mandatory.

`:start FUNCTION'
     A function to start the syntax checker.

     FUNCTION shall take two arguments and return a context
     object if the checker is started successfully.  Otherwise it
     shall signal an error.

     The first argument is the syntax checker being started.  The
     second is a callback function to report state changes to
     Flycheck.  The callback takes two arguments STATUS DATA,
     where STATUS is a symbol denoting the syntax checker status
     and DATA an optional argument with additional data for the
     status report.  See `flycheck-report-buffer-checker-status'
     for more information about STATUS and DATA.

     FUNCTION may be synchronous or asynchronous, i.e. it may
     call the given callback either immediately, or at some later
     point (e.g. from a process sentinel).

     A syntax checker _must_ call CALLBACK at least once with a
     STATUS that finishes the current syntax checker.  Otherwise
     Flycheck gets stuck at the current syntax check with this
     syntax checker.

     The context object returned by FUNCTION is passed to
     `:interrupt'.

`:interrupt FUNCTION'
     A function to interrupt the syntax check.

     FUNCTION is called with the syntax checker and the context
     object returned by the `:start' function and shall try to
     interrupt the syntax check.  The context may be nil, if the
     syntax check is interrupted before actually started.
     FUNCTION should handle this situation.

     If it cannot interrupt the syntax check, it may either
     signal an error or silently ignore the attempt to interrupt
     the syntax checker, depending on the severity of the
     situation.

     If interrupting the syntax check failed, Flycheck will let
     the syntax check continue, but ignore any status reports.
     Notably, it won't highlight any errors reported by the
     syntax check in the buffer.

     This property is optional.  If omitted, Flycheck won't
     attempt to interrupt syntax checks with this syntax checker,
     and simply ignore their results.

`:print-doc FUNCTION'
     A function to print additional documentation into the Help
     buffer of this checker.

     FUNCTION is called when creating the Help buffer for the
     syntax checker, with the syntax checker as single argument,
     after printing the name of the syntax checker and its modes
     and predicate, but before printing DOCSTRING.  It may insert
     additional documentation into the current buffer.

     The call occurs within `with-help-window'.  Hence
     `standard-output' points to the current buffer, so you may
     use `princ' and friends to add content.  Also, the current
     buffer is put into Help mode afterwards, which automatically
     turns symbols into references, if possible.

     This property is optional.  If omitted, no additional
     documentation is printed for this syntax checker.

:verify FUNCTION
     A function to verify the checker for the current buffer.

     FUNCTION is called with the syntax checker as single
     argument, and shall return a list of
     `flycheck-verification-result' objects indicating whether
     the syntax checker could be used in the current buffer, and
     highlighting potential setup problems.

     This property is optional.  If omitted, no additional
     verification occurs for this syntax checker.  It is however
     absolutely recommended that you add a `:verify' function to
     your syntax checker, because it will help users to spot
     potential setup problems.

`:modes MODES'
     A major mode symbol or a list thereof, denoting major modes
     to use this syntax checker in.

     This syntax checker will only be used in buffers whose
     `major-mode' is contained in MODES.

     If `:predicate' is also given the syntax checker will only
     be used in buffers for which the `:predicate' returns
     non-nil.

`:predicate FUNCTION'
     A function to determine whether to use the syntax checker in
     the current buffer.

     FUNCTION is called without arguments and shall return
     non-nil if this syntax checker shall be used to check the
     current buffer.  Otherwise it shall return nil.

     If this checker has a `:working-directory' FUNCTION is
     called with `default-directory' bound to the checker's
     working directory.

     FUNCTION is only called in matching major modes.

     This property is optional.

`:enabled FUNCTION'
     A function to determine whether to use the syntax checker in
     the current buffer.

     This property behaves as `:predicate', except that it's only
     called the first time a syntax checker is to be used in a buffer.

     FUNCTION is called without arguments and shall return
     non-nil if this syntax checker shall be used to check the
     current buffer.  Otherwise it shall return nil.

     If FUNCTION returns a non-nil value the checker is put in a
     whitelist in `flycheck--automatically-enabled-checkers' to
     prevent further invocations of `:enabled'.  Otherwise it is
     disabled via `flycheck--automatically-disabled-checkers' to
     prevent any further use of it.

     If this checker has a `:working-directory' FUNCTION is
     called with `default-directory' bound to the checker's
     working directory.

     FUNCTION is only called in matching major modes.

     This property is optional.

`:error-filter FUNCTION'
     A function to filter the errors returned by this checker.

     FUNCTION is called with the list of `flycheck-error' objects
     returned by the syntax checker and shall return another list
     of `flycheck-error' objects, which is considered the final
     result of this syntax checker.

     FUNCTION is free to add, remove or modify errors, whether in
     place or by copying.

     This property is optional.  The default filter is
     `identity'.

`:error-explainer FUNCTION'
     A function to return an explanation text for errors
     generated by this checker.

     FUNCTION is called with a `flycheck-error' object, in the
     buffer of that error.  It shall return an explanation
     message for the error.

     The message can take any of the following forms:
     - A string, which will be displayed to the user
     - A function (likely a closure), which will be called with
       `standard-output' set to a `flycheck-explain-error-mode'
       buffer, and should write to it.
     - A cons `(url . ,URL), indicating that the explanation can
       be found online at URL.
     - nil if there is no explanation for this error.

     For the common case of a URL keyed by the error ID, build
     FUNCTION with `flycheck-error-explainer-from-url'.

     If URL is provided by the checker, and cannot be composed
     from other elements in the `flycheck-error' object, consider
     passing the URL via text properties:

       ;; During the error object creation
       (put-text-property 0 1 \\='explainer-url .url .check_id)

       ;; In the error-explainer FUNCTION
       (let ((id (flycheck-error-id err)))
         (and id `(url . ,(get-text-property 0 \\='explainer-url id))))

     This property is optional.

`:next-checkers NEXT-CHECKERS'
     A list denoting syntax checkers to apply after this syntax
     checker, in what we call \"chaining\" of syntax checkers.

     Each ITEM is a cons cell `(LEVEL . CHECKER)'.  CHECKER is a
     syntax checker to run after this syntax checker.  LEVEL is
     an error level.  CHECKER will only be used if there are no
     current errors of at least LEVEL.  LEVEL may also be t, in
     which case CHECKER is used regardless of the current errors.

     ITEM may also be a syntax checker symbol, which is
     equivalent to `(t . ITEM)'.

     Flycheck tries all items in order of declaration, and uses
     the first whose LEVEL matches and whose CHECKER is
     registered and can be used for the current buffer.

     This feature is typically used to apply more than one syntax
     checker to a buffer.  For instance, you might first use a
     compiler to check a buffer for syntax and type errors, and
     then run a linting tool that checks for insecure code, or
     questionable style.

     This property is optional.  If omitted, it defaults to
     nil, i.e. no other syntax checkers are applied after this
     syntax checker.

`:working-directory FUNCTION'
     The value of `default-directory' when invoking `:start'.

     FUNCTION is a function taking the syntax checker as sole
     argument.  It shall return the absolute path to an existing
     directory to use as `default-directory' for `:start' or
     nil to fall back to the `default-directory' of the current
     buffer.

     This property is optional.  If omitted, invoke `:start'
     from the `default-directory' of the buffer being checked.

Signal an error, if any property has an invalid value."
  (declare (indent 1)
           (doc-string 2))
  (let ((start (plist-get properties :start))
        (interrupt (plist-get properties :interrupt))
        (print-doc (plist-get properties :print-doc))
        (modes (plist-get properties :modes))
        (predicate (plist-get properties :predicate))
        (verify (plist-get properties :verify))
        (enabled (plist-get properties :enabled))
        (filter (or (plist-get properties :error-filter) #'identity))
        (explainer (plist-get properties :error-explainer))
        (next-checkers (plist-get properties :next-checkers))
        (file (flycheck-current-load-file))
        (working-directory (plist-get properties :working-directory)))

    (unless (listp modes)
      (setq modes (list modes)))

    (unless (functionp start)
      (error ":start %S of syntax checker %s is not a function" start symbol))
    (unless (or (null interrupt) (functionp interrupt))
      (error ":interrupt %S of syntax checker %s is not a function"
             interrupt symbol))
    (unless (or (null print-doc) (functionp print-doc))
      (error ":print-doc %S of syntax checker %s is not a function"
             print-doc symbol))
    (unless (or (null verify) (functionp verify))
      (error ":verify %S of syntax checker %S is not a function"
             verify symbol))
    (unless (or (null enabled) (functionp enabled))
      (error ":enabled %S of syntax checker %S is not a function"
             enabled symbol))
    (unless modes
      (error "Missing :modes in syntax checker %s" symbol))
    (dolist (mode modes)
      (unless (symbolp mode)
        (error "Invalid :modes %s in syntax checker %s, %s must be a symbol"
               modes symbol mode)))
    (unless (or (null predicate) (functionp predicate))
      (error ":predicate %S of syntax checker %s  is not a function"
             predicate symbol))
    (unless (functionp filter)
      (error ":error-filter %S of syntax checker %s is not a function"
             filter symbol))
    (unless (or (null explainer) (functionp explainer))
      (error ":error-explainer %S of syntax checker %S is not a function"
             explainer symbol))
    (dolist (checker next-checkers)
      (flycheck-validate-next-checker checker))

    (let ((real-predicate
           (and predicate
                (lambda ()
                  ;; Run predicate in the checker's default directory
                  (let ((default-directory
                          (flycheck-compute-working-directory symbol)))
                    (funcall predicate)))))
          (real-enabled
           (lambda ()
             (if (flycheck-valid-checker-p symbol)
                 (or (null enabled)
                     ;; Run enabled in the checker's default directory
                     (let ((default-directory
                             (flycheck-compute-working-directory symbol)))
                       (funcall enabled)))
               (lwarn 'flycheck
                      :warning "%S is no valid Flycheck syntax checker.
Try to reinstall the package defining this syntax checker." symbol)
               nil))))
      (pcase-dolist (`(,prop . ,value)
                     `((start             . ,start)
                       (interrupt         . ,interrupt)
                       (print-doc         . ,print-doc)
                       (modes             . ,modes)
                       (predicate         . ,real-predicate)
                       (verify            . ,verify)
                       (enabled           . ,real-enabled)
                       (error-filter      . ,filter)
                       (error-explainer   . ,explainer)
                       (next-checkers     . ,next-checkers)
                       (documentation     . ,docstring)
                       (file              . ,file)
                       (working-directory . ,working-directory)))
        (setf (flycheck-checker-get symbol prop) value)))

    ;; Track the version, to avoid breakage if the internal format changes
    (setf (flycheck-checker-get symbol 'generic-checker-version)
          flycheck-generic-checker-version)))

(defun flycheck-valid-checker-p (checker)
  "Check whether a CHECKER is valid.

A valid checker is a symbol defined as syntax checker with
`flycheck-define-checker'."
  (and (symbolp checker)
       (= (or (get checker 'flycheck-generic-checker-version) 0)
          flycheck-generic-checker-version)))

(defun flycheck-checker-supports-major-mode-p (checker &optional mode)
  "Whether CHECKER supports the given major MODE.

CHECKER is a syntax checker symbol and MODE a major mode symbol.
Look at the `modes' property of CHECKER to determine whether
CHECKER supports buffers in the given major MODE.

MODE defaults to the value of `major-mode' if omitted or nil.

Return non-nil if CHECKER supports MODE and nil otherwise."
  (let ((mode (or mode major-mode)))
    (memq mode (flycheck-checker-get checker 'modes))))

(define-obsolete-variable-alias 'flycheck-enabled-checkers
  'flycheck--automatically-enabled-checkers "32")

(defvar flycheck--automatically-enabled-checkers nil
  "Syntax checkers included in automatic selection.

A list of Flycheck syntax checkers included in automatic
selection for the current buffer.")
(make-variable-buffer-local 'flycheck--automatically-enabled-checkers)

(defun flycheck-may-enable-checker (checker)
  "Whether a generic CHECKER may be enabled for current buffer.

Return non-nil if CHECKER may be used for the current buffer, and
nil otherwise.  The result of the `:enabled' check, if any, is
cached."
  (and
   ;; May only enable valid checkers
   (flycheck-valid-checker-p checker)
   ;; Don't run the :enabled check if the checker is already disabled…
   (not (flycheck-disabled-checker-p checker))
   (or
    ;; …or if we've already cached the result
    (memq checker flycheck--automatically-enabled-checkers)
    (let* ((enabled (flycheck-checker-get checker 'enabled))
           (may-enable (or (null enabled) (funcall enabled))))
      ;; Cache the result
      (if may-enable
          (cl-pushnew checker flycheck--automatically-enabled-checkers)
        (cl-pushnew checker flycheck--automatically-disabled-checkers))
      may-enable))))

(defun flycheck-reset-enabled-checker (checker)
  "Reset the `:enabled' test of CHECKER.

Forget that CHECKER has been enabled or automatically disabled
from a previous `:enabled' test.  The result of the `:enabled'
test is cached in `flycheck-may-enable-checker': if you wish to
test the `:enabled' predicate again, you must first reset its
state using this function."
  (when (memq checker flycheck--automatically-disabled-checkers)
    (setq flycheck--automatically-disabled-checkers
          (remq checker flycheck--automatically-disabled-checkers)))
  (when (memq checker flycheck--automatically-enabled-checkers)
    (setq flycheck--automatically-enabled-checkers
          (remq checker flycheck--automatically-enabled-checkers)))
  (flycheck-buffer))

(defun flycheck-may-use-checker (checker)
  "Whether a generic CHECKER may be used.

Return non-nil if CHECKER may be used for the current buffer, and
nil otherwise."
  (let ((predicate (flycheck-checker-get checker 'predicate)))
    (and (flycheck-valid-checker-p checker)
         (flycheck-checker-supports-major-mode-p checker)
         (flycheck-may-enable-checker checker)
         (or (null predicate) (funcall predicate)))))

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


;;; Help for generic syntax checkers
(define-button-type 'help-flycheck-checker-def
  :supertype 'help-xref
  'help-function #'flycheck-goto-checker-definition
  'help-echo "mouse-1, RET: find Flycheck checker definition")

(defconst flycheck-find-checker-regexp
  (rx line-start (zero-or-more (syntax whitespace))
      "(" symbol-start
      (or "flycheck-define-checker" "flycheck-define-command-checker")
      symbol-end
      (eval (list 'regexp find-function-space-re))
      (? "'")
      symbol-start "%s" symbol-end
      (or (syntax whitespace) line-end))
  "Regular expression to find a checker definition.")

(add-to-list 'find-function-regexp-alist
             '(flycheck-checker . flycheck-find-checker-regexp))

(defun flycheck-goto-checker-definition (checker file)
  "Go to the definition of CHECKER in FILE."
  (let ((location (find-function-search-for-symbol
                   checker 'flycheck-checker file)))
    (pop-to-buffer (car location))
    (if (cdr location)
        (goto-char (cdr location))
      (message "Unable to find checker location in file"))))

(defun flycheck-checker-at-point ()
  "Return the Flycheck checker found at or before point.

Return nil if there is no checker."
  (let ((symbol (variable-at-point 'any-symbol)))
    (when (flycheck-valid-checker-p symbol)
      symbol)))

(defun flycheck-describe-checker (checker)
  "Display the documentation of CHECKER.

CHECKER is a checker symbol.

Pop up a help buffer with the documentation of CHECKER."
  (interactive
   (let* ((enable-recursive-minibuffers t)
          (default (or (flycheck-checker-at-point)
                       (ignore-errors (flycheck-get-checker-for-buffer))))
          (prompt (if default
                      (format "Describe syntax checker (default %s): " default)
                    "Describe syntax checker: ")))
     (list (flycheck-read-checker prompt default))))
  (unless (flycheck-valid-checker-p checker)
    (user-error "You didn't specify a Flycheck syntax checker"))
  (let ((filename (flycheck-checker-get checker 'file))
        (modes (flycheck-checker-get checker 'modes))
        (predicate (flycheck-checker-get checker 'predicate))
        (print-doc (flycheck-checker-get checker 'print-doc))
        (next-checkers (flycheck-checker-get checker 'next-checkers))
        (help-xref-following
         ;; Ensure that we don't reuse buffers like `flycheck-verify-checker',
         ;; and that we don't error out if a `help-flycheck-checker-doc' button
         ;; is added outside of a documentation window.
         (and help-xref-following (eq major-mode 'help-mode))))
    (help-setup-xref (list #'flycheck-describe-checker checker)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (princ (format "%s is a Flycheck syntax checker" checker))
        (when filename
          (princ (format " in `%s'" (file-name-nondirectory filename)))
          (with-current-buffer standard-output
            (save-excursion
              (re-search-backward "`\\([^`']+\\)'" nil t)
              (help-xref-button 1 'help-flycheck-checker-def
                                checker filename))))
        (princ ".\n\n")

        (let ((modes-start (with-current-buffer standard-output (point-max))))
          ;; Track the start of the modes documentation, to properly re-fill
          ;; it later
          (princ "  This syntax checker checks syntax in the major mode(s) ")
          (princ (string-join
                  (mapcar (apply-partially #'format "`%s'") modes)
                  ", "))
          (when predicate
            (princ ", and uses a custom predicate"))
          (princ ".")
          (when next-checkers
            (princ "  It runs the following checkers afterwards:"))
          (with-current-buffer standard-output
            (save-excursion
              (fill-region-as-paragraph modes-start (point-max))))
          (princ "\n")

          ;; Print the list of next checkers
          (when next-checkers
            (princ "\n")
            (let ((beg-checker-list (with-current-buffer standard-output
                                      (point))))
              (dolist (next-checker next-checkers)
                (if (symbolp next-checker)
                    (princ (format "     * `%s'\n" next-checker))
                  (princ (format "     * `%s' (maximum level `%s')\n"
                                 (cdr next-checker) (car next-checker)))))
              ;;
              (with-current-buffer standard-output
                (save-excursion
                  (while (re-search-backward "`\\([^`']+\\)'"
                                             beg-checker-list t)
                    (let ((checker (intern-soft (match-string 1))))
                      (when (flycheck-valid-checker-p checker)
                        (help-xref-button 1 'help-flycheck-checker-doc
                                          checker)))))))))
        ;; Call the custom print-doc function of the checker, if present
        (when print-doc
          (funcall print-doc checker))
        ;; Ultimately, print the docstring
        (princ "\nDocumentation:\n")
        (princ (flycheck-checker-get checker 'documentation))))))


;;; Syntax checker verification
(cl-defstruct (flycheck-verification-result
               (:constructor flycheck-verification-result-new))
  "Structure for storing a single verification result.

Slots:

`label'
     A label for this result, as string

`message'
     A message for this result, as string

`face'
     The face to use for the `message'.

     You can either use a face symbol, or a list of face symbols."
  label message face)

(defun flycheck-verify-generic-checker (checker)
  "Verify a generic CHECKER in the current buffer.

Return a list of `flycheck-verification-result' objects."
  (let (results
        (predicate (flycheck-checker-get checker 'predicate))
        (enabled (flycheck-checker-get checker 'enabled))
        (verify (flycheck-checker-get checker 'verify)))
    (when enabled
      (let ((result (funcall enabled)))
        (push (flycheck-verification-result-new
               :label (propertize "may enable" 'help-echo ":enable")
               :message (if result "yes" "no")
               :face (if result 'success '(bold warning)))
              results)))
    (when predicate
      (let ((result (funcall predicate)))
        (push (flycheck-verification-result-new
               :label (propertize "may run" 'help-echo ":predicate")
               :message (prin1-to-string (not (null result)))
               :face (if result 'success '(bold warning)))
              results)))
    (append (nreverse results)
            (and verify (funcall verify checker)))))

(define-button-type 'help-flycheck-checker-doc
  :supertype 'help-xref
  'help-function #'flycheck-describe-checker
  'help-echo "mouse-1, RET: describe Flycheck checker")

(define-button-type 'flycheck-button
  'follow-link t
  'action (lambda (pos)
            (apply (get-text-property pos 'flycheck-action)
                   (get-text-property pos 'flycheck-data))
            ;; Revert the verify-setup buffer since it is now stale
            (revert-buffer))
  'face 'flycheck-verify-select-checker)

(define-button-type 'flycheck-checker-select
  :supertype 'flycheck-button
  'flycheck-action (lambda (buffer checker)
                     (with-current-buffer buffer
                       (flycheck-select-checker checker)))
  'help-echo "mouse-1, RET: select this checker")

(define-button-type 'flycheck-checker-enable
  :supertype 'flycheck-button
  'flycheck-action (lambda (buffer checker)
                     (interactive)
                     (with-current-buffer buffer
                       (flycheck--toggle-checker checker t)
                       (flycheck-buffer)))
  'help-echo "mouse-1, RET: re-enable this checker in this buffer")

(define-button-type 'flycheck-checker-reset-enabled
  :supertype 'flycheck-button
  'flycheck-action (lambda (buffer checker)
                     (with-current-buffer buffer
                       (flycheck-reset-enabled-checker checker)))
  'help-echo "mouse-1, RET: try to re-enable this checker")

(defun flycheck--verify-princ-checker (checker buffer
                                               &optional with-mm with-select)
  "Print verification result of CHECKER for BUFFER.

When WITH-MM is given and non-nil, also include the major mode
into the verification results.

When WITH-SELECT is non-nil, add a button to select this checker."
  (princ "  ")
  (insert-button (symbol-name checker)
                 'type 'help-flycheck-checker-doc
                 'help-args (list checker))
  (cond
   ((with-current-buffer buffer
      (flycheck-manually-disabled-checker-p checker))
    (insert (propertize " (manually disabled) " 'face '(bold error)))
    (insert-text-button "enable"
                        'type 'flycheck-checker-enable
                        'flycheck-data (list buffer checker)))
   ((with-current-buffer buffer
      (flycheck-automatically-disabled-checker-p checker))
    (insert (propertize " (automatically disabled) " 'face '(bold error)))
    (insert-text-button "reset"
                        'type 'flycheck-checker-reset-enabled
                        'flycheck-data (list buffer checker))))
  (when (eq checker (buffer-local-value 'flycheck-checker buffer))
    (insert (propertize " (explicitly selected)" 'face 'bold)))
  (when with-select
    (princ "  ")
    (insert-text-button "select"
                        'type 'flycheck-checker-select
                        'flycheck-data (list buffer checker)))
  (princ "\n")
  (let ((results (with-current-buffer buffer
                   (append (flycheck-verify-generic-checker checker)
                           (flycheck--verify-next-checkers checker)))))
    (when with-mm
      (with-current-buffer buffer
        (let ((message-and-face
               (if (flycheck-checker-supports-major-mode-p checker)
                   (cons (format "`%s' supported" major-mode) 'success)
                 (cons (format "`%s' not supported" major-mode) 'error))))
          (push (flycheck-verification-result-new
                 :label "major mode"
                 :message (car message-and-face)
                 :face (cdr message-and-face))
                results))))
    (let* ((label-length
            (seq-max (mapcar
                      (lambda (res)
                        (length (flycheck-verification-result-label res)))
                      results)))
           (message-column (+ 8 label-length)))
      (dolist (result results)
        (princ "    - ")
        (princ (flycheck-verification-result-label result))
        (princ ": ")
        (princ (make-string (- message-column (current-column)) ?\ ))
        (let ((message (flycheck-verification-result-message result))
              (face (flycheck-verification-result-face result)))
          ;; If face is nil, using propertize erases the face already contained
          ;; by the message.  We don't want that, since this would remove the
          ;; button face from the checker chain result.
          (insert (if face (propertize message 'face face) message)))
        (princ "\n"))))
  (princ "\n"))

(defun flycheck--get-next-checker-symbol (next)
  "Get the checker symbol of NEXT checker.

NEXT should be either a cons (LEVEL . CHECKER) or a
symbol."
  (if (consp next) (cdr next) next))

(defun flycheck-get-next-checkers (checker)
  "Return the immediate next checkers of CHECKER.

This is a list of checker symbols.  The error levels of the
`:next-checker' property are ignored."
  (mapcar #'flycheck--get-next-checker-symbol
          (flycheck-checker-get checker 'next-checkers)))

(defun flycheck-all-next-checkers (checker)
  "Return all checkers that may follow CHECKER.

Return the transitive closure of the next-checker relation.  The
return value is a list of checkers, not including CHECKER."
  (let ((next-checkers)
        (visited)
        (queue (list checker)))
    (while queue
      (let ((c (pop queue)))
        (push c visited)
        (dolist (n (flycheck-get-next-checkers c))
          (push n next-checkers)
          (unless (memq n visited)
            (cl-pushnew n queue)))))
    (seq-uniq next-checkers)))

(defun flycheck--verify-next-checkers (checker)
  "Return a verification result for the next checkers of CHECKER."
  (when-let* ((next (flycheck-get-next-checkers checker)))
    (list
     (flycheck-verification-result-new
      :label "next checkers"
      ;; We use `make-text-button' to preserve the button properties in the
      ;; string
      :message (mapconcat
                (lambda (checker)
                  (make-text-button (symbol-name checker) nil
                                    'type 'help-flycheck-checker-doc
                                    'help-args (list checker)))
                next
                ", ")))))

(defun flycheck--verify-print-header (desc buffer)
  "Print a title with DESC for BUFFER in the current buffer.

DESC is an arbitrary string containing a description, and BUFFER
is the buffer being verified.  The name and the major mode
of BUFFER are printed.

DESC and information about BUFFER are printed in the current
buffer."
  (princ desc)
  (insert (propertize (buffer-name buffer) 'face 'bold))
  (princ " in ")
  (let ((mode (buffer-local-value 'major-mode buffer)))
    (insert-button (symbol-name mode)
                   'type 'help-function
                   'help-args (list mode)))
  (princ ":\n\n"))

(defun flycheck--verify-print-footer (buffer)
  "Print a footer for BUFFER in the current buffer.

BUFFER is the buffer being verified."
  (princ "Flycheck Mode is ")
  (let ((enabled (buffer-local-value 'flycheck-mode buffer)))
    (insert (propertize (if enabled "enabled" "disabled")
                        'face (if enabled 'success '(warning bold)))))
  (princ
   (with-current-buffer buffer
     ;; Use key binding state in the verified buffer to print the help.
     (substitute-command-keys
      ".  Use \\[universal-argument] \\[flycheck-disable-checker] \
to enable disabled checkers.")))
  (save-excursion
    (let ((end (point)))
      (backward-paragraph)
      (fill-region-as-paragraph (point) end)))

  (princ "\n\n--------------------\n\n")
  (princ (format "Flycheck version: %s\n" (flycheck--pkg-version)))
  (princ (format "Emacs version:    %s\n" emacs-version))
  (princ (format "System:           %s\n" system-configuration))
  (princ (format "Window system:    %S\n" window-system)))

(define-derived-mode flycheck-verify-mode help-mode
  "Flycheck verification"
  "Major mode to display Flycheck verification results."
  ;; `help-mode-finish' will restore `buffer-read-only'
  (setq buffer-read-only nil))

(defun flycheck-verify-checker (checker)
  "Check whether a CHECKER can be used in this buffer.

Show a buffer listing possible problems that prevent CHECKER from
being used for the current buffer.

Note: Do not use this function to check whether a syntax checker
is applicable from Emacs Lisp code.  Use
`flycheck-may-use-checker' instead."
  (interactive (list (flycheck-read-checker "Checker to verify: ")))
  (unless (flycheck-valid-checker-p checker)
    (user-error "%s is not a syntax checker" checker))

  ;; Predicates and `:enabled' functions usually check the file on disk, so
  ;; the verification is only accurate for a saved buffer.  Ask instead of
  ;; saving behind the user's back, which may have unintended side effects
  ;; (e.g. save hooks and file watchers).
  (when (and (buffer-file-name) (buffer-modified-p)
             (y-or-n-p "Save the buffer to make the verification accurate? "))
    (save-buffer))

  (let ((buffer (current-buffer)))
    (with-help-window "*Flycheck checker*"
      (with-current-buffer standard-output
        (flycheck-verify-mode)
        (flycheck--verify-print-header "Syntax checker in buffer " buffer)
        (flycheck--verify-princ-checker checker buffer 'with-mm)
        (if (with-current-buffer buffer (flycheck-may-use-checker checker))
            (insert (propertize
                     "Flycheck can use this syntax checker for this buffer.\n"
                     'face 'success))
          (insert (propertize
                   "Flycheck cannot use this syntax checker for this buffer.\n"
                   'face 'error)))
        (insert "\n")
        (flycheck--verify-print-footer buffer)))))

(defvar-local flycheck--last-failure nil
  "What the last failed syntax check reported, or nil.

A list (CHECKER STATUS OUTPUT), recorded when a check ends `errored' or
`suspicious'.  The echo area only gets a short message about those, so
the checker's own output is kept here for `flycheck-verify-setup' to
show, where there is room for it.")

;; A server publishes diagnostics whenever it likes, and each push that
;; carries something new re-runs the check that publishes it.  How often
;; that happens is entirely the server's business: some publish once and
;; go quiet, others push continuously while they index or build.  Count
;; both, so `flycheck-verify-setup' can show the rate.  A report of
;; Flycheck bogging down in an LSP buffer is otherwise hard to tell apart
;; from one where each individual check is simply slow.

(defvar-local flycheck-lsp--push-count 0
  "How many diagnostics pushes this buffer's LSP server has sent.")

(defvar-local flycheck-lsp--recheck-count 0
  "How many of this buffer's pushes carried new diagnostics and re-ran a check.")

(defvar-local flycheck-lsp--first-push-time nil
  "When this buffer's LSP server sent its first diagnostics push.")

(defvar-local flycheck-lsp--last-push-time nil
  "When this buffer's LSP server sent its most recent diagnostics push.")

(defun flycheck--verify-princ-lsp-activity (activity)
  "Print ACTIVITY, a `flycheck--lsp-activity' value, if there is any.

How often a language server pushes diagnostics is the server's business,
and a buffer that feels slow because its server pushes constantly looks
nothing like one where each check is slow.  The counts tell them apart."
  (pcase activity
    (`(,pushes ,rechecks ,elapsed ,since)
     (princ (format "LSP diagnostics pushes: %d, of which %d carried \
changes and re-ran a check\n" pushes rechecks))
     ;; A rate over a window too short to measure says nothing
     (when (and elapsed (>= elapsed 1))
       (princ (format "                        %.1f per second over %.0fs\n"
                      (/ pushes (float elapsed)) elapsed)))
     (when since
       (princ (format "                        last push %.1fs ago\n" since)))
     (princ "\n"))))

(defun flycheck--lsp-activity ()
  "Return this buffer's LSP push counts, or nil when its server sent none."
  (when (and flycheck-lsp--first-push-time (> flycheck-lsp--push-count 0))
    (let ((now (float-time)))
      (list flycheck-lsp--push-count
            flycheck-lsp--recheck-count
            (- (or flycheck-lsp--last-push-time now)
               flycheck-lsp--first-push-time)
            (and flycheck-lsp--last-push-time
                 (- now flycheck-lsp--last-push-time))))))

(defun flycheck--verify-princ-last-failure (failure)
  "Print FAILURE, a `flycheck--last-failure' value, if there is one."
  (pcase failure
    (`(,checker ,status ,output)
     (insert (propertize
              (pcase status
                (`suspicious
                 (format "Flycheck could not read %s's output.\n" checker))
                (_ (format "Flycheck could not run %s.\n" checker)))
              'face '(bold warning)))
     (princ "\nThis usually means the tool is missing, misconfigured, or \
picking up\nthe wrong environment.  It reported:\n\n")
     (let ((start (point)))
       (princ (string-trim-right (or output "nothing at all")))
       (insert "\n\n")
       (put-text-property start (point) 'face 'shadow)))))

(defun flycheck-verify-setup ()
  "Check whether Flycheck can be used in this buffer.

Display a new buffer listing all syntax checkers that could be
applicable in the current buffer.  For each syntax checker,
possible problems are shown."
  (interactive)
  ;; Checkers that only work on saved buffers would fail the verification
  ;; for a modified buffer, so ask instead of saving behind the user's back
  (when (and (buffer-file-name) (buffer-modified-p)
             (y-or-n-p "Save the buffer to make the verification accurate? "))
    (save-buffer))

  (let* ((buffer (current-buffer))
         (last-failure flycheck--last-failure)
         (lsp-activity (flycheck--lsp-activity))
         (first-checker (flycheck-get-checker-for-buffer))
         (valid-checkers
          (remq first-checker
                (seq-filter #'flycheck-may-use-checker flycheck-checkers)))
         (valid-next-checkers
          (when first-checker
            (seq-intersection valid-checkers
                              (flycheck-all-next-checkers first-checker))))
         (valid-remaining (seq-difference valid-checkers valid-next-checkers))
         (other-checkers
          (seq-difference (seq-filter #'flycheck-checker-supports-major-mode-p
                                      flycheck-checkers)
                          (cons first-checker valid-checkers))))

    ;; Print all applicable checkers for this buffer
    (with-help-window "*Flycheck checkers*"
      (with-current-buffer standard-output
        (flycheck-verify-mode)

        (flycheck--verify-print-header "Syntax checkers for buffer " buffer)

        ;; Lead with the last failure: it is why most people get here
        (flycheck--verify-princ-last-failure last-failure)

        (if first-checker
            (progn
              (princ "First checker to run:\n\n")
              (flycheck--verify-princ-checker first-checker buffer))
          (insert (propertize
                   "No checker to run in this buffer.\n\n"
                   'face '(bold error))))

        (when valid-next-checkers
          (princ
           "Checkers that may run as part of the first checker's chain:\n\n")
          (dolist (checker valid-next-checkers)
            (flycheck--verify-princ-checker checker buffer)))

        (when valid-remaining
          (princ "Checkers that could run if selected:\n\n")
          (dolist (checker valid-remaining)
            (flycheck--verify-princ-checker checker buffer nil 'with-select)))

        (when other-checkers
          (princ
           "Checkers that are compatible with this mode, \
but will not run until properly configured:\n\n")
          (dolist (checker other-checkers)
            (flycheck--verify-princ-checker checker buffer)))

        ;; If we have no checkers at all, that's worth mentioning
        (unless (or first-checker valid-checkers other-checkers)
          (insert (propertize
                   "No checkers are available for this buffer.\n\n"
                   'face '(bold error))))

        (let ((unregistered-checkers
               (seq-difference (flycheck-defined-checkers) flycheck-checkers)))
          (when unregistered-checkers
            (insert (propertize
                     "The following syntax checkers are not registered:\n"
                     'face '(bold warning)))
            (dolist (checker unregistered-checkers)
              (princ "  - ")
              (princ checker)
              (princ "\n"))
            (princ
             "Try adding these syntax checkers to `flycheck-checkers'.\n\n")))

        (flycheck--verify-princ-lsp-activity lsp-activity)

        (flycheck--verify-print-footer buffer)

        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                      (with-current-buffer buffer (flycheck-verify-setup))))))))


;;; Predicates for generic syntax checkers
(defun flycheck-buffer-saved-p (&optional buffer)
  "Determine whether BUFFER is saved to a file.

BUFFER is the buffer to check.  If omitted or nil, use the
current buffer as BUFFER.

Return non-nil if the BUFFER is backed by a file, and not
modified, or nil otherwise."
  (let ((file-name (buffer-file-name buffer)))
    (and file-name (file-exists-p file-name) (not (buffer-modified-p buffer)))))


;;; Extending generic checkers
(defun flycheck-remove-next-checker (checker next)
  "After CHECKER remove a NEXT checker.

CHECKER is a syntax checker symbol, from which to remove NEXT
checker.

NEXT is a cons or a symbol, as documented in
`flycheck-add-next-checker'."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is not a valid syntax checker" checker))
  (let* ((next-symbol (flycheck--get-next-checker-symbol next)))
    (setf
     (flycheck-checker-get checker 'next-checkers)
     (seq-remove
      (lambda (next) (eq (flycheck--get-next-checker-symbol next) next-symbol))
      (flycheck-checker-get checker 'next-checkers)))))

(defun flycheck-add-next-checker (checker next &optional append)
  "After CHECKER add a NEXT checker.

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
  (flycheck-remove-next-checker checker next)
  (let ((next-checkers (flycheck-checker-get checker 'next-checkers)))
    (setf (flycheck-checker-get checker 'next-checkers)
          (if append (append next-checkers (list next))
            (cons next next-checkers)))))

(defun flycheck-add-mode (checker mode)
  "To CHECKER add a new major MODE.

CHECKER and MODE are symbols denoting a syntax checker and a
major mode respectively.

Add MODE to the `:modes' property of CHECKER, so that CHECKER
will be used in buffers with MODE."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is not a valid syntax checker" checker))
  (unless (symbolp mode)
    (error "%s is not a symbol" mode))
  (push mode (flycheck-checker-get checker 'modes)))


;;; Generic syntax checks
(cl-defstruct (flycheck-syntax-check
               (:constructor flycheck-syntax-check-new))
  "Structure for storing syntax check state.

Slots:

`buffer'
     The buffer being checked.

`checker'
     The syntax checker being used.

`context'
     The context object.

`working-directory'
     Working directory for the syntax checker.  Serves as a value for
     `default-directory' for a checker.

`start-time'
     The time the syntax check was started, as a float."
  buffer checker context working-directory
  (start-time (float-time)))

(defun flycheck-syntax-check-start (syntax-check callback)
  "Start a SYNTAX-CHECK with CALLBACK."
  (let ((checker (flycheck-syntax-check-checker syntax-check))
        (default-directory
          (flycheck-syntax-check-working-directory syntax-check)))
    (setf (flycheck-syntax-check-context syntax-check)
          (funcall (flycheck-checker-get checker 'start) checker callback))))

(defun flycheck-syntax-check-interrupt (syntax-check)
  "Interrupt a SYNTAX-CHECK."
  (let* ((checker (flycheck-syntax-check-checker syntax-check))
         (interrupt-fn (flycheck-checker-get checker 'interrupt))
         (context (flycheck-syntax-check-context syntax-check)))
    (when interrupt-fn
      (funcall interrupt-fn checker context))))


;;; Syntax checking mode

(defvar flycheck-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map flycheck-keymap-prefix flycheck-command-map)
    ;; We place the menu under a custom menu key.  Since this menu key is not
    ;; present in the menu of the global map, no top-level menu entry is added
    ;; to the global menu bar.  However, it still appears on the mode line
    ;; lighter.
    (define-key map [menu-bar flycheck] flycheck-mode-menu-map)
    map)
  "Keymap of command `flycheck-mode'.")

(defvar-local flycheck-old-next-error-function nil
  "Remember the old `next-error-function'.")

(defconst flycheck-hooks-alist
  '(
    ;; Handle events that may start automatic syntax checks
    (after-save-hook        . flycheck-handle-save)
    (after-change-functions . flycheck-handle-change)
    (after-revert-hook      . flycheck-handle-revert)
    ;; Handle events that may trigger pending deferred checks
    (window-configuration-change-hook . flycheck-perform-deferred-syntax-check)
    (post-command-hook                . flycheck-perform-deferred-syntax-check)
    ;; Teardown Flycheck whenever the buffer state is about to get lost, to
    ;; clean up temporary files and directories.
    (kill-buffer-hook       . flycheck-teardown)
    (change-major-mode-hook . flycheck-teardown)
    (before-revert-hook     . flycheck-teardown)
    ;; Update the error list if necessary
    (post-command-hook . flycheck-error-list-update-source)
    (post-command-hook . flycheck-error-list-highlight-errors)
    ;; Display errors.  Show errors at point after commands (like movements)
    ;; and hide the error buffer (for large error messages) if necessary.
    ;; Focus change handling is done separately via
    ;; `after-focus-change-function' (see `flycheck-handle-focus-change').
    (post-command-hook . flycheck-display-error-at-point-soon)
    (post-command-hook . flycheck-hide-error-buffer)
    ;; Immediately show error popups when navigating to an error
    (next-error-hook . flycheck-display-error-at-point))
  "Hooks which Flycheck needs to hook in.

The `car' of each pair is a hook variable, the `cdr' a function
to be added or removed from the hook variable if Flycheck mode is
enabled and disabled respectively.")

;;;###autoload
(define-minor-mode flycheck-mode
  "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:0'   Last check resulted in no errors and no warnings.
`FlyC:3|5' This buffer contains three errors and five warnings.
           Use `\\[flycheck-list-errors]' to see the list.
`FlyC-'    Flycheck doesn't have a checker for this buffer.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
\(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is ‘toggle’; disable the mode otherwise."
  :init-value nil
  :keymap flycheck-mode-map
  :lighter flycheck-mode-line
  :after-hook (flycheck-buffer-automatically 'mode-enabled 'force-deferred)
  (cond
   (flycheck-mode
    (flycheck-clear)
    (flycheck--sync-margin)
    (add-hook 'eldoc-documentation-functions #'flycheck-eldoc-function nil t)
    ;; Guarded by a buffer-local flag that only Flycheck sets, so this is
    ;; inert everywhere else.  It stays for the session: Eldoc displays for
    ;; whichever buffer answered last, so removing it when one buffer turns
    ;; Flycheck off would break the buffers that still have it on.
    (advice-add 'eldoc-display-in-buffer
                :around #'flycheck--eldoc-suppress-doc-window)
    ;; `global-eldoc-mode' may have skipped this buffer because no
    ;; documentation source was registered when it made its decision;
    ;; give it another chance now that Flycheck provides one.  Buffers
    ;; where the user disabled Eldoc entirely are left alone; there the
    ;; display timer picks up the slack.
    (when (and (bound-and-true-p global-eldoc-mode)
               (not (bound-and-true-p eldoc-mode)))
      (turn-on-eldoc-mode))

    (pcase-dolist (`(,hook . ,fn) (reverse flycheck-hooks-alist))
      (add-hook hook fn nil 'local))

    (setq flycheck-old-next-error-function
          (if flycheck-standard-error-navigation
              next-error-function
            :unset))
    (when flycheck-standard-error-navigation
      (setq next-error-function #'flycheck-next-error-function))

    ;; This hook must be added globally since otherwise we cannot
    ;; detect a change from a buffer where Flycheck is enabled to a
    ;; buffer where Flycheck is not enabled, and therefore cannot
    ;; notice that there has been any change when the user switches
    ;; back to the buffer where Flycheck is enabled.
    (add-hook 'buffer-list-update-hook #'flycheck-handle-buffer-switch))
   (t
    (unless (eq flycheck-old-next-error-function :unset)
      (setq next-error-function flycheck-old-next-error-function))

    (remove-hook 'eldoc-documentation-functions #'flycheck-eldoc-function t)

    (pcase-dolist (`(,hook . ,fn) flycheck-hooks-alist)
      (remove-hook hook fn 'local))

    (flycheck-teardown))))


;;; Syntax checker selection for the current buffer
(defun flycheck-get-checker-for-buffer ()
  "Find the checker for the current buffer.

Use the selected checker for the current buffer, if any,
otherwise search for the best checker from `flycheck-checkers'.

Return checker if there is a checker for the current buffer, or
nil otherwise."
  (if flycheck-checker
      (when (flycheck-may-use-checker flycheck-checker)
        flycheck-checker)
    (seq-find #'flycheck-may-use-checker flycheck-checkers)))

(defun flycheck-get-next-checker-for-buffer (checker)
  "Get the checker to run after CHECKER for the current buffer."
  (let ((next (seq-find #'flycheck-may-use-next-checker
                        (flycheck-checker-get checker 'next-checkers))))
    (when next
      (if (symbolp next) next (cdr next)))))

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
     (list (flycheck-read-checker "Select checker: "
                                  (flycheck-get-checker-for-buffer)))))
  (unless (eq checker flycheck-checker)
    (unless (or (not checker) (flycheck-may-use-checker checker))
      (flycheck-verify-checker checker)
      (user-error "Can't use syntax checker %S in this buffer" checker))
    (setq flycheck-checker checker)
    (when flycheck-mode
      (flycheck-buffer))))

(defun flycheck--toggle-checker (checker enable)
  "Enable or disable CHECKER for the current buffer.

If ENABLE, re-enable CHECKER by removing it from the buffer-local
value of `flycheck-disabled-checkers'.  Otherwise, add the syntax
checker to the buffer-local value of `flycheck-disabled-checkers'."
  (cond
   (enable
    ;; We must use `remq' instead of `delq', because we must _not_ modify the
    ;; list.  Otherwise we could potentially modify the global default value,
    ;; in case the list is the global default.
    (when (memq checker flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers
            (remq checker flycheck-disabled-checkers)))
    (when (memq checker flycheck--automatically-disabled-checkers)
      (setq flycheck--automatically-disabled-checkers
            (remq checker flycheck--automatically-disabled-checkers))))
   (t (unless (memq checker flycheck-disabled-checkers)
        (push checker flycheck-disabled-checkers)))))

(defun flycheck-disable-checker (checker &optional enable)
  "Interactively disable CHECKER for the current buffer.

Prompt for a syntax checker to disable, and add the syntax
checker to the buffer-local value of
`flycheck-disabled-checkers'.

With non-nil ENABLE or with prefix arg, prompt for a disabled
syntax checker and re-enable it by removing it from the
buffer-local value of `flycheck-disabled-checkers'."
  (declare
   (interactive-only "Directly set `flycheck-disabled-checkers' instead"))
  (interactive
   (let* ((enable current-prefix-arg)
          (candidates (if enable
                          (append flycheck-disabled-checkers
                                  flycheck--automatically-disabled-checkers)
                        flycheck-checkers))
          (prompt (if enable "Enable syntax checker: "
                    "Disable syntax checker: ")))
     (when (and enable (not candidates))
       (user-error "No syntax checkers disabled in this buffer"))
     (list (flycheck-read-checker prompt nil nil candidates) enable)))
  (unless checker
    (user-error "No syntax checker given"))
  (flycheck--toggle-checker checker enable)
  (flycheck-buffer))


;;; Syntax checks for the current buffer
(defvar-local flycheck-current-syntax-check nil
  "The current syntax check in this buffer.")
(put 'flycheck-current-syntax-check 'permanent-local t)

(defvar-local flycheck--syntax-check-modified-tick nil
  "`buffer-chars-modified-tick' when the current check started.

A fix a checker suggests carries this tick (see `flycheck--make-fix')
so `flycheck-apply-fix' can tell whether the buffer has changed since
the checker read it, and thus whether the fix's positions are stale.")

(defun flycheck-start-current-syntax-check (checker)
  "Start a syntax check in the current buffer with CHECKER.

Set `flycheck-current-syntax-check' accordingly."
  ;; Remember the buffer's modification state, so a fix this check produces
  ;; can be refused later if the buffer has changed in the meantime.
  (setq flycheck--syntax-check-modified-tick (buffer-chars-modified-tick))
  ;; Allocate the current syntax check *before* starting it.  This allows for
  ;; synchronous checks, which call the status callback immediately in their
  ;; start function.
  (let* ((check
          (flycheck-syntax-check-new
           :buffer (current-buffer)
           :checker checker
           :context nil
           :working-directory (flycheck-compute-working-directory checker)
           ;; Chained checkers continue the same logical check: keep the
           ;; original start time, so the age limit of
           ;; `flycheck-interrupt-running-checks' covers the whole chain
           :start-time (if flycheck-current-syntax-check
                           (flycheck-syntax-check-start-time
                            flycheck-current-syntax-check)
                         (float-time))))
         (callback (flycheck-buffer-status-callback check)))
    (setq flycheck-current-syntax-check check)
    (flycheck-report-status 'running)
    (flycheck-syntax-check-start check callback)))

(defun flycheck-running-p ()
  "Determine whether a syntax check is running in the current buffer."
  (not (null flycheck-current-syntax-check)))

(defun flycheck--interrupt-current-syntax-check ()
  "Interrupt the running syntax check, without reporting a status."
  (when (flycheck-running-p)
    (let ((syntax-check flycheck-current-syntax-check))
      ;; Remove the current syntax check FIRST, to reset Flycheck into a
      ;; non-running state, and to make
      ;; `flycheck-report-buffer-checker-status' ignore any status reports
      ;; from the interrupted syntax check.  Interrupting below may run the
      ;; process sentinel synchronously, so this must happen before.
      (setq flycheck-current-syntax-check nil)
      ;; A signaling interrupt function must not leave Flycheck in a
      ;; half-stopped state; the interrupted check's reports are already
      ;; ignored at this point
      (with-demoted-errors "Error interrupting syntax check: %S"
        (flycheck-syntax-check-interrupt syntax-check)))))

(defun flycheck-stop ()
  "Stop any ongoing syntax check in the current buffer."
  (when (flycheck-running-p)
    (flycheck--interrupt-current-syntax-check)
    ;; Match the observable behavior of older Flycheck versions, where
    ;; the interrupted process's own status report cleared the buffer
    ;; state and ran `flycheck-syntax-check-failed-hook' -- but do it
    ;; deterministically, instead of relying on the process sentinel
    ;; running inline.  Only after the check was interrupted: a
    ;; signaling hook must not leave the checker process running.
    (flycheck-report-failed-syntax-check 'interrupted)))

(defun flycheck--interruptible-check-p ()
  "Whether the running syntax check can be interrupted.

Only syntax checkers with an `:interrupt' function can be
interrupted; for others the check would keep running in the
background and pile up with its replacement."
  (and flycheck-current-syntax-check
       (flycheck-checker-get
        (flycheck-syntax-check-checker flycheck-current-syntax-check)
        'interrupt)
       t))

(defconst flycheck--interrupting-conditions '(idle-change save)
  "Trigger conditions that may interrupt a running syntax check.

Only conditions that imply changed buffer contents qualify: the
running check's results are stale then, so restarting has value.
Buffer switches and the deferred-check drain don't change the
contents, and `new-line' fires on every keystroke; interrupting
for these would kill and restart the checker without gaining
anything.")

(defun flycheck--may-interrupt-at-condition-p (condition)
  "Whether a check triggered at CONDITION may interrupt a running one.

Consult `flycheck-interrupt-running-checks'.  Only conditions in
`flycheck--interrupting-conditions' interrupt, and with a numeric
option value only checks younger than that many seconds are
interrupted, so that slow checkers eventually complete."
  (let ((allowed flycheck-interrupt-running-checks))
    (and allowed
         (seq-intersection (if (listp condition) condition (list condition))
                           flycheck--interrupting-conditions)
         (or (eq allowed t)
             (< (- (float-time)
                   (flycheck-syntax-check-start-time
                    flycheck-current-syntax-check))
                allowed)))))

(defun flycheck-buffer-status-callback (syntax-check)
  "Create a status callback for SYNTAX-CHECK in the current buffer."
  (lambda (&rest args)
    (apply #'flycheck-report-buffer-checker-status
           syntax-check args)))

(defvar flycheck--modes-without-checker nil
  "Major modes already reported as having nothing to check them with.

Flycheck mentions this once per major mode per session.  Saying it on
every check would be constant noise in a buffer nothing can check, and
saying it never leaves someone who has just enabled Flycheck watching an
inert mode line with no idea why.")

(defun flycheck--report-no-checker ()
  "Say once per major mode that no syntax checker can run in this buffer.

Distinguish a mode nothing supports, where there is nothing to be done,
from one whose checkers are all unusable here, which usually means the
tool is not installed and is worth looking into."
  (unless (or (memq major-mode flycheck--modes-without-checker)
              (flycheck-ephemeral-buffer-p))
    (push major-mode flycheck--modes-without-checker)
    (let ((supported (seq-some (lambda (checker)
                                 (flycheck-checker-supports-major-mode-p
                                  checker major-mode))
                               flycheck-checkers)))
      (message
       (substitute-command-keys
        (if supported
            "Flycheck: no syntax checker for %s can run here; \
\\[flycheck-verify-setup] shows why"
          "Flycheck: no syntax checker supports %s; \
\\[flycheck-verify-setup] lists what there is"))
       major-mode))))

(defun flycheck-buffer ()
  "Start checking syntax in the current buffer.

Get a syntax checker for the current buffer with
`flycheck-get-checker-for-buffer', and start it.

Interactively, a running syntax check is interrupted first, per
`flycheck-interrupt-running-checks' (without its age limit, since
you asked for fresh results explicitly).  When called from Lisp
while a check is running, do nothing; automatic re-checks handle
interruption in `flycheck-buffer-automatically' instead."
  (interactive)
  (flycheck-clean-deferred-check)
  (if flycheck-mode
      (progn
        (when (and (called-interactively-p 'any)
                   (flycheck-running-p)
                   flycheck-interrupt-running-checks
                   (flycheck--interruptible-check-p)
                   ;; Don't kill a useful running check when no new one
                   ;; could start in its place; resolution errors surface
                   ;; in the check below
                   (ignore-errors (flycheck-get-checker-for-buffer)))
          (flycheck--interrupt-current-syntax-check))
        (unless (flycheck-running-p)
          ;; Clear error list and mark all overlays for deletion.  We do not
          ;; delete all overlays immediately to avoid excessive re-displays
          ;; and flickering, if the same errors gets highlighted again after
          ;; the check completed.
          (run-hooks 'flycheck-before-syntax-check-hook)
          (flycheck-clear-errors)
          (setq flycheck--suppressed-error-count 0)
          ;; A failure describes the cycle that recorded it, not this one
          (setq flycheck--last-failure nil)
          (flycheck-mark-all-overlays-for-deletion)
          (condition-case err
              (let* ((checker (flycheck-get-checker-for-buffer)))
                (if checker
                    (flycheck-start-current-syntax-check checker)
                  (flycheck-clear)
                  (flycheck--report-no-checker)
                  (flycheck-report-status 'no-checker)))
            (error
             (flycheck-report-failed-syntax-check)
             (signal (car err) (cdr err))))))
    (user-error "Flycheck mode disabled")))

(defun flycheck-report-buffer-checker-status
    (syntax-check status &optional data)
  "Report a SYNTAX-CHECK STATUS with DATA.

SYNTAX-CHECK is the `flycheck-syntax-check' which reported
STATUS.  STATUS denotes the status of the syntax check, with an
optional DATA.  STATUS may be one of the following symbols:

`errored'
     The syntax checker has errored.  DATA is an optional error
     message.

     This report finishes the current syntax check.

`interrupted'
     The syntax checker was interrupted.  DATA is ignored.

     This report finishes the current syntax check.

`finished'
     The syntax checker has finished with a proper error report
     for the current buffer.  DATA is the (potentially empty)
     list of `flycheck-error' objects reported by the syntax
     check.

     This report finishes the current syntax check.

`suspicious'
     The syntax checker encountered a suspicious state, which the
     user needs to be informed about.  DATA is an optional
     message.

`self-disabled'
     The syntax checker diagnosed itself as inapplicable to the
     buffer, e.g. a linter without a configuration file.  The
     checker is disabled in the buffer like a failing `:enabled'
     test, and checker selection is re-run so that another
     checker can take over.  DATA is an optional reason string
     for the echo-area notice.

     This report finishes the current syntax check.

A syntax checker _must_ report a status at least once with any
symbol that finishes the current syntax checker.  Otherwise
Flycheck gets stuck with the current syntax check.

If CHECKER is not the currently used syntax checker in
`flycheck-current-syntax-check', the status report is largely
ignored.  Notably, any errors reported by the checker are
discarded."
  (let ((buffer (flycheck-syntax-check-buffer syntax-check)))
    ;; Ignore the status report if the buffer is gone, or if this syntax check
    ;; isn't the current one in buffer (which can happen if this is an old
    ;; report of an interrupted syntax check, and a new syntax check was started
    ;; since this check was interrupted)
    (when (and (buffer-live-p buffer)
               (eq syntax-check
                   (buffer-local-value 'flycheck-current-syntax-check buffer)))
      (with-current-buffer buffer
        (let ((checker (flycheck-syntax-check-checker syntax-check)))
          (pcase status
            ((or `errored `interrupted)
             (flycheck-report-failed-syntax-check status)
             (when (eq status 'errored)
               (setq flycheck--last-failure (list checker 'errored data))
               ;; In case of error, show the error message
               (message "Error from syntax checker %s: %s"
                        checker (or data "UNKNOWN!"))))
            (`suspicious
             (setq flycheck--last-failure (list checker 'suspicious data))
             (when flycheck-mode
               ;; The output is often a crash dump, far too much for the
               ;; echo area, so say what happened and where to read it
               (message
                (substitute-command-keys
                 "Flycheck: cannot read %s's output, so it may be \
misconfigured; \\[flycheck-verify-setup] shows what it printed")
                checker))
             (flycheck-report-status 'suspicious))
            (`self-disabled
             (when flycheck-mode
               ;; Disable the checker like a failing `:enabled' test.  A
               ;; fallback checker is selected on the next automatic
               ;; check, uniformly for all buffers.  We deliberately don't
               ;; force a fallback in this very cycle: doing so has to
               ;; route through the automatic-check gates (which refuse
               ;; e.g. read-only buffers) and re-runs earlier chain
               ;; members, for a marginal gain over the next idle tick.
               (cl-pushnew checker flycheck--automatically-disabled-checkers)
               (message
                (substitute-command-keys
                 "Flycheck: %s disabled itself in this buffer%s; \
\\[universal-argument] \\[flycheck-disable-checker] re-enables it")
                checker (if data (format " (%s)" data) ""))
               ;; Complete the check without running the disabled
               ;; checker's own `:next-checkers'
               (flycheck-finish-current-syntax-check
                nil (flycheck-syntax-check-working-directory syntax-check)
                'no-next)))
            (`finished
             (when flycheck-mode
               ;; Only report errors from the checker if Flycheck Mode is
               ;; still enabled.
               (flycheck-finish-current-syntax-check
                data
                (flycheck-syntax-check-working-directory syntax-check))))
            (_
             (error "Unknown status %s from syntax checker %s"
                    status checker))))))))

(defun flycheck-finish-current-syntax-check (errors working-dir &optional no-next)
  "Finish the current syntax-check in the current buffer with ERRORS.

ERRORS is a list of `flycheck-error' objects reported by the
current syntax check in `flycheck-current-syntax-check'.

Report all ERRORS and, unless NO-NEXT is non-nil, potentially
start any next syntax checkers.

If the current syntax checker reported excessive errors, they are
truncated or discarded via `flycheck--handle-excessive-errors',
according to `flycheck-checker-error-threshold-action'.

Relative file names in ERRORS will be expanded relative to
WORKING-DIR."
  (let* ((syntax-check flycheck-current-syntax-check)
         (checker (flycheck-syntax-check-checker syntax-check))
         ;; The full, file-name-expanded error set, including errors for
         ;; other files that `flycheck-relevant-errors' drops below.  Record
         ;; it in the project store before narrowing to the buffer.
         (all-errors (flycheck-fill-and-expand-error-file-names
                      (flycheck-filter-errors
                       (flycheck-assert-error-list-p errors) checker)
                      working-dir))
         (relevant (flycheck-relevant-errors all-errors))
         (reported (flycheck--handle-excessive-errors checker relevant)))
    ;; Record exactly what the buffer shows (`reported', already narrowed
    ;; and flood-handled) plus the cross-file errors this check also found,
    ;; so a buffer's own errors stay identical between buffer and project
    ;; scope and its flood handling is never bypassed.
    (flycheck--project-record-errors
     (append reported
             (flycheck--project-storable-errors
              (seq-remove (lambda (err) (memq err relevant)) all-errors))))
    (flycheck-report-current-errors reported)
    (let ((next-checker (unless no-next
                          (flycheck-get-next-checker-for-buffer checker))))
      (if next-checker
          (flycheck-start-current-syntax-check next-checker)
        (setq flycheck-current-syntax-check nil)
        (flycheck-report-status 'finished)
        ;; Delete overlays only after the very last checker has run, to avoid
        ;; flickering on intermediate re-displays
        (flycheck-delete-marked-overlays)
        (flycheck-error-list-refresh)
        (run-hooks 'flycheck-after-syntax-check-hook)
        (when (and flycheck-auto-display-errors-after-checking
                   (eq (current-buffer) (window-buffer)))
          (flycheck-display-error-at-point))
        ;; Immediately try to run any pending deferred syntax check, which
        ;; were triggered by intermediate automatic check event, to make sure
        ;; that we quickly refine outdated error information
        (flycheck-perform-deferred-syntax-check)))))

(defun flycheck--handle-excessive-errors (checker errors)
  "Handle ERRORS from CHECKER exceeding the error threshold.

Return the errors to report: ERRORS when
`flycheck-checker-error-threshold' is not exceeded, the most
severe errors up to the threshold when
`flycheck-checker-error-threshold-action' is `truncate', or nil
when it is `disable'."
  (let ((total (length errors)))
    (if (or (null flycheck-checker-error-threshold)
            (<= total flycheck-checker-error-threshold))
        (progn
          (setq flycheck--excessive-checkers
                (remq checker flycheck--excessive-checkers))
          errors)
      (if (eq flycheck-checker-error-threshold-action 'disable)
          (progn
            (setq flycheck--excessive-checkers
                  (remq checker flycheck--excessive-checkers))
            (flycheck-disable-excessive-checker checker errors)
            nil)
        (flycheck--truncate-excessive-errors checker errors total)))))

(defun flycheck--excessive-errors-< (err1 err2)
  "Determine the truncation order of ERR1 and ERR2.

Orders by severity, from most to least severe; errors of equal
severity keep their buffer-position order, so that truncation
drops the errors furthest down in the buffer."
  (let ((severity1 (flycheck-error-level-severity
                    (flycheck-error-level err1)))
        (severity2 (flycheck-error-level-severity
                    (flycheck-error-level err2))))
    (if (= severity1 severity2)
        (flycheck-error-< err1 err2)
      (> severity1 severity2))))

(defun flycheck--take-most-severe-errors (errors n)
  "Return the N most severe of ERRORS, most severe first.

Errors of equal severity keep their buffer-position order (see
`flycheck--excessive-errors-<')."
  (seq-take (sort (copy-sequence errors) #'flycheck--excessive-errors-<) n))

(defun flycheck--truncate-excessive-errors (checker errors total)
  "Truncate ERRORS from CHECKER to the error threshold.

TOTAL is the length of ERRORS.  Keep the most severe errors up to
`flycheck-checker-error-threshold' and record the number of
suppressed errors in `flycheck--suppressed-error-count'."
  (let* ((threshold flycheck-checker-error-threshold)
         (kept (flycheck--take-most-severe-errors errors threshold)))
    (cl-incf flycheck--suppressed-error-count (- total threshold))
    (unless (memq checker flycheck--excessive-checkers)
      (push checker flycheck--excessive-checkers)
      (message "Flycheck: %s reported %d errors; showing the %d most severe \
(see `flycheck-checker-error-threshold')"
               checker total threshold))
    kept))

(defun flycheck-disable-excessive-checker (checker errors)
  "Disable CHECKER if it reported excessive ERRORS.

If ERRORS has more items than `flycheck-checker-error-threshold',
add CHECKER to `flycheck--automatically-disabled-checkers', and
say so in the echo area.

Return t when CHECKER was disabled, or nil otherwise."
  (when (and flycheck-checker-error-threshold
             (> (length errors) flycheck-checker-error-threshold))
    ;; Disable CHECKER for this buffer
    ;; (`flycheck--automatically-disabled-checkers' is a local variable).
    (message (substitute-command-keys
              "Flycheck: %s reported %d errors and was disabled in this \
buffer; \\[universal-argument] \\[flycheck-disable-checker] re-enables it")
             checker (length errors))
    (push checker flycheck--automatically-disabled-checkers)
    t))

(defun flycheck-clear (&optional shall-interrupt)
  "Clear all errors in the current buffer.

With prefix arg or SHALL-INTERRUPT non-nil, also interrupt the
current syntax check."
  (interactive "P")
  (when shall-interrupt
    (flycheck-stop))
  (flycheck-delete-all-overlays)
  ;; Inline annotations are separate overlays that the error-overlay
  ;; teardown above doesn't touch; drop them so a manual clear doesn't
  ;; leave stale messages behind (a re-check rebuilds them).  Harmless
  ;; no-op when inline display was never active.
  (flycheck-annotate--clear)
  (flycheck-clear-errors)
  ;; Note: `flycheck--excessive-checkers' deliberately survives a clear,
  ;; so that the truncation notification doesn't re-fire after every
  ;; manual clear or transient no-checker pass
  (setq flycheck--suppressed-error-count 0)
  (flycheck-clear-displayed-error-messages)
  (flycheck-error-list-refresh)
  (flycheck-hide-error-buffer))

(defun flycheck--empty-variables ()
  "Empty variables used by Flycheck."
  (kill-local-variable 'flycheck--file-truename-cache)
  (kill-local-variable 'flycheck--idle-trigger-timer)
  (kill-local-variable 'flycheck--idle-trigger-conditions))

(defun flycheck-teardown (&optional ignore-global)
  "Teardown Flycheck in the current buffer.

Completely clear the whole Flycheck state.  Remove overlays, kill
running checks, and empty all variables used by Flycheck.

Unless optional argument IGNORE-GLOBAL is non-nil, check to see
if no more Flycheck buffers remain (aside from the current
buffer), and if so then clean up global hooks."
  (flycheck-safe-delete-temporaries)
  (flycheck-stop)
  (flycheck-clean-deferred-check)
  (flycheck-clear)
  (setq flycheck--excessive-checkers nil)
  (flycheck--release-margin)
  (flycheck-cancel-error-display-error-at-point-timer)
  (flycheck--clear-idle-trigger-timer)
  (flycheck--empty-variables)
  (unless (or ignore-global
              (seq-some (lambda (buf)
                          (and (not (equal buf (current-buffer)))
                               (buffer-local-value 'flycheck-mode buf)))
                        (buffer-list)))
    (flycheck-global-teardown 'ignore-local)))


;;; Automatic syntax checking in a buffer
(defun flycheck-may-check-automatically (&rest conditions)
  "Determine whether the buffer may be checked under one of CONDITIONS.

Read-only buffers may never be checked automatically.

If CONDITIONS are given, determine whether syntax may be checked
under at least one of them, according to
`flycheck-check-syntax-automatically'."
  (and (not (or buffer-read-only (flycheck-ephemeral-buffer-p)))
       (or (not conditions)
           (let ((allowed
                  ;; Remote buffers use a narrower set of trigger events by
                  ;; default, since each check spawns a slow remote process
                  (if (and (file-remote-p default-directory)
                           (not (eq flycheck-check-syntax-automatically-remote
                                    t)))
                      flycheck-check-syntax-automatically-remote
                    flycheck-check-syntax-automatically)))
             (seq-some (lambda (condition) (memq condition allowed))
                       conditions)))
       ;; Checked last so a disallowed trigger short-circuits before this
       ;; possibly-remote (and thus blocking) stat of `default-directory'.
       (file-exists-p default-directory)))

(defvar-local flycheck--idle-trigger-timer nil
  "Timer used to trigger a syntax check after an idle delay.")

(defvar-local flycheck--idle-trigger-conditions nil
  "List of conditions under which an idle syntax check will be triggered.
This will be some subset of the allowable values for
`flycheck-check-syntax-automatically'.

For example, if the user switches to a buffer and then makes an
edit, this list will have the values `idle-change' and
`idle-buffer-switch' in it, at least until the idle timer
expires.")

(defun flycheck-buffer-automatically (&optional condition force-deferred)
  "Automatically check syntax at CONDITION.

Syntax is not checked if `flycheck-may-check-automatically'
returns nil for CONDITION.  (CONDITION may be a single condition
or a list of them.)

The syntax check is deferred if FORCE-DEFERRED is non-nil, or if
the buffer is not visible or being reverted.  When a syntax check
is already running, it is interrupted per
`flycheck-interrupt-running-checks'; if it is kept instead, the
new check is deferred until it finishes."
  (when (and flycheck-mode (if (listp condition)
                               (apply #'flycheck-may-check-automatically
                                      condition)
                             (flycheck-may-check-automatically condition)))
    (flycheck--clear-idle-trigger-timer)
    (setq flycheck--idle-trigger-conditions nil)
    (if (or force-deferred (flycheck--must-defer-regardless-of-running-p))
        (flycheck-buffer-deferred)
      ;; A new check supersedes a running one: its results would be
      ;; outdated by the time it finishes anyway.  But don't kill a useful
      ;; running check when no new one could start in its place.
      (when (and (flycheck-running-p)
                 (flycheck--may-interrupt-at-condition-p condition)
                 (flycheck--interruptible-check-p)
                 (ignore-errors (flycheck-get-checker-for-buffer)))
        (flycheck--interrupt-current-syntax-check))
      ;; When the running check was kept -- it cannot be interrupted, or
      ;; has made too much progress to throw away -- queue the new check
      ;; behind it, like older Flycheck versions did
      (if (flycheck-must-defer-check)
          (flycheck-buffer-deferred)
        (with-demoted-errors "Error while checking syntax automatically: %S"
          (flycheck-buffer))))))

(defun flycheck--clear-idle-trigger-timer ()
  "Clear the idle trigger timer."
  (when flycheck--idle-trigger-timer
    (cancel-timer flycheck--idle-trigger-timer)
    (setq flycheck--idle-trigger-timer nil)))

(defun flycheck--handle-idle-trigger (buffer)
  "Run a syntax check in BUFFER if appropriate.
This function is called by `flycheck--idle-trigger-timer'."
  (save-match-data
    (let ((current-buffer (current-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (or flycheck-buffer-switch-check-intermediate-buffers
                      (eq buffer current-buffer))
            (setq flycheck--idle-trigger-conditions
                  (delq 'idle-buffer-switch
                        flycheck--idle-trigger-conditions)))
          (when flycheck--idle-trigger-conditions
            (flycheck-buffer-automatically flycheck--idle-trigger-conditions)
            (setq flycheck--idle-trigger-conditions nil)))))))

(defun flycheck-handle-change (beg end _len)
  "Handle a buffer change between BEG and END.

BEG and END mark the beginning and end of the change text.  _LEN
is ignored.

Start a syntax check if a new line has been inserted into the
buffer."
  ;; Save and restore the match data, as recommended in (elisp)Change Hooks
  (save-match-data
    (when flycheck-mode
      (if (string-match-p (rx "\n") (buffer-substring beg end))
          (flycheck-buffer-automatically 'new-line 'force-deferred)
        (when (memq 'idle-change flycheck-check-syntax-automatically)
          (flycheck--clear-idle-trigger-timer)
          (cl-pushnew 'idle-change flycheck--idle-trigger-conditions)
          (setq flycheck--idle-trigger-timer
                (run-at-time flycheck-idle-change-delay nil
                             #'flycheck--handle-idle-trigger
                             (current-buffer))))))))

(defvar flycheck--last-buffer (current-buffer)
  "The current buffer or the buffer that was previously current.
This is usually equal to the current buffer, unless the user just
switched buffers.  After a buffer switch, it is the previous
buffer.")

(defun flycheck-handle-buffer-switch ()
  "Handle a possible switch to another buffer.

If a buffer switch actually happened, schedule a syntax check."
  ;; Switching buffers here is weird, but unfortunately necessary.  It
  ;; turns out that `with-temp-buffer' triggers
  ;; `buffer-list-update-hook' twice, and the value of
  ;; `current-buffer' is bogus in one of those triggers (the one just
  ;; after the temp buffer is killed).  If we rely on the bogus value,
  ;; Flycheck will think that the user is switching back and forth
  ;; between different buffers during the `with-temp-buffer' call
  ;; (note: two different normal buffers, not the current buffer and
  ;; the temp buffer!), and that would trigger spurious syntax checks.
  ;; It seems that reading (window-buffer) gets us the correct current
  ;; buffer in all important real-life situations (although it doesn't
  ;; necessarily catch uses of `set-buffer').
  (with-current-buffer (window-buffer)
    (unless (or (equal flycheck--last-buffer (current-buffer))
                ;; Don't bother keeping track of changes to and from
                ;; the minibuffer, as they will never require us to
                ;; run a syntax check.
                (minibufferp))
      (setq flycheck--last-buffer (current-buffer))
      (when (and flycheck-mode
                 (memq 'idle-buffer-switch flycheck-check-syntax-automatically))
        (flycheck--clear-idle-trigger-timer)
        (cl-pushnew 'idle-buffer-switch flycheck--idle-trigger-conditions)
        (setq flycheck--idle-trigger-timer
              (run-at-time flycheck-idle-buffer-switch-delay nil
                           #'flycheck--handle-idle-trigger
                           (current-buffer)))))))

(defun flycheck-handle-save ()
  "Handle a save of the buffer."
  (flycheck-buffer-automatically 'save))

(defun flycheck-handle-revert ()
  "Handle a buffer revert.
Start a syntax check after the buffer has been reverted, but only
if `flycheck-mode' is still active (it may have been killed by
`revert-buffer' via `kill-all-local-variables')."
  (when flycheck-mode
    (flycheck-buffer)))


;;; Deferred syntax checking
(defvar-local flycheck-deferred-syntax-check nil
  "If non-nil, a deferred syntax check is pending.")

(defun flycheck-must-defer-check ()
  "Determine whether the syntax check has to be deferred.

A check has to be deferred if the buffer is not visible, or if the buffer is
currently being reverted.

Return t if the check is to be deferred, or nil otherwise."
  (or (flycheck--must-defer-regardless-of-running-p)
      ;; We defer the syntax check if Flycheck is already running, to
      ;; immediately start a new syntax check after the current one finished,
      ;; because the result of the current check will most likely be outdated by
      ;; the time it is finished.  `flycheck-buffer-automatically' may
      ;; interrupt the running check instead; see
      ;; `flycheck-interrupt-running-checks'.
      (flycheck-running-p)))

(defun flycheck--must-defer-regardless-of-running-p ()
  "Whether a new syntax check must be deferred, running check or not.

Like `flycheck-must-defer-check', but without considering a
running syntax check, which may be interrupted to make room; the
conditions here cannot be resolved by interruption."
  (or (not (get-buffer-window))
      ;; We must defer checks while a buffer is being reverted, to avoid race
      ;; conditions while the buffer contents are being restored.  Emacs 31
      ;; renamed the variable and kept the old name as an alias, which is the
      ;; one that works on every Emacs we support.
      (with-suppressed-warnings ((obsolete revert-buffer-in-progress-p))
        revert-buffer-in-progress-p)))

(defun flycheck-deferred-check-p ()
  "Determine whether the current buffer has a deferred check.

Return t if so, or nil otherwise."
  flycheck-deferred-syntax-check)

(defun flycheck-buffer-deferred ()
  "Defer syntax check for the current buffer."
  (setq flycheck-deferred-syntax-check t))

(defun flycheck-clean-deferred-check ()
  "Clean a deferred syntax checking state."
  (setq flycheck-deferred-syntax-check nil))

(defun flycheck-perform-deferred-syntax-check ()
  "Perform the deferred syntax check."
  (when (flycheck-deferred-check-p)
    (flycheck-clean-deferred-check)
    (flycheck-buffer-automatically)))


;;; Syntax checking in all buffers
(defun flycheck-may-enable-mode ()
  "Determine whether Flycheck mode may be enabled.

Flycheck mode is not enabled for

- the minibuffer,
- `fundamental-mode'
- major modes whose `mode-class' property is `special',
- Flycheck's own error message buffer,
- ephemeral buffers (see `flycheck-ephemeral-buffer-p'),
- encrypted buffers (see `flycheck-encrypted-buffer-p'),
- and major modes excluded by `flycheck-global-modes'.

Remote files (see `file-remote-p') are checked like local ones;
command checkers run on the remote host over TRAMP.

Return non-nil if Flycheck mode may be enabled, and nil
otherwise."
  (and (pcase flycheck-global-modes
         ;; Whether `major-mode' is disallowed by `flycheck-global-modes'
         (`t t)
         (`(not . ,modes) (not (memq major-mode modes)))
         (modes (memq major-mode modes)))
       (not (or (minibufferp)
                (eq major-mode 'fundamental-mode)
                (eq (get major-mode 'mode-class) 'special)
                (derived-mode-p 'flycheck-error-message-mode)
                (flycheck-ephemeral-buffer-p)
                (flycheck-encrypted-buffer-p)))))

(defun flycheck-mode-on-safe ()
  "Enable command `flycheck-mode' if it is safe to do so.

Command `flycheck-mode' is only enabled if
`flycheck-may-enable-mode' returns a non-nil result."
  (when (flycheck-may-enable-mode)
    (flycheck-mode)))

;;;###autoload
(define-globalized-minor-mode global-flycheck-mode flycheck-mode
  flycheck-mode-on-safe
  :init-value nil
  :group 'flycheck)

(defun flycheck-global-teardown (&optional ignore-local)
  "Teardown Flycheck in all buffers.

Completely clear the whole Flycheck state in all buffers, stop
all running checks, remove all temporary files, and empty all
variables of Flycheck.

Also remove global hooks.  (If optional argument IGNORE-LOCAL is
non-nil, then only do this and skip per-buffer teardown.)"
  (unless ignore-local
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when flycheck-mode
            (flycheck-teardown 'ignore-global))))))
  (remove-hook 'buffer-list-update-hook #'flycheck-handle-buffer-switch))

;; Clean up the entire state of Flycheck when Emacs is killed, to get rid of any
;; pending temporary files.
(add-hook 'kill-emacs-hook #'flycheck-global-teardown)


;;; Errors from syntax checks
(cl-defstruct (flycheck-related-location
               (:constructor flycheck-related-location-new)
               (:copier nil))
  "A secondary source location attached to a `flycheck-error'.

Many checkers attach one or more secondary locations to an error,
pointing at other places that explain or contribute to it (for example
the earlier definition behind a \"redefined here\" error, or the borrow
behind a Rust lifetime error).  Language servers report these as an LSP
diagnostic's `relatedInformation'.

Unlike an error's group (see `flycheck-related-errors'), a related
location is not itself an error: it carries only a position and a
message, and it may live in a different file than the error it belongs
to.  See the `relations' slot of `flycheck-error' and
`flycheck-visit-related-location'.

Slots:

`filename'
     The file the location refers to, as a string.  Defaults to the
     error's own file.

`line'
     The line the location starts on, as a 1-based number.

`column' (optional)
     The column the location starts at, as a 1-based number.

`end-line' (optional)
     The line the location ends on.

`end-column' (optional)
     The column the location ends at, right-open like a
     `flycheck-error' span.

`message' (optional)
     The message describing the location, as a string."
  filename line column end-line end-column message)

(cl-defstruct (flycheck-error
               (:constructor nil)
               (:constructor
                flycheck-error-new
                (&key
                 line column end-line end-column
                 buffer checker filename message level id group fix relations
                 tags
                 &aux (-end-line end-line) (-end-column end-column)
                 (-fix fix) (-relations relations) (-tags tags)))
               (:constructor
                flycheck-error-new-at
                (line
                 column
                 &optional level message
                 &key end-line end-column checker id group fix relations tags
                 (filename (buffer-file-name)) (buffer (current-buffer))
                 &aux (-end-line end-line) (-end-column end-column)
                 (-fix fix) (-relations relations) (-tags tags)))
               (:constructor
                flycheck-error-new-at-pos
                (pos
                 &optional level message
                 &key end-pos checker id group fix relations tags
                 (filename (buffer-file-name)) (buffer (current-buffer))
                 &aux
                 ((line . column)
                  (if pos (flycheck-line-column-at-pos pos)
                    '(nil . nil)))
                 ((-end-line . -end-column)
                  (if end-pos (flycheck-line-column-at-pos end-pos)
                    '(nil . nil)))
                 (-fix fix) (-relations relations) (-tags tags))))
  "Structure representing an error reported by a syntax checker.
Slots:

`buffer'
     The buffer that the error was reported for, as buffer object.

`checker'
     The syntax checker which reported this error, as symbol.

`filename'
     The file name the error refers to, as string.

`line'
     The line on which the error starts, as number.

`column' (optional)
     The column at which the error starts, as number.

     For compatibility with external tools and unlike Emacs
     itself (e.g. in Compile Mode) Flycheck uses _1-based_
     columns: The first character on a line is column 1.

     Occasionally some tools try to proactively adapt to Emacs
     and emit 0-based columns automatically.  In these cases, the
     columns must be adjusted for Flycheck, see
     `flycheck-increment-error-columns'.

     If nil, the whole line is highlighted.

`end-line' (optional)
    The line on which the error ends.  If nil, this is computed according to
    `flycheck-highlighting-mode'.

`end-column'
    The column at which the error ends.  If nil, this is computed according to
    `flycheck-highlighting-mode'.  Error intervals are right-open: the
    end-column points to the first character not included in the error.  For
    example, 1:1 is an empty range, and in \"line-number-at-pos\", the range
    6:12 covers the word \"number\".

`message' (optional)
     The error message as a string, if any.

`level'
     The error level, as either `info', `warning' or `error'.

`id' (optional)
     An ID identifying the kind of error.

`group' (optional)
     A symbol identifying the group the error belongs to.

     Some tools will emit multiple errors that relate to the same
     issue (e.g., lifetime errors in Rust).  All related errors
     collected by a checker should have the same `group` value,
     in order to be able to present them to the user.

     See `flycheck-related-errors`.

`relations' (optional)
     A list of `flycheck-related-location' objects pointing at
     secondary source locations that explain or contribute to the
     error, such as an earlier definition or a borrow behind a
     lifetime error.  These come from an LSP diagnostic's
     `relatedInformation' and may live in other files.

     See `flycheck-visit-related-location`.

`tags' (optional)
     A list of symbols saying something about the error beyond its
     severity: `unnecessary' for code that has no effect, such as an
     unused import or an unreachable branch, and `deprecated' for
     something that still works but should not be used.

     These come from an LSP diagnostic's `tags'.  They are not levels:
     an error is unnecessary or deprecated *as well as* being a warning
     or an error, and Flycheck renders them by adding a face rather than
     by changing the level."
  buffer checker filename line column message level id group
  ;; The fields below are at the end of the record to preserve backwards
  ;; compatibility; see https://github.com/flycheck/flycheck/pull/1400 and
  ;; https://lists.gnu.org/archive/html/emacs-devel/2018-07/msg00436.html
  -end-line -end-column -fix -relations -tags)

;; These accessors are defined for backwards compatibility
;; FIXME: Clean up once package.el learns how to recompile dependencies.

(defun flycheck-error-end-line (err)
  "Return the end line of a Flycheck error ERR."
  (condition-case nil (flycheck-error--end-line err)
    (args-out-of-range nil)))

(defun flycheck-error-end-column (err)
  "Return the end column of a Flycheck error ERR."
  (condition-case nil (flycheck-error--end-column err)
    (args-out-of-range nil)))

(defun flycheck-error--set-end-line (err line)
  "Set the end line of a Flycheck error ERR to LINE."
  (condition-case nil (setf (flycheck-error--end-line err) line)
    (args-out-of-range nil)))

(defun flycheck-error--set-end-column (err column)
  "Set the end column of a Flycheck error ERR to COLUMN."
  (condition-case nil (setf (flycheck-error--end-column err) column)
    (args-out-of-range nil)))

(gv-define-simple-setter flycheck-error-end-line
                         flycheck-error--set-end-line)
(gv-define-simple-setter flycheck-error-end-column
                         flycheck-error--set-end-column)

(defun flycheck-error-fix (err)
  "Return the suggested fix of a Flycheck error ERR, or nil.

The value is a `flycheck-fix' object when the checker offered a
machine-applicable fix for ERR; see `flycheck-apply-fix'.  It may instead
be a function of one argument (ERR) that produces the fix on demand -- a
lazy fix provider, used when computing the fix is expensive (e.g. an LSP
code-action request).  Call `flycheck-error-resolve-fix' to get the
concrete fix.

A non-nil value means a fix may be applicable, which is what the
commands act on.  It does not mean one is known to exist, since a
provider may still come up empty, so the indicators use
`flycheck-error-known-fix-p' instead."
  (condition-case nil (flycheck-error--fix err)
    (args-out-of-range nil)))

(defun flycheck-error-known-fix-p (err)
  "Whether ERR is known to carry a fix, without resolving anything.

True only for a fix Flycheck already holds.  A lazy provider (see
`flycheck-error-fix') may or may not produce a fix once asked, so an
error carrying one is not known to be fixable, and marking it as such
would promise a fix that may not exist."
  (let ((fix (flycheck-error-fix err)))
    (and fix (not (functionp fix)))))

(defun flycheck-error--set-fix (err fix)
  "Set the suggested fix of a Flycheck error ERR to FIX."
  (condition-case nil (setf (flycheck-error--fix err) fix)
    (args-out-of-range nil)))

(gv-define-simple-setter flycheck-error-fix flycheck-error--set-fix)

(defun flycheck-error-relations (err)
  "Return the related locations of a Flycheck error ERR, as a list.

Each element is a `flycheck-related-location' pointing at a secondary
source location that explains or contributes to ERR; see
`flycheck-visit-related-location'.  Returns nil when ERR has none."
  (condition-case nil (flycheck-error--relations err)
    (args-out-of-range nil)))

(defun flycheck-error--set-relations (err relations)
  "Set the related locations of a Flycheck error ERR to RELATIONS."
  (condition-case nil (setf (flycheck-error--relations err) relations)
    (args-out-of-range nil)))

(gv-define-simple-setter flycheck-error-relations
                         flycheck-error--set-relations)

(defun flycheck-error-tags (err)
  "Return the tags of a Flycheck error ERR, as a list of symbols.

`unnecessary' for code that has no effect and `deprecated' for something
that still works but should not be used; see `flycheck-error'.  Returns
nil when ERR has none."
  (condition-case nil (flycheck-error--tags err)
    (args-out-of-range nil)))

(defun flycheck-error--set-tags (err tags)
  "Set the tags of a Flycheck error ERR to TAGS."
  (condition-case nil (setf (flycheck-error--tags err) tags)
    (args-out-of-range nil)))

(gv-define-simple-setter flycheck-error-tags flycheck-error--set-tags)

(defun flycheck-error-resolve-fix (err)
  "Return the concrete `flycheck-fix' for ERR, or nil.

If ERR's fix slot holds a lazy provider (a function, see
`flycheck-error-fix'), call it with ERR to produce the fix; otherwise
return the stored fix as-is.  A provider may return nil when no fix turns
out to be available."
  (let ((fix (flycheck-error-fix err)))
    (if (functionp fix) (funcall fix err) fix)))

(cl-defstruct (flycheck-fix-edit (:constructor flycheck-fix-edit-new))
  "A single text edit of a `flycheck-fix'.

Replace the region from LINE, COLUMN to END-LINE, END-COLUMN with
REPLACEMENT.  Positions are one-based, as in `flycheck-error'; an
edit that only inserts text has END-LINE, END-COLUMN equal to
LINE, COLUMN, and an edit that only deletes has an empty
REPLACEMENT."
  line column end-line end-column replacement)

(cl-defstruct (flycheck-fix (:constructor flycheck-fix-new))
  "A machine-applicable fix a checker suggested for an error.

DESCRIPTION is a short human-readable summary, or nil.  EDITS is
the list of `flycheck-fix-edit' objects to apply together.  TICK
is the buffer's `buffer-chars-modified-tick' when the check that
produced the fix started; `flycheck-apply-fix' refuses to apply
the fix if the buffer has changed since, so stale line and column
numbers can never silently corrupt it.  See `flycheck-apply-fix'."
  description edits tick)

(defmacro flycheck-error-with-buffer (err &rest forms)
  "Switch to the buffer of ERR and evaluate FORMS.

If the buffer of ERR is not live, FORMS are not evaluated."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (flycheck-error-buffer ,err))
     (with-current-buffer (flycheck-error-buffer ,err)
       ,@forms)))

(defun flycheck--exact-region (err)
  "Get the region of ERR, if ERR specifies a range.

Return a cons cell `(BEG . END)'.  If the input range is empty,
it is expanded to cover at least one character so that END is
always greater than BEG.  If ERR doesn't specify an end-column
return nil."
  (if-let* ((line (flycheck-error-line err))
            (column (flycheck-error-column err))
            (end-line (or (flycheck-error-end-line err) line))
            (end-column (flycheck-error-end-column err)))
      ;; Ignoring fields speeds up calls to `line-end-position'.
      (let* ((inhibit-field-text-motion t)
             (beg (flycheck-line-column-to-position line column))
             (end (flycheck-line-column-to-position end-line end-column)))
        (cond
         ((< beg end) (cons beg end))
         ((= end (point-max)) (cons (1- end) end))
         (t (cons end (1+ end)))))))

(defun flycheck--line-region (pos)
  "Get the line region of position POS.

Return a cons cell `(BEG . END)' where BEG is the first
non-whitespace character on the line POS refers to, and END the
end of the line."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((bol (point))
          (end (line-end-position)))
      ;; Move to the beginning of this line's indentation, similar to
      ;; `back-to-indentation'
      (skip-syntax-forward " " end)
      (backward-prefix-chars)
      ;; If the current line is blank, highlight it in full; if it's
      ;; empty, include the previous line break character(s) to have
      ;; any region at all (when called with 0, `line-end-position'
      ;; gives us the end of the previous line).
      (cons (if (eolp) (if (= bol end) (line-end-position 0) bol) (point))
            end))))

(defun flycheck--column-region (pos)
  "Get the column region of position POS.

Return a cons cell `(BEG . END)' where BEG is the position at
the column, and END is one past it."
  (save-excursion
    (goto-char pos)
    ;; (eobp): Not enough lines in the buffer
    (if (eobp) (cons (1- (point-max)) (point-max))
      (cons pos (1+ pos)))))

(defun flycheck-bounds-of-thing-at-point (thing pos)
  "Get the region of THING at position POS.

THING is understood by `thing-at-point'.

Return a cons cell `(BEG . END)' where BEG is the beginning of
the THING at the column, and END the end of the THING."
  (save-excursion
    (goto-char pos)
    (bounds-of-thing-at-point thing)))

(defun flycheck--approximate-region (err mode)
  "Compute the region of ERR based on MODE and ERR's line and column."
  ;; Ignoring fields speeds up calls to `line-end-position'.
  (let* ((inhibit-field-text-motion t)
         (line (flycheck-error-line err))
         (column (flycheck-error-column err))
         (beg (flycheck-line-column-to-position line (or column 1))))
    (if (or (null column)
            (eq mode 'lines))
        (flycheck--line-region beg)
      (or (pcase mode
            (`symbols
             ;; Ensure that we're on a word or symbol.  See
             ;; https://github.com/flycheck/flycheck/issues/1519
             (and (<= (point-min) beg) (< beg (point-max))
                  (memq (char-syntax (char-after beg)) '(?w ?_))
                  (flycheck-bounds-of-thing-at-point 'symbol beg)))
            (`sexps
             (flycheck-bounds-of-thing-at-point 'sexp beg)))
          (flycheck--column-region beg)))))

(defun flycheck-error-region-for-mode (err mode)
  "Get the region of ERR for the highlighting MODE.

ERR is a Flycheck error.  If its position is fully specified, use
that to compute a region; otherwise, use MODE, as documented in
`flycheck-highlighting-mode'.  If MODE is nil, signal an error."
  (flycheck-error-with-buffer err
    (save-restriction
      (widen)
      (or (flycheck--exact-region err)
          (flycheck--approximate-region err mode)))))

(defun flycheck-error-pos (err)
  "Get the buffer position of ERR.

ERR is a Flycheck error whose position to get.

The error position is the error column, or the first
non-whitespace character of the error line, if ERR has no error column."
  (car (flycheck-error-region-for-mode
        err flycheck-highlighting-mode)))


;;; Applying fixes

(defun flycheck--fix-region (line column end-line end-column)
  "Return the buffer region (BEG . END) the coordinates name.

LINE, COLUMN, END-LINE and END-COLUMN are resolved in the current
buffer.  A missing column defaults to 1 and a missing end line to LINE."
  (let ((beg (flycheck-line-column-to-position line (or column 1)))
        (end (flycheck-line-column-to-position
              (or end-line line) (or end-column column 1))))
    (cons (min beg end) (max beg end))))

(defun flycheck--fix-edit-region (edit)
  "Return the buffer region (BEG . END) EDIT is to replace.

EDIT is a `flycheck-fix-edit'; positions are resolved in the
current buffer, so call this in the buffer being fixed."
  (flycheck--fix-region (flycheck-fix-edit-line edit)
                        (flycheck-fix-edit-column edit)
                        (flycheck-fix-edit-end-line edit)
                        (flycheck-fix-edit-end-column edit)))

(defun flycheck--make-fix (buffer description edits)
  "Return a `flycheck-fix' with DESCRIPTION and EDITS, or nil if no EDITS.

Stamp the fix with BUFFER's modification tick at the start of the
current check (see `flycheck--syntax-check-modified-tick'), so
`flycheck-apply-fix' can tell whether the buffer has changed since."
  (when edits
    (flycheck-fix-new
     :description description :edits edits
     :tick (and (buffer-live-p buffer)
                (buffer-local-value 'flycheck--syntax-check-modified-tick
                                    buffer)))))

(defun flycheck--check-fix-tick (fix)
  "Signal a `user-error' if the current buffer changed since FIX was computed."
  (when (and (flycheck-fix-tick fix)
             (/= (flycheck-fix-tick fix) (buffer-chars-modified-tick)))
    (user-error
     "The buffer changed since this fix was computed; re-check first")))

(defun flycheck--check-fix-edit (edit)
  "Signal a `user-error' when EDIT's coordinates cannot mean what they say.

A checker builds these out of a tool's output, and tools do emit ranges
that make no sense: a column of zero where Flycheck counts from one, an
end that comes before its start.  Resolving those anyway lands the edit
somewhere the checker never pointed at, which for a feature that writes
to the buffer is the one outcome worth refusing outright.  Positions off
the end of the buffer are left alone, since a fix that appends a missing
trailing newline legitimately names one."
  (let ((line (flycheck-fix-edit-line edit))
        (column (flycheck-fix-edit-column edit))
        (end-line (flycheck-fix-edit-end-line edit))
        (end-column (flycheck-fix-edit-end-column edit)))
    (unless (and (integerp line) (>= line 1))
      (user-error "This fix names line %S, which is not a line; not applying it"
                  line))
    (when (and column (< column 1))
      (user-error "This fix names column %S, and columns start at 1; \
not applying it" column))
    (when (and end-column (< end-column 1))
      (user-error "This fix names end column %S, and columns start at 1; \
not applying it" end-column))
    (when (and end-line (< end-line line))
      (user-error "This fix ends on line %S, before it starts on %S; \
not applying it" end-line line))
    (when (and end-column column (or (null end-line) (= end-line line))
               (< end-column column))
      (user-error "This fix ends at column %S, before it starts at %S; \
not applying it" end-column column))))

(defun flycheck--apply-edits (edits)
  "Apply EDITS in the current, widened buffer as one undoable change.

EDITS is a list of `flycheck-fix-edit' objects.  They are applied from
the end of the buffer backwards, so applying one does not shift the
positions of the ones above it.  Signal a `user-error', touching
nothing, when an edit's coordinates make no sense or the edits overlap."
  (mapc #'flycheck--check-fix-edit edits)
  (let ((regions
         ;; Apply from the bottom up; break ties on equal starts by the
         ;; larger region first.
         (sort (mapcar (lambda (edit)
                         (cons (flycheck--fix-edit-region edit)
                               (flycheck-fix-edit-replacement edit)))
                       edits)
               (lambda (a b)
                 (let ((ra (car a)) (rb (car b)))
                   (or (> (car ra) (car rb))
                       (and (= (car ra) (car rb))
                            (> (cdr ra) (cdr rb)))))))))
    ;; Reject overlapping edits: applying them bottom-up would let one
    ;; clobber another.  REGIONS are sorted with later positions first, so
    ;; each region must end at or before the previous one began.
    (let ((limit nil))
      (pcase-dolist (`((,beg . ,end) . ,_) regions)
        (when (and limit (> end limit))
          (user-error "This fix has overlapping edits; not applying it"))
        (setq limit beg)))
    (atomic-change-group
      (pcase-dolist (`((,beg . ,end) . ,replacement) regions)
        (delete-region beg end)
        (save-excursion
          (goto-char beg)
          (insert (or replacement "")))))))

(defun flycheck-apply-fix (fix &optional buffer)
  "Apply FIX in BUFFER, defaulting to the current buffer.

FIX is a `flycheck-fix' object.  Its edits are applied together as
a single undoable change, from the end of the buffer backwards so
that earlier edits do not invalidate the positions of later ones.

Signal a `user-error', touching nothing, when BUFFER is not live
or read-only, when the buffer has changed since the check that
produced the fix (its line and column numbers would be stale), or
when the fix's own edits overlap -- so a fix can never silently
corrupt the buffer."
  (let ((buffer (or buffer (current-buffer))))
    (unless (buffer-live-p buffer)
      (user-error "Cannot apply a fix: its buffer is gone"))
    (with-current-buffer buffer
      (when buffer-read-only
        (user-error "Cannot apply a fix in a read-only buffer"))
      (flycheck--check-fix-tick fix)
      (save-restriction
        (widen)
        (flycheck--apply-edits (flycheck-fix-edits fix))))))

(defun flycheck--fix-span (fix)
  "Return the buffer region (BEG . END) that FIX's edits span.

BEG is the earliest and END the latest position touched by any of
FIX's edits, resolved in the current buffer."
  (let ((regions (mapcar #'flycheck--fix-edit-region (flycheck-fix-edits fix))))
    (cons (apply #'min (mapcar #'car regions))
          (apply #'max (mapcar #'cdr regions)))))

(defun flycheck-apply-fixes (fixes &optional buffer)
  "Apply as many of FIXES together in BUFFER as do not conflict.

FIXES is a list of `flycheck-fix' objects.  Their edits are merged and
applied from the end of the buffer backwards as a single undoable
change, so applying one does not invalidate the positions of the
others.  A fix whose span would overlap an already-selected fix is
skipped whole (never applied partially).  Return the number of fixes
applied.

Signal a `user-error', touching nothing, when BUFFER is not live or
read-only, or when any fix is stale -- the buffer changed since it was
computed."
  (let ((buffer (or buffer (current-buffer))))
    (unless (buffer-live-p buffer)
      (user-error "Cannot apply fixes: their buffer is gone"))
    (with-current-buffer buffer
      (when buffer-read-only
        (user-error "Cannot apply fixes in a read-only buffer"))
      (mapc #'flycheck--check-fix-tick fixes)
      ;; Ignore fixes with no edits: they contribute nothing and would trip
      ;; up `flycheck--fix-span'.  In practice every live fix has edits.
      (setq fixes (seq-filter #'flycheck-fix-edits fixes))
      (save-restriction
        (widen)
        ;; Greedily select a non-overlapping subset, bottom-up: process the
        ;; fixes lowest in the buffer first and keep a fix only if its whole
        ;; span sits at or above every fix already selected below it.
        (let ((spanned (mapcar (lambda (fix)
                                 (cons (flycheck--fix-span fix) fix))
                               fixes))
              (boundary most-positive-fixnum)
              (selected nil))
          ;; Sort by span start, latest in the buffer first, and keep a fix
          ;; when its whole span ends at or before every fix already kept
          ;; below it.  Sorting by start (not end) maximizes the number of
          ;; fixes applied: it is the classic interval-scheduling greedy, so
          ;; one wide-span fix can't crowd out several small ones.
          (setq spanned (sort spanned (lambda (a b) (> (caar a) (caar b)))))
          (pcase-dolist (`((,beg . ,end) . ,fix) spanned)
            (when (<= end boundary)
              (push fix selected)
              (setq boundary (min boundary beg))))
          (when selected
            (flycheck--apply-edits
             (apply #'append (mapcar #'flycheck-fix-edits selected))))
          (length selected))))))

(defun flycheck--error-fix-buffer (err)
  "Return the live buffer in which ERR's fix may be applied, or nil.

A fix's line and column numbers only make sense in a buffer
visiting ERR's own file, so a cross-file error -- one a
whole-project checker reports for a file other than the one being
edited -- cannot be fixed in place."
  (when-let* ((buffer (flycheck-error-buffer err))
              ((buffer-live-p buffer)))
    (let ((filename (flycheck-error-filename err)))
      (when (with-current-buffer buffer
              (or (null filename)
                  (and buffer-file-name
                       (flycheck-same-files-p filename buffer-file-name))))
        buffer))))

(defun flycheck-error-format-snippet (err &optional max-length)
  "Extract the text that ERR refers to from the buffer.

Newlines and blanks are replaced by single spaces.  If ERR
doesn't include an end-position, return nil.

MAX-LENGTH is how many characters to read from the buffer, at
most.  It defaults to 20."
  (flycheck-error-with-buffer err
    (save-restriction
      (widen)
      (pcase (flycheck--exact-region err)
        (`(,beg . ,end)
         (truncate-string-to-width
          (replace-regexp-in-string
           "\\s-+" " " (buffer-substring beg (min end (point-max))))
          (or max-length 20) nil nil t))))))

(defun flycheck-error-format-message-and-id (err &optional include-snippet)
  "Format the message and id of ERR as human-readable string.

If INCLUDE-SNIPPET is non-nil, prepend the message with a snippet
of the text that the error applies to (such text can only be
determined if the error contains a full span, not just a
beginning position)."
  (let* ((id (flycheck-error-id err))
         (fname (flycheck-error-filename err))
         (other-file-p (and fname (not (equal fname (buffer-file-name))))))
    (concat (and other-file-p (format "In %S:\n" (file-relative-name fname)))
            (and include-snippet
                 (when-let* ((snippet (flycheck-error-format-snippet err)))
                   ;; \u2068 (FIRST STRONG ISOLATE) and \u2069 (POP
                   ;; DIRECTIONAL ISOLATE); the equivalent \N{...} escapes
                   ;; break native compilation on Emacs 32 (#2177)
                   (format-message "`\u2068%s\u2069': " snippet)))
            (or (flycheck-error-message err)
                (format "Unknown %S" (flycheck-error-level err)))
            (and id (format " [%s]" id)))))

(defun flycheck-error-format-position (err)
  "Format the position of ERR as a human-readable string."
  (let ((line (flycheck-error-line err))
        (column (flycheck-error-column err))
        (end-line (flycheck-error-end-line err))
        (end-column (flycheck-error-end-column err)))
    (if (and line column)
        (if (or (null end-line) (equal line end-line))
            (if (or (null end-column) (equal column (1- end-column)))
                (format "%d:%d" line column)
              (format "%d:%d-%d" line column end-column))
          (format "(%d:%d)-(%d:%d)" line column end-line end-column))
      (if (or (null end-line) (equal line end-line))
          (format "%d" line)
        (format "%d-%d" line end-line)))))

(defun flycheck-error-format (err &optional with-file-name)
  "Format ERR as human-readable string, optionally WITH-FILE-NAME.

Return a string that represents the given ERR.  If WITH-FILE-NAME
is given and non-nil, include the file-name as well, otherwise
omit it."
  (let* ((level (symbol-name (flycheck-error-level err)))
         (checker (symbol-name (flycheck-error-checker err)))
         (format `(,@(when with-file-name
                       (list (flycheck-error-filename err) ":"))
                   ,(flycheck-error-format-position err) ":"
                   ,level ": "
                   ,(flycheck-error-format-message-and-id err)
                   " (" ,checker ")")))
    (apply #'concat format)))

(defun flycheck-error-< (err1 err2)
  "Determine whether ERR1 is less than ERR2 by location."
  (let ((l1 (flycheck-error-line err1))
        (l2 (flycheck-error-line err2)))
    (if (/= l1 l2)
        (< l1 l2)
      (let ((c1 (or (flycheck-error-column err1) 1))
            (c2 (or (flycheck-error-column err2) 1)))
        (if (/= c1 c2)
            (< c1 c2)
          (let ((el1 (or (flycheck-error-end-line err1) l1))
                (el2 (or (flycheck-error-end-line err2) l2)))
            (if (/= el1 el2)
                (< el1 el2)
              (let ((cl1 (or (flycheck-error-end-column err1) 1))
                    (cl2 (or (flycheck-error-end-column err2) 1)))
                (< cl1 cl2)))))))))

(defun flycheck-error-level-< (err1 err2)
  "Determine whether ERR1 is less than ERR2 by error level.

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

(defun flycheck-assert-error-list-p (errors)
  "Assert that all items in ERRORS are of `flycheck-error' type.

Signal an error if any item in ERRORS is not a `flycheck-error'
object, as by `flycheck-error-p'.  Otherwise return ERRORS
again."
  (unless (listp errors)
    (signal 'wrong-type-argument (list 'listp errors)))
  (dolist (err errors)
    (unless (flycheck-error-p err)
      (signal 'wrong-type-argument (list 'flycheck-error-p err))))
  errors)


;;; Errors in the current buffer
(defvar-local flycheck-current-errors nil
  "A list of all errors and warnings in the current buffer.")

(defun flycheck-report-current-errors (errors)
  "Report ERRORS in the current buffer.

Add ERRORS to `flycheck-current-errors' and process each error
with `flycheck-process-error-functions'."
  ;; The frame type may have changed since the mode was enabled, e.g. a
  ;; buffer redisplayed on a TTY frame of the same daemon
  (flycheck--sync-margin)
  (setq flycheck-current-errors (append errors flycheck-current-errors))
  (overlay-recenter (point-max))
  (seq-do (lambda (err)
            (run-hook-with-args-until-success 'flycheck-process-error-functions
                                              err))
          (seq-sort-by #'flycheck-error-line #'< errors)))

(defun flycheck-clear-errors ()
  "Remove all error information from the current buffer."
  (setq flycheck-current-errors nil)
  (flycheck--project-forget-buffer)
  (flycheck-report-status 'not-checked))


;;; Project-wide diagnostics
;;
;; Besides the per-buffer `flycheck-current-errors', Flycheck keeps a
;; project-wide store of every error a check produces, including the
;; cross-file errors that `flycheck-relevant-errors' drops from the
;; buffer view (e.g. a `tsc' or `cargo check' run reporting errors across
;; a whole package).  The store aggregates those with the errors of every
;; open Flycheck buffer, so the error list can show a project at a glance
;; (see `flycheck-error-list-scope').

(defvar flycheck--project-error-store (make-hash-table :test 'eq)
  "Store of project-wide diagnostics.

Maps a source buffer to the list of `flycheck-error' objects its
last check produced, across every file rather than just the source
buffer.  A buffer's project is resolved lazily, when the errors are
aggregated (see `flycheck--project-errors'), so recording stays off
the check hot path.  Entries are retracted in `flycheck-clear-errors',
so a re-check, clear, or buffer teardown replaces the buffer's
contribution.")

(declare-function project-root "project" (project))

(defun flycheck--project-directory ()
  "Return a key identifying the current buffer's project.

Use Emacs' project (see `project-current') when a project is
found, so diagnostics from any file in the project aggregate
together; otherwise fall back to `default-directory', which
matches how a checker's working directory groups a multi-file
check.  The result is an expanded directory name."
  (file-name-as-directory
   (expand-file-name
    (or (and (require 'project nil 'noerror)
             (when-let* ((project (project-current nil)))
               (project-root project)))
        default-directory))))

(defun flycheck--project-storable-errors (errors)
  "Return the subset of ERRORS worth recording project-wide.

Drop errors without a line number or message: the buffer view
discards these too (see `flycheck-relevant-error-p') and the error
list cannot sort them."
  (seq-filter
   (lambda (err)
     (and (flycheck-error-line err)
          (let ((message (flycheck-error-message err)))
            (and message (not (string-empty-p message))))))
   errors))

(defun flycheck--project-record-errors (errors)
  "Add the storable subset of ERRORS to the current buffer's project.

Successive checkers of a check chain accumulate, mirroring
`flycheck-report-current-errors'.  The errors are not capped here:
the buffer's own errors arrive already flood-handled by
`flycheck--handle-excessive-errors', so nothing recorded exceeds
what the buffer itself shows for its file."
  (let ((buffer (current-buffer)))
    (puthash buffer
             (append (flycheck--project-storable-errors errors)
                     (gethash buffer flycheck--project-error-store))
             flycheck--project-error-store)))

(defun flycheck--project-forget-buffer (&optional buffer)
  "Drop BUFFER's contribution to the project store.

BUFFER defaults to the current buffer."
  (remhash (or buffer (current-buffer)) flycheck--project-error-store))

(defun flycheck--project-error-identity (err buffer)
  "Return a value uniquely identifying ERR contributed by BUFFER.

Two errors compare `equal' when they describe the same problem in
the same file -- e.g. one `cargo check' diagnostic reported once
per open crate file -- so duplicates collapse.  Errors without a
file name are distinguished by BUFFER, so identical diagnostics
from different unsaved buffers are not mistaken for duplicates."
  (list (or (flycheck-error-filename err) buffer)
        (flycheck-error-line err)
        (flycheck-error-column err)
        (flycheck-error-end-line err)
        (flycheck-error-end-column err)
        (flycheck-error-level err)
        (flycheck-error-message err)
        (flycheck-error-id err)
        (flycheck-error-checker err)))

(defun flycheck--project-errors (project-key)
  "Return the deduplicated diagnostics recorded for PROJECT-KEY.

Aggregate the errors every live buffer of the project contributed,
dropping duplicates (see `flycheck--project-error-identity').  Dead
buffers are pruned from the store on the way.

Cross-file errors reflect the last check that reported them: an
error a checker reported about another file stays until the buffer
that produced it is re-checked, since Flycheck has no way to know
the file changed without running a check."
  ;; OWNER maps an error identity to the first buffer that contributed it.
  ;; Duplicates within one buffer are kept (a checker may legitimately report
  ;; the same diagnostic twice), but the same diagnostic seen from several
  ;; buffers -- e.g. one `cargo check' error reported from each open crate
  ;; file -- collapses to a single entry.
  (let ((owner (make-hash-table :test 'equal))
        (dead nil)
        (result nil))
    (when project-key
      (maphash
       (lambda (buffer errors)
         (if (not (buffer-live-p buffer))
             (push buffer dead)
           ;; Resolve the project lazily, only now, to keep `project-current'
           ;; off the check hot path; a misbehaving project backend must not
           ;; abort the whole aggregation, so guard against it.
           (when (equal project-key
                        (ignore-errors
                          (with-current-buffer buffer
                            (flycheck--project-directory))))
             (dolist (err errors)
               (let* ((identity (flycheck--project-error-identity err buffer))
                      (seen-in (gethash identity owner)))
                 (when (or (null seen-in) (eq seen-in buffer))
                   (unless seen-in (puthash identity buffer owner))
                   (push err result)))))))
       flycheck--project-error-store)
      (dolist (buffer dead)
        (remhash buffer flycheck--project-error-store)))
    (nreverse result)))

(defun flycheck-fill-and-expand-error-file-names (errors directory)
  "Fill and expand file names in ERRORS relative to DIRECTORY.

Expand all file names of ERRORS against DIRECTORY.  If the file
name of an error is nil fill in the result of function
`buffer-file-name' in the current buffer.

Return ERRORS, modified in-place."
  (seq-do (lambda (err)
            (setf (flycheck-error-filename err)
                  (if-let* ((filename (flycheck-error-filename err)))
                      (flycheck--expand-file-name filename directory)
                    (buffer-file-name))))
          errors)
  errors)

(defun flycheck-relevant-error-other-file-p (err)
  "Determine whether ERR is a relevant error for another file."
  (let ((file-name (flycheck-error-filename err)))
    (and file-name
         flycheck-relevant-error-other-file-show
         (or (null buffer-file-name)
             (not (flycheck-same-files-p buffer-file-name file-name)))
         (<= (flycheck-error-level-severity
              flycheck-relevant-error-other-file-minimum-level)
             (flycheck-error-level-severity (flycheck-error-level err))))))

(defun flycheck-relevant-error-p (err)
  "Determine whether ERR is relevant for the current buffer.

Return t if ERR may be shown for the current buffer, or nil
otherwise."
  (flycheck-error-with-buffer err
    (let ((file-name (flycheck-error-filename err))
          (message (flycheck-error-message err)))
      (and
       (or
        ;; Neither the error nor buffer have a file name
        (and (not file-name) (not buffer-file-name))
        ;; Both have files, and they match
        (and buffer-file-name file-name
             (flycheck-same-files-p file-name buffer-file-name))
        ;; This is a significant error from another file
        (flycheck-relevant-error-other-file-p err))
       message
       (not (string-empty-p message))
       ;; Errors without line numbers are discarded.  If a linter
       ;; reports relevant errors without line numbers, use
       ;; `flycheck-fill-empty-line-numbers' as the checker's
       ;; `:error-filter' to set them to line 0.
       (flycheck-error-line err)))))

(defun flycheck-relevant-errors (errors)
  "Filter the relevant errors from ERRORS.

Return a list of all errors that are relevant for their
corresponding buffer."
  (seq-filter #'flycheck-relevant-error-p errors))

(defun flycheck-related-errors (err &optional error-set)
  "Get all the errors that are in the same group as ERR.

Return a list of all errors (from ERROR-SET) that have the same
`flycheck-error-group' as ERR, including ERR itself.

If ERROR-SET is nil, `flycheck-current-errors' is used instead."
  (let ((group (flycheck-error-group err))
        (checker (flycheck-error-checker err)))
    (if group
        (seq-filter (lambda (e)
                      (and (eq (flycheck-error-checker e) checker)
                           (eq (flycheck-error-group e) group)))
                    (or error-set flycheck-current-errors))
      (list err))))


;;; Status reporting for the current buffer
(defvar-local flycheck-last-status-change 'not-checked
  "The last status change in the current buffer.")

(defun flycheck-report-failed-syntax-check (&optional status)
  "Report a failed Flycheck syntax check with STATUS.

STATUS is a status symbol for `flycheck-report-status',
defaulting to `errored'.

Clear Flycheck state, run `flycheck-syntax-check-failed-hook' and
report an error STATUS."
  (flycheck-clear)
  (setq flycheck-current-syntax-check nil)
  (run-hooks 'flycheck-syntax-check-failed-hook)
  (flycheck-report-status (or status 'errored)))

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

(defun flycheck-mode-line-list-errors (&optional event)
  "Pop up the error list for the buffer of EVENT's window.

Without a mouse EVENT, e.g. when invoked from the keyboard, pop
up the error list for the current buffer."
  (interactive (list last-nonmenu-event))
  (let ((window (and (eventp event)
                     (posn-window (event-start event)))))
    ;; `posn-window' may return a frame for frame-relative positions
    (with-selected-window (if (windowp window) window (selected-window))
      (flycheck-list-errors))))

(defvar flycheck-mode-line-counts-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'flycheck-mode-line-list-errors)
    ;; Some setups render the mode-line construct in the header line
    (define-key map [header-line mouse-1] #'flycheck-mode-line-list-errors)
    map)
  "Keymap for the error counts in the mode line.")

(defun flycheck-mode-line-verify-setup (&optional event)
  "Show the setup of the buffer of EVENT's window.

Without a mouse EVENT, e.g. when invoked from the keyboard, show the
setup of the current buffer."
  (interactive (list last-nonmenu-event))
  (let ((window (and (eventp event)
                     (posn-window (event-start event)))))
    (with-selected-window (if (windowp window) window (selected-window))
      (flycheck-verify-setup))))

(defvar flycheck-mode-line-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'flycheck-mode-line-verify-setup)
    (define-key map [header-line mouse-1] #'flycheck-mode-line-verify-setup)
    map)
  "Keymap for the mode-line indicator of a status that ran no check.")

(defconst flycheck-mode-line-status-help
  '((not-checked . "Flycheck has not checked this buffer yet")
    (no-checker . "No syntax checker for this buffer")
    (running . "Flycheck is checking this buffer")
    (errored . "The syntax checker could not be run")
    (interrupted . "The syntax check was interrupted")
    (suspicious . "The syntax checker returned a result Flycheck did not \
understand"))
  "What each mode-line status means, for the indicator's tooltip.

Every status here shows no error counts, so its indicator is a single
opaque character.  `flycheck-mode-line-status-text' turns these into a
tooltip and a click that explains the buffer's setup.")

(defun flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let* ((current-status (or status flycheck-last-status-change))
         (indicator (pcase current-status
                      (`not-checked "")
                      (`no-checker "-")
                      (`running "*")
                      (`errored "!")
                      (`finished
                       (let-alist (flycheck-count-errors flycheck-current-errors)
                         (propertize
                          (concat
                           (if (or .error .warning .info)
                               (format ":%s|%s|%s" (or .error 0) (or .warning 0)
                                       (or .info 0))
                             flycheck-mode-line-success-indicator)
                           ;; Signal that some errors were suppressed over
                           ;; `flycheck-checker-error-threshold', even when
                           ;; the kept errors have no built-in level
                           (if (> flycheck--suppressed-error-count 0)
                               "+"
                             ""))
                          'local-map flycheck-mode-line-counts-map
                          'mouse-face 'mode-line-highlight
                          'help-echo
                          (concat
                           (when (> flycheck--suppressed-error-count 0)
                             (format "%d more errors not shown\
 (see flycheck-checker-error-threshold)\n"
                                     flycheck--suppressed-error-count))
                           "mouse-1: list errors"))))
                      (`interrupted ".")
                      (`suspicious "?")))
         ;; Every other status renders as one opaque character with no
         ;; counts behind it, which is exactly when a user needs to be
         ;; told what it means and where to look next.
         (indicator
          (if (or (eq current-status 'finished) (string-empty-p indicator))
              indicator
            (propertize
             indicator
             'local-map flycheck-mode-line-status-map
             'mouse-face 'mode-line-highlight
             'help-echo
             (concat (alist-get current-status flycheck-mode-line-status-help
                                (symbol-name current-status))
                     "\nmouse-1: check this buffer's setup"))))
         (face (when flycheck-mode-line-color
                 (pcase current-status
                   (`errored 'error)
                   (`finished
                    (let-alist (flycheck-count-errors flycheck-current-errors)
                      (if (or .error .warning) 'error 'success))))))
         (text (format " %s%s" flycheck-mode-line-prefix indicator)))
    (when face
      (setq text (propertize text 'face face)))
    text))


;;; Error levels
(defun flycheck-make-margin-spec (margin-str face)
  "Make a display spec to indicate errors in the margins.

Returns MARGIN-STR with FACE applied."
  (propertize margin-str 'face `(,face default)))

(defconst flycheck-default-margin-str "»"
  "String used to indicate errors in the margins.")

(defconst flycheck-fixable-margin-str "●"
  "String marking a line whose error carries a fix, in the margins.")

(defconst flycheck-default-margin-continuation-str "⋮"
  "String used to indicate continuation lines in the margins.")

;;;###autoload
(defun flycheck-define-error-level (level &rest properties)
  "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'."
  (declare (indent 1))
  (setf (get level 'flycheck-error-level) t)
  (setf (get level 'flycheck-error-severity)
        (or (plist-get properties :severity) 0))
  (setf (get level 'flycheck-compilation-level)
        (plist-get properties :compilation-level))
  (setf (get level 'flycheck-overlay-category)
        (plist-get properties :overlay-category))
  (setf (get level 'flycheck-fringe-bitmaps)
        (let ((bitmap (plist-get properties :fringe-bitmap)))
          (if (consp bitmap) bitmap (cons bitmap bitmap))))
  ;; Kept for compatibility
  (setf (get level 'flycheck-fringe-bitmap-double-arrow)
        (car (get level 'flycheck-fringe-bitmaps)))
  (setf (get level 'flycheck-fringe-face)
        (plist-get properties :fringe-face))
  (setf (get level 'flycheck-margin-spec)
        (or (plist-get properties :margin-spec)
            (flycheck-make-margin-spec
             flycheck-default-margin-str
             (or (get level 'flycheck-fringe-face) 'default))))
  (setf (get level 'flycheck-margin-continuation)
        (flycheck-make-margin-spec
         flycheck-default-margin-continuation-str
         (or (get level 'flycheck-fringe-face) 'default)))
  (setf (get level 'flycheck-error-list-face)
        (plist-get properties :error-list-face)))

(defun flycheck-error-level-p (level)
  "Determine whether LEVEL is a Flycheck error level."
  (get level 'flycheck-error-level))

(defun flycheck-error-level-severity (level)
  "Get the numeric severity of LEVEL."
  (or (get level 'flycheck-error-severity) 0))

(defun flycheck-error-level-compilation-level (level)
  "Get the compilation level for LEVEL."
  (get level 'flycheck-compilation-level))

(defun flycheck-error-level-overlay-category (level)
  "Get the overlay category for LEVEL."
  (get level 'flycheck-overlay-category))

(defun flycheck-error-level-margin-spec (level)
  "Get the margin spec for LEVEL."
  (get level 'flycheck-margin-spec))

(defun flycheck-error-level-margin-continuation-spec (level)
  "Get the margin continuation spec for LEVEL."
  (get level 'flycheck-margin-continuation))

(defun flycheck-error-level-fringe-bitmap (level &optional hi-res)
  "Get the fringe bitmap for LEVEL.

Optional argument HI-RES non-nil means that the returned bitmap
will be the high resolution version."
  (let ((bitmaps (get level 'flycheck-fringe-bitmaps)))
    (if hi-res (cdr bitmaps) (car bitmaps))))

(defun flycheck-error-level-fringe-face (level)
  "Get the fringe face for LEVEL."
  (get level 'flycheck-fringe-face))

(defun flycheck-error-level-error-list-face (level)
  "Get the error list face for LEVEL."
  (get level 'flycheck-error-list-face))

(defun flycheck-error-level-make-indicator (level side &optional continuation fixable)
  "Create the fringe or margin icon for LEVEL at SIDE.

Return a propertized string that shows an indicator according
to LEVEL and the given fringe or margin SIDE.

LEVEL is a Flycheck error level defined with
`flycheck-define-error-level', and SIDE is either `left-fringe',
`right-fringe', `left-margin', or `right-margin'.

CONTINUATION indicates which fringe bitmap or margin spec to use:
either the `:fringe-bitmap' and `:margin-spec' properties of
LEVEL when CONTINUATION is nil or omitted, or bitmaps and specs
indicating an error spanning more than one line.

FIXABLE non-nil marks a line whose error carries a fix: the
indicator uses the distinct `flycheck-fringe-bitmap-fixable' bitmap
or `flycheck-fixable-margin-str' string, kept in LEVEL's colour.
CONTINUATION takes precedence over FIXABLE.

Return a propertized string representing the fringe icon,
intended for use as `before-string' of an overlay to actually
show the indicator."
  (propertize
   "!" 'display
   (pcase side
     ((or `left-fringe `right-fringe)
      (let* ((fringe-width
              (pcase side
                (`left-fringe (car (window-fringes)))
                (`right-fringe (cadr (window-fringes)))))
             (high-res (>= fringe-width 16)))
        (list side
              (cond
               (continuation 'flycheck-fringe-bitmap-continuation)
               (fixable (if high-res 'flycheck-fringe-bitmap-fixable-hi-res
                          'flycheck-fringe-bitmap-fixable))
               (t (flycheck-error-level-fringe-bitmap level high-res)))
              (flycheck-error-level-fringe-face level))))
     ((or `left-margin `right-margin)
      `((margin ,side)
        ,(or (cond
              (continuation (flycheck-error-level-margin-continuation-spec level))
              (fixable (flycheck-make-margin-spec
                        flycheck-fixable-margin-str
                        (flycheck-error-level-fringe-face level)))
              (t (flycheck-error-level-margin-spec level)))
             "")))
     (_ (error "Invalid fringe side: %S" side)))))



;;; Built-in error levels
(defconst flycheck-fringe-bitmap-double-arrow
  [#b11011000
   #b01101100
   #b00110110
   #b00011011
   #b00110110
   #b01101100
   #b11011000]
  "Bitmaps used to indicate errors in the left fringes.")

(defconst flycheck-fringe-bitmap-double-left-arrow
  [#b00011011
   #b00110110
   #b01101100
   #b11011000
   #b01101100
   #b00110110
   #b00011011]
  "Bitmaps used to indicate errors in the right fringes.")

(defconst flycheck-fringe-bitmap-double-arrow-hi-res
  [#b1111001111000000
   #b0111100111100000
   #b0011110011110000
   #b0001111001111000
   #b0000111100111100
   #b0000011110011110
   #b0000011110011110
   #b0000111100111100
   #b0001111001111000
   #b0011110011110000
   #b0111100111100000
   #b1111001111000000]
  "High-resolution bitmap used to indicate errors in the left fringes.")

(defconst flycheck-fringe-bitmap-double-left-arrow-hi-res
  [#b0000001111001111
   #b0000011110011110
   #b0000111100111100
   #b0001111001111000
   #b0011110011110000
   #b0111100111100000
   #b0111100111100000
   #b0011110011110000
   #b0001111001111000
   #b0000111100111100
   #b0000011110011110
   #b0000001111001111]
  "High-resolution bitmap used to indicate errors in the right fringes.")

(defconst flycheck-fringe-bitmap-continuation
  [#b1000000010000000
   #b0010000000100000
   #b0000100000001000
   #b0000001000000010]
  "Bitmap used to indicate continuation lines in the fringes.")

(defconst flycheck-fringe-bitmap-fixable
  [#b00111100
   #b01111110
   #b11111111
   #b11111111
   #b11111111
   #b01111110
   #b00111100]
  "Bitmap marking a line whose error carries a fix, in the fringes.")

(defconst flycheck-fringe-bitmap-fixable-hi-res
  [#b0000001111000000
   #b0000111111110000
   #b0001111111111000
   #b0011111111111100
   #b0111111111111110
   #b0111111111111110
   #b1111111111111111
   #b1111111111111111
   #b0111111111111110
   #b0111111111111110
   #b0011111111111100
   #b0001111111111000
   #b0000111111110000
   #b0000001111000000]
  "High-resolution bitmap marking a fixable line in the fringes.")

(when (fboundp 'define-fringe-bitmap) ;; #ifdef HAVE_WINDOW_SYSTEM
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-arrow
    flycheck-fringe-bitmap-double-arrow)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-arrow-hi-res
    flycheck-fringe-bitmap-double-arrow-hi-res
    nil 16)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-left-arrow
    flycheck-fringe-bitmap-double-left-arrow)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-left-arrow-hi-res
    flycheck-fringe-bitmap-double-left-arrow-hi-res
    nil 16)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-continuation
    flycheck-fringe-bitmap-continuation
    nil 16 '(top repeat))
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-fixable
    flycheck-fringe-bitmap-fixable)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-fixable-hi-res
    flycheck-fringe-bitmap-fixable-hi-res
    nil 16))

(defun flycheck-redefine-standard-error-levels
    (&optional margin-str fringe-bitmap)
  "Redefine Flycheck's standard error levels.

This is useful to change the character drawn in the
margins (MARGIN-STR, a string) or the bitmap drawn in the
fringes (FRINGE-BITMAP, a fringe bitmap symbol or a cons of such
symbols, as in `flycheck-define-error-level')."
  (unless margin-str
    (setq margin-str flycheck-default-margin-str))

  (unless fringe-bitmap
    (setq fringe-bitmap
          (cons 'flycheck-fringe-bitmap-double-arrow
                'flycheck-fringe-bitmap-double-arrow-hi-res)))

  (setf (get 'flycheck-error-overlay 'face) 'flycheck-error)
  (setf (get 'flycheck-error-overlay 'priority) 110)

  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-error)
    :fringe-bitmap fringe-bitmap
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)

  (setf (get 'flycheck-warning-overlay 'face) 'flycheck-warning)
  (setf (get 'flycheck-warning-overlay 'priority) 100)

  (flycheck-define-error-level 'warning
    :severity 10
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-warning)
    :fringe-bitmap fringe-bitmap
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)

  (setf (get 'flycheck-info-overlay 'face) 'flycheck-info)
  (setf (get 'flycheck-info-overlay 'priority) 90)

  (flycheck-define-error-level 'info
    :severity -10
    :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-info)
    :fringe-bitmap fringe-bitmap
    :fringe-face 'flycheck-fringe-info
    :error-list-face 'flycheck-error-list-info))

(flycheck-redefine-standard-error-levels)


;;; Error filtering
(defun flycheck-filter-errors (errors checker)
  "Filter ERRORS from CHECKER.

Apply the error filter of CHECKER to ERRORS and return the
result.  If CHECKER has no error filter, fall back to
`flycheck-sanitize-errors'."
  (let ((filter (or (flycheck-checker-get checker 'error-filter)
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
            (id (flycheck-error-id err)))
        (when message
          (setq message (string-trim message))
          (setf (flycheck-error-message err)
                (if (string-empty-p message) nil message)))
        (when (and id (string-empty-p id))
          (setf (flycheck-error-id err) nil))
        (when (eq (flycheck-error-column err) 0)
          (setf (flycheck-error-column err) nil))
        (when (eq (flycheck-error-end-column err) 0)
          (setf (flycheck-error-end-column err) nil)))))
  errors)

(defun flycheck-remove-error-file-names (file-name errors)
  "Remove matching FILE-NAME from ERRORS.

Use as `:error-filter' for syntax checkers that output faulty
filenames.  Flycheck will later fill in the buffer file name.

Return ERRORS."
  (seq-do (lambda (err)
            (when (and (flycheck-error-filename err)
                       (string= (flycheck-error-filename err) file-name))
              (setf (flycheck-error-filename err) nil)))
          errors)
  errors)

(defun flycheck-increment-error-columns (errors &optional offset)
  "Increment all columns of ERRORS by OFFSET (default: 1).

  Use this as `:error-filter' if a syntax checker outputs 0-based
  columns."
  (setq offset (or offset 1)) ;; Emacs bug #31715
  (seq-do (lambda (err)
            (when (flycheck-error-column err)
              (cl-incf (flycheck-error-column err) offset))
            (when (flycheck-error-end-column err)
              (cl-incf (flycheck-error-end-column err) offset)))
          errors)
  errors)

(defun flycheck-collapse-error-message-whitespace (errors)
  "Collapse whitespace in all messages of ERRORS.

Return ERRORS."
  (dolist (err errors)
    (when-let* ((message (flycheck-error-message err)))
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
    (when-let* ((message (flycheck-error-message err)))
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
            ;; indentation offset.  Otherwise keep the line intact, as we might
            ;; lose valuable information
            (when (>= (- (point) (line-beginning-position)) indent-offset)
              (delete-char (- indent-offset)))
            (forward-line 1)))
        (delete-trailing-whitespace (point-min) (point-max))
        (setf (flycheck-error-message err)
              (buffer-substring-no-properties (point-min) (point-max))))))
  errors)

(defun flycheck-fold-include-levels (errors sentinel-message)
  "Fold levels of ERRORS from included files.

ERRORS is a list of `flycheck-error' objects.  SENTINEL-MESSAGE
is a regular expression matched against the error message to
determine whether the error denotes errors from an included
file.  Alternatively, it is a function that is given an error and
shall return non-nil, if the error denotes errors from an
included file."
  (unless (or (stringp sentinel-message) (functionp sentinel-message))
    (error "Sentinel must be string or function: %S" sentinel-message))
  (let ((sentinel (if (functionp sentinel-message)
                      sentinel-message
                    (lambda (err)
                      (string-match-p sentinel-message
                                      (flycheck-error-message err)))))
        (remaining-errors errors))
    (while remaining-errors
      (let* ((current-error (pop remaining-errors)))
        (when (funcall sentinel current-error)
          ;; We found an error denoting errors in the included file:
          ;; 1. process all subsequent errors until faulty include file is found
          ;; 2. process again all subsequent errors until an error has the
          ;;    current file name again
          ;; 3. find the most severe error level
          (let ((current-filename (flycheck-error-filename current-error))
                (current-level nil)
                (faulty-include-filename nil)
                (filename nil)
                (done (null remaining-errors)))

            (while (not done)
              (setq filename (flycheck-error-filename (car remaining-errors)))
              (unless faulty-include-filename
                (unless (string= filename current-filename)
                  (setq faulty-include-filename filename)))

              (let* ((error-in-include (pop remaining-errors))
                     (in-include-level (flycheck-error-level error-in-include)))
                (unless (funcall sentinel error-in-include)
                  ;; Ignore nested "included file" errors, we are only
                  ;; interested in real errors because these define our level
                  (when (or (not current-level)
                            (> (flycheck-error-level-severity in-include-level)
                               (flycheck-error-level-severity current-level)))
                    (setq current-level in-include-level))))

              (setq done (or (null remaining-errors)
                             (and faulty-include-filename
                                  (string= filename current-filename)))))

            (setf (flycheck-error-level current-error) current-level
                  (flycheck-error-message current-error)
                  (format "In include %s" faulty-include-filename))))))
    errors))

(defun flycheck-dequalify-error-ids (errors)
  "De-qualify error ids in ERRORS.

Remove all qualifications from error ids in ERRORS, by stripping
all leading dotted components from error IDs.  For instance, if
the error ID is com.foo.E100, replace it with E100.

This error filter is mainly useful to simplify error IDs obtained
from parsing Checkstyle XML, which frequently has very verbose
IDs, that include the name of the tool."
  (seq-do (lambda (err)
            (let ((id (flycheck-error-id err)))
              (when id
                (setf (flycheck-error-id err)
                      (replace-regexp-in-string
                       (rx string-start
                           (group
                            (optional (zero-or-more not-newline) "."))
                           (one-or-more (not (any ".")))
                           string-end)
                       "" id 'fixedcase 'literal 1)))))
          errors)
  errors)

(defun flycheck-remove-error-ids (errors)
  "Remove all error ids from ERRORS."
  (seq-do (lambda (err) (setf (flycheck-error-id err) nil)) errors)
  errors)

(defun flycheck-fill-empty-line-numbers (errors)
  "Set ERRORS without lines to line 0.

Use as `:error-filter' for syntax checkers that output errors
without line numbers.

Return ERRORS."
  (seq-do (lambda (err)
            (unless (flycheck-error-line err)
              (setf (flycheck-error-line err) 0)))
          errors)
  errors)


;;; Error analysis
(defun flycheck-count-errors (errors)
  "Count the number of ERRORS, grouped by level.

Return an alist, where each ITEM is a cons cell whose `car' is an
error level, and whose `cdr' is the number of errors of that
level."
  (let (counts-by-level)
    (dolist (err errors)
      (let* ((level (flycheck-error-level err))
             (item (assq level counts-by-level)))
        (if item
            (cl-incf (cdr item))
          (push (cons level 1) counts-by-level))))
    counts-by-level))

(defun flycheck-has-max-errors-p (errors level)
  "Check if there is no error in ERRORS more severe than LEVEL."
  (let ((severity (flycheck-error-level-severity level)))
    (seq-every-p (lambda (e) (<= (flycheck-error-level-severity
                                  (flycheck-error-level e))
                                 severity))
                 errors)))

(defun flycheck-has-max-current-errors-p (level)
  "Check if there is no current error more severe than LEVEL."
  (flycheck-has-max-errors-p flycheck-current-errors level))

(defun flycheck-has-errors-p (errors level)
  "Determine if there are any ERRORS with LEVEL."
  (seq-some (lambda (e) (eq (flycheck-error-level e) level)) errors))

(defun flycheck-has-current-errors-p (&optional level)
  "Determine if the current buffer has errors with LEVEL.

If LEVEL is omitted, check if the current buffer has any errors at all."
  (if level
      (flycheck-has-errors-p flycheck-current-errors level)
    (and flycheck-current-errors t)))


;;; Error overlays in the current buffer
(defvar-local flycheck--last-overlay-index 0
  "Last index given to a Flycheck overlay.

These indices are used to preserve error order (Emacs doesn't
preserve overlay order when calling `overlays-at').")

(defun flycheck--next-overlay-index ()
  "Compute the index to assign to a new Flycheck overlay."
  (cl-incf flycheck--last-overlay-index))

(defun flycheck--highlighting-style (err)
  "Determine the highlighting style to apply to ERR.

Styles are documented in `flycheck-highlighting-style'; this
function resolves `conditional' style specifications."
  (let* ((style flycheck-highlighting-style)
         (first-line (flycheck-error-line err))
         (end-line (or (flycheck-error-end-line err) first-line))
         (nlines (- end-line first-line)))
    (while (eq (car-safe style) 'conditional)
      (pcase-let ((`(,threshold ,s1 ,s2) (cdr style)))
        (setq style (if (< nlines threshold) s1 s2))))
    (pcase style
      (`(delimiters ,before ,after)
       (when (characterp before)
         (setq before (flycheck--make-highlighting-delimiter before)))
       (when (characterp after)
         (setq after (flycheck--make-highlighting-delimiter after)))
       (setq style `(delimiters ,before ,after))))
    style))

(defun flycheck--setup-highlighting (err overlay)
  "Apply properties to OVERLAY to highlight ERR."
  (let ((level (flycheck-error-level err)))
    (unless flycheck-highlighting-mode
      ;; Erase the highlighting from the overlay if requested by the user
      (setf (overlay-get overlay 'face) nil))
    (when-let* ((side (flycheck--resolve-indication-mode)))
      (let ((fixable (and flycheck-fixable-indicator
                          (flycheck-error-known-fix-p err)
                          (flycheck--error-fix-buffer err)
                          t)))
        (setf (overlay-get overlay 'before-string)
              (flycheck-error-level-make-indicator level side nil fixable)))
      (setf (overlay-get overlay 'wrap-prefix)
            (flycheck-error-level-make-indicator level side t))
      ;; Preserve existing text-property prefixes so the overlay doesn't
      ;; clobber indentation set by other modes.
      ;;
      ;; line-prefix: copy the text property onto the overlay unchanged
      ;; (e.g. from org-indent-mode).
      ;;
      ;; wrap-prefix: compose the flycheck fringe indicator with the
      ;; existing value (e.g. from visual-wrap-prefix-mode).  The fringe
      ;; indicator uses a `display' property for `!' that directly
      ;; renders in the fringe without producing any character in the
      ;; text area.  This effectively-zero-width character is composed
      ;; by concatenation with the preexisting wrap prefix.
      ;;
      ;; Per the Elisp manual ("Properties with Special Meanings"),
      ;; `wrap-prefix' may be a string, an image, or a stretch spec (`:width' or
      ;; `:align-to').  When the preexisting value is a string (e.g. a repeated
      ;; comment prefix like "% "), concatenate it directly; otherwise wrap it
      ;; in a propertized character via its `display' property so it can be
      ;; concatenated.
      ;;
      ;; Without this, an error overlay on the first character of a
      ;; soft-wrapped visual continuation line replaces the indentation
      ;; prefix with the fringe-only indicator, causing the line to
      ;; jump to column 0.
      (when (buffer-live-p (overlay-buffer overlay))
        (save-restriction
          (widen)
          (let* ((pos (overlay-start overlay))
                 (existing-lp (get-text-property pos 'line-prefix))
                 (existing-wp (get-text-property pos 'wrap-prefix)))
            (when existing-lp
              (setf (overlay-get overlay 'line-prefix) existing-lp))
            (when existing-wp
              (setf (overlay-get overlay 'wrap-prefix)
                    (concat (overlay-get overlay 'wrap-prefix)
                            (if (stringp existing-wp)
                                existing-wp
                              (propertize " " 'display existing-wp)))))))))
    (pcase (flycheck--highlighting-style err)
      ((or `nil (guard (null flycheck-highlighting-mode)))
       ;; Erase the highlighting
       (setf (overlay-get overlay 'face) nil))
      (`level-face)
      (`(delimiters ,before ,after)
       ;; Replace the highlighting with delimiters
       (let* ((fringe-face (flycheck-error-level-fringe-face level))
              (delim-face `(flycheck-error-delimiter ,fringe-face)))
         (setf (overlay-get overlay 'face) 'flycheck-delimited-error)
         (setf (overlay-get overlay 'before-string)
               (concat (propertize before 'face delim-face)
                       (or (overlay-get overlay 'before-string) "")))
         (setf (overlay-get overlay 'after-string)
               (propertize after 'face delim-face))))
      (other (error "Unsupported highlighting style: %S" other)))))

(defun flycheck-add-overlay (err)
  "Add overlay for ERR.

Return the created overlay."
  ;; We must have a proper error region for the sake of fringe indication,
  ;; error display and error navigation, even if the highlighting is disabled.
  ;; We erase the highlighting later on in this case
  (pcase-let* ((`(,beg . ,end)
                (if (flycheck-relevant-error-other-file-p err)
                    ;; Display overlays for other-file errors on the first line
                    (cons (point-min)
                          (save-excursion (goto-char (point-min))
                                          (line-end-position)))
                  (flycheck-error-region-for-mode
                   err (or flycheck-highlighting-mode 'lines))))
               (overlay (make-overlay beg end))
               (level (flycheck-error-level err))
               (category (flycheck-error-level-overlay-category level))
               (index (flycheck--next-overlay-index)))
    (unless (flycheck-error-level-p level)
      (error "Undefined error level: %S" level))
    (setf (overlay-get overlay 'flycheck-error-index) index)
    (setf (overlay-get overlay 'flycheck-overlay) t)
    (setf (overlay-get overlay 'flycheck-error) err)
    (setf (overlay-get overlay 'category) category)
    ;; A tag says something about the code, not about how bad the problem
    ;; is, so it adds a face rather than replacing the level's.  The
    ;; category supplies that one, and an explicit `face' overrides the
    ;; category, so name it again here.
    (when-let* ((faces (flycheck--error-tag-faces err)))
      (setf (overlay-get overlay 'face)
            (append faces (list (get category 'face)))))
    (setf (overlay-get overlay 'help-echo) #'flycheck-help-echo)
    (flycheck--setup-highlighting err overlay)
    overlay))

(defconst flycheck--tag-faces
  '((unnecessary . flycheck-unnecessary)
    (deprecated . flycheck-deprecated))
  "Map of `flycheck-error' tags to the faces that render them.")

(defun flycheck--error-tag-faces (err)
  "Return the faces rendering ERR's tags, or nil when it has none."
  (delq nil (mapcar (lambda (tag) (alist-get tag flycheck--tag-faces))
                    (flycheck-error-tags err))))

(defun flycheck-help-echo (_window object pos)
  "Construct a tooltip message.

Most of the actual work is done by calling
`flycheck-help-echo-function' with the appropriate list of
errors.  Arguments WINDOW, OBJECT and POS are as described in
info node `(elisp)Special properties', as this function is
intended to be used as the \\='help-echo property of flycheck error
overlays."
  (when-let* ((buf (cond ((bufferp object) object)
                        ((overlayp object) (overlay-buffer object)))))
    (with-current-buffer buf
      (when-let* ((fn flycheck-help-echo-function)
                  (errs (flycheck-overlay-errors-at pos)))
        (propertize (funcall fn errs) 'help-echo-inhibit-substitution t)))))

(defun flycheck-help-echo-all-error-messages (errs)
  "Concatenate error messages, ids and related locations from ERRS."
  (let* ((errs (delq nil errs)) ;; FIXME why would errors be nil here?
         ;; Prepend a snippet of the offending text only when disambiguating
         ;; several errors, matching the previous single-vs-multiple behavior.
         (include-snippet (and (cdr errs) 'include-snippet)))
    (mapconcat
     (lambda (err)
       (concat
        (flycheck-error-format-message-and-id err include-snippet)
        (when-let* ((rel (flycheck-error-format-relations err)))
          (concat "\n" rel))))
     errs "\n")))

(defun flycheck-filter-overlays (overlays)
  "Get all Flycheck overlays from OVERLAYS, in original order."
  ;; The order of errors returned from overlays is not stable, so we sort
  ;; them again using the internal index to guarantee errors are always
  ;; displayed in the same order.
  (seq-sort-by
   (lambda (o) (overlay-get o 'flycheck-error-index))
   #'<
   (seq-filter (lambda (o) (overlay-get o 'flycheck-overlay)) overlays)))

(defun flycheck-overlays-at (pos)
  "Get all Flycheck overlays at POS."
  (flycheck-filter-overlays (overlays-at pos)))

(defun flycheck-overlays-in (beg end)
  "Get all Flycheck overlays between BEG and END."
  (flycheck-filter-overlays (overlays-in beg end)))

(defun flycheck-overlay-errors-at (pos)
  "Return a list of all flycheck errors overlaid at POS."
  (mapcar (lambda (o) (overlay-get o 'flycheck-error))
           (flycheck-overlays-at pos)))

(defun flycheck-overlay-errors-in (beg end)
  "Return a list of all flycheck errors overlaid between BEG and END."
  (mapcar (lambda (o) (overlay-get o 'flycheck-error))
           (flycheck-overlays-in beg end)))

(defvar-local flycheck-overlays-to-delete nil
  "Overlays marked for deletion after all syntax checks completed.")
(put 'flycheck-overlays-to-delete 'permanent-local t)

(defun flycheck-delete-all-overlays ()
  "Remove all flycheck overlays in the current buffer."
  (overlay-recenter (point-max))
  (flycheck-delete-marked-overlays)
  (setq flycheck--last-overlay-index 0)
  (save-restriction
    (widen)
    (seq-do #'delete-overlay (flycheck-overlays-in (point-min) (point-max)))))

(defun flycheck-mark-all-overlays-for-deletion ()
  "Mark all current overlays for deletion."
  (setq flycheck-overlays-to-delete
        (append (flycheck-overlays-in (point-min) (point-max))
                flycheck-overlays-to-delete)))

(defun flycheck-delete-marked-overlays ()
  "Delete all overlays marked for deletion."
  (overlay-recenter (point-max))
  (seq-do #'delete-overlay flycheck-overlays-to-delete)
  (setq flycheck-overlays-to-delete nil))


;;; Error navigation in the current buffer
(defun flycheck-error-level-interesting-at-pos-p (pos)
  "Check if error severity at POS passes `flycheck-error-level-interesting-p'."
  (flycheck-error-level-interesting-p (get-char-property pos 'flycheck-error)))

(defun flycheck-error-level-interesting-p (err)
  "Check if ERR severity is >= `flycheck-navigation-minimum-level'."
  (when (flycheck-error-p err)
    (if-let* ((min-level flycheck-navigation-minimum-level))
        (<= (flycheck-error-level-severity min-level)
            (flycheck-error-level-severity (flycheck-error-level err)))
      t)))

(defun flycheck-next-error-pos (n &optional reset)
  "Get the position of the N-th next error.

With negative N, get the position of the (-N)-th previous error
instead.  With non-nil RESET, search from `point-min', otherwise
search from the current point.

Return the position of the next or previous error, or nil if
there is none.  If N is zero, return `point', or `point-min' if
RESET is non-nil."
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

N is the number of errors to advance by, where a negative N
advances backwards.  With non-nil RESET, advance from the
beginning of the buffer, otherwise advance from the current
position.

Intended for use with `next-error-function'."
  (if-let* ((pos (flycheck-next-error-pos n reset))
            (err (get-char-property pos 'flycheck-error)))
      (flycheck-jump-to-error err)
    (user-error "No more Flycheck errors")))

(defun flycheck-next-error (&optional n reset)
  "Visit the N-th error from the current point.

N is the number of errors to advance by, where a negative N
advances backwards.  With non-nil RESET, advance from the
beginning of the buffer, otherwise advance from the current
position."
  (interactive "P")
  (when (consp n)
    ;; Universal prefix argument means reset
    (setq reset t n nil))
  (flycheck-next-error-function n reset)
  (flycheck-display-error-at-point))

(defun flycheck-previous-error (&optional n)
  "Visit the N-th previous error.

If given, N specifies the number of errors to move backwards by.
If N is negative, move forwards instead."
  (interactive "P")
  (flycheck-next-error (- (or n 1))))

(defun flycheck-first-error (&optional n)
  "Visit the N-th error from beginning of the buffer.

If given, N specifies the number of errors to move forward from
the beginning of the buffer."
  (interactive "P")
  (flycheck-next-error n 'reset))


;;; Listing errors in buffers
(defconst flycheck-error-list-buffer "*Flycheck errors*"
  "The name of the buffer to show error lists.")

(defvar-local flycheck-error-list--checker-filter nil
  "When non-nil, show only errors from this syntax checker.")

(defvar-local flycheck-error-list--message-filter nil
  "When non-nil, show only errors matching this regexp.

The regexp is matched against the error message and the error
ID.")

(defmacro flycheck-error-list-with-buffer (&rest body)
  "Evaluate BODY in flycheck-error-list-buffer, if it exists."
  (declare (indent 0) (debug t))
  `(when (get-buffer flycheck-error-list-buffer)
     (with-current-buffer flycheck-error-list-buffer
       ,@body)))

(defvar flycheck-error-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'flycheck-error-list-set-filter)
    (define-key map (kbd "c") #'flycheck-error-list-set-checker-filter)
    (define-key map (kbd "/") #'flycheck-error-list-set-message-filter)
    (define-key map (kbd "F") #'flycheck-error-list-reset-filter)
    (define-key map (kbd "n") #'flycheck-error-list-next-error)
    (define-key map (kbd "p") #'flycheck-error-list-previous-error)
    (define-key map (kbd "g") #'flycheck-error-list-check-source)
    (define-key map (kbd "P") #'flycheck-error-list-toggle-scope)
    (define-key map (kbd "M-1") #'flycheck-error-list-group-by-none)
    (define-key map (kbd "M-2") #'flycheck-error-list-group-by-file)
    (define-key map (kbd "M-3") #'flycheck-error-list-group-by-checker)
    (define-key map (kbd "M-4") #'flycheck-error-list-group-by-level)
    (define-key map (kbd "TAB") #'flycheck-error-list-toggle-group-at-point)
    (define-key map (kbd "e") #'flycheck-error-list-explain-error)
    (define-key map (kbd "j") #'flycheck-error-list-visit-related-location)
    (define-key map (kbd "x") #'flycheck-error-list-apply-fix)
    (define-key map (kbd "X") #'flycheck-error-list-fix-all)
    (define-key map (kbd "RET") #'flycheck-error-list-goto-error)
    map)
  "The keymap of `flycheck-error-list-mode'.")

(defun flycheck-error-list-make-last-column (message checker)
  "Compute contents of the last error list cell.

MESSAGE and CHECKER are displayed in a single column to allow the
message to stretch arbitrarily far."
  (let ((checker-name (propertize (symbol-name checker)
                                  'face 'flycheck-error-list-checker-name))
        (message (propertize message
                             'face 'flycheck-error-list-error-message)))
    (format "%s (%s)" message checker-name)))

(defconst flycheck-error-list-format
  `[("File" 12)
    ("Line" 5 flycheck-error-list-entry-< :right-align t)
    ("Col" 3 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 6 t)
    (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]
  "Default table format for the error list.

The File and ID columns are resized to fit the errors being
displayed; see `flycheck-error-list--update-format'.")

(defconst flycheck-error-list-padding 1
  "Padding used in error list.")

(defun flycheck--error-list-compute-msg-offset (format)
  "Compute the space before the message column in FORMAT."
  (seq-reduce
   (lambda (offset fmt)
     (pcase-let* ((`(,_ ,width ,_ . ,props) fmt)
                  (padding (or (plist-get props :pad-right) 1)))
       (+ offset width padding)))
   (seq-subseq format 0 -1)
   flycheck-error-list-padding))

(defvar-local flycheck--error-list-msg-offset
    (flycheck--error-list-compute-msg-offset flycheck-error-list-format)
  "Amount of space to use in `flycheck-flush-multiline-message'.")

(defun flycheck-error-list--column-widths (errors)
  "Compute the File and ID column widths for ERRORS.

Return a cons cell (FILE-WIDTH . ID-WIDTH)."
  (let ((file-width 4) (id-width 2))
    (dolist (err errors)
      (when-let* ((file (flycheck-error-filename err)))
        (setq file-width (max file-width
                              (length (file-name-nondirectory file)))))
      (when-let* ((id (flycheck-error-id err)))
        (setq id-width (max id-width (length (format "%s" id))))))
    (cons (min file-width 40) (min id-width 24))))

(defun flycheck-error-list--set-column-width (format name width)
  "Set the width of the column NAME in FORMAT to WIDTH.

FORMAT is a tabulated list format vector; the other column
properties are left unchanged.  Unknown column names are
ignored."
  (when-let* ((index (seq-position format name
                                   (lambda (column name)
                                     (equal (car column) name)))))
    (setf (aref format index)
          (cons name (cons width (cddr (aref format index)))))))

(defun flycheck-error-list--update-format ()
  "Fit the File and ID column widths to the displayed errors."
  (pcase-let ((`(,file-width . ,id-width)
               (flycheck-error-list--column-widths
                (flycheck-error-list-apply-filter
                 (flycheck-error-list-current-errors)))))
    (let ((format (copy-sequence flycheck-error-list-format)))
      (flycheck-error-list--set-column-width format "File" file-width)
      (flycheck-error-list--set-column-width format "ID" id-width)
      (setq tabulated-list-format format))
    (setq flycheck--error-list-msg-offset
          (flycheck--error-list-compute-msg-offset tabulated-list-format))
    (tabulated-list-init-header)))

(define-derived-mode flycheck-error-list-mode tabulated-list-mode
  "Flycheck errors"
  "Major mode for listing Flycheck errors.

\\{flycheck-error-list-mode-map}"
  ;; Fit the column widths on every revert, including a manual
  ;; `revert-buffer', so that they never go stale
  (add-hook 'tabulated-list-revert-hook
            #'flycheck-error-list--update-format nil t)
  (setq tabulated-list-format flycheck-error-list-format
        ;; Sort by location initially
        tabulated-list-sort-key (cons "Line" nil)
        tabulated-list-padding flycheck-error-list-padding
        tabulated-list-entries #'flycheck-error-list-entries
        ;; `revert-buffer' updates the mode line for us, so all we need to do is
        ;; set the corresponding mode line construct.
        mode-line-buffer-identification flycheck-error-list-mode-line)
  ;; Advertise the grouping controls in the tab line, above the column names
  ;; Tabulated List mode keeps in the header line.
  (setq-local tab-line-format '(:eval (flycheck-error-list--grouping-line)))
  ;; See https://github.com/flycheck/flycheck/issues/1101
  (setq-local truncate-string-ellipsis "…")
  (tabulated-list-init-header))

(defvar-local flycheck-error-list-source-buffer nil
  "The current source buffer of the error list.")
;; Needs to be permanently local to preserve the source buffer across buffer
;; reversions
(put 'flycheck-error-list-source-buffer 'permanent-local t)

(defvar-local flycheck-error-list-scope 'buffer
  "The scope of the error list.

Either `buffer', to show the errors of the source buffer alone, or
`project', to show the project-wide diagnostics aggregated across
every open buffer of the source buffer's project (see
`flycheck--project-directory').  Toggle it with
\\<flycheck-error-list-mode-map>\\[flycheck-error-list-toggle-scope].")
;; Preserved across the reversions Tabulated List mode performs on refresh.
(put 'flycheck-error-list-scope 'permanent-local t)

(defvar-local flycheck-error-list-group-by nil
  "How the error list groups its errors.

Either nil, to show a flat list sorted by location, or a list of
the symbols `file', `checker' and `level' to group the errors
under a header per dimension.  With more than one dimension the
groups nest, e.g. (file checker) groups by file and then by
checker within each file.  Grouping helps a lot in `project'
scope, where errors span many files.  Toggle the dimensions with
\\<flycheck-error-list-mode-map>\\[flycheck-error-list-group-by-file],
\\[flycheck-error-list-group-by-checker] and \\[flycheck-error-list-group-by-level];
\\[flycheck-error-list-group-by-none] shows a flat list.")
(put 'flycheck-error-list-group-by 'permanent-local t)

(defvar-local flycheck-error-list--collapsed nil
  "Set of collapsed group paths, or nil when nothing is collapsed.

A hash table whose keys are the group paths (a list of group keys
from the outermost dimension inward) of the groups whose errors are
currently hidden.  Reset whenever the grouping changes.")
(put 'flycheck-error-list--collapsed 'permanent-local t)

(defface flycheck-error-list-group-header
  '((t :inherit flycheck-error-list-filename :weight bold))
  "Face for the group headers in a grouped error list."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defun flycheck-error-list-set-source (buffer)
  "Set BUFFER as the source buffer of the error list."
  (flycheck-error-list-with-buffer
    (setq flycheck-error-list-source-buffer buffer)
    (flycheck-error-list-refresh)))

(defun flycheck-error-list-update-source ()
  "Make the error list display errors from the current buffer.

The update is skipped if the current buffer is the error list or
if the error list is already pointing to the current buffer."
  (unless (memq (current-buffer)
                (list (get-buffer flycheck-error-list-buffer)
                      (flycheck-error-list-with-buffer
                        flycheck-error-list-source-buffer)))
    (flycheck-error-list-set-source (current-buffer))))

(defun flycheck-error-list-check-source ()
  "Trigger a syntax check in the source buffer of the error list."
  (interactive)
  (let ((buffer (get-buffer flycheck-error-list-source-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (flycheck-buffer)))))

(defun flycheck-error-list-toggle-scope ()
  "Toggle the error list between buffer and project scope.

See `flycheck-error-list-scope'."
  (interactive)
  (flycheck-error-list-with-buffer
    (setq flycheck-error-list-scope
          (if (eq flycheck-error-list-scope 'project) 'buffer 'project)
          ;; The groups change with the scope, so start fresh rather than
          ;; carrying collapse state that could hide a whole file's errors.
          flycheck-error-list--collapsed nil)
    (flycheck-error-list-refresh)
    (message "Flycheck error list now showing the %s"
             (if (eq flycheck-error-list-scope 'project)
                 "whole project" "current buffer"))))

(defconst flycheck-error-list--group-dimensions '(file checker level)
  "The error list grouping dimensions, in tab-line order.")

(defun flycheck-error-list--grouping-dimensions ()
  "Return the active grouping dimensions, in canonical order.

Normalize `flycheck-error-list-group-by' to a list and drop
anything that is not a known dimension, so a flat list yields nil."
  (let ((group-by (if (listp flycheck-error-list-group-by)
                      flycheck-error-list-group-by
                    (list flycheck-error-list-group-by))))
    (seq-filter (lambda (d) (memq d group-by))
                flycheck-error-list--group-dimensions)))

(defun flycheck-error-list--set-group-by (dimensions)
  "Group the error list by DIMENSIONS and refresh it.

DIMENSIONS is nil for a flat list, or a list of the symbols in
`flycheck-error-list--group-dimensions'; several dimensions nest."
  (flycheck-error-list-with-buffer
    (setq flycheck-error-list-group-by dimensions
          ;; Start each grouping fully expanded.
          flycheck-error-list--collapsed nil
          ;; Grouped entries are laid out in order already, so turn off column
          ;; sorting; restore the location sort for the flat list.
          tabulated-list-sort-key (unless dimensions (cons "Line" nil)))
    (flycheck-error-list-refresh)
    (message "Flycheck error list %s"
             (if dimensions
                 (concat "grouped by "
                         (mapconcat #'symbol-name dimensions " > "))
               "flat"))))

(defun flycheck-error-list--toggle-group-by (dimension)
  "Toggle grouping the error list by DIMENSION.

Add or remove DIMENSION from the grouping, keeping the remaining
dimensions in the canonical `flycheck-error-list--group-dimensions'
order, so they always nest file then checker then level."
  ;; Read the current grouping in the error list buffer, not whatever buffer a
  ;; tab-line mouse click happens to run this from.
  (flycheck-error-list-with-buffer
    (let* ((active (flycheck-error-list--grouping-dimensions))
           (next (if (memq dimension active)
                     (remq dimension active)
                   (cons dimension active))))
      (flycheck-error-list--set-group-by
       (seq-filter (lambda (d) (memq d next))
                   flycheck-error-list--group-dimensions)))))

(defun flycheck-error-list-group-by-none ()
  "Show the error list as a flat list, without grouping."
  (interactive)
  (flycheck-error-list--set-group-by nil))

(defun flycheck-error-list-group-by-file ()
  "Toggle grouping the errors in the error list by file."
  (interactive)
  (flycheck-error-list--toggle-group-by 'file))

(defun flycheck-error-list-group-by-checker ()
  "Toggle grouping the errors in the error list by syntax checker."
  (interactive)
  (flycheck-error-list--toggle-group-by 'checker))

(defun flycheck-error-list-group-by-level ()
  "Toggle grouping the errors in the error list by level."
  (interactive)
  (flycheck-error-list--toggle-group-by 'level))

(defun flycheck-error-list--group-command (dimension)
  "Return the command toggling grouping by DIMENSION (nil for flat)."
  (pcase dimension
    ('nil #'flycheck-error-list-group-by-none)
    ('file #'flycheck-error-list-group-by-file)
    ('checker #'flycheck-error-list-group-by-checker)
    ('level #'flycheck-error-list-group-by-level)))

(defun flycheck-error-list--grouping-label (dimension key active)
  "Return the tab-line label for DIMENSION.

KEY is the digit that toggles the dimension, shown in the label as
\\=`M-KEY'; ACTIVE is non-nil when the dimension is on.  Clicking the
label toggles DIMENSION too."
  (let ((label (format "M-%d %s" key (if dimension dimension "flat")))
        (map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1]
      (flycheck-error-list--group-command dimension))
    (propertize label
                'face (and active 'flycheck-error-list-group-header)
                'mouse-face 'highlight
                'keymap map
                'help-echo (if dimension
                               (format "mouse-1: toggle grouping by %s" dimension)
                             "mouse-1: show a flat list"))))

(defun flycheck-error-list--grouping-line ()
  "Return the tab-line string advertising the grouping controls."
  (let ((active (flycheck-error-list--grouping-dimensions)))
    (concat
     " Group:"
     (mapconcat
      (lambda (item)
        (let* ((dimension (car item))
               ;; Highlight every active dimension, or `flat' when none.
               (on (if dimension (memq dimension active) (null active))))
          (concat "  " (flycheck-error-list--grouping-label
                        dimension (cdr item) on))))
      ;; nil (flat) is M-1, then the dimensions M-2, M-3, M-4.
      (cons '(nil . 1)
            (seq-map-indexed (lambda (d i) (cons d (+ i 2)))
                             flycheck-error-list--group-dimensions))
      "")
     (when active "   TAB collapse"))))

(defun flycheck-error-list--error-group-path (error)
  "Return the group path of ERROR under the active grouping.

The path is the list of group keys from the outermost dimension
inward, identifying the innermost group ERROR belongs to."
  (mapcar (lambda (dimension)
            (funcall (flycheck-error-list--group-key-function dimension) error))
          (flycheck-error-list--grouping-dimensions)))

(defun flycheck-error-list-toggle-group-at-point (&optional pos)
  "Collapse or expand the group at POS in a grouped error list.

POS defaults to `point'.  Works both on a group header and on any
error row, whose innermost group it toggles.  With nothing to
toggle (a flat list, or point away from any group) move to the next
button instead, like Tabulated List mode's \\`TAB'."
  (interactive)
  (let* ((id (tabulated-list-get-id pos))
         (header (and (consp id) (eq (car id) 'flycheck-group)))
         (error (flycheck-error-p id)))
    (if (and (flycheck-error-list--grouping-dimensions) (or header error))
        ;; A group is identified by its path; for an error row use the path of
        ;; its innermost group.
        (let ((path (if header
                        (cadr id)
                      (flycheck-error-list--error-group-path id))))
          (unless flycheck-error-list--collapsed
            (setq flycheck-error-list--collapsed (make-hash-table :test 'equal)))
          (if (gethash path flycheck-error-list--collapsed)
              (remhash path flycheck-error-list--collapsed)
            (puthash path t flycheck-error-list--collapsed))
          (flycheck-error-list-refresh)
          (flycheck-error-list--goto-group path))
      (forward-button 1 t nil t))))

(defun flycheck-error-list--goto-group (path)
  "Move point to the header of the group PATH, when it is present."
  (let ((target (list 'flycheck-group path))
        (pos (point-min))
        (found nil))
    (while (and pos (not found))
      (if (equal (tabulated-list-get-id pos) target)
          (setq found pos)
        (setq pos (flycheck-error-list-next-error-pos pos))))
    (when found (goto-char found))))

(define-button-type 'flycheck-error-list
  'action #'flycheck-error-list-goto-error
  'help-echo "mouse-1, RET: goto error"
  'face nil)

(define-button-type 'flycheck-error-list-explain-error
  'action #'flycheck-error-list-explain-error
  'help-echo "mouse-1, RET: explain error")

(define-button-type 'flycheck-error-list-group
  'supertype 'flycheck-error-list
  'help-echo "mouse-1, RET, TAB: collapse or expand")

(defsubst flycheck-error-list-make-cell (text &optional face help-echo type)
  "Make an error list cell with TEXT and FACE.

If FACE is nil don't set a FACE on TEXT.  If TEXT already has
face properties, do not specify a FACE.  Note though, that if
TEXT gets truncated it will not inherit any previous face
properties.  If you expect TEXT to be truncated in the error
list, do specify a FACE explicitly!

If HELP-ECHO is non-nil, set a help-echo property on TEXT, with
value HELP-ECHO.  This is convenient if you expect TEXT to be
truncated.

The cell will have the type TYPE unless TYPE is nil, and the
default type `flycheck-error-list' will be used instead."
  (append (list text 'type (if type type
                             'flycheck-error-list))
          (and face (list 'face face))
          (and help-echo (list 'help-echo help-echo))))

(defsubst flycheck-error-list-make-number-cell (number face)
  "Make a table cell for a NUMBER with FACE.

Convert NUMBER to string, fontify it with FACE and return the
string with attached text properties."
  (flycheck-error-list-make-cell
   (if (numberp number) (number-to-string number) "")
   face))

(defun flycheck-error-list-make-entry (error &optional omit-file)
  "Make a table entry for the given ERROR.

Return a list of (ID CELLS) for `tabulated-list-entries'.  With
OMIT-FILE non-nil leave the File cell blank, as a file header
already names it in a grouped list."
  (let* ((level (flycheck-error-level error))
         (level-face (flycheck-error-level-error-list-face level))
         (filename (flycheck-error-filename error))
         (line (flycheck-error-line error))
         (column (flycheck-error-column error))
         (message (or (flycheck-error-message error)
                      (format "Unknown %S" level)))
         (flushed-msg (flycheck-flush-multiline-message message))
         (id (flycheck-error-id error))
         (id-str (if id (format "%s" id) ""))
         (checker (flycheck-error-checker error))
         (msg-and-checker
          (concat
           ;; Flag errors that carry an applicable machine fix (apply with
           ;; `x'/`flycheck-error-list-apply-fix'), for discoverability.  Only
           ;; badge errors whose fix can actually be applied here, not
           ;; cross-file ones the apply command would refuse.  A fix that has
           ;; to be fetched before we know it exists, as an LSP code action
           ;; does, is badged with a question mark rather than left bare: the
           ;; indicators cannot promise it, but there is room to mention it
           ;; here, and otherwise nothing would suggest trying.
           (when (and (flycheck-error-fix error)
                      (flycheck--error-fix-buffer error))
             (let ((known (flycheck-error-known-fix-p error)))
               (propertize (if known "[fix] " "[fix?] ")
                           'face 'flycheck-error-list-checker-name
                           'help-echo (if known
                                          "x: apply this fix"
                                        "x: ask the server for a fix"))))
           ;; Flag errors that carry secondary locations (visit with `j'/
           ;; `flycheck-error-list-visit-related-location'), showing how many
           ;; and listing them in the badge's tooltip.
           (when-let* ((relations (flycheck-error-relations error)))
             (propertize
              (format "↳%d " (length relations))
              'face 'flycheck-error-list-checker-name
              'help-echo (mapconcat #'flycheck-related-location-format
                                    relations "\n")))
           (flycheck-error-list-make-last-column flushed-msg checker)))
         (explainer (flycheck-checker-get checker 'error-explainer)))
    (list error
          (vector (flycheck-error-list-make-cell
                   (if (and filename (not omit-file))
                       (file-name-nondirectory filename)
                     "")
                   'flycheck-error-list-filename)
                  (flycheck-error-list-make-number-cell
                   line 'flycheck-error-list-line-number)
                  (flycheck-error-list-make-number-cell
                   column 'flycheck-error-list-column-number)
                  (flycheck-error-list-make-cell
                   (symbol-name (flycheck-error-level error)) level-face)
                  ;; Error IDs use a different face when an error-explainer is
                  ;; present
                  (flycheck-error-list-make-cell
                   id-str (if explainer 'flycheck-error-list-id-with-explainer
                            'flycheck-error-list-id)
                   id-str 'flycheck-error-list-explain-error)
                  (flycheck-error-list-make-cell
                   msg-and-checker nil msg-and-checker)))))

(defun flycheck-flush-multiline-message (msg)
  "Prepare error message MSG for display in the error list.

Prepend all lines of MSG except the first with enough space to
ensure that they line up properly once the message is displayed."
  (let* ((spc-spec `(space . (:width ,flycheck--error-list-msg-offset)))
         (spc (propertize " " 'display spc-spec))
         (rep (concat "\\1" spc "\\2")))
    (replace-regexp-in-string "\\([\r\n]+\\)\\(.\\)" rep msg)))

(defun flycheck-error-list-current-errors ()
  "Read the errors to display in the error list.

With `flycheck-error-list-scope' `buffer' (the default), read the
errors of `flycheck-error-list-source-buffer'.  With `project',
read the project-wide diagnostics of that buffer's project."
  (when (buffer-live-p flycheck-error-list-source-buffer)
    (if (eq flycheck-error-list-scope 'project)
        (flycheck--project-errors
         ;; Guard the project lookup: a misbehaving project backend must
         ;; not abort the error-list refresh that runs after every check.
         (ignore-errors
           (with-current-buffer flycheck-error-list-source-buffer
             (flycheck--project-directory))))
      (buffer-local-value 'flycheck-current-errors
                          flycheck-error-list-source-buffer))))

(defun flycheck-error-list--abbreviate-filename (filename)
  "Abbreviate FILENAME for a group header, relative to the source project."
  (if-let* ((filename)
            (dir (and (buffer-live-p flycheck-error-list-source-buffer)
                      (buffer-local-value 'default-directory
                                          flycheck-error-list-source-buffer))))
      (if (string-prefix-p dir filename)
          (file-relative-name filename dir)
        (abbreviate-file-name filename))
    (or filename "<no file>")))

(defun flycheck-error-list--group-key-function (dimension)
  "Return the function extracting the DIMENSION group key of an error."
  (pcase dimension
    ('file #'flycheck-error-filename)
    ('checker #'flycheck-error-checker)
    ('level #'flycheck-error-level)))

(defun flycheck-error-list--group-name (dimension key)
  "Return the display name of the group KEY under DIMENSION."
  (pcase dimension
    ('file (flycheck-error-list--abbreviate-filename key))
    ('checker (if key (symbol-name key) "without checker"))
    ('level (if key (symbol-name key) "without level"))))

(defun flycheck-error-list--sort-groups (dimension groups)
  "Sort GROUPS, an alist of (KEY . ERRORS), for DIMENSION.

Level groups are ordered from the most to the least severe; the
others alphabetically by group name."
  (if (eq dimension 'level)
      (sort groups (lambda (a b)
                     (> (flycheck-error-level-severity (car a))
                        (flycheck-error-level-severity (car b)))))
    ;; Decorate with the display name so it is computed (and, for files,
    ;; abbreviated) once per group instead of on every comparison.
    (mapcar #'cdr
            (sort (mapcar (lambda (group)
                            (cons (flycheck-error-list--group-name
                                   dimension (car group))
                                  group))
                          groups)
                  (lambda (a b) (string< (car a) (car b)))))))

(defun flycheck-error-list--group-error-< (a b)
  "Order errors A and B within a group by file, then by location.

The file tiebreak keeps a file's errors contiguous when a group
spans several files, as it does when grouping by checker or level."
  (let ((file-a (or (flycheck-error-filename a) ""))
        (file-b (or (flycheck-error-filename b) "")))
    (if (string= file-a file-b)
        (flycheck-error-< a b)
      (string< file-a file-b))))

(defun flycheck-error-list--group-collapsed-p (key)
  "Return non-nil when the group KEY is currently collapsed."
  (and flycheck-error-list--collapsed
       (gethash key flycheck-error-list--collapsed)))

(defun flycheck-error-list--prune-collapsed (errors)
  "Drop collapse state for groups absent from ERRORS.

ERRORS is the full, unfiltered error set, so a filter that merely
hides a group's errors keeps its collapse choice.  Pruning a group
that is genuinely gone (its errors were all fixed) stops it from
reappearing collapsed, and hiding a new error, when it comes back.

Every prefix of an error's group path is a live group, so nested
headers above a still-present error keep their collapse too."
  (when flycheck-error-list--collapsed
    ;; Resolve the dimensions and their key functions once, not per error.
    (let ((key-fns (mapcar #'flycheck-error-list--group-key-function
                           (flycheck-error-list--grouping-dimensions)))
          (live (make-hash-table :test 'equal)))
      (dolist (err errors)
        (let ((prefix nil))
          (dolist (key-fn key-fns)
            (setq prefix (append prefix (list (funcall key-fn err))))
            (puthash prefix t live))))
      (maphash (lambda (path _)
                 (unless (gethash path live)
                   (remhash path flycheck-error-list--collapsed)))
               flycheck-error-list--collapsed))))

(defun flycheck-error-list--group-header (path dimension key count collapsed depth)
  "Return a header entry for the group at PATH.

DIMENSION and KEY name the group, COUNT is the number of errors it
holds, COLLAPSED tells whether it is collapsed and DEPTH is its
nesting level, used to indent nested headers.  The entry's id is a
list headed by `flycheck-group', not a `flycheck-error', so
navigation and the fix/explain commands skip it."
  (list (list 'flycheck-group path)
        (vector (flycheck-error-list-make-cell
                 (format "%s%s %s (%d)"
                         (make-string (* 2 depth) ?\s)
                         (if collapsed "▸" "▾")
                         (flycheck-error-list--group-name dimension key)
                         count)
                 'flycheck-error-list-group-header nil
                 'flycheck-error-list-group)
                "" "" "" "" "")))

(defun flycheck-error-list--grouped-entries (errors dimensions path depth omit-file)
  "Return grouped entries for ERRORS under DIMENSIONS.

PATH is the group path leading to ERRORS, DEPTH its nesting level
and OMIT-FILE whether to blank the File cell of the leaf rows.  The
grouping recurses through DIMENSIONS; a collapsed group contributes
only its header."
  (if (null dimensions)
      (mapcar (lambda (err) (flycheck-error-list-make-entry err omit-file))
              (sort errors #'flycheck-error-list--group-error-<))
    (let* ((dimension (car dimensions))
           (key-fn (flycheck-error-list--group-key-function dimension))
           (groups (flycheck-error-list--sort-groups
                    dimension (seq-group-by key-fn errors))))
      (seq-mapcat
       (lambda (group)
         (let* ((key (car group))
                (group-path (append path (list key)))
                (collapsed (flycheck-error-list--group-collapsed-p group-path)))
           (cons (flycheck-error-list--group-header
                  group-path dimension key (length (cdr group)) collapsed depth)
                 (unless collapsed
                   (flycheck-error-list--grouped-entries
                    (cdr group) (cdr dimensions) group-path (1+ depth)
                    omit-file)))))
       groups))))

(defun flycheck-error-list-entries ()
  "Create the entries for the error list.

When `flycheck-error-list-group-by' is non-nil the errors are laid
out under a header per group; otherwise a flat list is returned and
Tabulated List mode sorts it."
  (when-let* ((errors (flycheck-error-list-current-errors))
              (filtered (flycheck-error-list-apply-filter errors)))
    (if-let* ((dimensions (flycheck-error-list--grouping-dimensions)))
        (progn
          ;; Prune against the unfiltered errors, so a filter that hides a
          ;; group does not discard the collapse state the user set.
          (flycheck-error-list--prune-collapsed errors)
          ;; The file name is redundant under a file header, but relevant when
          ;; grouping only by checker or level.
          (flycheck-error-list--grouped-entries
           filtered dimensions nil 0 (memq 'file dimensions)))
      (mapcar #'flycheck-error-list-make-entry filtered))))

(defun flycheck-error-list-entry-< (entry1 entry2)
  "Determine whether ENTRY1 is before ENTRY2 by location.

In a file-grouped list an entry can be a file header rather than an
error; such entries have no location, so they sort as equal here and
keep their place.

See `flycheck-error-<'."
  (let ((err1 (car entry1))
        (err2 (car entry2)))
    (and (flycheck-error-p err1) (flycheck-error-p err2)
         (flycheck-error-< err1 err2))))

(defun flycheck-error-list-entry-level-< (entry1 entry2)
  "Determine whether ENTRY1 is before ENTRY2 by level.

In a file-grouped list an entry can be a file header rather than an
error; such entries have no level, so they sort as equal here and keep
their place.

See `flycheck-error-level-<'."
  (let ((err1 (car entry1))
        (err2 (car entry2)))
    (and (flycheck-error-p err1) (flycheck-error-p err2)
         (not (flycheck-error-level-< err1 err2)))))

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
  (when-let* ((buf (get-buffer flycheck-error-list-buffer)))
    (get-buffer-window-list buf nil all-frames)))

(defun flycheck-get-error-list-window (&optional all-frames)
  "Get a window displaying the error list, or nil if none.

ALL-FRAMES specifies the frames to consider, as in
`get-buffer-window'."
  (when-let* ((buf (get-buffer flycheck-error-list-buffer)))
    (get-buffer-window buf all-frames)))

(defun flycheck-error-list-recenter-at (pos)
  "Recenter the error list at POS."
  (dolist (window (flycheck-get-error-list-window-list t))
    (with-selected-window window
      (goto-char pos)
      (let ((recenter-redisplay nil))
        (recenter)))))

(defun flycheck-error-list-refresh ()
  "Refresh the current error list.

Add all errors currently reported for the current
`flycheck-error-list-source-buffer', and recenter the error
list."
  ;; We only refresh the error list, when it is visible in a window, and we
  ;; select this window while reverting, because Tabulated List mode attempts to
  ;; recenter the error at the old location, so it must have the proper window
  ;; selected.
  (when-let* ((window (flycheck-get-error-list-window t)))
    (with-selected-window window
      (revert-buffer))
    (run-hooks 'flycheck-error-list-after-refresh-hook)
    (let ((preserve-pos (eq (current-buffer)
                            (get-buffer flycheck-error-list-buffer))))
      ;; If the error list is the current buffer, don't recenter when
      ;; highlighting
      (flycheck-error-list-highlight-errors preserve-pos))))

(defun flycheck-error-list-mode-line-filter-indicator ()
  "Create a string representing the current error list filters."
  (concat
   (when flycheck-error-list-minimum-level
     (format " [>= %s]" flycheck-error-list-minimum-level))
   (when flycheck-error-list--checker-filter
     (format " [%s]" flycheck-error-list--checker-filter))
   (when flycheck-error-list--message-filter
     (format " [/%s/]" flycheck-error-list--message-filter))))

(defun flycheck-error-list-mode-line-scope-indicator ()
  "Create a string representing the current error list scope."
  ;; The grouping is advertised in the header line, not here.
  (when (eq flycheck-error-list-scope 'project)
    " [project]"))

(defun flycheck-error-list-mode-line-suppressed-indicator ()
  "Create a string for the mode line about suppressed errors.

Shows the number of errors in the source buffer that were
suppressed over `flycheck-checker-error-threshold', if any."
  (let ((count (and (buffer-live-p flycheck-error-list-source-buffer)
                    (buffer-local-value 'flycheck--suppressed-error-count
                                        flycheck-error-list-source-buffer))))
    (if (and count (> count 0))
        (format " (+%d suppressed)" count)
      "")))

(defun flycheck-error-list--apply-filter-change (thunk)
  "Call THUNK in the error list buffer and refresh the list."
  (flycheck-error-list-with-buffer
    (funcall thunk)
    (force-mode-line-update)
    (flycheck-error-list-refresh)
    (flycheck-error-list-recenter-at (point-min))))

(defun flycheck-error-list-set-filter (level)
  "Restrict the error list to errors at level LEVEL or higher.

LEVEL is either an error level symbol, or nil, to remove the filter."
  (interactive
   (list (flycheck-read-error-level
          "Minimum error level (errors at lower levels will be hidden): ")))
  (when (and level (not (flycheck-error-level-p level)))
    (user-error "Invalid level: %s" level))
  (flycheck-error-list--apply-filter-change
   (lambda () (setq-local flycheck-error-list-minimum-level level))))

(defun flycheck-error-list-set-checker-filter (checker)
  "Restrict the error list to errors from CHECKER.

CHECKER is a syntax checker symbol, or nil to remove the filter."
  (interactive
   (list (flycheck-error-list-with-buffer
           (let ((checkers (seq-uniq
                            (seq-map
                             (lambda (err)
                               (symbol-name (flycheck-error-checker err)))
                             (flycheck-error-list-current-errors)))))
             (unless checkers
               (user-error "The error list contains no errors"))
             (let ((name (completing-read "Show only errors from checker: "
                                          checkers nil t)))
               ;; Empty input removes the filter
               (and (not (string-empty-p name)) (intern name)))))))
  (flycheck-error-list--apply-filter-change
   (lambda () (setq flycheck-error-list--checker-filter checker))))

(defun flycheck-error-list-set-message-filter (regexp)
  "Restrict the error list to errors matching REGEXP.

REGEXP is matched against the error messages and IDs.  An empty
or nil REGEXP removes the filter."
  (interactive (list (read-regexp "Show only errors matching regexp")))
  (setq regexp (and regexp (not (string-empty-p regexp)) regexp))
  ;; Reject invalid regexps here; storing one would break every
  ;; subsequent refresh of the error list
  (when regexp
    (condition-case err
        (string-match-p regexp "")
      (invalid-regexp
       (user-error "Invalid regexp: %s" (cadr err)))))
  (flycheck-error-list--apply-filter-change
   (lambda () (setq flycheck-error-list--message-filter regexp))))

(defun flycheck-error-list-reset-filter (&optional refresh)
  "Remove local error filters and reset to the default filter.

Interactively, or with non-nil REFRESH, refresh the error list."
  (interactive '(t))
  (flycheck-error-list-with-buffer
    (kill-local-variable 'flycheck-error-list-minimum-level)
    (setq flycheck-error-list--checker-filter nil
          flycheck-error-list--message-filter nil)
    (when refresh
      (flycheck-error-list-refresh)
      (flycheck-error-list-recenter-at (point-min))
      (force-mode-line-update))))

(defun flycheck-error-list-apply-filter (errors)
  "Filter ERRORS according to the error list filters.

Combines `flycheck-error-list-minimum-level',
`flycheck-error-list--checker-filter' and
`flycheck-error-list--message-filter'."
  (when-let* ((min-level flycheck-error-list-minimum-level)
              (min-severity (flycheck-error-level-severity min-level)))
    (setq errors
          (seq-filter (lambda (err) (>= (flycheck-error-level-severity
                                         (flycheck-error-level err))
                                        min-severity))
                      errors)))
  (when-let* ((checker flycheck-error-list--checker-filter))
    (setq errors
          (seq-filter (lambda (err) (eq (flycheck-error-checker err) checker))
                      errors)))
  (when-let* ((regexp flycheck-error-list--message-filter))
    (setq errors
          (seq-filter
           (lambda (err)
             (or (string-match-p regexp (or (flycheck-error-message err) ""))
                 (when-let* ((id (flycheck-error-id err)))
                   (string-match-p regexp (format "%s" id)))))
           errors)))
  errors)

(defcustom flycheck-error-list-after-jump-hook nil
  "Functions to run after jumping to an error from the error list.

This hook is run in the source buffer after
`flycheck-error-list-goto-error' jumps to the error location.
Useful for post-jump actions like recentering:

  (add-hook \\='flycheck-error-list-after-jump-hook #\\='recenter)"
  :group 'flycheck
  :type 'hook
  :package-version '(flycheck . "36"))

(defun flycheck-error-list-goto-error (&optional pos)
  "Go to the location of the error at POS in the error list.

On a group header collapse or expand the group instead.  POS
defaults to `point'."
  (interactive)
  (let ((error (tabulated-list-get-id pos)))
    (cond
     ((flycheck-error-p error)
      (flycheck-jump-to-error error)
      (run-hooks 'flycheck-error-list-after-jump-hook))
     ;; A group header, whose id is not a `flycheck-error'.
     ((and (consp error) (eq (car error) 'flycheck-group))
      (flycheck-error-list-toggle-group-at-point pos)))))

(defun flycheck-jump-to-error (error)
  "Go to the location of ERROR."
  (let* ((error-copy (copy-flycheck-error error))
         (filename (flycheck-error-filename error))
         (other-file-error (flycheck-relevant-error-other-file-p error))
         (buffer (if filename
                     (find-file-noselect filename)
                   (flycheck-error-buffer error))))
    (when (buffer-live-p buffer)
      (setf (flycheck-error-buffer error-copy) buffer)
      (flycheck-jump-in-buffer buffer error-copy)
      ;; When jumping to an error in another file, it may not have
      ;; this error available for highlighting yet, so we trigger a check
      ;; if necessary.
      (when other-file-error
        (with-current-buffer buffer
          (unless (seq-contains-p flycheck-current-errors error-copy #'equal)
            (when flycheck-mode
              (flycheck-buffer))))))))

(defun flycheck-jump-in-buffer (buffer error)
  "In BUFFER, jump to ERROR."
  ;; FIXME: we assume BUFFER and the buffer of ERROR are the same.  We don't
  ;; need the first argument then.
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
  ;; Re-highlight the errors.  We have post-command-hook for that, but calls to
  ;; `flycheck-jump-in-buffer' that come from other buffers (e.g. from the error
  ;; list) won't trigger it.
  (flycheck-error-list-highlight-errors 'preserve-pos))

(defun flycheck-error-list-explain-error (&optional pos)
  "Explain the error at POS in the error list.

POS defaults to `point'."
  (interactive)
  (when-let* ((error (tabulated-list-get-id pos))
              ((flycheck-error-p error))
              (explainer (flycheck-checker-get (flycheck-error-checker error)
                                               'error-explainer)))
    (flycheck-error-with-buffer error
      (when-let* ((explanation (funcall explainer error)))
        (flycheck-display-error-explanation explanation)))))

(defun flycheck-error-list-visit-related-location (&optional pos)
  "Visit a related location of the error at POS in the error list.

POS defaults to `point'.  With one related location, jump to it; with
several, prompt for one.  The target is shown in another window, so the
error list stays visible.  Signal a `user-error' when the error has no
related location; see `flycheck-visit-related-location'."
  (interactive)
  (let* ((error (tabulated-list-get-id pos))
         (relations (and (flycheck-error-p error)
                         (flycheck-error-relations error))))
    (unless relations
      (user-error "The error at point has no related locations"))
    (let ((location
           (if (cdr relations)
               (let ((candidates
                      (mapcar (lambda (loc)
                                (cons (flycheck-related-location-format loc) loc))
                              relations)))
                 (cdr (assoc (flycheck-completing-read
                              "Related location: "
                              (mapcar #'car candidates) (caar candidates))
                             candidates)))
             (car relations))))
      (flycheck-goto-related-location location (flycheck-error-filename error)))))

(defun flycheck-error-list-apply-fix (&optional pos)
  "Apply the suggested fix of the error at POS in the error list.

POS defaults to `point'.  Signal a `user-error' when the error
has no fix."
  (interactive)
  (let* ((error (tabulated-list-get-id pos))
         (fixable (and (flycheck-error-p error) (flycheck-error-fix error)))
         (buffer (and fixable (flycheck--error-fix-buffer error))))
    (unless fixable
      (user-error "The error at point has no fix"))
    (unless buffer
      (user-error "This fix cannot be applied here (the error is in another \
file, or its buffer is gone)"))
    ;; Resolve a lazy fix provider in the error's own buffer.
    (if-let* ((fix (with-current-buffer buffer
                     (flycheck-error-resolve-fix error))))
        (progn
          (flycheck-apply-fix fix buffer)
          (flycheck-error-list-refresh)
          (message "Applied fix%s"
                   (if-let* ((description (flycheck-fix-description fix)))
                       (concat ": " description) "")))
      (user-error "The fix for this error is not available"))))

(defun flycheck-error-list-fix-all ()
  "Apply every fixable error's fix in the error list's source buffer."
  (interactive)
  (if-let* ((buffer flycheck-error-list-source-buffer)
            ((buffer-live-p buffer)))
      (progn
        (with-current-buffer buffer
          (call-interactively #'flycheck-fix-all-errors))
        (flycheck-error-list-refresh))
    (user-error "The error list has no live source buffer")))

(defun flycheck-error-list-next-error-pos (pos &optional n)
  "Starting from POS get the N'th next error in the error list.

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
  (let* ((n (or n 1))
         (dir (if (< n 0) -1 1))
         (remaining (abs n))
         (pos (point))
         (target nil))
    ;; Step one row at a time, counting only error rows so a prefix argument
    ;; moves by that many errors and any file headers in a grouped list are
    ;; skipped.  Stop as soon as we can no longer advance, so navigating past
    ;; the top or bottom cannot loop forever.
    (while (> remaining 0)
      (let ((next (flycheck-error-list-next-error-pos pos dir)))
        (if (or (null next) (= next pos))
            (setq remaining 0)
          (setq pos next)
          (when (flycheck-error-p (tabulated-list-get-id pos))
            (setq target pos
                  remaining (1- remaining))))))
    (when (and target (/= target (point)))
      (goto-char target)
      (save-selected-window
        ;; Keep the error list selected, so that the user can navigate errors by
        ;; repeatedly pressing n/p, without having to re-select the error list
        ;; window.
        (flycheck-error-list-goto-error)))))

(defvar-local flycheck-error-list-highlight-overlays nil
  "Error highlight overlays in the error list buffer.")
(put 'flycheck-error-list-highlight-overlays 'permanent-local t)

(defvar-local flycheck-error-list--position-cache nil
  "Cached mapping from each listed error to its row positions.

A cons (TICK . TABLE), where TICK is the `buffer-modified-tick' at
which TABLE was built and TABLE maps each row's error (compared
with `equal') to the list of buffer positions where it appears.
It is rebuilt lazily whenever the error list is reprinted, so that
highlighting the errors at point does not have to scan the whole
buffer on every command.")
(put 'flycheck-error-list--position-cache 'permanent-local t)

(defun flycheck-error-list--positions ()
  "Return a table mapping each listed error to its row positions.

The table maps an error (compared with `equal', matching the old
`member' lookup) to the list of positions where it is shown, so a
single error highlighted on several rows still lights up all of
them.  The result is cached and only rebuilt when the error list
buffer changes, keyed on `buffer-modified-tick'.  This is safe
because every change to the rows goes through a reprint, which
edits the buffer text and bumps the tick, and the displayed error
objects are replaced wholesale on each check rather than mutated
in place."
  (let ((tick (buffer-modified-tick)))
    (unless (eql (car flycheck-error-list--position-cache) tick)
      (let ((table (make-hash-table :test 'equal))
            (pos (point-min)))
        (while pos
          (let ((err (tabulated-list-get-id pos)))
            (when (flycheck-error-p err)
              (push pos (gethash err table))))
          (setq pos (flycheck-error-list-next-error-pos pos)))
        (setq flycheck-error-list--position-cache (cons tick table))))
    (cdr flycheck-error-list--position-cache)))

(defun flycheck-error-list-highlight-errors (&optional preserve-pos)
  "Highlight errors in the error list.

Highlight all errors in the error list that are at point in the
source buffer, and on the same line as point.  Then recenter the
error list to the highlighted error, unless PRESERVE-POS is
non-nil.

Skip highlighting when the error list buffer is not visible, to
avoid slowing down editing when the error list is hidden."
  (when (get-buffer-window flycheck-error-list-buffer)
    (with-current-buffer flycheck-error-list-buffer
      (let ((current-errors
             (when (buffer-live-p flycheck-error-list-source-buffer)
               (with-current-buffer flycheck-error-list-source-buffer
                 (flycheck-overlay-errors-in (line-beginning-position)
                                             (line-end-position))))))
        (let ((old-overlays flycheck-error-list-highlight-overlays)
              (min-point (point-max))
              (max-point (point-min)))
          ;; Display the new overlays first, to avoid re-display flickering
          (setq flycheck-error-list-highlight-overlays nil)
          (when current-errors
            ;; Look up only the errors at point in the row-position index,
            ;; rather than scanning every row of the list on each command.
            ;; Collect the row positions first and drop duplicates, so several
            ;; `equal' errors at point (e.g. from two checkers) still yield a
            ;; single overlay per row.
            (let* ((positions (flycheck-error-list--positions))
                   (rows (delete-dups
                          (mapcan (lambda (err)
                                    (copy-sequence (gethash err positions)))
                                  current-errors))))
              (dolist (beg rows)
                (let ((end (flycheck-error-list-next-error-pos beg)))
                  (setq min-point (min min-point beg)
                        max-point (max max-point beg))
                  (let ((ov (make-overlay beg
                                          ;; Extend overlay to the beginning
                                          ;; of the next line, to highlight
                                          ;; the whole line
                                          (or end (point-max)))))
                    (push ov flycheck-error-list-highlight-overlays)
                    (setf (overlay-get ov 'flycheck-error-highlight-overlay)
                          t)
                    (setf (overlay-get ov 'face)
                          'flycheck-error-list-highlight))))))
          ;; Delete the old overlays
          (seq-do #'delete-overlay old-overlays)
          ;; Recenter only when we actually highlighted a row.  The errors at
          ;; point may all be filtered out of the list, leaving min/max-point
          ;; at their sentinels, which would send point to an unrelated row.
          (when (and (not preserve-pos) flycheck-error-list-highlight-overlays)
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
  ;; Reset the error filter
  (flycheck-error-list-reset-filter)
  (let ((source (current-buffer)))
    ;; Show the error list in a side window.  Under some configurations of
    ;; `display-buffer', this may select `flycheck-error-list-buffer' (see URL
    ;; `https://github.com/flycheck/flycheck/issues/1776').
    (display-buffer flycheck-error-list-buffer
                    flycheck-error-list-display-buffer-action)
    ;; Adjust the source, causing a refresh
    (flycheck-error-list-set-source source)))

(defalias 'list-flycheck-errors 'flycheck-list-errors)


;;; Displaying errors in the current buffer
(defun flycheck--display-errors-via-eldoc-p ()
  "Whether errors at point are displayed through Eldoc."
  (eq flycheck-display-errors-function #'flycheck-display-errors-via-eldoc))

(defvar-local flycheck--eldoc-refresh-pending nil
  "Non-nil while a Flycheck-initiated Eldoc refresh has yet to be displayed.

Flycheck documents interactively so that the echo area is refreshed even
after a command Eldoc does not recognise, but that request should not
reach `eldoc-display-in-buffer', which reads it as \\[eldoc-doc-buffer]
and pops the documentation window open.  Jumping to an error should not
rearrange the frame.

This cannot be a `let' around the request.  Eldoc gathers documentation
from every registered source and displays it once they have all
answered, so a single asynchronous source, such as a language server, is
enough to push the display past the end of any dynamic binding.  A flag
that lives until the display happens survives that.")

(defun flycheck--eldoc-suppress-doc-window (display docs interactive)
  "Keep a Flycheck-initiated refresh from popping the Eldoc window open.

DISPLAY is `eldoc-display-in-buffer', called with DOCS and INTERACTIVE.
The documentation buffer is still brought up to date; only the request to
put it on screen is dropped, and only for the refresh Flycheck asked
for."
  (if flycheck--eldoc-refresh-pending
      (progn
        (setq flycheck--eldoc-refresh-pending nil)
        (funcall display docs nil))
    (funcall display docs interactive)))

(defun flycheck-display-errors-via-eldoc (_errors)
  "Trigger Eldoc to document the errors at point.

Eldoc computes its documentation from all of its registered
sources, including `flycheck-eldoc-function', so refreshing it
shows the Flycheck errors at point alongside e.g. Eglot's
documentation.  This works from any display entry point --
interactive commands, error navigation, automatic display after a
check -- whether or not variable `eldoc-mode' is enabled.

Eldoc is asked to document interactively, because reaching this
function already means Flycheck decided to show the errors at
point.  Left to its own devices Eldoc keeps out of the echo area
unless the command that ran is one of `eldoc-message-commands',
which error navigation is not, and the errors would go unseen.
`flycheck--eldoc-refresh-pending' keeps that request from reaching the
documentation window."
  (setq flycheck--eldoc-refresh-pending t)
  (eldoc-print-current-symbol-info t))

(defun flycheck-eldoc-function (callback &rest _ignored)
  "Document the Flycheck errors at point by calling CALLBACK.

Intended for `eldoc-documentation-functions', where command
`flycheck-mode' registers it.  Only active when
`flycheck-display-errors-function' has its default value
`flycheck-display-errors-via-eldoc', so that user customizations
and third-party display packages keep working unchanged."
  (when (and flycheck-mode (flycheck--display-errors-via-eldoc-p)
             (not (flycheck-annotate--suppresses-echo-p)))
    (when-let* ((errors (flycheck-overlay-errors-at (point))))
      (funcall callback
               (mapconcat
                (lambda (err)
                  (let ((level (flycheck-error-level err)))
                    (concat
                     (propertize (symbol-name level)
                                 'face (flycheck-error-level-error-list-face
                                        level))
                     ": "
                     (flycheck-error-format-message-and-id err)
                     (when-let* ((rel (flycheck-error-format-relations err)))
                       (concat "\n" rel)))))
                errors "\n")))))

(defun flycheck-display-errors (errors)
  "Display ERRORS using `flycheck-display-errors-function'."
  (when flycheck-display-errors-function
    (funcall flycheck-display-errors-function errors)))

(defun flycheck-clear-displayed-errors ()
  "Clear errors using `flycheck-clear-displayed-errors-function'."
  (when flycheck-clear-displayed-errors-function
    (funcall flycheck-clear-displayed-errors-function)))

(defvar-local flycheck-display-error-at-point-timer nil
  "Timer to automatically show errors.")

(defun flycheck-cancel-error-display-error-at-point-timer ()
  "Cancel the error display timer for the current buffer."
  (when flycheck-display-error-at-point-timer
    (cancel-timer flycheck-display-error-at-point-timer)
    (setq flycheck-display-error-at-point-timer nil)))

(defun flycheck-display-error-at-point ()
  "Display all the error messages at point.

If there are no errors, clears the error messages at point."
  (interactive)
  ;; This function runs from a timer, so we must take care to not ignore any
  ;; errors
  (with-demoted-errors "Flycheck error display error: %s"
    (flycheck-cancel-error-display-error-at-point-timer)
    (when flycheck-mode
      (let ((errors (flycheck-overlay-errors-at (point))))
        (if errors
            (flycheck-display-errors errors)
          (flycheck-clear-displayed-errors))))))

(defun flycheck--eldoc-refreshes-echo-area-p ()
  "Whether Eldoc itself will refresh the echo area for this command.

Eldoc only writes to the echo area after a command registered in
`eldoc-message-commands' -- ordinary motion and editing.  After any
other command, such as a jump from `consult-flycheck', it computes its
documentation but leaves the echo area alone, so Flycheck has to ask
for the display itself."
  (and (flycheck--display-errors-via-eldoc-p)
       (bound-and-true-p eldoc-mode)
       (symbolp this-command)
       this-command
       (intern-soft (symbol-name this-command) eldoc-message-commands)
       t))

(defun flycheck-display-error-at-point-soon ()
  "Display error messages at point, with a delay."
  (flycheck-cancel-error-display-error-at-point-timer)
  ;; When errors are displayed through Eldoc and Eldoc will refresh the
  ;; echo area on its own, let it; otherwise fall back to Flycheck's
  ;; timer, which triggers the refresh itself.  When inline display
  ;; already covers the line at point, skip the echo-area message
  ;; entirely (see `flycheck-annotate-suppress-echo').
  (unless (or (flycheck-annotate--suppresses-echo-p)
              (flycheck--eldoc-refreshes-echo-area-p))
    (setq flycheck-display-error-at-point-timer
          (run-at-time flycheck-display-errors-delay nil
                       'flycheck-display-error-at-point))))


(defun flycheck-handle-focus-change ()
  "Handle a change of frame focus for Flycheck error display.

When the frame gains focus, schedule error display at point.
When the frame loses focus, cancel any pending error display."
  (when flycheck-mode
    (if (frame-focus-state)
        (flycheck-display-error-at-point-soon)
      (flycheck-cancel-error-display-error-at-point-timer))))

(add-function :after after-focus-change-function
              #'flycheck-handle-focus-change)


;;; Functions to display errors
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

(define-derived-mode flycheck-error-message-mode text-mode
  "Flycheck error messages"
  "Major mode for extended error messages.")

(defvar flycheck--last-displayed-message nil
  "Reference to the last displayed message so it can be cleared.

This value is the return value from `display-message-or-buffer',
thus it can be a string or a window.

See `flycheck-clear-displayed-error-messages'.")

(defun flycheck-display-error-messages (errors)
  "Display the messages of ERRORS.

Concatenate all non-nil messages of ERRORS as with
`flycheck-help-echo-all-error-messages', and display them with
`display-message-or-buffer', which shows the messages either in
the echo area or in a separate buffer, depending on the number of
lines.  See Info node `(elisp)Displaying Messages' for more
information.

In the latter case, show messages in the buffer denoted by
variable `flycheck-error-message-buffer'."
  (when (and errors (flycheck-may-use-echo-area-p))
    (let* ((message (flycheck-help-echo-all-error-messages errors))
           (retval (display-message-or-buffer
                    message flycheck-error-message-buffer 'not-this-window)))
      ;; We cannot rely on `display-message-or-buffer' returning the right
      ;; window. See URL `https://github.com/flycheck/flycheck/issues/1643'.
      (when-let* ((buf (get-buffer flycheck-error-message-buffer)))
        (with-current-buffer buf
          (unless (derived-mode-p 'flycheck-error-message-mode)
            (flycheck-error-message-mode))))
      (setq flycheck--last-displayed-message retval)
      retval)))

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
  (when-let* ((buffer (flycheck-error-message-buffer))
              (window (get-buffer-window buffer)))
    (unless (flycheck-overlays-at (point))
      ;; save-selected-window prevents `quit-window' from changing the current
      ;; buffer (see https://github.com/flycheck/flycheck/issues/648).
      (save-selected-window
        (quit-window nil window)))))

(defun flycheck-clear-displayed-error-messages ()
  "Clear error messages displayed by `flycheck-display-error-messages'."
  (when flycheck--last-displayed-message
    (if (and (stringp flycheck--last-displayed-message)
             (equal (current-message) flycheck--last-displayed-message))
        (message nil)
      (flycheck-hide-error-buffer))))


;;; Inline error annotations in the buffer
;;
;; Besides the fringe/margin indicators and the highlighting overlays,
;; Flycheck can render the error messages themselves right next to the
;; offending code, in the spirit of VS Code's Error Lens, Neovim's
;; virtual text/lines and Helix's inline diagnostics.  `flycheck-annotate-mode'
;; enables this.
;;
;; Two visualization styles ship out of the box (see
;; `flycheck-annotate-style-functions'): `eol' appends a compact message after
;; the code, and `below' lays the full messages out on their own lines
;; underneath.  The style is chosen per line: the line at point uses
;; `flycheck-annotate-current-line-style' and every other line uses
;; `flycheck-annotate-other-lines-style', so by default the focused line gets
;; the roomy `below' treatment while the rest get a terse `eol' summary.
;;
;; The annotations are drawn with dedicated overlays (tagged
;; `flycheck-annotate'), kept separate from the error overlays so they don't
;; clobber the `before-string'/`after-string' those use for indicators and
;; the `delimiters' highlighting style.  They cover only the visible portion
;; of the window and are rebuilt when the check reports, point changes line,
;; or the window scrolls.

(defface flycheck-annotate-error
  '((t :inherit flycheck-error-list-error))
  "Flycheck face for inline error messages."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defface flycheck-annotate-warning
  '((t :inherit flycheck-error-list-warning))
  "Flycheck face for inline warning messages."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defface flycheck-annotate-info
  '((t :inherit flycheck-error-list-info))
  "Flycheck face for inline informational messages."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defface flycheck-annotate-connector
  '((t :inherit shadow))
  "Flycheck face for the connectors of `below'-style inline messages."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defface flycheck-annotate-fix
  '((t :inherit flycheck-error-list-checker-name))
  "Flycheck face for the inline marker on errors that carry a fix."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defface flycheck-annotate-error-background
  '((((background dark)) :background "#402626" :extend t)
    (((background light)) :background "#fbe9e9" :extend t))
  "Flycheck face for the whole-line tint of error lines.

Used only when `flycheck-annotate-background' is non-nil."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defface flycheck-annotate-warning-background
  '((((background dark)) :background "#403626" :extend t)
    (((background light)) :background "#fbf3e0" :extend t))
  "Flycheck face for the whole-line tint of warning lines.

Used only when `flycheck-annotate-background' is non-nil."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defface flycheck-annotate-info-background
  '((((background dark)) :background "#26323f" :extend t)
    (((background light)) :background "#e6f0fb" :extend t))
  "Flycheck face for the whole-line tint of info lines.

Used only when `flycheck-annotate-background' is non-nil."
  :package-version '(flycheck . "38")
  :group 'flycheck-faces)

(defcustom flycheck-annotate-style-functions
  '((eol . flycheck-annotate-eol-style)
    (below . flycheck-annotate-below-style)
    (sideline . flycheck-annotate-sideline-style))
  "Alist mapping inline display styles to their renderers.

Each entry is a cons cell (STYLE . FUNCTION) where STYLE is a symbol
naming a style (as used by `flycheck-annotate-current-line-style' and
`flycheck-annotate-other-lines-style') and FUNCTION renders it.

FUNCTION is called with three arguments: ERRORS, the list of errors
anchored to one line, sorted most-severe first; ANCHOR, the buffer
position at the end of that line to attach the overlay to; and FOCUSED,
non-nil when the line is the one at point.  It must create its overlays
with `flycheck-annotate--make-overlay', which tags and tracks them for
teardown.

Add to this alist to register additional styles."
  :group 'flycheck
  :type '(alist :key-type symbol :value-type function)
  :risky t
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-current-line-style 'below
  "Inline display style for the line at point.

A style symbol resolved through `flycheck-annotate-style-functions'
\(`below', `eol' or `sideline' out of the box), or nil to leave the
current line unannotated.  See `flycheck-annotate-other-lines-style' for
every other line."
  :group 'flycheck
  :type '(choice (const :tag "Full messages below the line" below)
                 (const :tag "Compact message at end of line" eol)
                 (const :tag "Compact message at the right edge" sideline)
                 (const :tag "Do not annotate the current line" nil)
                 (symbol :tag "Other style"))
  :safe #'symbolp
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-other-lines-style 'eol
  "Inline display style for lines other than the one at point.

A style symbol resolved through `flycheck-annotate-style-functions'
\(`below', `eol' or `sideline' out of the box), or nil to annotate only
the line at point (the way Neovim and Helix show diagnostics for the
cursor line only).  See `flycheck-annotate-current-line-style' for the
line at point."
  :group 'flycheck
  :type '(choice (const :tag "Compact message at end of line" eol)
                 (const :tag "Full messages below the line" below)
                 (const :tag "Compact message at the right edge" sideline)
                 (const :tag "Annotate only the line at point" nil)
                 (symbol :tag "Other style"))
  :safe #'symbolp
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-levels t
  "Error levels to display inline.

Either t to display errors of every level, or a list of level symbols
\(e.g. \\='(error warning)) to restrict the inline display to those
levels.  Errors of other levels are still highlighted and listed as
usual; they just get no inline annotation.

This is the base filter for both tiers; `flycheck-annotate-current-line-levels'
and `flycheck-annotate-other-lines-levels' can narrow it per tier."
  :group 'flycheck
  :type '(choice (const :tag "All levels" t)
                 (repeat :tag "Only these levels" symbol))
  :safe (lambda (value) (or (eq value t) (flycheck-symbol-list-p value)))
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-current-line-levels t
  "Error levels to annotate on the line at point.

Either t to inherit `flycheck-annotate-levels', or a list of level
symbols to restrict the line at point to those levels.  Together with
`flycheck-annotate-other-lines-levels' this lets the focused line show
more levels than the rest."
  :group 'flycheck
  :type '(choice (const :tag "Inherit flycheck-annotate-levels" t)
                 (repeat :tag "Only these levels" symbol))
  :safe (lambda (value) (or (eq value t) (flycheck-symbol-list-p value)))
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-other-lines-levels t
  "Error levels to annotate on lines other than the one at point.

Either t to inherit `flycheck-annotate-levels', or a list of level
symbols to restrict the other lines to those levels.  Set to
\\='(error) to show only errors away from point, the way Helix limits
its non-cursor lines."
  :group 'flycheck
  :type '(choice (const :tag "Inherit flycheck-annotate-levels" t)
                 (repeat :tag "Only these levels" symbol))
  :safe (lambda (value) (or (eq value t) (flycheck-symbol-list-p value)))
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-format-function #'flycheck-error-format-message-and-id
  "Function to format an error for inline display.

Called with a single `flycheck-error' and must return the string to
show for it.  The default renders the message and the error ID."
  :group 'flycheck
  :type 'function
  :risky t
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-fix-marker "[fix] "
  "Marker shown inline before an error that carries a machine-applicable fix.

A string prefixed to the inline message of an error whose checker offered
a fix (applicable with \\[flycheck-fix-error-at-point]), or nil to show no
marker.  Uses the `flycheck-annotate-fix' face."
  :group 'flycheck
  :type '(choice (const :tag "No marker" nil) string)
  :safe #'string-or-null-p
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-suppress-echo t
  "Whether inline display suppresses the echo-area message at point.

When non-nil and the line at point is annotated inline (that is,
`flycheck-annotate-current-line-style' is non-nil), the errors at point
are not additionally shown through `flycheck-display-errors-function'
\(Eldoc or the echo area by default), to avoid displaying the same
message twice.  Set to nil to keep both."
  :group 'flycheck
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "38"))

(defcustom flycheck-annotate-background nil
  "Whether to tint the whole line of each annotated error by severity.

When non-nil, every visible line carrying an error that passes
`flycheck-annotate-levels' gets a subtle background in the colour of its
most severe error, in the spirit of VS Code's Error Lens.  The tint uses
the `flycheck-annotate-error-background', `flycheck-annotate-warning-background'
and `flycheck-annotate-info-background' faces, and is independent of the
message style, so it applies even to lines whose style is nil."
  :group 'flycheck
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "38"))

(defvar-local flycheck-annotate--overlays nil
  "Inline display overlays in the current buffer.")

(defvar-local flycheck-annotate--last-line-start nil
  "Beginning-of-line position the inline overlays were last built for.

Tracked instead of the line number, which would cost a scan from the
start of the buffer on every command.")

(defvar-local flycheck-annotate--last-window-start nil
  "Window start the inline overlays were last built for.")

(defvar-local flycheck-annotate--last-tick nil
  "Buffer-modification tick the inline overlays were last built for.

Tracked so `flycheck-annotate--post-command' also rebuilds after an edit
that left point on its line -- e.g. `open-line', which inserts a newline
but keeps point put -- and would otherwise strand an annotation on the
wrong line until the next check.")

(defun flycheck-annotate--level-face (level)
  "Return the inline face for error LEVEL."
  (pcase level
    ('error 'flycheck-annotate-error)
    ('warning 'flycheck-annotate-warning)
    ('info 'flycheck-annotate-info)
    (_ (flycheck-error-level-error-list-face level))))

(defun flycheck-annotate--fix-marker (err)
  "Return the propertized fix marker for ERR, or an empty string.

Non-empty only when `flycheck-annotate-fix-marker' is set and ERR
is known to carry a machine-applicable fix."
  (if (and flycheck-annotate-fix-marker (flycheck-error-known-fix-p err))
      (propertize flycheck-annotate-fix-marker 'face 'flycheck-annotate-fix)
    ""))

(defun flycheck-annotate--track (overlay)
  "Tag OVERLAY as ours and track it for teardown.  Return OVERLAY."
  (overlay-put overlay 'flycheck-annotate t)
  (push overlay flycheck-annotate--overlays)
  overlay)

(defun flycheck-annotate--make-overlay (anchor string)
  "Create a tracked inline overlay showing STRING at end-of-line ANCHOR.

STRING must be a single display line -- the `eol' and `sideline' styles
use this; the multi-line `below' style has its own placement, see
`flycheck-annotate--make-below-overlay'.  The overlay spans ANCHOR's
trailing newline and shows STRING as a `before-string', so the annotation
renders right after the code.  A `cursor' text property on STRING keeps the
cursor pinned to ANCHOR -- the end of the code -- rather than to the end of
the annotation, so `C-e' and typing behave as if the annotation were not
there.  This is the same technique Flymake uses for its end-of-line
diagnostics.  Return the overlay."
  (let* ((end (min (point-max) (1+ anchor)))
         (ov (make-overlay anchor end nil t nil)))
    (overlay-put ov 'priority 100)
    ;; Evaporate once the spanned newline is gone, but not when the overlay is
    ;; empty from the start (end of buffer, no trailing newline to span) -- an
    ;; empty evaporating overlay would delete itself and drop the annotation.
    (overlay-put ov 'evaporate (/= anchor end))
    ;; The cursor is drawn on the first character of the before-string.  Anchor
    ;; it to a dedicated plain space so `C-e' and typing park at the end of the
    ;; code rather than inside the annotation.  A plain space is required: the
    ;; `cursor' property lands at the far end of the `sideline' style's
    ;; `:align-to' stretch, leaving the cursor stranded.
    (overlay-put ov 'before-string
                 (if (> (length string) 0)
                     (concat (propertize " " 'cursor t) string)
                   string))
    (flycheck-annotate--track ov)))

(defun flycheck-annotate--background-face (level)
  "Return the whole-line background face for error LEVEL, or nil."
  (pcase level
    ('error 'flycheck-annotate-error-background)
    ('warning 'flycheck-annotate-warning-background)
    ('info 'flycheck-annotate-info-background)
    (_ nil)))

(defun flycheck-annotate--tint-line (anchor level)
  "Tint the whole line ending at ANCHOR with LEVEL's background face.

Does nothing for a LEVEL without a background face.  The overlay spans
the trailing newline so the tint reaches the window edge via the face's
`:extend' attribute."
  (when-let* ((face (flycheck-annotate--background-face level)))
    (let* ((beg (save-excursion (goto-char anchor) (line-beginning-position)))
           (end (min (point-max) (1+ anchor)))
           (ov (make-overlay beg end nil t)))
      ;; Below the message overlays (priority 100) so their strings win.
      (overlay-put ov 'priority 1)
      (overlay-put ov 'face face)
      (flycheck-annotate--track ov))))

(defun flycheck-annotate--connectors ()
  "Return the connector strings for `below'-style messages, as (MID . LAST).

MID prefixes every message but the last, LAST the final one.  Falls back
to ASCII when the box-drawing glyphs aren't displayable.  Computed once
per render so the font probe stays out of the per-error loop."
  (if (char-displayable-p ?\N{BOX DRAWINGS LIGHT UP AND RIGHT})
      (cons "\N{BOX DRAWINGS LIGHT VERTICAL AND RIGHT}\N{BOX DRAWINGS LIGHT HORIZONTAL} "
            "\N{BOX DRAWINGS LIGHT UP AND RIGHT}\N{BOX DRAWINGS LIGHT HORIZONTAL} ")
    (cons "`- " "`- ")))

(defun flycheck-annotate--one-line (text)
  "Collapse TEXT onto a single line, folding each run of whitespace.

Plenty of checkers wrap a message over several lines: a parser reporting
\"unexpected newline\" and \"expecting number\" on separate lines, rustc
explaining itself underneath.  The compact styles hang their message off
the end of the code, so a newline that reaches the screen gives the line
extra rows: `eol' stops being after the line, `sideline' loses the
right-edge alignment that defines it, and both take back the vertical
motion that keeping annotations off the anchored line bought."
  (string-trim (replace-regexp-in-string "[ \t]*\n[ \t\n]*" " " text)))

(defun flycheck-annotate--compact-text (errors)
  "Return the one-line summary of ERRORS for the compact styles.

Shows the most severe error's message (ERRORS is sorted most-severe
first) with a count of the rest, propertized with its level face.  A
message spanning lines is folded onto one; see
`flycheck-annotate--one-line'."
  (let* ((err (car errors))
         (face (flycheck-annotate--level-face (flycheck-error-level err)))
         (more (when (cdr errors) (format " (+%d)" (length (cdr errors)))))
         (msg (flycheck-annotate--one-line
               (funcall flycheck-annotate-format-function err))))
    (concat (flycheck-annotate--fix-marker err)
            (propertize (concat msg more) 'face face))))

(defun flycheck-annotate-eol-style (errors anchor _focused)
  "Render ERRORS as a compact message after the line ending at ANCHOR.

Only the most severe error's message is shown, with a count of the rest.
FOCUSED is ignored."
  (flycheck-annotate--make-overlay
   anchor (concat "  " (flycheck-annotate--compact-text errors))))

(defun flycheck-annotate--reserved-columns (&optional window)
  "Columns at WINDOW's right edge that text must not be aligned into.

WINDOW defaults to the selected window.  Without a right fringe to draw
it in, the rightmost column belongs to the glyph marking a line that
continues or was truncated, so text flush against `right' lands in it
and spills onto the next line.  A terminal has no fringes; a graphical
frame can have had them turned off just the same.

A right margin, such as `diff-hl-margin-mode' takes, needs nothing
reserved: `right' in an `:align-to' stretch stops at the text area's
edge, which sits short of any margins (#2312)."
  (if (zerop (or (nth 1 (window-fringes window)) 0)) 1 0))

(defconst flycheck-annotate--sideline-least 10
  "Least display columns of message worth showing truncated.

With less room than this beside the code, a truncated message would be
mostly ellipsis, so the full text is left to trail the code instead.")

(defun flycheck-annotate--window-geometry ()
  "Measure the window showing the current buffer, as (RESERVED . USABLE).

RESERVED is `flycheck-annotate--reserved-columns' for that window and
USABLE the display columns text can actually reach: the text area
minus RESERVED and minus the line-number gutter, which sits inside the
text area, so `window-text-width' counts columns the code cannot use.
A window showing another buffer answers for its own gutter; like the
width itself, it is the one approximation on offer."
  (let* ((window (or (get-buffer-window) (selected-window)))
         (reserved (flycheck-annotate--reserved-columns window))
         (gutter (ceiling (if (eq window (selected-window))
                              (line-number-display-width 'columns)
                            (with-selected-window window
                              (line-number-display-width 'columns))))))
    (cons reserved (- (window-text-width window) gutter reserved))))

(defvar flycheck-annotate--geometry nil
  "The window geometry for the render pass under way, or nil outside one.

Bound by `flycheck-annotate--refresh' so the per-line style functions
do not each measure the window again; see
`flycheck-annotate--window-geometry' for the shape.")

(defun flycheck-annotate--sideline-fit (text anchor usable)
  "Truncate TEXT to the room beside the line ending at ANCHOR.

The `:align-to' stretch only absorbs width the window can spare; it
cannot make room.  A message wider than the gap between the code and
the right edge lands right after the code and wraps, which takes back
the single line the style promises.  USABLE is the reachable width of
the window, per `flycheck-annotate--window-geometry'.  Return TEXT
itself when it fits, so a caller can tell truncation happened by
identity.

The truncated text ends in an ellipsis carrying the face of the last
character kept.  When the room left is less than
`flycheck-annotate--sideline-least', TEXT is returned whole and trails
the code as before: complete beats pretty when neither fits."
  (let* ((line-width (save-excursion (goto-char anchor) (current-column)))
         ;; Two columns of slack: one for the cursor-anchoring space
         ;; `flycheck-annotate--make-overlay' prepends, one to keep the
         ;; message from touching the code.
         (room (- usable line-width 2)))
    (if (or (<= (string-width text) room)
            (< room flycheck-annotate--sideline-least))
        text
      ;; Not `truncate-string-to-width's ETC argument: the ellipsis has
      ;; to carry the face of the last character kept, or it renders in
      ;; the default face beside a coloured message.  The room floor
      ;; keeps KEPT from coming out empty.
      (let* ((ellipsis (truncate-string-ellipsis))
             (kept (truncate-string-to-width
                    text (- room (string-width ellipsis)))))
        (concat kept
                (propertize ellipsis 'face
                            (get-text-property (1- (length kept))
                                               'face kept)))))))

(defun flycheck-annotate-sideline-style (errors anchor _focused)
  "Render ERRORS flushed to the window's right edge past line ANCHOR.

Like `flycheck-annotate-eol-style', but the message is right-aligned with
a stretch of whitespace, in the manner of `lsp-ui-sideline'.  A message
too wide for the room between the code and the window edge is truncated
to fit, with an ellipsis; the full text still reaches the echo area and
the error list.  When the code leaves almost no room at all, the message
simply follows it instead.  FOCUSED is ignored."
  (let* ((geometry (or flycheck-annotate--geometry
                       (flycheck-annotate--window-geometry)))
         (full (flycheck-annotate--compact-text errors))
         (text (flycheck-annotate--sideline-fit full anchor (cdr geometry)))
         (width (+ (string-width text) (car geometry)))
         (spacer (propertize " " 'display `(space :align-to (- right ,width))))
         (ov (flycheck-annotate--make-overlay anchor (concat spacer text))))
    ;; Marks the annotation as an incomplete rendering of its errors, so
    ;; the echo message is not suppressed on its line; see
    ;; `flycheck-annotate--suppresses-echo-p'.
    (unless (eq text full)
      (overlay-put ov 'flycheck-annotate-truncated t))
    ov))

(defun flycheck-annotate--display-column (err bol eol)
  "Return the display column of ERR's start on the line from BOL to EOL.

Uses the display column (via `current-column'), so tabs and other wide
characters before the error are accounted for.  The error's column is
clamped to the line so a checker column past the end still lands on it."
  (let ((offset (min (1- (max 1 (or (flycheck-error-column err) 1)))
                     (- eol bol))))
    (save-excursion
      (goto-char bol)
      (forward-char offset)
      (current-column))))

(defun flycheck-annotate--make-below-overlay (anchor block &optional background)
  "Create a tracked overlay rendering BLOCK on its own lines below ANCHOR.

BACKGROUND, when given, is a face put under the whole block so the tinted
line and its messages read as one region.  The block hangs off the *next*
line, outside the range the line tint covers, so it has to carry the tint
itself rather than inherit it.

BLOCK is the annotation text without surrounding newlines.  It is hung off
the beginning of the following line as a `before-string' ending in a
newline, so its extra screen rows belong to that line's buffer position
rather than to ANCHOR's line.  That keeps visual-line motion working:
`next-line' (with the variable `line-move-visual') and
`evil-next-visual-line' move point onto the next line of code, instead of
stalling on the annotation or, under Evil, getting stuck before it.  On
the last line of the buffer, where there is no following line, the block
is hung off ANCHOR with a leading newline and a `cursor'-anchored space
instead.  Return the overlay."
  (let ((string (if (< anchor (point-max))
                    (concat block "\n")
                  (concat (propertize " " 'cursor t) "\n" block))))
    ;; Appended, so the messages keep their own colours and only take the
    ;; background from the tint.  It has to cover the newlines too, or the
    ;; tint would stop at the text instead of reaching the window edge.
    (when background
      (add-face-text-property 0 (length string) background 'append string))
    (let ((ov (if (< anchor (point-max))
                  (make-overlay (1+ anchor) (1+ anchor) nil t nil)
                (make-overlay anchor anchor nil t nil))))
      (overlay-put ov 'priority 100)
      (overlay-put ov 'before-string string)
      (flycheck-annotate--track ov))))

(defun flycheck-annotate-below-style (errors anchor _focused)
  "Render ERRORS on their own lines below the line ending at ANCHOR.

Each error gets its own message, prefixed with a connector aligned under
its column.  Alignment uses a `:align-to' stretch measured in display
columns, so it lines up under tab-indented code and past a line-number
gutter.  FOCUSED is ignored."
  (let* ((n (length errors))
         (i 0)
         (connectors (flycheck-annotate--connectors))
         (bol (save-excursion (goto-char anchor) (line-beginning-position)))
         (lines nil))
    (dolist (err errors)
      (setq i (1+ i))
      (let* ((col (flycheck-annotate--display-column err bol anchor))
             (pad (if (> col 0)
                      (propertize " " 'display `(space :align-to ,col))
                    ""))
             (face (flycheck-annotate--level-face (flycheck-error-level err)))
             (conn (propertize (if (= i n) (cdr connectors) (car connectors))
                               'face 'flycheck-annotate-connector))
             (msg (concat (flycheck-annotate--fix-marker err)
                          (propertize (funcall flycheck-annotate-format-function err)
                                      'face face)))
             ;; Trail the error's secondary locations on their own aligned
             ;; lines, dimmed so they read as annotations of the message above.
             (rels (mapconcat
                    (lambda (loc)
                      (concat "\n" pad
                              (propertize "  ↳ " 'face 'flycheck-annotate-connector)
                              (propertize (flycheck-related-location-format loc)
                                          'face 'shadow)))
                    (flycheck-error-relations err) "")))
        (push (concat pad conn msg rels) lines)))
    (flycheck-annotate--make-below-overlay
     anchor (string-join (nreverse lines) "\n")
     ;; Same level the line tint uses, so the two always agree
     (and flycheck-annotate-background
          (flycheck-annotate--background-face
           (flycheck-error-level (car errors)))))))

(defun flycheck-annotate--clear ()
  "Delete all inline overlays in the current buffer."
  (mapc #'delete-overlay flycheck-annotate--overlays)
  (setq flycheck-annotate--overlays nil))

(defun flycheck-annotate--effective-levels (tier)
  "Resolve a per-tier levels setting TIER to a concrete filter.

TIER is `flycheck-annotate-current-line-levels' or
`flycheck-annotate-other-lines-levels'; t inherits
`flycheck-annotate-levels'."
  (if (eq tier t) flycheck-annotate-levels tier))

(defun flycheck-annotate--filter-levels (errors levels)
  "Keep the ERRORS whose level is a member of LEVELS.

LEVELS is t (keep all) or a list of level symbols."
  (if (eq levels t)
      errors
    (seq-filter (lambda (err)
                  (memq (flycheck-error-level err) levels))
                errors)))

(defun flycheck-annotate--region ()
  "Return the buffer region to annotate, as a cons (BEG . END).

The visible portion of the window showing the current buffer, or the
line at point when the buffer isn't displayed."
  (if-let* ((win (get-buffer-window)))
      ;; Derive the end from the window height rather than `window-end'
      ;; with its update flag, which would force a redisplay simulation on
      ;; this hot path.  A slight over-scan past the last visible line is
      ;; harmless; we only use the region to collect errors to annotate.
      (let ((start (window-start win)))
        (cons start
              (save-excursion
                (goto-char start)
                (forward-line (window-body-height win))
                (point))))
    (cons (line-beginning-position) (line-end-position))))

(defun flycheck-annotate--group-errors (beg end)
  "Group the errors overlaid between BEG and END by their anchor line.

Return an alist mapping the end-of-line position of each line to the
list of errors on it, in buffer order.  Skips errors that belong to
another file."
  (let ((groups nil))
    (dolist (ov (flycheck-overlays-in beg end))
      (when-let* ((err (overlay-get ov 'flycheck-error)))
        (unless (flycheck-relevant-error-other-file-p err)
          (let* ((anchor (save-excursion
                           (goto-char (overlay-start ov))
                           (line-end-position)))
                 (cell (assq anchor groups)))
            (if cell
                (setcdr cell (cons err (cdr cell)))
              (push (cons anchor (list err)) groups))))))
    groups))

(defun flycheck-annotate--refresh ()
  "Rebuild the inline overlays for the visible part of the buffer.

Records the line and window start the overlays were built for, so
`flycheck-annotate--post-command' can skip rebuilds that would not change
anything."
  (setq flycheck-annotate--last-line-start (line-beginning-position)
        flycheck-annotate--last-window-start (window-start)
        flycheck-annotate--last-tick (buffer-chars-modified-tick))
  (flycheck-annotate--clear)
  (when (and (bound-and-true-p flycheck-annotate-mode) flycheck-mode)
    (pcase-let ((`(,beg . ,end) (flycheck-annotate--region))
                (point-anchor (line-end-position))
                (flycheck-annotate--geometry (flycheck-annotate--window-geometry)))
      (pcase-dolist (`(,anchor . ,errors)
                     (flycheck-annotate--group-errors beg end))
        ;; The tier (focused vs not) selects both the style and the level
        ;; filter, so a line can show more levels at point than elsewhere.
        (let* ((focused (= anchor point-anchor))
               (levels (flycheck-annotate--effective-levels
                        (if focused
                            flycheck-annotate-current-line-levels
                          flycheck-annotate-other-lines-levels)))
               (style (if focused
                          flycheck-annotate-current-line-style
                        flycheck-annotate-other-lines-style)))
          (when-let* ((errors (flycheck-annotate--filter-levels errors levels)))
            (setq errors (sort errors #'flycheck--excessive-errors-<))
            ;; The tint applies to every filtered error line, independent of
            ;; the message style, so it survives an other-lines style of nil.
            (when flycheck-annotate-background
              (flycheck-annotate--tint-line
               anchor (flycheck-error-level (car errors))))
            (when-let* ((render (cdr (assq style
                                           flycheck-annotate-style-functions))))
              (funcall render errors anchor focused))))))))

(defun flycheck-annotate--line-clean-p (pos)
  "Whether the line around POS carries no Flycheck error overlays.

The scan runs through the newline: an error reported past the last
character, such as a missing semicolon, gets an overlay starting
exactly at the line's end, and it anchors an annotation on this line
all the same."
  (save-excursion
    (goto-char pos)
    (not (flycheck-overlays-in (line-beginning-position)
                               (min (point-max)
                                    (1+ (line-end-position)))))))

(defun flycheck-annotate--post-command ()
  "Rebuild the inline overlays if point, the window or the buffer changed.

Skips the rebuild when nothing that affects the annotations happened:
point stayed on its line, the window did not scroll, and the buffer was
not edited.  The buffer-change check catches edits that leave point on
its line \(such as `open-line'), which the line and window checks alone
would miss, and keeps a `below'-style connector aligned as the code
under it changes.  The check runs once per command, so a bulk edit
rebuilds once rather than once per change.

Crossing between two lines that carry no errors is also a skip: which
line has point only tells on the rendering through the current-line
tier, and a line without errors renders nothing under any tier.  Most
navigation is over clean lines, and every skipped rebuild is a screen's
worth of overlays not rebuilt mid-keystroke."
  (let ((same-window-and-text
         (and (eql (window-start) flycheck-annotate--last-window-start)
              (eql (buffer-chars-modified-tick) flycheck-annotate--last-tick))))
    (unless (and same-window-and-text
                 (or (eql (line-beginning-position)
                          flycheck-annotate--last-line-start)
                     ;; The recorded line is a live position: the text is
                     ;; unedited under this guard, so it has not shifted.
                     ;; Narrowing does not bump the tick, though, so the
                     ;; position must still be accessible to be inspected.
                     (and flycheck-annotate--last-line-start
                          (<= (point-min)
                              flycheck-annotate--last-line-start
                              (point-max))
                          (flycheck-annotate--line-clean-p (point))
                          (flycheck-annotate--line-clean-p
                           flycheck-annotate--last-line-start))))
      (flycheck-annotate--refresh))))

(defvar-local flycheck-annotate--rebuilding nil
  "Whether a rebuild is already under way in this buffer.")

(defun flycheck-annotate--after-scroll (window _start)
  "Rebuild the overlays for WINDOW after it scrolled.

`post-command-hook' runs before redisplay, so a command that sends
point off screen has not scrolled the window by the time the overlays
are rebuilt: `window-start' still describes where the window was, and
the line being jumped to is not part of what looks visible.  Nothing
is annotated, and nothing rebuilds it until the next command happens
to come along.  See #2293.

Scrolling is reported during redisplay instead, which is late enough
to know where the window ended up."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (not flycheck-annotate--rebuilding))
    (let ((flycheck-annotate--rebuilding t))
      ;; Laying out an annotation can scroll the window again, and this
      ;; runs inside redisplay, so guard against coming back round
      (flycheck-annotate--post-command))))

(defun flycheck-annotate--truncated-at-point-p ()
  "Return non-nil when the annotation on the current line was truncated."
  (let ((bol (line-beginning-position))
        (end (min (point-max) (1+ (line-end-position)))))
    (seq-some (lambda (ov)
                (and (overlay-get ov 'flycheck-annotate-truncated)
                     ;; Evaporated overlays linger in the registry with
                     ;; no start until the next rebuild clears them.
                     (when-let* ((start (overlay-start ov)))
                       (<= bol start end))))
              flycheck-annotate--overlays)))

(defun flycheck-annotate--suppresses-echo-p ()
  "Return non-nil when inline display covers the at-point echo message.

Only suppresses when the errors at point would actually be rendered
inline and in full, so an error that the inline display drops (because
its level is disabled for the current-line tier, or it belongs to
another file) or truncates (a `sideline' message too wide for the room
beside the code) is still shown through the echo area rather than
nowhere."
  (and (bound-and-true-p flycheck-annotate-mode)
       flycheck-annotate-suppress-echo
       flycheck-annotate-current-line-style
       (not (flycheck-annotate--truncated-at-point-p))
       (seq-some (lambda (err)
                   (not (flycheck-relevant-error-other-file-p err)))
                 (flycheck-annotate--filter-levels
                  (flycheck-overlay-errors-at (point))
                  (flycheck-annotate--effective-levels
                   flycheck-annotate-current-line-levels)))))

;;;###autoload
(define-minor-mode flycheck-annotate-mode
  "Minor mode to display Flycheck error messages inline in the buffer.

When enabled, the error messages are rendered right next to the code
they refer to, in addition to the fringe/margin indicators and the
highlighting.  The line at point is annotated with
`flycheck-annotate-current-line-style' and the rest with
`flycheck-annotate-other-lines-style'; see those options and
`flycheck-annotate-style-functions' for the available styles.

This mode only shows errors while command `flycheck-mode' is on in the
buffer.  With `flycheck-annotate-suppress-echo' (on by default), it also
suppresses the redundant echo-area/Eldoc message for the errors at
point.

The annotations track the window showing the buffer and the line at
point, and are rebuilt after a check, when point changes line, and when
the window scrolls.  When a buffer is shown in more than one window they
follow the selected one; lines revealed by an implicit scroll are
annotated on the next command."
  :lighter nil
  :group 'flycheck
  (cond
   (flycheck-annotate-mode
    (add-hook 'post-command-hook #'flycheck-annotate--post-command nil t)
    (add-hook 'window-scroll-functions #'flycheck-annotate--after-scroll nil t)
    (add-hook 'flycheck-after-syntax-check-hook
              #'flycheck-annotate--refresh nil t)
    (flycheck-annotate--refresh))
   (t
    (remove-hook 'post-command-hook #'flycheck-annotate--post-command t)
    (remove-hook 'window-scroll-functions #'flycheck-annotate--after-scroll t)
    (remove-hook 'flycheck-after-syntax-check-hook
                 #'flycheck-annotate--refresh t)
    (flycheck-annotate--clear))))

;;;###autoload
(define-globalized-minor-mode global-flycheck-annotate-mode
  flycheck-annotate-mode
  (lambda ()
    ;; Enable inline annotations in exactly the buffers `global-flycheck-mode'
    ;; checks; the mode stays inert until a check runs there anyway.
    (when (flycheck-may-enable-mode) (flycheck-annotate-mode 1)))
  :group 'flycheck)


;;; Working with errors
(defun flycheck-copy-errors-as-kill (pos &optional formatter)
  "Copy the errors at POS into the kill ring, using FORMATTER.

All of them go in as one entry, a line each, so a single yank pastes
everything reported at POS.  They used to go in one at a time, which put
the rest behind \[yank-pop] and left a paste showing only one of them.

FORMATTER is a function to turn an error into a string,
defaulting to `flycheck-error-message'.

Interactively, use `flycheck-error-format-message-and-id' as
FORMATTER with universal prefix arg, and `flycheck-error-id' with
normal prefix arg, i.e. copy the message and the ID with
universal prefix arg, and only the id with normal prefix arg."
  (interactive (list (point)
                     (pcase current-prefix-arg
                       ((pred not) #'flycheck-error-message)
                       ((pred consp) #'flycheck-error-format-message-and-id)
                       (_ #'flycheck-error-id))))
  (let ((messages (delq nil (mapcar (or formatter #'flycheck-error-message)
                                     (flycheck-overlay-errors-at pos)))))
    (when messages
      (let ((text (string-join messages "\n")))
        (kill-new text)
        ;; Not as a format string: a message may well contain a `%'
        (message "%s" text)))))

(defun flycheck-explain-error-at-point ()
  "Display an explanation for the first explainable error at point.

The first explainable error at point is the first error at point
with a non-nil `:error-explainer' function defined in its
checker.  The `:error-explainer' function is then called with
this error to produce the explanation to display."
  (interactive)
  (when-let* ((first-error
               ;; Get the first error at point that has an `error-explainer'.
               (seq-find (lambda (error)
                           (flycheck-checker-get
                            (flycheck-error-checker error) 'error-explainer))
                         (flycheck-overlay-errors-at (point))))
              (explainer
               (flycheck-checker-get (flycheck-error-checker first-error)
                                     'error-explainer))
              (explanation (funcall explainer first-error)))
    (flycheck-display-error-explanation explanation)))

(defun flycheck-fix-error-at-point ()
  "Apply the suggested fix of the first fixable error at point.

The first fixable error at point is the first error at point with
a non-nil `flycheck-error-fix'; its fix is applied with
`flycheck-apply-fix'.  Signal a `user-error' when no error at
point has a fix."
  (interactive)
  (if-let* ((error (seq-find (lambda (err)
                              (and (flycheck-error-fix err)
                                   (flycheck--error-fix-buffer err)))
                            (flycheck-overlay-errors-at (point)))))
      (if-let* ((fix (flycheck-error-resolve-fix error)))
          (progn
            (flycheck-apply-fix fix (flycheck--error-fix-buffer error))
            (message "Applied fix%s"
                     (if-let* ((description (flycheck-fix-description fix)))
                         (concat ": " description) "")))
        (user-error "The fix for the error at point is not available"))
    (user-error "No applicable fix at point")))

(defun flycheck-fix-all-errors ()
  "Apply every machine-applicable fix in the current buffer.

Gather the errors whose checker offered a fix (those the error list
marks with a fix) and apply them together as a single undoable change,
via `flycheck-apply-fixes'.  Fixes that would conflict with each other
are skipped; fixes for other files are ignored."
  (interactive)
  (let ((fixes (delq nil
                     (mapcar (lambda (err)
                               (and (eq (flycheck--error-fix-buffer err)
                                        (current-buffer))
                                    (flycheck-error-resolve-fix err)))
                             flycheck-current-errors))))
    (unless fixes
      (user-error "No applicable fixes in this buffer"))
    (let* ((total (length fixes))
           (applied (flycheck-apply-fixes fixes))
           (skipped (- total applied)))
      (message "Applied %d fix%s%s"
               applied (if (= applied 1) "" "es")
               (if (> skipped 0)
                   (format " (%d skipped as conflicting)" skipped)
                 "")))))


;;; Visiting related locations
(defun flycheck-related-location-at-point ()
  "Return the related locations of all Flycheck errors at point.

The result is the flattened `flycheck-error-relations' of every error
overlay at point, in error order."
  (seq-mapcat #'flycheck-error-relations (flycheck-overlay-errors-at (point))))

(defun flycheck-related-location-format (location)
  "Format the related LOCATION as a human-readable string.

Combines its message with its file and position, for completion
candidates and echo-area display."
  (let* ((message (or (flycheck-related-location-message location) ""))
         (filename (flycheck-related-location-filename location))
         (line (flycheck-related-location-line location))
         (column (flycheck-related-location-column location))
         (where (cond ((and filename line column)
                       (format "%s:%d:%d" (file-name-nondirectory filename)
                               line column))
                      ((and filename line)
                       (format "%s:%d" (file-name-nondirectory filename) line))
                      (line (format "%d:%d" line (or column 1)))
                      (t nil))))
    (if where (format "%s (%s)" message where) message)))

(defun flycheck-goto-related-location (location &optional default-file)
  "Visit the related LOCATION, a `flycheck-related-location'.

When LOCATION carries no file of its own, fall back to DEFAULT-FILE, and
then to the current buffer.  Push the current position on the `xref'
marker stack first, so the jump can be reverted with `xref-go-back'
\(\\[xref-go-back]).  When invoked from the error list, show the target
in another window; otherwise reuse the current one."
  (let* ((filename (or (flycheck-related-location-filename location)
                       default-file))
         (line (flycheck-related-location-line location))
         (column (flycheck-related-location-column location))
         (buffer (if filename (find-file-noselect filename) (current-buffer))))
    (require 'xref)
    (xref-push-marker-stack)
    (unless (eq buffer (current-buffer))
      (if (eq (window-buffer) (get-buffer flycheck-error-list-buffer))
          (pop-to-buffer buffer 'other-window)
        (switch-to-buffer buffer)))
    (when line
      (goto-char (flycheck-line-column-to-position line (or column 1))))))

(define-button-type 'flycheck-related-location
  'action (lambda (button)
            (flycheck-goto-related-location
             (button-get button 'flycheck-related-location)))
  'help-echo "mouse-1, RET: visit related location"
  'follow-link t)

(defun flycheck--related-location-button (location)
  "Return LOCATION's formatted text as a button that visits it.

Activating the button with RET or `mouse-1' calls
`flycheck-goto-related-location' on LOCATION.  Displayed inertly (as
plain text) in contexts without an active keymap, such as the echo area."
  (let ((label (flycheck-related-location-format location)))
    (make-text-button label nil
                      'type 'flycheck-related-location
                      'flycheck-related-location location)))

(defun flycheck-error-format-relations (err)
  "Return ERR's related locations as button lines, or nil when it has none.

Each line is indented and prefixed with a `↳' arrow, and is a button that
visits the location when activated; see `flycheck--related-location-button'."
  (when-let* ((relations (flycheck-error-relations err)))
    (mapconcat (lambda (loc)
                 (concat "  ↳ " (flycheck--related-location-button loc)))
               relations "\n")))

(defvar-local flycheck--related-location-walk nil
  "State of the in-progress related-location walk, or nil.

A cons of (LOCATIONS . INDEX): the list being walked and the index of
the location last visited.  See `flycheck-next-related-location'.")

(defun flycheck--related-location-continue-p ()
  "Return non-nil when the last command was a related-location command."
  (and flycheck--related-location-walk
       (memq last-command '(flycheck-visit-related-location
                            flycheck-next-related-location
                            flycheck-previous-related-location))))

(defun flycheck--related-location-step (n)
  "Visit the related location N away from the current one, cycling.

Continue the active walk when one is in progress (see
`flycheck--related-location-walk'); otherwise start a fresh walk from the
related locations at point, where a forward step lands on the first
location and a backward step on the last.  Signal a `user-error' when
there are none."
  (let* ((continue (flycheck--related-location-continue-p))
         (locations (if continue
                        (car flycheck--related-location-walk)
                      (flycheck-related-location-at-point))))
    (unless locations
      (user-error "No related locations at point"))
    (let* ((count (length locations))
           (index (if continue
                      (mod (+ (cdr flycheck--related-location-walk) n) count)
                    ;; No current location yet: a forward step starts at the
                    ;; first location, a backward step at the last.
                    (mod (if (> n 0) (1- n) n) count))))
      (setq flycheck--related-location-walk (cons locations index))
      (flycheck-goto-related-location (nth index locations))
      (when (> count 1)
        (message "Related location %d/%d" (1+ index) count)))))

(defun flycheck-next-related-location (&optional n)
  "Visit the next related location, cycling through those at point.

The first invocation starts from the related locations of the errors at
point (see `flycheck-error-relations'); further invocations, and the
`n'/`p' keys of `flycheck-related-location-repeat-map', step through the
same list.  With prefix arg N, move N locations forward."
  (interactive "p")
  (flycheck--related-location-step (or n 1)))

(defun flycheck-previous-related-location (&optional n)
  "Visit the previous related location, cycling through those at point.

Like `flycheck-next-related-location', but moves backward.  With prefix
arg N, move N locations backward."
  (interactive "p")
  (flycheck--related-location-step (- (or n 1))))

(defvar flycheck-related-location-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'flycheck-next-related-location)
    (define-key map "p" #'flycheck-previous-related-location)
    map)
  "Repeat map for stepping through related locations.
Active after `flycheck-next-related-location' or
`flycheck-previous-related-location' when `repeat-mode' is on.")
(put 'flycheck-next-related-location 'repeat-map
     'flycheck-related-location-repeat-map)
(put 'flycheck-previous-related-location 'repeat-map
     'flycheck-related-location-repeat-map)

(defun flycheck-visit-related-location ()
  "Visit a secondary location related to an error at point.

Gather the related locations of every Flycheck error at point (an LSP
diagnostic's `relatedInformation', a Rust lifetime borrow, and so on;
see `flycheck-error-relations').  With one, jump to it; with several,
prompt for one.  Visiting another file's location opens that file, and
the jump can be reverted with `xref-go-back' (\\[xref-go-back]).

Afterwards, step through the remaining locations with
`flycheck-next-related-location' and `flycheck-previous-related-location'
\(\\[flycheck-next-related-location] and \
\\[flycheck-previous-related-location]).  Signal a `user-error' when no
error at point has a related location."
  (interactive)
  (let ((locations (flycheck-related-location-at-point)))
    (unless locations
      (user-error "No related locations at point"))
    (let ((index (if (cdr locations)
                     (let* ((candidates
                             (seq-map-indexed
                              (lambda (loc i)
                                (cons (flycheck-related-location-format loc) i))
                              locations))
                            (choice (flycheck-completing-read
                                     "Related location: "
                                     (mapcar #'car candidates)
                                     (caar candidates))))
                       (cdr (assoc choice candidates)))
                   0)))
      (setq flycheck--related-location-walk (cons locations index))
      (flycheck-goto-related-location (nth index locations)))))

(defconst flycheck-explain-error-buffer "*Flycheck error explanation*"
  "The name of the buffer to show error explanations.")

(define-derived-mode flycheck-explain-error-mode help-mode
  "Error explanation"
  "Major mode for displaying error explanations."
  (setq buffer-read-only t))

(defun flycheck-display-error-explanation (explanation)
  "Display the EXPLANATION for an error."
  (pcase explanation
    (`nil)
    (`(url . ,url) (browse-url url))
    (_ (let ((inhibit-read-only t)
             (standard-output (temp-buffer-window-setup
                               flycheck-explain-error-buffer)))
         (with-current-buffer standard-output
           (flycheck-explain-error-mode))
         (cond
          ((functionp explanation) (funcall explanation))
          ((stringp explanation) (princ explanation))
          (t (error "Unsupported error explanation: %S" explanation)))
         (display-message-or-buffer standard-output nil 'not-this-window)))))

(defun flycheck-error-explainer-from-url (url-format &optional transform)
  "Return an `:error-explainer' that browses a URL for the error's ID.

URL-FORMAT is a format string with a single %s, replaced by the error's
`flycheck-error-id' -- first passed through TRANSFORM, a function of the ID
returning the string to interpolate, when given.  The returned explainer
yields a (url . STRING) cons, or nil for an error with no ID or whose
TRANSFORM returns nil (so TRANSFORM can skip errors that have no online
documentation).

Many tools document their diagnostics online, keyed by the error ID, so
their checkers can define an explainer in one line, e.g.

    :error-explainer
    (flycheck-error-explainer-from-url \"https://example.com/rules/%s\")"
  (lambda (err)
    (when-let* ((id (flycheck-error-id err))
                (arg (if transform (funcall transform id) id)))
      (cons 'url (format url-format arg)))))


;;; Syntax checkers using external commands
(defun flycheck-command-argument-p (arg)
  "Check whether ARG is a valid command argument."
  (pcase arg
    ((pred stringp) t)
    ((or `source `source-inplace `source-original) t)
    (`(,(or `source `source-inplace) ,suffix)
     (stringp suffix))
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

(defun flycheck-compute-working-directory (checker)
  "Get the default working directory for CHECKER.

Compute the value of `default-directory' for the invocation of
the syntax checker command, by calling the function in the
`working-directory' property of CHECKER, with CHECKER as sole
argument, and returning its value.  Signal an error if the
function returns a non-existing working directory.

If the property is undefined or if the function returns nil
return the `default-directory' of the current buffer."
  (let* ((def-directory-fn (flycheck-checker-get checker 'working-directory))
         (directory (or (and def-directory-fn
                             (funcall def-directory-fn checker))
                        ;; Default to the `default-directory' of the current
                        ;; buffer
                        default-directory)))
    (unless (file-exists-p directory)
      (error ":working-directory %s of syntax checker %S does not exist"
             directory checker))
    directory))

;;;###autoload
(defun flycheck-define-command-checker (symbol docstring &rest properties)
  "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     Flycheck provides ready-made parsers for common structured
     output formats: `flycheck-parse-checkstyle' for Checkstyle
     XML and `flycheck-parse-sarif' for SARIF, which many
     analyzers can emit.  Prefer these over `:error-patterns'
     when a checker offers such an output format, as they are
     more robust than matching human-readable text.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:handle-suspicious FUNCTION'
     A function to handle suspicious state: when the process
     returns non-zero code, but no standard errors (i.e. using
     `:error-patterns' or `:error-parser') are found.

     The function is called with three arguments: CHECKER,
     EXIT-STATUS and OUTPUT (as string) with the checked buffer
     as current.  It should process the output and return a list
     of non-standard errors that best describe what exactly has
     failed.  The returned errors go through `:error-filter' just
     like regular parsed errors.

     The function may also return symbol `disable', or a cons
     cell `(disable . REASON)' with a reason string, when the
     output shows that the checker doesn't apply to this buffer
     at all, e.g. a linter reporting that it has no configuration
     file: the checker is then disabled in the buffer like a
     failing `:enabled' test, with an echo-area notice including
     REASON, and checker selection re-runs so a fallback checker
     can take over.  This avoids probing for applicability with a
     blocking process call in `:enabled'; the asynchronous check
     itself serves as the probe.

     If the function cannot make sense of the output, it should
     return symbol `suspicious' to indicate that what has
     happened is really not expected.

     This property is optional.  If omitted, such state is always
     treated as suspicious.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Some checkers that support reading from standard input have
     a separate flag to indicate the name of the file whose
     contents are being passed on standard input (typically
     `stdin-filename').  In that case, use an `(eval)' form in
     `:command' to pass `flycheck-buffer-file-local-name', which
     yields the file name the checker's host understands even when
     the buffer visits a remote file over TRAMP (that is, use
     `eval (when buffer-file-name (list \"--stdin-file-name\"
     (flycheck-buffer-file-local-name)))').

     For buffers not backed by files, checkers that support input
     on stdin typically report a file name like `-' or `<stdin>'.
     Make sure your error parser or patterns expect these file
     names (for example, use `(or \"<stdin>\" (file-name))') or
     call `flycheck-remove-error-file-names' in a custom
     `:error-filter'.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers."
  (declare (indent 1)
           (doc-string 2))
  (dolist (prop '(:start :interrupt :print-doc))
    (when (plist-get properties prop)
      (error "%s not allowed in definition of command syntax checker %s"
             prop symbol)))

  (unless (plist-get properties :error-filter)
    ;; Default to `flycheck-sanitize-errors' as error filter
    (setq properties (plist-put properties :error-filter
                                #'flycheck-sanitize-errors)))
  (let ((verify-fn (plist-get properties :verify)))
    (setq properties
          (plist-put properties :verify
                     (lambda (checker)
                       (append (flycheck-verify-command-checker checker)
                               (and verify-fn
                                    (funcall verify-fn checker)))))))

  (let ((command (plist-get properties :command))
        (patterns (plist-get properties :error-patterns))
        (parser (or (plist-get properties :error-parser)
                    #'flycheck-parse-with-patterns))
        (handle-suspicious (plist-get properties :handle-suspicious))
        (enabled (plist-get properties :enabled))
        (standard-input (plist-get properties :standard-input)))
    (unless command
      (error "Missing :command in syntax checker %s" symbol))
    (unless (stringp (car command))
      (error "Command executable for syntax checker %s must be a string: %S"
             symbol (car command)))
    (dolist (arg (cdr command))
      (unless (flycheck-command-argument-p arg)
        (error "Invalid command argument %S in syntax checker %s" arg symbol)))
    (when (and (eq parser 'flycheck-parse-with-patterns)
               (not patterns))
      (error "Missing :error-patterns in syntax checker %s" symbol))

    (setq properties
          ;; Automatically disable command checkers if the executable does not
          ;; exist.
          (plist-put properties :enabled
                     (lambda ()
                       (and (flycheck-find-checker-executable symbol)
                            (flycheck-temp-files-writable-p symbol)
                            (or (not enabled) (funcall enabled))))))

    (apply #'flycheck-define-generic-checker symbol docstring
           :start #'flycheck-start-command-checker
           :interrupt #'flycheck-interrupt-command-checker
           :print-doc #'flycheck-command-checker-print-doc
           properties)

    ;; Pre-compile all errors patterns into strings, so that we don't need to do
    ;; that on each error parse
    (let ((patterns (mapcar (lambda (p)
                               (cons (flycheck-rx-to-string `(and ,@(cdr p))
                                                            'no-group)
                                     (car p)))
                             patterns)))
      (pcase-dolist (`(,prop . ,value)
                     `((command           . ,command)
                       (error-parser      . ,parser)
                       (error-patterns    . ,patterns)
                       (handle-suspicious . ,handle-suspicious)
                       (standard-input    . ,standard-input)))
        (setf (flycheck-checker-get symbol prop) value)))))

(eval-and-compile
  ;; Make this function available during byte-compilation, since we need it
  ;; at macro expansion of `flycheck-def-executable-var'.
  (defun flycheck-checker-executable-variable (checker)
    "Get the executable variable of CHECKER.

The executable variable is named `flycheck-CHECKER-executable'."
    (intern (format "flycheck-%s-executable" checker))))

(defun flycheck-checker-default-executable (checker)
  "Get the default executable of CHECKER."
  (car (flycheck-checker-get checker 'command)))

(defun flycheck-checker-executable (checker)
  "Get the command executable of CHECKER.

The executable is either the value of the variable
`flycheck-CHECKER-executable', or the default executable given in
the syntax checker definition, if the variable is nil."
  (let ((var (flycheck-checker-executable-variable checker)))
    (or (and (boundp var) (symbol-value var))
        (flycheck-checker-default-executable checker))))

(defun flycheck-find-checker-executable (checker)
  "Get the full path of the executable of CHECKER.

Return the full absolute path to the executable of CHECKER, or
nil if the executable does not exist."
  (funcall flycheck-executable-find (flycheck-checker-executable checker)))

(defun flycheck-call-checker-process
    (checker infile destination error &rest args)
  "Call CHECKER's executable with ARGS.

Return nil (or raise an error if ERROR is non-nil) when CHECKER's
executable cannot be found, and return a numeric exit status or a
signal description string otherwise.  CHECKER's input is taken
from INFILE, and its output is sent to DESTINATION, as in
`call-process'."
  (if-let* ((executable (flycheck-find-checker-executable checker)))
      (condition-case err
          ;; `process-file' runs EXECUTABLE on the remote host when
          ;; `default-directory' is remote, and behaves like
          ;; `call-process' otherwise.  The program must be the plain
          ;; local name on that host.
          (apply #'process-file (file-local-name executable)
                 infile destination nil args)
        (error (when error (signal (car err) (cdr err)))))
    (when error
      (user-error "Cannot find `%s' using `flycheck-executable-find'"
                  (flycheck-checker-executable checker)))))

(defun flycheck-call-checker-process-for-output
    (checker infile error &rest args)
  "Call CHECKER's executable with ARGS and return its output.

Call `flycheck-call-checker-process' with INFILE, ERROR, and
ARGS.  If it returns 0, return the process' output.  Otherwise,
return nil or throw an error.

This function is similar to `flycheck-call-checker-process'
called in a `with-output-to-string' block, but it takes care of
the error checking automatically."
  (let ((temp (generate-new-buffer " *temp*")))
    (unwind-protect
        ;; We need to call the checker process in the right buffer, so that it
        ;; uses the right exec-path, checker executable, etc.  See URL
        ;; `https://github.com/flycheck/flycheck/issues/1770'.
        (let ((exit-code (apply #'flycheck-call-checker-process
                                checker infile temp error args))
              (output (with-current-buffer temp (buffer-string))))
          ;; EXIT-CODE is nil when CHECKER cannot be found (and ERROR is nil, so
          ;; no error was raised) and a string for a signalled process; treat
          ;; both as failure rather than passing them to `zerop'.
          (if (eql 0 exit-code) output
            (when error
              (error "Process %s failed with %S (%s)"
                     checker exit-code output))))
      (kill-buffer temp))))

(defun flycheck--process-file-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.

Like `process-lines', but runs PROGRAM through `process-file', so
it executes on the host of `default-directory' (a remote host
over TRAMP) instead of always locally.  PROGRAM must be the plain
local name on that host.  Signal an error if PROGRAM cannot be
found or exits with a non-zero status."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args)))
      (unless (eq status 0)
        (error "%s exited with status %s" program status))
      (goto-char (point-min))
      (let (lines)
        (while (not (eobp))
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                lines)
          (forward-line 1))
        (nreverse lines)))))

(defun flycheck-checker-arguments (checker)
  "Get the command arguments of CHECKER."
  (cdr (flycheck-checker-get checker 'command)))

(defun flycheck-substitute-argument (arg checker)
  "Substitute ARG for CHECKER.

Return a list of real arguments for the executable of CHECKER,
substituted for the symbolic argument ARG.  Single arguments,
e.g. if ARG is a literal string, are wrapped in a list.

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

`(source SUFFIX)', `(source-inplace SUFFIX)'
     Like `source' and `source-inplace', but ensure generated
     file names end with the given suffix.  Use this when the
     checker requires that file names on its command line have a
     certain suffix (file extension).

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

     To ignore the output of syntax checkers, try symbol
     `null-device' first.

symbol `null-device'
     Return the value of variable `null-device', i.e. the system
     null device.

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
     before the file, either as a string or as a list.  If omitted,
     PREPEND-FN defaults to `list'.

`(option OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE and return a list of
     arguments that pass this value as value for OPTION to the
     syntax checker.

     PREPEND-FN is called with the OPTION and the value of
     VARIABLE, and should return OPTION prepended before the
     file, either as a string or as a list.  If omitted, PREPEND-FN
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
     before the item, either as a string or as a list.  If omitted,
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
    ;; File names below are reduced with `file-local-name': the checker
    ;; process runs on the host of `default-directory' (a remote host
    ;; over TRAMP) and must receive a plain local name, not a TRAMP file
    ;; name.  The temporary files are created and later deleted through
    ;; their full (possibly remote) names.
    (`source
     (list (file-local-name
            (flycheck-save-buffer-to-temp #'flycheck-temp-file-system))))
    (`source-inplace
     (list (file-local-name
            (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace))))
    (`(source ,suffix)
     (list (file-local-name
            (flycheck-save-buffer-to-temp
             (lambda (filename) (flycheck-temp-file-system filename suffix))))))
    (`(source-inplace ,suffix)
     (list (file-local-name
            (flycheck-save-buffer-to-temp
             (lambda (filename) (flycheck-temp-file-inplace filename suffix))))))
    (`source-original (list (if-let* ((f (buffer-file-name)))
                                (file-local-name f)
                              "")))
    (`temporary-directory (list (file-local-name (flycheck-temp-dir-system))))
    (`temporary-file-name
     (let ((directory (flycheck-temp-dir-system)))
       (list (file-local-name
              (make-temp-name (expand-file-name "flycheck" directory))))))
    (`null-device (list null-device))
    (`(config-file ,option-name ,file-name-var)
     (when-let* ((value (symbol-value file-name-var))
                 (file-name (flycheck-locate-config-file value checker)))
       (flycheck-prepend-with-option
        option-name (list (file-local-name file-name)))))
    (`(config-file ,option-name ,file-name-var ,prepend-fn)
     (when-let* ((value (symbol-value file-name-var))
                 (file-name (flycheck-locate-config-file value checker)))
       (flycheck-prepend-with-option
        option-name (list (file-local-name file-name)) prepend-fn)))
    (`(option ,option-name ,variable)
     (when-let* ((value (symbol-value variable)))
       (unless (stringp value)
         (error "Value %S of %S for option %s is not a string"
                value variable option-name))
       (flycheck-prepend-with-option option-name (list value))))
    (`(option ,option-name ,variable ,prepend-fn)
     (when-let* ((value (symbol-value variable)))
       (unless (stringp value)
         (error "Value %S of %S for option %s is not a string"
                value variable option-name))
       (flycheck-prepend-with-option option-name (list value) prepend-fn)))
    (`(option ,option-name ,variable ,prepend-fn ,filter)
     (when-let* ((value (funcall filter (symbol-value variable))))
       (unless (stringp value)
         (error "Value %S of %S (filter: %S) for option %s is not a string"
                value variable filter option-name))
       (flycheck-prepend-with-option option-name (list value) prepend-fn)))
    (`(option-list ,option-name ,variable)
     (let ((value (symbol-value variable)))
       (unless (and (listp value) (seq-every-p #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value)))
    (`(option-list ,option-name ,variable ,prepend-fn)
     (let ((value (symbol-value variable)))
       (unless (and (listp value) (seq-every-p #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(option-list ,option-name ,variable ,prepend-fn ,filter)
     (let ((value (delq nil (mapcar filter (symbol-value variable)))))
       (unless (and (listp value) (seq-every-p #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(option-flag ,option-name ,variable)
     (when (symbol-value variable)
       (list option-name)))
    (`(eval ,form)
     (let ((result (eval form t)))
       (cond
        ((and (listp result) (seq-every-p #'stringp result)) result)
        ((stringp result) (list result))
        (t (error "Invalid result from evaluation of %S: %S" form result)))))
    (_ (error "Unsupported argument %S" arg))))

(defun flycheck-checker-substituted-arguments (checker)
  "Get the substituted arguments of a CHECKER.

Substitute each argument of CHECKER using
`flycheck-substitute-argument'.  This replaces any special
symbols in the command."
  (seq-mapcat (lambda (arg) (flycheck-substitute-argument arg checker))
              (flycheck-checker-arguments checker)))

(defun flycheck-process-send-buffer (process)
  "Send all contents of current buffer to PROCESS.

Sends all contents of the current buffer to the standard input of
PROCESS, and terminates standard input with EOF."
  (save-restriction
    (widen)
    (process-send-region process (point-min) (point-max)))
  (process-send-eof process))

(defun flycheck--wrap-command (prog args)
  "Wrap PROG and ARGS using `flycheck-command-wrapper-function'."
  ;; We don't call `flycheck-executable-find' on the output of the wrapper
  ;; function, since it might not expect it (an executable-find function
  ;; designed to find binaries in a sandbox could get confused if we asked it
  ;; about the sandboxing program itself).
  (funcall flycheck-command-wrapper-function (cons prog args)))

(defun flycheck-start-command-checker (checker callback)
  "Start a command CHECKER with CALLBACK."
  (let (process)
    (condition-case err
        (let* (;; `flycheck-find-checker-executable' may return nil for a
               ;; cached-enabled checker whose executable later vanished.
               ;; Fail the check cleanly rather than starting a process with
               ;; a nil program, which never exits and hangs the check.
               (executable (or (flycheck-find-checker-executable checker)
                               (error "Cannot find the executable of checker %s"
                                      checker)))
               ;; `flycheck-find-checker-executable' may return a remote
               ;; (TRAMP) file name; the process program must be the plain
               ;; local name on the remote host.
               (program (file-local-name executable))
               (args (flycheck-checker-substituted-arguments checker))
               (command (flycheck--wrap-command program args))
               (sentinel-events nil)
               ;; Use pipes to receive output from the syntax checker.  They are
               ;; more efficient and more robust than PTYs, which Emacs uses by
               ;; default, and since we don't need any job control features, we
               ;; can easily use pipes.
               (process-connection-type nil)
               ;; Force English messages from checker processes so that
               ;; error patterns can match reliably.  We set LC_MESSAGES
               ;; rather than LC_ALL so that the character encoding
               ;; (LC_CTYPE) is left untouched; using LC_ALL=C forces an
               ;; ASCII locale that breaks checkers reading UTF-8 input,
               ;; such as hledger (see #2170).  For remote checks the
               ;; environment reaches the process through
               ;; `tramp-remote-process-environment'.
               (process-environment (cons "LC_MESSAGES=C" process-environment))
               (tramp-remote-process-environment
                (when (boundp 'tramp-remote-process-environment)
                  (cons "LC_MESSAGES=C" tramp-remote-process-environment))))
          ;; We do not associate the process with any buffer, by
          ;; passing nil for the BUFFER argument of `start-file-process'.
          ;; Instead, we just remember the buffer being checked in a
          ;; process property (see below).  This neatly avoids all
          ;; side-effects implied by attaching a process to a buffer, which
          ;; may cause conflicts with other packages.
          ;;
          ;; See https://github.com/flycheck/flycheck/issues/298 for an
          ;; example for such a conflict.
          ;;
          ;; We use `start-file-process' rather than `start-process' so
          ;; the checker runs on the remote host when `default-directory'
          ;; is remote; it behaves exactly like `start-process' otherwise.
          (setq process (apply 'start-file-process
                               (format "flycheck-%s" checker)
                               nil command))
          ;; Process sentinels can be called while sending input to the process.
          ;; We want to record errors raised by process-send before calling
          ;; `flycheck-handle-signal', so initially just accumulate events.
          (setf (process-sentinel process)
                (lambda (_ event) (push event sentinel-events)))
          (setf (process-filter process) #'flycheck-receive-checker-output)
          (set-process-query-on-exit-flag process nil)
          ;; Remember the syntax checker, the buffer and the callback
          (process-put process 'flycheck-checker checker)
          (process-put process 'flycheck-callback callback)
          (process-put process 'flycheck-buffer (current-buffer))
          ;; The default directory is bound in the `flycheck-syntax-check-start'
          ;; function.
          (process-put process 'flycheck-working-directory default-directory)
          ;; Track the temporaries created by argument substitution in the
          ;; process itself, to get rid of the global state ASAP.
          (process-put process 'flycheck-temporaries flycheck-temporaries)
          (setq flycheck-temporaries nil)
          ;; Send the buffer to the process on standard input, if enabled.
          (when (flycheck-checker-get checker 'standard-input)
            (condition-case err
                (flycheck-process-send-buffer process)
              ;; Some checkers exit before reading all input, causing errors
              ;; such as a `file-error' for a closed pipe, or a plain “no longer
              ;; connected to pipe; closed it” error for a disconnection.  We
              ;; report them if needed in `flycheck-finish-checker-process' (see
              ;; `https://github.com/flycheck/flycheck/issues/1278').
              (error (process-put process 'flycheck-error err))))
          ;; Set the actual sentinel and process any events that might have
          ;; happened while we were sending input.
          (setf (process-sentinel process) #'flycheck-handle-signal)
          (dolist (event (nreverse sentinel-events))
            (flycheck-handle-signal process event))
          ;; Return the process.
          process)
      (error
       ;; In case of error, clean up our resources, and report the error back to
       ;; Flycheck.
       (flycheck-safe-delete-temporaries)
       (when process
         ;; No need to explicitly delete the temporary files of the process,
         ;; because deleting runs the sentinel, which will delete them anyway.
         (delete-process process))
       (signal (car err) (cdr err))))))

(defun flycheck-interrupt-command-checker (_checker process)
  "Interrupt a PROCESS."
  ;; Deleting the process always triggers the sentinel, which does the cleanup
  (when process
    (delete-process process)))

(defun flycheck-command-checker-print-doc (checker)
  "Print additional documentation for a command CHECKER."
  (let ((executable (flycheck-checker-default-executable checker))
        (config-file-var (flycheck-checker-get checker 'config-file-var))
        (option-vars (seq-sort #'string<
                               (flycheck-checker-get checker 'option-vars))))
    (princ "\n")

    (let ((doc-start (with-current-buffer standard-output (point-max))))
      ;; Track the start of our documentation so that we can re-indent it
      ;; properly
      (princ "  This syntax checker executes \"")
      (princ executable)
      (princ "\"")
      (when config-file-var
        (princ ", using a configuration file from `")
        (princ (symbol-name config-file-var))
        (princ "'"))
      (princ ". The executable can be overridden with `")
      (princ (symbol-name (flycheck-checker-executable-variable checker)))
      (princ "'.")

      (with-current-buffer standard-output
        (save-excursion
          (fill-region-as-paragraph doc-start (point-max)))))
    (princ "\n")
    (when option-vars
      (princ
       "\n  This syntax checker can be configured with these options:\n\n")
      (dolist (var option-vars)
        (princ (format "     * `%s'\n" var))))))

(defun flycheck-verify-command-checker (checker)
  "Verify a command CHECKER in the current buffer.

Return a list of `flycheck-verification-result' objects for
CHECKER."
  (let ((executable (flycheck-find-checker-executable checker))
        (config-file-var (flycheck-checker-get checker 'config-file-var)))
    `(
      ,(flycheck-verification-result-new
        :label "executable"
        :message (if executable (format "Found at %s" executable) "Not found")
        :face (if executable 'success '(bold error)))
      ,@(when config-file-var
          (let* ((value (symbol-value config-file-var))
                 (path (and value (flycheck-locate-config-file value checker))))
            (list (flycheck-verification-result-new
                   :label "configuration file"
                   :message (if path (format "Found at %S" path) "Not found")
                   :face (if path 'success 'warning)))))
      ,@(unless (flycheck-temp-files-writable-p checker)
          (list (flycheck-verification-result-new
                 :label "temp directory"
                 :message (format "%s is not writable"
                                  (flycheck-temp-directory checker))
                 :face 'error))))))


;;; Process management for command syntax checkers
(defun flycheck-receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (push output (process-get process 'flycheck-pending-output)))

(defun flycheck-get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((pending-output (process-get process 'flycheck-pending-output)))
      (apply #'concat (nreverse pending-output)))))

(defun flycheck-handle-signal (process _event)
  "Handle a signal from the syntax checking PROCESS.

_EVENT is ignored."
  (when (memq (process-status process) '(signal exit))
    (let ((files (process-get process 'flycheck-temporaries))
          (buffer (process-get process 'flycheck-buffer))
          (callback (process-get process 'flycheck-callback))
          (cwd (process-get process 'flycheck-working-directory))
          (err (process-get process 'flycheck-error)))
      ;; Delete the temporary files
      (seq-do #'flycheck-safe-delete files)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (condition-case err
              (pcase (process-status process)
                (`signal
                 (funcall callback 'interrupted))
                (`exit
                 (flycheck-finish-checker-process
                  (process-get process 'flycheck-checker)
                  (or err (process-exit-status process))
                  files
                  (flycheck-get-output process) callback cwd)))
            ((debug error)
             (funcall callback 'errored (error-message-string err)))))))))

(defun flycheck-finish-checker-process
    (checker exit-status files output callback cwd)
  "Finish a checker process from CHECKER with EXIT-STATUS.

EXIT-STATUS can be a number or an arbitrary form (if it is not 0,
a `suspicious' status is reported to CALLBACK).

FILES is a list of files given as input to the checker.  OUTPUT
is the output of the syntax checker.  CALLBACK is the status
callback to use for reporting.

Parse the OUTPUT and report an appropriate error status.

Resolve all errors in OUTPUT using CWD as working directory."
  (let ((errors (flycheck-parse-output output checker (current-buffer))))
    (let ((self-disabled nil))
      (when (and (not (equal exit-status 0)) (null errors))
        ;; Give the checker a chance to recover from suspicious state:
        ;; exit status is nonzero, but there are no errors.
        (let ((recovered (flycheck-handle-suspicious-state checker exit-status
                                                           output)))
          (cond
           ((or (eq recovered 'disable)
                (and (consp recovered) (eq (car recovered) 'disable)))
            ;; The checker diagnosed itself as inapplicable to this
            ;; buffer, e.g. a linter without a configuration file.  The
            ;; status report performs the disabling, subject to the usual
            ;; staleness checks, and finishes the syntax check.
            (setq self-disabled t)
            (funcall callback 'self-disabled
                     (and (consp recovered) (cdr recovered))))
           ((listp recovered)
            (setf errors recovered))
           (t
            ;; Warn about a suspicious result from the syntax checker.  We do
            ;; right after parsing the errors, before filtering, because a
            ;; syntax checker might report errors from other files
            ;; (e.g. includes) even if there are no errors in the file being
            ;; checked.
            (funcall callback 'suspicious
                     (format "Exited with status %S, printing output that \
contained no errors Flycheck could read:\n\n%s"
                             exit-status output))))))
      (unless self-disabled
        (funcall callback 'finished
                 ;; Fix error file names, by substituting them backwards
                 ;; from the temporaries.
                 (mapcar (lambda (e) (flycheck-fix-error-filename e files cwd))
                         errors))))))


;;; Executables of command checkers.
(defmacro flycheck-def-executable-var (checker default-executable)
  "Define the executable variable for CHECKER.

DEFAULT-EXECUTABLE is the default executable.  It is only used in
the docstring of the variable.

The variable is defined with `defcustom' in the
`flycheck-executables' group.  It's also defined to be risky as
file-local variable, to avoid arbitrary executables being used
for syntax checking."
  (let ((executable-var (flycheck-checker-executable-variable checker)))
    `(progn
       (defcustom ,executable-var nil
         ,(format "The executable of the %s syntax checker.

Either a string containing the name or the path of the
executable, or nil to use the default executable from the syntax
checker declaration.

The default executable is %S." checker default-executable)
         :type '(choice (const :tag "Default executable" nil)
                        (string :tag "Name or path"))
         :group 'flycheck-executables
         :risky t))))

(defun flycheck-set-checker-executable (checker &optional executable)
  "Set the executable of CHECKER in the current buffer.

CHECKER is a syntax checker symbol.  EXECUTABLE is a string with
the name of an executable or the path to an executable file, which
is to be used as executable for CHECKER.  If omitted or nil,
reset the executable of CHECKER.

Interactively, prompt for a syntax checker and an executable
file, and set the executable of the selected syntax checker.
With prefix arg, prompt for a syntax checker only, and reset the
executable of the selected checker to the default.

Set the executable variable of CHECKER, that is,
`flycheck-CHECKER-executable' to EXECUTABLE.  Signal
`user-error', if EXECUTABLE does not denote a command or an
executable file.

This command is intended for interactive use only.  In Lisp, just
`let'-bind the corresponding variable, or set it directly.  Use
`flycheck-checker-executable-variable' to obtain the executable
variable symbol for a syntax checker."
  (declare (interactive-only "Set the executable variable directly instead"))
  (interactive
   (let* ((checker (flycheck-read-checker "Syntax checker: "))
          (default-executable (flycheck-checker-default-executable checker))
          (executable (if current-prefix-arg
                          nil
                        (read-file-name "Executable: " nil default-executable
                                        nil nil flycheck-executable-find))))
     (list checker executable)))
  (when (and executable (not (funcall flycheck-executable-find executable)))
    (user-error "%s is no executable" executable))
  (let ((variable (flycheck-checker-executable-variable checker)))
    (set (make-local-variable variable) executable)))


;;; Configuration files and options for command checkers
(defun flycheck-register-config-file-var (var checkers)
  "Register VAR as config file var for CHECKERS.

CHECKERS is a single syntax checker or a list thereof."
  (when (symbolp checkers)
    (setq checkers (list checkers)))
  (dolist (checker checkers)
    (setf (flycheck-checker-get checker 'config-file-var) var)))

;;;###autoload
(defmacro flycheck-def-config-file-var (symbol checker &optional file-name
                                               &rest custom-args)
  "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'."
  (declare (indent 3))
  `(progn
     (defcustom ,symbol ,file-name
       ,(format "Configuration file for `%s'.

If set to a string, locate the configuration file using the
functions from `flycheck-locate-config-file-functions'.  If the
file is found, pass it to the syntax checker as configuration
file.

If no configuration file is found, or if this variable is set to
nil, invoke the syntax checker without a configuration file.

Use this variable as file-local variable if you need a specific
configuration file for a buffer." checker)
       :type '(choice (const :tag "No configuration file" nil)
                      (string :tag "File name or path")
                      (repeat :tag "File names or paths" string))
       :safe #'flycheck-string-or-string-list-p
       :group 'flycheck-config-files
       ,@custom-args)
     (flycheck-register-config-file-var ',symbol ',checker)))

(defun flycheck-locate-config-file (filenames checker)
  "Locate the configuration file for CHECKER, based on FILENAMES.

FILENAMES can be either a single file, or a list.  Each filename
is passed to all `flycheck-locate-config-file-functions', until
one returns non-nil.

Return the absolute path of the configuration file, or nil if no
configuration file was found."
  (when (stringp filenames)
    (setq filenames (list filenames)))
  (let ((config-file nil))
    (while (and filenames (null config-file))
      (setq config-file (run-hook-with-args-until-success
                         'flycheck-locate-config-file-functions
                         (pop filenames) checker)))
    (when (and config-file (file-exists-p config-file))
      config-file)))

(defun flycheck-locate-config-file-by-path (filepath _checker)
  "Locate a configuration file by a FILEPATH.

If FILEPATH contains a path separator, expand it against the
default directory and return it if it points to an existing file.
Otherwise return nil.

_CHECKER is ignored."
  ;; If the path is just a plain file name, skip it.
  (unless (string= (file-name-nondirectory filepath) filepath)
    (let ((file-name (expand-file-name filepath)))
      (and (file-exists-p file-name) file-name))))

(defun flycheck-locate-config-file-ancestor-directories (filename _checker)
  "Locate a configuration FILENAME in ancestor directories.

If the current buffer has a file name, search FILENAME in the
directory of the current buffer and all ancestors thereof (see
`locate-dominating-file').  If the file is found, return its
absolute path.  Otherwise return nil.

_CHECKER is ignored."
  (when-let* ((basefile (buffer-file-name))
              (directory (locate-dominating-file basefile filename)))
    (expand-file-name filename directory)))

(defun flycheck-locate-config-file-home (filename _checker)
  "Locate a configuration FILENAME in the home directory.

Return the absolute path, if FILENAME exists in the user's home
directory, or nil otherwise.  For a remote buffer, the remote
user's home directory is searched."
  (let* ((remote (file-remote-p default-directory))
         (home (if remote (concat remote "~") "~"))
         (path (expand-file-name filename home)))
    (when (file-exists-p path)
      path)))

(seq-do (apply-partially #'custom-add-frequent-value
                         'flycheck-locate-config-file-functions)
        '(flycheck-locate-config-file-by-path
          flycheck-locate-config-file-ancestor-directories
          flycheck-locate-config-file-home))

(defun flycheck-register-option-var (var checkers)
  "Register an option VAR with CHECKERS.

VAR is an option symbol, and CHECKERS a syntax checker symbol or
a list thereof.  Register VAR with all CHECKERS so that it
appears in the help output."
  (when (symbolp checkers)
    (setq checkers (list checkers)))
  (dolist (checker checkers)
    (cl-pushnew var (flycheck-checker-get checker 'option-vars))))

;;;###autoload
(defmacro flycheck-def-option-var (symbol init-value checkers docstring
                                          &rest custom-args)
  "Define SYMBOL as option variable with INIT-VALUE for CHECKERS.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'."
  (declare (indent 3)
           (doc-string 4))
  `(progn
     (defcustom ,symbol ,init-value
       ,(concat docstring "

This variable is an option for the following syntax checkers:

"
                (mapconcat (lambda (c) (format "  - `%s'" c))
                           (if (symbolp checkers) (list checkers) checkers)
                           "\n"))
       :group 'flycheck-options
       ,@custom-args)
     (flycheck-register-option-var ',symbol ',checkers)))

(defun flycheck-option-int (value)
  "Convert an integral option VALUE to a string.

If VALUE is nil, return nil.  Otherwise return VALUE converted to
a string."
  (and value (number-to-string value)))

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
        (when-let* ((value (delq nil (mapcar filter value))))
          (string-join value separator))
      (funcall filter value))))

(defmacro flycheck-def-args-var (symbol checkers &rest custom-args)
  "Define SYMBOL as argument variable for CHECKERS.

SYMBOL is declared as customizable, risky and buffer-local
variable using `defcustom' to provide an option for arbitrary
arguments for the given syntax CHECKERS (either a single checker
or a list of checkers).  CUSTOM-ARGS is forwarded to `defcustom'.

Use the `eval' form to splice this variable into the
`:command'."
  (declare (indent 2))
  `(flycheck-def-option-var ,symbol nil ,checkers
     "A list of additional command line arguments.

The value of this variable is a list of strings with additional
command line arguments."
     :risky t
     :type '(repeat (string :tag "Argument"))
     ,@custom-args))


;;; Command syntax checkers as compile commands
(defun flycheck-checker-pattern-to-error-regexp (pattern)
  "Convert PATTERN into an error regexp for compile.el.

Return a list representing PATTERN, suitable as element in
`compilation-error-regexp-alist'."
  (let* ((regexp (car pattern))
         (level (cdr pattern))
         (level-no (flycheck-error-level-compilation-level level)))
    `(,regexp 1 (2 . 6) (3 . 7) ,level-no)))

(defun flycheck-checker-compilation-error-regexp-alist (checker)
  "Convert error patterns of CHECKER for use with compile.el.

Return an alist of all error patterns of CHECKER, suitable for
use with `compilation-error-regexp-alist'."
  (mapcar #'flycheck-checker-pattern-to-error-regexp
           (flycheck-checker-get checker 'error-patterns)))

(defun flycheck--substitute-shell-command-argument (arg checker)
  "Substitute ARG for CHECKER.

Like `flycheck-substitute-argument', except for source,
source-inplace, and source-original."
  (if (memq arg '(source source-inplace source-original))
      ;; The command runs on the host of `default-directory', so strip any
      ;; remote prefix from the file name.
      (list (file-local-name buffer-file-name))
    (flycheck-substitute-argument arg checker)))

(defun flycheck--checker-substituted-shell-command-arguments (checker)
  "Get the substituted arguments of a CHECKER to run as a shell command.

Substitute each argument of CHECKER using
`flycheck-substitute-shell-command-argument'."
  (seq-mapcat (lambda (arg)
                (flycheck--substitute-shell-command-argument arg checker))
              (flycheck-checker-arguments checker)))

(defun flycheck-checker-shell-command (checker)
  "Get a shell command for CHECKER.

Perform substitution in the arguments of CHECKER, but with
`flycheck--substitute-shell-command-argument'.

Return the command of CHECKER as single string, suitable for
shell execution."
  ;; Note: Do NOT use `combine-and-quote-strings' here.  Despite it's name it
  ;; does not properly quote shell arguments, and actually breaks for special
  ;; characters.  See https://github.com/flycheck/flycheck/pull/522
  (let* ((args (flycheck--checker-substituted-shell-command-arguments checker))
         (program
          (or (flycheck-find-checker-executable checker)
              (user-error "Cannot find `%s' using `flycheck-executable-find'"
                          (flycheck-checker-executable checker))))
         (wrapped (flycheck--wrap-command program args))
         (abs-prog
          ;; The executable path returned by `flycheck-command-wrapper-function'
          ;; may not be absolute, so expand it here.  See URL
          ;; `https://github.com/flycheck/flycheck/issues/1461'.  Resolve it on
          ;; the host the command runs on when `default-directory' is remote.
          (or (executable-find (car wrapped) (file-remote-p default-directory))
              (user-error "Cannot find `%s' using `executable-find'"
                          (car wrapped))))
         (command (mapconcat #'shell-quote-argument
                             (cons abs-prog (cdr wrapped)) " ")))
    (if (flycheck-checker-get checker 'standard-input)
        ;; If the syntax checker expects the source from standard input add an
        ;; appropriate shell redirection
        (concat command " < "
                (shell-quote-argument (file-local-name (buffer-file-name))))
      command)))

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
   (let* ((default (flycheck-get-checker-for-buffer))
          (prompt (concat
                   "Run syntax checker as compile command"
                   (when default (concat " [" (format "%S" default) "]"))
                   ": ")))
     (list (flycheck-read-checker prompt
                                  (when (flycheck-checker-get default 'command)
                                    default)
                                  'command))))
  (unless (flycheck-valid-checker-p checker)
    (user-error "%S is not a valid syntax checker" checker))
  (unless (buffer-file-name)
    (user-error "Cannot compile a buffer without a backing file"))
  (unless (flycheck-may-use-checker checker)
    (user-error "Cannot use syntax checker %S in this buffer" checker))
  (unless (flycheck-checker-executable checker)
    (user-error "Cannot run checker %S as shell command" checker))
  (save-some-buffers)
  (let* ((default-directory (flycheck-compute-working-directory checker))
         (command (flycheck-checker-shell-command checker))
         (buffer (compilation-start command nil #'flycheck-compile-name)))
    (with-current-buffer buffer
      (setq-local compilation-error-regexp-alist
                  (flycheck-checker-compilation-error-regexp-alist checker)))))


;;; General error parsing for command checkers
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (funcall (flycheck-checker-get checker 'error-parser) output checker buffer))

(defun flycheck-handle-suspicious-state (checker exit-status output)
  "Handle suspicious state of given CHECKER.
EXIT-STATUS and OUTPUT are passed to `:handle-suspicious'
function of the CHECKER, if any."
  (if-let* ((handle-suspicious
             (flycheck-checker-get checker 'handle-suspicious)))
      (funcall handle-suspicious checker exit-status output)
    'suspicious))

(defun flycheck-fix-error-filename (err buffer-files cwd)
  "Fix the file name of ERR from BUFFER-FILES.

Resolves error file names relative to CWD directory.

Make the file name of ERR absolute.  If the absolute file name of
ERR is in BUFFER-FILES, replace it with the value of variable
`buffer-file-name'."
  (flycheck-error-with-buffer err
    (when-let* ((filename (flycheck-error-filename err)))
      (when (seq-some (apply-partially #'flycheck-same-files-p
                                       (flycheck--expand-file-name filename cwd))
                      buffer-files)
        (setf (flycheck-error-filename err) buffer-file-name)
        (when (and buffer-file-name (flycheck-error-message err))
          (setf (flycheck-error-message err)
                (replace-regexp-in-string
                 (regexp-quote filename) buffer-file-name
                 (flycheck-error-message err) 'fixed-case 'literal))))))
  err)


;;; Error parsers for command syntax checkers
(defun flycheck-parse-xml-region (beg end)
  "Parse the xml region between BEG and END.

Wrapper around `xml-parse-region' which transforms the return
value of this function into one compatible to
`libxml-parse-xml-region' by simply returning the first element
from the node list."
  (ignore-errors (car (xml-parse-region beg end))))

(defun flycheck-parse-xml-region-with-fallback (beg end)
  "Parse the xml region between BEG and END.

Try parsing with libxml first; if that fails, revert to
`flycheck-parse-xml-region'.  Failures can be caused by incorrect
XML (see URL `https://github.com/flycheck/flycheck/issues/1298'),
or on Windows by a missing libxml DLL with a libxml-enabled Emacs
\(see URL `https://github.com/flycheck/flycheck/issues/1330')."
  (or (and (libxml-available-p)
           (libxml-parse-xml-region beg end))
      (flycheck-parse-xml-region beg end)))

(defvar flycheck-xml-parser 'flycheck-parse-xml-region-with-fallback
  "Function used to parse an xml string from a region.

The default uses libxml if available, and falls back to
`flycheck-parse-xml-region' otherwise.")

(defun flycheck-parse-xml-string (xml)
  "Parse an XML string.

Return the document tree parsed from XML in the form `(ROOT ATTRS
BODY...)'.  ROOT is a symbol identifying the name of the root
element.  ATTRS is an alist of the attributes of the root node.
BODY is zero or more body elements, either as strings (in case of
text nodes) or as XML nodes, in the same form as the root node."
  (with-temp-buffer
    (insert xml)
    (funcall flycheck-xml-parser (point-min) (point-max))))

(defun flycheck-parse-checkstyle (output checker buffer)
  "Parse Checkstyle errors from OUTPUT.

Parse Checkstyle-like XML output.  Use this error parser for
checkers that have an option to output errors in this format.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://checkstyle.org/' for information
about Checkstyle."
  (pcase (flycheck-parse-xml-string output)
    (`(checkstyle ,_ . ,file-nodes)
     (let (errors)
       (dolist (node file-nodes)
         (pcase node
           (`(file ,file-attrs . ,error-nodes)
            (dolist (node error-nodes)
              (pcase node
                (`(error ,error-attrs . ,_)
                 (let-alist error-attrs
                   (push (flycheck-error-new-at
                          (flycheck-string-to-number-safe .line)
                          (flycheck-string-to-number-safe .column)
                          (pcase .severity
                            (`"error"   'error)
                            (`"warning" 'warning)
                            (`"info"    'info)
                            ;; Default to error for unknown .severity
                            (_          'error))
                          .message
                          :checker checker :id .source
                          :buffer buffer
                          :filename (cdr (assq 'name file-attrs)))
                         errors))))))))
       (nreverse errors)))))

(defun flycheck-parse-cppcheck (output checker buffer)
  "Parse Cppcheck errors from OUTPUT.

Parse Cppcheck XML v2 output.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://cppcheck.sourceforge.net/' for more information
about Cppcheck."
  (pcase (flycheck-parse-xml-string output)
    (`(results ,_ . ,body)
     (let (errors)
       (dolist (node body)
         (pcase node
           (`(errors ,_ . ,error-nodes)
            (dolist (node error-nodes)
              (pcase node
                (`(error ,error-attrs . ,loc-nodes)
                 (let ((id (cdr (assq 'id error-attrs)))
                       (message (cdr (assq 'verbose error-attrs)))
                       (level (pcase (cdr (assq 'severity error-attrs))
                                (`"error" 'error)
                                (`"style" 'info)
                                (`"information" 'info)
                                (_ 'warning))))
                   (dolist (node loc-nodes)
                     (pcase node
                       (`(location ,loc-attrs . ,_)
                        (let-alist loc-attrs
                          (push (flycheck-error-new-at
                                 (flycheck-string-to-number-safe .line)
                                 nil
                                 level
                                 ;; cppcheck return newline characters as "\012"
                                 (replace-regexp-in-string "\\\\012" "\n"
                                                           message)
                                 :id id
                                 :checker checker
                                 :buffer buffer
                                 :filename .file)
                                errors))))))))))))
       (nreverse errors)))))

(defun flycheck-parse-phpmd (output checker buffer)
  "Parse phpmd errors from OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://phpmd.org/' for more information about phpmd."
  (pcase (flycheck-parse-xml-string output)
    (`(pmd ,_ . ,body)
     (let (errors)
       (dolist (node body)
         (pcase node
           (`(file ,file-attrs . ,violation-nodes)
            (let ((filename (cdr (assq 'name file-attrs))))
              (dolist (node violation-nodes)
                (pcase node
                  (`(violation ,vio-attrs ,(and message (pred stringp)))
                   (let-alist vio-attrs
                     (push
                      (flycheck-error-new-at
                       (flycheck-string-to-number-safe .beginline)
                       nil
                       'warning (string-trim message)
                       ;; Ignore .endline (phpmd marks giant spans as errors)
                       ;; :end-line (flycheck-string-to-number-safe .endline)
                       :id .rule
                       :checker checker
                       :buffer buffer
                       :filename filename)
                      errors)))))))))
       (nreverse errors)))))

(defun flycheck-parse-reek (output checker buffer)
  "Parse Reek warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/troessner/reek' for more information
about Reek."
  (let ((errors nil))
    (dolist (message (car (flycheck-parse-json output)))
      (let-alist message
        (dolist (line (delete-dups .lines))
          (push
           (flycheck-error-new-at
            line
            nil
            'warning (concat .context " " .message)
            :id .smell_type
            :checker checker
            :buffer buffer
            :filename .source)
           errors))))
    (nreverse errors)))

(defun flycheck-parse-go-staticcheck (output checker buffer)
  "Parse staticcheck warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://staticcheck.io/docs/formatters' for more
information about staticcheck."
  (let ((errors nil))
    (dolist (msg (flycheck-parse-json output))
      (let-alist msg
        (push
         (flycheck-error-new-at
          .location.line
          .location.column
          (pcase .severity
            (`"error"   'error)
            (`"warning" 'warning)
            (`"ignored" 'info)
            ;; Default to warning for unknown .severity
            (_          'warning))
          .message
          :id .code
          :checker checker
          :buffer buffer
          :filename .location.file)
         errors)))
    (nreverse errors)))

(defun flycheck-parse-rust-collect-spans (span)
  "Return a list of spans contained in a SPAN object."
  (let ((spans))
    (let-alist span
      ;; With macro expansion errors, some spans will point to phony file names
      ;; to indicate an error inside the std rust lib.  We skip these spans as
      ;; they won't appear in flycheck anyway.
      (unless (string= .file_name "<std macros>")
        (push span spans))

      ;; Macro expansion errors will have a span in the 'expansion' field, so we
      ;; recursively collect it.
      (if .expansion.span
          (append (flycheck-parse-rust-collect-spans .expansion.span)
                  spans)
        spans))))

(defun flycheck-parse-rustc--fix (spans file buffer)
  "Build a `flycheck-fix' for BUFFER from rustc SPANS in FILE, or nil.

Every span in FILE with a `suggested_replacement' whose
`suggestion_applicability' is \"MachineApplicable\" becomes one
edit, so a multi-part suggestion (e.g. inserting a `(' and a `)')
is applied in full.  Spans in another file -- a suggestion that
reaches into a macro or a different source file -- are dropped, so
the fix never edits this buffer at foreign line and column
numbers."
  (let ((edits
         (delq nil
               (seq-map
                (lambda (span)
                  (let-alist span
                    (when (and (equal .file_name file)
                               .suggested_replacement
                               (equal .suggestion_applicability
                                      "MachineApplicable"))
                      (flycheck-fix-edit-new
                       :line .line_start :column .column_start
                       :end-line .line_end :end-column .column_end
                       :replacement .suggested_replacement))))
                spans))))
    (flycheck--make-fix buffer nil edits)))

(defun flycheck-parse-rustc-diagnostic (diagnostic checker buffer)
  "Turn a rustc DIAGNOSTIC into a `flycheck-error'.

CHECKER and BUFFER denote the CHECKER that returned DIAGNOSTIC
and the BUFFER that was checked respectively.

DIAGNOSTIC should be a parsed JSON object describing a rustc
diagnostic, following the format described there:

https://github.com/rust-lang/rust/blob/master/src/librustc_errors/json.rs#L154"
  (let ((error-message)
        (error-level)
        (error-code)
        (primary-filename)
        (primary-line)
        (primary-column)
        (primary-end-line)
        (primary-end-column)
        (group (make-symbol "group"))
        (spans)
        (children)
        (errors))
    ;; The diagnostic format is described in the link above.  The gist of it is
    ;; that a diagnostic can have several causes in the source text; these
    ;; causes are represented by spans.  The diagnostic has a message and a
    ;; level (error, warning), while the spans have a filename, line, column,
    ;; and an optional label.  The primary span points to the root cause of the
    ;; error in the source text, while non-primary spans point to related
    ;; causes.  Spans may have an 'expansion' field for macro expansion errors;
    ;; these expansion fields will contain another span (and so on).  In
    ;; addition, a diagnostic can also have children diagnostics that are used
    ;; to provide additional information through their message field, but do not
    ;; seem to contain any spans (yet).
    ;;
    ;; We first gather spans in order to turn every span into a flycheck error
    ;; object, that we collect into the `errors' list.

    ;; Nested `let-alist' cause compilation warnings, hence we `setq' all
    ;; these values here first to avoid nesting.
    (let-alist diagnostic
      (setq error-message .message
            error-level (pcase .level
                          (`"error" 'error)
                          (`"warning" 'warning)
                          (`"note" 'info)
                          (_ 'error))
            ;; The 'code' field of the diagnostic contains the actual error
            ;; code and an optional explanation that we ignore
            error-code .code.code
            ;; Collect all spans recursively
            spans (seq-mapcat #'flycheck-parse-rust-collect-spans .spans)
            children .children))

    ;; Turn each span into a flycheck error
    (dolist (span spans)
      (let-alist span
        ;; Children may not have filename/line/column information, so we use
        ;; those from the primary span
        (when .is_primary
          (setq primary-filename .file_name
                primary-line .line_start
                primary-column .column_start
                primary-end-line .line_end
                primary-end-column .column_end))
        (push
         (flycheck-error-new-at
          .line_start
          .column_start
          ;; Non-primary spans are used for notes
          (if .is_primary error-level 'info)
          (if .is_primary
              ;; Primary spans may have labels with additional information
              (concat error-message (when .label
                                      (format " (%s)" .label)))
            ;; If the label is empty, fallback on the error message,
            ;; otherwise we won't be able to display anything
            (or .label error-message))
          :id error-code
          :checker checker
          :buffer buffer
          :filename .file_name
          :group group
          :end-line .line_end
          :end-column .column_end
          :fix (flycheck-parse-rustc--fix (list span) .file_name buffer))
         errors)))

    ;; Then we turn children messages into flycheck errors pointing to the
    ;; location of the primary span.
    (dolist (child children)
      (let* ((message (let-alist child .message))
             (child-spans (let-alist child .spans))
             ;; A child's suggestion may span several places (e.g. inserting
             ;; a `(' and a matching `)'); collect all of them into one fix.
             ;; A child's fix applies to the diagnostic's primary file; drop
             ;; any span that reaches into another file.
             (fix (flycheck-parse-rustc--fix child-spans primary-filename
                                             buffer)))
        (let-alist (car child-spans)
          (push
           (flycheck-error-new-at
            ;; Use the line/column from the first span if there is one, or
            ;; fallback to the line/column information from the primary span of
            ;; the diagnostic.
            (or .line_start primary-line)
            (or .column_start primary-column)
            'info
            ;; Messages from `cargo clippy' may suggest replacement code.  In
            ;; these cases, the `message' field itself is an unhelpful `try' or
            ;; `change this to'.  We add the `suggested_replacement' field in
            ;; these cases.
            (if .suggested_replacement
                (format "%s: `%s`" message .suggested_replacement)
              message)
            :id error-code
            :checker checker
            :buffer buffer
            :filename primary-filename
            :group group
            :end-line (or .line_end primary-end-line)
            :end-column (or .column_end primary-end-column)
            :fix fix)
           errors))))

    ;; If there are no spans, the error is not associated with a specific
    ;; file but with the project as a whole.  We still need to report it to
    ;; the user by emitting a corresponding flycheck-error object.
    ;; Check whether the code is non-nil because Rust≥1.44 includes the
    ;; warning count upon completion.
    (when (and error-code (not spans))
      (push (flycheck-error-new-at
             ;; We have no specific position to attach the error to, so
             ;; let's use the top of the file.
             1 1
             error-level
             error-message
             :id error-code
             :checker checker
             :buffer buffer
             :group group)
            errors))
    (nreverse errors)))

(defconst flycheck--json-parser
  (lambda ()
    (json-parse-buffer
     :object-type 'alist :array-type 'list
     :null-object nil :false-object nil))
  "Function to use to parse JSON strings.")

(defun flycheck-parse-json (output)
  "Return parsed JSON data from OUTPUT.

OUTPUT is a string that contains JSON data.  Each line of OUTPUT
may be either plain text, a JSON array (starting with `['), or a
JSON object (starting with `{').

This function ignores the plain text lines, parses the JSON
lines, and returns the parsed JSON lines in a list."
  (let ((objects nil))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (when (memq (char-after) '(?\{ ?\[))
          (push (funcall flycheck--json-parser) objects))
        (forward-line)))
    (nreverse objects)))

(defun flycheck-parse-sarif--level (level)
  "Map a SARIF result LEVEL string to a Flycheck error level."
  (pcase level
    ("error" 'error)
    ("warning" 'warning)
    ;; \"note\" is advisory, \"none\" carries no severity of its own
    ((or "note" "none") 'info)
    ;; SARIF defaults an unspecified level to \"warning\"
    (_ 'warning)))

(defun flycheck-parse-sarif--fix (fixes uri buffer)
  "Build a `flycheck-fix' for BUFFER from a SARIF result's FIXES array, or nil.

Use the first fix, and only its artifact changes targeting URI --
the file the diagnostic (and BUFFER) is for -- so a fix that also
edits other files never applies their changes to this buffer.
Each change's replacements carry a `deletedRegion' to replace with
`insertedContent'."
  (when fixes
    (let-alist (elt fixes 0)
      (flycheck--make-fix
       buffer .description.text
       (seq-mapcat
        (lambda (change)
          (let-alist change
            (when (equal .artifactLocation.uri uri)
              (seq-map
               (lambda (replacement)
                 (let-alist replacement
                   (flycheck-fix-edit-new
                    :line .deletedRegion.startLine
                    :column .deletedRegion.startColumn
                    :end-line .deletedRegion.endLine
                    :end-column .deletedRegion.endColumn
                    :replacement (or .insertedContent.text ""))))
               .replacements))))
        .artifactChanges)))))

(defun flycheck-parse-sarif (output checker buffer)
  "Parse SARIF errors from OUTPUT.

Parse output in the Static Analysis Results Interchange Format
\(SARIF) 2.1.0.  Use this error parser for checkers that have an
option to output errors in this format.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://sarifweb.azurewebsites.net/' for more
information about SARIF."
  (let-alist (car (flycheck-parse-json output))
    (seq-mapcat
     (lambda (run)
       (let-alist run
         ;; The rules of the run's driver supply the id and default level
         ;; of a result that omits them
         (let ((rules .tool.driver.rules))
           (seq-mapcat
            (lambda (result)
              (let-alist result
                (let* ((rule
                        ;; A result references its rule by index into the
                        ;; rules array, or by id
                        (or (and (natnump .ruleIndex)
                                 (nth .ruleIndex rules))
                            (and .ruleId
                                 (seq-find (lambda (r)
                                             (equal (alist-get 'id r)
                                                    .ruleId))
                                           rules))))
                       (id (or .ruleId (alist-get 'id rule)))
                       (level (flycheck-parse-sarif--level
                               (or .level
                                   (let-alist rule
                                     .defaultConfiguration.level))))
                       (message .message.text)
                       (fixes .fixes))
                  (if .locations
                      (seq-map
                       (lambda (location)
                         (let-alist location
                           (let* ((start-line
                                   .physicalLocation.region.startLine)
                                  (start-col
                                   .physicalLocation.region.startColumn)
                                  (end-line
                                   .physicalLocation.region.endLine)
                                  (end-col
                                   .physicalLocation.region.endColumn)
                                  ;; A zero-width region carries no span:
                                  ;; endColumn equals startColumn on the same
                                  ;; line (endLine defaults to startLine per
                                  ;; the SARIF spec).  Some tools emit these
                                  ;; for line-level findings, so treat them as
                                  ;; the whole line -- drop the column and end
                                  ;; and let the highlighting mode take over,
                                  ;; rather than highlight an empty range.
                                  (zero-width
                                   (and end-col
                                        (equal end-col start-col)
                                        (or (null end-line)
                                            (equal end-line start-line)))))
                             (flycheck-error-new-at
                              start-line (unless zero-width start-col)
                              level message
                              :id id
                              :checker checker
                              :buffer buffer
                              :filename
                              (flycheck-parse-sarif--uri
                               .physicalLocation.artifactLocation.uri)
                              :end-line (unless zero-width end-line)
                              :end-column (unless zero-width end-col)
                              ;; Only the fix changes for this location's file.
                              :fix (flycheck-parse-sarif--fix
                                    fixes
                                    .physicalLocation.artifactLocation.uri
                                    buffer)))))
                       .locations)
                    ;; A result without a location applies to the whole run
                    (list (flycheck-error-new-at
                           nil nil level message
                           :id id :checker checker :buffer buffer))))))
            .results))))
     .runs)))

(defun flycheck--file-uri-to-path (uri)
  "Strip a `file://' scheme and authority from URI and percent-decode it.

Both file:///abs/path and file://host/abs/path leave the leading slash of
the path.  `url-unhex-string' returns the raw bytes of the percent
escapes, which must be decoded, as file URIs percent-encode UTF-8."
  (when uri
    (when (string-match "\\`file://[^/]*" uri)
      (setq uri (substring uri (match-end 0))))
    (decode-coding-string (url-unhex-string uri) 'utf-8)))

(defun flycheck-parse-sarif--uri (uri)
  "Turn a SARIF artifact-location URI into a file name.

Strip a `file://' scheme and percent-decode URI; return relative URIs
unchanged, for Flycheck to expand against the working directory."
  (and uri (flycheck--file-uri-to-path uri)))

(defun flycheck-parse-rustc (output checker buffer)
  "Parse rustc errors from OUTPUT and return a list of `flycheck-error'.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

The expected format for OUTPUT is a mix of plain text lines and
JSON lines.  This function ignores the plain text lines and
parses only JSON lines.  Each JSON line is expected to be a JSON
object that corresponds to a diagnostic from the compiler.  The
expected diagnostic format is described there:

https://github.com/rust-lang/rust/blob/master/src/libsyntax/json.rs#L67-L139"
  (seq-mapcat (lambda (msg)
                (flycheck-parse-rustc-diagnostic msg checker buffer))
              (flycheck-parse-json output)))

(defun flycheck-parse-cargo-rustc (output checker buffer)
  "Parse Cargo errors from OUTPUT and return a list of `flycheck-error'.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

The expected format for OUTPUT is a mix of plain text lines and
JSON lines.  This function ignores the plain text lines and
parses only JSON lines.  Each JSON line is expected to be a JSON
object that represents a message from Cargo.  The format of
messages emitted by Cargo is described in cargo's
machine_message.rs at URL `https://github.com/rust-lang/cargo/blob/master/src/cargo/util/machine_message.rs'."
  (let ((errors))
    (dolist (msg (flycheck-parse-json output))
      (let-alist msg
        ;; Errors and warnings from rustc are wrapped by cargo, so we filter and
        ;; unwrap them, and delegate the actual construction of `flycheck-error'
        ;; objects to `flycheck-parse-rustc-diagnostic'.
        ;; We put the error record with nil code since flycheck regards
        ;; the case of nonzero return code without any error report
        ;; as abnormal result.
        (when (string= .reason "compiler-message")
          (push (flycheck-parse-rustc-diagnostic .message checker buffer)
                errors))))
    (apply #'nconc errors)))

;; Some checkers output ANSI terminal colors, which don't match up
;; with :error-patterns, so we strip those color codes from the output
;; here before passing it along to the default behavior. This is
;; originally only used in the rebar3 checker, but the systemd checker
;; now also makes use of it.
;;
;; The relevant discussion can be found at
;; https://github.com/flycheck/flycheck/pull/1144
(defun flycheck-parse-with-patterns-without-color (output checker buffer)
  "Strip color codes from OUTPUT before passing it to the default behavior.

CHECKER and BUFFER are passed along as well."
  (flycheck-parse-with-patterns
   (ansi-color-filter-apply output)
   checker buffer))


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
see `flycheck-parse-error-with-patterns'.

Return a list of error tokens."
  (let ((regexp (flycheck-get-regexp patterns))
        (last-match 0)
        errors)
    (while (string-match regexp output last-match)
      (push (match-string 0 output) errors)
      (setq last-match (match-end 0)))
    (nreverse errors)))

(defun flycheck-try-parse-error-with-pattern (err pattern checker)
  "Try to parse a single ERR with a PATTERN for CHECKER.

Return the parsed error if PATTERN matched ERR, or nil
otherwise.

`end-line' defaults to the value of `line' when `end-column' is
set, since checkers often omit redundant end lines (as in
<file>:<line>:<column>-<end-column>)."
  (let ((regexp (car pattern))
        (level (cdr pattern)))
    (when (string-match regexp err)
      (let ((filename (match-string 1 err))
            (line (flycheck-string-to-number-safe (match-string 2 err)))
            (column (flycheck-string-to-number-safe (match-string 3 err)))
            (message (match-string 4 err))
            (id (match-string 5 err))
            (end-line (flycheck-string-to-number-safe (match-string 6 err)))
            (end-column (flycheck-string-to-number-safe (match-string 7 err))))
        (flycheck-error-new-at
         line
         column
         level
         (unless (string-empty-p message) message)
         :id (unless (string-empty-p id) id)
         :checker checker
         :filename (if (or (null filename) (string-empty-p filename))
                       (buffer-file-name)
                     filename)
         :end-line (or end-line (and end-column line))
         :end-column end-column)))))

(defun flycheck-parse-error-with-patterns (err patterns checker)
  "Parse a single ERR with error PATTERNS for CHECKER.

Apply each pattern in PATTERNS to ERR, in the given order, and
return the first parsed error."
  ;; Try to parse patterns in the order of declaration to make sure that the
  ;; first match wins.
  (let (parsed-error)
    (while (and patterns
                (not (setq parsed-error
                           (flycheck-try-parse-error-with-pattern
                            err (car patterns) checker))))
      (setq patterns (cdr patterns)))
    parsed-error))

(defun flycheck-parse-with-patterns (output checker buffer)
  "Parse OUTPUT from CHECKER with error patterns.

Uses the error patterns of CHECKER to tokenize the output and
tries to parse each error token with all patterns, in the order
of declaration.  Hence an error is never matched twice by two
different patterns.  The pattern declared first always wins.

BUFFER is the buffer being checked.

Return a list of parsed errors and warnings (as `flycheck-error'
objects)."
  (with-current-buffer buffer
    (let ((patterns (flycheck-checker-get checker 'error-patterns)))
      (mapcar (lambda (err)
                 (flycheck-parse-error-with-patterns err patterns checker))
               (flycheck-tokenize-output-with-patterns output patterns)))))


;;; Convenience definition of command-syntax checkers

;; This macro is autoloaded to prevent `with-eval-after-load' from expanding its
;; arguments.  See https://github.com/flycheck/flycheck/issues/1398.
;;;###autoload
(defmacro flycheck-define-checker (symbol docstring &rest properties)
  "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'."
  (declare (indent 1)
           (doc-string 2))
  (let ((command (plist-get properties :command))
        (parser (plist-get properties :error-parser))
        (filter (plist-get properties :error-filter))
        (handle-suspicious (plist-get properties :handle-suspicious))
        (explainer (plist-get properties :error-explainer))
        (predicate (plist-get properties :predicate))
        (enabled-fn (plist-get properties :enabled))
        (verify-fn (plist-get properties :verify)))

    `(progn
       (flycheck-def-executable-var ,symbol ,(car command))

       (flycheck-define-command-checker ',symbol
         ,docstring
         :command ',command
         ,@(when parser
             `(:error-parser #',parser))
         :error-patterns ',(plist-get properties :error-patterns)
         ,@(when filter
             `(:error-filter #',filter))
         ,@(when handle-suspicious
             `(:handle-suspicious #',handle-suspicious))
         ,@(when explainer
             ;; A symbol or a `lambda' form names the explainer directly; any
             ;; other form is evaluated, so `:error-explainer' can be produced
             ;; by a helper such as `flycheck-error-explainer-from-url'.
             `(:error-explainer ,(if (or (symbolp explainer)
                                         (eq (car-safe explainer) 'lambda))
                                     `#',explainer
                                   explainer)))
         :modes ',(plist-get properties :modes)
         ,@(when predicate
             `(:predicate #',predicate))
         :next-checkers ',(plist-get properties :next-checkers)
         ,@(when enabled-fn
             `(:enabled #',enabled-fn))
         ,@(when verify-fn
             `(:verify #',verify-fn))
         :standard-input ',(plist-get properties :standard-input)
         :working-directory ',(plist-get properties :working-directory)))))


;;; LSP diagnostics
;;
;; Shared machinery for turning a Language Server Protocol diagnostic into a
;; `flycheck-error'.  Both the Eglot bridge (`eglot-check', below) and the
;; native `flycheck-lsp' checker use it, so the two produce identical errors -- the
;; same level, id, related locations and codeDescription -- regardless of how
;; the diagnostic reached Flycheck.  These helpers operate on the raw LSP
;; diagnostic object (a plist), not on any client's data structures.

(defun flycheck-lsp--severity-level (severity)
  "Map an LSP diagnostic SEVERITY (1-4) to a Flycheck error level.
A missing severity is treated as an error, as Eglot does."
  (pcase severity
    (1 'error)
    (2 'warning)
    (3 'info)
    (4 'info)                           ; LSP \"hint\"
    (_ 'error)))

(defun flycheck-lsp--diagnostic-id (lsp)
  "Return the Flycheck error id for the LSP diagnostic plist LSP.

Built from the diagnostic's `code', carrying its `codeDescription' href
\(if any) as an `explainer-url' text property so
`flycheck-explain-error-at-point' can open it."
  (when-let* ((code (plist-get lsp :code)))
    (let ((id (format "%s" code))
          (href (plist-get (plist-get lsp :codeDescription) :href)))
      (if href (propertize id 'explainer-url href) id))))

(defun flycheck-lsp--uri-to-path (uri)
  "Convert a `file:' URI to a local file path.

Percent-decoding and authority stripping are shared with the SARIF parser
via `flycheck--file-uri-to-path'; this additionally trims the leading
slash of a Windows drive URI (file:///c:/...).  A non-`file:' URI is
returned unchanged."
  (if (string-prefix-p "file://" uri)
      (let ((path (flycheck--file-uri-to-path uri)))
        (if (string-match-p "\\`/[a-zA-Z]:" path) (substring path 1) path))
    uri))

(defun flycheck-lsp--path-to-uri (path)
  "Return a `file:' URI for the local PATH."
  (let ((enc (url-hexify-string (expand-file-name path)
                                (cons ?/ url-unreserved-chars))))
    (concat "file://" (if (string-prefix-p "/" enc) enc (concat "/" enc)))))

(defun flycheck-lsp--related-location (info)
  "Convert one LSP `relatedInformation' entry INFO to a related location.

INFO is a plist with a `location' (a `uri' and a `range') and a
`message'.  LSP positions are 0-based; Flycheck columns are 1-based, so
each is incremented.  The range end is exclusive in both, so it maps
directly to Flycheck's right-open end column."
  (let* ((location (plist-get info :location))
         (uri (plist-get location :uri))
         (range (plist-get location :range))
         (start (plist-get range :start))
         (end (plist-get range :end)))
    (flycheck-related-location-new
     :filename (and uri (flycheck-lsp--uri-to-path uri))
     :line (and start (1+ (plist-get start :line)))
     :column (and start (1+ (plist-get start :character)))
     :end-line (and end (1+ (plist-get end :line)))
     :end-column (and end (1+ (plist-get end :character)))
     :message (plist-get info :message))))

(defun flycheck-lsp--related-locations (lsp)
  "Return the related locations of the LSP diagnostic plist LSP, as a list.

Maps the diagnostic's `relatedInformation' entries to
`flycheck-related-location' objects; nil when there are none."
  (mapcar #'flycheck-lsp--related-location
          (append (plist-get lsp :relatedInformation) nil)))

(defconst flycheck-lsp--diagnostic-tags
  '((1 . unnecessary) (2 . deprecated))
  "Map of LSP `DiagnosticTag' codes to the symbols Flycheck uses.")

(defun flycheck-lsp--tags (lsp)
  "Return the tags of the LSP diagnostic plist LSP, as a list of symbols.

Unknown codes are dropped rather than passed through: a tag Flycheck has
no rendering for would only be a symbol nothing acts on."
  (delq nil
        (mapcar (lambda (tag) (alist-get tag flycheck-lsp--diagnostic-tags))
                (append (plist-get lsp :tags) nil))))

(defun flycheck-lsp--text-edit (tedit)
  "Convert the LSP TextEdit TEDIT to a `flycheck-fix-edit'.
LSP positions are zero-based and end-exclusive; Flycheck's are one-based
and end-exclusive, so only the line/column need incrementing."
  (let ((start (plist-get (plist-get tedit :range) :start))
        (end (plist-get (plist-get tedit :range) :end)))
    (flycheck-fix-edit-new
     :line (1+ (plist-get start :line))
     :column (1+ (plist-get start :character))
     :end-line (1+ (plist-get end :line))
     :end-column (1+ (plist-get end :character))
     :replacement (plist-get tedit :newText))))

(defun flycheck-lsp--workspace-edit-fix (wedit description)
  "Build a `flycheck-fix' from the LSP WorkspaceEdit WEDIT, or nil.

A fix is built only when WEDIT edits a single file that is the current
buffer, since a `flycheck-fix' applies to one buffer; a multi-file edit,
or one with resource operations (create, rename, delete), is declined.
DESCRIPTION becomes the fix's description.  The tick is the buffer's
current one: the edit was just fetched against this buffer state, and the
fix is applied right after."
  (let* ((this (buffer-file-name))
         (dchanges (append (plist-get wedit :documentChanges) nil))
         (targets
          (cond
           ;; `documentChanges' with a resource operation (an entry without a
           ;; `:textDocument') is not a plain text fix; decline it whole.
           ((seq-some (lambda (tde) (null (plist-get tde :textDocument)))
                      dchanges)
            nil)
           ;; `documentChanges' (preferred): array of TextDocumentEdit.
           (dchanges
            (mapcar (lambda (tde)
                      (cons (flycheck-lsp--uri-to-path
                             (plist-get (plist-get tde :textDocument) :uri))
                            (plist-get tde :edits)))
                    dchanges))
           ;; `changes' (legacy): a JSON object of uri -> edits, which jsonrpc
           ;; decodes to a plist whose keys are keywords (`:file:///...'), so
           ;; strip the leading colon before resolving each URI.
           (t
            (cl-loop for (uri edits) on (plist-get wedit :changes) by #'cddr
                     collect (cons (flycheck-lsp--uri-to-path
                                    (if (keywordp uri)
                                        (substring (symbol-name uri) 1)
                                      uri))
                                   edits))))))
    (when (and this
               (= (length targets) 1)
               (flycheck-same-files-p (caar targets) this)
               (cdar targets))
      (flycheck-fix-new
       :description description
       :edits (mapcar #'flycheck-lsp--text-edit (append (cdar targets) nil))
       :tick (buffer-chars-modified-tick)))))

(defun flycheck-lsp--count-push (rechecked)
  "Record a diagnostics push, RECHECKED non-nil if it triggered a check."
  (cl-incf flycheck-lsp--push-count)
  (when rechecked (flycheck-lsp--count-recheck))
  (setq flycheck-lsp--last-push-time (float-time))
  (unless flycheck-lsp--first-push-time
    (setq flycheck-lsp--first-push-time flycheck-lsp--last-push-time)))

(defun flycheck-lsp--count-recheck ()
  "Record that a diagnostics push started a check.
Separate from `flycheck-lsp--count-push' for a bridge that decides to
check once the reports of an answer have all arrived, rather than on the
report that happens to carry the change."
  (cl-incf flycheck-lsp--recheck-count))

(defconst flycheck-lsp--bridges '(eglot-check flycheck-lsp)
  "The LSP bridge checkers, in the order they run when both are active.

Eglot comes first because it is the full language server, with the
single-purpose lint server behind it.  The order is fixed rather than
decided by whichever mode happened to enable last, and it is what keeps
the chaining acyclic: a bridge only ever chains forwards.")

(defun flycheck-lsp--primary-bridge ()
  "Return the bridge checker that should start the chain in this buffer.

The first of `flycheck-lsp--bridges' whose mode is on, or nil when
neither is.  Both bridges compute the buffer's checker with this, so
enabling them in either order settles on the same one."
  (seq-find (lambda (checker)
              (pcase checker
                ('eglot-check (flycheck-eglot--enabled-p))
                ('flycheck-lsp (flycheck-lsp--enabled-p))))
            flycheck-lsp--bridges))

(defun flycheck-lsp--select-primary-bridge ()
  "Point the buffer's `flycheck-checker' at `flycheck-lsp--primary-bridge'.

Leave a checker the user selected by hand alone, and fall back to
automatic selection once no bridge is left."
  (when (or (null flycheck-checker)
            (memq flycheck-checker flycheck-lsp--bridges))
    (setq flycheck-checker (flycheck-lsp--primary-bridge))))

(defun flycheck-lsp--register-checker (checker exclusive)
  "Teach CHECKER the current buffer's major mode and set up its chaining.

With EXCLUSIVE non-nil, CHECKER reports alone.  Otherwise it chains to
the bridges after it in `flycheck-lsp--bridges', so an Eglot server and a
lint server both contribute, and then to the first command checker that
supports the mode, so a command checker contributes too.

Chaining to a bridge is safe whether or not that bridge is on in a given
buffer: its predicate refuses the buffer, and Flycheck moves on to the
next entry.  That matters because `next-checkers' is a property of the
checker, shared by every buffer, while the modes are buffer-local.

Shared by the `flycheck-lsp' and `eglot-check' bridges."
  (unless (flycheck-checker-supports-major-mode-p checker major-mode)
    (flycheck-add-mode checker major-mode))
  ;; Rebuild from scratch, so enabling a mode repeatedly cannot pile up
  ;; duplicate entries
  (setf (flycheck-checker-get checker 'next-checkers) nil)
  (unless exclusive
    (dolist (bridge (cdr (memq checker flycheck-lsp--bridges)))
      (flycheck-add-next-checker checker bridge 'append))
    (when-let* ((next (seq-find
                       (lambda (c)
                         (and (not (memq c flycheck-lsp--bridges))
                              (flycheck-checker-supports-major-mode-p c major-mode)))
                       flycheck-checkers)))
      (flycheck-add-next-checker checker next 'append))))


;;; Native LSP diagnostics client
;;
;; The `flycheck-lsp' checker talks to a diagnostics language server directly, over
;; the built-in `jsonrpc' library -- no Eglot required.  It is meant for the
;; single-purpose LSP linters (`rubocop --lsp', `ruff server', ...), letting
;; Flycheck use them like any other checker without handing the buffer to a
;; full LSP client.
;;
;; LSP pushes diagnostics whenever it wants, which does not line up with
;; Flycheck's on-demand model.  The reconciliation mirrors the Eglot bridge:
;; a `:start' syncs the buffer to its server and reports the diagnostics
;; cached so far; the server's later `publishDiagnostics' push updates the
;; cache and re-triggers a check so the fresh diagnostics reach the buffer.
;; The `initialize' handshake runs asynchronously, so starting a server
;; never blocks Emacs; a check that arrives before it finishes reports
;; nothing and is re-run when the handshake completes.
;;
;; One server process is shared per (project root, command); a buffer opens
;; its document on the matching server on the first check and closes it when
;; the buffer or the mode goes away.  The server itself is kept for the rest
;; of the session and shut down only when Emacs exits, so reopening or
;; checking another of its files does not pay to restart it.

(declare-function jsonrpc-request "jsonrpc" (connection method params &rest _))
(declare-function jsonrpc-async-request "jsonrpc" (connection method params &rest _))
(declare-function jsonrpc-notify "jsonrpc" (connection method params))
(declare-function jsonrpc-shutdown "jsonrpc" (connection &optional cleanup))
(declare-function jsonrpc-running-p "jsonrpc" (connection))
(declare-function project-current "project" (&optional maybe-prompt directory))

(defcustom flycheck-lsp-servers
  '((ruby-mode "rubocop" "--lsp")
    (ruby-ts-mode "rubocop" "--lsp")
    (python-mode "ruff" "server")
    (python-ts-mode "ruff" "server")
    (js-mode "biome" "lsp-proxy")
    (js-ts-mode "biome" "lsp-proxy")
    (typescript-ts-mode "biome" "lsp-proxy")
    (tsx-ts-mode "biome" "lsp-proxy")
    (json-mode "biome" "lsp-proxy")
    (json-ts-mode "biome" "lsp-proxy")
    (jsonc-mode "biome" "lsp-proxy")
    (css-mode "biome" "lsp-proxy")
    (css-ts-mode "biome" "lsp-proxy")
    (markdown-mode "harper-ls" "--stdio")
    (gfm-mode "harper-ls" "--stdio"))
  "Alist mapping a major mode to a diagnostics LSP server command.

Each entry is (MAJOR-MODE PROGRAM ARG...): a buffer in MAJOR-MODE that
enables `flycheck-lsp-mode' has PROGRAM started with the ARGs as an LSP
server, is fed the buffer's text, and reports the diagnostics the server
pushes back through the `flycheck-lsp' checker.

The default entries are linters that ship a native LSP server and lint out
of the box, with no project configuration: RuboCop (Ruby), Ruff (Python),
Biome (JavaScript, TypeScript, JSON, CSS) and Harper (Markdown prose).  An
entry is only used when its PROGRAM is on the variable `exec-path' (see
`executable-find'), so listing a server you have not installed is
harmless.

A major mode maps to a single server.  To use a different one, replace
the entry -- e.g. Standard instead of RuboCop for Ruby, or Oxlint instead
of Biome for JavaScript:

    (setf (alist-get \\='ruby-mode flycheck-lsp-servers)
          \\='(\"standardrb\" \"--lsp\"))

To cover a mode that has no default, add an entry.  Some linters ship an
LSP server but need project configuration to report anything (a TFLint
config and plugins, a Buf module), so they are left out of the defaults;
add them once your project is set up:

    (add-to-list \\='flycheck-lsp-servers
                 \\='(terraform-mode \"tflint\" \"--langserver\"))

The server needs to speak LSP over stdio and publish diagnostics.  For a
full language server (hover, completion, rename) you usually want Eglot
and `flycheck-eglot-mode' instead."
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (repeat (string :tag "Command argument")))
  :group 'flycheck
  :package-version '(flycheck . "38"))

(defcustom flycheck-lsp-exclusive t
  "Whether the `flycheck-lsp' checker is the only checker or chains to others.

When non-nil (the default), a buffer using `flycheck-lsp-mode' reports
only the language server's diagnostics.  When nil, `flycheck-lsp' chains to
the first command checker that supports the buffer's major mode, so the
server and a command checker can both contribute.

To run this server behind Eglot's rather than instead of it, turn
`flycheck-eglot-mode' on as well and clear `flycheck-eglot-exclusive' too;
Eglot leads and `flycheck-lsp' follows.

Note that this takes effect globally when a buffer enables the mode: it is
stored in `flycheck-lsp's chain, not per buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'flycheck
  :package-version '(flycheck . "38"))

(defcustom flycheck-lsp-initialize-timeout 5
  "Seconds to wait for a language server to answer `initialize'.

The handshake runs asynchronously and does not block Emacs: the first
check of a buffer whose server is still starting reports no diagnostics,
and the buffer is rechecked once the handshake finishes.  A server that
does not answer within this many seconds is torn down and retried on the
next check."
  :type 'number
  :safe #'numberp
  :group 'flycheck
  :package-version '(flycheck . "38"))

(defcustom flycheck-lsp-code-actions t
  "Whether the `flycheck-lsp' checker offers LSP quick-fix code actions as fixes.

When non-nil (the default) and the server advertises code actions, each
diagnostic carries a lazy fix (see `flycheck-error-resolve-fix') that
requests the server's \"quickfix\" code action for it when applied with
\\[flycheck-fix-error-at-point].  Because the fix is only computed on
demand, Flycheck cannot know in advance whether the server has an action,
so these diagnostics carry no fix indicator - try
\\[flycheck-fix-error-at-point] to ask.  Set this to nil to turn the
feature off."
  :type 'boolean
  :safe #'booleanp
  :group 'flycheck
  :package-version '(flycheck . "38"))

(cl-defstruct (flycheck-lsp--doc (:constructor flycheck-lsp--doc-create)
                                 (:copier nil))
  "The state of one document open on a server.
VERSION is nil until the document has been opened."
  buffer version tick
  (diags nil))                          ; latest raw LSP diagnostics

(cl-defstruct (flycheck-lsp--server (:constructor flycheck-lsp--server-create)
                                    (:copier nil))
  "A running diagnostics LSP server and the state of its open documents.

The `documents' table maps a document's canonical path (see
`flycheck-lsp--doc-key') to a `flycheck-lsp--doc'.  `capabilities' is the
server's advertised capability plist from its `initialize' reply, filled
in once `initialized' turns non-nil (the handshake runs asynchronously)."
  connection root command stderr capabilities initialized
  (documents (make-hash-table :test 'equal)))

(defvar flycheck-lsp--servers (make-hash-table :test 'equal)
  "Live `flycheck-lsp--server's, keyed by (ROOT . COMMAND).")

(defvar-local flycheck-lsp--suppress-recheck nil
  "When non-nil, a diagnostics push does not re-trigger a check.
Bound while a push-triggered check runs, so it cannot recurse.  Buffer-local
so a synchronous round-trip during one buffer's check cannot suppress a
push that arrives for another buffer.")

(defun flycheck-lsp--command (mode)
  "Return the LSP server command configured for major MODE, or nil."
  (alist-get mode flycheck-lsp-servers))

(defvar-local flycheck-lsp--command-cache nil
  "Cached (MODE . RESULT) of `flycheck-lsp--available-command' for this buffer.
A server installed mid-session is not noticed until the cache is rebuilt
\(on a major-mode change, which clears buffer-local variables).")

(defun flycheck-lsp--available-command (mode)
  "Return the server command for MODE if its program is installed, else nil.

Uses `flycheck-executable-find', so it honours the user's setting and TRAMP.
The result is cached buffer-locally, keyed on MODE: `executable-find'
scans the variable `exec-path' (and probes the remote host over TRAMP),
and the `flycheck-lsp' checker's predicate calls this on every check."
  (if (eq (car flycheck-lsp--command-cache) mode)
      (cdr flycheck-lsp--command-cache)
    (let ((result (when-let* ((command (flycheck-lsp--command mode)))
                    (and (funcall flycheck-executable-find (car command))
                         command))))
      (setq-local flycheck-lsp--command-cache (cons mode result))
      result)))

(defun flycheck-lsp--language-id (mode)
  "Return a best-effort LSP languageId string for major MODE."
  (replace-regexp-in-string "\\(?:-ts\\)?-mode\\'" "" (symbol-name mode)))

(defvar-local flycheck-lsp--cached-root nil
  "Cached workspace root for this buffer; see `flycheck-lsp--root'.")

(defun flycheck-lsp--root ()
  "Return the workspace root for the current buffer, as an absolute path.
The result is cached buffer-locally, as `project-current' is not free and
the root does not change over a buffer's life."
  (or flycheck-lsp--cached-root
      (setq flycheck-lsp--cached-root
            (or (when-let* ((project (and (fboundp 'project-current)
                                          (project-current))))
                  (expand-file-name (project-root project)))
                (and buffer-file-name (file-name-directory buffer-file-name))
                (expand-file-name default-directory)))))

(defun flycheck-lsp--buffer-uri ()
  "Return the `file:' URI of the current buffer's file, or nil."
  (and buffer-file-name (flycheck-lsp--path-to-uri buffer-file-name)))

(defun flycheck-lsp--doc-key (uri)
  "Return the canonical key (an absolute path) for the document URI.

The client and the server may spell the same file's URI differently (a
re-encoded percent escape, an authority component, a Windows drive
case).  Keying open documents and their diagnostics on the decoded,
expanded path -- rather than the raw URI -- makes both sides agree."
  (expand-file-name (flycheck-lsp--uri-to-path uri)))

(defun flycheck-lsp--server-live-p (server)
  "Return non-nil when SERVER's connection is still running."
  (let ((conn (flycheck-lsp--server-connection server)))
    (and conn (jsonrpc-running-p conn))))

(defun flycheck-lsp--handle-notification (server method params)
  "Handle an LSP notification METHOD with PARAMS from SERVER.

Only `textDocument/publishDiagnostics' is used: cache the diagnostics on
their document and, if a live buffer owns it, re-trigger its check so the
fresh diagnostics are published (guarded against recursion)."
  (when (eq method 'textDocument/publishDiagnostics)
    (let* ((doc (flycheck-lsp--document
                 server (flycheck-lsp--doc-key (plist-get params :uri))))
           (new (append (plist-get params :diagnostics) nil))
           ;; A push repeating what we already hold changes nothing about
           ;; the buffer, and servers republish freely while they index
           (changed (not (equal new (flycheck-lsp--doc-diags doc)))))
      (when changed
        (setf (flycheck-lsp--doc-diags doc) new))
      (when-let* ((buffer (flycheck-lsp--doc-buffer doc))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (let ((recheck (and changed
                              flycheck-mode
                              (not flycheck-lsp--suppress-recheck))))
            (flycheck-lsp--count-push recheck)
            (when recheck
              (let ((flycheck-lsp--suppress-recheck t))
                (flycheck-buffer-automatically)))))))))

(defun flycheck-lsp--document (server key)
  "Return the `flycheck-lsp--doc' for KEY on SERVER, creating it if needed."
  (or (gethash key (flycheck-lsp--server-documents server))
      (puthash key (flycheck-lsp--doc-create)
               (flycheck-lsp--server-documents server))))

(defun flycheck-lsp--initialize-params (root)
  "Return the LSP `initialize' params for a server rooted at ROOT."
  (list :processId (emacs-pid)
        :rootUri (flycheck-lsp--path-to-uri root)
        :capabilities
        (list :textDocument
              (list :publishDiagnostics '(:relatedInformation t)
                    ;; Advertise that we can apply a quickfix's edit, so a
                    ;; server that gates code actions on client support offers
                    ;; them (see `flycheck-lsp--code-action-fix').
                    :codeAction
                    '(:codeActionLiteralSupport
                      (:codeActionKind (:valueSet ["quickfix"])))))))

(defun flycheck-lsp--server-key (server)
  "Return SERVER's key in `flycheck-lsp--servers'."
  (cons (flycheck-lsp--server-root server)
        (flycheck-lsp--server-command server)))

(defun flycheck-lsp--on-initialized (server result)
  "Finish SERVER's handshake with the `initialize' RESULT.

Store the server's capabilities, send the `initialized' notification, and
re-trigger a check in every buffer that opened a document while the
handshake was still in flight, so their diagnostics finally come through.
A no-op if the connection died in the meantime."
  (when (flycheck-lsp--server-live-p server)
    (setf (flycheck-lsp--server-capabilities server)
          (plist-get result :capabilities)
          (flycheck-lsp--server-initialized server) t)
    (flycheck-lsp--notify server 'initialized (make-hash-table :test 'eq))
    (maphash (lambda (_key doc)
               (when-let* ((buffer (flycheck-lsp--doc-buffer doc))
                           ((buffer-live-p buffer)))
                 (with-current-buffer buffer
                   (when flycheck-mode (flycheck-buffer-automatically)))))
             (flycheck-lsp--server-documents server))))

(defun flycheck-lsp--init-failed (server reason)
  "Tear SERVER down after its handshake failed for REASON.
Remove it from the registry so a later check starts a fresh one."
  (message "Flycheck LSP: %s failed to initialize (%s)"
           (car (flycheck-lsp--server-command server)) reason)
  (flycheck-lsp--shutdown-server server)
  (remhash (flycheck-lsp--server-key server) flycheck-lsp--servers))

(defun flycheck-lsp--start-server (root command)
  "Start an LSP server running COMMAND under ROOT and initialize it.

The `initialize' handshake runs asynchronously, so this returns the
`flycheck-lsp--server' before it is ready (its `initialized' slot is still
nil).  When the reply arrives, `flycheck-lsp--on-initialized' finishes the
handshake and re-checks the waiting buffers; a failure or timeout tears
the server down.  Return nil if the process could not be spawned at all."
  (require 'jsonrpc)
  (add-hook 'kill-emacs-hook #'flycheck-lsp--shutdown-all)
  (let* ((default-directory root)
         (name (format "flycheck-lsp:%s" (car command)))
         (stderr (get-buffer-create (format " *%s stderr*" name)))
         (server (flycheck-lsp--server-create :root root :command command
                                              :stderr stderr))
         (proc (make-process
                :name name :command command :connection-type 'pipe
                :coding 'utf-8-emacs-unix :noquery t :stderr stderr)))
    (condition-case err
        (let ((conn (make-instance
                     'jsonrpc-process-connection
                     :name name :process proc
                     :notification-dispatcher
                     (lambda (_conn method params)
                       (flycheck-lsp--handle-notification server method params))
                     :request-dispatcher (lambda (&rest _) nil))))
          (setf (flycheck-lsp--server-connection server) conn)
          (jsonrpc-async-request
           conn 'initialize (flycheck-lsp--initialize-params root)
           :timeout flycheck-lsp-initialize-timeout
           :success-fn (lambda (result)
                         (flycheck-lsp--on-initialized server result))
           :error-fn (lambda (err)
                       (flycheck-lsp--init-failed
                        server (or (plist-get err :message) err)))
           :timeout-fn (lambda () (flycheck-lsp--init-failed server "timeout")))
          server)
      (error
       (ignore-errors (delete-process proc))
       (ignore-errors (kill-buffer stderr))
       (message "Flycheck LSP: %s failed to start: %s"
                (car command) (error-message-string err))
       nil))))

(defun flycheck-lsp--ensure-server (root command)
  "Return a live server for ROOT and COMMAND, starting one if needed."
  (let* ((key (cons root command))
         (server (gethash key flycheck-lsp--servers)))
    (unless (and server (flycheck-lsp--server-live-p server))
      (setq server (flycheck-lsp--start-server root command))
      (if server
          (puthash key server flycheck-lsp--servers)
        (remhash key flycheck-lsp--servers)))
    server))

(defun flycheck-lsp--notify (server method params)
  "Send an LSP notification METHOD with PARAMS to SERVER."
  (jsonrpc-notify (flycheck-lsp--server-connection server) method params))

(defun flycheck-lsp--request (server method params)
  "Send the LSP request METHOD with PARAMS to SERVER and return its result."
  (jsonrpc-request (flycheck-lsp--server-connection server) method params))

(defun flycheck-lsp--capable (server &rest path)
  "Return SERVER's advertised capability at PATH, or nil.

PATH is a sequence of keyword keys walked into the capability plist, e.g.
\(flycheck-lsp--capable server :codeActionProvider :resolveProvider).  A
JSON `false' (which `jsonrpc' decodes to `:json-false', truthy in Elisp)
is treated as absent."
  (let ((caps (flycheck-lsp--server-capabilities server)))
    (dolist (key path (unless (eq caps :json-false) caps))
      (setq caps (if (listp caps) (plist-get caps key) nil)))))

(defun flycheck-lsp--sync-document (server doc uri language)
  "Send the current buffer's text to SERVER for the document DOC at URI.

LANGUAGE is the LSP languageId.  Send `textDocument/didOpen' the first
time the document is seen (DOC's version is nil), then
`textDocument/didChange' with the whole text whenever the buffer changed
since the last sync (tracked by `buffer-chars-modified-tick'), and nothing
when it did not -- so a push-triggered recheck does not re-send.  The
whole-buffer copy is taken only when a message is actually sent."
  (let ((tick (buffer-chars-modified-tick))
        (version (flycheck-lsp--doc-version doc)))
    (cond
     ((null version)
      (flycheck-lsp--notify
       server 'textDocument/didOpen
       (list :textDocument
             (list :uri uri :languageId language :version 1
                   :text (buffer-substring-no-properties (point-min) (point-max)))))
      (setf (flycheck-lsp--doc-version doc) 1
            (flycheck-lsp--doc-tick doc) tick))
     ((/= (flycheck-lsp--doc-tick doc) tick)
      (cl-incf (flycheck-lsp--doc-version doc))
      (flycheck-lsp--notify
       server 'textDocument/didChange
       (list :textDocument (list :uri uri :version (flycheck-lsp--doc-version doc))
             :contentChanges
             (vector (list :text (buffer-substring-no-properties
                                  (point-min) (point-max))))))
      (setf (flycheck-lsp--doc-tick doc) tick)))))

(defun flycheck-lsp--position-to-point (line character)
  "Return the buffer point for 0-based LINE and UTF-16 CHARACTER.

LSP counts a character offset in UTF-16 code units, so a character
outside the Basic Multilingual Plane counts as two; step over the line
accordingly to land on the right buffer position.

Seek the line through `flycheck-goto-line', whose cache turns the many
in-order lookups of a check (two per diagnostic) into a single forward
pass instead of rescanning from `point-min' each time."
  (save-excursion
    (save-restriction
      (widen)
      (flycheck-goto-line (1+ line))
      (let ((remaining character)
            (eol (line-end-position)))
        (while (and (> remaining 0) (< (point) eol))
          (setq remaining (- remaining (if (<= #x10000 (char-after)) 2 1)))
          (forward-char 1))
        (point)))))

(defun flycheck-lsp--resolve-action (server action)
  "Return ACTION with its edit filled in, resolving it against SERVER if needed.
A server may omit an action's `edit' until it is resolved via
`codeAction/resolve'."
  (if (and (null (plist-get action :edit))
           (plist-get action :data)
           (flycheck-lsp--capable server :codeActionProvider :resolveProvider))
      (flycheck-lsp--request server 'codeAction/resolve action)
    action))

(defun flycheck-lsp--code-action-fix (server uri lsp)
  "Resolve a \"quickfix\" code action for the LSP diagnostic into a fix, or nil.

Runs at apply time.  Sync the buffer first, so the server computes the
edit against its current text -- otherwise a change since the last check
yields stale coordinates that the fix's tick guard cannot catch.  Then
request SERVER's quickfix actions for the diagnostic LSP in the document
URI, prefer an `isPreferred' one, resolve its edit, and convert a
single-file WorkspaceEdit into a `flycheck-fix' stamped with the current
tick.  Any error talking to the server yields nil, so the fix just reports
as unavailable."
  (when (flycheck-lsp--server-live-p server)
    ;; A jsonrpc timeout or error must degrade to nil, not abort the fix
    ;; command (or a `flycheck-fix-all-errors' batch).
    (ignore-errors
      (flycheck-lsp--sync-document
       server (flycheck-lsp--document server (flycheck-lsp--doc-key uri))
       uri (flycheck-lsp--language-id major-mode))
      (when-let* ((actions (append
                            (flycheck-lsp--request
                             server 'textDocument/codeAction
                             (list :textDocument (list :uri uri)
                                   :range (plist-get lsp :range)
                                   :context (list :diagnostics (vector lsp)
                                                  :only ["quickfix"])))
                            nil))
                  (action (or (seq-find (lambda (a)
                                          (eq (plist-get a :isPreferred) t))
                                        actions)
                              (car actions)))
                  (edit (plist-get (flycheck-lsp--resolve-action server action)
                                   :edit)))
        (flycheck-lsp--workspace-edit-fix edit (plist-get action :title))))))

(defun flycheck-lsp--inline-fix (lsp)
  "Build a fix from a quickfix CodeAction embedded in the diagnostic LSP, or nil.

RuboCop and standardrb do not answer `textDocument/codeAction'; they ship
their autocorrect actions inline in each diagnostic's `data' slot, as a
`code_actions' array.  Pick the `isPreferred' one that carries an edit
\(the autocorrect, not the \"disable for this line\" action) and convert
its WorkspaceEdit into a `flycheck-fix'.  Returns the fix eagerly -- it is
already in the payload, so no request is needed."
  (when-let* ((actions (append (plist-get (plist-get lsp :data) :code_actions)
                               nil))
              (action (seq-find (lambda (a)
                                  (and (eq (plist-get a :isPreferred) t)
                                       (plist-get a :edit)))
                                actions))
              (edit (plist-get action :edit)))
    (flycheck-lsp--workspace-edit-fix edit (plist-get action :title))))

(defun flycheck-lsp--fix-provider (server uri lsp)
  "Return a code-action fix, or a lazy provider, for the diagnostic LSP.

Return nil when there is none.  SERVER is the connection to ask, and URI
names the document the diagnostic belongs to.

With `flycheck-lsp-code-actions' on, prefer a quickfix action the server
embedded in the diagnostic's `data' (see `flycheck-lsp--inline-fix'),
building the fix eagerly.  Otherwise, when SERVER advertises code actions,
return a lazy provider (see `flycheck-error-fix') that requests the
quickfix from SERVER on demand via `flycheck-lsp--code-action-fix'."
  (when flycheck-lsp-code-actions
    (or (flycheck-lsp--inline-fix lsp)
        (and (flycheck-lsp--capable server :codeActionProvider)
             (lambda (_err) (flycheck-lsp--code-action-fix server uri lsp))))))

(defun flycheck-lsp--diagnostic->error (lsp buffer server uri)
  "Convert the raw LSP diagnostic plist LSP for BUFFER to a `flycheck-error'.

Reuses the shared LSP mapping for the level, id and related locations, and
attaches a lazy quickfix from SERVER for the document URI (see
`flycheck-lsp--fix-provider')."
  (with-current-buffer buffer
    (let* ((range (plist-get lsp :range))
           (start (plist-get range :start))
           (end (plist-get range :end)))
      (flycheck-error-new-at-pos
       (flycheck-lsp--position-to-point
        (plist-get start :line) (plist-get start :character))
       (flycheck-lsp--severity-level (plist-get lsp :severity))
       (plist-get lsp :message)
       :end-pos (flycheck-lsp--position-to-point
                 (plist-get end :line) (plist-get end :character))
       :id (flycheck-lsp--diagnostic-id lsp)
       :relations (flycheck-lsp--related-locations lsp)
       :tags (flycheck-lsp--tags lsp)
       :fix (flycheck-lsp--fix-provider server uri lsp)
       :checker 'flycheck-lsp
       :buffer buffer
       :filename (buffer-file-name buffer)))))

(defun flycheck-lsp--start (_checker callback)
  "Start the `flycheck-lsp' syntax check, reporting through CALLBACK.

Ensure the buffer's server is running, sync the document to it, and report
the diagnostics cached for the buffer so far.  The server's later push
re-triggers the check to deliver fresh diagnostics.  The command, file and
server are all guaranteed by the checker's predicate, but a server that
fails to start still yields no diagnostics rather than an error.

While the server is still finishing its asynchronous `initialize'
handshake, report nothing and leave the document registered: the
handshake's completion re-triggers the check (see
`flycheck-lsp--on-initialized')."
  (condition-case err
      (let* ((command (flycheck-lsp--command major-mode))
             (uri (flycheck-lsp--buffer-uri))
             (server (and command uri
                          (flycheck-lsp--ensure-server (flycheck-lsp--root)
                                                       command))))
        (if (not server)
            (funcall callback 'finished nil)
          (let* ((buffer (current-buffer))
                 (doc (flycheck-lsp--document
                       server (flycheck-lsp--doc-key uri))))
            (setf (flycheck-lsp--doc-buffer doc) buffer)
            (if (not (flycheck-lsp--server-initialized server))
                (funcall callback 'finished nil)
              (flycheck-lsp--sync-document
               server doc uri (flycheck-lsp--language-id major-mode))
              (funcall callback 'finished
                       (mapcar (lambda (d)
                                 (flycheck-lsp--diagnostic->error d buffer server uri))
                               (flycheck-lsp--doc-diags doc)))))))
    (error (funcall callback 'errored (error-message-string err)))))

(defun flycheck-lsp--enabled-p ()
  "Return non-nil when the `flycheck-lsp' checker may run in the current buffer.

That is, `flycheck-lsp-mode' is on, the buffer visits a file, and its
major mode has a server in `flycheck-lsp-servers' whose program is
installed.  Used as the checker's predicate so `flycheck-lsp' is never selected
unless the mode opted in and the server is actually available."
  (and (bound-and-true-p flycheck-lsp-mode)
       buffer-file-name
       (flycheck-lsp--available-command major-mode)
       t))

(flycheck-define-generic-checker 'flycheck-lsp
  "Report the diagnostics of a Language Server Protocol server.

Talks to the server configured for the buffer's major mode in
`flycheck-lsp-servers' directly, over the built-in `jsonrpc' library, with
no Eglot involved.  Enabled by `flycheck-lsp-mode'."
  :start #'flycheck-lsp--start
  :predicate #'flycheck-lsp--enabled-p
  :modes '(prog-mode text-mode))

(defun flycheck-lsp--shutdown-server (server)
  "Politely shut SERVER's language server down and free its buffers."
  (let ((conn (flycheck-lsp--server-connection server)))
    (when (and conn (jsonrpc-running-p conn))
      (ignore-errors (jsonrpc-request conn 'shutdown nil :timeout 1))
      (ignore-errors (jsonrpc-notify conn 'exit nil))
      (ignore-errors (jsonrpc-shutdown conn t))))
  (when-let* ((stderr (flycheck-lsp--server-stderr server)))
    (when (buffer-live-p stderr) (kill-buffer stderr))))

(defun flycheck-lsp--close-buffer ()
  "Close the current buffer's document on any server holding it.

The server itself is left running -- like Eglot, it is kept for the rest
of the session and torn down only when Emacs exits (see
`flycheck-lsp--shutdown-all') -- so reopening or checking another of its
files does not pay to restart it."
  (when-let* ((uri (flycheck-lsp--buffer-uri))
              (key (flycheck-lsp--doc-key uri)))
    (maphash
     (lambda (_server-key server)
       (when (gethash key (flycheck-lsp--server-documents server))
         (remhash key (flycheck-lsp--server-documents server))
         (when (flycheck-lsp--server-live-p server)
           (ignore-errors
             (flycheck-lsp--notify server 'textDocument/didClose
                                   (list :textDocument (list :uri uri)))))))
     flycheck-lsp--servers)))

(defun flycheck-lsp--shutdown-all ()
  "Shut down every running LSP server.
Added to `kill-emacs-hook' the first time a server starts."
  (maphash (lambda (key server)
             (flycheck-lsp--shutdown-server server)
             (remhash key flycheck-lsp--servers))
           flycheck-lsp--servers))

(defun flycheck-lsp--enable ()
  "Set up the current buffer to report its LSP server's diagnostics.
A no-op unless the mode's server is configured and installed."
  (when (flycheck-lsp--available-command major-mode)
    (flycheck-lsp--register-checker 'flycheck-lsp flycheck-lsp-exclusive)
    (flycheck-lsp--select-primary-bridge)
    ;; Give the recheck guard a real buffer-local binding, so the `let' that
    ;; sets it around a push-triggered check isolates to this buffer instead
    ;; of leaking a global value to others (see `flycheck-lsp--suppress-recheck').
    (setq-local flycheck-lsp--suppress-recheck nil)
    (unless flycheck-mode (flycheck-mode 1))
    (flycheck-buffer-deferred)))

(defun flycheck-lsp--disable ()
  "Undo `flycheck-lsp--enable' in the current buffer."
  (flycheck-lsp--close-buffer)
  ;; Hand the buffer to the other bridge if it is still on, else back to
  ;; automatic selection
  (flycheck-lsp--select-primary-bridge)
  (when flycheck-mode
    (flycheck-buffer-deferred)))

;;;###autoload
(define-minor-mode flycheck-lsp-mode
  "Minor mode to report a Language Server's diagnostics through Flycheck.

When enabled, and the buffer's major mode has a server configured in
`flycheck-lsp-servers', Flycheck starts that server and shows the
diagnostics it reports (via the `flycheck-lsp' checker), talking LSP directly
without Eglot.  With `flycheck-lsp-exclusive' nil, `flycheck-lsp' chains to the
command checkers so both contribute.

Enable it for every configured buffer with `global-flycheck-lsp-mode'.
For a full language server, prefer Eglot and `flycheck-eglot-mode'."
  :lighter nil
  :group 'flycheck
  (if flycheck-lsp-mode
      (progn
        (add-hook 'kill-buffer-hook #'flycheck-lsp--close-buffer nil t)
        (flycheck-lsp--enable))
    (remove-hook 'kill-buffer-hook #'flycheck-lsp--close-buffer t)
    (flycheck-lsp--disable)))

;;;###autoload
(define-globalized-minor-mode global-flycheck-lsp-mode
  flycheck-lsp-mode
  (lambda ()
    (when (flycheck-lsp--available-command major-mode) (flycheck-lsp-mode 1)))
  :group 'flycheck)


;;; Eglot integration
;;
;; Eglot, Emacs' built-in LSP client, renders its diagnostics through
;; Flymake and deliberately offers no Flycheck support.  `flycheck-eglot-mode'
;; bridges the gap: it feeds the diagnostics an LSP server reports to Eglot
;; into Flycheck, so a buffer can use Eglot for LSP features while Flycheck
;; owns the error display, navigation and the error list -- alongside the
;; usual command checkers.
;;
;; This obsoletes the third-party `flycheck-eglot' package.
;;
;; Eglot pushes diagnostics whenever the server sends them, which does not
;; line up with Flycheck's on-demand checking model.  The `eglot-check'
;; generic checker adapts the two: its `:start' pulls Eglot's current
;; diagnostics (via `eglot-flymake-backend') and reports them, and a report
;; function caches later pushes and re-triggers a check so they reach the
;; buffer.  Diagnostics are converted from the original LSP object Eglot
;; stashes in each Flymake diagnostic's data slot, which is richer and more
;; stable across Eglot versions than the Flymake fields.

(declare-function eglot-managed-p "eglot")
(declare-function eglot-flymake-backend "eglot" (report-fn &rest _))
(declare-function flymake-mode "flymake" (&optional arg))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-data "flymake" (diag))
(declare-function eglot-code-actions "eglot" (beg &optional end action-kind interactive))
(declare-function eglot-server-capable "eglot" (&rest feats))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function eglot--uri-to-path "eglot" (uri))
(declare-function eglot--request "eglot" (server method params &rest _))
(declare-function eglot--current-server-or-lose "eglot" ())

(defcustom flycheck-eglot-code-actions t
  "Whether `eglot-check' offers LSP quick-fix code actions as fixes.

When non-nil (the default) and the server supports code actions, each
Eglot diagnostic carries a lazy fix (see `flycheck-error-resolve-fix')
that requests the server's \"quickfix\" code action for it when applied
with \\[flycheck-fix-error-at-point].  Because the fix is only computed on
demand, Flycheck cannot know in advance whether the server has an action,
so these diagnostics carry no fix indicator - try
\\[flycheck-fix-error-at-point] to ask.  Set this to nil to turn the
feature off."
  :group 'flycheck
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "38"))

(defcustom flycheck-eglot-exclusive t
  "Whether `eglot-check' is the only checker or chains to others.

When non-nil (the default), a buffer using `flycheck-eglot-mode' reports
only Eglot's diagnostics.  When nil, `eglot-check' chains onward: to
`flycheck-lsp' in a buffer that also uses `flycheck-lsp-mode', so a lint
server can run behind Eglot's, and then to the first command checker that
supports the buffer's major mode.

Note that this takes effect globally when a buffer enables the mode: it is
stored in `eglot-check's chain, not per buffer.  The chain is still safe
in buffers that use only one of the bridges, because each checker's
predicate refuses a buffer whose mode is off."
  :group 'flycheck
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "38"))

(defvar-local flycheck-eglot--diagnostics nil
  "Latest diagnostics Eglot reported for this buffer, in Flymake format.")

;; A check must not start another check, or the two feed each other: the
;; check asks Eglot for diagnostics, the answer starts a check, and round it
;; goes.  Earlier fixes tried to tell a report Flycheck asked for from one
;; the server volunteered, and kept missing a shape.  Eglot marks no such
;; boundary, and how many reports an answer takes is its business: Emacs 30
;; reports once, Emacs 31 twice, synchronously under the push model and
;; asynchronously under the pull model of LSP 3.17.
;;
;; So do not ask who a report belongs to.  Assemble the reports arriving now
;; into one set, and start a check only when that set differs from what the
;; checker last published.  Once Flycheck holds what Eglot holds, no report
;; of the same diagnostics starts anything, whoever asked for it, however
;; many reports it arrives in.

(defvar-local flycheck-eglot--pending nil
  "Diagnostics assembled from the reports arriving now.")

(defvar-local flycheck-eglot--settle-timer nil
  "Timer that ends the run of reports arriving now, or nil.")

(defun flycheck-eglot--replaces-all-p (region)
  "Whether a report about REGION is to replace the buffer's diagnostics.

REGION is the `:region' of `flymake-diagnostic-functions': the part of
the buffer a report accounts for, or nil for the whole of it.  Eglot
answers one request with two reports, the pulled diagnostics for the
whole buffer and then the pushed ones for an empty region, meaning add
these and delete nothing.  Together they are what the server holds."
  (or (null region)
      (and (<= (car region) (point-min))
           (>= (cdr region) (point-max)))))

(defun flycheck-eglot--settle (buffer)
  "Publish the diagnostics assembled for BUFFER, if they moved."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq flycheck-eglot--settle-timer nil)
      (unless (equal flycheck-eglot--pending flycheck-eglot--diagnostics)
        (setq flycheck-eglot--diagnostics flycheck-eglot--pending)
        (flycheck-lsp--count-recheck)
        (flycheck-buffer-automatically)))))

(defun flycheck-eglot--settle-soon ()
  "Arrange to settle the reports arriving now, once they stop coming.

The delay is what makes the run of reports one answer rather than
several, and it keeps the check off the process filter that delivered
them, where the recursion used to exhaust the Lisp stack."
  (unless flycheck-eglot--settle-timer
    (setq flycheck-eglot--settle-timer
          (run-at-time 0 nil #'flycheck-eglot--settle (current-buffer)))))

(defun flycheck-eglot--take-pending ()
  "Accept the reports assembled so far without starting a check.
For a check that is about to publish them itself."
  (when flycheck-eglot--settle-timer
    (cancel-timer flycheck-eglot--settle-timer)
    (setq flycheck-eglot--settle-timer nil))
  (setq flycheck-eglot--diagnostics flycheck-eglot--pending))

(defun flycheck-eglot--ask-for-diagnostics ()
  "Ask Eglot for this buffer's diagnostics."
  (eglot-flymake-backend #'flycheck-eglot--report))

(defun flycheck-eglot--available-p ()
  "Return non-nil when Eglot is managing the current buffer."
  (and (fboundp 'eglot-managed-p) (eglot-managed-p)))

(defun flycheck-eglot--enabled-p ()
  "Return non-nil when `eglot-check' may run in the current buffer.

That is, `flycheck-eglot-mode' is on and Eglot manages the buffer.  Used
as the checker's predicate so `eglot-check', though a registered checker,
is never selected unless the mode opted in."
  (and (bound-and-true-p flycheck-eglot-mode)
       (flycheck-eglot--available-p)))

(defun flycheck-eglot--type-level (type)
  "Map an Eglot Flymake diagnostic TYPE to a Flycheck error level."
  (pcase type
    ('eglot-note 'info)
    ('eglot-warning 'warning)
    (_ 'error)))

(defun flycheck-eglot--convert-diagnostic (diag)
  "Convert the Eglot Flymake diagnostic DIAG to a `flycheck-error'.

The buffer positions come from DIAG, which Eglot has already resolved.
The level, message and id come from the original LSP diagnostic Eglot
stashes in DIAG's data slot; if it is absent, the Flymake fields are used
as a fallback."
  (let ((lsp (and (fboundp 'flymake-diagnostic-data)
                  (alist-get 'eglot-lsp-diag (flymake-diagnostic-data diag)))))
    (flycheck-error-new-at-pos
     (flymake-diagnostic-beg diag)
     (if lsp
         (flycheck-lsp--severity-level (plist-get lsp :severity))
       (flycheck-eglot--type-level (flymake-diagnostic-type diag)))
     (if lsp
         (plist-get lsp :message)
       (format "%s" (flymake-diagnostic-text diag)))
     :end-pos (flymake-diagnostic-end diag)
     :id (and lsp (flycheck-lsp--diagnostic-id lsp))
     :relations (and lsp (flycheck-lsp--related-locations lsp))
     :tags (and lsp (flycheck-lsp--tags lsp))
     ;; Prefer a quickfix the server embedded in the diagnostic's data
     ;; (rubocop, standardrb); fall back to Eglot's on-demand code actions.
     :fix (or (and flycheck-eglot-code-actions lsp (flycheck-lsp--inline-fix lsp))
              (flycheck-eglot--fix-provider))
     :checker 'eglot-check
     :buffer (current-buffer)
     :filename (buffer-file-name))))

(defun flycheck-eglot--fix-provider ()
  "Return the lazy code-action fix provider, or nil when unavailable.

Non-nil only when `flycheck-eglot-code-actions' is on and the server
advertises code actions; see `flycheck-eglot--code-action-fix'.  The
capability probe needs a live server, so a buffer Eglot does not
manage never asks."
  (when (and flycheck-eglot-code-actions
             (flycheck-eglot--available-p)
             (fboundp 'eglot-server-capable)
             (eglot-server-capable :codeActionProvider))
    #'flycheck-eglot--code-action-fix))

(defun flycheck-eglot--error-region (err)
  "Return the (BEG . END) buffer region of ERR, for a code-action request."
  (save-restriction
    (widen)
    (let* ((line (flycheck-error-line err))
           (beg (flycheck-line-column-to-position
                 line (or (flycheck-error-column err) 1)))
           (end (if (and (flycheck-error-end-line err)
                         (flycheck-error-end-column err))
                    (flycheck-line-column-to-position
                     (flycheck-error-end-line err)
                     (flycheck-error-end-column err))
                  beg)))
      (cons beg end))))

(defun flycheck-eglot--resolve-action (action)
  "Return ACTION with its edit filled in, resolving it if necessary.
A server may omit the `edit' until the action is resolved via
`codeAction/resolve'."
  (if (and (null (plist-get action :edit))
           (plist-get action :data)
           (eglot-server-capable :codeActionProvider :resolveProvider))
      (eglot--request (eglot--current-server-or-lose)
                      :codeAction/resolve action)
    action))

(defun flycheck-eglot--code-action-fix (err)
  "Resolve a \"quickfix\" code action for ERR into a `flycheck-fix', or nil.

Used as a lazy fix provider (see `flycheck-error-fix'): requests the
server's quickfix actions overlapping ERR, prefers an `isPreferred' one,
resolves its edit, and converts a single-file edit into a fix.  Any error
talking to the server yields nil, so the fix just reports as unavailable."
  (when (flycheck-eglot--available-p)
    (pcase-let ((`(,beg . ,end) (flycheck-eglot--error-region err)))
      ;; A jsonrpc timeout or error from either request must degrade to nil,
      ;; not abort the fix command (or a `flycheck-fix-all-errors' batch).
      (ignore-errors
        (when-let* ((actions (eglot-code-actions beg end "quickfix" nil))
                    ;; `eq' to t, not bare truthiness: jsonrpc decodes JSON
                    ;; `false' to `:json-false', which is truthy in Elisp, so a
                    ;; server sending `isPreferred: false' must not be picked.
                    (action (or (seq-find (lambda (a)
                                            (eq (plist-get a :isPreferred) t))
                                          actions)
                                (car actions)))
                    (edit (plist-get (flycheck-eglot--resolve-action action)
                                     :edit)))
          (flycheck-lsp--workspace-edit-fix
           edit (plist-get action :title)))))))

(defun flycheck-eglot--report (diags &rest plist)
  "Take Eglot's DIAGS into the answer being assembled.
Registered with `eglot-flymake-backend' as its report function.  PLIST
is the rest of the `flymake-diagnostic-functions' call.

The diagnostics reach the buffer once the reports arriving now stop, and
only if they differ from the ones Flycheck already shows.  A server
republishes an unchanged set freely while it indexes or builds, and
every one of those used to cost a full check."
  (let ((new (append diags nil)))
    (setq flycheck-eglot--pending
          (if (flycheck-eglot--replaces-all-p (plist-get plist :region))
              new
            (append flycheck-eglot--pending new))))
  (flycheck-lsp--count-push nil)
  (flycheck-eglot--settle-soon))

(defun flycheck-eglot--start (_checker callback)
  "Start the `eglot-check' syntax check, reporting through CALLBACK.

Ask Eglot for the buffer's diagnostics and report the conversions of
whatever it has to hand.  Under the pull model the answer arrives later
instead, and reaches the buffer through `flycheck-eglot--report'."
  (flycheck-eglot--ask-for-diagnostics)
  ;; Anything Eglot handed over during the call belongs to this check, so
  ;; publish it here rather than leaving it to start another one
  (flycheck-eglot--take-pending)
  (funcall callback 'finished
           (mapcar #'flycheck-eglot--convert-diagnostic
                   flycheck-eglot--diagnostics)))

(flycheck-define-generic-checker 'eglot-check
  "Report the diagnostics Eglot receives from an LSP server.

Enabled by `flycheck-eglot-mode'; only usable in Eglot-managed buffers."
  :start #'flycheck-eglot--start
  :predicate #'flycheck-eglot--enabled-p
  :modes '(prog-mode text-mode))

(defun flycheck-eglot--flymake-diagnostics (orig &optional beg end &rest args)
  "Serve the cached Eglot diagnostics while `flycheck-eglot-mode' is on.

`flycheck-eglot-mode' turns Flymake's own mode off, which would otherwise
leave `flymake-diagnostics' (used e.g. by `eglot-code-actions') empty.
ORIG is the advised function; BEG, END and ARGS are its arguments."
  (if (not (bound-and-true-p flycheck-eglot-mode))
      (apply orig beg end args)
    ;; Mirror `flymake-diagnostics': return the diagnostics that OVERLAP
    ;; [BEG, END] (nil means unbounded), not just those contained in it, so
    ;; callers like `eglot-code-actions' still see a wide diagnostic at point.
    (seq-filter (lambda (d)
                  (let ((db (flymake-diagnostic-beg d))
                        (de (flymake-diagnostic-end d)))
                    (and (or (null end) (<= db end))
                         (or (null beg) (<= beg de)))))
                flycheck-eglot--diagnostics)))

(defun flycheck-eglot--enable ()
  "Set up the current buffer to report Eglot diagnostics through Flycheck."
  (when (flycheck-eglot--available-p)
    (flycheck-lsp--register-checker 'eglot-check flycheck-eglot-exclusive)
    (flycheck-lsp--select-primary-bridge)
    ;; Register as Eglot's report function without letting the answer start
    ;; a check; the trailing `flycheck-buffer-deferred' triggers the first
    ;; one instead.
    (flycheck-eglot--ask-for-diagnostics)
    (flycheck-eglot--take-pending)
    (advice-add 'flymake-diagnostics :around
                #'flycheck-eglot--flymake-diagnostics)
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))
    (unless flycheck-mode
      (flycheck-mode 1))
    (flycheck-buffer-deferred)))

(defun flycheck-eglot--disable ()
  "Undo `flycheck-eglot--enable' in the current buffer."
  (when (flycheck-eglot--available-p)
    (ignore-errors (eglot-flymake-backend #'ignore)))
  ;; Hand the buffer to the other bridge if it is still on, else back to
  ;; automatic selection
  (flycheck-lsp--select-primary-bridge)
  (when flycheck-eglot--settle-timer
    (cancel-timer flycheck-eglot--settle-timer)
    (setq flycheck-eglot--settle-timer nil))
  (setq flycheck-eglot--diagnostics nil
        flycheck-eglot--pending nil)
  (when flycheck-mode
    (flycheck-buffer-deferred)))

;;;###autoload
(define-minor-mode flycheck-eglot-mode
  "Minor mode to report Eglot's LSP diagnostics through Flycheck.

When enabled in an Eglot-managed buffer, Flycheck shows the diagnostics
the LSP server reports (via the `eglot-check' checker) instead of Flymake,
which is turned off.  With `flycheck-eglot-exclusive' nil, `eglot-check'
chains to the command checkers so both contribute.

Usually enabled for every Eglot buffer via `global-flycheck-eglot-mode'."
  :lighter nil
  :group 'flycheck
  (if flycheck-eglot-mode
      (flycheck-eglot--enable)
    (flycheck-eglot--disable)))

(defun flycheck-eglot--managed-mode-update ()
  "Turn `flycheck-eglot-mode' on or off to track Eglot managing the buffer.
For `eglot-managed-mode-hook', which fires on both enter and exit."
  (flycheck-eglot-mode (if (flycheck-eglot--available-p) 1 -1)))

;;;###autoload
(define-globalized-minor-mode global-flycheck-eglot-mode
  flycheck-eglot-mode
  (lambda () (when (flycheck-eglot--available-p) (flycheck-eglot-mode 1)))
  :group 'flycheck
  (if global-flycheck-eglot-mode
      (add-hook 'eglot-managed-mode-hook #'flycheck-eglot--managed-mode-update)
    (remove-hook 'eglot-managed-mode-hook
                 #'flycheck-eglot--managed-mode-update)))


;;; Handling checkers that could not run

;; A checker reaches `:handle-suspicious' only when it exited non-zero and
;; printed nothing Flycheck could read.  For most tools that means a broken
;; setup rather than a parsing gap on our side: a missing configuration
;; file, an interpreter without the linter installed, a plugin that failed
;; to load.  Where a tool distinguishes that from ordinary findings by its
;; exit status, say so plainly and let the checker step aside, instead of
;; emptying a crash dump into the echo area on every check.

(defun flycheck--python-traceback-p (output)
  "Whether OUTPUT has a Python traceback in it."
  (and output
       (string-match-p (rx bol "Traceback (most recent call last):") output)))

(defun flycheck--fatal-exit-reason (output)
  "Return the line of OUTPUT that best explains why a checker failed.

A Python traceback ends with the exception that caused it, which is the
only line worth showing.  Anything else leads with its own summary."
  (let ((lines (split-string (or output "") "\n" 'omit-nulls "[ \t\r]+")))
    (cond
     ((null lines) nil)
     ((flycheck--python-traceback-p output) (car (last lines)))
     (t (car lines)))))

(defun flycheck--handle-fatal-exit (exit-status output fatal-statuses)
  "Disable the checker when EXIT-STATUS means it could not run at all.

OUTPUT is what the tool printed, and goes into the message explaining
why the checker stepped aside.

FATAL-STATUSES lists the exit statuses with which the tool reports that
it could not run, as opposed to reporting findings.  Getting here already
means the check produced nothing readable, so a fatal status leaves the
checker no way to work in this buffer and it steps aside, explained by
`flycheck--fatal-exit-reason'.

Any other status stays `suspicious': the tool ran and Flycheck could not
make sense of what it printed, which is Flycheck's problem to fix."
  (if (memq exit-status fatal-statuses)
      (cons 'disable (flycheck--fatal-exit-reason output))
    'suspicious))

(defun flycheck--python-ruff-handle-suspicious (_checker exit-status output)
  "Disable `python-ruff' when EXIT-STATUS means ruff could not lint.

OUTPUT is what it printed, for the message that says why.

Ruff exits 2 on a bad invocation or an unparsable configuration file,
and 0 or 1 when it has actually looked at the code."
  (flycheck--handle-fatal-exit exit-status output '(2)))

(defun flycheck--python-flake8-handle-suspicious (_checker exit-status output)
  "Disable `python-flake8' when EXIT-STATUS means flake8 could not lint.

OUTPUT is what it printed, for the message that says why.

Flake8 exits 2 on a bad invocation, but a missing plugin or an
unreadable configuration crashes it with a traceback and the same
exit status 1 it uses to report findings, so the traceback is what
tells the two apart.  This is the usual shape of pointing Flycheck at
an interpreter that does not have flake8's dependencies installed."
  (if (and (equal exit-status 1) (flycheck--python-traceback-p output))
      (cons 'disable (flycheck--fatal-exit-reason output))
    (flycheck--handle-fatal-exit exit-status output '(2))))

(defun flycheck--python-pylint-handle-suspicious (_checker exit-status output)
  "Disable `python-pylint' when EXIT-STATUS means pylint could not lint.

OUTPUT is what it printed, for the message that says why.

Pylint's exit status is a bitmask of the message classes it emitted, so
only 32, its usage error, means it never got as far as looking at the
code."
  (flycheck--handle-fatal-exit exit-status output '(32)))

(defun flycheck--python-mypy-handle-suspicious (_checker exit-status output)
  "Disable `python-mypy' when EXIT-STATUS means mypy could not check.

OUTPUT is what it printed, for the message that says why.

Mypy exits 2 on a fatal error such as a bad flag, and 0 or 1 once it has
type-checked anything."
  (flycheck--handle-fatal-exit exit-status output '(2)))

(defun flycheck--rubocop-handle-suspicious (_checker exit-status output)
  "Disable a RuboCop-based checker when EXIT-STATUS means it could not run.

OUTPUT is what it printed, for the message that says why.

RuboCop exits 2 on a bad invocation or an unrecognised cop in the
configuration, and 1 when it found offences."
  (flycheck--handle-fatal-exit exit-status output '(2)))

(defun flycheck--shellcheck-handle-suspicious (_checker exit-status output)
  "Disable `sh-shellcheck' when EXIT-STATUS means shellcheck could not run.

OUTPUT is what it printed, for the message that says why.

Shellcheck exits 2 when it cannot read the file and 3 on a bad
invocation.  Findings, and even an unparsable script, come back as
JSON with exit status 0 or 1."
  (flycheck--handle-fatal-exit exit-status output '(2 3)))

(defun flycheck--stylelint-handle-suspicious (_checker exit-status output)
  "Disable a stylelint checker when EXIT-STATUS means it could not lint.

OUTPUT is what it printed, for the message that says why.

Stylelint is the exception to the usual convention: it exits 2 when it
found problems, and reports its own failures with 78 for a missing
configuration file, which is by far the most common way it fails, and
64 for a bad invocation."
  (flycheck--handle-fatal-exit exit-status output '(78 64)))


;;; Built-in checkers
(flycheck-def-args-var flycheck-gnat-args ada-gnat
  :package-version '(flycheck . "0.20"))

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
  :safe #'string-or-null-p
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
`https://www.adacore.com/community/'."
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
            (eval flycheck-gnat-args)
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
   ;; no specific error prefix in Ada
   (error line-start (file-name) ":" line ":" column
          ": " (message) line-end))
  :modes ada-mode)

(flycheck-define-checker asciidoctor
  "An AsciiDoc syntax checker using the Asciidoctor compiler.

See URL `https://asciidoctor.org'."
  :command ("asciidoctor" "-o" null-device "-")
  :standard-input t
  :error-patterns
  ((error line-start
          "asciidoctor: ERROR: <stdin>: Line " line ": " (message)
          line-end)
   (warning line-start
            "asciidoctor: WARNING: <stdin>: Line " line ": " (message)
            line-end))
  :modes (adoc-mode asciidoc-mode))

(defun flycheck-awk-gawk-fix-message (err)
  "Remove the repeated file-name/line from the error message of ERR."
  (setf (flycheck-error-message err)
        (replace-regexp-in-string
         (rx line-start
             (group (zero-or-more (any " " "\t")))
             (group (zero-or-more nonl) "\n")
             (backref 1))
         "\\2"
         (replace-regexp-in-string
          (rx "\ngawk: " (zero-or-more (not (any " "))) ":")
          "\n"
          (flycheck-error-message err))))
  err)

(defun flycheck-awk-gawk-error-filter (errors)
  "Remove repeated file-name/line from ERRORS."
  (seq-do #'flycheck-awk-gawk-fix-message errors)
  errors)

(flycheck-define-checker awk-gawk
  "GNU awk's built-in --lint checker."
  :command ("gawk"
            ;; Avoid code execution.  See https://github.com/w0rp/ale/pull/1411
            ;; The BEGIN/END blocks short-circuit the script's own rules so
            ;; only linting happens; exit 0 so that valid scripts don't get
            ;; flagged as a suspicious non-zero exit.
            "--source" "BEGIN{exit} END{exit}"
            "-f" source
            "--lint"
            "/dev/null")
  :standard-input nil
  :error-patterns
  ((warning line-start
            "gawk: "
            (file-name) ":" line ":" (optional column ":")
            (message (one-or-more not-newline)
                     (optional "\n"
                               (one-or-more not-newline)
                               " ^ "
                               (one-or-more not-newline)))
            line-end))
  :error-filter flycheck-awk-gawk-error-filter
  :modes awk-mode)

(flycheck-define-checker bazel-build-buildifier
  "A checker for Bazel BUILD and BUILD.bazel files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=build")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-build-mode)

(flycheck-define-checker bazel-module-buildifier
  "A checker for Bazel MODULE.bazel files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=default")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-module-mode)

(flycheck-define-checker bazel-starlark-buildifier
  "A checker for Starlark bzl files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=bzl")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-starlark-mode)

(flycheck-define-checker bazel-workspace-buildifier
  "A checker for Bazel WORKSPACE and WORKSPACE.bazel files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=workspace")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-workspace-mode)

(flycheck-def-args-var flycheck-clang-args c/c++-clang
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-clang-blocks nil c/c++-clang
  "Enable blocks in Clang.

When non-nil, enable blocks in Clang with `-fblocks'.  See URL
`https://clang.llvm.org/docs/BlockLanguageSpec.html' for more
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
  :safe #'string-or-null-p
  :package-version '(flycheck . "0.15"))
(make-variable-buffer-local 'flycheck-clang-language-standard)

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

(flycheck-def-option-var flycheck-clang-pedantic nil c/c++-clang
  "Whether to warn about language extensions in Clang.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-clang-pedantic-errors nil c/c++-clang
  "Whether to error on language extensions in Clang.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic-errors'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-clang-standard-library nil c/c++-clang
  "The standard library to use for Clang.

The value of this variable is the name of a standard library as
string, or nil to use the default standard library.

Refer to the Clang manual at URL
`https://clang.llvm.org/docs/UsersManual.html' for more
information about the standard library."
  :type '(choice (const :tag "Default standard library" nil)
                 (const "libc++")
                 (const :tag "GNU libstdc++" "libstdc++")
                 (string :tag "Library name"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-warnings '("all" "extra") c/c++-clang
  "A list of additional warnings to enable in Clang.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the Clang manual at URL
`https://clang.llvm.org/docs/UsersManual.html' for more
information about warnings."
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(defun flycheck-c/c++-quoted-include-directory ()
  "Get the directory for quoted includes.

C/C++ compilers typically look up includes with quotation marks
in the directory of the file being compiled.  However, since
Flycheck uses temporary copies for syntax checking, it needs to
explicitly determine the directory for quoted includes.

This function determines the directory by looking at function
`buffer-file-name', or if that is nil, at `default-directory'."
  (if-let* ((fn (buffer-file-name)))
      (file-name-directory fn)
    ;; If the buffer has no file name, fall back to its default directory
    default-directory))

(flycheck-define-checker c/c++-clang
  "A C/C++ syntax checker using Clang.

See URL `https://clang.llvm.org/'."
  :command ("clang"
            "-fsyntax-only"
            "-fno-color-diagnostics"    ; Do not include color codes in output
            "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-clang-language-standard concat)
            (option-flag "-pedantic" flycheck-clang-pedantic)
            (option-flag "-pedantic-errors" flycheck-clang-pedantic-errors)
            (option "-stdlib=" flycheck-clang-standard-library concat)
            (option-flag "-fms-extensions" flycheck-clang-ms-extensions)
            (option-flag "-fno-exceptions" flycheck-clang-no-exceptions)
            (option-flag "-fno-rtti" flycheck-clang-no-rtti)
            (option-flag "-fblocks" flycheck-clang-blocks)
            (option-list "-include" flycheck-clang-includes)
            (option-list "-W" flycheck-clang-warnings concat)
            (option-list "-D" flycheck-clang-definitions concat)
            (option-list "-I" flycheck-clang-include-path)
            (eval flycheck-clang-args)
            "-x" (eval
                  (pcase major-mode
                    ((or `c++-mode `c++-ts-mode) "c++")
                    ((or `c-mode `c-ts-mode) "c")))
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((info line-start (or "<stdin>" (file-name)) ":" line ":" column
         ": note: " (optional (message)) line-end)
   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": warning: " (optional (message)) line-end)
   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
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
      errors))
  :modes (c-mode c++-mode c-ts-mode c++-ts-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(flycheck-def-args-var flycheck-gcc-args c/c++-gcc
  :package-version '(flycheck . "0.22"))

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
  :safe #'string-or-null-p
  :package-version '(flycheck . "0.20"))
(make-variable-buffer-local 'flycheck-gcc-language-standard)

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

(flycheck-def-option-var flycheck-gcc-pedantic nil c/c++-gcc
  "Whether to warn about language extensions in GCC.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-gcc-pedantic-errors nil c/c++-gcc
  "Whether to error on language extensions in GCC.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic-errors'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

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

Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
  :command ("gcc"
            "-fshow-column"
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gcc-language-standard concat)
            (option-flag "-pedantic" flycheck-gcc-pedantic)
            (option-flag "-pedantic-errors" flycheck-gcc-pedantic-errors)
            (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
            (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
            (option-flag "-fopenmp" flycheck-gcc-openmp)
            (option-list "-include" flycheck-gcc-includes)
            (option-list "-W" flycheck-gcc-warnings concat)
            (option-list "-D" flycheck-gcc-definitions concat)
            (option-list "-I" flycheck-gcc-include-path)
            (eval flycheck-gcc-args)
            "-x" (eval
                  (pcase major-mode
                    ((or `c++-mode `c++-ts-mode) "c++")
                    ((or `c-mode `c-ts-mode) "c")))
            ;; GCC performs full checking only when actually compiling, so
            ;; `-fsyntax-only' is not enough. Just let it generate assembly
            ;; code.
            "-S" "-o" null-device
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((info line-start (or "<stdin>" (file-name))
         ":" line (optional ":" column)
         ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name))
            ":" line (optional ":" column)
            ": warning: " (message (one-or-more (not (any "\n["))))
            (optional "[" (id (one-or-more not-newline)) "]") line-end)
   (error line-start (or "<stdin>" (file-name))
          ":" line (optional ":" column)
          ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode c++-mode c-ts-mode c++-ts-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(flycheck-def-args-var flycheck-cppcheck-args c/c++-cppcheck
  :package-version '(flycheck . "35"))

(flycheck-def-option-var flycheck-cppcheck-checks '("style") c/c++-cppcheck
  "Enabled checks for Cppcheck.

The value of this variable is a list of strings, where each
string is the name of an additional check to enable.  By default,
all coding style checks are enabled.

See section \"Enable message\" in the Cppcheck manual at URL
`https://cppcheck.sourceforge.net/manual.pdf', and the
documentation of the `--enable' option for more information,
including a list of supported checks."
  :type '(repeat :tag "Additional checks"
                 (string :tag "Check name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-cppcheck-standards nil c/c++-cppcheck
  "The standards to use in cppcheck.

The value of this variable is either a list of strings denoting
the standards to use, or nil to pass nothing to cppcheck.  When
non-nil, pass the standards via one or more `--std=' options."
  :type '(choice (const :tag "Default" nil)
                 (repeat :tag "Custom standards"
                         (string :tag "Standard name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "28"))
(make-variable-buffer-local 'flycheck-cppcheck-standards)

(flycheck-def-option-var flycheck-cppcheck-suppressions-file nil c/c++-cppcheck
  "The suppressions file to use in cppcheck.

The value of this variable is a file with the suppressions to
use, or nil to pass nothing to cppcheck.  When non-nil, pass the
suppressions file via the `--suppressions-list=' option."
  :type '(choice (const :tag "Default" nil)
                 (file :tag "Suppressions file"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-cppcheck-suppressions-file)

(flycheck-def-option-var flycheck-cppcheck-suppressions nil c/c++-cppcheck
  "The suppressions to use in cppcheck.

The value of this variable is either a list of strings denoting
the suppressions to use, or nil to pass nothing to cppcheck.
When non-nil, pass the suppressions via one or more `--suppress='
options."
  :type '(choice (const :tag "Default" nil)
                 (repeat :tag "Additional suppressions"
                         (string :tag "Suppression")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "28"))

(flycheck-def-option-var flycheck-cppcheck-inconclusive nil c/c++-cppcheck
  "Whether to enable Cppcheck inconclusive checks.

When non-nil, enable Cppcheck inconclusive checks.  This allows Cppcheck to
report warnings it's not certain of, but it may result in false positives.

This will have no effect when using Cppcheck 1.53 and older."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.19"))

(flycheck-def-option-var flycheck-cppcheck-include-path nil c/c++-cppcheck
  "A list of include directories for cppcheck.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of cppcheck.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker c/c++-cppcheck
  "A C/C++ checker using cppcheck.

See URL `https://cppcheck.sourceforge.net/'."
  :command ("cppcheck" "--quiet" "--xml-version=2" "--inline-suppr"
            (option "--enable=" flycheck-cppcheck-checks concat
                    flycheck-option-comma-separated-list)
            (option-flag "--inconclusive" flycheck-cppcheck-inconclusive)
            (option-list "-I" flycheck-cppcheck-include-path)
            (option-list "--std=" flycheck-cppcheck-standards concat)
            (option-list "--suppress=" flycheck-cppcheck-suppressions concat)
            (option "--suppressions-list="
                    flycheck-cppcheck-suppressions-file concat)
            "-x" (eval
                  (pcase major-mode
                    ((or `c++-mode `c++-ts-mode) "c++")
                    ((or `c-mode `c-ts-mode) "c")))
            (eval flycheck-cppcheck-args)
            source)
  :error-parser flycheck-parse-cppcheck
  :modes (c-mode c++-mode c-ts-mode c++-ts-mode))

(flycheck-define-checker cfengine
  "A CFEngine syntax checker using cf-promises.

See URL `https://cfengine.com/'."
  :command ("cf-promises" "-Wall" "-f"
            ;; We must stay in the same directory to resolve @include
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": error: " (message) line-end))
  :modes (cfengine-mode cfengine3-mode))

(flycheck-define-checker coffee
  "A CoffeeScript syntax checker using coffee.

See URL `https://coffeescript.org/'."
  ;; --print suppresses generation of compiled .js files
  :command ("coffee" "--compile" "--print" "--stdio")
  :standard-input t
  :error-patterns
  ((error line-start "[stdin]:" line ":" column
          ": error: " (message) line-end))
  :modes coffee-mode)

(flycheck-def-args-var flycheck-stylelint-args
    (css-stylelint scss-stylelint sass-stylelint less-stylelint)
  :package-version '(flycheck . "39"))

;; Limit the length of the generated docstring by including only the first three
;; checker symbols, otherwise emacs will complain about the docstring length
;; and may refuse to compile the package.
(define-obsolete-variable-alias 'flycheck-stylelintrc
  'flycheck-stylelint-config "39")
(let ((print-length 3))
  (flycheck-def-config-file-var flycheck-stylelint-config
      (css-stylelint scss-stylelint sass-stylelint less-stylelint) nil))

(flycheck-def-option-var flycheck-stylelint-quiet
    nil (css-stylelint scss-stylelint sass-stylelint less-stylelint)
  "Whether to run stylelint in quiet mode.

When non-nil, enable quiet mode, via `--quiet'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "26"))

(defconst flycheck-stylelint-error-re
  (flycheck-rx-to-string
   '(: line-start (id (one-or-more word)) ": " (message) line-end))
  "Fallback regex for parsing stylelint errors from non-JSON output.")

(defun flycheck-parse-stylelint (output checker buffer)
  "Parse stylelint errors from OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

The CHECKER usually returns the errors as JSON.

If the CHECKER throws an Error it returns an Error message with a stacktrace."
  (condition-case nil
      (flycheck-parse-stylelint-json output checker buffer)

    ;; The output could not be parsed as JSON
    (json-error

     ;; Extract a flycheck error from the output (with a regular expression)
     ;; For match-string 4/5 see flycheck-rx-message/flycheck-rx-id
     (when (string-match flycheck-stylelint-error-re output)
       (list (flycheck-error-new-at
              1 nil 'error
              (match-string 4 output)
              :id (match-string 5 output)
              :checker checker
              :buffer buffer
              :filename (buffer-file-name buffer)))))))

(defun flycheck-parse-stylelint-json (output checker buffer)
  "Parse stylelint JSON errors from OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://stylelint.io/developer-guide/formatters/' for information
about the JSON format of stylelint."
  ;; stylelint returns a vector of result objects
  ;; Since we only passed one file, the first element is enough
  (let* ((stylelint-output (elt (json-parse-string output
                                                   :object-type 'plist
                                                   :null-object nil
                                                   :false-object nil)
                                0))
           (filename (buffer-file-name buffer))

           ;; Turn all deprecations into warnings
           (deprecations
            (mapcar (lambda (d)
                      (flycheck-error-new-at
                       1 nil 'warning
                       (plist-get d :text)
                       :id "Deprecation Warning"
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :deprecations)))

           ;; Turn all invalid options into errors
           (invalid-options
            (mapcar (lambda (io)
                      (flycheck-error-new-at
                       1 nil 'error
                       (plist-get io :text)
                       :id "Invalid Option"
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :invalidOptionWarnings)))

           ;; Read all linting warnings
           (warnings
            (mapcar (lambda (w)
                      (flycheck-error-new-at
                       (plist-get w :line) (plist-get w :column)
                       (pcase (plist-get w :severity)
                         (`"error"   'error)
                         (`"warning" 'warning)
                         ;; Default to info for unknown .severity
                         (_          'info))
                       (plist-get w :text)
                       :id (plist-get w :rule)
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :warnings))))

      ;; Return the combined errors (deprecations, invalid options, warnings)
      (append deprecations invalid-options warnings)))

(defun flycheck--stylelint-config-exists-p (checker)
  "Whether there is a valid stylelint CHECKER config for the current buffer."
  (zerop (flycheck-call-checker-process
          checker nil nil nil
          "--print-config" (flycheck-buffer-file-local-name "index.js"))))

(defun flycheck--stylelint-get-major-version (checker)
  "Return major version of stylelint CHECKER."
  (let ((cb (current-buffer)))
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer cb
          (flycheck-call-checker-process
           checker nil temp-buffer nil "--version"))
        (string-to-number (car (split-string (buffer-string) "\\.")))))))

(defun flycheck--stylelint-verify (checker)
  "Verify stylelint setup for CHECKER."
  (let ((have-config (flycheck--stylelint-config-exists-p checker)))
    (list
     (flycheck-verification-result-new
      :label "configuration available"
      :message (if have-config "yes" "no config file found")
      :face (if have-config 'success '(bold error)))
     (flycheck-verification-result-new
      :label "stylelint version"
      :message (number-to-string (flycheck--stylelint-get-major-version checker))
      :face 'success))))

(flycheck-define-checker css-stylelint
  "A CSS syntax and style checker using stylelint.

See URL `https://stylelint.io/'."
  :command ("stylelint"
            "--formatter" "json"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelint-config)
            "--stdin-filename" (eval (flycheck-buffer-file-local-name
                                      "style.css")))
  :standard-input t
  :verify (lambda (_) (flycheck--stylelint-verify 'css-stylelint))
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :handle-suspicious flycheck--stylelint-handle-suspicious
  :modes (css-mode css-ts-mode)
  :error-explainer
  (flycheck-error-explainer-from-url "https://stylelint.io/user-guide/rules/%s"))

(flycheck-def-option-var flycheck-cuda-language-standard nil cuda-nvcc
  "The CUDA language standard to use in nvcc."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-cuda-language-standard)

(flycheck-def-option-var flycheck-cuda-compiler-options '("-Wall" "-Wextra") cuda-nvcc
  "Additional options to pass to the compiler via `-Xcompiler'."
  :type '(choice (const :tag "No additional compiler options" nil)
                 (repeat :tag "Additional compiler options"
                         (string :tag "Compiler option")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "35"))

(flycheck-def-option-var flycheck-cuda-gencodes nil cuda-nvcc
  "GPU architectures to pass to nvcc via `-gencode'."
  :type '(repeat (string :tag "GPU architecture"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-cuda-includes nil cuda-nvcc
  "A list of additional include files for nvcc.

Relative paths are relative to the file being checked."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-cuda-definitions nil cuda-nvcc
  "Additional preprocessor definitions for nvcc.

The value of this variable is a list of strings, where each
string is an additional definition to pass to nvcc via `-D'."
  :type '(repeat (string :tag "Definitions"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-cuda-include-path nil cuda-nvcc
  "A list of include directories for nvcc."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-cuda-relaxed-constexpr nil cuda-nvcc
  "Enable calling host constexpr from device function for nvcc.

When non-nil, enable experimental calling of a constexpr __host__
function from a __device__ function."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "35"))

(flycheck-def-option-var flycheck-cuda-extended-lambda nil cuda-nvcc
  "Enable annotating lambda functions with __host__ or __device__.

When non-nil, enable experimental compilation of __host__ and
__device__ lambda functions."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "35"))

(flycheck-define-checker cuda-nvcc
  "A CUDA C/C++ syntax checker using nvcc.

See URL `https://developer.nvidia.com/cuda-llvm-compiler'."
  :command ("nvcc"
            "-c" ;; Compile Only
            "--output-file" "/dev/null" ;; avoid creating output .o
            "--x=cu" ;; explicitly specify it's a CUDA language file
            "-rdc=true" ;; Allow linking with external cuda funcions
            (option "-std=" flycheck-cuda-language-standard concat)
            (option-flag "--expt-relaxed-constexpr" flycheck-cuda-relaxed-constexpr)
            (option-flag "--expt-extended-lambda" flycheck-cuda-extended-lambda)
            (option-list "-include" flycheck-cuda-includes)
            (option-list "-gencode" flycheck-cuda-gencodes)
            (option-list "-Xcompiler" flycheck-cuda-compiler-options)
            (option-list "-D" flycheck-cuda-definitions concat)
            (option-list "-I" flycheck-cuda-include-path)
            source)
  :error-patterns
  ((error line-start
          (message "In file included from")
          " " (or "<stdin>" (file-name))
          ":" line ":" line-end)
   (error line-start (or "<stdin>" (file-name))
          "(" line "): error"
          (optional " #" (id (one-or-more digit) (optional "-D")))
          ": " (message) line-end)
   (error line-start (or "<stdin>" (file-name))
          ":" line ":" column
          ": fatal error"
          (optional " #" (id (one-or-more digit) (optional "-D")))
          ": " (optional (message)) line-end)
   (warning line-start (or "<stdin>" (file-name))
            "(" line "): warning"
            (optional " #" (id (one-or-more digit) (optional "-D")))
            ": " (message) line-end))
  :modes cuda-mode)


(flycheck-def-option-var flycheck-cwl-schema-path nil cwl
  "A path for the schema file for Common Workflow Language.

The value of this variable is a string that denotes a path for
the schema file of Common Workflow Language."
  :type '(choice (const :tag "None" nil)
                 (file :tag "Schema file"))
  :safe #'string-or-null-p)

(flycheck-define-checker cwl
  "A CWL syntax checker using Schema Salad validator.

Requires Schema Salad 2.6.20171101113912 or newer.
See URL `https://www.commonwl.org/v1.0/SchemaSalad.html'."
  :command ("schema-salad-tool"
            "--quiet"
            "--print-oneline"
            (eval flycheck-cwl-schema-path)
            source-inplace)
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column ":" (zero-or-more blank)
          (message (one-or-more not-newline))
          line-end))
  :modes cwl-mode)

(defconst flycheck-d-module-re
  (rx "module" (one-or-more (syntax whitespace))
      (group (one-or-more (not (syntax whitespace))))
      (zero-or-more (syntax whitespace))
      ";")
  "Regular expression to match a D module declaration.")

(defun flycheck-d-base-directory ()
  "Get the relative base directory path for this module."
  (let* ((file-name (buffer-file-name))
         (module-file (if (and file-name
                               (string= (file-name-nondirectory file-name)
                                        "package.d"))
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

(flycheck-def-args-var flycheck-dmd-args d-dmd
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker d-dmd
  "A D syntax checker using the DMD compiler.

Requires DMD 2.066 or newer.  See URL `https://dlang.org/'."
  :command ("dmd"
            "-debug"                    ; Compile in debug mode
            "-o-"                       ; Don't generate an object file
            "-vcolumns"                 ; Add columns in output
            "-wi" ; Compilation will continue even if there are warnings
            (eval (concat "-I" (flycheck-d-base-directory)))
            (option-list "-I" flycheck-dmd-include-path concat)
            (eval flycheck-dmd-args)
            (source ".d"))
  :error-patterns
  ((error line-start
          (file-name) "(" line "," column "): Error: " (message)
          line-end)
   (warning line-start (file-name) "(" line "," column "): "
            (or "Warning" "Deprecation") ": " (message) line-end)
   (info line-start (file-name) "(" line "," column "): "
         (one-or-more " ") (message) line-end))
  :modes d-mode)

(defun flycheck-dockerfile-hadolint-error-explainer (err)
  "Browse the docs for a hadolint (DL...) or ShellCheck (SC...) error ERR.
hadolint's own DL rules link to its wiki; the SC rules it forwards from
ShellCheck link to ShellCheck's wiki."
  (when-let* ((id (flycheck-error-id err)))
    (cond
     ((string-prefix-p "DL" id)
      (cons 'url (format "https://github.com/hadolint/hadolint/wiki/%s" id)))
     ((string-prefix-p "SC" id)
      (cons 'url (format "https://github.com/koalaman/shellcheck/wiki/%s" id))))))

(flycheck-def-config-file-var flycheck-dockerfile-hadolint-config
    dockerfile-hadolint '(".hadolint.yaml" ".hadolint.yml"))

(flycheck-def-args-var flycheck-dockerfile-hadolint-args dockerfile-hadolint
  :package-version '(flycheck . "39"))

(flycheck-define-checker dockerfile-hadolint
  "A Dockerfile syntax checker using hadolint.

See URL `https://github.com/hadolint/hadolint/'."
  :command ("hadolint" "--format" "sarif"
            (config-file "--config" flycheck-dockerfile-hadolint-config)
            (eval flycheck-dockerfile-hadolint-args)
            "-")
  :standard-input t
  :error-parser flycheck-parse-sarif
  :error-filter
  (lambda (errors)
    ;; hadolint reports stdin as "-" for lint findings but as
    ;; "/dev/stdin" for parse errors; strip both so the errors attach to
    ;; the current buffer
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names
      "/dev/stdin" (flycheck-remove-error-file-names "-" errors))))
  :error-explainer flycheck-dockerfile-hadolint-error-explainer
  :modes (dockerfile-mode dockerfile-ts-mode))

(defun flycheck-credo--working-directory (&rest _ignored)
  "Check if `credo' is installed as dependency in the application."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "deps/credo")))

(flycheck-def-option-var flycheck-elixir-credo-strict nil elixir-credo
  "Enable strict mode in `credo'.

When non-nil, pass the `--strict' flag to credo."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "32"))

(flycheck-def-args-var flycheck-elixir-credo-args elixir-credo
  :package-version '(flycheck . "39"))

(flycheck-define-checker elixir-credo
  "An Elixir checker for static code analysis using Credo.

See `https://credo-ci.org/'."
  :command ("mix" "credo"
            (option-flag "--strict" flycheck-elixir-credo-strict)
            "--format" "flycheck"
            (eval flycheck-elixir-credo-args)
            "--read-from-stdin" source-original)
  :standard-input t
  :working-directory flycheck-credo--working-directory
  :enabled flycheck-credo--working-directory
  :error-patterns
  ((info line-start
         (file-name) ":" line (optional ":" column) ": "
         (or "F" "R" "C")  ": " (message) line-end)
   (warning line-start
            (file-name) ":" line (optional ":" column) ": "
            (or "D" "W")  ": " (message) line-end))
  :modes (elixir-mode elixir-ts-mode))

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

(defun flycheck-emacs-lisp-bytecomp-config-form ()
  "Prepare an Emacs Lisp form to set byte-compiler variables."
  (flycheck-sexp-to-string
   `(progn
      (require 'bytecomp)
      (setq byte-compile-root-dir
            ,(if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))))

(defconst flycheck-emacs-lisp-check-form
  (flycheck-prepare-emacs-lisp-form
    ;; Mitigate CVE-2024-53920: prevent arbitrary code execution via
    ;; file-local eval: directives during byte-compilation.
    (setq enable-local-eval nil
          enable-local-variables :safe)
    ;; The subprocess only byte-compiles to collect warnings; producing
    ;; .eln files is a wasted side effect that also pollutes the user's
    ;; native-comp cache, so disable native compilation entirely.
    (setq no-native-compile t)
    ;; Keep track of the generated bytecode files, to delete them after byte
    ;; compilation.
    (require 'bytecomp)
    (defvar flycheck-byte-compiled-files nil)
    (let ((byte-compile-dest-file-function
           (lambda (source)
             (let ((temp-file (make-temp-file (file-name-nondirectory source))))
               (push temp-file flycheck-byte-compiled-files)
               temp-file))))
      (unwind-protect
          (byte-compile-file (car command-line-args-left))
        (mapc (lambda (f) (ignore-errors (delete-file f)))
              flycheck-byte-compiled-files))
      (when (bound-and-true-p flycheck-emacs-lisp-check-declare)
        (check-declare-file (car command-line-args-left))))))

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

When nil, never initialize packages.  When `auto', initialize
packages only when checking `user-init-file' or files from
`user-emacs-directory'.  For any other non-nil value, always
initialize packages.

When initializing packages is enabled the `emacs-lisp' syntax
checker calls `package-initialize' before byte-compiling the file
to be checked.  It also sets `package-user-dir' according to
`flycheck-emacs-lisp-package-user-dir'."
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
             (or (flycheck-in-user-emacs-directory-p
                  (or buffer-file-name default-directory))
                 ;; `user-init-file' is nil in non-interactive sessions.  Now,
                 ;; no user would possibly use Flycheck in a non-interactive
                 ;; session, but our unit tests run non-interactively, so we
                 ;; have to handle this case anyway
                 (and user-init-file buffer-file-name
                      (flycheck-same-files-p buffer-file-name user-init-file)))
           value)))
    (when shall-initialize
      ;; If packages shall be initialized, return the corresponding form,
      ;; otherwise make Flycheck ignore the option by returning nil.
      flycheck-emacs-lisp-package-initialize-form)))

(flycheck-def-option-var flycheck-emacs-lisp-package-user-dir nil emacs-lisp
  "Package directory for the Emacs Lisp syntax checker.

If set to a string set `package-user-dir' to the value of this
variable before initializing packages. If set to nil just inherit
the value of `package-user-dir' from the running Emacs session.

This variable has no effect, if
`flycheck-emacs-lisp-initialize-packages' is nil."
  :type '(choice (const :tag "Default package directory" nil)
                 (directory :tag "Custom package directory"))
  :risky t
  :package-version '(flycheck . "0.14"))

(defun flycheck-option-emacs-lisp-package-user-dir (value)
  "Option VALUE filter for `flycheck-emacs-lisp-package-user-dir'."
  ;; Inherit the package directory from our Emacs session
  (let ((value (or value (bound-and-true-p package-user-dir))))
    (when value
      (flycheck-sexp-to-string `(setq package-user-dir ,value)))))

(flycheck-def-option-var flycheck-emacs-lisp-check-declare nil emacs-lisp
  "If non-nil, check ‘declare-function’ forms using ‘check-declare-file’."
  :type '(choice (const :tag "Do not check declare forms" nil)
                 (const :tag "Check declare forms" t))
  :risky t
  :package-version '(flycheck . "31"))

(defun flycheck-option-emacs-lisp-check-declare (value)
  "Option VALUE filter for `flycheck-emacs-lisp-check-declare'."
  (when value
    (flycheck-sexp-to-string
     `(progn
        (defvar flycheck-emacs-lisp-check-declare)
        (setq flycheck-emacs-lisp-check-declare ,value)))))

(defun flycheck--emacs-lisp-enabled-p ()
  "Check whether to enable Emacs Lisp checker in the current buffer."
  (not
   (or
    ;; Do not check buffers used for autoloads generation during package
    ;; installation.  These buffers are too short-lived for being checked, and
    ;; doing so causes spurious errors.  See
    ;; https://github.com/flycheck/flycheck/issues/45 and
    ;; https://github.com/bbatsov/prelude/issues/248.  We must also not check
    ;; compilation buffers, but as these are ephemeral, Flycheck won't check
    ;; them anyway.
    (flycheck-autoloads-file-p)
    ;; Package-manager manifests and dir-locals files contain data, not
    ;; code, and don't need to follow Checkdoc conventions either.
    (and (buffer-file-name)
         (member (file-name-nondirectory (buffer-file-name))
                 '("Cask" "Carton" "Eask" "Eask-local"
                   ".dir-locals.el" ".dir-locals-2.el"))))))

(defun flycheck--emacs-lisp-byte-compile-enabled-p ()
  "Check whether to enable the Emacs Lisp byte compiler checker.

On Emacs 30+, the checker is only enabled for trusted files, to
mitigate CVE-2024-53920: byte-compilation involves macro expansion
which can execute arbitrary code.  Customize `trusted-content' to
mark files or directories as trusted.

Checkdoc doesn't expand macros, so `emacs-lisp-checkdoc' stays
enabled for untrusted files, like in Emacs core."
  (and (flycheck--emacs-lisp-enabled-p)
       (or (not (fboundp 'trusted-content-p))
           (trusted-content-p))))

(defun flycheck--emacs-lisp-checkdoc-enabled-p ()
  "Check whether to enable Emacs Lisp Checkdoc in the current buffer."
  (and (flycheck--emacs-lisp-enabled-p)
       ;; These files are valid Lisp, but don't contain "standard" comments.
       (not (and (buffer-file-name)
                 (member (file-name-nondirectory (buffer-file-name))
                         '("Eldev" "Eldev-local"))))))

(flycheck-define-checker emacs-lisp
  "An Emacs Lisp syntax checker using the Emacs Lisp Byte compiler.

On Emacs 30+, this checker is only enabled for files the user has
marked as trusted via the `trusted-content' variable, to mitigate
CVE-2024-53920 (byte-compilation involves macro expansion which can
execute arbitrary code).

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
            (option "--eval" flycheck-emacs-lisp-check-declare nil
                    flycheck-option-emacs-lisp-check-declare)
            "--eval" (eval (flycheck-emacs-lisp-bytecomp-config-form))
            "--eval" (eval flycheck-emacs-lisp-check-form)
            "--"
            source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":"
          (zero-or-more whitespace) "Error:" (zero-or-more whitespace)
          (message (zero-or-more not-newline)
                   (zero-or-more "\n    " (zero-or-more not-newline)))
          line-end)
   ;; Parse errors subsequent to byte-compilation (e.g., unbalanced
   ;; parentheses) may lack line/column info.
   (error line-start (file-name) ":"
          (zero-or-more whitespace) "Error:" (zero-or-more whitespace)
          (message (or "End of file during parsing"
                       (seq "Invalid read syntax:"
                            (zero-or-more not-newline))))
          (optional "," (zero-or-more whitespace) line
                    "," (zero-or-more whitespace) column)
          ;; Emacs 31 goes on to name the buffer it was reading, which is
          ;; always the compiler's own and tells nobody anything
          (optional ":" (zero-or-more not-newline))
          line-end)
   (warning line-start (file-name) ":" line ":" column ":"
            (zero-or-more whitespace) "Warning:" (zero-or-more whitespace)
            (message (zero-or-more not-newline)
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end)
   ;; Up to Emacs 28 the message followed on its own indented line; since
   ;; Emacs 29 the whole warning is on one line.
   (warning line-start (file-name) ":" line (optional ":" column) ":"
            (zero-or-more whitespace) "Warning (check-declare): said"
            (or (seq "\n" (one-or-more " ")) " ")
            (message (zero-or-more not-newline)
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end))
  :error-filter
  (lambda (errors)
    (flycheck-fill-empty-line-numbers
     (flycheck-collapse-error-message-whitespace
      (flycheck-sanitize-errors errors))))
  :modes (emacs-lisp-mode lisp-interaction-mode)
  :enabled flycheck--emacs-lisp-byte-compile-enabled-p
  :predicate
  (lambda ()
    ;; Do not check buffers that should not be byte-compiled.  The checker
    ;; process will refuse to compile these, which would confuse Flycheck
    (not (bound-and-true-p no-byte-compile)))
  :next-checkers (emacs-lisp-checkdoc))

(defconst flycheck-emacs-lisp-checkdoc-form
  (flycheck-prepare-emacs-lisp-form
    (require 'elisp-mode)
    (require 'checkdoc)

    (let ((source (car command-line-args-left))
          ;; Remember the default directory of the process
          (process-default-directory default-directory))
      ;; Note that we deliberately use our custom approach even despite of
      ;; `checkdoc-file' which was added to Emacs 25.1.  While it's conceptually
      ;; the better thing, its implementation has too many flaws to be of use
      ;; for us.
      (with-temp-buffer
        (insert-file-contents source 'visit)
        (setq buffer-file-name source)
        ;; And change back to the process default directory to make file-name
        ;; back-substitution work
        (setq default-directory process-default-directory)
        (with-demoted-errors "Error in checkdoc: %S"
          ;; Checkdoc needs the Emacs Lisp syntax table and comment syntax to
          ;; parse sexps and identify docstrings correctly; see
          ;; https://github.com/flycheck/flycheck/issues/833
          (delay-mode-hooks (emacs-lisp-mode))
          (setq delayed-mode-hooks nil)
          (checkdoc-current-buffer t)
          (with-current-buffer checkdoc-diagnostic-buffer
            (princ (buffer-substring-no-properties (point-min) (point-max)))
            (kill-buffer)))))))

(defconst flycheck-emacs-lisp-checkdoc-variables
  `(checkdoc-symbol-words
    checkdoc-arguments-in-order-flag
    checkdoc-force-history-flag
    checkdoc-permit-comma-termination-flag
    checkdoc-force-docstrings-flag
    checkdoc-package-keywords-flag
    checkdoc-spellcheck-documentation-flag
    checkdoc-verb-check-experimental-flag
    checkdoc-max-keyref-before-warn
    sentence-end-double-space
    checkdoc-column-zero-backslash-before-paren
    ,@(and (>= emacs-major-version 31)
           '(checkdoc-allow-quoting-nil-and-t
             checkdoc-arguments-missing-flag)))
  "Variables inherited by the checkdoc subprocess.")

(defun flycheck-emacs-lisp-checkdoc-variables-form ()
  "Make a sexp to pass relevant variables to a checkdoc subprocess.

Variables are taken from `flycheck-emacs-lisp-checkdoc-variables'."
  `(progn
     ,@(mapcar (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                (seq-filter #'boundp flycheck-emacs-lisp-checkdoc-variables))))

(defun flycheck-org-lint-available-p ()
  "Check if org-lint is available."
  (and (fboundp 'org-lint)
       (require 'org nil 'no-error)))

(flycheck-define-checker emacs-lisp-checkdoc
  "An Emacs Lisp style checker using CheckDoc.

The checker runs `checkdoc-current-buffer'."
  :command ("emacs" (eval flycheck-emacs-args)
            "--eval" (eval (flycheck-sexp-to-string
                            (flycheck-emacs-lisp-checkdoc-variables-form)))
            "--eval" (eval flycheck-emacs-lisp-checkdoc-form)
            "--" source)
  :error-patterns
  ((info line-start (file-name) ":" line ": " (message) line-end))
  :modes (emacs-lisp-mode)
  :enabled flycheck--emacs-lisp-checkdoc-enabled-p)

(dolist (checker '(emacs-lisp emacs-lisp-checkdoc))
  (setf (car (flycheck-checker-get checker 'command))
        flycheck-this-emacs-executable))

(flycheck-define-generic-checker 'org-lint
  "An Org mode syntax checker using `org-lint'.

The checker runs `org-lint' in the current Emacs process, so it
has access to all installed packages and user configuration."
  :start (lambda (checker callback)
           (condition-case err
               (let ((errors
                      (delq nil
                            (mapcar
                             (lambda (e)
                               (pcase e
                                 (`(,_n [,line ,_trust ,desc ,_checker])
                                  (flycheck-error-new-at
                                   (if (stringp line)
                                       (string-to-number line)
                                     line)
                                   nil 'info desc
                                   :checker checker))
                                 (_
                                  (flycheck-error-new-at
                                   1 nil 'warning
                                   (format "Unexpected org-lint format: %S" e)
                                   :checker checker))))
                             (org-lint)))))
                 (funcall callback 'finished errors))
             (error (funcall callback 'errored
                             (error-message-string err)))))
  :modes '(org-mode)
  :enabled #'flycheck-org-lint-available-p
  :verify (lambda (_)
            (let ((org-version (when (require 'org nil 'no-error)
                                 (org-version))))
              (list (flycheck-verification-result-new
                     :label "Org-lint available"
                     :message (if (fboundp 'org-lint)
                                  (format "yes (Org %s)" org-version)
                                "no")
                     :face (if (fboundp 'org-lint) 'success 'warning))))))

(defun flycheck-ember-template--check-for-config (&rest _ignored)
  "Check the required config file is available up the file system."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name ".template-lintrc.js")))

(defun flycheck-ember-template--parse-error (output checker buffer)
  "Parse Ember-template-lint errors/warnings from JSON OUTPUT.
CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .line
               .column
               (pcase .severity
                 (2 'error)
                 (1 'warning)
                 (_ 'warning))
               .message
               :id .rule
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer))))
          (cdr (car (car (flycheck-parse-json output))))))

(define-obsolete-variable-alias 'flycheck-ember-template-lintrc
  'flycheck-ember-template-lint-config "39")
(flycheck-def-config-file-var flycheck-ember-template-lint-config
    ember-template
    ".template-lintrc.js")

(flycheck-define-checker ember-template
  "An Ember template checker using ember-template-lint."
  :command ("ember-template-lint"
            (config-file "--config-path" flycheck-ember-template-lint-config)
            "--filename" source-original
            "--format=json")
  :standard-input t
  :error-parser flycheck-ember-template--parse-error
  :modes web-mode
  :enabled flycheck-ember-template--check-for-config
  :working-directory flycheck-ember-template--check-for-config)

(flycheck-def-option-var flycheck-erlang-include-path nil erlang
  "A list of include directories for Erlang.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of erlc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-erlang-library-path nil erlang
  "A list of library directories for Erlang.

The value of this variable is a list of strings, where each
string is a directory to add to the library path of erlc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Library directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-args-var flycheck-erlang-args erlang
  :package-version '(flycheck . "39"))

(flycheck-define-checker erlang
  "An Erlang syntax checker using the Erlang interpreter.

See URL `https://www.erlang.org/'."
  :command ("erlc"
            "-o" temporary-directory
            (option-list "-I" flycheck-erlang-include-path)
            (option-list "-pa" flycheck-erlang-library-path)
            "-Wall"
            (eval flycheck-erlang-args)
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" (optional column ":")
            " Warning:" (message) line-end)
   (error line-start (file-name) ":" line ":" (optional column ":") " "
          (message) line-end))
  :modes erlang-mode
  :enabled (lambda () (and buffer-file-name
                           (string-suffix-p ".erl" (buffer-file-name)))))

(defun flycheck--contains-rebar-config (dir-name)
  "Return DIR-NAME if rebar config file exists in DIR-NAME, nil otherwise."
  (when (or (file-exists-p (expand-file-name "rebar.config" dir-name))
            (file-exists-p (expand-file-name "rebar.config.script" dir-name)))
    dir-name))

(defun flycheck--locate-rebar3-project-root
    (file-name &optional prev-file-name acc)
  "Find the top-most rebar project root for source FILE-NAME.

A project root directory is any directory containing a
rebar.config file.  Find the top-most directory to move out of any
nested dependencies.

FILE-NAME is a source file for which to find the project.

PREV-FILE-NAME helps us prevent infinite looping

ACC is an accumulator that keeps the list of results, the first
non-nil of which will be our project root.

Return the absolute path to the directory"
  (if (string= file-name prev-file-name)
      (car (remove nil acc))
    (let ((current-dir (file-name-directory file-name)))
      (flycheck--locate-rebar3-project-root
       (directory-file-name current-dir)
       file-name
       (cons (flycheck--contains-rebar-config current-dir) acc)))))

(defun flycheck-rebar3-project-root (&optional _checker)
  "Return directory where rebar.config is located."
  (when buffer-file-name
    (flycheck--locate-rebar3-project-root buffer-file-name)))

(flycheck-def-option-var flycheck-erlang-rebar3-profile nil erlang-rebar3
  "The rebar3 profile to use.

The profile used when compiling, if VALUE is nil \"test\" will be used
when the file is located in test directory, otherwise \"default\" will be
used as profile."
  :type '(choice (const :tag "Automatic" nil)
                 (string :tag "Profile"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))

(defun flycheck-erlang-rebar3-get-profile ()
  "Return rebar3 profile.

Use flycheck-erlang-rebar3-profile if set, otherwise use test or eqc profile if
directory name is \"test\" or \"eqc\", or else \"default\"."
  (or
   flycheck-erlang-rebar3-profile
   (seq-contains-p '("test" "eqc")
                   (and buffer-file-name
                        (file-name-base
                         (directory-file-name
                          (file-name-directory buffer-file-name)))))
   "default"))

(defconst flycheck-rebar3--diagnostic-rx
  (rx line-start (zero-or-more " ") "┌─ " (group (one-or-more (not (any "\n"))))
      ":" (zero-or-more " ") "\n"
      ;; The empty gutter line above the source
      (zero-or-more " ") "│" (zero-or-more " ") "\n"
      ;; The offending line, echoed after its number
      (zero-or-more " ") (group (one-or-more digit)) " │"
      (group (zero-or-more " ")) (zero-or-more (not (any "\n"))) "\n"
      ;; And the message, under a mark sitting at the column it means
      (zero-or-more " ") "│" (group (zero-or-more " ")) "╰"
      (one-or-more "─") " " (group (one-or-more (not (any "\n")))))
  "Matches one diagnostic in the format rebar3 3.24 introduced.")

(defun flycheck-parse-rebar3--boxed (output checker buffer)
  "Parse the diagnostics rebar3 draws in a box out of OUTPUT.

CHECKER and BUFFER are as in `flycheck-parse-output'.

The mark under the offending line is what says which column the
diagnostic is about, so the column is the distance between that
mark and the start of the echoed source."
  (let (errors (start 0))
    (while (string-match flycheck-rebar3--diagnostic-rx output start)
      (setq start (match-end 0))
      (let* ((file (match-string 1 output))
             (line (string-to-number (match-string 2 output)))
             (source-indent (length (match-string 3 output)))
             (mark-indent (length (match-string 4 output)))
             (message (match-string 5 output))
             (warningp (string-prefix-p "Warning: " message)))
        (push (flycheck-error-new-at
               line
               (max 1 (1+ (- mark-indent source-indent)))
               (if warningp 'warning 'error)
               (if warningp (substring message (length "Warning: ")) message)
               :checker checker :buffer buffer :filename file)
              errors)))
    (nreverse errors)))

(defun flycheck-parse-rebar3 (output checker buffer)
  "Parse rebar3's OUTPUT, in either of the two shapes it comes in.

CHECKER and BUFFER are as in `flycheck-parse-output'.

rebar3 3.24 replaced `file:line:column: message' with a box drawn
around the offending line.  Older rebar3 is still about, so the
plain form is still read when the boxed one finds nothing."
  (let ((plain (ansi-color-filter-apply output)))
    (or (flycheck-parse-rebar3--boxed plain checker buffer)
        (flycheck-parse-with-patterns plain checker buffer))))

(flycheck-define-checker erlang-rebar3
  "An Erlang syntax checker using the rebar3 build tool."
  :command ("rebar3" "as" (eval (flycheck-erlang-rebar3-get-profile)) "compile")
  :error-parser flycheck-parse-rebar3
  :error-patterns
  ((warning line-start (file-name) ":" line ":" (optional column ":")
            " Warning:" (message) line-end)
   (error line-start (file-name) ":" line ":" (optional column ":") " "
          (message) line-end))
  :modes erlang-mode
  :enabled flycheck-rebar3-project-root
  :predicate flycheck-buffer-saved-p
  :working-directory flycheck-rebar3-project-root)

(flycheck-def-args-var flycheck-gfortran-args fortran-gfortran
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-gfortran-include-path nil fortran-gfortran
  "A list of include directories for GCC Fortran.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of gcc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gfortran-language-standard nil
                         fortran-gfortran
  "The language standard to use in GFortran.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'string-or-null-p
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

In any other case, an error is signaled."
  :type '(choice (const :tag "Guess layout from extension" nil)
                 (const :tag "Free form layout" free)
                 (const :tag "Fixed form layout" fixed))
  :safe (lambda (value) (or (not value) (memq value '(free fixed))))
  :package-version '(flycheck . "0.20"))

(defun flycheck-option-gfortran-layout (value)
  "Option VALUE filter for `flycheck-gfortran-layout'."
  (pcase value
    (`nil nil)
    (`free "free-form")
    (`fixed "fixed-form")
    (_ (error "Invalid value for flycheck-gfortran-layout: %S" value))))

(flycheck-def-option-var flycheck-gfortran-warnings '("all" "extra")
                         fortran-gfortran
  "A list of warnings for GCC Fortran.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the gfortran manual at URL
`https://gcc.gnu.org/onlinedocs/gfortran/' for more information
about warnings"
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker fortran-gfortran
  "A Fortran syntax checker using GCC.

Uses GCC's Fortran compiler gfortran.  See URL
`https://gcc.gnu.org/onlinedocs/gfortran/'."
  :command ("gfortran"
            "-fsyntax-only"
            "-fshow-column"
            ;; Do not visually indicate the source location
            "-fno-diagnostics-show-caret"
            ;; Do not show the corresponding warning group
            "-fno-diagnostics-show-option"
            ;; Fortran has similar include processing as C/C++
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gfortran-language-standard concat)
            (option "-f" flycheck-gfortran-layout concat
                    flycheck-option-gfortran-layout)
            (option-list "-W" flycheck-gfortran-warnings concat)
            (option-list "-I" flycheck-gfortran-include-path concat)
            (eval flycheck-gfortran-args)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line (or ":" ".") column (or ": " ":\n")
          (or (= 3 (zero-or-more not-newline) "\n") "")
          (or "Error" "Fatal Error") ": "
          (message) line-end)
   (warning line-start (file-name) ":" line (or ":" ".") column (or ": " ":\n")
            (or (= 3 (zero-or-more not-newline) "\n") "")
            "Warning: " (message) line-end))
  :modes (fortran-mode f90-mode))

(flycheck-def-config-file-var flycheck-yaml-actionlint-config
                              yaml-actionlint nil
  :package-version '(flycheck . "39"))

(flycheck-def-args-var flycheck-yaml-actionlint-args yaml-actionlint
  :package-version '(flycheck . "39"))

(flycheck-define-checker yaml-actionlint
  "A YAML syntax checker using actionlint.

See URL https://github.com/rhysd/actionlint/."
  :command ("actionlint" "-oneline"
            (config-file "-config-file" flycheck-yaml-actionlint-config)
            (eval flycheck-yaml-actionlint-args)
            source)
  :error-patterns ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes (yaml-mode yaml-ts-mode)
  :predicate (lambda ()
               (and buffer-file-name
                    (string-match-p
                     (rx (or ".github/workflows" ".github\\workflows"))
                     (buffer-file-name)))))

(flycheck-def-args-var flycheck-go-gofmt-args go-gofmt
  :package-version '(flycheck . "39"))

(flycheck-define-checker go-gofmt
  "A Go syntax and style checker using the gofmt utility.

See URL `https://go.dev/cmd/gofmt/'."
  :command ("gofmt"
            (eval flycheck-go-gofmt-args))
  :standard-input t
  :error-patterns
  ((error line-start "<standard input>:" line ":" column ": "
          (message) line-end))
  :modes (go-mode go-ts-mode)
  :next-checkers ((warning . go-vet)
                  ;; Fall back, if go-vet doesn't exist
                  (warning . go-build) (warning . go-test)
                  (warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-def-args-var flycheck-go-vet-args go-vet
  :package-version '(flycheck . "39"))

(flycheck-define-checker go-vet
  "A Go syntax checker using the `go vet' command.

See URL `https://go.dev/cmd/go/' and URL
`https://pkg.go.dev/cmd/vet/'."
  :command ("go" "vet"
            (option "-tags=" flycheck-go-build-tags concat
                    flycheck-option-comma-separated-list)
            (eval flycheck-go-vet-args)
            (source ".go"))
  :error-patterns
  ((warning line-start (file-name) ":" line ": " (message) line-end))
  :modes (go-mode go-ts-mode)
  :next-checkers (go-build
                  go-test
                  ;; Fall back if `go build' or `go test' can be used
                  go-errcheck
                  go-unconvert
                  go-staticcheck)
  :verify (lambda (_)
            (let* ((go (flycheck-checker-executable 'go-vet))
                   (have-vet (member "vet" (ignore-errors
                                             (flycheck--process-file-lines
                                              (file-local-name go) "tool")))))
              (list
               (flycheck-verification-result-new
                :label "go tool vet"
                :message (if have-vet "present" "missing")
                :face (if have-vet 'success '(bold error)))))))

(flycheck-def-option-var flycheck-go-build-tags nil
                         (go-vet go-build go-test go-errcheck go-staticcheck)
  "A list of tags for `go build'.

Each item is a string with a tag to be given to `go build'."
  :type '(repeat (string :tag "Tag"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.25"))


(flycheck-def-option-var flycheck-go-version nil go-staticcheck
  "The version of go that should be targeted by `staticcheck'.

Should be a string representing a version, like 1.6 or 1.11.4.
See `https://staticcheck.io/docs/#targeting-go-versions' for
details."
  :type '(choice (const :tag "Unspecified" nil)
                 (string :tag "Version"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "0.32"))

(flycheck-def-args-var flycheck-go-build-args go-build
  :package-version '(flycheck . "39"))

(flycheck-define-checker go-build
  "A Go syntax and type checker using the `go build' command.

See URL `https://go.dev/cmd/go/'."
  :command ("go" "build"
            ;; multiple tags are comma-separated: "dev,debug"
            (option "-tags=" flycheck-go-build-tags concat
                    flycheck-option-comma-separated-list)
            (eval flycheck-go-build-args)
            "-o" null-device)
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end)
   ;; Catch error message about multiple packages in a directory, which doesn't
   ;; follow the standard error message format.
   (info line-start
         (message "can't load package: package "
                  (one-or-more (not (any ?: ?\n)))
                  ": found packages "
                  (one-or-more not-newline))
         line-end))
  :error-filter
  (lambda (errors)
    (dolist (error errors)
      (unless (flycheck-error-line error)
        ;; Flycheck ignores errors without line numbers, but the error
        ;; message about multiple packages in a directory doesn't come with a
        ;; line number, so inject a fake one.
        (setf (flycheck-error-line error) 1)))
    errors)
  :modes (go-mode go-ts-mode)
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))
  :next-checkers ((warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-def-args-var flycheck-go-test-args go-test
  :package-version '(flycheck . "39"))

(flycheck-define-checker go-test
  "A Go syntax and type checker using the `go test' command.

See URL `https://go.dev/cmd/go/'."
  :command ("go" "test"
            (option "-tags=" flycheck-go-build-tags concat
                    flycheck-option-comma-separated-list)
            (eval flycheck-go-test-args)
            "-c" "-o" null-device)
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes (go-mode go-ts-mode)
  :predicate
  (lambda () (and (flycheck-buffer-saved-p)
                  (string-suffix-p "_test.go" (buffer-file-name))))
  :next-checkers ((warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-def-args-var flycheck-go-errcheck-args go-errcheck
  :package-version '(flycheck . "39"))

(flycheck-define-checker go-errcheck
  "A Go checker for unchecked errors.

Requires errcheck newer than commit 8515d34 (Aug 28th, 2015).

See URL `https://github.com/kisielk/errcheck'."
  :command ("errcheck"
            "-abspath"
            (option "-tags=" flycheck-go-build-tags concat
                    flycheck-option-comma-separated-list)
            (eval flycheck-go-errcheck-args)
            ".")
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column (or (one-or-more "\t") ": " ":\t")
            (message)
            line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
        (when-let* ((message (flycheck-error-message err)))
          ;; Improve the messages reported by errcheck to make them more clear.
          (setf (flycheck-error-message err)
                (format "Ignored `error` returned from `%s`" message)))))
    errors)
  :modes (go-mode go-ts-mode)
  :predicate (lambda () (flycheck-buffer-saved-p))
  :next-checkers ((warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-define-checker go-unconvert
  "A Go checker looking for unnecessary type conversions.

See URL `https://github.com/mdempsky/unconvert'."
  :command ("unconvert" ".")
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes (go-mode go-ts-mode)
  :predicate (lambda () (flycheck-buffer-saved-p)))

(flycheck-define-checker go-staticcheck
  "A Go checker that performs static analysis and linting using
the `staticcheck' command.

`staticcheck' is explicitly fully compatible with \"the last two
versions of go\". `staticcheck' can target earlier versions (with
limited features) if `flycheck-go-version' is set. See URL
`https://staticcheck.io/'."
  :command ("staticcheck" "-f" "json"
            (option "-tags" flycheck-go-build-tags nil
                    flycheck-option-comma-separated-list)
            (option "-go" flycheck-go-version))

  :error-parser flycheck-parse-go-staticcheck
  :error-explainer
  (flycheck-error-explainer-from-url "https://staticcheck.dev/docs/checks#%s")
  :modes (go-mode go-ts-mode))

(flycheck-define-checker groovy
  "A groovy syntax checker using groovy compiler API.

See URL `https://www.groovy-lang.org'."
  :command ("groovy" "-e"
            "import org.codehaus.groovy.control.*

unit = new CompilationUnit()
unit.addSource(\"input\", System.in)

try {
    unit.compile(Phases.CONVERSION)
} catch (MultipleCompilationErrorsException e) {
    e.errorCollector.write(new PrintWriter(System.out, true), null)
}")
  :standard-input t
  :error-patterns
  ((error line-start "input: " line ":" (message)
          " @ line " line ", column " column "." line-end))
  :modes groovy-mode)

(flycheck-define-checker haml-lint
  "HAML-Lint style checker.

See URL `https://github.com/sds/haml-lint'."
  :command ("haml-lint" "--no-color" "--no-summary" source)
  :error-patterns
  ((error line-start (file-name) ":" line " [E]" (message) line-end)
   (warning line-start (file-name) ":" line " [W]" (message) line-end))
  :modes haml-mode)

(flycheck-define-checker handlebars
  "A Handlebars syntax checker using the Handlebars compiler.

See URL `https://handlebarsjs.com/'."
  :command ("handlebars" "-i-")
  :standard-input t
  :error-patterns
  ((error line-start
          "Error: Parse error on line " line ":" (optional "\r") "\n"
          (zero-or-more not-newline) "\n" (zero-or-more not-newline) "\n"
          (message) line-end))
  :modes (handlebars-mode handlebars-sgml-mode web-mode)
  :predicate
  (lambda ()
    (if (eq major-mode 'web-mode)
        ;; Check if this is a handlebars file since web-mode does not store the
        ;; non-canonical engine name
        (let* ((regexp-alist (bound-and-true-p web-mode-engine-file-regexps))
               (pattern (cdr (assoc "handlebars" regexp-alist))))
          (and pattern (buffer-file-name)
               (string-match-p pattern (buffer-file-name))))
      t)))

(defconst flycheck-haskell-module-re
  (rx line-start (zero-or-more (or "\n" (any space)))
      "module" (one-or-more (or "\n" (any space)))
      (group (one-or-more (not (any space "(" "\n")))))
  "Regular expression for a Haskell module name.")

(flycheck-def-args-var flycheck-ghc-args (haskell-stack-ghc haskell-ghc)
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-ghc-stack-use-nix nil haskell-stack-ghc
  "Whether to enable nix support in stack.

When non-nil, stack will append '--nix' flag to any call."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "26"))

(flycheck-def-option-var flycheck-ghc-stack-project-file nil haskell-stack-ghc
  "Override project stack.yaml file.

The value of this variable is a file path that refers to a yaml
file for the current stack project. Relative file paths are
resolved against the checker's working directory. When non-nil,
stack will get overridden value via `--stack-yaml'."
  :type '(choice (const :tag "Unspecified" nil)
                 (file :tag "Project file"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))

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

(flycheck-def-option-var flycheck-ghc-search-path nil
                         (haskell-stack-ghc haskell-ghc)
  "Module search path for (Stack) GHC.

The value of this variable is a list of strings, where each
string is a directory containing Haskell modules.  Each directory
is added to the GHC search path via `-i'."
  :type '(repeat (directory :tag "Module directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-ghc-language-extensions nil
                         (haskell-stack-ghc haskell-ghc)
  "Language extensions for (Stack) GHC.

The value of this variable is a list of strings, where each
string is a Haskell language extension, as in the LANGUAGE
pragma.  Each extension is enabled via `-X'."
  :type '(repeat (string :tag "Language extension"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.19"))

;; A hash table (not the scalar of earlier versions -- hence the new name,
;; so an in-session reload does not leave a stale non-table value that
;; `gethash' would choke on).
(defvar flycheck--haskell-ghc-cache-directories (make-hash-table :test 'equal)
  "The cache directories for `ghc' output, keyed by host.
The key is the remote identifier of `default-directory' (see
`file-remote-p'), or nil for the local host, since `ghc' runs on
the host of the checked buffer and needs a cache directory there.")

(defun flycheck-haskell-ghc-cache-directory ()
  "Get the cache location for `ghc' output.

If no cache directory exists yet for the current host, create one
on that host and return it.  Otherwise return the previously used
cache directory."
  (let ((host (file-remote-p default-directory)))
    (or (gethash host flycheck--haskell-ghc-cache-directories)
        ;; `make-nearby-temp-file' creates the directory on the host of
        ;; `default-directory', so `ghc' running there can write to it.
        (puthash host
                 (make-nearby-temp-file "flycheck-haskell-ghc-cache" 'directory)
                 flycheck--haskell-ghc-cache-directories))))

(defun flycheck--locate-dominating-file-matching (directory regexp)
  "Search for a file in directory hierarchy starting at DIRECTORY.

Look up the directory hierarchy from DIRECTORY for a directory
containing a file that matches REGEXP."
  (locate-dominating-file
   directory
   (lambda (dir)
     (directory-files dir nil regexp t))))

(defun flycheck-haskell--find-stack-default-directory ()
  "Find a directory to run haskell-stack-ghc.

Return a parent directory with a stack*.y[a]ml file, or the
directory returned by \"stack path --project-root\"."
  (or
   (when (buffer-file-name)
     (flycheck--locate-dominating-file-matching
      (file-name-directory (buffer-file-name))
      (rx "stack" (* nonl) "." (or "yml" "yaml") eos)))
   (when-let* ((stack (funcall flycheck-executable-find "stack"))
               (output (ignore-errors
                         (flycheck--process-file-lines
                          (file-local-name stack)
                          "--no-install-ghc"
                          "path" "--project-root")))
               (root (car output))
               ;; `stack' reports a host-local path; name it on the host of
               ;; `default-directory' so it is usable as the remote working
               ;; directory (and checked on the right host below).
               (stack-dir (flycheck--expand-file-name root default-directory)))
     (and (file-directory-p stack-dir) stack-dir))))

(defun flycheck-haskell--ghc-find-default-directory (_checker)
  "Find a parent directory containing a cabal or package.yaml file."
  (when (buffer-file-name)
    (flycheck--locate-dominating-file-matching
     (file-name-directory (buffer-file-name))
     "\\.cabal\\'\\|\\`package\\.yaml\\'")))

(flycheck-define-checker haskell-stack-ghc
  "A Haskell syntax and type checker using `stack ghc'.

See URL `https://github.com/commercialhaskell/stack'."
  :command ("stack"
            "--no-install-ghc"
            (option "--stack-yaml" flycheck-ghc-stack-project-file)
            (option-flag "--nix" flycheck-ghc-stack-use-nix)
            "ghc" "--" "-Wall" "-no-link"
            "-outputdir" (eval (file-local-name
                                (flycheck-haskell-ghc-cache-directory)))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            (option-list "-i" flycheck-ghc-search-path concat)
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-literate-mode "lhs")
                    (_ "hs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") (in "Ww") "arning:"
            (optional " " "[" (id (one-or-more not-newline)) "]")
            (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more (not (any ?\n ?|)))))
            line-end)
   (error line-start (file-name) ":" line ":" column ":" (optional " error:")
          (optional " " "[" (id (one-or-more not-newline)) "]")
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more (not (any ?\n ?|)))))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode haskell-literate-mode haskell-ts-mode)
  :next-checkers ((warning . haskell-hlint))
  :working-directory (lambda (_)
                       (flycheck-haskell--find-stack-default-directory))
  :enabled flycheck-haskell--find-stack-default-directory
  :verify (lambda (_)
            (let* ((stack (flycheck-haskell--find-stack-default-directory)))
              (list
               (flycheck-verification-result-new
                :label "stack config"
                :message (or stack "Not found")
                :face (if stack 'success '(bold error)))))))

(flycheck-define-checker haskell-ghc
  "A Haskell syntax and type checker using ghc.

See URL `https://www.haskell.org/ghc/'."
  :command ("ghc" "-Wall" "-no-link"
            "-outputdir" (eval (file-local-name
                                (flycheck-haskell-ghc-cache-directory)))
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
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-literate-mode "lhs")
                    (_ "hs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") (in "Ww") "arning:"
            (optional " " "[" (id (one-or-more not-newline)) "]")
            (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more (not (any ?\n ?|)))))
            line-end)
   (error line-start (file-name) ":" line ":" column ":" (optional " error:")
          (optional " " "[" (id (one-or-more not-newline)) "]")
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more (not (any ?\n ?|)))))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode haskell-literate-mode haskell-ts-mode)
  :next-checkers ((warning . haskell-hlint))
  :working-directory flycheck-haskell--ghc-find-default-directory)

(define-obsolete-variable-alias 'flycheck-hlintrc
  'flycheck-hlint-config "39")
(flycheck-def-config-file-var flycheck-hlint-config haskell-hlint ".hlint.yaml")

(flycheck-def-args-var flycheck-hlint-args haskell-hlint
  :package-version '(flycheck . "0.25"))

(flycheck-def-option-var flycheck-hlint-language-extensions
    nil haskell-hlint
  "Extensions list to enable for hlint.

The value of this variable is a list of strings, where each
string is a name of extension to enable in
hlint (e.g. \"QuasiQuotes\")."
  :type '(repeat :tag "Extensions" (string :tag "Extension"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-hlint-ignore-rules
    nil haskell-hlint
  "Ignore rules list for hlint checks.

The value of this variable is a list of strings, where each
string is an ignore rule (e.g. \"Use fmap\")."
  :type '(repeat :tag "Ignore rules" (string :tag "Ignore rule"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-hlint-hint-packages
    nil haskell-hlint
  "Hint packages to include for hlint checks.

The value of this variable is a list of strings, where each
string is a default hint package (e.g. (\"Generalise\"
\"Default\" \"Dollar\"))."
  :type '(repeat :tag "Hint packages" (string :tag "Hint package"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker haskell-hlint
  "A Haskell style checker using hlint.

See URL `https://github.com/ndmitchell/hlint'."
  :command ("hlint"
            "--no-exit-code"
            (option-list "-X" flycheck-hlint-language-extensions concat)
            (option-list "-i=" flycheck-hlint-ignore-rules concat)
            (option-list "-h" flycheck-hlint-hint-packages concat)
            (config-file "-h" flycheck-hlint-config)
            (eval flycheck-hlint-args)
            source-inplace)
  :error-patterns
  ((info line-start
         (file-name) ":"
         (or (seq line ":" column (optional "-" end-column))
             (seq "(" line "," column ")-(" end-line "," end-column ")"))
         ": Suggestion: "
         (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
         line-end)
   (warning line-start
            (file-name) ":"
            (or (seq line ":" column (optional "-" end-column))
                (seq "(" line "," column ")-(" end-line "," end-column ")"))
            ": Warning: "
            (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
            line-end)
   (error line-start
          (file-name) ":"
          (or (seq line ":" column (optional "-" end-column))
              (seq "(" line "," column ")-(" end-line "," end-column ")"))
          ": Error: "
          (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
          line-end))
  :modes (haskell-mode haskell-literate-mode haskell-ts-mode))

(define-obsolete-variable-alias 'flycheck-tidyrc
  'flycheck-tidy-config "39")
(flycheck-def-config-file-var flycheck-tidy-config html-tidy ".tidyrc")

(flycheck-define-checker html-tidy
  "An HTML syntax and style checker using Tidy.

See URL `https://github.com/htacg/tidy-html5'."
  :command ("tidy" (config-file "-config" flycheck-tidy-config)
            "-lang" "en"
            "-e" "-q")
  :standard-input t
  :error-patterns
  ((error line-start
          "line " line
          " column " column
          " - Error: " (message) line-end)
   (warning line-start
            "line " line
            " column " column
            " - Warning: " (message) line-end))
  :modes (html-mode mhtml-mode nxhtml-mode html-ts-mode))

(flycheck-def-args-var flycheck-eslint-args javascript-eslint
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-eslint-rules-directories nil javascript-eslint
  "A list of directories with custom rules for ESLint.

The value of this variable is a list of strings, where each
string is a directory with custom rules for ESLint.

This passes the `--rulesdir' option, which ESLint 9 removed along
with the legacy `.eslintrc' configuration; with flat config, load
custom rules through a plugin in `eslint.config.js' instead.

Refer to the ESLint manual at URL
`https://eslint.org/docs/user-guide/command-line-interface#--rulesdir'
for more information about the custom directories."
  :type '(repeat (directory :tag "Custom rules directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "29"))

(defun flycheck-eslint-config-exists-p ()
  "Whether there is a valid eslint config for the current buffer."
  ;; `flycheck-call-checker-process' returns nil when eslint cannot be found, so
  ;; test for a zero exit status rather than passing a possible nil to `zerop'.
  (eql 0 (flycheck-call-checker-process
          'javascript-eslint nil nil nil
          "--print-config" (flycheck-buffer-file-local-name "index.js"))))

(defun flycheck--eslint-handle-suspicious (_checker exit-status output)
  "Disable the checker when EXIT-STATUS means eslint cannot lint at all.

OUTPUT is what it printed, for the message that says why.

Eslint exits with status 2 on any fatal failure -- a missing or
broken configuration, a crashing plugin -- rather than lint
findings, so the checker cannot be used in this buffer.  This
matches the semantics of the blocking `--print-config' probe that
previous versions ran in `:enabled' (see URL
`https://github.com/flycheck/flycheck/issues/1129'), and doesn't
depend on the wording of any particular eslint version.  The
first line of OUTPUT is included in the disable notice.

Any other exit status without parsable errors is suspicious: it
suggests an output format Flycheck fails to parse."
  (if (eq exit-status 2)
      (cons 'disable (car (split-string output "\n" t)))
    'suspicious))

(defun flycheck--utf16-offset-to-position (offset)
  "Return the buffer position at UTF-16 code-unit OFFSET from `point-min'.

ESLint (like the LSP protocol) counts offsets in UTF-16 code
units, so a character outside the Basic Multilingual Plane counts
as two.  Call in the buffer being converted, widened."
  (save-excursion
    (goto-char (point-min))
    (let ((remaining offset))
      (while (and (> remaining 0) (not (eobp)))
        (setq remaining (- remaining (if (>= (char-after) #x10000) 2 1)))
        (forward-char 1))
      (point))))

(defun flycheck-parse-eslint--fix (fix buffer)
  "Build a `flycheck-fix' from an ESLint FIX object for BUFFER, or nil.

An ESLint fix has a `range' of two UTF-16 code-unit offsets into
the source and the `text' to put in their place."
  (when fix
    (let-alist fix
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (let ((beg (flycheck-line-column-at-pos
                      (flycheck--utf16-offset-to-position (elt .range 0))))
                (end (flycheck-line-column-at-pos
                      (flycheck--utf16-offset-to-position (elt .range 1)))))
            (flycheck--make-fix
             buffer nil
             (list (flycheck-fix-edit-new
                    :line (car beg) :column (cdr beg)
                    :end-line (car end) :end-column (cdr end)
                    :replacement .text)))))))))

(defun flycheck-parse-eslint (output checker buffer)
  "Parse ESLint errors/warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://eslint.org' for more information about ESLint."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .line
               .column
               (pcase .severity
                 (2 'error)
                 (1 'warning)
                 (_ 'warning))
               .message
               :id .ruleId
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer)
               :end-line .endLine
               :end-column .endColumn
               :fix (flycheck-parse-eslint--fix .fix buffer))))
          (let-alist (caar (flycheck-parse-json output))
            .messages)))

(defun flycheck-eslint--find-working-directory (_checker)
  "Look for a working directory to run ESLint CHECKER in.

This will be the directory that contains the `node_modules'
directory.  If no such directory is found in the directory
hierarchy, it looks for `.eslintignore' and then for a
configuration file to detect the project root.  Both the flat
config files ESLint uses since version 9 (`eslint.config.js' and
its `.mjs'/`.cjs'/`.ts' variants) and the legacy `.eslintrc'
files are recognized."
  (let* ((regex-config (concat "\\`\\(?:"
                               "\\.eslintrc\\(?:\\.\\(?:js\\|ya?ml\\|json\\)\\)?"
                               "\\|eslint\\.config\\.[cm]?[jt]s"
                               "\\)\\'")))
    (when buffer-file-name
      (or (locate-dominating-file buffer-file-name "node_modules")
          (locate-dominating-file buffer-file-name ".eslintignore")
          (locate-dominating-file
           (file-name-directory buffer-file-name)
           (lambda (directory)
             (> (length (directory-files directory nil regex-config t)) 0)))))))

(flycheck-define-checker javascript-eslint
  "A Javascript syntax and style checker using eslint.

See URL `https://eslint.org/'."
  :command ("eslint" "--format=json"
            (option-list "--rulesdir" flycheck-eslint-rules-directories)
            (eval flycheck-eslint-args)
            "--stdin" "--stdin-filename" source-original)
  :standard-input t
  :error-parser flycheck-parse-eslint
  ;; A missing eslint config is diagnosed from the check's own output
  ;; (see `flycheck--eslint-handle-suspicious') instead of a blocking
  ;; `--print-config' probe in `:enabled', which used to freeze Emacs on
  ;; the first check in every buffer
  :handle-suspicious flycheck--eslint-handle-suspicious
  :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                  typescript-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
  :working-directory flycheck-eslint--find-working-directory
  :verify
  (lambda (_)
    (let* ((default-directory
             (flycheck-compute-working-directory 'javascript-eslint))
           (have-config (flycheck-eslint-config-exists-p)))
      (list
       (flycheck-verification-result-new
        :label "config file"
        :message (if have-config "found" "missing or incorrect")
        :face (if have-config 'success '(bold error))))))
  :error-explainer
  (flycheck-error-explainer-from-url
   "https://eslint.org/docs/rules/%s"
   ;; skip non-builtin (plugin) rules, which eslint.org does not document
   (lambda (id) (unless (seq-contains-p id ?/) id))))

(flycheck-def-config-file-var flycheck-javascript-oxlint-config
                              javascript-oxlint nil
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-javascript-oxlint-deny nil javascript-oxlint
  "Rules or categories oxlint should report as errors.

The value of this variable is a list of strings, where each
string is the name of a rule or of a category such as
`correctness' or `pedantic', passed to oxlint via `--deny'."
  :type '(repeat (string :tag "Rule or category"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-javascript-oxlint-allow nil javascript-oxlint
  "Rules or categories oxlint should not report.

The value of this variable is a list of strings, where each
string is the name of a rule or of a category such as
`correctness' or `pedantic', passed to oxlint via `--allow'."
  :type '(repeat (string :tag "Rule or category"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-args-var flycheck-javascript-oxlint-args javascript-oxlint
  :package-version '(flycheck . "39"))

(flycheck-define-checker javascript-oxlint
  "A JavaScript and TypeScript linter using oxlint.

See URL `https://oxc.rs/'."
  :command ("oxlint"
            "--format" "checkstyle"
            (config-file "--config" flycheck-javascript-oxlint-config)
            (option-list "--deny" flycheck-javascript-oxlint-deny)
            (option-list "--allow" flycheck-javascript-oxlint-allow)
            (eval flycheck-javascript-oxlint-args)
            source-inplace)
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-dequalify-error-ids errors)))
  :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                  typescript-mode js-ts-mode typescript-ts-mode tsx-ts-mode))

(flycheck-def-args-var flycheck-javascript-standard-args javascript-standard
  :package-version '(flycheck . "39"))

(flycheck-define-checker javascript-standard
  "A Javascript code and style checker for the (Semi-)Standard Style.

This checker works with `standard' and `semistandard', defaulting
to the former.  To use it with the latter, set
`flycheck-javascript-standard-executable' to `semistandard'.

See URL `https://github.com/standard/standard' and URL
`https://github.com/Flet/semistandard'."
  :command ("standard" "--stdin"
            (eval flycheck-javascript-standard-args))
  :standard-input t
  :error-patterns
  ((error line-start "  <text>:" line ":" column ":" (message) line-end))
  :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                  js-ts-mode))

(flycheck-define-checker json-python-json
  "A JSON syntax checker using Python json.tool module.

See URL `https://docs.python.org/3.5/library/json.html#command-line-interface'."
  :command ("python3" "-m" "json.tool" source
            ;; Send the pretty-printed output to the null device
            null-device)
  :error-patterns
  ((error line-start
          (message) ": line " line " column " column
          ;; Ignore the rest of the line which shows the char position.
          (one-or-more not-newline)
          line-end))
  :modes (json-mode js-json-mode json-ts-mode)
  ;; The JSON parser chokes if the buffer is empty and has no JSON inside
  :predicate flycheck-buffer-nonempty-p)

(flycheck-def-args-var flycheck-json-jq-args json-jq
  :package-version '(flycheck . "39"))

(flycheck-define-checker json-jq
  "JSON checker using the jq tool.

This checker accepts multiple consecutive JSON values in a
single input, which is useful for jsonlines data.

See URL `https://stedolan.github.io/jq/'."
  :command ("jq" (eval flycheck-json-jq-args) "." source null-device)
  ;; Example error message:
  ;;   jq: parse error: Expected another key-value pair at line 3, column 1
  ;; jq 1.7 grew the leading program name; older versions have neither it
  ;; nor, for some errors, the "parse error" part.
  :error-patterns
  ((error line-start
          (optional "jq: ")
          (optional "parse error: ")
          (message) "at line " line ", column " column
          (zero-or-more not-newline) line-end))
  :modes (json-mode js-json-mode json-ts-mode))

(flycheck-def-option-var flycheck-jsonnet-include-paths nil jsonnet
  "A list of include paths for the jsonnet binary.

The value of this variable is a list of strings, where each
string is a directory to add to the include path via `-J'."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "35.0"))

(define-obsolete-variable-alias 'flycheck-jsonnet-command-args
  'flycheck-jsonnet-args "39")
(flycheck-def-args-var flycheck-jsonnet-args jsonnet
  :package-version '(flycheck . "35.0"))

(flycheck-define-checker jsonnet
  "A Jsonnet syntax checker using the jsonnet binary.

See URL `https://jsonnet.org'."
  :command
  ("jsonnet"
   (option-list "-J" flycheck-jsonnet-include-paths)
   (eval flycheck-jsonnet-args)
   source-inplace)
  :error-patterns
  ((error line-start "STATIC ERROR: " (file-name) ":"
          (or (seq line ":" column (zero-or-one (seq "-" end-column)))
              (seq "(" line ":" column ")" "-"
                   "(" end-line ":" end-column ")"))
          ": " (message) line-end)
   (error line-start "RUNTIME ERROR: " (message) "\n"
          (? "\t" (file-name) ":" ;; first line of the backtrace
             (or (seq line ":" column (zero-or-one (seq "-" end-column)))
                 (seq "(" line ":" column ")" "-"
                      "(" end-line ":" end-column ")")))))
  :error-filter
  (lambda (errs)
    ;; Some errors are missing line numbers. See URL
    ;; `https://github.com/google/jsonnet/issues/786'.
    (dolist (err errs)
      (unless (flycheck-error-line err)
        (setf (flycheck-error-line err) 1)))
    (flycheck-sanitize-errors errs))
  :modes jsonnet-mode)

(flycheck-define-checker less
  "A LESS syntax checker using lessc.

Requires lessc 1.4 or newer.

See URL `https://lesscss.org'."
  :command ("lessc" "--lint" "--no-color"
            "-")
  :standard-input t
  :error-patterns
  ((error line-start (one-or-more word) ":"
          (message)
          " in - on line " line
          ", column " column ":"
          line-end))
  :modes less-css-mode)

(flycheck-define-checker less-stylelint
  "A LESS syntax and style checker using stylelint.

See URL `https://stylelint.io/'."
  :command ("stylelint"
            "--formatter" "json"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelint-config))
  :standard-input t
  :verify (lambda (_) (flycheck--stylelint-verify 'less-stylelint))
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :error-explainer
  (flycheck-error-explainer-from-url "https://stylelint.io/user-guide/rules/%s")
  :handle-suspicious flycheck--stylelint-handle-suspicious
  :modes (less-css-mode))

(flycheck-define-checker llvm-llc
  "Flycheck LLVM IR checker using llc.

See URL `https://llvm.org/docs/CommandGuide/llc.html'."
  :command ("llc" "-o" null-device source)
  :error-patterns
  ((error line-start
          ;; llc prints the executable path
          (zero-or-one (minimal-match (one-or-more not-newline)) ": ")
          (file-name) ":" line ":" column ": error: " (message)
          line-end))
  :error-filter
  (lambda (errors)
    ;; sanitize errors occurring in inline assembly
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "<inline asm>" errors)))
  :modes llvm-mode)

(define-obsolete-variable-alias 'flycheck-luacheckrc
  'flycheck-luacheck-config "39")
(flycheck-def-config-file-var flycheck-luacheck-config lua-luacheck ".luacheckrc")

(flycheck-def-option-var flycheck-luacheck-standards nil lua-luacheck
  "The standards to use in luacheck.

The value of this variable is either a list of strings denoting
the standards to use, or nil to pass nothing to luacheck.  When
non-nil, pass the standards via one or more `--std' options."
  :type '(choice (const :tag "Default" nil)
                 (repeat :tag "Custom standards"
                         (string :tag "Standard name")))
  :safe #'flycheck-string-list-p)
(make-variable-buffer-local 'flycheck-luacheck-standards)

(flycheck-def-option-var flycheck-luacheck-globals nil lua-luacheck
  "A list of globals to allow in luacheck, via `--globals'.

Each element is the name of an additional global variable to treat as
defined, such as \"love\" or \"vim\"."
  :type '(repeat (string :tag "Global"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-args-var flycheck-luacheck-args lua-luacheck
  :package-version '(flycheck . "39"))

(flycheck-define-checker lua-luacheck
  "A Lua syntax checker using luacheck.

See URL `https://github.com/mpeterv/luacheck'."
  :command ("luacheck"
            "--formatter" "plain"
            "--codes"                   ; Show warning codes
            "--no-color"
            (option-list "--std" flycheck-luacheck-standards)
            (option-list "--globals" flycheck-luacheck-globals)
            (config-file "--config" flycheck-luacheck-config)
            (eval flycheck-luacheck-args)
            "--filename" source-original
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((warning line-start
            (optional (file-name))
            ":" line ":" column
            ": (" (id "W" (one-or-more digit)) ") "
            (message) line-end)
   (error line-start
          (optional (file-name))
          ":" line ":" column ":"
          ;; `luacheck' before 0.11.0 did not output codes for errors, hence
          ;; the ID is optional here
          (optional " (" (id "E" (one-or-more digit)) ") ")
          (message) line-end))
  :modes (lua-mode lua-ts-mode))

(flycheck-define-checker lua
  "A Lua syntax checker using the Lua compiler.

See URL `https://www.lua.org/'."
  :command ("luac" "-p" "-")
  :standard-input t
  :error-patterns
  ((error line-start
          ;; Skip the name of the luac executable.
          (minimal-match (zero-or-more not-newline))
          ": stdin:" line ": " (message) line-end))
  :modes (lua-mode lua-ts-mode))

(flycheck-define-checker opam
  "An Opam syntax and style checker using opam lint.

See URL `https://opam.ocaml.org/doc/man/opam-lint.html'."
  :command ("opam" "lint" "-")
  :standard-input t
  :error-patterns
  ((error line-start                    ; syntax error
          (one-or-more space) "error  " (id ?2)
          ": File format error"
          (or (and " at line " line ", column " column ": " (message))
              (and ": " (message)))
          line-end)
   (error line-start
          (one-or-more space) "error  " (id ?3)
          (minimal-match (zero-or-more not-newline))
          "at line " line ", column " column ": " (message)
          line-end)
   (error line-start
          (one-or-more space) "error " (id (one-or-more num))
          ": " (message (one-or-more not-newline))
          line-end)
   (warning line-start
            (one-or-more space) "warning " (id (one-or-more num))
            ": " (message)
            line-end))
  :error-filter
  (lambda (errors)
    (flycheck-increment-error-columns
     (flycheck-fill-empty-line-numbers errors)))
  :modes (tuareg-opam-mode neocaml-opam-mode))

(defconst flycheck-ocaml-error-patterns
  ;; A location line is followed by the offending source lines, which the
  ;; compiler echoes back with a line number or a caret underneath, and
  ;; then by the message.  Both always start with a digit or a space,
  ;; which is what keeps the skip from running past the message and into
  ;; the next diagnostic.
  '((error line-start
           "File \"" (file-name) "\", line" (? "s") " " line (? "-" end-line)
           ", characters " column "-" end-column ":"
           (zero-or-more "\n" (any " " digit) (zero-or-more not-newline))
           "\nError: "
           (message (one-or-more not-newline)
                    (zero-or-more "\n" (one-or-more " ")
                                  (one-or-more not-newline)))
           line-end)
    ;; Dune's dev profile, and `ocamlc -warn-error', report warnings as
    ;; errors but still say which warning it was.  They are warnings.
    (warning line-start
             "File \"" (file-name) "\", line" (? "s") " " line (? "-" end-line)
             ", characters " column "-" end-column ":"
             (zero-or-more "\n" (any " " digit) (zero-or-more not-newline))
             "\n" (or "Warning " "Error (warning ")
             (id (one-or-more digit))
             (? " [" (one-or-more (any "a-z0-9-")) "]")
             (? ")") ": "
             (message (one-or-more not-newline)
                      (zero-or-more "\n" (one-or-more " ")
                                    (one-or-more not-newline)))
             line-end))
  "Error patterns shared by the OCaml checkers.")

(defun flycheck-ocaml--dune-root ()
  "Return the root directory of the Dune project of the current buffer.

Return nil if the buffer's file is not inside a Dune project."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "dune-project")))

(defun flycheck-ocaml--filter-errors (errors)
  "Sanitize ERRORS from an OCaml compiler, whose columns are 0-based."
  (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))

(flycheck-def-option-var flycheck-ocaml-packages nil ocaml
  "A list of findlib packages for the OCaml checker.

The value of this variable is a list of strings, where each
string is a findlib package name."
  :type '(repeat (string :tag "Package name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-args-var flycheck-ocaml-args ocaml
  :package-version '(flycheck . "39"))

(flycheck-def-executable-var ocaml "ocamlfind")
(flycheck-define-command-checker 'ocaml
  "An OCaml syntax and type checker using the OCaml compiler.

This checker compiles the file on its own, so it only knows about
the modules it is told about via `flycheck-ocaml-packages'.  That
is right for a standalone file, but not for a file that is part of
a larger project, where every reference to a sibling module would
be reported as an unbound module.  It therefore steps aside inside
a Dune project, where `ocaml-dune' takes over.

See URL `https://ocaml.org/'."
  :command '("ocamlfind" "ocamlc"
             (option-list "-package" flycheck-ocaml-packages)
             (eval flycheck-ocaml-args)
             "-c" source)
  :error-patterns flycheck-ocaml-error-patterns
  :error-filter #'flycheck-ocaml--filter-errors
  :predicate (lambda () (not (flycheck-ocaml--dune-root)))
  :modes '(tuareg-mode caml-mode neocaml-mode))

(flycheck-def-option-var flycheck-ocaml-dune-profile nil ocaml-dune
  "The build profile for the Dune checker.

When non-nil, pass this profile to Dune via `--profile'.  When
nil, let Dune pick, which means the `dev' profile unless the
project says otherwise."
  :type '(choice (const :tag "Default profile" nil)
                 (const :tag "Development" "dev")
                 (const :tag "Release" "release")
                 (string :tag "Profile name"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "39"))

(flycheck-def-args-var flycheck-ocaml-dune-args ocaml-dune
  :package-version '(flycheck . "39"))

(flycheck-def-executable-var ocaml-dune "dune")
(flycheck-define-command-checker 'ocaml-dune
  "An OCaml syntax and type checker using Dune.

Runs `dune build @check', which type-checks the whole project
without linking it, so unlike `ocaml' it resolves references to
other modules and to the project's dependencies.

Two things follow from Dune doing the build.  It reads the files
from disk, so this checker only runs once the buffer is saved.
And it reports the whole project, so errors may well belong to a
file other than the one you are visiting; Flycheck lists those in
the error list under their own file.

Dune only reports what it rebuilds.  A target that failed is
retried on every check, so errors keep being reported, and under
the default `dev' profile that covers warnings too, since Dune
promotes them to errors there.  A project that turns that off with
`-warn-error -a' gets its warnings reported once, on the check
that compiles the file, and not again until it changes.

Dune takes a lock on the build directory, so a check waits for any
`dune build' already running in a terminal, and vice versa, and
the first check of a cold project pays for the whole build.

See URL `https://dune.build/'."
  :command '("dune" "build"
             (option "--profile" flycheck-ocaml-dune-profile)
             (eval flycheck-ocaml-dune-args)
             "@check")
  :error-patterns flycheck-ocaml-error-patterns
  :error-filter #'flycheck-ocaml--filter-errors
  :working-directory (lambda (_checker) (flycheck-ocaml--dune-root))
  :predicate (lambda ()
               (and (flycheck-ocaml--dune-root) (flycheck-buffer-saved-p)))
  :modes '(tuareg-mode caml-mode neocaml-mode))

(flycheck-def-option-var flycheck-perl-include-path nil perl
  "A list of include directories for Perl.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Perl.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-perl-module-list nil perl
  "A list of modules to use for Perl.

The value of this variable is a list of strings, where each
string is a module to `use' in Perl."
  :type '(repeat (string :tag "Module"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-define-checker perl
  "A Perl syntax checker using the Perl interpreter.

See URL `https://www.perl.org'."
  :command ("perl" "-w" "-c"
            (option-list "-I" flycheck-perl-include-path)
            (option-list "-M" flycheck-perl-module-list concat))
  :standard-input t
  :error-patterns
  ((error line-start (minimal-match (message))
          " at - line " line
          (or "." (and ", " (zero-or-more not-newline))) line-end))
  :modes (perl-mode cperl-mode)
  :next-checkers (perl-perlcritic))

(flycheck-def-option-var flycheck-perlcritic-severity nil perl-perlcritic
  "The message severity for Perl Critic.

The value of this variable is a severity level as integer, for
the `--severity' option to Perl Critic."
  :type '(integer :tag "Severity level")
  :safe #'integerp
  :package-version '(flycheck . "0.18"))

(flycheck-def-option-var flycheck-perlcritic-theme nil perl-perlcritic
  "The theme expression for Perl Critic.

The value of this variable is passed as the `--theme' option to
`Perl::Critic'.  See the documentation of `Perl::Critic' for
details."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Theme expression"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))

(define-obsolete-variable-alias 'flycheck-perlcriticrc
  'flycheck-perlcritic-config "39")
(flycheck-def-config-file-var flycheck-perlcritic-config perl-perlcritic
                              ".perlcriticrc"
  :package-version '(flycheck . "26"))

(flycheck-define-checker perl-perlcritic
  "A Perl syntax checker using Perl::Critic.

See URL `https://metacpan.org/pod/Perl::Critic'."
  :command ("perlcritic" "--no-color" "--verbose" "%f/%l/%c/%s/%p/%m (%e)\n"
            (config-file "--profile" flycheck-perlcritic-config)
            (option "--severity" flycheck-perlcritic-severity nil
                    flycheck-option-int)
            (option "--theme" flycheck-perlcritic-theme))
  :standard-input t
  :error-patterns
  ((info line-start
         "STDIN/" line "/" column "/" (any "1") "/"
         (id (one-or-more (not (any "/")))) "/" (message)
         line-end)
   (warning line-start
            "STDIN/" line "/" column "/" (any "234") "/"
            (id (one-or-more (not (any "/")))) "/" (message)
            line-end)
   (error line-start
          "STDIN/" line "/" column "/" (any "5") "/"
          (id (one-or-more (not (any "/")))) "/" (message)
          line-end))
  :modes (cperl-mode perl-mode)
  :next-checkers (perl-perlimports)

  :error-explainer
  (flycheck-error-explainer-from-url
   "https://metacpan.org/pod/Perl::Critic::Policy::%s"))

(defun flycheck-perl-perlimports-parse-diff (diff)
  "Return the lines added by DIFF, as a string."
  (let ((start 0)
        (replacements ()))
    (while (string-match (rx line-start
                             "+"
                             (group (zero-or-more not-newline))
                             line-end)
                         diff
                         start)
      (setq start (match-end 0))
      (setq replacements (nconc replacements
                                (list (match-string 1 diff)))))
    (string-join replacements "\n")))

(defun flycheck-perl-perlimports-parse-errors (output checker buffer)
  "Parse perlimports json output errors from OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://metacpan.org/dist/App-perlimports/view/script/perlimports'
for more information about perlimports."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .location.start.line
               .location.start.column
               'info
               (concat .module " " .reason ":"
                       (with-temp-buffer
                         (insert (substring .diff (string-match-p "\n" .diff)))
                         (diff-mode)
                         (font-lock-ensure)
                         (buffer-string)))
               :end-line .location.end.line
               :end-column (+ 1 .location.end.column)
               :checker checker
               :fix (flycheck-fix-new
                     :description (concat .module " " .reason)
                     :edits (list
                             (flycheck-fix-edit-new
                              :line .location.start.line
                              :column .location.start.column
                              :end-line .location.end.line
                              :end-column (+ 1 .location.end.column)
                              :replacement (flycheck-perl-perlimports-parse-diff
                                            .diff)))
                     :tick (buffer-chars-modified-tick))
               :buffer buffer)))
          (flycheck-parse-json output)))

(flycheck-define-checker perl-perlimports
  "A checker for cleaning up Perl import statements.

See URL `https://metacpan.org/dist/App-perlimports/view/script/perlimports'."
  :command ("perlimports"
            "--filename" source
            "--json"
            "--lint"
            "--no-preserve-duplicates"
            "--no-preserve-unused"
            "--no-tidy-whitespace"
            "--read-stdin")
  :standard-input t
  :error-parser flycheck-perl-perlimports-parse-errors
  :modes (cperl-mode perl-mode))

(flycheck-define-checker php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `https://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php-ts-mode php+-mode)
  :next-checkers ((warning . php-phpmd)
                  (warning . php-phpcs)))

(flycheck-def-option-var flycheck-phpmd-rulesets
    '("cleancode" "codesize" "controversial" "design" "naming" "unusedcode")
    php-phpmd
  "The rule sets for PHP Mess Detector.

Set default rule sets and custom rule set files.

See section \"Using multiple rule sets\" in the PHP Mess Detector
manual at URL `https://phpmd.org/documentation/index.html'."
  :type '(repeat :tag "rule sets"
                 (string :tag "A filename or rule set"))
  :safe #'flycheck-string-list-p)

(flycheck-def-args-var flycheck-phpmd-args php-phpmd
  :package-version '(flycheck . "39"))

(flycheck-define-checker php-phpmd
  "A PHP style checker using PHP Mess Detector.

See URL `https://phpmd.org/'."
  :command ("phpmd" source "xml"
            (eval (flycheck-option-comma-separated-list
                   flycheck-phpmd-rulesets))
            (eval flycheck-phpmd-args))
  :error-parser flycheck-parse-phpmd
  :modes (php-mode php-ts-mode php+-mode)
  :next-checkers (php-phpcs))

(flycheck-def-option-var flycheck-phpcs-standard nil php-phpcs
  "The coding standard for PHP CodeSniffer.

When nil, use the default standard from the global PHP
CodeSniffer configuration.  When set to a string, pass the string
to PHP CodeSniffer which will interpret it as the name of a standard,
or as path to a standard specification."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Standard name or file"))
  :safe #'string-or-null-p)

(flycheck-def-args-var flycheck-phpcs-args php-phpcs
  :package-version '(flycheck . "39"))

(flycheck-define-checker php-phpcs
  "A PHP style checker using PHP Code Sniffer.

Needs PHP Code Sniffer 2.6 or newer.

See URL `https://pear.php.net/package/PHP_CodeSniffer/'."
  :command ("phpcs" "--report=checkstyle"
            ;; Use -q flag to force quiet mode
            ;; Quiet mode prevents errors from extra output when phpcs has
            ;; been configured with show_progress enabled
            "-q"
            (option "--standard=" flycheck-phpcs-standard concat)
            ;; Some files are not detected correctly
            ;; so it is necessary to pass the extension.
            (eval
             (when-let* ((fname buffer-file-name)
                         (ext (file-name-extension fname)))
               (concat "--extensions=" ext)))

            ;; Pass original file name to phpcs.  We need to concat explicitly
            ;; here, because phpcs really insists to get option and argument as
            ;; a single command line argument :|
            (eval (when buffer-file-name
                    (concat "--stdin-path=" (flycheck-buffer-file-local-name))))
            (eval flycheck-phpcs-args)
            ;; Read from standard input
            "-")
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "STDIN" errors)))
  :modes (php-mode php-ts-mode php+-mode)
  ;; phpcs seems to choke on empty standard input, hence skip phpcs if the
  ;; buffer is empty, see https://github.com/flycheck/flycheck/issues/907
  :predicate flycheck-buffer-nonempty-p)

(flycheck-def-option-var flycheck-phpcs-changed-git-base "main"
                         php-phpcs-changed
  "The git base branch for PHPCS-Changed.

The value of this variable is a string specifying the git branch
to compare against (e.g. \"main\", \"master\", \"trunk\")."
  :type '(string :tag "Branch name")
  :safe #'stringp)

(flycheck-define-checker php-phpcs-changed
  "A PHP style checker using PHPCS-Changed.

Needs PHP Code Sniffer 2.6 or newer.
See URL `https://github.com/sirbrillig/phpcs-changed'."
  :command ("phpcs-changed"
            "--git"
            "--git-base" (eval flycheck-phpcs-changed-git-base)
            "--git-unstaged"
            (option "--standard=" flycheck-phpcs-standard concat)
            (eval (flycheck-buffer-file-local-name)))
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "STDIN" errors)))
  :modes (php-mode php-ts-mode php+-mode)
  ;; phpcs seems to choke on empty standard input, hence skip phpcs if the
  ;; buffer is empty, see https://github.com/flycheck/flycheck/issues/907
  :predicate flycheck-buffer-nonempty-p)

(flycheck-define-checker processing
  "Processing command line tool.

See https://github.com/processing/processing/wiki/Command-Line"
  :command ("processing-java" "--force"
            ;; Don't change the order of these arguments, processing is pretty
            ;; picky
            (eval (concat "--sketch=" (file-local-name
                                       (file-name-directory (buffer-file-name)))))
            (eval (concat "--output=" (file-local-name
                                       (flycheck-temp-dir-system))))
            "--build")
  :error-patterns
  ((error line-start (file-name) ":" line ":" column
          (zero-or-more (or digit ":")) (message) line-end))
  :modes processing-mode
  ;; This syntax checker needs a file name
  :predicate (lambda () (buffer-file-name)))

(defun flycheck-proselint-parse-errors (output checker buffer)
  "Parse proselint json output errors from OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://proselint.com/' for more information about proselint."
  (let ((response (flycheck-parse-json output)))
    (if (eq (caaar response) 'data)
        ;; Proselint versions <= 0.14.0:
        (mapcar (lambda (err)
                  (let-alist err
                    (flycheck-error-new-at-pos
                     .start
                     (pcase .severity
                       (`"suggestion" 'info)
                       (`"warning"    'warning)
                       (`"error"      'error)
                       ;; Default to error
                       (_             'error))
                     .message
                     :id .check
                     :buffer buffer
                     :checker checker
                     ;; See https://github.com/amperser/proselint/issues/1048
                     :end-pos .end)))
                (let-alist (car response)
                  .data.errors))
      ;; Proselint versions >= 0.16.0
      (mapcar (lambda (err)
                (let-alist err
                  (flycheck-error-new-at-pos
                   (nth 0 .span)
                   'warning
                   .message
                   :id .check_path
                   :buffer buffer
                   :checker checker
                   :end-pos (nth 1 .span))))
              (let-alist (car response)
                .result.<stdin>.diagnostics)))))

;; A hash table (not the scalar of earlier versions -- hence the new name,
;; so an in-session reload does not leave a stale non-table value that
;; `gethash' would choke on).
(defvar flycheck--proselint-old-args-by-host (make-hash-table :test 'equal)
  "Cache for proselint version detection, keyed by host.
The key is the remote identifier of `default-directory' (see
`file-remote-p'), or nil for the local host, since the proselint
on each host may have a different version.  Each value is t for
old (<= 0.14.0) proselint and nil for new (>= 0.16.0); a host
absent from the table has not been probed yet.")

(defvar flycheck-proselint-executable)

(defun flycheck--proselint-args ()
  "Return command arguments for proselint, detecting the version once per host."
  (let ((host (file-remote-p default-directory)))
    (when (eq (gethash host flycheck--proselint-old-args-by-host 'unknown) 'unknown)
      (puthash host
               ;; Probe on the host the check will run on (remote over TRAMP).
               (zerop (process-file
                       (or flycheck-proselint-executable "proselint")
                       nil nil nil "--version"))
               flycheck--proselint-old-args-by-host))
    (if (gethash host flycheck--proselint-old-args-by-host)
        ;; Proselint versions <= 0.14.0:
        (list "--json" "-")
      ;; Proselint versions >= 0.16.0
      (list "check" "--output-format=json"))))

(flycheck-define-checker proselint
  "Flycheck checker using Proselint.

See URL `https://proselint.com/'."
  :command ("proselint"
            (eval (flycheck--proselint-args)))
  :standard-input t
  :error-parser flycheck-proselint-parse-errors
  :modes (text-mode markdown-mode gfm-mode message-mode org-mode rst-mode))

(flycheck-def-option-var flycheck-protoc-import-path nil protobuf-protoc
  "A list of directories to resolve import directives.

The value of this variable is a list of strings, where each
string is a directory to add to the import path.  Relative paths
are relative to the file being checked."
  :type '(repeat (directory :tag "Import directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-protoc-import-path)

(flycheck-def-args-var flycheck-protoc-args protobuf-protoc
  :package-version '(flycheck . "39"))

(flycheck-define-checker protobuf-protoc
  "A protobuf syntax checker using the protoc compiler.

See URL `https://developers.google.com/protocol-buffers/'."
  :command ("protoc" "--error_format" "gcc"
            (eval (concat "--java_out=" (file-local-name
                                         (flycheck-temp-dir-system))))
            ;; Add the current directory to resolve imports
            (eval (concat "--proto_path="
                          (file-local-name
                           (file-name-directory (buffer-file-name)))))
            ;; Add other import paths; this needs to be after the current
            ;; directory to produce the right output.  See URL
            ;; `https://github.com/flycheck/flycheck/pull/1655'
            (option-list "--proto_path=" flycheck-protoc-import-path concat)
            (eval flycheck-protoc-args)
            source-inplace)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (message) line-end)
   (error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":" line-end))
  :modes protobuf-mode
  :predicate (lambda () (buffer-file-name)))

(flycheck-define-checker pug
  "A Pug syntax checker using the pug compiler.

See URL `https://pugjs.org/'."
  :command ("pug" "-p"
            (eval (file-local-name (expand-file-name (buffer-file-name)))))
  :standard-input t
  :error-patterns
  ;; errors with includes/extends (e.g. missing files)
  ((error "Error: " (message) (zero-or-more not-newline) "\n"
          (zero-or-more not-newline) "at "
          (zero-or-more not-newline) " line " line)
   ;; error when placing anything other than a mixin or
   ;; block at the top-level of an extended template
   ;; also unknown filters
   (error line-start "Error: " (file-name) ":"
          line ":" column "\n\n" (message) line-end)
   ;; syntax/runtime errors (e.g. type errors, bad indentation, etc.)
   (error line-start
          (optional "Type") "Error: "  (file-name) ":"
          line (optional ":" column)
          (zero-or-more not-newline) "\n"
          (one-or-more (or (zero-or-more not-newline) "|"
                           (zero-or-more not-newline) "\n")
                       (zero-or-more "-")  (zero-or-more not-newline) "|"
                       (zero-or-more not-newline) "\n")
          (zero-or-more not-newline) "\n"
          (one-or-more
           (zero-or-more not-newline) "|"
           (zero-or-more not-newline) "\n")
          (zero-or-more not-newline) "\n"
          (message)
          line-end))
  :modes pug-mode)

(flycheck-define-checker puppet-parser
  "A Puppet DSL syntax checker using puppet's own parser.

See URL `https://puppet.com/'."
  :command ("puppet" "parser" "validate" "--color=false")
  :standard-input t
  :error-patterns
  (
   ;; Patterns for Puppet 4
   (error line-start "Error: Could not parse for environment "
          (one-or-more (in "a-z" "0-9" "_")) ":"
          (message) "(line: " line ", column: " column ")" line-end)
   ;; Errors from Puppet < 4
   (error line-start "Error: Could not parse for environment "
          (one-or-more (in "a-z" "0-9" "_")) ":"
          (message (minimal-match (one-or-more anything)))
          " at line " line line-end)
   (error line-start
          ;; Skip over the path of the Puppet executable
          (minimal-match (zero-or-more not-newline))
          ": Could not parse for environment " (one-or-more word)
          ": " (message (minimal-match (zero-or-more anything)))
          " at " (file-name "/" (zero-or-more not-newline)) ":" line line-end)
   ;; Errors without line/column (e.g., "end of file" or "end of input")
   (error line-start "Error: Could not parse for environment "
          (one-or-more (in "a-z" "0-9" "_")) ":"
          (message) line-end))
  :modes (puppet-mode puppet-ts-mode)
  :next-checkers ((warning . puppet-lint)))

(define-obsolete-variable-alias 'flycheck-puppet-lint-rc
  'flycheck-puppet-lint-config "39")
(flycheck-def-config-file-var flycheck-puppet-lint-config puppet-lint
                              ".puppet-lint.rc"
  :package-version '(flycheck . "26"))

(flycheck-def-option-var flycheck-puppet-lint-disabled-checks nil puppet-lint
  "Disabled checkers for `puppet-lint'.

The value of this variable is a list of strings, where each
string is the name of a check to disable (e.g. \"80chars\" or
\"double_quoted_strings\").

See URL `https://puppet-lint.com/checks/' for a list of all checks
and their names."
  :type '(repeat (string :tag "Check Name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "26"))

(defun flycheck-puppet-lint-disabled-arg-name (check)
  "Create an argument to disable a puppetlint CHECK."
  (concat "--no-" check "-check"))

(flycheck-def-args-var flycheck-puppet-lint-args puppet-lint
  :package-version '(flycheck . "39"))

(flycheck-define-checker puppet-lint
  "A Puppet DSL style checker using puppet-lint.

See URL `https://puppet-lint.com/'."
  ;; We must check the original file, because Puppetlint is quite picky on the
  ;; names of files and their place in the directory structure, to comply with
  ;; Puppet's autoload directory layout.  For instance, a class foo::bar is
  ;; required to be in a file foo/bar.pp.  Any other place, such as a Flycheck
  ;; temporary file will cause an error.
  :command ("puppet-lint"
            (config-file "--config" flycheck-puppet-lint-config)
            "--log-format"
            "%{path}:%{line}:%{kind}: %{message} (%{check})"
            (option-list "" flycheck-puppet-lint-disabled-checks concat
                         flycheck-puppet-lint-disabled-arg-name)
            (eval flycheck-puppet-lint-args)
            source-original)
  :error-patterns
  ((warning line-start (file-name) ":" line ":warning: " (message) line-end)
   (error line-start (file-name) ":" line ":error: " (message) line-end))
  :modes (puppet-mode puppet-ts-mode)
  ;; Since we check the original file, we can only use this syntax checker if
  ;; the buffer is actually linked to a file, and if it is not modified.
  :predicate flycheck-buffer-saved-p)

(defun flycheck-python-run-snippet (checker snippet)
  "Run a python SNIPPET and return the output.

CHECKER's executable is assumed to be a Python REPL."
  (when-let* ((output (flycheck-call-checker-process-for-output
                      checker nil nil "-c" snippet)))
    (string-trim output)))

(defun flycheck-python-get-path (checker)
  "Compute the current Python path (CHECKER is a Python REPL)."
  (flycheck-python-run-snippet checker "import sys; print(sys.path[1:])"))

(defun flycheck-python-find-module (checker module)
  "Check if a Python MODULE is available (CHECKER is a Python REPL)."
  (flycheck-python-run-snippet
   checker (concat "import sys; sys.path.pop(0);"
                   (format "import %s; print(%s.__file__)" module module))))

(defun flycheck-python-needs-module-p (checker)
  "Determine whether CHECKER needs to be invoked through Python.

Previous versions of Flycheck called pylint and flake8 directly,
while new versions call them through `python -c'.  This check
ensures that we don't break existing code; it also allows people
who use virtualenvs to run globally-installed checkers."
  (not (string-match-p (rx (or "pylint" "pylint3" "flake8")
                           (or "-script.pyw" ".exe" ".bat" "")
                           eos)
                       (flycheck-checker-executable checker))))

(defun flycheck-python-verify-module (checker module)
  "Verify that a Python MODULE is available.

Return nil if CHECKER's executable is not a Python REPL.  This
function is suitable for a checker's :verify."
  (when (flycheck-python-needs-module-p checker)
    (let ((mod-path (flycheck-python-find-module checker module)))
      (list (flycheck-verification-result-new
             :label (format "`%s' module" module)
             :message (if mod-path (format "Found at %S" mod-path)
                        (format "Missing; sys.path is %s"
                                (flycheck-python-get-path checker)))
             :face (if mod-path 'success '(bold error)))))))

(defun flycheck-python-module-args (checker module-name)
  "Compute arguments to pass to CHECKER's executable to run MODULE-NAME.

Return nil if CHECKER's executable is not a Python REPL.
Otherwise, return a list starting with -c (-m is not enough
because it adds the current directory to Python's path)."
  (when (flycheck-python-needs-module-p checker)
    `("-c" ,(concat "import sys;sys.path.pop(0);import runpy;"
                    (format "runpy.run_module(%S, run_name='__main__')" module-name )))))

(defcustom flycheck-python-project-files
  '("pyproject.toml" "setup.cfg" "mypy.ini" "pyrightconfig.json")
  "Files used to find where to run Python checkers from.
Currently used for pylint, flake8, and pyright.

The presence of one of these files indicates the root of the
current project; `.pylintrc' is not part of the list because it
is commonly found in ~/."
  :group 'flycheck
  :type '(repeat (string :tag "File name"))
  :package-version '(flycheck . "33")
  :safe #'flycheck-string-list-p)

(defun flycheck-python-find-project-root (_checker)
  "Find the root directory of a Python project.

The root directory is assumed to be the nearest parent directory
that contains one of `flycheck-python-project-files'.  If no such
file is found, we use the same heuristic as epylint: the nearest
parent directory that doesn't have a __init__.py file."
  (let ((start (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 default-directory)))
    (or (flycheck--locate-dominating-file-matching
         start (regexp-opt flycheck-python-project-files))
        (locate-dominating-file
         start (lambda (dir)
                 (not (file-exists-p (expand-file-name "__init__.py" dir))))))))

(define-obsolete-variable-alias 'flycheck-flake8rc
  'flycheck-flake8-config "39")
(flycheck-def-config-file-var flycheck-flake8-config python-flake8
                              '(".flake8" "setup.cfg" "tox.ini"))

(flycheck-def-option-var flycheck-flake8-error-level-alist
    '(("^E9.*$"  . error)               ; Syntax errors from pep8
      ("^F82.*$" . error)               ; undefined variables from pyflakes
      ("^F83.*$" . error)               ; Duplicate arguments from flake8
      ("^D.*$"   . info)                ; Docstring issues from flake8-pep257
      ("^N.*$"   . info)                ; Naming issues from pep8-naming
      )
    python-flake8
  "An alist mapping flake8 error IDs to Flycheck error levels.

Each item in this list is a cons cell `(PATTERN . LEVEL)' where
PATTERN is a regular expression matched against the error ID, and
LEVEL is a Flycheck error level symbol.

Each PATTERN is matched in the order of appearance in this list
against the error ID.  If it matches the ID, the level of the
corresponding error is set to LEVEL.  An error that is not
matched by any PATTERN defaults to warning level.

The default value of this option matches errors from flake8
itself and from the following flake8 plugins:

- pep8-naming
- flake8-pep257

You may add your own mappings to this option in order to support
further flake8 plugins."
  :type '(repeat (cons (regexp :tag "Error ID pattern")
                       (symbol :tag "Error level")))
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-flake8-maximum-complexity nil python-flake8
  "The maximum McCabe complexity of methods.

If nil, do not check the complexity of methods.  If set to an
integer, report any complexity greater than the value of this
variable as warning.

If set to an integer, this variable overrules any similar setting
in the configuration file denoted by `flycheck-flake8-config'."
  :type '(choice (const :tag "Do not check McCabe complexity" nil)
                 (integer :tag "Maximum complexity"))
  :safe #'integerp)

(flycheck-def-option-var flycheck-flake8-maximum-line-length nil python-flake8
  "The maximum length of lines.

If set to an integer, the value of this variable denotes the
maximum length of lines, overruling any similar setting in the
configuration file denoted by `flycheck-flake8-config'.  An error will
be reported for any line longer than the value of this variable.

If set to nil, use the maximum line length from the configuration
file denoted by `flycheck-flake8-config', or the PEP 8 recommendation
of 79 characters if there is no configuration with this setting."
  :type '(choice (const :tag "Default value" nil)
                 (integer :tag "Maximum line length in characters"))
  :safe #'integerp)

(flycheck-def-args-var flycheck-flake8-args python-flake8
  :package-version '(flycheck . "39"))

(defun flycheck-flake8-fix-error-level (err)
  "Fix the error level of ERR.

Update the error level of ERR according to
`flycheck-flake8-error-level-alist'."
  (when-let* ((entry (seq-find
                      (lambda (e)
                        (string-match-p (car e) (flycheck-error-id err)))
                      flycheck-flake8-error-level-alist)))
    (setf (flycheck-error-level err) (cdr entry)))
  err)

(flycheck-define-checker python-flake8
  "A Python syntax and style checker using Flake8.

Requires Flake8 3.0 or newer. See URL
`https://flake8.readthedocs.io/'."
  ;; Not calling flake8 directly makes it easier to switch between different
  ;; Python versions; see https://github.com/flycheck/flycheck/issues/1055.
  :command ("python3"
            (eval (flycheck-python-module-args 'python-flake8 "flake8"))
            "--format=default"
            (config-file "--append-config" flycheck-flake8-config)
            (option "--max-complexity" flycheck-flake8-maximum-complexity nil
                    flycheck-option-int)
            (option "--max-line-length" flycheck-flake8-maximum-line-length nil
                    flycheck-option-int)
            (eval (when buffer-file-name
                    (concat "--stdin-display-name="
                            (flycheck-buffer-file-local-name))))
            (eval flycheck-flake8-args)
            "-")
  :standard-input t
  :working-directory flycheck-python-find-project-root
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (mapcar #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-flake8))
                 (flycheck-python-find-module 'python-flake8 "flake8")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-flake8 "flake8"))
  :handle-suspicious flycheck--python-flake8-handle-suspicious
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-pylint)
                  (warning . python-mypy)))

;; same precedence as ruff when multiple configuration file detected
;; https://docs.astral.sh/ruff/configuration/#config-file-discovery
(flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                              '(".ruff.toml" "ruff.toml" "pyproject.toml"))

(flycheck-def-args-var flycheck-python-ruff-args python-ruff
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-python-ruff-select nil python-ruff
  "A list of rule codes to enable in Ruff, via `--select'.

Each element is a rule code or prefix, such as \"E\", \"F401\" or \"I\", or
the special value \"ALL\".  Passed to `ruff check' as a comma-separated
`--select' argument, replacing the selection Ruff would otherwise take from
its configuration.  When nil, Ruff's configured or default selection is
used."
  :type '(repeat (string :tag "Rule code"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-python-ruff-extend-select nil python-ruff
  "A list of rule codes to enable in Ruff on top of its configuration.

Like `flycheck-python-ruff-select', but passed via `--extend-select', so it
adds to the rules selected in `pyproject.toml' or `ruff.toml' rather than
replacing them."
  :type '(repeat (string :tag "Rule code"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-python-ruff-ignore nil python-ruff
  "A list of rule codes to disable in Ruff, via `--ignore'.

Each element is a rule code or prefix, such as \"E501\" or \"D\"."
  :type '(repeat (string :tag "Rule code"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-python-ruff-target-version nil python-ruff
  "The minimum Python version Ruff should assume, via `--target-version'.

A string such as \"py38\" or \"py312\", or nil to let Ruff infer the version
from the project's configuration."
  :type '(choice (const :tag "Inferred" nil)
                 (const "py37") (const "py38") (const "py39")
                 (const "py310") (const "py311") (const "py312")
                 (const "py313") (const "py314")
                 (string :tag "Version tag"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-python-ruff-preview nil python-ruff
  "Whether to enable Ruff's preview rules and fixes, via `--preview'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "39"))

(defun flycheck--explain-error-via-checker (checker &rest args)
  "Return an explainer function to call CHECKER with ARGS.
The checker output is fontified as Markdown."
  (lambda ()
    (apply #'flycheck-call-checker-process
           checker nil standard-output t args)
    (with-current-buffer standard-output
      (flycheck--fontify-as-markdown))))

(defun flycheck-python-ruff-explainer (err)
  "Return an explainer function for the ruff error ERR."
  (when-let* ((error-code (flycheck-error-id err)))
    (flycheck--explain-error-via-checker 'python-ruff "rule" error-code)))

(defun flycheck-parse-ruff--fix (fix buffer)
  "Build a `flycheck-fix' for BUFFER from a ruff FIX object, or nil.

Only safe fixes -- the ones `ruff check --fix' applies without
`--unsafe-fixes' -- are offered.  Each edit's `content' replaces
the region between its `location' and `end_location', both
one-based row/column pairs, as `flycheck-error' uses."
  (let-alist fix
    (when (equal .applicability "safe")
      (flycheck--make-fix
       buffer .message
       (seq-map
        (lambda (edit)
          (let-alist edit
            (flycheck-fix-edit-new
             :line .location.row :column .location.column
             :end-line .end_location.row :end-column .end_location.column
             :replacement .content)))
        .edits)))))

(defun flycheck-parse-ruff (output checker buffer)
  "Parse ruff JSON OUTPUT into Flycheck errors.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://docs.astral.sh/ruff/' for more information about
ruff."
  (seq-map
   (lambda (finding)
     (let-alist finding
       ;; ruff reports syntax errors with the code \"invalid-syntax\" (or a
       ;; null code before ruff 0.8); keep treating those as errors without a
       ;; rule id, and everything else as a warning that `:error-filter' may
       ;; promote (see `flycheck-flake8-fix-error-level').  ruff's own
       ;; severity is not used, to keep the levels Flycheck has always shown.
       (let ((syntax-error (or (null .code) (equal .code "invalid-syntax"))))
         ;; Keep the column-based region Flycheck showed with the text
         ;; output; the end position stays off so the highlighting does not
         ;; change.  The fix carries its own coordinates.
         (flycheck-error-new-at
          .location.row .location.column
          (if syntax-error 'error 'warning)
          .message
          :id (unless syntax-error .code)
          :checker checker
          :buffer buffer
          :filename (unless (equal .filename "-") .filename)
          :fix (flycheck-parse-ruff--fix .fix buffer)))))
   (car (flycheck-parse-json output))))

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using Ruff.

See URL `https://docs.astral.sh/ruff/'."
  :command ("ruff"
            "check"
            (config-file "--config" flycheck-python-ruff-config)
            (option "--select=" flycheck-python-ruff-select concat
                    flycheck-option-comma-separated-list)
            (option "--extend-select=" flycheck-python-ruff-extend-select concat
                    flycheck-option-comma-separated-list)
            (option "--ignore=" flycheck-python-ruff-ignore concat
                    flycheck-option-comma-separated-list)
            (option "--target-version=" flycheck-python-ruff-target-version concat)
            (option-flag "--preview" flycheck-python-ruff-preview)
            (eval flycheck-python-ruff-args)
            ;; JSON carries the machine-applicable fixes ruff computes (see
            ;; `flycheck-parse-ruff'); "--output-format" needs ruff >= 0.2.  Keep
            ;; it last so it wins over any --output-format in the args above.
            "--output-format=json"
            (eval (when buffer-file-name
                    (list "--stdin-filename" (flycheck-buffer-file-local-name))))
            "-")
  :standard-input t
  :error-parser flycheck-parse-ruff
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (dolist (err errors)
                      (when (flycheck-error-id err)
                        (flycheck-flake8-fix-error-level err)))
                    errors))
  :error-explainer flycheck-python-ruff-explainer
  :working-directory flycheck-python-find-project-root
  :handle-suspicious flycheck--python-ruff-handle-suspicious
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))

(define-obsolete-variable-alias 'flycheck-pylintrc
  'flycheck-pylint-config "39")
(flycheck-def-config-file-var
    flycheck-pylint-config python-pylint
    '("pylintrc" ".pylintrc" "pyproject.toml" "setup.cfg"))

(flycheck-def-option-var flycheck-pylint-use-symbolic-id t python-pylint
  "Whether to use pylint message symbols or message codes.

A pylint message has both an opaque identifying code (such as `F0401') and a
more meaningful symbolic code (such as `import-error').  This option governs
which should be used and reported to the user."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.25"))

(flycheck-def-args-var flycheck-pylint-args python-pylint
  :package-version '(flycheck . "39"))

(defun flycheck-parse-pylint (output checker buffer)
  "Parse JSON OUTPUT of CHECKER on BUFFER as Pylint errors."
  (mapcar (lambda (err)
            (let-alist err
              ;; Pylint can return -1 as a line or a column, hence the call to
              ;; `max'.  See `https://github.com/flycheck/flycheck/issues/1383'.
              (flycheck-error-new-at
               (and .line (max .line 1))
               (and .column (max (1+ .column) 1))
               (pcase .type
                 ;; See "pylint/utils.py"
                 ((or "fatal" "error") 'error)
                 ((or "info" "convention") 'info)
                 ((or "warning" "refactor" _) 'warning))
               ;; Drop lines showing the error in context
               (and (string-match (rx (*? nonl) eol) .message)
                    (match-string 0 .message))
               :id (if flycheck-pylint-use-symbolic-id .symbol .message-id)
               :checker checker
               :buffer buffer
               :filename .path)))
          (car (flycheck-parse-json output))))

(flycheck-define-checker python-pylint
  "A Python syntax and style checker using Pylint.

This syntax checker requires Pylint 1.0 or newer.

See URL `https://www.pylint.org/'."
  ;; --reports=n disables the scoring report.
  ;; Not calling pylint directly makes it easier to switch between different
  ;; Python versions; see https://github.com/flycheck/flycheck/issues/1055.
  :command ("python3"
            (eval (flycheck-python-module-args 'python-pylint "pylint"))
            "--reports=n"
            "--output-format=json"
            (config-file "--rcfile=" flycheck-pylint-config concat)
            (eval flycheck-pylint-args)
            ;; Need `source-inplace' for relative imports (e.g. `from .foo
            ;; import bar'), see https://github.com/flycheck/flycheck/issues/280
            source-inplace)
  :error-parser flycheck-parse-pylint
  :working-directory flycheck-python-find-project-root
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-pylint))
                 (flycheck-python-find-module 'python-pylint "pylint")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-pylint "pylint"))
  :error-explainer (lambda (err)
                     (when-let* ((id (flycheck-error-id err)))
                       (apply
                        #'flycheck-call-checker-process-for-output
                        'python-pylint nil t
                        (append
                         (flycheck-python-module-args 'python-pylint "pylint")
                         (list (format "--help-msg=%s" id))))))
  :handle-suspicious flycheck--python-pylint-handle-suspicious
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))

(flycheck-define-checker python-pycompile
  "A Python syntax checker using Python's builtin compiler.

See URL `https://docs.python.org/3.4/library/py_compile.html'."
  :command ("python3" "-m" "py_compile" source)
  :error-patterns
  ((error line-start "  File \"" (file-name) "\", line " line "\n"
          (>= 2 (zero-or-more not-newline) "\n")
          "SyntaxError: " (message) line-end)
   (error line-start "Sorry: IndentationError: "
          (message) "(" (file-name) ", line " line ")"
          line-end))
  :working-directory flycheck-python-find-project-root
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))

;; On systems where "python3" is not a working interpreter (e.g., Windows
;; where python3.exe is a Microsoft Store stub), fall back to "python".
(unless (ignore-errors (zerop (call-process "python3" nil nil nil "--version")))
  (dolist (checker '(json-python-json python-flake8 python-pylint
                     python-pycompile))
    (let ((var (flycheck-checker-executable-variable checker)))
      (set-default var "python"))))

(defun flycheck-pyright--parse-error (output checker buffer)
  "Parse pyright errors/warnings from JSON OUTPUT.
CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (mapcar
   (lambda (err)
     (let-alist err
       (flycheck-error-new-at
        (+ 1 .range.start.line)
        (+ 1 .range.start.character)
        (pcase .severity
          ("error" 'error)
          ("warning" 'warning)
          (_ 'warning))
        .message
        :id .rule
        :end-line (+ 1 .range.end.line)
        :end-column (+ 1 .range.end.character)
        :checker checker
        :buffer buffer
        :filename (buffer-file-name buffer))))
   (cdr (nth 2 (car (flycheck-parse-json output))))))

(flycheck-define-checker python-pyright
  "A Python static type checker using Pyright.

See URL `https://github.com/microsoft/pyright'."
  :command ("pyright"
            "--outputjson"
            source-inplace)
  :working-directory flycheck-python-find-project-root
  :error-parser flycheck-pyright--parse-error
  :modes (python-mode python-ts-mode))

(define-obsolete-variable-alias 'flycheck-python-mypy-ini
  'flycheck-python-mypy-config "32")

(flycheck-def-config-file-var flycheck-python-mypy-config python-mypy
                              '("mypy.ini" "pyproject.toml" "setup.cfg"))

(flycheck-def-option-var flycheck-python-mypy-cache-dir nil python-mypy
  "Directory used to write .mypy_cache directories."
  :type '(choice
          (const :tag "Write to the working directory" nil)
          (const :tag "Never write .mypy_cache directories" null-device)
          (string :tag "Path"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-python-mypy-python-executable nil python-mypy
  "Python executable to find the installed PEP 561 packages."
  :type '(choice (const :tag "Same as mypy's" nil)
                 (string :tag "Path"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "33"))

(flycheck-def-args-var flycheck-python-mypy-args python-mypy
  :package-version '(flycheck . "39"))

(flycheck-define-checker python-mypy
  "Mypy syntax and type checker.  Requires mypy>=0.730.

See URL `https://mypy-lang.org/'."
  :command ("mypy"
            "--show-column-numbers"
            "--show-error-codes"
            "--no-pretty"
            (config-file "--config-file" flycheck-python-mypy-config)
            (option "--cache-dir" flycheck-python-mypy-cache-dir)
            (option "--python-executable" flycheck-python-mypy-python-executable)
            (eval flycheck-python-mypy-args)
            source-original)
  :error-patterns
  ((error line-start (file-name) ":" line (optional ":" column)
          ": error:" (message) line-end)
   (warning line-start (file-name) ":" line (optional ":" column)
            ": warning:" (message) line-end)
   (info line-start (file-name) ":" line (optional ":" column)
         ": note:" (message) line-end))
  :error-filter
  (lambda (errors)
    (dolist (err errors)
      (let ((msg (flycheck-error-message err)))
        (when (and msg (string-match "\\(.*?\\)  \\[\\([^]]+\\)\\]\\'" msg))
          (setf (flycheck-error-message err) (match-string 1 msg))
          (setf (flycheck-error-id err) (match-string 2 msg)))))
    errors)
  :working-directory flycheck-python-find-project-root
  :error-explainer
  (flycheck-error-explainer-from-url
   "https://mypy.readthedocs.io/en/stable/error_code_list.html#code-%s")
  :handle-suspicious flycheck--python-mypy-handle-suspicious
  :modes (python-mode python-ts-mode)
  ;; Ensure the file is saved, to work around
  ;; https://github.com/python/mypy/issues/4746.
  :predicate flycheck-buffer-saved-p)

(flycheck-def-option-var flycheck-lintr-caching t r-lintr
  "Whether to enable caching in lintr.

By default, lintr caches all expressions in a file and re-checks
only those that have changed.  Setting this option to nil
disables caching in case there are problems."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-lintr-linters "default_linters" r-lintr
  "Linters to use with lintr.

The value of this variable is a string containing an R
expression, passed as the `linters' argument to the
lintr::lint() function."
  :type 'string
  :risky t
  :package-version '(flycheck . "0.23"))

(defun flycheck-r-has-lintr (checker)
  "Whether CHECKER (R) has installed the `lintr' library."
  (zerop (flycheck-call-checker-process
          checker nil nil nil
          "--slave" "--no-restore" "--no-save" "-e"
          "library('lintr')")))

(flycheck-define-checker r-lintr
  "An R style and syntax checker using the lintr package.

See URL `https://github.com/jimhester/lintr'."
  :command ("R" "--slave" "--no-restore" "--no-save" "-e"
            (eval (concat
                   "library(lintr);"
                   "try(lint(commandArgs(TRUE)"
                   ", cache=" (if flycheck-lintr-caching "TRUE" "FALSE")
                   ", linters=" flycheck-lintr-linters
                   "))"))
            "--args" source)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column ": style: " (message)
         line-end)
   (warning line-start (file-name) ":" line ":" column ": warning: " (message)
            line-end)
   (error line-start (file-name) ":" line ":" column ": error: " (message)
          line-end))
  :modes (ess-mode ess-r-mode)
  :predicate
  ;; Don't check ESS files which do not contain R, and make sure that lintr is
  ;; actually available
  (lambda ()
    (and (equal ess-language "S")
         (flycheck-r-has-lintr 'r-lintr)))
  :verify (lambda (checker)
            (let ((has-lintr (flycheck-r-has-lintr checker)))
              (list
               (flycheck-verification-result-new
                :label "lintr library"
                :message (if has-lintr "present" "missing")
                :face (if has-lintr 'success '(bold error)))))))

(flycheck-define-checker r
  "An R syntax checker using the builtin `parse' function.

See URL: `https://www.r-project.org/'."
  :command ("R" "--slave" "--no-restore" "--no-save" "-e"
            "parse(file=file('stdin'), srcfile='<stdin>')")
  :standard-input t
  :error-patterns
  ((error line-start (zero-or-more space) "<stdin>:" line ":" column ": "
          (message) line-end))
  :modes (ess-mode ess-r-mode)
  :predicate
  ;; Don't check ESS files which do not contain R
  (lambda () (equal ess-language "S")))

(defun flycheck-racket-has-expand-p (checker)
  "Whether the executable of CHECKER provides the `expand' command."
  (zerop (flycheck-call-checker-process checker nil nil nil "expand")))

(flycheck-define-checker racket
  "A Racket syntax checker with `raco expand'.

The `compiler-lib' racket package is required for this syntax
checker.

See URL `https://racket-lang.org/'."
  :command ("raco" "expand" source-inplace)
  :predicate
  (lambda ()
    (and (or (not (eq major-mode 'scheme-mode))
             ;; In `scheme-mode' we must check the current Scheme implementation
             ;; being used
             (and (boundp 'geiser-impl--implementation)
                  (eq geiser-impl--implementation 'racket)))
         (flycheck-racket-has-expand-p 'racket)))
  :verify
  (lambda (checker)
    (let ((has-expand (flycheck-racket-has-expand-p checker))
          (in-scheme-mode (eq major-mode 'scheme-mode))
          (geiser-impl (bound-and-true-p geiser-impl--implementation)))
      (list
       (flycheck-verification-result-new
        :label "compiler-lib package"
        :message (if has-expand "present" "missing")
        :face (if has-expand 'success '(bold error)))
       (flycheck-verification-result-new
        :label "Geiser Implementation"
        :message (cond
                  ((not in-scheme-mode) "Using Racket Mode")
                  ((eq geiser-impl 'racket) "Racket")
                  (geiser-impl (format "Other: %s" geiser-impl))
                  (t "Geiser not active"))
        :face (cond
               ((or (not in-scheme-mode) (eq geiser-impl 'racket)) 'success)
               (t '(bold error)))))))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-increment-error-columns
      (seq-remove
       (lambda (err)
         (string-suffix-p
          "/share/racket/pkgs/compiler-lib/compiler/commands/expand.rkt"
          (flycheck-error-filename err)))
       errors))))
  :error-patterns
  ((error line-start (zero-or-more space)
          (file-name) ":" line ":" column ":" (message) line-end))
  :modes (racket-mode scheme-mode))

(flycheck-define-checker rpm-rpmlint
  "A RPM SPEC file syntax checker using rpmlint.

See URL `https://github.com/rpm-software-management/rpmlint'."
  :command ("rpmlint" source)
  :error-patterns
  ((error line-start
          (file-name) ":" (optional line ":") " E: " (message)
          line-end)
   (warning line-start
            (file-name) ":" (optional line ":") " W: " (message)
            line-end))
  :error-filter
  ;; rpmlint 1.1 outputs a spurious error for the temp file created by flycheck
  (lambda (errors)
    (let ((filtered (seq-remove
                     (lambda (err)
                       (string-suffix-p "(none)" (flycheck-error-filename err)))
                     errors)))
      ;; Add fake line numbers if they are missing in the lint output
      (dolist (err filtered)
        (unless (flycheck-error-line err)
          (setf (flycheck-error-line err) 1)))
      filtered))
  :error-explainer
  (lambda (error)
    (when-let* ((error-message (flycheck-error-message error))
                (message-id (save-match-data
                              (string-match "\\([^ ]+\\)" error-message)
                              (match-string 1 error-message))))
      (flycheck-call-checker-process-for-output
       'rpm-rpmlint nil t "-I" message-id)))
  :modes (sh-mode rpm-spec-mode)
  :predicate (lambda () (or (not (eq major-mode 'sh-mode))
                            ;; In `sh-mode', we need the proper shell
                            (eq sh-shell 'rpm))))

(flycheck-def-config-file-var flycheck-markdown-markdownlint-cli-config
    markdown-markdownlint-cli
    '(".markdownlint.json" ".markdownlint.jsonc" ".markdownlint.yaml")
  :package-version '(flycheck . "33"))

(define-obsolete-variable-alias 'flycheck-markdown-markdownlint-cli-disable-rules
  'flycheck-markdown-markdownlint-cli-disabled-rules "39")
(flycheck-def-option-var flycheck-markdown-markdownlint-cli-disabled-rules
    nil markdown-markdownlint-cli
  "Rules to disable for markdownlint-cli."
  :type '(repeat :tag "Disabled rule"
                 (string :tag "Rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "33"))

(define-obsolete-variable-alias 'flycheck-markdown-markdownlint-cli-enable-rules
  'flycheck-markdown-markdownlint-cli-enabled-rules "39")
(flycheck-def-option-var flycheck-markdown-markdownlint-cli-enabled-rules
    nil markdown-markdownlint-cli
  "Rules to enable for markdownlint-cli."
  :type '(repeat :tag "Enabled rule"
                 (string :tag "Rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "33"))

(flycheck-def-args-var flycheck-markdown-markdownlint-cli-args
    markdown-markdownlint-cli
  :package-version '(flycheck . "39"))

(defun flycheck-markdownlint-error-filter (errors)
  "Error filter for markdownlint checkers, applied to ERRORS."
  (flycheck-sanitize-errors
   (flycheck-remove-error-file-names "(string)" errors)))

(defalias 'flycheck-markdownlint-error-explainer
  (flycheck-error-explainer-from-url
   "https://github.com/DavidAnson/markdownlint/blob/main/doc/Rules.md#%s"
   ;; the ID is "MDNNN/rule-name"; the doc anchor is just the "MDNNN" code
   (lambda (id) (substring id 0 5)))
  "Browse the markdownlint rule documentation for the error at point.")

(defun flycheck-parse-markdownlint--column (line index buffer)
  "Convert markdownlint's 1-based UTF-16 INDEX on LINE of BUFFER to a column.

markdownlint counts JS string indices, which are UTF-16 code units, so
a character outside the Basic Multilingual Plane counts twice and an
edit to the right of one lands a column short, inside text it should
leave alone.  Walk LINE's text consuming INDEX's units; the characters
walked are the column.  Units left past the line's end, or the whole
INDEX when BUFFER no longer has the line, count one column each, which
keeps an edit appending at the end of a line intact."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (let ((text (buffer-substring-no-properties
                     (point) (line-end-position)))
              (units (1- index))
              (chars 0))
          (while (and (> units 0) (< chars (length text)))
            (cl-decf units (if (> (aref text chars) #xFFFF) 2 1))
            (cl-incf chars))
          (+ 1 chars (max 0 units)))))))

(defun flycheck-parse-markdownlint--fix (info line description buffer)
  "Build a `flycheck-fix' for BUFFER from markdownlint's fixInfo INFO.

INFO edits one line, LINE unless it names another: it deletes
`deleteCount' characters at `editColumn' and puts `insertText' there.
A `deleteCount' of -1 deletes the whole line, newline included.
DESCRIPTION describes the fix to the user."
  (let-alist info
    (let ((fline (or .lineNumber line)))
      (flycheck--make-fix
       buffer description
       (list (if (eql .deleteCount -1)
                 (flycheck-fix-edit-new
                  :line fline :column 1
                  :end-line (1+ fline) :end-column 1
                  :replacement "")
               ;; Both endpoints go through the UTF-16 conversion; the
               ;; deleted span may itself contain astral characters, so
               ;; converting deleteCount alone would not do.
               (let ((start (or .editColumn 1)))
                 (flycheck-fix-edit-new
                  :line fline
                  :column (flycheck-parse-markdownlint--column
                           fline start buffer)
                  :end-line fline
                  :end-column (flycheck-parse-markdownlint--column
                               fline (+ start (or .deleteCount 0)) buffer)
                  :replacement (or .insertText "")))))))))

(defun flycheck-parse-markdownlint (output checker buffer)
  "Parse markdownlint JSON OUTPUT into Flycheck errors.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and the
BUFFER that was checked respectively.  OUTPUT is what `--json' prints:
one array of findings, each carrying the rule's names, an optional
1-based `errorRange', and, for the auto-fixable findings, a `fixInfo'.
A clean file prints nothing at all.

See URL `https://github.com/DavidAnson/markdownlint' for more
information."
  (seq-map
   (lambda (finding)
     (let-alist finding
       ;; The message mirrors the text output: the rule's description
       ;; with the detail and the offending context bracketed after it.
       ;; Only the start column is set, as the text output had it, so
       ;; the highlighting does not change; the fix carries its own
       ;; coordinates.
       (flycheck-error-new-at
        .lineNumber
        (and .errorRange (elt .errorRange 0))
        (if (equal .severity "warning") 'warning 'error)
        (concat .ruleDescription
                (when .errorDetail (format " [%s]" .errorDetail))
                (when .errorContext (format " [Context: \"%s\"]" .errorContext)))
        :id (string-join .ruleNames "/")
        :checker checker
        :buffer buffer
        :filename .fileName
        :fix (and .fixInfo
                  (flycheck-parse-markdownlint--fix
                   .fixInfo .lineNumber .ruleDescription buffer)))))
   (car (flycheck-parse-json output))))

(flycheck-define-checker markdown-markdownlint-cli
  "Markdown checker using markdownlint-cli.

See URL `https://github.com/igorshubovych/markdownlint-cli'."
  :command ("markdownlint"
            "--json"
            (config-file "--config" flycheck-markdown-markdownlint-cli-config)
            (option-list "--disable" flycheck-markdown-markdownlint-cli-disabled-rules)
            (option-list "--enable" flycheck-markdown-markdownlint-cli-enabled-rules)
            (eval flycheck-markdown-markdownlint-cli-args)
            "--"
            source)
  :error-parser flycheck-parse-markdownlint
  :error-filter flycheck-markdownlint-error-filter
  :modes (markdown-mode gfm-mode)
  :error-explainer flycheck-markdownlint-error-explainer
  :next-checkers ((warning . proselint)))

(flycheck-def-config-file-var flycheck-markdown-markdownlint-cli2-config
    markdown-markdownlint-cli2
    '(".markdownlint-cli2.json" ".markdownlint-cli2.jsonc" ".markdownlint-cli2.yaml")
  :package-version '(flycheck . "35"))

(flycheck-def-args-var flycheck-markdown-markdownlint-cli2-args
    markdown-markdownlint-cli2
  :package-version '(flycheck . "39"))

(flycheck-define-checker markdown-markdownlint-cli2
  "Markdown checker using markdownlint-cli2.

See URL `https://github.com/DavidAnson/markdownlint-cli2'."
  :command ("markdownlint-cli2"
            (config-file "--config" flycheck-markdown-markdownlint-cli2-config)
            (eval flycheck-markdown-markdownlint-cli2-args)
            "--"
            source)
  :error-patterns
  (;; markdownlint-cli v0.42+/cli2 v0.14+ include a severity word
   (error line-start
          (file-name) ":" line
          (? ":" column) " "
          (or "error" "warning") " "
          (id (one-or-more (not (any space))))
          " " (message) line-end)
   ;; older versions without severity word
   (error line-start
          (file-name) ":" line
          (? ":" column) " " (id (one-or-more (not (any space))))
          " " (message) line-end))
  :error-filter flycheck-markdownlint-error-filter
  :modes (markdown-mode gfm-mode)
  :error-explainer flycheck-markdownlint-error-explainer
  :next-checkers ((warning . proselint)))

(flycheck-def-option-var flycheck-markdown-mdl-rules nil markdown-mdl
  "Rules to enable for mdl.

The value of this variable is a list of strings each of which is
the name of a rule to enable.

By default all rules are enabled.

See URL `https://github.com/markdownlint/markdownlint/blob/main/docs/RULES.md'."
  :type '(repeat :tag "Enabled rules"
                 (string :tag "rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "27"))

(flycheck-def-option-var flycheck-markdown-mdl-tags nil markdown-mdl
  "Rule tags to enable for mdl.

The value of this variable is a list of strings each of which is
the name of a rule tag.  Only rules with these tags are enabled.

By default all rules are enabled.

See URL `https://github.com/markdownlint/markdownlint/blob/main/docs/RULES.md'."
  :type '(repeat :tag "Enabled tags"
                 (string :tag "tag name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "27"))

(flycheck-def-config-file-var flycheck-markdown-mdl-style markdown-mdl nil
  :package-version '(flycheck . "27"))

(flycheck-define-checker markdown-mdl
  "Markdown checker using mdl.

See URL `https://github.com/markdownlint/markdownlint'."
  :command ("mdl"
            (config-file "--style" flycheck-markdown-mdl-style)
            (option "--tags=" flycheck-markdown-mdl-tags concat
                    flycheck-option-comma-separated-list)
            (option "--rules=" flycheck-markdown-mdl-rules concat
                    flycheck-option-comma-separated-list))
  :standard-input t
  :error-patterns
  ((error line-start
          (file-name) ":" line ": " (id (one-or-more alnum)) " " (message)
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "(stdin)" errors)))
  :modes (markdown-mode gfm-mode)
  :next-checkers ((warning . proselint)))

(flycheck-def-config-file-var flycheck-markdown-pymarkdown-config
    markdown-pymarkdown nil
  :package-version '(flycheck . "34"))

(flycheck-define-checker markdown-pymarkdown
  "Markdown checker using PyMarkdown.

See URL `https://pypi.org/project/pymarkdownlnt/'."
  :command ("pymarkdown"
            (config-file "--config" flycheck-markdown-pymarkdown-config)
            "scan"
            source)
  :error-patterns
  ((error line-start
          (file-name) ":" line
          (? ":" column) ": " (id (one-or-more alnum))
          ": " (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "(string)" errors)))
  :modes (markdown-mode gfm-mode)
  :next-checkers ((warning . proselint)))

(flycheck-define-checker nix
  "Nix checker using nix-instantiate.

See URL `https://nixos.org/nix/manual/#sec-nix-instantiate'."
  :command ("nix-instantiate" "--parse" "-")
  :standard-input t
  :error-patterns
  ((error line-start
          "error: " (message)
          (one-or-more "\n")
          (zero-or-more space) "at «stdin»:" line ":" column ":" line-end)
   (error line-start
          "at: (" line ":" column ") from stdin"
          (one-or-more "\n" (zero-or-more space (one-or-more not-newline)))
          (message) line-end)
   (error line-start
          "error: " (message) " at " (file-name) ":" line ":" column
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "(string)" errors)))
  :modes (nix-mode nix-ts-mode))

(defun flycheck-parse-statix (output checker buffer)
  "Parse statix warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/nerdypepper/statix' for more
information about statix."
  (mapcar (lambda (err)
            (let-alist err
              ;; Diagnostic information is a (seemingly always) 1 element array.
              (let-alist (car .diagnostics)
                (flycheck-error-new-at
                 .at.from.line
                 .at.from.column
                 (pcase (alist-get 'severity err)
                   ("Error" 'error)
                   (_ 'warning))
                 (format "%s: %s" (alist-get 'note err) .message)
                 :id (format "%s%02d" (pcase (alist-get 'severity err)
                                        ("Error" "E")
                                        ("Warn" "W")
                                        (_ ""))
                             (alist-get 'code err))
                 :checker checker
                 :buffer buffer
                 :filename (buffer-file-name buffer)
                 :end-line .at.to.line
                 :end-column .at.to.column))))
          (alist-get 'report (car (flycheck-parse-json output)))))

(flycheck-def-args-var flycheck-statix-args statix
  :package-version '(flycheck . "39"))

(flycheck-define-checker statix
  "Nix checker using statix.

See URL `https://github.com/nerdypepper/statix'."
  :command ("statix" "check" "-o=json"
            (eval flycheck-statix-args)
            source)
  :error-parser flycheck-parse-statix
  :modes (nix-mode nix-ts-mode))

(defun flycheck-locate-sphinx-source-directory ()
  "Locate the Sphinx source directory for the current buffer.

Return the source directory, or nil, if the current buffer is not
part of a Sphinx project."
  (when-let* ((filename (buffer-file-name))
              (dir (locate-dominating-file filename "conf.py")))
    (expand-file-name dir)))

(flycheck-define-checker rst
  "A ReStructuredText (RST) syntax checker using Docutils.

Docutils 0.21 dropped the `.py' from the names of its front ends, so
this looks for `rst2pseudoxml'.  On an older Docutils, point
`flycheck-rst-executable' at `rst2pseudoxml.py'.

See URL `https://docutils.sourceforge.io/'."
  ;; include:: directives
  :command ("rst2pseudoxml" "--report=2" "--halt=5"
            ;; Read from standard input and throw output away
            "-" null-device)
  :standard-input t
  :error-patterns
  ((warning line-start "<stdin>:" line ": (WARNING/2) " (message) line-end)
   (error line-start "<stdin>:" line
          ": (" (or "ERROR/3" "SEVERE/4") ") "
          (message) line-end))
  :modes rst-mode
  :next-checkers ((warning . proselint)))

(flycheck-def-option-var flycheck-sphinx-warn-on-missing-references t rst-sphinx
  "Whether to warn about missing references in Sphinx.

When non-nil (the default), warn about all missing references in
Sphinx via `-n'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.17"))

(flycheck-define-checker rst-sphinx
  "A ReStructuredText (RST) syntax checker using Sphinx.

Requires Sphinx 1.2 or newer.  See URL `https://sphinx-doc.org'."
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
                             (flycheck-locate-sphinx-source-directory)))
  :next-checkers ((warning . proselint)))

(defun flycheck-ruby--find-project-root (_checker)
  "Compute an appropriate working-directory for flycheck-ruby.

This is either a parent directory containing a Gemfile, or nil."
  (and
   buffer-file-name
   (locate-dominating-file buffer-file-name "Gemfile")))

(defun flycheck-ruby--filter-rubocop-errors (errors)
  "Filter RuboCop ERRORS attributed to dummy stdin filename."
  (flycheck-remove-error-file-names
   (flycheck--file-truename (expand-file-name "stdin"))
   errors))

(define-obsolete-variable-alias 'flycheck-rubocoprc
  'flycheck-rubocop-config "39")
(flycheck-def-config-file-var flycheck-rubocop-config ruby-rubocop ".rubocop.yml")

(flycheck-def-option-var flycheck-rubocop-lint-only nil
                         (ruby-rubocop ruby-standard ruby-chef-cookstyle)
  "Whether to only report code issues in Rubocop, Cookstyle and Standard.

When non-nil, only report code issues, via `--lint'.  Otherwise
report style issues as well."
  :safe #'booleanp
  :type 'boolean
  :package-version '(flycheck . "0.16"))

(flycheck-def-args-var flycheck-rubocop-args ruby-rubocop
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-rubocop-server nil ruby-rubocop
  "Whether to run RuboCop in server mode, via `--server'.

When non-nil, RuboCop keeps a warm server process alive between runs,
which greatly cuts its startup time on repeated checks.  See URL
`https://docs.rubocop.org/rubocop/usage/server.html'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-rubocop-only nil ruby-rubocop
  "A list of cops to run exclusively in RuboCop, via `--only'.

Each element is a cop or department name, such as
\"Style/StringLiterals\" or \"Lint\"."
  :type '(repeat (string :tag "Cop or department"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-rubocop-except nil ruby-rubocop
  "A list of cops to disable in RuboCop, via `--except'.

Each element is a cop or department name, such as
\"Style/StringLiterals\" or \"Metrics\"."
  :type '(repeat (string :tag "Cop or department"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(defun flycheck-ruby-rubocop-error-explainer (err)
  "Browse the RuboCop documentation for the cop of error ERR.
The error ID is a DEPARTMENT/CopName cop name.  Only RuboCop's own built-in
departments are documented at docs.rubocop.org, so cops from extensions
\(RuboCop RSpec, Rails, ...) yield no explanation rather than a broken link."
  (when-let* ((id (flycheck-error-id err))
              ((string-match "\\`\\([A-Z][a-zA-Z]*\\)/" id))
              (dept (downcase (match-string 1 id)))
              ((member dept '("bundler" "gemspec" "layout" "lint" "metrics"
                              "migration" "naming" "security" "style")))
              (anchor (downcase (replace-regexp-in-string "/" "" id))))
    (cons 'url (format "https://docs.rubocop.org/rubocop/cops_%s.html#%s"
                       dept anchor))))

(defconst flycheck-ruby-rubocop-error-patterns
  '((info line-start (file-name) ":" line ":" column ": C: "
          (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
    (warning line-start (file-name) ":" line ":" column ": W: "
             (optional (id (one-or-more (not (any ":")))) ": ") (message)
             line-end)
    (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
           (optional (id (one-or-more (not (any ":")))) ": ") (message)
           line-end))
  "Error patterns shared by RuboCop-based checkers.")

(flycheck-def-executable-var ruby-rubocop "rubocop")
(flycheck-define-command-checker 'ruby-rubocop
  "A Ruby syntax and style checker using the RuboCop tool.

You need at least RuboCop 0.34 for this syntax checker.

See URL `https://rubocop.org/'."
  ;; ruby-standard is defined based on this checker
  :command '("rubocop"
             "--display-cop-names"
             "--force-exclusion"
             "--format" "emacs"
             (config-file "--config" flycheck-rubocop-config)
             (option-flag "--lint" flycheck-rubocop-lint-only)
             (option-flag "--server" flycheck-rubocop-server)
             (option "--only" flycheck-rubocop-only nil
                     flycheck-option-comma-separated-list)
             (option "--except" flycheck-rubocop-except nil
                     flycheck-option-comma-separated-list)
             (eval flycheck-rubocop-args)
             ;; RuboCop takes the original file name as argument when reading
             ;; from standard input, but it chokes when that name is the empty
             ;; string, so fall back to "stdin" in order to handle buffers with
             ;; no backing file (e.g. org-mode snippet buffers)
             "--stdin" (eval (flycheck-buffer-file-local-name "stdin")))
  :standard-input t
  :working-directory #'flycheck-ruby--find-project-root
  :error-patterns flycheck-ruby-rubocop-error-patterns
  :error-filter #'flycheck-ruby--filter-rubocop-errors
  :error-explainer #'flycheck-ruby-rubocop-error-explainer
  :handle-suspicious #'flycheck--rubocop-handle-suspicious
  :modes '(enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers '((warning . ruby-reek)
                   (warning . ruby-chef-cookstyle)))

(flycheck-def-executable-var ruby-chef-cookstyle "cookstyle")
(flycheck-define-command-checker 'ruby-chef-cookstyle
  "A Chef (Ruby) syntax and style checker using the Cookstyle tool.
Basically Cookstyle is a thin wrapper around RuboCop, so this
checker is essentially the same.

See URL `https://github.com/chef/cookstyle'."
  :command '("cookstyle"
             "--display-cop-names"
             "--force-exclusion"
             "--format" "emacs"
             (config-file "--config" flycheck-rubocop-config)
             (option-flag "--lint" flycheck-rubocop-lint-only)
             ;; RuboCop takes the original file name as argument when reading
             ;; from standard input, but it chokes when that name is the empty
             ;; string, so fall back to "stdin" in order to handle buffers with
             ;; no backing file (e.g. org-mode snippet buffers)
             "--stdin" (eval (flycheck-buffer-file-local-name "stdin")))
  :standard-input t
  :working-directory #'flycheck-ruby--find-project-root
  :error-patterns flycheck-ruby-rubocop-error-patterns
  :error-filter #'flycheck-ruby--filter-rubocop-errors
  :handle-suspicious #'flycheck--rubocop-handle-suspicious
  :modes '(enh-ruby-mode ruby-mode ruby-ts-mode)
  :predicate
  (lambda ()
    (let ((parent-dir (file-name-directory
                       (directory-file-name
                        (expand-file-name default-directory)))))
      (or
       ;; Chef CookBook
       ;; https://docs.opscode.com/chef/knife.html#id38
       (locate-dominating-file parent-dir "recipes")
       ;; Knife Solo
       ;; https://matschaffer.github.io/knife-solo/#label-Init+command
       (locate-dominating-file parent-dir "cookbooks"))))
  :next-checkers '((warning . ruby-reek)))

(define-obsolete-variable-alias 'flycheck-ruby-standardrc
  'flycheck-ruby-standard-config "39")
(flycheck-def-config-file-var flycheck-ruby-standard-config ruby-standard
                              ".standard.yml")

(flycheck-def-executable-var ruby-standard "standardrb")
(flycheck-define-command-checker 'ruby-standard
  "A Ruby syntax and style checker using the StandardRB gem.

See URL `https://github.com/testdouble/standard' for more information."
  ;; This checker is derived from ruby-rubocop; see above
  :command '("standardrb"
             "--display-cop-names"
             "--force-exclusion"
             "--format" "emacs"
             "--cache" "false"
             (config-file "--config" flycheck-ruby-standard-config)
             (option-flag "--lint" flycheck-rubocop-lint-only)
             "--stdin" source-original)
  :standard-input t
  :working-directory #'flycheck-ruby--find-project-root
  :error-patterns flycheck-ruby-rubocop-error-patterns
  :error-filter #'flycheck-ruby--filter-rubocop-errors
  :error-explainer #'flycheck-ruby-rubocop-error-explainer
  :handle-suspicious #'flycheck--rubocop-handle-suspicious
  :modes '(enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers '((warning . ruby-reek)
                   (warning . ruby-chef-cookstyle)))

(define-obsolete-variable-alias 'flycheck-reekrc
  'flycheck-reek-config "39")
(flycheck-def-config-file-var flycheck-reek-config ruby-reek ".reek.yml"
  :package-version '(flycheck . "30"))

(flycheck-define-checker ruby-reek
  "A Ruby smell checker using reek.

See URL `https://github.com/troessner/reek'."
  :command ("reek" "--format" "json"
            (config-file "--config" flycheck-reek-config)
            source)
  :error-parser flycheck-parse-reek
  :modes (enh-ruby-mode ruby-mode ruby-ts-mode))

(flycheck-define-checker ruby
  "A Ruby syntax checker using the standard Ruby interpreter.

Please note that the output of different Ruby versions and
implementations varies wildly.  This syntax checker supports
current versions of MRI and JRuby, but may break when used with
other implementations or future versions of these
implementations.

Please consider using `ruby-rubocop' or `ruby-reek' instead.

See URL `https://www.ruby-lang.org/'."
  :command ("ruby" "-w" "-c")
  :standard-input t
  :error-patterns
  ;; These patterns support output from JRuby, too, to deal with RVM or Rbenv
  ((error line-start "SyntaxError in -:" line ": " (message) line-end)
   (warning line-start "-:" line ":" (optional column ":")
            " warning: " (message) line-end)
   ;; Ruby 3.4 includes the interpreter path when emitting syntax errors
   (error line-start (optional (one-or-more (not (any ":"))) ": ") "-:" line ": " (message) line-end))
  :modes (enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers ((warning . ruby-chef-cookstyle)))

(flycheck-def-args-var flycheck-cargo-check-args (rust-cargo)
  :package-version '(flycheck . "32"))

(flycheck-def-args-var flycheck-rust-args (rust)
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-rust-check-tests t (rust-cargo rust)
  "Whether to check test code in Rust.

For the `rust' checker: When non-nil, `rustc' is passed the
`--test' flag, which will check any code marked with the
`#[cfg(test)]' attribute and any functions marked with
`#[test]'. Otherwise, `rustc' is not passed `--test' and test
code will not be checked.  Skipping `--test' is necessary when
using `#![no_std]', because compiling the test runner requires
`std'.

For the `rust-cargo' checker: When non-nil, calls `cargo test
--no-run' instead of `cargo check'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.19"))

(flycheck-def-option-var flycheck-rust-crate-root nil rust
  "A path to the crate root for the current buffer.

The value of this variable is either a string with the path to
the crate root for the current buffer, or nil if the current buffer
is a crate.  A relative path is relative to the current buffer.

If this variable is non-nil the current buffer will only be checked
if it is not modified, i.e. after it has been saved."
  :type '(choice (const :tag "Unspecified" nil)
                 (file :tag "Root"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "0.20"))
(make-variable-buffer-local 'flycheck-rust-crate-root)

(flycheck-def-option-var flycheck-rust-crate-type "lib" (rust-cargo rust)
  "The type of the Rust Crate to check.

For `rust-cargo', the value should be a string denoting the
target type passed to Cargo.  See
`flycheck-rust-valid-crate-type-p' for the list of allowed
values.

For `rust', the value should be a string denoting the crate type
for the `--crate-type' flag of rustc."
  :type '(choice (const :tag "nil (rust/rust-cargo)" nil)
                 (const :tag "lib (rust/rust-cargo)" "lib")
                 (const :tag "bin (rust/rust-cargo)" "bin")
                 (const :tag "example (rust-cargo)" "example")
                 (const :tag "test (rust-cargo)" "test")
                 (const :tag "bench (rust-cargo)" "bench")
                 (const :tag "rlib (rust)" "rlib")
                 (const :tag "dylib (rust)" "dylib")
                 (const :tag "cdylib (rust)" "cdylib")
                 (const :tag "staticlib (rust)" "staticlib")
                 (const :tag "metadata (rust)" "metadata"))
  :safe #'stringp
  :package-version '(flycheck . "0.20"))
(make-variable-buffer-local 'flycheck-rust-crate-type)

(flycheck-def-option-var flycheck-rust-binary-name nil rust-cargo
  "The name of the binary to pass to `cargo check --CRATE-TYPE'.

The value of this variable is a string denoting the name of the
target to check: usually the name of the crate, or the name of
one of the files under `src/bin', `tests', `examples' or
`benches'.

This always requires a non-nil value, unless
`flycheck-rust-crate-type' is `lib' or nil, in which case it is
ignored."
  :type '(choice (const :tag "Unspecified" nil)
                 (string :tag "Binary name"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "28"))
(make-variable-buffer-local 'flycheck-rust-binary-name)

(flycheck-def-option-var flycheck-rust-features nil (rust-cargo rust-clippy)
  "List of features to activate during build or check.

The value of this variable is a list of strings denoting features
that will be activated to build the target to check. Features will
be passed to `cargo check --features=FEATURES' and, for the
`rust-clippy' checker, to `cargo clippy --features=FEATURES'."
  :type '(repeat :tag "Features to activate"
                 (string :tag "Feature"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-rust-features)

(flycheck-def-option-var flycheck-rust-library-path nil rust
  "A list of library directories for Rust.

The value of this variable is a list of strings, where each
string is a directory to add to the library path of Rust.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Library directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.18"))

(flycheck-def-option-var flycheck-rust-edition nil rust
  "The Rust edition for the `rust' checker.

Passed to `rustc' as `--edition'.  When nil, `rustc' uses its own
default, edition 2015, which rejects code written for a newer edition.
Set this to a string such as \"2021\" or \"2024\" to check single-file
code that is not part of a Cargo project.  The `rust-cargo' checker takes
the edition from `Cargo.toml' and ignores this variable."
  :type '(choice (const :tag "rustc default (2015)" nil)
                 (const "2015") (const "2018")
                 (const "2021") (const "2024")
                 (string :tag "Other edition"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "39"))
(make-variable-buffer-local 'flycheck-rust-edition)

(flycheck-def-args-var flycheck-rust-clippy-args (rust-clippy)
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-rust-clippy-tests nil rust-clippy
  "Whether to lint test code with Clippy.

When non-nil, `cargo clippy' is passed `--tests', so code marked with
`#[cfg(test)]' or `#[test]' is linted as well."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-rust-clippy-all-targets nil rust-clippy
  "Whether to lint all targets with Clippy.

When non-nil, `cargo clippy' is passed `--all-targets', linting tests,
benches and examples on top of the default targets."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-rust-clippy-all-features nil rust-clippy
  "Whether to lint with all Cargo features enabled in Clippy.

When non-nil, `cargo clippy' is passed `--all-features'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "39"))

(defun flycheck--fontify-as-markdown ()
  "Place current buffer in `markdown-view-mode' and fontify it."
  (when (fboundp 'markdown-view-mode)
    (let ((markdown-fontify-code-block-default-mode 'rust-mode)
          (markdown-fontify-code-blocks-natively t)
          (markdown-hide-markup t))
      (markdown-view-mode)
      (font-lock-flush)
      (font-lock-ensure))))

(defun flycheck-rust-error-explainer (error)
  "Return an explainer function for the given rustc error ERROR."
  (when-let* ((error-code (flycheck-error-id error)))
    (flycheck--explain-error-via-checker 'rust "--explain" error-code)))

(defun flycheck-rust-error-filter (errors)
  "Filter ERRORS from rustc output that have no explanatory value."
  (seq-remove
   (lambda (err)
     (or
      ;; Macro errors emit a diagnostic in a phony file,
      ;; e.g. "<println macros>".
      (when-let* ((filename (flycheck-error-filename err)))
        (string-match-p (rx "macros>" line-end) filename))
      ;; Redundant message giving the number of failed errors
      (when-let* ((msg (flycheck-error-message err)))
        (string-match-p
         (rx
          (or (: "aborting due to " (optional (one-or-more num) " ")
                 "previous error")
              (: "For more information about this error, try `rustc --explain "
                 (one-or-more alnum) "`.")))
         msg))))
   errors))

(defun flycheck-rust-manifest-directory ()
  "Return the nearest directory holding the Cargo manifest.

Return the nearest directory containing the `Cargo.toml' manifest
file, starting from the current buffer and using
`locate-dominating-file'.  Return nil if there is no such file,
or if the current buffer has no file name."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "Cargo.toml")))

(defun flycheck-rust-cargo-metadata ()
  "Run `cargo metadata' and return the result as parsed JSON object."
  (car (flycheck-parse-json
        (flycheck-call-checker-process-for-output
         'rust-cargo nil t
         "metadata" "--no-deps" "--format-version" "1"))))

(defun flycheck-rust-cargo-workspace-root ()
  "Return the path to the workspace root of a Rust Cargo project.

Return nil if the workspace root does not exist (for Rust
versions inferior to 1.25)."
  (let-alist (flycheck-rust-cargo-metadata)
    .workspace_root))

(defun flycheck-rust-cargo-has-command-p (command)
  "Whether Cargo has COMMAND in its list of commands.

Execute `cargo --list' to find out whether COMMAND is present."
  (let ((cargo (funcall flycheck-executable-find "cargo")))
    (member command
            (mapcar (lambda (line)
                      (car (split-string (string-trim line))))
                    ;; Probe on the host the check will run on (remote over
                    ;; TRAMP), where cargo was resolved.
                    (ignore-errors
                      (flycheck--process-file-lines
                       (file-local-name cargo) "--list"))))))

(defun flycheck-rust-valid-crate-type-p (crate-type)
  "Whether CRATE-TYPE is a valid target type for Cargo.

A valid Cargo target type is one of `lib', `bin', `example',
`test' or `bench'."
  (member crate-type '(nil "lib" "bin" "example" "test" "bench")))

(flycheck-define-checker rust-cargo
  "A Rust syntax checker using Cargo.

This syntax checker requires Rust 1.17 or newer.  See URL
`https://www.rust-lang.org'."
  :command ("cargo"
            (eval (if flycheck-rust-check-tests
                      "test"
                    "check"))
            (eval (when flycheck-rust-check-tests
                    "--no-run"))
            (eval (when flycheck-rust-crate-type
                    (concat "--" flycheck-rust-crate-type)))
            ;; All crate targets except "lib" need a binary name
            (eval (when (and flycheck-rust-crate-type
                             (not (string= flycheck-rust-crate-type "lib")))
                    flycheck-rust-binary-name))
            (option "--features=" flycheck-rust-features concat
                    flycheck-option-comma-separated-list)
            (eval flycheck-cargo-check-args)
            "--message-format=json")
  :error-parser flycheck-parse-cargo-rustc
  :error-filter (lambda (errors)
                  ;; In Rust 1.25+, filenames are relative to the workspace
                  ;; root.
                  (let ((root (flycheck-rust-cargo-workspace-root)))
                    (seq-do (lambda (err)
                              ;; Some errors are crate level and do not have a
                              ;; filename
                              (when (flycheck-error-filename err)
                                (setf (flycheck-error-filename err)
                                      (expand-file-name
                                       (flycheck-error-filename err) root))))
                            (flycheck-rust-error-filter errors))))
  :error-explainer flycheck-rust-error-explainer
  :modes (rust-mode rust-ts-mode)
  :predicate flycheck-buffer-saved-p
  :enabled flycheck-rust-manifest-directory
  :working-directory (lambda (_) (flycheck-rust-manifest-directory))
  :verify
  (lambda (_)
    (and buffer-file-name
         (let* ((has-toml (flycheck-rust-manifest-directory))
                (valid-crate-type (flycheck-rust-valid-crate-type-p
                                   flycheck-rust-crate-type))
                (need-binary-name
                 (and flycheck-rust-crate-type
                      (not (string= flycheck-rust-crate-type "lib")))))
           (list
            (flycheck-verification-result-new
             :label "Cargo.toml"
             :message (if has-toml "Found" "Missing")
             :face (if has-toml 'success '(bold warning)))
            (flycheck-verification-result-new
             :label "Crate type"
             :message (if valid-crate-type
                          (format "%s" flycheck-rust-crate-type)
                        (format "%s (invalid, should be one of 'lib', 'bin', \
'test', 'example' or 'bench')"
                                flycheck-rust-crate-type))
             :face (if valid-crate-type 'success '(bold error)))
            (flycheck-verification-result-new
             :label "Binary name"
             :message (cond
                       ((not need-binary-name) "Not required")
                       ((not flycheck-rust-binary-name) "Required")
                       (t (format "%s" flycheck-rust-binary-name)))
             :face (cond
                    ((not need-binary-name) 'success)
                    ((not flycheck-rust-binary-name) '(bold error))
                    (t 'success))))))))

(flycheck-define-checker rust
  "A Rust syntax checker using the Rust compiler.

This syntax checker needs Rust 1.18 or newer.  See URL
`https://www.rust-lang.org'."
  :command ("rustc"
            (option "--crate-type" flycheck-rust-crate-type)
            (option "--edition" flycheck-rust-edition)
            "--emit=metadata"
            "--out-dir" (eval (file-local-name (flycheck-temp-dir-system))) ; avoid creating binaries
            "--error-format=json"
            (option-flag "--test" flycheck-rust-check-tests)
            (option-list "-L" flycheck-rust-library-path concat)
            (eval flycheck-rust-args)
            (eval (or flycheck-rust-crate-root
                      (flycheck-substitute-argument 'source-original 'rust))))
  :error-parser flycheck-parse-rustc
  :error-filter flycheck-rust-error-filter
  :error-explainer flycheck-rust-error-explainer
  :modes (rust-mode rust-ts-mode)
  :predicate flycheck-buffer-saved-p)

(flycheck-define-checker rust-clippy
  "A Rust syntax checker using clippy.

See URL `https://github.com/rust-lang-nursery/rust-clippy'."
  :command ("cargo" "clippy"
            (option "--features=" flycheck-rust-features concat
                    flycheck-option-comma-separated-list)
            (option-flag "--tests" flycheck-rust-clippy-tests)
            (option-flag "--all-targets" flycheck-rust-clippy-all-targets)
            (option-flag "--all-features" flycheck-rust-clippy-all-features)
            "--message-format=json"
            (eval flycheck-rust-clippy-args))
  :error-parser flycheck-parse-cargo-rustc
  :error-filter flycheck-rust-error-filter
  :error-explainer flycheck-rust-error-explainer
  :modes (rust-mode rust-ts-mode)
  :predicate flycheck-buffer-saved-p
  :enabled (lambda ()
             (and (flycheck-rust-cargo-has-command-p "clippy")
                  (flycheck-rust-manifest-directory)))
  :working-directory (lambda (_) (flycheck-rust-manifest-directory))
  :verify
  (lambda (_)
    (and buffer-file-name
         (let ((has-toml (flycheck-rust-manifest-directory))
               (has-clippy (flycheck-rust-cargo-has-command-p "clippy")))
           (list
            (flycheck-verification-result-new
             :label "Clippy"
             :message (if has-clippy "Found"
                        "Cannot find the `cargo clippy' command")
             :face (if has-clippy 'success '(bold warning)))
            (flycheck-verification-result-new
             :label "Cargo.toml"
             :message (if has-toml "Found" "Missing")
             :face (if has-toml 'success '(bold warning))))))))

(flycheck-define-checker salt-lint
  "A salt linter which applies common best practices for SaltStack.

See URL `https://salt-lint.readthedocs.io/en/latest/'."
  :command ("python" "-m" "saltlint" "--json")
  :standard-input t
  :error-parser flycheck-salt-lint-parser
  :error-filter flycheck-sanitize-errors
  :modes salt-mode)

(defun flycheck-salt-lint-parser (output checker buffer)
  "Parse salt lint JSON errors from OUTPUT.

CHECKER and BUFFER are used to construct the error objects."
  (condition-case nil
      (let ((filename (buffer-file-name buffer))
            (errors (json-parse-string output
                                      :object-type 'alist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil)))
        (mapcar (lambda (e)
                  (let-alist e
                    (flycheck-error-new-at
                     .linenumber 0
                     (pcase .severity
                       ("HIGH" 'error)
                       ("MEDIUM" 'warning)
                       ("LOW" 'warning)
                       ("INFO" 'info)
                       (_ 'info))
                     (concat .message .line)
                     :id .id
                     :checker checker
                     :buffer buffer
                     :filename filename)))
                errors))
    (json-parse-error nil)))

(flycheck-define-checker scala
  "A Scala syntax checker using the Scala compiler.

See URL `https://www.scala-lang.org/'."
  :command ("scalac" "-Ystop-after:parser" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": error: " (message) line-end)
   (warning line-start (file-name) ":" line ": warning: " (message) line-end))
  :modes (scala-mode scala-ts-mode)
  :next-checkers ((warning . scala-scalastyle)))

(define-obsolete-variable-alias 'flycheck-scalastylerc
  'flycheck-scalastyle-config "39")
(flycheck-def-config-file-var flycheck-scalastyle-config scala-scalastyle nil
  :package-version '(flycheck . "0.20"))

(flycheck-def-args-var flycheck-scalastyle-args scala-scalastyle
  :package-version '(flycheck . "39"))

(flycheck-define-checker scala-scalastyle
  "A Scala style checker using scalastyle.

Note that this syntax checker is not used if
`flycheck-scalastyle-config' is nil or refers to a non-existing file.

See URL `https://www.scalastyle.org'."
  :command ("scalastyle"
            (config-file "-c" flycheck-scalastyle-config)
            (eval flycheck-scalastyle-args)
            source)
  :error-patterns
  ((error line-start "error file=" (file-name) " message="
          (message) " line=" line (optional " column=" column) line-end)
   (warning line-start "warning file=" (file-name) " message="
            (message) " line=" line (optional " column=" column) line-end))
  :error-filter (lambda (errors)
                  (flycheck-sanitize-errors
                   (flycheck-increment-error-columns errors)))
  :modes (scala-mode scala-ts-mode)
  :predicate
  ;; Inhibit this syntax checker if the JAR or the configuration are unset or
  ;; missing
  (lambda () (and flycheck-scalastyle-config
                  (flycheck-locate-config-file flycheck-scalastyle-config
                                               'scala-scalastyle)))
  :verify (lambda (checker)
            (let ((config-file (and flycheck-scalastyle-config
                                    (flycheck-locate-config-file
                                     flycheck-scalastyle-config checker))))
              (list
               (flycheck-verification-result-new
                :label "Configuration file"
                :message (cond
                          ((not flycheck-scalastyle-config)
                           "`flycheck-scalastyle-config' not set")
                          ((not config-file)
                           (format "file %s not found" flycheck-scalastyle-config))
                          (t (format "found at %s" config-file)))
                :face (cond
                       ((not flycheck-scalastyle-config) '(bold warning))
                       ((not config-file) '(bold error))
                       (t 'success)))))))

(flycheck-def-args-var flycheck-scheme-chicken-args scheme-chicken
  :package-version '(flycheck . "32"))

(flycheck-define-checker scheme-chicken
  "A CHICKEN Scheme syntax checker using the CHICKEN compiler `csc'.

See URL `https://call-cc.org/'."
  :command ("csc" "-analyze-only" "-local"
            (eval flycheck-scheme-chicken-args)
            source)
  :error-patterns
  ((info line-start
         "Note: " (zero-or-more not-newline) ":\n"
         (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
         line-end)
   (warning line-start
            "Warning: " (zero-or-more not-newline) ",\n"
            (one-or-more (any space)) (zero-or-more not-newline) ":\n"
            (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
            line-end)
   (warning line-start
            "Warning: " (zero-or-more not-newline) ":\n"
            (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
            line-end)
   (error line-start
          "Error: Module `" (one-or-more not-newline) "' has unresolved identifiers\n"
          (zero-or-more space) "In file `" (file-name) "':"
          line-end)
   (error line-start
          (zero-or-more space) (message) "\n" (zero-or-more space) "On line " line
          line-end)
   (error line-start "Error: (line " line ") " (message) line-end)
   (error line-start "Syntax error: (" (file-name) ":" line ")"
          (zero-or-more not-newline) " - "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (zero-or-more space)
                                 (zero-or-more not-newline))
                   (one-or-more space) "<--")
          line-end)
   ;; As of version 4.12.0, the chicken compiler doesn't provide a
   ;; line number for this error.
   (error line-start "Syntax error: "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (zero-or-more space)
                                 (zero-or-more not-newline))
                   (one-or-more space) "<--")
          line-end)
   (error line-start
          "Error: " (zero-or-more not-newline) ":\n"
          (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
          line-end)
   ;; As of version 4.12.0, the chicken compiler doesn't provide a
   ;; line number for this error.
   (error line-start "Error: "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (zero-or-more space)
                                 (zero-or-more not-newline))
                   (one-or-more space) "<--")))
  :error-filter flycheck-fill-empty-line-numbers
  :predicate
  (lambda ()
    ;; In `scheme-mode' we must check the current Scheme implementation
    ;; being used
    (and (boundp 'geiser-impl--implementation)
         (eq geiser-impl--implementation 'chicken)))
  :verify
  (lambda (_checker)
    (let ((geiser-impl (bound-and-true-p geiser-impl--implementation)))
      (list
       (flycheck-verification-result-new
        :label "Geiser Implementation"
        :message (cond
                  ((eq geiser-impl 'chicken) "Chicken Scheme")
                  (geiser-impl (format "Other: %s" geiser-impl))
                  (t "Geiser not active"))
        :face (cond
               ((eq geiser-impl 'chicken) 'success)
               (t '(bold error)))))))
  :modes scheme-mode)

(flycheck-define-checker scss-stylelint
  "A SCSS syntax and style checker using stylelint.

See URL `https://stylelint.io/'."
  :command ("stylelint"
            "--formatter" "json"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelint-config))
  :standard-input t
  :verify (lambda (_) (flycheck--stylelint-verify 'scss-stylelint))
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :error-explainer
  (flycheck-error-explainer-from-url "https://stylelint.io/user-guide/rules/%s")
  :handle-suspicious flycheck--stylelint-handle-suspicious
  :modes (scss-mode))

(flycheck-define-checker sass-stylelint
  "A Sass syntax and style checker using stylelint.

See URL `https://stylelint.io/'."
  :command ("stylelint"
            "--formatter" "json"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelint-config))
  :standard-input t
  :verify (lambda (_) (flycheck--stylelint-verify 'sass-stylelint))
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :error-explainer
  (flycheck-error-explainer-from-url "https://stylelint.io/user-guide/rules/%s")
  :handle-suspicious flycheck--stylelint-handle-suspicious
  :modes (sass-mode))

(flycheck-def-args-var flycheck-sh-bash-args (sh-bash)
  :package-version '(flycheck . "32"))

(flycheck-define-checker sh-bash
  "A Bash syntax checker using the Bash shell.

See URL `https://www.gnu.org/software/bash/'."
  :command ("bash" "--norc" "-n"
            (eval flycheck-sh-bash-args)
            "--")
  :standard-input t
  :error-patterns
  ((error line-start
          ;; The name/path of the bash executable
          (one-or-more (not (any ":"))) ":"
          ;; A label "line", possibly localized
          (one-or-more (not (any digit)))
          line (zero-or-more " ") ":" (zero-or-more " ")
          (message) line-end))
  :modes (sh-mode bash-ts-mode)
  :predicate (lambda () (eq sh-shell 'bash))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-posix-dash
  "A POSIX Shell syntax checker using the Dash shell.

See URL `https://gondor.apana.org.au/~herbert/dash/'."
  :command ("dash" "-n")
  :standard-input t
  :error-patterns
  ((error line-start (one-or-more (not (any ":"))) ": " line ": " (message)))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-posix-bash
  "A POSIX Shell syntax checker using the Bash shell.

See URL `https://www.gnu.org/software/bash/'."
  :command ("bash" "--posix" "--norc" "-n" "--")
  :standard-input t
  :error-patterns
  ((error line-start
          ;; The name/path of the bash executable
          (one-or-more (not (any ":"))) ":"
          ;; A label "line", possibly localized
          (one-or-more (not (any digit)))
          line (zero-or-more " ") ":" (zero-or-more " ")
          (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-zsh
  "A Zsh syntax checker using the Zsh shell.

See URL `https://www.zsh.org/'."
  :command ("zsh" "--no-exec" "--no-globalrcs" "--no-rcs" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'zsh))
  :next-checkers ((warning . sh-shellcheck)))

(defconst flycheck-shellcheck-supported-shells '(bash ksh88 sh)
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

(flycheck-def-option-var flycheck-shellcheck-follow-sources t sh-shellcheck
  "Whether to follow external sourced files in scripts.

Shellcheck will follow and parse sourced files so long as a
pre-runtime resolvable path to the file is present.  This can
either be part of the source command itself:
   source /full/path/to/file.txt
or added as a shellcheck directive before the source command:
   # shellcheck source=/full/path/to/file.txt."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "31"))

(flycheck-def-option-var flycheck-shellcheck-infer-shell nil sh-shellcheck
  "Whether to let ShellCheck infer the shell from the script.

When non-nil, the --shell flag is not passed to ShellCheck,
allowing it to infer the shell from the shebang line or
shellcheck directives in the script."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "36"))

(flycheck-def-option-var flycheck-shellcheck-enabled-checks nil sh-shellcheck
  "A list of enabled optional checks for ShellCheck.

The value of this variable is a list of strings, where each
string is the name of an optional check to enable.  Use \"all\"
to enable all optional checks.

See the ShellCheck man page for a list of available optional checks."
  :type '(repeat :tag "Enabled checks"
                 (string :tag "Check name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "36"))

(flycheck-def-args-var flycheck-shellcheck-args sh-shellcheck
  :package-version '(flycheck . "36"))

(defun flycheck-parse-shellcheck--fix (fix buffer)
  "Build a `flycheck-fix' for BUFFER from a shellcheck FIX object, or nil.

A shellcheck fix carries `replacements', each replacing the region
from its `line', `column' to its `endLine', `endColumn' (one-based
character positions, as `flycheck-error' uses) with `replacement'."
  (when fix
    (let-alist fix
      (flycheck--make-fix
       buffer nil
       (seq-map
        (lambda (replacement)
          (let-alist replacement
            (flycheck-fix-edit-new
             :line .line :column .column
             :end-line .endLine :end-column .endColumn
             :replacement .replacement)))
        .replacements)))))

(defun flycheck-parse-shellcheck (output checker buffer)
  "Parse shellcheck JSON1 OUTPUT into Flycheck errors.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/koalaman/shellcheck/' for more
information about shellcheck."
  (seq-map
   (lambda (comment)
     (let-alist comment
       (flycheck-error-new-at
        .line .column
        (pcase .level
          ("error" 'error)
          ("warning" 'warning)
          ;; shellcheck's \"style\" level maps to info, as it did through the
          ;; CheckStyle format Flycheck used before.
          (_ 'info))
        .message
        :id (format "SC%s" .code)
        :checker checker
        :buffer buffer
        :filename (unless (equal .file "-") .file)
        :fix (flycheck-parse-shellcheck--fix .fix buffer))))
   (let-alist (car (flycheck-parse-json output)) .comments)))

(flycheck-define-checker sh-shellcheck
  "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :command ("shellcheck"
            ;; JSON1 carries shellcheck's fix replacements (see
            ;; `flycheck-parse-shellcheck'); it needs shellcheck >= 0.7.
            "--format" "json1"
            (eval
             (unless flycheck-shellcheck-infer-shell
               (list "--shell" (symbol-name sh-shell))))
            (option-flag "--external-sources"
                         flycheck-shellcheck-follow-sources)
            (option "--exclude" flycheck-shellcheck-excluded-warnings list
                    flycheck-option-comma-separated-list)
            (option "--enable" flycheck-shellcheck-enabled-checks list
                    flycheck-option-comma-separated-list)
            (eval flycheck-shellcheck-args)
            "-")
  :standard-input t
  :error-parser flycheck-parse-shellcheck
  :handle-suspicious flycheck--shellcheck-handle-suspicious
  :modes (sh-mode bash-ts-mode)
  :predicate (lambda () (memq sh-shell flycheck-shellcheck-supported-shells))
  :verify (lambda (_)
            (let ((supports-shell (memq sh-shell
                                        flycheck-shellcheck-supported-shells)))
              (list
               (flycheck-verification-result-new
                :label (format "Shell %s supported" sh-shell)
                :message (if supports-shell "yes" "no")
                :face (if supports-shell 'success '(bold warning))))))
  :error-explainer
  (flycheck-error-explainer-from-url "https://github.com/koalaman/shellcheck/wiki/%s"))

(flycheck-define-checker slim
  "A Slim syntax checker using the Slim compiler.

See URL `https://slim-lang.com'."
  :command ("slimrb" "--compile")
  :standard-input t
  :error-patterns
  ((error line-start
          "Slim::Parser::SyntaxError:" (message) (optional "\r") "\n  "
          "STDIN, Line " line (optional ", Column " column)
          line-end))
  :modes slim-mode
  :next-checkers ((warning . slim-lint)))

(flycheck-define-checker slim-lint
  "A Slim linter.

See URL `https://github.com/sds/slim-lint'."
  :command ("slim-lint" "--reporter=checkstyle" source)
  :error-parser flycheck-parse-checkstyle
  :modes slim-mode)

(flycheck-def-args-var flycheck-swift-args swift
  :package-version '(flycheck . "39"))

(flycheck-define-checker swift
  "A Swift syntax checker using the Swift compiler.

Runs `swiftc -parse', which parses the file without type-checking
it.  That is a deliberate limit: type-checking a single file means
telling the compiler about the rest of the module, the SDK and the
target, and getting any of it wrong reports things like `cannot
find X in scope' for code that builds perfectly well.  Parsing
needs none of that and is right for any file, in a package or not.

For type errors, run sourcekit-lsp through
`global-flycheck-eglot-mode' or `flycheck-lsp-mode'; a language
server knows how the project is built and Flycheck does not have
to guess.

See URL `https://www.swift.org/'."
  :command ("swiftc" "-parse" "-diagnostic-style" "llvm"
            (eval flycheck-swift-args)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": "
          "error: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": "
            "warning: " (message) line-end)
   (info line-start (file-name) ":" line ":" column ": "
         "note: " (message) line-end))
  :modes swift-mode)

(flycheck-define-checker sql-sqlint
  "A SQL syntax checker using the sqlint tool.

See URL `https://github.com/purcell/sqlint'."
  :command ("sqlint")
  :standard-input t
  :error-patterns
  ((warning line-start "stdin:" line ":" column ":WARNING "
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more "  ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start "stdin:" line ":" column ":ERROR "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more "  ")
                                 (one-or-more not-newline)))
          line-end))
  :modes (sql-mode))

(flycheck-define-checker systemd-analyze
  "A systemd unit checker using systemd-analyze(1).

See URL
`https://www.freedesktop.org/software/systemd/man/systemd-analyze.html'."
  :command ("systemd-analyze" "verify" source)
  :error-parser flycheck-parse-with-patterns-without-color
  :error-patterns
  ((error line-start (file-name) ":" (optional line ":") (message) line-end)
   (error line-start "[" (file-name) ":" line "]" (message) line-end))
  :error-filter (lambda (errors)
                  (flycheck-sanitize-errors
                   (flycheck-fill-empty-line-numbers errors)))
  :modes (systemd-mode))

(define-obsolete-variable-alias 'flycheck-chktexrc
  'flycheck-chktex-config "39")
(flycheck-def-config-file-var flycheck-chktex-config tex-chktex ".chktexrc")

(flycheck-def-option-var flycheck-tcl-nagelfar-syntax-databases nil tcl-nagelfar
  "A list of syntax database files for Nagelfar, passed with `-s'.

Nagelfar only knows the commands in its databases, so in a project whose
procedures live across several files it reports the ones it has not seen
as unknown commands.  Running Nagelfar with `-header' over the project
writes a database describing them; listing it here quiets those reports
when checking a single file.

Relative paths are relative to the file being checked."
  :type '(repeat (file :tag "Database file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-args-var flycheck-tcl-nagelfar-args tcl-nagelfar
  :package-version '(flycheck . "39"))

(flycheck-define-checker tcl-nagelfar
  "A Tcl syntax checker using Nagelfar.

See URL `https://nagelfar.sourceforge.net/'."
  :command ("nagelfar" "-H"
            (option-list "-s" flycheck-tcl-nagelfar-syntax-databases)
            (eval flycheck-tcl-nagelfar-args)
            source)
  :error-patterns
  ;; foo.tcl: 29: E Wrong number of arguments (4) to "set"
  ;; foo.tcl: 29: W Expr without braces
  ((info    line-start (file-name) ": " line ": N " (message) line-end)
   (warning line-start (file-name) ": " line ": W " (message) line-end)
   (error   line-start (file-name) ": " line ": E " (message) line-end))
  :modes tcl-mode)

(flycheck-define-checker terraform
  "A Terraform syntax checker with `terraform fmt'.

See URL `https://www.terraform.io/docs/commands/fmt.html'."
  :command ("terraform" "fmt" "-no-color" "-")
  :standard-input t
  :error-patterns
  ((error line-start "Error: " (one-or-more not-newline)
          "\n\n  on <stdin> line " line ", in " (one-or-more not-newline) ":"
          (one-or-more "\n" (zero-or-more space (one-or-more not-newline)))
          (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
          line-end)
   (error line-start "Error: " (one-or-more not-newline)
          "\n\n  on <stdin> line " line ":\n  (source code not available)\n\n"
          (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
          line-end))
  :next-checkers ((warning . terraform-tflint))
  :modes (terraform-mode terraform-ts-mode))

(flycheck-def-option-var flycheck-tflint-variable-files nil terraform-tflint
  "A list of files to resolve terraform variables.

The value of this variable is a list of strings, where each
string is a file to add to the terraform variables files.
Relative files are relative to the file being checked."
  :type '(repeat (file :tag "Variable file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-config-file-var flycheck-tflint-config terraform-tflint
                              '(".tflint.hcl"))

(flycheck-def-args-var flycheck-tflint-args terraform-tflint
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-tflint-enabled-rules nil terraform-tflint
  "A list of tflint rules to enable, via `--enable-rule'.

Each element is a rule name, such as \"terraform_unused_declarations\"."
  :type '(repeat (string :tag "Rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(flycheck-def-option-var flycheck-tflint-disabled-rules nil terraform-tflint
  "A list of tflint rules to disable, via `--disable-rule'.

Each element is a rule name, such as \"terraform_deprecated_syntax\"."
  :type '(repeat (string :tag "Rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "39"))

(defun flycheck-parse-tflint-linter (output checker buffer)
  "Parse tflint warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/terraform-linters/tflint' for more
information about tflint."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .range.start.line
               .range.start.column
               (pcase .rule.severity
                 ("error"   'error)
                 ("warning" 'warning)
                 (_         'error))
               .message
               :end-line .range.end.line
               :end-column .range.end.column
               :id .rule.name
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer))))
          (cdr (assq 'issues (car (flycheck-parse-json output))))))

(flycheck-define-checker terraform-tflint
  "A Terraform checker using tflint.

See URL `https://github.com/terraform-linters/tflint'."
  :command ("tflint" "--format=json" "--force"
            (config-file "--config" flycheck-tflint-config)
            (option-list "--enable-rule=" flycheck-tflint-enabled-rules concat)
            (option-list "--disable-rule=" flycheck-tflint-disabled-rules concat)
            (option-list "--var-file=" flycheck-tflint-variable-files concat)
            (eval flycheck-tflint-args))
  :error-parser flycheck-parse-tflint-linter
  :predicate flycheck-buffer-saved-p
  :modes (terraform-mode terraform-ts-mode))

(flycheck-def-option-var flycheck-chktex-extra-flags nil tex-chktex
  "A list of extra arguments to give to chktex.
This variable works the same way as `tex-chktex-extra-flags': its value
is a list of strings, where each string is an argument added to chktex.

For example, to ignore warnings 8 and 18, you would set this option to

  \\='(\"-n8\" \"-n18\")."
  :type '(repeat string)
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "35"))

(flycheck-define-checker tex-chktex
  "A TeX and LaTeX syntax and style checker using chktex.

See URL `https://www.nongnu.org/chktex/'."
  :command ("chktex"
            (config-file "--localrc" flycheck-chktex-config)
            (option-list "" flycheck-chktex-extra-flags concat)
            ;; Compact error messages, and no version information, and execute
            ;; \input statements
            "--verbosity=0" "--quiet" "--inputfiles")
  :standard-input t
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (id (one-or-more digit)) ":" (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names
      "stdin" (flycheck-increment-error-columns errors))))
  :modes (latex-mode LaTeX-mode plain-tex-mode plain-TeX-mode))

(flycheck-define-checker tex-lacheck
  "A LaTeX syntax and style checker using lacheck.

See URL `https://www.ctan.org/pkg/lacheck'."
  :command ("lacheck" source-inplace)
  :error-patterns
  ((warning line-start
            "\"" (file-name) "\", line " line ": " (message)
            line-end))
  :modes (latex-mode LaTeX-mode))

(flycheck-define-checker texinfo
  "A Texinfo syntax checker using makeinfo.

See URL `https://www.gnu.org/software/texinfo/'."
  :command ("makeinfo" "-o" null-device "-")
  :standard-input t
  :error-patterns
  ((warning line-start
            "-:" line (optional ":" column) ": " "warning: " (message)
            line-end)
   (error line-start
          "-:" line (optional ":" column) ": " (message)
          line-end))
  :modes (texinfo-mode Texinfo-mode))

(flycheck-def-config-file-var flycheck-textlint-config
    textlint "textlintrc.json")

;; This needs to be set because textlint plugins are installed separately,
;; and there is no way to check their installation status -- textlint simply
;; prints a backtrace.
(flycheck-def-option-var flycheck-textlint-plugin-alist
    '((markdown-mode . "@textlint/markdown")
      (gfm-mode . "@textlint/markdown")
      (t . "@textlint/text"))
    textlint
  "An alist mapping major modes to textlint plugins.

Each item is a cons cell `(MAJOR-MODE . PLUGIN)', where MAJOR-MODE is a mode
`flycheck-textlint' supports and PLUGIN is a textlint plugin. As a catch-all,
when MAJOR-MODE is t, that PLUGIN will be used for any supported mode that
isn't specified.

See URL `https://npms.io/search?q=textlint-plugin' for all textlint plugins
published on NPM."
  :type '(repeat (choice (cons symbol string)
                         (cons (const t) string))))

(defun flycheck--textlint-get-plugin ()
  "Return the textlint plugin for the current mode."
  (cdr (seq-find
        (lambda (arg)
          (pcase-let ((`(,mode . _) arg))
            (or (and (booleanp mode) mode) ; mode is t
                (derived-mode-p mode))))
        flycheck-textlint-plugin-alist)))

(flycheck-def-args-var flycheck-textlint-args textlint
  :package-version '(flycheck . "39"))

(flycheck-define-checker textlint
  "A text prose linter using textlint.

See URL `https://textlint.github.io/'."
  :command ("textlint"
            (config-file "--config" flycheck-textlint-config)
            "--format" "json"
            ;; get the first matching plugin from plugin-alist
            "--plugin"
            (eval (flycheck--textlint-get-plugin))
            (eval flycheck-textlint-args)
            source)
  ;; textlint seems to say that its json output is compatible with ESLint.
  ;; https://textlint.github.io/docs/formatter.html
  :error-parser flycheck-parse-eslint
  ;; textlint can support different formats with textlint plugins, but
  ;; only text and markdown formats are installed by default. Ask the
  ;; user to add mode->plugin mappings manually in
  ;; `flycheck-textlint-plugin-alist'.
  :modes
  (text-mode markdown-mode gfm-mode message-mode adoc-mode asciidoc-mode
             mhtml-mode latex-mode LaTeX-mode org-mode rst-mode)
  :enabled
  (lambda () (flycheck--textlint-get-plugin))
  :verify
  (lambda (_)
    (let ((plugin (flycheck--textlint-get-plugin)))
      (list
       (flycheck-verification-result-new
        :label "textlint plugin"
        :message plugin
        :face 'success)))))

(flycheck-def-option-var flycheck-verilator-include-path nil verilog-verilator
  "A list of include directories for Verilator.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Verilator.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker verilog-verilator
  "A Verilog syntax checker using the Verilator Verilog HDL simulator.

See URL `https://www.veripool.org/wiki/verilator'."
  :command ("verilator" "--lint-only" "-Wall" "--quiet-exit"
            (option-list "-I" flycheck-verilator-include-path concat)
            source)
  :error-patterns
  ((warning line-start "%Warning"
            (? "-" (id (+ (any "0-9A-Z_")))) ": "
            (? (file-name) ":" line ":" (? column ":") " ")
            (message) line-end)
   (error line-start "%Error"
          (? "-" (id (+ (any "0-9A-Z_")))) ": "
          (? (file-name) ":" line ":" (? column ":") " ")
          (message) line-end))
  :modes verilog-mode)

(flycheck-def-option-var flycheck-ghdl-language-standard nil vhdl-ghdl
  "The language standard to use in GHDL.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `--std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-ghdl-language-standard)

(flycheck-def-option-var flycheck-ghdl-workdir nil vhdl-ghdl
  "The directory to use for the file library.

The value of this variable is either a string with the directory
to use for the file library, or nil, to use the default value.
When non-nil, pass the directory via the `--workdir' option."
  :type '(choice (const :tag "Default directory" nil)
                 (string :tag "Directory for the file library"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-ghdl-workdir)

(flycheck-def-option-var flycheck-ghdl-ieee-library nil vhdl-ghdl
  "The standard to use for the IEEE library.

The value of this variable is either a string denoting an ieee library
standard, or nil, to use the default standard.  When non-nil,
pass the ieee library standard via the `--ieee' option."
  :type '(choice (const :tag "Default standard" nil)
                 (const :tag "No IEEE Library" "none")
                 (const :tag "IEEE standard" "standard")
                 (const :tag "Synopsys standard" "synopsys")
                 (const :tag "Mentor standard" "mentor"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-ghdl-ieee-library)

(flycheck-define-checker vhdl-ghdl
  "A VHDL syntax checker using GHDL.

See URL `https://github.com/ghdl/ghdl'."
  :command ("ghdl"
            "-s" ; only do the syntax checking
            (option "--std=" flycheck-ghdl-language-standard concat)
            (option "--workdir=" flycheck-ghdl-workdir concat)
            (option "--ieee=" flycheck-ghdl-ieee-library concat)
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ":error: " (message) line-end))
  :modes vhdl-mode)

(flycheck-def-option-var flycheck-xml-xmllint-xsd-path nil xml-xmllint
  "An XSD schema to validate against."
  :type '(choice (const :tag "None" nil)
                 (file :tag "XSD schema"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "31"))

(flycheck-def-option-var flycheck-xml-xmllint-relaxng-path nil xml-xmllint
  "A RELAX NG schema to validate against."
  :type '(choice (const :tag "None" nil)
                 (file :tag "RELAX NG schema"))
  :safe #'string-or-null-p
  :package-version '(flycheck . "34"))

(flycheck-define-checker xml-xmllint
  "An XML syntax checker and validator using the xmllint utility.

The xmllint is part of libxml2, see URL
`https://gitlab.gnome.org/GNOME/libxml2/-/wikis/home'."
  :command ("xmllint" "--noout"
            (option "--schema" flycheck-xml-xmllint-xsd-path)
            (option "--relaxng" flycheck-xml-xmllint-relaxng-path)
            "-")
  :standard-input t
  :error-patterns
  ((warning line-start "-:" line ": "
            (message (one-or-more (not (any ":"))) "warning : "
                     (one-or-more not-newline))
            line-end)
   (error line-start "-:" line ": " (message) line-end))
  :modes (xml-mode nxml-mode))

(flycheck-def-args-var flycheck-yaml-jsyaml-args yaml-jsyaml
  :package-version '(flycheck . "39"))

(flycheck-define-checker yaml-jsyaml
  "A YAML syntax checker using JS-YAML.

See URL `https://github.com/nodeca/js-yaml'."
  :command ("js-yaml" (eval flycheck-yaml-jsyaml-args))
  :standard-input t
  :error-patterns
  ((error line-start
          (or "JS-YAML" "YAMLException") ": "
          (message) " at line " line ", column " column ":"
          line-end)
   (error line-start
          (or "JS-YAML" "YAMLException") ": "
          (message) " (" line ":" column ")"
          line-end))
  :modes (yaml-mode yaml-ts-mode)
  :next-checkers ((warning . yaml-yamllint)
                  (warning . cwl)))

(define-obsolete-variable-alias 'flycheck-yamllintrc
  'flycheck-yamllint-config "39")
(flycheck-def-config-file-var flycheck-yamllint-config
    yaml-yamllint
    '(".yamllint"
      ".yamllint.yaml"
      ".yamllint.yml"
      "~/.config/yamllint/config"))

(flycheck-def-args-var flycheck-yamllint-args yaml-yamllint
  :package-version '(flycheck . "39"))

(flycheck-define-checker yaml-yamllint
  "A YAML syntax checker using YAMLLint.
See URL `https://github.com/adrienverge/yamllint'."
  :standard-input t
  :command ("yamllint" "-f" "parsable"
            (config-file "-c" flycheck-yamllint-config)
            (eval flycheck-yamllint-args)
            "-")
  :error-patterns
  ((error line-start
          "stdin:" line ":" column ": [error] "
          (message (minimal-match (one-or-more not-newline)))
          " (" (id (one-or-more (not (any ")")))) ")" line-end)
   (error line-start
          "stdin:" line ":" column ": [error] "
          (message) line-end)
   (warning line-start
            "stdin:" line ":" column ": [warning] "
            (message (minimal-match (one-or-more not-newline)))
            " (" (id (one-or-more (not (any ")")))) ")" line-end)
   (warning line-start
            "stdin:" line ":" column ": [warning] "
            (message) line-end))
  :modes (yaml-mode yaml-ts-mode)
  :next-checkers ((warning . cwl)))

(provide 'flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck.el ends here
