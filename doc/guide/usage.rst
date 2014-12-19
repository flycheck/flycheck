.. _usage:

=======
 Usage
=======

This chapter explains in-depth how to use Flycheck for your daily work.

.. contents:: Contents
   :local:

.. note::

   All commands in this chapter are documented with their standard key prefix
   :kbd:`C-c !`.  If you do not like this prefix, you can change it with
   :option:`flycheck-keymap-prefix`, but take care to remember your custom
   prefix while reading this chapter.

   .. option:: flycheck-keymap-prefix
      :auto:

.. require:: flycheck

.. _enabling-syntax-checking:

Enabling syntax checking
========================

:command:`global-flycheck-mode` enables syntax checking in all buffers whenever
possible:

.. command:: global-flycheck-mode

   Toggle Flycheck Mode for **all** live buffers, and for new buffers.

   With Global Flycheck Mode, Flycheck Mode is automatically enabled in all
   buffers, for which a :term:`suitable syntax checker` exists and is enabled.

   .. note::

      Flycheck Mode will **not** be enabled in buffers for remote or encrypted
      files.  The former is flaky and might be very slow, and the latter might
      leak confidential data to temporary directories.

      You can still explicitly enable Flycheck Mode in such buffers with
      :command:`flycheck-mode`.  This is **not** recommended though.

   .. option:: global-flycheck-mode

      Whether Flycheck Mode is enabled globally.

To permanently enable syntax checking, either customize
:option:`global-flycheck-mode` with :kbd:`M-x customize-variable RET
global-flycheck-mode` and select :guilabel:`Save for Future Sessions`, or add
the following code to your init file:

.. code-block:: cl

   (add-hook 'after-init-hook #'global-flycheck-mode)

You can also explicitly enable syntax checking just for the current buffer with
the local minor mode :command:`flycheck-mode`:

.. command:: flycheck-mode

   Toggle Flycheck Mode for the current buffer.

   .. option:: flycheck-mode

      Whether Flycheck Mode is enabled in the current buffer.

.. _checking-buffers:

Checking buffers
================

When :command:`flycheck-mode` is enabled, Flycheck automatically checks a buffer
whenever

- the buffer is saved (e.g. :kbd:`C-x C-s`),
- new lines are inserted,
- or a short time (see :option:`flycheck-idle-change-delay`) after the last
  change to the buffer.

You can customize this behaviour with the option
:option:`flycheck-check-syntax-automatically`:

.. option:: flycheck-check-syntax-automatically
   :auto:

.. option:: flycheck-idle-change-delay
   :auto:

You can also always check the current buffer manually:

.. command:: flycheck-buffer
   :binding: C-c ! c
   :auto:

.. note::

   If syntax checking does not work, check your setup:

   .. command:: flycheck-verify-setup
      :binding: C-c ! v
      :auto:

During syntax checks, Flycheck generates some temporary files for syntax checker
input and output.  Use :option:`flycheck-temp-prefix` to change the prefix of
these temporary files:

.. option:: flycheck-temp-prefix
   :auto:

.. _selecting-syntax-checkers:

Selecting syntax checkers
=========================

Whenever it checks a buffer, Flycheck selects a :term:`suitable syntax checker`
from :option:`flycheck-checkers`:

.. option:: flycheck-checkers
   :auto:

   An item in this list is a :term:`registered syntax checker`.

To disable a :term:`registered syntax checker`, add it to
:option:`flycheck-disabled-checkers`:

.. option:: flycheck-disabled-checkers
   :auto:

A syntax checker in :option:`flycheck-checkers` that is **not** in
:option:`flycheck-disabled-checkers` is an :term:`enabled syntax checker`.

Flycheck starts to check the current buffer with the first enabled and suitable
syntax checker from :option:`flycheck-checkers`.  See `Languages and syntax
checkers` for a list of all available syntax checkers.  If there is no enabled
and suitable checker for the current, Flycheck does not check this buffer.  It
does **not** signal an error.  Instead a special mode line indicator informs
about this state.  See :ref:`mode-line-reporting` for details.

You can also force Flycheck to use a specific syntax checker for the current
buffer with :command:`flycheck-select-checker`:

.. command:: flycheck-select-checker
   :binding: C-c ! s

   Select the syntax checker for the current buffer by setting
   :variable:`flycheck-checker`, and run a syntax check with the new syntax
   checker.

   Prompt for a syntax checker and set :variable:`flycheck-checker`.

   Any :term:`syntax checker` can be selected with this command, regardless of
   whether it is enabled.

.. command:: flycheck-select-checker
   :prefix-arg: C-u
   :binding: C-c ! s
   :noindex:

   Deselect the current syntax checker, and run a syntax check with an
   automatically selected syntax checker.

   Set :variable:`flycheck-checker` to `nil`.

.. function:: flycheck-select-checker
   :noindex:
   :auto:

You can change the completion system used by :command:`flycheck-select-checker`:

.. option:: flycheck-completion-system
   :auto:

:command:`flycheck-select-checker` sets the local variable
:variable:`flycheck-checker` for the current buffer.  You can also set this
variable explicitly, via :infonode:`(emacs)File Variables` or
:infonode:`(emacs)Directory Variables`, to enforce a specific syntax checker per
file or per directory:

.. variable:: flycheck-checker
   :auto:

Like everything else in Emacs, a syntax checker has online documentation, which
you can via with :command:`flycheck-describe-checker`:

.. command:: flycheck-describe-checker
   :binding: C-c ! ?

   Show the documentation of a syntax checker.

.. _configuring-syntax-checkers:

Configuring syntax checkers
===========================

.. _syntax-checker-executables:

Syntax checker executables
--------------------------

For each syntax checker, there is a buffer-local, customizable variable
:varcode:`flycheck-{checker}-executable`, where :var:`checker` is the name of
the syntax checker.

The value of this variable is either nil, or a string.  In the former case,
Flycheck uses the default executable from the syntax checker definition when
executing the syntax checker.  In the latter case, it uses the value of the
variable as executable.

Use these variables to override the executable from the definition per buffer.
For instance, you could use a different Emacs version with the `emacs-lisp` or
`emacs-lisp-checkdoc`.

You can either set these variables directly in your :file:`init.el`, or change
them interactively:

.. command:: flycheck-set-checker-executable
   :binding: C-c ! e

   Set the executable of a syntax checker in the current buffer.

   Prompt for a syntax checker and an executable file, and set the
   executable variable of the syntax checker.

.. command:: flycheck-set-checker-executable
   :prefix-arg: C-u
   :binding: C-c ! e
   :noindex:

   Reset the executable of a syntax checker in the current buffer.

   Prompt for a syntax checker and reset its executable to the default.

.. _syntax-checker-options:

Syntax checker options
----------------------

Some syntax checkers can be configured via options.  See :doc:`languages` for a
complete list of options for each syntax checkers.

All options are customizable via :kbd:`M-x customize-group RET
flycheck-options`, and automatically buffer-local to easily set them in hooks.

Options are mainly intended to be used by :ref:`extensions
<3rd-party-extensions>`, and via File or Directory Local variables.  See
:infonode:`(emacs)File Variables` and :infonode:`(emacs)Directory Variables`
respectively.

.. _syntax-checker-configuration-files:

Syntax checker configuration files
----------------------------------

Some syntax checkers also read configuration files, denoted by associated
*configuration file variables*.  See :doc:`languages` of these variables.

All options are customizable via :kbd:`M-x customize-group RET
flycheck-config-files`, and automatically buffer-local to easily set them in
hooks.  You may also set them via File or Directory Local variables.  See
:infonode:`(emacs)File Variables` and :infonode:`(emacs)Directory Variables`
respectively.

When set to a string, Flycheck tries to locate the configuration file using the
functions in :option:`flycheck-locate-config-file-functions` and passes the name
of the file to the syntax checker:

.. option:: flycheck-locate-config-file-functions
   :auto:

With the default value of this variable, configuration files are located by the
following algorithm:

1. If the configuration file variable contains a path a directory
   separator, expand the path against the buffer's default directory and
   use the resulting path as configuration file.
2. If the buffer has a file name, search the buffer's directory and any
   ancestors thereof for the configuration file.
3. Eventually attempt to locate the configuration file in the user's home
   directory.

If any of these steps succeeds, the subsequent steps are not executed.

.. _error-reporting:

Error reporting
===============

When a syntax check in the current buffer has finished, Flycheck highlights the
locations of errors and warnings in the buffer according to
:option:`flycheck-highlighting-mode`, and indicates these locations in the
fringe according to :option:`flycheck-indication-mode`.  Additionally it shows
the number of errors and warnings in the mode line.

.. note::

   To avoid flooding the buffer with excessive errors, Flycheck discards errors
   and warnings and **disables** the corresponding syntax checker subsequently,
   if the total number of reported errors of any level exceeds
   :option:`flycheck-checker-error-threshold`:

   .. option:: flycheck-checker-error-threshold
      :auto:

.. option:: flycheck-highlighting-mode
   :auto:

.. face:: flycheck-error
          flycheck-warning
          flycheck-info

   The faces to use to highlight errors, warnings and info messages
   respectively.

   .. note::

      The default faces provided by GNU Emacs are ill-suited to highlight errors
      because these are relatively pale and do not specify a background color or
      underline.  Hence highlights are easy to overlook and even **invisible**
      for white space.

   For best error highlighting customize these faces, or choose a color theme
   that has reasonable Flycheck faces.  The popular Solarized_ and Zenburn_
   themes are known to have good Flycheck faces.

.. option:: flycheck-indication-mode
   :auto:

.. face:: flycheck-fringe-error
          flycheck-fringe-warning
          flycheck-fringe-info

   The faces of fringe indicators for errors, warnings and info messages
   respectively.

If you hover a highlighted error with the mouse, a tooltip with the top-most
error message is shown.  Alternatively, you can move the point onto an error
location to see the error message.  Flycheck displays errors at point after a
short delay:

.. option:: flycheck-display-errors-delay
   :auto:

By default, Flycheck shows the messages and IDs of the errors at point in the
minibuffer, but this behaviour is entirely customizable via the
:option:`flycheck-display-errors-function` option:

.. option:: flycheck-display-errors-function
   :auto:

   Flycheck provides two built-in functions for this option:

   .. function:: flycheck-display-error-messages
      :auto:

   .. function:: flycheck-display-error-messages-unless-error-list
      :auto:

      .. seealso:: :ref:`listing-errors`

   .. seealso::

      The `flycheck-pos-tip`_ extension provides a display function to show
      errors at point in a graphical popup.

      .. _flycheck-pos-tip: https://github.com/flycheck/flycheck-pos-tip

You can clear all errors in the current buffer with :command:`flycheck-clear`:

.. command:: flycheck-clear
   :binding: C-c ! C

   Clear all Flycheck errors and warnings in the current buffer.

   You should not normally need this command, because Flycheck checks the buffer
   periodically anyway.

.. _Solarized: https://github.com/bbatsov/solarized-emacs
.. _Zenburn: https://github.com/bbatsov/zenburn-emacs

.. _listing-errors:

Listing errors
==============

To view all errors in the current buffer, pop up the error list with
:command:`flycheck-list-errors`:

.. command:: flycheck-list-errors list-flycheck-errors
   :binding: C-c ! l

   List all errors in the current buffer in a separate buffer.

   The error list automatically refreshes after a syntax check, and follows the
   current buffer and window, that is, if you switch to another buffer or
   window, the error list is updated to show the errors of the new buffer or
   window.

Every time the error list refreshes,
:hook:`flycheck-error-list-after-refresh-hook` is run:

.. hook:: flycheck-error-list-after-refresh-hook
   :auto:

When you move the point in the current buffer while the error list is visible,
all errors on the current line are highlighted in the error list with
:face:`flycheck-error-list-highlight`:

.. face:: flycheck-error-list-highlight
   :auto:

You can customize the appearance of the line and column numbers and of the
syntax checker name:

.. face:: flycheck-error-list-line-number
   :auto:

.. face:: flycheck-error-list-column-number
   :auto:

.. face:: flycheck-error-list-checker-name
   :auto:

.. _killing-errors:

Copying (killing) errors
========================

Frequently, it's convenient to not only see the error messages, but to also copy
them into the kill ring:

.. command:: flycheck-copy-errors-as-kill
   :binding: C-c ! C-w

   Copy all Flycheck error messages at the current point into kill ring.

   Each error message is killed separately, so you can use :kbd:`M-y` to cycle
   among the killed messages after yanking the first one with :kbd:`C-y`.

.. command:: flycheck-copy-errors-as-kill
   :binding: C-c ! C-w
   :prefix-arg: C-u
   :noindex:

   Copy all Flycheck error messages **and their IDs** at the current point into
   kill ring.

.. command:: flycheck-copy-errors-as-kill
   :binding: C-c ! C-w
   :prefix-arg: M-0
   :noindex:

   Copy all Flycheck error **IDs** at the current point into kill ring.  This
   command is particularly handy to copy an ID in order to add an inline
   suppression comment.

.. _navigating-errors:

Navigating and jumping to errors
================================

By default, Flycheck integrates into standard error navigation commands of
Emacs: :kbd:`M-g n` (`next-error`) and :kbd:`M-g p` (`previous-error`) will
navigate between Flycheck warnings and errors in the current buffer.  See
:infonode:`(emacs)Compilation Mode` for more information about these commands.

.. note::

   **Visible** compilation buffers (e.g. from :kbd:`M-x compile`, :kbd:`M-x
   grep`, :kbd:`M-x occur`, etc.) take precedence over Flycheck's error
   navigation.

If you find this integration annoying and would rather keep :kbd:`M-g n`
confined to compilation buffers, you may disable it by setting
:option:`flycheck-standard-error-navigation` to nil and re-enabling
:command:`flycheck-mode` afterwards:

.. option:: flycheck-standard-error-navigation
   :auto:

Since compilation buffers take precedence, Flycheck provides an independent set
of navigation commands which always navigate Flycheck errors regardless of
compilation buffers or :option:`flycheck-standard-error-navigation`:

.. command:: flycheck-next-error
   :binding: C-c ! n

   Jump to the next Flycheck error.

   With prefix argument, jump forwards by as many errors as specified by
   the prefix argument, e.g. :kbd:`M-3 C-c ! n` will move to the 3rd error
   from the current point.

.. command:: flycheck-previous-error
   :binding: C-c ! p

   Jump to the previous Flycheck error.

   With prefix argument, jump backwards by as many errors as specified by
   the prefix argument, e.g. :kbd:`M-3 C-c ! p` will move to the 3rd
   previous error from the current point.

.. command:: flycheck-first-error

   Jump to the first Flycheck error.

   With prefix argument, jump forwards to by as many errors as specified by
   the prefix argument, e.g. :kbd:`M-3 M-x flycheck-first-error` moves to
   the 3rd error from the beginning of the buffer.

If :option:`flycheck-standard-error-navigation` is `nil`, these commands are the
only way to navigate Flycheck errors.

By default, Flycheck's error navigation considers all error levels.  You can
specify a threshold for navigation with
:option:`flycheck-navigation-minimum-level`:

.. option:: flycheck-navigation-minimum-level
   :auto:

.. _mode-line-reporting:

Mode line reporting
===================

Flycheck always indicates its state in the mode line:

`FlyC`
    There are no errors in the current buffer.

`FlyC*`
    A syntax check is being performed currently.

`FlyC:3/4`
    There are three errors and four warnings in the current buffer.

`FlyC-`
    Automatic syntax checker selection did not find a suitable syntax checker.
    See :ref:`selecting-syntax-checkers` for more information.

`FlyC!`
    The syntax check failed.  Inspect the `*Messages*` buffer for details.

`FlyC?`
    The syntax check had a dubious result.  The definition of the syntax checker
    may be flawed.  Inspect the `*Messages*` buffer for details.

    This indicator should **never** be displayed for built-in syntax checkers.
    If it is, please report an issue to the Flycheck developers, as by
    :ref:`reporting-issues`.

Change :option:`flycheck-mode-line` to customize the mode line reporting:

.. option:: flycheck-mode-line
   :auto:

.. seealso::

   The flycheck-color-mode-line_ extension changes the background colour of the
   mode line according to the result of the last syntax check.

.. _flycheck-color-mode-line: https://github.com/flycheck/flycheck-color-mode-line
