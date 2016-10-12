==========
 Glossary
==========

The glossary explains most of the special terms we use in this documentation.
some of these are originally explained in the `Emacs manual`_ or the `Emacs Lisp
reference`_, but we reproduce them here for convenience.

.. _Emacs manual: https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
.. _Emacs Lisp reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html

.. glossary::

   init file
   user init file
      Your main Emacs configuration file.  It’s typically located in your
      :term:`user emacs directory` at :file:`$HOME/.emacs.d/init.el`.  Emacs
      also looks at :file:`$HOME/.emacs`, but this location is not recommended
      anymore.  To find out the actual path to your init file of your Emacs
      session inspect the value of the variable `user-init-file` with :kbd:`C-h
      v user-init-file`.  You can visit it directly with :kbd:`M-: (find-file
      user-init-file)`.

      .. seealso::

         :infonode:`(emacs)Init File`
            More information about the init file.

         :infonode:`(elisp)Init File`
            Programming interface for the init file.

   user emacs directory
      The directory for all Emacs related files of the current user, at
      :file:`~/.emacs.d/`.  Many Emacs packages create data files in this
      directory, and it holds the recommended location for the :term:`init file`
      at :file:`~/.emacs.d/init.el`.

   registered syntax checker
      A syntax checker in `flycheck-checkers`.  Flycheck will only use these
      syntax checkers when checking buffers automatically.

   verification buffer
      A buffer shown by `M-x flycheck-verify-setup`.  This buffer contains
      information about the Flycheck setup for the current buffer.

   executable option
   executable options
     Options to override the executables of syntax checkers that run external
     commands.  They are named :samp:`flycheck-{checker}-executable`,
     e.g. ``flycheck-c/c++-clang-executable`` for `c/c++-clang`.

     Flycheck implicit defines these options for all syntax checkers defined
     with `flycheck-define-checker`.
